### Function to generate a 'global view' vector of a phase-folded light curve. 
# Arguments: light_curve -> unprocessed light curve
#            ID -> Kepler Target ID
# Output: 

global_view_vector <- function(light_curve, k = 101L, ID) {
  
  x <- tce_training %>%
    filter(kepid == ID)
  
  if(nrow(x) > 1) {
    
    plnt_num <- readline(prompt = "tce_plnt_num: ")
    
    x <- filter(x, tce_plnt_num == plnt_num)
    
  }
  
  half_period = x$tce_period/2
  result <- (light_curve$TIME + half_period - x$tce_time0bk)%%x$tce_period
  result <- result - half_period
  
  light_curve <- cbind(light_curve, result)
  
  smoothing <- light_curve %>%
    na.exclude() %>%
    mutate(smooth_flux = (PDCSAP_FLUX/runmed(PDCSAP_FLUX, k))) %>%
    filter(smooth_flux < quantile(smooth_flux, 0.95))
  
  binning <- fields::stats.bin(x = smoothing$result, y = smoothing$smooth_flux, N = 2001)
  normalized_vector <- data.frame(time = binning$centers, flux = binning$stats['median',]) %>%
    na.exclude()%>%
    filter(flux < quantile(flux, 0.95))
  
  
  return(normalized_vector)
  
  
}

local_view_vector <- function(light_curve, k = 101L, ID) {
  
  x <- tce_training %>%
    filter(kepid == ID)
  
  if(nrow(x) > 1) {
    
    plnt_num <- readline(prompt = "tce_plnt_num: ")
    
    x <- filter(x, tce_plnt_num == plnt_num)
    
  }
  
  half_period = x$tce_period/2
  result <- (light_curve$TIME + half_period - x$tce_time0bk)%%x$tce_period
  result <- result - half_period
  
  light_curve <- cbind(light_curve, result)
  
  smoothing <- light_curve %>%
    na.exclude() %>%
    mutate(smooth_flux = (PDCSAP_FLUX/runmed(PDCSAP_FLUX, k))) %>%
    filter(smooth_flux < quantile(smooth_flux, 0.95))
  
  binning <- fields::stats.bin(x = smoothing$result, y = smoothing$smooth_flux, N = 401)
  normalized_vector <- data.frame(time = binning$centers, flux = binning$stats['median',]) %>%
    na.exclude()%>%
    filter(flux < quantile(flux, 0.95))
  
  
  return(normalized_vector)
  
  
}

# Connect to Spark 

Sys.setenv("SPARK_HOME" = '/usr/lib/spark/')

config <- spark_config()

sc <- sparklyr::spark_connect(master = 'yarn-client', config = config)


# Copy global_view vector to spark then write it to disk with spark_write_tfrecord().

sc2 <- spark_connect(master = "local")

# Copy training and test datasets to Spark.

tce_training_tbl <- sdf_copy_to(sc2, tce_training, "tce_training_tbl", overwrite = T)  # Training dataset

tce_test_tbl <- sdf_copy_to(sc2, tce_test, "tce_test_tbl", overwrite = T)  # Test dataset


# Create destination for tfrecord files. 

data_path3 <- file.path(tempdir(), "tce_training_tbl")   
data_path5 <- file.path(tempdir(), "tce_test_tbl")

# Write datasets to disk via spark_write_tfrecord().

tce_training_tbl <- tce_training_tbl %>%      # Training
  ft_string_indexer_model(
    'av_training_set', "label",
    labels = c("AFP", "NTP","PC")
  ) %>%
  spark_write_tfrecord(
    path = data_path3,
    write_locality = 'local'
  )

tce_test_tbl<- tce_test_tbl %>%        # Test
  ft_string_indexer_model(
    'av_training_set', "label",
    labels = c("AFP", "NTP","PC")
  ) %>%
  spark_write_tfrecord(
    path = data_path5,
    write_locality = 'local'
  )


## Reading saved TFRecords and parsing the contents to create dataset objects. 

# Training Dataset 

dataset <-tf$data$TFRecordDataset(filenames = list.files(data_path3, full.names = T)) %>%
  
  dataset_map(function(example_proto) {
    features <- list(
      label = tf$FixedLenFeature(shape(), tf$float32),
      tce_period = tf$FixedLenFeature(shape(), tf$float32),
      tce_time0bk = tf$FixedLenFeature(shape(), tf$float32),
      tce_duration = tf$FixedLenFeature(shape(), tf$float32)
      
    )
    features <- tf$parse_single_example(example_proto, features)
    x <- list(features$tce_period,
              features$tce_time0bk, 
              features$tce_duration)
    
    y <- tf$one_hot(tf$cast(features$label, tf$int32), 3L)
    list(x, y)
  }) %>%
  dataset_shuffle(75L) %>%
  dataset_batch(512)

# Test Dataset 

test <- tfrecord_dataset(filenames = list.files(data_path5, full.names = T)) %>%
  dataset_map(function(example_proto) {
    features <- list(
      label = tf$FixedLenFeature(shape(), tf$float32),
      tce_period = tf$FixedLenFeature(shape(), tf$float32),
      tce_time0bk = tf$FixedLenFeature(shape(), tf$float32),
      tce_duration = tf$FixedLenFeature(shape(), tf$float32)
      
      
    )
    
    features <- tf$parse_single_example(example_proto, features)
    x <- list(features$tce_period,
              features$tce_time0bk, 
              features$tce_duration
    )
    y <- tf$one_hot(tf$cast(features$label, tf$int32), 3L)
    list(x, y)
  }) %>%
  dataset_shuffle(150L) %>%
  dataset_batch(nrow(tce_test))


## Define Keras model.

model <- keras_model_sequential() %>%
  layer_dense(256, activation = "relu", input_shape = 3, kernel_regularizer = regularizer_l1(0.001)) %>%
  layer_dropout(rate = 0.25) %>%
  layer_dense(units = 128, activation = 'relu', kernel_regularizer = regularizer_l1(0.001)) %>% 
  layer_dropout(rate = 0.25) %>%
  layer_dense(units = 32, activation = 'relu', kernel_regularizer = regularizer_l1(0.001)) %>%
  layer_dropout(rate = 0.25) %>%
  layer_dense(units = 16,activation = 'relu',  kernel_regularizer = regularizer_l1(0.001)) %>%
  layer_dropout(rate = 0.25) %>%
  layer_dense(units = 3, activation = 'softmax')

model %>%
  keras::compile(loss = "categorical_crossentropy", 
          optimizer = 'rmsprop', 
          metrics = c("accuracy"))

# Train model by feeding training dataset into model. 

system.time(replicate(history <- model %>%
  fit(dataset, epochs = 50, steps_per_epoch = 250, batch_size = 64),n = 5))

# Evaluate model performance on training dataset. 

model %>% evaluate(dataset)

# Evaluate model performance on test dataset

model %>% evaluate(test)

pred <- model %>% predict_classes(test)
# Evaluate model on new data. 

new_data <- tf$constant(c(tce_test$tce_period[5], tce_test$tce_time0bk[5], tce_test$tce_duration[5]), shape = c(1, 3))

model(new_data)

inputs <- layer_input(shape = c(1,3))

predictions <- inputs %>%


model <- keras_model(inputs = inputs, outputs = predictions)
model %>% compile(
  optimizer = 'rmsprop',
  loss = 'categorical_crossentropy',
  metrics = c('accuracy')
)
