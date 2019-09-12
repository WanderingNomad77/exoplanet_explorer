# Create dataset composed of arrays of different lengths

mylist <- list( tce_training$tce_period[1], tce_training$tce_time0bk[1],  tce_training$tce_duration[1], 
            kep90$time,kep90$flux)

mylist2 <- list( tce_training$tce_period[1], tce_training$tce_time0bk[1],  tce_training$tce_duration[1], 
                 kep90$time,kep90$flux)

gen <- function(x) {
  
  tuple(mylist)
}

dt <- tf$data$Dataset$from_generator(gen, tf$float32, output_shapes = c(3,4) )


pad <- t(pad_sequences(mylist, value = 0, padding = 'post', dtype = 'float32'))
pad2 <- pad_sequences(mylist2, value = 0, padding = 'post', dtype = 'float32')
colnames(pad) <- c("period", "t0", "duration", "time", "flux")
colnames(pad2) <- c("period", "t0", "duration", "time", "flux")


#join pad and pad2

tst <- array( c( pad , pad2 ) , dim = c(nrow(pad) , 5, 2 ) )


embedding <- tf$keras$layers$Embedding(input_dim = 5000L, output_dim = 16L, mask_zero = T)
masked_output <- embedding(pad)

masking_layer <- tf$keras$layers$Masking(mask_value = 0 )

unmasked_embedding <- tf$cast(
  tf$tile(tf$expand_dims(pad, axis = -1L), c(1L,1L, 1L)), tf$float32
)

masked_embedding <- masking_layer(unmasked_embedding)
tce_train_labels <- tce_training$av_training_set %>%
  as.array()


model <- keras_model_sequential() 

model %>%
  layer_embedding(input_dim = 1000, output_dim = 64, mask_zero = T) %>%
  layer_masking(mask_value = 0, input_shape = c("timesteps", "features", "samples")) %>%
  layer_lstm(units = 32) %>%
  compile(loss = "categorical_crossentropy", optimizer = tf$train$AdamOptimizer(), metrics = c("accuracy"))





dset <- tf$data$Dataset$from_tensors(masked_output) %>%
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
  dataset_shuffle(150) %>%
  dataset_batch(16)


