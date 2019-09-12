library(tidyverse)
library(keras)
use_implementation("tensorflow")
library(parallel)
library(tensorflow)
library(EBImage)
library(caret)

# Decided to split training list in 9 sublists (mylist through mylist9), in order to facilitate image extraction.


# Load data, remove missing values, 

mylist <- readRDS('/mnt1/s3/list1')
mylist2 <- readRDS('/mnt1/s3/mylist2.RDS')
mylist3 <- readRDS('/mnt1/s3/mylist3.RDS')
mylist4 <- readRDS('/mnt1/s3/mylist4.RDS')
mylist5 <- readRDS('/mnt1/s3/mylist5.RDS')
mylist6 <- readRDS('/mnt1/s3/mylist6.RDS')
mylist7 <- readRDS("/mnt1/s3/mylist7.RDS")
mylist8 <- readRDS("/mnt1/s3/mylist8.RDS")
mylist9 <- readRDS("/mnt1/s3/mylist9.RDS")

test_targets <- readRDS('/mnt1/s3/test_targets.RDS')
test_targets <- test_targets[c(1:1000)]



# Name indexes with kepler ids

names(mylist) <- unique_kepid$kepid[1:1000]
names(mylist2) <- unique_kepid$kepid[1001:2000]
names(mylist3) <- unique_kepid$kepid[2001:3000]
names(mylist4) <- unique_kepid$kepid[3001:4000]
names(mylist5) <- unique_kepid$kepid[4001:5000]
names(mylist6) <- unique_kepid$kepid[5001:6000]
names(mylist7) <- unique_kepid$kepid[6001:7000]
names(mylist8) <- unique_kepid$kepid[7001:8000]
names(mylist9) <- unique_kepid$kepid[8001:8562]

names(test_targets) <- test_kepid$kepid[1:1000]


full_list <- list(mylist2, mylist3)

names(mylist3) <- unique_kepid$kepid[6001:nrow(unique_kepid)]

full_list <- do.call(c, list(mylist, mylist2, mylist3))


# Drop missing values

test_targets <- test_targets %>%
  lapply(function(x) filter(x, !is.na(PDCSAP_FLUX)))

# Normalize data

global_view_vector2 <- function(light_curve, k = 101L, ID) {
x <- tce_test %>%
filter(kepid == ID)
if(nrow(x) > 1) {
plnt_num <- x[1,3]      # pick top event
x <- filter(x, tce_plnt_num == plnt_num)
}
half_period = x$tce_period/2
result <- (light_curve$TIME + half_period - x$tce_time0bk)%%x$tce_period
result <- result - half_period
light_curve <- cbind(light_curve, result)
smoothing <- light_curve %>%
na.exclude() %>%
dplyr::mutate(smooth_flux = (PDCSAP_FLUX/runmed(PDCSAP_FLUX, k))) %>%
filter(smooth_flux < quantile(smooth_flux, 0.95))
binning <- fields::stats.bin(x = smoothing$result, y = smoothing$smooth_flux, N = 2001)
normalized_vector <- data.frame(time = binning$centers, flux = binning$stats['median',]) %>%
na.exclude()%>%
dplyr::filter(flux < quantile(flux, 0.95)) %>%
dplyr::mutate(flux = flux -1)
return(normalized_vector)
}   # Smoothing function

# Define function to loop over list of IDs and return smoothed light curve

smoothing_function <- function(kepid) {
dat <- global_view_vector2(test_targets[[as.character(kepid)]], ID = as.integer(kepid))
dat
}

# Process code below in parallel to speed up execution

cl <- makeCluster(detectCores(), type = 'FORK')

system.time(normalized_light_curves <- parLapply(cl, test_kepid[1:1000,], function(x) smoothing_function(x)))   # Apply smoothing function to training targets

names(normalized_light_curves) <- test_kepid[1:1000,]

stopCluster(cl = cl)

# Save light curve in folder based on label.

parLapply(cl, seq_along(normalized_light_curves),  function(x) {
  
  plt <-  ggplot(normalized_light_curves[[x]], aes(time, flux)) + geom_point() + theme_void()
  
  
  if (tce_training$av_training_set[tce_training$kepid == names(normalized_light_curves)[x]] == 'PC') {
    ggsave(plt, 
           filename = paste(names(normalized_light_curves)[x], ".png", sep = ""), device = 'png',
           path = '/mnt1/s3/images/PC/', height = 7, width = 12)
    
  }
  
  if (tce_training$av_training_set[tce_training$kepid == names(normalized_light_curves)[x]] == 'AFP') {
    ggsave(plt, 
           filename = paste(names(normalized_light_curves)[x], ".png", sep = ""), device = 'png',
           path = '/mnt1/s3/images/AFP/', height = 7, width = 12)
    
  }
  
  if (tce_training$av_training_set[tce_training$kepid == names(normalized_light_curves)[x]] == 'NTP') 
  {
    ggsave(plt, 
           filename = paste(names(normalized_light_curves)[x], ".png", sep = ""), device = 'png',
           path = '/mnt1/s3/images/NTP/', height = 7, width = 12)
  }
})    # For training data

parLapply(cl, seq_along(normalized_light_curves),  function(x) {
  
  plt <-  ggplot(normalized_light_curves[[x]], aes(time, flux)) + geom_point() + theme_void()
  
  
  if (tce_test$av_training_set[tce_test$kepid == names(normalized_light_curves)[x]] == 'PC') {
    ggsave(plt, 
           filename = paste(names(normalized_light_curves)[x], ".png", sep = ""), device = 'png',
           path = '/mnt1/images/PC', height = 7, width = 12)
    
  }
  
  if (tce_test$av_training_set[tce_test$kepid == names(normalized_light_curves)[x]] == 'AFP') {
    ggsave(plt, 
           filename = paste(names(normalized_light_curves)[x], ".png", sep = ""), device = 'png',
           path = '/mnt1/images/AFP', height = 7, width = 12)
    
  }
  
  if (tce_test$av_training_set[tce_test$kepid == names(normalized_light_curves)[x]] == 'NTP') 
  {
    ggsave(plt, 
           filename = paste(names(normalized_light_curves)[x], ".png", sep = ""), device = 'png',
           path = '/mnt1/images/NTP', height = 7, width = 12)
  }
})  # For test dataset

#######################
## PROCESSING IMAGES ##
#######################

# Lists for training images

pc_list <- data.frame(image = list.files("/mnt1/s3/images/PC/", full.names = T))
afp_list <- data.frame(afp_img = list.files('/mnt1/s3/images/AFP/', full.names = T))
ntp_list <- data.frame(afp_img = list.files('/mnt1/s3/images/NTP/', full.names = T))

# Lists for testing images

pc_test_list <- data.frame(image = list.files("/mnt1/images/PC", full.names = T))
afp_test_list <- data.frame(afp_img = list.files('/mnt1/images/AFP', full.names = T))
ntp_test_list <- data.frame(afp_img = list.files('/mnt1/images/NTP', full.names = T))


# Function to read, resize and turn images into greyscale. 

cl<- makeCluster(detectCores(), type = 'FORK')


process_img <- function(x){
  
  img <- readImage(x) %>%
    resize(72,72) %>%
    channel(mode = 'grey')
}


# Read, resize, and 'greyscale' images (Training)

system.time(pc_images <- parApply(cl, as.data.frame(pc_list[1:nrow(pc_list),]), 1,function(y) list(process_img(y))))   # PC Images

system.time(afp_images <- parApply(cl, as.data.frame(afp_list[1:nrow(afp_list),]), 1, function(x) list(process_img(x))))  # AFP Images

system.time(ntp_images <- parApply(cl, as.data.frame(ntp_list[1:nrow(ntp_list),]), 1, function(x) list(process_img(x)))) # NTP Images

stopCluster(cl)

# Read, resize, and 'greyscale' images (Test)

system.time(pc_test_images <- parApply(cl, as.data.frame(pc_test_list[1:nrow(pc_test_list),]), 1,function(y) list(process_img(y))))   # PC Images

system.time(afp_test_images <- parApply(cl, as.data.frame(afp_test_list[1:nrow(afp_test_list),]), 1, function(x) list(process_img(x))))  # AFP Images

system.time(ntp_test_images <- parApply(cl, as.data.frame(ntp_test_list[1:nrow(ntp_test_list),]), 1, function(x) list(process_img(x)))) # NTP Images

stopCluster(cl)

### RESHAPING

# Training 

pc_images <- parLapply(cl, pc_images, function(x) array_reshape(x, c(-1,72,72,1))) # Reshape PC light curves. 
afp_images <- parLapply(cl, afp_images, function(x) array_reshape(x, c(-1,72,72,1))) # Reshape AFP light curves. 
ntp_images <- parLapply(cl, ntp_images, function(x) array_reshape(x, c(-1,72,72,1))) # Reshape NTP light curves. 

# Test

pc_test_images <- parLapply(cl, pc_test_images, function(x) array_reshape(x, c(-1,72,72,1))) # Reshape PC light curves. 
afp_test_images <- parLapply(cl, afp_test_images, function(x) array_reshape(x, c(-1,72,72,1))) # Reshape AFP light curves. 
ntp_test_images <- parLapply(cl, ntp_test_images, function(x) array_reshape(x, c(-1,72,72,1))) # Reshape NTP light curves. 


# Prepare training dataset by binding rows.

set.seed(2019)

pc_train <- pc_images[c(1:(length(pc_images)))]
afp_train <- afp_images[c(1:(length(afp_images)))]
ntp_train <- ntp_images[c(1:(length(ntp_images)))]

trainx <- do.call(rbind, c(pc_train, afp_train, ntp_train))
trainx <- array_reshape(trainx, c(nrow(trainx),72,72,1))

trainy <- c(rep(0, each = (length(afp_train) + length(ntp_train))), rep(1, each = length(pc_train)))

# Prepare test dataset

testx <- do.call(rbind, c(pc_test_images, afp_test_images, ntp_test_images))
testx <- array_reshape(testx, c(nrow(testx), 72,72,1))

testy <- c(rep(0, each = (length(afp_test_images) + length(ntp_test_images))), rep(1, each = length(pc_test_images)))
# One hot encoding 

trainLabels <- to_categorical(trainy)
testLabels <- to_categorical(testy)


model <- keras_model_sequential()
model %>%
  layer_conv_2d(16, c(3,3), activation = 'relu', input_shape = c(72,72,1)) %>%
  layer_dropout(rate = 0.25) %>%
  layer_max_pooling_2d(c(2,2)) %>%
  layer_conv_2d(16, c(3,3), activation = 'relu') %>%
  layer_dropout(rate = 0.25) %>%
  layer_max_pooling_2d(c(2,2)) %>%
  layer_flatten() %>%
  layer_dense(16, activation = 'relu') %>%
  layer_dropout(rate = 0.25) %>%
  layer_dense(2, activation = 'sigmoid')

# Compile

model %>%
  keras::compile(loss = 'binary_crossentropy',
                 optimizer = "adam",
                 metrics = c("accuracy"))

summary(model)


# Fit model 

history <- model %>%
  fit(trainx,
      trainLabels, epochs = 15,
      batch_size = 64)

# Evaluation and Prediction

model %>% evaluate(trainx, trainLabels)

pred <- model %>% predict_classes(trainx)
table(Predicted = pred, Actual= trainy)

# Probabilities

prob <- model %>% predict_proba(trainx)
cbind(prob, Predicted = pred, Actual = trainy)

# Evaluate test data

model %>% evaluate(testx, testLabels)
test_predictions <- model %>% predict_classes(testx)
table(Predicted = test_predictions, Actual= testy)


stopCluster(cl)