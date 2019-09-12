# LOAD REQUIRED PACKAGES

library(tidyverse)
library(FITSio)
library(microbenchmark)
library(sparktf)
library(sparklyr)

library(keras)
use_implementation("tensorflow")

library(parallel)
library(compiler)

library(tensorflow)

library(tfestimators)
tfe_enable_eager_execution()

library(tfdatasets)
library(caret)
library(EBImage)

# Function to retrieve target file names. 

# TCEs

set.seed(2019)

tce <- read.csv("tce.csv", stringsAsFactors = F) %>%
  filter(av_training_set != "UNK") %>%
  mutate(tce_duration = tce_duration/24) %>%         # Convert hours to days. 
  slice(sample(1:n()))                               # Randomly permute rows.


# Create training and testing datasets:

partition <- createDataPartition(y = tce$av_training_set, p = 0.9, list = F)
tce_training <- tce[partition, ]
tce_test <- tce[-partition,] 

tce_test <- tce_test %>%
  array(dimnames = dimnames(tce_test))

# Validation dataset:

partition_training <- createDataPartition(y = tce_training$av_training_set, p = 0.88, list = F)

tce_training <- tce_training[partition_training,] 
tce_training <- tce_training %>%
  array(dimnames = dimnames(tce_training))

tce_training2 <- mutate(tce_training, kepid = ifelse(tce_plnt_num > 1, paste(kepid, tce_plnt_num, sep ="-"), kepid) )

tce_validation <- tce_training[-partition_training,]
tce_validation <- tce_validation %>%
  array(dimnames = dimnames(tce_validation))


tce_test_labels <- tce_test$av_training_set

unique_kepid <- tce_training[!duplicated(tce_training$kepid),] %>%
  dplyr::select(kepid)

test_kepid <- tce_test[!duplicated(tce_test$kepid),] %>%
  select(kepid)

base_directory <- "/mnt2/archive.stsci.edu/pub/kepler/lightcurves" 

# Create File list

list_files <- function(directory, ID) {

  ID <- str_pad(ID, side = 'left', width = 9, pad = "0")
  pat <- paste(directory, substring(ID, 1, 4), ID, sep = "/")
  df <- data.frame(file = list.files(pat, recursive = T, full.names = T, ignore.case = F, pattern = "*llc.fits"))
  df$file <- as.character(df$file)
  
  return(df)
  
}



# Apply above function to generate file list for specified target. 


file_list <- apply(unique_kepid, 1, function(x) list_files(base_directory, x))

test_file_list <- apply(test_kepid, 1, function(x) list_files(base_directory, x))

cl <- makeCluster(detectCores(), type = 'FORK')


extract_table_from_fits <- function(x) {
 
  

df <- parApply(cl, test_file_list[[x]], 1, function(x) readFrameFromFITS(x)) %>%
  lapply(function(x) dplyr::select(x, TIME, PDCSAP_FLUX)) %>%
  bind_rows()

df


}

system.time(mylist <- apply(as.data.frame(c(1:1494)), 1, function(x) extract_table_from_fits(x)))

saveRDS(mylist, "/mnt1/s3/test_targets.RDS")

stopCluster(cl)
