library(tidyverse)
library(FITSio)
library(microbenchmark)
library(sparktf)
library(sparklyr)
library(keras)
use_implementation("tensorflow")
library(tensorflow)
library(tfestimators)
tfe_enable_eager_execution()
library(tfdatasets)
library(caret)

library(modules)


options(scipen = 999)


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

tce_validation <- tce_training[-partition_training,]
tce_validation <- tce_validation %>%
  array(dimnames = dimnames(tce_validation))


# Labels

tce_train_labels <- tce_training$av_training_set %>%
  as.array()

tce_training <- tce_training %>%
  dplyr::select(-av_training_set)

tce_test_labels <- tce_test$av_training_set %>%
  as.array()

tce_test <- tce_test %>%
  dplyr::select(-av_training_set)

tce_validation_labels <- tce_validation$av_training_set %>%
  as.array()

tce_validation <- tce_validation %>%
  dplyr::select(-av_training_set)


unique_kepid <- tce_training[!duplicated(tce_training$kepid),] %>%
  dplyr::select(kepid)


# Kepler 90 Data

base_directory <- "/mnt2/archive.stsci.edu/pub/kepler/lightcurves" 

list_files <- function(directory, ID = "11442793") {
  
  ID <- str_pad(ID, side = 'left', width = 9, pad = "0")
  pat <- paste(directory, substring(ID, 1, 4), ID, sep = "/")
  df <- data.frame(file = list.files(pat, recursive = T, full.names = T, ignore.case = F, pattern = "*.fits")) 
 df
  
}


file_list <- apply(data.frame(base_directory), 1, list_files) %>%
  purrr::map_dfr(
    magrittr::extract, 'file'
  ) %>%
  tbl_df() %>%
  distinct()

# Read Kepler 90 table from .FITS file

read_fits <- apply(file_list, 1, function(x) data.frame(readFrameFromFITS(x)))

# Full Kepler 90 data frame

df <- do.call(rbind, lapply(read_fits, as.data.frame))

# Read table from pre saved RDS file

kepler90_df <- do.call(rbind, lapply(fits, as.data.frame))

# Export df to csv

savedf <-saveRDS(df, file = 'kepler90.RDS', row.names = F)

# Spark Connection

Sys.setenv("SPARK_HOME" = '/usr/lib/spark/')

config <- spark_config()

sc <- sparklyr::spark_connect(master = 'yarn-client', config = config)
sc2 <- spark_read_table(sc, 'df2')

## After creating tce hive table and loading the data into it, we need to cache the table into memory

# Cache tce table into memory
tbl_cache(sc, "tce")
tce_tbl <- tbl(sc, "tce") 

# 

full_tbl <- copy_to(sc, full_light_curve, name = "blabla")

tbl_cache(sc, "blabla")
blabla_tbl <- tbl(sc, "blabla")


