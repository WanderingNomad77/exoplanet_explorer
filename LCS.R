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

unique_kepid <- tce_training[!duplicated(tce_training$kepid),] %>%
  dplyr::select(kepid)

# Kepler 90 Data

base_directory <- "/data/archive.stsci.edu/pub/kepler/lightcurves/0114/011442793/" 

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

planets3 <- planets %>%
  select(disc = `Year of Discovery`, pl_name = `Planet Name`, method = `Discovery Method`)

gb <- planets3 %>%
  group_by(disc) %>%
  summarize(nb = n())



accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

d <- gb %>%
  accumulate_by(~disc)

p <- d %>%
  plot_ly(
    x = ~disc, 
    y = ~nb, 
    frame = ~frame,
    type = 'scatter', 
    mode = 'lines',
    
    fill = 'tozeroy', 
    fillcolor='rgba(114, 186, 59, 0.5)',
    line = list(color = 'rgb(114, 186, 59)'),
    text = ~paste("Year: ", disc, "<br># of Discoveries:", nb), 
    hoverinfo = 'text'
  ) %>%
  layout(
    title = "Yearly Evolution of Exoplanet Discoveries ",
    yaxis = list(
      title = "Number of Discovered Exoplanets", 
      range = c(0,1500), 
      zeroline = T
    ),
    xaxis = list(
      title = "Year", 
      range = c(1989,2019), 
      zeroline = F, 
      showgrid = T
    )
  ) %>% 
  animation_opts(
    frame = 200, 
    transition = 0, 
    redraw = FALSE, mode = 'afterall'
        
  ) %>%
  animation_slider(
    hide = T
  )





