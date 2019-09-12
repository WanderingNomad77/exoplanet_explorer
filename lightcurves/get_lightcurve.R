library(tidyverse)
library(FITSio)
library(astro)
library(ggthemes)

  
# Function to retrieve target file names. 

base_directory <- "/mnt2/archive.stsci.edu/pub/kepler/lightcurves" 


list_files <- function(directory, ID = NULL) {
  
  if (is.null(ID)) {
  
   ID <- readline(prompt = "Enter ID: ")
  }  
  
  ID <- str_pad(ID, side = 'left', width = 9, pad = "0")
  pat <- paste(directory, substring(ID, 1, 4), ID, sep = "/")
  df <- data.frame(file = list.files(pat, recursive = T, full.names = T, ignore.case = F, pattern = "*llc.fits"))
  
   return(df)

}


# Apply above function to generate file list for specified target. 

file_list <- list_files(base_directory)

# Read target light curve

read_fits <- apply(file_list, 1, function(x) FITSio::readFrameFromFITS(x))


# Function to extract light curves by segment

segment_curve <- function(segment) {
  
  if(segment > nrow(file_list)) {
    
    message("Segment does not exist")
    
  }
  
  else {
    
  segmented_light_curve <- dplyr::select(read_fits[[segment]], TIME , PDCSAP_FLUX) %>%
    filter(!is.na(TIME), !is.na(PDCSAP_FLUX))
  
  }
  
  segmented_light_curve
  
  ggplot(segmented_light_curve, aes(TIME, PDCSAP_FLUX)) + 
    geom_point(col = 'darkgreen') +
    theme_gdocs() +
    theme(axis.ticks= element_blank(),
          axis.text.y = element_blank(),
          axis.line = element_line(colour = 'darkgreen', linetype = 'solid')) +
    ylab("BRIGHTNESS")
}

# Bind all segments together

full_light_curve <- do.call(rbind, lapply(read_fits, as.data.frame))%>%
  dplyr::select(TIME, PDCSAP_FLUX) %>%
  filter(!is.na(TIME), !is.na(PDCSAP_FLUX)) 



