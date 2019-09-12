
# Prepare data frame for downloading light curves

dl_list <- unique_kepid$kepid %>%
  data.frame()

# Function to retrieve files of interest

download_light_curves <- function(kepid) {
  
  kepid <- str_pad(kepid, side = 'left', width = 9, pad = "0")
  x <- paste("wget http://archive.stsci.edu/pub/kepler/lightcurves", substring(kepid, 1,4), paste(kepid, "/", sep=""), sep ="/")
  
  system(paste(x, " -r -k -p --no-parent -e robots=off", sep = " "))
}

# 

download_all_files <- apply(dl_list, 1, download_light_curves)