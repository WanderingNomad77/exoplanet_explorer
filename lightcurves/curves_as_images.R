base_directory <- "/mnt2/archive.stsci.edu/pub/kepler/lightcurves" 


generate_view <- function(directory,keplerID) {
  
  x <- tce %>%
    filter(kepid == keplerID)
  
  keplerID <- str_pad(keplerID, side = 'left', width = 9, pad = "0")
  pat <- paste(base_directory, substring(keplerID, 1, 4), keplerID, sep = "/")
  df <- data.frame(file = list.files(pat, recursive = T, full.names = T, ignore.case = F, pattern = "*llc.fits"))

  read_fits <- apply(df, 1, function(x) FITSio::readFrameFromFITS(x))
  
  
  light_curve <- do.call(rbind, lapply(read_fits, as.data.frame)) %>%
    dplyr::select(TIME, PDCSAP_FLUX) %>%
    filter(!is.na(TIME), !is.na(PDCSAP_FLUX))
    

  
  if(nrow(x) > 1) {
    
    plnt_num <- readline(prompt = "tce_plnt_num: ")
    
    x <- filter(x, tce_plnt_num == plnt_num)
    
  }
  
  half_period = x$tce_period/2
  result <- (light_curve$TIME + half_period - x$tce_time0bk)%%x$tce_period
  result <- result - half_period
  
  
  light_curve <- cbind(light_curve, result)
  
  smooth_curve <- light_curve %>%
    na.exclude() %>%
    mutate(smooth_flux = (PDCSAP_FLUX/runmed(PDCSAP_FLUX, 101))) %>%
    filter(smooth_flux < quantile(smooth_flux, 0.999))
  
  lctbl <- sparklyr::copy_to(sc, smooth_curve, name = paste('keplr',keplerID, "_tbl", sep = ""), overwrite =T)

    
}

read_curve <- apply(data.frame(unique_kepid$kepid[1:5]), 1, function(x) generate_view(x, directory = base_directory))
