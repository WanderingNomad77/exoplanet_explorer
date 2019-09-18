library(parallel)

base_directory <- "/data/archive.stsci.edu/pub/kepler/lightcurves" 


generate_view <- function(directory,keplerID) {
  
  x <- tce %>%
    filter(kepid == keplerID)
  
  keplerID <- str_pad(keplerID, side = 'left', width = 9, pad = "0")
  pat <- paste(base_directory, substring(keplerID, 1, 4), keplerID, sep = "/")
  df <- data.frame(file = list.files(pat, recursive = T, full.names = T, ignore.case = F, pattern = "*llc.fits"))

  cl <- parallel::makeCluster(detectCores()/2, type = 'FORK')
  
  read_fits <- parApply(cl, df, 1, function(x) FITSio::readFrameFromFITS(x))
  
  
  light_curve <- do.call(rbind, lapply(read_fits, as.data.frame)) %>%
    dplyr::select(TIME, PDCSAP_FLUX) %>%
    filter(!is.na(TIME), !is.na(PDCSAP_FLUX))
    

  
  half_period = x$tce_period/2
  result <- (light_curve$TIME + half_period - x$tce_time0bk)%%x$tce_period
  result <- result - half_period
  
  
  light_curve <- cbind(light_curve, result)
  
  smooth_curve <- light_curve %>%
    dplyr::select(TIME, PDCSAP_FLUX) %>%
    na.exclude() %>%
    mutate(smooth_flux = (PDCSAP_FLUX/runmed(PDCSAP_FLUX, 101))) %>%
    filter(smooth_flux < quantile(smooth_flux, 0.999))
  
  print(ggplot(smooth_curve, aes(TIME, smooth_flux)) + 
    geom_point(col = 'darkgreen') +
    theme_gdocs() +
    theme(axis.ticks= element_blank(),
          axis.text.y = element_blank(),
          axis.line = element_line(colour = 'darkgreen', linetype = 'solid')) +
    ylab("BRIGHTNESS") +
    xlab("TIME (days)") +
    ggtitle("Brightness Vs Time"))

  stopCluster(cl)
    
}


generate_global_view <- function(directory, keplerID, k = 101L, plnt_num = 1) {
  

  x <- tce %>%
    filter(kepid == keplerID, tce_plnt_num == plnt_num)
  
  keplerID <- str_pad(keplerID, side = 'left', width = 9, pad = "0")
  pat <- paste(base_directory, substring(keplerID, 1, 4), keplerID, sep = "/")
  df <- data.frame(file = list.files(pat, recursive = T, full.names = T, ignore.case = F, pattern = "*llc.fits"))
  
  cl <- parallel::makeCluster(4, type = 'FORK')
  
  
  read_fits <- parApply(cl,df, 1, function(x) FITSio::readFrameFromFITS(x))
  
  light_curve <- do.call(rbind, lapply(read_fits, as.data.frame)) %>%
    dplyr::select(TIME, PDCSAP_FLUX) %>%
    filter(!is.na(TIME), !is.na(PDCSAP_FLUX))
  
  stopCluster(cl)
  

    half_period = x$tce_period/2
  result <- (light_curve$TIME + half_period - x$tce_time0bk)%%x$tce_period
  result <- result - half_period
  
  light_curve <- cbind(light_curve, result)
  
  smooth_curve <- light_curve %>%
    na.exclude() %>%
    mutate(smooth_flux = (PDCSAP_FLUX/runmed(PDCSAP_FLUX, k))) %>%
    filter(smooth_flux < quantile(smooth_flux, 0.999))
  
  ggplot(smooth_curve, aes(result, smooth_flux)) + 
    stat_summary_bin(fun.y = 'median', color = 'blue', geom = 'point', bins = 2001, binwidth = 1/2001 * x$tce_period) +
    xlim(c(-x$tce_period/2, x$tce_period/2)) +
    theme_gdocs() +
    theme(axis.ticks= element_blank(),
          axis.text.y = element_blank(),
          axis.line = element_line(colour = 'blue', linetype = 'solid')) +
    ylab("BRIGHTNESS") +
    xlab("TIME (days)") +
    ggtitle("Brightness Vs Time")
  
 
  
}
  
  






