# Median filtering. 

## The segments of time at which stellar brightness was recorded are on a different scale, thus producing a non-uniform light-curve. 
## To remedy this, a robust scatter plot smoothing method is applied to render the plot uniform .

## The following function creates a median filter, which computes the running median of odd spans.

smoothing <- function(light_curve, k = 101L) {
  
  smooth_curve <- light_curve %>%
    dplyr::select(TIME, PDCSAP_FLUX) %>%
    na.exclude() %>%
    mutate(smooth_flux = (PDCSAP_FLUX/runmed(PDCSAP_FLUX, k))) %>%
    filter(smooth_flux < quantile(smooth_flux, 0.999))
  
  ggplot(smooth_curve, aes(TIME, smooth_flux)) + 
    geom_point(col = 'darkgreen') +
    theme_gdocs() +
    theme(axis.ticks= element_blank(),
          axis.text.y = element_blank(),
          axis.line = element_line(colour = 'darkgreen', linetype = 'solid')) +
    ylab("BRIGHTNESS") +
    xlab("TIME (days)") +
    ggtitle("Brightness Vs Time")
  
}


global_view <- function(light_curve, k = 101L, ID) {
  
  x <- tce %>%
    filter(kepid == ID)
  
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



local_view <- function(light_curve, k = 101L, ID) {
  
  x <- tce %>%
    filter(kepid == ID)
  
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
    mutate(smooth_flux = (PDCSAP_FLUX/runmed(PDCSAP_FLUX, k))) %>%
    filter(smooth_flux < quantile(smooth_flux, 0.999))
  
  ggplot(smooth_curve, aes(result, smooth_flux)) + 
    stat_summary_bin(fun.y = 'median', color = 'blue', geom = 'point', bins = 201) +
    xlim(c(-1,1)) +
    theme_gdocs() +
    theme(axis.ticks= element_blank(),
          axis.text.y = element_blank(),
          axis.line = element_line(colour = 'blue', linetype = 'solid')) +
    ylab("BRIGHTNESS") +
    xlab("TIME (days)") +
    ggtitle("Brightness Vs Time")
  
}
