# Copy TCE table to spark

tce_tbl <- sparklyr::copy_to(sc, donnees, "tce_tbl")

# Copy light curve table to spark

lc_tbl <- sparklyr::copy_to(sc, df, "lc_tbl")



# Light curves

# Bash script 

bscript <- 'sudo wget -rkp --no-parent -R "index.html*" -e robots=off -i lc_urls -P /mnt2/' 


ind <- round(seq(1, length(df$TIME), len = 880300))
df$TIME <- df$TIME[ind]
df$PDCSAP_FLUX <- df$PDCSAP_FLUX[ind]
fit <- loess(df$PDCSAP_FLUX ~ df$TIME)
bias <- predict(fit, newdata = data.frame(df$TIME = TIME))
nPDC <- df$PDCSAP_FLUX - bias


microbenchmark::microbenchmark(
  setup = library(arrow),
  arrow_on = {
    sparklyr_df <<- copy_to(sc, df, overwrite = T)
    count(sparklyr_df) %>% collect()
  },
  arrow_off = {
    if ("arrow" %in% .packages()) detach("package:arrow")
    sparklyr_df <<- copy_to(sc, df, overwrite = T)
    count(sparklyr_df) %>% collect()
  },
  times = 10
) %>% print()


### USE AS REFERENCE IF FINAL CODE PRESENTS ERRORS

local_view <- function(light_curve, k = 101L, ID) {
  
  x <- donnees %>%
    filter(kepid == ID)
  
  half_period = x$tce_period/2
  result <- (light_curve$TIME + half_period - x$tce_time0bk)%%x$tce_period
  result <- result - half_period
  
  light_curve <- cbind(light_curve, result)
  
  smooth_curve <- light_curve %>%
    na.exclude() %>%
    mutate(smooth_flux = (PDCSAP_FLUX/runmed(PDCSAP_FLUX, k))) %>%
    filter(smooth_flux < quantile(smooth_flux, 0.999))
  
  ggplot(smooth_curve, aes(result, smooth_flux)) + 
    stat_summary_bin(fun.y = 'median', color = 'blue', geom = 'point', bins = 201, binwidth = 0.16*x$tce_duration) +
    xlim(c(max(-x$tce_period/2, -4*x$tce_duration), min(x$tce_period/2, 4*x$tce_duration))) +
    theme_gdocs() +
    theme(axis.ticks= element_blank(),
          axis.text.y = element_blank(),
          axis.line = element_line(colour = 'blue', linetype = 'solid')) +
    ylab("BRIGHTNESS") +
    xlab("TIME (days)") +
    ggtitle("Brightness Vs Time")
  
}



# Centering main event on x-axis. 

folding <- function(x) {
  
  smoothing(x - x$TIME[which.min(x$PDCSAP_FLUX)]) + xlim(c(-4,4))
}


folding2 <- function(x) {
  
  smoothing2(x - x$result[which.min(x$PDCSAP_FLUX)])
}

test <- sdf_copy_to(sc, lightcurve)



####################



smooth_curve <- full_light_curve %>%
  dplyr::select(TIME, PDCSAP_FLUX) %>%
  na.exclude() 

y <- fields::stats.bin(x = x$result, y = x$smooth_flux, N = 2001)
df2 <- data.frame(time = y$centers, flux = y$stats['median',]) %>%
  na.exclude() %>%
  mutate(smooth_flux = (flux/runmed(flux, 101))) 



z <- fields::stats.bin(x = smooth_curve$TIME, y = smooth_curve$PDCSAP_FLUX, N = 2001)
df <- data.frame(time = z$centers, flux = z$stats['median',]) %>%
  na.exclude() %>%
  mutate(smooth_flux = (flux/runmed(flux, 101))) 




global_view2 <- function(light_curve, k = 101L, ID) {
  
  x <- tce %>%
    filter(kepid == ID)
  
  if(nrow(x) > 1) {
    
    plnt_num <- readline(prompt = "tce_plnt_num: ")
    
    x <- filter(x, tce_plnt_num == plnt_num)
    
  }
  
  half_period = x$tce_period/2
  result <- (light_curve$time + half_period - x$tce_time0bk)%%x$tce_period
  result <- result - half_period
  
  light_curve <- cbind(light_curve, result) %>%
    filter(smooth_flux < quantile(smooth_flux, 0.95))
  
  
  
  ggplot(light_curve, aes(result, smooth_flux)) + 
    stat_summary_bin(fun.y = 'median', color = 'blue', geom = 'point', bins = 1104, binwidth = 1/1104 * x$tce_period) +
    xlim(c(-x$tce_period/2, x$tce_period/2)) +
    theme_gdocs() +
    theme(axis.ticks= element_blank(),
          axis.text.y = element_blank(),
          axis.line = element_line(colour = 'blue', linetype = 'solid')) +
    ylab("BRIGHTNESS") +
    xlab("TIME (days)") +
    ggtitle("Brightness Vs Time")
  
}







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




########

a = tf$Variable(matrix(c(0.33,0.33,0.33), nrow = 1), dtype = tf$float32)
b = tf$Variable(matrix(c(0.11, 0.22, 0.33, 0.44, 0.55, 0.66), nrow = 2), dtype = tf$float32)

tf$concat(list(a,b), 0L)


t0 = tf$Variable(matrix(tce_training$tce_time0bk), dtype = tf$float32)
period = tf$Variable(matrix(tce_training$tce_period), dtype = tf$float32)
duration = tf$Variable(matrix(tce_training$tce_duration), dtype = tf$float32)
labels = tf$Variable(matrix(tce_training$av_training_set), dtype = tf$string)

ccat <- tf$concat(list(period, t0, duration), 1L)

time = tf$Variable(matrix(kep90$time, nrow = nrow(kep90)), dtype = tf$float32)
flux = tf$Variable(matrix(kep90$flux, nrow = nrow(kep90)), dtype = tf$float32)

t <- tf$concat(list(period, t0, duration, time, flux), 1L)







read_curve <- apply(data.frame(unique_kepid$kepid[1]), 1, function(x) generate_view(x, directory = base_directory))
