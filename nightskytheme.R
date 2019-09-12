# Theme night sky

theme_nightsky <- function(base_size = 11, base_family = "") {
  half_line <- base_size/2
  theme_light(base_size = base_size, base_family = base_family) %+replace% 
    theme(
      # Specify axis options, remove both axis titles and ticks but leave the text in white
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_text(colour = "white",size=6),
      # Specify legend options, here no legend is needed
      legend.position = "none",
      # Specify background of plotting area
      panel.grid.major = element_line(color = "grey35"),  
      panel.grid.minor = element_line(color = "grey20"),  
      panel.spacing = unit(half_line, "pt"),
      panel.background = element_rect(fill = "black", color  =  NA),  
      panel.border = element_blank(),  
      # Specify plot options
      plot.background = element_rect( fill = "black",color = "black"),  
      plot.title = element_text(size = base_size*1.2, color = "white"),
      plot.margin = unit(rep(1, 4), "lines")
    )
  
}