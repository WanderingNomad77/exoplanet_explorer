# Required packages

library(tidyverse)
library(leaflet)
source('nightskytheme.R')
library(astrolibR)
library(sf)
library(corrplot)
library(ggmap)
library(concaveman)
source("lightcurves/curves_as_images.R")
library(DT)
library(grid)
library(Cairo)
library(ggthemes)
library(ggthemr)
library(parallel)
library(dplyr)
library(leafpop)
library(queryBuilder)
library(jsonlite)
library(shinyjs)
library(shinyBS)
library(plotly)
library(shinythemes)
library(shinyWidgets)
library(htmlwidgets)
library(htmltools)
library(shinydashboard)
library(promises)
library(future)

# Shortcut - loading pre-processed data - The code below is used to speed up loading time for the shiny app. 

# 1) General Data

planets <- read.csv("Data/planets.csv", stringsAsFactors = F, check.names = F)[,-1]

plnts <- planets %>% 
  select(which(sapply(.,class) %in% c('integer', 'character')), -`Right Ascension (sexagesimal)`, - `Declination (sexagesimal)`)%>%
  sapply(function(x) as.factor(x))

plnts2 <- planets %>% 
  select( - which(sapply(.,class) %in% c('integer', 'character')), `Right Ascension (sexagesimal)`, `Declination (sexagesimal)`) 

planets <- cbind(plnts, plnts2)
  
facility_coordinates <- read.csv("Data/discovery_facilities_coordinates.csv", stringsAsFactors = F, check.names = F)[,-1]

# TCEs

set.seed(2019)

tce <- read.csv("tce.csv", stringsAsFactors = F) %>%
  filter(av_training_set != "UNK") %>%
  mutate(tce_duration = tce_duration/24) %>%         # Convert hours to days. 
  slice(sample(1:n()))                               # Randomly permute rows.

## Query Builder

df.data <- planets
df.data$name <- row.names(df.data)
df.data$nameFactor <- as.factor(df.data$name)


# 2) Astromap Data

constellations_sf <- readRDS("Data/constellations_sf.RDS")
names(constellations_sf)[1] <- 'Constellation'
milky_sf_trans <- readRDS("Data/milky_sf_trans.RDS")
x <- readRDS("Data/planet_astromap.RDS")
x2 <- cbind(x, Planet = planets$`Planet Name`, Method = planets$`Discovery Method`, Size = planets$`Planet Radius (Jupiter radii)`)
stars_bright_sf <- readRDS("Data/stars_bright_sf.RDS")






