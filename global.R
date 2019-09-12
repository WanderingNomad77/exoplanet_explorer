library(tidyverse)
source('aitoff.R')
source('nightskytheme.R')
library(astrolibR)
library(sf)
library(corrplot)
library(ggmap)

# Import data

planets <- read.csv('planets_2019.07.09_21.07.01.csv', stringsAsFactors = F, skip = 38, na.strings = c("NA", ""))

# A little bit of cleanup...

# Name of observatories

planets$pl_facility[planets$pl_facility == "HATNet"] <- "Fred Lawrence Whipple Observatory (HATNet)"
planets$pl_facility[planets$pl_facility == 'HATSouth'] <- "High Energy Stereoscopic System (HATSouth)"
planets$pl_facility[planets$pl_facility %in% c("SuperWASP-South", "WASP-South")] <- "South African Astronomical Observatory (SuperWASP-South)"
planets$pl_facility[planets$pl_facility == "SuperWASP-North"] <- "SuperWASP-North - Roque de Los Muchachos"
planets$pl_facility[planets$pl_facility == "SuperWASP"] <- "SuperWASP - Roque de Los Muchachos "

######## Exploring Correlation 

num <- planets %>%
  select(which(sapply(.,class) == 'numeric'), pl_pnum)

y <- cor(num, use = "pairwise.complete.obs")

corrplot(y, type = 'lower')



### Celestial map

# Aitoff projection: Convert declination and right ascension to pair of cartesian coordinates adjusted to aitoff grid.

x <- astrolibR::aitoff(planets$ra, planets$dec)

#  Transform to sf object

x <- x %>%
  as.data.frame() %>%
  sf::st_as_sf(coords = c(1,2))

st_crs(x) = 4326


x <- st_transform(x, crs = "+proj=moll")

# Plot Celestial Map

ggplot(x, aes()) + geom_sf(col= 'white') + theme_nightsky()


# Read in Milky way data

milky_sf <- st_read("https://raw.githubusercontent.com/ofrohn/d3-celestial/master/data/mw.json", stringsAsFactors = F)

# Stars data

stars_sf <- st_read("https://raw.githubusercontent.com/ofrohn/d3-celestial/master/data/stars.6.json", stringsAsFactors = F)

# Constellations data

constellations_sf <- st_read("https://raw.githubusercontent.com/ofrohn/d3-celestial/master/data/constellations.lines.json", stringsAsFactors = F)

milky_sf_trans <- st_transform(milky_sf, crs = "+proj=moll")


### Facilities locations

fac_df <- data.frame("facility" = unique(planets$pl_facility)) 
fac_df$facility <- as.character(fac_df$facility)
locs <- ggmap::mutate_geocode(fac_df, facility)
  
m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng= locs$lon, lat=locs$lat,
             popup=locs$facility)


####

options(scipen = 999)
discovery_facilities <- planets %>%
  group_by(pl_facility) %>%
  summarize('Discovered Planets' = n(),
            'Discovery Methods' = n_distinct(pl_discmethod),
            'Average Orbital Period (days)' = round(mean(pl_orbper, na.rm = T),2),
            'Median Orbital Period (days)' = round(median(pl_orbper, na.rm = T),2),
            'Average Orbit Semi-Major Axis (AU)' = round(mean(pl_orbsmax, na.rm = T),2),
            'Median Orbit Semi-Major Axis (AU)' = round(median(pl_orbsmax, na.rm = T),2),
            'Average Inclination (deg)' = round(mean(pl_orbincl, na.rm = T), 2))

# Use Google API key
register_google(key = "AIzaSyDk3BpQG_3C2fgWJhgXXtuTRaDBbeB6FXg")

# Add coordinates
facility_coordinates <- ggmap::mutate_geocode(discovery_facilities, pl_facility)

# Kepler is actually not an observatory facility, but a space telescope. The Data is collected at the .. located on the John blabla. 
#We will use the coordinates of the data management center for Kepler related exoplanet discoveries.

facility_coordinates[,c("lon", "lat")][facility_coordinates$pl_facility == "Kepler",] <- c(-76.625466,39.3327283)

mp <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng= facility_coordinates$lon, lat= facility_coordinates$lat,
             popup = paste(
               paste(tags$strong("Observatory: "),facility_coordinates$pl_facility, sep = ""), 
               paste(tags$strong("Number of Discovered Planets: "), facility_coordinates$`Discovered Planets`, sep = ""), sep = '<br/>'))



