library(tidyverse)
library(leaflet)
source('nightskytheme.R')
library(astrolibR)
library(sf)
library(corrplot)
library(ggmap)
library(concaveman)
source("lightcurves/curves_as_images.R")

# Import data

planets <- read.csv('planets_2019.07.09_21.07.01.csv', stringsAsFactors = F, skip = 38, na.strings = c("NA", "")) %>%
  select(loc_rowid, `Planet Hostname` = pl_hostname, pl_letter, `Planet Name` = pl_name, `Discovery Method` = pl_discmethod, pl_controvflag, pl_pnum,
         `Orbital Period (days)` = pl_orbper, `Orbit Semi-Major Axis (AU)` = pl_orbsmax, Eccentricity = pl_orbeccen, `Inclination (deg)` = pl_orbincl, 
         `Planet Mass or M*sin(i) [Jupiter mass]` = pl_bmassj, `Planet Radius (Jupiter radii)` = pl_radj, `Planet Density (g/cm**3)` = pl_dens,
         `Planet Mass or M*sin(i) Provenance` = pl_bmassprov, `TTV Flag` = pl_ttvflag, `Kepler Field Flag` = pl_kepflag,
         `K2 Mission Flag` = pl_k2flag, pl_nnotes, `Right Ascension (sexagesimal)` = ra_str, `Declination (sexagesimal)` = dec_str, `Right Ascension (decimal degrees)` = ra,
         `Declination (decimal degrees)` = dec, st_dist:pl_facility)

planets <- read.csv("planets_2019.09.29_21.45.42.csv", stringsAsFactors = F, skip = 46, na.strings = c("NA", "")) %>%
  select(`Planet Hostname` = pl_hostname, `Planet Letter` = pl_letter, `Planet Name` = pl_name, `Discovery Method` = pl_discmethod, pl_controvflag, `Number of Planets in System` = pl_pnum,
         `Orbital Period (days)` = pl_orbper, `Orbit Semi-Major Axis (AU)` = pl_orbsmax, Eccentricity = pl_orbeccen, `Inclination (deg)` = pl_orbincl, 
         `Planet Mass or M*sin(i) [Jupiter mass]` = pl_bmassj, `Planet Radius (Jupiter radii)` = pl_radj, `Planet Density (g/cm**3)` = pl_dens,
         `Planet Mass or M*sin(i) Provenance` = pl_bmassprov, `TTV Flag` = pl_ttvflag, `Kepler Field Flag` = pl_kepflag,
         `K2 Mission Flag` = pl_k2flag, `Right Ascension (sexagesimal)` = ra_str, `Declination (sexagesimal)` = dec_str, `Right Ascension (decimal degrees)` = ra,
         `Declination (decimal degrees)` = dec, `Distance (pc)` = st_dist, `Gaia Distance (pc)` = gaia_dist ,`Optical Magnitude (Brightess of Host Star) [mag]` = st_optmag, 
         `Optical Magnitude Band` = st_optband, `G-band (Gaia) [mag]` = gaia_gmag, `Host Star Effective Temperature (K)` = st_teff, `Stellar Mass` = st_mass, `Stellar Radius (solar radii)` = st_rad,
          `Transit Depth (percentage)` = pl_trandep, `Transit Duration (days)` = pl_trandur, `Transit Midpoint (Julian Days)` = pl_tranmid, `Year of Discovery` = pl_disc,
         `Discovery Facility` = `pl_facility`, `Link to Exoplanet Encyclopedia` = pl_pelink)
 
plnts <- planets %>% 
  select(which(sapply(.,class) %in% c('integer', 'character')), -`Right Ascension (sexagesimal)`, - `Declination (sexagesimal)`)%>%
  sapply(function(x) as.factor(x))

plnts2 <- planets %>% 
  select( - which(sapply(.,class) %in% c('integer', 'character')), `Right Ascension (sexagesimal)`, `Declination (sexagesimal)`) 

planets <- cbind(plnts, plnts2)

# A little bit of cleanup...

# Name of observatories

planets$`Discovery Facility` <- as.character(planets$`Discovery Facility`)

planets$`Discovery Facility`[planets$`Discovery Facility` == "HATNet"] <- "Fred Lawrence Whipple Observatory (HATNet)"
planets$`Discovery Facility`[planets$`Discovery Facility` == 'HATSouth'] <- "High Energy Stereoscopic System (HATSouth)"
planets$`Discovery Facility`[planets$`Discovery Facility` %in% c("SuperWASP-South", "WASP-South")] <- "South African Astronomical Observatory (SuperWASP-South)"
planets$`Discovery Facility`[planets$`Discovery Facility` == "SuperWASP-North"] <- "SuperWASP-North - Roque de Los Muchachos"
planets$`Discovery Facility`[planets$`Discovery Facility` == "SuperWASP"] <- "SuperWASP - Roque de Los Muchachos "
planets$`Discovery Facility`[planets$`Discovery Facility` == "MOA"] <- "University of Canterbury Mt John Observatory"



######## Exploring Correlation 


num <- planets %>%
  dplyr::select(which(sapply(.,class) == 'numeric'))

y <- cor(num, use = "pairwise.complete.obs")

corrplot(y, type = 'lower')




### Celestial map

# Aitoff projection: Convert declination and right ascension to pair of cartesian coordinates adjusted to aitoff grid.

x <- astrolibR::aitoff(planets$`Right Ascension (decimal degrees)`, planets$`Declination (decimal degrees)`)

#  Transform to sf object

x <- x %>%
  as.data.frame() %>%
  sf::st_as_sf(coords = c(1,2))

st_crs(x) = 4326


x <- st_transform(x, crs = "+proj=moll")

# Plot Celestial Map

ggplot(x, aes()) + geom_sf(col= 'white') + theme_nightsky()


# Read in Milky way data

milky_sf <- st_read("https://raw.githubusercontent.com/ofrohn/d3-celestial/master/data/mw.json", stringsAsFactors = F)  %>% 
  st_cast("MULTILINESTRING") %>%  
  st_cast("LINESTRING") %>%
  group_by(id)%>%  
  st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180")) 




# Stars data

stars_sf <- st_read("https://raw.githubusercontent.com/ofrohn/d3-celestial/master/data/stars.6.json", stringsAsFactors = F)

# Constellations data

constellations_sf <- st_read("https://raw.githubusercontent.com/ofrohn/d3-celestial/master/data/constellations.lines.json", 
                             stringsAsFactors = F)  %>%  
  st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180")) %>% 
  st_cast("MULTILINESTRING")


milky_sf_trans <- st_transform(milky_sf, crs = "+proj=moll")

milky_sf_trans[3:202,]<- milky_sf_trans[3:202,] %>% 
  st_cast("MULTIPOLYGON")

milky_sf_transclosed <- concaveman::concaveman(milky_sf_trans[1:2,])

# Extract the constellation stars with names
stars_con_sf<- stars_sf %>%  
  filter(name!="") %>%  
  filter(con!="")
# Extract the brightest constellation stars with names, with an arbitrary cutoff 0.5 for mag - this extracts the top 10 brightest stars
stars_bright_sf<- stars_con_sf %>% 
  filter(mag<0.5)
# Change the mag to a new scale newmag for the size aesthetic
stars_bright_sf<-stars_bright_sf %>% 
  mutate(newmag=-(mag-1.1)/4)

# Use ggplot to plot the brighest stars by constellation
stars_bright_sf %>% 
  ggplot()+
  # Group the stars by constellation
  geom_sf(aes(size=newmag,fill=con,colour=con))+
  geom_sf_text(aes(label=name), colour="white")+
  theme_nightsky()+
  # In this case add a legend to see the constellations and new magnitudes
  theme(legend.position="right")



####

options(scipen = 999)

planets$`Discovery Facility` <- fct_explicit_na(planets$`Discovery Facility`, na_level = "Other")


discovery_facilities <- planets %>%
  group_by(`Discovery Facility`, .drop = F) %>%
  summarize('Discovered Planets' = n(),
            'Discovery Methods' = n_distinct(`Discovery Method`),
            'Average Orbital Period (days)' = round(mean(`Orbital Period (days)`, na.rm = T),2),
            'Median Orbital Period (days)' = round(median(`Orbital Period (days)`, na.rm = T),2),
            'Average Orbit Semi-Major Axis (AU)' = round(mean(`Orbit Semi-Major Axis (AU)`, na.rm = T),2),
            'Median Orbit Semi-Major Axis (AU)' = round(median(`Orbit Semi-Major Axis (AU)`, na.rm = T),2),
            'Average Inclination (deg)' = round(mean(`Inclination (deg)`, na.rm = T), 2)) 

discovery_facilities$`Discovery Facility` <- as.character(discovery_facilities$`Discovery Facility`)

# Use Google API key
register_google(key = "AIzaSyDk3BpQG_3C2fgWJhgXXtuTRaDBbeB6FXg")

# Add coordinates
facility_coordinates <- ggmap::mutate_geocode(discovery_facilities, `Discovery Facility`)

# Kepler is actually not an observatory facility, but a space telescope. The Data is collected at the .. located on the John blabla. 
#We will use the coordinates of the data management center for Kepler related exoplanet discoveries.

facility_coordinates[,c("lon", "lat")][facility_coordinates$`Discovery Facility` == "Kepler",] <- c(-76.625466,39.3327283)
facility_coordinates[,c("lon", "lat")][facility_coordinates$`Discovery Facility` == "KELT-North",] <- c(-110.6039461,31.6656759)
facility_coordinates[,c("lon", "lat")][facility_coordinates$`Discovery Facility` == "KELT-South",] <- c(18.47,-33.93454)
facility_coordinates[,c("lon", "lat")][facility_coordinates$`Discovery Facility` == "KMTNet",] <- c(-70.80634,-30.16896)
facility_coordinates[,c("lon", "lat")][facility_coordinates$`Discovery Facility` == "MEarth Project",] <- c(-110.952,31.67525)
facility_coordinates[,c("lon", "lat")][facility_coordinates$`Discovery Facility` == "OGLE",] <- c(-70.699,-29.01)
facility_coordinates[,c("lon", "lat")][facility_coordinates$`Discovery Facility` == "Transiting Exoplanet Survey Satellite (TESS)",] <- c(-79.7084365,28.4886723)
facility_coordinates[,c("lon", "lat")][facility_coordinates$`Discovery Facility` == "Xinglong Station",] <- c(117.575,40.3942)


# Facility Websites 

web <- data.frame("Discovery Facility" = facility_coordinates$`Discovery Facility` ,website = c("www.nasa.gov", "https://www.aao.gov.au/about-us/anglo-australian-telescope", "https://www.apo.nmsu.edu/", "https://www.naic.edu/ao/landing", "https://www.kasi.re.kr/eng/pageView/65",
                                                                              "https://www.caha.es/", "http://www.ctio.noao.edu/noao/", "https://sci.esa.int/web/corot", "https://www.eso.org/public/", "https://www.cfa.harvard.edu/flwo", 
                                                                              "https://www.cfa.harvard.edu/flwo", "https://www.gemini.edu", "http://www.obs-hp.fr/ohp.shtml", "https://hatsouth.org/", "https://hubblesite.org/",
                                                                              "www.nasa.gov", "https://keplerscience.arc.nasa.gov/", "https://keltsurvey.org/", "http://www.winer.org/", "https://www.saao.ac.za/", "https://www.nasa.gov/mission_pages/kepler/main/index.html",
                                                                              "https://www.noao.edu/kpno/", "http://kmtnet.kasi.re.kr/kmtnet-eng/", "http://koinet.astro.physik.uni-goettingen.de/", "http://www.lbto.org/", "http://www.lco.cl/", "https://www.eso.org/public/usa/teles-instr/lasilla/",
                                                                              "https://en.wikipedia.org/wiki/Leoncito_Astronomical_Complex", "https://www.ucolick.org/main/index.html", "https://maunakeaobservatories.org/",
                                                                              "https://mcdonaldobservatory.org/", "https://www.cfa.harvard.edu/MEarth/Telescopes.html", "www.nasa.gov",
                                                                              "www.nasa.gov", "http://tdc-www.harvard.edu/oakridge/oakridge/", "http://www.astrouw.edu.pl/", "http://www.oao.nao.ac.jp/en/","http://www.astro.caltech.edu/palomar/homepage.html",
                                                                              "https://www.eso.org/public/usa/teles-instr/paranal-observatory/", "https://www.parkes.atnf.csiro.au/", "http://www.qatarexoplanet.org/",
                                                                              "http://www.iac.es/eno.php?op1=2&lang=en", "https://www.saao.ac.za/", "http://www.spitzer.caltech.edu/", "https://subarutelescope.org/",
                                                                              "http://www.iac.es/eno.php?op1=2&lang=en", "http://www.iac.es/eno.php?op1=2&lang=en", "https://www.volcanoteide.com/en", "http://www.tls-tautenburg.de/TLS/index.php?id=2",
                                                                              "https://www.nasa.gov/tess-transiting-exoplanet-survey-satellite/","https://lowell.edu/", "https://www.ukirt.hawaii.edu/", "http://www.phys.canterbury.ac.nz/moa/", "http://www.keckobservatory.org/",
                                                                              "http://www.xinglong-naoc.org/html/en/", "https://www.ifa.hawaii.edu/haleakalanew/observatories.shtml", "http://english.ynao.cas.cn/"))


web <- select(web, `Discovery Facility` = Discovery.Facility, website)

web$website <- paste0("<a href='",web$website,"'target = '_blank'>",web$`Discovery Facility`,"</a>")


colnames(planets)[colnames(planets) == "Link to Exoplanet Encyclopedia"] <- "urls"
planets$urls <- as.character(planets$urls)
planets$urls[is.na(planets$urls)] <- "https://google.com"


facility_coordinates <- inner_join(facility_coordinates,web, by = "Discovery Facility")
facility_coordinates$`Discovery Facility` <- as.factor(facility_coordinates$`Discovery Facility`)



mp <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng= facility_coordinates$lon, lat= facility_coordinates$lat,
             popup = paste(
               paste(tags$strong("Observatory: "),facility_coordinates$`Discovery Facility`, sep = ""), 
               paste(tags$strong("Number of Discovered Planets: "), facility_coordinates$`Discovered Planets`, sep = ""), 
               paste(tags$strong("Website: "), paste(facility_coordinates$website), sep = ""),sep = '<br/>'))

## Query Builder

df.data <- planets
df.data$name <- row.names(df.data)
df.data$nameFactor <- as.factor(df.data$name)

# TCEs

set.seed(2019)

tce <- read.csv("tce.csv", stringsAsFactors = F) %>%
  filter(av_training_set != "UNK") %>%
  mutate(tce_duration = tce_duration/24) %>%         # Convert hours to days. 
  slice(sample(1:n()))                               # Randomly permute rows.
