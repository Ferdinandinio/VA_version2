# Valuation Atlas Interactive Map

library(tidyverse)
library(sf)
library(rgeos)
# library(rmapshaper)
# https://gis.stackexchange.com/questions/236340/simplifying-and-plotting-polygons-in-leaflet-package-in-r

countries <- read_sf("Data/gadm36_levels_shp/gadm36_0.shp") %>%     # load simplified data
                     rename(ISO_Alpha_3 = GID_0)

eqcpr <- "+proj=eqc"

countries <- st_transform(countries, eqcpr)

# sf_use_s2() # set to FALSE
# sf_use_s2(FALSE)

#Simplify
# countries1 <- st_simplify(countries, dTolerance=1000, preserveTopology = T) # is there an option to not mess with the dateline?
# countries2 <- st_simplify(countries, dTolerance=1500, preserveTopology = T)
countries3 <- st_simplify(countries, dTolerance=500, preserveTopology = F)
countries3 <- st_simplify(countries3, dTolerance=2000, preserveTopology = T)
# countries3 <- st_simplify(countries, dTolerance=100, preserveTopology = T)
st_is_empty(countries3) # check if empty (not empty at dT=5000)
as.numeric(object.size(countries3)/object.size(countries)) #~8.8% object size  dT=5000

# countries3 <- st_buffer(countries3,0.0) # Does it even do anything in this projection?
countries3 <- st_transform(countries3, 4326)

countries <- countries3 
rm(countries3)

#Edit data
countrydata <- read_sf("Data/GIS Shapefile/Countries_Separated_with_associated_territories.shp") %>%
  rename(ISO_Alpha_3 = ISO3CD) %>% subset(select=-c(OBJECTID,M49Code)) %>% st_drop_geometry()

# dups <- as.data.frame(countrydata[duplicated(countrydata$ISO_Alpha_3) | duplicated(countrydata$ISO_Alpha_3, fromLast = TRUE),])

countrydata[duplicated(countrydata$ISO_Alpha_3) | duplicated(countrydata$ISO_Alpha_3, fromLast = TRUE),] <- NA

countries <- left_join(countries, countrydata, by = "ISO_Alpha_3")

for(i in 1:nrow(countries)){
  if(is.na(countries[[i,"MAP_LABEL"]])==F){
    countries[i,"NAME_0"] <- countries[[i,"MAP_LABEL"]]
  }
}

countries3 <- countries[1:3]
rm(countries)



#try caching it?

#write to file
setwd("Data")
if (dir.exists("cntrs_smpl")==F) {dir.create("cntrs_smpl")}
setwd("cntrs_smpl")
# write_sf(countries1, "countries1.shp")
# write_sf(countries2, "countries2.shp")
write_sf(countries3, "countries3.shp")

setwd("../../")

rm(list=ls())

