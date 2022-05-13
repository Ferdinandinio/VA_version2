
library(tidyverse)
library(sf)

# Necessary datasets for the upcoming maps
countries <- read_sf("Data/cntrs_smpl/countries3.shp") %>%     # load simplified data
  rename(ISO_Alpha_3 = GID_0) #%>% st_drop_geometry()

#Get coordinates from country polygons for selection
coordpts <- SpatialPoints(as_Spatial(countries)) %>% as.data.frame()
names(coordpts) <- c("Lon","Lat")

df <- cbind(countries, coordpts) %>% st_drop_geometry()


write.csv(df, file="Data/ctrs_coordpts.csv", row.names = F)
