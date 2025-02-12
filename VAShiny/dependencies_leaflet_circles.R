

# Valuation Atlas Interactive Map

library(tidyverse)
library(cowplot)
library(sf)
library(sp)
library(readxl)
library(htmlwidgets)
library(shiny)
library(leaflet)
library(leafpop)
library(DT)
library(shinyBS)
library(shinydashboard)

# library(openxlsx)

# data <- readxl::read_excel("Data/IPBES_VA_Uptake_Corpus_06May20_GEONames_TS_16June20.xlsx", sheet = 1)

# Necessary datasets for the upcoming maps
countries <- read.csv("Data/ctrs_coordpts.csv") 

# Get coordinate pts for selection and for circles
coordpts <- countries[, c("Lon","Lat")]

# Keep only names 
# countries <- countries[, c("ISO_Alpha_3","NAME_0")]


processed_data <- read.csv("Data/processed_data_D.csv")

# Combine data
df <- left_join(countries, processed_data, by = "ISO_Alpha_3")

names(df)[c(1,2)] <- c("ISO Code", "Name") 

# Amend dataset for testing purposes
# nameskeep <- "DEU|LIE|CHE|AUT|FJI"

#the countries that cross the dateline + another

# Only keep selected countries
# df <- df[grep(nameskeep, df$ISO_Alpha_3),] # test with these (or other) countries for faster loading times

poly <- df
rm(df)

# drop empty geometries
# poly <- df %>% filter(!st_is_empty(.))

# Reformat to spdf for leaflet 
# poly <- as_Spatial(poly)
# poly <- as_Spatial(df)


# rm(df,data,countries)



# Relevant columns for valuation atlas
# poly <- poly[1:7] # includes geometry column (without as_Spatial(x))
# poly <- poly[1:4] # if class(poly) is spatialpolygonsdataframe # without log

# Change names of columns into what they actually are (may need to be updated when data is changed)
# oldnames <- names(poly)
# indicatornames <- c("ISO Alpha 3", "Country Name", "geometry", "Density of Studies", "Density of Studies (log)", "Density of Organizations", "Density of Organizations (log)")
# names(poly) <- indicatornames
# indicatornames <- c("ISO Alpha 3", "Country Name", "Density of Studies", "Density of Studies (log)", "Density of Organizations", "Density of Organizations (log)", "Ratio of Studies and Organizations")
# indicatornames <- c("ISO Alpha 3", "Country Name", "Density of Studies", "Density of Organizations", "Ratio of Studies and Organizations")
CNI <- c("Density of Studies", "Density of Organizations", "Ratio of Studies and Organizations") # ChoiceNames Indicator
CVI <- c("DoS", "DoO", "Ratio") # ChoiceValues Indicator

CNT <- c("Total", "Pre-1990", "1990-1999", "2000-2009", "2010-2020") # ChoiceNames Temporal Resolution
CVT <- c("Total", "b1990", "b2000", "b2010", "b2020") # ChoiceNames Temporal Resolution


cAttributeNames  <- append(paste(CNI[1],CNT), append(paste(CNI[2],CNT), paste(CNI[3], CNT)))
cAttributeValues <- append(paste0(CVI[1],CVT), append(paste0(CVI[2],CVT), paste0(CVI[3], CVT)))

tcAttributes <- data.frame(Names=cAttributeNames, Values = cAttributeValues)


# choiceValuesTotal <- paste0(iNames, "Total")
# choiceValuesb1990 <- paste0(iNames, "b1990")
# choiceValuesb2000 <- paste0(iNames, "b2000")
# choiceValuesb2010 <- paste0(iNames, "b2010")
# choiceValuesb2020 <- paste0(iNames, "b2020")

# choiceValues <- mget(ls(pattern="choiceValues"))
# names(choiceValues) <- gsub(pattern = "choiceValues", "", names(choiceValues))
# choiceValues <- choiceValues[c(5,1:4)]
# poly <- as_Spatial(poly)

# Create Choice names and values
# choiceNames <- indicatornames[3:length(indicatornames)]
# allAttributes <- names(poly)[3:length(names(poly))]
# names(poly) <- indicatornames # or just change them back like this?   # beware of error message

# Choice names based on choice values
# choiceNames[grep(paste0(input$Indicator,"$"), choiceValues)]

# Get data from poly
# pdata <- as.data.frame(poly[, -c(3,4)])  #-c("Lon","Lat")
pdata <- poly
names(pdata) <- c("ISO Code", "Name", "Lon", "Lat",  cAttributeNames)
names(pdata) <- c("ISO Code", "Name", "Lon", "Lat",  rep(CNI, each=5)) # So it always says e.g. DoS without the time

# Datatable display
polydt <- poly[-c(3,4)]
polydt[3:length(polydt)] <- round(polydt[3:length(polydt)], digits=3)


# Create color palettes for different datasets
{
pal <- list()

#Colors for Density of Studies
# low = "#F7FCB9",
# high = "#006837",
# pal$DoF <- colorRampPalette(colors = c("#F7FCB9", "#006837"))(10)
pal[["Density of Studies"]] <- colorRampPalette(colors = c("#F7FCB9", "#006837"))(10)
# pal[["Density of Studies"]] <- colorBin(palette = c("#F7FCB9", "#006837"), bins=2, domain=NULL)
# pal[["Density of Studies (log)"]] <- colorRampPalette(colors = c("#F7FCB9", "#006837"))(10)

#Colors for Density of Organizations
# low = "#DEEBF7",
# high = "#08519C",
# pal$DoO <- colorRampPalette(colors = c("#DEEBF7", "#08519C"))(10)
pal[["Density of Organizations"]] <- colorRampPalette(colors = c("#DEEBF7", "#08519C"))(10)
# pal[["Density of Organizations (log)"]] <- colorRampPalette(colors = c("#DEEBF7", "#08519C"))(10)

# Colors for ratio (Density of Studies/Density of Organizations)
# low = "#F7FCB9",
# high = "#FB8C00",
# pal$RSO <- colorRampPalette(colors = c("#F7FCB9", "#FB8C00"))(10)
pal[["Ratio of Studies and Organizations"]] <- colorRampPalette(colors = c("#F7FCB9", "#FB8C00"))(10)

# pal <- c(rep(pal[1],5), rep(pal[2],5), rep(pal[3],5))
names(pal) <- CVI
}

# Load Shapefiles for updated borders-----
grey_areas <- read_sf("Data/Data_Final/grey_areas.shp") # area grey, no outlines
# grey_areas <- st_transform(st_wrap_dateline(grey_areas), robin_crs)
# 
# solid_borders <- read_sf("Data/Data_Final/solid_borders.shp") # solid lines, color X
# solid_borders <- st_transform(st_wrap_dateline(solid_borders), robin_crs)
# 
# dashed_borders <- read_sf("Data/Data_Final/dashed_borders.shp") # dashed lines, color X
# dashed_borders <- st_transform(st_wrap_dateline(dashed_borders), robin_crs)
# 
# dotted_borders <- read_sf("Data/Data_Final/dotted_borders.shp") # dotted lines, color X
# dotted_borders <- st_transform(st_wrap_dateline(dotted_borders), robin_crs)
# 
# major_lakes <- read_sf("Data/Data_Final/Major_Lakes.shp") # dotted lines, color X
# major_lakes <- st_transform(st_wrap_dateline(major_lakes), robin_crs)

# Text to put in app
PAGE_TITLE <- "Valuation Atlas Viewer"

#bsPopover information
DoS_info <- "The number of studies is the number of valuation studies conducted within each country or territory for the entire corpus."
DoO_info <- "The number of organizations is the number of studies which reference the particular country within the affiliations, acknowledgements, or funding text for the entire corpus as a proxy for organizations which are conducting the research."
Ratio_info <- "The ratio of studies and organizations is the number of studies divided by the number of organizations."

# Disclaimer
Further_information <- tags$div(#style = "font-size:8px;text-align:justify;position:fixed;bottom:0%;width:14%;", # 14% if column width is 2
                                style = "text-align:justify",
                                
                                HTML("<br><br><br><br>
                                      <b>
                                      This app was created by Ferdinand Wilhelm with support by Joy Kumagai and Aidin Niamir.
                                      <br><br>
                                      DOI:
                                      <br><br>
                                      Version: 1.0
                                      <br><br>
                                      </b>
                                      <br><br><br><br>
                                      <b>
                                      The data behind this visualization are derived from a corpus of ~79,000 publications, gathered and analyzed for the IPEBS Values Assessment (<a href='https://doi.org/10.5281/zenodo.6522522'>Link</a>). 
                                      </b><br><br>
                                      <b>
                                      The number of studies is the number of valuation studies conducted within each country or territory for the entire corpus. 
                                      </b>
                                      There were 64,688 identifications of a country or territory from the title, abstract, or keywords consisting of 217 jurisdictions identified between [earliest date] and [latest date]. 
                                      <br><br>
                                      <b>
                                      The number of organizations is the number of studies which reference the particular country within the affiliations, acknowledgements, or funding text for the entire corpus as a proxy for organizations which are conducting the research. 
                                      </b>
                                      There were 140,188 identifications of a country or territory from the affiliations, acknowledgments, or funding text, consisting of 210 jurisdictions identified between [earliest date] and [latest date].
                                      <br><br>
                                      For more information about the corpus see the IPBES VA Chapter 3. Systematic review on Method Families (<a href='https://doi.org/10.5281/zenodo.4404436'>Link</a>) and IPBES VA Chapter 4. Systematic review on valuation uptake (<a href='https://doi.org/10.5281/zenodo.4391335'>Link</a>).
                                      <br><br>
                                      For more information on how the number of organizations and studies were derived, please review the documentation available on our github <a href='https://jkumagai96.github.io/VA_version2/Valuation_atlas.html'>here</a>. 
                                      <br><br>
                                      <i>
                                      The designations employed and the presentation of material on the maps used in the assessment do not imply the expression of any opinion whatsoever on the part of IPBES concerning the legal status of any country, territory, city or area or of its authorities, or concerning the delimitation of its frontiers or boundaries. These maps have been prepared or used for the sole purpose of facilitating the assessment of the broad biogeographical areas represented therein and for purposes of representing scientific data spatially.
                                      </i>
                                      "))






#leaflet functions needed for map----
# create map object
my_map <- leaflet(poly, options = leafletOptions(minZoom = 2, maxZoom = 7, worldCopyJump = T)) %>%
                  addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(noWrap = F)) %>%
                  addPolygons(data=grey_areas, fillColor = "grey", stroke = FALSE)






