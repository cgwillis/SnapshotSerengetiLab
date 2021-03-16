

library(raster)
library(sf)
library(rgdal)
library(mapview)
library(lattice)
library(leafpop)
library(RColorBrewer)
library(ggplot2)
library(ggridges)
library(tidyverse)
library(plyr)

source('helpers.R')

spatialplotCreate <- function(
  dat,
  Kopjes,
  species_input,
  standing_input,
  resting_input,
  moving_input,
  eating_input,
  interacting_input,
  babies_input,
  habitat_input,
  date_input,
  meta_input
) {
  
  # Assemble camera trap data set
  #subset of larger data set contain the static metadata for each camera trap site
  # Identify and filter unique camera traps and metadata
  # metadata includes: "Camera_Site", "Longitude..m.","Latitude..m.","Habitat","Amount.of.Shade","Distance.to.River..m.","Distance.to.Confluence..m.","Distance.to.Kopje..m.","Tree.Density.Measure","Lion.Risk..Wet.","Lion.Risk..Dry.","Greeness..Wet.""Greeness..Dry.","Camera.Mount"
  datCoor <- dat[!duplicated(dat$Camera_Site),names(dat)[15:28]]
  datCoor= datCoor[complete.cases(datCoor), ]
  
  # Transform camera grid in shape file
  coordinates(datCoor) = ~Longitude_m + Latitude_m
  # Set the projection to EPSG used for the study site
  proj4string(datCoor) <- CRS("+init=epsg:21036")
  
  # Crop the Kopjes shape file to limit it to the extent of the research area
  e <- extent(datCoor)*1.5
  Kopjes.crop <- crop(Kopjes, e)
  
  # Interactive mapping tool
  # primary reference: https://cran.r-project.org/web/packages/mapview/mapview.pdf
  # additional reference: https://geocompr.robinlovelace.net/adv-map.html#interactive-maps
  # map.types at https://leaflet-extras.github.io/leaflet-providers/preview/
  # zcol = point color by variable value
  # cex = point size by variable value
  # color = boarder color of points
  #example list of map layers: c('Stamen.Terrain','OpenTopoMap','Esri.WorldImagery','OpenStreetMap.DE')
  
  # Calculate animal frequency by camera site
  # This would have to been done after apply all additional filters
  # Add in option to filter by two date ranges per species -- would create two datasets per species
  df_sp <- filterSerengetiData(
    dat,
    species_input,
    standing_input,
    resting_input,
    moving_input,
    eating_input,
    interacting_input,
    babies_input,
    habitat_input,
    date_input
  )
  
  if (is.null(species_input)) {
    species_input <- c('cheetah')
  }
  
  df_spA = subset(dat, Species == species_input[1])
  dfA = count(df_spA, c('Species','Camera_Site'))
  dfA= dfA[complete.cases(dfA), ]
  # Merge with frequency data with camera trap spatial data
  dfACoor = merge(datCoor,dfA,by="Camera_Site")
  
  
  df_spB = subset(dat, Species == species_input[2])
  dfB = count(df_spB, c('Species','Camera_Site'))
  dfB= dfB[complete.cases(dfB), ]
  # Merge with frequency data with camera trap spatial data
  dfBCoor = merge(datCoor,dfB,by="Camera_Site")

  
  # Mapping Structure
  
  # Base Map: Uses cropped Kopjes shape file
  #m <- mapview(Kopjes.crop, col.regions='black', alpha.regions=1,
  #             layer.name='Kopjes') +
    # Species A Frequency, Date Range 1: based on filtered data
    m <- mapview(dfACoor,zcol = 'freq',cex='freq', color='black', col.regions=brewer.pal(9, "Reds"),
            alpha.region=1,layer.name=paste(species_input[1],"frequency",sep=" "),
            map.types = c( 'Esri.WorldImagery', 'CartoDB.Positron', 'OpenStreetMap', 'OpenTopoMap')) +
    # Species A Frequency, Date Range 2: based on filtered data
    #mapview(dfACoor2,zcol = 'freq',cex='freq', color='black', col.regions=brewer.pal(9, "Oranges"),
    #alpha.region=1,layer.name=paste(unique(dfCoor$Species)[[2]],"frequency",sep=" ")) +
    # Species B Frequency, Date Range 1: based on filtered data
    mapview(dfBCoor,zcol = 'freq',cex='freq', color='black', col.regions=brewer.pal(9, "Blues"),
            alpha.region=1,layer.name=paste(species_input[2],"frequency",sep=" ")) +
    # Species B Frequency, Date Range 2: based on filtered data
    #mapview(dfACoor2,zcol = 'freq',cex='freq', color='black', col.regions=brewer.pal(9, "PuBu"),
    #alpha.region=1,layer.name=paste(unique(dfCoor$Species)[[2]],"frequency",sep=" ")) +
    # Metadata: students can select one metadata variable to include in their plot. If they don't select one, nothing is ploted here.
    mapview(datCoor,zcol = meta_input,cex=meta_input, color='black',col.regions=brewer.pal(9, "BrBG"))
  
  return(m)
}