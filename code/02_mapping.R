## make a map of Canadian closures for the 

#load libraries
library(tidyverse)
library(sf)
library(rnaturalearth)
library(patchwork)
library(viridis)
library(ggrepel)
library(MarConsNetData) # https://github.com/dfo-mar-mpas/MarConsNetData/
library(arcpullr)

#map projections
latlong <- "+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
CanProj <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

#Load shapefiles
cpcad_marine <- read_sf("data/cpcad/cpcad_areas_marine.shp")%>%
                st_transform(CanProj)

canada_eez <- read_sf("data/shapefiles/Canada_EEZ.shp")%>%
              st_transform(CanProj)

bioregion <- get_spatial_layer("https://egisp.dfo-mpo.gc.ca/arcgis/rest/services/open_data_donnees_ouvertes/federal_marine_bioregions_bioregions_marines_federales/MapServer/0")
