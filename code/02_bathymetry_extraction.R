##bathymetry extraction 

library(tidyverse)
library(terra)
library(sf)

# Set terra options for better performance
terraOptions(memfrac = 0.8)

load("data/can_network_extract.RData")

# Load and prepare data
can_gebco <- rast("c:/Users/stanleyr/Documents/Github/CanadaMPAs/data/Bathymetry/GEBCO/gebco_2019_Canada.tif")
can_gebco_proj <- st_crs(can_gebco)

can_net_df <- can_mcn_extracts %>%
  mutate(CELLID = 1:n()) %>%
  st_transform(can_gebco_proj)

# Optimization 1: Crop raster to area of interest
bbox_poly <- st_bbox(can_net_df) %>% st_as_sfc() %>% st_as_sf(crs = can_gebco_proj)
can_gebco_cropped <- crop(can_gebco, bbox_poly)

# Optimization 2: Pre-filter for marine values only
can_gebco_marine <- ifel(can_gebco_cropped < 0, can_gebco_cropped, NA)

# Optimization 3: Use optimized extraction
bathy_vals <- terra::extract(can_gebco_marine, can_net_df, fun = mean, na.rm = TRUE)

save(bathy_vals,file="data/bathy_vals.RData")


## 


can_gebco <- rast("c:/Users/stanleyr/Documents/Github/CanadaMPAs/data/Bathymetry/GEBCO/gebco_2019_Canada.tif")
can_gebco_proj <- st_crs(can_gebco)

can_network <- read_sf("data/cpcad_complete.shp")%>%
  st_transform(can_gebco_proj)


# Convert sf object to SpatVector for terra operations
can_network_vect <- vect(can_network)

# Extract values with cells weights
# depth_values <- extract(can_gebco, can_network_vect, weights=TRUE)
# 
# save(depth_values,file="output/gebco_MCN_extract.RData")

load("output/gebco_MCN_extract.RData") #this is big

depth_summary <- depth_values %>%
  # Group by ID (each polygon)
  group_by(ID) %>%
  # Filter for negative values only
  filter(gebco_2019_Canada < 0) %>%
  # Calculate weighted mean
  summarise(
    weighted_mean_depth = weighted.mean(
      gebco_2019_Canada, 
      weight, 
      na.rm = TRUE
    )
  )


depth_values%>%
  rename(depth=gebco_2019_Canada)%>%
  filter(depth<0)%>%
  summarise(mean=weighted.mean(depth,weight))
