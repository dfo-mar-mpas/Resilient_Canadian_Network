##bathymetry extraction 

library(tidyverse)
library(terra)
library(sf)

# Set terra options for better performance
terraOptions(memfrac = 0.8)

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


