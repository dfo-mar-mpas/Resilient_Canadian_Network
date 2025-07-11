#download measures in Canada from the Canadian Protected and Conserved Areas Database - CPCAD

#libraries
library(tidyverse)
library(sf)
library(MarConsNetData)  # https://github.com/dfo-mar-mpas/MarConsNetData/
library(arcgisbinding)
library(arcpullr)

sf_use_s2(FALSE)

#url for the CPCAD database
cpcad_url <- "https://data-donnees.az.ec.gc.ca/api/file?path=%2Fspecies%2Fprotectrestore%2Fcanadian-protected-conserved-areas-database%2FDatabases%2FProtectedConservedArea_2023.zip"

# Create directory for extraction
unzip_dir <- "data/cpcad/"
dir.create(unzip_dir, showWarnings = FALSE)

#file path for the
zip_file <- file.path(unzip_dir, "cpcad_areas.zip")

#download the file. Have to mannually reset the hard line download timer limit to 200 seconds to accomodate the download. THis might need
#to be further adjusted based on the download speed. 
options(timeout=200)

download.file(
  cpcad_url,
  destfile = zip_file,
  mode = "wb"
)

# Unzip the file ** note sometimes this doesn't work and you have to unzip mannually
unzip(zip_file, exdir = unzip_dir)

#view the layers
st_layers("data/cpcad/ProtectedConservedArea_2023/ProtectedConservedArea_2023.gdb/")


cpcad_areas <- st_read("data/cpcad/ProtectedConservedArea_2023/ProtectedConservedArea_2023.gdb/",
                  layer = "ProtectedConservedArea_2023")|>
          st_cast("MULTIPOLYGON")|>
          st_make_valid() 

#st_write(cpcad_areas ,dsn = "data/cpcad/cpcad_areas_all.shp")

cpcad_marine <- cpcad_areas%>%
                filter(BIOME == "M")%>%
                select(-c(MPLAN_REF,Shape_Area,NAME_F)) #long variables (number of characters) that aren't captured well when writing to .shp

#update the ThT closure
tht_name <- "Tang.ɢ̲wan — ḥačxʷiqak — Tsig̱is Marine Protected Area"

tht <- read_sf("data/shapefiles/ThT_MPA.shp")%>%st_transform(st_crs(cpcad_marine))

cpcad_marine[cpcad_marine$NAME_E == "Offshore Pacific Seamounts and Vents Closure","Shape"] <- tht$geometry
cpcad_marine[cpcad_marine$NAME_E == "Offshore Pacific Seamounts and Vents Closure","NAME_E"] <- "ThT Marine Protected Area"
cpcad_marine[cpcad_marine$NAME_E == "ThT Marine Protected Area","TYPE_E"] <- "Marine Protected Area"
cpcad_marine[cpcad_marine$NAME_E == "ThT Marine Protected Area","MECH_E"] <- "Oceans Act ( 1996, c. 31 )"

#update the area
cpcad_marine <- cpcad_marine%>%mutate(area=round(as.numeric(st_area(.)/1000/1000),2))

st_write(cpcad_marine,dsn="data/cpcad/cpcad_areas_marine.shp") #some errors 

### remove the zones

meta_grouped <- c("NAME_E","MECH_E","TYPE_E" ,"OWNER_E",
                  "MGMT_E","STATUS","ESTYEAR","area")

cpcad_marine_zones <- cpcad_marine%>%
                      data.frame()%>%
                      pull(NAME_E)%>%
                      table()%>%
                      data.frame()%>%
                      filter(Freq>1)%>%
                      rename(name=1)

cpcad_marine_grouped <- cpcad_marine%>%
                        filter(NAME_E %in% cpcad_marine_zones$name)%>%
                        group_by(NAME_E)%>%
                        summarize(geometry = st_union(geometry))%>%
                        left_join(.,cpcad_marine%>%
                                    data.frame()%>%
                                    distinct(NAME_E,.keep_all=TRUE)%>%
                                    select(all_of(meta_grouped)))%>%
                        rbind(.,cpcad_marine%>%
                                filter(!NAME_E %in% cpcad_marine_zones$name)%>%
                                select(all_of(meta_grouped)))

write_sf(cpcad_marine_grouped,dsn="data/cpcad/cpcad_areas_marine_nozones.shp")

#Canadian Bioregions and EEZ ---------

can_bioregions <- data_bioregion(bioregion="All")%>%
                  st_make_valid()

can_eez <- can_bioregions%>%
           st_union()

st_write(can_bioregions,dsn="data/shapefiles/canadian_bioregions.shp",delete_layer = TRUE)
st_write(can_eez,dsn="data/shapefiles/can_eez.shp",delete_layer = TRUE)


##load in bioregions

#Load in Canadian Bioregions and fix the Scotian SHelf to make it match up for plotting
bioregion_ord <- c("Southern Shelf","Strait of Georgia","Northern Shelf","Offshore Pacific",
                   "Western Arctic","Arctic Archipelago","Arctic Basin","Eastern Arctic","Hudson Bay Complex",
                   "Newfoundland-Labrador Shelves","Gulf of Saint Lawrence","Scotian Shelf")   

bioregion_abbrev <- read.csv("data/region_abbreviations.csv")%>%
  mutate(region = factor(region,levels=bioregion_ord))%>%
  arrange(region)%>%
  mutate(region=as.character(region))

bioregion <- get_spatial_layer("https://egisp.dfo-mpo.gc.ca/arcgis/rest/services/open_data_donnees_ouvertes/federal_marine_bioregions_bioregions_marines_federales/MapServer/0")%>%
  rename(region=NAME_E,
         ocean=OCEAN_E)%>%
  filter(!region %in% c("Great Lakes","Saint-Pierre et Miquelon"))%>%
  mutate(region = factor(region,levels=bioregion_ord))%>%
  dplyr::select(ocean,region)

ocean_df <- bioregion%>%
  st_make_valid()%>%
  group_by(ocean)%>%
  summarise(geometry = st_union(geoms))%>%
  st_make_valid()%>%
  st_transform(CanProj)

#fix the Scotian Shelf 
nl_updated <- st_difference(bioregion%>%
                              filter(region =="Newfoundland-Labrador Shelves")%>%
                              st_make_valid(), scotian_shelf)%>%
              dplyr::select(ocean,region)

gulf_updated <- st_difference(bioregion%>%
                                filter(region =="Gulf of Saint Lawrence")%>%
                                st_make_valid(), scotian_shelf)%>%
              dplyr::select(ocean,region)

bioregion_df <- rbind(nl_updated,
                      gulf_updated,
                      scotian_shelf,
                      bioregion%>%filter(!region %in% c("Newfoundland-Labrador Shelves","Gulf of Saint Lawrence","Scotian Shelf")))%>%
                mutate(region = factor(region,levels=bioregion$region))%>%
                arrange(region)%>%
                st_transform(CanProj)%>%
                st_make_valid()

st_write(bioregion_df,dsn="data/shapefiles/canadian_planning_regions.shp",append=FALSE)

### clean up and assign oceans, bioregion, add in missing Reguges from the west coast. 

#laod in the Pacific bioregion to fill in missing pieces
nsb_path <- "data/shapefiles/NSB/Proposed_MPA_Network_NSB_Jan2023_PUBLIC.gdb/"

nsb_layers <- st_layers(nsb_path)

nsb_network <- st_read(nsb_path, layer = nsb_layers$name)%>%
  st_transform(CanProj)

#Severa; refuges designated in March 2025 missing from the CPCAD
#Banks refuge  https://www.dfo-mpo.gc.ca/oceans/oecm-amcepz/refuges/banks-eng.html
#Gaw Kaahlii https://www.dfo-mpo.gc.ca/oceans/oecm-amcepz/refuges/masset-inlet-embouchure-eng.html
#Xaana Kaahlii https://www.dfo-mpo.gc.ca/oceans/oecm-amcepz/refuges/skidegate-inlet-embouchure-eng.html

#find them by the map and using the Map_Label since for reasons I can't understand the Pacific polygons are devoid of actual names. 

nsb_polygons <- st_cast(nsb_network, "MULTIPOLYGON") |>
  st_make_valid() |>
  st_transform(4326)

leaflet(nsb_polygons) |>
  addTiles() |>
  addPolygons(popup = ~as.character(Map_Label))

#key out the missing ones
banks_refuge <- nsb_network%>%
  filter(Map_Label %in% c(305:307))%>%
  st_union()%>%
  st_as_sf()%>%
  st_transform(4326)%>% #closer to the projection used to calculate area
  mutate(area = round(as.numeric(st_area(.)/1000/1000),2),
         NAME_E = "Banks Marine Refuge",
         ocean = "Pacific",
         type = "Marine Refuge",
         bioregion = "Northern Shelf")%>%
  st_transform(CanProj)%>%
  rename(geometry=x)%>%
  dplyr::select(NAME_E,type,ocean,bioregion,area)

gaw_kaahlii <- nsb_network%>%
  filter(Map_Label %in% c(414,416,411,412,413))%>% #note that 413 shape doesn't match exactly the extent of Aayan Gandee component (smaller)
  st_union()%>%
  st_as_sf()%>%
  st_transform(4326)%>% #closer to the projection used to calculate area
  mutate(area = round(as.numeric(st_area(.)/1000/1000),2),
         NAME_E = "Gaw Kaahlii Marine Refuge",
         ocean = "Pacific",
         type = "Marine Refuge",
         bioregion = "Northern Shelf")%>%
  st_transform(CanProj) %>%
  rename(geometry=x)%>%
  dplyr::select(NAME_E,type,ocean,bioregion,area)

xaanakaahlii <- nsb_network%>%
  filter(Map_Label %in% c(421,423,424))%>% #note the extent of 421 Tigadu gandaay is bigger than the map on the weblink
  st_union()%>%
  st_as_sf()%>%
  st_transform(4326)%>% #closer to the projection used to calculate area
  mutate(area = round(as.numeric(st_area(.)/1000/1000),2),
         NAME_E = "Xaana Kaahlii Marine Refuge",
         ocean = "Pacific",
         type = "Marine Refuge",
         bioregion = "Northern Shelf")%>%
  st_transform(CanProj)%>%
  rename(geometry=x)%>%
  dplyr::select(NAME_E,type,ocean,bioregion,area)


#laod the cpcad and assign an ocean region to each site
cpcad_marine <- read_sf("data/cpcad/cpcad_areas_marine_nozones.shp")%>%
  mutate(type = case_when(TYPE_E == "Marine Protected Area By Ministerial Order" ~ "MPA",
                          TYPE_E == "Marine Protected Area" ~ "MPA",
                          TYPE_E == "Other Effective Area-Based Conservation Measure" ~ "Marine Refuge",
                          TRUE ~ "OECM"),
         type = factor(type,levels=c("MPA","Marine Refuge","OECM")))%>%
  st_transform(CanProj)

#assign them to an bioregion then associate with an ocean  ----
cpcad_marine2 <- cpcad_marine %>%
  mutate(bioregion = sapply(1:nrow(.), function(i) {
    # Find which bioregions the polygon intersects
    intersected_bioregions <- bioregion_df$region[st_intersects(.[i,], bioregion_df, sparse = FALSE)]
    
    # If multiple bioregions, choose the one with the largest intersection
    if (length(intersected_bioregions) > 1) {
      intersect_areas <- sapply(intersected_bioregions, function(bioregion) {
        st_area(st_intersection(.[i,], bioregion_df[bioregion_df$region == bioregion,]))
      })
      intersected_bioregions[which.max(intersect_areas)]
    } else if (length(intersected_bioregions) == 1) {
      intersected_bioregions
    } else {
      NA  # No bioregion found
    }
  }))

# If some areas are not assigned, find nearest bioregion
# First, identify unassigned areas
unassigned_areas <- cpcad_marine2[is.na(cpcad_marine2$bioregion), ]

# Calculate centroids of unassigned areas
unassigned_centroids <- st_centroid(unassigned_areas)

# Calculate distances to bioregion centroids
distance_to_bioregions <- st_distance(unassigned_centroids, st_centroid(bioregion_df))

# Assign to the nearest bioregion
nearest_bioregion_assignments <- apply(distance_to_bioregions, 1, function(x) {
  bioregion_df$region[which.min(x)]
})

# Update the original dataframe
cpcad_marine2$bioregion[is.na(cpcad_marine2$bioregion)] <- nearest_bioregion_assignments

#add in the oceans that assign to each bioregion
cpcad_marine2 <- cpcad_marine2%>%
  left_join(.,bioregion_df%>%
              data.frame()%>%
              rename(bioregion=region)%>%
              dplyr::select(ocean,bioregion))

##Add in missing areas from the Pacific

cpcad_marine_complete <- cpcad_marine2%>%
                         dplyr::select(NAME_E,type,ocean,bioregion,area)%>%
                         rbind(.,banks_refuge,xaanakaahlii,gaw_kaahlii)


st_write(cpcad_marine,dsn="data/shapefiles/cpcad_complete.shp",append=TRUE)
