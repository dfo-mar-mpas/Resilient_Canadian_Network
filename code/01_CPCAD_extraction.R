#download measures in Canada from the Canadian Protected and Conserved Areas Database - CPCAD

#libraries
library(tidyverse)
library(sf)
library(MarConsNetData)  # https://github.com/dfo-mar-mpas/MarConsNetData/

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

meta_grouped <- c("NAME_E","MECH_E","TYPE_E" ,"OWNER_TYPE","OWNER_E","GOV_TYPE",
                  "MGMT_E","STATUS","ESTYEAR","QUALYEAR","DELISTYEAR","area")

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
                        summarize(geometry = st_union(Shape))%>%
                        left_join(.,cpcad_marine%>%
                                    data.frame()%>%
                                    distinct(NAME_E,.keep_all=TRUE)%>%
                                    select(all_of(meta_grouped)))%>%
                        rbind(.,cpcad_marine%>%
                                filter(!NAME_E %in% cpcad_marine_zones$name)%>%
                                rename(geometry = Shape)%>%
                                select(all_of(meta_grouped)))

write_sf(cpcad_marine_grouped,dsn="data/cpcad/cpcad_areas_marine_nozones.shp")

#Canadian Bioregions and EEZ ---------

can_bioregions <- data_bioregion(bioregion="All")

can_eez <- can_bioregions%>%
           st_make_valid()%>%
           st_union()

st_write(can_bioregions,dsn="data/shapefiles/canadian_bioregions.shp")
st_write(can_eez,dsn="data/shapefiles/can_eez.shp")

