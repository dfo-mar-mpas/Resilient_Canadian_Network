#Map of the Laurentian Channel MPA for the Managing Expectations Paper

#code setup -------
#load libraries
library(sf)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(patchwork)
library(viridis)
library(ggrepel)
library(terra)
library(tidyterra)
library(ggspatial)

#map projections
latlong <- "+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
CanProj <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
utm <- "+proj=utm +zone=20 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0"

##download MPA shapes
cpcad_marine <- read_sf("data/cpcad_complete.shp")%>%
  filter(NAME_E != "Laurentian Channel Marine Protected Area",
         ocean == "Atlantic")%>%
  st_transform(CanProj)%>%
  st_make_valid()

#Laurentian Channel ----

      # lc <- read_sf("r:/Science/CESD/HES_MPAGroup/Data/Shapefiles/DFO_MPA_MPO_ZPM_SHP/DFO_MPA_MPO_ZPM.shp")%>%
      #       filter(NAME_E == "Laurentian Channel Marine Protected Area")%>%
      #       st_transform(CanProj)
      # 
      # write_sf(lc,"data/shapefiles/lc_shape.shp")
  lc <- read_sf("data/shapefiles/lc_shape.shp")%>%
        st_transform(CanProj)

#bioregions

bioregions <- read_sf("data/Shapefiles/canadian_bioregions.shp")%>%
              st_transform(CanProj)%>%
              filter(NAME_E %in% c("Scotian Shelf","Newfoundland-Labrador Shelves",
                                   "Gulf of Saint Lawrence"))

can_eez <- read_sf("data/shapefiles/can_eez.shp")%>%st_transform(CanProj)

#basemapes
basemap_can <- ne_states(country = "Canada",returnclass = "sf")%>%
                dplyr::select(name_en,geometry)%>%
                st_as_sf()%>%
                st_union()%>%
                st_transform(latlong)%>%
                st_as_sf()%>%
                mutate(country="Canada")%>%
                st_transform(CanProj)

basemap <- ne_states(country = "Canada",returnclass = "sf")%>%
            dplyr::select(name_en,geometry)%>%
            st_as_sf()%>%
            st_union()%>%
            st_transform(latlong)%>%
            st_as_sf()%>%
            mutate(country="Canada")%>%
            rbind(.,ne_states(country = "United States of America",returnclass = "sf")%>%
                    dplyr::select(name_en,geometry)%>%
                    st_as_sf()%>%
                    st_union()%>%
                    st_transform(latlong)%>%
                    st_as_sf()%>%
                    mutate(country="US"),
                  ne_states(country = "Greenland",returnclass = "sf")%>%
                    dplyr::select(name_en,geometry)%>%
                    st_as_sf()%>%
                    st_union()%>%
                    st_transform(latlong)%>%
                    st_as_sf()%>%
                    mutate(country="Greenland"),
                  ne_states(country = "Iceland",returnclass = "sf")%>%
                    dplyr::select(name_en,geometry)%>%
                    st_as_sf()%>%
                    st_union()%>%
                    st_transform(latlong)%>%
                    st_as_sf()%>%
                    mutate(country="Iceland"))%>%
            st_transform(CanProj)

#Ploting regions

plot_lim_atlantic <- st_bbox(bioregions)

plot_lim_lc <- lc%>%
               st_buffer(100*1000)%>%
               st_bbox()


#bathymetry set up
        # can_gebco <- rast("c:/Users/stanleyr/Documents/Github/CanadaMPAs/data/Bathymetry/GEBCO/gebco_2019_Canada.tif")
        # 
        # lc_bathy <- can_gebco%>%
        #             crop(.,plot_lim_lc%>%st_as_sfc()%>%st_transform(st_crs(can_gebco)))
        
        # atlantic_bathy <- can_gebco%>%
        #                   crop(.,plot_lim_atlantic%>%st_as_sfc()%>%st_transform(st_crs(can_gebco)))
        
        # writeRaster(lc_bathy,filename = "data/lc_bathy.tif",overwrite=TRUE)
        # writeRaster(atlantic_bathy,filename="data/atlantic_bathy.tif",overwrite=TRUE)

lc_bathy <- rast("data/lc_bathy.tif")
atlantic_bathy <- rast("data/atlantic_bathy.tif")

contour_250 <- as.contour(lc_bathy, levels = -250) %>%
  st_as_sf()%>%
  st_transform(CanProj)

contour_250_atlantic <- as.contour(atlantic_bathy, levels = -250) %>%
                        st_as_sf()%>%
                        st_transform(CanProj)


  p1 <- ggplot()+
        geom_sf(data=basemap_can)+
        geom_sf(data=contour_250,linewidth=0.1,col="grey20")+
        geom_sf(data=cpcad_marine,fill="grey80",alpha=0.5)+
        geom_sf(data=lc,fill="grey50",col="black",linewidth=0.5,alpha=0.5)+
        coord_sf(expand=0,xlim=plot_lim_lc[c(1,3)],ylim=plot_lim_lc[c(2,4)])+
        theme_bw()+
        annotation_scale(location="br")

  p2 <- ggplot()+
        geom_sf(data=can_eez,fill=NA)+
        geom_sf(data=basemap)+
        geom_sf(data=contour_250_atlantic,linewidth=0.1,col="grey20")+
        geom_sf(data=cpcad_marine,fill="grey80")+
        geom_sf(data=lc,fill="grey50",col="black")+
        geom_sf(data=plot_lim_lc%>%st_as_sfc(),fill=NA)+
        coord_sf(expand=0,xlim=plot_lim_atlantic[c(1,3)],ylim=plot_lim_atlantic[c(2,4)])+
        theme_bw()+
        theme(axis.text=element_blank(),
              axis.title = element_blank(),
              plot.background = element_blank())
  
  
  combo <- p1 + inset_element(p2, left = 0.53, bottom = 0.55, right = 1.23, top = 1.07)
  
  ggsave("output/lc_plot.png",combo,height=6,width=8,units="in",dpi=300)
