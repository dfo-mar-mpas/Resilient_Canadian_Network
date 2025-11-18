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
library(ggspatial)
library(cowplot)
library(terra)

#map projections
latlong <- "+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
CanProj <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

#load functions
source("code/00_functions.R")

#load the bioregion and network extractions (from Boyce et al. 2022- Canada) processed by keen et al
load("data/bioregion_extract.RData")
load("data/can_network_extract.RData")

#load the GEBCO extraction of bathymetry values associated with the MPA gridcells
load("data/bathy_vals.RData")

#Load in Canadian Bioregions and fix the Scotian SHelf to make it match up for plotting
bioregion_ord <- c("Southern Shelf","Strait of Georgia","Northern Shelf","Offshore Pacific",
                   "Western Arctic","Arctic Archipelago","Arctic Basin","Eastern Arctic","Hudson Bay Complex",
                   "Newfoundland-Labrador Shelves","Gulf of Saint Lawrence","Scotian Shelf")   

bioregion_abbrev <- read.csv("data/region_abbreviations.csv")%>%
  mutate(region = factor(region,levels=bioregion_ord))%>%
  arrange(region)%>%
  mutate(region=as.character(region))

#load bioregions                  
bioregion_df <- read_sf("data/shapefiles/canadian_planning_regions.shp")%>% #these are the output from 01_cpcad_extraction
                st_transform(CanProj)%>%
                mutate(region=factor(region,levels=bioregion_ord))
          

ocean_df <- bioregion_df%>%
          st_make_valid()%>%
          group_by(ocean)%>%
          summarise(geometry = st_union(geometry))%>%
          st_make_valid()%>%
          st_transform(CanProj)

#load the cpcad_marine datafile from the 01_CPCAD_extraction code
cpcad_marine <- read_sf("data/cpcad_complete.shp")%>%
              st_transform(CanProj)%>%
              st_make_valid()

#colour palatte for plotting
colour_pal_types <- c("MPA" = "#252A6B", #ocean'ee themed colours
                      "OECM" = "#2AA2BD",
                      "Marine Refuge" = "#84E7DA",
                      "Gap" = "white",
                      "AOI" = "#005E6B",
                      "Draft" = "grey85")

colour_pal_bioregion <- c("Newfoundland-Labrador Shelves" = "#BCE4DF",
                          "Gulf of Saint Lawrence" = "#51AFA5",
                          "Scotian Shelf" = "#005E6B",
                          
                          "Western Arctic"  = "#E3F6FC",
                          "Arctic Archipelago" = "#49E8F2",
                          "Arctic Basin" = "#0087D1",
                          "Eastern Arctic" = "#27ADF5",
                          "Hudson Bay Complex" = "#2B3686",
                          
                          
                          "Southern Shelf" = "#F3DE02",
                          "Strait of Georgia" = "#F78E12",
                          "Northern Shelf"= "#F7680C",
                          "Offshore Pacific"= "#C70007")

mar_network <- data_draft_areas()%>%st_transform(CanProj)

#Canadian eex --- 
canada_eez <- read_sf("data/shapefiles/can_EEZ.shp")%>%
              st_transform(CanProj)


#load basema of Canada
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

plot_region <- canada_eez%>%st_bbox()

canada <- basemap%>%
          filter(country=="Canada")

#Maritimes Draft Conservation network
mar_network <- data_draft_areas()%>%
               filter(SiteName_E != "Northeast Channel Coral Marine Refuge")%>% #this is just overlapped by the Fundian
               st_transform(CanProj)%>%
               mutate(type=case_when(grepl("refuge",tolower(SiteName_E)) ~ "Marine Refuge",
                                     grepl("marine protected",tolower(SiteName_E)) ~ "MPA",
                                     TRUE ~ "Draft"),
                      type=ifelse(type == "Draft" & Classification_E == "Existing site", "OECM",type),
                      type=ifelse(SiteName_E %in% c("Fundian Channel-Browns Bank","Eastern Shore Islands"),"AOI",type),
                      type=factor(type,levels=c("MPA","Marine Refuge","OECM","AOI","Draft")))


ss_bound <- bioregion_df%>%
  filter(region == "Scotian Shelf")%>%
  st_transform(CanProj)%>%
  st_buffer(100*1000)%>%
  st_bbox()

contour_region <- read_sf("data/contour_region.shp")


#create a bathymetry layer based on gebco
# gebco_bathy <- rast("c:/Users/stanleyr/Documents/Github/data/bathymetry/gebco_2023_n86.4537_s40.0402_w-141.003_e-47.7434.tif")
# 
# dem_proj <- st_crs(gebco_bathy) 
# 
# contour_region <- gebco_bathy%>%
#   crop(.,ss_bound%>%
#          st_as_sfc()%>%
#          st_buffer(100*1000)%>% #extend out a bit. 
#          st_bbox()%>%
#          st_as_sfc()%>%
#          st_transform(dem_proj))%>%
#   as.contour(., levels = -225)%>%
#   st_as_sf()%>%
#   st_transform(CanProj)
# 
# write_sf(contour_region,dsn="data/contour_region.shp")

contour_region <- read_sf("data/contour_region.shp")%>%st_transform(CanProj)
contour_bioregion <- contour_region%>%st_intersection(.,bioregion_df%>%filter(region=="Scotian Shelf")%>%st_transform(CanProj))

mar_cpcad <- cpcad_marine%>%
             filter(ocean=="Atlantic",
                    bioregion!="Scotian Shelf",
                    grepl("Marine Protected Area",NAME_E))%>%
             mutate(type=="MPA")


p1 <- ggplot()+
  geom_sf(data=contour_region,linewidth=0.25,col="grey80")+
  geom_sf(data=contour_bioregion,linewidth=0.25,col="grey40")+
  geom_sf(data=bioregion_df%>%filter(region=="Scotian Shelf"),fill=NA)+
  geom_sf(data=basemap)+
  geom_sf(data=basemap%>%filter(country == "Canada"),fill="grey60")+
  geom_sf(data=mar_cpcad,aes(fill=type),alpha=0.5)+
  geom_sf(data=mar_network,aes(fill=type),alpha=0.75,col="black")+
  geom_sf(data=ss_bound%>%st_as_sfc(),fill=NA,linewidth=0.5,linetype=2)+
  theme_bw()+
  #geom_sf(data=mar_network%>%filter(SiteName_E=="Sambro Bank Sponge Marine Refuge"),fill="red")+
  coord_sf(xlim=ss_bound[c(1,3)],ylim=ss_bound[c(2,4)],expand=0)+
  labs(fill="")+
  scale_fill_manual(values=colour_pal_types)+
  annotation_scale(location="br")+
  theme(legend.position="inside",
        legend.position.inside = c(0.9,0.9),
        legend.title = element_blank(),
        legend.background = element_blank())

ggsave("output/maritimes_network.png",p1,height=7,width=7,units="in",dpi=600)

##analysis of CMIP downscaling - McKee et al. 

cmip_df <- read.csv("data/McKee_et_al_data.csv")%>%
           mutate(site=ifelse(site == "Gully MPA","The Gully",site))%>%
           filter(!variable %in% c("MLD","Bspeed"), #ffocus on salinity and temp
                  rcp %in% c(4.5,8.5))%>% #intermediate carbon scenario and 8.5 (business as usual)
            pivot_longer(
              cols = starts_with(c("p1_", "p2_", "p3_", "p4_")),
              names_to = c("period", ".value"),
              names_pattern = "p(.)_(mean|var)"
            ) %>%
            mutate(
              rcp=paste("RCP",rcp,sep=" "),
              rcp=factor(rcp,levels=c("RCP 4.5","RCP 8.5")),
              period = paste0("Period ", period),
              period = factor(period, levels = paste0("Period ",1:4)),
              time_period = case_when(period == "Period 1" ~ "2020-2039",
                                      period == "Period 2" ~ "2040-2059",
                                      period == "Period 3" ~ "2060-2079",
                                      period == "Period 4" ~ "2080-2099"),
              #time_period = factor(time_period, levels=c("2020-2039","2040-2059","2060-2079","2080-2099")),
              variable = factor(variable, levels=c("SST","BT","SSS","BS"))
            )%>%
            filter(time_period %in% c("2040-2059","2080-2099"))


temp_data <- cmip_df %>%
  filter(variable %in% c("SST", "BT")) %>%
  group_by(site) %>%
  # Calculate maximum SST change for ordering
  mutate(max_sst_change = max(abs(mean[variable == "SST"]))) %>%
  ungroup() %>%
  mutate(site = factor(site, 
                  levels = rev(names(sort(tapply(.$max_sst_change, .$site, max), decreasing = FALSE)))),
         time_period=factor(time_period,levels=rev(c("2040-2059","2080-2099"))),
         rcp = factor(rcp,levels=c("RCP 8.5","RCP 4.5")))%>%
  data.frame()

temp_plot_bars <- ggplot(temp_data%>%filter(variable=="SST"),
       aes(x=time_period,y=mean,fill=mean,group=interaction(time_period,rcp)))+
  geom_bar(stat = "identity", 
           col="black",
           position = position_dodge(width = 0.8),
           width = 0.8)+
  geom_errorbar(
    aes(ymin = mean - sqrt(var), 
        ymax = mean + sqrt(var)),
    position = position_dodge(width = 0.8),
    width = 0.25
  ) +
  facet_grid(site~., 
             space = "free_y",
             scales = "free_y")+
  labs(
    x = "Time period",
    y = expression(paste("Mean ", Delta, "T (°C)"," ± sd"))
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill="white"),
    #axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    panel.spacing = unit(0.1, "lines")
  ) +
  coord_flip()+
  scale_fill_gradient(
    low = "#FED976",   # Light Orange
    high = "#800026"   # Very Dark Red
  ) +
  scale_y_continuous(expand = c(0, 0, 0.1, 0))  # Added expansion to top of y-axis

ggsave("output/temp_plot_atlantic_bars.png",temp_plot_bars,height=8,width=6,units="in",dpi=300)

#Coverage analysis  ------

ocean_area <- ocean_df%>%
              st_transform(4326)%>% #this is the projection used in CPCAD calculations
              group_by(ocean)%>%
              summarise(ocean_area=as.numeric(st_area(geometry))/1000/1000)%>%
              data.frame()%>%
              dplyr::select(ocean,ocean_area)

bioregion_area <- bioregion_df%>%
                  st_transform(4326)%>%#this is the projection used in CPCAD calculations
                  mutate(bioregion_area=as.numeric(st_area(geometry))/1000/1000)%>%
                  data.frame()%>%
                  rename(bioregion=region)%>%
                  dplyr::select(bioregion,ocean,bioregion_area)

cpcad_marine_df <- cpcad_marine%>%
                   data.frame()%>%
                   dplyr::select(-geometry)%>%
                   left_join(.,ocean_area)%>%
                   left_join(.,bioregion_area)
  
#area calcuations

area_df_bioregion <- cpcad_marine_df%>%
                      group_by(bioregion,type)%>%
                      summarise(total_count = n(),
                                total_area = sum(area),
                                prop_area = total_area/unique(bioregion_area))%>%
                      ungroup()%>%
                      left_join(.,bioregion_area)%>%
                      mutate(type=factor(type,levels=c("MPA","Marine Refuge","OECM")),
                             bioregion=factor(bioregion,levels=bioregion_ord),
                             ocean=factor(ocean,levels=c("Pacific","Arctic","Atlantic")))%>%
                             mutate(target_gap = NA)


bioregion_gap <- area_df_bioregion%>%
         group_by(bioregion)%>%
         summarise(
         prop_area = sum(prop_area),
         total_count = sum(total_count),
         total_area = sum(total_area),
         bioregion_area = unique(bioregion_area),
         target_area = bioregion_area * 0.3,
         target_gap = round(target_area - total_area,1),
         target_gap = ifelse(target_gap<0,0,target_gap),
         prop_area = 0.3 - prop_area,
         prop_area = ifelse(prop_area<0,NA,prop_area)
         )%>%
         ungroup()%>%
         mutate(type="Gap")%>%
         left_join(.,area_df_bioregion%>%dplyr::select(bioregion,ocean)%>%distinct(bioregion,.keep_all=T))%>%
         dplyr::select(names(area_df_bioregion))%>%
         data.frame()
         
bioregion_gap%>%filter(target_gap>0)%>%pull(target_gap)%>%median()

area_df_bioregion2 <- area_df_bioregion%>%
                      rbind(.,bioregion_gap)%>%
                      mutate(type=factor(type,levels=c("Gap","MPA","Marine Refuge","OECM")),
                             bioregion=factor(bioregion,levels=bioregion_ord),
                             ocean=factor(ocean,levels=c("Pacific","Arctic","Atlantic")))%>%
                      arrange(ocean,bioregion,type)%>%
                      data.frame()
         

area_df_ocean <- cpcad_marine_df%>%
  group_by(ocean,type)%>%
  summarise(total_count = n(),
            total_area = sum(area),
            prop_area = total_area/unique(ocean_area))%>%
  ungroup()%>%
  left_join(.,ocean_area)%>%
  mutate(type=factor(type,levels=c("MPA","Marine Refuge","OECM")),
         ocean=factor(ocean,levels=c("Pacific","Arctic","Atlantic")))%>%
  arrange(ocean)%>%
  mutate(target_gap = NA,
         bioregion_area = ocean_area)

#add in the ocean totals -
ocean_gap <- area_df_ocean%>%
  group_by(ocean)%>%
  summarise(
    prop_area = sum(prop_area),
    total_count = sum(total_count),
    total_area = sum(total_area),
    ocean_area = unique(ocean_area),
    target_area = ocean_area * 0.3,
    target_gap = round(target_area - total_area,1),
    target_gap = ifelse(target_gap<0,0,target_gap),
    prop_area = 0.3 - prop_area,
    prop_area = ifelse(prop_area<0,NA,prop_area)
  )%>%
  ungroup()%>%
  mutate(type="Gap",
         bioregion_area=ocean_area)%>%
  data.frame()%>%
  dplyr::select(names(area_df_ocean))


area_df_ocean2 <- area_df_ocean%>%
  rbind(.,ocean_gap)%>%
  mutate(type=factor(type,levels=c("Gap","MPA","Marine Refuge","OECM")),
         ocean=factor(ocean,levels=c("Pacific","Arctic","Atlantic")),
         bioregion=paste(ocean,"Region",sep=" "))%>%
  arrange(ocean,type)%>%
  data.frame()%>%
  dplyr::select(names(area_df_bioregion2))


#combine
area_df_bioregion3 <- area_df_bioregion2%>%
  mutate(bioregion=gsub("Newfoundland-Labrador","NL",bioregion),
         bioregion=factor(bioregion,levels=gsub("Newfoundland-Labrador","NL",bioregion_ord)))%>%
                      rbind(.,area_df_ocean2)

##Area Coverage plots -------

coverage_plot <- ggplot(area_df_bioregion3, aes(x = bioregion, y = prop_area, fill = type)) +
  
  # Existing stacked bar plot for conservation types
  geom_bar(
    data = area_df_bioregion3,
    aes(fill=type),
    stat = "identity", 
    position = "stack", 
    width = 0.7, 
    col = "black"
  ) +
  
  geom_bar(data=data.frame(ocean=c("Atlantic","Arctic","Pacific"),
                           bioregion=c("Atlantic Region","Arctic Region","Pacific Region"))%>%
                mutate(ocean = factor(ocean,levels=c("Pacific","Arctic","Atlantic")),
                       prop_area = c(0.3,0.3,area_df_ocean%>%filter(ocean=="Pacific")%>%pull(prop_area)%>%sum())),
           aes(x=bioregion,y=prop_area),
           fill=NA,
           col="black",
           linewidth=1.1,
           stat = "identity",
           position = "stack",
           width = 0.7) +
                
  
  # Text labels for conservation types
  geom_text(
    data = area_df_bioregion3 %>% filter(type != "Gap", prop_area > 0.01),
    aes(label = sprintf("%.1f%%", prop_area * 100)),
    position = position_stack(vjust = 0.5),
    size = 3,
    col = "grey50",
    fontface = "bold"
  ) +
  
  # 30% target line
  geom_hline(yintercept = 0.3, linewidth=1,col="grey60",linetype=2) +
  
  # Gap labels
  geom_text(
    data = area_df_bioregion3 %>% 
      group_by(bioregion) %>%
      mutate(
        non_gap_total = sum(prop_area[type != "Gap"]),
        label_position = non_gap_total + prop_area[type == "Gap"]/2
      ) %>%
      filter(type == "Gap" & prop_area > 0),
    aes(y = label_position, label = sprintf("%skm²", format(target_gap, big.mark = ","))),
    size = 2.5,
    col = "black",
    fontface = "bold",
    angle = 90,
    hjust = 0.5
  ) +
  
 
  
  # Y-axis settings
  scale_y_continuous(
    labels = scales::percent_format(), 
    expand = expansion(mult = c(0, 0.2))  # Extra space for gap labels
  ) +
  
  # Facet by ocean
  facet_wrap(~ocean, ncol = 3, scales = "free_x") +
  
  # Color palette
  scale_fill_manual(values = colour_pal_types) +
  
  # Labels
  labs(
    x = "",
    y = "% bioregion area coverage",
    fill = ""
  ) +
  
  # Theme
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "white"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "inside",
    legend.position.inside = c(0.93,0.93),
    legend.background = element_blank()
  )

ggsave("output/Canada_MCN_Coverage_2025.png",coverage_plot,height=8,width = 10)


#make plots
area_plot_ocean <- ggplot(area_df_ocean, aes(x = ocean, y = prop_area, fill = type)) +
              geom_bar(stat = "identity", position = "stack", width = 0.7, col = "black") +
              geom_text(
                data = area_df_ocean %>% filter(prop_area > 0.01, type != "MPA"),
                aes(label = sprintf("%.1f%%", prop_area * 100)),
                position = position_stack(vjust = 0.5),
                size = 4,
                col = "grey30",
                fontface = "bold"
              ) +
              geom_text(
                data = area_df_ocean %>% filter(prop_area > 0.01, type == "MPA"),
                aes(label = sprintf("%.1f%%", prop_area * 100)),
                position = position_stack(vjust = 1.1),  # Adjust this value for MPA
                size = 4,
                col = "grey90",
                fontface = "bold"
              ) +
              geom_hline(yintercept = 0.3, linetype = 2) +
              scale_y_continuous(
                labels = scales::percent_format(), 
                expand = expansion(mult = c(0, 0.1))  # Add some extra space at the top
              ) +
              scale_fill_manual(values = colour_pal_types) +
              labs(
                x = "",
                y = "% ocean area coverage",
                fill = ""
              ) +
              theme_bw() +
              theme(
                panel.grid.major.x = element_blank(),
                panel.grid.minor.y = element_blank(),
                axis.text.x = element_text(angle = 45, hjust = 1)
              )

area_plot_bioregion <- ggplot(area_df_bioregion, aes(x = bioregion, y = prop_area, fill = type)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7, col = "black") +
  geom_text(
    data = area_df_bioregion %>% filter(prop_area > 0.01),
    aes(label = sprintf("%.1f%%", prop_area * 100)),
    position = position_stack(vjust = 0.5),
    size = 3,  # Reduced text size
    col = "grey50",  # Consistent grey color
    fontface = "bold"
  ) +
  geom_hline(yintercept = 0.3, linetype = 2) +
  scale_y_continuous(
    labels = scales::percent_format(), 
    expand = expansion(mult = c(0, 0.1))  # Add some extra space at the top
  ) +
  facet_wrap(~ocean, ncol=3, scales="free_x") +
  scale_fill_manual(values = colour_pal_types) +
  labs(
    x = "",
    y = "% bioregion area coverage",
    fill = ""
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill="white"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

## Canada Starburst -------
canada_area <- sum(ocean_area$ocean_area)
canada_target <- canada_area*0.3

bioregion_area%>%
  group_by(ocean)%>%
  summarise(total=sum(bioregion_area))%>%
  ungroup()

bioregion_coverage <- cpcad_marine%>%
  data.frame()%>%
  group_by(bioregion)%>%
  summarise(area=sum(area))%>%
  ungroup()%>%
  data.frame()%>%
  left_join(bioregion_area)%>%
  mutate(target=bioregion_area*0.3,
         gap=target-area,
         gap=ifelse(gap<0,0,abs(gap)),
         bioregion = factor(bioregion,levels=bioregion_ord))%>%
  arrange(bioregion)

ocean_coverage <- bioregion_coverage%>%
  group_by(ocean)%>%
  summarise(coverage=sum(area),
            target=sum(target),
            gap = target - coverage)%>%
  ungroup()%>%
  mutate(ocean=factor(ocean,levels=c("Pacific","Arctic","Atlantic")))%>%
  arrange(ocean)


#example starburst code from https://plotly.com/r/sunburst-charts/ ------------
plot_ly(
  labels = c("Eve", "Cain", "Seth", "Enos", "Noam", "Abel", "Awan", "Enoch", "Azura","Ryan"),
  parents = c("", "Eve", "Eve", "Seth", "Seth", "Eve", "Eve", "Awan", "Eve","Enos"),
  values = c(10, 14, 12, 10, 2, 6, 6, 4, 4,2),
  type = 'sunburst'
)


plot_ly(
  labels=c("National Target",
           "2025 Coverage","2025 Gap",
           "Pacific","Arctic","Atlantic",
           as.character(bioregion_coverage$bioregion)),
  parents=c("",
            "National Target","National Target",
            rep("2025 Coverage",3),
            bioregion_coverage$ocean),
  values=c("",
           sum(bioregion_coverage$area),canada_target-sum(bioregion_coverage$area),
           ocean_coverage$coverage,
           bioregion_coverage$area),
  type="sunburst",
  branchvalues="total"
)


plot_ly(
  labels=c("National Target",
           
           "Pacific","Arctic","Atlantic",
           as.character(bioregion_coverage$bioregion)),
  parents=c("",
            
            rep("2025 Coverage",3),
            bioregion_coverage$ocean),
  values=c("",
           
           ocean_coverage$coverage,
           bioregion_coverage$area),
  type="sunburst",
  branchvalues="total"
)





##map of Canadian MPAs  --------------
#Scotian Shelf Bioregion map
ss_bound <- bioregion_df%>%
  filter(region == "Scotian Shelf")%>%
  st_transform(CanProj)%>%
  st_buffer(100*1000)%>%
  st_bbox()

#bounding boxes for ocean plots
atlantic_box <- oceans%>%
  filter(ocean=="Atlantic")%>%
  st_transform(CanProj)%>%
  st_buffer(50*1000)%>%
  st_bbox()

pacific_box <- oceans%>%
  filter(ocean=="Pacific")%>%
  st_transform(CanProj)%>%
  st_buffer(50*1000)%>%
  st_bbox()

arctic_box <- oceans%>%
  filter(ocean=="Arctic")%>%
  st_transform(CanProj)%>%
  st_buffer(20*1000)%>%
  st_bbox()

  p1 <- ggplot()+
        geom_sf(data=bioregion_df,fill=NA)+
        geom_sf(data=basemap)+
        geom_sf(data=basemap%>%filter(country == "Canada"),fill="grey60")+
        geom_sf(data=canada_eez,fill=NA)+
        geom_sf(data=cpcad_marine,aes(fill=type))+
        geom_sf(data=ss_bound%>%st_as_sfc(),fill=NA,linewidth=0.5,linetype=2)+
        theme_bw()+
        scale_fill_manual(values = colour_pal_types)+
        coord_sf(xlim=plot_region[c(1,3)],ylim=plot_region[c(2,4)],expand=0)+
        labs(fill="Closure type")
  

  p2_ss <- ggplot()+
        geom_sf(data=bioregion_df,fill=NA)+
        geom_sf(data=basemap)+
        geom_sf(data=basemap%>%filter(country == "Canada"),fill="grey60")+
        geom_sf(data=canada_eez,fill=NA)+
        geom_sf(data=cpcad_marine,aes(fill=type))+
        geom_sf(data=mar_network%>%
                     filter(Classification_E == "Areas of Interest (AOI)")%>%
                     mutate(type="MPA"),aes(fill=type),alpha=0.6,linetype=2,linewidth=1.2)+
        geom_sf(data=mar_network%>%
                  filter(!Classification_E %in% c("Existing site","Areas of Interest (AOI)")),
                col="grey50",fill="white")+
        theme_bw()+
        scale_fill_manual(values = colour_pal_types)+
        coord_sf(xlim=ss_bound[c(1,3)],ylim=ss_bound[c(2,4)],expand=0)+
        labs(fill="Closure type")+
        theme(legend.position = "none")+
        annotation_scale()
  
  p_atlantic <- ggplot()+
                geom_sf(data=bioregion_df,fill=NA)+
                geom_sf(data=basemap)+
                geom_sf(data=basemap%>%filter(country == "Canada"),fill="grey60")+
                geom_sf(data=canada_eez,fill=NA)+
                geom_sf(data=cpcad_marine%>%filter(ocean=="Atlantic"),aes(fill=type))+
                theme_bw()+
                scale_fill_manual(values = colour_pal_types)+
                coord_sf(xlim=atlantic_box[c(1,3)],ylim=atlantic_box[c(2,4)],expand=0)+
                theme(legend.position = "none")+
                annotation_scale()
  
  p_arctic <- ggplot()+
              geom_sf(data=bioregion_df,fill=NA)+
              geom_sf(data=basemap)+
              geom_sf(data=basemap%>%filter(country == "Canada"),fill="grey60")+
              geom_sf(data=canada_eez,fill=NA)+
              geom_sf(data=cpcad_marine%>%filter(ocean=="Arctic"),aes(fill=type))+
              theme_bw()+
              scale_fill_manual(values = colour_pal_types)+
              coord_sf(xlim=arctic_box[c(1,3)],ylim=arctic_box[c(2,4)],expand=0)+
              theme(legend.position = "none")+
              annotation_scale()
  
  p_pacific <- ggplot()+
                geom_sf(data=bioregion_df,fill=NA)+
                geom_sf(data=basemap)+
                geom_sf(data=basemap%>%filter(country == "Canada"),fill="grey60")+
                geom_sf(data=canada_eez,fill=NA)+
                geom_sf(data=cpcad_marine%>%filter(ocean=="Pacific"),aes(fill=type))+
                theme_bw()+
                scale_fill_manual(values = colour_pal_types)+
                coord_sf(xlim=pacific_box[c(1,3)],ylim=pacific_box[c(2,4)],expand=0)+
                annotation_scale()
  
  p_canada <- ggplot()+
              geom_sf(data=bioregion_df,aes(fill=region))+
              geom_sf(data=basemap)+
              geom_sf(data=basemap%>%filter(country == "Canada"),fill="grey60")+
              geom_sf(data=canada_eez,fill=NA)+
              geom_sf(data=cpcad_marine,fill=NA,col="black")+
              theme_bw()+
              scale_fill_manual(values=colour_pal_bioregion)+
              coord_sf(xlim=plot_region[c(1,3)],ylim=plot_region[c(2,4)],expand=0)+
              labs(fill="")+
              theme(legend.title = element_blank())
  
  #grab a legend for the closure types
  legend_p_mpas <- cowplot::get_legend(p_pacific + 
                                          theme(legend.position = "right",
                                          legend.title = element_blank()))
  
  p_pacific <- p_pacific + theme(legend.position = "none")
                                
  p_legend <- ggplot() +
    annotation_custom(grob = legend_p_mpas) +
    theme_void()
  # 
  # combo_plot <- (p_canada)/(p_arctic + p_pacific + p_atlantic) #doesn't render right 
  # ggsave("output/combo_map.png",combo_plot,width=10,height=10,units="in",dpi=300)
  
  combo_regions <- p_pacific + p_arctic +  p_atlantic
  ggsave("output/combo_regions.png",combo_regions,width=10,height=5,units="in",dpi=300)
  ggsave("output/canada_mcn.png",p_canada,width=10,height=5,units="in",dpi=300)
  ggsave("output/mcn_lengend.png",p_legend,width=2,height=2,units="in",dpi=300)
  

  
p2 <- ggplot()+
  geom_sf(data=basemap)+
  geom_sf(data=basemap%>%filter(country == "Canada"),fill="grey60")+
  geom_sf(data=eez,fill=NA)+
  geom_sf(data=CCA_simple,fill="grey90")+
  geom_sf(data=CCA_simple%>%filter(type!="Marine Protected Area"),fill="blue")+
  theme_bw()+
  coord_sf(xlim=plot_region[c(1,3)],ylim=plot_region[c(2,4)])+
  labs(title = "Canadian Federal OECMs")

p3 <- p1 + p2 + plot_layout(ncol=2)

#Canada coastal - will define coastal 
  #https://www150.statcan.gc.ca/n1/pub/38-20-0001/2021001/m01-eng.htm - 'coastal' is 50m or shallower
  #Ishore - 5 nm from the coast, "nearshore up to 50 nm https://rusi-ns.ca/coastal-zones-and-ccg-vessel-capabilities/ 

#DATA SET UP ----------------------------

#process the CMIP extracts
bioregion_df <- bioregion_extract%>%
                filter(!is.na(vuln))%>%
                data.frame()%>%
                mutate(depth_cat = case_when(bathy < (-200) ~ "Deep",
                                             bathy < (-50) & bathy > (-200) ~ "Shelf", #or epipelagic
                                             TRUE ~ "Coastal"),
                       vuln_cat = case_when(vuln>0.25 & vuln<0.45 ~ "Moderate",
                                            vuln>=0.45 & vuln<0.65 ~ "High",
                                            vuln>=0.65 ~ "Critcal",
                                            TRUE ~ "Negligible"))%>%
                group_by(rcp,region,depth_cat,vuln_cat)%>%
                summarise(area=sum(cell_area,is.na=TRUE))%>%
                ungroup()%>%
                data.frame()%>%
                left_join(.,bioregion_extract%>%data.frame()%>%dplyr::select(region,ocean)%>%distinct(region,.keep_all=TRUE))%>%
                mutate(ocean=factor(ocean,levels=c("Atlantic","Arctic","Pacific")),
                       vuln_cat= factor(vuln_cat,levels=c("Negligible","Moderate","High")),
                       depth_cat = factor(depth_cat,levels=rev(c("Coastal","Shelf","Deep"))))

can_net_df <- can_mcn_extracts%>%
              data.frame()%>%
              mutate(bathy_old=bathy,
                     bathy=bathy_vals$gebco_2019_Canada)%>% #this lines up with the code from 02_bathymetry_extraction.R
              filter(!is.na(vuln))

can_net_summary <- can_net_df%>%
                   group_by(region)%>%
                   summarize(n_points = n(),
                          
                          mean_vuln = mean(vuln,na.rm=T),
                          sd_vuln = sd(vuln, na.rm = TRUE),
                          se_vuln = sd_vuln / sqrt(n_points),
                          
                          mean_depth = mean(bathy,na.rm=T),
                          sd_depth = sd(bathy,na.rm=T),
                          se_depth = sd_depth/sqrt(n_points))%>%
                   ungroup()%>%
                   data.frame()%>%
                   left_join(bioregion_abbrev)%>%
                   mutate(ab_region =factor(ab_region,levels=bioregion_abbrev$ab_region))

ggplot(can_net_summary,aes(x=mean_depth,y=mean_vuln))+
  geom_errorbar(aes(ymin=mean_vuln - (1.96*se_vuln),
                    ymax=mean_vuln + (1.96*se_vuln)),width=0.2)+
  geom_errorbarh(aes(xmin=mean_depth - (1.96*se_depth),
                     xmax=mean_depth + (1.96*se_depth)),height=0.001)+
  geom_point()+
  theme_bw()

### TILE PLOT ---------------------------

# Prepare the ordered data (same as before)
region_order <- bioregion_df %>%
  distinct(ocean, region) %>%
  left_join(
    bioregion_df %>%
      filter(vuln_cat == "High") %>%
      group_by(ocean, region) %>%
      summarise(high_area = sum(area, na.rm = TRUE), .groups = "drop"),
    by = c("ocean", "region")
  ) %>%
  mutate(high_area = ifelse(is.na(high_area), 0, high_area)) %>%
  arrange(ocean, desc(high_area)) %>%
  pull(region)

bioregion_df_ordered <- bioregion_df %>%
  mutate(
    region = factor(region, levels = region_order),
    vuln_cat = factor(vuln_cat, levels = c("Negligible", "Moderate", "High")),
    depth_cat = factor(depth_cat, levels = c("Coastal", "Shelf", "Deep")),
    id=paste0(rcp,ocean,region,depth_cat,vuln_cat)
  )

complete_grid <- expand_grid(
  rcp = unique(bioregion_df$rcp),
  ocean = unique(bioregion_df$ocean),
  region = unique(bioregion_df$region),
  depth_cat = unique(bioregion_df$depth_cat),
  vuln_cat = unique(bioregion_df$vuln_cat)
)%>%
  mutate(area = NA,
         id=paste0(rcp,ocean,region,depth_cat,vuln_cat))%>%
  dplyr::select(names(bioregion_df_ordered))%>%
  filter(id %in% setdiff(id,bioregion_df_ordered$id))

plot_dat <- rbind(bioregion_df_ordered,complete_grid)

# OPTION 1: Depth as additional faceting (my recommendation)
plot_heatmap_depth_facet <- bioregion_df_ordered %>%
  group_by(rcp, ocean, region, depth_cat, vuln_cat) %>%
  summarise(total_area = sum(area, na.rm = TRUE), .groups = "drop") %>%
  group_by(rcp, ocean, region, depth_cat) %>%
  mutate(prop_area = total_area / sum(total_area)) %>%
  ggplot(aes(x = vuln_cat, y = region, fill = prop_area)) +
  geom_tile(color = "black", size = 0.5) +
  scale_fill_viridis_c(name = "Proportion\nof Area", labels = percent_format(),option = "C") +
  facet_grid(ocean ~ rcp + depth_cat, scales = "free_y", space = "free_y") +
  labs(
    title = "Vulnerability Patterns by Region and Depth (Faceted)",
    x = "Vulnerability Category",
    y = ""
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    strip.text = element_text(face = "bold", size = 9),
    strip.background = element_rect(fill="white")
  )

fill_lims <- bioregion_df_ordered %>%
  group_by(rcp, ocean, region, depth_cat, vuln_cat) %>%
  summarise(total_area = sum(area, na.rm = TRUE), .groups = "drop") %>%
  group_by(rcp, ocean, region, depth_cat) %>%
  mutate(prop_area = total_area / sum(total_area))%>%
  pull(prop_area)%>%
  range()


plot_heatmap_depth_26 <- bioregion_df_ordered %>%
  filter(rcp=="2.6")%>%
  group_by(rcp, ocean, region, depth_cat, vuln_cat) %>%
  summarise(total_area = sum(area, na.rm = TRUE), .groups = "drop") %>%
  group_by(rcp, ocean, region, depth_cat) %>%
  mutate(prop_area = total_area / sum(total_area)) %>%
  ggplot(aes(x = vuln_cat, y = region, fill = prop_area)) +
  geom_tile(color = "black", size = 0.5) +
  scale_fill_viridis_c(name = "Proportion\nof Area", 
                       labels = percent_format(),
                       option = "C",
                       limits=fill_lims) +
  facet_grid(ocean ~ depth_cat, scales = "free_y", space = "free_y") +
  labs(
    title = "RCP 2.6",
    x = "Vulnerability Category",
    y = ""
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    strip.text.y = element_blank(),
    strip.text.x = element_text(face = "bold", size = 9),
    strip.background.y  = element_blank(),
    strip.background.x = element_rect(fill="white"),
    legend.position = "none",
    panel.grid = element_blank()
  )

plot_heatmap_depth_85 <- bioregion_df_ordered %>%
  filter(rcp=="8.5")%>%
  group_by(rcp, ocean, region, depth_cat, vuln_cat) %>%
  summarise(total_area = sum(area, na.rm = TRUE), .groups = "drop") %>%
  group_by(rcp, ocean, region, depth_cat) %>%
  mutate(prop_area = total_area / sum(total_area)) %>%
  ggplot(aes(x = vuln_cat, y = region, fill = prop_area)) +
  geom_tile(color = "black", size = 0.5) +
  scale_x_discrete(limits=c("Negligible", "Moderate", "High"))+
  scale_fill_viridis_c(name = "Proportion\nof Area", 
                       labels = percent_format(),
                       option = "C",
                       limits=fill_lims) +
  facet_grid(ocean ~ depth_cat, scales = "free_y", space = "free_y") +
  labs(
    title = "RCP 8.5",
    x = "Vulnerability Category",
    y = ""
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold", size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_blank(),
    strip.background = element_rect(fill="white"),
    panel.grid = element_blank()
  )

combo_plot <- plot_heatmap_depth_26 + plot_heatmap_depth_85 + plot_layout(ncol=2,guides="collect") & xlab(NULL) & theme(plot.margin = margin(5.5, 5.5, 0, 5.5))

combo_plot2 <- wrap_elements(panel = combo_plot ) +
  labs(tag = "Vulnerability Category") +
  theme(
    plot.tag = element_text(size = rel(1)),
    plot.tag.position = "bottom"
  )

ggsave("output/bioregion_vulnerability.png",combo_plot2,width=10,height=6,units="in",dpi=300)


### Depth Zone plot -------------------

depth_zone_plot <- bioregion_df %>%
  filter(rcp == "2.6") %>%  # Takes the first RCP level
  mutate(
    region = factor(region, levels = area_order),
    depth_cat = factor(depth_cat, levels = rev(c("Coastal", "Shelf", "Deep")))
  ) %>%
  group_by(ocean, region, depth_cat) %>%
  summarise(depth_area = sum(area, na.rm = TRUE), .groups = "drop") %>%
  mutate(depth_area_km2 = depth_area ) %>%
  ggplot(aes(x = depth_area_km2, y = region, fill = depth_cat)) +
  geom_col(position = "stack", width = 0.7,col="black") +
  scale_fill_viridis_d(name = "Depth\nCategory", option = "plasma") +
  # scale_x_continuous(
  #   labels = function(x) {
  #     case_when(
  #       x >= 1e6 ~ paste0(round(x/1e6, 1), "M"),
  #       x >= 1e3 ~ paste0(round(x/1e3, 1), "K"),
  #       TRUE ~ as.character(round(x, 1))
  #     )
  #   }
  # ) +
  facet_grid(ocean ~ ., scales = "free_y", space = "free_y") +
  labs(
    x = "Area (km²)",
    y = "",
    fill = ""
  ) +
  theme_bw() +
  theme(
    axis.text.y = element_text(size = 8),
    strip.background = element_rect(fill="white"),
    strip.text = element_text(face = "bold")
  )

ggsave("output/depth_zone_biorgion_plot.png",depth_zone_plot)

#Vuln ~ depth  ------

bioregion_depth_summary <- bioregion_extract %>%
  data.frame()%>%
  dplyr::select(-geometry)%>%
  filter(!is.na(vuln), !is.na(bathy), !is.na(cell_area)) %>%  # Remove missing values
  mutate(
    depth = abs(bathy),
    depth_bin = floor(depth / 10) * 10 + 5  # Bin centers (5, 15, 25, etc.)
  ) %>%
  group_by(rcp,region, depth_bin) %>%
  summarise(
    n_points = n(),
    mean_vuln = weighted.mean(vuln,cell_area, na.rm = TRUE),
    sd_vuln = sd(vuln, na.rm = TRUE),
    se_vuln = sd_vuln / sqrt(n_points),
    q25_vuln = quantile(vuln, 0.25, na.rm = TRUE),
    q75_vuln = quantile(vuln, 0.75, na.rm = TRUE),
    min_depth = min(depth, na.rm = TRUE),
    max_depth = max(depth, na.rm = TRUE),
    .groups = "drop"
  )%>%
  left_join(.,bioregion_abbrev) %>%
  #filter(n_points >= 5) %>%  # Filter bins with sufficient data points adjust this threshold as needed
  mutate(
    # Calculate confidence intervals
    ci_lower = mean_vuln - 1.96 * se_vuln,
    ci_upper = mean_vuln + 1.96 * se_vuln,
    region = factor(region,levels=bioregion_ord),
    ab_region =factor(ab_region,levels=bioregion_abbrev$ab_region),
    depth_cat = case_when(depth_bin >= 200 ~ "Deep",
                          depth_bin >= 50 & depth_bin < 200 ~ "Shelf", #or epipelagic
                          TRUE ~ "Coastal"),
    depth_cat = factor(depth_cat,levels=c("Coastal","Shelf","Deep"))
  )

# Prepare raw data for plotting (subsample if too many points)
# set.seed(123)
# bioregion_raw <- bioregion_extract %>%
#   data.frame() %>%
#   dplyr::select(-geometry) %>%
#   filter(!is.na(vuln), !is.na(bathy)) %>%
#   mutate(depth = abs(bathy)) %>%
#   group_by(region) %>%
#   {if(nrow(.) > 2000) slice_sample(., n = 1200) else .} %>%  # Sample only if group is large
#   ungroup()

bioregion_raw <- bioregion_extract %>%
                data.frame() %>%
                dplyr::select(-geometry) %>%
                filter(!is.na(vuln), !is.na(bathy)) %>%
                left_join(.,bioregion_abbrev) %>%
                mutate(depth = abs(bathy),
                       region = factor(region,levels=bioregion_ord),
                       ab_region =factor(ab_region,levels=bioregion_abbrev$ab_region),
                       scenario = factor(rcp,levels=c("2.6","8.5")))

depth_bin_target <- c(5,15,25,35,45,65,95,125,155,185,seq(215,5095,10)[seq(5, length(seq(215,5095,10)), 5)])

# Plot 1: Points with error bars and smooth lines (faceted by ocean)
plot_vuln_depth_faceted_26 <- ggplot() +
  geom_hline(yintercept = c(0.25,0.45),lty=2)+
  #geom_vline(xintercept = c(50, 200), lty = 2, color = "black", alpha = 0.7) +
  # Raw data points (with transparency)
  geom_point(data = bioregion_raw%>%filter(rcp=="2.6"), 
             aes(x = depth, y = vuln), 
             alpha = 0.1, size = 0.5, color = "gray60") +
  # Binned means with error bars
  geom_point(data = bioregion_depth_summary%>%filter(depth_bin %in% depth_bin_target,rcp=="2.6"),
             aes(x = depth_bin, y = mean_vuln,fill = depth_cat),
             size = 2,shape=21,col="black") +
  geom_errorbar(data = bioregion_depth_summary%>%filter(depth_bin %in% depth_bin_target,rcp=="2.6"),
                aes(x = depth_bin, ymin = ci_lower, ymax = ci_upper,color=depth_cat),
                width = 5, alpha = 0.8,show.legend = FALSE) +
  # Smooth fitted line
  geom_smooth(data = bioregion_raw%>%filter(rcp=="2.6"),
              aes(x = depth, y = vuln),
              method = "loess", span = 0.3, 
              color = "blue", fill = "lightblue", alpha = 0.3) +
  
  #coord_cartesian(xlim = range(bioregion_raw$depth, na.rm = TRUE)) +
  facet_wrap(~ ab_region, scales = "free") +
  labs(title="RCP 2.6",
       #title = "Vulnerability vs Depth Relationship by Ocean Basin",
       #subtitle = "Red points: 10m binned means ± 95% CI, Blue line: LOESS smooth, Gray points: raw data",
       x = "Depth (m)",
       y = "Mean vulnerability ± 95% CI",
       fill=""
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    axis.title = element_text(size = 11),
    plot.title = element_text(size = 14, face = "bold"),
    strip.background = element_rect(fill="white")
  )

plot_vuln_depth_faceted_85 <- ggplot() +
  geom_hline(yintercept = c(0.25,0.45),lty=2)+
  #geom_vline(xintercept = c(50, 200), lty = 2, color = "black", alpha = 0.7) +
  # Raw data points (with transparency)
  geom_point(data = bioregion_raw%>%filter(rcp=="8.5"), 
             aes(x = depth, y = vuln), 
             alpha = 0.1, size = 0.5, color = "gray60") +
  # Binned means with error bars
  geom_point(data = bioregion_depth_summary%>%filter(depth_bin %in% depth_bin_target,rcp=="8.5"),
             aes(x = depth_bin, y = mean_vuln,fill = depth_cat),
             size = 2,shape=21,col="black") +
  geom_errorbar(data = bioregion_depth_summary%>%filter(depth_bin %in% depth_bin_target,rcp=="8.5"),
                aes(x = depth_bin, ymin = ci_lower, ymax = ci_upper,color=depth_cat),
                width = 5, alpha = 0.8,show.legend = FALSE) +
  # Smooth fitted line
  geom_smooth(data = bioregion_raw%>%filter(rcp=="8.5"),
              aes(x = depth, y = vuln),
              method = "loess", span = 0.3, 
              color = "blue", fill = "lightblue", alpha = 0.3) +
  
  #coord_cartesian(xlim = range(bioregion_raw$depth, na.rm = TRUE)) +
  facet_wrap(~ ab_region, scales = "free") +
  labs(title="RCP 8.5",
       #title = "Vulnerability vs Depth Relationship by Ocean Basin",
       #subtitle = "Red points: 10m binned means ± 95% CI, Blue line: LOESS smooth, Gray points: raw data",
       x = "Depth (m)",
       y = "Mean vulnerability ± 95% CI",
       fill=""
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    axis.title = element_text(size = 11),
    plot.title = element_text(size = 14, face = "bold"),
    strip.background = element_rect(fill="white")
  )

ggsave("output/vulnerability_bioregion_depth_rcp26.png",plot_vuln_depth_faceted_26,height=6,width=8,units='in',dpi=300)
ggsave("output/vulnerability_bioregion_depth_rcp85.png",plot_vuln_depth_faceted_85,height=6,width=8,units="in",dpi=300)

#all together 

depth_vul_biorigon <- ggplot() +
  geom_hline(yintercept = c(0.25,0.45),lty=2, inherit.aes = FALSE)+
  geom_point(data = bioregion_raw, 
             aes(x = depth, y = vuln,col=scenario), 
             alpha = 0.1, size = 0.5,show.legend = FALSE) +
  geom_smooth(data = bioregion_raw,
              aes(x = depth, y = vuln,col=scenario,fill=scenario,group=scenario),
              method = "loess", span = 0.5, alpha = 0.3,show.legend = FALSE) +
  coord_cartesian(xlim = range(bioregion_raw$depth, na.rm = TRUE)) +
  facet_wrap(~ ab_region, scales = "free") +
  labs(
    #title = "Vulnerability vs Depth Relationship by Ocean Basin",
    #subtitle = "Red points: 10m binned means ± 95% CI, Blue line: LOESS smooth, Gray points: raw data",
    x = "Depth (m)",
    y = "Vulnerability",
    fill=""
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold", size = 8),
    axis.title = element_text(size = 11),
    plot.title = element_text(size = 14, face = "bold"),
    strip.background = element_rect(fill="white")
  )+
  scale_colour_manual(values=c("cornflowerblue","#FF0000"))+
  scale_fill_manual(values=c("cornflowerblue","#FF0000"))

ggsave("output/depth_vuln_bioregion.png",height=6,width=8,units='in',dpi=300)
