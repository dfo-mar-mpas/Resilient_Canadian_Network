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

#load functions
source("code/00_functions.R")

#load the bioregion extraction (from Boyce et al. 2022- Canada) processed by keen et al
load("data/bioregion_extract.RData")

#Load shapefiles
cpcad_marine <- read_sf("data/cpcad/cpcad_areas_marine.shp")%>%
                st_transform(CanProj)

canada_eez <- read_sf("data/shapefiles/Canada_EEZ.shp")%>%
              st_transform(CanProj)

bioregion <- get_spatial_layer("https://egisp.dfo-mpo.gc.ca/arcgis/rest/services/open_data_donnees_ouvertes/federal_marine_bioregions_bioregions_marines_federales/MapServer/0")

bioregion_ord <- c("Southern Shelf","Strait of Georgia","Northern Shelf","Offshore Pacific",
                   "Western Arctic","Arctic Archipelago","Arctic Basin","Eastern Arctic","Hudson Bay Complex",
                   "Newfoundland-Labrador Shelves","Gulf of Saint Lawrence","Scotian Shelf")   

bioregion_abbrev <- read.csv("data/region_abbreviations.csv")%>%
                    mutate(region = factor(region,levels=bioregion_ord))%>%
                    arrange(region)%>%
                    mutate(region=as.character(region))

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

canada <- basemap%>%
          filter(country=="Canada")

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
