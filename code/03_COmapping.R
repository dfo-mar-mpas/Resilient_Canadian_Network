#download the COs for Marine Refuges in Canada based on the marine refuge website and combine with the MPA file that was 

#load libaries
library(rvest)
library(tidyverse)
library(MarConsNetData)
library(DT)
library(sf)

CanProj <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

#load the cleaned cpcad_marine dataset so that we can add bioregion to each MPA/MR
cpcad_marine <- read_sf("data/cpcad_complete.shp")%>%
  data.frame()

## functions for 
source("code/00_functions.R") # this has the text extraction functions needed to parse out the COs. 

#Marine Refuge COs -----

#URL for the Marine Refuge Website
url <- "https://www.dfo-mpo.gc.ca/oceans/oecm-amcepz/refuges/index-eng.html"

# Extract tables
tables <- read_html(url) %>% 
  html_table()

#convert to dataframe
downloaded_table <- tables[[1]]


cleaned_table <- downloaded_table %>%
  mutate(
    conservation_objective = str_extract(Description, 
                                         "(?<=Conservation objective: ).*?(?=Prohibited Activities:)"),
    prohibited_activities = str_extract(Description, 
                                        "(?<=Prohibited Activities: ).*"),
    conservation_objective = str_trim(conservation_objective),
    prohibited_activities = str_trim(prohibited_activities)
  )


refuge_df <- cleaned_table%>%
             mutate(type="Marine Refuge")%>%
             rename(mechanism = names(cleaned_table)[grepl("establishment",tolower(names(cleaned_table)))],
                    site=Name,
                    prohibitions = prohibited_activities,
                    bioregion=Bioregion,
                    conservation_objectives = conservation_objective)%>%
             dplyr::select(site,bioregion,type,mechanism,conservation_objectives,prohibitions)%>%
             mutate(ocean = case_when(bioregion %in% c("Northern Shelf Bioregion","Strait of Georgia") ~ "Pacific",
                                      bioregion %in% c("Eastern Arctic","Eastern Arctic and Newfoundland-Labrador Shelves") ~ "Arctic", #the NL shelves/EA one is mostly in the arctic so assigning there. 
                                      TRUE ~ "Atlantic"),
                    bioregion = case_when(bioregion == "Eastern Arctic and Newfoundland-Labrador Shelves"~"Eastern Arctic",
                                          bioregion == "Newfoundland and Labrador Shelves"~"Newfoundland-Labrador Shelves",
                                          bioregion == "Newfoundland-Labrador Shelves; Estuary and Gulf of St. Lawrence"~"Estuary and Gulf of St. Lawrence",
                                          bioregion == "Northern Shelf Bioregion"~"Northern Shelf",
                                          TRUE ~ bioregion),
                    bioregion = ifelse(bioregion == "Estuary and Gulf of St. Lawrence","Gulf of Saint Lawrence",bioregion))
            
              
#MPA Conservation Objectives - web scraping functions pulled from MarConsNetData and the MPA webpage. NOte that these functions
#are based on the structure of those webpages (i.e., breaks, headings) as of July 2025

eastport <- cpcad_marine%>%
            filter(grepl("Eastport",NAME_E))%>%
            mutate(NAME_E = "Eastport Marine Protected Area")%>%
            slice(1)

cpc_mpas <- cpcad_marine%>%
            filter(type=="MPA",
                   !grepl("Eastport",NAME_E), #replaced with above
                   !grepl("Endeavour",NAME_E))%>%
            rbind(.,eastport)%>%
            rename(site=NAME_E)%>%
            dplyr::select(site,ocean,bioregion)%>%
            mutate(site = case_when(site == "SG?a?an K?i?nghlas-Bowie Marine Protected Area" ~ "SGaan Kinghlas-Bowie Seamount Marine Protected Area",
                                    site== "Banc-des-Américains" ~ "Bank-des-Americains", #fix names for matching
                                    site=="ThT Marine Protected Area" ~ "Tanggwan - hacxwiquak-Tsigis Marine Protected Area",
                                    TRUE ~ site))

mpa_names <- c("St. Anns Bank Marine Protected Area", "Musquash Estuary Marine Protected Area", "Laurentian Channel Marine Protected Area",
             "Gully Marine Protected Area", "Banc-des-Américains Marine Protected Area", "Basin Head Marine Protected Area",
             "Eastport – Round Island Marine Protected Area", "Gilbert Bay Marine Protected Area", "Tarium Niryutait Marine Protected Area",
             "Anguniaqvia niqiqyuam Marine Protected Area", "Tuvaijuittuq Marine Protected Area", "Hecate Strait/Queen Charlotte Sound Glass Sponge Reefs Marine Protected Area",
             "Sgaan Kinghlas-Bowie Seamount Marine Protected Area", "Tanggwan - hacxwiquak-Tsigis Marine Protected Area")

tables <- list()
for (i in seq_along(mpa_names)) {
  message(mpa_names[i])
  message(i)
  tables[[i]] <- data_objectives(area = mpa_names[i], type="site", prohibiton=TRUE)
}

mpa_text <- bind_rows(tables)%>%
            rename(MPA = 1, site_objectives = 2, prohibition = 3)

mpa_df <- mpa_text  %>%
  group_by(MPA) %>%
  mutate(conservation_objectives = list(extract_conservation_objectives(conservation_text)),
         prohibitios = extract_regulations(prohibition)) %>%
  ungroup() %>%
  unnest(conservation_objectives)%>%
  rename(site=MPA)

mpa_df <- mpa_text %>%
  group_by(MPA) %>%
  mutate(
    # Use map to ensure function works on each row
    conservation_objectives = map(site_objectives, extract_conservation_objectives),
    prohibitions = extract_regulations(prohibition)
  ) %>%
  ungroup() %>%
  # Use unnest_longer to handle list columns
  unnest_longer(conservation_objectives) %>%
  # Remove any empty conservation objectives
  filter(conservation_objectives != "") %>%
  rename(site = MPA)%>%
  dplyr::select(site,conservation_objectives,prohibitions)%>%
  mutate(type="MPA",
         site = case_when(site=="Eastport – Round Island Marine Protected Area" ~ "Eastport Marine Protected Area", #fix names for matching
                          site=="Hecate Strait/Queen Charlotte Sound Glass Sponge Reefs Marine Protected Area" ~ "Hecate Strait and Queen Charlotte Sound Glass Sponge Reefs Marine Protected Areas",
                          site=="Sgaan Kinghlas-Bowie Seamount Marine Protected Area" ~ "SGaan Kinghlas-Bowie Seamount Marine Protected Area",
                          TRUE ~ site),
         mechanism="Oceans Act")%>%
  left_join(.,cpc_mpas)%>%
  data.frame()


#National Marine Conservation Areas 
#https://parks.canada.ca/amnc-nmca/gestion-management/politique-policy-2022#section_3

#The overarching objective is "protecting and conserving representative marine areas for the benefit, education and enjoyment of the people of Canada and the world"

#Site objectives derived from managmement plans 
    #Saguenay-St. Lawrence Marine Park
      #https://parks.canada.ca/amnc-nmca/qc/saguenay/info/plan/gestion-management
    #Gwaii Haanas National Marine Conservation Area Reserve & Haida Heritage Site
      #https://parks.canada.ca/pn-np/bc/gwaiihaanas/info/plan/gestion-management-2018#section6-0
    #Tallurutiup Imanga National Marine Conservation Area - intierim as of this assembly
    #https://parks.canada.ca/amnc-nmca/cnamnc-cnnmca/tallurutiup-imanga/ebauche-draft

#manually assembled the COs from the management plans and curated into those with some ecological basis (i.e., those that could be measured via an ecological-based indicator) 
#and those with a 'social' basis in that they are associated primarily with the site, cultural/geographic context  and management of the site. 

NMCA_df <- read.csv("data/NMCA_objectives.csv")%>%
           filter(co_type=="Ecological")%>% #see notes above for how this was deteremined
           mutate(bioregion=case_when(site=="Tallurutiup Imanga National Marine Conservation Area" ~ "Eastern Arctic",
                                      site=="Saguenay-St. Lawrence Marine Park" ~ "Gulf of Saint Lawrence",
                                      site=="Gwaii Haanas National Marine Conservation Area Reserve & Haida Heritage Site" ~ "Northern Shelf"),
                  mechanism = "Canada National Marine Conservation Areas Act",
                type = "National Marine Conservation Area",
                prohibitions = NA)%>%
            rename(conservation_objectives = CO,
                   ocean=Ocean)

#Make the combined table
cols <- c("type","mechanism","ocean","bioregion","site","conservation_objectives","prohibitions")

canada_mcn_df <- rbind(mpa_df%>%dplyr::select(all_of(cols)),
                       refuge_df%>%dplyr::select(all_of(cols)),
                       NMCA_df%>%dplyr::select(all_of(cols)))%>%
                 mutate(ocean=factor(ocean,levels=c("Pacific","Arctic","Atlantic")))%>%
                 arrange(ocean,bioregion,type,site)

write.csv(canada_mcn_df,"data/canada_mcn_objectives.csv",row.names = FALSE)


#or view in a fancy html table
# dt <- datatable(
#   df,
#   escape = FALSE,         # <--- key to rendering HTML!
#   options = list(
#     pageLength = 5,
#     scrollX = TRUE,
#     autoWidth = TRUE,
#     rownames = FALSE
#   )
# )
# dt
