#download the COs for Marine Refuges in Canada based on the marine refuge website and combine with the MPA file that was 

#load libaries
library(rvest)
library(tidyverse)
library(MarConsNetData)
library(DT)
library(sf)
library(readr)
library(tidytext)

#load the cleaned cpcad_marine dataset so that we can add bioregion to each MPA/MR
cpcad_marine <- read_sf("data/cpcad_complete.shp")%>%
  data.frame()

## functions for 
source("code/00_functions.R") # this has the text extraction functions needed to parse out the COs. 

### Extract the COs for Marine Refuges ------

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
            
       
### Extract the COs for MPAs ------       
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


### Extract the COs National Marine Conservation Areas  -------
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

### Compile the Conservation Objective grouped dataset -----
cols <- c("type","mechanism","ocean","bioregion","site","conservation_objectives","prohibitions")

canada_mcn_df <- rbind(mpa_df%>%dplyr::select(all_of(cols)),
                       refuge_df%>%dplyr::select(all_of(cols)),
                       NMCA_df%>%dplyr::select(all_of(cols)))%>%
                 mutate(ocean=factor(ocean,levels=c("Pacific","Arctic","Atlantic")))%>%
                 arrange(ocean,bioregion,type,site)

#Eastern Canyons has a singe line objective with two distinct components. To be fair this will be split
canada_mcn_df[canada_mcn_df$site == "Eastern Canyons Conservation Area","conservation_objectives"] <- "to protect cold-water corals"

ec2 <- canada_mcn_df[canada_mcn_df$site == "Eastern Canyons Conservation Area",]
ec2$conservation_objectives <- "to protect Lophelia pertusa coral reef"

canada_mcn_df <- rbind(canada_mcn_df,ec2)%>%
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



### classification of the objectives --------------

canada_mcn_df <- read.csv("data/canada_mcn_objectives.csv")%>% #compilation above. 
                 filter(!conservation_objectives %in% c("Promotion of public awareness, education, and support of the Gilbert Bay MPA", #these are named MPA objectives best measured by non-ecological data. 
                                                        "Facilitation of scientific research opportunities in the Gilbert Bay ecosystem",
                                                        "Implement environmental monitoring mechanisms so as to learn more about and measure the evolution of the ecosystems in the Marine Park and the effectiveness of the management terms."
                                                        ))
objectives <- canada_mcn_df$conservation_objectives

objectives_df <- tibble(id = 1:length(objectives), text = objectives)
clean_obj <- objectives_df |>
  unnest_tokens(word, text) |>
  filter(!word %in% stop_words$word)

#Classify based on climate change centric words
robust_terms <- c("resilience", "adapt", "ecosystem function", "diversity", "sustainable use", "integrity", 
                  "conserve", "dynamic", "process", "ecological role",
                  "habitats","benthic habitat","sponge",'coral',"species","biodiversity", "sustainable",
                  "representative","distrubance","minimize impacts","productivity","benthic"
                  )

vulnerable_terms <- c("restore", "previous", "historic", "current", "specific", 
                      "maintain", "recovery", "restoration", "preserve", "structure",
                      "conservation","integrity")

species_names <- c("American lobster", "Atlantic salmon", "Beluga whale", "Wolffish", "Northern Wolffish","Leatherback sea turtle", "Porbeagle shark", 
                   "Irish moss", "Chondrus crispus","Ringed seal", "Bearded seal", "Arctic char", "Atlantic herring", "Winter flounder",
                    "Witch flounder", "Yellowtail flounder", "American plaice", "Vazella pourtalesi",
                    "Lophelia pertusa", "Black dogfish", "Smooth skate", "Gilbert Bay cod", "Atlantic cod",
                    "Haddock","lobster","sea pens")

#function to do a semi-quantitative analysis of the COs
score_objective <- function(text) {
  # First check for named species
  if (any(sapply(species_names, function(species) str_detect(tolower(text), tolower(species))))) {
    return("Vulnerable")
  }
  
  n_vulnerable <- sum(sapply(vulnerable_terms, function(term) str_detect(tolower(text), tolower(term))))
  
  n_robust <- sum(sapply(robust_terms, function(term) str_detect(tolower(text), tolower(term))))
  
  if (n_robust > n_vulnerable) return("Robust")
  if (n_vulnerable > n_robust) return("Vulnerable")
  return("Unclear")
}

canada_mcn_df$climate_robustness <- sapply(canada_mcn_df$conservation_objectives, score_objective)

#manual check here - these don't fit obvious groupings using the key word classification

canada_mcn_df%>%filter(climate_robustness == "Unclear")%>%pull(conservation_objectives)

canada_mcn_df <- canada_mcn_df%>%
                 mutate(climate_robustness = case_when(conservation_objectives == "Maintain the overall ecological integrity of the Basin Head lagoon and inner channel. This includes avoidance of excessive <em>Ulva</em> growth, maintenance of adequate oxygen levels, and maintenance of the diversity of indigenous flora and fauna." ~ "Vulnerable",
                                                       conservation_objectives == "Ensure the conservation and protection of threatened or endangered species" ~ "Robust",
                                                       TRUE ~ climate_robustness))

#output table
table_out <- canada_mcn_df%>%
             arrange(ocean,bioregion,type,site,climate_robustness)%>%
             dplyr::select(ocean,bioregion,type,site,conservation_objectives,climate_robustness)%>%
             rename(CO = conservation_objectives, categorization = climate_robustness)

write.csv(table_out,"output/CO_categorization.csv",row.names=FALSE)

#now do a singular classification of the site level objective vulnerability
mcn_site_vulnerability <- canada_mcn_df%>%
                          group_by(site)%>%
                          summarise(n_obj = n(),
                                      n_vul = sum(climate_robustness=="Vulnerable"),
                                      n_rob = sum(climate_robustness=="Robust"),
                                      categorization = case_when(n_vul == n_obj ~ "high",
                                                                 n_rob>0 & n_rob<n_obj ~ "medium",
                                                                 n_rob == n_obj ~ "low"))%>%
                          ungroup()%>%
                          mutate(categorization = factor(categorization, levels=c("high","medium","low")))%>%
                          left_join(.,canada_mcn_df%>%distinct(site,.keep_all=TRUE)%>%dplyr::select(ocean,bioregion,type,site))%>%
                          mutate(new_site=tolower(site))%>%
                          left_join(.,cpcad_marine%>%mutate(new_site=tolower(NAME_E))%>%dplyr::select(new_site,area))%>%
                          dplyr::select(-new_site)

#match up the areas
mismatched_sites <- setdiff(tolower(mcn_site_vulnerability$site),tolower(cpcad_marine$NAME_E))

fix_df <- data.frame(missed=mismatched_sites,name=c("Bay of Islands Salmon Migration Closure",
                                                     "Eastport ? Duck Island Marine Protected Area", #only 1 of 2 parts to Eastport so need to add 0.33 to that size. Also the name is messed up in the cpcad when loaded to R
                                                     "Emerald Basin Sponge Conservation Area",
                                                     "Gwa?xdlala/Nala?xdlala (Lull/Hoeya) marine refuge",
                                                     "Gaw Kaahlii Marine Refuge",
                                                     "Lobster Area Closure (Penguin Islands)", #in CPCAD this is all split up into indivdiual sites so the area will have to be aggregated for the dataframe
                                                     "Magdalen Islands Lagoons Closures (6 Overlapping Closures)",
                                                     "SG?a?an K?i?nghlas-Bowie Marine Protected Area",
                                                     "Scallop Buffer Zone (SFA 21)", #three parts it the CPCAD that will need to be aggregated. 
                                                     "Strait of Georgia and Howe Sound Glass Sponge Reef Closure (East Defence Islands)", #multiple parts will need to be aggregated
                                                     "ThT Marine Protected Area",
                                                     "Xaana Kaahlii Marine Refuge"))%>%
          mutate(fix=tolower(name))

mcn_site_vulnerability[tolower(mcn_site_vulnerability$site) %in% mismatched_sites,"site"] <- fix_df$name

#rematch to cpcad
mcn_site_vulnerability <- mcn_site_vulnerability%>%
                          dplyr::select(-area)%>%
                          mutate(new_site=tolower(site))%>%
                          left_join(.,cpcad_marine%>%mutate(new_site=tolower(NAME_E))%>%dplyr::select(new_site,area))%>%
                          dplyr::select(-new_site)

#fix the grouped issues with cpc vs the site profiles

#Eastport MPA - two parts
mcn_site_vulnerability[mcn_site_vulnerability$site == "Eastport ? Duck Island Marine Protected Area","area"] <- 
  cpcad_marine%>%filter(grepl("eastport",tolower(NAME_E)))%>%pull(area)%>%sum()

mcn_site_vulnerability[mcn_site_vulnerability$site == "Eastport ? Duck Island Marine Protected Area","site"] <- 
  "Eastport Marine Protected Area"

#Lobster Area Closures 7 parts
mcn_site_vulnerability[mcn_site_vulnerability$site == "Lobster Area Closure (Penguin Islands)","area"] <- 
cpcad_marine%>%filter(grepl("Lobster Area Closure",(NAME_E)))%>%pull(area)%>%sum()

mcn_site_vulnerability[mcn_site_vulnerability$site == "Lobster Area Closure (Penguin Islands)","site"] <- 
  "Lobster Area closures (Trout River, Shoal Point, Penguin Islands, Gooseberry Island, Glovers Harbour, Mouse Island and Gander Bay)"

#Scallop buffer zones - 3 parts
mcn_site_vulnerability[mcn_site_vulnerability$site == "Scallop Buffer Zone (SFA 21)","area"] <- 
cpcad_marine%>%filter(grepl("Scallop Buffer Zone",(NAME_E)))%>%pull(area)%>%sum()

mcn_site_vulnerability[mcn_site_vulnerability$site == "Scallop Buffer Zone (SFA 21)","site"] <- 
"Scallop Buffer Zones (SFA 21, 22, 24)"

#Glass Sponge Reef Closures (17 parts in CPCAD)
mcn_site_vulnerability[mcn_site_vulnerability$site == "Strait of Georgia and Howe Sound Glass Sponge Reef Closure (East Defence Islands)","area"] <- 
cpcad_marine%>%filter(grepl("Glass Sponge Reef Closure",(NAME_E)))%>%pull(area)%>%sum()

mcn_site_vulnerability[mcn_site_vulnerability$site == "Strait of Georgia and Howe Sound Glass Sponge Reef Closure (East Defence Islands)","site"] <-
"Strait of Georgia and Howe Sound Glass Sponge Reef (17 fisheries area closures)"

#save outputs for quicker loading
write.csv(mcn_site_vulnerability,"output/CO_site_categorization.csv",row.names = FALSE)

#summarize the results
mcn_site_vulnerability%>%
  group_by(categorization)%>%
  summarise(area=sum(area))%>%
  ungroup()%>%
  mutate(prop_area = area/sum(area))

#
plot_df <- canada_mcn_df%>%
           distinct(site,.keep_all = TRUE)%>%
           dplyr::select(-conservation_objectives,-prohibitions)%>%
           left_join(.,mcn_site_vulnerability)%>%
           left_join(.,cpcad_marine%>%
                       rename(site=NAME_E)%>%
                       dplyr::select(site,area)) #get the sites to match up to area so you can do an area -weighted proportion. 
          

library(ggplot2)
library(dplyr)

# Calculate proportions
plot_data <- plot_df %>%
  group_by(ocean, climate_robustness) %>%
  summarise(count = n()) %>%
  group_by(ocean) %>%
  mutate(prop = count / sum(count),
         # Calculate y position for labels
         ypos = cumsum(prop) - prop/2)

# Create the plot
ggplot(plot_data, aes(x = ocean, y = prop, fill = climate_robustness)) +
  geom_bar(stat = "identity", position = "fill",col="black") +
  geom_text(
    aes(y = ypos, label = count), 
    color = "white",
    fontface = "bold"
  ) +
  labs(
    x = "Type",
    y = "Proportion",
    fill = "Climate Robustness",
    title = "Proportion of Climate Robustness by Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::percent)
