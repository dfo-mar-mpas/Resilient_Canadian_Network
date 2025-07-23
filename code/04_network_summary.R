library(plotly)
library(dplyr)

# Prepare data for plotly sunburst
sunburst_data <- area_df_bioregion %>%
  group_by(ocean, bioregion, type) %>%
  summarise(area = sum(prop_area), .groups = 'drop') %>%
  mutate(
    parent = case_when(
      type != "" ~ paste(ocean, bioregion, sep = " - "),
      bioregion != "" ~ ocean,
      TRUE ~ ""
    ),
    label = case_when(
      type != "" ~ paste(type, sprintf("(%.1f%%)", area * 100)),
      bioregion != "" ~ bioregion,
      TRUE ~ ocean
    ),
    value = area,
    color = case_when(
      ocean == "Arctic" ~ "#66c2a5",
      ocean == "Atlantic" ~ "#fc8d62",
      ocean == "Pacific" ~ "#8da0cb",
      type == "MPA" ~ "#1f78b4",
      type == "Marine Refuge" ~ "#33a02c",
      type == "OECM" ~ "#e31a1c",
      TRUE ~ "#cccccc"
    )
  )

# Create plotly sunburst
plot_ly(
  data = sunburst_data,
  type = "sunburst",
  labels = ~label,
  parents = ~parent,
  values = ~value,
  branchvalues = "total",
  marker = list(
    colors = ~color
  )
) %>%
  layout(
    title = "Marine Conservation Coverage",
    margin = list(t = 50, l = 25, r = 25, b = 25)
  )


canada_area = canada_eez%>%st_transform(4326)%>%mutate(area=round(as.numeric(st_area(.)/1000/1000),2))%>%pull(area)

canada_area = sum()



plot_ly(labels=c("Canada","Pacific","MPA","Mr","OECM","Arctic","MPA2","MR2","OECM2","Atlantic","MPA3","MR","OECM3"),
        parents=c("","Canada","Pacific","Pacific","Pacific","Canada","Arctic","Arctic","Arctic","Canada","Atlantic","Atlantic","Atlantic"),
        values=c(canada_area,area_df_ocean%>%slice(1:3)%>%pull(total_area),
                 canada_area,area_df_ocean%>%slice(4:6)%>%pull(total_area),
                 canada_area,area_df_ocean%>%slice(7:9)%>%pull(total_area)),
        type="sunburst")


ocean_df

        
        
        area_df_ocean$type,
        parents=area_df_ocean$ocean,
        values=area_df_ocean$total_area,
        type="sunburst")
