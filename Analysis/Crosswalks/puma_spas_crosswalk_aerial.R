# Script that creates a crosswalk of PUMAs to Service Planning Areas based on spatial intersect or aerial apportionment
# Original method used for matching ACS public use microdata to SPAs

#loading libraries
library(dplyr)  
library(RPostgreSQL) 
library(rpostgis) 
library(leaflet)
library(sf)
library(formattable)
library(tidyverse)
library(htmltools)
# create connection

con_bv <- connect_to_db("bold_vision")

con_rda <- connect_to_db("rda_shared_data")

#download datasets
spas <- st_read("W:/Data/Geographies/LA County/LA_County_Service_Planning_Area/2022/Service_Planning_Areas_2022/Service_Planning_Areas__2022_.shp")
spas <- st_transform(spas, crs = 4326) 


puma <- st_read(con_rda, query = "SELECT * FROM geographies_ca.cb_2018_06_puma10_500k WHERE geoid10 LIKE '06037%'")
puma <- st_transform(puma, crs = 4326)

# calculate area of spa/pum 
spas$spas_area <- st_area(spas)
puma$puma_area <- st_area(puma)

# intersect spas and puma
intersects <- st_intersection(puma, spas)

# calculate area of the intersect
intersects$intersect_area <- st_area(intersects)

# calculate percent of intersect out of tract area and agency area
intersects$prc_spas_area <- as.numeric(intersects$intersect_area/intersects$spas_area)
intersects$prc_puma_area <- as.numeric(intersects$intersect_area/intersects$puma_area)

#ten percent threshold
intersects_ten <- intersects %>% filter(prc_puma_area>=.10)

#twenty percent threshold 
intersects_twenty <- intersects %>% filter(prc_puma_area>=.20)


#checking the merge

leaflet(width = "100%", height = "495px") %>%
  # add cartodb tiles
  addProviderTiles("CartoDB.Positron") %>% 
  #setView
  setView(-118.6368454, 34.2324274, zoom = 8) %>%
  addPolygons(
    data = intersects, fill = "#004DA8", weight = 3, dashArray = "3", color ="#004DA8",
    smoothFactor = 2, popup = ~paste0("<strong>SPA: </strong>", SPA_NAME,
                                      "</br><strong>PUMA: </strong>", geoid10),
    group = "PUMAs"
  ) %>% 
  addPolygons(
    data = spas, fill = FALSE, weight = 3, dashArray = "3", color ="#fdae61",
    smoothFactor = 2, popup = ~paste0("<strong>SPA: </strong>", SPA),
    group = "SPAs"
  ) %>% 
  addPolygons(
    data = intersects_ten, fill = "#807dba", weight = 3, dashArray = "3", color ="#807dba",
    smoothFactor = 2, popup = ~paste0("<strong>SPA: </strong>", SPA_NAME,
                                      "</br><strong>PUMA: </strong>", geoid10), 
    group = "PUMAs_ten"
  ) %>% 
  addPolygons(
    data = intersects_twenty, fill = "pink", weight = 3, dashArray = "3", color ="pink",
    smoothFactor = 2, popup = ~paste0("<strong>SPA: </strong>", SPA_NAME,
                                      "</br><strong>PUMA: </strong>", geoid10), 
    group = "PUMAs_twenty"
  ) %>% 
  addLayersControl(
    overlayGroups = c("SPAs", "PUMAs", "PUMAs_ten","PUMAs_twenty"),
    options = layersControlOptions(collapsed = FALSE)
  )

#for postgres upload
intersect_postgres<-   intersects %>% 
  filter(prc_puma_area>=.20) %>% 
  mutate(unique_id = row_number()) %>%
  select(unique_id, geoid10, name10, SPA, SPA_NAME, intersect_area, prc_puma_area, prc_spas_area, geom) %>%
  rename("puma_id" = "geoid10",
         "puma_name" = "name10",
         "spa_id" = "SPA",
         "spa_name" ="SPA_NAME") 

#uploading to postgres 

# dbWriteTable(con_bv, c("bv_2023", "crosswalk_puma_spas_2023"),
#              intersect_postgres, overwrite = TRUE, row.names = FALSE)

# # write comment to table
# sql_comment_xwalk <- "COMMENT ON TABLE bv_2023.crosswalk_puma_spas_2023 IS 'The following crosswalk was developed joining 2022 LA County SPAs and 2018 PUMAs. 
# A 20% intersection threshold was set where at least 20% of the PUMA area needed to intersect with the SPA to be included in the crosswalk. 
# Geom projection is in 4326.
# Read more in the QA doc here: W:/Project/OSI/Bold Vision/BV 2023/Documentation/QA_PUMS_SPA_xwalk.docx
# prc_puma_area is the percent area of each PUMA in each SPA combination
# prc_spas_area is the percent of each SPA the PUMA intersect covers;'"
# dbSendQuery(conn = con_bv, sql_comment_xwalk)

# # export SPA table to postgres for future use
# dbWriteTable(con_rda, c("geographies_la", "la_county_service_planning_areas_2022"),
#              spas, overwrite = TRUE, row.names = FALSE)
# sql_comment_spa <- "COMMENT ON TABLE geographies_la.la_county_service_planning_areas_2022 IS 'The following SPAs shapefile was downloaded to W:/Data/Geographies/LA County/LA_County_Service_Planning_Area/2022 and then read into an R script and transformed from 2229 to SRID 4326
# Read more in the QA doc here: W:/Project/OSI/Bold Vision/BV 2023/Documentation/QA_PUMS_SPA_xwalk.docx
# Data Source https://data.lacounty.gov/datasets/3b337956e643448d8705e5d991855eb0_4/explore;'"
# dbSendQuery(conn = con_rda, sql_comment_spa)

dbDisconnect(con_bv)
dbDisconnect(con_rda)
