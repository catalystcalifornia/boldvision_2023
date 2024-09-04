# Script that creates a crosswalk of PUMAs to Service Planning Areas based on population distribution in census tracts and PUMAs
# Revised method used for matching ACS public use microdata to SPAs

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
source("W:\\RDA Team\\R\\credentials_source.R")

con_bv <- connect_to_db("bold_vision")

con_rda <- connect_to_db("rda_shared_data")

#download datasets
spas <- st_read("W:/Data/Geographies/LA County/LA_County_Service_Planning_Area/2022/Service_Planning_Areas_2022/Service_Planning_Areas__2022_.shp")
spas <- st_transform(spas, crs = 3310) 


puma <- st_read(con_rda, query = "SELECT * FROM geographies_ca.cb_2018_06_puma10_500k WHERE geoid10 LIKE '06037%'")
puma <- st_transform(puma, crs = 3310)


tract <- st_read(con_rda, query = "SELECT ct_geoid, geom_3310 as geom FROM geographies_ca.cb_2020_06_tract_500k WHERE ct_geoid LIKE '06037%'")
tract <- st_transform(tract, crs = 3310) 
#population in each tract by 0-24 youth
tract_population <- st_read(con_bv, query = "SELECT * FROM bv_2023.dhc_tract_2020_youth_0_24_race WHERE race = 'total'")

# 3 tracts seem to have been dropped off? 
tract_final <- merge(tract, tract_population, by.x = "ct_geoid", by.y = "geoid")
tract_final$ct_area<-st_area(tract_final)

####Step 1: Total youth (0-24) population by census tract (tract pop in bv database already) and calculate total youth population by PUMA####

intersect_p_t <- st_intersection(tract_final, puma)
intersect_p_t$intersect_area<-st_area(intersect_p_t)
intersect_p_t <- intersect_p_t%>%mutate(prc_ct_area=as.numeric(intersect_area)/as.numeric(ct_area))

# intersect_p_t <- st_centroid(tract_final) %>% st_join(puma)



# sf_use_s2(FALSE)
intersect_p_t_pop <- intersect_p_t %>%
  group_by(pumace10) %>%
  summarise(youth_pop = sum(pop*prc_ct_area, na.rm = FALSE)) %>%
  as.data.frame() %>%
  select(-geometry)

puma_youth_pop <- merge(intersect_p_t_pop, puma, by = 'pumace10') %>%
  mutate(puma_id = pumace10,
         geoid = geoid10,
         puma_name = name10,
         puma_youth_pop = youth_pop) %>%
  select(puma_id, puma_youth_pop, geoid, puma_name, geom) %>%
  st_as_sf()

####Step 2: Intersect PUMAs to SPAs ####

intersect_puma_spa <- st_intersection(puma_youth_pop, spas)

####Step 3: Take intersects from #2 and intersect to census tracts####
puma_spa_tract <- st_intersection(intersect_puma_spa, tract_final)
puma_spa_tract$intersect_area<-st_area(puma_spa_tract)
puma_spa_tract <- puma_spa_tract%>%mutate(prc_ct_area=as.numeric(intersect_area)/as.numeric(ct_area))


####Step 4: Aggregate and calculate total youth population in each PUMA/SPA intersect and percent of that intersect population out of total youth PUMA population####

puma_spa_summary <- puma_spa_tract %>%
  group_by(SPA,SPA_NAME, puma_id, puma_name) %>% 
  summarise(puma_spa_pop=sum(pop*prc_ct_area), puma_spa_pop_prc= puma_spa_pop/min(puma_youth_pop), puma_youth_pop = min(puma_youth_pop)) 
#note: pop is census tract population


####Step 5: Set a 30% or more threshold to assign pumas to spas ####

puma_spa_xwalk <- puma_spa_summary %>% filter(puma_spa_pop_prc>=.30) %>%
  select(SPA, SPA_NAME, puma_id, puma_name, puma_spa_pop, puma_spa_pop_prc, geom) %>%
  rename("spa_id" = "SPA",
         "spa_name" ="SPA_NAME") 

puma_spa_xwalk <- rowid_to_column(puma_spa_xwalk, "unique_id")
puma_spa_xwalk <- st_drop_geometry(puma_spa_xwalk) 

# ####Step 6: Push to Postgres####
# dbWriteTable(con_bv, c("bv_2023", "crosswalk_puma_spas_2023_v2"),
#              puma_spa_xwalk, overwrite = TRUE, row.names = FALSE)
# 
# # write comment to table
# sql_comment_xwalk <- "COMMENT ON TABLE bv_2023.crosswalk_puma_spas_2023_v2 IS 'The following crosswalk was developed joining 2022 LA County SPAs and 2018 PUMAs and filtering pumas to be assigned to the SPAs based on youth (ages 0 to 24) population density. 
# A 30% intersection threshold was set. Geom projection is in 3310.
# Read more in the QA doc here: W:/Project/OSI/Bold Vision/BV 2022-2023/Documentation/QA_PUMS_SPA_xwalk.docx';
# 
# COMMENT ON COLUMN bv_2023.crosswalk_puma_spas_2023_v2.unique_id IS 'a unique id';
# COMMENT ON COLUMN bv_2023.crosswalk_puma_spas_2023_v2.spa_id IS 'the service planning area number';
# COMMENT ON COLUMN bv_2023.crosswalk_puma_spas_2023_v2.spa_name IS 'the service planning area name';
# COMMENT ON COLUMN bv_2023.crosswalk_puma_spas_2023_v2.puma_id IS 'the puma id number;';
# COMMENT ON COLUMN bv_2023.crosswalk_puma_spas_2023_v2.puma_name IS 'the puma name';
# COMMENT ON COLUMN bv_2023.crosswalk_puma_spas_2023_v2.puma_spa_pop IS 'total population in this puma_spa intersect area';
# COMMENT ON COLUMN bv_2023.crosswalk_puma_spas_2023_v2.puma_spa_pop_prc IS 'prc of population of puma that resides in this spa';
# "
# dbSendQuery(conn = con_bv, sql_comment_xwalk)

dbDisconnect(con_bv)
dbDisconnect(con_rda)



