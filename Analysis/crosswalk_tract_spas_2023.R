#Census tract to LA County Service Planning Area (SPA) crosswalk by centroid
library(tigris)
library(ggplot2)
library(dplyr)
library(RPostgreSQL)
library(sf)

#get credentials & create connection to rda database for SPA shapefile
source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("rda_shared_data")

#get spa shapefile and reduce fields
spas <- st_read(con, query = "select * from geographies_la.la_county_service_planning_areas_2022")
spas <- spas %>% select(SPA, SPA_NAME, geometry)

#disconnect
dbDisconnect(con)

#get tract shapefile
la_county_tracts <- tracts(state = "CA", county = "Los Angeles", cb = TRUE, year = 2021)

#transform CRS of SPAs and Tracts
spas <- st_transform(spas, 3310)
la_county_tracts <- st_transform(la_county_tracts, 3310)

#test st_crs(spas)

#spatial join SPAs to tracts
tract_spa_xwalk <- st_join(spas, st_centroid(la_county_tracts), join = st_contains)

#convert to df and reduce columns
tract_spa_xwalk <- as.data.frame(tract_spa_xwalk) %>% select(GEOID, SPA, SPA_NAME) %>% 
  rename(geoid = GEOID, spa = SPA, spa_name = SPA_NAME)

#add back one tract that fails in spatial join
tract_spa_xwalk[nrow(tract_spa_xwalk) + 1, ] = c("06037599100", "8", "South Bay")

###Send to Postgres###
con3 <- connect_to_db("bold_vision")
table_name <- "crosswalk_tract_spas_2023"
schema <- 'bv_2023'

indicator <- "Crosswalk of Census Tracts to LA County Service Planning Areas or SPAs by centroid"
source <- "Census cartographic boundary tracts from Tigris R package (2021). SPAs from County of Los Angeles Entreprise GIS (2022) https://egis-lacounty.hub.arcgis.com/datasets/3b337956e643448d8705e5d991855eb0/explore. See QA doc: W:\\Project\\OSI\\Bold Vision\\BV 2023\\Documentation\\QA_CT_SPA_xwalk.docx"

dbWriteTable(con3, c(schema, table_name), tract_spa_xwalk,
             overwrite = FALSE, row.names = FALSE)

#comment on table and columns
comment <- paste0("COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
                                         COMMENT ON COLUMN ", schema, ".", table_name, ".geoid IS 'Census Tract FIPS';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".spa IS 'SPA number';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".spa_name IS 'SPA name';")
print(comment)
dbSendQuery(con3, comment)

#disconnect
dbDisconnect(con3)



#######################
#ditto for block groups
#######################

#get bg shapefile and transform crs
# la_county_bgs <- block_groups(state = "CA", county = "Los Angeles", cb = TRUE, year = 2021)
# la_county_bgs <- st_transform(la_county_bgs, 4326)
# 
# #spatial join SPAs to bgs
# bg_spa_xwalk <- st_join(spas, st_centroid(la_county_bgs), join = st_contains)
# 
# #convert to df and reduce columns
# bg_spa_xwalk <- as.data.frame(bg_spa_xwalk) %>% select(GEOID, SPA, SPA_NAME)
# 
# #add back two bgs that fail in spatial join
# bg_spa_xwalk[nrow(bg_spa_xwalk) + 1, ] = c("060376706042", "8", "South Bay")
# bg_spa_xwalk[nrow(bg_spa_xwalk) + 1, ] = c("060376212041", "8", "South Bay")
# 
