#ZIP Code to LA County SPA crosswalk by intersection (20% or more overlap)
#for Bold Vision 2023 update

library(dplyr)  
library(RPostgreSQL) 
library(rpostgis) 
library(sf)
library(tidyverse)

source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("rda_shared_data")

#import zip codes and ensure crs and shape validity
zip_codes <- st_read(con, query = "select * from geographies_la.lacounty_zipcodes_2022")
zip_codes <- st_transform(zip_codes, crs = 3310)
zip_codes <- st_make_valid(zip_codes)

#disconnect
dbDisconnect(con)

#download spas
spas <- st_read("W:/Data/Geographies/LA County/LA_County_Service_Planning_Area/2022/Service_Planning_Areas_2022/Service_Planning_Areas__2022_.shp")
spas <- st_transform(spas, crs = 3310) 

# calculate area of spas/zip_codes 
spas$spa_area <- st_area(spas)
zip_codes$zip_code_area <- st_area(zip_codes)

# intersect spas and puma
intersects <- st_intersection(zip_codes, spas)

# calculate area of the intersect
intersects$intersect_area <- st_area(intersects)

# calculate percent of intersect out of tract area and agency area
intersects$prc_spa_area <- as.numeric(intersects$intersect_area/intersects$spa_area)
intersects$prc_zip_code_area <- as.numeric(intersects$intersect_area/intersects$zip_code_area)

#twenty percent threshold 
intersects_twenty <- intersects %>% filter(prc_zip_code_area>=.20)

#format zip spa crosswalk
zip_spa_xwalk <- intersects_twenty %>% select(zipcode, SPA, SPA_NAME, prc_zip_code_area) %>%
  rename(spa = SPA, spa_name = SPA_NAME)

#convert to data frame for easier table calculations
zip_spa_xwalk <- as.data.frame(zip_spa_xwalk) %>% select(-geom)

#address multipolys
zip_spa_xwalk <- zip_spa_xwalk %>% filter(zipcode != "90802") 
zip_spa_xwalk <- zip_spa_xwalk %>% filter(zipcode != "90803") 
zip_spa_xwalk[nrow(zip_spa_xwalk) + 1, ] = c("90802", "8", "South Bay", 1)
zip_spa_xwalk[nrow(zip_spa_xwalk) + 1, ] = c("90803", "8", "South Bay", 1)

#address population center issues
zip_spa_xwalk <- zip_spa_xwalk %>% filter(zipcode != "93550")
zip_spa_xwalk <- zip_spa_xwalk %>% filter(zipcode != "93543")
zip_spa_xwalk <- zip_spa_xwalk %>% filter(zipcode != "93553")
zip_spa_xwalk <- zip_spa_xwalk %>% filter(zipcode != "91042")

zip_spa_xwalk[nrow(zip_spa_xwalk) + 1, ] = c("93550", "1", "Antelope Valley", 1)
zip_spa_xwalk[nrow(zip_spa_xwalk) + 1, ] = c("93543", "1", "Antelope Valley", 1)
zip_spa_xwalk[nrow(zip_spa_xwalk) + 1, ] = c("93553", "1", "Antelope Valley", 1)
zip_spa_xwalk[nrow(zip_spa_xwalk) + 1, ] = c("91042", "2", "San Fernando", 1)
zip_spa_xwalk$prc_zip_code_area <- as.numeric(zip_spa_xwalk$prc_zip_code_area)

#format zip codes
zip_codes <- as.data.frame(zip_codes) %>% select(zipcode)

#address multipolys
zip_codes <- zip_codes %>% filter(zipcode != "90802") 
zip_codes <- zip_codes %>% filter(zipcode != "90803") 
zip_codes[nrow(zip_codes) + 1, ] = c("90802")
zip_codes[nrow(zip_codes) + 1, ] = c("90803")


###Send to Postgres###
bv_con <- connect_to_db("bold_vision")
table_name <- "crosswalk_zip_spas_2023"
schema <- 'bv_2023'

indicator <- "Crosswalk of LA County ZIP codes and Service Planning Areas or SPAs by intersection with at least 20% overlap required. Some ZIP codes were manually matched."
source <- "County of Los Angeles Entreprise GIS (2022). See QA doc for details: W:\\Project\\OSI\\Bold Vision\\BV 2023\\Documentation\\QA_ZIP_SPA_xwalk.docx"

dbWriteTable(bv_con, c(schema, table_name), zip_spa_xwalk,
             overwrite = FALSE, row.names = FALSE)

#comment on table and columns
comment <- paste0("COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
                                          COMMENT ON COLUMN ", schema, ".", table_name, ".zipcode IS 'ZIP code';
                   COMMENT ON COLUMN ", schema, ".", table_name, ".spa IS 'SPA number';
                   COMMENT ON COLUMN ", schema, ".", table_name, ".spa_name IS 'SPA name';
                   COMMENT ON COLUMN ", schema, ".", table_name, ".prc_zip_code_area IS 'Percent of ZIP code area in SPA';")
# print(comment)
dbSendQuery(bv_con, comment)

#disconnect
dbDisconnect(bv_con)


