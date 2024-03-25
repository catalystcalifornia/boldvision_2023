# packages
library(tidyverse)
library(readxl)
library(sf)
library(httr)
library(jsonlite)
library(sp)
#remotes::install_github("catalystcalifornia/RC")
library(RC) # RaceCounts package created by RDA
library(janitor)
library(lubridate)

## data setup ##
source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("rda_shared_data")
con2 <- connect_to_db("bold_vision")


# # Indicator: Calculating youth arrests ------------------------------------
# 
# # Crosswalk ---------------------------------------------------------------
# 
# ## LAPD reporting districts
# lapd_districts_2022 <- st_read(con, query = "SELECT * FROM geographies_la.lapd_reporting_districts_2022") %>% st_transform(lapd_districts_2022, crs = 3310)
# 
# ## Public Safety Boundaries
# 
# saf_boundaries <-  st_read(con, query = "SELECT * FROM geographies_la.la_public_safety_station_boundaries_2023")   %>% st_transform(saf_boundaries, crs = 3310)
# 
# 
# ## SPAs
spa <- st_read(con, query = "SELECT * FROM geographies_la.la_county_service_planning_areas_2022")  %>% st_transform(spa, crs = 3310)
colnames(spa) <- tolower(colnames(spa))
# 
# # cross-walk LAPD-Districts/SPAS ------------------------------------------
# 
# # calculate area of districts and spas
# lapd_districts_2022$dist_area <- st_area(lapd_districts_2022)
# spa$spa_area <- st_area(spa)
#     
# # rename geoid fields
# # run intersect
# districts_spa <- st_intersection(lapd_districts_2022, spa) 
# # calculate area of intersect
# districts_spa$intersect_area <- st_area(districts_spa)
#  
# # calculate percent of intersect out of total district area
# districts_spa$prc_area <- as.numeric(districts_spa$intersect_area/districts_spa$dist_area)
#     
# # convert to df
# xwalk <- as.data.frame(districts_spa)
# 
# # add 25% threshold
# threshold <- .25
# 
# dist_spa_xwalk <- xwalk %>% filter(prc_area >= threshold) %>% select(name,bureau,  spa, spa_name, prc_area)
# 
# # export dist_spa xwalk table
# table_name <- "crosswalk_lapd_district_spas_2022"
# schema <- "bv_2023"
# source <- "Created with W:\\Project\\OSI\\Bold Vision\\BV 2023\\R\\Systems Impact\\si_youth_arrest.R
# LAPD districts with 25% or more of their area within a SPA is assigned to that SPA"
# indicator <- "LAPD district SPA crosswalk"
# 
# #dbWriteTable(con2, c(schema, table_name),  dist_spa_xwalk,
# #           overwrite = FALSE, row.names = FALSE)    
# 
# 
# comment <- paste0("COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
#                  COMMENT ON COLUMN ", schema, ".", table_name, ".name IS 'LAPD District Name';
#                 COMMENT ON COLUMN ", schema, ".", table_name, ".bureau IS 'LAPD bureau';
#                   COMMENT ON COLUMN ", schema, ".", table_name, ".spa IS 'Service Planning Area (SPA) number';
#                    COMMENT ON COLUMN ", schema, ".", table_name, ".spa_name IS 'Service Planning Area (SPA) name';
#                  COMMENT ON COLUMN ", schema, ".", table_name, ".prc_area IS 'Percent of LAPDs area with 25% or more of intersection area in a SPA';
#                 ")
# 
# #dbSendQuery(con2, comment)
# 
# 
# 
# # Cross-walk: Public Safety Boundaries and SPA ----------------------------
# 
# saf_boundaries$saf_area <- st_area(saf_boundaries)
# spa$spa_area <- st_area(spa)
#     
# # rename geoid fields
# # run intersect
# saf_spa <- st_intersection(saf_boundaries, spa) 
# # calculate area of intersect
# saf_spa$intersect_area <- st_area(saf_spa)
#     
# # calculate percent of intersect out of total district area
# saf_spa$prc_area <- as.numeric(saf_spa$intersect_area/saf_spa$saf_area)
#     
# # convert to df
# xwalk <- as.data.frame(saf_spa)
# 
# 
# # add 20% threshold
# threshold <- .20
#     
# saf_spa_xwalk <- xwalk %>% filter(prc_area >= threshold) %>% select(gid, station, st_name, s_type, spa, spa_name, prc_area)
# 
# # 7 public safety stations with duplicated SPAs
# saf_spa_duplicated <- saf_spa_xwalk %>% filter(gid %in% c("29", "33", "43", "57", "59", "74", "86")) %>% arrange(gid)
# 
# 
# 
# # export dist_spa xwalk table
# table_name <- "crosswalk_pubsafety_stations_spas_2022"
# schema <- "bv_2023"
# indicator <- "Public Safety Station SPAS cross-walk"
# source <- "Created with W:\\Project\\OSI\\Bold Vision\\BV 2023\\R\\Systems Impact\\si_youth_arrest.R
# Public Safety Stations with 20% or more of their area within a SPA is assigned to that SPA"
# 
# #dbWriteTable(con2, c(schema, table_name),  saf_spa_xwalk,
# #         overwrite = FALSE, row.names = FALSE)    
# 
# 
# comment <- paste0("COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
# COMMENT ON COLUMN ", schema, ".", table_name, ".station IS 'Public Safety Station';
# COMMENT ON COLUMN ", schema, ".", table_name, ".st_name IS 'Public Safety Station name without type (sheriff, police, lapd)';
# COMMENT ON COLUMN ", schema, ".", table_name, ".s_type IS 'Public Safety Station type';
# COMMENT ON COLUMN ", schema, ".", table_name, ".spa IS 'Service Planning Area (SPA) number';
# COMMENT ON COLUMN ", schema, ".", table_name, ".spa_name IS 'Service Planning Area (SPA) name';
# COMMENT ON COLUMN ", schema, ".", table_name, ".prc_area IS 'Percent of Public Safety Stations area with 20% or more of intersection area in a SPA';
# 
# 
# ")
# 
# 
# # dbSendQuery(con2, comment)
# 
# 
# 
# # UPLOAD LASD/LAPD/RIPA DATA TO POSTGRES ---------------------------------------
# 
# #lasd_person <- read_csv("W:/Data/Crime and Justice/LASD/2023/lasd_stops_person_2023.csv")
# #df <- lasd_person
# 
# # clean up column names
# 
# #df <- df %>% clean_names()
# #colnames(df) <- tolower(colnames(df))
# #colnames(df) <- gsub("\\?.*", "", colnames(df))
# #colnames(df) <- gsub("\\#.*", "", colnames(df))
# #colnames(df) = gsub("-", "", colnames(df)) # remove hyphens
# #colnames(df) = gsub(" ", "_", colnames(df))
# 
# # rename columns
# #df <- df %>%
# 
# #rename("reasonable_suspicion_officer_witnessed_commission_crime" = "reasonable_suspicion_that_the_officer_witnessed_commission_of_a_crime") %>% 
# 
# #  rename("reasonable_suspicion_action_indicative_casing_victimlocation" = "reasonable_suspicion_that_the_persons_actions_was_indicative_of_casing_a_victim_or_location") %>% 
#   
# # rename("reasonable_suspicion_action_indicative_drugtransaction" = "reasonable_suspicion_that_the_persons_actions_was_indicative_of_a_drug_transaction") %>%
#  
# # rename("reasonable_suspicion_action_indicative_violentcrime" = "reasonable_suspicion_that_the_persons_actions_was_indicative_of_engaging_in_a_violent_crime") %>% 
#  
# #rename("reasonable_suspicion_that_the_person_matched_description" = "reasonable_suspicion_that_the_person_matched_suspect_description") %>% 
# 
# #    rename("reasonable_suspicion_person_witness_or_victim_ofsuspect" = "reasonable_suspicion_that_the_person_was_a_witness_or_victim_id_of_suspect_at_the_scene") %>%
#   
# #    rename("reasonable_suspicion_person_carrying_suspicious_object" = "reasonable_suspicion_that_the_person_may_be_carrying_suspicious_object") %>%
#   
# #   rename("reasonable_suspicion_lookout" = "reasonable_suspicion_that_the_person_was_suspected_of_acting_as_a_lookout") %>%
#   
# #    rename("search_basis:parole_probation_prcs_mandatorysupervision" = "search_basis_condition_of_parole_probation_prcs_mandatory_supervision") %>%
#   
# #    rename("contraband_evidence_discovered_cell_phone_electronics" = "contraband_evidence_discovered_cell_phone_s_or_electronic_device_s") %>%
#   
# #    rename("result_of_contact_custodial_arrest_warrant" = "result_of_contact_custodial_arrest_pursuant_to_outstanding_warrant") %>%
#   
# #    rename("result_of_contact_custodial_arrest_no_warrant" = "result_of_contact_custodial_arrest_without_warrant_offense_codes") %>%
#   
# #   rename("result_of_contact_noncriminal_caretaking_transport" = "result_of_contact_noncriminal_transport_or_care_taking_transport") %>%
#   
# #  rename("result_of_contact_contacted_legal_guardian" = "result_of_contact_contacted_parent_legal_guardian_or_other_person_responsible_for_minor") %>%
#   
# #    rename("result_of_contact_contacted_dept_homeland_security" = "result_of_contact_contacted_u_s_department_of_homeland_security") %>%
#  
# # rename("result_of_contact_referral_school_staff" = "result_of_contact_referral_to_school_counselor_or_other_support_staff")
#     
# 
#   
# #table_name <- "lasd_stops_person_2018_2022"
# #schema <- 'crime_and_justice'
# #source <- "County of Los Angeles Sheriff Officer Contacts Person Details downloaded here: https://data.lacounty.gov/datasets/lacounty::sheriff-officer-contacts-person-details-/about
# #Years for data are: 2018-2022"
# 
# #dbWriteTable(con, c(schema, table_name), df, 
# #            overwrite = FALSE, row.names = FALSE)
# 
# #comment <- paste0("
# #               COMMENT ON TABLE ", schema, ".", table_name,  " IS '"," from ", source, ".';")
# 
# #dbSendQuery(con, comment)
# 
# ### INCIDENT DATA ###
# 
# lasd_incident <- read_csv("W:/Data/Crime and Justice/LASD/2023/lasd_stops_incident_2023.csv") %>% clean_names()
# 
# df <- lasd_incident
# 
# ## clean up time-zone
# 
# df_final <- df %>% mutate(date_reformatted = parse_date_time(date_time, '%m/%d/%Y %I:%M:%S %p'),
# date_reformatted = as_datetime(date_reformatted, tz = "America/Los_Angeles")) %>% select(contact_id, date_time, date_reformatted, everything())
# 
# 
# table_name <- "lasd_stops_incident_2018_2022"
# schema <- 'crime_and_justice'
# source <- "County of Los Angeles Sheriff Officer Contacts Incident Details downloaded here: https://data.lacounty.gov/datasets/lacounty::sheriff-officer-contacts-incident-details-/about
# Years for data are: 2018-2022"
# 
# #dbWriteTable(con, c(schema, table_name), df_final, 
# #            overwrite = FALSE, row.names = FALSE)
#            
# #comment <- paste0("
# #                COMMENT ON TABLE ", schema, ".", table_name,  " IS '"," from ", source, ".';")
# 
# #dbSendQuery(con, comment)
# 
# 
# ## lapd reporting districts
# lapd_arrest <- read_excel("W:/Data/Crime and Justice/LAPD/Arrest_Data_from_2020_to_Present.xlsx") 
# 
# df <- lapd_arrest %>% clean_names()
# 
# ## add leading 0 to reporting district with less than 4 characters
# 
# df <- df %>% mutate(
#   num_character = nchar(gsub("^A-Z", "", reporting_district)),
#   reporting_district = ifelse(num_character <4, paste0("0", reporting_district), reporting_district)) %>% select(-num_character)
#   
# df <- transform(df, report_id = as.character(report_id),
#           time = as.character(time),
#           area_id = as.character(area_id),
#           age = as.numeric(age),
#           charge_group_code = as.character(charge_group_code),
#           lat = as.numeric(lat),
#           lon = as.numeric(lon),
#           booking_time = as.character(booking_time),
#           booking_location_code = as.character(booking_location_code)
# )
# 
# table_name <- "lapd_arrests_2020_2023"
# schema <- 'crime_and_justice'
# source <- "Arrest Data from 2020 to Present downloaded here: https://data.lacity.org/Public-Safety/Arrest-Data-from-2020-to-Present/amvf-fr72/about_data"
# 
# indicator <- "LAPD Arrest Data 2020-20203"
# source <- "LAPD Stops/arrests: https://data.lacity.org/Public-Safety/Arrest-Data-from-2020-to-Present/amvf-fr72/about_data
# See QA doc for details: W:\\Project\\OSI\\Bold Vision\\BV 2023\\Documentation\\System Impact\\QA_Youth_Arrests.docx"
# 
# 
# #dbWriteTable(con, c(schema, table_name), df, 
# #            overwrite = FALSE, row.names = FALSE)
# 
# #comment on table and columns
# 
# #comment <- paste0("COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
# #                  COMMENT ON COLUMN ", schema, ".", table_name, ".report_id IS 'ID for the arrest';
# #                  COMMENT ON COLUMN ", schema, ".", table_name, ".report_type IS 'BOOKING = Person is booked at a detention facility RFC = Person is cited #and Released From Custody (RFC)';
# #                  COMMENT ON COLUMN ", schema, ".", table_name, ".arrest_date IS 'Mm/DD/YYYY';
# #                  COMMENT ON COLUMN ", schema, ".", table_name, ".time IS '24 hr military time';
# #                  COMMENT ON COLUMN ", schema, ".", table_name, ".area_id IS 'The LAPD has 21 Community Police Stations referred to as Geographic Areas within the department. These Geographic Areas are sequentially numbered from 1-21.';
# #                  COMMENT ON COLUMN ", schema, ".", table_name, ".area_name IS 'The 21 Geographic Areas or Patrol Divisions are also given a name designation that references a landmark or the surrounding community that it is responsible for. For example 77th Street Division is located at the intersection of South Broadway and 77th Street, serving neighborhoods in South Los Angeles.';
# #                 COMMENT ON COLUMN ", schema, ".", table_name, ".reporting_district IS 'A four-digit code that represents a sub-area within a Geographic Area. All arrest records reference the RD that it occurred in for statistical comparisons. Find LAPD Reporting Districts on the LA City GeoHub at http://geohub.lacity.org/datasets/lapd-reporting-districts?geometry=-121.023%2C33.621%2C-115.797%2C34.418';
# #                  COMMENT ON COLUMN ", schema, ".", table_name, ".age IS 'age';
# #                  COMMENT ON COLUMN ", schema, ".", table_name, ".sex_code IS 'Female, Male';
# #                  COMMENT ON COLUMN ", schema, ".", table_name, ".descent_code IS 'Descent Code: A - Other Asian B - Black C - Chinese D - Cambodian F - Filipino G - Guamanian H - Hispanic/Latin/Mexican I - American Indian/Alaskan Native J - Japanese K - Korean L - Laotian O - Other P - Pacific Islander S - Samoan U - Hawaiian V - Vietnamese W - White X - Unknown Z - Asian Indian';
# #                  COMMENT ON COLUMN ", schema, ".", table_name, ".charge_group_code IS 'Category of Arrest Charge';
# #                 COMMENT ON COLUMN ", schema, ".", table_name, ".charge_group_description IS 'Defines the Charge Group Code provided.';
# #                  COMMENT ON COLUMN ", schema, ".", table_name, ".arrest_type_code IS 'A code to indicate the type of charge the individual was arrested for. D - Dependent F - Felony I - Infraction M - Misdemeanor O - Other';
# #                  COMMENT ON COLUMN ", schema, ".", table_name, ".charge IS 'The charge the individual was arrested for.';
# #                  COMMENT ON COLUMN ", schema, ".", table_name, ".charge_description IS 'Defines the Charge Provided';
# #                  COMMENT ON COLUMN ", schema, ".", table_name, ".disposition_description IS 'Disposition of Arrest';
# #                  COMMENT ON COLUMN ", schema, ".", table_name, ".address IS 'Street address of crime incident rounded to the nearest hundred block to maintain anonymity';
# #                  COMMENT ON COLUMN ", schema, ".", table_name, ".cross_street IS 'Cross Street of rounded Address.';
# #                  COMMENT ON COLUMN ", schema, ".", table_name, ".lat IS 'Latitude - The location where the crime incident occurred. Actual address is omitted for confidentiality. XY coordinates reflect the nearest 100 block.';
# #                  COMMENT ON COLUMN ", schema, ".", table_name, ".lon IS 'Longitude - The location where the crime incident occurred. Actual address is omitted for confidentiality. XY coordinates reflect the nearest 100 block.';
# #                  COMMENT ON COLUMN ", schema, ".", table_name, ".location IS 'The location where the crime incident occurred. Actual address is omitted for confidentiality. XY coordinates reflect the nearest 100 block';
# #                  COMMENT ON COLUMN ", schema, ".", table_name, ".booking_date IS 'MM/DD/YYYY - Date person was booked at a detention facility.';
# #                  COMMENT ON COLUMN ", schema, ".", table_name, ".booking_time IS 'In 24 hour military time - Time person was booked at a detention facility.';
# #                  COMMENT ON COLUMN ", schema, ".", table_name, ".booking_location IS 'Location person was booked';
# #                  COMMENT ON COLUMN ", schema, ".", table_name, ".booking_location_code IS 'Code of location where person was booked';
# #                 ")
# 
# 
# #dbSendQuery(con, comment)
# 
# 
# 
# 
# 
# 
# 
# #  Import RIPA Q1-Q4 ------------------------------------------------------
# 
# #ripa_q1 <- read_excel("W:/Data/Crime and Justice/CA DOJ/RIPA Stop Data/2022/RIPA Stop Data _ Los Angeles 2022 Q1.xlsx")
# #ripa_q2 <- read_excel("W:/Data/Crime and Justice/CA DOJ/RIPA Stop Data/2022/RIPA Stop Data _ Los Angeles 2022 Q2.xlsx")
# #ripa_q3 <- read_excel("W:/Data/Crime and Justice/CA DOJ/RIPA Stop Data/2022/RIPA Stop Data _ Los Angeles 2022 Q3.xlsx")
# #ripa_q4 <- read_excel("W:/Data/Crime and Justice/CA DOJ/RIPA Stop Data/2022/RIPA Stop Data _ Los Angeles 2022 Q4.xlsx")
# 
# #la_ripa <- rbind(ripa_q1, ripa_q2, ripa_q3, ripa_q4)
# 
# #df <- la_ripa
# #colnames(df) <- tolower(colnames(df))
# #colnames(df) = gsub(" ", "_", colnames(df))
# #table_name <- "cadoj_ripa_la_2022"
# #schema <- 'crime_and_justice'
# #source <- "2022 RIPA Data for LA County. Downloaded here: https://openjustice.doj.ca.gov/data"
# 
# #dbWriteTable(con, c(schema, table_name), df, 
# #            overwrite = FALSE, row.names = FALSE)
# 
# 
# #comment <- paste0("
# #                  COMMENT ON TABLE ", schema, ".", table_name,  " IS '"," from ", source, ".';")
# 
# #dbSendQuery(con, comment)
# 
# 
# # Import Shapefile Boundaries ---------------------------------------------

## public safety station boundaries
crosswalk_pubsafety_stations_spas_2022 <- dbGetQuery(con2, "SELECT station, spa, spa_name, prc_area FROM bv_2023.crosswalk_pubsafety_stations_spas_2022") %>% arrange(station)
# edit column name for stations
crosswalk_pubsafety_stations_spas_2022$station<-  gsub(" Police", "", crosswalk_pubsafety_stations_spas_2022$station)
crosswalk_pubsafety_stations_spas_2022$station <-  gsub(" Sheriff", "", crosswalk_pubsafety_stations_spas_2022$station)

## LAPD District/SPAS crosswalk
crosswalk_lapd_district_spas_2022 <- dbGetQuery(con2, "SELECT * FROM bv_2023.crosswalk_lapd_district_spas_2022")




# # Import arrest data, merge to crosswalk ------------------------------------------------------
# 
# # combine the two LASD files, merge patrol station to: crosswalk_pubsafety_stations_spas_2022. Filter to sheriff type
# 
# LASD Data Cleaning ------------------------------------------------------
# 
# lasd_incident <- dbGetQuery(con, "SELECT * FROM crime_and_justice.lasd_stops_incident_2018_2022") %>% select(contact_id, patrol_station, date_time)
# lasd <- dbGetQuery(con, "SELECT * FROM crime_and_justice.lasd_stops_person_2018_2022")  %>% left_join(lasd_incident, by = "contact_id")
# 
# # clean column names
# colnames(lasd) <- gsub("/", "_", colnames(lasd))
# 
# # rename agency abbreviations with full name
# 
# lasd_recode <- lasd %>% mutate(patrol_station_recode = recode(patrol_station, 
#                                                AIR = "Airborne", # needs QA, missing from boundaries
#                                                AVA = "Avalon",
#                                                CAS = "Carson", 
#                                                CCS = "Unknown", # Not Sure
#                                                CEN = "Century", 
#                                                CER = "Cerritos", # needs QA
#                                                CPT = "Compton", # needs QA
#                                                CSB = "County Services Bureau", # needs QA, missing from boundaries
#                                                CVS = "Crescenta Valley", 
#                                                DET = "Unknown", # Not Sure
#                                                ELA = "East Los Angeles",  
#                                                EOB = "Emergency Outreach Bureau", #needs QA, missing from boundaries
#                                                FPK = "Firestone Park",  #needs QA, missing from boundaries
#                                                IDT = "Industry",  
#                                                LAN = "Lancaster", 
#                                                LHS = "Malibu / Lost Hills", 
#                                                LKD = "Lakewood", 
#                                                LMT = "Lomita", 
#                                                LNX = "South Los Angeles", #needs QA(serves Lennox), https://lasd.org/south-los-angeles/
#                                                MDR = "Marina Del Rey", 
#                                                NWK = "Norwalk", 
#                                                OCS = "Office of County Services",  #needs QA, missing from boundaries
#                                                OPS = "Office of Public Safety",  #needs QA, missing from boundaries
#                                                OTH = "Other", # needs QA, missing from boundaries
#                                                PKB = "Parks Bureau", # needs QA, missing from boundaries
#                                                PLM = "Palmdale", # needs QA
#                                                PRV = "Pico Rivera", 
#                                                SCT = "Santa Clarita Valley", 
#                                                SDM = "San Dimas", 
#                                                SLA = "South Los Angeles", 
#                                                TEM = "Temple", 
#                                                TSB = "Transit Services Bureau", # needs QA, missing from boundaries
#                                                WAL = "Walnut / Diamond Bar", 
#                                                WHD = "West Hollywood"))
#                                                
#                                                
# # filter for 2022 youth 
# lasd_filter <- lasd_recode %>% filter(grepl('2022', date_time) & age <=24) %>% select(contact_id, date_time, age, everything())
# 
# # filter for arrests
# lasd_filter <-lasd_filter %>% filter(result_of_contact_custodial_arrest_without_warrant == "Yes" | result_of_contact_custodial_arrest_warrant == "Yes") %>% select(contact_id, person_id, date_time, age, street_number, street_direction, street_name, street_type, suite, cross_street, landmark, full_address, city, state, zip_code, asian, black_african_american, hispanic_latino_latina, middle_eastern_south_asian, native_american, pacific_islander, white,result_of_contact_custodial_arrest_without_warrant, result_of_contact_custodial_arrest_warrant, patrol_station, patrol_station_recode, age)


# ## push to post-gres for youth diversion
# indicator <- "LASD Youth Arrests Individual Records"
# table_name = "lasd_youth_arrests_2022"
# schema = "bv_2023"
# 
# 
# source <- "Created with  W:\\Project\\OSI\\Bold Vision\\BV 2023\\R\\Systems Impact\\si_youth_arrest.R
# Sheriff Officer Contacts Incident Details. https://data.lacounty.gov/datasets/5d079a13bd914010a513c11f7d581d95_0/explore
# Sheriff Officer Contacts Person Details. https://data.lacounty.gov/datasets/ab74ffc494c94bbfaff3dd45c9678144_0/explore
# Youth Arrests 0-24
# "


#dbWriteTable(con2, c(schema, table_name), lasd_filter, overwrite = FALSE, row.names = FALSE)

# #comment on table and columns
# comment <- paste0("
#                   COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
#                   COMMENT ON COLUMN ", schema, ".", table_name, ".contact_id IS 'Unique Identification';
#                   COMMENT ON COLUMN ", schema, ".", table_name, ".person_id IS 'Unique identification at the Person Level';
#                   COMMENT ON COLUMN ", schema, ".", table_name, ".patrol_station IS 'Original Column for the Patrol Station';
#                   COMMENT ON COLUMN ", schema, ".", table_name, ".patrol_station_recode IS 'Patrol Station Name Recoded to match cross-walk';
#                   COMMENT ON COLUMN ", schema, ".", table_name, ".asian IS 'Asian';
#                   COMMENT ON COLUMN ", schema, ".", table_name, ".black_african_american IS 'Black/African American';
#                   COMMENT ON COLUMN ", schema, ".", table_name, ".hispanic_latino_latina IS 'Hispanic or Latino';
#                   COMMENT ON COLUMN ", schema, ".", table_name, ".middle_eastern_south_asian IS 'Middle Eastern or South Asian';
#                   COMMENT ON COLUMN ", schema, ".", table_name, ".native_american IS 'Native American';
#                   COMMENT ON COLUMN ", schema, ".", table_name, ".pacific_islander IS 'Pacific Islander';
#                   COMMENT ON COLUMN ", schema, ".", table_name, ".white IS 'White';
#                   ")
# #dbSendQuery(con2, comment)

### LASD SPA JOIN ---------
# get individual records from postgres
lasd_filter <- st_read(con2, query = "SELECT * FROM bv_2023.lasd_youth_arrests_2022")

# merge to SPA, select columns we want
lasd_spa <- lasd_filter %>% left_join(crosswalk_pubsafety_stations_spas_2022, by = c("patrol_station_recode" = "station")) %>% select(contact_id, person_id, patrol_station, patrol_station_recode,full_address, city, state, asian, black_african_american, hispanic_latino_latina, 'middle_eastern_south_asian', native_american, pacific_islander, white, spa, spa_name, prc_area)

# # these don't have a SPA and probably need to be geo-coded
# lasd_na_agency <- lasd_filter %>% left_join(crosswalk_pubsafety_stations_spas_2022, by = c("patrol_station_recode" = "station")) %>% filter(is.na(spa)) %>% group_by(patrol_station_recode) %>% summarize(count = n())
# 
# lasd_na_geocode <- lasd_filter %>% left_join(crosswalk_pubsafety_stations_spas_2022, by = c("patrol_station_recode" = "station")) %>% filter(is.na(spa)) %>% select(contact_id, person_id, street_number:zip_code) 

# table_name <- "lasd_youth_arrests_togeocode"
# schema <- 'bv_2023'
# source <- "LASD data that needs to be geo-coded"

#dbWriteTable(con2, c(schema, table_name), lasd_na_geocode, 
#            overwrite = FALSE, row.names = FALSE)

#comment on table and columns
#comment <- paste0("
#                  COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
#                  COMMENT ON COLUMN ", schema, ".", table_name, ".contact_id IS 'Unique Identification';
#                  COMMENT ON COLUMN ", schema, ".", table_name, ".street_number IS 'Street Number';
#                  COMMENT ON COLUMN ", schema, ".", table_name, ".street_direction IS 'Direction';
#                  COMMENT ON COLUMN ", schema, ".", table_name, ".street_name IS 'Street Name';
#                 COMMENT ON COLUMN ", schema, ".", table_name, ".street_type IS 'Avenue, Boulevard, Drive, Highway, Plaza, Road, street, way';
#                  COMMENT ON COLUMN ", schema, ".", table_name, ".suite IS 'Suite';
#                  COMMENT ON COLUMN ", schema, ".", table_name, ".cross_street IS 'Nearest Cross-street';
#                  COMMENT ON COLUMN ", schema, ".", table_name, ".landmark IS 'Landmark';
#                  COMMENT ON COLUMN ", schema, ".", table_name, ".full_address IS 'Address';
#                  COMMENT ON COLUMN ", schema, ".", table_name, ".city IS 'City';
#                  COMMENT ON COLUMN ", schema, ".", table_name, ".state IS 'State';
#                   COMMENT ON COLUMN ", schema, ".", table_name, ".zip_code IS 'Zip Codes';
#                  ")
#dbSendQuery(con2, comment)

# pull in geocoded results and merge to spas
lasd_geocoded <- st_read(con2, query = "SELECT * FROM bv_2023.lasd_youth_arrests_geocoded")
lasd_spa_geocoded <- lasd_geocoded %>% st_join(spa) %>% as.data.frame()
lasd_spa_geocoded<-lasd_spa_geocoded%>%filter(location_type!='APPROXIMATE')%>%filter(!is.na(spa_name)) # filter out addresses that were generalized the most


#Abbreviations: 
## Sources:
# https://pars.lasd.org/Viewer/Manuals/12547/Content/14097#!
#https://data.lacounty.gov/maps/lacounty::sheriff-and-police-stations/about

# LAPD Data cleaning ------------------------------------------------------
# 
# # merge reporting districts to: crosswalk_lapd_district_spas_2022 
# lapd <- dbGetQuery(con, "SELECT * FROM crime_and_justice.lapd_arrests_2020_2023") #128/1263 missing SPAS for reporting districts, a rate of 10%
# 
# 
# ## filter for 2022 youth arrests
# lapd_filter <- lapd %>% filter(grepl('2022', arrest_date) & age <= 24)
# 
# # recode coordinates for reporting id 6333848: from 0.000 and 0.000 to 34.1837 and -118.4476. the address is 14400 Erwin St Mall which exists in this data-frame and it has coordinates (34.1837, -118.4476).
# 
# lapd_filter <- lapd_filter %>% mutate(lat = ifelse(report_id == "6333848", "34.1837", lat),
#                        lon = ifelse(report_id == "6333848", "-118.4476", lon)) %>% select(report_id, arrest_date, area_name, reporting_district, descent_code, lat, lon, age)


# indicator <- "LAPD Youth Arrests"
# table_name = "lapd_youth_arrests_2022"
# schema = "bv_2023"
# 
# 
# source <- "Created with  W:\\Project\\OSI\\Bold Vision\\BV 2023\\R\\Systems Impact\\si_youth_arrest.R
# LAPD Youth Arrests 0-24: https://data.lacity.org/Public-Safety/Arrest-Data-from-2020-to-Present/amvf-fr72/about_data
# "
# 
# 
# #dbWriteTable(con2, c(schema, table_name), lapd_filter, overwrite = FALSE, row.names = FALSE)
# 
# #comment on table and columns
# comment <- paste0("
#                   COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
#                   COMMENT ON COLUMN ", schema, ".", table_name, ".report_id IS 'Unique Identification Column';
#                   COMMENT ON COLUMN ", schema, ".", table_name, ".arrest_date IS 'Date of the Arrest';
#                   COMMENT ON COLUMN ", schema, ".", table_name, ".area_name IS 'Reporting District Name';
#                   COMMENT ON COLUMN ", schema, ".", table_name, ".reporting_district IS 'Reporting District';
#                   COMMENT ON COLUMN ", schema, ".", table_name, ".descent_code IS 'Race/Category';
#                   COMMENT ON COLUMN ", schema, ".", table_name, ".lat IS 'Latitude';
#                   COMMENT ON COLUMN ", schema, ".", table_name, ".lon IS 'Longitude';
#                   COMMENT ON COLUMN ", schema, ".", table_name, ".age IS 'Age';
#                   ")


#dbSendQuery(con2, comment)

### LAPD SPA Join ---------
# pull data from postgres now
lapd_filter <- st_read(con2, query = "SELECT * FROM bv_2023.lapd_youth_arrests_2022")

# merge with crosswalk                       
lapd_spa <- lapd_filter %>% left_join(crosswalk_lapd_district_spas_2022, by = c("reporting_district" = "name")) %>% filter(!is.na(spa_name)) %>% select(report_id, descent_code,  spa,spa_name)

# Some observations here don't have spatial coordinates.

lapd_spa_na <- lapd_filter %>% left_join(crosswalk_lapd_district_spas_2022, by = c("reporting_district" = "name")) %>% filter(is.na(spa_name)) 

lapd_spa_na_sf <- st_as_sf(x = lapd_spa_na, 
                           coords = c("lon", "lat"), 
                           crs = "EPSG:4326")

# transform to 3310

lapd_spa_na_sf <- st_transform(lapd_spa_na_sf, crs = 3310)

# rejoin this back to original spa df
lapd_spa_na_coordinates <- lapd_spa_na_sf %>% st_join(spa) %>% as.data.frame() %>% select(-spa.x, -spa_name.x) %>% rename(spa = spa.y, spa_name = spa_name.y)  %>% select(report_id, descent_code,  spa,spa_name)

lapd_spa <- bind_rows(lapd_spa,lapd_spa_na_coordinates) %>% select(report_id, descent_code,  spa,spa_name)


# RIPA data cleaning ------------------------------------------------------
# # merge to crosswalk_pubsafety_stations_spas_2022
# ripa <- dbGetQuery(con, "SELECT * FROM crime_and_justice.cadoj_ripa_la_2022")
# 
# # make lower-case
# ripa$agency_name <- str_to_title(ripa$agency_name)
# # remove PD from agency name
# ripa <- ripa %>% mutate(agency_name = gsub(" Pd", "", agency_name))
# 
# # according to the cross-walk, all Long Beach divisions belong to South Bay Spa. We will just recode 'Long Beach' to South Division division 
# ripa <- ripa %>% mutate(agency_name = recode(agency_name, 'Long Beach' = "Long Beach South Division"))
# 
# # filter for 2022 youth arrests and remove sheriff/LAPD 
# ripa_filter <- ripa %>% filter(grepl('2022', date_of_stop) & age <=24 & !grepl('Los Angeles Co Sd', agency_name) & !grepl('Lapd', agency_name))
# 
# # filter for arrests only
# 
# ripa_filter <- ripa_filter %>% filter(ros_custodial_warrant == "1" | ros_custodial_without_warrant == "1") %>% select(doj_record_id, person_number, agency_name, closest_city, age, starts_with("rae")) 
# 
# # push table for youth diversion
# 
# indicator <- "RIPA Youth Arrests including agency name"
# table_name = "ripa_youth_arrests_2022"
# schema = "bv_2023"
# 
# 
# source <- "Created with  W:\\Project\\OSI\\Bold Vision\\BV 2023\\R\\Systems Impact\\si_youth_arrest.R
# RIPA Youth Arrests 0-24: c"

# dbWriteTable(con2, c(schema, table_name), ripa_filter, overwrite = FALSE, row.names = FALSE)

#comment on table and columns
# comment <- paste0("
#                   COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
#                   COMMENT ON COLUMN ", schema, ".", table_name, ".doj_record_id IS 'Department of Justice Record ID';
#                   COMMENT ON COLUMN ", schema, ".", table_name, ".person_number IS 'Unique Person ID when combined with doj_record_id';
#                   COMMENT ON COLUMN ", schema, ".", table_name, ".agency_name IS 'Law enforcement agency in LA County excluding LASD and LAPD';
#                   COMMENT ON COLUMN ", schema, ".", table_name, ".age IS 'Age';
#                   COMMENT ON COLUMN ", schema, ".", table_name, ".rae_full IS 'Perceived Race or Ethnicityof Person stopped: Individuals perceived as more than one race/ethnicity are counted as Multiracial for this variable. 1 = Asian, 2 = Black, 3 = Hispanic/Latino, 4 = Middle Eastern/South Asian, 5 = Native American, 6 = Pacific Islander, 7 = White, 8 = Multiracial';
#       
#                   ")

# dbSendQuery(con2, comment)
## RIPA SPA Join ------
# pull postgres table
ripa_filter <- dbGetQuery(con2, "SELECT * FROM bv_2023.ripa_youth_arrests_2022")

## SPA level analysis

ripa_spa_notna <- ripa_filter %>% left_join(crosswalk_pubsafety_stations_spas_2022,  by = c("agency_name" = "station")) %>% select(doj_record_id, person_number, agency_name, closest_city, starts_with("rae"), spa, spa_name, prc_area) %>% filter(!is.na(spa_name))


ripa_spa_na <-  ripa_filter %>% left_join(crosswalk_pubsafety_stations_spas_2022,  by = c("agency_name" = "station")) %>% select(doj_record_id, person_number, agency_name, closest_city, starts_with("rae"), spa, spa_name, prc_area) %>% filter(is.na(spa_name)) %>% mutate(
  spa_name = case_when(agency_name == 'Bur-Glen-Pasa Airport' ~ "San Fernando",
                       agency_name == 'Compton Community College' ~ "South",
                       agency_name == 'Csu-Dominguez Hills' ~ "South Bay",
                       agency_name == 'La City Park Rangers' ~ NA,
                       agency_name == 'La Dept Of Airport' ~ "West", 
                       agency_name == 'La Port Police' ~ "South Bay",
                       agency_name == 'Lausd-Grand' ~ NA, 
                       agency_name == 'Pasadena Comm College' ~ "San Gabriel",
                       agency_name == 'St Univ-Los Angeles' ~ "Metro",
                       agency_name == 'St Univ-Northridge' ~ "San Fernando",
                       agency_name == 'St Univ-Pomona' ~ "San Gabriel",
                       agency_name == 'State Univ-Long Beach' ~ "South Bay",
                       agency_name == 'Uc-Los Angeles' ~ "West"),
  
  
  spa = case_when(spa_name == 'San Fernando' ~ "2",
                  spa_name == 'San Gabriel' ~ "3",
                  spa_name == 'Metro' ~ "4",
                  spa_name == 'West' ~ "5",
                  spa_name == 'South' ~ "6",
                  spa_name == 'South Bay' ~ "8",
  )
)


ripa_spa <- rbind(ripa_spa_notna, ripa_spa_na)


# LA County Race Analysis ----------------------------------------------------------------

# Get Race Data -----------------------------------------------------------
# 1) Get total race counts for RIPA, LASD, LAPD
# 2) Join data together to get total count per youth 
# 3) Divide by total youth raced population then multiply by 1,000

# Dictionary: https://data-openjustice.doj.ca.gov/sites/default/files/dataset/2024-01/RIPA%20Dataset%20Read%20Me%202022.pdf

# 1 Asian
# 2 Black/African American
# 3 Hispanic/Latino
# 4 Middle Eastern/South Asian
# 5 Native American
# 6 Pacific Islander
# 7 White
# 8 Multiracial 


# we will be using the filtered ripa data so we don't come across potential duplicates for SPA/patrol stations

ripa_total <- ripa_filter %>% mutate(race = "total") %>% group_by(race) %>% summarize(count = n())

ripa_latino <- ripa_filter %>% filter(rae_hispanic_latino == "1") %>% mutate(race = "latino") %>% group_by(race) %>% summarize(count = n())

ripa_nh_white <- ripa_filter %>% filter(rae_full == "7" & rae_hispanic_latino == "0") %>% group_by(rae_full) %>% summarize(count = n()) %>% mutate(race = "nh_white")%>% select(-starts_with("rae"))

ripa_nh_black <- ripa_filter %>% filter(rae_full == "2" & rae_hispanic_latino == "0") %>% group_by(rae_full) %>% summarize(count = n()) %>% mutate(race = "nh_black") %>% select(-starts_with("rae"))

ripa_nh_asian <- ripa_filter %>% filter(rae_full == "1" & rae_hispanic_latino == "0") %>% group_by(rae_full) %>% summarize(count = n()) %>% mutate(race = "nh_asian") %>% select(-starts_with("rae"))

ripa_nh_twoormor <- ripa_filter %>% filter(rae_full == "8" & rae_hispanic_latino == "0") %>% group_by(rae_full) %>% summarize(count = n()) %>% mutate(race = "nh_twoormor") %>% select(-starts_with("rae"))

ripa_aian <- ripa_filter %>% filter(rae_native_american == "1") %>% group_by(rae_native_american) %>% summarize(count = n()) %>% mutate(race = "aian") %>% select(-starts_with("rae")) # includes two or more

ripa_pacisl <- ripa_filter %>% filter(rae_pacific_islander == "1") %>% group_by(rae_pacific_islander) %>% summarize(count = n()) %>% mutate(race = "pacisl")  %>% select(-starts_with("rae")) # includes two or more

ripa_swana <- ripa_filter %>% filter(rae_middle_eastern_south_asian == "1") %>% group_by(rae_middle_eastern_south_asian) %>% summarize(count = n()) %>% mutate(race = "swana") %>% select(-starts_with("rae")) # includes two or more

ripa_race <- rbind(ripa_total, ripa_latino, ripa_nh_white, ripa_nh_black, ripa_nh_asian, ripa_nh_twoormor, ripa_aian, ripa_pacisl, ripa_swana) %>% mutate(data = "ripa")


# Dictionary: https://data.lacity.org/Public-Safety/Arrest-Data-from-2020-to-Present/amvf-fr72/about_data

# Descent Code:
#A - Other Asian 
#B - Black 
#C - Chinese 
#D - Cambodian
#F - Filipino 
#G - Guamanian 
#H - Hispanic/Latin/Mexican 
#I - American Indian/Alaskan Native 
#J - Japanese 
#K - Korean 
#L - Laotian 
#O - Other 
#P - Pacific Islander 
#S - Samoan 
#U - Hawaiian 
#V - Vietnamese 
#W - White 
#X - Unknown 
#Z - Asian Indian  

# Descent recode:
# latino: Hispanic/Latin/Mexican 
# NH AIAN: American Indian/Alaskan Native 
# NH Asian: Chinese, Cambodian, Filipino,  Japanese,  Korean, Laotian, Vietnamese, Asian Indian
# NH Black: Black
# NH Two or More: ?
# NH Pacisl: Guamanian, Pacific Islander, Hawaiian, Samoan
# Nh Other: Other, Unknown 
# MENA/SSWANA
# NH White: White


lapd_total <- lapd_spa %>% mutate(race = "total") %>% group_by(race) %>% summarize(count = n())

lapd_latino <- lapd_spa %>% filter(descent_code == "H") %>% group_by(descent_code) %>% summarize(count = n()) %>% mutate(race = "latino") %>% select(race, count)

lapd_nh_white <- lapd_spa %>% filter(descent_code == "W") %>% group_by(descent_code) %>% summarize(count = n()) %>% mutate(race = "nh_white") %>% select(race, count)

lapd_nh_black <- lapd_spa %>% filter(descent_code == "B") %>% group_by(descent_code) %>% summarize(count = n()) %>% mutate(race = "nh_black") %>% select(race, count)

lapd_nh_asian <- lapd_spa %>% filter(descent_code %in% c("A", "C","D", "F", "J", "K", "L", "V", "Z")) %>% group_by(descent_code) %>% summarize(sub_group_count = n()) %>% mutate(race = "nh_asian", count = sum(sub_group_count)) %>% distinct(race, count)

lapd_nh_aian <- lapd_spa %>% filter(descent_code %in% c("I")) %>% group_by(descent_code) %>% summarize(count = n()) %>% mutate(race = "nh_aian") %>% select(race, count)

lapd_nh_pacisl <- lapd_spa %>% filter(descent_code %in% c("G", "P", "U", "S")) %>% group_by(descent_code) %>% summarize(sub_group_count = n()) %>% mutate(race = "nh_pacisl", count = sum(sub_group_count)) %>% distinct(race, count) 

lapd_nh_twoormor <- lapd_spa %>% filter(descent_code %in% c("O")) %>% group_by(descent_code) %>% summarize(sub_group_count = n()) %>% mutate(race = "nh_twoormor", count = sum(sub_group_count)) %>% distinct(race, count) 

lapd_race <- rbind(lapd_total, lapd_latino, lapd_nh_white, lapd_nh_black, lapd_nh_asian, lapd_nh_aian, lapd_nh_pacisl, lapd_nh_twoormor) %>% mutate(data = "lapd")



# LASD Data ---------------------------------------------------------------

lasd_spa_duplicates <- lasd_spa %>% group_by(contact_id, person_id) %>% summarize(count = n()) %>% filter(count >1) %>% arrange(desc(count))

## we are using lasd_filter since this has unique observations for each race because lasd spas has duplicates since some patrol stations belong to multiple spas

lasd_race_cols <- lasd_filter %>% select(contact_id, person_id, asian, black_african_american, hispanic_latino_latina, middle_eastern_south_asian, native_american, pacific_islander, white) 

lasd_total <- lasd_race_cols %>% mutate(race = "total") %>% group_by(race) %>% summarize(count = n())

lasd_latino <- lasd_race_cols %>% filter(hispanic_latino_latina == "Yes") %>% mutate(race = "latino") %>% group_by(race) %>% summarize(count = n())

lasd_nh_white <- lasd_race_cols %>% filter(asian == "No" & black_african_american == "No" & hispanic_latino_latina == "No" & middle_eastern_south_asian == "No" & native_american == "No" & pacific_islander == "No" & white == "Yes") %>% mutate(race = "nh_white") %>% group_by(race) %>% summarize(count = n())

lasd_nh_black <- lasd_race_cols %>% filter(asian == "No" & black_african_american == "Yes" & hispanic_latino_latina == "No" & middle_eastern_south_asian == "No" & native_american == "No" & pacific_islander == "No" & white == "No") %>% mutate(race = "nh_black") %>% group_by(race) %>% summarize(count = n())

lasd_nh_asian <- lasd_race_cols %>% filter(asian == "Yes" & black_african_american == "No" & hispanic_latino_latina == "No" & middle_eastern_south_asian == "No" & native_american == "No"& pacific_islander == "No" & white == "No") %>% mutate(race = "nh_asian") %>% group_by(race) %>% summarize(count = n())

lasd_aian <- lasd_race_cols %>% filter(native_american == "Yes") %>% mutate(race = "aian") %>% group_by(race) %>% summarize(count = n())

lasd_pacisl <- lasd_race_cols %>% filter(pacific_islander == "Yes") %>% mutate(race = "pacisl") %>% group_by(race) %>% summarize(count = n())

lasd_swana <- lasd_race_cols %>% filter(middle_eastern_south_asian == "Yes") %>% mutate(race = "swana") %>% group_by(race) %>% summarize(count = n())


# create multi-racial column where a person has more than 1 race
multiracial <- lasd_race_cols %>% pivot_longer(cols = asian:white, names_to = "race", values_to = "value") %>% group_by(contact_id, person_id) %>% filter(value == "Yes") %>% group_by(contact_id, person_id, value) %>% summarize(count = n()) %>% filter(count >1) %>% mutate(multiracial = "Yes") %>% ungroup() %>% select(contact_id, person_id, multiracial) 

lasd_nh_twoormor <- lasd_race_cols %>% left_join(multiracial) %>% filter(multiracial == "Yes" & hispanic_latino_latina == "No") %>% mutate(race = "nh_twoormor") %>% group_by(race) %>% summarize(count = n())

lasd_race <- rbind(lasd_total, lasd_latino, lasd_nh_white, lasd_nh_black, lasd_nh_asian, lasd_aian, lasd_pacisl, lasd_swana, lasd_nh_twoormor) %>% mutate(data = "lasd")

# combine all 3: 
df <- rbind(lasd_race, lapd_race, ripa_race)


# recoding 1 observation for AIAn and 4 for PACISL to alone or in combo but will need to include
df <- df %>% mutate(race = recode(race, nh_aian = "aian", 
                                  nh_pacisl = "pacisl"))

# calculate total sum
df_final <- df %>% group_by(race) %>% mutate(count = sum(count)) %>% distinct(race, count)

#rename swana to sswana to match pop estimates
df_final<-df_final%>%mutate(race=ifelse(race=="swana","sswana",race))

# Now get total pop and get rates. Then calculate ID, etc.

# pull county pop
county_pop <- dbGetQuery(con2, "SELECT * FROM bv_2023.acs_pums_multigeo_2021_youth_0_24_race_long") %>% filter(geolevel == "county") %>% select(raceeth, count) %>% rename(pop = count, race = raceeth)

# combine nh_twoormor with nh_other 

nh_twoormor_other <- county_pop %>% filter(race %in% c("nh_twoormor",  "nh_other")) %>% adorn_totals() %>% as.data.frame() %>% filter(race == "Total") %>% mutate(race = "nh_twoormor")

county_pop <- county_pop %>% filter(!(race %in% c("nh_other", "nh_twoormor"))) 

county_pop <- rbind(county_pop, nh_twoormor_other)

d_long <- df_final %>% left_join(county_pop) %>% mutate(rate = (count/pop)* 1000, geoid = "06037", county = "Los Angeles County")

# add bipoc
bipoc_pop<-county_pop[county_pop$race == "bipoc",2] #extract bipoc count from county population figures

bipoc <- d_long %>% filter(race %in% c("total",  "nh_white")) %>% mutate(subgroup = "bipoc") %>% group_by(subgroup) %>% summarize(
  count = diff(range(count)),
  pop = bipoc_pop,
  rate = (count/pop * 1000)
) %>% rename(race = subgroup) %>% mutate(geoid = "06037", county = "Los Angeles County")

d_long_bipoc <- rbind(d_long, bipoc)

# pivot wider including bipoc group

d <- d_long_bipoc %>% pivot_wider(
  names_from = race, 
  names_glue = "{race}_{.value}",
  values_from = count:rate
)

# rename bipoc rate so it doesn't get included in index of disparity
d  <- d %>% rename(bipoc_rate_ = bipoc_rate)

d$asbest = 'min'    #YOU MUST UPDATE THIS FIELD AS NECESSARY: assign 'min' or 'max'

d <- count_values(d) #calculate number of "_rate" values
d <- calc_best(d) #calculate best rates -- be sure to update previous line of code accordingly before running this function.
d <- calc_diff(d) #calculate difference from best
d <- calc_id(d) #calculate index of disparity

# rename bipoc rate again to normal
d <- d %>% rename(bipoc_rate = bipoc_rate_)


subgroup <- d %>% select(-county, -asbest, -best, -values_count, -index_of_disparity) %>% 
  pivot_longer(!geoid, names_to = "subgroup", values_to = "metric") %>% 
  mutate(variable = case_when(grepl("_rate", subgroup) ~ "rate",
                              grepl("_pop", subgroup) ~ "pop",
                              grepl("_diff", subgroup) ~ "diff", 
                              grepl("_count", subgroup) ~ "count")) %>%
  mutate(across(subgroup, str_replace, "_rate", "")) %>%
  mutate(across(subgroup, str_replace, "_pop", "")) %>%
  mutate(across(subgroup, str_replace, "_diff", "")) %>%
  mutate(across(subgroup, str_replace, "_count", "")) %>%
  pivot_wider(names_from = variable, values_from = metric) %>%
  left_join(d %>% select(geoid, best, index_of_disparity, values_count, asbest), by = "geoid")  %>%
  mutate(name = "Los Angeles County") %>% select(geoid, name, everything())

# SPA Analysis ------------------------------------------------------------

lasd_spa_count <- lasd_spa %>% group_by(patrol_station_recode, spa, spa_name, prc_area) %>% summarize(count = n()) %>% group_by(patrol_station_recode) %>% mutate(count_weighted = (prc_area*count)) %>% group_by(spa, spa_name) %>% summarize(count = sum(count_weighted)) %>% mutate(data = "lasd")
lasd_spa_count_geocoded<-lasd_spa_geocoded%>%group_by(spa,spa_name)%>%summarise(count=n())%>%mutate(data = "lasd_geocoded")

lapd_spa_count <- lapd_spa %>% group_by(spa, spa_name) %>% summarize(count = n()) %>% mutate(data = "lapd")

ripa_spa_count <- ripa_spa %>% group_by(spa, spa_name) %>% summarize(count = n()) %>% mutate(data = "ripa")

spa_data <- rbind(lasd_spa_count, lasd_spa_count_geocoded,lapd_spa_count, ripa_spa_count)


spa_df <- spa_data %>% group_by(spa, spa_name) %>% mutate(count = sum(count)) %>% distinct(spa, spa_name,count) %>% filter(!is.na(spa))


# get total youth pop
# pull tract pop
tract_pop <- dbGetQuery(con2, "SELECT * FROM bv_2023.dhc_tract_2020_youth_0_24_race")

# pull tract-SPA cross-walk
crosswalk_tract_spas <- dbGetQuery(con2, "SELECT * FROM bv_2023.crosswalk_tract_spas_2023") %>% rename(sub_id = geoid)

tract_pop_wide <- tract_pop %>% filter(race == "total") %>% mutate(race = paste0(race, "_pop"))     %>% pivot_wider(
  names_from = race,
  values_from = pop
) %>% rename(sub_id = geoid)

spa_youth <- tract_pop_wide %>% left_join(crosswalk_tract_spas)

#update target_id for the 3 tracts that fail in join (because they're water)
spa_youth["spa"][spa_youth["sub_id"] == "06037990100"] <- "5"
spa_youth["spa"][spa_youth["sub_id"] == "06037990200"] <- "8"
spa_youth["spa"][spa_youth["sub_id"] == "06037990300"] <- "8"

spa_total_pop <- spa_youth %>% group_by(spa) %>% 
  summarize(
    pop = sum(total_pop)
  )



df_region <- spa_df %>% left_join(spa_total_pop) %>% mutate(rate = (count/pop) * 1000) %>% rename(geoid = spa, name = spa_name)

df_region$asbest <- "min"

df_region <- df_region %>% ungroup() %>%  mutate(best = min(df_region$rate, na.rm=T),
                                                 diff = rate-best,
                                                 values_count = length(df_region$rate) - sum(is.na(df_region$rate))) %>%
  mutate(
    sumdiff = sum(diff, na.rm = TRUE), 
    index_of_disparity = (sumdiff / best) / (values_count - 1) * 100) %>% select(-sumdiff) 


# Send to Postgres ----------------------------------------------------------------

# Subgroup -----------------------------------------------------------

con3 <- connect_to_db("bold_vision")
table_name <- "si_youth_arrest_subgroup"
schema <- 'bv_2023'

indicator <- "Youth Arrests for youth 0-24 per 1K "
source <- "LASD Stops/arrests: https://data.lacounty.gov/datasets/ab74ffc494c94bbfaff3dd45c9678144_0/explore
https://data.lacounty.gov/explore?query=Sheriff%20Officer%20COntacts 
LAPD data:  https://data.lacity.org/Public-Safety/Arrest-Data-from-2020-to-Present/amvf-fr72 
CADOJ Ripa: : https://openjustice.doj.ca.gov/data
See QA doc for details: W:\\Project\\OSI\\Bold Vision\\BV 2023\\Documentation\\System Impact\\QA_Youth_Arrests.docx"


# dbWriteTable(con3, c(schema, table_name), subgroup,
#            overwrite = TRUE, row.names = FALSE)

#comment on table and columns

comment <- paste0("COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
                 COMMENT ON COLUMN ", schema, ".", table_name, ".geoid IS 'County number';
                 COMMENT ON COLUMN ", schema, ".", table_name, ".name IS 'County Name';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".subgroup IS 'Race Group. Includes rates for SSWANA as opposed to SWANA based on how RIPA data are collected. Rates for AIAN, PACISL, and LATINE may be depressed due to LAPD not collecting these groups as alone or in combo, unlike other data sources. Two or more races includes multiracial and another race';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".count IS 'indicator numerator';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".pop IS 'indicator denominator';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".diff IS 'indicator difference from the best';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".rate IS 'indicator rate per 1K';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".best IS 'best rate among race groups';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".index_of_disparity IS 'index of disparity';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".values_count IS 'number of values';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".asbest IS 'minimum or maximum as best';")
# dbSendQuery(con3, comment)


# SPA ---------------------------------------------------------------------

con3 <- connect_to_db("bold_vision")
table_name <- "si_youth_arrest_region"
schema <- 'bv_2023'

indicator <- "Youth Arrests for youth 0-24 per 1K"
source <- "LASD Stops/arrests: https://data.lacounty.gov/datasets/ab74ffc494c94bbfaff3dd45c9678144_0/explore
https://data.lacounty.gov/explore?query=Sheriff%20Officer%20COntacts 
LAPD data:  https://data.lacity.org/Public-Safety/Arrest-Data-from-2020-to-Present/amvf-fr72 
CADOJ Ripa: : https://openjustice.doj.ca.gov/data
See QA doc for details: W:\\Project\\OSI\\Bold Vision\\BV 2023\\Documentation\\System Impact\\QA_Youth_Arrests.docx"

# dbWriteTable(con3, c(schema, table_name), df_region,
#         overwrite = TRUE, row.names = FALSE)

#comment on table and columns
comment <- paste0("
                  COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".geoid IS 'SPA number';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".name IS 'SPA Name';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".pop IS 'indicator denominator';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".count IS 'indicator numerator';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".rate IS 'indicator rate per 1K';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".asbest IS 'minimum or maximum as best';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".best IS 'best rate among regions';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".diff IS 'indicator difference from the best';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".values_count IS 'number of values';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".index_of_disparity IS 'index of disparity';
                  ")
# dbSendQuery(con3, comment)

#disconnect
# dbDisconnect(con3)


