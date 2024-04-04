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

source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("rda_shared_data")
con2 <- connect_to_db("bold_vision")

options(scipen = 100)

# diversion_race <- read_excel("W:/Data/Crime and Justice/LA County Dept of Youth Development/updated DYD Diversion Data for Bold Vision 2.23.24.xlsx", 
#      sheet = "source table (race)") %>% clean_names()
# 
# diversion_race<-diversion_race%>%rename(latinx=identified_as_latinx)
# 
# indicator <- "Youth Diversion"
# table_name = "lac_dyd_youth_diversion_race"
# schema = "bv_2023"
# 
# source <- "Created with  W:\\Project\\OSI\\Bold Vision\\BV 2023\\R\\Systems Impact\\si_youth_diversion.R
# Youth Diversion by Race for Ages 0-20 - Individual records per youth. Source: LA County Department of Youth Development received by special request in February 2024"
# 
# 
# # dbWriteTable(con2, c(schema, table_name), diversion_race, overwrite = FALSE, row.names = FALSE)
# 
# 
# #comment on table and columns
# comment <- paste0("
#                   COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
#                   COMMENT ON COLUMN ", schema, ".", table_name, ".referring_agency IS 'Agency Referred';
#                   COMMENT ON COLUMN ", schema, ".", table_name, ".race IS 'Race of Youth';
#                   COMMENT ON COLUMN ", schema, ".", table_name, ".latinx IS 'Whether the person identifies as Latinx or not';
#                   COMMENT ON COLUMN ", schema, ".", table_name, ".overall_status IS 'Status of Diversion: Substantially Complete, Youth Did not Enroll, Enrolled, Enrollment Pending, Did Not Substantially Complete';
#                   ")
# 
# # dbSendQuery(con2, comment)
# 
# 
# 
# diversion_zip <- read_excel("W:/Data/Crime and Justice/LA County Dept of Youth Development/updated DYD Diversion Data for Bold Vision 2.23.24.xlsx", 
#      sheet = "source table (zipcodes)")
# 
# indicator <- "Youth Diversion"
# table_name = "lac_dyd_youth_diversion_zip"
# schema = "bv_2023"
# 
# source <- "Created with  W:\\Project\\OSI\\Bold Vision\\BV 2023\\R\\Systems Impact\\si_youth_diversion.R
# Youth Diversion by ZIP Code for Ages 0-20 - Individual records per youth. Source: LA County Department of Youth Development received by special request in February 2024"
# 
# # dbWriteTable(con2, c(schema, table_name), diversion_zip, overwrite = FALSE, row.names = FALSE)
# 
# #comment on table and columns
# comment <- paste0("
#                   COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
#                   COMMENT ON COLUMN ", schema, ".", table_name, ".referring_agency IS 'Agency Referred';
#                   COMMENT ON COLUMN ", schema, ".", table_name, ".youth_zip IS 'Zip Code';
#                   COMMENT ON COLUMN ", schema, ".", table_name, ".overall_status IS 'Status of Diversion: Substantially Complete, Youth Did not Enroll, Enrolled, Enrollment Pending, Did Not Substantially Complete';
#                   ")
# # dbSendQuery(con2, comment)
# 
# # By Race ---------------------------------------------------------------
# ## LA County Diversion Data---------------------------------------------------------------
youth_diversion_race <- dbGetQuery(con2, "SELECT * FROM bv_2023.lac_dyd_youth_diversion_race")

## filter for enrolled/completed
youth_diversion2 <- youth_diversion_race %>% filter(overall_status %in% c("Substantially Complete", "Enrolled"))

youth_diversion_total <-  youth_diversion2 %>% mutate(race = "total") %>% group_by(race) %>% summarize(count = n())

youth_diversion_latino <-  youth_diversion2 %>% filter(latinx == "Latinx") %>% mutate(race = "latino") %>% group_by(race) %>% summarize(count = n())

youth_diversion_nh_api <-  youth_diversion2 %>% filter(race == "API" & latinx == "Not Identified as Latinx") %>% group_by(race) %>% summarize(count = n()) %>% mutate(race = recode(race, 'API' = "nh_api"))
# two or more includes other now because of LAPD arrest data and decision to group other and two or more for this indicator
youth_diversion_nh_twoormor <-  youth_diversion2 %>% filter(race %in% c("Bi/Multiracial","Other") & latinx == "Not Identified as Latinx") %>% mutate(race = "nh_twoormor") %>% group_by(race) %>% summarize(count = n()) 

youth_diversion_nh_black <-  youth_diversion2 %>% filter(race == "Black or African American" & latinx == "Not Identified as Latinx") %>% group_by(race) %>% summarize(count = n()) %>% mutate(race = "nh_black")

youth_diversion_sswana <-  youth_diversion2 %>% filter(race == "Middle Eastern or South Asian") %>% group_by(race) %>% summarize(count = n()) %>% mutate(race = "sswana")

youth_diversion_aian <-  youth_diversion2 %>% filter(race == "Native American or Alaska Native") %>% group_by(race) %>% summarize(count = n()) %>% mutate(race = "aian")

youth_diversion_nh_white <-  youth_diversion2 %>% filter(race == "White" & latinx == "Not Identified as Latinx") %>% group_by(race) %>% summarize(count = n()) %>% mutate(race = "nh_white")

# check
qa<-youth_diversion2%>%group_by(race,latinx)%>%summarise(count=n())
# Unknown/Declined to Answer - 10 youth are unknown or declined to answer AND did not identify as latinx

youth_diversion_count <- rbind(youth_diversion_total, youth_diversion_latino, youth_diversion_nh_api, youth_diversion_nh_twoormor, youth_diversion_nh_black, youth_diversion_sswana, youth_diversion_aian, youth_diversion_nh_white)

## LASD Arrest Data --------------------------------------------------------
lasd_youth_arrests <- dbGetQuery(con2, "SELECT * FROM bv_2023.lasd_youth_arrests_2022")

# filter ages 0-20
lasd_youth_arrests <- lasd_youth_arrests %>% filter(age <= 20)

lasd_total <- lasd_youth_arrests %>% mutate(race = "total") %>% group_by(race) %>% summarize(count = n())

lasd_latino <- lasd_youth_arrests %>% filter(hispanic_latino_latina == "Yes") %>% mutate(race = "latino") %>% group_by(race) %>% summarize(count = n())

lasd_nh_white <- lasd_youth_arrests %>% filter(asian == "No" & black_african_american == "No" & hispanic_latino_latina == "No" & middle_eastern_south_asian == "No" & native_american == "No" & pacific_islander == "No" & white == "Yes") %>% mutate(race = "nh_white") %>% group_by(race) %>% summarize(count = n())

lasd_nh_black <- lasd_youth_arrests %>% filter(asian == "No" & black_african_american == "Yes" & hispanic_latino_latina == "No" & middle_eastern_south_asian == "No" & native_american == "No" & pacific_islander == "No" & white == "No") %>% mutate(race = "nh_black") %>% group_by(race) %>% summarize(count = n())

lasd_nh_asian <- lasd_youth_arrests %>% filter(asian == "Yes" & black_african_american == "No" & hispanic_latino_latina == "No" & middle_eastern_south_asian == "No" & native_american == "No"& pacific_islander == "No" & white == "No") %>% mutate(race = "nh_asian") %>% group_by(race) %>% summarize(count = n())

lasd_nh_pacisl <- lasd_youth_arrests %>% filter(asian == "No" & black_african_american == "No" & hispanic_latino_latina == "No" & middle_eastern_south_asian == "No" & native_american == "No"& pacific_islander == "Yes" & white == "No") %>% mutate(race = "nh_pacisl") %>% group_by(race) %>% summarize(count = n())

# count sum of non-hispanic pacisl + non-hispanic asian
lasd_nh_api <- rbind(lasd_nh_asian, lasd_nh_pacisl) %>% mutate(count= sum(count), 
                                                               race = "nh_api") %>% distinct(race, count)

lasd_aian <- lasd_youth_arrests %>% filter(native_american == "Yes") %>% mutate(race = "aian") %>% group_by(race) %>% summarize(count = n())

lasd_sswana <- lasd_youth_arrests %>% filter(middle_eastern_south_asian == "Yes") %>% mutate(race = "sswana") %>% group_by(race) %>% summarize(count = n())

# create multi-racial column where a person has more than 1 race
multiracial <- lasd_youth_arrests %>% pivot_longer(cols = asian:white, names_to = "race", values_to = "value") %>% group_by(contact_id, person_id) %>% filter(value == "Yes") %>% group_by(contact_id, person_id, value) %>% summarize(count = n()) %>% filter(count >1) %>% mutate(multiracial = "Yes") %>% ungroup() %>% select(contact_id, person_id, multiracial) 

lasd_nh_twoormor <- lasd_youth_arrests %>% left_join(multiracial) %>% filter(multiracial == "Yes" & hispanic_latino_latina == "No") %>% mutate(race = "nh_twoormor") %>% group_by(race) %>% summarize(count = n())

lasd_race <- rbind(lasd_total, lasd_latino, lasd_nh_white, lasd_nh_black, lasd_nh_api, lasd_aian, lasd_sswana, lasd_nh_twoormor) %>% mutate(data = "lasd")

## LAPD Arrest Data --------------------------------------------------------

lapd_youth_arrests <- dbGetQuery(con2, "SELECT * FROM bv_2023.lapd_youth_arrests_2022")

# filter for ages 0-20
lapd_youth_arrests  <- lapd_youth_arrests  %>% filter(age <=20)

lapd_total <-lapd_youth_arrests %>% mutate(race = "total") %>% group_by(race) %>% summarize(count = n())

lapd_latino <-lapd_youth_arrests %>% filter(descent_code == "H") %>% group_by(descent_code) %>% summarize(count = n()) %>% mutate(race = "latino") %>% select(race, count)

lapd_nh_white <-lapd_youth_arrests %>% filter(descent_code == "W") %>% group_by(descent_code) %>% summarize(count = n()) %>% mutate(race = "nh_white") %>% select(race, count)

lapd_nh_black <-lapd_youth_arrests %>% filter(descent_code == "B") %>% group_by(descent_code) %>% summarize(count = n()) %>% mutate(race = "nh_black") %>% select(race, count)

lapd_nh_api <-lapd_youth_arrests %>% filter(descent_code %in% c("A", "C","D", "F", "J", "K", "L", "V", "Z", "G", "P", "U", "S")) %>% group_by(descent_code) %>% summarize(sub_group_count = n()) %>% mutate(race = "nh_api", count = sum(sub_group_count)) %>% distinct(race, count)

lapd_nh_aian <-lapd_youth_arrests %>% filter(descent_code %in% c("I")) %>% group_by(descent_code) %>% summarize(count = n()) %>% mutate(race = "nh_aian") %>% select(race, count)

lapd_nh_twoormor <- lapd_youth_arrests %>% filter(descent_code=="O") %>% group_by(descent_code) %>% summarize(count = n()) %>% mutate(race = "nh_twoormor") %>% select(race, count)

lapd_race <- rbind(lapd_total, lapd_latino, lapd_nh_white, lapd_nh_black, lapd_nh_api, lapd_nh_aian, lapd_nh_twoormor) %>% mutate(data = "lapd")


## RIPA Arrest Data --------------------------------------------------------

ripa_youth_arrests <- dbGetQuery(con2, "SELECT * FROM bv_2023.ripa_youth_arrests_2022")

## filter for 0-20 (for youth diversion)

ripa_youth_arrests <- ripa_youth_arrests %>% filter(age<=20)

ripa_total <- ripa_youth_arrests %>% mutate(race = "total") %>% group_by(race) %>% summarize(count = n())

ripa_latino <- ripa_youth_arrests %>% filter(rae_hispanic_latino == "1") %>% mutate(race = "latino") %>% group_by(race) %>% summarize(count = n())

ripa_nh_white <- ripa_youth_arrests %>% filter(rae_full == "7" & rae_hispanic_latino == "0") %>% group_by(rae_full) %>% summarize(count = n()) %>% mutate(race = "nh_white")%>% select(-starts_with("rae"))

ripa_nh_black <- ripa_youth_arrests %>% filter(rae_full == "2" & rae_hispanic_latino == "0") %>% group_by(rae_full) %>% summarize(count = n()) %>% mutate(race = "nh_black") %>% select(-starts_with("rae"))

ripa_nh_api <- ripa_youth_arrests %>% filter(rae_full == "1" | rae_full == "6") %>% filter(rae_hispanic_latino == "0")   %>% mutate(race = "nh_api") %>% group_by(race) %>% summarize(count = n())

ripa_nh_twoormor <- ripa_youth_arrests %>% filter(rae_full == "8" & rae_hispanic_latino == "0") %>% group_by(rae_full) %>% summarize(count = n()) %>% mutate(race = "nh_twoormor") %>% select(-starts_with("rae"))

ripa_aian <- ripa_youth_arrests %>% filter(rae_native_american == "1") %>% group_by(rae_native_american) %>% summarize(count = n()) %>% mutate(race = "aian") %>% select(-starts_with("rae")) # includes two or more

ripa_sswana <- ripa_youth_arrests %>% filter(rae_middle_eastern_south_asian == "1") %>% group_by(rae_middle_eastern_south_asian) %>% summarize(count = n()) %>% mutate(race = "sswana") %>% select(-starts_with("rae")) # includes two or more

ripa_race <- rbind(ripa_total, ripa_latino, ripa_nh_white, ripa_nh_black, ripa_nh_api, ripa_nh_twoormor, ripa_aian, ripa_sswana) %>% mutate(data = "ripa")


# combine all 3 arrest data, rename nh_aian to aian (because LAPD does not capture this data)
df <- rbind(lasd_race, lapd_race, ripa_race) %>% rename(pop = count) %>% mutate(race = recode(race, nh_aian = "aian"))


# calculate total sum
youth_arrest_count <- df %>% group_by(race) %>% summarise(pop = sum(pop))

# combine arrest with diversion
df_combined <- youth_arrest_count %>% left_join(youth_diversion_count) %>% mutate(rate = (count/pop)* 1000, geoid = "06037", county = "Los Angeles County")

# add bipoc

bipoc <- df_combined %>% filter(race %in% c("total",  "nh_white")) %>% mutate(subgroup = "bipoc") %>% group_by(subgroup) %>% summarize(
  count = diff(range(count)),
  pop = diff(range(pop)),
  rate = (count/pop * 1000)
) %>% rename(race = subgroup) %>% mutate(geoid = "06037", county = "Los Angeles County")


df_combined <- rbind(df_combined, bipoc)

# pivot wider

d <- df_combined %>% pivot_wider(
  names_from = race, 
  names_glue = "{race}_{.value}",
  values_from = pop:rate
)
#rename bipoc and nh_twoormore rate so it doesn't get included in index of disparity
#nh_twoormor has a really high rate, likely due to discrepancy between self-reported data and officer-reported, but also because of issues in other and two or more categories
d  <- d %>% rename(bipoc_rate_ = bipoc_rate, nh_twoormor_rate_=nh_twoormor_rate)

d$asbest = 'max'    #YOU MUST UPDATE THIS FIELD AS NECESSARY: assign 'min' or 'max'

d <- count_values(d) #calculate number of "_rate" values
d <- calc_best(d) #calculate best rates -- be sure to update previous line of code accordingly before running this function.
d <- calc_diff(d) #calculate difference from best
d <- calc_id(d) #calculate index of disparity

# rename bipoc rate again to normal
d <- d %>% rename(bipoc_rate = bipoc_rate_,nh_twoormor_rate=nh_twoormor_rate_)

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


# By SPA ---------------------------------------------------------------------

## Data-sets

youth_diversion_zip <- dbGetQuery(con2, "SELECT * FROM bv_2023.lac_dyd_youth_diversion_zip") %>% mutate(youth_zip = as.character(youth_zip))

youth_diversion_zip_filter <- youth_diversion_zip %>% filter(overall_status %in% c("Substantially Complete", "Enrolled"))

crosswalk_zip_spas <- dbGetQuery(con2, "SELECT * FROM bv_2023.crosswalk_zip_spas_2023")

spa <- st_read(con, query = "SELECT * FROM geographies_la.la_county_service_planning_areas_2022")  
colnames(spa) <- tolower(colnames(spa))

# change crs to 3310
spa <- st_transform(spa, crs = 3310)

## LAPD District/SPAS crosswalk
crosswalk_lapd_district_spas_2022 <- dbGetQuery(con2, "SELECT * FROM bv_2023.crosswalk_lapd_district_spas_2022")

## Division/Agency/SPAs crosswalk ------------
crosswalk_pubsafety_stations_spas_2022 <- dbGetQuery(con2, "SELECT station, spa, spa_name, prc_area FROM bv_2023.crosswalk_pubsafety_stations_spas_2022") %>% arrange(station)


# calculate analysis at SPA level, multiply by the intersection of prc_area
youth_diversion_spa <- youth_diversion_zip_filter %>% left_join(crosswalk_zip_spas, by = c("youth_zip" = "zipcode"))  %>% group_by(youth_zip, spa, spa_name, prc_zip_code_area) %>% summarize(count = n()) %>% group_by(youth_zip) %>% mutate(count_weighted = (prc_zip_code_area * count)) %>% group_by(spa, spa_name) %>% summarize(count = sum(count_weighted)) %>% filter(!is.na(spa))


# these have missing cases in zip-spa crosswalk. We need to use referring agency and merge with the safety stations cross-walk. I needed to clean the data since not everything matches up 100%

youth_diversion_na <- youth_diversion_zip_filter %>% left_join(crosswalk_zip_spas, by = c("youth_zip" = "zipcode")) %>% filter(is.na(spa)) %>%mutate(referring_agency = 
                                                                                                                                                       gsub("LA County DA - |Office", "", referring_agency))  %>% mutate(referring_agency = recode(referring_agency, 'LASD Industry [YDD]' = "Industry", 
                                                                                                                                                                                                                                                   'LASD Lancaster [YDD]' = "Lancaster")) %>% mutate(referring_agency = str_trim(referring_agency)) %>% select(-spa, -spa_name)


# clean name
crosswalk_pubsafety_stations_spas_2022 <- crosswalk_pubsafety_stations_spas_2022 %>% mutate(referring_agency = gsub(
  "Police|Sheriff", "", station)) %>% mutate(referring_agency = str_trim(referring_agency),
                                             referring_agency = gsub("\\s+"," ",referring_agency))


# fill in missing SPAs  with crosswalk
youth_diversion_agency <- youth_diversion_na %>% left_join(crosswalk_pubsafety_stations_spas_2022) %>% mutate(
  spa_name_2 = case_when(referring_agency == 'Antelope Valley' ~ "Antelope Valley",
                         referring_agency == 'Eastlake' ~ "East",
                         referring_agency == 'Sylmar' ~ "San Fernando"),
  
  
  
  spa_2 = case_when(spa_name_2 == 'Antelope Valley' ~ "1",
                    spa_name_2 == 'San Fernando' ~ "2",
                    spa_name_2 == 'East' ~ "7")
) %>% mutate(spa_name = ifelse(is.na(spa_name), spa_name_2, spa_name),
             spa = ifelse(is.na(spa), spa_2, spa)) %>% select(-spa_name_2, -spa_2)

youth_diversion_agency_spa <- youth_diversion_agency %>% group_by(referring_agency, spa, spa_name, prc_area) %>% summarize(count = n()) %>% group_by(referring_agency) %>% mutate(count_weighted = ifelse(!is.na(prc_area), (prc_area*count), count)) %>% group_by(spa, spa_name) %>% summarize(count = sum(count_weighted)) 


## add youth diversion by zip and agency together

df_diversion_spa <- rbind(youth_diversion_spa, youth_diversion_agency_spa) %>% group_by(spa, spa_name) %>% summarize(count = sum(count))

## Calculate denominator, youth arrests by SPA----------------------------------------------------------------

# merge LASD, LAPD, RIPA youth arrest with cross-walk, assign SPAS to agency (QA'ed in youth arrests script), 


### LASD_SPA lines 440----------------------------------------------------------------
lasd_spa <- lasd_youth_arrests %>% left_join(crosswalk_pubsafety_stations_spas_2022, by = c("patrol_station_recode" = "referring_agency")) %>% select(contact_id, person_id, patrol_station, patrol_station_recode,full_address, city, state, asian, black_african_american, hispanic_latino_latina, 'middle_eastern_south_asian', native_american, pacific_islander, white, spa, spa_name, prc_area) %>% filter(!is.na(spa))

# add geocoded data
lasd_geocoded <- st_read(con2, query = "SELECT * FROM bv_2023.lasd_youth_arrests_geocoded")
# filter for age range
lasd_geocoded<-lasd_geocoded%>%left_join(lasd_youth_arrests, by=c("contact_id","person_id"))
lasd_geocoded<-lasd_geocoded%>%filter(!is.na(age))
lasd_spa_geocoded <- lasd_geocoded %>% st_join(spa) %>% as.data.frame()
lasd_spa_geocoded<-lasd_spa_geocoded%>%filter(location_type!='APPROXIMATE')%>%filter(!is.na(spa_name)) # filter out addresses that were generalized the most

### LAPD SPA- lines 525-539 in youth arrest script --------------------------

lapd_spa <- lapd_youth_arrests %>% left_join(crosswalk_lapd_district_spas_2022, by = c("reporting_district" = "name")) %>% filter(!is.na(spa_name))  %>% select(report_id, descent_code,  spa,spa_name)


## Some observations here don't have spatial coordinates.

lapd_spa_na <- lapd_youth_arrests %>% left_join(crosswalk_lapd_district_spas_2022, by = c("reporting_district" = "name")) %>% filter(is.na(spa_name)) 

lapd_spa_na_sf <- st_as_sf(x = lapd_spa_na, 
                           coords = c("lon", "lat"), 
                           crs = "EPSG:4326")

lapd_spa_na_sf <- st_transform(lapd_spa_na_sf, crs = 3310)


# rejoin this back to original spa df
lapd_spa_na_coordinates <- lapd_spa_na_sf %>% st_join(spa) %>% as.data.frame() %>% select(-spa.x, -spa_name.x) %>% rename(spa = spa.y, spa_name = spa_name.y)  %>% select(report_id, descent_code,  spa,spa_name)

lapd_spa <- bind_rows(lapd_spa,lapd_spa_na_coordinates) %>% select(report_id, descent_code,  spa,spa_name)

### RIPA SPA (QA'ed in Youth Arrest: lines 593-621 --------------------------


ripa_spa_notna <- ripa_youth_arrests %>% left_join(crosswalk_pubsafety_stations_spas_2022,  by = c("agency_name" = "referring_agency")) %>% select(doj_record_id, person_number, agency_name, closest_city, starts_with("rae"), spa, spa_name, prc_area) %>% filter(!is.na(spa_name))



ripa_spa_na <-  ripa_youth_arrests %>% left_join(crosswalk_pubsafety_stations_spas_2022,  by = c("agency_name" = "referring_agency")) %>% select(doj_record_id, person_number, agency_name, closest_city, starts_with("rae"), spa, spa_name, prc_area) %>% filter(is.na(spa_name)) %>% mutate(
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
  
  
  spa = case_when(
    spa_name == 'Antelope Valley' ~ "1",
    spa_name == 'San Fernando' ~ "2",
    spa_name == 'San Gabriel' ~ "3",
    spa_name == 'Metro' ~ "4",
    spa_name == 'West' ~ "5",
    spa_name == 'South' ~ "6",
    spa_name == 'East' ~ "7",
    spa_name == 'South Bay' ~ "8",
  )
)

ripa_spa <- rbind(ripa_spa_notna, ripa_spa_na) %>% filter(!is.na(spa_name))



# calculate count

lasd_spa_count <- lasd_spa %>% group_by(patrol_station_recode, spa, spa_name, prc_area) %>% summarize(count = n()) %>% group_by(patrol_station_recode) %>% mutate(count_weighted = (prc_area*count)) %>% group_by(spa, spa_name) %>% summarize(count = sum(count_weighted)) %>% mutate(data = "lasd")
lasd_spa_count_geocoded<-lasd_spa_geocoded%>%group_by(spa,spa_name)%>%summarise(count=n())%>%mutate(data = "lasd_geocoded")

lapd_spa_count <- lapd_spa %>% group_by(spa, spa_name) %>% summarize(count = n()) %>% mutate(data = "lapd")

ripa_spa_count <- ripa_spa %>% group_by(spa, spa_name) %>% summarize(count = n()) %>% mutate(data = "ripa")

spa_data <- rbind(lasd_spa_count, lasd_spa_count_geocoded,lapd_spa_count, ripa_spa_count)

df_arrests_spa <- spa_data %>% group_by(spa, spa_name) %>% mutate(pop = sum(count)) %>% distinct(spa, spa_name,pop)

# Calcs: Rate per 1000, Index of Disparity, 

df_region <- df_diversion_spa %>% left_join(df_arrests_spa) %>% mutate(rate = (count/pop) * 1000) %>% rename(geoid = spa, name = spa_name)

df_region$asbest <- "max"

df_region <- df_region %>% ungroup() %>%  mutate(best = max(df_region$rate, na.rm=T),
                                                 diff = best-rate,
                                                 values_count = length(df_region$rate) - sum(is.na(df_region$rate))) %>%
  mutate(
    sumdiff = sum(diff, na.rm = TRUE), 
    index_of_disparity = (sumdiff / best) / (values_count - 1) * 100) %>% select(-sumdiff) 


# Send to Postgres ----------------------------------------------------------------

# Subgroup -----------------------------------------------------------

con3 <- connect_to_db("bold_vision")
table_name <- "si_youth_diversion_subgroup"
schema <- 'bv_2023'

indicator <- "Youth Diversion for youth 0-20 per 1K youth arrested in 2022 by race"
source <- "Source: LA County Department of Youth Development, CADOJ Arrest Data, LASD Arrest data, LAPD arrest data. Nh two or more is excluded from ID calc due to discrepancy between data sources
See QA doc for details: W:\\Project\\OSI\\Bold Vision\\BV 2023\\Documentation\\System Impact\\QA_Youth_Diversion.docx"

# dbWriteTable(con3, c(schema, table_name), subgroup,
#            overwrite = TRUE, row.names = FALSE)

#comment on table and columns

comment <- paste0("COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
                 COMMENT ON COLUMN ", schema, ".", table_name, ".geoid IS 'County number';
                 COMMENT ON COLUMN ", schema, ".", table_name, ".name IS 'County Name';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".subgroup IS 'Race Group nh_api is not disaggregated in the data source, nh_twoormor includes multiracial and other due to data discrepancies';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".count IS 'indicator numerator: youth diversion';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".pop IS 'indicator denominator: youth arrests';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".diff IS 'indicator difference from the best';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".rate IS 'indicator rate per 1K youth arrests';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".best IS 'best rate among race groups';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".index_of_disparity IS 'index of disparity excludes bipoc, two or more, and total';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".values_count IS 'number of values';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".asbest IS 'minimum or maximum as best';")
# dbSendQuery(con3, comment)



# SPA ---------------------------------------------------------------------
con3 <- connect_to_db("bold_vision")
table_name <- "si_youth_diversion_region"
schema <- 'bv_2023'

indicator <- "Youth Diversion for youth 0-20 per 1K youth arrested in 2022 by SPA"
source <- "Source: LA County Department of Youth Development, CADOJ Arrest Data, LASD Arrest data, LAPD arrest data. Nh two or more is excluded from ID calc due to discrepancy between data sources
See QA doc for details: W:\\Project\\OSI\\Bold Vision\\BV 2023\\Documentation\\System Impact\\QA_Youth_Diversion.docx"


# dbWriteTable(con3, c(schema, table_name), df_region,
#        overwrite = TRUE, row.names = FALSE)

#comment on table and columns
comment <- paste0("
                  COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".geoid IS 'SPA number';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".name IS 'SPA Name';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".pop IS 'indicator denominator: youth arrests';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".count IS 'indicator numerator: youth diversion';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".rate IS 'indicator rate per 1K youth arrests';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".asbest IS 'minimum or maximum as best';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".best IS 'best rate among regions';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".diff IS 'indicator difference from the best';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".values_count IS 'number of values';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".index_of_disparity IS 'index of disparity';
                  ")
# dbSendQuery(con3, comment)

#disconnect
dbDisconnect(con3)










