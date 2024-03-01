# Objective:
# Translate SQL for bv_2021.cwb_pollution_burden_lacounty_2017 to R script for bv_2023
# 2023 updates: 
## 1. population data for youth 0-24 years old: bv_2023.dhc_tract_2020_youth_0_24_race
## 2. analysis for SPA and county (see pollution_burden_county.R and pollution_burden_spa.R)

# Load in libraries
library(dplyr)
library(RPostgreSQL)
library(sf)
library(data.table)
source("W:\\RDA Team\\R\\credentials_source.R")
options(scipen=99, digits=15)
bv_conn <- connect_to_db("bold_vision")

##### Step 0: Identify tables needed for pollution burden indicator and import #####
### Sensitive land uses:
# childcare centers (geom)
licensed_childcare_centers_sql <- "SELECT facility_number, geom 
  FROM bv_2023.ccld_childcare_centers_2023
  WHERE facility_status = 'LICENSED';"
licensed_childcare_centers <- st_read(bv_conn, query=licensed_childcare_centers_sql)   

# family homes (zip)
licensed_familyhomes_sql <- "SELECT facility_zip, gid
  FROM bv_2023.ccld_family_childcare_homes_2023
  WHERE facility_status = 'LICENSED';"
licensed_familyhomes <- dbGetQuery(bv_conn, licensed_familyhomes_sql)

# parks (geom)
parks_sql <- "SELECT unit_id, geom_3310 FROM bv_2023.greeninfo_cpad_units_2023a;"
parks <- st_read(bv_conn, query=parks_sql)

# schools (geom)
schools_sql <- "SELECT gid, school_cou, geom FROM bv_2023.greeninfo_cscd_school_property_2021;"
schools <- st_read(bv_conn, query=schools_sql)

### Pollution Burden Score:
# ces (tracts)
ces4_sql <- "SELECT ct_geoid, polburdsc FROM bv_2023.oehha_ces4_tract_2021;"
ces4 <- dbGetQuery(bv_conn, ces4_sql) %>%
  rename(census_tract=ct_geoid,
         pollution_burden_score=polburdsc)


### Geo tables
# LA County zip geoms
zips_sql <- "SELECT zipcode, geom FROM bv_2023.lacounty_zipcodes_2022"
zips <- st_read(bv_conn, query=zips_sql)
zips$geom <- st_transform(zips$geom, 3310)
st_crs(zips)

# dissolving multipolygons so there are no duplicate zipcodes
zips <- zips %>%
  group_by(zipcode) %>%
  summarize()

# LA county 2020 (2020 vintage) tracts 
tracts_sql <- "SELECT ct_geoid, geom_3310 FROM	bv_2023.cb_2020_06_tract_500k WHERE countyfp='037';"
tracts <- st_read(bv_conn, query=tracts_sql) %>%
  rename(census_tract=ct_geoid,
         geom=geom_3310)

# CA census relationships 2010 tracts to 2020 tracts
# also downloaded txt here: W:\Data\Geographies\Relationships\cb_tract2020_tract2010_st06.txt
# note: CES uses 2010 tract vintage, population estimates use 2020 tract vintage
cb_tract_2010_2020 <- fread("W:\\Data\\Geographies\\Relationships\\cb_tract2020_tract2010_st06.txt", sep="|", colClasses = 'character', data.table = FALSE) %>%
  select(GEOID_TRACT_10, NAMELSAD_TRACT_10, AREALAND_TRACT_10, GEOID_TRACT_20, NAMELSAD_TRACT_20, AREALAND_TRACT_20, AREALAND_PART) %>%
  mutate_at(vars(contains("AREALAND")), function(x) as.numeric(x)) %>%
  # calculate overlapping land area of 2010 and 2020 tracts (AREALAND_PART) as a percent of 2020 tract land area (AREALAND_TRACT_20)
  mutate(prc_overlap=AREALAND_PART/AREALAND_TRACT_20) 

# # check prc_overlaps sum to 1 for our purposes 
# check_prc <- cb_tract_2010_2020 %>%
#   filter(grepl("^06037", GEOID_TRACT_10)) %>%
#   group_by(GEOID_TRACT_10) %>%
#   summarise(total_prc=sum(prc_overlap))

# tract to spa
tract_spa_xwalk_sql <- "SELECT * FROM bv_2023.crosswalk_tract_spas_2023;"
tract_spa_xwalk <- dbGetQuery(bv_conn, tract_spa_xwalk_sql)

# zip to spa
zip_spa_xwalk_sql <- "SELECT * FROM bv_2023.crosswalk_zip_spas_2023;"
zip_spa_xwalk <- dbGetQuery(bv_conn, zip_spa_xwalk_sql)

# close db connection
dbDisconnect(bv_conn)


##### Create tract to zip xwalk for familyhomes allocations #####
# # calculate area of zips/tracts
zips$zip_area <- st_area(zips)
tracts$tract_area <- st_area(tracts)

# intersect zips and tracts
tract_zip <- st_intersection(zips, tracts)

# calculate area of the intersect
tract_zip$intersect_area <- st_area(tract_zip)

# calculate percent of intersect out of tract area and agency area
tract_zip$prc_zip_area <- as.numeric(tract_zip$intersect_area/tract_zip$zip_area)
tract_zip$prc_tract_area <- as.numeric(tract_zip$intersect_area/tract_zip$tract_area)
# drop where prc_zip_area == 0
tract_zip <- tract_zip %>%
  filter(prc_zip_area > 0)

# # check all census tracts still included
# # n = 2495 - one is missing
# length(unique(tract_zip$census_tract))
# 
# # Note: we start with 2495 tracts in LA County, intersecting with LA county zips - all remain but we have 5185 intersections
# check_multiple_tract_intersections <- tract_zip %>%
#   group_by(census_tract) %>%
#   filter(n() > 1)
# 
# # 1680 tracts have multiple zip matches
# length(unique(check_multiple_tract_intersections$census_tract))

##### Step 1: get counts for sensitive land uses (childcare homes, familyhomes, parks, and schools) at 2020 tract level #####
### childcare counts
tracts_childcare_counts <- tracts %>%
  st_join(licensed_childcare_centers, join=st_intersects) %>% 
  st_drop_geometry() %>%
  select(-tract_area) %>%
  group_by(census_tract) %>%
  summarize(across(everything(), ~n())) %>%
  rename(childcare_count=facility_number)


### familyhome counts - 
# get zip count of familyhomes, join in matched tracts and allocate by prc_zip_area
ccld_familyhomes <- licensed_familyhomes %>%
  group_by(facility_zip) %>%
  summarize(across(everything(), ~n())) %>%
  rename(familyhome_count=gid)

# add in zips that have no familyhomes (not tracked in ccld database) and set count to 0
zips_familyhomes <- zips %>%
  left_join(ccld_familyhomes, by=c("zipcode"="facility_zip"))
zips_familyhomes$familyhome_count <- replace_na(zips_familyhomes$familyhome_count, 0)
zips_familyhomes <- zips_familyhomes %>%
  st_drop_geometry()

# join census tracts to zips
# note: here is where we can see zipcodes that don't match to census tracts
# All are zero except for 91710 (might be more San Bernardino?)
tracts_familyhomes <- zips_familyhomes %>%
  left_join(tract_zip, by=c("zipcode"))

# allocate familyhomes to get tract count
# drop zips with no census tract match
tracts_familyhomes_counts <- tracts_familyhomes %>%
  select(zipcode, census_tract, familyhome_count, prc_zip_area) %>%
  filter(!is.na(census_tract)) %>%
  mutate(tract_count = familyhome_count * prc_zip_area)

## check for duplicate zip-tract matches
# check_zip_tract_duplicates <- tracts_familyhomes_counts %>%
#   group_by(zipcode, census_tract) %>%
#   summarise(n=n()) %>%
#   filter(n>1)

# # total familyhomes in la county (n=3210) - based on zip counts that match to an LA County census tract
# check_total_lacounty_familyhomes <- tracts_familyhomes_counts %>%
#   select(zipcode, familyhome_count) %>%
#   distinct() %>%
#   summarize(total=sum(familyhome_count))
# 
# check_total_lacounty_familyhomes
# 
# # total familyhomes allocated to LA County census tracts
# sum(tracts_familyhomes_counts$tract_count) # 3209.4375

# # find what's missing
# check_allocations <- tracts_familyhomes_counts %>%
#   rename(zip_count=familyhome_count) %>%
#   select(zipcode, zip_count, tract_count) %>%
#   group_by(zipcode, zip_count) %>%
#   summarize(allocation = sum(tract_count)) %>%
#   mutate(difference = zip_count-allocation)
# 
# sum(check_allocations$difference) # 0.562513

# clean up cols
tracts_familyhomes_counts <- tracts_familyhomes_counts %>%
  select(census_tract, tract_count) %>%
  group_by(census_tract) %>%
  summarize(familyhome_count=sum(tract_count)) 

### park counts
tracts_parks_counts <- tracts %>%
  st_join(parks, join=st_intersects) %>%
  st_drop_geometry() %>%
  select(-tract_area) %>%
  group_by(census_tract) %>%
  summarise(across(everything(), ~n())) %>%
  rename(park_count = unit_id) 

# school counts
tracts_schools_counts <- tracts %>%
  st_join(schools, join=st_intersects) %>%
  st_drop_geometry() %>%
  select(census_tract, school_cou) %>%
  group_by(census_tract) %>%
  summarise(across(everything(), ~sum(., na.rm = TRUE))) %>%
  rename(school_count=school_cou) 

# combine all sensitive land use counts, add one
# note: minimum total count is 3 (no cases of census tracts with 0 sensitive land uses)
counts_tract <- tracts %>%
  st_drop_geometry() %>%
  select(census_tract) %>%
  distinct() %>%
  left_join(tracts_childcare_counts, by=c("census_tract")) %>%
  left_join(tracts_familyhomes_counts, by=c("census_tract")) %>%
  left_join(tracts_parks_counts, by=c("census_tract")) %>%
  left_join(tracts_schools_counts, by=c("census_tract"))

# add +1 to all site counts
counts_tract[c("childcare_count", "familyhome_count", "park_count", "school_count")] <- counts_tract[c("childcare_count", "familyhome_count", "park_count", "school_count")] + 1


##### Step 2: Calculate pollution burden by tracts #####
# Find total county count for each SLU type

total_childcare_sites <- sum(counts_tract$childcare_count)
total_childcare_sites # 6848

total_familyhome_sites <- sum(counts_tract$familyhome_count)
total_familyhome_sites # 5637.62053279922

total_park_sites <- sum(counts_tract$park_count)
total_park_sites # 7663

total_school_sites <- sum(counts_tract$school_count)
total_school_sites # 4888

# Because CES uses 2010 vintage tracts - need to convert to 2020 vintage and allocate score accordingly
ces_2010_2020 <- ces4 %>%
  left_join(cb_tract_2010_2020, by=c("census_tract"="GEOID_TRACT_10")) %>%
  select(census_tract, pollution_burden_score, AREALAND_TRACT_10, AREALAND_PART, GEOID_TRACT_20, AREALAND_TRACT_20, prc_overlap) %>%
  # Allocate CES scores from 2010 tracts to 2020 using prc_overlap
  mutate(pollution_burden_score_2020=pollution_burden_score*prc_overlap)

# # some checks
# # check that prc_overlap sums to 1 for 2020 tracts
# check_prc_is_1 <- ces_2010 %>%
#   select(GEOID_TRACT_20, prc_overlap) %>%
#   group_by(GEOID_TRACT_20) %>%
#   summarize(total_prc = sum(prc_overlap))
# # check QA example specifically
# check_06037604200 <- ces_2010 %>%
#   filter(GEOID_TRACT_20=="06037604200") %>%
#   mutate(allocation = pollution_burden_score*prc_overlap)

# # check if any tracts from other counties correspond to LA County (some minor from Orange and Kern)
# # call out in QA in case we want to limit
# check_ces_2010_la <- ces_2010 %>%
#   filter(grepl("06037", GEOID_TRACT_20))

ces_2020 <- ces_2010_2020 %>%
  # filter for 2020 tracts in LA county
  select(GEOID_TRACT_20, pollution_burden_score_2020) %>%
  filter(grepl("^06037", GEOID_TRACT_20)) %>%
  rename(census_tract=GEOID_TRACT_20) %>%
  # sum weighted scores by tract geoid
  group_by(census_tract) %>%
  summarize(total_pollution_burden = sum(pollution_burden_score_2020)) %>%
  # Clean up names to match following steps
  rename(unweighted_pollution_burden_score=total_pollution_burden) %>%
  mutate(unweighted_pollution_burden_percentile=percent_rank(unweighted_pollution_burden_score))


# calc pollution burden weighted by sensitive land use sites
pollution_burden_tract <- counts_tract %>%
  select(census_tract, childcare_count, familyhome_count, park_count, school_count) %>%
  left_join(ces_2020, by=c("census_tract")) %>%
  # weight each SLU type by county total for that type
  mutate(childcare_wa = childcare_count/total_childcare_sites,
         familyhome_wa = familyhome_count/total_familyhome_sites,
         park_wa = park_count/total_park_sites,
         school_wa = school_count/total_school_sites) %>%
  rowwise() %>%
  # multiple the tract pollution burden score by each site type weight
  mutate(childcare_pollbur_score=childcare_wa*unweighted_pollution_burden_percentile,
         familyhome_pollbur_score=familyhome_wa*unweighted_pollution_burden_percentile,
         park_pollbur_score=park_wa*unweighted_pollution_burden_percentile,
         school_pollbur_score=school_wa*unweighted_pollution_burden_percentile) %>%
  ungroup() %>%
  # calculate percentiles for each tract-site score
  mutate(childcare_pollbur_percentile = percent_rank(childcare_pollbur_score),
         familyhome_pollbur_percentile = percent_rank(familyhome_pollbur_score),
         park_pollbur_percentile = percent_rank(park_pollbur_score),
         school_pollbur_percentile = percent_rank(school_pollbur_score)) %>%
  rowwise() %>%
  # calculate tract-level pollution burden by taking a simple average across the 4 site type percentiles
  mutate(pollution_burden = mean(c(childcare_pollbur_percentile,
                                   familyhome_pollbur_percentile,
                                   park_pollbur_percentile,
                                   school_pollbur_percentile))) %>%
  filter(!is.na(pollution_burden)) %>%
  ungroup() %>%
  mutate(pollution_burden_percentile=percent_rank(pollution_burden))

# # Check for census tracts with no CES score - there are none!
# check_tracts_no_ces <- counts_tract %>%
#   select(census_tract, total_count) %>%
#   left_join(ces_2020, by=c("census_tract")) %>%
#   rowwise() %>%
#   mutate(pollution_burden=total_count*pollution_burden_score/total_slu_sites) %>%
#   filter(is.na(pollution_burden)) %>%
#   left_join(tract_zip, by=c("census_tract"))
# 
# length(unique(check_tracts_no_ces$census_tract)) # 0!


##### Step 3: EXPORT TO PG #####

# Prep df columns
pollution_burden_tract <- pollution_burden_tract %>%
  select(order(colnames(pollution_burden_tract))) %>%
  select(census_tract, pollution_burden_percentile, pollution_burden, everything())

# Set up metadata
table_schema <- "bv_2023"
table_name <- "hbe_pollution_burden_indicator"
conn <- connect_to_db("bold_vision")

# Column names
colname_charvar <- names(pollution_burden_tract)

# Column comments
colcomments_charvar <- c("Census tract", 
                         "Percentile of average pollution burden score (weighted by proportion of sensitive land uses). Final indicator used in _subgroup and _region calcs.",
                         "Average pollution burden score (weighted by proportion of sensitive land uses",
                         rep(c("sensitive land use count", 
                               "percentile of pollution burden weighted by proportion of this land use type at the census tract level to total county count",
                               "pollution burden score weighted by proportion  of this land use type at the census tract level to total county count",
                               "proportion of this land use type at the census tract level relative to county total count"),
                             times=4),
                         "CES Pollution Burden percentile for 2020 census tracts (unweighted by sensitive land uses)",
                         "CES Pollution Burden score for 2020 census tracts (unweighted by sensitive land uses)"                         )

# Write table
dbWriteTable(conn, c(table_schema, table_name), pollution_burden_tract,
             overwrite = FALSE, row.names = FALSE)

# Write and send table comment
indicator <- "Pollution Burden (Average Percentile and Scores) weighted by sensitive land uses (i.e., licensed family childcare homes, licensed childcare centers, parks, and schools) at the LA County census-tract level. Every census tract is assumed to have a land use count of at least 1 for each sensitive land use type."
source <- "GreenInfo Network California Protected Areas Database (2023a), California Community Care Licensing Division (2023), GreenInfo California School Campus Database (2021), California Office of Environmental Health Hazard Assessment CalEnviroScreen 4.0 (2021), Census DHC (2020). See QA doc for details: W:\\Project\\OSI\\Bold Vision\\BV 2023\\Documentation\\Healthy Built Environment\\QA_CWB_Pollution_Burden_calcs.docx"

table_comment <- paste0("COMMENT ON TABLE ", table_schema, ".", table_name,  " IS '", indicator, " from ", source, ".';")
dbSendQuery(conn, table_comment)

# loop through the columns, writing comment, then sends to the db.
for (i in seq_along(colname_charvar)){
  
  sqlcolcomment <- paste0("COMMENT ON COLUMN ", table_schema, ".", table_name, ".",
           colname_charvar[[i]], " IS '", colcomments_charvar[[i]], "';" )
  
  # send sql comment to database
  dbSendQuery(conn, sqlcolcomment)
}

dbDisconnect(conn)
