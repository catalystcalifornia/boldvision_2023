# Indicator: Transit Injury 

# Load Packages
#remotes::install_github("catalystcalifornia/RC")
library(RC) # RaceCounts package created by RDA
library(tidyverse)
library(sf)
library(sp)
library(tidycensus)
library(tigris)
library(janitor)

source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("rda_shared_data")
con2 <- connect_to_db("bold_vision")


# Load Data
#crashes <- read_csv("W:/Project/OSI/Bold Vision/BV 2023/Data/Healthy Built Environment/TIMS/Crashes.csv")
#parties <- read_csv("W:/Project/OSI/Bold Vision/BV 2023/Data/Healthy Built Environment/TIMS/Parties.csv")
#victims <- read_csv("W:/Project/OSI/Bold Vision/BV 2023/Data/Healthy Built Environment/TIMS/Victims.csv")

# Clean columns
#colnames(crashes) <- tolower(colnames(crashes))
#colnames(parties) <- tolower(colnames(parties))
#colnames(victims) <- tolower(colnames(victims))

###Send data to Postgres###
table_name_1 <- "tims_crashes_2015_22"
table_name_2 <- "tims_parties_2015_22"
table_name_3 <- "tims_victims_2015_22"

schema <- 'built_environment'
source_1 <- "Transportation Injury Mapping System 2015-22 Crash-Level Data downloaded from: #https://tims.berkeley.edu/tools/query/index.php?clear=true.See Code-Book for more details: https://tims.berkeley.edu/help/SWITRS.php#Codebook"

source_2 <- "Transportation Injury Mapping System 2015-22 Party-Level Data downloaded from: https://tims.berkeley.edu/tools/query/index.php?clear=true.See Code-Book for more details: https://tims.berkeley.edu/help/SWITRS.php#Codebook"

source_3 <- "Transportation Injury Mapping System 2015-22 Victim-Level Data downloaded from: https://tims.berkeley.edu/tools/query/index.php?clear=true. See Code-Book for more details: https://tims.berkeley.edu/help/SWITRS.php#Codebook"

#dbWriteTable(con, c(schema, table_name_1), crashes, overwrite = FALSE, row.names = FALSE)

comment_1 <- paste0("COMMENT ON TABLE ", schema, ".", table_name_1,  " IS '", " Data downloaded from ", source_1, ".';")

#dbSendQuery(con, comment_1)

#dbWriteTable(con, c(schema, table_name_2), parties, overwrite = FALSE, row.names = FALSE)

comment_2 <- paste0("COMMENT ON TABLE ", schema, ".", table_name_2,  " IS '", " Data downloaded from ", source_2, ".';")

dbSendQuery(con, comment_2)

#dbWriteTable(con, c(schema, table_name_3), victims, overwrite = FALSE, row.names = FALSE)

comment_3 <- paste0("COMMENT ON TABLE ", schema, ".", table_name_3,  " IS '", " Data downloaded from ", source_3, ".';")

#dbSendQuery(con, comment_3)


## Load Data from postgres
crashes <- dbGetQuery(con, "SELECT * FROM built_environment.tims_crashes_2015_22")
parties <- dbGetQuery(con, "SELECT * FROM built_environment.tims_parties_2015_22")
victims <- dbGetQuery(con, "SELECT * FROM built_environment.tims_victims_2015_22")

# pull tract pop
tract_pop <- dbGetQuery(con2, "SELECT * FROM bv_2023.dhc_tract_2020_youth_0_24_race")

# pull tract shapefile 
tract_sf <- st_read(con, query = "SELECT * FROM geographies_ca.cb_2022_06_tract_500k") %>% filter(namelsadco == "Los Angeles County")

# pull crosswalk tract to spas

crosswalk_tract_spas <- dbGetQuery(con2, "SELECT * FROM bv_2023.crosswalk_tract_spas_2023")

# get spas

spas <- st_read(con, query = "select * from geographies_la.la_county_service_planning_areas_2022")

# Code book: https://tims.berkeley.edu/help/SWITRS.php#Codebook

# coordinates data
coordinates <- crashes %>% select(case_id, accident_year, latitude, longitude, point_x, point_y, primary_rd, secondary_rd, direction) %>% mutate(point_x = ifelse(is.na(point_x), longitude, point_x), 
                                                                                                                                                 point_y = ifelse(is.na(point_y), latitude, point_y)) 
# make the coordinate columns equal the latitude and longitude columns if it is missing

## Step 1: # filter data for pedestrian/bicyclists killed or severely injured --------

## According to the codebook: "For the purpose of analysis across multiple years with old definition (2, 3, and 4), we combined injury status categories using the latest definitions (5, 6 and 7); i.e., all victims coded as "Severe Injury" or "Suspected Serious Injury" are shown as "Suspected Serious Injury" in our tools."
victims_filtered <- victims %>% filter(victim_degree_of_injury %in% c("1", "2", "5") & victim_role %in% c("3", "4") & victim_age <=24 & accident_year %in% c("2018", "2019","2020", "2021", "2022")) %>% select(case_id, victim_degree_of_injury, victim_role, victim_age, county, city, accident_year)

victims_coordinates <- victims_filtered %>% left_join(coordinates, by = c("case_id", "accident_year")) 

coordinates_na <- victims_coordinates %>% filter(is.na(point_x)) # there are 13 observations without geo-code, or around 2% of data

victims_coordinates_df <- victims_coordinates %>% filter(!is.na(point_x))


victims_spdf <- SpatialPointsDataFrame(
  coords = victims_coordinates_df [, c("point_x", "point_y")],
  data = victims_coordinates_df, 
  proj4string = CRS("+init=epsg:3310")
)

victims_sf <- st_as_sf(victims_spdf)


# Set CRS to 3310 to do a spatial join for victims/tract data -------------

victims_sf <- st_set_crs(victims_sf, 3310)
tract_sf <- st_set_crs(tract_sf, 3310)


# Step 2: merge victims with tracts  --------------------------------------

victims_tract <- victims_sf %>% st_join(tract_sf) %>% as.data.frame() 

victims_tract %>% filter(is.na(ct_geoid)) # no census tract for 2 observations?

## use latitude/longitude columns to track 

victims_tract_na <- victims_tract %>% filter(is.na(ct_geoid)) # no census tract for this observation?

victims_tract_na <- victims_tract_na %>% mutate(longitude = ifelse(is.na(longitude), point_x, longitude),
                                                latitude = ifelse(is.na(latitude), point_y, latitude)) %>% select(case_id:direction)



victims_spdf_na <- SpatialPointsDataFrame(
  coords = victims_tract_na [, c("longitude", "latitude")],
  data = victims_tract_na, 
  proj4string = CRS("+init=epsg:3310")
)

victims_sf_na <- st_as_sf(victims_spdf_na)

victims_sf_na <- st_set_crs(victims_sf_na, 3310)

victims_tract_na <- victims_sf_na %>% st_join(tract_sf) %>% as.data.frame()

## remove 2 observations without census tract, join 2 observations(only 1 with census tract)

victims_tract <- victims_tract %>% filter(!is.na(ct_geoid))

victims_tract <- rbind(victims_tract, victims_tract_na)

# manually add census tract: 06037262706 to this observation. I did this using census geocoding: https://geocoding.geo.census.gov/geocoder/geographies/coordinates?x=-118.5295&y=34.03286&benchmark=4&vintage=4
victims_tract <- victims_tract %>% mutate(ct_geoid = ifelse(case_id == "8648602", "06037262706", ct_geoid))

# Step 3: Calculate rate per 1,000 at the census tract level --------------

ind_df <- victims_tract %>% select(ct_geoid) %>% rename(sub_id = ct_geoid) %>% group_by(sub_id) %>% summarize(raw = n()) 

# merge with total youth, multiply rate * 1000 per census tract

total_youth <- tract_pop %>% filter(race == "total") %>% rename(sub_id = geoid) %>% select(-race)

ind_df <- ind_df %>% right_join(total_youth, by = c()) %>% mutate(raw = ifelse(is.na(raw), 0, raw), indicator = (raw/pop)*1000)

## add pop threshold of 100
ind_df <- ind_df %>% mutate(indicator = ifelse(pop<100, NA, indicator))

#set source for Weighted Average Functions script
source("W:/RDA Team/R/Functions/Cnty_St_Wt_Avg_Functions.R")

tract_pop_wide <- tract_pop %>% mutate(race = paste0(race, "_sub_pop"))     %>% pivot_wider(
  names_from = race,
  values_from = pop
) %>% rename(sub_id = geoid)

# Step 4: Calculate weighted average in LA County -------------------------

## calculate total youth pop by race in LA county
pop_df <- tract_pop_wide %>% mutate(target_id = "06037", 
                                    total_target_pop = sum(total_sub_pop),
                                    aian_target_pop = sum(aian_sub_pop), 
                                    latino_target_pop = sum(latino_sub_pop), 
                                    nh_asian_target_pop = sum(nh_asian_sub_pop), 
                                    nh_black_target_pop = sum (nh_black_sub_pop), 
                                    nh_twoormor_target_pop = sum(nh_twoormor_sub_pop),
                                    nh_other_target_pop = sum(nh_other_sub_pop), 
                                    nh_white_target_pop = sum(nh_white_sub_pop),
                                    pacisl_target_pop = sum(pacisl_sub_pop), 
                                    bipoc_target_pop = sum(bipoc_sub_pop),
                                    n = nrow(tract_pop_wide) # number of tracts in LA county
)



##### COUNTY WEIGHTED AVG CALCS ######

pop_threshold <- 0 # arbitrary pop threshold value so function could work. We normally use these functions for other counties/cities with low population so we screen them out but we are not screening out since we are only working with LA county.

pct_df <- pop_pct(pop_df) 


#run wa
wa <- wt_avg(pct_df)        # calc weighted average and apply reliability screens


wa$county <- "Los Angeles County"
wa <- wa %>% select(target_id, county, everything()) %>% rename(geoid = target_id)


# Step 5: Calculate Stats ----------------------------------------------
d <- wa

#remove bipoc from ID calcs
d <- d %>% select(-bipoc_rate, -bipoc_pop)

# multiply by 100 to get 100,000
d <- d %>% pivot_longer(cols = ends_with("rate"), 
                        names_to = "group", 
                        values_to = "rate") %>% mutate(rate = rate * 100) %>% pivot_wider(
                          names_from = group, 
                          values_from = rate
                        )

#set source for RC Functions script
source("W:/Project/RACE COUNTS/Functions/RC_Functions.R")

d$asbest = 'min'    #YOU MUST UPDATE THIS FIELD AS NECESSARY: assign 'min' or 'max'

d <- count_values(d) #calculate number of "_rate" values
d <- calc_best(d) #calculate best rates -- be sure to update previous line of code accordingly before running this function.
d <- calc_diff(d) #calculate difference from best
d <- calc_id(d) #calculate index of disparity

#add bipoc data back
county_table <- left_join(d, wa %>% select(geoid, bipoc_rate, bipoc_pop), by="geoid")

# multiply bipoc by 100 to get 100,000 rate
county_table <- county_table %>% mutate(bipoc_rate = bipoc_rate*100)


# pivot to Elycia's desired format
subgroup <- county_table %>% select(-county, -asbest, -n, -best, -values_count, -index_of_disparity) %>% 
  pivot_longer(!geoid, names_to = "subgroup", values_to = "metric") %>% 
  mutate(variable = case_when(grepl("_rate", subgroup) ~ "rate",
                              grepl("_pop", subgroup) ~ "pop",
                              grepl("_diff", subgroup) ~ "diff")) %>%
  mutate(across(subgroup, str_replace, "_rate", "")) %>%
  mutate(across(subgroup, str_replace, "_pop", "")) %>%
  mutate(across(subgroup, str_replace, "_diff", "")) %>%
  pivot_wider(names_from = variable, values_from = metric) %>%
  left_join(county_table %>% select(geoid, best, index_of_disparity, values_count, asbest), by = "geoid") %>%
  mutate(name = "Los Angeles County") %>% select(geoid, name, everything())

##### SPA WEIGHTED AVG CALCS ######

ind_df <- ind_df %>% mutate(county = "06037") %>%  select(sub_id, indicator,county)

crosswalk_tract_spas <- crosswalk_tract_spas %>% rename(sub_id = geoid)


# Step 1: merge with crosswalk tract spas ---------------------------------

ind_df <- left_join(crosswalk_tract_spas, ind_df, by="sub_id") %>% 
  rename(target_id = spa, geoname = spa_name) %>%
  select(target_id, sub_id, geoname, indicator) %>%
  as.data.frame()

spa_youth <- pop_df %>% select(-ends_with("target_pop"), -c(target_id, n)) %>% left_join(ind_df) %>% select(-c(geoname, indicator))

#update target_id for the 3 tracts that fail in join (because they're water)
spa_youth["target_id"][spa_youth["sub_id"] == "06037990100"] <- "5"
spa_youth["target_id"][spa_youth["sub_id"] == "06037990200"] <- "8"
spa_youth["target_id"][spa_youth["sub_id"] == "06037990300"] <- "8"


# Step 2: Get SPA population data  ----------------------------------------

# group tract data by spa and summarize
spa_total_pop <- spa_youth %>% group_by(target_id) %>% 
  summarize(
    total_target_pop = sum(total_sub_pop), n=n()
  )


pop_df <- left_join(spa_youth, spa_total_pop, by="target_id")

pop_df$geolevel = "tract"
pop_df <- pop_df %>% select(sub_id, target_id, geolevel, everything())


# Step 3: Run weighted average --------------------------------------------

pop_threshold = 0
pct_df <- pop_pct(pop_df) 
#run wa
wa <- wt_avg(pct_df)      


df <- left_join(wa, spas%>%select(SPA_NAME,SPA), by=c("target_id"="SPA")) %>% rename(name = SPA_NAME, geoid = target_id) %>% select(geoid, name, total_rate, total_pop )

# multiply by 100 to get 100,000
df  <- df %>% mutate(total_rate = total_rate * 100)


# Step 4: Calculate stats -------------------------------------------------

df_region <- df %>% rename(rate = total_rate, pop = total_pop)

df_region$asbest <- "min"
df_region <- df_region %>% ungroup() %>%  mutate(best = min(df_region$rate, na.rm=T),
                                                 diff = rate-best,
                                                 values_count = length(df_region$rate) - sum(is.na(df_region$rate))) %>%
  mutate(
    sumdiff = sum(diff, na.rm = TRUE), 
    index_of_disparity = (sumdiff / best) / (values_count - 1) * 100) %>% select(-sumdiff)   


# Push to Postgres --------------------------------------------------------

## Subgroup ----------------------------------------------------------------


###Send to Postgres###
con3 <- connect_to_db("bold_vision")
table_name <- "hbe_transit_injury_subgroup"
schema <- 'bv_2023'

indicator <- "Transit Injury"
source <- "Transportation Injury Mapping Systems: https://tims.berkeley.edu/login.php?next=%2Ftools%2Fquery%2Findex.php%3Fclear%3Dtrue. See QA doc for details: W:\\Project\\OSI\\Bold Vision\\BV 2023\\Documentation\\Healthy Built Environment\\QA_Transit_Injury.docx"
dbWriteTable(con3, c(schema, table_name), subgroup,
             overwrite = FALSE, row.names = FALSE)

#comment on table and columns


comment <- paste0("COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".geoid IS 'County number';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".name IS 'County Name';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".subgroup IS 'Race Group';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".pop IS 'indicator denominator';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".diff IS 'indicator difference from the best';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".rate IS 'indicator weighted average rate';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".best IS 'best rate among race groups';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".index_of_disparity IS 'index of disparity';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".values_count IS 'number of values';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".asbest IS 'minimum or maximum as best';")
#dbSendQuery(con3, comment)



# SPA ---------------------------------------------------------------------

###Send to Postgres###
table_name <- "hbe_transit_injury_region"


#dbWriteTable(con3, c(schema, table_name), df_region,
#           overwrite = FALSE, row.names = FALSE)

#comment on table and columns

comment <- paste0("
                  COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".geoid IS 'SPA number';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".name IS 'SPA Name';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".pop IS 'indicator denominator';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".rate IS 'indicator weighted average rate';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".asbest IS 'minimum or maximum as best';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".best IS 'best rate among race groups';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".diff IS 'indicator difference from the best';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".values_count IS 'number of values';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".index_of_disparity IS 'index of disparity';
                  ")
#dbSendQuery(con3, comment)


#disconnect
#dbDisconnect(con3)









