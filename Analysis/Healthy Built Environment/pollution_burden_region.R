# Calculate weighted average of Pollution Burden for LA County SPAs,
# using RACE COUNTS functions and Census DHC populations


source("W:\\RDA Team\\R\\credentials_source.R")

bv_conn <- connect_to_db("bold_vision")

##### Prep: Get data from pg #####
indicator_query <- "SELECT census_tract, pollution_burden_percentile from bv_2023.hbe_pollution_burden_indicator"
ind_df <- dbGetQuery(bv_conn, indicator_query) %>%
  rename(sub_id=census_tract,
         indicator=pollution_burden_percentile) %>%
  mutate(geoname="tract",
         target_id="06037")

tract_spa_xwalk_sql <- "SELECT * FROM bv_2023.crosswalk_tract_spas_2023;"
tract_spa_xwalk <- dbGetQuery(bv_conn, tract_spa_xwalk_sql)


##### 4. join SPA ids #####
ind_df <- ind_df %>%
  left_join(tract_spa_xwalk %>% 
              rename(sub_id = geoid), by = "sub_id") %>%
  filter(is.na(spa) != TRUE) %>% 
  filter(is.na(indicator) != TRUE) %>%
  #format for wa later
  select(-spa_name, -target_id) %>% 
  rename(target_id = spa) %>%
  select(target_id, sub_id, geoname, indicator)


##### 5. get tract 0-24 pop by race ####
race_0_24_sql <- "SELECT geoid, pop FROM bv_2023.dhc_tract_2020_youth_0_24_race WHERE race = 'total';"
race_0_24 <- dbGetQuery(bv_conn, race_0_24_sql) %>%
  rename(sub_id = geoid,
         total_sub_pop = pop)

#make wide
# race_0_24_wide <- race_0_24 %>% 
#   pivot_wider(names_from = race, values_from = pop) %>% 
#   select(geoid, total) %>%
#   # rename 
#   rename(sub_id = geoid,
#          total_sub_pop = total)


race_0_24 <- race_0_24 %>%
  # add spa id for join later
  left_join(ind_df, by="sub_id") %>%
  
  # filter out tracts with NA pollution burden
  filter(!is.na(indicator)) %>%
  
  #remove tracts with no population
  filter(total_sub_pop != "NA") %>%
  filter(total_sub_pop > 0) %>%
  
  #format (sub_id, sub_pops, target_id)
  select(sub_id, ends_with("pop"), target_id)

#add unique id to ind_df so wa works later
ind_df$sub_id <- paste0(ind_df$sub_id, ind_df$target_id)


##### 6. Make spa data from tract data ####

race_0_24_spa <- race_0_24 %>% 
  group_by(target_id) %>% 
  summarize(
    total_target_pop = sum(total_sub_pop), n=n())

# join tract data to spa data and format
pop_df <- left_join(race_0_24, race_0_24_spa, by="target_id")
pop_df$geolevel = "tract"
pop_df <- pop_df %>% 
  select(sub_id, target_id, geolevel, everything())

#give unique ids so pct_df and wa work
pop_df$sub_id <- paste0(pop_df$sub_id, pop_df$target_id)


########### 7. Run weighted average ################
#set source for WA Functions script
source("W:/RDA Team/R/Functions/Cnty_St_Wt_Avg_Functions.R")

#run pct_df
pop_threshold = 0
pct_df <- pop_pct(pop_df) 


#run wa
wa <- wt_avg(pct_df)        # calc weighted average and apply reliability screens
wa <- wa %>% 
  select(target_id, everything()) %>% 
  rename(geoid = target_id)



############# 8. Calculate RC calcs ##############
d <- wa %>% 
  rename(rate = total_rate, pop = total_pop) %>% 
  select(-n)


##### calculate RACE COUNTS Index of Disparity (calc_id function)
# Set "max" or "min" depending on indicator
asbest <- "min"
d$asbest <- asbest

# Find best rate 
best <- min(d$rate, na.rm=T)
d$best <- best

# Calculate the difference from best for each SPA
d$diff <- abs(best - d$rate)

# Determine valid counts (number of non-NA rates)
values_count <- length(d$rate) - sum(is.na(d$rate))

### ID  adapted/removed df name from RC_Functions###
# Calculate sum of differences
sumdiff <- sum(d$diff, na.rm = TRUE)

# Calculate index of disparity
index_of_disparity <-  ifelse(values_count < 2 | values_count == 2 & asbest == 'min' & sumdiff == best, NA,
                              (((sumdiff / best) / (values_count - 1)) * 100))

d$index_of_disparity <- index_of_disparity
d$values_count <- values_count

d <- d %>% 
  mutate(name = case_when(geoid == 1 ~ "Antelope Valley",
                                   geoid == 2 ~ "San Fernando Valley",
                                   geoid == 3 ~ "San Gabriel Valley",
                                   geoid == 4 ~ "Metro LA",
                                   geoid == 5 ~ "West LA",
                                   geoid == 6 ~ "South LA",
                                   geoid == 7 ~ "East LA",
                                   geoid == 8  ~ "South Bay")) %>% 
  select(geoid, name, everything())


##### 9. Send to Postgres #####
con3 <- connect_to_db("bold_vision")
table_name <- "hbe_pollution_burden_region"
schema <- 'bv_2023'

indicator <- "Average Pollution Burden weighted by sensitive land uses for LA County SPAs (Service Planning Areas)"
source <- "GreenInfo Network California Protected Areas Database (2023a), California Community Care Licensing Division (2023), GreenInfo California School Campus Database (2021), California Office of Environmental Health Hazard Assessment CalEnviroScreen 4.0 (2021), Census DHC (2020). See QA doc for details: W:\\Project\\OSI\\Bold Vision\\BV 2023\\Documentation\\Healthy Built Environment\\QA_CWB_Pollution_Burden_calcs.docx"

dbWriteTable(con3, c(schema, table_name), d,
             overwrite = FALSE, row.names = FALSE)

#comment on table and columns
comment <- paste0("COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
                                         COMMENT ON COLUMN ", schema, ".", table_name, ".geoid IS 'SPA number';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".name IS 'SPA name';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".rate IS 'average percentile of pollution burden';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".pop IS 'SPA youth population, ages 0-24 years old';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".best IS 'best rate among SPAs';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".diff IS 'difference from the best';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".index_of_disparity IS 'index of disparity';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".values_count IS 'number of values';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".asbest IS 'minimum or maximum as best';")
print(comment)
dbSendQuery(con3, comment)

#disconnect
dbDisconnect(con3)