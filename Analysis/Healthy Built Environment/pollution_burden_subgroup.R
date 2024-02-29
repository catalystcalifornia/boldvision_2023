# Calculate weighted average of Pollution Burden by race for LA County 
# using RACE COUNTS functions and Census DHC populations

library(dplyr)

source("W:\\RDA Team\\R\\credentials_source.R")

options(scipen=99, digits=15)
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


##### 4. get tract 0-24 pop by race #####
race_0_24_sql <- "SELECT * FROM bv_2023.dhc_tract_2020_youth_0_24_race;"
race_0_24 <- dbGetQuery(bv_conn, race_0_24_sql) 

#make wide
race_0_24_wide <- race_0_24 %>% 
  pivot_wider(names_from = race, values_from = pop)

#rename race groups with "_sub_pop" suffix
names(race_0_24_wide)[2:11] <- paste(names(race_0_24_wide)[2:11], "sub_pop", sep="_")


# add county id by joining with just LA County census tracts
race_0_24_wide <- race_0_24_wide %>%
  left_join(tract_spa_xwalk, by=c("geoid"="geoid")) %>%
  rename(sub_id=geoid)
race_0_24_wide$target_id <- "06037"

#remove tracts with no population
race_0_24_wide <- race_0_24_wide %>% 
  filter(total_sub_pop != "NA") %>% 
  filter(total_sub_pop > 0)


##### 5. Make county data from tract data ####

race_0_24_wide_county <- race_0_24_wide %>% 
  group_by(target_id) %>% 
  summarize(
    total_target_pop = sum(total_sub_pop),
    aian_target_pop = sum(aian_sub_pop),
    latino_target_pop = sum(latino_sub_pop),
    nh_asian_target_pop = sum(nh_asian_sub_pop),
    nh_black_target_pop = sum(nh_black_sub_pop),
    nh_twoormor_target_pop = sum(nh_twoormor_sub_pop),
    nh_other_target_pop = sum(nh_other_sub_pop),
    nh_white_target_pop = sum(nh_white_sub_pop),
    pacisl_target_pop = sum(pacisl_sub_pop),
    bipoc_target_pop = sum(bipoc_sub_pop),
    n=n()
  )


#join tract data to county data and format
pop_df <- left_join(race_0_24_wide, race_0_24_wide_county, by="target_id")
pop_df$geolevel = "tract"
pop_df <- pop_df %>% select(sub_id, target_id, geolevel, everything())


########### 6. Run weighted average ################
#set source for WA Functions script
source("W:/RDA Team/R/Functions/Cnty_St_Wt_Avg_Functions.R")

#run pct_df
pop_threshold = 0
pct_df <- pop_pct(pop_df) 

# run wa
# NOTE: table must have the following for wa to work:
# ind_df table (with cols: sub_id and indicator)
wa <- wt_avg(pct_df)        # calc weighted average and apply reliability screens
wa$county <- "Los Angeles County"
wa <- wa %>% select(target_id, county, everything()) %>% rename(geoid = target_id)


# # quick check - ind_df census tracts are fewer than race
# # 5 census tracts with ces data, no pop data
# # 474 census tracts with race pops, no ces data
# # noting because n col in wa table is 2487, but only 2018 tracts have pollution burden values
poll_tracts <- ind_df %>%
  select(sub_id)
race_tracts <- race_0_24_wide %>%
  select(sub_id)

missing1 <- anti_join(poll_tracts, race_tracts)
missing2 <- anti_join(race_tracts, poll_tracts) %>% nrow()
############# 7. Calculate RC calcs ##############
d <- wa

#remove bipoc from ID calcs
d <- d %>% select(-bipoc_rate, -bipoc_pop)

#set source for RC Functions script
source("W:/Project/RACE COUNTS/Functions/RC_Functions.R")

d$asbest = 'min'    #YOU MUST UPDATE THIS FIELD AS NECESSARY: assign 'min' or 'max'

d <- count_values(d) #calculate number of "_rate" values
d <- calc_best(d) #calculate best rates -- be sure to update previous line of code accordingly before running this function.
d <- calc_diff(d) #calculate difference from best
#d <- calc_avg_diff(d) #calculate (row wise) mean difference from best
#d <- calc_s_var(d) #calculate (row wise) population or sample variance. be sure to use calc_s_var for sample data or calc_p_var for population data.
d <- calc_id(d) #calculate index of disparity

#add bipoc data back
county_table <- left_join(d, wa %>% select(geoid, bipoc_rate, bipoc_pop), by="geoid")

#pivot to Elycia's desired format
county_table <- county_table %>% 
  select(-county, -asbest, -n, -best, -values_count, -index_of_disparity) %>% 
  pivot_longer(!geoid, names_to = "subgroup", values_to = "metric") %>% 
  mutate(variable = case_when(grepl("_rate", subgroup) ~ "rate",
                              grepl("_pop", subgroup) ~ "pop",
                              grepl("_diff", subgroup) ~ "diff")) %>%
  mutate(across(subgroup, str_replace, "_rate", "")) %>%
  mutate(across(subgroup, str_replace, "_pop", "")) %>%
  mutate(across(subgroup, str_replace, "_diff", "")) %>%
  pivot_wider(names_from = variable, values_from = metric) %>%
  left_join(county_table %>% select(geoid, best, index_of_disparity, values_count, asbest), by = "geoid") %>%
  mutate(name = "Los Angeles County") %>% 
  select(geoid, name, everything())


##### 9. Send to Postgres #####
con3 <- connect_to_db("bold_vision")
table_name <- "hbe_pollution_burden_subgroup"
schema <- 'bv_2023'

indicator <- "Average Pollution Burden weighted by sensitive land uses for different racial subgroups in LA County"
source <- "GreenInfo Network California Protected Areas Database (2023a), California Community Care Licensing Division (2023), GreenInfo California School Campus Database (2021), California Office of Environmental Health Hazard Assessment CalEnviroScreen 4.0 (2021), Census DHC (2020). See QA doc for details: W:\\Project\\OSI\\Bold Vision\\BV 2023\\Documentation\\Healthy Built Environment\\QA_CWB_Pollution_Burden_calcs.docx"

dbWriteTable(con3, c(schema, table_name), county_table,
             overwrite = FALSE, row.names = FALSE)

#comment on table and columns
comment <- paste0("COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
                                         COMMENT ON COLUMN ", schema, ".", table_name, ".geoid IS 'County fips';
                                         COMMENT ON COLUMN ", schema, ".", table_name, ".name IS 'county name';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".subgroup IS 'race';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".rate IS 'average percentile of pollution burden';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".pop IS 'subgroup population, ages 0-24 years old';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".diff IS 'difference from best rate';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".best IS 'best rate';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".index_of_disparity IS 'index_of_disparity';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".values_count IS 'subgroups with values';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".asbest IS 'whether minimum or maximum is best';")
print(comment)
dbSendQuery(con3, comment)

#disconnect
dbDisconnect(con3)
