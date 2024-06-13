# Calculate access to grassroots and base-building orgs for youth 0-24, and by race, BIPOC, and SPA
# method is based on the enhanced two-step floating catchment area (E2SFCA) method where access is a function of supply-demand ratio around each org (youth population served within 3,7,10 miles) and the sum of supply-demand ratios for each org within 3,7,10 miles of each census tract population-weighted centroid

#### ENVIRONMENT SET UP ----
library(data.table)
library(readxl)
library(dplyr)
library(RPostgreSQL)
library(sf)
library(tidyr)
library(stringr)
# install.packages("rPraat")
library(rPraat)
library(nngeo)
library(tidygeocoder)
options(scipen=999)


source("W:\\RDA Team\\R\\credentials_source.R")
bv_con <- connect_to_db("bold_vision")

#### Get indicator data ----
ind_df <- st_read(bv_con, query = "select * from bv_2023.yp_baseorgs_tract_indicator")
ind_df <- ind_df %>% rename(sub_id = ct_geoid) %>% select(sub_id, indicator_final)
ind_df <- ind_df %>% rename(indicator = indicator_final)
ind_df$target_id <- "06037"


#### Part 1: Calculate weighted average at county level by youth race -----
###### Step 1: Get census tract youth population data -----
youth_pop_tract <- st_read(bv_con, query = "select * from bv_2023.dhc_tract_2020_youth_0_24_race")

#make wide
youth_pop_wide <- youth_pop_tract %>% pivot_wider(names_from = race, values_from = pop)

#rename
names(youth_pop_wide) <- c("sub_id", "total_sub_pop", "aian_sub_pop", "latino_sub_pop", "nh_asian_sub_pop",
                           "nh_black_sub_pop", "nh_twoormor_sub_pop", "nh_other_sub_pop", "nh_white_sub_pop",
                           "pacisl_sub_pop", "bipoc_sub_pop")

# add county id
youth_pop_wide$target_id = substr(youth_pop_wide$sub_id, 1, 5)


###### Step 2: Make county population data from tract data -------

youth_df_wide_county <- youth_pop_wide %>% group_by(target_id) %>% 
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
pop_df <- left_join(youth_pop_wide, youth_df_wide_county, by="target_id")
pop_df$geolevel = "tract"
pop_df <- pop_df %>% select(sub_id, target_id, geolevel, everything())

###### Step 3: Run weighted average ------
#set source for WA Functions script
source("W:/RDA Team/R/Functions/Cnty_St_Wt_Avg_Functions.R")

#run pct_df
pop_threshold = 0
pct_df <- pop_pct(pop_df) 

#run wa
wa <- wt_avg(pct_df)        # calc weighted average and apply reliability screens
wa$county <- "Los Angeles County"
wa <- wa %>% select(target_id, county, everything()) %>% rename(geoid = target_id)

##### Step 4: Calculate Index of disparity ------
d <- wa

# remove bipoc from ID calcs
d <- d %>% select(-bipoc_rate, -bipoc_pop)

#set source for RC Functions script
source("W:/Project/RACE COUNTS/Functions/RC_Functions.R")

d$asbest = 'max'    #YOU MUST UPDATE THIS FIELD AS NECESSARY: assign 'min' or 'max'

d <- count_values(d) #calculate number of "_rate" values
d <- calc_best(d) #calculate best rates -- be sure to update previous line of code accordingly before running this function.
d <- calc_diff(d) #calculate difference from best
#d <- calc_avg_diff(d) #calculate (row wise) mean difference from best
#d <- calc_s_var(d) #calculate (row wise) population or sample variance. be sure to use calc_s_var for sample data or calc_p_var for population data.
d <- calc_id(d) #calculate index of disparity

#add bipoc data back
county_table <- left_join(d, wa %>% select(geoid, bipoc_rate, bipoc_pop), by="geoid")

# pivot to long
county_table_long <- county_table %>% select(-county, -asbest, -n, -best, -values_count, -index_of_disparity) %>% 
  pivot_longer(!geoid, names_to = "subgroup", values_to = "metric") %>% 
  mutate(variable = case_when(grepl("_rate", subgroup) ~ "rate",
                              grepl("_pop", subgroup) ~ "pop",
                              grepl("_diff", subgroup) ~ "diff"))%>%
  mutate(across(subgroup, str_replace, "_rate", "")) %>%
  mutate(across(subgroup, str_replace, "_pop", "")) %>%
  mutate(across(subgroup, str_replace, "_diff", "")) %>%
  pivot_wider(names_from = variable, values_from = metric) %>%
  left_join(county_table %>% select(geoid, best, index_of_disparity, values_count, asbest), by = "geoid") %>%
  mutate(diff = ifelse(subgroup %in% c("bipoc","total"), NA, rate - best),
         index_of_disparity=ifelse(subgroup %in% c("bipoc","total"), NA, index_of_disparity))

county_table_long$best[county_table_long$subgroup %in% c("bipoc","total")]<-NA
county_table_long$values_count[county_table_long$subgroup %in% c("bipoc","total")]<-NA
county_table_long$asbest[county_table_long$subgroup %in% c("bipoc","total")]<-NA

county_table_long$name<-"Los Angeles County"

county_table_long<-county_table_long%>%mutate(diff=abs(diff))%>%select(geoid,name,everything())

###### Send to postgres -----
table_name <- "yp_baseorgs_subgroup"
schema <- 'bv_2023'
d_pg<-county_table_long

indicator <- "Access to grassroots, base building orgs and social advocacy orgs focusing on youth is the weighted average of the average access percentile for youth 0-24 by race based on census tract level. Rate indicates the average percentile of access each youth racial group has ages 0-24 have based on census tract population of youth"
source <- "See R Script for details: W:/Project/OSI/Bold Vision/BV 2023/R/Youth Power/yp_baseorgs.R"

dbWriteTable(bv_con, c(schema, table_name), d_pg,
             overwrite = FALSE, row.names = FALSE)

#comment on table and columns
comment <- paste0("COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".geoid IS 'County ID';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".name IS 'County name';
                   COMMENT ON COLUMN ", schema, ".", table_name, ".subgroup IS 'race subgroup including bipoc and total';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".rate IS 'indicator rate for each subgroup- average percentile of access to grassroots, basebuilding, youth advocacy orgs based on CT percentiles and population distribution in each CT. Higher percentile is higher access';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".pop IS 'Youth population 0-24 for each racial group';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".asbest IS 'minimum or maximum as best';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".best IS 'best rate among SPAs';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".diff IS 'difference from the best';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".index_of_disparity IS 'index of disparity';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".values_count IS 'number of values';")
# print(comment)
dbSendQuery(bv_con, comment)

#### Part 2: Calculate weighted average by SPA -----
###### Step 1: Get indicator data and join SPA ids -----
ind_df <- ind_df %>% rename(county=target_id) %>% select(sub_id, indicator,county)

#join SPA ids
tract_spa_xwalk <- st_read(bv_con, query = "select * from bv_2023.crosswalk_tract_spas_2023")
tract_spa_xwalk <- tract_spa_xwalk %>% rename(sub_id = geoid)

ind_df <- left_join(ind_df, tract_spa_xwalk, by="sub_id") %>% 
  rename(target_id = spa, geoname = spa_name) %>%
  select(target_id, sub_id, geoname, indicator) %>%
  as.data.frame()

###### Step 2: Get tract population data -----
#add target_id to join to spa data later
youth_df_wide <- left_join(youth_pop_wide%>%select(-c(target_id)), ind_df, by = "sub_id") %>% select(-geoname, -indicator)

#update target_id for the 3 tracts that fail in join (because they're water)
youth_df_wide["target_id"][youth_df_wide["sub_id"] == "06037990100"] <- "5"
youth_df_wide["target_id"][youth_df_wide["sub_id"] == "06037990200"] <- "8"
youth_df_wide["target_id"][youth_df_wide["sub_id"] == "06037990300"] <- "8"

###### Step 3: Get SPA population data -----
# group tract data by spa and summarize
youth_df_wide_spa <- youth_df_wide %>% group_by(target_id) %>% 
  summarize(
    total_target_pop = sum(total_sub_pop), n=n()
  )

#join tract data with spa data and format
pop_df <- left_join(youth_df_wide, youth_df_wide_spa, by="target_id")
pop_df$geolevel = "tract"
pop_df <- pop_df %>% select(sub_id, target_id, geolevel, everything())

###### Step 4: Run weighted averages at SPA level -----
#set source for WA Functions script
source("W:/RDA Team/R/Functions/Cnty_St_Wt_Avg_Functions.R")

#run pct_df
pop_threshold = 0
pct_df <- pop_pct(pop_df) 

#run wa
wa <- wt_avg(pct_df)        # calc weighted average and apply reliability screens

# add spa info to it
rda_con <- connect_to_db("rda_shared_data")
spas <- st_read(rda_con, query = "select * from geographies_la.la_county_service_planning_areas_2022")
dbDisconnect(rda_con)

wa <- left_join(wa, spas%>%select(SPA_NAME,SPA), by=c("target_id"="SPA"))
wa<-wa%>%rename(name=SPA_NAME)%>% select(target_id, name, total_rate,total_pop) %>% rename(geoid = target_id)

###### Step 5: Calculate index of disparity -----
d <- wa %>% rename(rate = total_rate, pop = total_pop)

asbest <- "max"
d$asbest <- "max"

best <- max(d$rate, na.rm=T)
d$best <- best

d$diff <- abs(d$rate - best)
values_count <- length(d$rate) - sum(is.na(d$rate))

sumdiff <- sum(d$diff, na.rm = TRUE)
index_of_disparity <-  ifelse(values_count < 2 | values_count == 2 & asbest == 'max' & sumdiff == best, NA,
                              (((sumdiff / best) / (values_count - 1)) * 100))

d$index_of_disparity <- index_of_disparity
d$values_count <- values_count


###### Send to postgres -----
table_name <- "yp_baseorgs_region"
schema <- 'bv_2023'
d_pg<-d

indicator <- "Access to grassroots, base building orgs and social advocacy orgs focusing on youth is the weighted average of the average access percentile for youth 0-24 based on census tract level. Rate indicates the average percentile of access census tracts in that SPA have based on census tract population of youth"
source <- "See R Script for details: W:/Project/OSI/Bold Vision/BV 2023/R/Youth Power/yp_baseorgs.R"

dbWriteTable(bv_con, c(schema, table_name), d,
             overwrite = FALSE, row.names = FALSE)

#comment on table and columns
comment <- paste0("COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".geoid IS 'SPA number';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".name IS 'SPA name';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".rate IS 'indicator rate - average percentile of access to grassroots, basebuilding, youth advocacy orgs';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".pop IS 'SPA youth population ages 0-24';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".asbest IS 'minimum or maximum as best';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".best IS 'best rate among SPAs';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".diff IS 'difference from the best';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".index_of_disparity IS 'index of disparity';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".values_count IS 'number of values';")
print(comment)
dbSendQuery(bv_con, comment)

#disconnect
dbDisconnect(bv_con)