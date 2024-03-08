#Calculate weighted average of ECE enrollment by race for LA County 
#using RACE COUNTS functions and Census DHC populations

library(readxl)
library(dplyr)
library(RPostgreSQL)
library(sf)

source("W:\\RDA Team\\R\\credentials_source.R")
source("W:\\Project\\OSI\\Bold Vision\\BV 2023\\R\\Positive Youth Development\\ece_access_indicator.R")


##### 5. get zcta under5 pop by race ####

bv_con <- connect_to_db("bold_vision")
under5_df <- st_read(bv_con, query = "select * from bv_2023.dhc_zcta_2020_under5_race")
dbDisconnect(bv_con)


#make wide
under5_df_wide <- under5_df %>% pivot_wider(names_from = race, values_from = pop)

#rename
names(under5_df_wide) <- c("sub_id", "total_sub_pop", "aian_sub_pop", "latino_sub_pop", "nh_asian_sub_pop",
                           "nh_black_sub_pop", "nh_twoormor_sub_pop", "nh_other_sub_pop", "nh_white_sub_pop",
                           "pacisl_sub_pop", "bipoc_sub_pop")


# add county id after joining to filter for just LA County ZIP codes
bv_con <- connect_to_db("bold_vision")
zip_codes <- st_read(bv_con, query = "select * from bv_2023.crosswalk_zip_spas_2023")
zip_codes<-distinct(zip_codes, zipcode)
dbDisconnect(bv_con)


under5_df_wide <- left_join(zip_codes %>% rename(sub_id = zipcode), under5_df_wide, by="sub_id")
under5_df_wide$target_id <- "06037"

#remove zips with no population
under5_df_wide <- under5_df_wide %>% filter(total_sub_pop != "NA") %>% filter(total_sub_pop > 0)


##### 6. Make county data from zcta data ####

under5_df_wide_county <- under5_df_wide %>% group_by(target_id) %>% 
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


#join zip data to county data and format
pop_df <- left_join(under5_df_wide, under5_df_wide_county, by="target_id")
pop_df$geolevel = "zcta"
pop_df <- pop_df %>% select(sub_id, target_id, geolevel, everything())


########### 7. Run weighted average ################
#set source for WA Functions script
source("W:/RDA Team/R/Functions/Cnty_St_Wt_Avg_Functions.R")

#run pct_df
pop_threshold = 0
pct_df <- pop_pct(pop_df) 

#run wa
wa <- wt_avg(pct_df)        # calc weighted average and apply reliability screens
wa$county <- "Los Angeles County"
wa <- wa %>% select(target_id, county, everything()) %>% rename(geoid = target_id)



############# 8. Calculate RC calcs ##############
d <- wa

#remove bipoc from ID calcs
d <- d %>% select(-bipoc_rate, -bipoc_pop)

#set source for RC Functions script
source("W:/Project/RACE COUNTS/Functions/RC_Functions.R")

d$asbest = 'max'    #YOU MUST UPDATE THIS FIELD AS NECESSARY: assign 'max' or 'max'

d <- count_values(d) #calculate number of "_rate" values
d <- calc_best(d) #calculate best rates -- be sure to update previous line of code accordingly before running this function.
d <- calc_diff(d) #calculate difference from best
#d <- calc_avg_diff(d) #calculate (row wise) mean difference from best
#d <- calc_s_var(d) #calculate (row wise) population or sample variance. be sure to use calc_s_var for sample data or calc_p_var for population data.
d <- calc_id(d) #calculate index of disparity

#add bipoc data back
county_table <- left_join(d, wa %>% select(geoid, bipoc_rate, bipoc_pop), by="geoid")

#pivot to Elycia's desired format
county_table <- county_table %>% select(-county, -asbest, -n, -best, -values_count, -index_of_disparity) %>% 
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


###Send to Postgres###
con3 <- connect_to_db("bold_vision")
table_name <- "pyd_ece_access_subgroup"
schema <- 'bv_2023'

indicator <- "ECE enrollment rate"
source <- "Weighted average by race of age under 5 ECE enrollment rate from American Institutes for Research (2020), California Child Care Resource & Referral Network (2021), Census DHC (2020). See QA doc for details: W:\\Project\\OSI\\Bold Vision\\BV 2023\\Documentation\\Positive Youth Development\\QA_ECE_Access.docx"

dbWriteTable(con3, c(schema, table_name), county_table,
             overwrite = FALSE, row.names = FALSE)

#comment on table and columns
comment <- paste0("COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
                                         COMMENT ON COLUMN ", schema, ".", table_name, ".geoid IS 'County fips';
                                         COMMENT ON COLUMN ", schema, ".", table_name, ".name IS 'county name';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".subgroup IS 'race';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".rate IS 'indicator rate';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".pop IS 'age under 5 subgroup population';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".diff IS 'difference from best rate';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".best IS 'best rate';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".index_of_disparity IS 'index_of_disparity';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".values_count IS 'subgroups with values';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".asbest IS 'whether minimum or maximum is best';")
print(comment)
dbSendQuery(con3, comment)

#disconnect
dbDisconnect(con3)
