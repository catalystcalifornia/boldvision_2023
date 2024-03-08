#Calculate weighted average of ECE enrollment for LA County SPAs,
#using RACE COUNTS functions and Census DHC populations

library(readxl)
library(dplyr)
library(RPostgreSQL)

source("W:\\RDA Team\\R\\credentials_source.R")
source("W:\\Project\\OSI\\Bold Vision\\BV 2023\\R\\Positive Youth Development\\ece_access_indicator.R")

#join SPA ids
bv_conn <- connect_to_db("bold_vision")
zip_spa_xwalk <- dbGetQuery(bv_conn, "select * from bv_2023.crosswalk_zip_spas_2023")
dbDisconnect(bv_conn)

#format for wa later
ind_df <- left_join(ind_df, zip_spa_xwalk %>% rename(sub_id = zipcode), by = "sub_id") %>%
  filter(is.na(spa) != TRUE) %>% filter(is.na(indicator) != TRUE) %>% 
  select(-spa_name, -target_id) %>% rename(target_id = spa) %>%
  select(target_id, sub_id, geoname, indicator, prc_zip_code_area)



##### 5. get zcta under5 pop by race ####

bv_conn <- connect_to_db("bold_vision")
under5_df <- dbGetQuery(bv_conn, "select * from bv_2023.dhc_zcta_2020_under5_race")
dbDisconnect(bv_conn)

#make wide
under5_df_wide <- under5_df %>% pivot_wider(names_from = race, values_from = pop) %>% select(geoid, total)

#rename
names(under5_df_wide) <- c("sub_id", "total_sub_pop")


# add spa id for join later
under5_df_wide <- left_join(ind_df, under5_df_wide, by="sub_id")

#apply prc_zip to all the population data
under5_df_wide <- under5_df_wide %>% mutate(
  total_sub_pop = prc_zip_code_area * total_sub_pop
)

#remove zips with no population
under5_df_wide <- under5_df_wide %>% filter(total_sub_pop != "NA") %>%
  
  #format (sub_id, sub_pops, target_id)
  select(sub_id, ends_with("pop"), target_id)

#drop prc from ind_df
ind_df <- ind_df %>% select(-prc_zip_code_area)

#add unique id to ind_df so wa works later
ind_df$sub_id <- paste0(ind_df$sub_id, ind_df$target_id)



##### 6. Make spa data from zcta data ####

under5_df_wide_spa <- under5_df_wide %>% group_by(target_id) %>% 
  summarize(
    total_target_pop = sum(total_sub_pop), n=n()
  )


#join zip data to spa data and format
pop_df <- left_join(under5_df_wide, under5_df_wide_spa, by="target_id")
pop_df$geolevel = "zcta"
pop_df <- pop_df %>% select(sub_id, target_id, geolevel, everything())

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
wa <- wa %>% select(target_id, everything()) %>% rename(geoid = target_id)



############# 8. Calculate RC calcs ##############

d <- wa %>% rename(rate = total_rate, pop = total_pop) %>% select(-n)


##### calculate RACE COUNTS stats
asbest <- "max"
d$asbest <- "max"

best <- max(d$rate, na.rm=T)
d$best <- best

d$diff <- best - d$rate
values_count <- length(d$rate) - sum(is.na(d$rate))

### ID  adapted/removed df name from RC_Functions###
sumdiff <- sum(d$diff, na.rm = TRUE)
index_of_disparity <-  ifelse(values_count < 2 | values_count == 2 & asbest == 'max' & sumdiff == best, NA,
                              (((sumdiff / best) / (values_count - 1)) * 100))

d$index_of_disparity <- index_of_disparity
d$values_count <- values_count

d <- d %>% mutate(name = case_when(geoid == 1 ~ "Antelope Valley",
                                   geoid == 2 ~ "San Fernando Valley",
                                   geoid == 3 ~ "San Gabriel Valley",
                                   geoid == 4 ~ "Metro LA",
                                   geoid == 5 ~ "West LA",
                                   geoid == 6 ~ "South LA",
                                   geoid == 7 ~ "East LA",
                                   geoid == 8  ~ "South Bay")) %>% 
  select(geoid, name, everything())



###Send to Postgres###

con3 <- connect_to_db("bold_vision")
table_name <- "pyd_ece_access_region"
schema <- 'bv_2023'

indicator <- "ECE enrollment rate"
source <- "Weighted average by SPA of age under 5 ECE enrollment rate from American Institutes for Research (2020), California Child Care Resource & Referral Network (2021), Census DHC (2020). See QA doc for details: W:\\Project\\OSI\\Bold Vision\\BV 2023\\Documentation\\Positive Youth Development\\QA_ECE_Access.docx"
dbWriteTable(con3, c(schema, table_name), d,
             overwrite = FALSE, row.names = FALSE)

#comment on table and columns
comment <- paste0("COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
                                         COMMENT ON COLUMN ", schema, ".", table_name, ".geoid IS 'SPA number';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".name IS 'SPA name';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".rate IS 'indicator rate';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".pop IS 'SPA age under 5 population';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".best IS 'best rate among SPAs';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".diff IS 'difference from the best';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".index_of_disparity IS 'index of disparity';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".values_count IS 'number of values';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".asbest IS 'minimum or maximum as best';")
print(comment)
dbSendQuery(con3, comment)

#disconnect
dbDisconnect(con3)