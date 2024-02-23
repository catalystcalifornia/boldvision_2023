#Calculate weighted average of impervious land for LA county and RACE COUNTS stats
#for Bold Vision 2023 update

##install packages if not already installed ------------------------------
list.of.packages <- c("dplyr","data.table","sf","tigris","readr","tidyr","DBI","RPostgreSQL","tidycensus", "rvest", "tidyverse", "stringr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages)
library(dplyr)
library(data.table)
library(tidycensus)
library(sf)
library(RPostgreSQL)
library(stringr)
library(tidyr)
library(tigris)
options(scipen=999)

###### SET UP WORKSPACE #######
# create connection for rda database
source("W:\\RDA Team\\R\\credentials_source.R")


##### 1. GET INDICATOR DATA ######
# You MUST load indicator data using these table/column names (ind_df / subid / indicator) in order for functions to work
ind_df <- read.table("W:\\Data\\Built Environment\\NationalLandCoverDatabase\\2021\\nlcd_census_tract_pct_impervious_2021.txt", header = TRUE, sep = ",")
ind_df <- ind_df %>% rename(sub_id = GEOID, indicator = MEDIAN) %>% select(sub_id, indicator)
ind_df$sub_id <- as.character(paste0("0", ind_df$sub_id))

# add county id
ind_df$county = substr(ind_df$sub_id, 1, 5)

#filter for la county
ind_df <- ind_df %>% filter(county == '06037')

#join SPA ids
source("W:/Project/OSI/Bold Vision/BV 2023/R/boldvision_22_23/tract_spa_crosswalk.R")
tract_spa_xwalk <- tract_spa_xwalk %>% rename(sub_id = geoid)

ind_df <- left_join(ind_df, tract_spa_xwalk, by="sub_id") %>% 
  rename(target_id = spa, geoname = spa_name) %>%
  select(target_id, sub_id, geoname, indicator) %>%
  as.data.frame()


################ 2. Get tract population data #################
source("W:/Project/OSI/Bold Vision/BV 2023/R/boldvision_22_23/ct_youth_population_by_race.R")

#make wide
youth_df_wide <- youth_df %>% pivot_wider(names_from = race, values_from = pop) %>% select(geoid, total)

#rename
names(youth_df_wide) <- c("sub_id", "total_sub_pop")

#add target_id to join to spa data later
youth_df_wide <- left_join(youth_df_wide, ind_df, by = "sub_id") %>% select(-geoname, -indicator)

#update target_id for the 3 tracts that fail in join (because they're water)
youth_df_wide["target_id"][youth_df_wide["sub_id"] == "06037990100"] <- "5"
youth_df_wide["target_id"][youth_df_wide["sub_id"] == "06037990200"] <- "8"
youth_df_wide["target_id"][youth_df_wide["sub_id"] == "06037990300"] <- "8"


############### 3. Get SPA population data ####################

#group tract data by spa and summarize
youth_df_wide_spa <- youth_df_wide %>% group_by(target_id) %>% 
  summarize(
    total_target_pop = sum(total_sub_pop), n=n()
  )

#join tract data with spa data and format
pop_df <- left_join(youth_df_wide, youth_df_wide_spa, by="target_id")
pop_df$geolevel = "tract"
pop_df <- pop_df %>% select(sub_id, target_id, geolevel, everything())

###############4. Run weighted averages###############
#set source for WA Functions script
source("W:/RDA Team/R/Functions/Cnty_St_Wt_Avg_Functions.R")

#run pct_df
pop_threshold = 0
pct_df <- pop_pct(pop_df) 

#run wa
wa <- wt_avg(pct_df)        # calc weighted average and apply reliability screens

names(spas) <- c("target_id", "name", "geometry")
wa <- left_join(wa, spas, by="target_id") %>% select(-geometry)
wa <- wa %>% select(target_id, name, everything()) %>% rename(geoid = target_id)

############ ADDITIONAL SCREENING FOR GREENSPACE ####################
# # This screen converts any rate = 0 into NA, bc it's virtually impossible for there to be no roads, parking lots, or roofs
#wa[, 3:12][wa[, 3:12] == 0] <- NA  # screens only cols 3:12 which are the _rate columns

############# 5. Calculate RC calcs ##############
d <- wa %>% rename(rate = total_rate, pop = total_pop) %>% select(-n)


##### calculate RACE COUNTS stats
asbest <- "min"
d$asbest <- "min"

best <- min(d$rate, na.rm=T)
d$best <- best

d$diff <- d$rate - best
values_count <- length(d$rate) - sum(is.na(d$rate))

### ID  adapted/removed df name from RC_Functions###
sumdiff <- sum(d$diff, na.rm = TRUE)
#index_of_disparity <-  ifelse(values_count < 2 | values_count == 2 & asbest == 'min' & sumdiff == best, NA,
index_of_disparity <-     (((sumdiff / best) / (values_count - 1)) * 100)#)

d$index_of_disparity <- index_of_disparity
d$values_count <- values_count



###Send to Postgres###
con3 <- connect_to_db("bold_vision")
table_name <- "hbe_lack_of_greenspace_region"
schema <- 'bv_2023'

indicator <- "Impervious Landcover (%) is the weighted average of percentage of impervious land cover out of all land cover by race. Impervious land cover includes roads, roof tops, and parking lots"
source <- "Multi-Resolution Land Characteristics Consortium, National Land Cover Database (2019), Census DHC (2020). See QA doc for details: W:\\Project\\OSI\\Bold Vision\\BV 2023\\Documentation\\Healthy Built Environment\\QA_Impervious_Land.docx"

dbWriteTable(con3, c(schema, table_name), d,
             overwrite = FALSE, row.names = FALSE)

#comment on table and columns
comment <- paste0("COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".geoid IS 'SPA number';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".name IS 'SPA name';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".rate IS 'indicator rate';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".pop IS 'SPA youth population';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".asbest IS 'minimum or maximum as best';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".best IS 'best rate among SPAs';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".diff IS 'difference from the best';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".index_of_disparity IS 'index of disparity';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".values_count IS 'number of values';")
print(comment)
dbSendQuery(con3, comment)

#disconnect
dbDisconnect(con3)