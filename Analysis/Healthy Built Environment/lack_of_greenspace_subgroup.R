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

# get census county geoids to paste to the front of the tracts from the data-----
census_api_key(census_key1, overwrite=TRUE) # In practice, may need to include install=TRUE if switching between census api keys
Sys.getenv("CENSUS_API_KEY")
ca <- get_acs(geography = "county", 
              variables = c("B01001_001"), 
              state = "CA", 
              year = 2021)
ca <- ca[,1:2]
ca$NAME <- gsub(" County, California", "", ca$NAME)
names(ca) <- c("geoid", "geoname")


#merge dfs by geoname then paste the county id to the front of the tract IDs
ind_df <- left_join(ca, ind_df, by = c("geoid" = "county")) %>% 
  rename(c("target_id" = "geoid")) %>%
  select(target_id, sub_id, geoname, indicator)%>% 
  as.data.frame()


################ 2. Get tract population data #################
source("W:/Project/OSI/Bold Vision/BV 2023/R/boldvision_22_23/ct_youth_population_by_race.R")

#make wide
youth_df_wide <- youth_df %>% pivot_wider(names_from = race, values_from = pop)

#rename
names(youth_df_wide) <- c("sub_id", "total_sub_pop", "aian_sub_pop", "latino_sub_pop", "nh_asian_sub_pop",
                          "nh_black_sub_pop", "nh_twoormor_sub_pop", "nh_other_sub_pop", "nh_white_sub_pop",
                          "pacisl_sub_pop", "bipoc_sub_pop")

# add county id
youth_df_wide$target_id = substr(youth_df_wide$sub_id, 1, 5)


########### 3. Make county population data from tract data ################

youth_df_wide_county <- youth_df_wide %>% group_by(target_id) %>% 
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
pop_df <- left_join(youth_df_wide, youth_df_wide_county, by="target_id")
pop_df$geolevel = "tract"
pop_df <- pop_df %>% select(sub_id, target_id, geolevel, everything())

########### 4. Run weighted average ################
#set source for WA Functions script
source("W:/RDA Team/R/Functions/Cnty_St_Wt_Avg_Functions.R")

#run pct_df
pop_threshold = 0
pct_df <- pop_pct(pop_df) 

#run wa
wa <- wt_avg(pct_df)        # calc weighted average and apply reliability screens
wa$county <- "Los Angeles County"
wa <- wa %>% select(target_id, county, everything()) %>% rename(geoid = target_id)

############ ADDITIONAL SCREENING FOR GREENSPACE ####################
# # This screen converts any rate = 0 into NA, bc it's virtually impossible for there to be no roads, parking lots, or roofs
#wa[, 3:12][wa[, 3:12] == 0] <- NA  # screens only cols 3:12 which are the _rate columns

############# 5. Calculate RC calcs ##############
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
  mutate(diff = rate - best)


###Send to Postgres###
con3 <- connect_to_db("bold_vision")
table_name <- "hbe_lack_of_greenspace_subgroup"
schema <- 'bv_2023'

indicator <- "Impervious Landcover (%) is the weighted average of percentage of impervious land cover out of all land cover by race. Impervious land cover includes roads, roof tops, and parking lots"
source <- "Multi-Resolution Land Characteristics Consortium, National Land Cover Database (2019), Census DHC (2020). See QA doc for details: W:\\Project\\OSI\\Bold Vision\\BV 2023\\Documentation\\Healthy Built Environment\\QA_Impervious_Land.docx"

dbWriteTable(con3, c(schema, table_name), county_table,
             overwrite = FALSE, row.names = FALSE)

#comment on table and columns
comment <- paste0("COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
                                         COMMENT ON COLUMN ", schema, ".", table_name, ".geoid IS 'County fips';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".subgroup IS 'race';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".rate IS 'indicator rate';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".pop IS 'subgroup population';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".diff IS 'difference from best rate';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".best IS 'best rate';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".index_of_disparity IS 'index_of_disparity';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".values_count IS 'subgroups with values';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".asbest IS 'whether minimum or maximum is best';")
print(comment)
dbSendQuery(con3, comment)

#disconnect
dbDisconnect(con3)