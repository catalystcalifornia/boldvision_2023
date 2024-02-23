#Systems Impact Child Welfare (Youth Foster Care and Probation from CCWIP)

#install packages if not already installed
list.of.packages <- c("ipumsr","data.table","dplyr","tidycensus","forcats","RPostgreSQL", "sf", "stringr", "XLConnect", "purrr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#load libraries
library(data.table)
library(dplyr)
library(tidycensus)
library(forcats)
library(RPostgreSQL)
library(sf)
library(tidyverse)
library(here)
library(stringr)
library(readxl)
library(tigris)
library(janitor)
library(XLConnect)
library(purrr)
options(scipen = 100) # disable scientific notation

#source in the passwords
source("W:/RDA Team/R/credentials_source.R")

##get pop data for ages 0-20 and push it to postgres----

## I commented it out after pushing it to postgres, I also added nh_aian since this indicator uses it
## Calculate corresponding population estimates for youth 0-20 in LA County by race and SPA using same method Chris used for tract-level data for 0-24.
## See QA doc here: W:\Project\OSI\Bold Vision\BV 2023\Documentation\QA_DHC_race_popcalc.docx & W:/Project/OSI/Bold Vision/BV 2023/R/boldvision_22_23/ct_youth_population_by_race.R----

### load variables
# v20 <- load_variables(2020, "dhc")
# # View(v20) #removed variables ending in _009,_010, _033, and _034 to only include ages 0-20 as opposed to 0-24
# 
# # total population variables
# total_pop_vars <- c(total = "P12_001N",
#                     latino = "P12H_001N",
#                     nh_white = "P12I_001N",
#                     nh_black = "P12J_001N",
#                     nh_asian = "P12L_001N",
#                     nh_other = "P12N_001N",
#                     nh_twoormor = "P12O_001N",
#                     aian = "P12Y_001N",
#                     aian = "P12AE_001N",
#                     pacisl = "P12AG_001N",
#                     pacisl = "P12AA_001N") 
# 
# # get total population
# total_pop <- get_decennial(geography = "tract",
#                            variables = total_pop_vars,
#                            state = "CA",
#                            county = "Los Angeles",
#                            year = 2020,
#                            sumfile = "dhc")
# 
# # summarize total population
# total_pop <- total_pop %>% group_by(GEOID, variable) %>% summarize(pop = sum(value))
# 
# #format
# total_pop <- total_pop %>% rename(race = variable) %>% select(GEOID, pop, race)
# 
# head(total_pop)
# 
# # total youth population variables
# total_youth_vars <- c(
#   "P12_003N",
#   "P12_004N",
#   "P12_005N",
#   "P12_006N",
#   "P12_007N",
#   "P12_008N",
#   "P12_027N",
#   "P12_028N",
#   "P12_029N",
#   "P12_030N",
#   "P12_031N",
#   "P12_032N")
# 
# # get total youth
# total_youth <- get_decennial(geography = "tract",
#                              variables = total_youth_vars,
#                              state = "CA",
#                              county = "Los Angeles",
#                              year = 2020,
#                              sumfile = "dhc")
# 
# # summarize total youth
# total_youth <- total_youth %>% group_by(GEOID) %>% summarize(pop = sum(value), race = "total")
# 
# #######################################
# # latino
# ######################################
# 
# # latino youth population variables
# latino_youth_vars <- c(
#   "P12H_003N",
#   "P12H_004N",
#   "P12H_005N",
#   "P12H_006N",
#   "P12H_007N",
#   "P12H_008N",
#   "P12H_027N",
#   "P12H_028N",
#   "P12H_029N",
#   "P12H_030N",
#   "P12H_031N",
#   "P12H_032N")
# 
# # get latino youth
# latino_youth <- get_decennial(geography = "tract",
#                               variables = latino_youth_vars,
#                               state = "CA",
#                               county = "Los Angeles",
#                               year = 2020,
#                               sumfile = "dhc")
# 
# # summarize latino youth
# latino_youth <- latino_youth %>% group_by(GEOID) %>% summarize(pop = sum(value), race = "latino")
# 
# 
# #######################################
# # nh_white
# ######################################
# 
# # nh_white youth population variables
# nh_white_youth_vars <- c(
#   "P12I_003N",
#   "P12I_004N",
#   "P12I_005N",
#   "P12I_006N",
#   "P12I_007N",
#   "P12I_008N",
#   "P12I_027N",
#   "P12I_028N",
#   "P12I_029N",
#   "P12I_030N",
#   "P12I_031N",
#   "P12I_032N")
# 
# # get nh_white youth
# nh_white_youth <- get_decennial(geography = "tract",
#                                 variables = nh_white_youth_vars,
#                                 state = "CA",
#                                 county = "Los Angeles",
#                                 year = 2020,
#                                 sumfile = "dhc")
# 
# # summarize nh_white youth
# nh_white_youth <- nh_white_youth %>% group_by(GEOID) %>% summarize(pop = sum(value), race = "nh_white")
# 
# 
# #######################################
# # nh_black
# ######################################
# 
# # nh_black youth population variables
# nh_black_youth_vars <- c(
#   "P12J_003N",
#   "P12J_004N",
#   "P12J_005N",
#   "P12J_006N",
#   "P12J_007N",
#   "P12J_008N",
#   "P12J_027N",
#   "P12J_028N",
#   "P12J_029N",
#   "P12J_030N",
#   "P12J_031N",
#   "P12J_032N")
# 
# # get nh_black youth
# nh_black_youth <- get_decennial(geography = "tract",
#                                 variables = nh_black_youth_vars,
#                                 state = "CA",
#                                 county = "Los Angeles",
#                                 year = 2020,
#                                 sumfile = "dhc")
# 
# # summarize nh_black youth
# nh_black_youth <- nh_black_youth %>% group_by(GEOID) %>% summarize(pop = sum(value), race = "nh_black")
# 
# 
# #######################################
# # nh_asian
# ######################################
# 
# # nh_asian youth population variables
# nh_asian_youth_vars <- c(
#   "P12L_003N",
#   "P12L_004N",
#   "P12L_005N",
#   "P12L_006N",
#   "P12L_007N",
#   "P12L_008N",
#   "P12L_027N",
#   "P12L_028N",
#   "P12L_029N",
#   "P12L_030N",
#   "P12L_031N",
#   "P12L_032N")
# 
# # get nh_asian youth
# nh_asian_youth <- get_decennial(geography = "tract",
#                                 variables = nh_asian_youth_vars,
#                                 state = "CA",
#                                 county = "Los Angeles",
#                                 year = 2020,
#                                 sumfile = "dhc")
# 
# # summarize nh_asian youth
# nh_asian_youth <- nh_asian_youth %>% group_by(GEOID) %>% summarize(pop = sum(value), race = "nh_asian")
# 
# 
# #######################################
# # nh_other
# ######################################
# 
# # nh_other youth population variables
# nh_other_youth_vars <- c(
#   "P12N_003N",
#   "P12N_004N",
#   "P12N_005N",
#   "P12N_006N",
#   "P12N_007N",
#   "P12N_008N",
#   "P12N_027N",
#   "P12N_028N",
#   "P12N_029N",
#   "P12N_030N",
#   "P12N_031N",
#   "P12N_032N")
# 
# # get nh_other youth
# nh_other_youth <- get_decennial(geography = "tract",
#                                 variables = nh_other_youth_vars,
#                                 state = "CA",
#                                 county = "Los Angeles",
#                                 year = 2020,
#                                 sumfile = "dhc")
# 
# # summarize nh_other youth
# nh_other_youth <- nh_other_youth %>% group_by(GEOID) %>% summarize(pop = sum(value), race = "nh_other")
# 
# 
# #######################################
# # nh_twoormor
# ######################################
# 
# # nh_twoormor youth population variables
# nh_twoormor_youth_vars <- c(
#   "P12O_003N",
#   "P12O_004N",
#   "P12O_005N",
#   "P12O_006N",
#   "P12O_007N",
#   "P12O_008N",
#   "P12O_027N",
#   "P12O_028N",
#   "P12O_029N",
#   "P12O_030N",
#   "P12O_031N",
#   "P12O_032N")
# 
# # get nh_twoormor youth
# nh_twoormor_youth <- get_decennial(geography = "tract",
#                                    variables = nh_twoormor_youth_vars,
#                                    state = "CA",
#                                    county = "Los Angeles",
#                                    year = 2020,
#                                    sumfile = "dhc")
# 
# # summarize nh_twoormor youth
# nh_twoormor_youth <- nh_twoormor_youth %>% group_by(GEOID) %>% summarize(pop = sum(value), race = "nh_twoormor")
# 
# 
# #######################################
# # aian
# ######################################
# 
# # aian youth population variables
# aian_youth_vars <- c(
#   "P12AE_003N",
#   "P12AE_004N",
#   "P12AE_005N",
#   "P12AE_006N",
#   "P12AE_007N",
#   "P12AE_008N",
#   "P12AE_027N",
#   "P12AE_028N",
#   "P12AE_029N",
#   "P12AE_030N",
#   "P12AE_031N",
#   "P12AE_032N",
# 
#   "P12Y_003N",
#   "P12Y_004N",
#   "P12Y_005N",
#   "P12Y_006N",
#   "P12Y_007N",
#   "P12Y_008N",
#   "P12Y_027N",
#   "P12Y_028N",
#   "P12Y_029N",
#   "P12Y_030N",
#   "P12Y_031N",
#   "P12Y_032N")
# 
# # get aian youth
# aian_youth <- get_decennial(geography = "tract",
#                             variables = aian_youth_vars,
#                             state = "CA",
#                             county = "Los Angeles",
#                             year = 2020,
#                             sumfile = "dhc")
# 
# # summarize aian youth
# aian_youth <- aian_youth %>% group_by(GEOID) %>% summarize(pop = sum(value), race = "aian")
# 
# 
# #######################################
# # nh_aian #added just for this indicator
# ######################################
# 
# # nh_aian youth population variables
# nh_aian_youth_vars <- c(
#   "P12K_003N",
#   "P12K_004N",
#   "P12K_005N",
#   "P12K_006N",
#   "P12K_007N",
#   "P12K_008N",
#   "P12K_027N",
#   "P12K_028N",
#   "P12K_029N",
#   "P12K_030N",
#   "P12K_031N",
#   "P12K_032N")
# 
# # get nh_aian youth
# nh_aian_youth <- get_decennial(geography = "tract",
#                             variables = nh_aian_youth_vars,
#                             state = "CA",
#                             county = "Los Angeles",
#                             year = 2020,
#                             sumfile = "dhc")
# 
# # summarize nh_aian youth
# nh_aian_youth <- nh_aian_youth %>% group_by(GEOID) %>% summarize(pop = sum(value), race = "nh_aian")
# 
# 
# #######################################
# # pacisl
# ######################################
# 
# # pacisl youth population variables
# pacisl_youth_vars <- c(
#   "P12AG_003N",
#   "P12AG_004N",
#   "P12AG_005N",
#   "P12AG_006N",
#   "P12AG_007N",
#   "P12AG_008N",
#   "P12AG_027N",
#   "P12AG_028N",
#   "P12AG_029N",
#   "P12AG_030N",
#   "P12AG_031N",
#   "P12AG_032N",
# 
#   "P12AA_003N",
#   "P12AA_004N",
#   "P12AA_005N",
#   "P12AA_006N",
#   "P12AA_007N",
#   "P12AA_008N",
#   "P12AA_027N",
#   "P12AA_028N",
#   "P12AA_029N",
#   "P12AA_030N",
#   "P12AA_031N",
#   "P12AA_032N")
# 
# # get pacisl youth
# pacisl_youth <- get_decennial(geography = "tract",
#                               variables = pacisl_youth_vars,
#                               state = "CA",
#                               county = "Los Angeles",
#                               year = 2020,
#                               sumfile = "dhc")
# 
# # summarize pacisl youth
# pacisl_youth <- pacisl_youth %>% group_by(GEOID) %>% summarize(pop = sum(value), race = "pacisl")
# 
# 
# 
# #######################################
# # nh_pacisl
# ######################################
# 
# # nh_pacisl youth population variables
# nh_pacisl_youth_vars <- c(
#   "P12M_003N",
#   "P12M_004N",
#   "P12M_005N",
#   "P12M_006N",
#   "P12M_007N",
#   "P12M_008N",
#   "P12M_027N",
#   "P12M_028N",
#   "P12M_029N",
#   "P12M_030N",
#   "P12M_031N",
#   "P12M_032N")
# 
# # get nh_pacisl youth
# nh_pacisl_youth <- get_decennial(geography = "tract",
#                               variables = nh_pacisl_youth_vars,
#                               state = "CA",
#                               county = "Los Angeles",
#                               year = 2020,
#                               sumfile = "dhc")
# 
# # summarize nh_pacisl youth
# nh_pacisl_youth <- nh_pacisl_youth %>% group_by(GEOID) %>% summarize(pop = sum(value), race = "nh_pacisl")
# 
# head(nh_pacisl_youth)
# 
# # combine into one data frame
# youth_df <- rbind(total_youth, aian_youth, nh_aian_youth, latino_youth, nh_asian_youth, nh_black_youth,
#                   nh_twoormor_youth, nh_other_youth, nh_white_youth, pacisl_youth, nh_pacisl_youth) %>% rename(geoid = GEOID)
# 
# #calculate BIPOC youth
# bipoc_df <- youth_df %>% filter(race == "total" | race == "nh_white") %>%
#   group_by(geoid) %>% 
#   summarize(pop = sum(pop[race=="total"]-pop[race=="nh_white"])) %>%  
#   mutate(race = "bipoc")
# 
# 
# #and add to youth df
# youth_df <- rbind(youth_df, bipoc_df)
# 
# ###Send to Postgres### #commented out  after sending to postgres
# con3 <- connect_to_db("bold_vision")
# table_name <- "dhc_tract_2020_youth_0_20_race"
# schema <- "bv_2023"
# 
# indicator <- "Census 2020 youth (0-20) by race for LA County census tracts. All races are non-Hispanic alone, except for latino, AIAN, and pacisl. AIAN and pacisl are alone or in combination with other races and ethnicities. See QA Doc for details: W:\\Project\\OSI\\Bold Vision\\BV 2023\\Documentation\\QA_DHC_race_popcalc.docx"
# source <- "Census DHC (2020) table P12"
# 
# dbWriteTable(con3, c(schema, table_name), youth_df,
#              overwrite = FALSE, row.names = FALSE)
# 
# #comment on table and columns
# comment <- paste0("COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
#                   COMMENT ON COLUMN ", schema, ".", table_name, ".geoid IS 'Census Tract FIPS';
#                   COMMENT ON COLUMN ", schema, ".", table_name, ".pop IS 'youth (0-20) population';
#                   COMMENT ON COLUMN ", schema, ".", table_name, ".race IS 'Racial-ethnic group';")
# #print(comment)
# dbSendQuery(con3, comment)
# 
# #disconnect
# dbDisconnect(con3)

########## Child Welfare Data ############

#load password protected data
wb <- loadWorkbook("W:/Project/OSI/Bold Vision/BV 2023/Data/Systems Impact/CCWIP/LA County PIT Counts by Race & SPA.xlsx", password=ccwip_pw)

#download all and combine all sheets (only the first 5 had usable data) but dropped Oct 2022 quarter (sheet 1) and just used the ones for 2023
## Please note: CCWIP uses CA Department of Finance demographic data https://dof.ca.gov/forecasting/Demographics/2020-census-data/ 
# California Department of Finance uses US Census DHC demographic data with races alone and latino of any race, missing refers to the missing category from the census

dat1 <- readWorksheet(wb, sheet = 2)%>%
  row_to_names(row_number = 8)  %>% clean_names()%>% drop_na()%>% mutate(quarter = "Jan 2023", indicator="cw")
names(dat1) <- c("office", "nh_black", "nh_white", "latino", "nh_aapi", "nh_aian", "missing", "total", "quarter", "indicator") 

dat2 <- readWorksheet(wb, sheet = 3)%>%
  row_to_names(row_number = 8)  %>% clean_names()%>% drop_na()%>% mutate(quarter = "Apr 2023", indicator="cw")
names(dat2) <- c("office", "nh_black", "nh_white", "latino", "nh_aapi", "nh_aian", "missing", "total", "quarter", "indicator") 

dat3 <- readWorksheet(wb, sheet = 4)%>%
  row_to_names(row_number = 8)  %>% clean_names()%>% drop_na()%>% mutate(quarter = "Jul 2023", indicator="cw")
names(dat3) <- c("office", "nh_black", "nh_white", "latino", "nh_aapi", "nh_aian", "missing", "total", "quarter", "indicator") 

dat4 <- readWorksheet(wb, sheet = 5)%>%
  row_to_names(row_number = 8)  %>% clean_names()%>% drop_na()%>% mutate(quarter = "Oct 2023", indicator="cw") 
names(dat4) <- c("office", "nh_black", "nh_white", "latino", "nh_aapi", "nh_aian", "missing", "total", "quarter", "indicator")

dat5 <- readWorksheet(wb, sheet = 7)%>%
  row_to_names(row_number = 8)  %>% clean_names()%>% drop_na()%>% mutate(quarter = "Jan 2023", indicator="prob") 
names(dat5) <- c("office", "nh_black", "nh_white", "latino", "nh_aapi", "nh_aian", "missing", "total", "quarter", "indicator")

dat6 <- readWorksheet(wb, sheet = 8)%>%
  row_to_names(row_number = 8)  %>% clean_names()%>% drop_na()%>% mutate(quarter = "Apr 2023", indicator="prob")
names(dat6) <- c("office", "nh_black", "nh_white", "latino", "nh_aapi", "nh_aian", "missing", "total", "quarter", "indicator") 

dat7 <- readWorksheet(wb, sheet = 9)%>%
  row_to_names(row_number = 8)  %>% clean_names()%>% drop_na()%>% mutate(quarter = "Jul 2023", indicator="prob")
names(dat7) <- c("office", "nh_black", "nh_white", "latino", "nh_aapi", "nh_aian", "missing", "total", "quarter", "indicator") 

dat8 <- readWorksheet(wb, sheet = 10)%>%
  row_to_names(row_number = 8)  %>% clean_names()%>% drop_na()%>% mutate(quarter = "Oct 2023", indicator="prob") 
names(dat8) <- c("office", "nh_black", "nh_white", "latino", "nh_aapi", "nh_aian", "missing", "total", "quarter", "indicator")

#combine into one df
data <- rbind(dat1, dat2, dat3, dat4, dat5, dat6, dat7, dat8)


# Calculate the annual average of children in child_welfare care or probation using the 2023 quarterly counts by race, SPA, and BIPOC/non-BIPOC. 
data_df <- data %>% 
  filter(office %in% c("SPA 1", "SPA 2","SPA 3","SPA 4","SPA 5","SPA 6","SPA 7","SPA 8", "Los Angeles")) %>% # & indicator=="cw"
  rename("geoid"="office")# %>% select(-indicator)

data_df$nh_black <- as.numeric(gsub(",","",data_df$nh_black))
data_df$nh_white <- as.numeric(gsub(",","",data_df$nh_white))
data_df$latino <- as.numeric(gsub(",","",data_df$latino))
data_df$nh_aapi <- as.numeric(gsub(",","",data_df$nh_aapi))
data_df$nh_aian <- as.numeric(gsub(",","",data_df$nh_aian))
data_df$missing <- as.numeric(gsub(",","",data_df$missing))
data_df$total <- as.numeric(gsub(",","",data_df$total))
data_df$geoid <- as.character(gsub("SPA ","",data_df$geoid))
data_df$geoid <- as.character(gsub("Los Angeles","06037",data_df$geoid))

# pivot longer and summarize quarters
child_welfare <- data_df  %>% pivot_longer(c(nh_black, nh_white, latino, nh_aapi, nh_aian, missing, total),names_to = "race", values_to = "count") %>% 
  select(-quarter) %>%  group_by(geoid, race) %>% summarize(num_qrts = 4, sum_count = sum(count), avg_count = sum_count/num_qrts) #num_qrts = ifelse(geoid=="06037",8,4)
# View(child_welfare)

#add bipoc
bipoc <- child_welfare %>% filter(race %in% c("total","nh_white")) %>% group_by(geoid) %>%  
  mutate(sum_count = sum(sum_count[race=="total"]-sum_count[race=="nh_white"]), avg_count=sum_count/num_qrts, race="bipoc") %>% distinct()
# View(bipoc)

# add bipoc to table
childwelfare_count <- rbind(child_welfare, bipoc) %>% filter(race!="missing")
# View(childwelfare_count)


####### Load tract level data -----
# tract-SPA xwalk in database can be used to calculate 0-20 population by SPA based on tract intersections. Table to use: bv_2023.crosswalk_tract_spas_2023

#set connection to postgresql directory in pgadmin
con <- connect_to_db("bold_vision")

xwalk <- st_read(con, query = "select * from bv_2023.crosswalk_tract_spas_2023") #get census tract to spa crosswalk
youth_df <- st_read(con, query = "select * from bv_2023.dhc_tract_2020_youth_0_20_race") #get pop by race & tract

#disconnect
dbDisconnect(con)


#combining the two tract DFs to get youth pop by spa ----

#combine pop by tract & crosswalk
youth <- left_join(youth_df, xwalk, by="geoid") %>% group_by(spa, spa_name,race) %>% summarize(pop=sum(pop)) %>% drop_na(spa) %>% 
  rename("geoid"="spa", "name"="spa_name")

lac <- youth %>% mutate(geoid="06037") %>% group_by(geoid, race) %>% summarize(pop = sum(pop)) %>% distinct() %>% mutate(name="Los Angeles County")
#combine 
pop <- rbind(youth, lac)

# combine nh_asian and nh_pacisl so it cam match nh_aapi for child welfare counts
nh_aapi <- pop %>% filter(race %in% c("nh_asian","nh_pacisl")) %>%  group_by(geoid) %>%  mutate(pop = sum(pop), race="nh_aapi") %>% distinct()

#combine into one df
pop <- rbind(pop, nh_aapi)
View(pop)

#create main dataframe by combining child welfare counts and DHC population demographics----

#combine child welfare counts and DHC population demographics
df <- left_join(childwelfare_count,pop,by=(c("geoid","race")))
#View(df)
#Calculations for SPA (region) level data excluding race----

region <- df %>%  filter(race=="total" & geoid!="06037") %>% select(-race) %>% ungroup() %>% 
  summarize(rate = (avg_count/pop)*1000, #per 1000 people per the last update's methodology
                   best = min(rate, na.rm=T),
                   diff =  abs(best - rate),
                   asbest = "min",
                   values_count = sum(!is.na(rate)), # get differences from best rate
                   index_of_disparity = ifelse(min(values_count) < 2, NA, # ID: if there are fewer than 2 values count, NULL, otherwise...
                               # divide the sum of differences by the minimum value, divided by the values count - 1
                               ((sum(diff, na.rm=T)/min(best))/(min(values_count) - 1))*100), across()) %>% 
  select(-sum_count, -num_qrts) %>% rename("count"="avg_count")

col_order <- c("geoid","name","count","pop", "rate", "best","asbest","diff","values_count", "index_of_disparity")
region <- region[, col_order]
# View(region)


## Calculations for Subgroups in LA County ----

df_subgroups <- df %>% ungroup() %>% filter(geoid=="06037") %>% select(-name) %>% group_by(race) %>%
  summarize(sum_count = sum(sum_count), avg_count=sum_count/num_qrts, pop=sum(pop),  
            rate=(avg_count/pop)*1000,geoid="06037") %>% select(-sum_count) %>% rename("count"="avg_count") #rate is per 1000 per the methodology from the last update

diff_county <- df_subgroups %>%
  filter(race !="total" | race != "bipoc") %>% group_by(geoid) %>%
  summarize(best = min(rate, na.rm=T), values_count = sum(!is.na(rate))) %>%

  # join best and values count to the original df
  right_join(df_subgroups, by=c("geoid")) %>%

  # get differences from best rate
  mutate(diff = ifelse(race =="total" | race == "bipoc", NA, abs(best - rate)))

# get the avg and variance of differences, and the index of disparity
id_table_county <- diff_county %>%
  filter(race !="total") %>%
  dplyr::group_by(geoid) %>%
  summarize(

    # ID: if there are fewer than 2 values count, NULL, otherwise...
    index_of_disparity = ifelse(min(values_count) < 2, NA,

                                # divide the sum of differences by the minimum value, divided by the values count - 1
                                ((sum(diff, na.rm=T)/min(best))/(min(values_count) - 1))*100))
subgroups <-   diff_county %>%
  full_join(id_table_county,
            by = c("geoid")) %>% mutate(name = "Los Angeles County", asbest = "min") %>% rename("subgroup"="race")


col_order <- c("geoid", "name", "subgroup", "count", "pop", "rate", "best", "asbest", "diff", "index_of_disparity")
subgroups <- subgroups[, col_order]
View(subgroups)

#### Send to Postgres ####
con2 <- connect_to_db("bold_vision")
table_name <- "si_childwelfare_subgroup"
schema <- 'bv_2023'
indicator <- "Foster Care and Probation Rates for Ages 0-20 by Subgroup"
source <- "California Child Welfare Indicators Project, Rates of Children Point in Time in Foster Care or Probation,\nUC Berkeley 2023. Data Notes: Per 1000 youth, Average Rates of Children Point of Time in Foster Care/Probation.\nRace groups are Latinx-exclusive and Asian Pacific Islander is not disaggregated in the data source.\n Children defined as 0-20 years old"

dbWriteTable(con2, c(schema, table_name), subgroups,
             overwrite = FALSE, row.names = FALSE)

#comment on table and columns
comment <- paste0("COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
                   COMMENT ON COLUMN ", schema, ".", table_name, ".count IS 'average count across 4 quarters';
                   COMMENT ON COLUMN ", schema, ".", table_name, ".rate IS 'average count across 4 quarters divided by population count';
                   COMMENT ON COLUMN ", schema, ".", table_name, ".geoid IS 'County FIPS';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".name IS 'County name';")
print(comment)
dbSendQuery(con2, comment)


table_name <- "si_childwelfare_region"
indicator <- "Foster Care and Probation Rates for Ages 0-20 by Service Planning Area (SPA)"
source <- "California Child Welfare Indicators Project, Rates of Children Point in Time in Foster Care. Probation data was not available by SPA,\nUC Berkeley 2023. Data Notes: Per 1000 youth, Average Rates of Children Point of Time in Foster Care.\n Children defined as 0-20 years old"

dbWriteTable(con2, c(schema, table_name), region,
             overwrite = FALSE, row.names = FALSE)

#comment on table and columns
comment2 <- paste0("COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
                   COMMENT ON COLUMN ", schema, ".", table_name, ".count IS 'average count across 4 quarters';
                   COMMENT ON COLUMN ", schema, ".", table_name, ".rate IS 'average count across 4 quarters divided by population count';
                   COMMENT ON COLUMN ", schema, ".", table_name, ".geoid IS 'SPA number';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".name IS 'SPA name';")
print(comment2)
dbSendQuery(con2, comment2)

#disconnect
dbDisconnect(con2)

