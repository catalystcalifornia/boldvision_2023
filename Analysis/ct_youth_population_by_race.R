#Calculating Census tract population estimates for youth (ages 0-24)
#using Detailed Demographic & Housing Characteristics acquired via tidycensus

#install.packages("tidycensus") latest package required for dhc
library(tidycensus)
library(tidyverse)
library(RPostgreSQL)

# API key
source("W:\\RDA Team\\R\\credentials_source.R")

# load variables
v20 <- load_variables(2020, "dhc")

# total population variables
total_pop_vars <- c(total = "P12_001N", 
                    latino = "P12H_001N", 
                    nh_white = "P12I_001N", 
                    nh_black = "P12J_001N", 
                    nh_asian = "P12L_001N", 
                    nh_other = "P12N_001N",
                    nh_twoormor = "P12O_001N", 
                    aian = "P12Y_001N", 
                    aian = "P12AE_001N", 
                    pacisl = "P12AG_001N", 
                    pacisl = "P12AA_001N")

# get total population
total_pop <- get_decennial(geography = "tract", 
                           variables = total_pop_vars,
                           state = "CA",
                           county = "Los Angeles",
                           year = 2020,
                           sumfile = "dhc")

# summarize total population
total_pop <- total_pop %>% group_by(GEOID, variable) %>% summarize(pop = sum(value))

#format
total_pop <- total_pop %>% rename(race = variable) %>% select(GEOID, pop, race)

head(total_pop)

# total youth population variables
total_youth_vars <- c(
  "P12_003N", 
  "P12_004N", 
  "P12_005N", 
  "P12_006N", 
  "P12_007N", 
  "P12_008N", 
  "P12_009N", 
  "P12_010N",
  "P12_027N",
  "P12_028N",
  "P12_029N",
  "P12_030N",
  "P12_031N",
  "P12_032N",
  "P12_033N",
  "P12_034N")

# get total youth
total_youth <- get_decennial(geography = "tract", 
                             variables = total_youth_vars,
                             state = "CA",
                             county = "Los Angeles",
                             year = 2020,
                             sumfile = "dhc")

# summarize total youth
total_youth <- total_youth %>% group_by(GEOID) %>% summarize(pop = sum(value), race = "total")

#######################################
# latino
######################################

# latino youth population variables
latino_youth_vars <- c(
  "P12H_003N", 
  "P12H_004N", 
  "P12H_005N", 
  "P12H_006N", 
  "P12H_007N", 
  "P12H_008N", 
  "P12H_009N", 
  "P12H_010N",
  "P12H_027N",
  "P12H_028N",
  "P12H_029N",
  "P12H_030N",
  "P12H_031N",
  "P12H_032N",
  "P12H_033N",
  "P12H_034N")

# get latino youth
latino_youth <- get_decennial(geography = "tract", 
                              variables = latino_youth_vars,
                              state = "CA",
                              county = "Los Angeles",
                              year = 2020,
                              sumfile = "dhc")

# summarize latino youth
latino_youth <- latino_youth %>% group_by(GEOID) %>% summarize(pop = sum(value), race = "latino")


#######################################
# nh_white
######################################

# nh_white youth population variables
nh_white_youth_vars <- c(
  "P12I_003N", 
  "P12I_004N", 
  "P12I_005N", 
  "P12I_006N", 
  "P12I_007N", 
  "P12I_008N", 
  "P12I_009N", 
  "P12I_010N",
  "P12I_027N",
  "P12I_028N",
  "P12I_029N",
  "P12I_030N",
  "P12I_031N",
  "P12I_032N",
  "P12I_033N",
  "P12I_034N")

# get nh_white youth
nh_white_youth <- get_decennial(geography = "tract", 
                                variables = nh_white_youth_vars,
                                state = "CA",
                                county = "Los Angeles",
                                year = 2020,
                                sumfile = "dhc")

# summarize nh_white youth
nh_white_youth <- nh_white_youth %>% group_by(GEOID) %>% summarize(pop = sum(value), race = "nh_white")


#######################################
# nh_black
######################################

# nh_black youth population variables
nh_black_youth_vars <- c(
  "P12J_003N", 
  "P12J_004N", 
  "P12J_005N", 
  "P12J_006N", 
  "P12J_007N", 
  "P12J_008N", 
  "P12J_009N", 
  "P12J_010N",
  "P12J_027N",
  "P12J_028N",
  "P12J_029N",
  "P12J_030N",
  "P12J_031N",
  "P12J_032N",
  "P12J_033N",
  "P12J_034N")

# get nh_black youth
nh_black_youth <- get_decennial(geography = "tract", 
                                variables = nh_black_youth_vars,
                                state = "CA",
                                county = "Los Angeles",
                                year = 2020,
                                sumfile = "dhc")

# summarize nh_black youth
nh_black_youth <- nh_black_youth %>% group_by(GEOID) %>% summarize(pop = sum(value), race = "nh_black")


#######################################
# nh_asian
######################################

# nh_asian youth population variables
nh_asian_youth_vars <- c(
  "P12L_003N", 
  "P12L_004N", 
  "P12L_005N", 
  "P12L_006N", 
  "P12L_007N", 
  "P12L_008N", 
  "P12L_009N", 
  "P12L_010N",
  "P12L_027N",
  "P12L_028N",
  "P12L_029N",
  "P12L_030N",
  "P12L_031N",
  "P12L_032N",
  "P12L_033N",
  "P12L_034N")

# get nh_asian youth
nh_asian_youth <- get_decennial(geography = "tract", 
                                variables = nh_asian_youth_vars,
                                state = "CA",
                                county = "Los Angeles",
                                year = 2020,
                                sumfile = "dhc")

# summarize nh_asian youth
nh_asian_youth <- nh_asian_youth %>% group_by(GEOID) %>% summarize(pop = sum(value), race = "nh_asian")


#######################################
# nh_other
######################################

# nh_other youth population variables
nh_other_youth_vars <- c(
  "P12N_003N", 
  "P12N_004N", 
  "P12N_005N", 
  "P12N_006N", 
  "P12N_007N", 
  "P12N_008N", 
  "P12N_009N", 
  "P12N_010N",
  "P12N_027N",
  "P12N_028N",
  "P12N_029N",
  "P12N_030N",
  "P12N_031N",
  "P12N_032N",
  "P12N_033N",
  "P12N_034N")

# get nh_other youth
nh_other_youth <- get_decennial(geography = "tract", 
                                variables = nh_other_youth_vars,
                                state = "CA",
                                county = "Los Angeles",
                                year = 2020,
                                sumfile = "dhc")

# summarize nh_other youth
nh_other_youth <- nh_other_youth %>% group_by(GEOID) %>% summarize(pop = sum(value), race = "nh_other")


#######################################
# nh_twoormor
######################################

# nh_twoormor youth population variables
nh_twoormor_youth_vars <- c(
  "P12O_003N", 
  "P12O_004N", 
  "P12O_005N", 
  "P12O_006N", 
  "P12O_007N", 
  "P12O_008N", 
  "P12O_009N", 
  "P12O_010N",
  "P12O_027N",
  "P12O_028N",
  "P12O_029N",
  "P12O_030N",
  "P12O_031N",
  "P12O_032N",
  "P12O_033N",
  "P12O_034N")

# get nh_twoormor youth
nh_twoormor_youth <- get_decennial(geography = "tract", 
                                   variables = nh_twoormor_youth_vars,
                                   state = "CA",
                                   county = "Los Angeles",
                                   year = 2020,
                                   sumfile = "dhc")

# summarize nh_twoormor youth
nh_twoormor_youth <- nh_twoormor_youth %>% group_by(GEOID) %>% summarize(pop = sum(value), race = "nh_twoormor")


#######################################
# aian
######################################

# aian youth population variables
aian_youth_vars <- c(
  "P12AE_003N", 
  "P12AE_004N", 
  "P12AE_005N", 
  "P12AE_006N", 
  "P12AE_007N", 
  "P12AE_008N", 
  "P12AE_009N", 
  "P12AE_010N",
  "P12AE_027N",
  "P12AE_028N",
  "P12AE_029N",
  "P12AE_030N",
  "P12AE_031N",
  "P12AE_032N",
  "P12AE_033N",
  "P12AE_034N",
  
  "P12Y_003N", 
  "P12Y_004N", 
  "P12Y_005N", 
  "P12Y_006N", 
  "P12Y_007N", 
  "P12Y_008N", 
  "P12Y_009N", 
  "P12Y_010N",
  "P12Y_027N",
  "P12Y_028N",
  "P12Y_029N",
  "P12Y_030N",
  "P12Y_031N",
  "P12Y_032N",
  "P12Y_033N",
  "P12Y_034N")

# get aian youth
aian_youth <- get_decennial(geography = "tract", 
                            variables = aian_youth_vars,
                            state = "CA",
                            county = "Los Angeles",
                            year = 2020,
                            sumfile = "dhc")

# summarize aian youth
aian_youth <- aian_youth %>% group_by(GEOID) %>% summarize(pop = sum(value), race = "aian")


#######################################
# pacisl
######################################

# pacisl youth population variables
pacisl_youth_vars <- c(
  "P12AG_003N", 
  "P12AG_004N", 
  "P12AG_005N", 
  "P12AG_006N", 
  "P12AG_007N", 
  "P12AG_008N", 
  "P12AG_009N", 
  "P12AG_010N",
  "P12AG_027N",
  "P12AG_028N",
  "P12AG_029N",
  "P12AG_030N",
  "P12AG_031N",
  "P12AG_032N",
  "P12AG_033N",
  "P12AG_034N",
  
  "P12AA_003N", 
  "P12AA_004N", 
  "P12AA_005N", 
  "P12AA_006N", 
  "P12AA_007N", 
  "P12AA_008N", 
  "P12AA_009N", 
  "P12AA_010N",
  "P12AA_027N",
  "P12AA_028N",
  "P12AA_029N",
  "P12AA_030N",
  "P12AA_031N",
  "P12AA_032N",
  "P12AA_033N",
  "P12AA_034N")

# get pacisl youth
pacisl_youth <- get_decennial(geography = "tract", 
                              variables = pacisl_youth_vars,
                              state = "CA",
                              county = "Los Angeles",
                              year = 2020,
                              sumfile = "dhc")

# summarize pacisl youth
pacisl_youth <- pacisl_youth %>% group_by(GEOID) %>% summarize(pop = sum(value), race = "pacisl")

head(pacisl_youth)

# combine into one data frame
youth_df <- rbind(total_youth, aian_youth, latino_youth, nh_asian_youth, nh_black_youth,
                  nh_twoormor_youth, nh_other_youth, nh_white_youth, pacisl_youth) %>% rename(geoid = GEOID)

bipoc_df <- youth_df %>% filter(race == "total" | race == "nh_white") %>% 
  pivot_wider(names_from = race, values_from = pop) %>%
  mutate(bipoc = total - nh_white, race = "bipoc") %>%
  select(geoid, bipoc, race) %>%
  rename(pop = bipoc)

#and add to youth df
youth_df <- rbind(youth_df, bipoc_df)

#test <- total_pop[ which(total_pop$race == 'aian'), ]
#sum(test$pop) #327,930 matches LA County AIAN total here: https://www.census.gov/library/stories/state-by-state/california-population-change-between-census-decade.html#:~:text=Race%20and%20ethnicity%20(White%20alone,%25%2C%20up%20from%2054.9%25).



###Send to Postgres###
con3 <- connect_to_db("bold_vision")
table_name <- "dhc_tract_2020_youth_0_24_race"
schema <- "bv_2023"

indicator <- "Census 2020 youth (0-24) by race for LA County census tracts. All races are non-Hispanic alone, except for latino, AIAN, and pacisl. AIAN and pacisl are alone or in combination with other races and ethnicities. See QA Doc for details: W:\\Project\\OSI\\Bold Vision\\BV 2023\\Documentation\\QA_DHC_race_popcalc.docx"
source <- "Census DHC (2020) table P12"

dbWriteTable(con3, c(schema, table_name), youth_df,
             overwrite = FALSE, row.names = FALSE)

#comment on table and columns
comment <- paste0("COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".geoid IS 'Census Tract FIPS';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".pop IS 'youth (0-24) population';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".race IS 'Racial-ethnic group';")
#print(comment)
dbSendQuery(con3, comment)

#disconnect
dbDisconnect(con3)

