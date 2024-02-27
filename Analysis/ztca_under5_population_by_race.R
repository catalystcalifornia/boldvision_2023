#Calculating ZCTA population estimates for children under age 5
#using Detailed Demographic & Housing Characteristics acquired via tidycensus

#EXACT SAME script as the ct_youth_population_by_race script save for 
#find/replace zcta for tract and under5 for youth,
#removing county parameters, and commenting out above age 5

#install.packages("tidycensus") latest package required for dhc
library(tidycensus)
library(tidyverse)
library(RPostgreSQL)

# API key
source("W:\\RDA Team\\R\\credentials_source.R")

# load variables
#v20 <- load_variables(2020, "dhc")
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
# total_pop <- get_decennial(geography = "zcta", 
#                              variables = total_pop_vars,
#                              state = "CA",
#                              
#                              year = 2020,
#                              sumfile = "dhc")
# 
# # summarize total population
# total_pop <- total_pop %>% group_by(GEOID, variable) %>% summarize(pop = sum(value))
# 
# #format
# total_pop <- total_pop %>% rename(race = variable) %>% select(GEOID, value, race)
# 
# head(total_pop)

# total under5 population variables
total_under5_vars <- c(
  "P12_003N", 
  # "P12_004N", 
  # "P12_005N", 
  # "P12_006N", 
  # "P12_007N", 
  # "P12_008N", 
  # "P12_009N", 
  # "P12_010N",
  "P12_027N"#,
  # "P12_028N",
  # "P12_029N",
  # "P12_030N",
  # "P12_031N",
  # "P12_032N",
  # "P12_033N",
  # "P12_034N"
)

# get total under5
total_under5 <- get_decennial(geography = "zcta", 
                              variables = total_under5_vars,
                              state = "CA",
                              
                              year = 2020,
                              sumfile = "dhc")

# summarize total under5
total_under5 <- total_under5 %>% group_by(GEOID) %>% summarize(pop = sum(value), race = "total")

#######################################
# latino
######################################

# latino under5 population variables
latino_under5_vars <- c(
  "P12H_003N", 
  # "P12H_004N", 
  # "P12H_005N", 
  # "P12H_006N", 
  # "P12H_007N", 
  # "P12H_008N", 
  # "P12H_009N", 
  # "P12H_010N",
  "P12H_027N"#,
  # "P12H_028N",
  # "P12H_029N",
  # "P12H_030N",
  # "P12H_031N",
  # "P12H_032N",
  # "P12H_033N",
  # "P12H_034N"
)

# get latino under5
latino_under5 <- get_decennial(geography = "zcta", 
                               variables = latino_under5_vars,
                               state = "CA",
                               
                               year = 2020,
                               sumfile = "dhc")

# summarize latino under5
latino_under5 <- latino_under5 %>% group_by(GEOID) %>% summarize(pop = sum(value), race = "latino")


#######################################
# nh_white
######################################

# nh_white under5 population variables
nh_white_under5_vars <- c(
  "P12I_003N", 
  # "P12I_004N", 
  # "P12I_005N", 
  # "P12I_006N", 
  # "P12I_007N", 
  # "P12I_008N", 
  # "P12I_009N", 
  # "P12I_010N",
  "P12I_027N"#,
  # "P12I_028N",
  # "P12I_029N",
  # "P12I_030N",
  # "P12I_031N",
  # "P12I_032N",
  # "P12I_033N",
  # "P12I_034N"
)

# get nh_white under5
nh_white_under5 <- get_decennial(geography = "zcta", 
                                 variables = nh_white_under5_vars,
                                 state = "CA",
                                 
                                 year = 2020,
                                 sumfile = "dhc")

# summarize nh_white under5
nh_white_under5 <- nh_white_under5 %>% group_by(GEOID) %>% summarize(pop = sum(value), race = "nh_white")


#######################################
# nh_black
######################################

# nh_black under5 population variables
nh_black_under5_vars <- c(
  "P12J_003N", 
  # "P12J_004N", 
  # "P12J_005N", 
  # "P12J_006N", 
  # "P12J_007N", 
  # "P12J_008N", 
  # "P12J_009N", 
  # "P12J_010N",
  "P12J_027N"#,
  # "P12J_028N",
  # "P12J_029N",
  # "P12J_030N",
  # "P12J_031N",
  # "P12J_032N",
  # "P12J_033N",
  # "P12J_034N"
)

# get nh_black under5
nh_black_under5 <- get_decennial(geography = "zcta", 
                                 variables = nh_black_under5_vars,
                                 state = "CA",
                                 
                                 year = 2020,
                                 sumfile = "dhc")

# summarize nh_black under5
nh_black_under5 <- nh_black_under5 %>% group_by(GEOID) %>% summarize(pop = sum(value), race = "nh_black")


#######################################
# nh_asian
######################################

# nh_asian under5 population variables
nh_asian_under5_vars <- c(
  "P12L_003N", 
  # "P12L_004N", 
  # "P12L_005N", 
  # "P12L_006N", 
  # "P12L_007N", 
  # "P12L_008N", 
  # "P12L_009N", 
  # "P12L_010N",
  "P12L_027N"#,
  # "P12L_028N",
  # "P12L_029N",
  # "P12L_030N",
  # "P12L_031N",
  # "P12L_032N",
  # "P12L_033N",
  # "P12L_034N"
)

# get nh_asian under5
nh_asian_under5 <- get_decennial(geography = "zcta", 
                                 variables = nh_asian_under5_vars,
                                 state = "CA",
                                 
                                 year = 2020,
                                 sumfile = "dhc")

# summarize nh_asian under5
nh_asian_under5 <- nh_asian_under5 %>% group_by(GEOID) %>% summarize(pop = sum(value), race = "nh_asian")


#######################################
# nh_other
######################################

# nh_other under5 population variables
nh_other_under5_vars <- c(
  "P12N_003N", 
  # "P12N_004N", 
  # "P12N_005N", 
  # "P12N_006N", 
  # "P12N_007N", 
  # "P12N_008N", 
  # "P12N_009N", 
  # "P12N_010N",
  "P12N_027N"#,
  # "P12N_028N",
  # "P12N_029N",
  # "P12N_030N",
  # "P12N_031N",
  # "P12N_032N",
  # "P12N_033N",
  # "P12N_034N"
)

# get nh_other under5
nh_other_under5 <- get_decennial(geography = "zcta", 
                                 variables = nh_other_under5_vars,
                                 state = "CA",
                                 
                                 year = 2020,
                                 sumfile = "dhc")

# summarize nh_other under5
nh_other_under5 <- nh_other_under5 %>% group_by(GEOID) %>% summarize(pop = sum(value), race = "nh_other")


#######################################
# nh_twoormor
######################################

# nh_twoormor under5 population variables
nh_twoormor_under5_vars <- c(
  "P12O_003N", 
  # "P12O_004N", 
  # "P12O_005N", 
  # "P12O_006N", 
  # "P12O_007N", 
  # "P12O_008N", 
  # "P12O_009N", 
  # "P12O_010N",
  "P12O_027N"#,
  # "P12O_028N",
  # "P12O_029N",
  # "P12O_030N",
  # "P12O_031N",
  # "P12O_032N",
  # "P12O_033N",
  # "P12O_034N"
)

# get nh_twoormor under5
nh_twoormor_under5 <- get_decennial(geography = "zcta", 
                                    variables = nh_twoormor_under5_vars,
                                    state = "CA",
                                    
                                    year = 2020,
                                    sumfile = "dhc")

# summarize nh_twoormor under5
nh_twoormor_under5 <- nh_twoormor_under5 %>% group_by(GEOID) %>% summarize(pop = sum(value), race = "nh_twoormor")


#######################################
# aian
######################################

# aian under5 population variables
aian_under5_vars <- c(
  "P12AE_003N", 
  # "P12AE_004N", 
  # "P12AE_005N", 
  # "P12AE_006N", 
  # "P12AE_007N", 
  # "P12AE_008N", 
  # "P12AE_009N", 
  # "P12AE_010N",
  "P12AE_027N",
  # "P12AE_028N",
  # "P12AE_029N",
  # "P12AE_030N",
  # "P12AE_031N",
  # "P12AE_032N",
  # "P12AE_033N",
  # "P12AE_034N",
  
  "P12Y_003N", 
  # "P12Y_004N", 
  # "P12Y_005N", 
  # "P12Y_006N", 
  # "P12Y_007N", 
  # "P12Y_008N", 
  # "P12Y_009N", 
  # "P12Y_010N",
  "P12Y_027N"#,
  # "P12Y_028N",
  # "P12Y_029N",
  # "P12Y_030N",
  # "P12Y_031N",
  # "P12Y_032N",
  # "P12Y_033N",
  # "P12Y_034N"
)

# get aian under5
aian_under5 <- get_decennial(geography = "zcta", 
                             variables = aian_under5_vars,
                             state = "CA",
                             
                             year = 2020,
                             sumfile = "dhc")

# summarize aian under5
aian_under5 <- aian_under5 %>% group_by(GEOID) %>% summarize(pop = sum(value), race = "aian")


#######################################
# pacisl
######################################

# pacisl under5 population variables
pacisl_under5_vars <- c(
  "P12AG_003N", 
  # "P12AG_004N", 
  # "P12AG_005N", 
  # "P12AG_006N", 
  # "P12AG_007N", 
  # "P12AG_008N", 
  # "P12AG_009N", 
  # "P12AG_010N",
  "P12AG_027N",
  # "P12AG_028N",
  # "P12AG_029N",
  # "P12AG_030N",
  # "P12AG_031N",
  # "P12AG_032N",
  # "P12AG_033N",
  # "P12AG_034N",
  
  "P12AA_003N", 
  # "P12AA_004N", 
  # "P12AA_005N", 
  # "P12AA_006N", 
  # "P12AA_007N", 
  # "P12AA_008N", 
  # "P12AA_009N", 
  # "P12AA_010N",
  "P12AA_027N"#,
  # "P12AA_028N",
  # "P12AA_029N",
  # "P12AA_030N",
  # "P12AA_031N",
  # "P12AA_032N",
  # "P12AA_033N",
  # "P12AA_034N"
)

# get pacisl under5
pacisl_under5 <- get_decennial(geography = "zcta", 
                               variables = pacisl_under5_vars,
                               state = "CA",
                               
                               year = 2020,
                               sumfile = "dhc")

# summarize pacisl under5
pacisl_under5 <- pacisl_under5 %>% group_by(GEOID) %>% summarize(pop = sum(value), race = "pacisl")

head(pacisl_under5)

# combine into one data frame
under5_df <- rbind(total_under5, aian_under5, latino_under5, nh_asian_under5, nh_black_under5,
                   nh_twoormor_under5, nh_other_under5, nh_white_under5, pacisl_under5) %>% rename(geoid = GEOID)


#calculate BIPOC under5
bipoc_under5 <- left_join(total_under5, nh_white_under5, by="GEOID") 
names(bipoc_under5) <- c("geoid", "total", "total_name", "nh_white", "nh_white_name")

bipoc_under5 <- bipoc_under5 %>% mutate(pop = total - nh_white, race = "bipoc") %>%
  select(geoid, pop, race)


#and add to under5 df
under5_df <- rbind(under5_df, bipoc_under5)


##Send to Postgres###
con3 <- connect_to_db("bold_vision")
table_name <- "dhc_zcta_2020_under5_race"
schema <- 'bv_2023'

indicator <- "Census 2020 under5 by race for LA County zctas. All races are non-Hispanic alone, except for latino, AIAN, and pacisl. AIAN and pacisl are alone or in combination with other races and ethnicities."
source <- "Census DHC (2020) table P12. For details, see QA doc: W:\\Project\\OSI\\Bold Vision\\BV 2023\\Documentation\\QA_DHC_race_under5calc.docx"

dbWriteTable(con3, c(schema, table_name), under5_df,
             overwrite = FALSE, row.names = FALSE)

#comment on table and columns
comment <- paste0("COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
                                         COMMENT ON COLUMN ", schema, ".", table_name, ".geoid IS 'ZCTA';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".pop IS 'Under 5 population';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".race IS 'Racial-ethnic group';")
#print(comment)
dbSendQuery(con3, comment)

#disconnect
dbDisconnect(con3)


#clean up
rm(list = c("aian_under5", "bipoc_under5", "latino_under5", "nh_asian_under5", 
            "nh_black_under5", "nh_other_under5", "nh_twoormor_under5", 
            "nh_white_under5", "pacisl_under5", "total_under5", "con3"))