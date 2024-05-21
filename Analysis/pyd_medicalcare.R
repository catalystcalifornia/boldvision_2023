# BV Domain: Positive Youth Development
# Indicator: Youth who Reported Ease in Obtaining Needed Medical Care (0-24)

#Load libraries
# library(data.table)
# library(stringr)
# library(dplyr)
# library(RPostgreSQL)
# library(dbplyr)
# library(srvyr)
# library(tidycensus)
# library(tidyr)
# library(rpostgis)
# library(tidyr)
# library(here)
# library(sf)
library(readxl)
# library(ggchicklet)

options(scipen = 100) # disable scientific notation

# create connection for bv database
source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("bold_vision")

con_rda <- connect_to_db("rda_shared_data")

####SUBGROUP Analysis and Data Table ####
#pull data and only select data needed
df_subgroup <- read_xlsx("W:\\Project\\OSI\\Bold Vision\\BV 2023\\Data\\Positive Youth Development\\LACHS 2023_Catalyst California_Youth Health_final.xlsx", 
                         range = "B7:H26",  sheet = "ACCDIFFN", na = "*") %>% # choose sheet and range
  select(c(subgroup = 1, rate = 3, ci_95_low = 4, ci_95_hi = 6, count = 7)) %>% #only keep columns needed and rename
  mutate(rate = rate*100)

#remove rows not needed
df_subgroup <- df_subgroup[-c(2, 3:7, 12:13, 15, 17),] 



#rename to follow BV workflow and standards
df_subgroup <- df_subgroup %>% mutate(subgroup = ifelse(subgroup == "LA County", "total",
                                                        ifelse(subgroup == "Latinx", "latino",
                                                               ifelse(subgroup == "NH White", "nh_white",
                                                                      ifelse(subgroup == "NH Black or African American", "nh_black",
                                                                             ifelse(subgroup == "NH Asian", "nh_asian",
                                                                                    ifelse(subgroup == "NH Multi-Racial or Other Race", "nh_twoormor",
                                                                                           ifelse(subgroup == "All Youths of Color", "bipoc",
                                                                                                  ifelse(subgroup == "Native Hawaiian or Pacific Islander", "pacisl",
                                                                                                         ifelse(subgroup == "American Indian or Alaska Native", "aian",
                                                                                                                'NA'))))))))))

#create a new column for CV based on CI columns 
df_subgroup <- df_subgroup %>% mutate(rate_moe = (ci_95_hi - ci_95_low)/2,
                                      rate_cv = (rate_moe/1.96)/rate*100
)



#calculate the index of disparity
df_subgroup <-
  df_subgroup %>% 
  ungroup() %>%
  mutate(best=max(rate[subgroup!='total' & subgroup!='bipoc'],na.rm=T),asbest="max",#create columns for best rate and as best treatment, ignoring total and bipoc
         values_count = sum(!is.na(subgroup))-2) %>% # remove total and bipoc from values count
  mutate(diff = ifelse(subgroup == 'total' | subgroup =='bipoc', NA, abs(best-rate))) %>%
  mutate(index_of_disparity = ((sum(diff, na.rm=T)/best/(values_count - 1))*100)) 


# Write table to bold vision database
dbWriteTable(con, c("bv_2023", "pyd_medicalcare_subgroup"), df_subgroup,
             overwrite = TRUE, row.names = FALSE,
             field.types = c(
               subgroup = "varchar",
               count ="integer",
               ci_95_low = "numeric",
               ci_95_hi = "numeric",
               rate = "numeric",
               rate_moe = "numeric",
               rate_cv = "numeric",
               asbest = "varchar",
               values_count = "numeric",
               best = "numeric",
               diff= "numeric",
               index_of_disparity= "numeric"
             ))


# write comment to table

dbSendQuery(con, 
            "COMMENT ON TABLE bv_2023.pyd_medicalcare_subgroup IS 'Table for Youth who reported ease in obtaining needed medical care in LA County. 
           Youth are defined as those betweeen the ages of 0 to 24 years Data source: LA County Health Survey 2023.
Race codes for AIAN and NHPI are All AIAN and All NHPI (multiracial + latino inclusive).
This table was prepared in the R script W:\\Project\\OSI\\Bold Vision\\BV 2023\\R\\boldvision_22_23\\Positive Youth Development\\pyd_medicalcare.R
QA doc:  W:\\Project\\OSI\\Bold Vision\\BV 2023\\Documentation\\QA_lachs_inidcators.docx';

COMMENT ON COLUMN bv_2023.pyd_medicalcare_subgroup.diff is 'The difference between the best rate and the respective race group. total and BIPOC not included in calculation.';
COMMENT ON COLUMN bv_2023.pyd_medicalcare_subgroup.index_of_disparity is 'The index of disparity by race with the best rate of the subgroups.';
COMMENT ON COLUMN bv_2023.pyd_medicalcare_subgroup.subgroup is 'Race/ethnicity category. AIAN and pacisl are multiracial latino-inclusive (All AIAN, All pacisl).';
COMMENT ON COLUMN bv_2023.pyd_medicalcare_subgroup.count is 'Count of youth who reported ease in obtaining needed medical care';
COMMENT ON COLUMN bv_2023.pyd_medicalcare_subgroup.rate is 'Percentage of youth who reported ease in obtaining needed medical care';
COMMENT ON COLUMN bv_2023.pyd_medicalcare_subgroup.rate_cv is 'CV (coefficient of variation)Values over 40%, are generally considered to be unstable estimates.';
COMMENT ON COLUMN bv_2023.pyd_medicalcare_subgroup.rate_moe is 'Margin of Error';
COMMENT ON COLUMN bv_2023.pyd_medicalcare_subgroup.ci_95_hi is '95% Confidence Interval Upper Bound';
COMMENT ON COLUMN bv_2023.pyd_medicalcare_subgroup.ci_95_low is '95% Confidence Interval Lower Bound';
"
)

####REGION Analysis and Data Table ####
#pull data and only select data needed
df_region <- read_xlsx("W:\\Project\\OSI\\Bold Vision\\BV 2023\\Data\\Positive Youth Development\\LACHS 2023_Catalyst California_Youth Health_final.xlsx", 
                       range = "B7:H36",  sheet = "ACCDIFFN", na = "*") %>% # choose sheet and range
  select(c(spa_name = 1, rate = 3, ci_95_low = 4, ci_95_hi = 6, count = 7)) %>% #only keep columns needed and rename
  mutate(rate = as.numeric(rate)*100)

#remove rows not needed
df_region <- df_region[-c(1:21),] 

#rename to follow BV workflow and standards
df_region <- df_region %>% mutate(spa_id = ifelse(spa_name == "Antelope Valley", "1",
                                                  ifelse(spa_name == "San Fernando", "2",
                                                         ifelse(spa_name == "San Gabriel", "3",
                                                                ifelse(spa_name == "Metro", "4",
                                                                       ifelse(spa_name == "West", "5",
                                                                              ifelse(spa_name == "South", "6",
                                                                                     ifelse(spa_name == "East", "7",
                                                                                            ifelse(spa_name == "South Bay", "8",
                                                                                                   'NA')))))))))

#create a new column for CV based on CI columns 
df_region <- df_region %>% mutate(rate_moe = (ci_95_hi - ci_95_low)/2,
                                  rate_cv = (rate_moe/1.96)/rate*100
)

#calculate the index of disparity
df_region <-
  df_region %>% 
  ungroup() %>%
  mutate(best=max(rate,na.rm=T), asbest="max",#create columns for best rate and as best treatment
         values_count = sum(!is.na(spa_name))) %>% 
  mutate(diff = abs(best-rate)) %>%
  mutate(index_of_disparity = ((sum(diff, na.rm=T)/best/(values_count - 1))*100)) 


# Write table to bold vision database
dbWriteTable(con, c("bv_2023", "pyd_medicalcare_region"), df_region,
             overwrite = TRUE, row.names = FALSE,
             field.types = c(
               spa_name = "varchar",
               spa_id = "varchar",
               count ="integer",
               ci_95_low = "numeric",
               ci_95_hi = "numeric",
               rate = "numeric",
               rate_moe = "numeric",
               rate_cv = "numeric",
               asbest = "varchar",
               values_count = "numeric",
               best = "numeric",
               diff= "numeric",
               index_of_disparity= "numeric"
             ))


# write comment to table

dbSendQuery(con, 
            "COMMENT ON TABLE bv_2023.pyd_medicalcare_region IS 'Table for Youth who reported ease in obtaining needed medical care in LA County. 
           Youth are defined as those betweeen the ages of 0 to 24 years Data source: LA County Health Survey 2023.
This table was prepared in the R script W:\\Project\\OSI\\Bold Vision\\BV 2023\\R\\boldvision_22_23\\Positive Youth Development\\pyd_medicalcare.R
QA doc:  W:\\Project\\OSI\\Bold Vision\\BV 2023\\Documentation\\QA_lachs_inidcators.docx';

COMMENT ON COLUMN bv_2023.pyd_medicalcare_region.diff is 'The difference between the best rate and the respective region group.';
COMMENT ON COLUMN bv_2023.pyd_medicalcare_region.index_of_disparity is 'The index of disparity by region with the best rate of the regions';
COMMENT ON COLUMN bv_2023.pyd_medicalcare_region.spa_name is 'Name of SPA region';
COMMENT ON COLUMN bv_2023.pyd_medicalcare_region.spa_id is 'LA County Service Planning Area (SPA) ID Number';
COMMENT ON COLUMN bv_2023.pyd_medicalcare_region.count is 'Count of youth who reported ease in obtaining needed medical care';
COMMENT ON COLUMN bv_2023.pyd_medicalcare_region.rate is 'Percentage of youth who reported ease in obtaining needed medical care';
COMMENT ON COLUMN bv_2023.pyd_medicalcare_region.rate_cv is 'CV (coefficient of variation)Values over 40% are generally considered to be unstable estimates.';
COMMENT ON COLUMN bv_2023.pyd_medicalcare_region.rate_moe is 'Margin of Error';
COMMENT ON COLUMN bv_2023.pyd_medicalcare_region.ci_95_hi is '95% Confidence Interval Upper Bound';
COMMENT ON COLUMN bv_2023.pyd_medicalcare_region.ci_95_low is '95% Confidence Interval Lower Bound';
"
)


dbDisconnect(conn = con)
dbDisconnect(conn = con_rda)
