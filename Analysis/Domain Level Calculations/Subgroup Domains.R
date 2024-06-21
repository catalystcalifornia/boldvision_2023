## Script creates domain level tables for estimates by subgroup/race
# pulls together indicators for each domain and ranks subgroups from best to lowest rates for heatmaps

# install packages if not already installed -----
list.of.packages <- c("dplyr","janitor","tidyr","stringr","showtext","RPostgreSQL","tigris","forcats", 
                      "data.table") 

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])] 

if(length(new.packages)) install.packages(new.packages) 


# Prep and Set Up 
#### Loading Libraries ####
library(dplyr)
library(janitor)
library(tidyr)
library(stringr)
library(showtext)
library(data.table)
library(forcats)
library(RPostgreSQL)
# library(sf)
library(tidyverse)
# library(here)
library(tigris)
# library(sp)


source("W:\\RDA Team\\R\\credentials_source.R")
conBV <- connect_to_db("bold_vision")

# #### DO NOT RUN -- EDIT IN POSTGRES - Indicator Labels ----
# 
# indicator_base <- c("regular_source_of_health_care",
#                     "ece_access",
#                     "asthma",
#                     "lack_of_greenspace",
#                     "food_access",
#                     "pollution_burden",
#                     "transit_injury",
#                     "housing_burden",
#                     "youth_voter_turnout",
#                     "civic_engage",
#                     "baseorgs",
#                     "livingwage",
#                     "mixedstatus",
#                     "childwelfare",
#                     "academic_attainment",
#                     "youth_arrest",
#                     "youth_diversion",
#                     "connectedyouth",
# "health_status",
# "access_health_care")
# 
# 
# indicator_label_short <- c("Regular Source of Health Care",
#                           "Early Childhood Education Access",
#                           "Asthma",
#                           "Lack of Green Space",
#                           "Access to Fruits and Vegetables",
#                           "Pollution Exposure",
#                           "Traffic Injuries / Fatalities",
#                           "Rent Burden",
#                           "Voter Turnout",
#                           "Political Engagement and Advocacy",
#                           "Access to Base-Building Organizations",
#                           "Living Wage",
#                           "Youth in Undocumented Families",
#                           "Youth in Foster Care / Probation Systems",
#                           "Graduation Rates of Systems-impacted Youth",
#                           "Youth Arrests",
#                           "Youth Diverted from Arrests",
#                           "Connected Youth",
# "Good/Excellent Health Status",
# "Access to Needed Medical Care")
# 
# indicator_label_long <- c("Youth with a Regular Source of Health Care",
#                           "Access to Early Care and Education",
#                           "Asthma",
#                           "Impervious Land Cover",
#                           "Access to Fruits and Vegetables",
#                           "Pollution Burden around Sensitive Land Uses for Youth",
#                           "Traffic Injuries/Fatalities",
#                           "Housing Burden Among Renter Households with Youth",
#                           "Youth Voter Turnout",
#                           "Youth Civic Engagement",
#                           "Access to Grassroots/Base-Building Organizations for Youth",
#                           "Youth Receiving a Living Wage",
#                           "Youth in Mixed Status Families",
#                           "Youth Who Are in the Foster Care or Probation System",
#                           "Academic Attainment among System-impacted Youth?(HS Graduation Rates)",
#                           "Yoth Arrests",
#                           "Youth Who Enrolled or Substantially Completed Diversion Program",
#                           "Connected Youth",
#                           "Good/Excellent Health Status",
#                           "Access to Needed Medical Care")
# 
# indicator_measure <- c("Percentage",
#                       "Percentage",
#                       "Percentage",
#                       "Percentage",
#                       "Percentage",
#                       "Percentile",
#                       "Weighted Average Rate per 1000",
#                       "Percentage",
#                       "Percentage",
#                       "Percentage",
#                       "Percentile",
#                       "Percentage",
#                       "Percentage",
#                       "Average Rate per 1000",
#                       "Percentage",
#                       "Rate per 1000",
#                       "Rate per 1000",
#                       "Percentage",
#                       "Percentage",
#                       "Percentage")
# 
# indicator_age_groups <- c("0-24",
#                           "0-4",
#                           "0-24",
#                           "0-24",
#                           "0-24",
#                           "0-24",
#                           "0-24",
#                           "0-24",
#                           "18-29",
#                           "18-29",
#                           "0-24",
#                           "15-24",
#                           "0-24",
#                           "0-20",
#                           "14-18",
#                           "0-24",
#                           "0-20",
#                           "14-24",
#                           "0-24",
#                           "0-24")
# 
# indicator_race_groups <- c("nh_asian, latino, nh_black, nh_white, aian, pacisl, bipoc",
#                           "nh_asian, latino, nh_black, nh_white, aian, pacisl, nh_twoormor, bipoc",
#                           "nh_asian, latino, nh_black, nh_white, aian, pacisl, bipoc",
#                           "nh_asian, latino, nh_black, nh_white, aian, pacisl, nh_twoormor, bipoc",
#                           "nh_asian, latino, nh_black, nh_white, aian, pacisl, bipoc",
#                           "aian, latino, nh_asian, nh_black, nh_other, nh_twoormor, nh_white, pacisl, bipoc",
#                           "aian, latino, nh_asian, nh_black, nh_other, nh_twoormor, nh_white, pacisl, bipoc",
#                           "nh_asian, nh_black,nh_other, nh_twoormor, nh_white, latino, aian, pacisl, swana, bipoc",
#                           "nh_asian, latino, nh_black, nh_white, aian, pacisl, nh_twoormor, bipoc",
#                           "nh_asian, latino, nh_black, nh_white, aian, pacisl, nh_twoormor, bipoc",
#                           "aian, latino, nh_asian, nh_black, nh_other, nh_twoormor, nh_white, pacisl, bipoc",
#                           "nh_asian, nh_black,nh_other, nh_twoormor, nh_white, latino, aian, pacisl, swana, bipoc",
#                           "nh_asian, nh_black, nh_other, nh_white, latino, bipoc",
#                           "nh_api, aian, latino, nh_black, nh_white",
#                           "nh_asian, nh_black, nh_filipino, latino, nh_aian, nh_pacisl, nh_twoormor, nh_white, foster, bipoc",
#                           "latino, nh_white, nh_black, nh_asian, nh_aian, pacisl, swana, nh_twoormor, bipoc",
#                           "latino, nh_white, nh_black, nh_api, nh_aian, swana, nh_twoormor, bipoc",
#                           "nh_asian, nh_black,nh_other, nh_twoormor, nh_white, latino, aian, pacisl, swana, bipoc",
#                           "aian, latino, nh_asian, nh_black, nh_twoormor, nh_white, pacisl, bipoc",
#                           "aian, latino, nh_asian, nh_black, nh_twoormor, nh_white, pacisl, bipoc")
# 
# # Join the variables to create a data frame
# domains <- data.frame(indicator_base, indicator_label_short, indicator_label_long, indicator_measure, indicator_age_groups, indicator_race_groups) #, indicator_description
# 
# #### Send to Postgres ####
# table_name <- "metadata_indicator_labels"
# schema <- 'bv_2023'
# dbWriteTable(conBV, c(schema, table_name), domains, overwrite = TRUE, row.names = FALSE)

##### Youth Power #####

#download civic engagement data
cv_subgroup <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.yp_civic_engage_youth_subgroup")%>% 
  mutate(category = "civic_engage") %>%  
  select(category, subgroup, diff, rate, index_of_disparity) %>% filter(!is.na(rate))

#Youth Voter Turnout Data
youth_voter_turnout_subgroup <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.yp_youth_voter_turnout_subgroup") %>% 
  mutate(category = "youth_voter_turnout") %>%  
  select(category, subgroup, diff, rate, index_of_disparity) %>% filter(!is.na(rate))

#Base Orgs data
baseorgs_subgroup <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.yp_baseorgs_subgroup") %>% 
  mutate(category = "baseorgs") %>% 
  select(category, subgroup, diff, rate, index_of_disparity)%>% filter(!is.na(rate)) 
baseorgs_subgroup <- baseorgs_subgroup%>% mutate(index_of_disparity = ifelse(subgroup=="bipoc"| subgroup=="total",baseorgs_subgroup$index_of_disparity[baseorgs_subgroup$subgroup=="aian"], index_of_disparity)) 

# Living Wage
livingwage_subgroup <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.yp_livingwage_subgroup") %>%   
  mutate(category = "livingwage") %>% rename("subgroup"="race") %>% 
  select(category, subgroup, diff, rate, index_of_disparity)%>% filter(!is.na(rate))

# combine into one dataframe
yp_subgroups <- rbind(baseorgs_subgroup, youth_voter_turnout_subgroup, cv_subgroup, livingwage_subgroup)

#### replace labels w/ label from metadata

#load metadata race labels
race_labels <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.metadata_race_labels") 
#replace race names with metadata race names using str_replace_all() from stringr package #https://stackoverflow.com/questions/69377802/string-replacing-in-a-data-frame-based-on-another-data-frame
subs <- setNames(race_labels$`race_label_short`, race_labels$`race_base`)
yp_subgroups$subgroup <- str_replace_all(yp_subgroups$subgroup, subs)

#load metadata indicator labels
indicator_labels <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.metadata_indicator_labels") 
#replace indicator names with metadata indicator names using str_replace_all() from stringr package #https://stackoverflow.com/questions/69377802/string-replacing-in-a-data-frame-based-on-another-data-frame
subs <- setNames(indicator_labels$`indicator_label_short`, indicator_labels$`indicator_base`)
yp_subgroups$category <- str_replace_all(yp_subgroups$category, subs)

#round 
yp_subgroups$rate <- round(yp_subgroups$rate,1)
yp_subgroups$index_of_disparity <- round(yp_subgroups$index_of_disparity,1)

# rank form best
subgroups_rescaled <- yp_subgroups %>% select(category, subgroup, diff) %>% filter(subgroup!="Total" & subgroup != "BIPOC") %>% 
  group_by(category) %>% mutate(rank = rank(diff)) %>% select(-diff)
subgroups_rescaled <- melt(subgroups_rescaled)
subgroups_rescaled <- data.table(subgroups_rescaled)
subgroups_rescaled[,avg_rank:=value/mean(value),by=.(category)]
subgroups_rescaled <- subgroups_rescaled %>% pivot_wider(names_from = variable, values_from = value) %>% as.data.frame()

#rejoin rank back to main df
yp_df <- left_join(yp_subgroups, subgroups_rescaled, by=c("category","subgroup")) %>% 
  as.data.table()
yp_df<-yp_df%>%left_join(indicator_labels%>%select(indicator_label_short,rate_label),by=c("category"="indicator_label_short"))
yp_df$rate <- paste0(yp_df$rate, yp_df$rate_label)
yp_df<-yp_df%>%select(-rate_label)
View(yp_df)

#### Send to Postgres ####
table_name <- "yp_subgroup_domain"
schema <- 'bv_2023'
# dbWriteTable(conBV, c(schema, table_name), yp_df, overwrite = TRUE, row.names = FALSE)

indicator <- "Table listing all indicators in the youth power domain with rates and differences by subgroups and ID scores by indicator. Table includes rank based on difference and a rescaled rank avg rank to keep the number of ranks consistent between indicators for chart purposes. Using avg rank for heatmap purposes"
source <- "See script and QA doc for details W:/Project/OSI/Bold Vision/BV 2023/R/Subgroup Domains.R and W:\\Project\\OSI\\Bold Vision\\BV 2023\\Documentation\\QA_Domains.docx"


#comment on table and columns
comment <- paste0("
                  COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".category IS 'indicator label based on metadata_indicator_labels the short label';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".subgroup IS 'race group or other group';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".diff IS 'Difference from the best for that indicator, excluding BIPOC and total';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".rate IS 'Rate for that subgroup and indicator with the unit of measure attached';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".index_of_disparity IS 'Overall ID score for the indicator, excluding BIPOC and total';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".avg_rank IS 'Rank rescaled to keep consistent ranks between indicators even if number of groups varies. To be used for heatmap charting';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".rank IS 'Original rank with rank 1 meaning best rate';
                  ")
# dbSendQuery(conBV, comment)

#### Positive Youth Development ####

# Health Status
health_status_subgroup <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.pyd_healthstatus_subgroup") %>% 
  mutate(category = "health_status") %>% 
  select(category, subgroup, diff, rate, index_of_disparity) %>% filter(!is.na(rate)) 

# Ease of Access to Care
access_health_care_subgroup <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.pyd_medicalcare_subgroup") %>% 
  mutate(category = "access_health_care") %>% 
  select(category, subgroup, diff, rate, index_of_disparity) %>% filter(!is.na(rate)) 

# Asthma
asthma_subgroup <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.pyd_asthma_subgroup") %>% 
  mutate(category = "asthma") %>% 
  select(category, subgroup, diff, rate, index_of_disparity) %>% filter(!is.na(rate)) 

# #Access to early care and education Data
ece_subgroup <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.pyd_ece_access_subgroup") %>% 
  mutate(category = "ece_access") %>% 
  select(category, subgroup, diff, rate, index_of_disparity) %>% filter(!is.na(rate)) 

#download Connected Youth data
connectedyouth_subgroup <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.pyd_connectedyouth_subgroup") %>% 
  mutate(category = "connectedyouth") %>% rename("subgroup"="race") %>% 
  select(category, subgroup, diff, rate, index_of_disparity) %>% filter(!is.na(rate))

# combine into one dataframe
pyd_subgroups <- rbind(ece_subgroup, connectedyouth_subgroup,asthma_subgroup,access_health_care_subgroup,health_status_subgroup)

#### replace labels w/ label from metadata

#load metadata race labels
race_labels <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.metadata_race_labels") 
#replace race names with metadata race names using str_replace_all() from stringr package #https://stackoverflow.com/questions/69377802/string-replacing-in-a-data-frame-based-on-another-data-frame
subs <- setNames(race_labels$`race_label_short`, race_labels$`race_base`)
pyd_subgroups$subgroup <- str_replace_all(pyd_subgroups$subgroup, subs)

#load metadata indicator labels
indicator_labels <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.metadata_indicator_labels") 
#replace indicator names with metadata indicator names using str_replace_all() from stringr package #https://stackoverflow.com/questions/69377802/string-replacing-in-a-data-frame-based-on-another-data-frame
subs <- setNames(indicator_labels$`indicator_label_short`, indicator_labels$`indicator_base`)
pyd_subgroups$category <- str_replace_all(pyd_subgroups$category, subs)

#round 
pyd_subgroups$rate <- round(pyd_subgroups$rate,1)
pyd_subgroups$index_of_disparity <- round(pyd_subgroups$index_of_disparity,1)

# rank form best
subgroups_rescaled <- pyd_subgroups %>% select(category, subgroup, diff) %>% filter(subgroup!="Total" & subgroup != "BIPOC") %>% 
  group_by(category) %>% mutate(rank = rank(diff)) %>% select(-diff)
subgroups_rescaled <- melt(subgroups_rescaled)
subgroups_rescaled <- data.table(subgroups_rescaled)
subgroups_rescaled[,avg_rank:=value/mean(value),by=.(category)]
subgroups_rescaled <- subgroups_rescaled %>% pivot_wider(names_from = variable, values_from = value) %>% as.data.frame()

#rejoin rank back to main df
pyd_df <- left_join(pyd_subgroups, subgroups_rescaled, by=c("category","subgroup")) %>% 
  as.data.table()
pyd_df<-pyd_df%>%left_join(indicator_labels%>%select(indicator_label_short,rate_label),by=c("category"="indicator_label_short"))
pyd_df$rate <- paste0(pyd_df$rate, pyd_df$rate_label)
pyd_df<-pyd_df%>%select(-rate_label)
View(pyd_df)

#### Send to Postgres ####
table_name <- "pyd_subgroup_domain"
schema <- 'bv_2023'
# dbWriteTable(conBV, c(schema, table_name), pyd_df, overwrite = TRUE, row.names = FALSE)

indicator <- "Table listing all indicators in the positive youth development domain with rates and differences by subgroups and ID scores by indicator. Table includes rank based on difference and a rescaled rank avg rank to keep the number of ranks consistent between indicators for chart purposes. Using avg rank for heatmap purposes"
source <- "See script and QA doc for details W:/Project/OSI/Bold Vision/BV 2023/R/Subgroup Domains.R and W:\\Project\\OSI\\Bold Vision\\BV 2023\\Documentation\\QA_Domains.docx"


#comment on table and columns
comment <- paste0("
                  COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".category IS 'indicator label based on metadata_indicator_labels the short label';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".subgroup IS 'race group or other group';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".diff IS 'Difference from the best for that indicator, excluding BIPOC and total';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".rate IS 'Rate for that subgroup and indicator with the unit of measure attached';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".index_of_disparity IS 'Overall ID score for the indicator, excluding BIPOC and total';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".avg_rank IS 'Rank rescaled to keep consistent ranks between indicators even if number of groups varies. To be used for heatmap charting';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".rank IS 'Original rank with rank 1 meaning best rate';
                  ")
# dbSendQuery(conBV, comment)

#### Healthy Built Environment ####
# Access to fruits and vegetables
food_access_subgroup <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.hbe_frtsveg_subgroup") %>% 
  mutate(category = "food_access") %>% 
  select(category, subgroup, diff, rate, index_of_disparity) %>% filter(!is.na(rate)) 

#download Housing burden among households with youth (ages 0-24) Data
housing_burden_subgroup <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.hbe_housing_burden_subgroup") %>% 
  mutate(category = "housing_burden") %>% 
  select(category, subgroup, diff, rate, index_of_disparity) %>% filter(!is.na(rate)) 

#download Impervious Land Cover data
lack_of_greenspace_subgroup <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.hbe_lack_of_greenspace_subgroup") %>% 
  mutate(category = "lack_of_greenspace") %>% 
  select(category, subgroup, diff, rate, index_of_disparity) %>% filter(!is.na(rate))

#download Transit service availability (frequency or high-quality transit area access) or traffic injuries/fatalities data
transit_injury_subgroup <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.hbe_transit_injury_subgroup") %>% 
  mutate(category = "transit_injury") %>% 
  select(category, subgroup, diff, rate, index_of_disparity) %>% filter(!is.na(rate))

#download Pollution Burden data
pollution_burden_subgroup <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.hbe_pollution_burden_subgroup") %>% 
  mutate(category = "pollution_burden",rate=rate*100) %>% rename("spa" = "geoid") %>%  
  select(category, subgroup, diff, rate, index_of_disparity) %>% filter(!is.na(rate))

# combine into one dataframe
hbe_subgroups <- rbind(food_access_subgroup,housing_burden_subgroup, lack_of_greenspace_subgroup, transit_injury_subgroup, pollution_burden_subgroup)

#### replace labels w/ label from metadata

#load metadata race labels
race_labels <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.metadata_race_labels") 
#replace race names with metadata race names using str_replace_all() from stringr package #https://stackoverflow.com/questions/69377802/string-replacing-in-a-data-frame-based-on-another-data-frame
subs <- setNames(race_labels$`race_label_short`, race_labels$`race_base`)
hbe_subgroups$subgroup <- str_replace_all(hbe_subgroups$subgroup, subs)

#load metadata indicator labels
indicator_labels <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.metadata_indicator_labels") 
#replace indicator names with metadata indicator names using str_replace_all() from stringr package #https://stackoverflow.com/questions/69377802/string-replacing-in-a-data-frame-based-on-another-data-frame
subs <- setNames(indicator_labels$`indicator_label_short`, indicator_labels$`indicator_base`)
hbe_subgroups$category <- str_replace_all(hbe_subgroups$category, subs)

#round 
hbe_subgroups$rate <- round(hbe_subgroups$rate,1)
hbe_subgroups$index_of_disparity <- round(hbe_subgroups$index_of_disparity,1)

# rank form best
subgroups_rescaled <- hbe_subgroups %>% select(category, subgroup, diff) %>% filter(subgroup!="Total" & subgroup != "BIPOC") %>% 
  group_by(category) %>% mutate(rank = rank(diff)) %>% select(-diff)
subgroups_rescaled <- melt(subgroups_rescaled)
subgroups_rescaled <- data.table(subgroups_rescaled)
subgroups_rescaled[,avg_rank:=value/mean(value),by=.(category)]
subgroups_rescaled <- subgroups_rescaled %>% pivot_wider(names_from = variable, values_from = value) %>% as.data.frame()

#rejoin rank back to main df
hbe_df <- left_join(hbe_subgroups, subgroups_rescaled, by=c("category","subgroup")) %>% 
  as.data.table()
hbe_df<-hbe_df%>%left_join(indicator_labels%>%select(indicator_label_short,rate_label),by=c("category"="indicator_label_short"))
hbe_df$rate <- paste0(hbe_df$rate, hbe_df$rate_label)
hbe_df<-hbe_df%>%select(-rate_label)
View(hbe_df)

#### Send to Postgres ####
table_name <- "hbe_subgroup_domain"
schema <- 'bv_2023'
# dbWriteTable(conBV, c(schema, table_name), hbe_df, overwrite = TRUE, row.names = FALSE)

indicator <- "Table listing all indicators in the healthy built environment domain with rates and differences by subgroups and ID scores by indicator. Table includes rank based on difference and a rescaled rank avg rank to keep the number of ranks consistent between indicators for chart purposes. Using avg rank for heatmap purposes"
source <- "See script and QA doc for details W:/Project/OSI/Bold Vision/BV 2023/R/Subgroup Domains.R and W:\\Project\\OSI\\Bold Vision\\BV 2023\\Documentation\\QA_Domains.docx"


#comment on table and columns
comment <- paste0("
                  COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".category IS 'indicator label based on metadata_indicator_labels the short label';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".subgroup IS 'race group or other group';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".diff IS 'Difference from the best for that indicator, excluding BIPOC and total';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".rate IS 'Rate for that subgroup and indicator with the unit of measure attached';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".index_of_disparity IS 'Overall ID score for the indicator, excluding BIPOC and total';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".avg_rank IS 'Rank rescaled to keep consistent ranks between indicators even if number of groups varies. To be used for heatmap charting';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".rank IS 'Original rank with rank 1 meaning best rate';
                  ")
# dbSendQuery(conBV, comment)

#### Systems Impact ####

# download Mixed Status Data
mixedstatus_subgroup <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.si_mixedstatus_subgroup") %>% 
  mutate(category = "mixedstatus") %>% 
  select(category, subgroup, diff, rate, index_of_disparity) %>% filter(!is.na(rate)) 

#download Academic Attainment data
academic_attainment_subgroup <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.si_academic_attainment_subgroup") %>% 
  mutate(category = "academic_attainment") %>% 
  select(category, subgroup, diff, rate, index_of_disparity) %>% filter(!is.na(rate) & subgroup!="foster")

# download Child Welfare Data
childwelfare_subgroup <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.si_childwelfare_subgroup") %>% 
  mutate(category = "childwelfare") %>% 
  select(category, subgroup, diff, rate, index_of_disparity) %>% filter(!is.na(rate)) 

#download Youth Diversion data
youth_diversion_subgroup <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.si_youth_diversion_subgroup") %>% 
  mutate(category = "youth_diversion") %>% 
  select(category, subgroup, diff, rate, index_of_disparity) %>% filter(!is.na(rate))

#download Youth Arrest data
youth_arrest_subgroup <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.si_youth_arrest_subgroup") %>% 
  mutate(category = "youth_arrest") %>% 
  select(category, subgroup, diff, rate, index_of_disparity) %>% filter(!is.na(rate))

# combine into one dataframe
si_subgroups <- rbind(mixedstatus_subgroup, academic_attainment_subgroup, childwelfare_subgroup, youth_diversion_subgroup, youth_arrest_subgroup)

#### replace labels w/ label from metadata
#load metadata race labels
race_labels <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.metadata_race_labels") 
#replace race names with metadata race names using str_replace_all() from stringr package #https://stackoverflow.com/questions/69377802/string-replacing-in-a-data-frame-based-on-another-data-frame
subs <- setNames(race_labels$`race_label_short`, race_labels$`race_base`)
si_subgroups$subgroup <- str_replace_all(si_subgroups$subgroup, subs)
#load metadata indicator labels
#load metadata indicator labels
indicator_labels <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.metadata_indicator_labels") 
#replace indicator names with metadata indicator names using str_replace_all() from stringr package #https://stackoverflow.com/questions/69377802/string-replacing-in-a-data-frame-based-on-another-data-frame
subs <- setNames(indicator_labels$`indicator_label_short`, indicator_labels$`indicator_base`)
si_subgroups$category <- str_replace_all(si_subgroups$category, subs)

#round 
si_subgroups$rate <- round(si_subgroups$rate,1)
si_subgroups$index_of_disparity <- round(si_subgroups$index_of_disparity,1)

# rank form best
subgroups_rescaled <- si_subgroups %>% select(category, subgroup, diff) %>% filter(subgroup!="Total" & subgroup != "BIPOC") %>% 
  group_by(category) %>% mutate(rank = rank(diff)) %>% select(-diff)
subgroups_rescaled <- melt(subgroups_rescaled)
subgroups_rescaled <- data.table(subgroups_rescaled)
subgroups_rescaled[,avg_rank:=value/mean(value),by=.(category)]
subgroups_rescaled <- subgroups_rescaled %>% pivot_wider(names_from = variable, values_from = value) %>% as.data.frame()

#rejoin rank back to main df
si_df <- left_join(si_subgroups, subgroups_rescaled, by=c("category","subgroup")) %>% 
  as.data.table()
si_df<-si_df%>%left_join(indicator_labels%>%select(indicator_label_short,rate_label),by=c("category"="indicator_label_short"))
si_df$rate <- paste0(si_df$rate, si_df$rate_label)
si_df<-si_df%>%select(-rate_label)

# rename variables that were not renamed by the metadata
si_df$subgroup <- gsub("nh_filipino","Filipine",si_df$subgroup)
si_df$subgroup <- gsub("nh_AIAN","AIAN",si_df$subgroup)
si_df$subgroup <- gsub("nh_NHPI","NHPI",si_df$subgroup)
si_df$subgroup <- gsub("nh_api","API",si_df$subgroup)
si_df$subgroup <- gsub("sSWANA","SWANA/SA",si_df$subgroup) # for some reason sswana label doesn't work--change

View(si_df)

#### Send to Postgres ####

table_name <- "si_subgroup_domain"
schema <- 'bv_2023'

# dbWriteTable(conBV, c(schema, table_name), si_df,
#             overwrite = TRUE, row.names = FALSE)

indicator <- "Table listing all indicators in the systems impact domain with rates and differences by subgroups and ID scores by indicator. Table includes rank based on difference and a rescaled rank avg rank to keep the number of ranks consistent between indicators for chart purposes. Using avg rank for heatmap purposes"
source <- "See script and QA doc for details W:/Project/OSI/Bold Vision/BV 2023/R/Subgroup Domains.R and W:\\Project\\OSI\\Bold Vision\\BV 2023\\Documentation\\QA_Domains.docx"


#comment on table and columns
comment <- paste0("
                  COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".category IS 'indicator label based on metadata_indicator_labels the short label';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".subgroup IS 'race group or other group';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".diff IS 'Difference from the best for that indicator, excluding BIPOC and total';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".rate IS 'Rate for that subgroup and indicator with the unit of measure attached';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".index_of_disparity IS 'Overall ID score for the indicator, excluding BIPOC and total';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".avg_rank IS 'Rank rescaled to keep consistent ranks between indicators even if number of groups varies. To be used for heatmap charting';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".rank IS 'Original rank with rank 1 meaning best rate';
                  ")
# dbSendQuery(conBV, comment)

#disconnect -----
dbDisconnect(conBV)
