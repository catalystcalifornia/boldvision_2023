## Script creates domain level tables for estimates by SPA/region
# pulls together indicators for each domain and ranks regions from best to lowest rate for heatmap visuals

###### install packages if not already installed -----
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
library(tidyverse)
# library(here)
library(tigris)
library(sf)
# library(sp)


source("W:\\RDA Team\\R\\credentials_source.R")
conBV <- connect_to_db("bold_vision")

### DO NOT RUN EDIT IN POSTGRES - Regional metadata labels ----

# spa_id <- c("1","2","3","4","5","6","7","8")
# spa_name <- c("Antelope Valley", "San Fernando Valley", "San Gabriel Valley", "Metro LA", "West LA", "South LA", "East LA", "South Bay")
# 
# # Join the variables to create a data frame
# domains <- data.frame(spa_id, spa_name)

####### Send to Postgres -------
# table_name <- "metadata_spa_labels"
# schema <- 'bv_2023'
# dbWriteTable(conBV, c(schema, table_name), domains, overwrite = FALSE, row.names = FALSE)

# indicator <- "Metadata table includes SPA id numbers and labels for visuals"
# source <- "See script W:/Project/OSI/Bold Vision/BV 2023/R/Region Domains.R and QA doc W:\\Project\\OSI\\Bold Vision\\BV 2023\\Documentation\\QA_Spa_level_heatmaps_and_ID_charts.docx for details "

# add primary key
# pk_query<-paste0("ALTER TABLE ", schema, ".",table_name," ADD PRIMARY KEY (spa_id);")
# dbSendQuery(conBV, pk_query)

#comment on table and columns
# comment <- paste0("
#                   COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
#                   COMMENT ON COLUMN ", schema, ".", table_name, ".spa_id IS 'SPA numeric id also serves as primary key';
#                   COMMENT ON COLUMN ", schema, ".", table_name, ".spa_name IS 'Service Planning Area (SPA) name for labeling';
#                                 ")
# dbSendQuery(conBV, comment)


##### Youth Power #####

#Base Orgs data
baseorgs_region <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.yp_baseorgs_region") %>% 
  mutate(category = "baseorgs") %>% rename("spa" = "geoid") %>% 
  select(category, spa, diff, rate, index_of_disparity)%>% filter(!is.na(rate)) 

# Living Wage
livingwage_region <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.yp_livingwage_region") %>%   
  mutate(category = "livingwage") %>% rename("spa" = "spa_id") %>% 
  select(category, spa, diff, rate, index_of_disparity)%>% filter(!is.na(rate))

# Youth voter turnout
voterturnout_region <-  dbGetQuery(conBV, "SELECT geoid,name,diff,rate,index_of_disparity FROM bv_2023.yp_youth_voter_turnout_region") %>%
  mutate(category = "youth_voter_turnout") %>% rename("spa" = "name") %>% 
  select(category, spa, diff, rate, index_of_disparity)%>% filter(!is.na(rate))

# Civic engagement
ce_region <-  dbGetQuery(conBV, "SELECT geoid,name,diff,rate,index_of_disparity FROM bv_2023.yp_civic_engage_youth_region") %>%
  mutate(category = "civic_engage") %>% rename("spa" = "name") %>% 
  select(category, spa, diff, rate, index_of_disparity)%>% filter(!is.na(rate))

# combine into one dataframe
yp_region <- rbind(baseorgs_region, livingwage_region,voterturnout_region,ce_region)

#load metadata indicator labels
indicator_labels <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.metadata_indicator_labels") 
#replace indicator names with metadata indicator names using str_replace_all() from stringr package #https://stackoverflow.com/questions/69377802/string-replacing-in-a-data-frame-based-on-another-data-frame
subs <- setNames(indicator_labels$`indicator_label_short`, indicator_labels$`indicator_base`)
yp_region$category <- str_replace_all(yp_region$category, subs)

#load metadata spa labels
spa_labels <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.metadata_spa_labels") 
#replace indicator names with metadata indicator names using str_replace_all() from stringr package #https://stackoverflow.com/questions/69377802/string-replacing-in-a-data-frame-based-on-another-data-frame
subs <- setNames(spa_labels$`spa_name`, spa_labels$`spa_id`)
yp_region$spa <- str_replace_all(yp_region$spa, subs)

#round 
yp_region$rate <- round(yp_region$rate,1)
yp_region$index_of_disparity <- round(yp_region$index_of_disparity,1)

# rank form best
spa_rescaled <- yp_region %>% select(category, spa, diff) %>% group_by(category) %>% mutate(rank = rank(diff)) %>% select(-diff)
spa_rescaled <- melt(spa_rescaled)
spa_rescaled <- data.table(spa_rescaled)
spa_rescaled[,avg_rank:=value/mean(value),by=.(category)]
spa_rescaled <- spa_rescaled %>% pivot_wider(names_from = variable, values_from = value) %>% as.data.frame()

#rejoin rank back to main df
yp_df <- left_join(yp_region, spa_rescaled, by=c("category", "spa")) %>% as.data.table()
yp_df<-yp_df%>%left_join(indicator_labels%>%select(indicator_label_short,rate_label),by=c("category"="indicator_label_short"))
yp_df$rate <- paste0(yp_df$rate, yp_df$rate_label)
yp_df<-yp_df%>%select(-rate_label)
View(yp_df)

#### Send to Postgres ####
table_name <- "yp_region_domain"
schema <- 'bv_2023'
# dbWriteTable(conBV, c(schema, table_name), yp_df, overwrite = TRUE, row.names = FALSE)

indicator <- "Table listing all indicators in the youth power domain with rates and differences by regions such as Service Planning Areas (SPAs) and ID scores by indicator. Note that for youth voter turnout and civic engagement the regions are cities, not SPAs. Table includes rank based on difference and a rescaled rank avg rank to keep the number of ranks consistent between indicators for chart purposes. Using avg rank for heatmap purposes"
source <- "See script and QA doc for details W:/Project/OSI/Bold Vision/BV 2023/R/Region Domains.R and W:\\Project\\OSI\\Bold Vision\\BV 2023\\Documentation\\QA_Spa_level_heatmaps_and_ID_charts.docx"

#comment on table and columns
comment <- paste0("
                  COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".category IS 'indicator label based on metadata_indicator_labels the short label';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".spa IS 'Service Planning Area (SPA)';

                  COMMENT ON COLUMN ", schema, ".", table_name, ".diff IS 'Difference from the best for that indicator';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".rate IS 'Rate for that region and indicator with the unit of measure attached';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".index_of_disparity IS 'Overall ID score for the indicator';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".avg_rank IS 'Rank rescaled to keep consistent ranks between indicators even if number of groups varies. To be used for heatmap charting';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".rank IS 'Original rank with rank 1 meaning best rate';
                  ")
# dbSendQuery(conBV, comment)

#### Positive Youth Development ####
# Health Status
health_status_region <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.pyd_healthstatus_region") %>% 
  mutate(category = "health_status",spa=spa_id) %>% 
  select(category, spa, diff, rate, index_of_disparity) %>% filter(!is.na(rate)) 

# Ease of Access to Care
access_health_care_region <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.pyd_medicalcare_region") %>% 
  mutate(category = "access_health_care",spa=spa_id) %>% 
  select(category, spa, diff, rate, index_of_disparity) %>% filter(!is.na(rate)) 

# Asthma
asthma_region <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.pyd_asthma_region") %>% 
  mutate(category = "asthma",spa=spa_id) %>% 
  select(category, spa, diff, rate, index_of_disparity) %>% filter(!is.na(rate)) 

#Access to early care and education Data
ece_region <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.pyd_ece_access_region") %>% 
  mutate(category = "ece_access") %>% rename("spa" = "geoid") %>% 
  select(category, spa, diff, rate, index_of_disparity) %>% filter(!is.na(rate)) 

#download Connected Youth data
connectedyouth_region <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.pyd_connectedyouth_region") %>% 
  mutate(category = "connectedyouth") %>% rename("spa" = "spa_id")%>%  
  select(category, spa, diff, rate, index_of_disparity) %>% filter(!is.na(rate)) 

# combine into one dataframe
pyd_region <- rbind(ece_region, connectedyouth_region, health_status_region, access_health_care_region,asthma_region)

#load metadata indicator labels
indicator_labels <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.metadata_indicator_labels") 
#replace indicator names with metadata indicator names using str_replace_all() from stringr package #https://stackoverflow.com/questions/69377802/string-replacing-in-a-data-frame-based-on-another-data-frame
subs <- setNames(indicator_labels$`indicator_label_short`, indicator_labels$`indicator_base`)
pyd_region$category <- str_replace_all(pyd_region$category, subs)

#load metadata spa labels
spa_labels <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.metadata_spa_labels") 
#replace indicator names with metadata indicator names using str_replace_all() from stringr package #https://stackoverflow.com/questions/69377802/string-replacing-in-a-data-frame-based-on-another-data-frame
subs <- setNames(spa_labels$`spa_name`, spa_labels$`spa_id`)
pyd_region$spa <- str_replace_all(pyd_region$spa, subs)

#round 
pyd_region$rate <- round(pyd_region$rate,1)
pyd_region$index_of_disparity <- round(pyd_region$index_of_disparity,1)

# rank form best
spa_rescaled <- pyd_region %>% select(category, spa, diff) %>% 
  group_by(category) %>% mutate(rank = rank(diff)) %>% select(-diff)
spa_rescaled <- melt(spa_rescaled)
spa_rescaled <- data.table(spa_rescaled)
spa_rescaled[,avg_rank:=value/mean(value),by=.(category)]
spa_rescaled <- spa_rescaled %>% pivot_wider(names_from = variable, values_from = value) %>% as.data.frame()

#rejoin rank back to main df
pyd_df <- left_join(pyd_region, spa_rescaled, by=c("category", "spa")) %>% 
  as.data.table()
pyd_df<-pyd_df%>%left_join(indicator_labels%>%select(indicator_label_short,rate_label),by=c("category"="indicator_label_short"))
pyd_df$rate <- paste0(pyd_df$rate, pyd_df$rate_label)
pyd_df<-pyd_df%>%select(-rate_label)
View(pyd_df)

#### Send to Postgres ####
table_name <- "pyd_region_domain"
schema <- 'bv_2023'
# dbWriteTable(conBV, c(schema, table_name), pyd_df, overwrite = TRUE, row.names = FALSE)


indicator <- "Table listing all indicators in the positive youth development domain with rates and differences by regions such as Service Planning Areas (SPAs) and ID scores by indicator. Table includes rank based on difference and a rescaled rank avg rank to keep the number of ranks consistent between indicators for chart purposes. Using avg rank for heatmap purposes"
source <- "See script and QA doc for details W:/Project/OSI/Bold Vision/BV 2023/R/Region Domains.R and W:\\Project\\OSI\\Bold Vision\\BV 2023\\Documentation\\QA_Spa_level_heatmaps_and_ID_charts.docx"


#comment on table and columns
comment <- paste0("
                  COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".category IS 'indicator label based on metadata_indicator_labels the short label';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".spa IS 'Service Planning Area (SPA)';

                  COMMENT ON COLUMN ", schema, ".", table_name, ".diff IS 'Difference from the best for that indicator';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".rate IS 'Rate for that region and indicator with the unit of measure attached';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".index_of_disparity IS 'Overall ID score for the indicator';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".avg_rank IS 'Rank rescaled to keep consistent ranks between indicators even if number of groups varies. To be used for heatmap charting';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".rank IS 'Original rank with rank 1 meaning best rate';
                  ")
# dbSendQuery(conBV, comment)

#### Healthy Built Environment ####
# Access to fruits and vegetables
food_access_region <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.hbe_frtsveg_region") %>% 
  mutate(category = "food_access",spa=spa_id) %>% 
  select(category, spa, diff, rate, index_of_disparity) %>% filter(!is.na(rate)) 

#download Housing burden among households with youth (ages 0-24) Data
housing_burden_region <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.hbe_housing_burden_region") %>% 
  mutate(category = "housing_burden") %>% rename("spa" = "geoid") %>%  
  select(category, spa, diff, rate, index_of_disparity) %>% filter(!is.na(rate)) 

#download Impervious Land Cover data
lack_of_greenspace_region <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.hbe_lack_of_greenspace_region") %>% 
  mutate(category = "lack_of_greenspace") %>% rename("spa" = "geoid") %>% 
  select(category, spa, diff, rate, index_of_disparity) %>% filter(!is.na(rate))

#download Transit service availability (frequency or high-quality transit area access) or traffic injuries/fatalities data
transit_injury_region <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.hbe_transit_injury_region") %>% 
  mutate(category = "transit_injury") %>% rename("spa" = "geoid") %>%  
  select(category, spa, diff, rate, index_of_disparity) %>% filter(!is.na(rate))

#download Pollution Burden availability (frequency or high-quality transit area access) or traffic injuries/fatalities data
pollution_burden_region <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.hbe_pollution_burden_region") %>% 
  mutate(category = "pollution_burden",rate=rate*100) %>% rename("spa" = "geoid") %>%  
  select(category, spa, diff, rate, index_of_disparity) %>% filter(!is.na(rate))

# combine into one dataframe
hbe_region <- rbind(food_access_region,housing_burden_region, lack_of_greenspace_region, transit_injury_region, pollution_burden_region)


#load metadata indicator labels
indicator_labels <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.metadata_indicator_labels") 
#replace indicator names with metadata indicator names using str_replace_all() from stringr package #https://stackoverflow.com/questions/69377802/string-replacing-in-a-data-frame-based-on-another-data-frame
subs <- setNames(indicator_labels$`indicator_label_short`, indicator_labels$`indicator_base`)
hbe_region$category <- str_replace_all(hbe_region$category, subs)

#load metadata spa labels
spa_labels <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.metadata_spa_labels") 
#replace indicator names with metadata indicator names using str_replace_all() from stringr package #https://stackoverflow.com/questions/69377802/string-replacing-in-a-data-frame-based-on-another-data-frame
subs <- setNames(spa_labels$`spa_name`, spa_labels$`spa_id`)
hbe_region$spa <- str_replace_all(hbe_region$spa, subs)

#round 
hbe_region$rate <- round(hbe_region$rate,1)
hbe_region$index_of_disparity <- round(hbe_region$index_of_disparity,1)

# rank form best
spa_rescaled <- hbe_region %>% select(category, spa, diff) %>%  
  group_by(category) %>% mutate(rank = rank(diff)) %>% select(-diff)
spa_rescaled <- melt(spa_rescaled)
spa_rescaled <- data.table(spa_rescaled)
spa_rescaled[,avg_rank:=value/mean(value),by=.(category)]
spa_rescaled <- spa_rescaled %>% pivot_wider(names_from = variable, values_from = value) %>% as.data.frame()

#rejoin rank back to main df
hbe_df <- left_join(hbe_region, spa_rescaled, by=c("category", "spa")) %>% 
  as.data.table()
hbe_df<-hbe_df%>%left_join(indicator_labels%>%select(indicator_label_short,rate_label),by=c("category"="indicator_label_short"))
hbe_df$rate <- paste0(hbe_df$rate, hbe_df$rate_label)
hbe_df<-hbe_df%>%select(-rate_label)
View(hbe_df)

#### Send to Postgres ####
table_name <- "hbe_region_domain"
schema <- 'bv_2023'
# dbWriteTable(conBV, c(schema, table_name), hbe_df, overwrite = TRUE, row.names = FALSE)

indicator <- "Table listing all indicators in the healthy built environment domain with rates and differences by regions such as Service Planning Areas (SPAs) and ID scores by indicator. Table includes rank based on difference and a rescaled rank avg rank to keep the number of ranks consistent between indicators for chart purposes. Using avg rank for heatmap purposes"
source <- "See script and QA doc for details W:/Project/OSI/Bold Vision/BV 2023/R/Region Domains.R and W:\\Project\\OSI\\Bold Vision\\BV 2023\\Documentation\\QA_Spa_level_heatmaps_and_ID_charts.docx"


#comment on table and columns
comment <- paste0("
                  COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".category IS 'indicator label based on metadata_indicator_labels the short label';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".spa IS 'Service Planning Area (SPA)';

                  COMMENT ON COLUMN ", schema, ".", table_name, ".diff IS 'Difference from the best for that indicator';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".rate IS 'Rate for that region and indicator with the unit of measure attached';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".index_of_disparity IS 'Overall ID score for the indicator';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".avg_rank IS 'Rank rescaled to keep consistent ranks between indicators even if number of groups varies. To be used for heatmap charting';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".rank IS 'Original rank with rank 1 meaning best rate';
                  ")
# dbSendQuery(conBV, comment)

#### Systems Impact ####

# download Mixed Status Data
mixedstatus_region <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.si_mixedstatus_region") %>% 
  mutate(category = "mixedstatus") %>% rename("spa" = "spa_id") %>%  
  select(category, spa, diff, rate, index_of_disparity) %>% filter(!is.na(rate)) 

#download Academic Attainment data
academic_attainment_region <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.si_academic_attainment_region") %>% 
  mutate(category = "academic_attainment",rate=rate*100) %>% rename("spa" = "geoid") %>%  
  select(category, spa, diff, rate, index_of_disparity) %>% filter(!is.na(rate))

# download Child Welfare Data
childwelfare_region <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.si_childwelfare_region") %>% 
  mutate(category = "childwelfare") %>% rename("spa" = "geoid") %>%  
  select(category, spa, diff, rate, index_of_disparity) %>% filter(!is.na(rate)) 

#download Youth Diversion data
youth_diversion_region <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.si_youth_diversion_region") %>% 
  mutate(category = "youth_diversion") %>% rename("spa" = "geoid") %>%  
  select(category, spa, diff, rate, index_of_disparity) %>% filter(!is.na(rate))

#download Youth Arrest data
youth_arrest_region <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.si_youth_arrest_region") %>% 
  mutate(category = "youth_arrest") %>% rename("spa" = "geoid") %>%  
  select(category, spa, diff, rate, index_of_disparity) %>% filter(!is.na(rate))

# combine into one dataframe
si_region <- rbind(mixedstatus_region, academic_attainment_region, childwelfare_region, youth_diversion_region, youth_arrest_region)

#load metadata indicator labels
indicator_labels <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.metadata_indicator_labels") 
#replace indicator names with metadata indicator names using str_replace_all() from stringr package #https://stackoverflow.com/questions/69377802/string-replacing-in-a-data-frame-based-on-another-data-frame
subs <- setNames(indicator_labels$`indicator_label_short`, indicator_labels$`indicator_base`)
si_region$category <- str_replace_all(si_region$category, subs)

#load metadata spa labels
spa_labels <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.metadata_spa_labels") 
#replace indicator names with metadata indicator names using str_replace_all() from stringr package #https://stackoverflow.com/questions/69377802/string-replacing-in-a-data-frame-based-on-another-data-frame
subs <- setNames(spa_labels$`spa_name`, spa_labels$`spa_id`)
si_region$spa <- str_replace_all(si_region$spa, subs)

#round 
si_region$rate <- round(si_region$rate,1)
si_region$index_of_disparity <- round(si_region$index_of_disparity,1)

# rank form best
spa_rescaled <- si_region %>% select(category, spa, diff)  %>% 
  group_by(category) %>% mutate(rank = rank(diff)) %>% select(-diff)
spa_rescaled <- melt(spa_rescaled)
spa_rescaled <- data.table(spa_rescaled)
spa_rescaled[,avg_rank:=value/mean(value),by=.(category)]
spa_rescaled <- spa_rescaled %>% pivot_wider(names_from = variable, values_from = value) %>% as.data.frame()

#rejoin rank back to main df
si_df <- left_join(si_region, spa_rescaled, by=c("category", "spa")) %>% 
  as.data.table()
si_df<-si_df%>%left_join(indicator_labels%>%select(indicator_label_short,rate_label),by=c("category"="indicator_label_short"))
si_df$rate <- paste0(si_df$rate, si_df$rate_label)
si_df<-si_df%>%select(-rate_label)
View(si_df)

#### Send to Postgres ####

table_name <- "si_region_domain"
schema <- 'bv_2023'

# dbWriteTable(conBV, c(schema, table_name), si_df, overwrite = TRUE, row.names = FALSE)

indicator <- "Table listing all indicators in the systems impact domain with rates and differences by regions such as Service Planning Areas (SPAs) and ID scores by indicator. Table includes rank based on difference and a rescaled rank avg rank to keep the number of ranks consistent between indicators for chart purposes. Using avg rank for heatmap purposes"
source <- "See script and QA doc for details W:/Project/OSI/Bold Vision/BV 2023/R/Region Domains.R and W:\\Project\\OSI\\Bold Vision\\BV 2023\\Documentation\\QA_Spa_level_heatmaps_and_ID_charts.docx"


#comment on table and columns
comment <- paste0("
                  COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".category IS 'indicator label based on metadata_indicator_labels the short label';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".spa IS 'Service Planning Area (SPA)';

                  COMMENT ON COLUMN ", schema, ".", table_name, ".diff IS 'Difference from the best for that indicator';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".rate IS 'Rate for that region and indicator with the unit of measure attached';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".index_of_disparity IS 'Overall ID score for the indicator';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".avg_rank IS 'Rank rescaled to keep consistent ranks between indicators even if number of groups varies. To be used for heatmap charting';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".rank IS 'Original rank with rank 1 meaning best rate';
                  ")
# dbSendQuery(conBV, comment)

#disconnect -----
dbDisconnect(conBV)
