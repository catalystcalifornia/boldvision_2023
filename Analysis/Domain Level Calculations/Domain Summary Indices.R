# Script creates domain level index of disparity scores by region and subgroup

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

##### Subgroups #####

yp <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.yp_subgroup_domain")
hbe <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.hbe_subgroup_domain")
pyd <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.pyd_subgroup_domain")
si <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.si_subgroup_domain")

yp_id<-yp%>%distinct(category,index_of_disparity)%>%
  mutate(domain="Youth Power")%>%group_by(domain)%>%summarize(domain_id=mean(index_of_disparity),
                                                              indicator_max= category[which.max(index_of_disparity)])

hbe_id<-hbe%>%distinct(category,index_of_disparity)%>%
  mutate(domain="Healthy Built Environment")%>%group_by(domain)%>%summarize(domain_id=mean(index_of_disparity),
                                                                            indicator_max= category[which.max(index_of_disparity)])

si_id<-si%>%distinct(category,index_of_disparity)%>%
  mutate(domain="Systems Impact")%>%group_by(domain)%>%summarize(domain_id=mean(index_of_disparity),
                                                                 indicator_max= category[which.max(index_of_disparity)])

pyd_id<-pyd%>%distinct(category,index_of_disparity)%>%
  mutate(domain="Positive Youth Development")%>%group_by(domain)%>%summarize(domain_id=mean(index_of_disparity),
                                                                             indicator_max= category[which.max(index_of_disparity)])

subgroup_id<-rbind(yp_id,hbe_id,si_id,pyd_id)%>%mutate(level="subgroup")


#### Send to Postgres ####
table_name <- "domain_subgroup_id"
schema <- 'bv_2023'
# dbWriteTable(conBV, c(schema, table_name), subgroup_id, overwrite = TRUE, row.names = FALSE)

indicator <- "Domain level index of disparity scores for racial disparities. Includes average ID score for every domain and the indicator with the highest ID score"
source <- "See script for details W:/Project/OSI/Bold Vision/BV 2023/R/Domain Indices.R"


#comment on table and columns
comment <- paste0("
                  COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".domain IS 'Domain';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".domain_id IS 'Index of disparity score for the domains average racial disparity';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".indicator_max IS 'Indicator with the highest ID score';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".level IS 'subgroup or region summary';
                  ")
dbSendQuery(conBV, comment)


##### Regions #####

yp <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.yp_region_domain")
hbe <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.hbe_region_domain")
pyd <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.pyd_region_domain")
si <-  dbGetQuery(conBV, "SELECT * FROM bv_2023.si_region_domain")

yp_id<-yp%>%distinct(category,index_of_disparity)%>%filter(!category %in% c('Political Engagement and Advocacy','Voter Turnout'))%>%
  mutate(domain="Youth Power")%>%group_by(domain)%>%summarize(domain_id=mean(index_of_disparity),
                                                              indicator_max= category[which.max(index_of_disparity)])

hbe_id<-hbe%>%distinct(category,index_of_disparity)%>%
  mutate(domain="Healthy Built Environment")%>%group_by(domain)%>%summarize(domain_id=mean(index_of_disparity),
                                                                            indicator_max= category[which.max(index_of_disparity)])

si_id<-si%>%distinct(category,index_of_disparity)%>%
  mutate(domain="Systems Impact")%>%group_by(domain)%>%summarize(domain_id=mean(index_of_disparity),
                                                                 indicator_max= category[which.max(index_of_disparity)])

pyd_id<-pyd%>%distinct(category,index_of_disparity)%>%
  mutate(domain="Positive Youth Development")%>%group_by(domain)%>%summarize(domain_id=mean(index_of_disparity),
                                                                             indicator_max= category[which.max(index_of_disparity)])

region_id<-rbind(yp_id,hbe_id,si_id,pyd_id)%>%mutate(level="region")


#### Send to Postgres ####
table_name <- "domain_region_id"
schema <- 'bv_2023'
# dbWriteTable(conBV, c(schema, table_name), region_id, overwrite = TRUE, row.names = FALSE)

indicator <- "Domain level index of disparity scores for regional disparities. Includes average ID score for every domain and the indicator with the highest ID score"
source <- "See script for details W:/Project/OSI/Bold Vision/BV 2023/R/Domain Indices.R"


#comment on table and columns
comment <- paste0("
                  COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".domain IS 'Domain';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".domain_id IS 'Index of disparity score for the domains average regional disparity';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".indicator_max IS 'Indicator with the highest ID score';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".level IS 'subgroup or region summary';
                  ")
dbSendQuery(conBV, comment)

#disconnect -----
dbDisconnect(conBV)