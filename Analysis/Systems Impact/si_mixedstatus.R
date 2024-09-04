# Calculate rate of mixed status of families for youth 0-24, and by race, BIPOC, and SPA
# based on PUMA-level data for LA County
# race categories included in the data are:
## BIPOC--all groups minus nh_white
## nh-White
## nh-Black
## nh-Asian
## Latino
## nh-Other
## Summing Non-Hispanic White, Latino, African American or Black, Asian American, and "All other groups" will give you the actual total for the region (LA County/"All PUMAs", or the specific PUMA), equal to "All".

#### ENVIRONMENT SET UP ----
library(data.table)
library(readxl)
library(dplyr)
library(RPostgreSQL)
library(sf)
library(tidyr)
library(stringr)
options(scipen=999)


source("W:\\RDA Team\\R\\credentials_source.R")
bv_con <- connect_to_db("bold_vision")

#### Get indicator raw data  and push to postgres data ----
df_original <-  read_excel("W:/Project/OSI/Bold Vision/BV 2023/Data/Systems Impact/ERI/Catalyst_CA_Mixed_Status_Fam_FINAL_20231219.xlsx",sheet="Catalyst_CA_Mixed_Status_Fam_w_")

##### Clean up dataframe -----
# rename columns
df_postgres<-df_original%>%
  rename(stateid=statefip,
         geoname=PUMANAME,
         race_long=raceth02,
         pop_raw=U_undoc_or_undoc_in_fam_lt25_UNWTD,
         mixedstatus_raw=N_undoc_or_undoc_in_fam_lt25_UNWTD,
         pop=U_undoc_or_undoc_in_fam_lt25,
         mixedstatus_count=N_undoc_or_undoc_in_fam_lt25,
         mixedstatus_rate=P_undoc_or_undoc_in_fam_lt25
  )

# add and cleanup columns according to naming conventions
df_postgres<-df_postgres%>%
  mutate(geoid=ifelse(puma!=0,paste0("060",puma),"06037"),
         stateid="06",
         geolevel=ifelse(geoid=="06037","county","puma"),
         geoname=ifelse(geoname=="Los Angeles County (All PUMAs)","Los Angeles County",geoname),
         race=ifelse(race_long=="African-American or black","nh_black",
                     ifelse(race_long=="All","total",
                            ifelse(race_long=="Asian American","nh_asian",
                                   ifelse(race_long=="Latino","latino",
                                          ifelse(race_long=="BIPOC","bipoc",
                                                 ifelse(race_long=="Non-Hispanic White", "nh_white",
                                                        "nh_other")))))))%>%
  select(stateid,geoid,geolevel,geoname,race,4:9)

##### Push data to postgres -----
table_name <- "eri_acs_mixed_status_youth_2021"
schema <- 'bv_2023'
d_pg<-df_postgres

indicator <- "USC Equity Research Institute analysis of 2021 5-year American Community Survey microdata from IPUMS USA and the 2014 Survey of Income and Program Participation. Data provided by request for Bold Vision for LA County and all PUMAS in LA County. Based on modeling methodology developed by ERI. See more here: https://immigrantdataca.org/about/methodology/demographics#mixed-status-families. Universe is Individuals in Los Angeles County under age 25."
source <- "Original data saved here: W:\\Project\\OSI\\Bold Vision\\BV 2023\\Data\\Systems Impact\\ERI\\Catalyst_CA_Mixed_Status_Fam_FINAL_20231219.xlsx"

# dbWriteTable(bv_con, c(schema, table_name), d_pg,
#              overwrite = TRUE, row.names = FALSE)

#comment on table and columns
comment <- paste0("COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " ", source, ".';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".stateid IS 'State FIPS code';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".geoid IS 'County or PUMA FIPS code';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".geoname IS 'County or PUMA Name';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".race IS 'Race category abbreviated all categories are Latinx exclusive with the exception of Latinx. Other includes AIAN, NHPI, other and multiracial. BIPOC includes all groups except nh_white';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".race_long IS 'Original race description provided by ERI';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".pop_raw IS 'Unweighted universe (denominator) - Youth under 25.';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".mixedstatus_raw IS 'Unweighted sample (numerator) - Youth under 25 who are undocumented OR living in families with undocumented family members.';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".pop IS 'Weighted universe (denominator) - Youth under 25.';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".mixedstatus_count IS 'Number of youth under 25 who are undocumented OR living in families with undocumented family members (numerator)';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".mixedstatus_rate IS 'Percentage of youth under 25 who are undocumented OR living in families with undocumented family members (equivalent to mixed_status_count / pop)';")

# dbSendQuery(bv_con, comment)

#### Part 1: Calculate rates at county level by youth race -----
df <- st_read(bv_con, query = "select * from bv_2023.eri_acs_mixed_status_youth_2021")

###### Calculate index of disparity -----
d <- df %>% rename(rate = mixedstatus_rate, count = mixedstatus_count)%>%filter(geoname=="Los Angeles County")%>%
  mutate(rate=rate*100)

county_id <-
  d %>%
  mutate(best=min(rate[race!='total' & race!='bipoc'],na.rm=T),asbest="min",#create columns for best rate and as best treatment, ignoring total and bipoc
         values_count = sum(!is.na(race) & race!='total' & race!="bipoc")) %>% # remove total and bipoc from values count
  mutate(diff = ifelse(race == "total" | race == "bipoc", NA, abs(best-rate))) %>%
  mutate(index_of_disparity = ((sum(diff, na.rm=T)/best/(values_count - 1))*100)) %>% # use the best value identified above
  #select just the columns needed 
  select(geoid,race,race_long,count,pop,rate,asbest,values_count,best,diff,index_of_disparity)%>%
  rename(subgroup=race,
         subgroup_detail=race_long)

###### Send to postgres -----
table_name <- "si_mixedstatus_subgroup"
schema <- 'bv_2023'
d_pg<-county_id

indicator <- "Percentage of youth under 25 who are undocumented OR living in families with undocumented family members by race in LA County. Includes rates for total population and BIPOC youth. Data provided by ERI based on modeling methodology developed by ERI. See more here: https://immigrantdataca.org/about/methodology/demographics#mixed-status-families.
Note race categories are different than standard race groups used by Catalyst. Specifically Other includes AIAN, NHPI, Other and Multiracial due to small sample size"
source <- "See R Script for details: W:/Project/OSI/Bold Vision/BV 2023/R/Systems Impact/si_mixedstatus.R"

# dbWriteTable(bv_con, c(schema, table_name), d_pg,
#              overwrite = TRUE, row.names = FALSE)

#comment on table and columns
comment <- paste0("COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".geoid IS 'County ID';
                   COMMENT ON COLUMN ", schema, ".", table_name, ".subgroup IS 'race subgroup including bipoc and total. Note nh_other includes AIAN, NHPI, Other, and multiracial';
                   COMMENT ON COLUMN ", schema, ".", table_name, ".subgroup_detail IS 'Further detail about race groupings provided by ERI';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".rate IS 'indicator rate for each subgroup- Percentage of youth under 25 who are undocumented OR living in families with undocumented family members';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".pop IS 'Youth population under 25 for each racial group';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".asbest IS 'minimum or maximum as best';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".best IS 'best rate among subgroups excluding BIPOC and total';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".diff IS 'difference from the best';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".index_of_disparity IS 'index of disparity';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".values_count IS 'number of values';")
# print(comment)
# dbSendQuery(bv_con, comment)

#### Part 2: Calculate rates by SPA-----
###### Join indicator data to SPA crosswalk -----
xwalk <- st_read(bv_con, query = "select * from bv_2023.crosswalk_puma_spas_2023")

d <- df %>% 
  rename(rate = mixedstatus_rate, count = mixedstatus_count)%>%
  filter(geolevel=="puma" & race=="total") # select just total rows for each PUMA

spa_id <- left_join(d, xwalk, by=c("geoid"="puma_id") )%>%  # join to PUMA - SPA xwalk
  group_by(spa_id,spa_name)%>%
  summarize(count=sum(count*prc_puma_area), # allocate counts and pops proportionately based on prc puma intersect
            pop=sum(pop*prc_puma_area),
            rate=sum(count*prc_puma_area)/sum(pop*prc_puma_area)*100)%>%
  as.data.frame()

###### Calculate index of disparity -----
asbest <- "min"
spa_id$asbest <- "min"

best <- min(spa_id$rate, na.rm=T)
spa_id$best <- best

spa_id$diff <- abs(spa_id$rate - best)
values_count <- length(spa_id$rate) - sum(is.na(spa_id$rate))

sumdiff <- sum(spa_id$diff, na.rm = TRUE)
index_of_disparity <- (sumdiff / best) / (values_count - 1) * 100

spa_id$index_of_disparity <- index_of_disparity
spa_id$values_count <- values_count


###### Send to postgres -----
table_name <- "si_mixedstatus_region"
schema <- 'bv_2023'
d_pg<-spa_id

indicator <- "Percentage of youth under 25 who are undocumented OR living in families with undocumented family members by SPA in LA County. Includes rates for total population by SPA. Data provided by ERI based on modeling methodology developed by ERI. See more here: https://immigrantdataca.org/about/methodology/demographics#mixed-status-families.
Data originally at the PUMA level. PUMAs are joined to SPAs based on prc_intersect"
source <- "See R Script for details: W:/Project/OSI/Bold Vision/BV 2023/R/Systems Impact/si_mixedstatus.R"

# dbWriteTable(bv_con, c(schema, table_name), d_pg,
#              overwrite = TRUE, row.names = FALSE)

#comment on table and columns
comment <- paste0("COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".spa_id IS 'SPA ID';
                   COMMENT ON COLUMN ", schema, ".", table_name, ".spa_name IS 'SPA Name';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".rate IS 'indicator rate for each SPA- Percentage of youth under 25 who are undocumented OR living in families with undocumented family members';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".pop IS 'Youth population under 25 for each SPA';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".asbest IS 'minimum as best';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".best IS 'best rate among regions';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".diff IS 'difference from the best';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".index_of_disparity IS 'index of disparity';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".values_count IS 'number of values';")
# print(comment)
# dbSendQuery(bv_con, comment)

#disconnect
dbDisconnect(bv_con)