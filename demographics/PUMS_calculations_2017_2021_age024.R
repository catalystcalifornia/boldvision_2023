# 3/2024
# This script is for calculating population estimates by race for our standard race/ethnicity groups using PUMS microdata for LA County Youth
# PUMS data year 2017-2021
# Includes calcs for nh_race groupings, swana, nhpi, aian, latino

##### ENVIRONMENT SET UP #####
# load packages
library(dplyr)
library(tidycensus)
library(stringr)
library(tidyverse)
library(data.table)
library(openxlsx)
library(RPostgreSQL)
library(dbplyr)
library(srvyr)
library(tidyr)
library(rpostgis)
library(here)
library(sf)
library(knitr)
library(tidycensus)
library(glue)
library(readxl)
library(janitor)

options(scipen = 100) # disable scientific notation

# connect to postgres database

source("W:\\RDA Team\\R\\credentials_source.R")

con <- connect_to_db("rda_shared_data")



##### PREP AND LOAD PUMS DATA #####

# Data Dictionary: W:\Data\Demographics\PUMS\CA_2017_2021\PUMS_Data_Dictionary_2017-2021.pdf 

# get crosswalk
crosswalk <- st_read(con, query = "select county_id AS geoid, county_name AS geoname, puma, num_county from crosswalks.puma_county_2021 where county_id = '06037'")

# PUMS Data
root <- "W:/Data/Demographics/PUMS/"

## Load the people PUMS data
ppl <- fread(paste0(root, "CA_2017_2021/psam_p06.csv"), header = TRUE, data.table = FALSE, 
             colClasses = list(character = c("PUMA", "ANC1P", "ANC2P", "HISP", "RAC1P", "RAC2P", "RAC3P", "RACAIAN", "RACPI", "RACNH", "AGEP")))

#  Prep data
## Add state_geoid to ppl, add state_geoid to PUMA id, so it aligns with crosswalks.puma_county_2021
ppl$state_geoid <- "06"
ppl$puma_id <- paste0(ppl$state_geoid, ppl$PUMA)

## create list of replicate weights
repwlist = rep(paste0("PWGTP", 1:80))

## join county crosswalk using right join function to only keep LA County
ppl <- right_join(ppl, crosswalk, by=c("puma_id" = "puma"))    # specify the field join

# save copy of unadulterated data prior to filtering
ppl_orig <- ppl
# ppl <- ppl_orig #use this line to refresh ppl to this point but keep it commented out in the meantime

##### AGE GROUP CLASSIFICATIONS #####
ppl<-ppl%>%mutate(age=as.numeric(AGEP))
ppl <- ppl%>%filter(age <= 24 & age >=0)  

###### Set up codes for SWANA ######
# First use ancestry codes to help identify estimated swana pop
## Create list of swana codes for PUMS
pums_swana_list<-list("Algerian","Arab","Assyrian","Bahraini","Berber","Chaldean","Egyptian","Emirati","Iranian","Iraqi","Israeli","Jordanian","Kurdish","Kuwaiti","Lebanese","Libyan","Middle Eastern","Moroccan","North African","Omani","Palestinian","Qatari","Saudi","Syriac","Syrian","Tunisian","Yazidi","Yemeni","Mideast","Saudi Arabian","Arabic","Other Arab","Libyan (2017 or later)","Kuwaiti (2017 or later)","Turkish","Sudanese","Afghan") # 2017 or later needed based on reviewing data dictionary and saw fields for Arabic and Other Arab

## import PUMS codes
pums_codes <- read_excel("W:/Data/Demographics/PUMS/CA_2017_2021/PUMS_Data_Dictionary_2017-2021_ANC1P.xlsx")%>%
  mutate_all(as.character) # create this excel document separate by opening PUMS Data Dictionary in excel and deleting everything but ancestry fields -- since ANC1P and ANC2P have same data values no need to do both
pums_codes <- pums_codes %>% dplyr::rename("ANC_Code" = "Code_1")

## filter PUMS codes for swana descriptions based on our swana_list
swana_codes<-pums_codes%>%filter(Description %in% pums_swana_list)

###### Set up additional codes for SSWANA or SWANA/SA ######
# Use detailed race fields (RAC2P and RAC3P) to identify south asian pop for sswana pop category
# create list of south asian categories
pums_sa_list<-list("Asian Indian", "Bangladeshi", "Bhutanese","Maldivian","Nepalese","Pakistani","Sikh","Sindhi","Sri Lankan") 

## import PUMS RAC2P codes
sa_codes_RAC2P <- read_excel("W:/Data/Demographics/PUMS/CA_2017_2021/PUMS_Data_Dictionary_2017-2021_RAC2P.xlsx")%>%
  mutate_all(as.character) # create this excel document separate by opening PUMS Data Dictionary in excel and deleting everything but RAC2P

### filter PUMS RAC2P codes for SA descriptions based on our sa_list
sa_codes_RAC2P<-sa_codes_RAC2P%>%dplyr::filter(grepl(paste(pums_sa_list,collapse="|"), Description, ignore.case = TRUE))
sa_codes_RAC2P$RAC_Code<-str_pad(sa_codes_RAC2P$Code_1, 2, pad = "0") # add padding to match to microdata later

View(sa_codes_RAC2P)

## import PUMS RAC3P codes, because RAC2P and RAC3P have different codes, we need to import both
sa_codes_RAC3P <- read_excel("W:/Data/Demographics/PUMS/CA_2017_2021/PUMS_Data_Dictionary_2017-2021_RAC3P.xlsx")%>%
  mutate_all(as.character) # created this excel document separate by opening PUMS Data Dictionary in excel and deleting everything but RAC3P
sa_codes_RAC3P$RAC_Code<-str_pad(sa_codes_RAC3P$Code_1, 3, pad = "0") # add padding to match to microdata later

### filter PUMS RAC3P codes for SA descriptions based on our sa_list
sa_codes_RAC3P<-sa_codes_RAC3P%>%dplyr::filter(grepl(paste(pums_sa_list,collapse="|"), Description, ignore.case = TRUE))

head(sa_codes_RAC3P)

##### RACE RECLASSIFY #####
# Recode race/eth
## latinx - alone or in combination with another race
ppl$latino <- "latino"
ppl$latino[ppl$HISP=="01"] <- "not latino"
ppl$latino <- as.factor(ppl$latino)

##aian - alone or in combination with another race or latino
ppl$aian <- "not aian"
ppl$aian[ppl$RACAIAN==1] <- "aian"
ppl$aian <- as.factor(ppl$aian)

##nhpi - alone or in combination with another race or latino
ppl$pacisl <- "not pacisl"
ppl$pacisl[ppl$RACPI==1 | ppl$RACNH==1] <- "pacisl"
ppl$pacisl <- as.factor(ppl$pacisl)

##swana - alone or in combination with another race or latino
ppl$swana <- "not swana"
ppl$swana[ppl$ANC1P%in% swana_codes$ANC_Code| ppl$ANC2P%in% swana_codes$ANC_Code] <- "swana"
ppl$swana <- as.factor(ppl$swana)

#sswana or swana/sa combo of swana and south asian - alone or in combination with another race or latino
ppl$sswana <- "not sswana"
ppl$sswana[ppl$ANC1P%in% swana_codes$ANC_Code | ppl$ANC2P%in% swana_codes$ANC_Code| ppl$RAC2P%in% sa_codes_RAC2P$RAC_Code | ppl$RAC3P%in% sa_codes_RAC3P$RAC_Code] <- "sswana"
ppl$sswana <- as.factor(ppl$sswana)

## nh_race groups
ppl$race = as.factor(ifelse(ppl$RAC1P == 1 & ppl$latino =="not latino", "nh_white",
                            ifelse(ppl$RAC1P == 1 & ppl$latino =="latino", "white",
                                   ifelse(ppl$RAC1P == 2 & ppl$latino =="not latino", "nh_black",
                                          ifelse(ppl$RAC1P == 2 & ppl$latino =="latino", "black",
                                                 ifelse(ppl$RAC1P == 6 & ppl$latino =="not latino", "nh_asian",
                                                        ifelse(ppl$RAC1P == 6 & ppl$latino =="latino", "asian",
                                                               ifelse(ppl$RAC1P == 8 & ppl$latino =="not latino", "nh_other", 
                                                                      ifelse(ppl$RAC1P == 8 & ppl$latino =="latino", "other",
                                                                             ifelse(ppl$RAC1P == 9 & ppl$latino =="not latino", "nh_twoormor",
                                                                                    ifelse(ppl$RAC1P == 9 & ppl$latino =="latino", "twoormor",
                                                                                           ifelse(ppl$RAC1P %in% c(3,4,5) & ppl$latino =="latino", "aian",
                                                                                                  ifelse(ppl$RAC1P %in% c(3,4,5) & ppl$latino =="not latino", "nh_aian",
                                                                                                         ifelse(ppl$RAC1P==7 & ppl$latino =="latino", "pacisl",
                                                                                                                ifelse(ppl$RAC1P==7 & ppl$latino =="not latino", "nh_pacisl",
                                                                                                                       
                                                                                                                       NA)))))))))))))))


## add BIPOC
##bipoc youth 
bipoc_list <- c("asian", "nh_asian","black" ,"nh_black", "twoormor","nh_twoormor", "aian", "nh_aian","pacisl", "nh_pacisl","nh_other", "other")

ppl$bipoc <- "not bipoc"
ppl$bipoc[ppl$race %in% bipoc_list] <- "bipoc"
ppl$bipoc[ppl$swana=="swana"] <- "bipoc"
ppl$bipoc[ppl$latino=="latino"] <- "bipoc"
ppl$bipoc <- as.factor(ppl$bipoc)

# Check that columns are added and populated correctly
# latino includes all races. AIAN is AIAN alone/combo latino/non-latino, NHPI is alone/combo latino/non-latino
View(ppl[c("HISP","latino","RAC1P","race","RAC2P","RAC3P","ANC1P","ANC2P","swana", "age","bipoc")])
table(ppl$race)
ppl%>%group_by(race,latino)%>%summarise(count=n())
check<-ppl%>%group_by(race,bipoc,swana,aian,pacisl,latino)%>%summarise(count=n())

##### PREP DATA FOR ESTIMATE CALCS #####

# Define weight variable and population base which will be used in the survey design set up
## You must use to WGTP (if you are using psam_h06.csv and want housing units, like for Low Quality Housing) or PWGTP (if you want person units, like for Connected Youth)
weight <- 'PWGTP' 
## Specify population base. Use 100 for percents, or 1000 for rate per 1k.
pop_base <- 100

##### COUNTY ESTIMATES #####

# prep data
c <- ppl %>% dplyr::select(-state_geoid) 
# c_0_24 <- c %>% filter(age_0_24 == '0-24')

# create survey design

ppl_county_age_0_24<- c %>%               
  as_survey_rep(
    variables = c(geoid, race, latino, aian, pacisl, swana,sswana,bipoc),   # dplyr::select grouping variables
    weights = weight,                       # person weight
    repweights = repwlist,                  # list of replicate weights
    combined_weights = TRUE,                # tells the function that replicate weights are included in the data
    mse = TRUE,                             # tells the function to calc mse
    type="other",                           # statistical method
    scale=4/80,                             # scaling set by ACS
    rscale=rep(1,80)                        # setting specific to ACS-scaling
  )


###### Latino ######
lat <- ppl_county_age_0_24 %>%
  group_by(geoid, latino) %>%   # group by race cat
  summarise(
    num = survey_total(na.rm=T), # get the (survey weighted) count for the numerators
    rate = survey_mean()) %>%        # get the (survey weighted) proportion for the numerator
  left_join(ppl_county_age_0_24 %>%                                        # left join in the denominators
              group_by(geoid) %>%                                     # group by geo
              summarise(pop = survey_total(na.rm=T))) %>%              # get the weighted total for overall geo
  mutate(rate=rate*100,
         rate_moe = rate_se*1.645*100,    # calculate the margin of error for the rate based on se
         rate_cv = ((rate_moe/1.645)/rate) * 100, # calculate cv for rate
         count_moe = num_se*1.645, # calculate moe for numerator count based on se
         count_cv = ((count_moe/1.645)/num) * 100)  # calculate cv for numerator count

head(lat)

###### AIAN ######
aian <- ppl_county_age_0_24 %>%
  group_by(geoid, aian) %>%   # group by race cat
  summarise(
    num = survey_total(na.rm=T), # get the (survey weighted) count for the numerators
    rate = survey_mean()) %>%        # get the (survey weighted) proportion for the numerator
  left_join(ppl_county_age_0_24 %>%                                        # left join in the denominators
              group_by(geoid) %>%                                     # group by geo
              summarise(pop = survey_total(na.rm=T))) %>%              # get the weighted total for overall geo
  mutate(rate=rate*100,
         rate_moe = rate_se*1.645*100,    # calculate the margin of error for the rate based on se
         rate_cv = ((rate_moe/1.645)/rate) * 100, # calculate cv for rate
         count_moe = num_se*1.645, # calculate moe for numerator count based on se
         count_cv = ((count_moe/1.645)/num) * 100)  # calculate cv for numerator count

head(aian)

###### NHPI ######
pacisl <- ppl_county_age_0_24 %>%
  group_by(geoid, pacisl) %>%   # group by race cat
  summarise(
    num = survey_total(na.rm=T), # get the (survey weighted) count for the numerators
    rate = survey_mean()) %>%        # get the (survey weighted) proportion for the numerator
  left_join(ppl_county_age_0_24 %>%                                        # left join in the denominators
              group_by(geoid) %>%                                     # group by geo
              summarise(pop = survey_total(na.rm=T))) %>%              # get the weighted total for overall geo
  mutate(rate=rate*100,
         rate_moe = rate_se*1.645*100,    # calculate the margin of error for the rate based on se
         rate_cv = ((rate_moe/1.645)/rate) * 100, # calculate cv for rate
         count_moe = num_se*1.645, # calculate moe for numerator count based on se
         count_cv = ((count_moe/1.645)/num) * 100)  # calculate cv for numerator count

head(pacisl)

###### SWANA ######
swana <- ppl_county_age_0_24 %>%
  group_by(geoid, swana) %>%   # group by race cat
  summarise(
    num = survey_total(na.rm=T), # get the (survey weighted) count for the numerators
    rate = survey_mean()) %>%        # get the (survey weighted) proportion for the numerator
  left_join(ppl_county_age_0_24 %>%                                        # left join in the denominators
              group_by(geoid) %>%                                     # group by geo
              summarise(pop = survey_total(na.rm=T))) %>%              # get the weighted total for overall geo
  mutate(rate=rate*100,
         rate_moe = rate_se*1.645*100,    # calculate the margin of error for the rate based on se
         rate_cv = ((rate_moe/1.645)/rate) * 100, # calculate cv for rate
         count_moe = num_se*1.645, # calculate moe for numerator count based on se
         count_cv = ((count_moe/1.645)/num) * 100)  # calculate cv for numerator count

head(swana)

###### SWANA/SA ######
sswana <- ppl_county_age_0_24 %>%
  group_by(geoid, sswana) %>%   # group by race cat
  summarise(
    num = survey_total(na.rm=T), # get the (survey weighted) count for the numerators
    rate = survey_mean()) %>%        # get the (survey weighted) proportion for the numerator
  left_join(ppl_county_age_0_24 %>%                                        # left join in the denominators
              group_by(geoid) %>%                                     # group by geo
              summarise(pop = survey_total(na.rm=T))) %>%              # get the weighted total for overall geo
  mutate(rate=rate*100,
         rate_moe = rate_se*1.645*100,    # calculate the margin of error for the rate based on se
         rate_cv = ((rate_moe/1.645)/rate) * 100, # calculate cv for rate
         count_moe = num_se*1.645, # calculate moe for numerator count based on se
         count_cv = ((count_moe/1.645)/num) * 100)  # calculate cv for numerator count

head(sswana)

###### BIPOC ######
bipoc <- ppl_county_age_0_24 %>%
  group_by(geoid, bipoc) %>%   # group by race cat
  summarise(
    num = survey_total(na.rm=T), # get the (survey weighted) count for the numerators
    rate = survey_mean()) %>%        # get the (survey weighted) proportion for the numerator
  left_join(ppl_county_age_0_24 %>%                                        # left join in the denominators
              group_by(geoid) %>%                                     # group by geo
              summarise(pop = survey_total(na.rm=T))) %>%              # get the weighted total for overall geo
  mutate(rate=rate*100,
         rate_moe = rate_se*1.645*100,    # calculate the margin of error for the rate based on se
         rate_cv = ((rate_moe/1.645)/rate) * 100, # calculate cv for rate
         count_moe = num_se*1.645, # calculate moe for numerator count based on se
         count_cv = ((count_moe/1.645)/num) * 100)  # calculate cv for numerator count

head(bipoc)

###### NH Race ######
race <- ppl_county_age_0_24 %>%
  group_by(geoid, race) %>%   # group by race cat
  summarise(
    num = survey_total(na.rm=T), # get the (survey weighted) count for the numerators
    rate = survey_mean()) %>%        # get the (survey weighted) proportion for the numerator
  left_join(ppl_county_age_0_24 %>%                                        # left join in the denominators
              group_by(geoid) %>%                                     # group by geo
              summarise(pop = survey_total(na.rm=T))) %>%              # get the weighted total for overall geo
  mutate(rate=rate*100,
         rate_moe = rate_se*1.645*100,    # calculate the margin of error for the rate based on se
         rate_cv = ((rate_moe/1.645)/rate) * 100, # calculate cv for rate
         count_moe = num_se*1.645, # calculate moe for numerator count based on se
         count_cv = ((count_moe/1.645)/num) * 100)  # calculate cv for numerator count

head(race)

###### Total ######
tot <- ppl_county_age_0_24 %>%
  group_by(geoid) %>%   # group by race cat
  summarise(
    num = survey_total(na.rm=T), # get the (survey weighted) count for the numerators
    rate = survey_mean()) %>%        # get the (survey weighted) proportion for the numerator
  left_join(ppl_county_age_0_24 %>%                                        # left join in the denominators
              group_by(geoid) %>%                                     # group by geo
              summarise(pop = survey_total(na.rm=T))) %>%              # get the weighted total for overall geo
  mutate(rate=rate*100,
         rate_moe = rate_se*1.645*100,    # calculate the margin of error for the rate based on se
         rate_cv = ((rate_moe/1.645)/rate) * 100, # calculate cv for rate
         count_moe = num_se*1.645, # calculate moe for numerator count based on se
         count_cv = ((count_moe/1.645)/num) * 100)  # calculate cv for numerator count

tot <- mutate(tot, "raceeth" = "total") # note I've added a column to assign race = total

head(tot)


###### Combine the data frames into one #####
race$raceeth <- as.character(race$race) # make a new column called race, convert factor to character
lat$raceeth <- as.character(lat$latino)
aian$raceeth <- as.character(aian$aian)
pacisl$raceeth <- as.character(pacisl$pacisl)
swana$raceeth <- as.character(swana$swana)
sswana$raceeth <- as.character(sswana$sswana)
bipoc$raceeth <- as.character(bipoc$bipoc)


# Combine race/eth estimates with total estimates
county <- 
  bind_rows(
    race %>% 
      dplyr::select(-race) %>% 
      dplyr::filter(!raceeth %in% c("white", "aian","pacisl","asian", "black", "other", "twoormor")),      # keep only non-latino race groups
    lat %>% 
      dplyr::select(-latino) %>% 
      dplyr::filter(raceeth =="latino"), 
    aian %>% 
      dplyr::select(-aian) %>% 
      dplyr::filter(raceeth =="aian"), 
    pacisl %>% 
      dplyr::select(-pacisl) %>% 
      dplyr::filter(raceeth =="pacisl"), 
    swana %>% 
      dplyr::select(-swana) %>% 
      dplyr::filter(raceeth =="swana"),
    sswana %>% 
      dplyr::select(-sswana) %>% 
      dplyr::filter(raceeth =="sswana"), 
    bipoc %>% 
      dplyr::select(-bipoc) %>% 
      dplyr::filter(raceeth =="bipoc"),
    tot)

# review combined table
head(county)

###### Prep final data frame #####
# Trim down columns
county_long <- 
  county %>% 
  mutate(pop_moe=pop_se*1.645, # add the moe and cv for population since county estimates can be unstable
         pop_cv=((pop_moe/1.645)/pop) * 100)%>%
  rename(count=num)%>%
  select(geoid,raceeth,count,count_moe,count_cv,rate,rate_moe,rate_cv,pop,pop_moe,pop_cv)%>%
  as.data.frame()

# convert long format to wide
county_wide <- county_long %>% 
  filter(raceeth!="total")%>%
  select(-c(pop,pop_moe,pop_cv))%>%
  # convert to wide format
  pivot_wider(id_cols = geoid,
              names_from = raceeth,
              names_glue="{raceeth}_{.value}",
              values_from = c("count", "count_moe", "count_cv","rate", "rate_moe", "rate_cv"))%>%
  left_join(county_long%>% 
              filter(raceeth=="total")%>%
              select(geoid,pop,pop_moe,pop_cv))

# add geoname and level
# #get census geonames ----------------------------------------------------
census_api_key(census_key1, overwrite=TRUE) # In practice, may need to include install=TRUE if switching between census api keys
Sys.getenv("CENSUS_API_KEY")
ca <- get_acs(geography = "county",
              variables = c("B01001_001"),
              state = "CA",
              year = 2021)

ca <- ca[,1:2]
names(ca) <- c("geoid", "geoname")
View(ca)

# add names and level to wide data
county_wide <- county_wide%>%
  mutate(geolevel='county') %>%
  left_join(ca)%>%
  relocate(geoname, .after = geoid)%>%
  relocate(geolevel,.after=geoid)%>%
  filter(!is.na(geoid))#drop NA geoid

# add names and level to long data
county_long <- county_long%>%
  mutate(geolevel='county') %>%
  left_join(ca,by="geoid")%>%
  relocate(geoname, .after = geoid)%>%
  relocate(geolevel,.after=geoid)%>%
  filter(!is.na(geoid)) #drop NA geoid



##### PUMA ESTIMATES #####

# prep data
ppl_puma <- ppl
p <- ppl_puma %>% dplyr::select(-geoid) %>% dplyr::rename(geoid = puma_id)   # drop county geoids (geoid), dplyr::rename puma to geoid

# create survey design
ppl_puma<- p %>%               
  as_survey_rep(
    variables = c(geoid, race, latino, aian, pacisl, swana, sswana,bipoc,age),   # dplyr::select grouping variables
    weights = weight,                       # person weight
    repweights = repwlist,                  # list of replicate weights
    combined_weights = TRUE,                # tells the function that replicate weights are included in the data
    mse = TRUE,                             # tells the function to calc mse
    type="other",                           # statistical method
    scale=4/80,                             # scaling set by ACS
    rscale=rep(1,80)                        # setting specific to ACS-scaling
  )

# create function
estimate_calc<-function(df,geoid,var){
  var_df <- df %>%rename(race_cat=var)%>%
    group_by(geoid, race_cat) %>%   # group by race cat
    summarise(
      num = survey_total(na.rm=T), # get the (survey weighted) count for the numerators
      rate = survey_mean()) %>%        # get the (survey weighted) proportion for the numerator
    left_join(df %>%                                        # left join in the denominators
                group_by(geoid) %>%                                     # group by geo
                summarise(pop = survey_total(na.rm=T))) %>%              # get the weighted total for overall geo
    mutate(rate=rate*100,
           rate_moe = rate_se*1.645*100,    # calculate the margin of error for the rate based on se
           rate_cv = ((rate_moe/1.645)/rate) * 100, # calculate cv for rate
           count_moe = num_se*1.645, # calculate moe for numerator count based on se
           count_cv = ((count_moe/1.645)/num) * 100)  # calculate cv for numerator count
  return(var_df)
}

latino<-estimate_calc(df=ppl_puma,geoid=geoid,var="latino")
aian<-estimate_calc(df=ppl_puma,geoid=geoid,var="aian")
pacisl<-estimate_calc(df=ppl_puma,geoid=geoid,var="pacisl")
swana<-estimate_calc(df=ppl_puma,geoid=geoid,var="swana")
sswana<-estimate_calc(df=ppl_puma,geoid=geoid,var="sswana")
bipoc<-estimate_calc(df=ppl_puma,geoid=geoid,var="bipoc")
race<-estimate_calc(df=ppl_puma,geoid=geoid,var="race")

###### Test estimates for latinx #####
# lat_pumas_check <- ppl_puma %>%
#   group_by(geoid, latino) %>%   # group by latino
#   summarise(
#     num = survey_total(na.rm=T), # get the (survey weighted) count for the numerators
#     rate = survey_mean()) %>%        # get the (survey weighted) proportion for the numerator
#   left_join(ppl_puma %>%                                        # left join in the denominators
#               group_by(geoid) %>%                                     # group by geo
#               summarise(pop = survey_total(na.rm=T))) %>%              # get the weighted total for overall geo
#   mutate(rate=rate*100,
#          rate_moe = rate_se*1.645*100,    # calculate the margin of error for the rate based on se
#          rate_cv = ((rate_moe/1.645)/rate) * 100, # calculate cv for rate
#          count_moe = num_se*1.645, # calculate moe for numerator count based on se
#          count_cv = ((count_moe/1.645)/num) * 100)  # calculate cv for numerator count


###### Total ######

tot <- ppl_puma %>%
  group_by(geoid) %>%   # group by race cat
  summarise(
    num = survey_total(na.rm=T))%>% # get the (survey weighted) count for the numerators - same as denominators here
  # need to add in other columns, wouldn't calculate the rate here for PUMAs probably due to division by 0 or something like that
  mutate(rate=100, # rate is 100 given it's total population
         rate_se= NULL, # se for rate would be null
         rate_moe = NULL,    # moe for rate would be null
         rate_cv = NULL , # cv for rate would be null
         count_moe = num_se*1.645, # calculate moe for numerator count based on se
         count_cv = ((count_moe/1.645)/num) * 100,
         pop = num,      # pop and numerator are same here     
         pop_se=num_se # as is se for num and pop
  )

tot <- mutate(tot, "raceeth" = "total") # note I've added a column to assign race = total

head(tot)


###### Combine the data frames into one #####
race$raceeth <- as.character(race$race_cat) # make a new column called race, convert factor to character
latino$raceeth <- as.character(latino$race_cat)
aian$raceeth <- as.character(aian$race_cat)
pacisl$raceeth <- as.character(pacisl$race_cat)
swana$raceeth <- as.character(swana$race_cat)
sswana$raceeth <- as.character(sswana$race_cat)
bipoc$raceeth <- as.character(bipoc$race_cat)

# Combine race/eth estimates with total estimates
puma <- 
  bind_rows(
    race %>% 
      dplyr::select(-race_cat) %>% 
      dplyr::filter(!raceeth %in% c("white", "aian","pacisl","asian", "black", "other", "twoormor")),      # keep only non-latino race groups
    latino %>% 
      dplyr::select(-race_cat) %>% 
      dplyr::filter(raceeth =="latino"), 
    aian %>% 
      dplyr::select(-race_cat) %>% 
      dplyr::filter(raceeth =="aian"), 
    pacisl %>% 
      dplyr::select(-race_cat) %>% 
      dplyr::filter(raceeth =="pacisl"), 
    swana %>% 
      dplyr::select(-race_cat) %>% 
      dplyr::filter(raceeth =="swana"),
    sswana %>% 
      dplyr::select(-race_cat) %>% 
      dplyr::filter(raceeth =="sswana"), 
    bipoc %>% 
      dplyr::select(-race_cat) %>% 
      dplyr::filter(raceeth =="bipoc"),
    tot)

# review combined table
head(puma)

###### Prep final data frame #####
# Trim down columns
puma_long_1 <- 
  puma %>% 
  mutate(pop_moe=pop_se*1.645, # add the moe and cv for population since puma estimates can be unstable
         pop_cv=((pop_moe/1.645)/pop) * 100)%>%
  rename(count=num)%>%
  select(geoid,raceeth,count,count_moe,count_cv,rate,rate_moe,rate_cv,pop,pop_moe,pop_cv)%>%
  as.data.frame()

# check that there are values for all groups across PUMAs
qa<-puma_long_1%>%group_by(raceeth)%>%summarise(count=n())
# there are some instances where there are 0 nh_pacisl, nh_aian, swana, and/or sswana in a PUMA
# let's make sure these get counted as 0 in count and rate

# create a data frame with unique combos of race groups and pumas
race_groups<-data.frame("raceeth"=unique(qa$raceeth)) # this is every race group there should be
length(unique(puma$geoid)) # add unique pumas
puma_unique<-puma_long_1%>%filter(raceeth=="total")%>%select(geoid,pop,pop_moe,pop_cv) # this is every PUMA and their total youth pop
main_frame<-merge(race_groups,puma_unique,all=T)
check<-main_frame%>%group_by(raceeth)%>%summarise(count=n()) # checks out
check<-main_frame%>%group_by(geoid)%>%summarise(count=n()) # checks out

# join data to main frame without pop columns
puma_long<-main_frame%>%left_join(puma_long_1%>%select(geoid,raceeth,count,count_moe,count_cv,rate,rate_moe,rate_cv), by=c("geoid","raceeth"))

# add zeros to counts and rates where applicable
puma_long <- puma_long %>% mutate(count = ifelse(is.na(count), 0, count))
puma_long <- puma_long %>% mutate(rate = ifelse(is.na(rate), 0, rate))
qa<-puma_long%>%group_by(raceeth)%>%summarise(count=n()) # checks out


# convert long format to wide
puma_wide <- puma_long %>% 
  filter(raceeth!="total")%>%
  select(-c(pop,pop_moe,pop_cv))%>%
  # convert to wide format
  pivot_wider(id_cols = geoid,
              names_from = raceeth,
              names_glue="{raceeth}_{.value}",
              values_from = c("count", "count_moe", "count_cv","rate", "rate_moe", "rate_cv"))%>%
  left_join(puma_long%>% 
              filter(raceeth=="total")%>%
              select(geoid,pop,pop_moe,pop_cv))

# add geoname and level
# #get census geonames ----------------------------------------------------
census_api_key(census_key1, overwrite=TRUE) # In practice, may need to include install=TRUE if switching between census api keys
Sys.getenv("CENSUS_API_KEY")
ca <- get_acs(geography = "puma",
              variables = c("B01001_001"),
              state = "CA",
              year = 2021)

ca <- ca[,1:2]
names(ca) <- c("geoid", "geoname")
View(ca)

# add names and level to wide data
puma_wide <- puma_wide%>%
  mutate(geolevel='puma') %>%
  left_join(ca)%>%
  relocate(geoname, .after = geoid)%>%
  relocate(geolevel,.after=geoid)%>%
  filter(!is.na(geoid))#drop NA geoid

# add names and level to long data
puma_long <- puma_long%>%
  mutate(geolevel='puma') %>%
  left_join(ca,by="geoid")%>%
  relocate(geoname, .after = geoid)%>%
  relocate(geolevel,.after=geoid)%>%
  filter(!is.na(geoid)) #drop NA geoid


##### COMBINE ESTIMATES & SEND TO POSTGRES #####
# COMBINE COUNTY/STATE/PUMS DATA
long <- rbind(county_long,puma_long)

wide <- rbind(county_wide,puma_wide)

# export to postgres 
drv <- dbDriver("PostgreSQL")

con_bv <- connect_to_db("bold_vision")

dbWriteTable(con_bv, c("bv_2023", "acs_pums_multigeo_2021_youth_0_24_race_long"),long, 
             overwrite = FALSE, row.names = FALSE)

###NEED TO RUN ThE BOTTOM###
#comment on table and columns
table_comment <- paste0("COMMENT ON TABLE  bv_2023.acs_pums_multigeo_2021_youth_0_24_race_long  IS 'Non-Latinx and Latinx race counts by PUMA, County, and only for those Ages 0-24, including counts for all-AIAN, all-NHPI, all-SWANA, and all-SWANA/SA (SSWANA).
  Latinx, non-Latinx, AIAN, NHPI counts are based on race ethnicity fields in the census. SWANA is estimated based on reported ancestry fields. SWANA/SA is estimated based on reported ancestry fields and detailed race/ethnicity for south asian groups.
  See script and QA doc for details.
See W:/Project/OSI/Bold Vision/BV 2022-2023/R/boldvision_22_23/PUMS_calculations_2017_2021_age024.R W:/Project/OSI/Bold Vision/BV 2022-2023/Documentation/QA_PUMS_race_popcalc.docx'; 

COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_long.geoid IS 'GEOID for PUMA, County, or state';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_long.geolevel IS 'Level of geography for which data are for - puma, county or state';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_long.geoname IS 'Census name for geography';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_long.raceeth IS 'Race/ethnicity category nh_ means non-hispanic grouping. AIAN, NHPI, SWANA, and SWANA/SA (SSWANA) are alone or in combo with another race or Latinx.Includes row for total';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_long.count IS 'Count estimate for race/ethnicity category';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_long.count_moe IS 'Margin of error at 90% for count estimates';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_long.count_cv IS 'Coefficient of variation for count estimate';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_long.rate IS 'Estimated percent for race/ethnicity category out of total population in geography';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_long.rate_moe IS 'Margin of error at 90% for percent estimate';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_long.rate_cv IS 'Coefficient of variation for percent estimate';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_long.pop IS 'Total population estimate for geography';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_long.pop_moe IS 'Margin of error for total population estimate';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_long.pop_cv IS 'Coefficient of variation for population estimate';
                            ")

# send table comment + column metadata
dbSendQuery(conn = con_bv, table_comment)



dbWriteTable(con_bv, c("bv_2023", "acs_pums_multigeo_2021_youth_0_24_race_wide"),wide, 
             overwrite = FALSE, row.names = FALSE)

table_comment_wide <- paste0("COMMENT ON TABLE bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide  IS 'Non-Latinx and Latinx race counts by PUMA, County, and only for those Ages 0-24, including counts for all-AIAN, all-NHPI, all-SWANA, and all-SWANA/SA (SSWANA).
  Latinx, non-Latinx, AIAN, NHPI counts are based on race ethnicity fields in the census. SWANA is estimated based on reported ancestry fields. SWANA/SA is estimated based on reported ancestry fields and detailed race/ethnicity for south asian groups.
  See script and QA doc for details.
See W:/Project/OSI/Bold Vision/BV 2022-2023/R/boldvision_22_23/PUMS_calculations_2017_2021_age024.R W:/Project/OSI/Bold Vision/BV 2022-2023/Documentation/QA_PUMS_race_popcalc.docx';
                               
  COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.geoid IS 'GEOID for PUMA, County, or state';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.geolevel IS 'Level of geography for which data are for - puma, county or state';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.geoname IS 'Census name for geography';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.nh_aian_count IS 'Count estimate for NH AIAN';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.nh_asian_count IS 'Count estimate for NH Asian';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.nh_black_count IS 'Count estimate for NH Black';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.nh_other_count IS 'Count estimate for NH Other';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.nh_pacisl_count IS 'Count estimate for NH NHPI';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.nh_twoormor_count IS 'Count estimate for NH Two or More';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.nh_white_count IS 'Count estimate for NH White';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.latino_count IS 'Count estimate for Latino';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.aian_count IS 'Count estimate for AIAN alone or in combo';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.pacisl_count IS 'Count estimate for NHPI alone or in combo';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.swana_count IS 'Count estimate for SWANA alone or in combo';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.nh_aian_count_moe IS 'Margin of error at 90% for count estimate for NH AIAN';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.nh_asian_count_moe IS 'Margin of error at 90% for count estimate for NH Asian';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.nh_black_count_moe IS 'Margin of error at 90% for count estimate for NH Black';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.nh_other_count_moe IS 'Margin of error at 90% for count estimate for NH Other';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.nh_pacisl_count_moe IS 'Margin of error at 90% for count estimate for NH NHPI';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.nh_twoormor_count_moe IS 'Margin of error at 90% for count estimate for NH Two or More';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.nh_white_count_moe IS 'Margin of error at 90% for count estimate for NH White';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.latino_count_moe IS 'Margin of error at 90% for count estimate for Latino';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.aian_count_moe IS 'Margin of error at 90% for count estimate for AIAN alone or in combo';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.pacisl_count_moe IS 'Margin of error at 90% for count estimate for NHPI alone or in combo';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.swana_count_moe IS 'Margin of error at 90% for count estimate for SWANA alone or in combo';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.nh_aian_count_cv IS 'Coefficient of variation for count estimate for NH AIAN';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.nh_asian_count_cv IS 'Coefficient of variation for count estimate for NH Asian';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.nh_black_count_cv IS 'Coefficient of variation for count estimate for NH Black';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.nh_other_count_cv IS 'Coefficient of variation for count estimate for NH Other';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.nh_pacisl_count_cv IS 'Coefficient of variation for count estimate for NH NHPI';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.nh_twoormor_count_cv IS 'Coefficient of variation for count estimate for NH Two or More';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.nh_white_count_cv IS 'Coefficient of variation for count estimate for NH White';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.latino_count_cv IS 'Coefficient of variation for count estimate for Latino';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.aian_count_cv IS 'Coefficient of variation for count estimate for AIAN alone or in combo';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.pacisl_count_cv IS 'Coefficient of variation for count estimate for NHPI alone or in combo';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.swana_count_cv IS 'Coefficient of variation for count estimate for SWANA alone or in combo';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.nh_aian_rate IS 'Estimated percent for NH AIAN out of total population';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.nh_asian_rate IS 'Estimated percent for NH Asian out of total population';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.nh_black_rate IS 'Estimated percent for NH Black out of total population';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.nh_other_rate IS 'Estimated percent for NH Other out of total population';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.nh_pacisl_rate IS 'Estimated percent for NH NHPI out of total population';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.nh_twoormor_rate IS 'Estimated percent for NH Two or More out of total population';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.nh_white_rate IS 'Estimated percent for NH White out of total population';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.latino_rate IS 'Estimated percent for Latino out of total population';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.aian_rate IS 'Estimated percent for AIAN alone or in combo out of total population';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.pacisl_rate IS 'Estimated percent for NHPI alone or in combo out of total population';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.swana_rate IS 'Estimated percent for SWANA alone or in combo out of total population';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.nh_aian_rate_moe IS 'Margin of error at 90% for percent estimate for NH AIAN';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.nh_asian_rate_moe IS 'Margin of error at 90% for percent estimate for NH Asian';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.nh_black_rate_moe IS 'Margin of error at 90% for percent estimate for NH Black';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.nh_other_rate_moe IS 'Margin of error at 90% for percent estimate for NH Other';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.nh_pacisl_rate_moe IS 'Margin of error at 90% for percent estimate for NH NHPI';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.nh_twoormor_rate_moe IS 'Margin of error at 90% for percent estimate for NH Two or More';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.nh_white_rate_moe IS 'Margin of error at 90% for percent estimate for NH White';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.latino_rate_moe IS 'Margin of error at 90% for percent estimate for Latino';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.aian_rate_moe IS 'Margin of error at 90% for percent estimate for AIAN alone or in combo';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.pacisl_rate_moe IS 'Margin of error at 90% for percent estimate for NHPI alone or in combo';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.swana_rate_moe IS 'Margin of error at 90% for percent estimate for SWANA alone or in combo';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.nh_aian_rate_cv IS 'Coefficient of variation for percent estimate for NH AIAN';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.nh_asian_rate_cv IS 'Coefficient of variation for percent estimate for NH Asian';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.nh_black_rate_cv IS 'Coefficient of variation for percent estimate for NH Black';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.nh_other_rate_cv IS 'Coefficient of variation for percent estimate for NH Other';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.nh_pacisl_rate_cv IS 'Coefficient of variation for percent estimate for NH NHPI';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.nh_twoormor_rate_cv IS 'Coefficient of variation for percent estimate for NH Two or More';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.nh_white_rate_cv IS 'Coefficient of variation for percent estimate for NH White';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.latino_rate_cv IS 'Coefficient of variation for percent estimate for Latino';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.aian_rate_cv IS 'Coefficient of variation for percent estimate for AIAN alone or in combo';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.pacisl_rate_cv IS 'Coefficient of variation for percent estimate for NHPI alone or in combo';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.swana_rate_cv IS 'Coefficient of variation for percent estimate for SWANA alone or in combo';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.pop IS 'Population count';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.pop_moe IS 'Margin of error for population';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.pop_cv IS 'Coefficient of variation for population';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.sswana_count IS 'Count estimate for SWANA/SA alone or in combo';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.sswana_rate IS 'Estimated percent for SWANA/SA alone or in combo out of total population';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.sswana_rate_moe IS 'Margin of error at 90% for percent estimate for SWANA/SA alone or in combo';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.sswana_rate_cv IS 'Coefficient of variation for percent estimate for SWANA/SA alone or in combo';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.sswana_count_cv IS 'Coefficient of variation for count estimate for SWANA/SA alone or in combo';
COMMENT ON COLUMN bv_2023.acs_pums_multigeo_2021_youth_0_24_race_wide.sswana_count_moe IS 'Margin of error at 90% for count estimate for SWANA/SA alone or in combo';
                               
                               ")

dbSendQuery(conn = con_bv, table_comment_wide)


dbDisconnect(con)
dbDisconnect(con_bv)