# BV Domain: Youth Power
# Indicator: Youth earning a living wage

####Step 0: Setting Up Libraries and Connecting to Databases/Functions ####

# install.packages('ggchicklet',repos = 'https://cinc.rud.is')


#Load libraries
library(data.table)
library(stringr)
library(dplyr)
library(RPostgreSQL)
library(dbplyr)
library(srvyr)
library(tidycensus)
library(tidyr)
library(rpostgis)
library(tidyr)
library(here)
library(sf)
library(readxl)
library(ggchicklet)

options(scipen = 100) # disable scientific notation

#For Visual Functions in Steps D
source("W:\\Project\\OSI\\Bold Vision\\BV 2023\\R\\boldvision_22_23\\bv_visuals_functions.R")

# create connection for bv database
source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("bold_vision")

con_rda <- connect_to_db("rda_shared_data")

root <- "W:/Data/Demographics/PUMS/"

# Load the people PUMS data with living_wage Youth Data 
dat <- fread(paste0(root, "CA_2017_2021/psam_p06.csv"), header = TRUE, data.table = FALSE,  
             colClasses = list(character = c("PUMA", "ANC1P", "ANC2P", "HISP", "RAC1P", "RAC2P", "RAC3P", "RACAIAN", "RACPI", "RACNH", "AGEP")))
#### Step 0: Recoding for Age Limit, Race, Etc####

dat$county <-  as.factor(paste0("06", substr(dat$PUMA, 1, 3)))

# create list of replicate weights
repwlist = rep(paste0("PWGTP", 1:80))

ppl <- 
  dat %>%
  select(SERIALNO, PUMA, ST, county, AGEP, HISP, RAC1P, RAC2P, RAC3P, ANC1P, ANC2P, RACAIAN, RACNH, RACPI, 
         ESR, WAGP, ADJINC, PWGTP, WRK, COW, WKHP, WKW, WKWN, all_of(repwlist)) %>%
  filter((AGEP >= 14 & AGEP <= 24 & AGEP != 2) & county == "06037") %>%
  collect() %>%
  as.data.frame()

###### Set up codes for SWANA ##
# First use ancestry codes to help identify estimated swana pop
## Create list of swana codes for PUMS
pums_swana_list<-list("Algerian","Arab","Assyrian","Bahraini","Berber","Chaldean","Egyptian","Emirati","Iranian","Iraqi","Israeli","Jordanian","Kurdish","Kuwaiti","Lebanese","Libyan","Middle Eastern","Moroccan","North African","Omani","Palestinian","Qatari","Saudi","Syriac","Syrian","Tunisian","Yazidi","Yemeni","Mideast","Saudi Arabian","Arabic","Other Arab","Libyan (2017 or later)","Kuwaiti (2017 or later)","Turkish","Sudanese","Afghan") # 2017 or later needed based on reviewing data dictionary and saw fields for Arabic and Other Arab

## import PUMS codes
pums_codes <- read_excel("W:/Data/Demographics/PUMS/CA_2017_2021/PUMS_Data_Dictionary_2017-2021_ANC1P.xlsx")%>%
  mutate_all(as.character) # create this excel document separate by opening PUMS Data Dictionary in excel and deleting everything but ancestry fields -- since ANC1P and ANC2P have same data values no need to do both
pums_codes <- pums_codes %>% dplyr::rename("ANC_Code" = "Code_1")

## filter PUMS codes for swana descriptions based on our swana_list
swana_codes<-pums_codes%>%filter(Description %in% pums_swana_list)
##### RACE RECLASSIFY ##
# Recode race/eth
## latino - alone or in combination with another race
ppl$latino <- "latino"
ppl$latino[ppl$HISP=="01"] <- "not latino"
ppl$latino <- as.factor(ppl$latino)

##aian - alone or in combination with another race or latino
ppl$aian <- "not aian"
ppl$aian[ppl$RACAIAN==1] <- "aian"
ppl$aian <- as.factor(ppl$aian)

##pacisl - alone or in combination with another race or latino
ppl$pacisl <- "not pacisl"
ppl$pacisl[ppl$RACPI==1 | ppl$RACNH==1] <- "pacisl"
ppl$pacisl <- as.factor(ppl$pacisl)

##swana - alone or in combination with another race or latino
ppl$swana <- "not swana"
ppl$swana[ppl$ANC1P%in% swana_codes$ANC_Code| ppl$ANC2P%in% swana_codes$ANC_Code] <- "swana"
ppl$swana <- as.factor(ppl$swana)

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
                                                                                                                       ifelse(ppl$ANC1P%in% swana_codes$ANC_Code| ppl$ANC2P%in% swana_codes$ANC_Code & ppl$latino =="latino", "swana",
                                                                                                                              ifelse(ppl$ANC1P%in% swana_codes$ANC_Code| ppl$ANC2P%in% swana_codes$ANC_Code & ppl$latino =="not latino", "nh_swana",
                                                                                                                                     NA)))))))))))))))))


##bipoc youth 
bipoc_list <- c("asian", "nh_asian","black" ,"nh_black", "twoormor","nh_twoormor", "aian", "nh_aian","pacisl", "nh_pacisl","swana", "nh_swana","nh_other", "other")

ppl$bipoc <- "not bipoc"
ppl$bipoc[ppl$race%in% bipoc_list] <- "bipoc"
ppl$bipoc <- as.factor(ppl$bipoc)


names(ppl) <- tolower(names(ppl))

# create list of replicate weights
repwlist = rep(paste0("pwgtp", 1:80))

# Check that columns are added and populated correctly
# latino includes all races. AIAN is AIAN alone/combo latino/non-latino, pacisl is alone/combo latino/non-latino
# View(ppl[c("hisp","latino","rac1p","race", "rac2p","rac3p","anc1p","anc2p","swana", "bipoc")])
table(ppl$race)
ppl%>%group_by(race,latino)%>%summarise(count=n())

#PREP DATA FOR ESTIMATE CALCS #

# Define weight variable and population base which will be used in the survey design set up
## You must use to WGTP (if you are using psam_h06.csv and want housing units, like for Low Quality Housing) or PWGTP (if you want person units, like for living_wage Youth)
weight <- 'pwgtp' 
## Specify population base. Use 100 for percents, or 1000 for rate per 1k.
pop_base <- 100

#### Step 0: Filtered for Indicator: Living Wage ####
# Adjust wage or salary income in past 12 months: WAGP (adjust with ADJINC)
# trying wages first then will try earnings 
ppl$wages_adj <- (as.numeric(ppl$wagp)*(as.numeric(ppl$adjinc)/1000000))

# Keep records for those with non-zero earnings in past year
ppl <- ppl %>% filter(wages_adj>0)

# Keep records for those who were at work last week or had a job but were not at work last week
ppl <- ppl %>% filter(wrk=='1' | esr %in% c(1, 2, 3, 4, 5))

# Filter for those who were not self-employed or unpaid family workers
# Note this is a different from v3 which didn't do this step
# View(ppl[c("RT","SERIALNO","COW","ESR","wages_adj","WKW","WRK","WKWN")])
# mean_cow <- ppl%>%
#   group_by(COW)%>%
#   summarize(mean_wages=weighted.mean(wages_adj,PWGTP))
# 6-8 which are self-employed and then employed in family business do seem to have different average earnings than others
ppl <- ppl %>% filter(!cow %in% c('6','7','8'))

## Calculate Living Wage ##
# Calculate hourly wage 
# First calculate number of hours worked based on weekly hours and weeks worked
## convert usual hours worked per week past 12 months: WKHP to integer
ppl$wkly_hrs <- as.integer(ppl$wkhp)

## average number of weeks worked in past 12 months for each value 1-6: WKW pre-2019 data
ppl$wks_worked_avg <- as.numeric(ifelse(ppl$wkw == 1, 51,
                                        ifelse(ppl$wkw == 2, 48.5,
                                               ifelse(ppl$wkw == 3, 43.5,
                                                      ifelse(ppl$wkw == 4, 33,
                                                             ifelse(ppl$wkw == 5, 20, 
                                                                    ifelse(ppl$wkw == 6, 7, 0)))))))

## create final weeks worked variable using WKWN variable for 2019 or later
ppl$wks_worked <- as.numeric(ifelse(ppl$wkwn>=1, ppl$wkwn, ppl$wks_worked_avg))
## View(ppl[c("RT","SERIALNO","wages_adj","WKW","WKWN","wks_worked","wks_worked_avg")])

# Then calculate hourly wage
## Use 16.90 since that is the LA County min wage as of July 1, 2023 (https://dcba.lacounty.gov/newsroom/minimum-wage-in-unincorporated-l-a-county-increases-to-16-90-per-hour/#:~:text=Minimum%20Wage%20in%20Unincorporated%20L.A.%20County%20Increases%20to%20%2416.90%20Per%20Hour,-Minimum%20Wage%20in&text=As%20of%20July%201%2C%202023,unincorporated%20areas%20of%20the%20County)
ppl$hrly_wage <- as.numeric(ppl$wages_adj/(ppl$wks_worked * ppl$wkly_hrs))
## View(ppl[c("RT","SERIALNO","wages_adj","WKHP","WKW","WKWN","wks_worked","wkly_hrs","hrly_wage")])

#* Code for Living Wage Indicator
# When $16.90 or more, code as livable. When less than $16.90 code as not livable. All other values code as NULL.
ppl$living_wage <- case_when(ppl$hrly_wage >= 16.90 ~ "livable", ppl$hrly_wage < 16.90 ~ "not livable", TRUE ~ "NA")
# View(ppl[c("RT","SERIALNO","COW","ESR","wages_adj","WKHP","WKW","WKWN","wks_worked","wkly_hrs","hrly_wage","living_wage")])

# # Convert to factor for indicator
# ppl$indicator <- as.factor(ppl$living_wage)




#### STEP 1A: Calculate the rate of living wage by puma (just for QA purposes, not exporting this table)####
##Create survey design youth earning a living wage by PUMA by race/ethnicity 
survey_design <- ppl %>%      # Use the filtered data (only 14-24 years)
  as_survey_rep(
    variables = c(puma, race, latino, aian, pacisl, swana, bipoc, living_wage),   # select grouping variables
    weights = weight,                        # person weight
    repweights = all_of(repwlist),          # list of replicate weights
    combined_weights = TRUE,                # tells the function that replicate weights are included in the data
    mse =TRUE,                              # tells the function to calc mse
    type="other",                           # statistical method
    scale=4/80,                             # scaling set by ACS
    rscale=rep(1,80)                        # setting specific to ACS-scaling
  )

survey_design

##### Summarize count of youth earning a living wage by PUMA by race/ethnicity ##

### Summarize by race
lw_race <- 
  
  # Calculate numerator
  survey_design %>%
  group_by(puma, race, living_wage) %>%   
  summarise(
    count = survey_total(na.rm = T), # get the (survey weighted) count for the numerators
    rate = survey_mean() # calc the % / get the (survey weighted) proportion for the numerator
  ) %>%       
  
  # join in denominator
  left_join(survey_design %>%                                # left join in the denominators
              group_by(puma, race) %>%                       # group by race/eth and puma
              summarise(pop = survey_total(na.rm = T))) %>%           # get the weighted total
  mutate(
    rate = rate*100,                                                # calc the % 
    rate_moe = rate_se*1.645*100,   # calculate the derived margin of error for the percentage
    rate_cv = ((rate_moe/1.645)/rate)) %>%                             # calculate the coefficient of variation 
  
  # remove NAs--these are groups coded other ways (latino, all aian, all pacisl)
  filter(!is.na(race))

# review
head(lw_race)


### Summarize by latino
lw_lat <- 
  survey_design %>%
  group_by(puma, latino, living_wage) %>%   
  summarise(
    count = survey_total(na.rm = T), # get the (survey weighted) count for the numerators
    rate = survey_mean() # calc the % / get the (survey weighted) proportion for the numerator
  ) %>%          
  
  left_join(survey_design %>%                                           
              group_by(puma, latino) %>%                    
              summarise(pop = survey_total(na.rm = T))) %>%        
  mutate(
    rate = rate*100,                                                # calc the % 
    rate_moe = rate_se*1.645*100,   # calculate the derived margin of error for the percentage
    rate_cv = ((rate_moe/1.645)/rate)) %>%                             # calculate the coefficient of variation 
  filter(latino !="not latino") %>% 
  rename("race"=latino)

# review
head(lw_lat)


### Summarize by All AIAN
lw_aian <- 
  survey_design %>%
  group_by(puma, aian, living_wage) %>%   
  summarise(
    count = survey_total(na.rm = T), # get the (survey weighted) count for the numerators
    rate = survey_mean() # calc the % / get the (survey weighted) proportion for the numerator
  ) %>%          
  
  left_join(survey_design %>%                                         
              group_by(puma, aian) %>%                     
              summarise(pop = survey_total(na.rm = T))) %>%            
  mutate(
    rate = rate*100,                                                # calc the % 
    rate_moe = rate_se*1.645*100,   # calculate the derived margin of error for the percentage
    rate_cv = ((rate_moe/1.645)/rate)) %>%                             # calculate the coefficient of variation 
  filter(aian !="not aian") %>% 
  rename("race"=aian)

# review
head(lw_aian)


### Summarize by All pacisl
lw_pacisl <- 
  survey_design %>%
  group_by(puma, pacisl, living_wage) %>%   
  summarise(
    count = survey_total(na.rm = T), # get the (survey weighted) count for the numerators
    rate = survey_mean() # calc the % / get the (survey weighted) proportion for the numerator
  ) %>%            
  
  left_join(survey_design %>%                                         
              group_by(puma, pacisl) %>%                     
              summarise(pop = survey_total(na.rm = T))) %>%            
  mutate(
    rate = rate*100,                                                # calc the % 
    rate_moe = rate_se*1.645*100,   # calculate the derived margin of error for the percentage
    rate_cv = ((rate_moe/1.645)/rate)) %>%                             # calculate the coefficient of variation 
  filter(pacisl !="not pacisl") %>% 
  rename("race"=pacisl)

# review
head(lw_pacisl)

### Summarize by All SWANA
lw_swana <- 
  survey_design %>%
  group_by(puma, swana, living_wage) %>%   
  summarise(
    count = survey_total(na.rm = T), # get the (survey weighted) count for the numerators
    rate = survey_mean() # calc the % / get the (survey weighted) proportion for the numerator
  ) %>%             
  
  left_join(survey_design %>%                                         
              group_by(puma, swana) %>%                     
              summarise(pop = survey_total(na.rm = T))) %>%            
  mutate(
    rate = rate*100,                                                # calc the % 
    rate_moe = rate_se*1.645*100,   # calculate the derived margin of error for the percentage
    rate_cv = ((rate_moe/1.645)/rate)) %>%                             # calculate the coefficient of variation 
  filter(swana !="not SWANA") %>% 
  rename("race"=swana)

# review
head(lw_swana)

### Summarize by BIPOC
lw_bipoc <- 
  survey_design %>%
  group_by(puma, bipoc, living_wage) %>%   
  summarise(
    count = survey_total(na.rm = T), # get the (survey weighted) count for the numerators
    rate = survey_mean() # calc the % / get the (survey weighted) proportion for the numerator
  ) %>%             
  
  left_join(survey_design %>%                                         
              group_by(puma, bipoc) %>%                     
              summarise(pop = survey_total(na.rm = T))) %>%            
  mutate(
    rate = rate*100,                                                # calc the % 
    rate_moe = rate_se*1.645*100,   # calculate the derived margin of error for the percentage
    rate_cv = ((rate_moe/1.645)/rate)) %>%                             # calculate the coefficient of variation 
  filter(bipoc !="not bipoc") %>% 
  rename("race"=bipoc)

# review
head(lw_bipoc)

### Summarize data for total pop (raceeth = total)
lw_pop <- 
  survey_design %>%
  group_by(puma, living_wage) %>% 
  summarise(
    count = survey_total(na.rm = T), # get the (survey weighted) count for the numerators
    rate = survey_mean() # calc the % / get the (survey weighted) proportion for the numerator
  ) %>%         
  
  left_join(survey_design %>%               
              group_by(puma) %>%                   
              summarise(pop = survey_total(na.rm = T))) %>% 
  mutate(
    race = "total",
    rate = rate*100,                                                # calc the % 
    rate_moe = rate_se*1.645*100,   # calculate the derived margin of error for the percentage
    rate_cv = ((rate_moe/1.645)/rate))                          # calculate the coefficient of variation                

# review
head(lw_pop)

## combine race/eth estimates with total
lw_puma <-  
  bind_rows(lw_race, lw_lat, lw_aian, lw_pacisl, lw_swana, lw_bipoc, lw_pop) %>%
  filter(living_wage =="livable") %>%  # we're going with asset-based
  mutate(
    geolevel = "puma",
    county = "06037"  
  )

# review
table(lw_puma$race, lw_puma$living_wage, useNA = "always")
head(lw_puma)

#### STEP 2A: Calculate  the rate of living wage by race for LA COUNTY ####
survey_design_county <- ppl %>%      # Use the filtered data (only 14-24 years)
  as_survey_rep(
    variables = c(county, race, latino, aian, pacisl, swana, bipoc, living_wage),   # select grouping variables
    weights = weight,                        # person weight
    repweights = all_of(repwlist),          # list of replicate weights
    combined_weights = TRUE,                # tells the function that replicate weights are included in the data
    mse =TRUE,                              # tells the function to calc mse
    type="other",                           # statistical method
    scale=4/80,                             # scaling set by ACS
    rscale=rep(1,80)                        # setting specific to ACS-scaling
  )

survey_design_county

## SUMMARIZE BY COUNTY 
### Summarize by race

lw_race_county <- 
  survey_design_county %>%
  group_by(county, race, living_wage) %>%   
  summarise(
    count = survey_total(na.rm = T), # get the (survey weighted) count for the numerators
    rate = survey_mean() # calc the % / get the (survey weighted) proportion for the numerator
  ) %>%        
  
  left_join(survey_design_county %>%                               # left join in the denominators
              group_by(county, race) %>%                           # group by race/eth and puma
              summarise(pop = survey_total(na.rm = T))) %>%        # get the weighted total
  mutate(
    rate = rate*100,                                                # calc the % 
    rate_moe = rate_se*1.645*100,   # calculate the derived margin of error for the percentage
    rate_cv = ((rate_moe/1.645)/rate)) %>%                             # calculate the coefficient of variation 
  filter(!is.na(race))



### Summarize by latino
lw_lat_county <- 
  survey_design_county %>%
  group_by(county, latino, living_wage) %>%   
  summarise(
    count = survey_total(na.rm = T), # get the (survey weighted) count for the numerators
    rate = survey_mean() # calc the % / get the (survey weighted) proportion for the numerator
  ) %>%             
  
  left_join(survey_design_county %>%                                      
              group_by(county, latino) %>%                   
              summarise(pop = survey_total(na.rm = T))) %>%  
  mutate(
    rate = rate*100,                                                # calc the % 
    rate_moe = rate_se*1.645*100,   # calculate the derived margin of error for the percentage
    rate_cv = ((rate_moe/1.645)/rate)) %>%                             # calculate the coefficient of variation 
  filter(latino !="not latino") %>% 
  rename("race" = latino)


### Summarize by aian
lw_aian_county <- 
  survey_design_county %>%
  group_by(county, aian, living_wage) %>%   
  summarise(
    count = survey_total(na.rm = T), # get the (survey weighted) count for the numerators
    rate = survey_mean() # calc the % / get the (survey weighted) proportion for the numerator
  ) %>%       
  
  left_join(survey_design_county %>%                                       
              group_by(county, aian) %>%                
              summarise(pop = survey_total(na.rm = T))) %>%       
  mutate(
    rate = rate*100,                                                # calc the % 
    rate_moe = rate_se*1.645*100,   # calculate the derived margin of error for the percentage
    rate_cv = ((rate_moe/1.645)/rate)) %>%                             # calculate the coefficient of variation 
  filter(aian !="not aian") %>% 
  rename("race" = aian)


### Summarize by pacisl
lw_pacisl_county <- 
  survey_design_county %>%
  group_by(county, pacisl, living_wage) %>%   
  summarise(
    count = survey_total(na.rm = T), # get the (survey weighted) count for the numerators
    rate = survey_mean() # calc the % / get the (survey weighted) proportion for the numerator
  ) %>%       
  
  left_join(survey_design_county %>%                                       
              group_by(county, pacisl) %>%                
              summarise(pop = survey_total(na.rm = T))) %>%       
  mutate(
    rate = count/pop,                                                 
    rate_moe = moe_prop(count, pop, count_se*1.645, pop_se*1.645),  
    rate_cv = ((rate_moe/1.645)/rate)) %>% 
  filter(pacisl !="not pacisl") %>% 
  rename("race" = pacisl)

### Summarize by SWANA
lw_swana_county <- 
  survey_design_county %>%
  group_by(county, swana, living_wage) %>%   
  summarise(
    count = survey_total(na.rm = T), # get the (survey weighted) count for the numerators
    rate = survey_mean() # calc the % / get the (survey weighted) proportion for the numerator
  ) %>%       
  
  left_join(survey_design_county %>%                                       
              group_by(county, swana) %>%                
              summarise(pop = survey_total(na.rm = T))) %>%       
  mutate(
    rate = rate*100,                                                # calc the % 
    rate_moe = rate_se*1.645*100,   # calculate the derived margin of error for the percentage
    rate_cv = ((rate_moe/1.645)/rate)) %>%                             # calculate the coefficient of variation 
  filter(swana !="not swana") %>% 
  rename("race" = swana)


### Summarize by BIPOC
lw_bipoc_county <- 
  survey_design_county %>%
  group_by(county, bipoc, living_wage) %>%   
  summarise(
    count = survey_total(na.rm = T), # get the (survey weighted) count for the numerators
    rate = survey_mean() # calc the % / get the (survey weighted) proportion for the numerator
  ) %>%            
  
  left_join(survey_design_county %>%                                         
              group_by(county, bipoc) %>%                     
              summarise(pop = survey_total(na.rm = T))) %>%            
  mutate(
    rate = rate*100,                                                # calc the % 
    rate_moe = rate_se*1.645*100,   # calculate the derived margin of error for the percentage
    rate_cv = ((rate_moe/1.645)/rate)) %>%                             # calculate the coefficient of variation 
  filter(bipoc !="not bipoc") %>% 
  rename("race"=bipoc)

# review
head(lw_bipoc_county)

# Summarize data for total pop (raceeth = total)
lw_pop_county <- 
  survey_design_county %>%
  group_by(county, living_wage) %>%  
  summarise(
    count = survey_total(na.rm = T), # get the (survey weighted) count for the numerators
    rate = survey_mean() # calc the % / get the (survey weighted) proportion for the numerator
  ) %>%        
  
  left_join(survey_design_county %>%                                           
              group_by(county) %>%                  
              summarise(pop = survey_total(na.rm = T))) %>%            
  mutate(
    race = "total",
    rate = count/pop,                                                 
    rate_moe = moe_prop(count, pop, count_se*1.645, pop_se*1.645),   
    rate_cv = ((rate_moe/1.645)/rate))       


## combine race/eth estimates with total
lw_county <-  
  bind_rows(lw_race_county, lw_lat_county, 
            lw_aian_county, lw_pacisl_county, lw_swana_county, lw_bipoc_county, lw_pop_county) %>% 
  filter(living_wage =="livable")


#### STEP 2B: Calculate disparity index (ala RC) by race ####

lw_county_id <-
  lw_county %>% ungroup() %>%
  mutate(best=max(rate[race!='total' & race!='bipoc'],na.rm=T),asbest="max",#create columns for best rate and as best treatment, ignoring total and bipoc
         values_count = sum(!is.na(race) & race!='total' & race!="bipoc")) %>% # remove total and bipoc from values count
  mutate(diff = ifelse(race == "total" | race == "bipoc", NA, abs(best-rate))) %>%
  mutate(index_of_disparity = ((sum(diff, na.rm=T)/best/(values_count - 1))*100)) %>% # use the best value identified above
  #select just the columns needed 
  select(county, race, count, count_se, pop, pop_se, rate, rate_moe, rate_cv, asbest, values_count, best, diff, index_of_disparity)


#### STEP 2C: Upload yp_livingwage_race to postgres ####

# Write living wage table to bold vision database
dbWriteTable(con, c("bv_2023", "yp_livingwage_subgroup"), lw_county_id,
             overwrite = TRUE, row.names = FALSE,
             field.types = c(
               county = "varchar",
               race = "varchar",
               count ="integer",
               count_se="numeric",
               pop = "integer",
               pop_se = "numeric",
               rate = "numeric",
               rate_moe = "numeric",
               rate_cv = "numeric",
               asbest = "varchar",
               values_count = "numeric",
               best = "varchar",
               diff= "numeric",
               index_of_disparity= "numeric"
             ))


# write comment to table

dbSendQuery(con, 
            "COMMENT ON TABLE bv_2023.yp_livingwage_subgroup IS 'Table for Youth earning a Living Wage in LA County. 
Living Wage is defined as youth (14-24 years) who are in the labor force and who earn $16.90 (LA County Minimum Wage) or more per hour.
Race codes for AIAN and pacisl are All AIAN and All pacisl (multiracial + latino inclusive).
This table was prepared in the R script W:\\Project\\OSI\\Bold Vision\\BV 2023\\R\\boldvision_22_23\\Youth Power\\yp_livingwage.R
QA doc:  W:\\Project\\OSI\\Bold Vision\\BV 2023\\Documentation\\QA_livingwage.docx';

COMMENT ON COLUMN bv_2023.yp_livingwage_subgroup.diff is 'The difference between the best rate and the respective race group. Total and BIPOC not included in calculation.';
COMMENT ON COLUMN bv_2023.yp_livingwage_subgroup.index_of_disparity is 'The index of disparity by race with the best rate of the subgroups.';
COMMENT ON COLUMN bv_2023.yp_livingwage_subgroup.race is 'Race/ethnicity category. AIAN and pacisl are multiracial latino-inclusive (All AIAN, All pacisl).';
COMMENT ON COLUMN bv_2023.yp_livingwage_subgroup.count is 'Count of youth who are who are/are not making above living wage (see column var for which category)';
COMMENT ON COLUMN bv_2023.yp_livingwage_subgroup.count_se is 'Standard error of count of youth who are are/are not making above living wage (see column var for which category)';
COMMENT ON COLUMN bv_2023.yp_livingwage_subgroup.pop is 'Universe of youth who are are/are not making above living wage, youth 14-24 years';
COMMENT ON COLUMN bv_2023.yp_livingwage_subgroup.pop_se is 'Standard error of universe of youth who are are/are not making above living wage, youth 14-24 years';
COMMENT ON COLUMN bv_2023.yp_livingwage_subgroup.rate is 'Percentage youth of youth who are are/are not making above living wage (see column var for which category)';
COMMENT ON COLUMN bv_2023.yp_livingwage_subgroup.rate_moe is 'Margin of error for percentage youth of youth who are are/are not making above living wage (see column var for which category)';
COMMENT ON COLUMN bv_2023.yp_livingwage_subgroup.rate_cv is 'CV (coefficient of variation) for percentage youth of youth who areare/are not making above living wage (see column var for which category). Values over .4, i.e. 40%, are generally considered to be unstable estimates.';
COMMENT ON COLUMN bv_2023.yp_livingwage_subgroup.county is 'County FIPS';"
)

#### STEP 2D: Visualize bargraph by Race ####

#pull data you're using by subgroup
df_subgroup <- st_read(con, query = "select * from bv_2023.yp_livingwage_subgroup")

#pull race labels
race_label_df <- st_read(con, query = "select * from bv_2023.metadata_race_labels") 

#join to your table
df_subgroup <- left_join(df_subgroup, race_label_df, by=c("race" = "race_base"))  %>%
  select(-"id")

#defining vectors AND re-ordering if you have total and bipoc, take out
df <- subset(df_subgroup, race != "total" & race != "bipoc") %>%
  arrange(rate)

#use function for bv_visuals_functions.R script
fx_barchart_subgroup(
  df = df,
  domain = "Youth Power",
  indicator = "Living Wage",
  title = "[insert]",
  subtitle = "[insert]",
  caption_datasource = "American Community Survey Public Use Microdata, 2017-2021 5-Year Estimate.",
  caption_racenote = "AIAN=American Indian or Alaska Native; NHPI=Native Hawaiian or Pacific Islander; SWANA/MENA=Southwest Asian or North African/Middle Eastern or North African",
  caption_indicator_def = "Living Wage is defined as youth (14-24 years) who are in the labor force and who earn $16.90 (LA County Minimum Wage) or more per hour."
)


#### STEP 3A: Calculate the rate of living wage by SPA ####

#use crosswalk to get data by spa
crosswalk <- st_read(con, query = "select * from bv_2023.crosswalk_puma_spas_2023")

ppl$puma_id <- as.factor(paste0("06", ppl$puma))

ppl_spa <- right_join(ppl, crosswalk, by=c("puma_id" = "puma_id"))    # specify the field join


survey_design_spa <- ppl_spa %>%      # Use the filtered data (only 14-24 years)
  as_survey_rep(
    variables = c(spa_id, spa_name, living_wage),   # select grouping variables
    weights = weight,                        # person weight
    repweights = all_of(repwlist),          # list of replicate weights
    combined_weights = TRUE,                # tells the function that replicate weights are included in the data
    mse =TRUE,                              # tells the function to calc mse
    type="other",                           # statistical method
    scale=4/80,                             # scaling set by ACS
    rscale=rep(1,80)                        # setting specific to ACS-scaling
  )

survey_design_spa

### Summarize by region
lw_spa <- 
  survey_design_spa %>%
  group_by(spa_id, spa_name, living_wage) %>%   
  summarise(
    count = survey_total(na.rm = T), # get the (survey weighted) count for the numerators
    rate = survey_mean() # calc the % / get the (survey weighted) proportion for the numerator
  ) %>%            # get the (survey weighted) count 
  
  left_join(survey_design_spa %>%                               # left join in the denominators
              group_by(spa_id) %>%                           # group by race/eth and puma
              summarise(pop = survey_total(na.rm = T))) %>%                 # get the weighted total
  mutate(
    rate = count/pop,                                                # calc the % 
    rate_moe = moe_prop(count, pop, count_se*1.645, pop_se*1.645),   # calculate the derived margin of error for the percentage
    rate_cv = ((rate_moe/1.645)/rate)) %>%
  filter(living_wage =="livable")


#### STEP 3B: Calculate disparity index (ala RC) by SPA ####
lw_spa_id <-
  lw_spa %>% ungroup() %>%
  mutate(best=max(rate,na.rm=T), asbest="max",#create columns for best rate and as best treatment, ignoring total and bipoc
         values_count = sum(!is.na(spa_id))) %>% # remove total and bipoc from values count
  mutate(diff = ifelse(abs(best - rate) == 0 , NA, abs(best-rate))) %>%
  mutate(index_of_disparity = ((sum(diff, na.rm=T)/best/(values_count - 1))*100)) %>% # use the best value identified above
  #select just the columns needed 
  select(spa_id, spa_name, count, count_se, pop, pop_se, rate, rate_moe, rate_cv, asbest, values_count, best, diff, index_of_disparity)

#### STEP 3C: Upload yp_livingwage_region to postgres ####

# Write living_wage youth table to bold vision database
dbWriteTable(con, c("bv_2023", "yp_livingwage_region"), lw_spa_id,
             overwrite = TRUE, row.names = FALSE,
             field.types = c(
               spa_id = "numeric",
               spa_name = "varchar",
               count ="integer",
               count_se="numeric",
               pop = "integer",
               pop_se = "numeric",
               rate = "numeric",
               rate_moe = "numeric",
               rate_cv = "numeric",
               asbest = "varchar",
               values_count = "numeric",
               best = "varchar",
               diff= "numeric",
               index_of_disparity= "numeric"
             ))


# write comment to table

dbSendQuery(con, 
            "COMMENT ON TABLE bv_2023.yp_livingwage_region IS 'Table for Youth earning a Living Wage in LA County by SPA. 
Living Wage is defined as youth (14-24 years) who are in the labor force and who earn $16.90 (LA County Minimum Wage) or more per hour.
This table was prepared in the R script W:\\Project\\OSI\\Bold Vision\\BV 2023\\R\\boldvision_22_23\\Youth Power\\yp_livingwage.R
QA doc:  W:\\Project\\OSI\\Bold Vision\\BV 2023\\Documentation\\QA_livingwage.docx';

COMMENT ON COLUMN bv_2023.yp_livingwage_region.diff is 'The difference between the best rate and the respective race group. Total and BIPOC not included in calculation.';
COMMENT ON COLUMN bv_2023.yp_livingwage_region.index_of_disparity is 'The index of disparity by race with the best rate of the regions.';
COMMENT ON COLUMN bv_2023.yp_livingwage_region.spa_name is 'Name of SPA region';
COMMENT ON COLUMN bv_2023.yp_livingwage_region.count is 'Count of youth who are/are not earning above living wage (see column var for which category)';
COMMENT ON COLUMN bv_2023.yp_livingwage_region.count_se is 'Standard error of count of youth who are/are not earning above living wage  (see column var for which category)';
COMMENT ON COLUMN bv_2023.yp_livingwage_region.pop is 'Universe of youth who are/are not earning above living wage , youth 14-24 years';
COMMENT ON COLUMN bv_2023.yp_livingwage_region.pop_se is 'Standard error of universe of youth who are/are not earning above living wage , youth 14-24 years';
COMMENT ON COLUMN bv_2023.yp_livingwage_region.rate is 'Percentage youth of youth who are/are not earning above living wage  (see column var for which category)';
COMMENT ON COLUMN bv_2023.yp_livingwage_region.rate_moe is 'Margin of error for percentage youth of youth who are/are not earning above living wage  (see column var for which category)';
COMMENT ON COLUMN bv_2023.yp_livingwage_region.rate_cv is 'CV (coefficient of variation) for percentage youth of youth who are/are not earning above living wage  (see column var for which category). Values over .4, i.e. 40%, are generally considered to be unstable estimates.';
COMMENT ON COLUMN bv_2023.yp_livingwage_region.spa_id is 'LA County Service Planning Area (SPA) ID Number';"
)


dbDisconnect(conn = con)
dbDisconnect(conn = con_rda)
