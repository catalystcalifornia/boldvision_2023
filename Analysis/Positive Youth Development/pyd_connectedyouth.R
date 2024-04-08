# BV Domain: Positive Youth Development
# Indicator: Connected youth who are enrolled in school and/or employed (14-24)

# PART A. Calculate the percentage of youth (ages 14-24) who are enrolled in school and/or employed by race, SPA, and BIPOC/non-BIPOC. 
# PART B. Calculate disparity index (ala RC) by race and by SPA (one racial disparity and one geographic disparity index).
# PART C. Push the above to postgres one table for BIPOC/race rates/disparities and one table for SPA rates/disparities



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


# create connection for bv database
source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("bold_vision")

con_rda <- connect_to_db("rda_shared_data")

root <- "W:/Data/Demographics/PUMS/"

# Load the people PUMS data with Connected Youth Data 
dat <- fread(paste0(root, "CA_2017_2021/psam_p06.csv"), header = TRUE, data.table = FALSE,  
             colClasses = list(character = c("PUMA", "ANC1P", "ANC2P", "HISP", "RAC1P", "RAC2P", "RAC3P", "RACAIAN", "RACPI", "RACNH", "AGEP")))
#### Step 0: Recoding for Age Limit, Race, Etc####

dat$county <-  as.factor(paste0("06", substr(dat$PUMA, 1, 3)))

# create list of replicate weights
repwlist = rep(paste0("PWGTP", 1:80))

ppl <- 
  dat %>%
  select(SERIALNO, PUMA, ST, county, AGEP, HISP, RAC1P, RAC2P, RAC3P, ANC1P, ANC2P, RACAIAN, RACNH, RACPI, ESR, SCH, PWGTP, all_of(repwlist)) %>%
  filter((AGEP >= 14 & AGEP <= 24 & AGEP != 2) & county == "06037") %>%
  collect() %>%
  as.data.frame()

###### Set up codes for SWANA ##
# First use ancestry codes to help identify estimated swana pop
## Create list of swana codes for PUMS
pums_swana_list<-list("Armenian","Algerian","Arab","Assyrian","Bahraini","Berber","Chaldean","Egyptian","Emirati","Iranian","Iraqi","Israeli","Jordanian","Kurdish","Kuwaiti","Lebanese","Libyan","Middle Eastern","Moroccan","North African","Omani","Palestinian","Qatari","Saudi","Syriac","Syrian","Tunisian","Yazidi","Yemeni","Mideast","Saudi Arabian","Arabic","Other Arab","Libyan (2017 or later)","Kuwaiti (2017 or later)","Turkish","Sudanese","Afghan") # 2017 or later needed based on reviewing data dictionary and saw fields for Arabic and Other Arab

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
                                                                                                                       NA)))))))))))))))


##bipoc youth 
bipoc_list <- c("asian", "nh_asian","black" ,"nh_black", "twoormor","nh_twoormor", "aian", "nh_aian","pacisl", "nh_pacisl","nh_other", "other")

ppl$bipoc <- "not bipoc"
ppl$bipoc[ppl$race%in% bipoc_list] <- "bipoc"
ppl$bipoc[ppl$swana=="swana"] <- "bipoc"
ppl$bipoc[ppl$latino=="latino"] <- "bipoc"
ppl$bipoc <- as.factor(ppl$bipoc)


names(ppl) <- tolower(names(ppl))

# create list of replicate weights
repwlist = rep(paste0("pwgtp", 1:80))

# Check that columns are added and populated correctly
# latino includes all races. AIAN is AIAN alone/combo latino/non-latino, pacisl is alone/combo latino/non-latino
# View(ppl[c("hisp","latino","rac1p","race", "rac2p","rac3p","anc1p","anc2p","swana", "bipoc")])
table(ppl$race)
ppl%>%group_by(race,latino)%>%summarise(count=n())


### filtered for connected/disconnected youth ###
# code employment -- unemployed or not in the labor force AND filter out for those 14/15 becuase they are not even asked if they are employed. 
ppl$employment <- ifelse(ppl$esr %in% c(3,6) | ppl$agep == 14 | ppl$agep == 15, "not employed", "employed")

# code school enrollment
ppl$schl_enroll <-  ifelse(ppl$sch == 1, "not attending school", "in school")

# create connected/disconnected variable (data type as factor)
ppl$connected <- as.factor(ifelse(ppl$employment == "not employed" & ppl$schl_enroll =="not attending school", "disconnected", "connected"))

#PREP DATA FOR ESTIMATE CALCS #

# Define weight variable and population base which will be used in the survey design set up
## You must use to WGTP (if you are using psam_h06.csv and want housing units, like for Low Quality Housing) or PWGTP (if you want person units, like for Connected Youth)
weight <- 'pwgtp' 
## Specify population base. Use 100 for percents, or 1000 for rate per 1k.
pop_base <- 100


# #### STEP 1A: Calculate the percentage of youth (ages 14-24) who are enrolled in school and/or employed by race by puma (just for QA purposes, not exporting this table)####
# ##Create survey design disconnected youth by PUMA by race/ethnicity 
# survey_design <- ppl %>%      # Use the filtered data (only 14-24 years)
#   as_survey_rep(
#     variables = c(puma, race, latino, aian, pacisl, swana, bipoc, connected),   # select grouping variables
#     weights = weight,                        # person weight
#     repweights = all_of(repwlist),          # list of replicate weights
#     combined_weights = TRUE,                # tells the function that replicate weights are included in the data
#     mse =TRUE,                              # tells the function to calc mse
#     type="other",                           # statistical method
#     scale=4/80,                             # scaling set by ACS
#     rscale=rep(1,80)                        # setting specific to ACS-scaling
#   )
# 
# survey_design
# 
# ##### Summarize count of disconnected youth by PUMA by race/ethnicity ##
# 
# ### Summarize by race
# discon_race <- 
#   
#   # Calculate numerator
#   survey_design %>%
#   group_by(puma, race, connected) %>%   
#   summarise(
#     count = survey_total(na.rm = T), # get the (survey weighted) count for the numerators
#     rate = survey_mean() # calc the % / get the (survey weighted) proportion for the numerator
#   ) %>%       
#   
#   # join in denominator
#   left_join(survey_design %>%                                # left join in the denominators
#               group_by(puma, race) %>%                       # group by race/eth and puma
#               summarise(pop = survey_total(na.rm = T))) %>%           # get the weighted total
#   mutate(
#     rate = rate*100,                                                # calc the % 
#     rate_moe = rate_se*1.645*100,   # calculate the derived margin of error for the percentage
#     rate_cv = ((rate_moe/1.645)/rate)*100, 
#     count_moe = count_se*1.645, # calculate moe for numerator count based on se provided by the output 
#     count_cv = ((count_moe/1.645)/count) * 100)  # calculate cv for numerator count) %>%                             # calculate the coefficient of variation 
# 
# # remove NAs--these are groups coded other ways (latino, all aian, all pacisl)
# filter(!is.na(race))
# 
# # review
# head(discon_race)
# 
# 
# ### Summarize by latino
# discon_lat <- 
#   survey_design %>%
#   group_by(puma, latino, connected) %>%   
#   summarise(
#     count = survey_total(na.rm = T), # get the (survey weighted) count for the numerators
#     rate = survey_mean() # calc the % / get the (survey weighted) proportion for the numerator
#   ) %>%          
#   
#   left_join(survey_design %>%                                           
#               group_by(puma, latino) %>%                    
#               summarise(pop = survey_total(na.rm = T))) %>%        
#   mutate(
#     rate = rate*100,                                                # calc the % 
#     rate_moe = rate_se*1.645*100,   # calculate the derived margin of error for the percentage
#     rate_cv = ((rate_moe/1.645)/rate)*100,
#     count_moe = count_se*1.645, # calculate moe for numerator count based on se provided by the output
#     count_cv = ((count_moe/1.645)/count) * 100) %>%                             # calculate the coefficient of variation 
#   filter(latino !="not latino") %>% 
#   rename("race"=latino)
# 
# # review
# head(discon_lat)
# 
# 
# ### Summarize by All AIAN
# discon_aian <- 
#   survey_design %>%
#   group_by(puma, aian, connected) %>%   
#   summarise(
#     count = survey_total(na.rm = T), # get the (survey weighted) count for the numerators
#     rate = survey_mean() # calc the % / get the (survey weighted) proportion for the numerator
#   ) %>%          
#   
#   left_join(survey_design %>%                                         
#               group_by(puma, aian) %>%                     
#               summarise(pop = survey_total(na.rm = T))) %>%            
#   mutate(
#     rate = rate*100,                                                # calc the % 
#     rate_moe = rate_se*1.645*100,   # calculate the derived margin of error for the percentage
#     rate_cv = ((rate_moe/1.645)/rate)*100,
#     count_moe = count_se*1.645, # calculate moe for numerator count based on se provided by the output
#     count_cv = ((count_moe/1.645)/count) * 100) %>%                             # calculate the coefficient of variation 
#   filter(aian !="not aian") %>% 
#   rename("race"=aian)
# 
# # review
# head(discon_aian)
# 
# 
# ### Summarize by All pacisl
# discon_pacisl <- 
#   survey_design %>%
#   group_by(puma, pacisl, connected) %>%   
#   summarise(
#     count = survey_total(na.rm = T), # get the (survey weighted) count for the numerators
#     rate = survey_mean() # calc the % / get the (survey weighted) proportion for the numerator
#   ) %>%            
#   
#   left_join(survey_design %>%                                         
#               group_by(puma, pacisl) %>%                     
#               summarise(pop = survey_total(na.rm = T))) %>%            
#   mutate(
#     rate = rate*100,                                                # calc the % 
#     rate_moe = rate_se*1.645*100,   # calculate the derived margin of error for the percentage
#     rate_cv = ((rate_moe/1.645)/rate)*100,
#     count_moe = count_se*1.645, # calculate moe for numerator count based on se provided by the output
#     count_cv = ((count_moe/1.645)/count) * 100) %>%                             # calculate the coefficient of variation 
#   filter(pacisl !="not pacisl") %>% 
#   rename("race"=pacisl)
# 
# # review
# head(discon_pacisl)
# 
# ### Summarize by All SWANA
# discon_swana <- 
#   survey_design %>%
#   group_by(puma, swana, connected) %>%   
#   summarise(
#     count = survey_total(na.rm = T), # get the (survey weighted) count for the numerators
#     rate = survey_mean() # calc the % / get the (survey weighted) proportion for the numerator
#   ) %>%             
#   
#   left_join(survey_design %>%                                         
#               group_by(puma, swana) %>%                     
#               summarise(pop = survey_total(na.rm = T))) %>%            
#   mutate(
#     rate = rate*100,                                                # calc the % 
#     rate_moe = rate_se*1.645*100,   # calculate the derived margin of error for the percentage
#     rate_cv = ((rate_moe/1.645)/rate)*100,
#     count_moe = count_se*1.645, # calculate moe for numerator count based on se provided by the output
#     count_cv = ((count_moe/1.645)/count) * 100) %>%                             # calculate the coefficient of variation 
#   filter(swana !="not swana") %>% 
#   rename("race"=swana)
# 
# # review
# head(discon_swana)
# 
# ### Summarize by BIPOC
# discon_bipoc <- 
#   survey_design %>%
#   group_by(puma, bipoc, connected) %>%   
#   summarise(
#     count = survey_total(na.rm = T), # get the (survey weighted) count for the numerators
#     rate = survey_mean() # calc the % / get the (survey weighted) proportion for the numerator
#   ) %>%             
#   
#   left_join(survey_design %>%                                         
#               group_by(puma, bipoc) %>%                     
#               summarise(pop = survey_total(na.rm = T))) %>%            
#   mutate(
#     rate = rate*100,                                                # calc the % 
#     rate_moe = rate_se*1.645*100,   # calculate the derived margin of error for the percentage
#     rate_cv = ((rate_moe/1.645)/rate)*100,
#     count_moe = count_se*1.645, # calculate moe for numerator count based on se provided by the output
#     count_cv = ((count_moe/1.645)/count) * 100) %>%                             # calculate the coefficient of variation 
#   filter(bipoc !="not bipoc") %>% 
#   rename("race"=bipoc)
# 
# # review
# head(discon_bipoc)
# 
# ### Summarize data for total pop (raceeth = total)
# discon_pop <- 
#   survey_design %>%
#   group_by(puma, connected) %>% 
#   summarise(
#     count = survey_total(na.rm = T), # get the (survey weighted) count for the numerators
#     rate = survey_mean() # calc the % / get the (survey weighted) proportion for the numerator
#   ) %>%         
#   
#   left_join(survey_design %>%               
#               group_by(puma) %>%                   
#               summarise(pop = survey_total(na.rm = T))) %>% 
#   mutate(
#     race = "total",
#     rate = rate*100,                                                # calc the % 
#     rate_moe = rate_se*1.645*100,   # calculate the derived margin of error for the percentage
#     rate_cv = ((rate_moe/1.645)/rate)*100,
#     count_moe = count_se*1.645, # calculate moe for numerator count based on se provided by the output
#     count_cv = ((count_moe/1.645)/count) * 100)  # calculate cv for numerator count
# # calculate the coefficient of variation                
# 
# # review
# head(discon_pop)
# 
# ## combine race/eth estimates with total
# # leave in connected and disconnected so we can look asset or deficit based
# discon_puma <-  
#   bind_rows(discon_race, discon_lat, discon_aian, discon_pacisl, discon_swana, discon_bipoc, discon_pop) %>%
#   filter(connected =="connected") %>%  # we're going with asset-based
#   mutate(
#     geolevel = "puma",
#     county = "06037"
#   )
# 
# # review
# table(discon_puma$race, discon_puma$connected, useNA = "always")
# head(discon_puma)

#### STEP 2A: Calculate the percentage of youth (ages 14-24) who are enrolled in school and/or employed by race for LA COUNTY ####
survey_design_county <- ppl %>%      # Use the filtered data (only 14-24 years)
  as_survey_rep(
    variables = c(county, race, latino, aian, pacisl, swana, bipoc, connected),   # select grouping variables
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

discon_race_county <- 
  survey_design_county %>%
  group_by(county, race, connected) %>%   
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
    rate_cv = ((rate_moe/1.645)/rate)*100,
    count_moe = count_se*1.645, # calculate moe for numerator count based on se provided by the output
    count_cv = ((count_moe/1.645)/count) * 100) %>%                             # calculate the coefficient of variation 
  filter(!is.na(race))



### Summarize by latino
discon_lat_county <- 
  survey_design_county %>%
  group_by(county, latino, connected) %>%   
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
    rate_cv = ((rate_moe/1.645)/rate)*100,
    count_moe = count_se*1.645, # calculate moe for numerator count based on se provided by the output
    count_cv = ((count_moe/1.645)/count) * 100) %>%                             # calculate the coefficient of variation 
  filter(latino !="not latino") %>% 
  rename("race" = latino)


### Summarize by aian
discon_aian_county <- 
  survey_design_county %>%
  group_by(county, aian, connected) %>%   
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
    rate_cv = ((rate_moe/1.645)/rate)*100,
    count_moe = count_se*1.645, # calculate moe for numerator count based on se provided by the output
    count_cv = ((count_moe/1.645)/count) * 100) %>%                             # calculate the coefficient of variation 
  filter(aian !="not aian") %>% 
  rename("race" = aian)


### Summarize by pacisl
discon_pacisl_county <- 
  survey_design_county %>%
  group_by(county, pacisl, connected) %>%   
  summarise(
    count = survey_total(na.rm = T), # get the (survey weighted) count for the numerators
    rate = survey_mean() # calc the % / get the (survey weighted) proportion for the numerator
  ) %>%       
  
  left_join(survey_design_county %>%                                       
              group_by(county, pacisl) %>%                
              summarise(pop = survey_total(na.rm = T))) %>%       
  mutate(
    rate = rate*100,                                                 
    rate_moe = rate_se*1.645*100,  
    rate_cv = ((rate_moe/1.645)/rate)*100,
    count_moe = count_se*1.645, # calculate moe for numerator count based on se provided by the output
    count_cv = ((count_moe/1.645)/count) * 100) %>% 
  filter(pacisl !="not pacisl") %>% 
  rename("race" = pacisl)

### Summarize by SWANA
discon_swana_county <- 
  survey_design_county %>%
  group_by(county, swana, connected) %>%   
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
    rate_cv = ((rate_moe/1.645)/rate)*100,
    count_moe = count_se*1.645, # calculate moe for numerator count based on se provided by the output
    count_cv = ((count_moe/1.645)/count) * 100) %>%                             # calculate the coefficient of variation 
  filter(swana !="not swana") %>% 
  rename("race" = swana)


### Summarize by BIPOC
discon_bipoc_county <- 
  survey_design_county %>%
  group_by(county, bipoc, connected) %>%   
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
    rate_cv = ((rate_moe/1.645)/rate)*100,
    count_moe = count_se*1.645, # calculate moe for numerator count based on se provided by the output
    count_cv = ((count_moe/1.645)/count) * 100) %>%                             
  filter(bipoc !="not bipoc") %>% 
  rename("race"=bipoc)

# review
head(discon_bipoc_county)

# Summarize data for total pop (raceeth = total)
discon_pop_county <- 
  survey_design_county %>%
  group_by(county, connected) %>%  
  summarise(
    count = survey_total(na.rm = T), # get the (survey weighted) count for the numerators
    rate = survey_mean() # calc the % / get the (survey weighted) proportion for the numerator
  ) %>%        
  
  left_join(survey_design_county %>%                                           
              group_by(county) %>%                  
              summarise(pop = survey_total(na.rm = T))) %>%            
  mutate(
    race = "total",
    rate = rate*100,                                                 
    rate_moe = rate_se*1.645*100,   
    rate_cv = ((rate_moe/1.645)/rate)*100,
    count_moe = count_se*1.645, # calculate moe for numerator count based on se provided by the output
    count_cv = ((count_moe/1.645)/count) * 100)  # calculate cv for numerator count




## combine race/eth estimates with total

discon_county<-  
  bind_rows(discon_race_county %>% filter(race != "white" & race != "asian" & race != "black" & 
                                            race != "other" & race != "twoormor" & race != "nh_pacisl" & 
                                            race != "nh_aian" & race != "nh_swana" & race!='aian' & race!='pacisl'), 
            discon_lat_county, 
            discon_aian_county, 
            discon_pacisl_county, 
            discon_swana_county, 
            discon_bipoc_county, 
            discon_pop_county) %>% 
  filter(connected =="connected")


#### STEP 2B: Calculate disparity index (ala RC) by race ####

discon_county_id <-
  discon_county %>% ungroup() %>%
  mutate(best=max(rate[race!='total' & race!='bipoc'],na.rm=T),asbest="max",#create columns for best rate and as best treatment, ignoring total and bipoc
         values_count = sum(!is.na(race) & race!='total' & race!="bipoc")) %>% # remove total and bipoc from values count
  mutate(diff = ifelse(race == "total" | race == "bipoc", NA, abs(best-rate))) %>%
  mutate(index_of_disparity = ((sum(diff, na.rm=T)/best/(values_count - 1))*100)) %>% # use the best value identified above
  #select just the columns needed 
  select(county, race, count, count_cv, rate, rate_cv, asbest, values_count, best, diff, index_of_disparity)


#### STEP 2C: Upload pyd_connectedyouth_race to postgres ####

# Write connected youth table to bold vision database
dbWriteTable(con, c("bv_2023", "pyd_connectedyouth_subgroup"), discon_county_id,
             overwrite = TRUE, row.names = FALSE,
             field.types = c(
               county = "varchar",
               race = "varchar",
               count ="integer",
               count_cv = "numeric",
               rate = "numeric",
               rate_cv = "numeric",
               asbest = "varchar",
               values_count = "numeric",
               best = "varchar",
               diff= "numeric",
               index_of_disparity= "numeric"
             ))


# write comment to table

dbSendQuery(con, 
            "COMMENT ON TABLE bv_2023.pyd_connectedyouth_subgroup IS 'Table for Connected Youth in LA County. 
           Connected youth are people between the ages of 14 and 24 years who are enrolled in school and/or are employed. 
Disconnected youth is defined as people between the ages of 14 and 24 years who are not enrolled in school and are not employed nor in the labor force. Data source: PUMS 2021 5-Year Estimate.
Race codes for AIAN and pacisl are All AIAN and All pacisl (multiracial + latino inclusive).
This table was prepared in the R script W:\\Project\\OSI\\Bold Vision\\BV 2023\\R\\boldvision_22_23\\Positive Youth Development\\pyc_connectedyouth.R
QA doc:  W:\\Project\\OSI\\Bold Vision\\BV 2023\\Documentation\\QA_connectedyouth.docx';

COMMENT ON COLUMN bv_2023.pyd_connectedyouth_subgroup.diff is 'The difference between the best rate and the respective race group. total and BIPOC not included in calculation.';
COMMENT ON COLUMN bv_2023.pyd_connectedyouth_subgroup.index_of_disparity is 'The index of disparity by race with the best rate of the subgroups.';
COMMENT ON COLUMN bv_2023.pyd_connectedyouth_subgroup.race is 'Race/ethnicity category. AIAN and pacisl are multiracial latino-inclusive (All AIAN, All pacisl).';
COMMENT ON COLUMN bv_2023.pyd_connectedyouth_subgroup.count is 'Count of youth who are connected/disconnected (see column var for which category)';
COMMENT ON COLUMN bv_2023.pyd_connectedyouth_subgroup.count_cv is 'CV (coefficient of variation) for count of youth who are connected/disconnected (see column var for which category). Values over .4, i.e. 40%, are generally considered to be unstable estimates.';
COMMENT ON COLUMN bv_2023.pyd_connectedyouth_subgroup.rate is 'Percentage youth of youth who are connected/disconnected (see column var for which category)';
COMMENT ON COLUMN bv_2023.pyd_connectedyouth_subgroup.rate_cv is 'CV (coefficient of variation) for percentage of youth who are connected/disconnected (see column var for which category). Values over .4, i.e. 40%, are generally considered to be unstable estimates.';
COMMENT ON COLUMN bv_2023.pyd_connectedyouth_subgroup.county is 'County FIPS';"
)

#### STEP 3A: Calculate the percentage of youth (ages 14-24) who are enrolled in school and/or employed by SPA ####

#use crosswalk to get data by spa
crosswalk <- st_read(con, query = "select * from bv_2023.crosswalk_puma_spas_2023_v2")

ppl$puma_id <- as.factor(ppl$puma)

ppl_spa <- right_join(ppl, crosswalk, by=c("puma_id" = "puma_id"))    # specify the field join


survey_design_spa <- ppl_spa %>%      # Use the filtered data (only 14-24 years)
  as_survey_rep(
    variables = c(spa_id, spa_name, connected),   # select grouping variables
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
discon_spa <- 
  survey_design_spa %>%
  group_by(spa_id, spa_name, connected) %>%   
  summarise(
    count = survey_total(na.rm = T), # get the (survey weighted) count for the numerators
    rate = survey_mean() # calc the % / get the (survey weighted) proportion for the numerator
  ) %>%            # get the (survey weighted) count 
  
  left_join(survey_design_spa %>%                               # left join in the denominators
              group_by(spa_id) %>%                           # group by race/eth and puma
              summarise(pop = survey_total(na.rm = T))) %>%                 # get the weighted total
  mutate(
    rate = rate*100,                                                # calc the % 
    rate_moe = rate_se*1.645*100,   # calculate the derived margin of error for the percentage
    rate_cv = ((rate_moe/1.645)/rate)*100,
    count_moe = count_se*1.645, # calculate moe for numerator count based on se provided by the output
    count_cv = ((count_moe/1.645)/count) * 100) %>%
  filter(connected =="connected")



#### STEP 3B: Calculate disparity index (ala RC) by SPA ####

discon_spa_id <-
  discon_spa %>% ungroup() %>%
  mutate(best=max(rate,na.rm=T), asbest="max",#create columns for best rate and as best treatment, ignoring total and bipoc
         values_count = sum(!is.na(spa_id))) %>% # remove total and bipoc from values count
  mutate(diff = abs(best-rate)) %>%
  mutate(index_of_disparity = ((sum(diff, na.rm=T)/best/(values_count - 1))*100)) %>% # use the best value identified above
  #select just the columns needed 
  select(spa_id, spa_name, count, count_cv, rate, rate_cv, asbest, values_count, best, diff, index_of_disparity)


#### STEP 3C: Upload pyd_connectedyouth_region to postgres ####

# Write connected youth table to bold vision database
dbWriteTable(con, c("bv_2023", "pyd_connectedyouth_region"), discon_spa_id,
             overwrite = TRUE, row.names = FALSE,
             field.types = c(
               spa_id = "numeric",
               spa_name = "varchar",
               count ="integer",
               count_cv = "numeric",
               rate = "numeric",
               rate_cv = "numeric",
               asbest = "varchar",
               values_count = "numeric",
               best = "varchar",
               diff= "numeric",
               index_of_disparity= "numeric"
             ))


# write comment to table

dbSendQuery(con, 
            "COMMENT ON TABLE bv_2023.pyd_connectedyouth_region IS 'Table for Connected Youth in LA County by SPA. 
           Connected youth are people between the ages of 14 and 24 years who are enrolled in school and/or are employed. 
Disconnected youth is defined as people between the ages of 14 and 24 years who are not enrolled in school and are not employed nor in the labor force. Data source: PUMS 2021 5-Year Estimate.
This table was prepared in the R script W:\\Project\\OSI\\Bold Vision\\BV 2023\\R\\boldvision_22_23\\Positive Youth Development\\pyc_connectedyouth.R
QA doc:  W:\\Project\\OSI\\Bold Vision\\BV 2023\\Documentation\\QA_connectedyouth.docx';

COMMENT ON COLUMN bv_2023.pyd_connectedyouth_region.diff is 'The difference between the best rate and the respective SPA district. total and BIPOC not included in calculation.';
COMMENT ON COLUMN bv_2023.pyd_connectedyouth_region.index_of_disparity is 'The index of disparity by SPA with the best rate of the regions.';
COMMENT ON COLUMN bv_2023.pyd_connectedyouth_region.spa_name is 'Name of SPA region';
COMMENT ON COLUMN bv_2023.pyd_connectedyouth_region.count is 'Count of youth who are connected/disconnected (see column var for which category)';
COMMENT ON COLUMN bv_2023.pyd_connectedyouth_region.count_cv is 'CV (coefficient of variation) for count of youth who are connected/disconnected (see column var for which category). Values over .4, i.e. 40%, are generally considered to be unstable estimates.';
COMMENT ON COLUMN bv_2023.pyd_connectedyouth_region.rate is 'Percentage youth of youth who are connected/disconnected (see column var for which category)';
COMMENT ON COLUMN bv_2023.pyd_connectedyouth_region.rate_cv is 'CV (coefficient of variation) for percentage of youth who are connected/disconnected (see column var for which category). Values over .4, i.e. 40%, are generally considered to be unstable estimates.';
COMMENT ON COLUMN bv_2023.pyd_connectedyouth_region.spa_id is 'LA County Service Planning Area (SPA) ID Number';"
)

dbDisconnect(conn = con)
dbDisconnect(conn = con_rda)

