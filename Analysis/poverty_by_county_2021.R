# Households with children under 5 paying 30%+ of their income on rent
# as percentage of all LA County households under 5

# Data Dictionary: https://www2.census.gov/programs-surveys/acs/tech_docs/pums/data_dict/PUMS_Data_Dictionary_2022.pdf

library(tidyverse)
library(data.table)
library(readxl)
library(tidycensus)
library(srvyr)
library(stringr)

#SOURCE from the script that has: styling, packages, dbconnection, colors
source("W:\\RDA Team\\R\\credentials_source.R")


#### Step 1: load the data ####

# PUMS Data
root <- "W:/Data/Demographics/PUMS/"
indicator_name <- "poverty"
data_type <- "county" #race, spa, county


###### function for this script's mapply --- will change per script

  
  # Load the people PUMS data
  people <- fread(paste0(root, "CA_2021/psam_h06.csv"), header = TRUE, data.table = FALSE,
                  # people <- fread(paste0(root, pca), header = TRUE, data.table = FALSE,
                  colClasses = list(character = c("PUMA", "ANC1P", "ANC2P", "HISP", "RAC1P", "RAC2P", "RAC3P", "RACAIAN", "RACPI", "RACNH")))
  
  
  # Load the housing PUMS data
  housing <- fread(paste0(root, "CA_2021/psam_h06.csv"), header = TRUE, data.table = FALSE,
                   # housing <- fread(paste0(root, hca), header = TRUE, data.table = FALSE,                 
                   colClasses = list(character = c("PUMA")))
####  Step 2: filter households eligible for calculation  #### 

## Select LA County people
eligible_hhs <- people %>% 
  
  #filtering for universe and LA county
  filter(!is.na(POVPIP) & grepl('037', PUMA))   %>% 
  
  # join their housing info  
  left_join(housing %>% filter(grepl('037', PUMA)), 
            by = c("SERIALNO", "PUMA")) %>%
  
  #remove records with no weights
  filter(!is.na(WGTP)) %>%
  
  #filter for age 0-5 and select distinct households
  filter(AGEP < 5) %>% distinct(SERIALNO, .keep_all = TRUE)


####  Step 3: set up and run survey and format  #### 

# add geoid and indicator
eligible_hhs$geoid <- "037"
eligible_hhs$indicator=(ifelse(eligible_hhs$POVPIP <= 100, "at or below poverty", "above poverty"))

weight <- 'WGTP' # using WGTP b/c calculating percentage of rent-burdened households


repwlist = rep(paste0("WGTP", 1:80))


# create survey design

hh_geo <- eligible_hhs %>%
  as_survey_rep(
    variables = c(geoid, indicator),   # dplyr::select grouping variables
    weights = weight,                       #  weight
    repweights = repwlist,                  # list of replicate weights
    combined_weights = TRUE,                # tells the function that replicate weights are included in the data
    mse = TRUE,                             # tells the function to calc mse
    type="other",                           # statistical method
    scale=4/80,                             # scaling set by ACS
    rscale=rep(1,80)                        # setting specific to ACS-scaling
  )

###### TOTAL ######
total <- hh_geo  %>%
  group_by(geoid,indicator) %>%   # group by race cat
  summarise(
    num = survey_total(na.rm=T), # get the (survey weighted) count for the numerators
    rate = survey_mean()) %>%        # get the (survey weighted) proportion for the numerator
  left_join(hh_geo %>%                                        # left join in the denominators
              group_by(geoid) %>%                                     # group by geo
              summarise(pop = survey_total(na.rm=T))) %>%              # get the weighted total for overall geo
  mutate(rate=rate*100,
         rate_moe = rate_se*1.645*100,    # calculate the margin of error for the rate based on se
         rate_cv = ((rate_moe/1.645)/rate) * 100, # calculate cv for rate
         count_moe = num_se*1.645, # calculate moe for numerator count based on se
         count_cv = ((count_moe/1.645)/num) * 100)  # calculate cv for numerator count


# select burdened and not NA
d_long <- total %>% filter(indicator == "at or below poverty" & !is.na(geoid))

# make data frame
d_long <- as.data.frame(d_long)#### Step 4: Repeat steps 2 and 3 above without the 0-5 filter ####

## Select LA County people
eligible_hhs2 <- people %>% 
  
  #filtering for universe and LA county
  filter(!is.na(POVPIP) & grepl('037', PUMA))   %>% 
  
  # join their housing info  
  left_join(housing %>% filter(grepl('037', PUMA)), 
            by = c("SERIALNO", "PUMA")) %>%
  
  #remove records with no weights
  filter(!is.na(WGTP)) %>%
  
  #filter for age 0-5 and select distinct households
  distinct(SERIALNO, .keep_all = TRUE)

# add geoid and indicator
eligible_hhs2$geoid <- "037"
eligible_hhs2$indicator=(ifelse(eligible_hhs2$POVPIP <= 100, "at or below poverty", "above poverty"))

weight <- 'WGTP' # using WGTP b/c calculating percentage of rent-burdened households

if (year %in% c(2012,2013,2014,2015,2016)){
  repwlist = rep(paste0("wgtp", 1:80))
} else if (year %in% c(2017,2018,2019,2021)) {
  repwlist = rep(paste0("WGTP", 1:80))
}

# create survey design

hh_geo <- eligible_hhs2 %>%
  as_survey_rep(
    variables = c(geoid, indicator),   # dplyr::select grouping variables
    weights = weight,                       #  weight
    repweights = repwlist,                  # list of replicate weights
    combined_weights = TRUE,                # tells the function that replicate weights are included in the data
    mse = TRUE,                             # tells the function to calc mse
    type="other",                           # statistical method
    scale=4/80,                             # scaling set by ACS
    rscale=rep(1,80)                        # setting specific to ACS-scaling
  )

###### TOTAL ######
total <- hh_geo  %>%
  group_by(geoid,indicator) %>%   # group by race cat
  summarise(
    num = survey_total(na.rm=T), # get the (survey weighted) count for the numerators
    rate = survey_mean()) %>%        # get the (survey weighted) proportion for the numerator
  left_join(hh_geo %>%                                        # left join in the denominators
              group_by(geoid) %>%                                     # group by geo
              summarise(pop = survey_total(na.rm=T))) %>%              # get the weighted total for overall geo
  mutate(rate=rate*100,
         rate_moe = rate_se*1.645*100,    # calculate the margin of error for the rate based on se
         rate_cv = ((rate_moe/1.645)/rate) * 100, # calculate cv for rate
         count_moe = num_se*1.645, # calculate moe for numerator count based on se
         count_cv = ((count_moe/1.645)/num) * 100)  # calculate cv for numerator count

# select burdened and not NA
d_long2 <- total %>% filter(indicator == "at or below poverty" & !is.na(geoid))

# make data frame
d_long2 <- as.data.frame(d_long2)

# bind both data frames in final
d_final <- rbind(d_long, d_long2)

# Push to postgres