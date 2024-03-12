#data, cv, best, difference, index of disparity
# Civic Engagement Supplement
# Youth Civic Engagement: Participated in politics somehow/engaged civically (pool categories)

#install packages if not already installed
list.of.packages <- c("ipumsr","data.table","dplyr","tidycensus","forcats","RPostgreSQL", "sf", "stringr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#load packages
library(ipumsr)
library(data.table)
library(dplyr)
library(tidycensus)
library(forcats)
library(RPostgreSQL)
library(sf)
library(tidyverse)
library(here)
library(stringr)
library(tigris)
options(scipen = 100) # disable scientific notation

###### SET UP WORKSPACE #######
# create connection for rda database
source("W:\\RDA Team\\R\\credentials_source.R")
conBV <- connect_to_db("bold_vision")
conRDA <- connect_to_db("rda_shared_data")


####### Load data ####
# save root director
root_bv =  "W:/Project/OSI/Bold Vision/BV 2023/Data/Youth Power/Youth Civic Engagement/"
root_ipums =  "W:/Project/OSI/Bold Vision/BV 2023/Data/Youth Power/Youth Civic Engagement/source/IPUMS/"

ddi <- read_ipums_ddi(paste0(root_ipums, "cps_00005.xml"))
data <- read_ipums_micro(ddi)


# import the list of "Individual Principle City" codes-- this is as sub county as it gets with cps supplements
# https://cps.ipums.org/cps/codes/individcc_2004onward_codes.shtml
cities = read.csv(paste0(root_ipums, "PrincipleCities_IPUMS.csv"), header = TRUE, stringsAsFactors = FALSE)
cities$INDIVIDCC_f = as.factor(as.character(cities$INDIVIDCC))


####### Clean Data / Data Prep #######

# limit to CA
data <- data[data$STATEFIP ==6, ]
head(data)
class(data)
table(data$YEAR)


#GVF Parameters -----
# Generalized Variance Function (GVF) parameters (2013 CE supplement) (Table 4. Parameters for Computation of Standard Errors for Civic Engagement Characteristics: November 2013)
#       total            / White           / Black           / API, aian, NHOPI       / latino
#      # a         b     / a        b      /  a        b     / a         b     / a          b
#      # -0.000019 4687  /  -0.000019 4687 / -0.000130 6733  / -0.000318 6733  /-0.000300 11347

# Generalized Variance Function (GVF) parameters (2017 CE supplement) (Table 7. Parameters for Computation of Standard Errors for Volunteering and Civic Engagement Characteristics: November 2017)
#       total            / White           / Black           / API, aian, NHOPI       / latino
#      # a         b     / a        b      /  a        b     / a         b     / a          b
#      # -0.000030 7599  /  -0.000030 6673 / -0.000123 6930  / -0.000306 7349  /-0.000198  8,245

# Generalized Variance Function (GVF) parameters (2019 CE supplement) (Table 7. Parameters for Computation of Standard Errors for Volunteering and Civic Life Characteristics: September 2019)
#       total            / White           / Black           / API, aian, NHOPI       / latino
#      # a         b     / a        b      /  a        b     / a         b     / a          b
#      # -0.000033 8590  /  -0.000033 7469 / -0.000138 8067  / -0.000332 8440  /-0.000233 10191

# Generalized Variance Function (GVF) parameters (2021 CE supplement) (Table 7. Parameters for Computation of Standard Errors for Volunteering and Civic Life Characteristics: September 2021)
#       total            / White           / Black           / API, aian, NHOPI       / latino
#      # a         b     / a        b      /  a        b     / a         b     / a          b
#      # -0.000036 9427  /  -0.000039 8847 / -0.000157 9395  / -0.000380 9885  /-0.000269 12143


#there doesn't seem to be data for 2012,2014,2015,2016,2018,2020,2022,2023 and the last version did not include 2008 or 2009 so I am just going to include years (2013, 2017, 2019, and 2021) There is data for 2010,2011 but taking it out in favor of more recent data
#Calculating GVF parameters
# `a` parameter formula was (sum of a / num years) * (num years + correlation coefficient * (3 * (num years)-4)/(num years^2))
total_parameter_a <- ((-0.000019-0.000030-0.000033-0.000036)/4)*(4+0*(3*(4)-4)/(4^2))
white_parameter_a <- ((-0.000019-0.000030-0.000033-0.000039)/4)*(4+0*(3*(4)-4)/(4^2))
black_parameter_a <- ((-0.000130-0.000123-0.000138-0.000157)/4)*(4+0*(3*(4)-4)/(4^2))
asian_pacisl_aian_parameter_a <- ((-0.000318-0.000306-0.000332-0.000380)/4)*(4+0*(3*(4)-4)/(4^2))
latino_parameter_a <- ((-0.000300-0.000198-0.000233-0.000269)/4)*(4+0*(3*(4)-4)/(4^2))
bipoc_parameter_a <- ((-0.000318-0.000306-0.000332-0.000380)/4)*(4+0*(3*(4)-4)/(4^2)) #API, aian, pacisl are the most conservative parameters so going to use that one for bipoc as well

# `b` parameter formula was (sum of b / num years) * (num years + correlation coefficient * (3 * (num years)-4)/num years)
total_parameter_b <- ((4687+7599+8590+9427)/4)*(4+0*(3*(4)-4)/4)
white_parameter_b <- ((4687+6673+7469+8847)/4)*(4+0*(3*(4)-4)/4)
black_parameter_b <- ((6733+6930+8067+9395)/4)*(4+0*(3*(4)-4)/4)
asian_pacisl_aian_parameter_b <- ((6733+7349+8440+9885)/4)*(4+0*(3*(4)-4)/4)
latino_parameter_b <- ((11347+8245+10191+12143)/4)*(4+0*(3*(4)-4)/4)
bipoc_parameter_b <- ((6733+7349+8440+9885)/4)*(4+0*(3*(4)-4)/4) #API, aian, pacisl are the most conservative parameters so going to use that one for bipoc as well

# GVF Pooled Parameters (2013-2021) calculated above (lines 55-93)
#       total            / White           / Black           / API, aian, pacisl       / latino
#      # a         b     / a        b      /  a        b     / a         b     / a          b
#      # -0.000118 30303  /  -0.000121 27676 / -0.000548 31125  / -0.001336 32407  /-0.001 41926



####### CODE DATA
# CONVERT LABELS TO FACTORS
cps_data <-
  data %>%
  mutate(STATE_factor = as_factor(lbl_clean(STATEFIP)), #lbl_clean() removes labels that do not appear in the data. When converting labelled values to a factor, this avoids the creation of additional factor levels. From the IPUMS package
         COUNTY_f = as_factor(lbl_clean(COUNTY)),
         METFIPS_f = as_factor(lbl_clean(METFIPS)),
         METAREA_f = as_factor(lbl_clean(METAREA)),
         INDIVIDCC_f = as_factor(lbl_clean(INDIVIDCC)),
         AGE_f = as_factor(lbl_clean(AGE)),
         RACE_f = as_factor(lbl_clean(RACE)),
         HISPAN_f = as_factor(lbl_clean(HISPAN)),
         CITIZEN_f = as_factor(lbl_clean(CITIZEN)),
         CEBOYCOTT_f = as_factor(lbl_clean(CEBOYCOTT)),
         CECOMSERV_f = as_factor(lbl_clean(CECOMSERV)),
         CEORGCIV_F = as_factor(lbl_clean(CEORGCIV)),
         CEORGCOM_f = as_factor(lbl_clean(CEORGCOM)),
         CEPOLCONV_f = as_factor(lbl_clean(CEPOLCONV)),
         CEWEBOPIN_f = as_factor(lbl_clean(CEWEBOPIN)),
         CEPUBOFF_f = as_factor(lbl_clean(CEPUBOFF)),
         CEVOTEFREQ_f = as_factor(lbl_clean(CEVOTEFREQ)))

table(cps_data$COUNTY, useNA="always")
table(cps_data$CEBOYCOTT_f, useNA = "always")
table(cps_data$CECOMSERV_f, useNA = "always")


##### RACE AND ETH CODING ####
cps_data$latino <-  ifelse(cps_data$HISPAN_f != "Not Hispanic", "latino", "not latino")

# add category for aian - aian in all combinations
cps_data$aian <- ifelse(cps_data$RACE_f %like% "American Indian" | 
                          cps_data$RACE_f %like% "Aleut" | 
                          cps_data$RACE_f %like% "Eskimo", "aian", "not aian")

# add category for All pacisl - pacisl in all combinations
cps_data$pacisl <- ifelse(cps_data$RACE_f %like% "Pacific Islander" | 
                          cps_data$RACE_f %like% "Hawaiian", "pacisl", "not pacisl")

cps_data$bipoc <- ifelse(cps_data$HISPAN_f != "Not Hispanic" | cps_data$RACE_f %like% "American Indian" |
                           cps_data$RACE_f %like% "Aleut" | cps_data$RACE_f %like% "Eskimo" |
                           cps_data$RACE_f %like% "Pacific Islander" | cps_data$RACE_f %like% "Hawaiian" |
                           cps_data$RACE_f %like% "Black" |
                           cps_data$RACE_f %like% "Asian" , 
                         "bipoc", "not bipoc")

table(cps_data$bipoc, useNA="always")


# code NH race
cps_data$race <- ifelse(
  # NH Black
  cps_data$RACE_f =="Black" & cps_data$latino =="not latino", "nh_black",
  
  # NH Asian
  ifelse(cps_data$RACE_f == "Asian only" & cps_data$latino =="not latino", "nh_asian",
         
  # NH White
  ifelse(cps_data$RACE_f == "White" & cps_data$latino =="not latino", "nh_white",

                       
  # everything else is nh_twoormor (this is alone or in combo including latino)
  ifelse(!cps_data$RACE_f %in% c("White", "Black", "American Indian/Aleut/Eskimo", "Asian only", "Hawaiian/Pacific Islander only") & cps_data$latino =="not latino", "nh_twoormor",  NA))))

                              
# cps_data <- cps_data %>%drop_na(race) #I wonder if this line is causing the weird errors later below commenting out for now

 
# review
# table(cps_data$race, cps_data$latino, useNA = "always")

#check that the race codes are accurate
# races_qa <- cps_data %>% select(HISPAN_f, RACE_f, race,latino, aian,pacisl, bipoc)
# View(races_qa)

# cps_data
#####################################################################
####### INDICATOR: CIVIC PARTICIPATION IN THE COMMUNITY #######
#####################################################################

##### Code civic engagement participation ####
# Contacted a public official
table(cps_data$CEPUBOFF_f, cps_data$YEAR, useNA = "always")

# Frequency expressing opinion on the internet
table(cps_data$CEWEBOPIN_f, cps_data$YEAR,  useNA = "always") # not  2010 or 2017? 

# Frequency discussing politics with friends or family
table(cps_data$CEPOLCONV_f, cps_data$YEAR,  useNA = "always")

# Participated in school, neighborhood or community association
table(cps_data$CEORGCOM_f, cps_data$YEAR,  useNA = "always") # not 2017?

# Participated in service or civic organization
table(cps_data$CEORGCIV_F, cps_data$YEAR,  useNA = "always") # not 2017?

# Served on committee or as officer of organization
table(cps_data$CECOMSERV_f, cps_data$YEAR,  useNA = "always") # not 2017?

# Bought based on political views or participated in boycott
table(cps_data$CEBOYCOTT_f, cps_data$YEAR,  useNA = "always")


# code participation -- count any of these as participation. 
## COUNT only samples in all years
cps_data$civic_engage <-  
  ifelse( 
    (is.na(cps_data$CEPUBOFF_f) | cps_data$CEPUBOFF_f !="Yes") & 
      (is.na(cps_data$CEBOYCOTT_f) | cps_data$CEBOYCOTT_f !="Yes") & 
      (is.na(cps_data$CEPOLCONV_f) | !cps_data$CEPOLCONV_f %in% c("Once a month", "A few times a month", "A few times a week", "Basically every day")),
    "does not engage", "civically engaged")


# make universe flag
# count only the 3 indicators that are in all years
cps_data$civic_engage_univ <- ifelse(cps_data$CEPUBOFF_f =="NIU" & cps_data$CEBOYCOTT_f =="NIU" & 
                                       cps_data$CEPOLCONV_f =="NIU", "Not in universe", "in universe")


table(cps_data$civic_engage_univ, cps_data$civic_engage, useNA = "always")



##### COUNTY: People who participate in some way #####
ce_county <- 
  
  # by race
  cps_data %>%
  filter(YEAR %in% c(2013, 2017, 2019, 2021) &  COUNTY_f != "Not identified"  & (AGE >= 18 & AGE <= 29) & #there doesn't seem to be data for 2012,2014,2015,2016,2018,2020,2022,2023 and the last version did not include 2008 or 2009 so I am just going to include years (2010, 2011, 2013, 2017, 2019, and 2021)
           
           !is.na(race)) %>%
  group_by(COUNTY_f, race, civic_engage) %>%
  summarize( 
    estSAMP = sum(CESUPPWT)) %>%
  mutate_if(is.factor,
            fct_na_value_to_level, level = "Missing Data") %>% #message said that fct_explicit_na() was depreciated in forcats 1.0 and to use fct_na_value instead
  
  # add in latino population
  bind_rows(                   
    cps_data %>% 
      filter(YEAR %in% c(2013, 2017, 2019, 2021) & COUNTY_f != "Not identified"  & (AGE >= 18 & AGE <= 29) &  
               latino =="latino") %>%
      group_by(COUNTY_f, civic_engage) %>%
      summarize(
        race = "latino",
        estSAMP = sum(CESUPPWT)) %>%
      mutate_if(is.factor,
                fct_na_value_to_level, level = "Missing Data")) %>%
  
  # add in aian, alone and in combination population
  bind_rows(                   
    cps_data %>% 
      filter(YEAR %in% c(2013, 2017, 2019, 2021) & COUNTY_f != "Not identified"  & (AGE >= 18 & AGE <= 29) &  
               aian =="aian") %>%
      group_by(COUNTY_f, civic_engage) %>%
      summarize(
        race = "aian",
        estSAMP = sum(CESUPPWT)) %>%
      mutate_if(is.factor,
                fct_na_value_to_level, level = "Missing Data")) %>%
  
  # add in pacisl population
  bind_rows(                   
    cps_data %>% 
      filter(YEAR %in% c(2013, 2017, 2019, 2021) & COUNTY_f != "Not identified"  & (AGE >= 18 & AGE <= 29) &  
               pacisl =="pacisl") %>%
      group_by(COUNTY_f, civic_engage) %>%
      summarize(
        race = "pacisl",
        estSAMP = sum(CESUPPWT)) %>%
      mutate_if(is.factor,
                fct_na_value_to_level, level = "Missing Data")) %>%
  
  # add in bipoc population
  bind_rows(                   
    cps_data %>% 
      filter(YEAR %in% c(2013, 2017, 2019, 2021) & COUNTY_f != "Not identified"  & (AGE >= 18 & AGE <= 29) &  
               bipoc =="bipoc") %>%
      group_by(COUNTY_f, civic_engage) %>%
      summarize(
        race = "bipoc",
        estSAMP = sum(CESUPPWT)) %>%
      mutate_if(is.factor,
                fct_na_value_to_level, level = "Missing Data")) %>%
  
  # add in the vote total for the total population
  bind_rows(                   
    cps_data %>% 
      filter(YEAR %in% c(2013, 2017, 2019, 2021) & COUNTY_f != "Not identified"  & (AGE >= 18 & AGE <= 29) ) %>%
      group_by(COUNTY_f, civic_engage) %>%
      summarize(
        race = "total",
        estSAMP = sum(CESUPPWT)) %>% 
      mutate_if(is.factor,
                fct_na_value_to_level, level = "Missing Data")) %>% 
  
  mutate(                 
    
    # estimate the standard error using GVF parameters -- general SE parameters by race attribute. Note diff years use diff parameters 
    estSAMP_se = 
      #### add a filter by year for this section
      # Calc SE for 2013 CE Supplement
      ifelse( race %in% c("aian, non-latino", "aian", "nh_asian", "pacisl","nh_twoormor"),  
              sqrt((asian_pacisl_aian_parameter_a * (estSAMP^2)) + (asian_pacisl_aian_parameter_b*estSAMP)),  # cps says let two or more races use the same as aian etc
              ifelse( race == "total", sqrt((total_parameter_a * (estSAMP^2)) + (total_parameter_b*estSAMP)),
                      ifelse( race =="nh_white", sqrt((white_parameter_a * (estSAMP^2)) + (white_parameter_b*estSAMP)),
                              ifelse( race =="bipoc", sqrt((bipoc_parameter_a * (estSAMP^2)) + (bipoc_parameter_b*estSAMP)),
                                      ifelse( race == "nh_black", sqrt((black_parameter_a * (estSAMP^2)) + (black_parameter_b*estSAMP)),
                                              ifelse( race == "latino", sqrt((latino_parameter_a * (estSAMP^2)) + (latino_parameter_b*estSAMP)), NA)))))),
    
    
    # Calc CV and final estSAMP count
    est_cv = estSAMP_se/estSAMP,
    est = estSAMP/4) #divide by 6 if using 6 years

head(ce_county)


##### COUNTY: Population Base ##### 
ce_pop_county <- 
  
  # by race
  cps_data %>%
  filter(YEAR %in% c(2013, 2017, 2019, 2021) &  COUNTY_f != "Not identified"  & civic_engage_univ != "Not in universe"  & (AGE >= 18 & AGE <= 29) & 
           !is.na(race)) %>%
  group_by(COUNTY_f, race) %>%
  summarize( 
    eligSAMP = sum(CESUPPWT)) %>%
  mutate_if(is.factor,
            fct_na_value_to_level, level = "Missing Data") %>%
  
  # add in latino population
  bind_rows(                   
    cps_data %>% 
      filter(YEAR %in% c(2013, 2017, 2019, 2021) & COUNTY_f != "Not identified" & civic_engage_univ != "Not in universe" & (AGE >= 18 & AGE <= 29) &
               latino =="latino") %>%
      group_by(COUNTY_f) %>%
      summarize(
        race = "latino",
        eligSAMP = sum(CESUPPWT)) %>%
      mutate_if(is.factor,
                fct_na_value_to_level, level = "Missing Data")) %>%
  
  # add in aian population
  bind_rows(                   
    cps_data %>% 
      filter(YEAR %in% c(2013, 2017, 2019, 2021) & COUNTY_f != "Not identified" & civic_engage_univ != "Not in universe" & (AGE >= 18 & AGE <= 29) &
               aian =="aian") %>%
      group_by(COUNTY_f) %>%
      summarize(
        race = "aian",
        eligSAMP = sum(CESUPPWT)) %>%
      mutate_if(is.factor,
                fct_na_value_to_level, level = "Missing Data")) %>%
  
  # add in pacisl population
  bind_rows(                   
    cps_data %>% 
      filter(YEAR %in% c(2013, 2017, 2019, 2021) & COUNTY_f != "Not identified" & civic_engage_univ != "Not in universe" & (AGE >= 18 & AGE <= 29) &
               pacisl =="pacisl") %>%
      group_by(COUNTY_f) %>%
      summarize(
        race = "pacisl",
        eligSAMP = sum(CESUPPWT)) %>%
      mutate_if(is.factor,
                fct_na_value_to_level, level = "Missing Data")) %>%
  
  # add in bipoc population
  bind_rows(                   
    cps_data %>% 
      filter(YEAR %in% c(2013, 2017, 2019, 2021) & COUNTY_f != "Not identified" & civic_engage_univ != "Not in universe" & (AGE >= 18 & AGE <= 29) &
               bipoc =="bipoc") %>%
      group_by(COUNTY_f) %>%
      summarize(
        race = "bipoc",
        eligSAMP = sum(CESUPPWT)) %>%
      mutate_if(is.factor,
                fct_na_value_to_level, level = "Missing Data")) %>%
  
  
  # add in the vote total for the total population
  bind_rows(                   
    cps_data %>% 
      filter(YEAR %in% c(2013, 2017, 2019, 2021) & COUNTY_f != "Not identified" & civic_engage_univ != "Not in universe"  & (AGE >= 18 & AGE <= 29) ) %>%
      group_by(COUNTY_f) %>%
      summarize(
        race = "total",
        eligSAMP = sum(CESUPPWT)) %>% 
      mutate_if(is.factor,
                fct_na_value_to_level, level = "Missing Data")) %>% 
  
  mutate(                 
    
    # estimate the standard error using GVF parameters -- general SE parameters by race attribute. Note diff years use diff parameters 
    eligSAMP_se = 
      
      # Calc SE for 2016 (not published for 2018)
      ifelse( race %in% c("aian", "aian, non-latino", "nh_asian", "pacisl","nh_twoormor"),  
              sqrt((asian_pacisl_aian_parameter_a * (eligSAMP^2)) + (asian_pacisl_aian_parameter_b*eligSAMP)),  # cps says let two or more races use the same as aian etc
              ifelse( race =="total", sqrt((total_parameter_a * (eligSAMP^2)) + (total_parameter_b*eligSAMP)),
                      ifelse( race =="nh_white", sqrt((white_parameter_a  * (eligSAMP^2)) + (white_parameter_b*eligSAMP)),
                              ifelse( race =="bipoc", sqrt((bipoc_parameter_a * (eligSAMP^2)) + (bipoc_parameter_b*eligSAMP)),
                                      ifelse( race == "nh_black", sqrt((black_parameter_a * (eligSAMP^2)) + (black_parameter_b*eligSAMP)),
                                              ifelse( race == "latino", sqrt((latino_parameter_a * (eligSAMP^2)) + (latino_parameter_b*eligSAMP)), NA)))))),
    #make parameters a data object ex: parameters == 2016
    # Calc CV and final eligSAMP count
    elig_cv = eligSAMP_se/eligSAMP,
    elig = eligSAMP/4) 

### Join tables to calculate the % of people who have engaged/participated civically in some way ---------
civEng_County <- 
  ce_pop_county  %>% 
  left_join(ce_county) %>%
  
  mutate(
    rate = (est/elig)*100,
    
    rate_se = 
      ifelse(race %in% c("aian", "nh_asian", "pacisl","nh_twoormor"),
             sqrt((asian_pacisl_aian_parameter_b/eligSAMP) * rate * (100-rate)),  # cps says let two or more races use the same as aian etc
             ifelse( race =="total",  sqrt((total_parameter_b/eligSAMP) * rate * (100-rate)), 
                     ifelse( race =="nh_white",  sqrt((white_parameter_b/eligSAMP) * rate * (100-rate)), 
                             ifelse( race =="bipoc",  sqrt((bipoc_parameter_b/eligSAMP) * rate * (100-rate)), 
                                     ifelse(race =="nh_black", sqrt((black_parameter_b/eligSAMP) * rate * (100-rate)),
                                            ifelse( race=="latino",sqrt((latino_parameter_b/eligSAMP) * rate * (100-rate)), NA)))))),
    rate_cv = rate_se/rate*100,
    
    # # reliability threshold #this threshold is too high
    # rate = ifelse(rate_cv > 0.4 | elig < 60, NA, rate)
  ) %>%
  
  filter(civic_engage =="civically engaged" & COUNTY_f=="6037")

civEng_County


##View(civEng_County)
# Export county version to share with partners
write.csv(civEng_County, here("W:/Project/OSI/Bold Vision/BV 2023/Data/Youth Power/Youth Civic Engagement/civicengagement.csv"), row.names = FALSE)

##### Disparity Index ######
# calculate perf and disparity index according to race counts method.
# note I'm following a tutorial I wrote here: https://advancementprojectca.github.io/rda_resources/ACS-Data-Prep-for-Race-Counts.html


# do some data clean up first

# County
# prep -- remove some cols that aren't needed
civEng_County <-  civEng_County[,c(1, 2, 7, 11, 6, 12, 13, 14)]

# rename
colnames(civEng_County) <- c("geoid", "race","category", "count", "pop", "rate", "rate_se", "rate_cv")
civEng_County$geolevel <- "county"


# review
head(civEng_County)
# View(civEng_County)


# difference table: indicator_df is the data frame with the indicator. ma
diff_county <-
  
  civEng_County %>% 
  
  filter(race !="total" | race != "bipoc") %>% 
  group_by(geoid, geolevel) %>% 
  summarize(max = max(rate, na.rm=T),
            values_count = sum(!is.na(rate))) %>% 
  
  # join max and values count to the original df
  right_join(civEng_County, 
             by=c("geoid", "geolevel")) %>% 
  
  # get differences from max rate
  mutate(diff = ifelse(race =="total" | race == "bipoc", NA, abs(max - rate)))

# get the avg and variance of differences, and the index of disparity
id_table_county <- diff_county %>% 
  filter(race !="total") %>% 
  dplyr::group_by(geoid, geolevel) %>% 
  summarize(
    
    # ID: if there are fewer than 2 values count, NULL, otherwise...
    index_of_disparity = ifelse(max(values_count) < 2, NA, 
                                
                                # divide the sum of differences by the maximum value, divided by the values count - 1
                                ((sum(diff, na.rm=T)/max(max))/(max(values_count) - 1))*100))

head(id_table_county)
### finalize table format in a way that will work well for visualization
final_county <-   diff_county %>% 
  full_join(id_table_county,
            by = c("geoid", "geolevel")) 

# format county fips
final_county$geoid <- paste0("0", final_county$geoid)
final_county$NAME <- "Los Angeles County"



##### CITY: People who participate in some way #####
ce_city <- 
  
  cps_data %>%
  filter(YEAR %in% c(2013, 2017, 2019, 2021) &  COUNTY_f==6037 & (AGE >= 18 & AGE <= 29)) %>%
  group_by(COUNTY_f, INDIVIDCC_f, civic_engage) %>%
  summarize(estSAMP = sum(CESUPPWT)) %>%
  mutate_if(is.factor, fct_na_value_to_level, level = "Missing Data") %>%
  
  mutate(                 
    
    # estimate the standard error using GVF parameters -- general SE parameters by geography attribute. Note diff years use diff parameters 
    estSAMP_se = sqrt((total_parameter_a * (estSAMP^2)) + (total_parameter_b*estSAMP)),
    
    # Calc CV and final estSAMP count
    est_cv = estSAMP_se/estSAMP,
    est = estSAMP/4) 

# review
head(ce_city)

# ##### CITY: Population Base ##### 
ce_pop_city <-
  
  cps_data %>%
  filter(YEAR %in% c(2013, 2017, 2019, 2021) &  COUNTY_f==6037  & 
           civic_engage_univ != "Not in universe"  & (AGE >= 18 & AGE <= 29)) %>% 
  group_by(COUNTY_f, INDIVIDCC_f) %>%
  summarize(eligSAMP = sum(CESUPPWT)) %>%
  mutate_if(is.factor, fct_na_value_to_level, level = "Missing Data") %>% 
  
  mutate(
    
    # estimate the standard error using GVF parameters -- general SE parameters by geography attribute. Note diff years use diff parameters
    eligSAMP_se = sqrt((total_parameter_a * (eligSAMP^2)) + (total_parameter_b*eligSAMP)),
    
    # Calc CV and final eligSAMP count
    elig_cv = eligSAMP_se/eligSAMP,
    elig = eligSAMP/4)

# review
head(ce_pop_city)


civEng_City  <-
  ce_pop_city  %>%
  left_join(ce_city) %>%
  
  mutate(
    rate = (est/elig)*100,
    
    rate_se = sqrt((total_parameter_b/eligSAMP) * rate * (100-rate)),
    
    rate_cv = rate_se/rate*100) %>% 
  
  # # reliability threshold #cut for now
  # rate = ifelse(rate_cv > 0.4 | elig < 60, NA, rate)) %>%
  
  # join cities
  filter(INDIVIDCC_f != "Not identified" & civic_engage == "civically engaged") %>%
  full_join(cities %>%
              filter(COUNTY == 6037)) %>% 
  select(COUNTY_f, civic_engage, elig, est, rate, rate_se, rate_cv, City.name) %>% # prep -- remove some cols that aren't needed
  filter(COUNTY_f==6037) %>% 
  rename("county_geoid"="COUNTY_f","category"="civic_engage","pop"="elig", "count"="est", "NAME"="City.name") %>% 
  mutate(geolevel="city")

# review
#View(civEng_City)

#add geoids 
# # load city spatial files (but don't include the spatial part until the very end)
city <- places(state = 'CA', year = 2021, cb = TRUE) %>% 
  select(-c(STATEFP, PLACEFP, PLACENS, AFFGEOID, STUSPS, STATE_NAME, LSAD, ALAND, AWATER))%>% 
  rename("geoid"="GEOID")

citysp <- st_transform(city, 3310) # change projection to 3310
# st_crs(citysp) #check the CRS
civEng_City_sp <- left_join(civEng_City, citysp, by="NAME") %>% 
  filter(NAMELSAD!="Burbank CDP") %>% #Burbank is referring to the city so remove CDP
  ungroup() %>% select(-c(NAMELSAD, geometry)) 

##### Disparity Index ######

# note I'm following a tutorial I wrote here: https://advancementprojectca.github.io/rda_resources/ACS-Data-Prep-for-Race-Counts.html


# difference tables ----
diff_city <- civEng_City_sp %>% 
  
  # filter(race !="total") %>%
  group_by(geolevel) %>%
  summarize(max = max(rate, na.rm=T),
            values_count = sum(!is.na(rate))) %>%
  
  # join max and values count to the original df
  right_join(civEng_City_sp, 
             by=c("geolevel")) %>% 
  
  # get differences from max rate
  mutate(diff = abs(max - rate)) 

#View(diff_city)


# get the avg and variance of differences, and the index of disparity
id_city <- diff_city %>% mutate(
  
  # ID: if there are fewer than 2 values count, NULL, otherwise...
  index_of_disparity = ifelse(max(values_count) < 2, NA, 
                              
                              # divide the sum of differences by the maximum value, divided by the values count - 1
                              ((sum(diff, na.rm=T)/max(max))/(max(values_count) - 1))*100))

# Final tables
final_city <-   diff_city %>% 
  full_join(id_city,
            by = c("county_geoid", "geoid", "geolevel", "category", "pop", "count", "rate", 
                   "rate_se", "rate_cv", "NAME", "values_count", "max", "diff"))



##### Add Geometry and Combine #####

# load county spatial files 
countysp <- counties(state = 'CA', year = 2021, cb = TRUE)%>% select(-c(STATEFP, COUNTYFP, COUNTYNS, NAME, AFFGEOID, STUSPS, STATE_NAME, LSAD, ALAND, AWATER)) %>% 
  filter(GEOID=="06037") %>% rename("NAME"="NAMELSAD")  
countysp <- st_transform(countysp, 3310)
#View(countysp)

final_county_sp <- left_join(final_county, countysp, by="NAME") %>% ungroup() %>% select(-geoid) %>% rename(geoid=GEOID)


# join city shapefiles
final_city_sp <- left_join(final_city, citysp, by=c("NAME","geoid")) %>% 
  filter(NAMELSAD!="Burbank CDP") %>% #Burbank is referring to the city so remove CDP
  ungroup() %>% select(-c(NAMELSAD))



# Reorder the columns and make sure the ID fields match
#county
final_county_sp <- final_county_sp %>% select(-c(geolevel, category)) %>% mutate(asbest = "max") %>% 
  rename("name"="NAME", "best"="max", "subgroup"="race") 

col_order <- c("geoid", "name", "subgroup", "asbest", "best", "values_count", "pop", "count", "rate", "rate_se", "rate_cv", 
               "diff", "index_of_disparity","geometry")

final_county_sp <- final_county_sp[, col_order]
final_county_sp <- st_as_sf(final_county_sp) #object stopped being recognized as an sf object so convert it from a data.frame to an sf object again
final_county_sp <- st_transform(final_county_sp, 3310) #transform the projection again
#st_crs(final_county_sp$geometry) #check the crs again
# #View(final_county_sp)

#city
final_city_sp <- final_city_sp %>% select(-c(county_geoid, geolevel, category)) %>% mutate(asbest = "max") %>% 
  rename("name"="NAME", "best"="max") 

col_order <- c("geoid", "name", "asbest", "best", "values_count", "pop", "count", "rate", "rate_se", "rate_cv", 
               "diff", "index_of_disparity","geometry")

final_city_sp <- final_city_sp[, col_order]
final_city_sp <- st_as_sf(final_city_sp) #object stopped being recognized as an sf object so convert it from a data.frame to an sf object again
final_city_sp <- st_transform(final_city_sp, 3310) #transform the projection again
#st_crs(final_city_sp$geometry) #check the crs again
# #View(final_city_sp)


##### WRITE TABLE TO POSTGRES DB #####


# write County table to bd db
dbWriteTable(conBV, c("bv_2023", "yp_civic_engage_youth_subgroup"), final_county_sp,
             overwrite=TRUE, row.names=FALSE,

             field.types = c(
               geoid= "varchar",
               name = "varchar",
               subgroup = "varchar",
               asbest = "varchar",
               best = "numeric",
               values_count = "integer",
               pop = "numeric",
               count = "numeric",
               rate = "double precision",
               rate_se = "numeric",
               rate_cv ="numeric",
               diff = "numeric",
               index_of_disparity = "numeric",
               geometry = "geometry(MultiPolygon,3310)"
             ))


# send table comments to database
dbGetQuery(conBV,
           "COMMENT ON TABLE bv_2023.yp_civic_engage_youth_subgroup IS 'Table for the share of people who report engaging/participating in politics or their community. Age range 18-29 years.
People who did not respond to the question or did not know are not included as having participated.
Data downloaded from IPUMS CPS Civic Engagement Supplement. Samples from 2013, 2017, 2019, and 2021 were pooled to calculate the estimates. Data for 2022 and 2023 did not include data by racial and ethnic group so was not included in the analysis.
Race groups are exclusive of latino (Latino), except aian and pacisl and bipoc. Race codes for aian and pacisl are All aian and All pacisl (nh_twoormor + latino-inclusive). bipoc includes Black, Indigenous, and other people of color including latino.
Geographic levels: LA County and Principal Cities within LA County, see here: https://cps.ipums.org/cps/codes/individcc_2004onward_codes.shtml).
Estimates have been supressed when the universe (pop) is less than 60 or the CV (rate_cv) is greater than 40%. pacisl rates are unstable because they are over the coeffient of variance (cv) threshold.
Performance and disparity ranks calculated according to the RACE COUNTS method.
This table was prepared in the R script W:\\Project\\OSI\\Bold Vision\\BV 2023\\R\\boldvision_22_23\\yp_civic_engagement.R';

COMMENT ON COLUMN bv_2023.yp_civic_engage_youth_subgroup.geoid IS 'Geographic ID.';
COMMENT ON COLUMN bv_2023.yp_civic_engage_youth_subgroup.name IS 'Geography name.';
COMMENT ON COLUMN bv_2023.yp_civic_engage_youth_subgroup.subgroup IS 'Race/ethnicity category. Race groups are exclusive of latino, except aian and pacisl. Race codes for aian and pacisl are All aian and All pacisl.';
COMMENT ON COLUMN bv_2023.yp_civic_engage_youth_subgroup.count IS 'Estimated count of youth (18-29) who report having participated in their community/been civically engaged.';
COMMENT ON COLUMN bv_2023.yp_civic_engage_youth_subgroup.pop IS 'Youth (18-29) population';
COMMENT ON COLUMN bv_2023.yp_civic_engage_youth_subgroup.rate IS 'Percent of youth (18-29) who report having participated in their community/been civically engaged.';
COMMENT ON COLUMN bv_2023.yp_civic_engage_youth_subgroup.rate_se IS 'Standard error for percent of youth (18-29) who report having participated in their community/been civically engaged.';
COMMENT ON COLUMN bv_2023.yp_civic_engage_youth_subgroup.rate_cv IS 'Coefficient of variation for percent of youth (18-29) who report having participated in their community/been civically engaged.';
COMMENT ON COLUMN bv_2023.yp_civic_engage_youth_subgroup.diff IS 'Difference from the best (or max) value.';")


# write city table to bd db
dbWriteTable(conBV, c("bv_2023", "yp_civic_engage_youth_region"), final_city_sp,
             overwrite=TRUE, row.names=FALSE,

             field.types = c(
               geoid= "varchar",
               name = "varchar",
               asbest = "varchar",
               best = "numeric",
               values_count = "integer",
               pop = "numeric",
               count = "numeric",
               rate = "double precision",
               rate_se = "numeric",
               rate_cv ="numeric",
               diff = "numeric",
               index_of_disparity = "numeric",
               geometry = "geometry(MultiPolygon,3310)"
             ))


# send table comments to database
dbGetQuery(conBV,
           "COMMENT ON TABLE bv_2023.yp_civic_engage_youth_region IS 'Table for the share of people who report engaging/participating in politics or their community. Age range 18-29 years.
People who did not respond to the question or did not know are not included as having participated.
Data downloaded from IPUMS CPS Civic Engagement Supplement. Samples from 2013, 2017, 2019, and 2021 were pooled to calculate the estimates.
Geographic levels: LA County and Principal Cities within LA County, see here: https://cps.ipums.org/cps/codes/individcc_2004onward_codes.shtml).
Estimates have been supressed when the universe (pop) is less than 60 or the CV (rate_cv) is greater than 40%.
Performance and disparity ranks calculated according to the RACE COUNTS method.
This table was prepared in the R script W:\\Project\\OSI\\Bold Vision\\BV 2023\\R\\boldvision_22_23\\yp_civic_engagement.R';

COMMENT ON COLUMN bv_2023.yp_civic_engage_youth_region.geoid IS 'Geographic ID.';
COMMENT ON COLUMN bv_2023.yp_civic_engage_youth_region.name IS 'Geography name.';
COMMENT ON COLUMN bv_2023.yp_civic_engage_youth_region.count IS 'Estimated count of youth (18-29) who report having participated in their community/been civically engaged.';
COMMENT ON COLUMN bv_2023.yp_civic_engage_youth_region.pop IS 'Youth (18-29) population';
COMMENT ON COLUMN bv_2023.yp_civic_engage_youth_region.rate IS 'Percent of youth (18-29) who report having participated in their community/been civically engaged.';
COMMENT ON COLUMN bv_2023.yp_civic_engage_youth_region.rate_se IS 'Standard error for percent of youth (18-29) who report having participated in their community/been civically engaged.';
COMMENT ON COLUMN bv_2023.yp_civic_engage_youth_region.rate_cv IS 'Coefficient of variation for percent of youth (18-29) who report having participated in their community/been civically engaged.';
COMMENT ON COLUMN bv_2023.yp_civic_engage_youth_region.diff IS 'Difference from the best (or max) value.';")

dbDisconnect(conn = conBV)