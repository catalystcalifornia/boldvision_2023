#data, cv, bvote, difference, index of disparity
# Voter Supplement
# Youth Voter Turnout: registered to vote inPresidential or midter elections

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
root_bv =  "W:/Project/OSI/Bold Vision/BV 2023/Data/Youth Power/Youth Voter Turnout/"
root_ipums =  "W:/Project/OSI/Bold Vision/BV 2023/Data/Youth Power/Youth Voter Turnout/source/IPUMS/"

ddi <- read_ipums_ddi(paste0(root_ipums, "cps_00002.xml"))
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
# Table 4. Parameters for Computation of Standard Errors for Voting and Registration Characteristics: November 2014 
#       Total            / White           / Black           / API AIAN NHOPI       / latino   
#      # a         b     / a        b      /  a        b     / a         b     / a          b
#      -0.000012 2913 / -0.000012 2913 / -0.000081 4270 / -0.000213 4654 / -0.000186 7196

# Table 4. Parameters for Computation of Standard Errors for Voting and Registration Characteristics: November 2016
#       Total            / White           / Black           / API AIAN NHOPI       / latino   
#      # a         b     / a        b      /  a        b     / a         b     /    a          b
#      -0.000014 3496 / -0.000014 3496 /  -0.000097 5124 /  -0.000256 5586 /  -0.000223 8636

#Table 7. Parameters for Computation of Standard Errors for Labor Force Characteristics: November 2018
#       Total            / White           / Black           / API AIAN NHOPI       / latino   
#      # a         b    / a        b      /  a        b     / a         b     / a          b
# -0.000014 3496 /  -0.000014 3496 / -0.000088 5124 / -0.000223 5586 / -0.000200 8636


## Table 8. Parameters for Computation of Standard Errors for Voting and Registration Characteristics: November 2020
#       Total            / White           / Black           / API AIAN NHOPI       / latino   
#      # a         b     / a        b      /  a        b     / a         b     / a          b
#      -0.000021 5411 / -0.000021 5411 / -0.000086 4941 / -0.000211 5249 / -0.000139 5923

# GVF parameters are the same for all years in the sample so I'm just going to use the same parameters
# `a` parameter formula was (sum of a / num years) * (num years + correlation coefficient * (3 * (num years)-4)/(num years^2))
total_parameter_a <- ((-0.000012-0.000014-0.000014-0.000021)/4)*(4+0*(3*(4)-4)/(4^2))
white_parameter_a <- ((-0.000012-0.000014-0.000014-0.000021)/4)*(4+0*(3*(4)-4)/(4^2))
black_parameter_a <- ((-0.000081-0.000097-0.000088-0.000086)/4)*(4+0*(3*(4)-4)/(4^2))
asian_pacisl_aian_parameter_a <- ((-0.000213-0.000256-0.000223-0.000211)/4)*(4+0*(3*(4)-4)/(4^2))
latino_parameter_a <- ((-0.000186-0.000223-0.000200-0.000139)/4)*(4+0*(3*(4)-4)/(4^2))
bipoc_parameter_a <- ((-0.000213-0.000256-0.000223-0.000211)/4)*(4+0*(3*(4)-4)/(4^2)) #API AIAN pacisl are the most conservative parameters so going to use that one for bipoc as well

# `b` parameter formula was (sum of b / num years) * (num years + correlation coefficient * (3 * (num years)-4)/num years)  
total_parameter_b <- ((2913+3496+3496+5411)/4)*(4+0*(3*(4)-4)/4)
white_parameter_b <- ((2913+3496+3496+5411)/4)*(4+0*(3*(4)-4)/4)
black_parameter_b <- ((4270+5124+5124+4941)/4)*(4+0*(3*(4)-4)/4)
asian_pacisl_aian_parameter_b <- ((4654+5586+5586+5249)/4)*(4+0*(3*(4)-4)/4)
latino_parameter_b <- ((7196+8636+8636+5923)/4)*(4+0*(3*(4)-4)/4)
bipoc_parameter_b <- ((4654+5586+5586+5249)/4)*(4+0*(3*(4)-4)/4) #API AIAN pacisl are the most conservative parameters so going to use that one for bipoc as well


####### CODE DATA 
# CONVERT LABELS TO FACTORS
cps_data <- 
  data %>%
  mutate(STATE_factor = as_factor(lbl_clean(STATEFIP)), #lbl_clean() removes labels that do not appear in the data. When converting labelled values to a factor, this avoids the creation of additional factor levels. From the IPUMS package
         COUNTY_f = as_factor(lbl_clean(COUNTY)),
         METFIPS_f = as_factor(lbl_clean(METFIPS)),
         INDIVIDCC_f = as_factor(lbl_clean(INDIVIDCC)),
         AGE_f = as_factor(lbl_clean(AGE)),
         RACE_f = as_factor(lbl_clean(RACE)),
         HISPAN_f = as_factor(lbl_clean(HISPAN)),
         CITIZEN_f = as_factor(lbl_clean(CITIZEN)),
         VOTED_f = as_factor(lbl_clean(VOTED)),
         VOREG_f = as_factor(lbl_clean(VOREG)))



table(cps_data$COUNTY, useNA="always")
table(cps_data$RACE_f, useNA = "always")
table(cps_data$VOTED_f, useNA = "always")
table(cps_data$VOREG_f, useNA = "always")
table(cps_data$HISPAN_f)


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




# review
# table(cps_data$race, cps_data$latino, useNA = "always")

#check that the race codes are accurate
# races_qa <- cps_data %>% select(HISPAN_f, RACE_f, race,latino, aian,pacisl, bipoc)
# View(races_qa)


# note that everyone who is marked as having voted is at least 18 years
# View(subset(cps_data,  VOTED_f =="Voted"))

table(cps_data$VOTED_f, cps_data$YEAR, useNA = "always")

#####################################################################
####### INDICATOR:  YOUTH ELECTION VOTER TURNOUT #######
#####################################################################
# code participation -- count any of these as participation. 
## COUNT only samples in all years
# cps_data$VOTED_f <-
#   ifelse(
#       (is.na(cps_data$VOTED_f) | !cps_data$VOTED_f %in% c("Did not vote", "Refused", "Don't know", "No Response", "Not in universe")),
#     "does not vote", "Voted")
#previous version did not do anything w/ voter registration so I took that part out too.


# make universe flag
# count only the 3 indicators that are in all years
cps_data$VOTED_f_univ <- ifelse(cps_data$VOTED_f =="Not in universe", "Not in universe", "in universe")
# 
# 
# table(cps_data$VOTED_f_univ, cps_data$VOTED_f, useNA = "always")

# Combines presidential and midterm (county level)
vote_pres_mid_county <-  
  
  # by race
  cps_data %>%
  filter(YEAR %in% c(2014, 2016, 2018, 2020) & VOTED_f=="Voted" & COUNTY_f != "Not identified" 
         & AGE_f %in% c(18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29) & !is.na(race)) %>% 
  group_by(COUNTY_f, VOTED_f, race) %>%
  summarize( 
    voteSAMP = sum(VOSUPPWT)) %>%  
  
  # Add latino
  bind_rows(                   
    cps_data %>% 
      filter(YEAR %in% c(2014, 2016, 2018, 2020) & VOTED_f=="Voted" & COUNTY_f != "Not identified" 
             & AGE_f %in% c(18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29) 
             & latino =="latino") %>%
      group_by(COUNTY_f, VOTED_f) %>%
      summarize(
        race = "latino",  
        voteSAMP = sum(VOSUPPWT))) %>%
  
  # Add aian (including latino)
  bind_rows(                   
    cps_data %>% 
      filter(YEAR %in% c(2014, 2016, 2018, 2020) & VOTED_f=="Voted" & COUNTY_f != "Not identified" 
             & AGE_f %in% c(18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29) 
             & aian =="aian") %>%
      group_by(COUNTY_f, VOTED_f) %>%
      summarize(
        race = "aian",  
        voteSAMP = sum(VOSUPPWT))) %>%
  
  # Add pacisl
  bind_rows(                   
    cps_data %>% 
      filter(YEAR %in% c(2014, 2016, 2018, 2020) & VOTED_f=="Voted" & COUNTY_f != "Not identified" 
             & AGE_f %in% c(18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29) 
             & pacisl =="pacisl") %>%
      group_by(COUNTY_f, VOTED_f) %>%
      summarize(
        race = "pacisl",  
        voteSAMP = sum(VOSUPPWT))) %>%
  
  # Add bipoc
  bind_rows(                   
    cps_data %>% 
      filter(YEAR %in% c(2014, 2016, 2018, 2020) & VOTED_f=="Voted" & COUNTY_f != "Not identified" 
             & AGE_f %in% c(18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29) 
             & bipoc =="bipoc") %>%
      group_by(COUNTY_f, VOTED_f) %>%
      summarize(
        race = "bipoc",  
        voteSAMP = sum(VOSUPPWT))) %>%
  
  # add in the vote total for the total population
  bind_rows(                   
    cps_data %>% 
      filter(YEAR %in% c(2014, 2016, 2018, 2020) & VOTED_f=="Voted" & COUNTY_f != "Not identified" & 
               AGE_f %in% c(18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29)) %>%
      group_by(COUNTY_f, VOTED_f) %>%
      summarize(
        race = "Total",
        voteSAMP = sum(VOSUPPWT))) %>%
  
  mutate(                 
    
    # estimate the standard error using GVF parameters -- general SE parameters by race attribute. Note diff years use diff parameters 
    voteSAMP_se = 
      #### add a filter by year for this section
      # Calc SE for 2013 CE Supplement
      ifelse(race %in% c("aian", "nh_asian", "pacisl","nh_twoormor"),  
              sqrt((asian_pacisl_aian_parameter_a * (voteSAMP^2)) + (asian_pacisl_aian_parameter_b*voteSAMP)),  # cps says let two or more races use the same as aian etc
              ifelse(race == "Total", sqrt((total_parameter_a * (voteSAMP^2)) + (total_parameter_b*voteSAMP)),
                      ifelse(race =="nh_white", sqrt((white_parameter_a * (voteSAMP^2)) + (white_parameter_b*voteSAMP)),
                              ifelse( race =="bipoc", sqrt((bipoc_parameter_a * (voteSAMP^2)) + (bipoc_parameter_b*voteSAMP)),
                      ifelse(race == "nh_black", sqrt((black_parameter_a * (voteSAMP^2)) + (black_parameter_b*voteSAMP)),
                              ifelse( race == "latino", sqrt((latino_parameter_a * (voteSAMP^2)) + (latino_parameter_b*voteSAMP)), NA)))))),
    
    
    # Calc CV and final voteSAMP count
    vote_cv = voteSAMP_se/voteSAMP,
    vote = voteSAMP/4) #divide by 6 if using 6 years

head(vote_pres_mid_county)


## Calculate voting eligible population
# think of age and citizenship status to get voting age

votingeligible_county <- 
  
  # by race
  bind_rows( 
    cps_data %>%
      filter(YEAR %in% c(2014, 2016, 2018, 2020)  & AGE_f %in% c(18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29) &
               CITIZEN_f != "Not a citizen" & COUNTY_f !="Not identified"  & !is.na(race) & VOTED_f !="Not in universe") %>%
      group_by(COUNTY_f, race) %>%
      summarize( 
        veSAMP = sum(VOSUPPWT))) %>%
  
  # add in latino
  bind_rows(  
    cps_data %>%  
      filter(YEAR %in% c(2014, 2016, 2018, 2020)  & AGE_f %in% c(18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29) &
               CITIZEN_f != "Not a citizen" & COUNTY_f !="Not identified" 
             & latino=="latino" & VOTED_f !="Not in universe") %>%
      group_by(COUNTY_f) %>%
      summarize(race = "latino",
                veSAMP = sum(VOSUPPWT))) %>%
  
  # add in aian
  bind_rows(  
    cps_data %>%  
      filter(YEAR %in% c(2014, 2016, 2018, 2020)  & AGE_f %in% c(18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29) &
               CITIZEN_f != "Not a citizen" & COUNTY_f !="Not identified" 
             & aian=="aian" & VOTED_f !="Not in universe") %>%
      group_by(COUNTY_f) %>%
      summarize(race = "aian",
                veSAMP = sum(VOSUPPWT))) %>%
  
  # add in pacisl
  bind_rows(  
    cps_data %>%  
      filter(YEAR %in% c(2014, 2016, 2018, 2020)  & AGE_f %in% c(18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29) &
               CITIZEN_f != "Not a citizen" & COUNTY_f !="Not identified" 
             & pacisl == "pacisl" & VOTED_f !="Not in universe") %>%
      group_by(COUNTY_f) %>%
      summarize(race = "pacisl",
                veSAMP = sum(VOSUPPWT))) %>%
  # add in pacisl
  bind_rows(  
    cps_data %>%  
      filter(YEAR %in% c(2014, 2016, 2018, 2020)  & AGE_f %in% c(18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29) &
               CITIZEN_f != "Not a citizen" & COUNTY_f !="Not identified" 
             & bipoc == "bipoc" & VOTED_f !="Not in universe") %>%
      group_by(COUNTY_f) %>%
      summarize(race = "bipoc",
                veSAMP = sum(VOSUPPWT))) %>%
  
  #  total population
  bind_rows(  
    cps_data %>%  
      filter(YEAR %in% c(2014, 2016, 2018, 2020)  & AGE_f %in% c(18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29) & 
               CITIZEN_f != "Not a citizen" & COUNTY_f !="Not identified" & VOTED_f !="Not in universe") %>%
      group_by(COUNTY_f) %>%
      summarize(race = "Total",
                veSAMP = sum(VOSUPPWT))) %>%
  
  # estimate the standard error using GVF parameters -- general SE parameters by race attribute for 2016
  mutate(                 
    veSAMP_se =
      
      # Calc SE for 2016 (not published for 2018)
      ifelse( race %in% c("aian", "nh_asian", "pacisl", "nh_twoormor"),    
              sqrt((asian_pacisl_aian_parameter_a * (veSAMP^2)) + (asian_pacisl_aian_parameter_b*veSAMP)),  # cps says let two or more races use the same as aian etc
              ifelse( race =="Total", sqrt((total_parameter_a * (veSAMP^2)) + (total_parameter_b*veSAMP)),
                      ifelse( race =="nh_white", sqrt((white_parameter_a  * (veSAMP^2)) + (white_parameter_b*veSAMP)),
                              ifelse( race =="bipoc", sqrt((bipoc_parameter_a * (veSAMP^2)) + (bipoc_parameter_b*veSAMP)),
                      ifelse( race == "nh_black", sqrt((black_parameter_a * (veSAMP^2)) + (black_parameter_b*veSAMP)),
                              ifelse( race == "latino", sqrt((latino_parameter_a * (veSAMP^2)) + (latino_parameter_b*veSAMP)), NA)))))),

    # Calc CV and final veSAMP count
    ve_cv = veSAMP_se/veSAMP,
    ve = veSAMP/4) 
head(votingeligible_county)
table(votingeligible_county$race)

# join to calc eligible voter turnout (evt_county)
## See cpsnov16.pdf, page 196 (voteimating SE of percentages)

evt_county<- 
  votingeligible_county%>%
  filter(COUNTY_f=="6037") %>%
  left_join(vote_pres_mid_county, by = c("COUNTY_f", "race")) %>%
  mutate(
    rate = (vote/ve)*100,
    rate_se = 
      ifelse(race %in% c("aian", "nh_asian", "pacisl", "nh_twoormor"),
             sqrt((asian_pacisl_aian_parameter_b/veSAMP) * rate * (100-rate)),  # cps says let two or more races use the same as aian etc
             ifelse( race =="Total",  sqrt((total_parameter_b/veSAMP) * rate * (100-rate)), 
                     ifelse( race =="nh_white",  sqrt((white_parameter_b/veSAMP) * rate * (100-rate)), 
                             ifelse( race =="bipoc",  sqrt((bipoc_parameter_b/veSAMP) * rate * (100-rate)), 
                     ifelse(race =="nh_black", sqrt((black_parameter_b/veSAMP) * rate * (100-rate)),
                            ifelse( race=="latino",sqrt((latino_parameter_b/veSAMP) * rate * (100-rate)), NA)))))),
    rate_cv = rate_se/rate*100,
    
    # # reliability threshold
    # rate = ifelse(rate_cv > 0.4 | ve < 60, NA, rate)
    ) 

View(evt_county)
table(evt_county$race)




##### Disparity Index ######
# calculate perf and disparity index according to race counts method.
# note I'm following a tutorial I wrote here: https://advancementprojectca.github.io/rda_resources/ACS-Data-Prep-for-Race-Counts.html


# do some data clean up first

# County
# prep -- remove some cols that aren't needed
evt_county<-  evt_county %>% select(COUNTY_f, VOTED_f, race, vote, ve, rate, rate_se, rate_cv) %>% 
  rename("geoid"="COUNTY_f", "category"="VOTED_f", "count"="vote", "pop"="ve" )


evt_county$geolevel <- "county"


# review
head(evt_county)



# difference table: indicator_df is the data frame with the indicator. ma
diff_county <-
  
  evt_county%>% 
    
    filter(race !="Total") %>%
    group_by(geoid, geolevel) %>% 
    summarize(max = max(rate, na.rm=T),
              values_count = sum(!is.na(rate))) %>% 
    
    # join max and values count to the original df
    right_join(evt_county, 
               by=c("geoid", "geolevel")) %>% 
    
    # get differences from max rate
    mutate(diff = ifelse(race =="Total" | race == "bipoc", NA, abs(max - rate)))
head(diff_county)

# get the avg and variance of differences, and the index of disparity
id_table_county <- diff_county %>% 
    filter(race !="Total") %>% 
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
#View(final_county)

# format county fips
final_county$geoid <- paste0("0", final_county$geoid)
final_county$NAME <- "Los Angeles County"



##### CITY: People who participate in some way #####
vote_pres_mid_city <- 
  
  cps_data %>%
  filter(YEAR %in% c(2014, 2016, 2018, 2020) &  COUNTY_f==6037 & (AGE >= 18 & AGE <= 29)) %>%
  group_by(COUNTY_f, INDIVIDCC_f, VOTED_f) %>%
  summarize(voteSAMP = sum(VOSUPPWT)) %>%
  mutate_if(is.factor, fct_na_value_to_level, level = "Missing Data") %>%
  
  mutate(                 
    
    # estimate the standard error using GVF parameters -- general SE parameters by geography attribute. Note diff years use diff parameters 
    voteSAMP_se = sqrt((total_parameter_a * (voteSAMP^2)) + (total_parameter_b*voteSAMP)),
    
    # Calc CV and final voteSAMP count
    vote_cv = voteSAMP_se/voteSAMP,
    vote = voteSAMP/4) 

# review
head(vote_pres_mid_city)

# ##### CITY: Population Base ##### 
votingeligible_city <-
  
  cps_data %>%
  filter(YEAR %in% c(2014, 2016, 2018, 2020) &  COUNTY_f==6037  & 
           VOTED_f_univ != "Not in universe"  & (AGE >= 18 & AGE <= 29)) %>% 
  group_by(COUNTY_f, INDIVIDCC_f) %>%
  summarize(veSAMP = sum(VOSUPPWT)) %>%
  mutate_if(is.factor, fct_na_value_to_level, level = "Missing Data") %>% 
  
  mutate(
    # estimate the standard error using GVF parameters -- general SE parameters by geography attribute. Note diff years use diff parameters
    veSAMP_se = sqrt((total_parameter_a * (veSAMP^2)) + (total_parameter_b*veSAMP)),
    
    # Calc CV and final veSAMP count
    ve_cv = veSAMP_se/veSAMP,
    ve = veSAMP/4)

# review
head(votingeligible_city)


evt_City  <-
  votingeligible_city  %>%
  left_join(vote_pres_mid_city) %>%
  
  mutate(rate = (vote/ve)*100,
    rate_se = sqrt((total_parameter_b/veSAMP) * rate * (100-rate)),
    rate_cv = rate_se/rate*100)

evt_City  <- evt_City %>% 
  
  ## reliability threshold #cut for now
  # mutate(rate = ifelse(rate_cv > 0.4 | ve < 60, NA, rate)) %>%
  
  # join cities
  filter(INDIVIDCC_f != "Not identified" & VOTED_f == "Voted") %>%
  full_join(cities %>%
              filter(COUNTY == 6037)) %>% 
  select(COUNTY_f, VOTED_f, ve, vote, rate, rate_se, rate_cv, City.name) %>% # prep -- remove some cols that aren't needed
  rename("county_geoid"="COUNTY_f","category"="VOTED_f","pop"="ve", "count"="vote", "NAME"="City.name") %>% 
  mutate(geolevel="city")

# review
#View(evt_City)

#add geoids 
# # load city spatial files (but don't include the spatial part until the very end)
city <- places(state = 'CA', year = 2020, cb = TRUE) %>% select(-c(STATEFP, PLACEFP, PLACENS, AFFGEOID, STUSPS, STATE_NAME, LSAD, ALAND, AWATER))%>% 
  rename("geoid"="GEOID")
citysp <- st_transform(city, 3310) # change projection to 3310
# st_crs(citysp) #check the CRS
evt_City_sp <- left_join(evt_City, citysp, by="NAME") %>% 
  filter(NAMELSAD!="Burbank CDP") %>% #Burbank is referring to the city so remove CDP
  ungroup() %>% select(-c(NAMELSAD, geometry)) 

##### Disparity Index ######

# note I'm following a tutorial I wrote here: https://advancementprojectca.github.io/rda_resources/ACS-Data-Prep-for-Race-Counts.html


# difference tables ----
diff_city <- evt_City_sp %>% 
  
  # filter(race !="Total") %>%
  group_by(geolevel) %>%
  summarize(max = max(rate, na.rm=T),
            values_count = sum(!is.na(rate))) %>%
  
  # join max and values count to the original df
  right_join(evt_City_sp, 
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
countysp <- counties(state = 'CA', year = 2020, cb = TRUE)%>% select(-c(STATEFP, COUNTYFP, COUNTYNS, NAME, AFFGEOID, STUSPS, STATE_NAME, LSAD, ALAND, AWATER)) %>% 
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
  rename("name"="NAME", "best"="max") 

col_order <- c("geoid", "name", "race", "asbest", "best", "values_count", "pop", "count", "rate", "rate_se", "rate_cv", 
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
# write table to bd db
dbWriteTable(conBV, c("bv_2023", "yp_youth_voter_turnout_subgroup"), final_county_sp,
             overwrite=TRUE, row.names=FALSE,
             
             field.types = c(
               geoid= "varchar",
               name = "varchar",
               race = "varchar",
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
           "COMMENT ON TABLE bv_2023.yp_youth_voter_turnout_region IS 'Table for youth (18-29 years) eligible voter turnout during presidential and midterm elections. 
# eligible voters are defined as adult citizens. Data Source: IPUMS CPS Voting Supplement. Samples from 2014, 2016, 2018, 2020 were pooled to calculated turnout by race-ethnicity. 
# Race groups are exclusive of latino except aian and pacisl. Race codes for aian and pacisl are All aian and All pacisl (nh_twoormor + latino inclusive).
# Included Geographies are major cities (within the Los Angeles MSA (Prinicipal Cities, see here: https://cps.ipums.org/cps/codes/individcc_2004onward_codes.shtml) and LA County. 
# Connectedness Estimates have been supressed when the universe (pop) is less than 60 or the CV (rate_cv) is greater than 40%.
# Performance and disparity ranks calculated according to the RACE COUNTS method.
This table was prepared in the R script W:\\Project\\OSI\\Bold Vision\\BV 2023\\R\\boldvision_22_23\\Youth_Voter_Turnout.R';

COMMENT ON COLUMN bv_2023.yp_youth_voter_turnout_subgroup.geoid IS 'Geographic ID.';
COMMENT ON COLUMN bv_2023.yp_youth_voter_turnout_subgroup.name IS 'Geography name.';
COMMENT ON COLUMN bv_2023.yp_youth_voter_turnout_subgroup.race IS 'Race/ethnicity category. Race groups are exclusive of latino, except aian and pacisl. Race codes for aian and pacisl are All aian and All pacisl.';
COMMENT ON COLUMN bv_2023.yp_youth_voter_turnout_subgroup.count IS 'estimated count of youth (18-29) who report having registered to vote inpresidential or midterm elections.';
COMMENT ON COLUMN bv_2023.yp_youth_voter_turnout_subgroup.pop IS 'Youth (18-29) population';
COMMENT ON COLUMN bv_2023.yp_youth_voter_turnout_subgroup.rate IS 'Percent of youth (18-29) who report having registered to vote inpresidential or midterm elections.';
COMMENT ON COLUMN bv_2023.yp_youth_voter_turnout_subgroup.rate_se IS 'Standard error for percent of youth (18-29) who report having registered to vote inpresidential or midterm elections.';
COMMENT ON COLUMN bv_2023.yp_youth_voter_turnout_subgroup.rate_cv IS 'Coefficient of variation for percent of youth (18-29) who report having registered to vote inpresidential or midterm elections.';
")

#COMMENT ON COLUMN bv_2023.yp_youth_voter_turnout_subgroup.category IS 'Youth Voter Turnout label. CE defined as people who have contacted a public official in the last 12 months, people who discuss politics with family and friends at least once a month, and people who have either bought or boycotted a specific product or service in the past 12 months due to the social or political values of the company.';


# write table to bd db
dbWriteTable(conBV, c("bv_2023", "yp_youth_voter_turnout_region"), final_city_sp,
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
           "COMMENT ON TABLE bv_2023.yp_youth_voter_turnout_region IS 'Table for youth (18-29 years) eligible voter turnout during presidential and midterm elections. 
# eligible voters are defined as adult citizens. Data Source: IPUMS CPS Voting Supplement. Samples from 2014, 2016, 2018, and 2020 were pooled to calculated turnout by race-ethnicity. 
# Race groups are exclusive of latino except aian and pacisl. Race codes for aian and pacisl are All aian and All pacisl (nh_twoormor + latino inclusive).
# Included Geographies are major cities (within the Los Angeles MSA (Prinicipal Cities, see here: https://cps.ipums.org/cps/codes/individcc_2004onward_codes.shtml) and LA County. 
# Connectedness Estimates have been supressed when the universe (pop) is less than 60 or the CV (rate_cv) is greater than 40%.
# Performance and disparity ranks calculated according to the RACE COUNTS method.
This table was prepared in the R script W:\\Project\\OSI\\Bold Vision\\BV 2023\\R\\boldvision_22_23\\Youth_Voter_Turnout.R';

COMMENT ON COLUMN bv_2023.yp_youth_voter_turnout_region.geoid IS 'Geographic ID.';
COMMENT ON COLUMN bv_2023.yp_youth_voter_turnout_region.name IS 'Geography name.';
COMMENT ON COLUMN bv_2023.yp_youth_voter_turnout_region.count IS 'estimated count of youth (18-29) who report having registered to vote inpresidential or midterm elections.';
COMMENT ON COLUMN bv_2023.yp_youth_voter_turnout_region.pop IS 'Youth (18-29) population';
COMMENT ON COLUMN bv_2023.yp_youth_voter_turnout_region.rate IS 'Percent of youth (18-29) who report having registered to vote inpresidential or midterm elections.';
COMMENT ON COLUMN bv_2023.yp_youth_voter_turnout_region.rate_se IS 'Standard erorr for percent of youth (18-29) who report having registered to vote inpresidential or midterm elections.';
COMMENT ON COLUMN bv_2023.yp_youth_voter_turnout_region.rate_cv IS 'Coefficient of variation for percent of youth (18-29) who report having registered to vote inpresidential or midterm elections.';
")

dbDisconnect(conn = conBV)