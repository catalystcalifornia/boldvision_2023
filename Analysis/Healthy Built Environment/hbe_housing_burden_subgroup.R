# get LA County youth ages 0-24 by race in rent-burdened households for Bold Vision LA
# Data Dictionary: W:\Data\Demographics\PUMS\CA_2017_2021\PUMS_Data_Dictionary_2017-2021.pdf 

library(tidyverse)
library(data.table)
library(RPostgreSQL)
library(readxl)

source("W:\\RDA Team\\R\\credentials_source.R")

####  Step 1: load data  ####

# PUMS Data
root <- "W:/Data/Demographics/PUMS/"

## Load the people PUMS data
people <- fread(paste0(root, "CA_2017_2021/psam_p06.csv"), header = TRUE, data.table = FALSE,
                colClasses = list(character = c("PUMA", "ANC1P", "ANC2P", "HISP", "RAC1P", "RAC2P", "RAC3P", "RACAIAN", "RACPI", "RACNH")))


## Load the housing PUMS data
housing <- fread(paste0(root, "CA_2017_2021/psam_h06.csv"), header = TRUE, data.table = FALSE,
                 colClasses = list(character = c("PUMA")))



####  Step 2: filter LA County youth and re-code race/ethnicity ####

## Select LA County youth and columns I want
la_cnty_youth <- people %>% filter(AGEP <= 24 & grepl('037', PUMA))

## Sum youth to test accuracy
#N <- sum(la_cnty_youth$PWGTP) #3,095,848 close enough to the 3,097,574 I see here: 
#https://data.census.gov/table/ACSST5Y2021.S0101?q=Age%20and%20Sex&g=050XX00US06037


## Recode race/eth
la_cnty_youth<-la_cnty_youth%>%
  mutate(latino=(ifelse(HISP %in% "01", "Not Latinx", "Latinx")))

la_cnty_youth<-la_cnty_youth%>%
  mutate(aian=(ifelse(RACAIAN %in% "0", "Not AIAN", "AIAN Alone or in Combination")))

la_cnty_youth<-la_cnty_youth%>%
  mutate(nhpi=(ifelse(RACPI %in% "1", "NHPI Alone or in Combination",
                      ifelse(RACNH %in% '1', 'NHPI Alone or in Combination', "Not NHPI"))))


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

##swana - alone or in combination with another race or latino
la_cnty_youth$swana <- "Not SWANA"
la_cnty_youth$swana[la_cnty_youth$ANC1P%in% swana_codes$ANC_Code| la_cnty_youth$ANC2P%in% swana_codes$ANC_Code] <- "SWANA"
la_cnty_youth$swana <- as.factor(la_cnty_youth$swana)

# code other race groups
la_cnty_youth$race = as.factor(ifelse(la_cnty_youth$RAC1P == "1" & la_cnty_youth$latino =="Not Latinx", "White NL",
                                      ifelse(la_cnty_youth$RAC1P == "1" & la_cnty_youth$latino =="Latinx", "Latinx placeholder",
                                             ifelse(la_cnty_youth$RAC1P == "2" & la_cnty_youth$latino =="Not Latinx", "Black NL",
                                                    ifelse(la_cnty_youth$RAC1P== "3" | la_cnty_youth$RAC1P== "4"|la_cnty_youth$RAC1P== "5", "AIAN placeholder",
                                                           ifelse(la_cnty_youth$RAC1P == "6" & la_cnty_youth$latino =="Not Latinx", "Asian NL",
                                                                  ifelse(la_cnty_youth$RAC1P == "7", "NHPI placeholder",
                                                                         ifelse(la_cnty_youth$RAC1P == "8" & la_cnty_youth$latino =="Not Latinx", "Other NL", 
                                                                                ifelse(la_cnty_youth$RAC1P== "9" 
                                                                                       & la_cnty_youth$latino =="Not Latinx", "Two or More NL", "Latinx placeholder"))))))))) 


la_cnty_youth<-la_cnty_youth%>%
  mutate(bipoc=(ifelse(race == "White NL" & swana == "Not SWANA", "Not bipoc", "bipoc")))

#test <- la_cnty_youth %>% select(SERIALNO, HISP, RAC1P, race, aian, latino, nhpi, swana, bipoc) 
#View(test)


####  Step 3: filter LA County households eligible for rent-burden calculation and join to youth  #### 

## Select LA County renter households (in cost-burden universe)
la_cnty_renter_hhs <- housing %>% filter(TEN <= "3" & is.na(GRPIP) == FALSE & grepl('037', PUMA))

## Sum renter hhs to test accuracy
#N <- sum(la_cnty_renter_hhs$WGTP) #1,722,545 close enough to the 1,796,882 I see here: 
#https://data.census.gov/table/ACSDT5Y2021.B25070?q=B25070:%20Gross%20Rent%20as%20a%20Percentage%20of%20Household%20Income%20in%20the%20Past%2012%20Months&g=050XX00US06037&tid=ACSDT1Y2022.B25070

#join and filter
eligible_youth <- la_cnty_youth %>% left_join(la_cnty_renter_hhs, by = "SERIALNO") %>%
  filter(is.na(WGTP) == FALSE)

#sum(eligible_youth$PWGTP) = 1,658,574 seems about right if slightly more than half of youth are eligible (renters)



#### Step 4: set up surveys and calculate percentages of youth in rent-burdened households by race/ethnicity

# survey design code

# Define weight variable and population base which will be used in the survey design set up
## You must use WGTP (if you are using psam_h06.csv and want housing units, like for Low Quality Housing) or PWGTP (if you want person units, like for Connected Youth)
weight <- 'PWGTP' # using PWGTP b/c calculating percentage of youth in rent-burdened households (from psam_p06.csv)

repwlist = rep(paste0("PWGTP", 1:80))
# prep data and add in burdened indicator
youth <- eligible_youth
youth$geoid <- "037"

youth<-youth%>%
  mutate(indicator=(ifelse(youth$GRPIP > 29, "burdened", "not burdened")))

# create survey design
library(tidycensus)
library(srvyr)

youth_county <- youth %>%               
  as_survey_rep(
    variables = c(geoid, indicator, race, latino, aian, nhpi, swana, bipoc),   # dplyr::select grouping variables
    weights = weight,                       #  weight
    repweights = repwlist,                  # list of replicate weights
    combined_weights = TRUE,                # tells the function that replicate weights are included in the data
    mse = TRUE,                             # tells the function to calc mse
    type="other",                           # statistical method
    scale=4/80,                             # scaling set by ACS
    rscale=rep(1,80)                        # setting specific to ACS-scaling
  )

###### Latino ######
lat <- youth_county  %>%
  group_by(geoid,latino,indicator) %>%   # group by race cat
  summarise(
    num = survey_total(na.rm=T), # get the (survey weighted) count for the numerators
    rate = survey_mean()) %>%        # get the (survey weighted) proportion for the numerator
  left_join(youth_county %>%                                        # left join in the denominators
              group_by(geoid,latino) %>%                                     # group by geo
              summarise(pop = survey_total(na.rm=T))) %>%              # get the weighted total for overall geo
  mutate(rate=rate*100,
         rate_moe = rate_se*1.645*100,    # calculate the margin of error for the rate based on se
         rate_cv = ((rate_moe/1.645)/rate) * 100, # calculate cv for rate
         count_moe = num_se*1.645, # calculate moe for numerator count based on se
         count_cv = ((count_moe/1.645)/num) * 100)  # calculate cv for numerator count

###### NHPI ######
nhpi <- youth_county  %>%
  group_by(geoid,nhpi,indicator) %>%   # group by race cat
  summarise(
    num = survey_total(na.rm=T), # get the (survey weighted) count for the numerators
    rate = survey_mean()) %>%        # get the (survey weighted) proportion for the numerator
  left_join(youth_county %>%                                        # left join in the denominators
              group_by(geoid,nhpi) %>%                                     # group by geo
              summarise(pop = survey_total(na.rm=T))) %>%              # get the weighted total for overall geo
  mutate(rate=rate*100,
         rate_moe = rate_se*1.645*100,    # calculate the margin of error for the rate based on se
         rate_cv = ((rate_moe/1.645)/rate) * 100, # calculate cv for rate
         count_moe = num_se*1.645, # calculate moe for numerator count based on se
         count_cv = ((count_moe/1.645)/num) * 100)  # calculate cv for numerator count

###### AIAN ######
aian <- youth_county  %>%
  group_by(geoid,aian,indicator) %>%   # group by race cat
  summarise(
    num = survey_total(na.rm=T), # get the (survey weighted) count for the numerators
    rate = survey_mean()) %>%        # get the (survey weighted) proportion for the numerator
  left_join(youth_county %>%                                        # left join in the denominators
              group_by(geoid,aian) %>%                                     # group by geo
              summarise(pop = survey_total(na.rm=T))) %>%              # get the weighted total for overall geo
  mutate(rate=rate*100,
         rate_moe = rate_se*1.645*100,    # calculate the margin of error for the rate based on se
         rate_cv = ((rate_moe/1.645)/rate) * 100, # calculate cv for rate
         count_moe = num_se*1.645, # calculate moe for numerator count based on se
         count_cv = ((count_moe/1.645)/num) * 100)  # calculate cv for numerator count

###### SWANA ######
swana <- youth_county  %>%
  group_by(geoid,swana,indicator) %>%   # group by race cat
  summarise(
    num = survey_total(na.rm=T), # get the (survey weighted) count for the numerators
    rate = survey_mean()) %>%        # get the (survey weighted) proportion for the numerator
  left_join(youth_county %>%                                        # left join in the denominators
              group_by(geoid,swana) %>%                                     # group by geo
              summarise(pop = survey_total(na.rm=T))) %>%              # get the weighted total for overall geo
  mutate(rate=rate*100,
         rate_moe = rate_se*1.645*100,    # calculate the margin of error for the rate based on se
         rate_cv = ((rate_moe/1.645)/rate) * 100, # calculate cv for rate
         count_moe = num_se*1.645, # calculate moe for numerator count based on se
         count_cv = ((count_moe/1.645)/num) * 100)  # calculate cv for numerator count

###### RACE ######
race <- youth_county  %>%
  group_by(geoid,race,indicator) %>%   # group by race cat
  summarise(
    num = survey_total(na.rm=T), # get the (survey weighted) count for the numerators
    rate = survey_mean()) %>%        # get the (survey weighted) proportion for the numerator
  left_join(youth_county %>%                                        # left join in the denominators
              group_by(geoid,race) %>%                                     # group by geo
              summarise(pop = survey_total(na.rm=T))) %>%              # get the weighted total for overall geo
  mutate(rate=rate*100,
         rate_moe = rate_se*1.645*100,    # calculate the margin of error for the rate based on se
         rate_cv = ((rate_moe/1.645)/rate) * 100, # calculate cv for rate
         count_moe = num_se*1.645, # calculate moe for numerator count based on se
         count_cv = ((count_moe/1.645)/num) * 100)  # calculate cv for numerator count

###### bipoc ######
bipoc <- youth_county  %>%
  group_by(geoid,bipoc,indicator) %>%   # group by race cat
  summarise(
    num = survey_total(na.rm=T), # get the (survey weighted) count for the numerators
    rate = survey_mean()) %>%        # get the (survey weighted) proportion for the numerator
  left_join(youth_county %>%                                        # left join in the denominators
              group_by(geoid,bipoc) %>%                                     # group by geo
              summarise(pop = survey_total(na.rm=T))) %>%              # get the weighted total for overall geo
  mutate(rate=rate*100,
         rate_moe = rate_se*1.645*100,    # calculate the margin of error for the rate based on se
         rate_cv = ((rate_moe/1.645)/rate) * 100, # calculate cv for rate
         count_moe = num_se*1.645, # calculate moe for numerator count based on se
         count_cv = ((count_moe/1.645)/num) * 100)  # calculate cv for numerator count

###### TOTAL ######
total <- youth_county  %>%
  group_by(geoid,indicator) %>%   # group by race cat
  summarise(
    num = survey_total(na.rm=T), # get the (survey weighted) count for the numerators
    rate = survey_mean()) %>%        # get the (survey weighted) proportion for the numerator
  left_join(youth_county %>%                                        # left join in the denominators
              group_by(geoid) %>%                                     # group by geo
              summarise(pop = survey_total(na.rm=T))) %>%              # get the weighted total for overall geo
  mutate(rate=rate*100,
         rate_moe = rate_se*1.645*100,    # calculate the margin of error for the rate based on se
         rate_cv = ((rate_moe/1.645)/rate) * 100, # calculate cv for rate
         count_moe = num_se*1.645, # calculate moe for numerator count based on se
         count_cv = ((count_moe/1.645)/num) * 100)  # calculate cv for numerator count



####  Step 5: format and run RC calcs

# rename race name columns as subgroup
total$subgroup = "Total"
total <- total %>% select(geoid, subgroup, everything())

aian <- aian %>% rename(subgroup = aian)
bipoc <- bipoc %>% rename(subgroup = bipoc)
lat <- lat %>% rename(subgroup = latino)
nhpi <- nhpi %>% rename(subgroup = nhpi)
race <- race %>% rename(subgroup = race)
swana <- swana %>% rename(subgroup = swana)

# merge tables except for bipoc - need total
d_long <- rbind(total, aian, lat, race, nhpi, swana) %>%
  filter(indicator == "burdened" & 
           subgroup != "Not AIAN" &
           subgroup != "Not Latinx" &
           subgroup != "Not NHPI" &
           subgroup != "Not SWANA" &
           subgroup != "AIAN placeholder" &
           subgroup != "NHPI placeholder" &
           subgroup != "Latinx placeholder")

d_long <- as.data.frame(d_long)
d_long$geoid <- "06037"
d_long <- d_long %>% select(geoid, everything())


#pivot to get into RC function format
d_wide_rate <- d_long %>% select(geoid, subgroup, rate) %>%
  pivot_wider(names_from = subgroup, values_from = rate)

names(d_wide_rate) <- c("geoid", "total_rate", "aian_rate", "latino_rate", "nh_asian_rate", "nh_black_rate", 
                        "nh_other_rate", "nh_twoormor_rate", "nh_white_rate", "nhpi_rate", "swana_rate")

d_wide_pop <- d_long %>% select(geoid, subgroup, pop) %>%
  pivot_wider(names_from = subgroup, values_from = pop)

names(d_wide_pop) <- c("geoid", "total_pop", "aian_pop", "latino_pop", "nh_asian_pop", "nh_black_pop", 
                       "nh_other_pop", "nh_twoormor_pop", "nh_white_pop", "nhpi_pop", "swana_pop")

d_wide_count <- d_long %>% select(geoid, subgroup, num) %>%
  pivot_wider(names_from = subgroup, values_from = num)

names(d_wide_count) <- c("geoid", "total_count", "aian_count", "latino_count", "nh_asian_count", "nh_black_count", 
                         "nh_other_count", "nh_twoormor_count", "nh_white_count", "nhpi_count", "swana_count")

d_wide_rate_cv <- d_long %>% select(geoid, subgroup, rate_cv) %>%
  pivot_wider(names_from = subgroup, values_from = rate_cv)

names(d_wide_rate_cv) <- c("geoid", "total_rate_cv", "aian_rate_cv", "latino_rate_cv", "nh_asian_rate_cv", "nh_black_rate_cv", 
                           "nh_other_rate_cv", "nh_twoormor_rate_cv", "nh_white_rate_cv", "nhpi_rate_cv", "swana_rate_cv")


d <- left_join(d_wide_rate, d_wide_pop, by = "geoid")
d <- left_join(d, d_wide_count, by = "geoid")
d <- left_join(d, d_wide_rate_cv, by = "geoid")


#set source for RC Functions script
source("W:/Project/RACE COUNTS/Functions/RC_Functions.R")

d$asbest = 'min'  

d <- count_values(d) #calculate number of "_rate" values
d <- calc_best(d) #calculate best rates
d <- calc_diff(d) #calculate difference from best
d <- calc_id(d) #calculate index of disparity



####  Step 6: final format and upload to Postgres  ####

#pivot to Elycia's desired format
county_table <- d %>% select(-asbest, -best, -values_count, -index_of_disparity) %>% 
  pivot_longer(!geoid, names_to = "subgroup", values_to = "metric") %>% 
  mutate(variable = case_when(grepl("_rate_cv", subgroup) ~ "rate_cv",
                              grepl("_rate", subgroup) ~ "rate",
                              grepl("_pop", subgroup) ~ "pop",
                              grepl("_diff", subgroup) ~ "diff",
                              grepl("_count", subgroup) ~ "count")) %>%
  mutate(across(subgroup, str_replace, "_rate_cv", "")) %>%
  mutate(across(subgroup, str_replace, "_pop", "")) %>%
  mutate(across(subgroup, str_replace, "_diff", "")) %>%
  mutate(across(subgroup, str_replace, "_rate", "")) %>%
  mutate(across(subgroup, str_replace, "_count", "")) %>%
  pivot_wider(names_from = variable, values_from = metric) %>%
  left_join(d %>% select(geoid, best, index_of_disparity, values_count, asbest), by = "geoid") %>%
  mutate(diff = rate - best)

# #add back bipoc
bipoc_final <- bipoc %>% filter(subgroup == "bipoc" & indicator == "burdened") %>%
  rename(count = num) %>%
  select(subgroup, rate, pop, count, rate_cv) %>%
  mutate(geoid = "06037", diff = NA, best = NA, index_of_disparity = NA, values_count = NA, asbest = NA) %>%
  select(geoid, everything())

county_table <- rbind(county_table, bipoc_final)
county_table$best[which(county_table$subgroup == "bipoc")] <- county_table$best[which(county_table$subgroup == "total")]
county_table$index_of_disparity[which(county_table$subgroup == "bipoc")] <- county_table$index_of_disparity[which(county_table$subgroup == "total")]
county_table$values_count[which(county_table$subgroup == "bipoc")] <- county_table$values_count[which(county_table$subgroup == "total")]
county_table$asbest[which(county_table$subgroup == "bipoc")] <- county_table$asbest[which(county_table$subgroup == "total")]
county_table$diff[which(county_table$subgroup == "bipoc")] <- county_table$rate[which(county_table$subgroup == "bipoc")] - 
  county_table$best[which(county_table$subgroup == "bipoc")]

county_table$diff[which(county_table$subgroup == "total")] = NA
county_table$diff[which(county_table$subgroup == "bipoc")] = NA
county_table$subgroup[which(county_table$subgroup == "nhpi")] = "pacisl"

###Send to Postgres###
con3 <- connect_to_db("bold_vision")
table_name <- "hbe_housing_burden_subgroup"
schema <- 'bv_2023'

indicator <- "Housing Burden (%) is the percent of youth 0-24 in renter households paying 30% or more of income for rent."
source <- "American Community Survey 2017-2021 5-year PUMS estimates. See QA doc for details: W:\\Project\\OSI\\Bold Vision\\BV 2023\\Documentation\\Healthy Built Environment\\QA_Housing_Burden.docx"

dbWriteTable(con3, c(schema, table_name), county_table,
             overwrite = TRUE, row.names = FALSE)

#comment on table and columns
comment <- paste0("COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
                                         COMMENT ON COLUMN ", schema, ".", table_name, ".geoid IS 'County fips';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".subgroup IS 'race';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".rate IS 'indicator rate';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".pop IS 'subgroup population';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".count IS 'rent-burdened subgroup';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".rate_cv IS 'cv of indicator rate';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".diff IS 'difference from best rate';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".best IS 'best rate';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".index_of_disparity IS 'index_of_disparity';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".values_count IS 'subgroups with values';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".asbest IS 'whether minimum or maximum is best';")
print(comment)
dbSendQuery(con3, comment)

#disconnect
dbDisconnect(con3)
