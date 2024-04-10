# get LA SPA youth ages 0-24 by race in rent-burdened households for Bold Vision LA
# Data Dictionary: W:\Data\Demographics\PUMS\CA_2017_2021\PUMS_Data_Dictionary_2017-2021.pdf 

library(tidyverse)
library(data.table)
library(RPostgreSQL)
library(sf)
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



####  Step 2: filter LA County youth ####

## Select LA County youth and columns I want
la_cnty_youth <- people %>% filter(AGEP <= 24 & grepl('037', PUMA))

## Sum youth to test accuracy
#N <- sum(la_cnty_youth$PWGTP) #3,095,848 close enough to the 3,097,574 I see here: 
#https://data.census.gov/table/ACSST5Y2021.S0101?q=Age%20and%20Sex&g=050XX00US06037



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




#### Step 3b: join PUMAs to SPAs

# GET PUMA-SPA crosswalk
con_bv <- connect_to_db("bold_vision")

puma_xwalk <- st_read(con_bv, query = "SELECT * FROM bv_2023.crosswalk_puma_spas_2023_v2")

dbDisconnect(con_bv)

# join indicator to PUMA SPA xwalk
eligible_youth <- eligible_youth %>% mutate(geoid = PUMA.x)
eligible_youth_spa <- eligible_youth %>% left_join(puma_xwalk, by = c("geoid"="puma_id"))



#### Step 4: set up surveys and calculate percentages of youth in rent-burdened households by race/ethnicity

# survey design code

# Define weight variable and population base which will be used in the survey design set up
## You must use WGTP (if you are using psam_h06.csv and want housing units, like for Low Quality Housing) or PWGTP (if you want person units, like for Connected Youth)
weight <- 'PWGTP' # using PWGTP b/c calculating percentage of youth in rent-burdened households (from psam_p06.csv)

repwlist = rep(paste0("PWGTP", 1:80))
# prep data and add in burdened indicator
youth <- eligible_youth_spa
youth$geoid <- youth$spa_id

youth<-youth%>%
  mutate(indicator=(ifelse(youth$GRPIP > 29, "burdened", "not burdened")))

# create survey design
library(tidycensus)
library(srvyr)

youth_spa <- youth %>%               
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
total <- youth_spa  %>%
  group_by(geoid,indicator) %>%   # group by race cat
  summarise(
    num = survey_total(na.rm=T), # get the (survey weighted) count for the numerators
    rate = survey_mean()) %>%        # get the (survey weighted) proportion for the numerator
  left_join(youth_spa %>%                                        # left join in the denominators
              group_by(geoid) %>%                                     # group by geo
              summarise(pop = survey_total(na.rm=T))) %>%              # get the weighted total for overall geo
  mutate(rate=rate*100,
         rate_moe = rate_se*1.645*100,    # calculate the margin of error for the rate based on se
         rate_cv = ((rate_moe/1.645)/rate) * 100, # calculate cv for rate
         count_moe = num_se*1.645, # calculate moe for numerator count based on se
         count_cv = ((count_moe/1.645)/num) * 100)  # calculate cv for numerator count



####  Step 5: format, merge, reformat, and run RC calcs

d <- as.data.frame(total)
d <- d %>% filter(indicator == "burdened") %>% 
  select(geoid, rate, pop, num, rate_cv) %>% rename(count = num)


#set source for RC Functions script
source("W:/Project/RACE COUNTS/Functions/RC_Functions.R")

##### calculate RACE COUNTS stats
asbest <- "min"
d$asbest <- "min"

best <- min(d$rate, na.rm=T)
d$best <- best

d$diff <- d$rate - best
values_count <- length(d$rate) - sum(is.na(d$rate))

### ID  adapted/removed df name from RC_Functions###
sumdiff <- sum(d$diff, na.rm = TRUE)
index_of_disparity <-  ifelse(values_count < 2 | values_count == 2 & asbest == 'min' & sumdiff == best, NA,
                              (((sumdiff / best) / (values_count - 1)) * 100))

d$index_of_disparity <- index_of_disparity
d$values_count <- values_count

d <- d %>% mutate(name = case_when(geoid == 1 ~ "Antelope Valley",
                                   geoid == 2 ~ "San Fernando Valley",
                                   geoid == 3 ~ "San Gabriel Valley",
                                   geoid == 4 ~ "Metro LA",
                                   geoid == 5 ~ "West LA",
                                   geoid == 6 ~ "South LA",
                                   geoid == 7 ~ "East LA",
                                   geoid == 8  ~ "South Bay")) %>% 
  
  select(geoid, name, everything())



####  Step 6: Send to Postgres  ####

con3 <- connect_to_db("bold_vision")
table_name <- "hbe_housing_burden_region"
schema <- 'bv_2023'

indicator <- "Housing Burden (%) is the percent of youth 0-24 in renter households paying 30% or more of income for rent."
source <- "American Community Survey 2017-2021 5-year PUMS estimates. See QA doc for details: W:\\Project\\OSI\\Bold Vision\\BV 2023\\Documentation\\Healthy Built Environment\\QA_Housing_Burden.docx"
dbWriteTable(con3, c(schema, table_name), d,
             overwrite = FALSE, row.names = FALSE)

#comment on table and columns
comment <- paste0("COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
                                         COMMENT ON COLUMN ", schema, ".", table_name, ".geoid IS 'SPA number';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".name IS 'SPA name';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".rate IS 'indicator rate';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".pop IS 'SPA youth population';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".count IS 'rent-burdened subgroup';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".rate_cv IS 'cv of indicator rate';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".best IS 'best rate among SPAs';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".diff IS 'difference from the best';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".index_of_disparity IS 'index of disparity';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".values_count IS 'number of values';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".asbest IS 'minimum or maximum as best';")
print(comment)
dbSendQuery(con3, comment)

#disconnect
dbDisconnect(con3)