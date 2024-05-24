# Youth Poverty in LA County ages 05-24

# Data Dictionary: https://www2.census.gov/programs-surveys/acs/tech_docs/pums/data_dict/PUMS_Data_Dictionary_2022.pdf
    # POVPIP Numeric 3
        # Income-to-poverty ratio recode
        # bbb .N/A (individuals who are under 15 and are either living
        #           .in a housing unit but are unrelated to the householder
        #           .or are living in select group quarters)
        # 0..500 .Below 501 percent
        # 501 .501 percent or more

# Install packages if not already installed
packages <- c("tidyverse", "data.table","readxl","tidycensus", "srvyr","stringr") 

install_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(install_packages) > 0) {
  install.packages(install_packages)
} else {
  print("All required packages are already installed.")
}

for(pkg in packages){
  library(pkg, character.only = TRUE)
}

#SOURCE from the script that has: styling, packages, dbconnection, colors
source("W:\\RDA Team\\R\\credentials_source.R")

#### Load the data ####

# PUMS Data
root <- "W:/Data/Demographics/PUMS/"
indicator_name <- "poverty"
data_type <- "county" #race, spa, county
  
# Load the people PUMS data
people <- fread(paste0(root, "CA_2021/psam_p06.csv"), header = TRUE, data.table = FALSE,
                colClasses = list(character = c("PUMA", "AGEP", "POVPIP")))

  
####  Filter households eligible for calculation  #### 

## Select LA County people
eligible_hhs <- people %>% 
  
  #filtering for universe and LA county
  filter(!is.na(POVPIP) & grepl('037', PUMA) & AGEP < 25)   %>% 

  
  #remove records with no weights
  filter(!is.na(PWGTP)) %>%
  
  #filter for age 0-5 and select distinct households
  distinct(SERIALNO, .keep_all = TRUE)


####  Set up and run survey and format  #### 

# add geoid and indicator
eligible_hhs$geoid <- "037"
eligible_hhs$indicator=(ifelse(eligible_hhs$POVPIP <= 100, "at or below 100% FPL", "above poverty"))

weight <- 'PWGTP' # using PWGTP b/c calculating percentage of rent-burdened households


repwlist = rep(paste0("PWGTP", 1:80))


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
d_long <- total %>% filter(indicator == "at or below 100% FPL" & !is.na(geoid))

# make data frame
d_long <- as.data.frame(d_long)
#### Step 4: Repeat steps 2 and 3 above with 200% filter ####

## Select LA County people
eligible_hhs2 <- people %>% 
  
  #filtering for universe and LA county
  filter(!is.na(POVPIP) & grepl('037', PUMA) & AGEP < 25)   %>% 
  
  # join their housing info  
  left_join(housing %>% filter(grepl('037', PUMA)), 
            by = c("SERIALNO", "PUMA")) %>%
  
  #remove records with no weights
  filter(!is.na(PWGTP)) %>%
  
  #filter for age 0-5 and select distinct households
  distinct(SERIALNO, .keep_all = TRUE)

# add geoid and indicator
eligible_hhs2$geoid <- "037"
eligible_hhs2$indicator=(ifelse(eligible_hhs2$POVPIP <= 200, "at or below 200% FPL", "above poverty"))

weight <- 'PWGTP' # using PWGTP b/c calculating percentage of rent-burdened households


repwlist = rep(paste0("PWGTP", 1:80))


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
d_long2 <- total %>% filter(indicator == "at or below 200% FPL" & !is.na(geoid))

# make data frame
d_long2 <- as.data.frame(d_long2)

# bind both data frames in final
d_final <- rbind(d_long, d_long2)

### Send to Postgres ###
con3 <- connect_to_db("bold_vision")
table_name <- "demo_poverty"
schema <- 'bv_2023'

indicator <- "Poverty at the 100 and 200 percent levels"
source <- "American Community Survey 2017-2021 5-year PUMS estimates. See QA doc for details: W:\\Project\\OSI\\Bold Vision\\BV 2023\\Documentation\\Healthy Built Environment\\QA_Housing_Burden.docx"

dbWriteTable(con3, c(schema, table_name), d_final,
             overwrite = TRUE, row.names = FALSE)

#comment on table and columns
comment <- paste0("COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".geoid IS 'County fips';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".rate IS 'indicator rate';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".pop IS 'total population';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".num IS 'number of people at or below the federal poverty line';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".rate_cv IS 'cv of indicator rate';")
print(comment)
dbSendQuery(con3, comment)

#disconnect
dbDisconnect(con3)
