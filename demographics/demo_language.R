#Youth who live in a household that speak a language other than english ages 5-24

# Data Dictionary: https://www2.census.gov/programs-surveys/acs/tech_docs/pums/data_dict/PUMS_Data_Dictionary_2022.pdf

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


#### Step 1: load the data ####

# PUMS Data
root <- "W:/Data/Demographics/PUMS/"
indicator_name <- "language"
data_type <- "county" #race, spa, county
  
# Load the people PUMS data
people <- fread(paste0(root, "CA_2021/psam_p06.csv"), header = TRUE, data.table = FALSE,
                colClasses = list(character = c("PUMA", "HHL")))


# Load the housing PUMS data
housing <- fread(paste0(root, "CA_2021/psam_h06.csv"), header = TRUE, data.table = FALSE, colClasses = list(character = c("PUMA", "HHL")))


############ function that all the variables are going to use ---------
pums_run <- function(x){
  weight <- 'PWGTP' # using WGTP b/c calculating percentage of rent-burdened households
  repwlist = rep(paste0("PWGTP", 1:80))
  
  # create survey design
  
  pums_survey <- x %>%
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
  pums_calcs <- pums_survey  %>%
    group_by(geoid,indicator) %>%   # group by race cat
    summarise(
      num = survey_total(na.rm=T), # get the (survey weighted) count for the numerators
      rate = survey_mean()) %>%        # get the (survey weighted) proportion for the numerator
    left_join(pums_survey %>%                                        # left join in the denominators
                group_by(geoid) %>%                                     # group by geo
                summarise(pop = survey_total(na.rm=T))) %>%              # get the weighted total for overall geo
    mutate(rate=rate*100,
           rate_moe = rate_se*1.645*100,    # calculate the margin of error for the rate based on se
           rate_cv = ((rate_moe/1.645)/rate) * 100, # calculate cv for rate
           count_moe = num_se*1.645, # calculate moe for numerator count based on se
           count_cv = ((count_moe/1.645)/num) * 100) %>%   # calculate cv for numerator count
    as.data.frame()
  
  return(pums_calcs)
}

######## HHL Variable instead of LANX ######
# HHL == 1 # English
# HHL == 2 # Spanish
# HHL == 3 # Other Indo-European languages
# HHL == 4 # Asian and Pacific Island languages
# HHL == 5 # Other language

#filter
household_language <- people %>% 
  
  #filtering for universe and LA county
  filter(grepl('037', PUMA) & AGEP < 25)   %>% 
  
  # join their housing info  
  left_join(housing %>% filter(grepl('037', PUMA)), 
            by = c("SERIALNO", "PUMA")) %>%
  
  #remove records with no language reported
  filter(HHL!='') %>% 

  # add geoid and indicator
  mutate(geoid = "037")

# make data frame

household_language$indicator=(ifelse(household_language$HHL == 1, "English", 
                                  ifelse(household_language$HHL == 2, "Spanish",
                                         ifelse(household_language$HHL == 3, "Other Indo-European language",
                                                ifelse(household_language$HHL == 4, "Asian and Pacific Island languages",
                                                       ifelse(household_language$HHL == 5, "Other Language", "Other"))))))


df <- pums_run(household_language)

###Send to Postgres###
con3 <- connect_to_db("bold_vision")
table_name <- "demo_language"
schema <- 'bv_2023'

indicator <- "Language spoken at home other than English"
source <- "American Community Survey 2017-2021 5-year PUMS estimates. See QA doc for details: W:\\Project\\OSI\\Bold Vision\\BV 2023\\Documentation\\Healthy Built Environment\\QA_Housing_Burden.docx"

dbWriteTable(con3, c(schema, table_name), df,
             overwrite = TRUE, row.names = FALSE)

#comment on table and columns
comment <- paste0("COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".geoid IS 'County fips';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".rate IS 'indicator rate';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".pop IS 'total population';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".num IS 'number of people at or below the federal language line';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".rate_cv IS 'cv of indicator rate';")
print(comment)
dbSendQuery(con3, comment)

#disconnect
dbDisconnect(con3)
