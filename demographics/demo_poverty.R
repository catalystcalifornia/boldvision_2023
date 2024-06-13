# Youth Poverty in LA County ages 0-24

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
                colClasses = list(character = c("PUMA")))

## Select LA County people
poverty <- people %>% 
  
  #filtering for universe and LA county
  filter(grepl('037', PUMA) & AGEP < 25 & !is.na(POVPIP)) %>% 

  # add geoid and indicator
  mutate(geoid = "037")

poverty$indicator <- (ifelse(poverty$POVPIP <= 100, "at or below 100% FPL", "above poverty"))
View(poverty %>% select(geoid, PUMA, AGEP, POVPIP, indicator)) #check that it filtered correctly

pums_run <- function(x){
  weight <- 'PWGTP' # using PWGTP b/c calculating person values and not households
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
# make data frame
pov_100 <- pums_run(poverty) %>% filter(indicator == "at or below 100% FPL") %>%  as.data.frame()

#### Step 4: Repeat steps 2 and 3 above with 200% filter ####

poverty$indicator <- (ifelse(poverty$POVPIP <= 200, "at or below 200% FPL", "above poverty"))
View(poverty %>% select(geoid, PUMA, AGEP, POVPIP, indicator)) #check that it filtered correctly
# calculate estimates
pov_200 <- pums_run(poverty) %>%
  # select burdened
  filter(indicator == "at or below 200% FPL") %>% 
  #make sure it a dataframe
  as.data.frame()

# bind both data frames in final
df_final <- rbind(pov_100, pov_200)

### Send to Postgres ###
con3 <- connect_to_db("bold_vision")
table_name <- "demo_poverty"
schema <- 'bv_2023'

indicator <- "Poverty at the 100 and 200 percent levels"
source <- "American Community Survey 2017-2021 5-year PUMS estimates. See QA doc for details: W:\\Project\\OSI\\Bold Vision\\BV 2023\\Documentation\\QA_Demo_Poverty.docx"

dbWriteTable(con3, c(schema, table_name), df_final,
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
