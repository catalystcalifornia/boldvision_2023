# Youth (Under 25) with a disability in LA County as percentage of all LA County youth with a disability

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
indicator_name <- "disability"

  
# Load the people PUMS data
people <- fread(paste0(root, "CA_2021/psam_p06.csv"), header = TRUE, data.table = FALSE,
                colClasses = list(character = c("PUMA", "DIS", "DDRS","DEAR","DEYE", "DOUT", "DPHY")))


#create function
pums_run <- function(x){
  weight <- 'PWGTP' # using PWGTP b/c calculating people not households
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

## Select LA County people
disability_status <- people %>% 
  
  #filtering for universe and LA county
  filter(grepl('037', PUMA) & AGEP < 25)   %>% 
  # add geoid and indicator
  mutate(geoid = "037")

disability_status$indicator=(ifelse(disability_status$DIS == 1, "disability", "no disability"))

d_long <- pums_run(disability_status) %>% filter(indicator=="disability")

#filter for disability to change the denominator to the total number of people in LA County w/ a disability for the specific disabilities
disability_status_filtered <- disability_status %>% filter(DIS==1)

disability_status_filtered$indicator=(ifelse(disability_status_filtered$DDRS == 1, "self-care difficulty", "no disability"))
d_long2 <- pums_run(disability_status_filtered) %>% filter(indicator=="self-care difficulty")

disability_status_filtered$indicator=(ifelse(disability_status_filtered$DEAR == 1, "hearing difficulty", "no disability"))
d_long3 <- pums_run(disability_status_filtered)%>% filter(indicator=="hearing difficulty")

disability_status_filtered$indicator=(ifelse(disability_status_filtered$DEYE == 1, "vision difficulty", "no disability"))
d_long4 <- pums_run(disability_status_filtered)%>% filter(indicator=="vision difficulty")

disability_status_filtered$indicator=(ifelse(disability_status_filtered$DOUT == 1, "independent living difficulty", "no disability"))
d_long5 <- pums_run(disability_status_filtered)%>% filter(indicator=="independent living difficulty")

disability_status_filtered$indicator=(ifelse(disability_status_filtered$DPHY == 1, "ambulatory difficulty", "no disability"))
d_long6 <- pums_run(disability_status_filtered)%>% filter(indicator=="ambulatory difficulty")

# bind all data frames in final
df_final <- rbind(d_long, d_long2, d_long3, d_long4, d_long5, d_long6)

###Send to Postgres###
con3 <- connect_to_db("bold_vision")
table_name <- "demo_disability"
schema <- 'bv_2023'

indicator <- "Youth with a disability"
source <- "American Community Survey 2017-2021 5-year PUMS estimates. Rates for all disability types are calculated with youth population with a disability in LA County as the denominator except for general disability which is calculated with all youth in LA County as the denominator. Self-care difficulty and ambulatory difficulty do not include ages less than 5 years old. Independent living difficulty does not include youth ages less than 15 years old. Other than thone ones, all disability types were filtered to include youth ages 0-24. See QA doc for details: W:\\Project\\OSI\\Bold Vision\\BV 2023\\Documentation\\QA_Demo_Disability.docx"

dbWriteTable(con3, c(schema, table_name), df_final,
             overwrite = TRUE, row.names = FALSE)

#comment on table and columns
comment <- paste0("COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".geoid IS 'County fips';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".rate IS 'indicator rate';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".pop IS 'total population';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".num IS 'number of youth ages 0-24 with a disability';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".rate_cv IS 'cv of indicator rate';")
print(comment)
dbSendQuery(con3, comment)

#disconnect
dbDisconnect(con3)
