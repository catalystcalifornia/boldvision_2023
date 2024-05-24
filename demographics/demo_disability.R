# Youth (Under 25) with a disability in LA County
# as percentage of all LA County households under 5

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
                colClasses = list(character = c("PUMA", "AGEP", "DIS", "DDRS","DEAR","DEYE", "DOUT", "DPHY")))
 
####  Step 2: filter households eligible for calculation  #### 

#create functions


## Select LA County people
eligible_hhs <- people %>% 
  
  #filtering for universe and LA county
  filter(!is.na(DIS) & grepl('037', PUMA) & AGEP < 25)   %>% 
  
  #remove records with no weights
  filter(!is.na(PWGTP)) %>%
  
  #filter for age 0-5 and select distinct households
  distinct(SERIALNO, .keep_all = TRUE)


####  Step 3: set up and run survey and format  #### 

# add geoid and indicator
eligible_hhs$geoid <- "037"
eligible_hhs$indicator=(ifelse(eligible_hhs$DIS == 1, "disability", "no disability"))


pums_run <- function(x){
weight <- 'PWGTP' # using WGTP b/c calculating percentage of rent-burdened households
repwlist = rep(paste0("PWGTP", 1:80))

# create survey design

hh_geo <- x %>%
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
d_long <- total %>% filter(indicator == "disability" & !is.na(geoid))

# make data frame
d_long <- as.data.frame(d_long)

return(d_long)
}


d_long <- pums_run(eligible_hhs)

eligible_hhs$indicator=(ifelse(eligible_hhs$DDRS == 1, "disability", "no disability"))
d_long2 <- pums_run(eligible_hhs) %>% mutate(indicator="self-care difficulty")

eligible_hhs$indicator=(ifelse(eligible_hhs$DEAR == 1, "disability", "no disability"))
d_long3 <- pums_run(eligible_hhs)%>% mutate(indicator="hearing difficulty")

eligible_hhs$indicator=(ifelse(eligible_hhs$DEYE == 1, "disability", "no disability"))
d_long4 <- pums_run(eligible_hhs)%>% mutate(indicator="vision difficulty")

eligible_hhs$indicator=(ifelse(eligible_hhs$DOUT == 1, "disability", "no disability"))
d_long5 <- pums_run(eligible_hhs)%>% mutate(indicator="independent living difficulty")

eligible_hhs$indicator=(ifelse(eligible_hhs$DPHY == 1, "disability", "no disability"))
d_long6 <- pums_run(eligible_hhs)%>% mutate(indicator="ambulatory difficulty")

# bind all data frames in final
d_final <- rbind(d_long, d_long2, d_long3, d_long4, d_long5, d_long6)


####  Step 6: final format and upload to Postgres  ####


###Send to Postgres###
con3 <- connect_to_db("bold_vision")
table_name <- "demo_disability"
schema <- 'bv_2023'

indicator <- "Youth with a disability"
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
