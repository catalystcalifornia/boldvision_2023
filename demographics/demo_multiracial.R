# Multiracial Youth (Under 25) LA County

# Data Dictionary: https://www2.census.gov/programs-surveys/acs/tech_docs/pums/data_dict/PUMS_Data_Dictionary_2022.pdf

# Install packages if not already installed
packages <- c("tidyverse", "data.table","readxl","tidycensus", "srvyr","stringr", "openxlsx", "dplyr") 

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

# PUMS Data
root <- "W:/Data/Demographics/PUMS/"
indicator_name <- "multiracial"

  
# Load the people PUMS data
people <- fread(paste0(root, "CA_2021/psam_p06.csv"), header = TRUE, data.table = FALSE,
                colClasses = list(character = c("PUMA", "RAC1P", "RAC3P", "HISP")))


## Select LA County people
nh_multiracial_youth <- people %>% 
  
  #filtering for universe and LA county
  filter(grepl('037', PUMA) & AGEP < 25 & RAC1P == 9 & HISP == "01")   %>% 
  # add geoid and indicator
  mutate(geoid = "037")


#pull in PUMS data dictionary codes for RAC3P
race_codes <- read_excel("W:/Data/Demographics/PUMS/CA_2017_2021/PUMS_Data_Dictionary_2017-2021_RAC3P.xlsx")%>% mutate_all(as.character) # created this excel document separate by opening PUMS Data Dictionary in excel and deleting everything but RAC2P
race_codes$RAC3P<-str_pad(race_codes$Code_1, 3, pad = "0")
race_codes <- race_codes %>% select(RAC3P, Description)

nh_multiracial_youth <- left_join(nh_multiracial_youth, race_codes, by=("RAC3P"))%>%rename(multiracial_subgroup=Description)

weight <- 'PWGTP'  # weight
repwlist = rep(paste0("PWGTP", 1:80)) # replicate weights

multiracial_youth_svry <- nh_multiracial_youth %>%               
  as_survey_rep(
    variables = c(geoid, multiracial_subgroup),   # dplyr::select grouping variables
    weights = weight,                       # person weight
    repweights = repwlist,                  # list of replicate weights
    combined_weights = TRUE,                # tells the function that replicate weights are included in the data
    mse = TRUE,                             # tells the function to calc mse
    type="other",                           # statistical method
    scale=4/80,                             # scaling set by ACS
    rscale=rep(1,80)                        # setting specific to ACS-scaling
  )


###### Non-latine, multiracial ######
multiracial_youth_table <- multiracial_youth_svry %>%
  group_by(geoid,multiracial_subgroup) %>%   # group by asian subgroup multiracial_subgroup
  summarise(
    num = survey_total(na.rm=T), # get the (survey weighted) count for the numerators
    rate = survey_mean()) %>%        # get the (survey weighted) proportion for the numerator
  left_join( multiracial_youth_svry %>%                                        # left join in the denominators
               group_by(geoid) %>%                                     # group by geo
               summarise(pop = survey_total(na.rm=T))) %>%              # get the weighted total for overall geo
  mutate(rate=rate*100,
         rate_moe = rate_se*1.645*100,    # calculate the margin of error for the rate based on se
         rate_cv = ((rate_moe/1.645)/rate) * 100, # calculate cv for rate
         count_moe = num_se*1.645, # calculate moe for numerator count based on se
         count_cv = ((count_moe/1.645)/num) * 100)  # calculate cv for numerator count

# like asian recode anything under 1% as Another Multiracial Identity and rerun model

# recode rates under 1% and create a list for recoding
other_list<-multiracial_youth_table%>%filter(rate<1)%>%ungroup()%>%select(multiracial_subgroup)
other_list<-other_list$multiracial_subgroup


nh_multiracial_youth_re<-nh_multiracial_youth%>%
  mutate(multiracial_subgroup=ifelse(multiracial_subgroup %in% other_list, 'Another Multiracial Identity',multiracial_subgroup))

nh_multiracial_youth_svry <- nh_multiracial_youth_re%>%               
  as_survey_rep(
    variables = c(geoid, multiracial_subgroup),   # dplyr::select grouping variables
    weights = weight,                       # person weight
    repweights = repwlist,                  # list of replicate weights
    combined_weights = TRUE,                # tells the function that replicate weights are included in the data
    mse = TRUE,                             # tells the function to calc mse
    type="other",                           # statistical method
    scale=4/80,                             # scaling set by ACS
    rscale=rep(1,80)                        # setting specific to ACS-scaling
  )


###### nh_multiracial subgroups, nh_multiracial alone ######
multiracial_subgroups_table_re <- nh_multiracial_youth_svry %>%
  group_by(geoid, multiracial_subgroup) %>%   # group by nh_multiracial subgroup description
  summarise(
    num = survey_total(na.rm=T), # get the (survey weighted) count for the numerators
    rate = survey_mean()) %>%        # get the (survey weighted) proportion for the numerator
  left_join(nh_multiracial_youth_svry %>%                                        # left join in the denominators
              group_by(geoid) %>%                                     # group by geo
              summarise(pop = survey_total(na.rm=T))) %>%              # get the weighted total for overall geo
  mutate(rate=rate*100,
         rate_moe = rate_se*1.645*100,    # calculate the margin of error for the rate based on se
         rate_cv = ((rate_moe/1.645)/rate) * 100, # calculate cv for rate
         count_moe = num_se*1.645, # calculate moe for numerator count based on se
         count_cv = ((count_moe/1.645)/num) * 100) %>%   # calculate cv for numerator count
  arrange(desc(rate)) %>%
  slice(1:10)

# Upload to Postgres 

# connect to pgadmin
con3 <- connect_to_db("bold_vision")
table_name <- "demo_multiracial"
schema <- 'bv_2023'

indicator <- "Multiracial Youth"
source <- "American Community Survey 2017-2021 5-year PUMS estimates. See QA doc for details: W:\\Project\\OSI\\Bold Vision\\BV 2023\\Documentation\\Healthy Built Environment\\QA_Housing_Burden.docx"

dbWriteTable(con3, c(schema, table_name), multiracial_subgroups_table_re,
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
