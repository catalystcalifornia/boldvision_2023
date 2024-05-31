# Disaggregated Asian Youth (Under 25) LA County

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

#### Step 1: load the data ####

# PUMS Data
root <- "W:/Data/Demographics/PUMS/"
indicator_name <- "Disaggregated Asian"

  
# Load the people PUMS data
people <- fread(paste0(root, "CA_2021/psam_p06.csv"), header = TRUE, data.table = FALSE,
                colClasses = list(character = c("PUMA", "RACASN", "RAC2P")))
 
####  Step 2: filter households eligible for calculation  #### 

#create functions

## Select LA County people
eligible_hhs <- people %>% 
  
  #filtering for universe and LA county
  filter( grepl('037', PUMA) & AGEP < 25 & RAC1P == 6)


####  Step 3: set up and run survey and format  #### 

# add geoid and indicator
eligible_hhs$geoid <- "037"

#pull in pUMS data dictionary codes for RAC2P
race_codes <- read_excel("W:/Data/Demographics/PUMS/CA_2017_2021/PUMS_Data_Dictionary_2017-2021_RAC2P.xlsx")%>% 
  mutate_all(as.character) # created this excel document separate by opening PUMS Data Dictionary in excel and deleting everything but RAC2P
race_codes$RAC2P<-str_pad(race_codes$Code_1, 2, pad = "0")
race_codes <- race_codes %>% select(RAC2P, Description)

eligible_hhs <- left_join(eligible_hhs, race_codes, by=("RAC2P"))


########### find out the top 10 ------

table(eligible_hhs$RAC2P)
eligible_hhs_test <- eligible_hhs %>% group_by(RAC2P, geoid, Description) %>% summarize(indicator = n())


hhs_test2 <- eligible_hhs_test %>% select(RAC2P, geoid, indicator, Description) %>%
  ungroup()%>% as.data.frame() %>%
  arrange(desc(indicator)) %>%
  slice(1:10)
unique(hhs_test2$RAC2P) #"68" "43" "45" "49" "59" "38" "57" "48" "44" "42"
# View(hhs_test2)
################ regularly scheduled programming ----

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
d_long <- total %>% filter(indicator == "Disaggregated Asian" & !is.na(geoid))

# make data frame
d_long <- as.data.frame(d_long)

return(d_long)
}


#"68" "43" "45" "49" "59" "38" "57" "48" "44" "42"
eligible_hhs$indicator=(ifelse(eligible_hhs$RAC2P == "68", "Disaggregated Asian", "other"))
d_long <- pums_run(eligible_hhs) %>% mutate(indicator="Two or More Races")

eligible_hhs$indicator=(ifelse(eligible_hhs$RAC2P == "43", "Disaggregated Asian", "other"))
d_long2 <- pums_run(eligible_hhs) %>% mutate(indicator="Chinese, except Taiwanese, alone")

eligible_hhs$indicator=(ifelse(eligible_hhs$RAC2P == "45", "Disaggregated Asian", "other"))
d_long3 <- pums_run(eligible_hhs) %>% mutate(indicator="Filipino alone")

eligible_hhs$indicator=(ifelse(eligible_hhs$RAC2P == "49", "Disaggregated Asian", "other"))
d_long4 <- pums_run(eligible_hhs) %>% mutate(indicator="Korean alone")

eligible_hhs$indicator=(ifelse(eligible_hhs$RAC2P == "59", "Disaggregated Asian", "other"))
d_long5 <- pums_run(eligible_hhs) %>% mutate(indicator="All combinations of Asian races only")

eligible_hhs$indicator=(ifelse(eligible_hhs$RAC2P == "38", "Disaggregated Asian", "other"))
d_long6 <- pums_run(eligible_hhs) %>% mutate(indicator="Asian Indian alone")

eligible_hhs$indicator=(ifelse(eligible_hhs$RAC2P == "57", "Disaggregated Asian", "other"))
d_long7 <- pums_run(eligible_hhs) %>% mutate(indicator="Vietnamese alone")

eligible_hhs$indicator=(ifelse(eligible_hhs$RAC2P == "48", "Disaggregated Asian", "other"))
d_long8 <- pums_run(eligible_hhs) %>% mutate(indicator="Japanese alone")

eligible_hhs$indicator=(ifelse(eligible_hhs$RAC2P == "44", "Disaggregated Asian", "other"))
d_long9 <- pums_run(eligible_hhs) %>% mutate(indicator="Taiwanese alone")

eligible_hhs$indicator=(ifelse(eligible_hhs$RAC2P == "42", "Disaggregated Asian", "other"))
d_long10 <- pums_run(eligible_hhs) %>% mutate(indicator="Cambodian alone")

# bind all data frames in final
d_final <- rbind(d_long,d_long2, d_long3, d_long4, d_long5, d_long6, d_long7, d_long8, d_long9, d_long10)
d_final <- d_final %>% arrange(desc(num)) 

####  Step 6: final format and upload to Postgres  ####


###Send to Postgres###
con3 <- connect_to_db("bold_vision")
table_name <- "demo_disaggregated_asian"
schema <- 'bv_2023'

indicator <- "Disaggregated Asian Youth"
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
