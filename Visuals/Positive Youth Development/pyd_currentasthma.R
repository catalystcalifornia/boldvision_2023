####Step 1: Pull function from github script ####
source("bv_barchart_function.R")

####Step 2: Pull your by subgroup data and race labels metadata from the pgadmin BV database ####

source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("bold_vision")

#this TEMPLATE example will use the connected youth df
df_subgroup <- st_read(con, query = "select * from bv_2023.pyd_asthma_subgroup")

#pull race labels
race_label_df <- st_read(con, query = "select * from bv_2023.metadata_race_labels")

dbDisconnect(conn = con)

####Step 2: join your table to the metadata table to get the correct labels ####
#join to your table if needed rename subgroup to race
df_subgroup<-df_subgroup%>%rename(race=subgroup) # comment out this line if not needed

#Please do NOT rename dataframe tables as the function will pull from them. 
df_subgroup <- left_join(df_subgroup, race_label_df, by=c("race" = "race_base"))  %>%
  select(-"id")

####Step 3: Arrange by rate and take away the total and bipoc data if you have it
df <- subset(df_subgroup, race != "total" & race != "bipoc") %>%
  arrange(rate)

####Step 4: Run the function ####
fx_barchart_subgroup(
  df = df,
  #be sure to write in the domain this way so it reflects the correct folders that the function will insert the visual deliverables in. 
  domain = "Positive Youth Development",
  indicator = "Asthma",
  # insert a findings based systems led title
  title = "More NHPI, AIAN, and Multiracial youth on average have asthma",
  #explanation of what the we are looking at // use sentence case 
  subtitle = "Percent of youth who have asthma in Los Angeles County",
  #please follow the format of the datasource below
  caption_datasource = "Catalyst California's calculations based on LA County Department of Public Health, Office of Health Assessment and Epidemiology, Los Angeles County Health Survey, 2023.",
  #only input the full names for the groups that are in acronyms and do NOT modify this racenote unless necessary for their indicator
  caption_racenote = "Rates for AIAN and NHPI youth are unstable. AIAN=American Indian or Alaska Native; NHPI=Native Hawaiian or Pacific Islander",
  #define the indicator
  caption_indicator_def = "Asthma is defined as youth whose caregivers or themselves reported them as currently having asthma. Youth are defined as ages 0-24 years.",
  #define the unit of the data and remember to use quotations (i.e. "%" or "per 1k") 
  data_unit = "%"
)


