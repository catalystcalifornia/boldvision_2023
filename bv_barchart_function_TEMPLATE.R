# Follow this template to use the bold vision function 

####Step 1: Pull function from github script ####
source("bv_visuals_functions.R")

####Step 2: Pull your by subgroup data and race labels metadata from the pgadmin BV database ####

source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("bold_vision")

#this TEMPLATE example will use the counnected youth df
df_subgroup <- st_read(con, query = "select * from bv_2023.pyd_connectedyouth_subgroup")

#pull race labels
race_label_df <- st_read(con, query = "select * from bv_2023.metadata_race_labels")

####Step 2: join your table to the metadata table to get the correct labels ####
#join to your table
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
  indicator = "Connected Youth",
  # insert a findings based systems led title
  title = "Educational Institutions and Employers are less likely to engage with Black, NHPI, and Latine Youth",
  #explanation of what the we are looking at // don't 
  subtitle = "Percent of Connected Youth by Race in Los Angeles County",
  #please follow the format of the datasource below
  caption_datasource = "American Community Survey Public Use Microdata, 2017-2021 5-Year Estimates.",
  #only input the full names for the groups that are in acronyms and do NOT modify this racenote unless necessary for their inidicator
  caption_racenote = "AIAN=American Indian or Alaska Native; NHPI=Native Hawaiian or Pacific Islander; SWANA=Southwest Asian or North African/Middle Eastern or North African; Another Race= persons who identify with a racial group not presented",
  #define the indicator
  caption_indicator_def = "Connected youth are defined as youth (ages 14-24) who are enrolled in school or are employed.",
  #define the unit of the data and remember to use quotations (i.e. "%" or "per 1k") 
  data_unit = "%"
)

dbDisconnect(conn = con)