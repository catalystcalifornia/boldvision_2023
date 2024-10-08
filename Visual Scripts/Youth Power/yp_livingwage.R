#Living Wage 

####Step 1: Pull function from github script ####
source("bv_barchart_function.R")
# source("W:/Project/OSI/Bold Vision/BV 2023/GitHub/EMG/boldvision_2023/bv_barchart_function.R")

####Step 2: Pull your by subgroup data and race labels metadata from the pgadmin BV database ####

source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("bold_vision")

#this TEMPLATE example will use the connected youth df
df_subgroup <- st_read(con, query = "select * from bv_2023.yp_livingwage_subgroup")

#pull race labels
race_label_df <- st_read(con, query = "select * from bv_2023.metadata_race_labels")

####Step 2: join your table to the metadata table to get the correct labels ####
#join to your table if needed rename subgroup to race
# df_subgroup<-df_subgroup%>%rename(race=subgroup) # comment out this line if not needed

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
  domain = "Youth Power",
  indicator = "Living Wage",
  # insert a findings based systems led title
  title = "Black and Latine youth on average are less likely to receive a living wage",
  #explanation of what the we are looking at // use sentence case 
  subtitle = "Percent of youth receiving a living wage in Los Angeles County",
  #please follow the format of the datasource below
  caption_datasource = "Catalyst California's calculations based on American Community Survey Public Use Microdata, 2017-2021 5-Year Estimates.",
  #only input the full names for the groups that are in acronyms and do NOT modify this racenote unless necessary for their indicator
  caption_racenote = "AIAN=American Indian or Alaska Native; NHPI=Native Hawaiian or Pacific Islander; SWANA=Southwest Asian or North African/Middle Eastern or North African; Another Race=Persons who identify with a racial group not presented",
  #define the indicator
  caption_indicator_def = "Living Wage is defined as youth (15-24 years) who are in the labor force and who receive $16.90 (LA County Minimum Wage) or more per hour.",
  #define the unit of the data and remember to use quotations (i.e. "%" or "per 1k") 
  data_unit = "%"
)

dbDisconnect(conn = con)
