# Follow this template to use the bold vision function 

####Step 1: Pull function from github script ####
source("bv_barchart_function.R")

####Step 2: Pull your by subgroup data and race labels metadata from the pgadmin BV database ####

source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("bold_vision")

#this TEMPLATE example will use the connected youth df
df_subgroup <- st_read(con, query = "select * from bv_2023.si_childwelfare_subgroup") 

#pull race labels
race_label_df <- st_read(con, query = "select * from bv_2023.metadata_race_labels")

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
  domain = "Systems Impact",
  indicator = "Foster care and Probation",
  # insert a findings based systems led title
  title = "Foster care and probation systems are most likely to affect Black and AIAN youth",
  #explanation of what the we are looking at // use sentence case 
  subtitle = "Average rates per 1K youth in foster care / probation in Los Angeles County",
  #please follow the format of the datasource below
  caption_datasource = "Catalyst California's calculations of California Child Welfare Indicators Project, LA County Point in Time Counts for Foster Care and Probation, UC Berkeley, 2023.",
  #only input the full names for the groups that are in acronyms and do NOT modify this racenote unless necessary for their indicator
  caption_racenote = "API = Asian or Pacific Islander; AIAN=American Indian or Alaska Native",
  #define the indicator
  caption_indicator_def = "Children in foster care or probation are defined as the average rate per 1000 youth ages 0-20 in foster care or probation based on quarterly point-in-time counts.",
  #define the unit of the data and remember to use quotations (i.e. "%" or "per 1k") 
  data_unit = " per 1K"
)

dbDisconnect(conn = con)
