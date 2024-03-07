# Follow this template to use the bold vision function 

####Step 1: Pull function from github script ####
source("bv_barchart_function.R")

####Step 2: Pull your by subgroup data and race labels metadata from the pgadmin BV database ####

source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("bold_vision")

df_subgroup <- st_read(con, query = "select * from bv_2023.yp_baseorgs_subgroup")

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
  domain = "Youth Power",
  indicator = "Access to Base-Building Organizations",
  # insert a findings based systems led title
  title = "Advocacy and base-building organizations are less available where Asian and NHPI youth live",
  #explanation of what the we are looking at // use sentence case 
  subtitle = "Average percentile(%ile) access to base-building organizations by race in Los Angeles County",
  #please follow the format of the datasource below
  caption_datasource = "Catalyst California's calculations of the USC Equity Research Institute, California Health and Justice for All Power-Building Landscape: Defining the Ecosystem (2019); IRS Business Master File Extract (2023); and Cause IQ Nonprofit Directory (2022).",
  #only input the full names for the groups that are in acronyms and do NOT modify this racenote unless necessary for their indicator
  caption_racenote = "AIAN=American Indian or Alaska Native; NHPI=Native Hawaiian or Pacific Islander; Another Race=Persons who identify with a racial group not presented",
  #define the ind
  caption_indicator_def = "Access to advocacy and base-building organizations is defined as the average percentile (%ile) access each racial group (ages 0-24) has to organizations that are focused on youth advocacy and base-building based on youth population and organizational density in their area. A higher percentile indicates higher access.",
  #define the unit of the data and remember to use quotations (i.e. "%" or "per 1k") 
  data_unit = "%ile"
)

dbDisconnect(conn = con)
