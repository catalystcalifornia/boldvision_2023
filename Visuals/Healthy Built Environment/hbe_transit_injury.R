# Follow this template to use the bold vision function 

####Step 1: Pull function from github script ####
source("bv_barchart_function.R")

####Step 2: Pull your by subgroup data and race labels metadata from the pgadmin BV database ####

source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("bold_vision")

# transit
df_subgroup <- st_read(con, query = "select * from bv_2023.hbe_transit_injury_subgroup") %>% rename(race = subgroup)

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
  domain = "Healthy Built Environment",
  indicator = "Transit Injuries",
  # insert a findings based systems led title
  title = "Black youth are killed or severely injured by traffic collisions more than any other group",
  #explanation of what the we are looking at // use sentence case 
  subtitle = "Youth pedestrians or bicyclists killed or severely injured in Los Angeles County, Per 100K youth",
  #please follow the format of the datasource below
  caption_datasource = "Catalyst California's calculations of the California Statewide Integrated Traffic Records System data, 2018-2022",
  #only input the full names for the groups that are in acronyms and do NOT modify this racenote unless necessary for their indicator
  caption_racenote = "AIAN=American Indian or Alaska Native; NHPI=Native Hawaiian or Pacific Islander; Another Race=Persons who identify with a racial group not presented",
  #define the indicator
  caption_indicator_def = "Transit injuries defined as youth (0-24) who are pedestrians or bicyclists and killed or severely injured by a traffic collision per 100K youth. Rates by race are extrapolated based on census tract demographics.",
  #define the unit of the data and remember to use quotations (i.e. "%" or "per 1k") 
  data_unit = ""
)









