# Follow this template to use the bold vision function 

#### Step 1: Pull function from github script ####
source("bv_barchart_function.R")


#### Step 2: Pull your by subgroup data and race labels metadata from the pgadmin BV database ####
source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("bold_vision")

# this TEMPLATE example will use the connected youth df
df_subgroup <- st_read(con, query = "select * from bv_2023.hbe_pollution_burden_subgroup") %>%
  mutate(rate = rate * 100)

# pull race labels
race_label_df <- st_read(con, query = "select * from bv_2023.metadata_race_labels")

dbDisconnect(conn = con)


#### Step 3: join your table to the metadata table to get the correct labels ####
# join to your table if needed rename subgroup to race
df_subgroup <- df_subgroup %>%
  rename(race=subgroup) # comment out this line if not needed

# Please do NOT rename dataframe tables as the function will pull from them. 
df_subgroup <- left_join(df_subgroup, race_label_df, by=c("race" = "race_base"))  %>%
  select(-"id")


#### Step 4: Arrange by rate and take away the total and bipoc data if you have it
df <- subset(df_subgroup, race != "total" & race != "bipoc") %>%
  arrange(rate) 
data_source <- "Catalyst California's calculations based on GreenInfo Network California Protected Areas Database (2023a), California Community Care Licensing Division (2023), GreenInfo California School Campus Database (2021), California Office of Environmental Health Hazard Assessment CalEnviroScreen 4.0 (2021), Census DHC (2020)."

####Step 4: Run the function ####
fx_barchart_subgroup(
  df = df,
  # be sure to write in the domain this way so it reflects the correct folders that the function will insert the visual deliverables in. 
  domain = "Healthy Built Environment",
  indicator = "Pollution Burden",
  # insert a findings based systems led title
  title = "Pollution Burden impacts Latine Youth more than any other group",
  # explanation of what the we are looking at // use sentence case 
  subtitle = "Average Percentile of Pollution Burden by race in Los Angeles County",
  # please follow the format of the datasource below
  caption_datasource = data_source,
  # only input the full names for the groups that are in acronyms and do NOT modify this racenote unless necessary for their indicator
  caption_racenote = "AIAN=American Indian or Alaska Native; NHPI=Native Hawaiian or Pacific Islander; SWANA=Southwest Asian or North African/Middle Eastern or North African; Another Race=Persons who identify with a racial group not presented",
  # define the indicator
  caption_indicator_def = "Pollution Burden is the average percentile of pollution weighted for the proportion of sensitive land uses (e.g., early childcare facilities, schools, and parks).",
  # define the unit of the data and remember to use quotations (i.e. "%" or "per 1k") 
  data_unit = "" # percentile?
)