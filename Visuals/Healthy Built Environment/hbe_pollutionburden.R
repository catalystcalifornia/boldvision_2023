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
data_source <- "Catalyst California's calculations of GreenInfo Network's California Protected Areas (2023a) & School Campus (2021) Databases, California Community Care Licensing Division (2023), and California OEHHA CalEnviroScreen 4.0 (2021)."

####Step 4: Run the function ####
fx_barchart_subgroup(
  df = df,
  # be sure to write in the domain this way so it reflects the correct folders that the function will insert the visual deliverables in. 
  domain = "Healthy Built Environment",
  indicator = "Pollution Burden",
  # insert a findings based systems led title
  title = "Pollution burden around sensitive land uses is higher in areas where Latine youth live",
  # explanation of what the we are looking at // use sentence case 
  subtitle = "Average pollution burden percentile (%ile) around sensitive land uses for youth in LA County by race",
  # please follow the format of the datasource below
  caption_datasource = data_source,
  # only input the full names for the groups that are in acronyms and do NOT modify this racenote unless necessary for their indicator
  caption_racenote = "AIAN=American Indian or Alaska Native; NHPI=Native Hawaiian or Pacific Islander; Another Race=Persons who identify with a racial group not presented",
  # define the indicator
  caption_indicator_def = "Pollution burden around sensitive land uses is the average pollution burden percentile (%ile) around sensitive land uses for youth (ages 0-24 years old). A higher percentile indicates higher exposure to pollution.",
  # define the unit of the data and remember to use quotations (i.e. "%" or "per 1k") 
  data_unit = "%ile" # 
)
