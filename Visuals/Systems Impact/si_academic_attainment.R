# Follow this template to use the bold vision function 

####Step 1: Pull function from github script ####
source("bv_barchart_function.R")

####Step 2: Pull your by subgroup data and race labels metadata from the pgadmin BV database ####

source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("bold_vision")

# academic attainment
df_subgroup <- st_read(con, query = "select * from bv_2023.si_academic_attainment_subgroup") %>% rename(race = subgroup)

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

# Step 4: Add Race Labels for Foster/Filipinx

df <- df %>% mutate(
  race_label_short = ifelse(
    race == "foster", "Foster", race_label_short),
  
  race_label_long = ifelse(
    race == "foster", "Foster", race_label_long),
  
  race_description = ifelse(
    race == "foster", "Foster Students", race_description
  ),
  
    race_label_short = ifelse(
    race == "nh_filipino", "Filipine", race_label_short),
  
  race_label_long = ifelse(
    race == "nh_filipino", "Filipine", race_label_long),
  
  race_description = ifelse(
    race == "nh_filipino", "Filipine, non-Latine", race_description
  
  
  
  )
)
  
 
  
####Step 4: Run the function ####
fx_barchart_subgroup(
  df = df,
  #be sure to write in the domain this way so it reflects the correct folders that the function will insert the visual deliverables in. 
  domain = "Systems Impact",
  indicator = "Academic Attainment",
  # insert a findings based systems led title
  title = "Educational institutions are less likely to graduate systems-impacted AIAN and Black youth",
  #explanation of what the we are looking at // use sentence case 
  subtitle = "Four-year graduation rates among systems-impacted youth in Los Angeles County",
  #please follow the format of the datasource below
  caption_datasource = "Catalyst California's calculations of California Department of Education (CDE) Four-year Cohort Graduation rate data, 2021-2022",
  #only input the full names for the groups that are in acronyms and do NOT modify this racenote unless necessary for their indicator
  caption_racenote = "AIAN=American Indian or Alaska Native; NHPI=Native Hawaiian or Pacific Islander",
  #define the indicator
  caption_indicator_def = "Graduation rates among systems-impacted youth are defined as the percentage of foster youth and youth enrolled in Dashboard Alternative School Status (DASS) schools who graduate within four-years.",
  #define the unit of the data and remember to use quotations (i.e. "%" or "per 1k") 
  data_unit = "%"
)

dbDisconnect(conn = con)



