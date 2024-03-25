# Follow this template to use the bold vision function 

####Step 1: Pull function from github script ####
source("bv_barchart_function.R")

####Step 2: Pull your by subgroup data and race labels metadata from the pgadmin BV database ####

source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("bold_vision")

# youth arrest
df_subgroup <- st_read(con, query = "select * from bv_2023.si_youth_arrest_subgroup") %>% rename(race = subgroup)

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
  domain = "Systems Impact",
  indicator = "Youth Arrests",
  # insert a findings based systems led title
  title = "Black youth experience a markedly higher arrest rate compared to other racial groups",
  #explanation of what the we are looking at // use sentence case 
  subtitle = "Youth arrest rate by race in Los Angeles County.",
  #please follow the format of the datasource below
  caption_datasource = "Catalyst California's calculations of Los Angeles County Sheriff Department (LASD), Los Angeles Police Department (LAPD), and California Department of Justice (DOJ) Racial and Identity Profiling Act (RIPA) data, 2022.",
  #only input the full names for the groups that are in acronyms and do NOT modify this racenote unless necessary for their indicator
  caption_racenote = "AIAN=American Indian or Alaska Native; NHPI=Native Hawaiian or Pacific Islander; SWANA/SA=Southwest Asian (Middle Eastern) or North African, or South Asian; Multiracial = Persons identified as two or more races or another race",
  #define the indicator
  caption_indicator_def = "Youth (ages 0-24) arrested by any law enforcement agency in Los Angeles County.",
  #define the unit of the data and remember to use quotations (i.e. "%" or "per 1k") 
  data_unit = " per 1k"
)

dbDisconnect(conn = con)
