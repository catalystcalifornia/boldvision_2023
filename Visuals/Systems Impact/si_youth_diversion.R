# Chart for youth diversion using chart template

####Step 1: Pull function from github script ####
source("bv_barchart_function.R")

####Step 2: Pull your by subgroup data and race labels metadata from the pgadmin BV database ####

source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("bold_vision")
# youth diversion
df_subgroup <- st_read(con, query = "select * from bv_2023.si_youth_diversion_subgroup") %>% rename(race = subgroup)


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
    race == "nh_api", "API", race_label_short),
  
  race_label_long = ifelse(
    race == "nh_api", "Asian and Pacific Islander", race_label_long),
  
  race_description = ifelse(
    race == "nh_api", "Asian and Pacific Islander, non-Latine", race_description
  ))


####Step 4: Run the function ####
fx_barchart_subgroup(
  df = df,
  #be sure to write in the domain this way so it reflects the correct folders that the function will insert the visual deliverables in. 
  domain = "Systems Impact",
  indicator = "Youth Diversion",
  # insert a findings based systems led title
  title = "Black, API, and SWANA/SA youth are least likely to be diverted from arrests compared to other groups",
  #explanation of what the we are looking at // use sentence case 
  subtitle = "Youth diversion rate per 100 youth arrests in Los Angeles County",
  #please follow the format of the datasource below
  caption_datasource = "Catalyst California's calculations of Los Angeles County Sheriff Department (LASD), Los Angeles Police Department (LAPD), California Department of Justice (DOJ) Racial and Identity Profiling Act (RIPA), and Los Angeles County Department of Youth Development data, 2022.",
  #only input the full names for the groups that are in acronyms and do NOT modify this racenote unless necessary for their indicator
  caption_racenote = "AIAN=American Indian or Alaska Native; API = Asian and Pacific Islander; SWANA/SA=Southwest Asian (Middle Eastern) or North African, or South Asian; Multiracial = Persons identified as two or more races or another race. Estimates for multiracial may be unreliable due to data reporting differences",
  #define the indicator
  caption_indicator_def = "Youth diversion defined as youth (ages 10-17) who are enrolled in and substantially completed a diversion program in Los Angeles County per 100 youth arrests.",
  #define the unit of the data and remember to use quotations (i.e. "%" or "per 1k") 
  data_unit = " per 100"
)

dbDisconnect(conn = con)

