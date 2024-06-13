# Calculate access to grassroots and base-building orgs for youth 0-24, and by race, BIPOC, and SPA
# script preps the base indicator at the census tract level as measured by tracts with the least to highest access to grassroots, basebuilding, youth advocacy orgs
# method is based on the enhanced two-step floating catchment area (E2SFCA) method where access is a function of supply-demand ratio around each org (youth population served within 3,7,10 miles) and the sum of supply-demand ratios for each org within 3,7,10 miles of each census tract population-weighted centroid

#### ENVIRONMENT SET UP ----
library(data.table)
library(readxl)
library(dplyr)
library(RPostgreSQL)
library(sf)
library(tidyr)
library(stringr)
#install.packages("rPraat")
library(rPraat)
library(nngeo)
library(tidygeocoder)
options(scipen=999)


source("W:\\RDA Team\\R\\credentials_source.R")

#### Part 1: Load and prep grassroots orgs and IRS data ----
##### Step 1: Download new IRS data ----
# downloaded from this link for CA https://www.irs.gov/charities-non-profits/exempt-organizations-business-master-file-extract-eo-bmf
irs_bmf<-fread("W:/Project/OSI/Bold Vision/BV 2023/Data/Youth Power/Grassroots Base Building Orgs/eo_ca.csv")

##### Step 2: Import and prep youth thriving survey orgs ----
srvy_orgs <-  read_excel("W:/Project/OSI/Bold Vision/BV 2023/Data/Youth Power/Grassroots Base Building Orgs/Appendix 4 - BV_CBO database_4.21.23.xlsx",sheet="Youth CBOs")

# rename column and drop last row
colnames(srvy_orgs)[1]<-c("id")
srvy_orgs_final<-srvy_orgs%>% filter(row_number() <= n()-1)

# keep only youth serving orgs and orgs with an advocay tag
srvy_orgs_final<-srvy_orgs_final%>%filter(`youth-serving org class CC&Imo`==1)
srvy_orgs_final<-srvy_orgs_final%>%filter(grepl("Social advocacy | Civil rights and social justice | Civic / social organizations | Community Action Programs",Types,ignore.case=TRUE))

# test result for those without a primary activity of social advocacy
qa<-srvy_orgs_final%>%filter(`NAICS code, primary`!="813319: Social Advocacy Organizations")
qa<-srvy_orgs_final%>%filter(`NAICS code, primary`!="813319: Social Advocacy Organizations")%>%filter(!grepl("Social advocacy",Types,ignore.case=TRUE))
# checked websites of 2 remaining orgs and decided to remove

# keep those just with primary activity of social advocacy or social advocacy in their types listed
srvy_orgs_final<-srvy_orgs_final%>%filter(`NAICS code, primary`=="813319: Social Advocacy Organizations" | grepl("Social advocacy",Types,ignore.case=TRUE))

# clean up column names
names(srvy_orgs_final)<-tolower(names(srvy_orgs_final))

##### Step 3: Import and prep curated BV 2021 grassroots orgs ----
bv_con <- connect_to_db("bold_vision")
bv_baseorgs <- st_read(bv_con, query = "select * from bv_2021.grassroots_organizers_and_base_builders_budget")

##### Step 4: Join, prep, and clean master list of grassroots orgs ----
###### Step 4a: Create initial master list combining two data sources and matching to IRS ----
# clean up survey orgs
## clean up column names
srvy_orgs_final<-select(srvy_orgs_final, ein,name,"street address",city,state,"zip code",x,y,"website domain",types,issues,characteristics,"num. employees","num. volunteers")
colnames(srvy_orgs_final)<-c("ein","organization_name","full_address","city","state","zip","x","y","website","program_types","issues","characteristics","employees","volunteers")

## transform columns to match BV base building list
srvy_orgs_final <- transform(srvy_orgs_final, x = as.numeric(x), 
                             y = as.numeric(y),
                             ein=as.character(ein))
srvy_orgs_final<-srvy_orgs_final%>%
  separate(full_address, into = c("address1", "address2"), sep= "(?=No |Ste |Suite)", remove=FALSE, convert= TRUE)
srvy_orgs_final<-srvy_orgs_final%>%mutate(full_address=paste0(address1,", ",city,", ",state,", ",zip))
srvy_orgs_final$full_address<-gsub(" ,",",",srvy_orgs_final$full_address)

# clean up BV base building
## select and rename columns
bv_baseorgs_final<-select(bv_baseorgs,ein,organization,full_address,address,address2,city,state_name,zip,x,y,website)
colnames(bv_baseorgs_final)<-c("ein","organization_name","full_address","address1","address2","city","state","zip","x","y","website")

# Create a single list
## join everything together to start
all_orgs<-srvy_orgs_final%>%full_join(bv_baseorgs_final, by="ein")
View(all_orgs[c("organization_name.x","organization_name.y","full_address.x","full_address.y")])

## keep orgs without a match in survey dataset
all_orgs_a<-all_orgs%>%filter(is.na(organization_name.x))%>%select(1,17:26)
names(all_orgs_a) <-  sub(".y$","", names(all_orgs_a))

## keep survey orgs and duplicates, retaining addresses from base building list when appropriate
all_orgs_b<-all_orgs%>%filter(!is.na(organization_name.x) & !is.na(organization_name.y))%>%
  select(1,2,18:26)
names(all_orgs_b) <-  sub(".y$","", names(all_orgs_b))
names(all_orgs_b) <-  sub(".x$","", names(all_orgs_b))

### replace address for California Latinas for reproductive justice
all_orgs_b<-all_orgs_b%>%mutate(full_address=
                                  ifelse(organization_name=='California Latinas for Reproductive Justice',
                                         "533 Glendale Blvd, Los Angeles, CA, 90026",full_address),
                                address1=ifelse(organization_name=='California Latinas for Reproductive Justice',
                                                "533 Glendale Blvd",address1),
                                address2=ifelse(organization_name=='California Latinas for Reproductive Justice',
                                                "No 101",address2),
                                zip=ifelse(organization_name=='California Latinas for Reproductive Justice',
                                           "90026",zip),
                                x=ifelse(organization_name=='California Latinas for Reproductive Justice',
                                         -118.2611,x),
                                y=ifelse(organization_name=='California Latinas for Reproductive Justice',
                                         34.068680000000001,y))                                                             

## keep orgs without a match in BV base building dataset
all_orgs_c<-all_orgs%>%filter(is.na(organization_name.y))%>%select(1,2:11)
names(all_orgs_c) <-  sub(".x$","", names(all_orgs_c))

## join together
all_orgs_final<-rbind(all_orgs_a,all_orgs_b,all_orgs_c)

# match to updated IRS data
all_orgs_final<-all_orgs_final%>%left_join(irs_bmf%>%transform(ein = as.character(EIN)))


###### Step 4b: Find partial matches in IRS data to complete info for orgs missing detail-----
#Potential matches will return a filtered list of organizations from irs_bmf that fully/partially match to org names in order to get EIN numbers, addresses, and greater org detail
# in 'names' df. It should include which org from 'names' it matched and potentially join any other information 
# about the matched org that would be helpful to deduce accuracy of match (e.g. address, website, etc)
names <- all_orgs_final %>%
  filter(is.na(ein))%>%
  select(organization_name, full_address, website)
# Remove_tail function: 
# For org's with four words or less in the title, no words are dropped
# For longer org names, the last two words are dropped
# Note: dropped parentheses from names to avoid regex issues but can add back in
# Usually parentheses are at the end of names and the contents would get dropped anyway
# but there are at least two exceptions (YO! and Free San Francisco)

remove_tail <- function(x, sep = " "){
  x <- gsub("(", "", x, fixed = TRUE)
  x <- gsub(")", "", x, fixed = TRUE)
  x <- gsub("-", "", x, fixed = TRUE)
  number_of_words <- length(strsplit(x, split = sep, fixed = TRUE)[[1]])
  
  print(paste0("org_name: ", x))
  print(paste0("number of words: ", number_of_words))
  
  if (number_of_words <= 4) {
    search_phrase <- x
  }
  else {
    search_phrase <- unlist(lapply(strsplit(x, " "), function(i) paste(head(i, -2), collapse = sep)))
  }
  
  print(paste0("search phrase: ", search_phrase))
  return(search_phrase)
}

# Setting up an empty df to add filtered irs_bmf results
# Add in source match and source phrase to track which "organization_name" 
# each irs_bmf org matched to and also the search phrase used (as determined by remove_tail function)

potential_matches <- irs_bmf[0,] 
potential_matches$source_match <- ""
potential_matches$search_phrase <- ""


# For loop - could be a better way but going the naive route since the original solution
# wouldn't let us easily add the source org, etc
for (source_org in names$organization_name) {
  source_split <- remove_tail(source_org, sep=" ")
  print(paste0("source org - for loop: ", source_org))
  print(paste0("source split - for loop: ", source_split))
  
  temp <- irs_bmf %>%
    filter(str_detect(NAME, regex(source_split, ignore_case=TRUE)))
  
  temp$source_match <- source_org
  temp$search_phrase <- source_split
  
  potential_matches <- rbind(potential_matches, temp)
}

# Do a full join to see which organization_name are missing (no match in irs_bmf)
# and also add in hopefully helpful context on matched organizations (source_match, search_phrase, full_address, website)
potential_matches_final <- potential_matches %>%
  full_join(names, by=c("source_match" = "organization_name")) %>%
  select(EIN, NAME, STREET, CITY, STATE, ZIP, source_match, search_phrase, full_address, website, everything())

# checking number of org with preliminary matches
check <- potential_matches_final%>% filter(!is.na(EIN)) 
length(unique(check$source_match))

####### Step 4c: Review matches and clean data with new matches ----
# reviewed potential_matches_final, comparing name, address, and looking up individual websites. 
# Decided to match these eins to these orgs:
# 872249221 for PICO California
# 741563270 for Mexican American Legal Defense and Educational Fund (MALDEF)
# 863395355 for Los Angeles Black Worker Center
# 941506251 for League of Women Voters of California Education Fund, but rename to LEAGUE OF WOMEN VOTERS OF CALIFORNIA based on address and info online
# 872742396 for LA Forward
# 954531076 for East Los Angeles Community Corporation, found online
# 853440899 for Climate Justice Alliance 

# replace ein field for these orgs
all_orgs_d<-all_orgs_final%>%filter(is.na(ein))%>%select(1:11)%>%
  filter(organization_name %in% c("PICO California","Mexican American Legal Defense and Educational Fund (MALDEF)","Los Angeles Black Worker Center","League of Women Voters of California Education Fund","LA Forward","East Los Angeles Community Corporation","Climate Justice Alliance"))

all_orgs_d<-all_orgs_d%>%mutate(ein=ifelse(organization_name=='PICO California',"872249221",ein))
all_orgs_d<-all_orgs_d%>%mutate(ein=ifelse(organization_name=='Mexican American Legal Defense and Educational Fund (MALDEF)',"741563270",ein))
all_orgs_d<-all_orgs_d%>%mutate(ein=ifelse(organization_name=='Los Angeles Black Worker Center',"863395355",ein))
all_orgs_d<-all_orgs_d%>%mutate(ein=ifelse(organization_name=='League of Women Voters of California Education Fund',"941506251",ein))
all_orgs_d<-all_orgs_d%>%mutate(ein=ifelse(organization_name=='LA Forward',"872742396",ein))
all_orgs_d<-all_orgs_d%>%mutate(ein=ifelse(organization_name=='East Los Angeles Community Corporation',"954531076",ein))
all_orgs_d<-all_orgs_d%>%mutate(ein=ifelse(organization_name=='Climate Justice Alliance',"853440899",ein))

# rename League of Women Voters of California
all_orgs_d$organization_name[all_orgs_d$organization_name=="League of Women Voters of California Education Fund"]<-"League of Women Voters of California"

# join to irs data
all_orgs_d<-all_orgs_d%>%left_join(irs_bmf%>%transform(ein = as.character(EIN)))

###### Step 4d: Create final binded list and replace address for instances where address is missing, but in IRS data -----
all_orgs_final_a<-all_orgs_final%>%filter(!(organization_name %in% c("PICO California","Mexican American Legal Defense and Educational Fund (MALDEF)","Los Angeles Black Worker Center","League of Women Voters of California Education Fund","LA Forward","East Los Angeles Community Corporation","Climate Justice Alliance")))

all_orgs_final<-rbind(all_orgs_final_a,all_orgs_d)

# replace address field if NA and there is an address in irs data
all_orgs_final_b<-all_orgs_final%>%filter(is.na(address1))

all_orgs_final_b<-all_orgs_final_b%>%mutate(STREET=str_to_title(STREET))%>%
  separate(STREET, into = c("address1", "address2"), sep= "(?=No |Ste |Suite)", remove=FALSE, convert= TRUE)

# manually change climate justice alliance
all_orgs_final_b$address1[all_orgs_final_b$address1=="1960a University Ave"]<-"1960 University Ave"
all_orgs_final_b$address2[all_orgs_final_b$address1=="1960 University Ave"]<-"A"

# replace the rest of the address field
all_orgs_final_b<-all_orgs_final_b%>%mutate(
  zip=ifelse(is.na(zip),
             ZIP,zip),
  city=ifelse(is.na(city),
              str_to_title(CITY),city),
  state=ifelse(is.na(state),
               STATE,state),
  full_address=ifelse(!is.na(address1),paste0(address1,", ",city,", ",state,", ",zip),full_address))  

# add x,y based on google maps (tried OSM and they didn't geocode)
all_orgs_final_b$x[all_orgs_final_b$full_address=="1545 Wilshire Blvd , Los Angeles, CA, 90017-4508"]<--118.26913834791048
all_orgs_final_b$y[all_orgs_final_b$full_address=="1545 Wilshire Blvd , Los Angeles, CA, 90017-4508"]<-34.05499485616618
all_orgs_final_b$x[all_orgs_final_b$full_address=="1960 University Ave, Berkeley, CA, 94704-1238"]<-122.2740545
all_orgs_final_b$y[all_orgs_final_b$full_address=="1960 University Ave, Berkeley, CA, 94704-1238"]<-37.8715133

all_orgs_final<-rbind(all_orgs_final_b,all_orgs_final%>%filter(!is.na(address1)))


##### Step 5: Load and prep tract data -----
# get census tract polygons for first step - ratio of provider to demand/inhabitants within catchment area
con <- connect_to_db("rda_shared_data")
tract_polys<-st_read(con, query = "select * from geographies_ca.cb_2020_06_tract_500k",geom="geom_3310")

# get census tract weighted population centroids for 2nd step - supply locations within X distance from census tract centroid
tract_centroids <- fread("https://www2.census.gov/geo/docs/reference/cenpop2020/tract/CenPop2020_Mean_TR06.txt",data.table=F)

# Load tract population
youth_pop <- st_read(bv_con, query = "select * from bv_2023.dhc_tract_2020_youth_0_24_race")

# Join total youth population to tracts
tract_polys_pop<-tract_polys%>%filter(namelsadco=="Los Angeles County")%>%
  left_join(youth_pop%>%filter(race=="total"),by=c("ct_geoid"="geoid"))

# there are 2498 unique tracts in youth_pop, so 3 do not join here, but these 3 have 0 population

#### Part 2: Calculate accessibility of grassroots/civic orgs using the enhanced two-step floating catchment area method (E2SFCA) ----
# Reference: https://ij-healthgeographics.biomedcentral.com/articles/10.1186/s12942-017-0105-9

###### Step 6A: Step 1 Calculate the supply-to-demand ratio for each org ------
# We will calculate the population within 3 zones of each organization, using buffers of 0-3 miles, 4-7 miles, 8-10 miles
# employ a fast-step decay function of 1.00, 0.42 and 0.09
# Filter orgs with x,y
all_orgs_geom<-all_orgs_final%>%filter(!is.na(x) & !is.na(y))%>%select(ein,organization_name,full_address,INCOME_AMT,x,y)
all_orgs_geom<-st_as_sf(all_orgs_geom, coords=c("x","y"),crs=4326,agr="identity")


# transform spatial data frames
all_orgs_geom<-st_transform(all_orgs_geom, crs=3310)
st_crs(tract_polys_pop) # right system
tract_polys_pop$ct_area<-st_area(tract_polys_pop)
tract_polys_pop<-tract_polys_pop%>%select(ct_geoid,pop,ct_area)

# filter for orgs in LA County
county_polys<-st_read(con, query = "select * from geographies_ca.cb_2020_06_county_500k where countyfp='037'",geom="geom_3310")
st_crs(county_polys)
all_orgs_geom<-st_filter(all_orgs_geom,county_polys)

# Create buffers around each org at 0-3 miles, 4-7 miles, 8-10 miles
## 3 miles in meters: 4828.032
all_orgs_geom_3 <- st_buffer(all_orgs_geom, 4828.032)
orgs_tracts_3 <-st_intersection(all_orgs_geom_3, tract_polys_pop)
orgs_tracts_3$intersect_area <- st_area(orgs_tracts_3)
orgs_tracts_3$prc_ct_area <- as.numeric(orgs_tracts_3$intersect_area/orgs_tracts_3$ct_area)
orgs_pop_3<-orgs_tracts_3%>%
  mutate(prc_ct_area=ifelse(prc_ct_area>=1,1,prc_ct_area))%>%
  group_by(ein,organization_name,full_address)%>%
  summarise(pop_3mi=sum(prc_ct_area*pop))%>%
  as.data.frame()%>%
  select(-geometry)

## 7 miles in meters: 11,265.408 
all_orgs_geom_7 <- st_buffer(all_orgs_geom, 11265.408)
orgs_tracts_7 <-st_intersection(all_orgs_geom_7, tract_polys_pop)
orgs_tracts_7$intersect_area <- st_area(orgs_tracts_7)
orgs_tracts_7$prc_ct_area <- as.numeric(orgs_tracts_7$intersect_area/orgs_tracts_7$ct_area)
orgs_pop_7<-orgs_tracts_7%>%
  mutate(prc_ct_area=ifelse(prc_ct_area>=1,1,prc_ct_area))%>%
  group_by(ein,organization_name,full_address)%>%
  summarise(pop_7mi=sum(prc_ct_area*pop))%>%
  as.data.frame()%>%
  select(-geometry)

## 10 miles in meters: 16,093.44
all_orgs_geom_10 <- st_buffer(all_orgs_geom, 16093.44)
orgs_tracts_10 <-st_intersection(all_orgs_geom_10, tract_polys_pop)
orgs_tracts_10$intersect_area <- st_area(orgs_tracts_10)
orgs_tracts_10$prc_ct_area <- as.numeric(orgs_tracts_10$intersect_area/orgs_tracts_10$ct_area)
orgs_pop_10<-orgs_tracts_10%>%
  mutate(prc_ct_area=ifelse(prc_ct_area>=1,1,prc_ct_area))%>%
  group_by(ein,organization_name,full_address)%>%
  summarise(pop_10mi=sum(prc_ct_area*pop))%>%
  as.data.frame()%>%
  select(-geometry)


# calculate the ratio per 1000 habitants
# employ a fast-step decay function of 1.00 @ 3, 0.42 @ 7 and 0.09 @ 10
step1<-all_orgs_geom%>%left_join(orgs_pop_3)%>%left_join(orgs_pop_7)%>%left_join(orgs_pop_10)

step1<-step1%>%
  mutate(pop_total=pop_3mi*1+(pop_7mi-pop_3mi)*.42+(pop_10mi-pop_7mi)*.09,
         r=(pop_3mi*1+(pop_7mi-pop_3mi)*.42+(pop_10mi-pop_7mi)*.09)/1000,
         r_final=1/((pop_3mi/1000*1+(pop_7mi-pop_3mi)/1000*.42+(pop_10mi-pop_7mi)/1000*.09)))


###### Step 6B: Step 2 Calculate the supply-to-demand ratio for each org ------
tract_centroids_geom<-st_as_sf(tract_centroids, coords=c("LONGITUDE","LATITUDE"),crs=4326,agr="identity")%>%filter(COUNTYFP==37)
tract_centroids_geom<-st_transform(tract_centroids_geom, crs=3310)

# Create buffers around each ct centroid @ 3mi, 7mi, 10mi
## 3 miles in meters: 4828.032
tracts_orgs_3<-st_join(tract_centroids_geom,step1,join=st_is_within_distance,dist=4828.032)
tracts_supply_3<-tracts_orgs_3%>%group_by(TRACTCE)%>%
  summarise(org_count_3=sum(!is.na(organization_name)),
            pop_total_3=sum(pop_total,na.rm=TRUE),
            r_3=sum(r,na.rm=TRUE),
            r_3_final=sum(r_final,na.rm=TRUE))%>%
  as.data.frame()%>%
  select(-geometry)

## 7 miles in meters: 11,265.408 
tracts_orgs_7<-st_join(tract_centroids_geom,step1,join=st_is_within_distance,dist=11265.408)
tracts_supply_7<-tracts_orgs_7%>%group_by(TRACTCE)%>%
  summarise(org_count_7=sum(!is.na(organization_name)),
            pop_total_7=sum(pop_total,na.rm=TRUE),
            r_7=sum(r,na.rm=TRUE),
            r_7_final=sum(r_final,na.rm=TRUE))%>%
  as.data.frame()%>%
  select(-geometry)

## 10 miles in meters: 16,093.44
tracts_orgs_10<-st_join(tract_centroids_geom,step1,join=st_is_within_distance,dist=16093.44)
tracts_supply_10<-tracts_orgs_10%>%group_by(TRACTCE)%>%
  summarise(org_count_10=sum(!is.na(organization_name)),
            pop_total_10=sum(pop_total,na.rm=TRUE),
            r_10=sum(r,na.rm=TRUE),
            r_10_final=sum(r_final,na.rm=TRUE))%>%
  as.data.frame()%>%
  select(-geometry)

# calculate the final accessibility ratio
# employ a fast-step decay function of 1.00 @ 3, 0.42 @ 7 and 0.09 @ 10
step2<-tract_centroids_geom%>%left_join(tracts_supply_3)%>%left_join(tracts_supply_7)%>%left_join(tracts_supply_10)

step2<-step2%>%
  mutate(org_count=org_count_10,
         a_sum=pop_total_3*1+(pop_total_7-pop_total_3)*.42+(pop_total_10-pop_total_7)*.09,
         a=r_3*1+(r_7-r_3)*.42+(r_10-r_7)*.09,
         a_final=r_3_final*1+(r_7_final-r_3_final)*.42+(r_10_final-r_7_final)*.09)

##### Step 6C: Convert access score to percentiles per tract ----
indicator_df<-step2 %>% 
  mutate(indicator = percent_rank(a)*100,
         indicator_final=percent_rank(a_final)*100)

##### Step 7: Send orgs and indicator data frame to postgres ----
# orgs
table_name <- "yp_baseorgs_list"
schema <- 'bv_2023'
df_pg<-all_orgs_final

indicator <- "List of youth-serving base building, grassroots, and social advocacy orgs"
source <- "CauseIQ youth thriving list generated by Imoyase and Catalyst as well as 2021 base building list generated with Community Council and USC ERI
See script for details: W:/Project/OSI/Bold Vision/BV 2023/R/Youth Power/yp_baseorgs_indicator_prep.R
See IRS BMF master file for data dictionary W:\\Project\\OSI\\Bold Vision\\BV 2023\\Data\\Youth Power\\Grassroots Base Building Orgs\\eo_info.pdf"

# dbWriteTable(bv_con, c(schema, table_name), df_pg,
#              overwrite = FALSE, row.names = FALSE)

#comment on table and columns
comment <- paste0("COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".ein IS 'Employer identification number, null for orgs that verified to be registered with IRS as c3 or c4';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".organization_name IS 'Name of organization';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".full_address IS 'Full address of organization that was geocoded. Address may be verified through IRS or could be independently provided by partner orgs in instances where org is a satellite office or chapter of an organization headquartered elsewhere. Null for some orgs without addresses provided or in IRS data';")
# print(comment)
dbSendQuery(bv_con, comment)

# indicator
# clean census tract column and columns
indicator_df$ct_geoid<-paste0("0",indicator_df$STATEFP,"0",indicator_df$COUNTYFP,indicator_df$TRACTCE)
indicator_df<-indicator_df%>%select(ct_geoid,indicator,indicator_final,a,a_final,org_count,org_count_3,r_3,r_3_final,org_count_7,r_7,r_7_final,org_count_10,r_10,r_10_final)%>%st_drop_geometry()

table_name <- "yp_baseorgs_tract_indicator"
schema <- 'bv_2023'
df_pg<-indicator_df

indicator <- "Access to grassroots, base building, or social advocacy organizations based on the nhanced two-step floating catchment area (E2SFCA) method 
E2SFCA calculates access as a function of supply-demand ratio for each org calculated as the initial ratio per 1K youth within catchment areas (defined within 3,7,10 miles) and a sum of supply-to-demand ratios for each demand location (census tract) within 3,7,10 miles
The result is a percentile rank of each census tract where 100th percentile indicates greater access
See https://ij-healthgeographics.biomedcentral.com/articles/10.1186/s12942-017-0105-9 for more information on methodology
and https://www.brookings.edu/articles/building-for-proximity-the-role-of-activity-centers-in-reducing-total-miles-traveled/ for how buffer zones where identified"
source <- "CauseIQ youth thriving list generated by Imoyase and Catalyst as well as 2021 base building list generated with Community Council and USC ERI 
See script for details: W:/Project/OSI/Bold Vision/BV 2023/R/Youth Power/yp_baseorgs_indicator_prep.R
Calculations based on census tract population weighted centroids for 2020 census tracts"

dbWriteTable(bv_con, c(schema, table_name), df_pg,
             overwrite = FALSE, row.names = FALSE)

#comment on table and columns
comment <- paste0("COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".ct_geoid IS 'Census tract geoid only CA tracts included';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".indicator IS 'Access percentile by tract with 100th percentile indicating greater access to base building and advocacy youth-serving orgs accounting for supply and demand';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".a IS 'Final accessibility score where larger a implies better accessiblity at tract level, used to calculate percentiles';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".org_count IS 'Total orgs within 10 miles of ct population weighted centroids';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".org_count_3 IS 'Total orgs within 3 miles of ct population weighted centroids';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".org_count_7 IS 'Total orgs within 7 miles of ct population weighted centroids';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".org_count_10 IS 'Total orgs within 10 miles of ct population weighted centroids';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".r_3 IS 'Step 1 of E2SFCA initial ratio of supply (1 org) per 1K youth within 3 miles';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".r_7 IS 'Step 1 of E2SFCA initial ratio of supply (1 org) per 1K youth within 7 miles';
                   COMMENT ON COLUMN ", schema, ".", table_name, ".r_10 IS 'Step 1 of E2SFCA initial ratio of supply (1 org) per 1K youth within 10 miles';
                  ")
# print(comment)
dbSendQuery(bv_con, comment)

