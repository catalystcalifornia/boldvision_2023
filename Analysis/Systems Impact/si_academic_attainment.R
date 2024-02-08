# Task: Academic attainment among systems-impacted youth (HS graduation rates)

# packages
library(tidyverse)
library(readxl)
#remotes::install_github("catalystcalifornia/RC")
library(RC)
library(sf)

# Data Setup -------------------------------------------------------------------
source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("rda_shared_data")
df <- dbGetQuery(con, "SELECT * FROM education.cde_multigeo_calpads_graduation_2021_22")

# Data Dictionary: https://www.cde.ca.gov/ds/ad/fsacgr.asp

## filter for LA County, DASS Yes, reporting category

df_dass <- df  %>% filter(aggregatelevel %in% c("C", "S") & charterschool == "All" & dass == "Yes" & 
                            reportingcategory %in% c("TA", "RB", "RI", "RA", "RF", "RH", "RP", "RT", "RW") & countyname == "Los Angeles") %>%
  
  #select just fields we need
  dplyr::select(aggregatelevel, cdscode, countyname, districtname, reportingcategory, cohortstudents, regularhsdiplomagraduatescount, regularhsdiplomagraduatesrate)


# foster youth: filter for all charter,DASS-all, youth reporting category
df_foster <- df  %>% filter(aggregatelevel %in% c("C", "S") & charterschool == "All" & dass == "All" & 
                              reportingcategory %in% c("SF") & countyname == "Los Angeles") %>%
  
  #select just fields we need
  dplyr::select(aggregatelevel, cdscode, countyname, districtname, reportingcategory, cohortstudents, regularhsdiplomagraduatescount, regularhsdiplomagraduatesrate)


## combine raced groups with foster
df_subset<- rbind(df_dass, df_foster)

# rename cols
df_subset <- rename(df_subset, 
                    count = "regularhsdiplomagraduatescount",
                    pop = "cohortstudents",
                    rate = "regularhsdiplomagraduatesrate")

df_subset$reportingcategory <- gsub("TA", "total", df_subset$reportingcategory)
df_subset$reportingcategory <- gsub("RB", "nh_black", df_subset$reportingcategory)
df_subset$reportingcategory <- gsub("RI", "nh_aian", df_subset$reportingcategory)
df_subset$reportingcategory <- gsub("RA", "nh_asian", df_subset$reportingcategory)
df_subset$reportingcategory <- gsub("RF", "nh_filipino", df_subset$reportingcategory)
df_subset$reportingcategory <- gsub("RH", "latino", df_subset$reportingcategory)
df_subset$reportingcategory <- gsub("RP", "nh_pacisl", df_subset$reportingcategory)
df_subset$reportingcategory <- gsub("RT", "nh_twoormor", df_subset$reportingcategory)
df_subset$reportingcategory <- gsub("RW", "nh_white", df_subset$reportingcategory)
df_subset$reportingcategory <- gsub("SF", "foster", df_subset$reportingcategory)

df_subset<- rename(df_subset,c("name" = "countyname"))

# pivot wider
df_county <- df_subset %>% pivot_wider(names_from = reportingcategory, names_glue = "{reportingcategory}_{.value}", 
                                       values_from = c(count, pop, rate)) %>% filter(aggregatelevel == "C") %>% mutate(geoid = "06037") %>% select(geoid, name, ends_with("count"), ends_with("pop"), ends_with("rate")) 



#stats

df_county$asbest = 'max'    #YOU MUST UPDATE THIS FIELD AS NECESSARY: assign 'min' or 'max'
df_county <- count_values(df_county) #calculate number of "_rate" values
df_county<- calc_best(df_county) #calculate best rates -- be sure to update previous line of code accordingly before running this function.
df_county <- calc_diff(df_county) #calculate difference from best
df_county <- df_county %>% rename(foster_diff_rename = foster_diff) # rename foster group so we don't include this in index of disparity. The function looks for columns that end in "diff" so we rename this
df_county <- calc_id(df_county) #calculate index of disparity
df_county <- df_county %>% rename(foster_diff = foster_diff_rename) # rename back to foster_diff

# change to long ----------------------------------------------------------

subgroup_count <- df_county  %>% pivot_longer(
  cols = ends_with("count"),
  names_to = "subgroup",
  values_to = "count"
) %>% mutate(subgroup = gsub('_count', '', subgroup)) %>% select(geoid, name, subgroup, count) %>% filter(subgroup != "values")


subgroup_pop <- df_county  %>% pivot_longer(
  cols = ends_with("pop"),
  names_to = "subgroup",
  values_to = "pop"
) %>% mutate(subgroup = gsub('_pop', '', subgroup)) %>% select(geoid, name, subgroup, pop)

subgroup_rate <- df_county  %>% pivot_longer(
  cols = ends_with("rate"),
  names_to = "subgroup",
  values_to = "rate"
) %>% mutate(subgroup = gsub('_rate', '', subgroup)) %>% select(geoid, name, subgroup, rate)


subgroup_diff <- df_county  %>% pivot_longer(
  cols = ends_with("diff"),
  names_to = "subgroup",
  values_to = "diff"
) %>% mutate(subgroup = gsub('_diff', '', subgroup)) %>% select(geoid, name, subgroup, diff)

subgroup <- subgroup_count %>% left_join(subgroup_pop) %>% left_join(subgroup_diff)  %>% left_join(subgroup_rate)

# bipoc calcs
bipoc <- subgroup %>% filter(!subgroup %in% c("total", "foster", "nh_white")) %>% select(-diff) %>% mutate(count = sum(count), pop = sum(pop), diff = NA, rate = (count/pop), subgroup = "bipoc") %>% distinct(geoid, name, subgroup, count, pop, diff, rate)

# add bipoc to table
subgroup <- rbind(subgroup, bipoc)

# add best, index of disparity, values count, asbest. All are the same and were calculated earlier in wide format
subgroup$best <- df_county$best
subgroup$index_of_disparity <- df_county$index_of_disparity
subgroup$values_count <- df_county$values_count
subgroup$asbest <- df_county$asbest

# SPA Analysis ------------------------------------------------------------

# get school geoids (NCES District ID) - pull in active district records w/ geoids and names from CDE schools' list
schools_sf <- st_read(con, query = "SELECT* FROM education.cde_public_schools_2021_22 WHERE statustype = 'Active' AND county = 'Los Angeles'") 


# merge subset with school sf
df_school_sf  <- df_subset %>% filter(aggregatelevel == "S") %>% left_join(schools_sf) 

## I looked up these 20 schools in the California School Directory: https://www.cde.ca.gov/schooldirectory/. ##

df_school_sf  %>% filter(is.na(latitude))

#They did not appear in the website. One school (the one ending in cdscode 835) appeared but it was closed. We can filter out these schools.

df_school_sf  <- df_school_sf %>% filter(!is.na(latitude)) %>% select(-geom) %>% st_as_sf()

# spatial join school to SPA
la_spa <- st_read(con, query = "SELECT * FROM geographies_la.la_county_service_planning_areas_2022")

# find intersection SPA for each school, filter for foster and total
df_spa <- df_school_sf %>% st_join(la_spa) %>% as.data.frame() %>% filter(reportingcategory %in% c("foster","total"))

schools_no_spa <- df_spa %>% filter(is.na(SPA_NAME)) %>% select(cdscode, school, district, street, city, latitude, longitude)


df_spa %>% filter(is.na(SPA_NAME)) # 5 schools seem to be outside of LA county

# calculate rates
d <- df_spa %>%  filter(!is.na(SPA_NAME)) %>% group_by(SPA, SPA_NAME) %>% summarize(pop = sum(pop, na.rm = TRUE), count= sum(count, na.rm = TRUE), rate = (count/pop)) 



# rename column names and recode SPA names
d <- d %>% rename(geoid = SPA, name = SPA_NAME) %>% mutate(name = case_when(geoid == 1 ~ "Antelope Valley",
                                                                            geoid == 2 ~ "San Fernando Valley",
                                                                            geoid == 3 ~ "San Gabriel Valley",
                                                                            geoid == 4 ~ "Metro LA",
                                                                            geoid == 5 ~ "West LA",
                                                                            geoid == 6 ~ "South LA",
                                                                            geoid == 7 ~ "East LA",
                                                                            geoid == 8  ~ "South Bay"))

# Stats -------------------------------------------------------------------
df_region <- d
df_region$asbest <- "max"
df_region <- df_region %>% ungroup() %>%  mutate(best = max(df_region$rate, na.rm=T),
                                                 diff = best-rate,
                                                 values_count = length(df_region$rate) - sum(is.na(df_region$rate))) %>%
  mutate(
    sumdiff = sum(diff, na.rm = TRUE), 
    index_of_disparity = (sumdiff / best) / (values_count - 1) * 100) %>% select(-sumdiff)



# Push to Postgres --------------------------------------------------------


# Subgroup ----------------------------------------------------------------

###Send to Postgres###
con3 <- connect_to_db("bold_vision")
table_name <- "si_academic_attainment_subgroup"
schema <- 'bv_2023'

indicator <- "Academic Attainment"
source <- "California Department of Education Four-Year Adjusted Cohort Graduation Rate 2021-22: https://www.cde.ca.gov/ds/ad/filesacgr.asp. See QA doc for details: W:\\Project\\OSI\\Bold Vision\\BV 2023\\Documentation\\System Impact\\QA_Academic_Attainment.docx"
# dbWriteTable(con3, c(schema, table_name), subgroup,
#             overwrite = FALSE, row.names = FALSE)

#comment on table and columns


comment <- paste0("COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".geoid IS 'County number';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".name IS 'County Name';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".subgroup IS 'Race Group';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".count IS 'indicator numerator';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".pop IS 'indicator denominator';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".diff IS 'indicator difference from the best';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".rate IS 'indicator rate';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".best IS 'best rate among race groups';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".index_of_disparity IS 'index of disparity(note: this excludes foster group)';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".values_count IS 'number of values';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".asbest IS 'minimum or maximum as best';")
# dbSendQuery(con3, comment)


# SPA ---------------------------------------------------------------------

###Send to Postgres###
con3 <- connect_to_db("bold_vision")
table_name <- "si_academic_attainment_region"
schema <- 'bv_2023'

indicator <- "Academic Attainment"
source <- "California Department of Education Four-Year Adjusted Cohort Graduation Rate 2021-22: https://www.cde.ca.gov/ds/ad/filesacgr.asp. See QA doc for details: W:\\Project\\OSI\\Bold Vision\\BV 2023\\Documentation\\System Impact\\QA_Academic_Attainment.docx"
#dbWriteTable(con3, c(schema, table_name), df_region,
#             overwrite = FALSE, row.names = FALSE)

#comment on table and columns


comment <- paste0("
                  COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".geoid IS 'SPA number';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".name IS 'SPA Name';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".pop IS 'indicator denominator';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".count IS 'indicator numerator';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".rate IS 'indicator rate';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".asbest IS 'minimum or maximum as best';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".best IS 'best rate among race groups';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".diff IS 'indicator difference from the best';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".values_count IS 'number of values';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".index_of_disparity IS 'index of disparity';
                  ")
#dbSendQuery(con3, comment)


#disconnect
#dbDisconnect(con3)



