#Calculate ECE enrollment rate by ZIP code using enrollment and population data from
#American Institutes of Research and California Childcare Resource & Referral Network
#QA doc: W:\Project\OSI\Bold Vision\BV 2023\Documentation\Positive Youth Development\QA_ECE_Access.docx


library(readxl)
library(dplyr)

source("W:\\RDA Team\\R\\credentials_source.R")

##### 1. AIR IT DATA ####

#get la county it data, converting asterisks to nulls
air_it <- read_xlsx("W:/Data/Education/American Institute for Research/2020/air_it_2020.xlsx", range = "A515:V960", na = "*")

#subset it data to columns we want
air_it <- air_it[c(1:2, 6, 10, 14, 18, 22)]

#rename columns
names(air_it) <- c("geoname", "pct_zip", "it", "it_under_85smi", "it_enrollment", "it_unmet_need", "it_pct_unmet_need")

#pick out la county
air_it_la <- air_it %>% filter(geoname == "Los Angeles") %>% select(-pct_zip)

#remove counties and rename geoid column
air_it <- air_it %>% filter(pct_zip != "Percent of Zip Code Allocation") %>% rename(geoid = geoname)



#### 2. AIR PREK DATA ####

#get prek data, converting asterisks to nulls
air_prek <- read_xlsx("W:/Data/Education/American Institute for Research/2020/air_prek_2020.xlsx", range = "A513:V958", na = "*")

#subset prek data to columns we want
air_prek <- air_prek[c(1:2, 3:4, 7:8, 11:12, 15:16, 19:20)]

#rename columns
names(air_prek) <- c("geoname", "pct_zip", 
                     "prek3","prek4", 
                     "prek_under_85smi3","prek_under_85smi4", 
                     "prek_enrollment3","prek_enrollment4", 
                     "prek_unmet_need3", "prek_unmet_need4",
                     "prek_pct_unmet_need3", "prek_pct_unmet_need4")

#add 3 and 4 year olds
air_prek <- air_prek %>% mutate(
  prek = prek3 + prek4,
  prek_under_85smi = prek_under_85smi3 + prek_under_85smi4,
  prek_enrollment = prek_enrollment3 + prek_enrollment4,
  prek_unmet_need = prek_unmet_need3 + prek_unmet_need4,
  prek_pct_unmet_need = (prek_unmet_need3 + prek_unmet_need4) / (prek_under_85smi3 + prek_under_85smi4)
) %>% select (geoname, pct_zip, prek, prek_under_85smi, prek_enrollment, prek_unmet_need, prek_pct_unmet_need)

#pick out la county
air_prek_la <- air_prek %>% filter(geoname == "Los Angeles") %>% select(-pct_zip)

#remove counties and pct_zip column as it is duplicative in join next
air_prek <- air_prek %>% filter(pct_zip != "Percent of Zip Code Allocation") %>% rename(geoid = geoname) %>% select(-pct_zip)


#### 2.5 AIR TK DATA ####

#get tk data
air_tk <- read_xlsx("W:/Data/Education/American Institute for Research/2020/tk.xlsx", range = "A513:F958", na = "*")
names(air_tk) <- c("geoname", "pct_zip", "three", "four", "five", "tk") 
air_tk <- air_tk %>% select(-"pct_zip", -"three", -"four", -"five")

#pick out la county
air_tk_la <- air_tk %>% filter(geoname == "Los Angeles")
air_tk <- air_tk %>% filter(geoname != "Los Angeles") %>% rename(geoid = geoname)


#### 3. CCCRRN DATA ####

#get CCCRRN data
cccrrn <- read_xlsx("W:/Data/Education/CCCRRN/2021/CatalystCA2021Data_rev.xlsx") %>% rename(geoid = ZIPCODE)

#format columns for join
air_it$geoid <- as.character(air_it$geoid)
air_it$pct_zip <- as.numeric(air_it$pct_zip)
air_prek$geoid <- as.character(air_prek$geoid)
cccrrn$geoid <- as.character(cccrrn$geoid)

#### 3.5 Join AIR IT, PREK, TK, & CCRRN data)

#join it, prek, and tk data
df <- left_join(air_it, air_prek, by = "geoid")
df <- left_join(df, cccrrn, by = "geoid")
df <- left_join(df, air_tk, by = "geoid")

#remove ZIP codes with less that 10% overlap in County and treat the rest as 100%
#the only ZIP remaining with less the 90% in is 91361, which is primarily open space outside of LA
df <- df %>% filter(pct_zip >= 10)



#### 4. calculate indicator ####

#calculating as we did for racecounts.v3.education.ece_zip_code_enrollment_rate_2018
#which assumes ccrrn capacity = full enrollment. 
df$children <- rowSums(df[,c("it", "prek")], na.rm = TRUE)
df$enrollment <- rowSums(df[,c("INFCAP", "PRECAP", "FCCCAP", "tk")], na.rm = TRUE)
df$enrollment_rate <- df$enrollment / df$children * 100

#clean up and format
df$enrollment_rate[df$enrollment_rate == "Inf"] <- 100
df$target_id <- "06037"
df$geoname <- "zcta"

ind_df <- df %>% select(target_id, geoid, geoname, enrollment_rate) %>%
  rename (sub_id = geoid, indicator = enrollment_rate)

ind_df$sub_id <- as.character(ind_df$sub_id)

#clean up
rm(list = c("air_it", "air_it_la", "air_prek", "air_prek_la", "air_tk", 
            "air_tk_la", "cccrrn", "df"))