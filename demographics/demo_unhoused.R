# Unhoused youth ages 0-24 in LA County
# Data Sources
# LAHSA 2023: https://www.lahsa.org/documents?id=7689-yc2023-la-coc-data-summary
# Glendale 2023: https://www.glendaleca.gov/home/showpublisheddocument/71115/638205459970370000
# Pasadena 2023: https://docs.google.com/spreadsheets/d/1ZEeF1KNOE7UFh493WRSreEI_O3WfSZ22CBJWkEXIgLk/edit?gid=1813887752#gid=1813887752
# Long Beach

# Environment Set Up ----
library("readxl")
library("stringr")
library("dplyr")

# create connection for bv database
source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("bold_vision")

# Data import and prep ----
#### LAHSA ----
source <- c("LAHSA")
sheltered <- c(1743)
unsheltered<-c(2151)
total<-c(3894)

# Join the variables to create a data frame
lahsa <- data.frame(source,sheltered,unsheltered,total)

#### Glendale ----
source <- c("Glendale")
sheltered <- c(44+8)
unsheltered<-c(4+2)
total<-sheltered+unsheltered

# Join the variables to create a data frame
glendale <- data.frame(source,sheltered,unsheltered,total)

#### Pasadena ----
source <- c("Pasadena")
sheltered <- c(24+13+5+12)
unsheltered<-c(0+20)
total<-sheltered+unsheltered

# Join the variables to create a data frame
pasadena <- data.frame(source,sheltered,unsheltered,total)

#### Long Beach ----
source <- c("Long Beach")
sheltered <-NA
unsheltered<-NA
total <- c(48+145)

# Join the variables to create a data frame
long_beach<- data.frame(source,sheltered,unsheltered,total)

# Calculate rate of unhoused across Continuum of Cares ----
# combine CoC dataframes
unhoused<-rbind(lahsa,glendale,pasadena,long_beach)

unhoused_total<-unhoused%>%
  mutate(indicator="Sheltered and unsheltered youth")%>%
  group_by(indicator)%>%summarise(count=sum(total))

# load population estimate for youth 0-24
youth_pop<-dbGetQuery(con, "SELECT * FROM bv_2023.acs_pums_multigeo_2021_youth_0_24_race_long WHERE geolevel = 'county' and raceeth='total'")

youth_pop<-youth_pop%>%select(geoid,pop)

final_df<-cbind(unhoused_total,youth_pop)%>%
  mutate(rate=count/pop*1000)%>%
  select(geoid,indicator,rate,count,pop)

# Push to Postgres ----
# function for adding table and column comments from RC
add_table_comments <- function(con, schema, table_name, indicator, source, column_names, column_comments) {
  comments <- character()
  comments <- c(comments, paste0("
    COMMENT ON TABLE ", schema, ".", table_name, " IS '", table_comment, "';"))
  for (i in seq_along(column_names)) {
    comment <- paste0("
      COMMENT ON COLUMN ", schema, ".", table_name, ".", column_names[i], " IS '", column_comments[i], "';
      ")
    comments <- c(comments, comment)
  }
  sql_commands <- paste(comments, collapse = "")
  dbSendQuery(con, sql_commands)
}

table_name <- "demo_unhoused"
schema <- 'bv_2023'

indicator <- "Estimated percentage and count of youth who are unhoused (sheltered or unsheltered). 
Includes youth 0-24"
source <- "Based on 2023 data from LAHSA, Glendale, Pasadena, and Long Beach Continuums of Care. Population data from ACS 2017-21 estimates
See QA doc for details: W:\\Project\\OSI\\Bold Vision\\BV 2023\\Documentation\\Demo_lgbt_unhoused.docx
Script: W:/Project/OSI/Bold Vision/BV 2023/GitHub/EMG/boldvision_2023/demographics/demo_unhoused.R"
table_comment <- paste0(indicator, source)

# write table
# dbWriteTable(con, c(schema, table_name),final_df,
#        overwrite = FALSE, row.names = FALSE)

#comment on table and columns
column_names <- colnames(final_df) # get column names

column_comments<-c(
  "Geoid for county",
  "Demographic indicator",
  "Rate of homelessess per 1K youth 0-24 in LA County",
  "Count of youth who are homeless either sheltered or unsheltered",
  "Total population of youth 0-24 in LA County"
)

add_table_comments(con, schema, table_name, indicator, source, column_names, column_comments)

