# Calculate the average tenure and budget of grassroots orgs in LA County
# Use the list of advocacy and base-building organizations generated from IRS/Cause IQ and USC Equity Research Institute

#### ENVIRONMENT SET UP ----
library(dplyr)
library(RPostgreSQL)
options(scipen=999)
library(sf)
library(lubridate)

source("W:\\RDA Team\\R\\credentials_source.R")
bv_con <- connect_to_db("bold_vision")

#### Load and explore data ----
bv_baseorgs <- st_read(bv_con, query = "select * from bv_2023.yp_baseorgs_list")

# see number with NA values for ruling date--when first filed as exempt org--and income fields
table(bv_baseorgs$RULING,useNA='always')
# 74 out of 222 are blank

table(bv_baseorgs$INCOME_CD,useNA='always')
# 74 out of 222 are blank

table(bv_baseorgs$INCOME_AMT,useNA='always')
# 74 out of 222 are blank

# Because Income CD still has same number of null values, let's just income amt
# check that we are dealing with unique EINs
check<-bv_baseorgs%>%group_by(ein)%>%summarise(count=n())
# there are some orgs with multiple sites, but the same EIN and budget, so make sure to take unique records

#### Calculate average tenure and budget of grassroots orgs ----
tenure<-bv_baseorgs%>%distinct(ein,RULING)%>%filter(!is.na(ein) & ein!='')%>%
  mutate(ruling_date=parse_date_time(RULING,"ym"),
         tenure_yrs=time_length(difftime(parse_date_time("2024-01-01","ymd"),ruling_date),"years"))

tenure<-tenure%>%mutate(indicator="Tenure")%>%
  group_by(indicator)%>%
  summarise(average=mean(tenure_yrs,na.rm=TRUE),
            median=median(tenure_yrs,na.rm=TRUE),
            count=n())

budget<-bv_baseorgs%>%distinct(ein,INCOME_AMT)%>%filter(!is.na(ein) & ein!='')%>%
  mutate(indicator="Budget")%>%
  group_by(indicator)%>%
  summarise(average=mean(as.numeric(INCOME_AMT),na.rm=TRUE),
            median=median(as.numeric(INCOME_AMT),na.rm=TRUE),
            count=n())

# because budget median and average are so different, go with median values for tenure and budget, but save both

#### Expore to postgres ----
df<-rbind(budget,tenure)

table_name <- "copb_tenure_budget"
schema <- 'bv_2023'
df_pg<-df

indicator <- "Average and median tenure and budget of advocacy and base-building organizations based on data from Cause IQ, IRS, and USC ERI.
 Calculations only include unique organizations with unique EINs registered with IRS
We do not have budget data for smaller orgs not registered with IRS. About 74 orgs on our list are missing from this analysis"
source <- "List is based on orgs from bv_2023.yp_baseorgs_list.
See script for details: W:/Project/OSI/Bold Vision/BV 2023/GitHub/EMG/boldvision_2023/Analysis/Community Organizing and Power Building/grassroots_orgs_stats.R"

# dbWriteTable(bv_con, c(schema, table_name), df_pg,
#              overwrite = FALSE, row.names = FALSE)

#comment on table and columns
comment <- paste0("COMMENT ON TABLE ", schema, ".", table_name,  " IS '", indicator, " from ", source, ".';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".indicator IS 'Indicator of interest--either tenure or budget of orgs';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".average IS 'Average for indicator';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".median IS 'Median for indicator';
                  COMMENT ON COLUMN ", schema, ".", table_name, ".count IS 'Count of orgs with unique EINs included in the calcs';
        
                  ")

dbSendQuery(bv_con, comment)

