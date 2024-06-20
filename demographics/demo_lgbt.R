# Demographic estimates for LGBT youth in LA County
# Age range 0-24
# data source: https://ask.chis.ucla.edu/
# Data available includes (1) transgender or gender non-conforming youth ages 12-17, and 18-24, and (2) LGB youth ages 18-24-sexual orientation not asked of youth under 18
# Pooled years 2019-2022


# Environment Set Up ----
library("readxl")
library("stringr")
library("dplyr")

# create connection for bv database
source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("bold_vision")

# Data import and prep ----
#### transgender and gender non-conforming youth ages 12-17 from teen sample ----
trans_12_17 <- read_excel("W:/Project/OSI/Bold Vision/BV 2023/Data/Demographics/AskCHISResults202406121117.xlsx")

# set column names
colnames(trans_12_17)<-c("variable","percent","ci_95","population")

# drop extra rows without data
trans_12_17<-trans_12_17%>%filter(!is.na(variable) & !is.na(percent))

#### transgender/gender non-conforming and LGB youth ages 18-24 from adult sample----
lgbt_18_24 <- read_excel("W:/Project/OSI/Bold Vision/BV 2023/Data/Demographics/AskCHISResults202406121121.xlsx")

# set column names
colnames(lgbt_18_24)<-c("variable","percent_cis","ci_95_cis","population_cis","percent_trans","ci_95_trans","population_trans","percent","ci_95","population")

# drop extra rows without data
lgbt_18_24<-lgbt_18_24%>%select(1:10)%>%filter(!is.na(variable) & !is.na(population))

# Calculate LGB youth ages 18-24 ----
# select the record we need with percent and count LGB
lgb<-lgbt_18_24%>%filter(variable=='Gay, lesbian, or homosexual, Bisexual')%>%
  select(variable,percent,ci_95,population)

# calculate cv
lgb[c('ci_95_low','ci_95_high')]<-str_split_fixed(lgb$ci_95," - ",2)

# convert columns to numeric and clean up
i <- c('percent','population','ci_95_low','ci_95_high')  
lgb <- lgb %>% mutate_at(vars(i), as.numeric)

# cv calc
lgb <- lgb %>% mutate(rate_moe = (ci_95_high - ci_95_low)/2,
                      rate_cv = (rate_moe/1.96)/percent*100)

# clean up df
lgb<-lgb%>%select(variable,percent,population,rate_cv,ci_95_low,ci_95_high)%>%
  rename(indicator=variable,rate=percent,count=population)

# add total population for 18-24
population<-lgbt_18_24%>%filter(variable=='Total')%>%select(population)%>%as.numeric()

lgb<-cbind(lgb,population)

# Calculate Transgender youth 12-24 ----
#### pull 18-24 trans count and population ----
trans_18_24<-lgbt_18_24%>%filter(variable=='Total')%>%select(variable,population_trans,population)

# convert columns to numeric and clean up
i <- c('population','population_trans')  
trans_18_24<-trans_18_24%>%mutate(across(i, ~as.numeric(.)))%>%
  rename(trans_count=population_trans)

#### pull 12-17 trans count and population ----
trans_12_17_re<-trans_12_17%>%filter(variable=='Transgender or gender non-conforming')%>%
  select(variable,population)%>%
  rename(trans_count=population)

# add total population for 12-17
population<-trans_12_17%>%filter(variable=='Total')%>%select(population)

trans_12_17_re<-cbind(trans_12_17_re,population)

# convert columns to numeric and clean up
i <- c('population','trans_count')  
trans_12_17_re<-trans_12_17_re%>%mutate(across(i, ~as.numeric(.)))

#### join 2 age groups for total rate ----
trans<-rbind(trans_18_24,trans_12_17_re)

trans<-trans%>%mutate(indicator='Transgender or gender non-conforming')%>%
  group_by(indicator)%>%
  summarise(count=sum(trans_count),population=sum(population),rate=count/population*100)%>%
  select(indicator,rate,count,population)

# Bind LGB and Trans data frames ----
lgb<-lgb%>%select(indicator,rate,count,population,everything())
lgbt<-bind_rows(trans,lgb)%>%
  mutate(indicator=ifelse(indicator=='Gay, lesbian, or homosexual, Bisexual','Lesbian, Gay, or Bisexual (LGB)',indicator))

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

table_name <- "demo_lgbt"
schema <- 'bv_2023'

indicator <- "Estimated percentage and count of youth who are LGB or transgender/gender non-conforming. Based on data from CHIS. LGB youth is just for ages 18-24, while transgender and gender non-conforming includes youth 12-24. LGB data not available for youth 12-17"
source <- "CHIS 2019-2022 pooled estimates teen and adult samples
See QA doc for details: W:\\Project\\OSI\\Bold Vision\\BV 2023\\Documentation\\Demo_lgbt_unhoused.docx
Script: W:/Project/OSI/Bold Vision/BV 2023/GitHub/EMG/boldvision_2023/demographics/demo_lgbt.R"
table_comment <- paste0(indicator, source)

# write table
# dbWriteTable(con, c(schema, table_name),lgbt,
#        overwrite = FALSE, row.names = FALSE)

#comment on table and columns
column_names <- colnames(lgbt) # get column names

column_comments<-c(
  "Demographic indicator count and rates are for",
  "Rate or percentage of youth population",
  "Count of youth identifying with indicator",
  "Total population of youth data are available for - Transgender is for 12-24 and LGB for 18-24, LGB data not available for youth 12-17",
  "Coefficient of variation, not calculated for transgender",
  "lower bound of 95% CI not calculated for transgender",
  "higher bound of 95% CI not calculated for transgender"
)

add_table_comments(con, schema, table_name, indicator, source, column_names, column_comments)
