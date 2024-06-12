# Demographic estimates for LGBT youth in LA County
# age range 0-24
# data source: https://ask.chis.ucla.edu/

# Environment Set Up ----
library("readxl")
library("stringr")
library("dplyr")

# create connection for bv database
source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("bold_vision")


# Data import and prep ----
# transgender and gender non-conforming youth ages 12-17
trans_12_17 <- read_excel("W:/Project/OSI/Bold Vision/BV 2023/Data/Demographics/AskCHISResults202406121117.xlsx")

# set column names
colnames(trans_12_17)<-c("variable","percent","ci_95","population")

# drop extra rows without data
trans_12_17<-trans_12_17%>%filter(!is.na(variable) & !is.na(percent))

# transgender/gender non-conforming and LGB youth ages 18-24
lgbt_18_24 <- read_excel("W:/Project/OSI/Bold Vision/BV 2023/Data/Demographics/AskCHISResults202406121121.xlsx")

# set column names
colnames(lgbt_18_24)<-c("variable","percent_cis","ci_95_cis","population_cis","percent_trans","ci_95_trans","population_trans","percent","ci_95","population")

# drop extra rows without data
lgbt_18_24<-lgbt_18_24%>%select(1:10)%>%filter(!is.na(variable) & !is.na(population))

# Calculate LGB youth ----
# only available for 18-24
lgb<-lgbt_18_24%>%filter(variable=='Gay, lesbian, or homosexual, Bisexual')%>%select(variable,percent,ci_95,population)

# calculate cv
lgb[c('ci_95_low','ci_95_high')]<-str_split_fixed(lgb$ci_95," - ",2)

# convert columns to numeric and clean up
i <- c('percent','population','ci_95_low','ci_95_high')  
lgb <- lgb %>% mutate_at(vars(i), as.numeric)

lgb <- lgb %>% mutate(rate_moe = (ci_95_high - ci_95_low)/2,
                      rate_cv = (rate_moe/1.96)/percent*100)

# clean up df
lgb<-lgb%>%select(variable,percent,population,rate_cv,ci_95_low,ci_95_high)%>%
  rename(indicator=variable,rate=percent,count=population)

# add total population for 18-24
population<-lgbt_18_24%>%filter(variable=='Total')%>%select(population)

lgb<-cbind(lgb,population)

# Calculate Transgender youth ----
# available for 0-24
# pull 18-24 trans count and population
trans_18_24<-lgbt_18_24%>%filter(variable=='Total')%>%select(variable,population_trans,population)

# convert columns to numeric and clean up
i <- c('population','population_trans')  
trans_18_24<-trans_18_24%>%mutate(across(i, ~as.numeric(.)))%>%
  rename(trans_count=population_trans)

# pull 12-17 trans count and population
trans_12_17_re<-trans_12_17%>%filter(variable=='Transgender or gender non-conforming')%>%select(variable,population)%>%rename(trans_count=population)

# add total population for 12-17
population<-trans_12_17%>%filter(variable=='Total')%>%select(population)

trans_12_17_re<-cbind(trans_12_17_re,population)

# convert columns to numeric and clean up
i <- c('population','trans_count')  
trans_12_17_re<-trans_12_17_re%>%mutate(across(i, ~as.numeric(.)))

# join together for total rate
trans<-rbind(trans_18_24,trans_12_17_re)

trans<-trans%>%mutate(indicator='Transgender or gender non-conforming')%>%
  group_by(indicator)%>%
  summarise(count=sum(trans_count),population=sum(population),rate=count/population*100)%>%
  select(indicator,rate,count,population)

# bind together indicator data frames
lgb<-lgb%>%select(indicator,rate,count,population,everything())
lgbt<-rbind.fill(trans,lgb)

