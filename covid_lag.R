#-----------------------------------
# Covid & Air Pollution Project
#code for creating lagged data and mergging data
# Funmi Arogbokun 
# April 19, 2021
#-------------------------------------  

# tidyverse
library(dplyr)
library(readr)
library(tidyr)
library(magrittr)
# plotting libraries
library(ggplot2)
library(ggmap)
library(gridExtra)

# COVID data- adding lag values to the dataset ---------------------------------

#reading in data and setting working directory
setwd("/Users/funmiarogbokun/Desktop/CZ Biohub Contractor/Wildfires & Covid Project/Covid_data")
covid_data_lag <- read.csv("county_data.csv", stringsAsFactors = F, header = T) 

#want to make sure R is reading the date in as a data, not char
covid_data_lag$date <- as.Date(covid_data_lag$date, "%Y-%m-%d")
covid_lag_fall <- filter (covid_data_lag, date >= "2020-08-01" & date <= "2020-12-31")

#checked above numbers with the table below - filtered correctly
summary(covid_lag_fall$date)

#keeping just relevent columns
short_covid_lag_fall <- covid_lag_fall[c("state", "county", "date", "cases", "deaths")]
test_lag <- short_covid_lag_fall

#adding the lags to the dataset - it works 
test_lag$lag_cases<-lag(test_lag$cases, n = 3L, default = NA, order_by = test_lag$county)

#3 - 14 day lags
test_lag$cases_3day_lag<-lag(test_lag$cases, n = 3L, default = NA, order_by = test_lag$county)
test_lag$cases_6day_lag<-lag(test_lag$cases, n = 6L, default = NA, order_by = test_lag$county)
test_lag$cases_10day_lag<-lag(test_lag$cases, n = 10L, default = NA, order_by = test_lag$county)
test_lag$cases_14day_lag<-lag(test_lag$cases, n = 14L, default = NA, order_by = test_lag$county)


# Grouping Air Pollution data by County & date ----------------------------

# reading in data and setting working directory
setwd("/Users/funmiarogbokun/Desktop/CZ Biohub Contractor/Wildfires & Covid Project/EPA Data/Air Pollution - CA WA OR")
pollution <- read.csv("air_pollution_updated.csv", stringsAsFactors = F, header = T) 

pollution_group1 <- pollution %>% group_by(State.Name, County.Name, Parameter.Name, Sample.Duration, Date.Local)
pollution_group2 <- pollution_group1 %>%
  summarize(grouped_mean = mean(Arithmetic.Mean, na.rm = TRUE)) # good. think this worked properly

# merging data with covid dataset
#reading in data and setting working directory
setwd("/Users/funmiarogbokun/Desktop/CZ Biohub Contractor/Wildfires & Covid Project/Covid_data")
covid_allstates <- read.csv("county_data.csv", stringsAsFactors = F, header = T) 

#want to make sure R is reading the date in as a data, not char
covid_allstates$date <- as.Date(covid_allstates$date, "%Y-%m-%d")
covid_allstates <- filter (covid_allstates, date >= "2020-08-01" & date <= "2020-12-31")

#keeping only the relevant columns
covid_merge <- covid_allstates[c("state","county","date","cases","deaths")]
test4_group_merge <- test4_group

testing<- test4_group_merge %>% 
  rename(
    state = State.Name,
    county = County.Name,
    date = Date.Local
  )

covid_pollution <- merge(covid_merge, testing, by=c("state","county", "date"))


#How to check to see if merging correctly
test2_group <- test_group_pollution %>% group_by(State.Name, County.Name, Parameter.Name, Sample.Duration, Date.Local)
test3_group <- test2_group


# Section below shows testing of grouping code and merging of data --------


# testing to see if works correctly (air pollution & covid merge)  *worked correctly - 1 county and date: Fresno, 8.4.20, all pollutants--------

  #creating smaller pollution dataset to work with
  test_pollution <- test_group_pollution[c("State.Name","County.Name","Parameter.Name", "Pollutant.Standard", "Sample.Duration","Date.Local", "Arithmetic.Mean")]
  

  #filtering down to one date and one county to be able to check - checked and works properly!
  test_pollution <-  filter (test_pollution, Date.Local == "2020-08-04" & County.Name == "Fresno")

  test_pollution_group <- test_pollution %>% group_by(State.Name, County.Name, Parameter.Name, Sample.Duration, Date.Local)
  
  test_pollution_group_sum <- test_pollution_group %>%
  summarize(grouped_mean = mean(Arithmetic.Mean, na.rm = TRUE)) # good. think this worked properly

  #trying to connect covid data and check
  test_covid <- filter (covid_allstates, date == "2020-08-04" & state == "California" & county == "Fresno")

  #keeping only the relevant columns
  test_covid_merge <- test_covid[c("state","county","date","cases","deaths")]
  
  #renaming pollution columns for easier merge with covid data
  pollution_group_sum_renamed <- test_pollution_group_sum #making a copy before renaming
  #renmaing the variables
  pollution_group_sum_renamed <- 
    rename(test_pollution_group_sum,
      state = State.Name,
      county = County.Name,
      date = Date.Local
    )

covid_pollution_fresnotest <- merge(test_covid_merge, pollution_group_sum_renamed, by=c("state","county", "date"))

#check adding another date (which has infor for another state)
#check adding another county in CA on same date

# testing to see if works correctly (air pollution & covid merge)  *worked correctly - 2 counties in CA and date: Fresno & Inyo, 8.4.20, all pollutants--------

#creating smaller pollution dataset to work with
test_pollution_2 <- test_group_pollution[c("State.Name","County.Name","Parameter.Name", "Pollutant.Standard", "Sample.Duration","Date.Local", "Arithmetic.Mean")]

#filtering down to one date and one county to be able to check - checked and works properly!
test_pollution_2 <-  filter (test_pollution_2, County.Name == "Fresno" | County.Name ==  "Inyo")
test_pollution_2 <- filter (test_pollution_2, Date.Local == "2020-08-04")

test_pollution_group_2 <- test_pollution_2 %>% group_by(State.Name, County.Name, Parameter.Name, Sample.Duration, Date.Local)

test_pollution_group_sum_2 <- test_pollution_group_2 %>%
  summarize(grouped_mean = mean(Arithmetic.Mean, na.rm = TRUE)) # good. think this worked properly

#trying to connect covid data and check
test_covid_2 <- filter (covid_allstates, (date == "2020-08-04" & state == "California" & county == "Fresno") | (date == "2020-08-04" & state == "California" & county == "Inyo"))

#keeping only the relevant columns
test_covid_merge_2 <- test_covid_2[c("state","county","date","cases","deaths")]

#renaming pollution columns for easier merge with covid data
pollution_group_sum_renamed_2 <- test_pollution_group_sum_2 #making a copy before renaming
#renmaing the variables
pollution_group_sum_renamed_2 <- 
  rename(test_pollution_group_sum_2,
         state = State.Name,
         county = County.Name,
         date = Date.Local
  )

covid_pollution_twoca <- merge(test_covid_merge_2, pollution_group_sum_renamed_2, by=c("state","county", "date"))

# testing to see if works correctly (air pollution & covid merge)  *worked correctly - 3 counties (in CA in each state) and 1 date: Spokane & Kern & Multnomah, 6.2.20, all pollutants--------

#creating smaller pollution dataset to work with
test_pollution_3 <- test_group_pollution[c("State.Name","County.Name","Parameter.Name", "Pollutant.Standard", "Sample.Duration","Date.Local", "Arithmetic.Mean")]

#filtering down to one date and one county to be able to check - checked and works properly!
test_pollution_3 <-  filter (test_pollution_3, County.Name == "Multnomah" | County.Name ==  "Spokane" | County.Name ==  "Kern" )
test_pollution_3 <- filter (test_pollution_3, Date.Local == "2020-06-02")

test_pollution_group_3 <- test_pollution_3 %>% group_by(State.Name, County.Name, Parameter.Name, Sample.Duration, Date.Local)

test_pollution_group_sum_3 <- test_pollution_group_3 %>%
  summarize(grouped_mean = mean(Arithmetic.Mean, na.rm = TRUE)) # good. think this worked properly

#trying to connect covid data and check
test_covid_3 <- filter (covid_allstates, (date == "2020-06-02" & state == "California" & county == "Kern") | (date == "2020-06-02" & state == "Washington" & county == "Spokane") | (date == "2020-06-02" & state == "Oregon" & county == "Multnomah"))

#keeping only the relevant columns
test_covid_merge_3 <- test_covid_3[c("state","county","date","cases","deaths")]

#renaming pollution columns for easier merge with covid data
pollution_group_sum_renamed_3 <- test_pollution_group_sum_3 #making a copy before renaming
#renmaing the variables
pollution_group_sum_renamed_3 <- 
  rename(test_pollution_group_sum_3,
         state = State.Name,
         county = County.Name,
         date = Date.Local
  )

covid_pollution_allthree <- merge(test_covid_merge_3, pollution_group_sum_renamed_3, by=c("state","county", "date"))
