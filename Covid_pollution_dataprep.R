#-----------------------------------
# Covid & Air Pollution Project
# code for making smaller covid datasets from NYT data / adding lag values for covid data / making refined pollution dataset (grouping means by county)
# Funmi Arogbokun 
# May 2 2021
####last updated May 16, 2020 (exported datasets from code below for covid & pollution)
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
install.packages("devtools")
install.packages("ggpubr")


# COVID data - NT Times & CA Health Dept ---------------------------------

#NY Times USA data
  #reading in data and setting working directory
  setwd("/Users/funmiarogbokun/Desktop/CZ Biohub Contractor/Wildfires & Covid Project/Covid_data/New - 05.01.21")
  #code for cumulative that don't need: covid_cumulative_NYT <- read.csv("us-counties_cumulative.csv", stringsAsFactors = F, header = T) 
  covid_daily_NYT <- read.csv("us-counties_daily.csv", stringsAsFactors = F, header = T) # NY Times data
  #NYT CA data: covid_daily_CA <- read.csv("CA_daily_casesd_deaths.csv", stringsAsFactors = F, header = T)
  
  #converting to date
  #covid_cumulative_NYT$date <- as.Date(covid_cumulative_NYT$date, "%Y-%m-%d")
  covid_daily_NYT$date <- as.Date(covid_daily_NYT$date, "%Y-%m-%d")
  #covid_daily_CA$date <- as.Date(covid_daily_CA$date, "%Y-%m-%d")
  
  #fall_covid_filtered_CA_test[order(as.Date(fall_covid_filtered_CA_test$date, format="%m/%d/%Y")),]
  
  #filtering to CA, WA, OR
  #covid_cumulative_NYT_CAWAOR <- filter (covid_cumulative_NYT,  state == "California" |  state == "Oregon" | state == "Washington")
  covid_daily_NYT_CAWAOR <- filter (covid_daily_NYT,  state == "California" |  state == "Oregon" | state == "Washington")
  
  #exporting the smaller datasets to csv files
  write.csv(covid_cumulative_NYT_CAWAOR,'covid_cumulative_westcoast_NYT.csv')
  write.csv(covid_daily_NYT_CAWAOR,'covid_incidence_westcoast_NYT.csv')

#CA County Data
  #reading in data
  covid_hospital_CAcounty <- read.csv("CA_HealthDept_hospital_data.csv", stringsAsFactors = F, header = T) #CA county data
  #setting date to read as such in R 
  covid_hospital_CAcounty$todays_date <- as.Date(covid_hospital_CAcounty$todays_date, "%m/%d/%y") 

# COVID data - restricting to fall only & preparing data for merge with air pollution & adding lags ---------------------------------

#NY Times Data  
  #preparing data for merge with air pollution
    #NY Times Data
    fall_covid_filtered_NYT<- filter (covid_daily_NYT_CAWAOR, date >= "2020-08-01" & date <= "2020-12-31") #daily incident cases  NY Times data
    fall_covid_filtered_NYT<- filter (fall_covid_filtered_NYT, county != "Unknown") #removing unknown counties
    
  #creating the lags in the  covid NYT dataset (cases & deaths)
  #NY Times good - correct
  #CA
    fall_covid_filtered_NYT %>% group_by(state)
    fall_covid_filtered_NYT_CA <- filter (fall_covid_filtered_NYT, state == "California")
    fall_covid_filtered_NYT_CA$cases_3day_lag<-lag(fall_covid_filtered_NYT_CA$cases, n = 3L, default = NA, order_by = fall_covid_filtered_NYT_CA$county)
    fall_covid_filtered_NYT_CA$cases_6day_lag<-lag(fall_covid_filtered_NYT_CA$cases, n = 6L, default = NA, order_by = fall_covid_filtered_NYT_CA$county)
    fall_covid_filtered_NYT_CA$deaths_18day_lag<-lag(fall_covid_filtered_NYT_CA$deaths, n = 18L, default = NA, order_by = fall_covid_filtered_NYT_CA$county)
    fall_covid_filtered_NYT_CA$deaths_21day_lag<-lag(fall_covid_filtered_NYT_CA$deaths, n = 21L, default = NA, order_by = fall_covid_filtered_NYT_CA$county)
    write.csv(fall_covid_filtered_NYT_CA, file = "CA_covid_lag.csv")
  #WA
    fall_covid_filtered_NYT_WA <- filter (fall_covid_filtered_NYT, state == "Washington")
    fall_covid_filtered_NYT_WA$cases_3day_lag<-lag(fall_covid_filtered_NYT_WA$cases, n = 3L, default = NA, order_by = fall_covid_filtered_NYT_WA$county)
    fall_covid_filtered_NYT_WA$cases_6day_lag<-lag(fall_covid_filtered_NYT_WA$cases, n = 6L, default = NA, order_by = fall_covid_filtered_NYT_WA$county)
    fall_covid_filtered_NYT_WA$deaths_18day_lag<-lag(fall_covid_filtered_NYT_WA$deaths, n = 18L, default = NA, order_by = fall_covid_filtered_NYT_WA$county)
    fall_covid_filtered_NYT_WA$deaths_21day_lag<-lag(fall_covid_filtered_NYT_WA$deaths, n = 21L, default = NA, order_by = fall_covid_filtered_NYT_WA$county)
    write.csv(fall_covid_filtered_NYT_WA, file = "WA_covid_lag.csv")
  #OR
    fall_covid_filtered_NYT_OR <- filter (fall_covid_filtered_NYT, state == "Oregon")
    fall_covid_filtered_NYT_OR$cases_3day_lag<-lag(fall_covid_filtered_NYT_OR$cases, n = 3L, default = NA, order_by = fall_covid_filtered_NYT_OR$county)
    fall_covid_filtered_NYT_OR$cases_6day_lag<-lag(fall_covid_filtered_NYT_OR$cases, n = 6L, default = NA, order_by = fall_covid_filtered_NYT_OR$county)
    fall_covid_filtered_NYT_OR$deaths_18day_lag<-lag(fall_covid_filtered_NYT_OR$deaths, n = 18L, default = NA, order_by = fall_covid_filtered_NYT_OR$county)
    fall_covid_filtered_NYT_OR$deaths_21day_lag<-lag(fall_covid_filtered_NYT_OR$deaths, n = 21L, default = NA, order_by = fall_covid_filtered_NYT_OR$county)
    write.csv(fall_covid_filtered_NYT_OR, file = "OR_covid_lag.csv")
  
#CA health dept data
  #CASES - NON SEVERE
    #health dept data - not severe casees (just daily)
      #fall_covid_filtered_CA<- filter (covid_daily_CA, date >= "2020-08-01" & date <= "2020-12-31") #daily incident cases in CA (CA health dept data)
      #fall_covid_filtered_CA <- filter (fall_covid_filtered_CA, area_type == "County") #removing state aggregate cases
      #fall_covid_filtered_CA_dt <- fall_covid_filtered_CA
      #ordering dates in descending order - health dept data
      #fall_covid_filtered_CA_dt <- fall_covid_filtered_CA_dt[order(fall_covid_filtered_CA_dt$date),]
    
  #SEVERE COVID - hospital & ICU data from CA health dept for CA only
    # filtering to fall only
    covid_hospital_CAcounty_fall <- filter (covid_hospital_CAcounty, todays_date >= "2020-08-01" & todays_date <= "2020-12-31") 
    #need to sort dates from oldest to newest
    covid_hospital_CAcounty_fall <- covid_hospital_CAcounty_fall[order(covid_hospital_CAcounty_fall$todays_date),]
    # adding lags for hosp & ICU
    covid_hospital_CAcounty_fall$confirmed_hospitalized_10day_lag<-lag(covid_hospital_CAcounty_fall$hospitalized_covid_confirmed_patients, n = 10L, default = NA, order_by = covid_hospital_CAcounty_fall$county)
    covid_hospital_CAcounty_fall$confirmed_ICU_14day_lag<-lag(covid_hospital_CAcounty_fall$icu_covid_confirmed_patients, n = 14L, default = NA, order_by = covid_hospital_CAcounty_fall$county)
    # exporting data to csv & folder
    write.csv(covid_hospital_CAcounty_fall, file = "CA_hospitalized_covid_lag.csv")
    

# AIR POLLUTION data - grouping county values ---------------------------------
  #should be able to update with the new data by just rerunning the code below :)
setwd("/Users/funmiarogbokun/Desktop/CZ Biohub Contractor/Wildfires & Covid Project/EPA Data/Air Pollution - CA WA OR")
pollution <- read.csv("air_pollution_updated.csv", stringsAsFactors = F, header = T) 

pollution$Date.Local <- as.Date(pollution$Date.Local, "%m/%d/%y")
pollution$Date.of.Last.Change <- as.Date(pollution$Date.of.Last.Change, "%m/%d/%y")

pollution_group1 <- pollution %>% group_by(State.Name, County.Name, Parameter.Name, Sample.Duration, Date.Local)
pollution_group2 <- pollution_group1 %>%
  summarize(grouped_mean = mean(Arithmetic.Mean, na.rm = TRUE)) # good. think this worked properly

#filtering pollution to fall time period only
pollution_fall<- filter (pollution_group2, Date.Local >= "2020-08-01" & Date.Local <= "2020-12-31")
  #filtering pollution by each state
  pollution_fall_CA<- filter (pollution_fall, State.Name =="California")
  write.csv(pollution_fall_CA, file = "CA_grouped_pollution.csv")
  #filtering pollution by each state
  pollution_fall_WA<- filter (pollution_fall, State.Name =="Washington")
  write.csv(pollution_fall_WA, file = "WA_grouped_pollution.csv")
  #filtering pollution by each state
  pollution_fall_OR<- filter (pollution_fall, State.Name =="Oregon")
  write.csv(pollution_fall_OR, file = "OR_grouped_pollution.csv")

  
# START HERE: Creating Plots with R_squared values (reading in data created in above sections) ---------------------------------
  
#reading in pollution data & covid data for each state
  #AIR POLLUTION DATA
    setwd("/Users/funmiarogbokun/Desktop/CZ Biohub Contractor/Wildfires & Covid Project/EPA Data/Air Pollution - CA WA OR")
    CA_pollution <- read.csv("CA_grouped_pollution.csv", stringsAsFactors = F, header = T) 
    WA_pollution <- read.csv("WA_grouped_pollution.csv", stringsAsFactors = F, header = T) 
    OR_pollution <- read.csv("OR_grouped_pollution.csv", stringsAsFactors = F, header = T) 
    
  #COVID DATA
    setwd("/Users/funmiarogbokun/Desktop/CZ Biohub Contractor/Wildfires & Covid Project/Covid_data/New - 05.01.21/Clean_final_data")
    CA_covid <- read.csv("CA_covid_lag.csv", stringsAsFactors = F, header = T) 
    WA_covid <- read.csv("WA_covid_lag.csv", stringsAsFactors = F, header = T) 
    OR_covid <- read.csv("OR_covid_lag.csv", stringsAsFactors = F, header = T) 
    CA_state_covid <- read.csv("CA_hospitalized_covid_lag.csv", stringsAsFactors = F, header = T)
    
  #deleting negative values
    #abut 1% or less of each dataset for each state has negative CASE values
    CA_covid_final <- subset(CA_covid, cases >= 0,) # is same as doing covid_final_test <- filter (CA_covid, cases >= 0)
    WA_covid_final <- subset(WA_covid, cases >= 0,)
    OR_covid_final <- subset(OR_covid, cases >= 0,)
    #abut 2% or less of each dataset for each state has negative DEATH values
    CA_covid_final_use <- subset(CA_covid_final, deaths >= 0,)
    WA_covid_final_use <- subset(WA_covid_final, deaths >= 0,)
    OR_covid_final_use <- subset(OR_covid_final, deaths >= 0,)
    
    
#MERGING COVID & AIR POLLUTION DATA
    #first merge to have 1 dataset for each state (then do rowbind to have 1 dataset for all states together)

 ##### NY TIMES DATA     
    #CA
        #renaming the columns of the pollution data so can merge with covid data  
         CA_pollution_use<- CA_pollution %>% 
          rename(
            county = County.Name,
            date = Date.Local
          )
        #merging the two datasets
         CA_covid_pollution <- merge(CA_covid_final_use, CA_pollution_use, by=c("county", "date"))
         
     #WA
         #renaming the columns of the pollution data so can merge with covid data  
         WA_pollution_use<- WA_pollution %>% 
           rename(
             county = County.Name,
             date = Date.Local
           )
         #merging the two datasets
         WA_covid_pollution <- merge(WA_covid_final_use, WA_pollution_use, by=c("county", "date"))
         
      #OR
         #renaming the columns of the pollution data so can merge with covid data  
         OR_pollution_use<- OR_pollution %>% 
           rename(
             county = County.Name,
             date = Date.Local
           )
         #merging the two datasets
         OR_covid_pollution <- merge(OR_covid_final_use, OR_pollution_use, by=c("county", "date"))
         
      #ALL 3 STATES TOGETHER
         #adding datasets together
        Allstates_covid_pollution <- rbind(CA_covid_pollution, WA_covid_pollution, OR_covid_pollution)

 ##### CA STATE DATA   
        #renaming the columns of the pollution data so can merge with covid data  
        CA_STATE_pollution_use<- CA_pollution %>% 
          rename(
            county = County.Name,
            todays_date = Date.Local
          )
        #merging the two datasets
        CA_health_dept_covid_pollution <- merge(CA_state_covid, CA_STATE_pollution_use, by=c("county", "todays_date"))
        
    #saving all datasets
    setwd("/Users/funmiarogbokun/Desktop/CZ Biohub Contractor/Wildfires & Covid Project/Manuscript/Clean_final_datasets")
    write.csv(CA_covid_pollution, file = "CA_covid_pollution.csv")
    write.csv(WA_covid_pollution, file = "WA_covid_pollution.csv")
    write.csv(OR_covid_pollution, file = "OR_covid_pollution.csv")
    write.csv(Allstates_covid_pollution, file = "Allstates_covid_pollution.csv")
    write.csv(CA_health_dept_covid_pollution, file = "CA_health_dept_covid_pollution.csv")
    
  #Filter to PM 2.5  
    CA_PM25 <- filter(CA_covid_pollution, Parameter.Name == "PM2.5 - Local Conditions" & Sample.Duration =="24-HR BLK AVG")
    WA_PM25 <- filter(WA_covid_pollution, Parameter.Name == "PM2.5 - Local Conditions" & Sample.Duration =="24-HR BLK AVG")
    OR_PM25 <- filter(OR_covid_pollution, Parameter.Name == "PM2.5 - Local Conditions" & Sample.Duration =="24-HR BLK AVG")
    ALL_PM25 <- filter(Allstates_covid_pollution, Parameter.Name == "PM2.5 - Local Conditions" & Sample.Duration =="24-HR BLK AVG")
    CA_helth_dept_PM25 <- filter(CA_health_dept_covid_pollution, Parameter.Name == "PM2.5 - Local Conditions" & Sample.Duration =="24-HR BLK AVG")
  
# Creating Plots & r_squared values ---------------------------------------

  #CASES
    
#CA - NYT 
  #3 day lag
  CA_PM25_PLOT_lag3 <- ggplot(CA_PM25, aes(grouped_mean, cases_3day_lag)) +
    geom_point() +
    ggtitle("Covid Cases & 24-hour Average Level of PM 2.5 in CA (Aug - Dec 2020)")+
    labs(y="Case count in CA (3 day lag)", x = "daily average value of PM2.5 [R_sq = 0.00]")
      #r values for cases
      r <- cor(CA_PM25$grouped_mean, CA_PM25$cases_3day_lag, method = "spearman", use = "complete.obs")
      r_squared_CA_NYT = r*r
      
    #6 day lag
    CA_PM25_PLOT_lag6 <- ggplot(CA_PM25, aes(grouped_mean, cases_6day_lag)) +
        geom_point() +
        ggtitle("Covid Cases & 24-hour Average Level of PM 2.5 in CA (Aug - Dec 2020)")+
        labs(y="Case count in CA (6 day lag)", x = "daily average value of PM2.5 [R_sq = 0.00]")
      #r values for cases
      r <- cor(CA_PM25$grouped_mean, CA_PM25$cases_6day_lag, method = "spearman", use = "complete.obs")
      r_squared_CA_NYT = r*r
    
      #save as pdf  
      pdf("CA_NYT_cases.pdf") 
      grid.arrange(CA_PM25_PLOT_lag3, CA_PM25_PLOT_lag6, nrow = 2)
      dev.off() 

  #WA - NYT 
      #3 day lag
      WA_PM25_PLOT_lag3 <- ggplot(WA_PM25, aes(grouped_mean, cases_3day_lag)) +
        geom_point() +
        ggtitle("Covid Cases & 24-hour Average Level of PM 2.5 in WA (Aug - Dec 2020)")+
        labs(y="Case count in WA (3 day lag)", x = "daily average value of PM2.5 [R_sq = 0.00]")
      #r values for cases
      r <- cor(WA_PM25$grouped_mean, WA_PM25$cases_3day_lag, method = "spearman", use = "complete.obs")
      r_squared = r*r
      
      #6 day lag
      WA_PM25_PLOT_lag6 <- ggplot(WA_PM25, aes(grouped_mean, cases_6day_lag)) +
        geom_point() +
        ggtitle("Covid Cases & 24-hour Average Level of PM 2.5 in WA (Aug - Dec 2020)")+
        labs(y="Case count in WA (6 day lag)", x = "daily average value of PM2.5 [R_sq = 0.00]")
      #r values for cases
      r <- cor(WA_PM25$grouped_mean, WA_PM25$cases_6day_lag, method = "spearman", use = "complete.obs")
      r_squared = r*r
      
      #save as pdf  
      pdf("WA_NYT_cases.pdf") 
      grid.arrange(WA_PM25_PLOT_lag3, WA_PM25_PLOT_lag6, nrow = 2)
      dev.off()   
      
  #OR - NYT 
      #3 day lag
      OR_PM25_PLOT_lag3 <- ggplot(OR_PM25, aes(grouped_mean, cases_3day_lag)) +
        geom_point() +
        ggtitle("Covid Cases & 24-hour Average Level of PM 2.5 in OR (Aug - Dec 2020)")+
        labs(y="Case count in OR (3 day lag)", x = "daily average value of PM2.5 [R_sq = 0.014]")
      #r values for cases
      r <- cor(OR_PM25$grouped_mean,OR_PM25$cases_3day_lag, method = "spearman", use = "complete.obs")
      r_squared = r*r
      
      #6 day lag
      OR_PM25_PLOT_lag6 <- ggplot(OR_PM25, aes(grouped_mean, cases_6day_lag)) +
        geom_point() +
        ggtitle("Covid Cases & 24-hour Average Level of PM 2.5 in OR (Aug - Dec 2020)")+
        labs(y="Case count in OR (6 day lag)", x = "daily average value of PM2.5 [R_sq = 0.010]")
      #r values for cases
      r <- cor(OR_PM25$grouped_mean, OR_PM25$cases_6day_lag, method = "spearman", use = "complete.obs")
      r_squared = r*r
      
      #save as pdf  
      pdf("OR_NYT_cases.pdf") 
      grid.arrange(OR_PM25_PLOT_lag3, OR_PM25_PLOT_lag6, nrow = 2)
      dev.off()   
      
  #ALL - NYT 
      #3 day lag
      ALL_PM25_PLOT_lag3 <- ggplot(ALL_PM25, aes(grouped_mean, cases_3day_lag)) +
        geom_point() +
        ggtitle("Covid Cases & 24-hour Average Level of PM 2.5 in ALL STATES (Aug - Dec 2020)")+
        labs(y="Case count (3 day lag)", x = "daily average value of PM2.5 [R_sq = 0.00]")
      #r values for cases
      r <- cor(ALL_PM25$grouped_mean,ALL_PM25$cases_3day_lag, method = "spearman", use = "complete.obs")
      r_squared = r*r
      
      #6 day lag
      ALL_PM25_PLOT_lag6 <- ggplot(ALL_PM25, aes(grouped_mean, cases_6day_lag)) +
        geom_point() +
        ggtitle("Covid Cases & 24-hour Average Level of PM 2.5 in ALL STATES (Aug - Dec 2020)")+
        labs(y="Case count (6 day lag)", x = "daily average value of PM2.5 [R_sq = 0.017]")
      #r values for cases
      r <- cor(ALL_PM25$grouped_mean, ALL_PM25$cases_6day_lag, method = "spearman", use = "complete.obs")
      r_squared = r*r
      
      #save as pdf  
      pdf("ALL_NYT_cases.pdf") 
      grid.arrange(ALL_PM25_PLOT_lag3, ALL_PM25_PLOT_lag6, nrow = 2)
      dev.off()   
      
  #DEATHS

    #CA - NYT 
      #18 day lag
      CA_PM25_PLOT_lag18 <- ggplot(CA_PM25, aes(grouped_mean, deaths_18day_lag)) +
        geom_point() +
        ggtitle("Covid Deaths & 24-hour Average Level of PM 2.5 in CA (Aug - Dec 2020)")+
        labs(y="Death count in CA (18 day lag)", x = "daily average value of PM2.5 [R_sq = 0.007]")
      #r values for cases
      r <- cor(CA_PM25$grouped_mean, CA_PM25$deaths_18day_lag, method = "spearman", use = "complete.obs")
      r_squared_CA_NYT = r*r
      
      #21 day lag
      CA_PM25_PLOT_lag21 <- ggplot(CA_PM25, aes(grouped_mean, deaths_21day_lag)) +
        geom_point() +
        ggtitle("Covid Deaths & 24-hour Average Level of PM 2.5 in CA (Aug - Dec 2020)")+
        labs(y="Death count in CA (21 day lag)", x = "daily average value of PM2.5 [R_sq = 0.008]")
      #r values for cases
      r <- cor(CA_PM25$grouped_mean, CA_PM25$deaths_21day_lag, method = "spearman", use = "complete.obs")
      r_squared_CA_NYT = r*r
      
      #save as pdf  
      pdf("CA_NYT_deaths.pdf") 
      grid.arrange(CA_PM25_PLOT_lag18, CA_PM25_PLOT_lag21, nrow = 2)
      dev.off() 
      
   #WA - NYT 
      #18 day lag
      WA_PM25_PLOT_lag18 <- ggplot(WA_PM25, aes(grouped_mean, deaths_18day_lag)) +
        geom_point() +
        ggtitle("Covid Deaths & 24-hour Average Level of PM 2.5 in WA (Aug - Dec 2020)")+
        labs(y="Death count in WA (18 day lag)", x = "daily average value of PM2.5 [R_sq = 0.001]")
      #r values for cases
      r <- cor(WA_PM25$grouped_mean, WA_PM25$deaths_18day_lag, method = "spearman", use = "complete.obs")
      r_squared_CA_NYT = r*r
      
      #21 day lag
      WA_PM25_PLOT_lag21 <- ggplot(WA_PM25, aes(grouped_mean, deaths_21day_lag)) +
        geom_point() +
        ggtitle("Covid Deaths & 24-hour Average Level of PM 2.5 in WA (Aug - Dec 2020)")+
        labs(y="Death count in WA (21 day lag)", x = "daily average value of PM2.5 [R_sq = 0.004]")
      #r values for cases
      r <- cor(WA_PM25$grouped_mean, WA_PM25$deaths_21day_lag, method = "spearman", use = "complete.obs")
      r_squared_CA_NYT = r*r
      
      #save as pdf  
      pdf("WA_NYT_deaths.pdf") 
      grid.arrange(WA_PM25_PLOT_lag18, WA_PM25_PLOT_lag21, nrow = 2)
      dev.off() 
      
  #OR - NYT 
      #18 day lag
      OR_PM25_PLOT_lag18 <- ggplot(OR_PM25, aes(grouped_mean, deaths_18day_lag)) +
        geom_point() +
        ggtitle("Covid Deaths & 24-hour Average Level of PM 2.5 in OR (Aug - Dec 2020)")+
        labs(y="Death count in OR (18 day lag)", x = "daily average value of PM2.5 [R_sq = 0.023]")
      #r values for cases
      r <- cor(OR_PM25$grouped_mean, OR_PM25$deaths_18day_lag, method = "spearman", use = "complete.obs")
      r_squared_CA_NYT = r*r
      
      #21 day lag
      OR_PM25_PLOT_lag21 <- ggplot(OR_PM25, aes(grouped_mean, deaths_21day_lag)) +
        geom_point() +
        ggtitle("Covid Deaths & 24-hour Average Level of PM 2.5 in OR (Aug - Dec 2020)")+
        labs(y="Death count in OR (21 day lag)", x = "daily average value of PM2.5 [R_sq = 0.035]")
      #r values for cases
      r <- cor(OR_PM25$grouped_mean, OR_PM25$deaths_21day_lag, method = "spearman", use = "complete.obs")
      r_squared_CA_NYT = r*r
      
      #save as pdf  
      pdf("OR_NYT_deaths.pdf") 
      grid.arrange(OR_PM25_PLOT_lag18, OR_PM25_PLOT_lag21, nrow = 2)
      dev.off() 
      
   #ALL - NYT 
      #18 day lag
      ALL_PM25_PLOT_lag18 <- ggplot(ALL_PM25, aes(grouped_mean, deaths_18day_lag)) +
        geom_point() +
        ggtitle("Covid Deaths & 24-hour Average Level of PM 2.5 in ALL STATES (Aug - Dec 2020)")+
        labs(y="Death count  (18 day lag)", x = "daily average value of PM2.5 [R_sq = 0.006]")
      #r values for cases
      r <- cor(ALL_PM25$grouped_mean, ALL_PM25$deaths_18day_lag, method = "spearman", use = "complete.obs")
      r_squared_CA_NYT = r*r
      
      #21 day lag
      ALL_PM25_PLOT_lag21 <- ggplot(ALL_PM25, aes(grouped_mean, deaths_21day_lag)) +
        geom_point() +
        ggtitle("Covid Deaths & 24-hour Average Level of PM 2.5 in ALL STATES (Aug - Dec 2020)")+
        labs(y="Death count (21 day lag)", x = "daily average value of PM2.5 [R_sq = 0.008]")
      #r values for cases
      r <- cor(ALL_PM25$grouped_mean, ALL_PM25$deaths_21day_lag, method = "spearman", use = "complete.obs")
      r_squared_CA_NYT = r*r
      
      #save as pdf  
      pdf("ALL_NYT_deaths.pdf") 
      grid.arrange(ALL_PM25_PLOT_lag18, ALL_PM25_PLOT_lag21, nrow = 2)
      dev.off() 
      
  ########## STATE HEALTH DEPT
      
  #CA - NYT 
      #3 day lag
      CA_PM25_PLOT_lag10 <- ggplot(CA_health_dept_covid_pollution, aes(grouped_mean, confirmed_hospitalized_10day_lag)) +
        geom_point() +
        ggtitle("Covid Hospitalized & 24-hour Average Level of PM 2.5 in CA (Aug - Dec 2020)")+
        labs(y="Case count in CA (10 day lag)", x = "daily average value of PM2.5 [R_sq = 0.004]")
      #r values for cases
      r <- cor(CA_health_dept_covid_pollution$grouped_mean, CA_health_dept_covid_pollution$confirmed_hospitalized_10day_lag, method = "spearman", use = "complete.obs")
      r_squared_CA_NYT = r*r
      
      #6 day lag
      CA_PM25_PLOT_lag14 <- ggplot(CA_health_dept_covid_pollution, aes(grouped_mean, confirmed_ICU_14day_lag)) +
        geom_point() +
        ggtitle("Covid ICU & 24-hour Average Level of PM 2.5 in CA (Aug - Dec 2020)")+
        labs(y="Case count in CA (14 day lag)", x = "daily average value of PM2.5 [R_sq = 0.005]")
      #r values for cases
      r <- cor(CA_health_dept_covid_pollution$grouped_mean, CA_health_dept_covid_pollution$confirmed_ICU_14day_lag, method = "spearman", use = "complete.obs")
      r_squared_CA_NYT = r*r
      
      #save as pdf  
      pdf("CA_healthdept_cases.pdf") 
      grid.arrange(CA_PM25_PLOT_lag10, CA_PM25_PLOT_lag14, nrow = 2)
      dev.off() 
      
      
      
      
#plot x by y
#cases
ggplot(covid_pollution_CAonly_PM, aes(x=grouped_mean, y=cases_3day_lag)) + geom_point()
ggplot(covid_pollution_CAonly_PM, aes(x=grouped_mean, y=cases_6day_lag)) + geom_point()
  #r values for cases
  # 3 day lag
  r <- cor(covid_pollution_CAonly_PM$grouped_mean, covid_pollution_CAonly_PM$cases_3day_lag, method = "spearman", use = "complete.obs")
  cor.test(covid_pollution_CAonly_PM$grouped_mean, covid_pollution_CAonly_PM$cases_3day_lag, method=c("pearson", "kendall", "spearman"))
  #6 day lag
  r <- cor(covid_pollution_CAonly_PM$grouped_mean, covid_pollution_CAonly_PM$cases_6day_lag, method = "spearman", use = "complete.obs")
  cor.test(covid_pollution_CAonly_PM$grouped_mean, covid_pollution_CAonly_PM$cases_6day_lag, method=c("spearman"))

#deaths
ggplot(covid_pollution_CAonly_PM, aes(x=grouped_mean, y=deaths_18day_lag)) + geom_point()
ggplot(covid_pollution_CAonly_PM, aes(x=grouped_mean, y=deaths_21day_lag)) + geom_point()
  #r values for deaths
  # 18 day lag
    #graph
    CA_PM25allday_plot_max <-
      ggplot(covid_pollution_CAonly_PM, aes(x=grouped_mean, y=cases_6day_lag)) +
      geom_point() +
      ggtitle("PM 2.5 in Califormia: 24-hour Average Level of PM 2.5 in California per Day (January - October 2020)")+
      labs(y="Case count in CA", x = "daily average value of PM2.5")
    #r_square value
    r <- cor(covid_pollution_CAonly_PM$grouped_mean, covid_pollution_CAonly_PM$deaths_18day_lag, method = "spearman", use = "complete.obs")
    cor.test(covid_pollution_CAonly_PM$grouped_mean, covid_pollution_CAonly_PM$deaths_18day_lag, method=c("pearson", "kendall", "spearman"))

    
  # 21 day lag
    #graph
    CA_PM25allday_plot_max <-
      ggplot(covid_pollution_CAonly_PM, aes(x=grouped_mean, y=cases_6day_lag)) +
      geom_point() +
      ggtitle("PM 2.5 in Califormia: 24-hour Average Level of PM 2.5 in California per Day (January - October 2020)")+
      labs(y="Case count in CA", x = "daily average value of PM2.5")
    #r_square value
    r <- cor(covid_pollution_CAonly_PM$grouped_mean, covid_pollution_CAonly_PM$deaths_21day_lag, method = "spearman", use = "complete.obs")
    cor.test(covid_pollution_CAonly_PM$grouped_mean, covid_pollution_CAonly_PM$deaths_21day_lag, method=c("pearson", "kendall", "spearman"))
 

CA_PM25allday_plot_max <-
  ggplot(covid_pollution_CAonly_PM, aes(x=grouped_mean, y=cases_6day_lag)) +
  geom_point() +
  ggtitle("PM 2.5 in Califormia: 24-hour Average Level of PM 2.5 in California per Day (January - October 2020)")+
  labs(y="Case count in CA", x = "daily average value of PM2.5")

#### creating boxplots ####

boxplot(cases_6day_lag~grouped_mean,data=CA_PM25, main="Boxplots: Covid Cases (6-day lag) by Daily Avg PM2.5 in CA [Aug-Dec]",
        xlab="daily average value of PM2.5", ylab="daily case count in CA")

### trying grouping daily data pollution into months #####
CA_PM25_test <- CA_PM25
#creating a month variable
if(CA_PM25_test$date <= "2020-08-31"){
  ( CA_PM25_test$month = "August")
}
else if ( CA_PM25_test$date >= "2020-09-01" & CA_PM25_test$date <= "2020-09-31") {
  statement2
} else if ( test_expression3) {
  statement3
} else {
  statement4
}
if(CA_PM25$date >= "2020-08-01" & date <= "2020-12-31"){
  print("Positive number")
}
CA_PM25$month <- filter(CA_covid_pollution, Parameter.Name == "PM2.5 - Local Conditions" & Sample.Duration =="24-HR BLK AVG")

