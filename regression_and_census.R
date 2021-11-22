#-----------------------------------
# Covid & Air Pollution Project
# creating statistical models
# Funmi Arogbokun 
# May 2 2021
####last updated Nov 18, 2021   
#-------------------------------------  
 
# old code-  I M P O R T I N G   C E N S U S   D A T A ---------------------------------------

library(latticeExtra)
install.packages("gee")
# tidyverse
library(dplyr)

# --> construct separate plots for each series
PM_LA <- subset(CA_PM25_wk_PM, county =="Los Angeles")

obj1 <- xyplot(hospitalized10lag_wkavg ~ week, PM_LA, type = "l" , lwd=2, col="steelblue")
obj2 <- xyplot(PM25_wk_avg ~ week, PM_LA, type = "l", lwd=2, col="#69b3a2")

# --> Make the plot with second y axis:
doubleYScale(obj1, obj2, add.ylab2 = TRUE, use.style=FALSE )

CA_PM25HOUR_plot_max <-
  ggplot(PM_LA, aes(week, PM25_wk_avg)) +
  geom_line(stat="identity")+ 
  ggtitle("Weekly Avg PM 2.5 in LA (AUG - DEC)")+
  labs(y="Level of PM 2.5 (Micrograms/cubic meter (LC))", x = "Date (week, Aug - Dec)")

covid_LA <- ggplot(PM_LA, aes(week, ICU14lag_wkavg)) +
  geom_line(stat="identity")+ 
  ggtitle("Total number of people currently hospitalized in the ICU in LA")+
  labs(y="Total number of people currently hospitalized in the ICU", x = "Date (AUG - DEC)")

#RAW   
pdf("NO2_raw_CA_Apr1820.pdf") 
grid.arrange(CA_PM25HOUR_plot_max, covid_LA, nrow = 2)
dev.off()

# old code - U N A D J U S T E D   M O D E L S ---------------------------------------

linearmod_unadjust <- lm (y ~ x, data=dataset)

#reading in data and setting working directory
setwd("/Users/funmiarogbokun/Desktop/CZ Biohub Contractor/Wildfires & Covid Project/Manuscript/Clean_final_datasets/Weekly_data")
CA_PM25_wk_PM <- read.csv("CA_weekly_lagged_averages.csv", stringsAsFactors = F, header = T)  
setwd("/Users/funmiarogbokun/Desktop/CZ Biohub Contractor/Wildfires & Covid Project/Census")
census_counties_2020 <- read.csv("census_CAcountydata_2020.csv")

#cutoff at 30 PM
PM_CA_below30 <- subset(pol_covid_census_2020, PM25_wk_avg <= 30)
PM_CA_above30 <- subset(pol_covid_census_2020, PM25_wk_avg > 30)


plot(PM_CA_below30$PM25_wk_avg, PM_CA_below30$case3lag_wkavg)

#linear regression
  #below 30 ; POSITIVE LINE - cutoff
  lm_unadj_below30 <- lm (case3lag_wkavg ~ PM25_wk_avg, data=PM_CA_below30)
  print(summary(lm_unadj_below30)) #the mean increase in lagged 3 day covid for every 1 unit increase in weekly PM

  lm_unadj_below30 <- lm (case6lag_wkavg ~ PM25_wk_avg, data=PM_CA_below30)
  print(summary(lm_unadj_below30)) #the mean increase in lagged 3 day covid for every 1 unit increase in weekly PM
  
  lm_unadj_below30 <- lm (hospitalized10lag_wkavg ~ PM25_wk_avg, data=PM_CA_below30)
  print(summary(lm_unadj_below30)) #the mean increase in lagged 3 day covid for every 1 unit increase in weekly PM
  
  lm_unadj_below30 <- lm (ICU14lag_wkavg ~ PM25_wk_avg, data=PM_CA_below30)
  print(summary(lm_unadj_below30)) #the mean increase in lagged 3 day covid for every 1 unit increase in weekly PM (mod sig)
  
  lm_unadj_below30 <- lm (death21lag_wkavg ~ PM25_wk_avg, data=PM_CA_below30)
  print(summary(lm_unadj_below30)) #the mean increase in lagged 3 day covid for every 1 unit increase in weekly PM (sig)
  
  lm_unadj_below30 <- lm (death18lag_wkavg ~ PM25_wk_avg, data=PM_CA_below30)
  print(summary(lm_unadj_below30)) #the mean increase in lagged 3 day covid for every 1 unit increase in weekly PM (very sig)
  
  #above 30; NEGATIVE LINE - cutoff
  lm_unadj_above30 <- lm (case3lag_wkavg ~ PM25_wk_avg, data=PM_CA_above30)
  print(summary(lm_unadj_above30)) #the mean increase in lagged 3 day covid for every 1 unit increase in weekly PM (sig)
  
  lm_unadj_above30 <- lm (case6lag_wkavg ~ PM25_wk_avg, data=PM_CA_above30)
  print(summary(lm_unadj_above30)) #the mean increase in lagged 3 day covid for every 1 unit increase in weekly PM (sig)
  
  lm_unadj_above30 <- lm (hospitalized10lag_wkavg ~ PM25_wk_avg, data=PM_CA_above30)
  print(summary(lm_unadj_above30)) #the mean increase in lagged 3 day covid for every 1 unit increase in weekly PM (sig)
  
  lm_unadj_above30 <- lm (ICU14lag_wkavg ~ PM25_wk_avg, data=PM_CA_above30)
  print(summary(lm_unadj_above30)) #the mean increase in lagged 3 day covid for every 1 unit increase in weekly PM ( sig)
  
  lm_unadj_above30 <- lm (death21lag_wkavg ~ PM25_wk_avg, data=PM_CA_above30)
  print(summary(lm_unadj_above30)) #the mean increase in lagged 3 day covid for every 1 unit increase in weekly PM (not)
  
  lm_unadj_above30 <- lm (death18lag_wkavg ~ PM25_wk_avg, data=PM_CA_above30)
  print(summary(lm_unadj_above30)) #the mean increase in lagged 3 day covid for every 1 unit increase in weekly PM (not)
  
#MODEL ADJUSTED BY MONTH
  
  ##### NY TIMES DATA     
  #CA
  #renaming the columns of the pollution data so can merge with covid data  
  census_counties_2020_update<- census_counties_2020 %>% 
    rename(
      county = BASENAME,
    )
  #merging the two datasets
  pol_covid_census_2020 <- merge(CA_PM25_wk_PM, census_counties_2020_update, by=c("county"))
  
  #linear regression
  #below 30 ; POSITIVE LINE - cutoff
  lm_adj_below30 <- lm (case3lag_wkavg ~ PM25_wk_avg + month, data=PM_CA_below30)
  print(summary(lm_adj_below30)) #the mean increase in lagged 3 day covid for every 1 unit increase in weekly PM
  
  lm_adj_below30 <- lm (case6lag_wkavg ~ PM25_wk_avg + month, data=PM_CA_below30)
  print(summary(lm_adj_below30)) #the mean increase in lagged 3 day covid for every 1 unit increase in weekly PM
  
  
  
  
  #SHOW THIS ONE
  
  lm_adj_below30 <- lm (hospitalized10lag_wkavg ~ PM25_wk_avg , data=PM_CA_below30)
  print(summary(lm_adj_below30)) #the mean increase in lagged 3 day covid for every 1 unit increase in weekly PM
  
  lm_adj_below30 <- lm (ICU14lag_wkavg ~ PM25_wk_avg , data=PM_CA_below30)
  print(summary(lm_adj_below30)) #the mean increase in lagged 3 day covid for every 1 unit increase in weekly PM (mod sig)
  
  lm_adj_below30 <- lm (hospitalized10lag_wkavg ~ PM25_wk_avg + month + P0020002 + P0020003 + P0020004 +P0020005 + P0020006 + P0020007 + P0020008
                         + P0020009 + P0020010 + P0020011, data=PM_CA_below30)
  print(summary(lm_adj_below30)) #the mean increase in lagged 3 day covid for every 1 unit increase in weekly PM
  
  lm_adj_below30 <- lm (ICU14lag_wkavg ~ PM25_wk_avg + month + P0020002 + P0020003 + POP100, data=PM_CA_below30)
  print(summary(lm_adj_below30)) #the mean increase in lagged 3 day covid for every 1 unit increase in weekly PM (mod sig)
  
  lm_adj_below30 <- lm (death21lag_wkavg ~ PM25_wk_avg + month, data=PM_CA_below30)
  print(summary(lm_adj_below30)) #the mean increase in lagged 3 day covid for every 1 unit increase in weekly PM (sig)
  
  lm_adj_below30 <- lm (death18lag_wkavg ~ PM25_wk_avg + month, data=PM_CA_below30)
  print(summary(lm_adj_below30)) #the mean increase in lagged 3 day covid for every 1 unit increase in weekly PM (very sig)
  
  #above 30; NEGATIVE LINE - cutoff
  lm_adj_above30 <- lm (case3lag_wkavg ~ PM25_wk_avg + month, data=PM_CA_above30)
  print(summary(lm_adj_above30)) #the mean increase in lagged 3 day covid for every 1 unit increase in weekly PM (sig)
  
  lm_adj_above30 <- lm (case6lag_wkavg ~ PM25_wk_avg + month, data=PM_CA_above30)
  print(summary(lm_adj_above30)) #the mean increase in lagged 3 day covid for every 1 unit increase in weekly PM (sig)
  
  lm_adj_above30 <- lm (hospitalized10lag_wkavg ~ PM25_wk_avg + month, data=PM_CA_above30)
  print(summary(lm_adj_above30)) #the mean increase in lagged 3 day covid for every 1 unit increase in weekly PM (sig)
  
  lm_adj_above30 <- lm (ICU14lag_wkavg ~ PM25_wk_avg + month, data=PM_CA_above30)
  print(summary(lm_adj_above30)) #the mean increase in lagged 3 day covid for every 1 unit increase in weekly PM ( sig)
  
  lm_adj_above30 <- lm (death21lag_wkavg ~ PM25_wk_avg + month, data=PM_CA_above30)
  print(summary(lm_adj_above30)) #the mean increase in lagged 3 day covid for every 1 unit increase in weekly PM (not)
  
  lm_adj_above30 <- lm (death18lag_wkavg ~ PM25_wk_avg + month, data=PM_CA_above30)
  print(summary(lm_adj_above30)) #the mean increase in lagged 3 day covid for every 1 unit increase in weekly PM (not)
  
  #regression model



# Creating GEE Adjusted & Unadj.  Models + FUNCTIONS --------------------------
  library(geepack)   
  
  #reading in data and setting working directory
  setwd("/Users/funmiarogbokun/Desktop/CZ Biohub Contractor/Wildfires & Covid Project/Manuscript/Clean_final_datasets/Weekly_data")
  CA_PM25_wk_PM <- read.csv("CA_weekly_lagged_averages.csv", stringsAsFactors = F, header = T)  
  setwd("/Users/funmiarogbokun/Desktop/CZ Biohub Contractor/Wildfires & Covid Project/Census")
  census_counties_2020 <- read.csv("census_CAcountydata_2020.csv")
  
  #RENAMING DATA COLUMNS AND ADDING POP DENSITY
      #renaming the columns of the pollution data so can merge with covid data  
      census_counties_2020_update<- census_counties_2020 %>% 
        rename(
          county = BASENAME,
        )
      #merging the two datasets
      pol_covid_census_2020 <- merge(CA_PM25_wk_PM, census_counties_2020_update, by=c("county"))
      # creating pop density variable from census data
      pol_covid_census_2020$pop_density_new <- ((pol_covid_census_2020$POP100) / (pol_covid_census_2020$AREALAND))
  
  #UPDATE - data with cutoff at 30 PM
  PM_CA_below30 <- subset(pol_covid_census_2020, PM25_wk_avg <= 30)
  PM_CA_above30 <- subset(pol_covid_census_2020, PM25_wk_avg > 30)
  
  plot(PM_CA_below30$PM25_wk_avg, PM_CA_below30$case3lag_wkavg)
  
#NOTES
    #should be able to replace gee("formula" with x= and y=) // create a for loop of the model to insert the x and y values
    #will want to put the results in a list (can send lucy code and files -- put all on github (post the csv and code on github)
  install.packages("gee")
  library(geepack)
  
  #adding population density to the dataset
  #need to order gee data by time or observation within each level
        #maximum cluster size = 26 (each county has 26 weeks of data)
         #total of 42 counties in our dataset // not all counties have weeks 31-53 (some only have 4, 23, 25 obs) --> only have the counties in which we have PM2.5 measures for

#    #UNADJUSTED  GEE (no pvalue)
#    gee_test <- gee(hospitalized10lag_wkavg ~ PM25_wk_avg, 
#                    data = pol_covid_census_2020, 
#                    id = factor(county), 
#                    family = gaussian,
#                    corstr = "AR-M",
#                    na.action=na.omit)
#    #ADJUSTED MODEL GEE (no pvalue)
#    gee_test <- gee(hospitalized10lag_wkavg ~ PM25_wk_avg + month + pop_density_new + P0020002 + P0020003 + P0020005 + P0020006 + P0020007 + P0020008 + P0020009 + P0020010, 
#                    data = pol_covid_census_2020, 
#                    id = factor(county), 
#                    family = gaussian,
#                    corstr = "AR-M",
#                    na.action=na.omit)
    
    #UNADJUSTED MODEL GEEGLM (has p values)
    geeglm_unadj <- geeglm(hospitalized10lag_wkavg ~ PM25_wk_avg, 
                    data = pol_covid_census_2020, 
                    id = factor(county), 
                    family = gaussian,
                    corstr = "ar1",
                    na.action=na.omit)
    coef(summary(geeglm_unadj))
    
    #ADJUSTED MODEL GEEGLM (has p values)
    geeglm_adj <- geeglm(hospitalized10lag_wkavg ~ PM25_wk_avg + month + pop_density_new + P0020002 + P0020003 + P0020005 + P0020006 + P0020007 + P0020008 + P0020009 + P0020010, 
                    data = pol_covid_census_2020, 
                    id = factor(county), 
                    family = gaussian,
                    corstr = "ar1",
                    na.action=na.omit)
    coef(summary(geeglm_adj))
    
  #how to keep just coeffiences: coef(geeglm_test)
    
  #testing how to keep p-values and list outcome being tested  
  matrix_coef <- coef(summary(geeglm_test))
  matrix_coef_est_p <- matrix_coef [ 2, ]
  matrix_coef_est_p$outcome <-  "example"
  
# GEE model functions - unadjusted & adjusted models ----------------------
  
#unadjusted GEE model function
  geeglm_unadj_function <- function (dataframe, lagoutcome, exposure) {
    geeglm_model_unadj <- geeglm(lagoutcome ~ exposure,
                                 data = dataframe, 
                                 id = factor(county), 
                                 family = gaussian,
                                 corstr = "ar1",
                                 na.action=na.omit)
    return (coef(summary(geeglm_model_unadj))) #using coef returns just the coefficients
  }
 
#ADJUSTED GEE model function
  geeglm_adj_function <- function (dataframe, lagoutcome, exposure) {
    geeglm_model_adj <- geeglm(lagoutcome ~ exposure + month + pop_density_new + P0020002 + P0020003 + P0020005 + P0020006 + P0020007 + P0020008 + P0020009 + P0020010,
                                 data = dataframe, 
                                 id = factor(county), 
                                 family = gaussian,
                                 corstr = "ar1",
                                 na.action=na.omit)
   
    matrix_coef <- coef(summary(geeglm_model_adj))
    matrix_coef_est_p <- matrix_coef [ 2, ] #keeping only results for exposure in 2nd row
   
      return (matrix_coef_est_p) #using coef returns just the coefficients
  }
  
  #GEE models with cutpoints
  PM_CA_below30 <- subset(pol_covid_census_2020, PM25_wk_avg <= 30)
  PM_CA_above30 <- subset(pol_covid_census_2020, PM25_wk_avg > 30)
  
  
  #model below 30 & adjusted for census (- income)####
  
  #cases 3 day lag
  geeglm_unadj_function(PM_CA_below30, lagoutcome=PM_CA_below30$case3lag_wkavg, exposure=PM_CA_below30$PM25_wk_avg) 
  geeglm_adj_function(PM_CA_below30, lagoutcome=PM_CA_below30$case3lag_wkavg, exposure=PM_CA_below30$PM25_wk_avg) 
 
  #cases 6 day lag
  geeglm_unadj_function(PM_CA_below30, lagoutcome=PM_CA_below30$case6lag_wkavg, exposure=PM_CA_below30$PM25_wk_avg) 
  geeglm_adj_function(PM_CA_below30, lagoutcome=PM_CA_below30$case6lag_wkavg, exposure=PM_CA_below30$PM25_wk_avg) 
  
  #hosp lag
  geeglm_unadj_function(PM_CA_below30, lagoutcome=PM_CA_below30$hospitalized10lag_wkavg, exposure=PM_CA_below30$PM25_wk_avg) 
  geeglm_adj_function(PM_CA_below30, lagoutcome=PM_CA_below30$hospitalized10lag_wkavg, exposure=PM_CA_below30$PM25_wk_avg) 
  
  #icu
  geeglm_unadj_function(PM_CA_below30, lagoutcome=PM_CA_below30$ICU14lag_wkavg, exposure=PM_CA_below30$PM25_wk_avg) 
  geeglm_adj_function(PM_CA_below30, lagoutcome=PM_CA_below30$ICU14lag_wkavg, exposure=PM_CA_below30$PM25_wk_avg) 
  
  #death 18
  geeglm_unadj_function(PM_CA_below30, lagoutcome=PM_CA_below30$death18lag_wkavg, exposure=PM_CA_below30$PM25_wk_avg) 
  geeglm_adj_function(PM_CA_below30, lagoutcome=PM_CA_below30$death18lag_wkavg, exposure=PM_CA_below30$PM25_wk_avg) 
  
  #death 21
  geeglm_unadj_function(PM_CA_below30, lagoutcome=PM_CA_below30$death21lag_wkavg, exposure=PM_CA_below30$PM25_wk_avg) 
  geeglm_adj_function(PM_CA_below30, lagoutcome=PM_CA_below30$death21lag_wkavg, exposure=PM_CA_below30$PM25_wk_avg) 
  
  
  #above 30
  #model above 30 & adjusted for census (- income)####
  
  #cases 3 day lag
  geeglm_unadj_function(PM_CA_above30, lagoutcome=PM_CA_above30$case3lag_wkavg, exposure=PM_CA_above30$PM25_wk_avg) 
  geeglm_adj_function(PM_CA_above30, lagoutcome=PM_CA_above30$case3lag_wkavg, exposure=PM_CA_above30$PM25_wk_avg) 
  
  #cases 6 day lag
  geeglm_unadj_function(PM_CA_above30, lagoutcome=PM_CA_above30$case6lag_wkavg, exposure=PM_CA_above30$PM25_wk_avg) 
  geeglm_adj_function(PM_CA_above30, lagoutcome=PM_CA_above30$case6lag_wkavg, exposure=PM_CA_above30$PM25_wk_avg) 
  
  #hosp lag
  geeglm_unadj_function(PM_CA_above30, lagoutcome=PM_CA_above30$hospitalized10lag_wkavg, exposure=PM_CA_above30$PM25_wk_avg) 
  geeglm_adj_function(PM_CA_above30, lagoutcome=PM_CA_above30$hospitalized10lag_wkavg, exposure=PM_CA_above30$PM25_wk_avg) 
  
  #icu
  geeglm_unadj_function(PM_CA_above30, lagoutcome=PM_CA_above30$ICU14lag_wkavg, exposure=PM_CA_above30$PM25_wk_avg) 
  geeglm_adj_function(PM_CA_above30, lagoutcome=PM_CA_above30$ICU14lag_wkavg, exposure=PM_CA_above30$PM25_wk_avg) 
  
  #death 18
  geeglm_unadj_function(PM_CA_above30, lagoutcome=PM_CA_above30$death18lag_wkavg, exposure=PM_CA_above30$PM25_wk_avg) 
  geeglm_adj_function(PM_CA_above30, lagoutcome=PM_CA_above30$death18lag_wkavg, exposure=PM_CA_above30$PM25_wk_avg) 
  
  #death 21
  geeglm_unadj_function(PM_CA_above30, lagoutcome=PM_CA_above30$death21lag_wkavg, exposure=PM_CA_above30$PM25_wk_avg) 
  geeglm_adj_function(PM_CA_above30, lagoutcome=PM_CA_above30$death21lag_wkavg, exposure=PM_CA_above30$PM25_wk_avg) 
  