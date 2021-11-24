#-----------------------------------
# Covid & Air Pollution Project
# creating statistical models
# Funmi Arogbokun 
# May 2 2021
####last updated Nov 18, 2021   
#-------------------------------------  
 
# old code-  I M P O R T I N G   C E N S U S   D A T A ---------------------------------------

library(latticeExtra)
library(gee)
library(geepack)
library(MASS)
library(broom)
# tidyverse
library(dplyr)
library(readr)
library(tidyr)
library(magrittr)
library(stringr)
library(purrr)
# plotting libraries
library(ggplot2)
library(ggmap)
library(gridExtra)



# Load data ---------------------------------------------------------------

clean_weekly_data_dir <- "." #"/Users/funmiarogbokun/Desktop/CZ Biohub Contractor/Wildfires & Covid Project/Manuscript/Clean_final_datasets/Weekly_data"
CA_PM25_wk_PM <- read_csv(file.path(clean_weekly_data_dir, "CA_weekly_lagged_averages.csv")) %>%
  mutate(county=factor(county))

census_data_dir <- "." #"/Users/funmiarogbokun/Desktop/CZ Biohub Contractor/Wildfires & Covid Project/Census"
census_counties_2020 <- read_csv(file.path(census_data_dir, "census_CAcountydata_2020.csv"))


# Clean and transform data for California ---------------------------------

#merging the two datasets
pol_covid_census_2020 <- CA_PM25_wk_PM %>%
  left_join(census_counties_2020, by=c("county"="BASENAME")) %>%
  # creating pop density variable from census data
  mutate(pop_density_new=POP100 /AREALAND)


# explanatory and response variables
explanatory_var <- "PM25_wk_avg"
# This line checks the column names in CA_PM25_wk_PM to identify which columns
# are response variables: those with 'lag_wkg' in the colum name)
response_vars <- grep("lag_wkavg", names(CA_PM25_wk_PM), value=TRUE)
covariates <- c("month", "pop_density_new", "P0020002", "P0020003", "P0020005",
                "P0020006", "P0020007", "P0020008", "P0020009", "P0020010")


# Create datasets to fit models to ----------------------------------------

#UPDATE - data with cutoff at 30 PM
PM_CA_all <- pol_covid_census_2020 %>% 
  # Month should be a categorical variable, hence changing its type numeric --> factor.
  mutate(month=factor(month)) %>%
  filter(., complete.cases(.)) %>%
  slice(-unique(which(select(., c(explanatory_var, response_vars, covariates)) < 0, arr.ind=TRUE)[, 1]))
PM_CA_all[, response_vars] %<>% sapply(., as.integer)
cols_to_standardize <- c("pop_density_new", grep("P00", covariates, value=TRUE))
multipliers <- select(PM_CA_all, cols_to_standardize) %>% apply(2, max)
PM_CA_all[, cols_to_standardize] %<>%
  sapply(., function (x) x/max(x))

PM_CA_below30 <- filter(PM_CA_all, PM25_wk_avg <= 30)
PM_CA_above30 <- filter(PM_CA_all, PM25_wk_avg > 30)


plot(PM_CA_below30$PM25_wk_avg, PM_CA_below30$case3lag_wkavg)


# Run models --------------------------------------------------------------

# Structure of this section
# 1. `run_model` function to run a model and return its results in a user-friendly object.
# 2. `get_model_results` function to call the `run_model` function on a specific model 
#     configuration, e.g lm, PM < 30, with/without covariates.
# 3. A list defining the model configurations.
# 4. Run models and create output tables


# 1. Function to run a model and return its results in a user-friendly object.

run_model <- function (model_func_str, model_name, dataset, ycol, xcol, other_args=NULL) {
  # Creates the call to the model fitting function, fit the model to data, and 
  # return the results in a user-friendly table.
  formula_str <- paste(ycol, "~", paste(xcol, collapse="+"))
  formula_obj <- as.formula(formula_str)
  model_func <- eval(parse(text=model_func_str))
  if (model_func_str=="geeglm") {
    model <- model_func(formula=formula_obj, data=dataset, na.action=na.omit, id=county, family=gaussian, corstr="ar1")
  } else {
    model <- model_func(formula=formula_obj, data=dataset, na.action=na.omit)
  }
  results_df <- broom::tidy(model, conf.int=TRUE)[2, c("estimate", "conf.low", "conf.high", "p.value")]
  results_df$outcome <- str_extract(ycol[1], "^\\D+")
  results_df$lag <- parse_number(ycol[1]) %>% as.numeric()
  results_df$formula <- formula_str
  results_df$model <- model_name
  return (list(model=model, results=data.frame(results_df)))
}


# 2. `get_model_results` function to call the `run_model` function on a specific model 
#     configuration, e.g lm, PM < 30, with/without covariates.

get_model_results <- function (model_config, exposure="PM25_wk_avg") {
  # Calls the `run_model` function on each model
  model_names <- names(model_config)
  lapply(names(model_config), function (model_name) {
    # Loop over each type of model (lm, geeglm, nb glm)
    model_config_x <- model_config[[model_name]]
    lapply(grep("lag_wkavg", names(model_config[[model_name]]$dataset), value=TRUE), function (varname) {
      # Loop over each outcome variable (cases, hospitalizations, icu stays, deaths at each lag value)
      if ("covariates" %in% names(model_config_x)) exposure <- c(exposure, model_config_x$covariates)
      func_args <- list(model_func=model_config[[model_name]]$model_func, model_name=model_name, 
                        dataset=PM_CA_below30, ycol=varname, xcol=exposure)
      if ("other_args" %in% names(model_config_x)) func_args$other_args <- model_config_x$other_args
      output <- do.call(run_model, func_args)
      return (output)
    })
  })
}

# 3. A list defining the model configurations.

model_list <- list(
  lm_unadj_below30 = list(model_func="lm", dataset=PM_CA_below30),
  lm_unadj_above30 = list(model_func="lm", dataset=PM_CA_above30),
  geeglm_unadj_below30 = list(model_func="geeglm", dataset=PM_CA_below30),
  geeglm_unadj_above30 = list(model_func="geeglm", dataset=PM_CA_above30),
  nbglm_unadj_below30 = list(model_func="glm.nb", dataset=PM_CA_below30),
  nbglm_unadj_above30 = list(model_func="glm.nb", dataset=PM_CA_above30)
) %>%
  c(., lapply(1:length(covariates), function (i) {
    # Add covariates iteratively
    covars <- covariates[1:i]
    output <- list(
      lm_adj_below30 = list(model_func="lm", dataset=PM_CA_below30, covariates=covars),
      lm_adj_below30 = list(model_func="lm", dataset=PM_CA_above30, covariates=covars),
      geeglm_adj_below30 = list(model_func="geeglm", dataset=PM_CA_below30, covariates=covars),
      geeglm_adj_above30 = list(model_func="geeglm", dataset=PM_CA_above30, covariates=covars),
      nbglm_adj_below30 = list(model_func="glm.nb", dataset=PM_CA_below30, covariates=covars),
      nbglm_adj_above30 = list(model_func="glm.nb", dataset=PM_CA_above30, covariates=covars)
    )
    names(output) %<>% paste0(., " ", paste(covars, collapse="+"))
    output
  }) %>%
  unlist(recursive=FALSE))

# 4. Run models and create output tables
model_results_list <- get_model_results(model_list)
results_table <- lapply(unlist(model_results_list, recursive=FALSE), `[[`, "results") %>% bind_rows()
print(results_table)
write_excel_csv(results_table, "regression_results.csv")










# Previous code -----------------------------------------------------------


# --> construct separate plots for each series
PM_LA <- subset(CA_PM25_wk_PM, county =="Los Angeles")

obj1 <- xyplot(hospitalized10lag_wkavg ~ week, PM_LA, type = "l" , lwd=2, col="steelblue")
obj2 <- xyplot(PM25_wk_avg ~ week, PM_LA, type = "l", lwd=2, col="#69b3a2")

# --> Make the plot with second y axis:
doubleYScale(obj1, obj2, add.ylab2 = TRUE, use.style=FALSE )

CA_PM25HOUR_plot_max <-
  ggplot(PM_LA, aes(week, PM25_wk_avg)) +
  theme_bw() +
  geom_line(stat="identity")+ 
  ggtitle("Weekly Avg PM 2.5 in LA (AUG - DEC)")+
  labs(y="Level of PM 2.5 (Micrograms/cubic meter (LC))", x = "Date (week, Aug - Dec)")

covid_LA <- ggplot(PM_LA, aes(week, ICU14lag_wkavg)) +
  theme_bw() +
  geom_line(stat="identity")+ 
  ggtitle("Total number of people currently hospitalized in the ICU in LA")+
  labs(y="Total number of people currently hospitalized in the ICU", x = "Date (AUG - DEC)")

#RAW   
ggsave("NO2_raw_CA_Apr1820.pdf", arrangeGrob(CA_PM25HOUR_plot_max, covid_LA, nrow = 2),
       width=10, height=10)

# old code - U N A D J U S T E D   M O D E L S ---------------------------------------

linearmod_unadjust <- lm (y ~ x, data=dataset)


ggplot(PM_CA_below30, aes(x=PM25_wk_avg, y=case3lag_wkavg)) +
  theme_bw() +
  geom_point() +
  xlab("Weekly average PM 2.5") +
  ylab("Weekly average cases (3-day lag)")




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

#NOTES
    #should be able to replace gee("formula" with x= and y=) // create a for loop of the model to insert the x and y values
    #will want to put the results in a list (can send lucy code and files -- put all on github (post the csv and code on github)
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
  