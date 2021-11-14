#title: "HarvardX PH125.9x - My Own Project: Bike sharing prediction"
#author: "Stefania_Ravazzini"
#date: "2021/11/14"

# Install and load useful packages

if(!require(utils)) install.packages("utils", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")

library(utils)
library(tidyverse)
library(lubridate)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)

# Download dataset in R
## The dataset is about Bike-sharing.
## It contains 2011 and 2012 bike sharing counts aggregated on hourly basis from Capital Bikeshare system, 
## Washington D.C., USA. Historical logs can be analyzed to predict bike rental count hourly or daily
## based on the environmental and seasonal settings.

url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00275/Bike-Sharing-Dataset.zip"
path <- file.path("~", "Bike-Sharing-Dataset.zip")
download.file(url, path)
dataset <- read.table(unzip(path, "hour.csv"), 
                      header = TRUE, 
                      sep = ",",
                      stringsAsFactors = FALSE)

# Explore dataset

head(dataset)
str(dataset)

# Data cleaning
## Check if some missing values need to be fixed

sum(is.na(dataset)) # --> no missing value

## Information on features can be found in source zip file.
## Some adjustments need to be made.

dataset <- dataset %>% 
           mutate(dteday = as.Date(dteday),
                  season_fct = as.factor(case_when(season == 1 ~ "Winter",
                                     season == 2 ~ "Spring",
                                     season == 3 ~ "Summer",
                                     season == 4 ~ "Fall"
                                        )),
                  yr_fct = as.factor(ifelse(yr == 0, "2011", "2012")),
                  mnth_fct = as.factor(case_when(mnth == 1  ~ "Jan",
                                                 mnth == 2  ~ "Feb",
                                                 mnth == 3  ~ "Mar",
                                                 mnth == 4  ~ "Apr",
                                                 mnth == 5  ~ "May",
                                                 mnth == 6  ~ "Jun",
                                                 mnth == 7  ~ "Jul",
                                                 mnth == 8  ~ "Aug",
                                                 mnth == 9  ~ "Sep",
                                                 mnth == 10 ~ "Oct",
                                                 mnth == 11 ~ "Nov",
                                                 mnth == 12 ~ "Dec"
                                      )),
                  holiday_log = as.logical(holiday),
                  weekday_fct = as.factor(case_when(weekday == 0  ~ "Sun",
                                                    weekday == 1  ~ "Mon",
                                                    weekday == 2  ~ "Tue",
                                                    weekday == 3  ~ "Wed",
                                                    weekday == 4  ~ "Thu",
                                                    weekday == 5  ~ "Fri",
                                                    weekday == 6  ~ "Sat"
                                         )),
                  workingday_log = as.logical(workingday),
                  weathersit_fct = as.factor(case_when(weathersit == 1  ~ "Clear",
                                                       weathersit == 2  ~ "Mist",
                                                       weathersit == 3  ~ "Light Rain",
                                                       weathersit == 4  ~ "Heavy Rain/Snow"
                                            ))
                  ) 

dataset <- dataset %>%
           mutate(season_fct = factor(season_fct, levels = c("Winter", "Spring", "Summer", "Fall")),
                  mnth_fct = factor(mnth_fct, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                                         "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
                  weekday_fct = factor(weekday_fct, levels = c("Sun", "Mon", "Tue", "Wed", "Thu", 
                                                               "Fri", "Sat")),
                  weathersit_fct = factor(weathersit_fct, levels = c("Clear",
                                                                     "Mist",
                                                                     "Light Rain",
                                                                     "Heavy Rain/Snow")))

## Check data consistency between the following variables:
## dteday and yr / mnth / weekday / season
## cnt and casual / registered

nrow <- nrow(dataset)

dataset_test <- dataset %>%
  summarize(yr_check = nrow - sum(ifelse(yr_fct == "2011", 2011, 2012) == year(dteday)),
            mnth_check = nrow - sum(mnth == month(dteday)),
            weekday_check = nrow - sum(weekday == wday(dteday)-1),
            season_check = sum(season - case_when(dteday <= ymd("2011-03-20") ~ 1,
                                                  dteday > ymd("2011-12-20") & 
                                                    dteday <= ymd("2012-03-20") ~ 1,
                                                  dteday > ymd("2012-12-20") ~ 1, # --> Winter
                                                  dteday > ymd("2011-03-20") &
                                                    dteday <= ymd("2011-06-20") ~ 2,
                                                  dteday > ymd("2012-03-20") &
                                                    dteday <= ymd("2012-06-20") ~ 2, # --> Spring
                                                  dteday > ymd("2011-06-20") &
                                                    dteday <= ymd("2011-09-22") ~ 3,
                                                  dteday > ymd("2012-06-20") &
                                                    dteday <= ymd("2012-09-22") ~ 3, # --> Summer                                                            
                                                  TRUE ~ 4 # --> Fall
            )),
            cnt_check = sum(cnt - (casual + registered))
  ) 

dataset_test  # --> check always zero = data consistency is ok

# Data exploration
## Check range of dates / numeric variables and explore factors by count

dataset %>%
  summarize(min_dteday = min(dteday),
            max_dteday = max(dteday)) # --> dates range from 2011-01-01 to 2012-12-31

table(dataset$yr_fct, dataset$season_fct) # --> observations are equally distributed between seasons, 
                                          #     by each year

table(dataset$yr_fct, dataset$mnth_fct) # --> observations are quite equally distributed between 
                                        #     months, by each year

table(dataset$yr_fct, dataset$weekday_fct) # --> observations are quite equally distributed between 
                                           #     weekdays, by each year

table(dataset$yr_fct, dataset$holiday_log) # --> less than 3% of observations are related to holidays  

table(dataset$workingday_log, dataset$yr_fct) # --> more than 2/3 of observations are related to working days

table(dataset$yr_fct, dataset$hr) # --> observations are quite equally distributed between 
                                  #     hours, by each year

table(dataset$yr_fct, dataset$weathersit_fct) # --> 2011 and 2012 have quite the same number of observations
                                              #     split more or less by the same weather condition

dataset %>%
  summarize(min_temp = min(temp),
            max_temp = max(temp),
            mean_temp = mean(temp),
            sd_temp = sd(temp))    # --> temperatures range between 0.02 and 1, proof that they are normalized,
                                   #     as declared in source zip file

dataset %>%
  summarize(min_atemp = min(atemp),
            max_atemp = max(atemp),
            mean_atemp = mean(atemp),
            sd_atemp = sd(atemp))   # --> feeling temperatures range between 0 and 1, proof that they 
                                    #     are normalized, as declared in source zip file

dataset %>%
  summarize(min_hum = min(hum),
            max_hum = max(hum),
            mean_hum = mean(hum),
            sd_hum = sd(hum))    # --> humidity range between 0 and 1, proof that it is normalized, 
                                 #     as declared in source zip file

dataset %>%
  summarize(min_windspeed = min(windspeed),
            max_windspeed = max(windspeed),
            mean_windspeed = mean(windspeed),
            sd_windspeed = sd(windspeed))  # --> wind speed range between 0 and 0.8507, proof that it is 
                                             #     normalized, as declared in source zip file

dataset %>%
  summarize(min_casual = min(casual),
            max_casual = max(casual),
            mean_casual = mean(casual),
            sd_casual = sd(casual))       # --> hourly bike rental observations of casual users range between
                                          # 0 and a peak of 367, mean value is 36 and variability is high

dataset %>%
  summarize(min_registered = min(registered),
            max_registered = max(registered),
            mean_registered = mean(registered),
            sd_registered = sd(registered)) # --> hourly bike rental observations of registered users range 
                                            # between 0 and a peak of 886, mean value is 154 and 
                                            # variability is high

dataset %>%
  summarize(min_cnt = min(cnt),
            max_cnt = max(cnt),
            mean_cnt = mean(cnt),
            sd_cnt = sd(cnt)) # --> hourly bike rental observations of users range between 1 and a peak of 977,
                              # mean value is 189 and variability is high

# Data visualization
## Visualize bike rentals by seasonal and environmental settings

ggplot(data = dataset, aes(x = dteday, y = cnt, color = season_fct)) + 
  geom_point() +
  ylim(c(0, 1000)) +
  labs(x = "Date", 
       y = "Count of bike rentals") +
  ggtitle("Count of bike rentals by date / season")

ggplot(data = dataset, aes(x = dteday, y = registered, color = season_fct)) + 
  geom_point() +
  ylim(c(0, 1000)) +
  labs(x = "Date", 
       y = "Count of registered-users bike rentals") +
  ggtitle("Count of registered-users bike rentals by date / season")

ggplot(data = dataset, aes(x = dteday, y = casual, color = season_fct)) + 
  geom_point() +
  ylim(c(0, 1000)) +
  labs(x = "Date", 
       y = "Count of casual-users bike rentals") +
  ggtitle("Count of casual-users bike rentals by date / season")

ggplot(data = dataset, aes(x = dteday, y = cnt, color = mnth_fct)) + 
  geom_point() +
  ylim(c(0, 1000)) +
  labs(x = "Date", 
       y = "Count of bike rentals") +
  ggtitle("Count of bike rentals by date / month")

ggplot(data = dataset, aes(x = dteday, y = registered, color = mnth_fct)) + 
  geom_point() +
  ylim(c(0, 1000)) +
  labs(x = "Date", 
       y = "Count of registered-users bike rentals") +
  ggtitle("Count of registered-users bike rentals by date / month")

ggplot(data = dataset, aes(x = dteday, y = casual, color = mnth_fct)) + 
  geom_point() +
  ylim(c(0, 1000)) +
  labs(x = "Date", 
       y = "Count of casual-users bike rentals") +
  ggtitle("Count of casual-users bike rentals by date / month")

ggplot(data = dataset, aes(x = hr, y = cnt)) + 
  geom_point() +
  ylim(c(0, 1000)) +
  facet_grid(rows = vars(season_fct)) +
  labs(x = "Hour", 
       y = "Count of bike rentals") +
  ggtitle("Count of bike rentals by hour / season")

ggplot(data = dataset, aes(x = hr, y = registered)) + 
  geom_point() +
  ylim(c(0, 1000)) +
  facet_grid(rows = vars(season_fct)) +
  labs(x = "Hour", 
       y = "Count of registered-users bike rentals") +
  ggtitle("Count of registered-users bike rentals by hour / season")

ggplot(data = dataset, aes(x = hr, y = casual)) + 
  geom_point() +
  ylim(c(0, 1000)) +
  facet_grid(rows = vars(season_fct)) +
  labs(x = "Hour", 
       y = "Count of casual-users bike rentals") +
  ggtitle("Count of casual-users bike rentals by hour / season")

ggplot(data = dataset, aes(x = dteday, y = cnt, color = holiday_log)) + 
  geom_point() +
  ylim(c(0, 1000)) +
  labs(x = "Date", 
       y = "Count of bike rentals") +
  ggtitle("Count of bike rentals by date / holiday")

ggplot(data = dataset, aes(x = dteday, y = registered, color = holiday_log)) + 
  geom_point() +
  ylim(c(0, 1000)) +
  labs(x = "Date", 
       y = "Count of registered-users bike rentals") +
  ggtitle("Count of registered-users bike rentals by date / holiday")

ggplot(data = dataset, aes(x = dteday, y = casual, color = holiday_log)) + 
  geom_point() +
  ylim(c(0, 1000)) +
  labs(x = "Date", 
       y = "Count of casual-users bike rentals") +
  ggtitle("Count of casual-users bike rentals by date / holiday")

ggplot(data = dataset, aes(x = dteday, y = cnt, color = weekday_fct)) + 
  geom_point() +
  ylim(c(0, 1000)) +
  labs(x = "Date", 
       y = "Count of bike rentals") +
  ggtitle("Count of bike rentals by date / weekday")

ggplot(data = dataset, aes(x = dteday, y = registered, color = weekday_fct)) + 
  geom_point() +
  ylim(c(0, 1000)) +
  labs(x = "Date", 
       y = "Count of registered-users bike rentals") +
  ggtitle("Count of registered-users bike rentals by date / weekday")

ggplot(data = dataset, aes(x = dteday, y = casual, color = weekday_fct)) + 
  geom_point() +
  ylim(c(0, 1000)) +
  labs(x = "Date", 
       y = "Count of casual-users bike rentals") +
  ggtitle("Count of casual-users bike rentals by date / weekday")

ggplot(data = dataset, aes(x = dteday, y = cnt, color = workingday_log)) + 
  geom_point() +
  ylim(c(0, 1000)) +
  labs(x = "Date", 
       y = "Count of bike rentals") +
  ggtitle("Count of bike rentals by date / workingday")

ggplot(data = dataset, aes(x = dteday, y = registered, color = workingday_log)) + 
  geom_point() +
  ylim(c(0, 1000)) +
  labs(x = "Date", 
       y = "Count of registered-users bike rentals") +
  ggtitle("Count of registered-users bike rentals by date / workingday")

ggplot(data = dataset, aes(x = dteday, y = casual, color = workingday_log)) + 
  geom_point() +
  ylim(c(0, 1000)) +
  labs(x = "Date", 
       y = "Count of casual-users bike rentals") +
  ggtitle("Count of casual-users bike rentals by date / workingday")

ggplot(data = dataset, aes(x = dteday, y = cnt, color = weathersit_fct)) + 
  geom_point() +
  ylim(c(0, 1000)) +
  labs(x = "Date", 
       y = "Count of bike rentals") +
  ggtitle("Count of bike rentals by date / weather situation")

ggplot(data = dataset, aes(x = dteday, y = registered, color = weathersit_fct)) + 
  geom_point() +
  ylim(c(0, 1000)) +
  labs(x = "Date", 
       y = "Count of registered-users bike rentals") +
  ggtitle("Count of registered-users bike rentals by date / weather situation")

ggplot(data = dataset, aes(x = dteday, y = casual, color = weathersit_fct)) + 
  geom_point() +
  ylim(c(0, 1000)) +
  labs(x = "Date", 
       y = "Count of casual-users bike rentals") +
  ggtitle("Count of casual-users bike rentals by date / weather situation")

ggplot(data = dataset, aes(x = temp, y = cnt, color = season_fct)) + 
  geom_point() +
  ylim(c(0, 1000)) +
  labs(x = "Temperature", 
       y = "Count of bike rentals") +
  ggtitle("Count of bike rentals by temperature / season")

ggplot(data = dataset, aes(x = temp, y = registered, color = season_fct)) + 
  geom_point() +
  ylim(c(0, 1000)) +
  labs(x = "Temperature", 
       y = "Count of registered-users bike rentals") +
  ggtitle("Count of registered-users bike rentals by temperature / season")

ggplot(data = dataset, aes(x = temp, y = casual, color = season_fct)) + 
  geom_point() +
  ylim(c(0, 1000)) +
  labs(x = "Temperature", 
       y = "Count of casual-users bike rentals") +
  ggtitle("Count of casual-users bike rentals by temperature / season")

ggplot(data = dataset, aes(x = atemp, y = cnt, color = season_fct)) + 
  geom_point() +
  ylim(c(0, 1000)) +
  labs(x = "Feeling Temperature", 
       y = "Count of bike rentals") +
  ggtitle("Count of bike rentals by feeling temperature / season")

ggplot(data = dataset, aes(x = atemp, y = registered, color = season_fct)) + 
  geom_point() +
  ylim(c(0, 1000)) +
  labs(x = "Feeling Temperature", 
       y = "Count of registered-users bike rentals") +
  ggtitle("Count of registered-users bike rentals by feeling temperature / season")

ggplot(data = dataset, aes(x = atemp, y = casual, color = season_fct)) + 
  geom_point() +
  ylim(c(0, 1000)) +
  labs(x = "Feeling Temperature", 
       y = "Count of casual-users bike rentals") +
  ggtitle("Count of casual-users bike rentals by feeling temperature / season")

ggplot(data = dataset, aes(x = hum, y = cnt, color = season_fct)) + 
  geom_point() +
  ylim(c(0, 1000)) +
  labs(x = "Humidity", 
       y = "Count of bike rentals") +
  ggtitle("Count of bike rentals by humidity / season")

ggplot(data = dataset, aes(x = hum, y = registered, color = season_fct)) + 
  geom_point() +
  ylim(c(0, 1000)) +
  labs(x = "Humidity", 
       y = "Count of registered-users bike rentals") +
  ggtitle("Count of registered-users bike rentals by humidity / season")

ggplot(data = dataset, aes(x = hum, y = casual, color = season_fct)) + 
  geom_point() +
  ylim(c(0, 1000)) +
  labs(x = "Humidity", 
       y = "Count of casual-users bike rentals") +
  ggtitle("Count of casual-users bike rentals by humidity / season")

ggplot(data = dataset, aes(x = windspeed, y = cnt, color = season_fct)) + 
  geom_point() +
  ylim(c(0, 1000)) +
  labs(x = "Windspeed", 
       y = "Count of bike rentals") +
  ggtitle("Count of bike rentals by windspeed / season")

ggplot(data = dataset, aes(x = windspeed, y = registered, color = season_fct)) + 
  geom_point() +
  ylim(c(0, 1000)) +
  labs(x = "Windspeed", 
       y = "Count of registered-users bike rentals") +
  ggtitle("Count of registered-users bike rentals by windspeed / season")

ggplot(data = dataset, aes(x = windspeed, y = casual, color = season_fct)) + 
  geom_point() +
  ylim(c(0, 1000)) +
  labs(x = "Windspeed", 
       y = "Count of casual-users bike rentals") +
  ggtitle("Count of casual-users bike rentals by windspeed / season")

# Select only relevant columns from dataset for machine learning purpose
## instant column: this is the id of the observations, it will not be used for ML purpose
## dteday column: this will be dropped because it contains quite the same information as yr and mnth features
## season column: this will be dropped because it contains the same information as mnth feature
## atemp column: this will be dropped because it is a function of temp feature
## casual and registered column: these are dependent variables, but in this project we develop a model only 
##for cnt dependent variable, so these will be dropped
## all variables like *_fct, *_log: these are duplicates of existing variables, so these will be dropped

dataset_selection <- dataset %>%
                     select(yr, mnth, hr, holiday, weekday, workingday, weathersit, temp,
                            hum, windspeed, cnt)

# Select the models for prediction
## The task is to predict cnt = counts of bike rentals. Therefore, poisson model could fit the task. If the mean
## of cnt is much different from the variance of cnt, then quasipoisson model should be used.

dataset_selection %>%
  summarize(mean_cnt = mean(cnt),
            var_cnt = var(cnt)) # --> var >> mean. Quasipoisson model should be used

## A second model could be a decision tree model.
## A final model could be a random forest model.

# Split data into train set and test set
## Given that random forest is keen to overfitting, a split of 70% train set - 30% test set could be helpful

set.seed(1, sample.kind = "Rounding")
index<-createDataPartition(dataset$instant,times=1,p=0.7,list=FALSE)
dataset_selection_train <- dataset_selection[index,]
dataset_selection_test <- dataset_selection[-index,]

## Check that number of rows is consistent with the split

row_check_train <- nrow(dataset_selection_train) - 0.7 * nrow  
row_check_train # --> difference is only 1.7, test ok

row_check_test <- nrow(dataset_selection_test) - 0.3 * nrow  
row_check_test # --> difference is only - 1.7, test ok

# Fit Quasipoisson model

qp_model <- glm(cnt ~ yr + mnth + hr + holiday + weekday + workingday + weathersit + 
                      temp + hum + windspeed, 
                data = dataset_selection_train,
                family = quasipoisson)

qp_model

y_hat_qp_model <- predict(qp_model, 
                          newdata = dataset_selection_test, 
                          type = "response")

# Fit decision tree model

dt_model <- rpart(cnt ~ yr + mnth + hr + holiday + weekday + workingday + weathersit + 
                  temp + hum + windspeed, 
                data = dataset_selection_train)

dt_model

prp(dt_model, type = 2, branch=1, varlen=0, yesno=2)

y_hat_dt_model <- predict(dt_model, 
                          newdata = dataset_selection_test)

# Fit random forest model

rf_model <- randomForest(cnt ~ yr + mnth + hr + holiday + weekday + workingday + weathersit + 
                               temp + hum + windspeed, 
                         data = dataset_selection_train)


plot(rf_model) # --> 100 trees are sufficient for explaining most of variability

y_hat_rf_model <- predict(rf_model, 
                          newdata = dataset_selection_test)

## RMSES and model performance by graph
#RMSES

rmses <- dataset_selection_test %>%
  mutate(residual_qp = y_hat_qp_model - cnt,
         residual_dt = y_hat_dt_model - cnt,
         residual_rf = y_hat_rf_model - cnt) %>%
  summarize(rmse_qp_model = sqrt(mean(residual_qp^2)),
            rmse_dt_model = sqrt(mean(residual_dt^2)),
            rmse_rf_model = sqrt(mean(residual_rf^2)),
            sd_cnt = sd(cnt)) 

rmses # --> Quasipoisson model has an rmse lower than sd of count variable, but it doesn't seem to be too 
      #     explanatory.
      #     Decision tree performs better than quasipoisson.
      #     Random forest model performs way better than the quasipoisson and the decision tree models: its rmse 
      #     is the lowest 

dataset_selection_test <- dataset_selection_test %>%
                          mutate(y_hat_qp_model = y_hat_qp_model, 
                                 y_hat_dt_model = y_hat_dt_model,
                                 y_hat_rf_model = y_hat_rf_model)

ggplot(data = dataset_selection_test, aes(x = cnt, y = y_hat_qp_model)) +
  geom_point() + 
  geom_abline(color = "blue") +
  labs(x = "Count of bike rentals", 
       y = "Quasipoisson model predictions") +
  ggtitle("Quasipoisson model vs actual bike rentals (test set)")

ggplot(data = dataset_selection_test, aes(x = cnt, y = y_hat_dt_model)) +
  geom_point() + 
  geom_abline(color = "blue") +
  labs(x = "Count of bike rentals", 
       y = "Decision tree model predictions") +
  ggtitle("Decision tree vs actual bike rentals (test set)")

ggplot(data = dataset_selection_test, aes(x = cnt, y = y_hat_rf_model)) +
  geom_point() + 
  geom_abline(color = "blue") +
  labs(x = "Count of bike rentals", 
       y = "Random forest model predictions") +
  ggtitle("Random forest model vs actual bike rentals (test set)")

# --> Graphically, it is clear that the random forest model performs better than the other models.  


