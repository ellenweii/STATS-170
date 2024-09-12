##############################################################################
## Ellen Wei
## UID: 505606652

# Chapter 10                                                                 #
# Sanchez, J. Introduction to time series for Data Science, CUP 2023.                        #
# Explanation of the code created by package TSstudio author Rami Krispin   #         
# We connect code learned earlier in the book using Base R to the code       #
# and wrappers developed by R. Krispin for the TSstudio package   
# Commented code is obtained from Krispin (2019)-Time series Analysis   # 
# with R. (Packt editors)
# Program MLreg.R in Base R version                                          #
# Last modified: 2/25/2023                                                   #
# See the tsibble version of this program in the CH10-CODE-DATA-TSIBBLE      #
##############################################################################

#   install.packages("TSstudio")
   library(TSstudio)
## As a package, TSstudio has its own functions. 
library(tsfeatures)
library(Quandl)
Quandl.api_key("L-ywzPEgfS1cqHss39zN")
unemployment_rate = Quandl(code="FRED/UNRATENSA",
                           type="ts",  
                           collapse="monthly", 
                           order="asc", 
                           start_date="1980-01-01",
                           end_date="2019-01-01",
                           meta=TRUE)
   #data(unemployment_rate)
   class(unemployment_rate) # manageable, relatively short time series
   unemployment_rate  # small data set 
     
   ts_info(unemployment_rate) # rapper for frequency(), start() and  end() combined
   ts_plot(unemployment_rate, 
        title="Unemployment Rate", 
        Ytitle="Percentage",
        Xtitle="Year") 
# The series components 
   ts_decompose(unemployment_rate) #wrapper for decompose 

#Seasonal analysis
 
   unemployment_rate_detrend=unemployment_rate-decompose(unemployment_rate)$trend

# the following is an enhanced wrapper for seasonal boxplot.
    ts_seasonal(unemployment_rate_detrend,type="box") 


#The following is a wrapper for correlation analysis with acf and ccf 
# notice that the wrapper omits the lag 0 in the acf
# which the acf of a ts() object does not do. 
    ts_cor(unemployment_rate)
## the following is a wrapper for lag plots 
  ts_lags(unemployment_rate, lags=c(12,24,36))

##Select training data 
  # See below a wrapper to create a training window of the data 
  # and at the same time rename the variables to be ready for prophet. 
   df=ts_to_prophet(window(unemployment_rate,start=c(1980,1)))
      names(df)=c("date","y")
     head(df)
  ts_plot(df, 
        title="Unemployment Rate (subset)", 
        Ytitle="Percentage",
        Xtitle="Year") 
  
   ##Feature engineering 
   # create new features that will be used as inputs in the model 
  
 #  install.packages("dplyr")
    library(dplyr)
   library(lubridate)

   df <- df %>% mutate(month = factor(lubridate::month(date, label = TRUE), ordered = FALSE),
                    lag12 = lag(y, n = 12)) %>%
     filter(!is.na(lag12))
  df$trend <- 1:nrow(df)

   str(df)


## training, testing and model evaluation

   h <- 12
   train_df <- df[1:(nrow(df) - h), ]
   test_df <- df[(nrow(df) - h + 1):nrow(df), ]

## inputs for forecast 

forecast_df <- data.frame(date = seq.Date(from = max(train_df$date) + lubridate::month(1),
                                          length.out = h, by = "month"),
                          trend = seq(from = max(df$trend) + 1, length.out = h, by = 1))
   #forecast_df$trend_sqr <- forecast_df$trend ^ 2

# to avoid conflict with the h2o `month` function use the "lubridate::month" 
# to explicly call the month from the lubridate function 
   forecast_df$month <- factor(lubridate::month(forecast_df$date, 
                                                label = TRUE), ordered= FALSE) 
   forecast_df$lag12 <- tail(df$y, 12)
   

## benchmark the ML models with this model
#lr <- lm(y~month+lag12+trend+linearity+entropy,data=train_df)
   
   lr <- lm(y ~ month + lag12 + trend, data = train_df)
summary(lr)
test_df$yhat <- predict(lr, newdata = test_df)
mape_lr <- mean(abs(test_df$y - test_df$yhat) / test_df$y)
mape_lr #0.1404531
rmse_lr = sqrt(mean((test_df$y - test_df$yhat)^2))
rmse_lr #0.5533557

#### Prepare for ML 
#install.packages("h2o")
library(h2o)

h2o.init(max_mem_size = "16G")
train_h <- as.h2o(train_df)
test_h <- as.h2o(test_df)
forecast_h <- as.h2o(forecast_df)



###### Simple RF model using 500 trees and 5 folder CV 

### Training process. Will add stop criterion. Stopping metric 
## is RMSE, tolerance is 0.0001, stopping rounds set 
# to 10 (Krispin, 2019)

x=c("month", "lag12", "trend")
y="y"

rf_md = h2o.randomForest(training_frame=train_h,
                      nfolds=5,
                      x=x, 
                      y=y, 
                      ntrees=500,
                      stopping_rounds=10,
                      stopping_metric="RMSE", 
                      score_each_iteration=TRUE,
                      stopping_tolerance=0.0001,
                      seed=1234)

 ## see the contribution of the mode inputs

    h2o.varimp_plot(rf_md)

## See model output 

    rf_md@model$model_summary
    
    rf_md

## learning process of the model as a function of the number of trees

  library(plotly)
  tree_score =rf_md@model$scoring_history$training_rmse
 
  plot_ly(x=seq_along(tree_score), y=tree_score,
          type="scatter", mode="line") %>%
          layout(title="The trained Model Score History", 
          yaxis=list(title="RMSE"), 
           xaxis=list(title="Num. of Trees"))
  
## Model performance in test set. Forecasting performance 
  
  test_h$pred_rf = h2o.predict(rf_md, test_h)
  test_h$pred_rf  
  
  ## transfer h2o data frame to a data.frame object 
  
  test_1= as.data.frame(test_h)
  test_1$date = as_date(test_1$date)
  
  ## Calculate the MAPE score of the RF model on the test partition
  
  mape_rf = mean(abs(test_1$y -test_1$pred_rf)/test_1$y)
   mape_rf  #0.1991295
   rmse_rf = sqrt(mean((test_1$y -test_1$pred_rf)^2))
   rmse_rf #0.775971
   
  ## Visualizing model performance 
   
  plot_ly(data=test_1) %>%
      add_lines(x=~date, y=~y, name="Actual") %>%
      add_lines(x=~date, y=~yhat, name="Linear Regression", line=
                  list(dash="dot")) %>%
      add_lines(x=~date, y=~pred_rf, name="RF", line=
                   list(dash="dash")) %>% 
       layout(title="Unemployment Rate vs. Prediction (Random Forest)", 
              yaxis= list(title="Percentage"), 
              xaxis=list(title="Month"))
   
         ####################################
          # Gradient boosting 
          ######################################
  
  ### Train the GB model with the same input used in RF 
  
  gbm_md =h2o.gbm( 
      training_frame = train_h,
      nfold=5, 
      x=x,
      y=y, 
       max_depth=20, 
      distribution="gaussian",
      ntrees=500, 
      learn_rate=0.1,
      score_each_iteration=TRUE 
  )
  
  ## See model output 
  
  gbm_md@model$model_summary
  
  gbm_md
  
## How important are the model variables in the training (fitting)
  h2o.varimp_plot(gbm_md)
  
  ## learning process of the model as a function of the number of trees
  
  library(plotly)
  tree_score =gbm_md@model$scoring_history$training_rmse
  
  plot_ly(x=seq_along(tree_score), y=tree_score,
          type="scatter", mode="line") %>%
    layout(title="The trained Model Score History", 
           yaxis=list(title="RMSE"), 
           xaxis=list(title="Num. of Trees"))
  
# test the model's performance on the testing set 
  
  test_h$pred_gbm = h2o.predict(gbm_md, test_h)
  test_1= as.data.frame(test_h)  
  test_1$date = as_date(test_1$date)

## calculate mape in the test set (of the forecast)
   mape_gbm = mean(abs(test_1$y -test_1$pred_gbm)/test_1$y)
   mape_gbm  #0.1037769
   rmse_gbm = sqrt(mean((test_1$y -test_1$pred_gbm)^2))
   rmse_gbm #0.4336706
   
   ## Visualizing model performance in the test set 
   
   plot_ly(data=test_1) %>%
     add_lines(x=~date, y=~y, name="Actual") %>%
     add_lines(x=~date, y=~yhat, name="Linear Regression", line=
                 list(dash="dot")) %>%
     add_lines(x=~date, y=~pred_gbm, name="GBM", line=
                 list(dash="dash")) %>% 
     add_lines(x=~date, y=~pred_rf, name="RF", line=
                 list(dash="dash")) %>% 
     layout(title="Unemployment Rate-Actual vs. Prediction (GBM)", 
            yaxis= list(title="Percentage"), 
            xaxis=list(title="Month"))
   
  #### Prophet
   library(prophet)
   ## errors with my prophet package and dependency cli version
   
   