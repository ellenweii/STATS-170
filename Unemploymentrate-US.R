##### Hw1
## 

### Section 1: Introduction
?Quandl()
library(Quandl)
Quandl.api_key("L-ywzPEgfS1cqHss39zN")

ts1= Quandl(code="FRED/UNRATENSA",
            type="ts",
            collapse="monthly",
            order="asc",
            start_date="2001-01-01",
            end_date="2018-12-31",
            meta=TRUE, na.omit=TRUE)
ts1

ts2= Quandl(code="FRED/FEDFUNDS",
            type="ts",
            collapse="monthly",
            order="asc",
            start_date="2001-01-01",
            end_date="2018-12-31",
            meta=TRUE)
ts2

ts3= Quandl(code="FRED/JTUJOL",
            type="ts",
            collapse="monthly",
            order="asc",
            start_date="2001-01-01",
            end_date="2018-12-31",
            meta=TRUE)
ts3=ts3/1000 #converted in thousands unit to in millions unit

par(
  mfrow=c(1,1),
  font.axis=2,
  mar=c(5,5,5,5),
  font.main=2,
  font.lab=2
)

library(dygraphs)

dygraph(ts1,main="US Unemployment Rate",
        ylab="Percentage", xlab="Date") %>% dyRangeSelector

dygraph(ts2,main="US Interest Rate",
        ylab="Number of Job Openings", xlab="Date") %>% dyRangeSelector

dygraph(ts3,main="Number of US Non-farm Job Openings",
        ylab="Percentage", xlab="Date") %>% dyRangeSelector

# training dataset from 2001 to 2017
ts1.train = window(ts1, start=c(2000, 1), end=c(2017, 12), freq=12)
y = ts1.train

# test dataset from 2017 to 2018
ts1.test = window(ts1, start=c(2018, 1), freq=12)
ts1.test


### Section 2: Component Features

plot(decompose(y,type="add"))
plot(decompose(y,type="mult"))

# The random term fluctuates less with additive decomposition.

components=decompose(ts1.train,type="add")
ts1.train.trend=components$trend
ts1.train.seasonal=components$seasonal
ts1.train.random=components$random

# Seasonal Boxplot
boxplot(ts1.train~cycle(ts1.train), ylab="Training data", xlab="Season", main="Seasonal Boxplot")


### Section 3: Autocorrelation Features

par(mfrow=c(3,1))
y.random=ts1.train.random
ynew=window(y.random, start=c(2002,1), end=c(2016,12))

plot.ts(ynew, main="Random part of Unemployment from 2002 to 2016")
acf(ynew, main="ACF of random of Unemployment")
acf(ynew, main="PACF of random of Unemployment")
dev.off()

### Section 4: Exponential Smoothing and Forecasting

# Trend-corrected exponential smoothing
y.exp = HoltWinters(y,gamma=FALSE)
y.exp

y.exp$SSE
sqrt(y.exp$SSE/length(y))

fitted(y.exp)
plot(y.exp)

plot(y,ylab="Monthly unemployment rate")
lines(fitted(y.exp)[,1], type="o",col="red")

plot(fitted(y.exp))

forecasts=predict(y.exp,n.ahead=12)
forecasts

ts.plot(y, forecasts
        ,main="Figure 4:Forecasted values 1 year ahead (dashed line)",lty=1:2)
ts.plot(y, ts1.test
        ,main="Figure 5:Actual Value")

# Trend-corrected+Seasonal
y.exp2=HoltWinters(y)
y.exp2

y.exp2$SSE
sqrt(y.exp2$SSE/length(therm))

fitted(y.exp2)  # see the last training set gamma_0 and gamma_1 

# plot of data and fitted values, yhat 
plot(y.exp2,ylab="Monthly Unemployment rate")
lines(fitted(y.exp2)[,1], type="o",col="red")
plot(fitted(y.exp2))

forecasts=predict(y.exp2,n.ahead=12)
forecasts

ts.plot(y, forecasts
        ,main="Figure 4:Forecasted values 1 year ahead (dashed line)",lty=1:2)

ts.plot(y, ts1.test
        ,main="Figure 5:Actual Value")

## Selection: Trend-corrected+seasonal expoential smoothing
ts.plot(window(y, start=c(2015,1), end=c(2017,12)), forecasts, ts1.test,col=c("black", "red")
        ,main="Trend-corrected+seasonal exponential smoothing",lty=1:2, ylab="Unemployment Rate")
lines(ts1.test, type="l",col="blue")
legend("topright", legend=c("train","fcast","fitted"), lty=c(1,2,1), col=c("black", "red", "blue"))

# RMSE
sqrt(mean((ts1.test - forecasts)^2))

### Section V:  Polynomial Regression

time=seq(1:length(y))
time2=time^2
time3=time^3
time4=time^4
regmodel = lm(y~time+time2+time3+time4)

summary(regmodel)
forecast=c(rep(0,12))
n = length(time)
for(j in 1:12){
  forecast[j] = 6.552e+00-1.349e-01*(n+j)+2.934e-03*(n+j)^2-1.801e-05*(n+j)^3+3.172e-08*(n+j)^4+ts1.train.seasonal[j]
}

# RMSE
sqrt(mean((ts1.test - forecast)^2))

plot(c(y, forecast), lty=1,col=c("black"),type="l",
     xlab="Time", ylab="y", 
     main="Data and Forecast with Poly Trend")

### Section 6: Conclusion

# RMSE: Calculated in each model section













##### Hw2

### Section 7: ARIMA modeling and forecasting

## (1) Pre-transformations

y #training dataset
ts1.test #testing dataset

# remove non-stationarity in the variance with polynomial transformations
ln.ts1=ts(log(y),frequency=12,start=c(2000,1))  #log transform
sqr.ts1=ts(sqrt(y),frequency=12,start=c(2000,1)) # square root
qrt.ts1=ts(y^(0.25),frequency=12,start=c(2000,1))  # quartic

par(mfrow=c(2,2))
plot.ts(y,type="l",main="tsplot of unemployment rate data ")
plot.ts(ln.ts1,type="l",main="tsplot of ln(unemployment rate) data")
plot.ts(sqr.ts1,type="l",main="tsplot of sqr(unemployment rate) data") 
plot.ts(qrt.ts1,type="l",main="tsplot of qrt(unemployment rate) data")
dev.off()

# The data seems cyclical

## (2) Assessment of Stationarity in the Mean

par(mfrow=c(3,2))

# Regular first order differencing
diff1=diff(y,lag=1,differences=1)
plot.ts(diff1)
acf(diff1,lag.max=50,main="ACF of (1-B)X")   

# Seasonal differencing
diff12=diff(y,lag=12,differences=1)
plot.ts(diff12)
acf(diff12,lag.max=50,main="ACF of (1-B^12)X")    

# Both
diff1diff12=diff(diff1,lag=12,differences=1)
plot.ts(diff1diff12, main="TS Plot of (1-B^12)(1-B)X")
acf(diff1diff12,lag.max=50,main="ACF of (1-B^12)(1-B)X")
dev.off()

y.star = diff1diff12

## Seasonal of regular differencing appears to be the best fit

## (3) Model Identification
par(mfrow=c(1,2))
acf(y.star,lag.max=50,main="ACF of (1-B^12)X") 
pacf(y.star,lag.max=50, main="PACF of (1-B^12)")
dev.off()

# Order: AR 1, degree of differencing 1, MA 2
# Seasonal order: AR 1, degree of differencing 1, MA 1

model1=arima(y, order=c(1,1,2), seas=list(order=c(1,1,1), 12))
model1


## (4) Fit and diagnose
par(mfrow=c(1,2))
acf(resid(model1),lag.max=50, main="ACF of residuals")
pacf(resid(model1),lag.max=50, main="PACF of residuals")

# Other than at lag 0, there are no significant autocorrelations

model2=arima(y, order=c(1,1,2), seas=list(order=c(0,1,1), 12))
model2

acf(resid(model2),lag.max=50, main="ACF of residuals")
pacf(resid(model2),lag.max=50, main="PACF of residuals")

# (5): Forecast
forecast=predict(model2,12)
forecast.value=ts(forecast$pred, start=c(2018,1),end=c(2018,12), freq=12)

ci.low= ts(forecast$pred-1.96*forecast$se, start=c(2018,1),end=c(2018,12),freq=12)
ci.high=ts(forecast$pred+1.96*forecast$se,start=c(2018,1),end=c(2018,12),freq=12)

par(mfrow=c(2,1))
ts.plot(cbind(y, forecast.value, ci.low, ci.high),lty=c(1,3,2,3), 
        col=c("black","red","blue","blue"), main="Predicted monthly values from best model",ylab="Unemployment rate")
legend(x=2002,y=8, lty=c(1,3,2,2), text.col=c("black","red","blue","blue"), 
       legend=c("actual rate", "forecast", "CIlow", "CIhigh"),text.font=1, cex=0.5)
abline(v=2018)

ts.plot(cbind(ts1.test, forecast.value,ci.low, ci.high),lty=c(1,3,2,2),
        col=c("black","red","blue","blue"),
        main="Forecast and observed data 2018",ylab="Unemployment rate")
legend(x=2018.8,y=3.2, lty=c(1,3,2,2), text.col=c("black","red","blue","blue"), 
       legend=c("actual rate", "forecast", "CIlow", "CIhigh"),text.font=1, cex=0.5)
dev.off()

# y, forecast, forecast interval, standard error
ts1.test
forecast.value
ci.low
ci.high
sqrt(mean((ts1.test - forecast.value)^2))


### Section 8: Multiple regression with ARMA residuals
ts2.train = window(ts2, start=c(2001, 1), end=c(2017, 12), freq=12)
ts2.train

ts2.test = window(ts2, start=c(2018, 1), freq=12)
ts2.test

ts3.train = window(ts3, start=c(2001, 1), end=c(2017, 12), freq=12)
ts3.train

ts3.test = window(ts3, start=c(2018, 1), freq=12)
ts3.test

y = ts1.train
x1 = ts2.train
x2 = ts3.train

mrmodel = lm(y~x1+x2)
par(mfrow=c(2,1))
acf(resid(mrmodel),lag.max=50, main="ACF of residuals of model")
pacf(resid(mrmodel),lag.max=50, main="PACF of residuals of model")
dev.off()

ynew = diff(diff(ts1.train,lag=12, differences = 1))
x1new = diff(diff(ts2.train,lag=12, differences = 1))
x2new = diff(diff(ts3.train,lag=12, differences = 1))
  
mrmodel = lm(ynew~x1new+x2new)
par(mfrow=c(2,1))
acf(resid(mrmodel),lag.max=50, main="ACF of residuals of model")
pacf(resid(mrmodel),lag.max=50, main="PACF of residuals of model")
dev.off()

plot(mrmodel)

resmodel1=arima(residuals(mrmodel), order=c(1,0,0), 
                seas=list(order=c(0,0,0), 12), include.mean=F)
resmodel1

par(mfrow=c(2,1))
acf(resid(resmodel1),lag.max=50, main="ACF of res of res")
hist(resid(model1))
dev.off()


library(nlme)
modelgls=gls(y~x1+x2, correlation=corARMA(c(0.8467),p=1))
summary(modelgls)
plot(y=residuals(modelgls,type="normalized"),
     x=as.vector(time(y)), type="l",
     ylab="uncorrelated residuals",
     xlab="Time",
     main="Example of uncorrelated residuals")
abline(h=0)

acf(ts(residuals(modelgls,type="normalized")))

resmean=mean(mrmodel$residuals)
ressd=sd(mrmodel$residuals)
sdresiduals=(mrmodel$residuals-resmean)/ressd
sdresidualsts=ts(sdresiduals,frequency=12,
                 start=c(2001,1))
par(mfrow=c(1,2))
plot.ts(sdresidualsts, 
        main="Model residuals",ylab="standardized residuals")
abline(h=0) 
plot(y=residuals(modelgls,type="normalized"),
     x=as.vector(time(y)), type="l",
     ylab=" residuals",
     xlab="Time",
     main="Residuals after correcting autocorrelation")
abline(h=0)

forecast=predict(modelgls, ts1.test)
forecast[1:12]

sqrt(mean((ts1.test - forecast[1:12])^2))


### Section IX: Vector autoregression

unem = ts1.train
inte = ts2.train
jobs = ts3.train

par(mfrow=c(3,1))
acf(unem)
acf(inte)
acf(jobs)
dev.off()

tsdata = cbind(unem, inte, jobs)

par(mfrow = c(1,1),
    font.axis = 2,
    font.main = 2,
    font.lab = 2,
    mar = c(5, 5, 4, 4))
acf(tsdata,lwd=1.5,cex=1.5)

regDiff.data = diff(tsdata,lag=1, differences = 1)
seasDiff.data = diff(tsdata,lag=12, differences = 1)
bothDiff.data = diff(diff(tsdata,lag=12, differences = 1))

acf(seasDiff.data,lwd=1.5,cex=1.5)
acf(regDiff.data,lwd=1.5,cex=1.5)
acf(bothDiff.data,lwd=1.5,cex=1.5, lag=50)

library(vars)
VAR.model <- VAR(bothDiff.data, p=2,type=)
coef(VAR.model)
summary(VAR.model)
acf(resid(VAR.model))



# Impulse Response Functions

par(mfrow = c(1,1),
    font.axis = 2,
    font.main = 2,
    font.lab = 2,
    mar = c(5, 5, 4, 4))

irf=irf(VAR.model, impulse = "unem", response = c("unem", "inte", "jobs"), boot =
          FALSE,n.ahead=36,lwd=2)
plot(irf, main="Orthogonal Impulse Response from Unemployment Rate")

dev.off()

irf=irf(VAR.model, impulse = "inte", response = c("unem", "inte", "jobs"), boot =
          FALSE,n.ahead=40)
plot(irf, main="Orthogonal Impulse Response from Interest Rate")

par(mfrow = c(1,1),
    font.axis = 2,
    font.main = 2,
    font.lab = 2,
    mar = c(5, 5, 4, 4))

irf=irf(VAR.model, impulse = "jobs", response = c("unem", "inte", "jobs"), boot =
          FALSE,n.ahead=40,lwd=2)
## plot the four impulse response variables
plot(irf, main="Orthogonal Impulse Response from Job Openings")

dev.off()



# Forecast

VAR.pred <- predict(VAR.model, n.ahead=12)
unem.pred <- ts(VAR.pred$fcst$unem[,1],st=c(2018,1),fr=12) 
unem.pred.low <-ts(VAR.pred$fcst$unem[,2],st=c(2018,1),fr=12) 
unem.pred.upper<- ts(VAR.pred$fcst$unem[,3],st=c(2018,1),fr=12) 

inte.pred <-ts(VAR.pred$fcst$inte[,1],st=c(2018,1),fr=12)
inte.pred.low <-ts(VAR.pred$fcst$inte[,2],st=c(2018,1),fr=12) 
inte.pred.upper<- ts(VAR.pred$fcst$inte[,3],st=c(2018,1),fr=12) 

jobs.pred <-ts(VAR.pred$fcst$jobs[,1],st=c(2018,1),fr=12)
jobs.pred.low <-ts(VAR.pred$fcst$jobs[,2],st=c(2018,1),fr=12) 
jobs.pred.upper<- ts(VAR.pred$fcst$jobs[,3],st=c(2018,1),fr=12) 

par(mfrow = c(1,1))

ts1.test2 = ts(c(window(ts1.train, start=c(2017, 1), freq=12), ts1.test), start=c(2017, 1), end=c(2018, 12), freq=12)
ts1.test2

ts2.test2 = ts(c(window(ts2.train, start=c(2017, 1), freq=12), ts2.test), start=c(2017, 1), end=c(2018, 12), freq=12)
ts2.test2

ts3.test2 = ts(c(window(ts3.train, start=c(2017, 1), freq=12), ts3.test), start=c(2017, 1), end=c(2018, 12), freq=12)
ts3.test2

ts.plot(cbind(bothDiff.data[,1], unem.pred, unem.pred.low, unem.pred.upper, diff(diff(ts1.test2,lag=12, differences = 1))),
        lty=c(1,2,3,3,4), col=c("black","red", "blue","blue","black"), lwd=c(2,2,2,2,2),
        main = "Forecast of change in unemployemntR with VAR(2)",ylab="unem",ylim=c(-2,2))
legend(2015,1.8,c("unem","forecast", "CIL", "CIU","test"),lty=c(1,2,3,3,4),col=c("black","red", "blue","blue","black"))

ts.plot(cbind(bothDiff.data[,2], inte.pred, inte.pred.low, inte.pred.upper, diff(diff(ts2.test2,lag=12, differences = 1))),
        lty=c(1,2,3,3,4), col=c("black","red", "blue","blue","black"), lwd=c(2,2,2,2,2),
        main = "Forecast of change in inte with VAR(2)",ylab="inte",ylim=c(-2,2))

#ts.plot(cbind(bothDiff.data[,3], jobs.pred, jobs.pred.low, jobs.pred.upper, diff(diff(ts3.test2,lag=12, differences = 1))),
        #lty=c(1,2,3,3,4), col=c("black","red", "blue","blue","black"), lwd=c(2,2,2,2,2),
        #main="Forecast of jobs with VAR(2)", ylab="jobs",ylim=c(-1200, 1200))


sqrt(mean((diff(diff(ts1.test2,lag=12, differences = 1)) - unem.pred)^2))

