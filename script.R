
# Used Vector Autoregression [VAR] - Forecasting algorithm
# Relations between Temperature, Pressure and Partculate Matter
#


library(lubridate)

#separating                                                                            #
Hipodruma_30_Days$Day = day(Hipodruma_30_Days$`Date and Time`)                         #
Hipodruma_30_Days$Hour = as.integer(substr(Hipodruma_30_Days$`Date and Time`,12,13))   #  
                                                                                       #  <- Preparing the data
#sorting data                                                                          #  
library(dplyr)                                                                         #
indata = Hipodruma_30_Days[order(Hipodruma_30_Days$Day,Hipodruma_30_Days$Hour),]       #



#converting data in to time seriess
ts_PM10=ts(indata[,2],frequency = 23)
ts_TEMPERATURE=ts(indata[,3],frequency = 23)
ts_PRESSURE=ts(indata[,4],frequency = 23)

# Plotting time series
plot(ts_TEMPERATURE)
plot(ts_PM10)
plot(ts_PRESSURE)



#Create the models
Temperature.stlm <- stlm(ts_TEMPERATURE)
Pressure.stlm <- stlm(ts_PRESSURE)
PM10.stlm <- stlm(ts_PM10)

require(graphics)

#Forecast
library(forecast)

Temperature.Prediction <- forecast(Temperature.stlm)
Pressure.Prediction <- forecast(Pressure.stlm)
PM10.Prediction <- forecast(PM10.stlm)

plot(PM10.Prediction,main="PM10 Forecast")
plot(Temperature.Prediction, main="Temperature Forecast")
plot(Pressure.Prediction, main="Pressure Forecast")

library(vars)

# [,2:4] refers to PM10, Temperature an Pressure
# Test using a Portmanteau test if the residuals are uncorrelated#
# Portmanteau test - hypothesis that tests if the null hypothesis is specified

var1 <- VAR(Hipodruma_30_Days[,2:4],p=1,type="const")
serial.test(var1,lags.pt = 10,type="PT.asymptotic")
var2 <- VAR(Hipodruma_30_Days[,2:4],p=2,type="const")
serial.test(var2,lags.pt = 10,type="PT.asymptotic")

# Both var1 and var2 have residual serial correlation
# so we fit a var3

var3 <- VAR(Hipodruma_30_Days[,2:4],p=3,type="const")
serial.test(var3,lags.pt = 10,type="PT.asymptotic")

summary(var1) # see the summary (correlation between 3 variables)

forecasts <- predict(var3) #store all the predictions
forecasts  # see the predictions for each variable (PM10,TEMPERATURE,PRESSURE)

plot(forecasts) # Plot and see the predictions
