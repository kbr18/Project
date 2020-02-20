if(!require(quantmod)) install.packages("quantmod")
if(!require(forecast)) install.packages("forecast")
if(!require(xlsx)) install.packages("xlsx")
if(!require(tseries)) install.packages("tseries")
if(!require(timeSeries)) install.packages("timeSeries")
if(!require(dplyr)) install.packages("dplyr")
if(!require(fGarch)) install.packages("fGarch")
if(!require(prophet)) install.packages("prophet")
library(prophet)
library(quantmod)
library(forecast)
library("xlsx")
library(tseries)
library(timeSeries)
library(dplyr)
library(fGarch)


if(!require(rugarch)) install.packages("rugarch")
library(rugarch)

View(dataJP)


data<-merge(Tata,Mahindra, Bajaj, ICICI, LT)
dataIN <-data[,c(4, 10, 16, 22, 28)]              

data<-merge(Amada,Hokuetsu, Secom, Tokyu, Mitsubishi)
dataJP <-data[,c(4, 10, 16, 22, 28)]


TataClose <- dataIN[,1]
MahindraClose <-dataIN[,2]
BajajClose <- dataIN[,3]
ICICIClose <- dataIN[,4]
LTClose  <- dataIN[,5]

AmadaClose <- dataJP[,1]
HokuetsuClose <-dataJP[,2]
MitsubishiClose <- dataJP[,3]
SecomClose <- dataJP[,4]
TokyoClose  <- dataJP[,5]


list_data <- list("TataClose", "MahindraClose","BajajClose", "ICICIClose", "LTClose",
                  "AmadaClose", "HokuetsuClose", "MitsubishiClose", "SecomClose", "TokyoClose")


for (i in list_data)
  {
  
  #Dataset forecast upper first 5 values
  print(adf.test(i))
  modelfit <- auto.arima(i, lambda = "auto")
  
  #Box test for lag=2
  Box.test(modelfit$residuals, lag= 2, type="Ljung-Box")
  
  #Dataset forecasting for the next 30 days
  price_forecast <- forecast(modelfit, h=30)
  
  
  #Dataset forecast lower first 5 values
  plot(price_forecast)
  
  head(price_forecast$mean)
  
  head(price_forecast$lower)
  
  head(price_forecast$upper)
  
  #Dividing the data into train and test, applying the model
  N = length(i)
  n = 0.7*N
  train = i[1:n, ]
  test  = i[(n+1):N,  ]
  trainarimafit <- auto.arima(train, lambda = "auto")
  predlen=length(test)
  trainarimafit <- forecast(trainarimafit, h=predlen)
  
  #Plotting mean predicted values vs real data
  meanvalues <- as.vector(trainarimafit$mean)
  precios <- as.vector(test$AMZN.Close)
  plot(meanvalues, type= "l", col= "red")
  lines(precios, type = "l")
  
  ###Dataset forecast upper first 5 values
  if(!require(rugarch)) install.packages("rugarch")
  
  library(rugarch)
  
  #Dataset forecast upper first 5 values
  fitarfima = autoarfima(data = i, ar.max = 2, ma.max = 2, 
                         criterion = "AIC", method = "full")
  #define the model
  garch11closeprice=ugarchspec(variance.model=list(garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,2)))
  #estimate model 
  garch11closepricefit=ugarchfit(spec=garch11closeprice, data=i)
  
  #conditional volatility plot
  plot.ts(sigma(garch11closepricefit), ylab="sigma(t)", col="blue")
  
  #Model akike
  infocriteria(garch11closepricefit)
  
  #Normal residuals
  garchres <- data.frame(residuals(garch11closepricefit))  
  plot(garchres$residuals.garch11closepricefit.)
  
  #Standardized residuals
  garchres <- data.frame(residuals(garch11closepricefit, standardize=TRUE)) 
  
  #Squared standardized residuals Ljung Box
  garchres <- data.frame(residuals(garch11closepricefit, standardize=TRUE)^2) 
  Box.test(garchres$residuals.garch11closepricefit..standardize...TRUE..2, type="Ljung-Box")
  
  #GARCH Forecasting
  garchforecast <- ugarchforecast(garch11closepricefit, n.ahead = 30 )
  plot(garchforecast)

  ########Feed Foward Neural network
  #Hidden layers creation
  alpha <- 1.5^(-10)
  hn <- length(i)/(alpha*(length(i)+30))
  
  #Fitting nnetar
  lambda <- BoxCox.lambda(i)
  dnn_pred <- nnetar(i, size= hn, lambda = lambda)
  
  #Fitting nnetar
  dnn_forecast <- forecast(dnn_pred, h= 30, PI = TRUE)
  
  plot(dnn_forecast)
  
}


  

