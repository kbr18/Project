#Getting AMAZON stock dataset and loading the needed packages
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


#Dataset forecast upper first 5 values
getSymbols("AMZN", src="yahoo", from="2015-01-01")

c("AMZN")
AMZNCLOSE <- subset(AMZN$AMZN.Close)
print(adf.test(AMZNCLOSE))
modelfit <- auto.arima(AMZNCLOSE, lambda = "auto")

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
N = length(AMZNCLOSE)
n = 0.7*N
train = AMZNCLOSE[1:n, ]
test  = AMZNCLOSE[(n+1):N,  ]
trainarimafit <- auto.arima(train, lambda = "auto")
predlen=length(test)
trainarimafit <- forecast(trainarimafit, h=predlen)

#Plotting mean predicted values vs real data
meanvalues <- as.vector(trainarimafit$mean)
precios <- as.vector(test$AMZN.Close)
plot(meanvalues, type= "l", col= "red")
lines(precios, type = "l")

#Dataset forecast upper first 5 values
if(!require(rugarch)) install.packages("rugarch")

library(rugarch)

#Dataset forecast upper first 5 values
fitarfima = autoarfima(data = AMZNCLOSE, ar.max = 2, ma.max = 2, 
                       criterion = "AIC", method = "full")
#define the model
garch11closeprice=ugarchspec(variance.model=list(garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,2)))
#estimate model 
garch11closepricefit=ugarchfit(spec=garch11closeprice, data=AMZNCLOSE)

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
hn <- length(AMZNCLOSE)/(alpha*(length(AMZNCLOSE)+30))

#Fitting nnetar
lambda <- BoxCox.lambda(AMZNCLOSE)
dnn_pred <- nnetar(AMZNCLOSE, size= hn, lambda = lambda)

#Fitting nnetar
dnn_forecast <- forecast(dnn_pred, h= 30, PI = TRUE)

plot(dnn_forecast)

---------------------------------------------------------
  
NOW DO IT FOROUR PROJECT 

View(dataJP)
AmadaClose <- dataJP[,1]
HokuetsuClose <-dataJP[,2]
MitsubishiClose <- dataJP[,3]
SecomClose <- dataJP[,4]
TokyoClose  <- dataJP[,5]




