#FORECAST COMBINE



##JAPAN




data<-merge(Tata,Mahindra, Bajaj, ICICI, LT)
dataIN <-data[,c(4, 10, 16, 22, 28)]              

data<-merge(Amada,Hokuetsu, Secom, Tokyu, Mitsubishi)
dataJP <-data[,c(4, 10, 16, 22, 28)]

View(dataJP)
View(dataIN)

TataClose <- dataIN[,1]
MahindraClose <-dataIN[,2]
BajajClose <- dataIN[,3]
ICICIClose <- dataIN[,4]
LTClose  <- dataIN[,5]

AmadaClose <- dataJP[,1]
HokuetsuClose <-dataJP[,2]
MitsubishiClose <- dataJP[,3]
SecomClose <- dataJP[,4]
TokyuClose  <- dataJP[,5]

ForecastAVGJP <- AmadaClose*(1/(AmadaClose+HokuetsuClose+MitsubishiClose+SecomClose+TokyuClose))
+HokuetsuClose*(1/(AmadaClose+HokuetsuClose+MitsubishiClose+SecomClose+TokyuClose))
+MitsubishiClose*(1/(AmadaClose+HokuetsuClose+MitsubishiClose+SecomClose+TokyuClose))
+SecomClose*(1/(AmadaClose+HokuetsuClose+MitsubishiClose+SecomClose+TokyuClose))
+TokyuClose*(1/(AmadaClose+HokuetsuClose+MitsubishiClose+SecomClose+TokyuClose))

ForecastAVGIN <- TataClose*(1/(TataClose+MahindraClose+BajajClose+ICICIClose+LTClose))
+MahindraClose*(1/(TataClose+MahindraClose+BajajClose+ICICIClose+LTClose))
+BajajClose*(1/(TataClose+MahindraClose+BajajClose+ICICIClose+LTClose))
+ICICIClose*(1/(TataClose+MahindraClose+BajajClose+ICICIClose+LTClose))
+LTClose*(1/(TataClose+MahindraClose+BajajClose+ICICIClose+LTClose))

plot(ForecastAVGJP)
plot(ForecastAVGIN)

#JAPAN

#Dataset forecast upper first 5 values
print(adf.test(ForecastAVGJP))
modelfit <- auto.arima(ForecastAVGJP, lambda = "auto")

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
N = length(ForecastAVGJP)
train = ForecastAVGJP[1:18, ]
test  = ForecastAVGJP[(19):N,  ]
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
fitarfima = autoarfima(data = ForecastAVGJP, ar.max = 2, ma.max = 2, 
                       criterion = "AIC", method = "full")
#define the model
garch11closeprice=ugarchspec(variance.model=list(garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,2)))
#estimate model 
garch11closepricefit=ugarchfit(spec=garch11closeprice, data=ForecastAVGJP)

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
hn <- length(ForecastAVGJP)/(alpha*(length(ForecastAVGJP)+30))

#Fitting nnetar
lambda <- BoxCox.lambda(ForecastAVGJP)
dnn_pred <- nnetar(ForecastAVGJP, size= hn, lambda = lambda)

#Fitting nnetar
dnn_forecast <- forecast(dnn_pred, h= 30, PI= TRUE)

plot(dnn_forecast)






##INDIA 





#Dataset forecast upper first 5 values
print(adf.test(ForecastAVGIN))
modelfit <- auto.arima(ForecastAVGIN, lambda = "auto")

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
N = length(ForecastAVGIN)
train = ForecastAVGIN[1:18,]
test  = ForecastAVGIN[(19):N,]
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
fitarfima = autoarfima(data = ForecastAVGIN, ar.max = 2, ma.max = 2, 
                       criterion = "AIC", method = "full")
#define the model
garch11closeprice=ugarchspec(variance.model=list(garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,2)))
#estimate model 
garch11closepricefit=ugarchfit(spec=garch11closeprice, data=ForecastAVGIN)

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
hn <- length(ForecastAVGIN)/(alpha*(length(ForecastAVGIN)+30))

#Fitting nnetar
lambda <- BoxCox.lambda(ForecastAVGIN)
dnn_pred <- nnetar(ForecastAVGIN, size= hn, lambda = lambda)

#Fitting nnetar
dnn_forecast <- forecast(dnn_pred, h= 30, PI = TRUE)

plot(dnn_forecast)

