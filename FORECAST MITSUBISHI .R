
#Dataset forecast upper first 5 values
print(adf.test(MitsubishiClose))
modelfit <- auto.arima(MitsubishiClose, lambda = "auto")

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
N = length(MitsubishiClose)
n = 0.7*N
train = MitsubishiClose[1:n, ]
test  = MitsubishiClose[(n+1):N,  ]
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
fitarfima = autoarfima(data = MitsubishiClose, ar.max = 2, ma.max = 2, 
                       criterion = "AIC", method = "full")
#define the model
garch11closeprice=ugarchspec(variance.model=list(garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,2)))
#estimate model 
garch11closepricefit=ugarchfit(spec=garch11closeprice, data=MitsubishiClose)

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
hn <- length(MitsubishiClose)/(alpha*(length(MitsubishiClose)+30))

#Fitting nnetar
lambda <- BoxCox.lambda(MitsubishiClose)
dnn_pred <- nnetar(MitsubishiClose, size= hn, lambda = lambda)

#Fitting nnetar
dnn_forecast <- forecast(dnn_pred, h= 30, PI = TRUE)

plot(dnn_forecast)