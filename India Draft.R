# 1. Data Source
# Get quantmod
if (!require("quantmod")) {
  install.packages("quantmod")
  library(quantmod)
}

startIN <- as.Date("2018-06-18")
endIN <- as.Date("2018-10-18")

#Infosys Limited:
symbolBasket6 <- c('INFY.BO') #Infosys Limited
getSymbols(symbolBasket6 , src='yahoo', from = startIN, to = endIN)
summary('INFY.BO')

Infosys <- as.xts(`INFY.BO`)
names(Infosys)
names(Infosys) <- c("Infosys.Open"   ,  "Infosys.High"   ,  "Infosys.Low"   ,   "Infosys.Close"  ,  "Infosys.Volume",  "Infosys.Adjusted")
names(Infosys)
View(Infosys)

#Tech Mahindra Limited:
symbolBasket7 <- c('TECHM.BO') #Tech Mahindra Limited
getSymbols(symbolBasket7, src = 'yahoo', from = startIN, to = endIN)
summary('TECHM.BO')

Mahindra <- as.xts(`TECHM.BO`)
names(Mahindra)
names(Mahindra) <- c("Mahindra.Open"   ,  "Mahindra.High"   ,  "Mahindra.Low"   ,   "Mahindra.Close"  ,  "Mahindra.Volume",  "Mahindra.Adjusted")
names(Mahindra)
View(Mahindra)

#Bajaj Auto Limited:
symbolBasket8 <- c('BAJAJ-AUTO.BO') #Bajaj Auto Limited
getSymbols(symbolBasket8, src = 'yahoo', from = startIN, to = endIN)
summary('BAJAJ-AUTO.BO')

Bajaj <- as.xts(`BAJAJ-AUTO.BO`)
names(Bajaj)
names(Bajaj) <- c("Bajaj.Open"   ,  "Bajaj.High"   ,  "Bajaj.Low"   ,   "Bajaj.Close"  ,  "Bajaj.Volume",  "Bajaj.Adjusted")
names(Bajaj)
View(Bajaj)

#ICICI Bank Limited:
symbolBasket9 <- c('ICICIBANK.BO') #ICICI Bank Limited
getSymbols(symbolBasket9, src = 'yahoo', from = startIN, to = endIN)
summary('ICICIBANK.BO')

ICICI <- as.xts(`ICICIBANK.BO`)
names(ICICI)
names(ICICI) <- c("ICICI.Open"   ,  "ICICI.High"   ,  "ICICI.Low"   ,   "ICICI.Close"  ,  "ICICI.Volume",  "ICICI.Adjusted")
names(ICICI)
View(ICICI)

#Larsen & Toubro Limited:
symbolBasket10 <- c('LT.BO') #Larsen & Toubro Limited
getSymbols(symbolBasket10, src = 'yahoo', from = startIN, to = endIN)
summary('LT.BO')

LT <- as.xts(`LT.BO`)
names(LT)
names(LT) <- c("LT.Open"   ,  "LT.High"   ,  "LT.Low"   ,   "LT.Close"  ,  "LT.Volume",  "LT.Adjusted")
names(LT)
View(LT)


#2. Calculations

#Market Data

INRF <- read.csv("IN RF.csv")
INMKT <- read.csv("IN MKT.csv")

INRF$Return <- (INRF$Close-INRF$Open)/INRF$Open
INMKT$Return <- (INMKT$Close-INMKT$Open)/INMKT$Open

#Stock Data

#Infosys Limited:
#Calculating Return
Infosys$Infosys.Return <- (Infosys$Infosys.Close-Infosys$Infosys.Open)/Infosys$Infosys.Open
Infosys$RF.Return <- (INRF$Return)
Infosys$MKT.Return <- (INMKT$Return)

#Calculating Abnormal Return and Cumulative Abnormal Return
Infosys$Infosys.AR <- Infosys$Infosys.Return-Infosys$MKT.Return
Infosys$Infosys.CAR <- cumsum(Infosys$Infosys.AR)

#Plot Graphs
Infosys$Infosys.DateNum <- 1:nrow(Infosys) 

plot(Infosys$Infosys.Return)

plot(Infosys$Infosys.CAR)
plot(Infosys$Infosys.CAR[1:38])
plot(Infosys$Infosys.CAR[39:88])

Infosys.EP <- Infosys[1:38,] #Creating a subset for the Estimation Period, from 9th May to 29th June
Infosys.EW<- Infosys[39:88,] #Creating a subset for the Event Window, from 2nd July to 7th September
