# 1. Data Source
# Get quantmod
if (!require("quantmod")) {
  install.packages("quantmod")
  library(quantmod)
}

startIN <- as.Date("2018-06-18")
endIN <- as.Date("2018-10-18")

#Tata Steel Limited:
symbolBasket6 <- c('TATASTEEL.BO') #Tata Steel Limited
getSymbols(symbolBasket6 , src='yahoo', from = startIN, to = endIN)
summary('TATASTEEL.BO')

Tata <- as.xts(`TATASTEEL.BO`)
names(Tata)
names(Tata) <- c("Tata.Open"   ,  "Tata.High"   ,  "Tata.Low"   ,   "Tata.Close"  ,  "Tata.Volume",  "Tata.Adjusted")
names(Tata)
View(Tata)

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
INMKT <- read.csv("IN MKT.csv")

INMKT$Return <- (INMKT$Close-INMKT$Open)/INMKT$Open

#Stock Data

#Tata Steel Limited:
#Calculating Return
Tata$Tata.Return <- (Tata$Tata.Close-Tata$Tata.Open)/Tata$Tata.Open
Tata$MKT.Return <- (INMKT$Return)

#Calculating Abnormal Return and Cumulative Abnormal Return
Tata$Tata.AR <- Tata$Tata.Return-Tata$MKT.Return
Tata$Tata.CAR <- cumsum(Tata$Tata.AR)

#Plot Graphs
Tata$Tata.DateNum <- 1:nrow(Tata) 

plot(Tata$Tata.Return)
hist(Tata$Tata.Return)

plot(Tata$Tata.CAR)
plot(Tata$Tata.CAR[1:38])
plot(Tata$Tata.CAR[39:83])

Tata.EP <- Tata[1:38,] #Creating a subset for the Estimation Period
Tata.EW<- Tata[39:83,] #Creating a subset for the Event Window


#Tech Mahindra Limited:
#Calculating Return
Mahindra$Mahindra.Return <- (Mahindra$Mahindra.Close-Mahindra$Mahindra.Open)/Mahindra$Mahindra.Open
Mahindra$MKT.Return <- (INMKT$Return)

#Calculating Abnormal Return and Cumulative Abnormal Return
Mahindra$Mahindra.AR <- Mahindra$Mahindra.Return-Mahindra$MKT.Return
Mahindra$Mahindra.CAR <- cumsum(Mahindra$Mahindra.AR)

#Plot Graphs
Mahindra$Mahindra.DateNum <- 1:nrow(Mahindra) 

plot(Mahindra$Mahindra.Return)
hist(Mahindra$Mahindra.Return)

plot(Mahindra$Mahindra.CAR)
plot(Mahindra$Mahindra.CAR[1:38])
plot(Mahindra$Mahindra.CAR[39:83])

Mahindra.EP <- Mahindra[1:38,] #Creating a subset for the Estimation Period
Mahindra.EW<- Mahindra[39:83,] #Creating a subset for the Event Window


#Bajaj Auto Limited:
#Calculating Return
Bajaj$Bajaj.Return <- (Bajaj$Bajaj.Close-Bajaj$Bajaj.Open)/Bajaj$Bajaj.Open
Bajaj$MKT.Return <- (INMKT$Return)

#Calculating Abnormal Return and Cumulative Abnormal Return
Bajaj$Bajaj.AR <- Bajaj$Bajaj.Return-Bajaj$MKT.Return
Bajaj$Bajaj.CAR <- cumsum(Bajaj$Bajaj.AR)

#Plot Graphs
Bajaj$Bajaj.DateNum <- 1:nrow(Bajaj) 

plot(Bajaj$Bajaj.Return)
hist(Bajaj$Bajaj.Return)

plot(Bajaj$Bajaj.CAR)
plot(Bajaj$Bajaj.CAR[1:38])
plot(Bajaj$Bajaj.CAR[39:83])

Bajaj.EP <- Bajaj[1:38,] #Creating a subset for the Estimation Period
Bajaj.EW<- Bajaj[39:83,] #Creating a subset for the Event Window


#ICICI Bank Limited:
#Calculating Return
ICICI$ICICI.Return <- (ICICI$ICICI.Close-ICICI$ICICI.Open)/ICICI$ICICI.Open
ICICI$MKT.Return <- (INMKT$Return)

#Calculating Abnormal Return and Cumulative Abnormal Return
ICICI$ICICI.AR <- ICICI$ICICI.Return-ICICI$MKT.Return
ICICI$ICICI.CAR <- cumsum(ICICI$ICICI.AR)

#Plot Graphs
ICICI$ICICI.DateNum <- 1:nrow(ICICI) 

plot(ICICI$ICICI.Return)
hist(ICICI$ICICI.Return)

plot(ICICI$ICICI.CAR)
plot(ICICI$ICICI.CAR[1:38])
plot(ICICI$ICICI.CAR[39:83])

ICICI.EP <- ICICI[1:38,] #Creating a subset for the Estimation Period
ICICI.EW<- ICICI[39:83,] #Creating a subset for the Event Window


#Larsen & Toubro Limited:
#Calculating Return
LT$LT.Return <- (LT$LT.Close-LT$LT.Open)/LT$LT.Open
LT$MKT.Return <- (INMKT$Return)

#Calculating Abnormal Return and Cumulative Abnormal Return
LT$LT.AR <- LT$LT.Return-LT$MKT.Return
LT$LT.CAR <- cumsum(LT$LT.AR)

#Plot Graphs
LT$LT.DateNum <- 1:nrow(LT) 

plot(LT$LT.Return)
hist(LT$LT.Return)

plot(LT$LT.CAR)
plot(LT$LT.CAR[1:38])
plot(LT$LT.CAR[39:83])

LT.EP <- LT[1:38,] #Creating a subset for the Estimation Period
LT.EW<- LT[39:83,] #Creating a subset for the Event Window

