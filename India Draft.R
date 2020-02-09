# 1. Data Source
# Get quantmod
if (!require("quantmod")) {
  install.packages("quantmod")
  library(quantmod)
}

startIN <- as.Date("2018-06-18")
endIN <- as.Date("2018-10-16")

#Infosys Limited:

symbolBasket6 <- c('INFY.BO') #Infosys Limited
getSymbols(symbolBasket6 , src='yahoo', from = startIN, to = endIN)
summary('INFY.BO')

Infosys <- as.xts(`500209`)
names(Infosys)
names(Infosys) <- c("Infosys.Open"   ,  "Infosys.High"   ,  "Infosys.Low"   ,   "Infosys.Close"  ,  "Infosys.Volume",  "Infosys.Adjusted")
names(Infosys)
View(Infosys)



#2. Calculations

#Market Data

JPRF <- read.csv("JP RF.csv")
JPMKT <- read.csv("JP MKT.csv")

JPRF$Return <- (JPRF$Close-JPRF$Open)/JPRF$Open
JPMKT$Return <- (JPMKT$Close-JPMKT$Open)/JPMKT$Open

#Stock Data

#Amada:
#Calculating Return
Amada$Amada.Return <- (Amada$Amada.Close-Amada$Amada.Open)/Amada$Amada.Open
Amada$RF.Return <- (JPRF$Return)
Amada$MKT.Return <- (JPMKT$Return)

#Calculating Abnormal Return and Cumulative Abnormal Return
Amada$Amada.AR <- Amada$Amada.Return-Amada$MKT.Return
Amada$Amada.CAR <- cumsum(Amada$Amada.AR)

#Plot Graphs
Amada$Amada.DateNum <- 1:nrow(Amada) 

plot(Amada$Amada.Return)

plot(Amada$Amada.CAR)
plot(Amada$Amada.CAR[1:38])
plot(Amada$Amada.CAR[39:88])

Amada.EP <- Amada[1:38,] #Creating a subset for the Estimation Period, from 9th May to 29th June
Amada.EW<- Amada[39:88,] #Creating a subset for the Event Window, from 2nd July to 7th September
