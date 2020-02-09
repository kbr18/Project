# 1. Data Source
# Get quantmod
if (!require("quantmod")) {
  install.packages("quantmod")
  library(quantmod)
}

start <- as.Date("2018-05-09")
end <- as.Date("2018-09-09")

#Amada:

symbolBasket1 <- c('6113.T') #Amada Holdings Co. 
getSymbols(symbolBasket1 , src='yahoo', from = start, to = end)
summary('6113.T')

Amada <- as.xts(`6113.T`)
names(Amada)
names(Amada) <- c("Amada.Open"   ,  "Amada.High"   ,  "Amada.Low"   ,   "Amada.Close"  ,  "Amada.Volume",  "Amada.Adjusted")
names(Amada)
View(Amada)

#Hokuetsu Corporation:

symbolBasket2 <- c('3865.T') #Hokuetsu Corporation. 
getSymbols(symbolBasket2 , src='yahoo', from = start, to = end)
summary('3865.T')

Hokuetsu <- as.xts(`3865.T`)
names(Hokuetsu)
names(Hokuetsu) <- c("Hokuetsu.Open"   ,  "Hokuetsu.High"   ,  "Hokuetsu.Low"   ,   "Hokuetsu.Close"  ,  "Hokuetsu.Volume",  "Hokuetsu.Adjusted")
names(Hokuetsu)
View(Hokuetsu)

#Mitsubishi Motors:

symbolBasket3 <- c('7211.T') #Mitsubishi Motors 
getSymbols(symbolBasket3 , src='yahoo', from = start, to = end)
summary('7211.T')

Mitsubishi <- as.xts(`7211.T`)
names(Mitsubishi)
names(Mitsubishi) <- c("Mitsubishi.Open"   ,  "Mitsubishi.High"   ,  "Mitsubishi.Low"   ,   "Mitsubishi.Close"  ,  "Mitsubishi.Volume",  "Mitsubishi.Adjusted")
names(Mitsubishi)
View(Mitsubishi)

#Secom:

symbolBasket4 <- c('9735.T') #Secom 
getSymbols(symbolBasket4 , src='yahoo', from = start, to = end)
summary('9735.T')

Secom <- as.xts(`9735.T`)
names(Secom)
names(Secom) <- c("Secom.Open"   ,  "Secom.High"   ,  "Secom.Low"   ,   "Secom.Close"  ,  "Secom.Volume",  "Secom.Adjusted")
names(Secom)
View(Secom)

#Tokyu Fudosan Holdings Corporation:

symbolBasket5 <- c('3289.T') #Tokyu 
getSymbols(symbolBasket5 , src='yahoo', from = start, to = end)
summary('3289.T')

Tokyu <- as.xts(`3289.T`)
names(Tokyu)
names(Tokyu) <- c("Tokyu.Open"   ,  "Tokyu.High"   ,  "Tokyu.Low"   ,   "Tokyu.Close"  ,  "Tokyu.Volume",  "Tokyu.Adjusted")
names(Tokyu)
View(Tokyu)



#2. Calculations

#Market Data

JPRF <- read.csv("JP RF.csv")
JPMKT <- read.csv("JP MKT.csv")

JPRF$Return <- (JPRF$Close-JPRF$Open)/JPRF$Open
JPMKT$Return <- (JPMKT$Close-JPMKT$Open)/JPMKT$Open

#Stock Data

Amada$Amada.Return <- (Amada$Amada.Close-Amada$Amada.Open)/Amada$Amada.Open
Amada$RF.Return <- (JPRF$Return)
Amada$MKT.Return <- (JPMKT$Return)

Hokuetsu$Hokuetsu.Return <- (Hokuetsu$Hokuetsu.Close-Hokuetsu$Hokuetsu.Open)/Hokuetsu$Hokuetsu.Open
Hokuetsu$RF.Return <- (JPRF$Return)
Hokuetsu$MKT.Return <- (JPMKT$Return)

Mitsubishi$Mitsubishi.Return <- (Mitsubishi$Mitsubishi.Close-Mitsubishi$Mitsubishi.Open)/Mitsubishi$Mitsubishi.Open
Mitsubishi$RF.Return <- (JPRF$Return)
Mitsubishi$MKT.Return <- (JPMKT$Return)

Secom$Secom.Return <- (Secom$Secom.Close-Secom$Secom.Open)/Secom$Secom.Open
Secom$RF.Return <- (JPRF$Return)
Secom$MKT.Return <- (JPMKT$Return)

Tokyu$Tokyu.Return <- (Tokyu$Tokyu.Close-Tokyu$Tokyu.Open)/Tokyu$Tokyu.Open
Tokyu$RF.Return <- (JPRF$Return)
Tokyu$MKT.Return <- (JPMKT$Return)

# Calculating Abnormal Return

Amada$Amada.AR <- Amada$Amada.Return-Amada$MKT.Return


# Calculating Cumulative Abnormal Return

Amada$Amada.CAR <- cumsum(Amada$Amada.AR)


# Plot CAR
Amada$Amada.DateNum <- 1:nrow(Amada) 
plot(Amada.CAR)
plot(Amada$Amada.Return)
Amada.EP <- Amada[1:38,] #Creating a subset from the risk-free data, i.e. our Estimation Period, from 9th May to 29th June
Amada.EW<- Amada[39:88,] #Creating a subset from the risk-free data, i.e. our Event Window, from 2nd July to 7th September

plot(Amada.CAR[1:38])
plot(Amada.CAR[39:88])

logisticRegression=glm(Amada.CAR~Amada$Amada.Open+Amada$Amada.High+Amada$Amada.Low+Amada$Amada.Volume+Amada$Amada.Adjusted+Amada$Amada.Return, data=Amada.CAR,family=binomial)

Regression <- glm(Amada.CAR ~ MRP, data=Amada.CAR[1:38])

summary(Amada$Amada.CAR)

#Tree time

install.packages("tree")
install.packages("ISLR")
library(tree)
library(ISLR)

High=ifelse(Amada.Return<=0.004384, "No", "Yes")
Amada2=data.frame(Amada, High)
tree.Amada=tree(High~.-Amada.Return, Amada2)
summary(tree.Amada)

Event=ifelse(Amada.CAR<-0.10, "Yes", "No")
Amada2=data.frame(Amada, Event)
tree.Amada=tree(Event~.-Amada.CAR, Amada2)
summary(tree.Amada)

plot(Amada.CAR[39:50])

# Trees