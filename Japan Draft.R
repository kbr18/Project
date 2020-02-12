# 1. Data Source
# Get quantmod
if (!require("quantmod")) {
  install.packages("quantmod")
  library(quantmod)
}

startJP <- as.Date("2018-05-09")
endJP <- as.Date("2018-09-09")

#Amada:
symbolBasket1 <- c('6113.T') #Amada Holdings Co. 
getSymbols(symbolBasket1 , src='yahoo', from = startJP, to = endJP)
summary('6113.T')

Amada <- as.xts(`6113.T`)
names(Amada)
names(Amada) <- c("Amada.Open"   ,  "Amada.High"   ,  "Amada.Low"   ,   "Amada.Close"  ,  "Amada.Volume",  "Amada.Adjusted")
names(Amada)
View(Amada)

#Hokuetsu Corporation:
symbolBasket2 <- c('3865.T') #Hokuetsu Corporation. 
getSymbols(symbolBasket2 , src='yahoo', from = startJP, to = endJP)
summary('3865.T')

Hokuetsu <- as.xts(`3865.T`)
names(Hokuetsu)
names(Hokuetsu) <- c("Hokuetsu.Open"   ,  "Hokuetsu.High"   ,  "Hokuetsu.Low"   ,   "Hokuetsu.Close"  ,  "Hokuetsu.Volume",  "Hokuetsu.Adjusted")
names(Hokuetsu)
View(Hokuetsu)

#Mitsubishi Motors:
symbolBasket3 <- c('7211.T') #Mitsubishi Motors 
getSymbols(symbolBasket3 , src='yahoo', from = startJP, to = endJP)
summary('7211.T')

Mitsubishi <- as.xts(`7211.T`)
names(Mitsubishi)
names(Mitsubishi) <- c("Mitsubishi.Open"   ,  "Mitsubishi.High"   ,  "Mitsubishi.Low"   ,   "Mitsubishi.Close"  ,  "Mitsubishi.Volume",  "Mitsubishi.Adjusted")
names(Mitsubishi)
View(Mitsubishi)

#Secom:
symbolBasket4 <- c('9735.T') #Secom 
getSymbols(symbolBasket4 , src='yahoo', from = startJP, to = endJP)
summary('9735.T')

Secom <- as.xts(`9735.T`)
names(Secom)
names(Secom) <- c("Secom.Open"   ,  "Secom.High"   ,  "Secom.Low"   ,   "Secom.Close"  ,  "Secom.Volume",  "Secom.Adjusted")
names(Secom)
View(Secom)

#Tokyu Fudosan Holdings Corporation:
symbolBasket5 <- c('3289.T') #Tokyu 
getSymbols(symbolBasket5 , src='yahoo', from = startJP, to = endJP)
summary('3289.T')

Tokyu <- as.xts(`3289.T`)
names(Tokyu)
names(Tokyu) <- c("Tokyu.Open"   ,  "Tokyu.High"   ,  "Tokyu.Low"   ,   "Tokyu.Close"  ,  "Tokyu.Volume",  "Tokyu.Adjusted")
names(Tokyu)
View(Tokyu)


#2. Calculations

#Market Data
JPMKT <- read.csv("JP MKT.csv")

JPMKT$Return <- (JPMKT$Close-JPMKT$Open)/JPMKT$Open

#Stock Data

#Amada:
#Calculating Return
Amada$Amada.Return <- (Amada$Amada.Close-Amada$Amada.Open)/Amada$Amada.Open
Amada$MKT.Return <- (JPMKT$Return)

#Calculating Abnormal Return and Cumulative Abnormal Return
Amada$Amada.AR <- Amada$Amada.Return-Amada$MKT.Return
Amada$Amada.CAR <- cumsum(Amada$Amada.AR)

#Plot Graphs
Amada$Amada.DateNum <- 1:nrow(Amada) 

plot(Amada$Amada.Return)
hist(Amada$Amada.Return)

plot(Amada$Amada.CAR)
plot(Amada$Amada.CAR[1:38])
plot(Amada$Amada.CAR[39:88])

Amada.EP <- Amada[1:38,] #Creating a subset for the Estimation Period, from 9th May to 29th June
Amada.EW<- Amada[39:88,] #Creating a subset for the Event Window, from 2nd July to 7th September


#Hokuetsu Corporation:
#Calculating Return
Hokuetsu$Hokuetsu.Return <- (Hokuetsu$Hokuetsu.Close-Hokuetsu$Hokuetsu.Open)/Hokuetsu$Hokuetsu.Open
Hokuetsu$MKT.Return <- (JPMKT$Return)

#Calculating Abnormal Return and Cumulative Abnormal Return
Hokuetsu$Hokuetsu.AR <- Hokuetsu$Hokuetsu.Return-Hokuetsu$MKT.Return
Hokuetsu$Hokuetsu.CAR <- cumsum(Hokuetsu$Hokuetsu.AR)

#Plot Graphs
Hokuetsu$Hokuetsu.DateNum <- 1:nrow(Hokuetsu) 

plot(Hokuetsu$Hokuetsu.Return)
hist(Hokuetsu$Hokuetsu.Return)

plot(Hokuetsu$Hokuetsu.CAR)
plot(Hokuetsu$Hokuetsu.CAR[1:38])
plot(Hokuetsu$Hokuetsu.CAR[39:88])

Hokuetsu.EP <- Hokuetsu[1:38,] #Creating a subset for the Estimation Period, from 9th May to 29th June
Hokuetsu.EW<- Hokuetsu[39:88,] #Creating a subset for the Event Window, from 2nd July to 7th September


#Mitsubishi Motors:
#Calculating Return
Mitsubishi$Mitsubishi.Return <- (Mitsubishi$Mitsubishi.Close-Mitsubishi$Mitsubishi.Open)/Mitsubishi$Mitsubishi.Open
Mitsubishi$MKT.Return <- (JPMKT$Return)

#Calculating Abnormal Return and Cumulative Abnormal Return
Mitsubishi$Mitsubishi.AR <- Mitsubishi$Mitsubishi.Return-Mitsubishi$MKT.Return
Mitsubishi$Mitsubishi.CAR <- cumsum(Mitsubishi$Mitsubishi.AR)

#Plot Graphs
Mitsubishi$Mitsubishi.DateNum <- 1:nrow(Mitsubishi) 

plot(Mitsubishi$Mitsubishi.Return)
hist(Mitsubishi$Mitsubishi.Return)

plot(Mitsubishi$Mitsubishi.CAR)
plot(Mitsubishi$Mitsubishi.CAR[1:38])
plot(Mitsubishi$Mitsubishi.CAR[39:88])

Mitsubishi.EP <- Mitsubishi[1:38,] #Creating a subset for the Estimation Period, from 9th May to 29th June
Mitsubishi.EW<- Mitsubishi[39:88,] #Creating a subset for the Event Window, from 2nd July to 7th September


#Secom:
#Calculating Return
Secom$Secom.Return <- (Secom$Secom.Close-Secom$Secom.Open)/Secom$Secom.Open
Secom$MKT.Return <- (JPMKT$Return)

#Calculating Abnormal Return and Cumulative Abnormal Return
Secom$Secom.AR <- Secom$Secom.Return-Secom$MKT.Return
Secom$Secom.CAR <- cumsum(Secom$Secom.AR)

#Plot Graphs
Secom$Secom.DateNum <- 1:nrow(Secom) 

plot(Secom$Secom.Return)
hist(Secom$Secom.Return)

plot(Secom$Secom.CAR)
plot(Secom$Secom.CAR[1:38])
plot(Secom$Secom.CAR[39:88])

Secom.EP <- Secom[1:38,] #Creating a subset for the Estimation Period, from 9th May to 29th June
Secom.EW<- Secom[39:88,] #Creating a subset for the Event Window, from 2nd July to 7th September


#Tokyu Fudosan Holdings Corporation:
#Calculating Return
Tokyu$Tokyu.Return <- (Tokyu$Tokyu.Close-Tokyu$Tokyu.Open)/Tokyu$Tokyu.Open
Tokyu$MKT.Return <- (JPMKT$Return)

#Calculating Abnormal Return and Cumulative Abnormal Return
Tokyu$Tokyu.AR <- Tokyu$Tokyu.Return-Tokyu$MKT.Return
Tokyu$Tokyu.CAR <- cumsum(Tokyu$Tokyu.AR)

#Plot Graphs
Tokyu$STokyu.DateNum <- 1:nrow(Secom) 

plot(Tokyu$Tokyu.Return)
hist(Tokyu$Tokyu.Return)

plot(Tokyu$Tokyu.CAR)
plot(Tokyu$Tokyu.CAR[1:38])
plot(Tokyu$Tokyu.CAR[39:88])

Tokyu.EP <- Tokyu[1:38,] #Creating a subset for the Estimation Period, from 9th May to 29th June
Tokyu.EW<- Tokyu[39:88,] #Creating a subset for the Event Window, from 2nd July to 7th September



#Tree
#WORKING ON TREE BELOW

#Regression trees
#Linear regression specifies an explicit model, which is linear in coef-
#ficients, for the regression function. It works well when the model is
#about correct. When the true function is highly nonlinear, a tree model 
#may provide a valid alternative.

#build a tree for predicting a continuous variable, a method called 
#regression analysis

#Basic regression trees partition a data set into smaller groups and then 
#fit a simple model (constant) for each subgroup. Unfortunately, a single 
#tree model tends to be highly unstable and a poor predictor. However, by 
#bootstrap aggregating (bagging) regression trees, this technique can become 
#quite powerful and effective

install.packages("tree")
install.packages("ISLR")
library(tree)
library(ISLR)
# Load CART packages
library(rpart)
# install rpart package
install.packages("rpart.plot")
library(rpart.plot)

#Chapter 4 tree code

library(MASS); library(tree)
train=sample(1:nrow(Amada), 70)
dim(Amada)
names(Amada)
tree.AmadaCAR=tree(Amada.CAR~., data=Amada, subset=train)
summary(tree.AmadaCAR)
plot(tree.AmadaCAR, col="blue")
text(tree.AmadaCAR, pretty=0)
#Mean RSS for the training data is 0.0001682

medv.test=Amada[-train, "Amada.CAR"]
medv.predict=predict(tree.AmadaCAR, newdata=Amada[-train,])
mean((medv.predict-medv.test)^2)

#The mean squares of predictive errors is 0.00027096 for the testing sample,
#which is greater than 0.0001682 for the training sample.

prune.tree(tree.AmadaCAR, best=4)
?prune.tree

#ANOTHER REGRESSION?

# Create training (80%) and test (20%) sets for the Amada data.
# Use set.seed for reproducibility

library(rsample)     # data splitting 
library(dplyr)       # data wrangling
library(rpart)       # performing regression trees
library(rpart.plot)  # plotting regression trees
library(ipred)       # bagging
library(caret)       # bagging

set.seed(123)
Amada_split <- initial_split(Amada, prop = 0.8)
Amada_train <- training(Amada_split)
Amada_test  <- testing(Amada_split)

tree1 <- rpart(formula=Amada.CAR ~.,data= Amada_train, method = "anova")
tree1
rpart.plot(tree1)
plotcp(tree1)

tree2 <- rpart(formula=Amada.CAR ~ .,data= Amada_train, method  = "anova", control = list(cp = 0, xval = 10))
plotcp(tree2)
abline(v = 12, lty = "dashed")

tree3 <- rpart(formula=Amada.CAR ~ .,data= Amada_train, method  = "anova", control = list(minsplit = 10, maxdepth = 12, xval = 10))
plotcp(tree3)

optimal_tree <- rpart(formula=Amada.CAR ~ .,data= Amada_train, method  = "anova", control = list(minsplit = 11, maxdepth = 8, cp = 0.01))
plotcp(optimal_tree)

pred <- predict(optimal_tree, newdata = Amada_test)
RMSE(pred = pred, obs = Amada_test$Amada.CAR)

#The final RMSE is 0.01316045 which suggests that, on average, our predicted CAR is about 0.01316045 off from the actual CAR.

#ANOTHER ANOTHER TREE????

# Grow tree
fit <- rpart(Amada.CAR~ Amada.Open + Amada.High + Amada.Low + Amada.Close + Amada.Volume + Amada.Adjusted + Amada.Return + MKT.Return + Amada.AR, method="anova", data=Amada)

prp(fit) 

#In regression trees, we  predict the number.
#That number here is the average of the median CAR in that bucket.

plot(fit)
text(fit)
summary(fit)

printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits

# create additional plots
par(mfrow=c(1,2)) # two plots on one page
rsq.rpart(fit) # visualize cross-validation results  

# plot tree
plot(fit, uniform=TRUE, main="Regression Tree for CAR")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# prune the tree
pfit<- prune(fit, cp=0.01160389) # from cptable   

# plot the pruned tree
plot(pfit, uniform=TRUE, main="Pruned Regression Tree for CAR")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)





