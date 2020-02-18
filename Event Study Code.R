install.packages("EventStudy")
library(EventStudy)
install.packages("quantmod")
library(quantmod)


#Step 1: List the assets and the date of the event 
eventsDates<-data.frame("name"=c("Amada","Hokuetsu","Mitsubishi", "Secom", "Tokyu"), "when"=c("2018-07-09","2018-07-09","2018-07-09","2018-07-09","2018-07-09")) 

#Step 2: Convert the text 
eventsDates$name<-as.character(eventsDates$name) 
eventsDates$when<-as.character(eventsDates$when) 

#Step 3: Return Lists 
BVSP<- read.csv("^BVSP.csv") 
BVSP<-BVSP[,c(1,6)] colnames(BVSP)<-c("Date","BVSP") 
GAFISA<- read.csv("GFSA3.SA.csv") 
GAFISA<-GAFISA[,c(1,6)] 
colnames(GAFISA)<-c("Date","GAFISA") 
CYRELA<- read.csv("CYRE3.SA.csv") 
CYRELA<-CYRELA[,c(1,6)] 
colnames(CYRELA)<-c("Date","CYRELA") 

#Step 4: Merge the data bases 
data<-merge(BVSP,GAFISA,by="Date",all=T) 
data<-merge(dados,CYRELA,by="Date",all=T) 

#Step 5: Calculate the return 
data$BVSP <- c(NA,diff(log(as.numeric(data$BVSP)), lag=1)) 
data$GAFISA <- c(NA,diff(log(as.numeric(data$GAFISA)), lag=1)) 
data$CYRELA <- c(NA,diff(log(as.numeric(data$CYRELA)), lag=1)) 

#Step 6: Convert into zoo objects 
data.zoo<-read.zoo(data) 

#Step 7: Event Study Market Model 
es.mm <- eventstudy(firm.returns = data.zoo, #Returns base 
                    event.list = eventsDatas, #Event Dates 
                    event.window = 5, #Safety Window 
                    size type = "marketModel", #Abnormal return model 
                    to.remap = TRUE, #Recalculate the return using cumsum 
                    remap = "cumsum", #Accumulative return 
                    inference = TRUE, #Inference of the event 
                    inference.strategy = "bootstrap", #Boostrap to standert error 
                    model.args = list(market.returns=dados$BVSP)) #Data base with the index IBOVESPA 

#Step 8: Plot event study 
plot(es.mm) 

#Step 9: Results 
summary(es.mm)