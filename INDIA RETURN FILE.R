```{r eval=FALSE, include=TRUE}
startIN <- as.Date("2018-07-16")
endIN <- as.Date("2018-09-16")

symbolBasketIN <- c("TATASTEEL.BO", "TECHM.BO", "BAJAJ-AUTO.BO", "ICICIBANK.BO", "LT.BO")

getSymbols(symbolBasketIN, src='yahoo', from = startIN, to = endIN)

Tata <- as.xts(`TATASTEEL.BO`)
names(Tata) <- c("Tata.Open"   ,  "Tata.High"   ,  "Tata.Low"   ,   "Tata.Close"  ,  "Tata.Volume",  "Tata.Adjusted")

Mahindra <- as.xts(`TECHM.BO`)
names(Mahindra) <- c("Mahindra.Open"   ,  "Mahindra.High"   ,  "Mahindra.Low"   ,   "Mahindra.Close"  ,  "Mahindra.Volume",  "Mahindra.Adjusted")

Bajaj <- as.xts(`BAJAJ-AUTO.BO`)
names(Bajaj) <- c("Bajaj.Open"   ,  "Bajaj.High"   ,  "Bajaj.Low"   ,   "Bajaj.Close"  ,  "Bajaj.Volume",  "Bajaj.Adjusted")

ICICI <- as.xts(`ICICIBANK.BO`)
names(ICICI) <- c("ICICI.Open"   ,  "ICICI.High"   ,  "ICICI.Low"   ,   "ICICI.Close"  ,  "ICICI.Volume",  "ICICI.Adjusted")

LT <- as.xts(`LT.BO`)
names(LT) <- c("LT.Open"   ,  "LT.High"   ,  "LT.Low"   ,   "LT.Close"  ,  "LT.Volume",  "LT.Adjusted")


data<-merge(Tata,Mahindra, Bajaj, ICICI, LT)
dataIN <-data[,c(4, 10, 16, 22, 28)]

View(dataIN)
TataClose <- dataIN[,1]
MahindraClose <-dataIN[,2]
BajajClose <- dataIN[,3]
ICICIClose <- dataIN[,4]
LTClose  <- dataIN[,5]

View(dataJP)
AmadaClose <- dataJP[,1]
HokuetsuClose <-dataJP[,2]
MitsubishiClose <- dataJP[,3]
SecomClose <- dataJP[,4]
TokyoClose  <- dataJP[,5]




