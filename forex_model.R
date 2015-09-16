##install base packages
install.packages("forecast")
install.packages("caret")
install.packages("TTR")
install.packages("mc2d")
install.packages("Quandl")
install.packages("devtools")
install_github("quandl/R-package")
install.packages("stringi")
install.packages("MCMCpack")
install.packages("quantmod")
install.packages("PerformanceAnalytics")
install.packages("coda")
install.packages("xlsx")
install.packages("tsDyn")
library(tsDyn)
library(xlsx)
#load base packages
library(coda)
library(MCMCpack)
library(TTR)
library(caret)
library(forecast)
library("mc2d")
library(devtools)
library(Quandl)
library(quantmod)
library(PerformanceAnalytics)
require(quantmod)
require(PerformanceAnalytics)

##load api key##
Quandl.auth("TGvCqLsEZx6fqMuqAcJw")

##############bring in Bitcoin data/models#################
##6 years of bitcoin closing bitcoin values
Bitcoin<-scan("BIT.csv")
Bitcoin
BitcoinTS<-ts(Bitcoin, frequency=365, start=c(2009,3))

##bring in data pipe##traditional currencies#################################################
YEN<-Quandl("CURRFX/USDJPY", start_date="2010-01-01", end_date="2015-12-31", type="zoo")
GBP<-Quandl("CURRFX/USDGBP", start_date="2010-01-01", end_date="2015-12-31", type="zoo")
EUR<-Quandl("CURRFX/USDEUR", start_date="2010-01-01", end_date="2015-12-31", type="zoo")
CAD<-Quandl("CURRFX/USDCAD", start_date="2010-01-01", end_date="2015-12-31", type="zoo")
AUD<-Quandl("CURRFX/USDAUD", start_date="2010-01-01", end_date="2015-12-31", type="zoo")

##create data frame to call from###

EURTS<-ts(EUR$Rate,frequency=365, start=c(2010,1))
JPYTS<-ts(YEN$Rate,frequency=365, start=c(2010,1))
CADTS<-ts(CAD$Rate,frequency=365, start=c(2010,1))
AUDTS<-ts(AUD$Rate,frequency=365, start=c(2010,1))
GBPTS<-ts(GBP$Rate,frequency=365, start=c(2010,1))
fx<-data.frame(YEN$Rate, GBP$Rate, EUR$Rate, CAD$Rate, AUD$Rate)

########################basic plots##########################
fxts<-ts(fx)
fxts2<-data.frame(GBP$Rate, EUR$Rate, CAD$Rate, AUD$Rate)
fxts3<-ts(fxts2)
ts.plot(fxts3)

chart.fxts<-as.zoo(fxts3)
fxtsrainbow<-rainbow(ncol(chart.fxts))
plot(x=chart.fxts, ylab="Daily Value", main="FOREX Values", col=fxtsrainbow, screens=1)
legend(x="topleft", legend=c("GBP","EUR", "CAD", "AUD"), lty=1, col=fxtsrainbow)

plot(Bitcoin, col="lightgreen")
plot.ts(BitcoinTS, col="red", ylab="Bitcoin Values", main="BITCOIN")
mean(Bitcoin)
sd(Bitcoin)

###time series linear model#############################
BIT <- tslm(BitcoinTS ~ trend + season)
BITcast<-forecast.lm(BIT, h=100)
BITcast
BITcastplot<-plot(forecast(BITcast))
BITcastplot
summary(BIT)
summary(BITcast)
BITX<-data.frame(BITcast)
BITtslm<-(BITX$Point.Forecast)
BITtslm

EUR1 <- tslm(EURTS ~ trend + season)
EURcast<-forecast.lm(EUR1, h=100)
EURcast
EURcastplot<-plot(forecast(EURcast))
EURcastplot
summary(EUR1)
summary(EURcast)
plot(decompose(EURTS))
EURX<-data.frame(EURcast)
EURtslm<-(EURX$Point.Forecast)
EURtslm

JPY1<-tslm(JPYTS ~ trend + season)
JPYcast<-forecast.lm(JPY1, h=100)
JPYcast
JPYcastplot<-plot(forecast(JPYcast))
JPYcastplot
summary(JPY1)
summary(JPYcast)
plot(decompose(JPYTS))
JPYX<-data.frame(JPYcast)
JPYtslm<-(JPYX$Point.Forecast)
JPYtslm  

CAD1<-tslm(CADTS ~ trend + season)
CADcast<-forecast.lm(CAD1, h=100)
CADcast
CADcastplot<-plot(forecast(CADcast))
CADcastplot
summary(CAD1)
summary(CADcast)
plot(decompose(CADTS),xlab ="Canadian Dollar")
CADX<-data.frame(CADcast)
CADtslm<-(CADX$Point.Forecast)
CADtslm   

AUD1<-tslm(AUDTS ~ trend + season)
AUDcast<-forecast.lm(AUD1, h=100)
AUDcast
AUDcastplot<-plot(forecast(AUDcast))
AUDcastplot
summary(AUD1)
summary(AUDcast)
plot(decompose(AUDTS),xlab ="Aussie")
AUDX<-data.frame(AUDcast)
AUDtslm<-(AUDX$Point.Forecast)
AUDtslm  

GBP1<-tslm(GBPTS ~ trend + season)
GBPcast<-forecast.lm(GBP1, h=100)
GBPcast
GBPcastplot<-plot(forecast(GBPcast))
GBPcastplot
summary(GBP1)
summary(GBPcast)
plot(decompose(GBPTS),xlab ="Pound")
GBPX<-data.frame(GBPcast)
GBPtslm<-(GBPX$Point.Forecast)
GBPtslm   

##########Neural Net trend indicator model
GBPNN<-nnetTs(GBPTS,m=1, size=1)
GBPNNPred<-predict(GBPNN, n.ahead=100)
GBPNNPred
plot.ts(GBPNNPred)
GBPX1<-data.frame(GBPNNPred)
GBPX1
 
EURNN<-nnetTs(EURTS,m=1, size=1)
EURNNPred<-predict(EURNN, n.ahead=100)
EURNNPred
plot.ts(EURNNPred)
EURX1<-data.frame(EURNNPred)
EURX1
head(EURTS)

JPYNN<-nnetTs(JPYTS,m=1, size=1)
JPYNNPred<-predict(JPYNN, n.ahead=100)
JPYNNPred
plot.ts(JPYNNPred)
JPYX1<-data.frame(JPYNNPred)
JPYX1

CADTSNN<-nnetTs(CADTS,m=1, size=1)
CADTSNNPred<-predict(CADTSNN, n.ahead=100)
CADTSNNPred
plot.ts(CADTSNNPred)
CADX1<-data.frame(CADTSNNPred)
CADX1

AUDTSNN<-nnetTs(AUDTS,m=1, size=1)
AUDTSNNPred<-predict(AUDTSNN, n.ahead=100)
AUDTSNNPred
plot.ts(AUDTSNNPred)
AUDX1<-data.frame(AUDTSNNPred)
AUDX1

BitNN<-nnetTs(BitcoinTS,m=5, size=5)
BitNNPred<-predict(BitNN, n.ahead=100)
BitNNPred
BitX1<-data.frame(BitNNPred)
BitX1



#######################################stl model#####################################
EURstl<-stlf(EURTS,h=100, t.window=15, s.window="periodic", robust=TRUE)
plot(EURstl, col="blue")
EURstldf<-data.frame(EURstl)
EURstldf
plot.forecast(EURstl, main="EURO forecast", col="orange")

JPYstl<-stlf(JPYTS,h=100, t.window=15, s.window="periodic", robust=TRUE)
plot(JPYstl, col="blue")
JPYstldf<-data.frame(JPYstl)
JPYstldf1<-(JPYstldf$Point.Forecast)
JPYstldf1
plot.forecast(JPYstl, main="YEN forecast", col="orange")

GBPstl<-stlf(GBPTS,h=100, t.window=15, s.window="periodic", robust=TRUE)
plot(GBPstl, col="blue")
GBPstldf<-data.frame(GBPstl)
head(GBPstldf)
plot.forecast(GBPstl, main="GBP forecast", col="orange")

BITstl<-stlf(BitcoinTS,h=100, t.window=15, s.window="periodic", robust=TRUE)
plot(BITstl, col="blue")
BITstldf<-data.frame(BITstl)
BITstldf
plot.forecast(BITstl, main="Bitcoin", col="orange")

CADstl<-stlf(GBPTS,h=100, t.window=15, s.window="periodic", robust=TRUE)
plot(CADstl, col="blue")
CADstldf<-data.frame(CADstl)
head(CADstldf)
plot.forecast(CADstl, main="GBP forecast", col="orange")

AUDstl<-stlf(AUDTS,h=100, t.window=15, s.window="periodic", robust=TRUE)
plot(AUDstl, col="blue")
AUDstldf<-data.frame(AUDstl)
head(AUDstldf)
plot.forecast(AUDstl, main="GBP forecast", col="orange")

stldf<-data.frame(EURstldf$Point.Forecast,JPYstldf$Point.Forecast,GBPstldf$Point.Forecast)
colnames(stldf)<-c("Euro", "Yen", "GBP")
write.xlsx(c(stldf),"\\\\deltafile01/DeltaUsers/001VIR/NonVABeach/jason.pfaff/My Documents/mydata.xlsx")

stageoneframe<-data.frame(BITtslm, EURtslm,JPYtslm, AUDtslm, CADtslm, GBPtslm, GBPX1, EURX1, JPYX1, AUDX1, CADX1, BitX1, EURstldf, GBPstldf, JPYstldf, AUDstldf, CADstldf, BITstldf)
colnames(stageoneframe)<-c("BITtslm", "Eurotslm", "Yentslm", "AUDtslm", "CADtslm", "GBPtslm", "GBPnnet", "Euronnet", "JPYnnet", "AUDnnet", "CADnnet", "Bitnnet", "EURstl", "GBPstl", "JPYstl", "AUDstl", "CADstl", "BITstl")
write.xlsx(c(stageoneframe),"\\\\deltafile01/DeltaUsers/001VIR/NonVABeach/jason.pfaff/My Documents/mydata.xlsx")

BITtslm
EURtslm
JPYtslm
AUDtslm
CADtslm
GBPtslm
GBPX1
EURX1
JPYX1
AUDX1
CADX1
BitX1
EURstldf
GBPstldf
JPYstldf
AUDstldf
CADstldf
BITstldf



###############################stlmodel##########################################

################################Core arima model########################

GBPdiff1<-diff(GBPTS, differences=1)
GBPtimeseriesarima<-arima(GBPTS, order=c(1,0,1))
GBPtimeseriesarima
GBPtimeseriesforecasts<-forecast.Arima(GBPtimeseriesarima, h=100)
GBPtimeseriesforecasts
plot(GBPtimeseriesforecasts, main="GBP Forecast")
acf(GBPdiff1, lag.max=20, main="Lag of model differences")
acf(GBPdiff1, lag.max=20, plot=FALSE)
plot.ts(GBPdiff1, main="Plot of Differeneces")
plot.forecast(GBPtimeseriesforecasts)

EURTSdiff1<-diff(EURTS, differences=1)
EURTStimeseriesarima<-arima(EURTS, order=c(1,0,1))
EURTStimeseriesarima
EURTStimeseriesforecasts<-forecast.Arima(EURTStimeseriesarima, h=100)
EURTStimeseriesforecasts
acf(EURTSdiff1, lag.max=20, main="Lag of model differences")
acf(EURTSdiff1, lag.max=20, plot=FALSE)
plot.ts(EURTSdiff1, main="Plot of Differeneces")
plot.forecast(EURTStimeseriesforecasts)

CADTSdiff1<-diff(CADTS, differences=1)
EURTStimeseriesarima<-arima(EURTS, order=c(1,0,1))
EURTStimeseriesarima
EURTStimeseriesforecasts<-forecast.Arima(EURTStimeseriesarima, h=100)
EURTStimeseriesforecasts
plot(EURTStimeseriesforecasts, main="EURO Forecast")
CADTStimeseriesarima<-auto.arima(CADTS)
CADTStimeseriesarima
CADTStimeseriesforecasts<-forecast.Arima(CADTStimeseriesarima, h=100)
CADTStimeseriesforecasts
plot(CADTStimeseriesforecasts, main="Canadian Dollar Forecast")
acf(CADTSdiff1, lag.max=20, main="Lag of model differences")
acf(CADTSdiff1, lag.max=20, plot=FALSE)
plot.ts(CADTSdiff1, main="Plot of Differeneces")
plot.forecast(CADTStimeseriesforecasts)

AUDTSDiff1<-diff(AUDTS, differences=1)
EURTStimeseriesarima<-arima(EURTS, order=c(1,0,1))
EURTStimeseriesarima
EURTStimeseriesforecasts<-forecast.Arima(EURTStimeseriesarima, h=100)
EURTStimeseriesforecasts
plot(EURTStimeseriesforecasts, main="EURO Forecast")
AUDTStimeseriesarima<-auto.arima(AUDTS)
AUDTStimeseriesarima
AUDTStimeseriesforecasts<-forecast.Arima(AUDTStimeseriesarima, h=100)
AUDTStimeseriesforecasts
plot(AUDTStimeseriesforecasts, main="Aussie Forecast")
acf(AUDTSdiff1, lag.max=20, main="Lag of model differences")
acf(AUDTSdiff1, lag.max=20, plot=FALSE)
plot.ts(AUDTSdiff1, main="Plot of Differeneces")
plot.forecast(AUDTStimeseriesforecasts)

JPYTSDiff1<-diff(JPYTS, differences=1)
EURTStimeseriesarima<-arima(EURTS, order=c(1,0,1))
EURTStimeseriesarima
EURTStimeseriesforecasts<-forecast.Arima(EURTStimeseriesarima, h=100)
EURTStimeseriesforecasts
plot(EURTStimeseriesforecasts, main="EURO Forecast")
JPYTStimeseriesarima<-auto.arima(JPYTS)
JPYTStimeseriesarima
JPYTStimeseriesforecasts<-forecast.Arima(JPYTStimeseriesarima, h=100)
JPYTStimeseriesforecasts
plot(JPYTStimeseriesforecasts, main="Japan Forecast")
acf(JPYTSdiff1, lag.max=20, main="Lag of model differences")
acf(JPYTSdiff1, lag.max=20, plot=FALSE)
plot.ts(JPYTSdiff1, main="Plot of Differeneces")
plot.forecast(JPYTStimeseriesforecasts)

BTC<-diff(BTCTS, differences=1)
BTCTStimeseriesarima<-arima(BTCTS, order=c(1,0,1))
BTCTStimeseriesarima
BTCTStimeseriesforecasts<-forecast.Arima(BTCTStimeseriesarima, h=100)
BTCTStimeseriesforecasts
plot(BTCTStimeseriesforecasts, main="EURO Forecast")
BTCTStimeseriesarima<-auto.arima(BTCTS)
BTCTStimeseriesarima
BTCTStimeseriesforecasts<-forecast.Arima(BTCTStimeseriesarima, h=100)
BTCTStimeseriesforecasts
plot(BTCTStimeseriesforecasts, main="Japan Forecast")
acf(BTCTSdiff1, lag.max=20, main="Lag of model differences")
acf(BTCTSdiff1, lag.max=20, plot=FALSE)
plot.ts(BTCTSdiff1, main="Plot of Differeneces")
plot.forecast(BTCTStimeseriesforecasts)

################################Core arima model########################

#######################HIGHER LEVEL ANALYSIS#######################

###Get currency data   from FRED (Stl FED)####Read in basic symbols from earlier model

##AUSSIE
getSymbols("DEXUSAL", src="FRED")
###EURO
getSymbols("DEXUSEU", src="FRED")
###Canada
getSymbols("EXCAUS", src="FRED")
####GBP
getSymbols("DEXUSUK", src="FRED")
###Japan
getSymbols("DEXJPUS", src="FRED")

###4 column and 3 row basic charting
par(mfrow=c(3,4))
plot(1/coredata(DEXUSAL["1995:2015"],type="l", ylab="Aussie"))
plot(1/coredata(DEXUSEU["1995:2015"],type="l", ylab="EURO"))
plot(1/coredata(EXCAUS["1995:2015"],type="l", ylab="Canada"))
plot(1/coredata(DEXUSUK["1995:2015"],type="l", ylab="GBP"))
plot(1/coredata(DEXJPUS["1995:2015"],type="l", ylab="JPY"))
plot(1/coredata(DTWEXO["1995:2015"],type="l", ylab="USD"))
plot(1/coredata(DTWEXB["1995:2015"],type="l", ylab="USDbroad"))

###########chart bollinger bands#########

chartSeries(to.monthly(1/DEXUSAL),theme=chartTheme("white"), name="Australia/USD", TA="addBBands(10)")
chartSeries(to.monthly(1/DEXUSEU),theme=chartTheme("white"), name="EURO/USD", TA="addBBands(10)")
chartSeries(to.monthly(1/EXCAUS),theme=chartTheme("white"), name="Canada/USD", TA="addBBands(10)")
chartSeries(to.monthly(1/DEXJPUS),theme=chartTheme("white"), name="GBP/USD", TA="addBBands(10)")
chartSeries(to.monthly(1/DTWEXO),theme=chartTheme("white"), name="USD", TA="addBBands(10)")
chartSeries(to.monthly(1/DTWEXB),theme=chartTheme("white"), name="USDbroad", TA="addBBands(10)")

###roll in FED 10/yr data
##base Fed 10 year rate
getSymbols("DGS10",src="FRED")
##Fed dollar rate
getSymbols("DTWEXM",src="FRED")
##fed dollar other index
getSymbols("DTWEXO",src="FRED")
##SP500
getSymbols("SP500",src="FRED")

returns<-merge(monthlyReturn(to.monthly(DGS10/DTWEXM)),monthlyReturn(to.monthly(SP500)))
corSP100USD<-runCor(returns["1973::2015-09",1],returns["1973::2015-09",2],n=6)

###chart correlation on SP500 and 10yr###
####looking for additional signal for overall forex model
chartSeries(corSP100USD,theme="white", name="SP rolling Cor w/ US$-US 10y")



