#---
#title: "ARIMA, Sharpe and Beta for Asset Selection and Optimization"
#author: "Biao Huan Foo"
#---

#library(GMCM)
#library(ggplot2)
#library(xts)
#library(forecast)
#library(PortfolioAnalytics)
#library(urca)
#Set Working Directory to full_history (Contains csv of all stocks)

#Step 1.1: Cleaning of Assets - Less than 5% Missing Values
masterList = list.files(pattern="*.csv")
for (i in 1:length(masterList)){
  print(i)
  assign(masterList[i], read.csv(masterList[i])[1:2520,c(1,7)])
}
masterList2=lapply(ls(pattern="*.csv"),get)
nadf<-data.frame()
for(i in 1:length(masterList2)){
  print(i)
  nadf<-rbind(nadf,mean(is.na(masterList2[i][[1]][[2]])))
}
criteria = data.frame()
for(i in 1:length(masterList)){
  print(i)
  if(nadf[1][[1]][[i]]<0.05){
    criteria <- rbind(criteria,i)
  }
}
rm(list=ls(pattern="*.csv"))
for(i in 1:length(criteria[1][[1]])){
  print(i)
  assign(masterList[criteria[i,1]], read.csv(masterList[criteria[i,1]])[1:2520,c(1,2,7)])
}
#Step 1.2: Cleaning of Assets - Must have Recorded Volume
masterList2=lapply(ls(pattern="*.csv"),get)
criteria2<-data.frame()
for(i in 1:length(criteria[1][[1]])){
  print(i)
  if(!is.na(mean(masterList2[i][[1]][["volume"]]))){
    criteria2 <- rbind(criteria2,criteria[i,1])
  }
}
rm(list=ls(pattern="*.csv"))
for(i in 1:length(criteria2[1][[1]])){
  print(i)
  assign(masterList[criteria2[i,1]], read.csv(masterList[criteria2[i,1]])[1:2520,c(1,2,7)])
}

#Step 1.3: Cleaning of Assets - Must Have Sufficiently High (>1000) Volume
criteria3<-data.frame()
masterList2=lapply(ls(pattern="*.csv"),get)
for(i in 1:length(masterList2)){
  print(i)
  if(max(na.omit(masterList2[i][[1]][["volume"]]))>1000){
    criteria3 <- rbind(criteria3,criteria2[i,1])
  }
}
masterList2=lapply(ls(pattern="*.csv"),get)
rm(list=ls(pattern="*.csv"))
for(i in 1:length(criteria3[1][[1]])){
  print(i)
  assign(masterList[criteria3[i,1]], read.csv(masterList[criteria3[i,1]])[1:2520,c(1,7)])
}

#Step 2: Compute Daily Log Returns
masterList2=lapply(ls(pattern="*.csv"),get)
length(masterList2)
for(i in 1:length(masterList2)){
  print(i)
  lrest=log(masterList2[i][[1]][["adjclose"]][-2520]/masterList2[i][[1]][["adjclose"]][-1])
  lrest[2520]<-0
  masterList2[i][[1]][["logreturns"]]<- lrest
}
for(i in 1:length(masterList2)){
  print(i)
  assign(ls(pattern="*.csv")[i],masterList2[i][[1]][[3]])
}
masterList2=lapply(ls(pattern="*.csv"),get)

#Step 3: Replace Missing Values
masterList2=lapply(ls(pattern="*.csv"),get)
for(i in 1:length(masterList2)){
  print(i)
  masterList2[i][[1]]<-na.interp(masterList2[i][[1]])
}
for(i in 1:length(masterList2)){
  print(i)
  assign(ls(pattern="*.csv")[i],masterList2[i][[1]])
}

#Step 4.1-4.4.1: Processing Data for Asset Selection
masterList2=lapply(ls(pattern="*.csv"),get)
logReturnsRaw<-unlist(masterList2[1])
for(i in 2:length(masterList2)){
  print(i)
  logReturnsRaw<-cbind(logReturnsRaw,unlist(masterList2[i]))
}
logReturnsOrdered<-logReturnsRaw
logReturnsOrdered<-logReturnsOrdered[nrow(logReturnsOrdered):1,]
dates<-unlist(read.csv(masterList[1])[1:2520,1])
dates<-matrix(dates,nrow=2520,ncol=1)
dates<-dates[nrow(logReturnsOrdered):1,]
logReturnsWithTime<-cbind(dates,logReturnsOrdered)

#Flip objects in Workspace
masterList2=lapply(ls(pattern="*.csv"),get)
for(i in 1:3930){
  print(i)
  tempflip<-data.frame(masterList2[i][[1]])
  tempflip<-tempflip[nrow(tempflip):1,]
  masterList2[i][[1]]<-tempflip
  assign(ls(pattern="*.csv")[i],masterList2[i][[1]])
}

#Step 4.4.2: Developing an Asset Selection Based on Sharpe Ratio, Beta, Returns and ARIMA
#Rolling Window with Sample Size(T) = 2520, Size(m) = 252, Forecast Window (h) = 20
count=0 
loopcount=1
index<-matrix(c(1:3930), nrow=1, ncol=3930)
for(i in 232:2520){
  print(i)
  if(count==20){
    #Find Top 240 Stocks with Highest Sharpe Ratio in Last 252 Days
    criteriaSharpe<-(colMeans(logReturnsOrdered[(i-251):(i),]))/(GMCM:::colSds(logReturnsOrdered[(i-251):(i),]))
    orderedCriteriaSharpe<-rbind(index,criteriaSharpe)
    orderedCriteriaSharpe<-data.frame(orderedCriteriaSharpe)
    orderedCriteriaSharpe<-orderedCriteriaSharpe[order(orderedCriteriaSharpe[2,],decreasing=TRUE)]
    topSharpeStocks<-c(as.integer(orderedCriteriaSharpe[1,1:240]))
    topSharpeStocksReturns<-logReturnsOrdered[(i-251):(i),topSharpeStocks]
    marketReturns<-(rowSums(logReturnsOrdered[(i-251):(i),1:3930]))
    marketReturns<-unlist(marketReturns)
    #Find Top 120 Stocks out of 240 with Highest Beta in Last 252 Days
    topSharpeStocksCov<-cov(marketReturns,topSharpeStocksReturns)
    topSharpeStocksBeta<-topSharpeStocksCov/sd(marketReturns)
    orderedCriteriaBeta<-rbind(matrix(topSharpeStocks,nrow=1,ncol=240),matrix(topSharpeStocksBeta,nrow=1,ncol=240))
    orderedCriteriaBeta<-data.frame(orderedCriteriaBeta)
    orderedCriteriaBeta<-orderedCriteriaBeta[order(orderedCriteriaBeta[2,],decreasing=TRUE)]            
    topBetaStocks<-c(as.integer(orderedCriteriaBeta[1,1:120]))
    #Find Top 60 Stocks of out 120 with Highest Returns in Last 252 Days
    topBetaStocksReturns<-colSums(logReturnsOrdered[1:252,topBetaStocks])
    orderedCriteriaReturn<-rbind(matrix(topBetaStocks,nrow=1,ncol=120),matrix(topBetaStocksReturns,nrow=1,ncol=120))
    orderedCriteriaReturn<-data.frame(orderedCriteriaReturn)
    orderedCriteriaReturn<-orderedCriteriaReturn[order(orderedCriteriaReturn[2,],decreasing=TRUE)]   
    #Finding Top 30 Stocks out of 30 with Highest ARIMA Forecasts Over a 20 Day Forecast Window
    topReturnStocks<-c(as.integer(orderedCriteriaReturn[1,1:60]))
    topReturnStocksReturns<-data.frame(logReturnsOrdered[(i-251):(i),topReturnStocks])
    topReturnStocksReturns<-cbind(data.frame(as.Date(logReturnsWithTime[(i-251):(i),1])),topReturnStocksReturns)
    topReturnStocksXTS<-xts(topReturnStocksReturns[ ,2], order.by=topReturnStocksReturns[,1])
    #Creating Stationary Data and Best-Fit ARIMA Model
    topReturnStocksFit<-auto.arima(diff(topReturnStocksXTS))
    fitForecasted<-topReturnStocksFit %>% forecast(h=20)
    fitForecasted<-data.frame(fitForecasted)
    #Re-compiling Forecasted Log Returns for Each 20-Day Forecast Window
    fitForecastedPrices<-topReturnStocksXTS[[252]]+fitForecasted[1,1]
    for(j in 1:(20-1)){
      fitForecastedPrices<-rbind(fitForecastedPrices,fitForecastedPrices[j]+fitForecasted[j+1,1])
    }
    fitReturn<-data.frame(sum(fitForecastedPrices))
    for(k in 2:30){
      print(k+100)
      topReturnStocksXTS<-xts(topReturnStocksReturns[ ,(k+1)], order.by=topReturnStocksReturns[,1])
      topReturnStocksFit<-auto.arima(diff(topReturnStocksXTS))
      fitForecasted<-topReturnStocksFit %>% forecast(h=20)
      fitForecasted<-data.frame(fitForecasted)
      fitForecastedPrices<-topReturnStocksXTS[[252]]+fitForecasted[1,1]
      for(l in 1:(20-1)){
        fitForecastedPrices<-rbind(fitForecastedPrices,fitForecastedPrices[l]+fitForecasted[l+1,1])
      }
      tempFitReturn<-data.frame(sum(fitForecastedPrices))
      fitReturn<-cbind(fitReturn,tempFitReturn)
    }
    orderedFitReturn<-rbind(topReturnStocks,fitReturn)
    orderedFitReturn<-orderedFitReturn[order(orderedFitReturn[2,],decreasing=TRUE)]  
    if(loopcount==1){
      topArimaStocks<-matrix(c(as.integer(orderedFitReturn[1,1:30])),nrow=1,ncol=30)
    }else{
      tempTopArimaStocks<-matrix(c(as.integer(orderedFitReturn[1,1:30])),nrow=1,ncol=30)
      topArimaStocks<-rbind(topArimaStocks,tempTopArimaStocks)
    }
    loopcount=loopcount+1
    count=0
  }
  count=count+1
} 
dim(topArimaStocks)

#Step 4.5: Portfolio Optimization
count=0
loopcount=1
for(i in 232:2520){
  print(i)
  if(count==20){
    myStocks<-logReturnsOrdered[(i-251):(i),c(topArimaStocks[loopcount,])]
    myStocksXTS<-cbind(data.frame(as.Date(logReturnsWithTime[(i-251):(i),1])),myStocks)
    myStocksXTS<-xts(myStocksXTS[,2:31],order.by=myStocksXTS[,1])
    portfolioConstructor <-portfolio.spec(c(topArimaStocks[loopcount,]))
    portfolioConstructor<-add.constraint(portfolioConstructor, type="full_investment")
    portfolioConstructor<-add.constraint(portfolioConstructor, type ="box", min=0.01, max=0.1)
    portfolioConstructor<-add.constraint(portfolioConstructor, type ="long_only")
    portfolioConstructor<-add.objective(portfolioConstructor, type="return", name="mean")
    portfolioConstructor<-add.objective(portfolioConstructor, type="risk", name="StdDev")
    optimizedPortfolio <- optimize.portfolio(myStocksXTS, portfolioConstructor, optimize_method="random",search_size=10000,maxSR=TRUE, message=TRUE)
    if(loopcount==1){
      weightsForPortfolio <- data.frame(extractWeights(optimizedPortfolio))
    }else{
      tempWeightsForPortfolio <- data.frame(extractWeights(optimizedPortfolio))
      weightsForPortfolio<-cbind(weightsForPortfolio,tempWeightsForPortfolio)
    }
    count=0
    loopcount=loopcount+1
  }
  count=count+1
}
weightsForPortfolio<-matrix(weightsForPortfolio,nrow=114,ncol=30)
weightsForPortfolio<-t(weightsForPortfolio)

#Step 5.1: Cumulative Returns for Optimized Portfolio
count=0
loopcount=1
for(i in 232:2520){
  print(i)
  if(count==20){
    myReturns<-matrix(logReturnsOrdered[(i+1):(i+20),c(topArimaStocks[loopcount,])],nrow=20,ncol=30)
    tempMyPortfolio<-myReturns %*% as.matrix(unlist(weightsForPortfolio[,loopcount]))
    if(loopcount==1){
      myPortfolio<-tempMyPortfolio
    }else{
      myPortfolio<-rbind(myPortfolio,as.matrix(tempMyPortfolio))
    }
    count=0
    loopcount=loopcount+1
  }
  count=count+1
  if(i==2511){
    break
  }
}
dim(myPortfolio)
myPortfolioXTS<-cbind(data.frame(as.Date(logReturnsWithTime[(253):(2512),1])),myPortfolio)
myPortfolioXTS<-xts(myPortfolioXTS[,2],order.by=myPortfolioXTS[,1])
myPortfolioXTSBlown=myPortfolioXTS*1000
myPortfolioXTSBlown[2260,]
cumPortfolioXTSBlown<-cumsum(myPortfolioXTSBlown)
cumPortfolioXTSBlown[2260,]
plot(cumPortfolioXTSBlown, main="Optimized Portfolio",xlab="Time",ylab="logreturns*1000" )

#Step 5.2:Cumulative Returns for Equal Weight Portfolio
count=0
loopcount=1
for(i in 232:2520){
  print(i)
  if(count==20){
    myReturnsEqual<-matrix(logReturnsOrdered[(i+1):(i+20),c(topArimaStocks[loopcount,])],nrow=20,ncol=30)
    tempMyPortfolioEqual<-myReturnsEqual %*% matrix(1/30, nrow=30, ncol=1)
    if(loopcount==1){
      myPortfolioEqual<-tempMyPortfolioEqual
    }else{
      myPortfolioEqual<-rbind(myPortfolioEqual,as.matrix(tempMyPortfolioEqual))
    }
    count=0
    loopcount=loopcount+1
  }
  count=count+1
  if(i==2511){
    break
  }
}
myPortfolioEqualXTS<-cbind(data.frame(as.Date(logReturnsWithTime[(253):(2512),1])),myPortfolioEqual)
myPortfolioEqualXTS<-xts(myPortfolioEqualXTS[,2],order.by=myPortfolioEqualXTS[,1])
myPortfolioEqualXTSBlown=myPortfolioEqualXTS*1000
myPortfolioEqualXTSBlown[2260,]
cumEqualPortfolioXTSBlown<-cumsum(myPortfolioEqualXTSBlown)
cumEqualPortfolioXTSBlown[2260,]
plot(cumEqualPortfolioXTSBlown, main="Equally Weighted Portfolio",xlab="Time",ylab="logreturns*1000" )

#Step 5.3: Cumulative Returns for S&P 500
SP500<-read.csv("SP500new.csv")
SP500<-SP500[37920:35401,c(5)]
for(i in 1:2520){
  print(i)
  if(i==2520) break
  SP500[i]<-log(SP500[i]/SP500[i+1])
}
SP500[2520]<-0
incorrectDates<-dates[nrow(logReturnsOrdered):1]
SP500<-cbind(incorrectDates,SP500)
SP500<-SP500[2520:1,]
SP500<-as.matrix(SP500)
SP500XTS<-cbind(data.frame(as.Date(logReturnsWithTime[(253):(2512),1])),as.numeric(SP500[253:2512,2]))
SP500XTS<-xts(SP500XTS[,2],order.by=SP500XTS[,1])
SP500XTSBlown=SP500XTS*1000
SP500XTSBlown[2260,]
cumSP500XTSBlown<-cumsum(SP500XTSBlown)
cumSP500XTSBlown[2260,]
plot(cumSP500XTSBlown, main="S&P 500",xlab="Time",ylab="logreturns*1000" )
allThree<-merge.xts(cumSP500XTSBlown,cumPortfolioXTSBlown,cumEqualPortfolioXTSBlown)
plot(allThree, main="All Portfolios",xlab="Time",ylab="logreturns*1000")
addLegend(legend.loc = "bottomright", legend.names = c("S&P500 - Black", "Equal Weight - Orange", "Optimized - Red"))

#Step 5.4.1: Compare Equal Weight with ARIMA Forecasts
count=0
loopcount=1
for(i in 232:2520){
  print(i)
  if(count==20){
    tempReturns<-data.frame(logReturnsOrdered[(i-251):(i),c(topArimaStocks[loopcount,])])
    tempReturns<-cbind(data.frame(as.Date(logReturnsWithTime[(i-251):(i),1])),tempReturns)
    tempReturnsXTS<-xts(tempReturns[ ,2], order.by=tempReturns[,1])
    tempFit<-auto.arima(diff(tempReturnsXTS))
    predictedDiff<-tempFit %>% forecast(h=20)
    predictedDiff<-data.frame(predictedDiff)
    predictedReturns<-tempReturnsXTS[[252]]+predictedDiff[1,1]
    for(j in 1:(20-1)){
      predictedReturns<-rbind(predictedReturns,predictedReturns[j]+predictedDiff[j+1,1])
    }
    predictedReturns<-data.frame(predictedReturns)
    for(k in 2:30){
      print(k+100)
      tempReturnsXTS<-xts(tempReturns[ ,(k+1)], order.by=tempReturns[,1])
      tempFit<-auto.arima(diff(tempReturnsXTS))
      predictedDiff<-tempFit %>% forecast(h=20)
      predictedDiff<-data.frame(predictedDiff)
      tempPredictedReturns<-tempReturnsXTS[[252]]+predictedDiff[1,1]
      for(l in 1:(20-1)){
        tempPredictedReturns<-rbind(tempPredictedReturns,tempPredictedReturns[l]+predictedDiff[l+1,1])
      }
      tempPredictedReturns<-data.frame(tempPredictedReturns)
      predictedReturns<-cbind(predictedReturns,tempPredictedReturns)
    }
    if(loopcount==1){
      totalReturns<-matrix(predictedReturns,nrow=20,ncol=30)
    }else{
      tempTotalReturns<-matrix(predictedReturns,nrow=20,ncol=30)
      totalReturns<-rbind(totalReturns,tempTotalReturns)
    }
    loopcount=loopcount+1
    count=0
  }
  count=count+1
} 
totalReturns[1,]
for(i in 1:114){
  print(i)
  tempEnlargedArimaStocks<-matrix(topArimaStocks[i,],nrow=1,ncol=30)
  for(k in 1:19){
    tempEnlargedArimaStocks<-rbind(tempEnlargedArimaStocks,matrix(topArimaStocks[i,],nrow=1,ncol=30))
    count=count+1
  }
  if(i==1){
    enlargedArimaStocks<-tempEnlargedArimaStocks
  }else{
    enlargedArimaStocks<-rbind(enlargedArimaStocks,tempEnlargedArimaStocks)
  }
}

sumTotalReturns<-matrix(unlist(totalReturns),nrow=2280,ncol=30)
sumTotalReturns<-rowSums(sumTotalReturns)
as.matrix(sumTotalReturns)
sumTotalReturnsXTS<-cbind(data.frame(as.Date(logReturnsWithTime[(253):(2512),1])),sumTotalReturns[1:2260])
sumTotalReturnsXTS<-xts(sumTotalReturnsXTS[,2],order.by=sumTotalReturnsXTS[,1])
sumTotalReturnsXTSBlown=sumTotalReturnsXTS*100/30
cumSumTotalReturnsXTSBlown<-cumsum(sumTotalReturnsXTSBlown)
cumSumTotalReturnsXTSBlown[2260,]
plot(cumSumTotalReturnsXTSBlown, main="Forecasted ARIMA Returns",xlab="Time",ylab="logreturns*1000" )
allFour<-merge.xts(allThree,cumSumTotalReturnsXTSBlown)
plot(allFour, main="Forecast v. Actual",xlab="Time",ylab="logreturns*1000" )
addLegend(legend.loc = "topleft", legend.names = c("S&P500 - Black", "Equal Weight - Orange", "Optimized - Red","Forecasted ARIMA (Equal Weight) - Blue"))


#Step 5.4.2: Compare with Optimized Forecasts
weightedReturns<-enlargedWeights*matrix(unlist(totalReturns), nrow=2280, ncol=30)
sumWeightedReturns<-rowSums(weightedReturns)
sumWeightedReturnsXTS<-cbind(data.frame(as.Date(logReturnsWithTime[(253):(2512),1])),sumWeightedReturns[1:2260])
sumWeightedReturnsXTS<-xts(sumWeightedReturnsXTS[,2],order.by=sumWeightedReturnsXTS[,1])
sumWeightedReturnsXTSBlown=sumWeightedReturnsXTS*1000/30
cumSumWeightedReturnsXTSBlown<-cumsum(sumWeightedReturnsXTSBlown)
cumSumTotalReturnsXTSBlown[2260,]

#Step 5.4.3: Compare with Equal Forecasts
equalReturns<-enlargedWeights*matrix(1/30, nrow=2280, ncol=30)
sumEqualReturns<-rowSums(equalReturns)
sumEqualReturnsXTS<-cbind(data.frame(as.Date(logReturnsWithTime[(253):(2512),1])),sumEqualReturns[1:2260])
sumEqualReturnsXTS<-xts(sumEqualReturnsXTS[,2],order.by=sumEqualReturnsXTS[,1])
sumEqualReturnsXTSBlown=sumEqualReturnsXTS*1000/30
cumSumEqualReturnsXTSBlown<-cumsum(sumEqualReturnsXTSBlown)
cumSumEqualReturnsXTSBlown[2260,]

#Step 5.4.4: Compare with S&P 500 Forecasts
SP500XTS2<-cbind(data.frame(as.Date(logReturnsWithTime[(1):(2520),1])),as.numeric(SP500[1:2520,2]))
SP500XTS2<-xts(SP500XTS2[,2],order.by=SP500XTS2[,1])
count=0
loopcount=1
for(i in 232:2520){
  print(i)
  if(count==20){
    tempSP500ReturnsXTS<-SP500XTS2[(i-251):(i)]
    tempSP500Fit<-auto.arima(diff(tempSP500ReturnsXTS))
    predictedSP500Diff<-tempSP500Fit %>% forecast(h=20)
    predictedSP500Diff<-data.frame(predictedSP500Diff)
    predictedSP500Returns<-tempSP500ReturnsXTS[[252]]+predictedSP500Diff[1,1]
    for(j in 1:(20-1)){
      predictedSP500Returns<-rbind(predictedSP500Returns,predictedSP500Returns[j]+predictedSP500Diff[j+1,1])
    }
    predictedSP500Returns<-data.frame(predictedSP500Returns)
    if(loopcount==1){
      totalSP500Returns<-data.frame(predictedSP500Returns)
    }else{
      tempTotalSP500Returns<-data.frame(predictedSP500Returns)
      totalSP500Returns<-rbind(totalSP500Returns,tempTotalSP500Returns)
    }
    loopcount=loopcount+1
    count=0
  }
  count=count+1
} 
tempTotalSP500Returns[2000,]

sumSP500ReturnsXTS<-cbind(data.frame(as.Date(logReturnsWithTime[(253):(2512),1])),totalSP500Returns[1:2260,])
sumSP500ReturnsXTS<-xts(sumSP500ReturnsXTS[,2],order.by=sumSP500ReturnsXTS[,1])
sumSP500ReturnsXTSBlown=sumSP500ReturnsXTS*1000
cumSumSP500ReturnsXTSBlown<-cumsum(sumSP500ReturnsXTSBlown)
cumSumSP500ReturnsXTSBlown[2260]
cumSumWeightedReturnsXTSBlown[2260]
cumSumEqualReturnsXTSBlown[2260]
portfolioForecastTwo<-merge.xts(cumSumSP500ReturnsXTSBlown,cumSumEqualReturnsXTSBlown,cumSumWeightedReturnsXTSBlown)
plot(portfolioForecastTwo, main="Forecast v. Actual",xlab="Time",ylab="logreturns*1000" )
addLegend(legend.loc = "topleft", legend.names = c("S&P500 - Black", "Equal Weight - Red", "Optimized - Orange","Forecasted ARIMA (Equal Weight) - Blue"))

#Step 6: Comparative Statistics

#Step 6.1: Mean Returns
meanOptimized<-mean(myPortfolioXTSBlown)
meanEqual<-mean(myPortfolioEqualXTSBlown)
meanSP500<-mean(SP500XTSBlown)
meanOptimized
meanEqual
meanSP500

#Step 6.2: Volatility
volOptimized<-StdDev(myPortfolioXTS)
volEqual<-StdDev(myPortfolioEqualXTS)
volSP500<-StdDev(SP500XTS)
volOptimized
volEqual
volSP500

#Step 6.3: Sharpe Ratio
sharpeOptimized<-SharpeRatio.annualized(myPortfolioXTS)
sharpeEqual<-SharpeRatio.annualized(myPortfolioEqualXTS)
sharpeSP500<-SharpeRatio.annualized(SP500XTS)
sharpeOptimized
sharpeEqual
sharpeSP500

#Step 6.4: Sortino Ratio
sortinoOptimized<-SortinoRatio(myPortfolioXTS, MAR=0)
sortinoEqual<-SortinoRatio(myPortfolioEqualXTS, MAR=0)
sortinoSP500<-SortinoRatio(SP500XTS, MAR=0)
sortinoOptimized
sortinoEqual
sortinoSP500

#Step 6.5: Maximum Drawdown
mDrawOptimized<-maxDrawdown(myPortfolioXTS)
mDrawEqual<-maxDrawdown(myPortfolioEqualXTS)
mDrawSP500<-maxDrawdown(SP500XTS)
mDrawOptimized
mDrawEqual
mDrawSP500


#Step 6.6: Turnover
#Weights for 2260 Days
fWeightsForPortfolio=t(weightsForPortfolio)
for(i in 1:114){
  print(i)
  tempEnlargedWeights<-matrix(fWeightsForPortfolio[i,],nrow=1,ncol=30)
  for(k in 1:19){
    tempEnlargedWeights<-rbind(tempEnlargedWeights,matrix(fWeightsForPortfolio[i,],nrow=1,ncol=30))
    count=count+1
  }
  if(i==1){
    enlargedWeights<-tempEnlargedWeights
  }else{
    enlargedWeights<-rbind(enlargedWeights,tempEnlargedWeights)
  }
}

count=0
loopcount=1
for(i in 232:2520){
  print(i)
  if(count==20){
    tempRawLogReturns<-matrix(logReturnsOrdered[(i+1):(i+20),c(topArimaStocks[loopcount,])],nrow=20,ncol=30)
    if(loopcount==1){
      rawLogReturns<-as.matrix(tempRawLogReturns)
    }else{
      rawLogReturns<-rbind(rawLogReturns,as.matrix(tempRawLogReturns))
    }
    count=0
    loopcount=loopcount+1
  }
  count=count+1
  if(i==2511){
    break
  }
}
enlargedWeightsXTS<-cbind(data.frame(as.Date(logReturnsWithTime[(253):(2512),1])),enlargedWeights[1:2260,])
enlargedWeightsXTS<-xts(enlargedWeightsXTS[,2:31],order.by=enlargedWeightsXTS[,1])
rawLogReturnsXTS<-cbind(data.frame(as.Date(logReturnsWithTime[(253):(2512),1])),rawLogReturns[1:2260,])
rawLogReturnsXTS<-xts(rawLogReturnsXTS[,2:31],order.by=rawLogReturnsXTS[,1])
rawLogReturnsXTS[1,]

#Step 6.6.1: Daily Turnover for Optimized
outOptimized <- Return.portfolio(R = rawLogReturnsXTS, weights = enlargedWeightsXTS, verbose = TRUE)
beginWeightsOptimized <- outOptimized$BOP.Weight
endWeightsOptimized <- outOptimized$EOP.Weight
txnsOptimized <- beginWeightsOptimized - lag(endWeightsOptimized)
dailyTOOptimized <- xts(rowSums(abs(txnsOptimized[,1:30])), order.by=index(txnsOptimized))
barplot(dailyTOOptimized, main="Daily Turnover for Optimized")

#Step 6.6.2: Daily Turnover for Equal
equalWeightsXTS<-cbind(data.frame(as.Date(logReturnsWithTime[(253):(2512),1])),matrix(1/30,nrow=2260,ncol=30))
equalWeightsXTS<-xts(equalWeightsXTS[,2:31],order.by=equalWeightsXTS[,1])
outEqual <- Return.portfolio(R = rawLogReturnsXTS, weights = equalWeightsXTS , verbose = TRUE)
beginWeightsEqual <- outEqual$BOP.Weight
endWeightsEqual <- outEqual$EOP.Weight
txnsEqual <- beginWeightsEqual - lag(endWeightsEqual)
dailyTOEqual <- xts(rowSums(abs(txnsEqual[,1:30])), order.by=index(txnsEqual))
barplot(dailyTOEqual, main="Daily Turnover for Equal")

write.table(enlargedWeights ,file="enlargedWeights.txt")
