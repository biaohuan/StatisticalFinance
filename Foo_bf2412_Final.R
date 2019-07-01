#---
#title: "ARIMA, ARMA-GARCH and ARFIMA Priors in Black-Litterman Models"
#author: "Biao Huan Foo"
#---

#This program uses linear regression methods for asset selection as well as ARIMA, ARMA-GARCH and ARFIMA Black-Litterman Models 
#for portfolio optimization

#NOTE: Change working directory in lines 47 and 245 before running the whole program
#NOTE: Possible popup: Reinstallation of packages; consider running from line 47 onwards if you have already installed them
#If not, ensure that packages are installed before starting the program

#Step 0.1: The following packages are required-------------------------------------------------------------------------
install.packages("GMCM")
install.packages("ggplot2")
install.packages("xts")
install.packages("forecast")
install.packages("PortfolioAnalytics")
install.packages("urca")
install.packages("tsoutliers")
install.packages("PerformanceAnalytics")
install.packages("zoo")
install.packages("xts")
install.packages("MASS")
install.packages("IntroCompFinR", repos="http://R-Forge.R-project.org")
install.packages("rugarch")
install.packages("data.table")
install.packages("arfima")

library(GMCM)
library(ggplot2)
library(xts)
library(forecast)
library(PortfolioAnalytics)
library(urca)
library(tsoutliers)
library(PerformanceAnalytics)
library(zoo)
library(xts)
library(MASS)
library(IntroCompFinR)
library(rugarch)
library(data.table)
library(arfima)

#Step 0.2: Set Working Directory to full_history, which contains csv of all stocks--------------------------------------
setwd("~/Biao Huan/Columbia/Academics/Summer 2019 (Final)/Statistical Methods for Finance/SH/fh_20190420/full_history")

#Step 1: Cleaning of Assets; Remove those with More than 5% Missing Values----------------------------------------------
masterList = list.files(pattern="*.csv")
df <- read.csv(masterList[1])
df <- data.frame(df$date, df$adjclose)
df <- df[1:2521,]
substring <- substr(masterList[1], start = 1, stop = nchar(masterList[1])-4)
colnames(df)[2] <- substring
for (i in 2:length(masterList)){
  print(i)
  temp <- read.csv(masterList[i])
  temp <- data.frame(temp$date, temp$adjclose)
  if(nrow(temp)>0){
    if(temp[1,1] == "2019-04-18"){
      substring <- substr(masterList[i], start = 1, stop = nchar(masterList[i])-4)
      colnames(temp)[2] <- substring
      if(nrow(temp)>=2521){
        temp <- temp[1:2521,]
        df <- merge(x=df, y=temp, by.x = "df.date",by.y = "temp.date", all.x = TRUE, all.y = TRUE)
      }else{
        df <- merge(x=df, y=temp, by.x = "df.date",by.y = "temp.date", all.x = TRUE, all.y = TRUE)
      }
    }
  }
}

which(df[,1]=="2009-04-15")
which(df[,1]=="2019-04-18")
check <- c()
for(i in 2: length(df)){
  print(i)
  if (df[4636,i] == df[4637,i]){
    check[i-1] <- TRUE
  } else{
    check[i-1] <- FALSE
  }
}
length(check[check == TRUE])
pricesRaw <- df[2116:4636,]
pricesRaw[,2]
sum(is.na(pricesRaw[,4]))
pricesRaw2 <- data.frame(pricesRaw[,1])
names(pricesRaw2)[1]<-names(pricesRaw)[1]
criteria <- c()
index = 2
for(i in 2:length(pricesRaw)){
  print(i)
  criteria[i] <- sum(is.na(pricesRaw[2:2521,i]))/2520
  if(criteria[i] <= 0.05){
    pricesRaw2[,index] = pricesRaw[,i]
    names(pricesRaw2)[index]=names(pricesRaw)[i]
    index = index +1
  }
}

#Step 2: Prices are Flipped; Compute Log Returns------------------------------------------------------------------------
logReturnsRaw <- data.frame(pricesRaw2[2:nrow(pricesRaw2),1])
names(logReturnsRaw)[1] <- names(pricesRaw2)[1]
for(i in 2:length(pricesRaw2)){
  print(i)
  for(j in 1: nrow(logReturnsRaw)){
    logReturnsRaw[j,i] = log(pricesRaw2[(j+1),i]/pricesRaw2[j,i])
  }
  names(logReturnsRaw)[i] <- names(pricesRaw2)[i]
}
criteria2 <- c(0)
for(i in 2:length(logReturnsRaw)){
  print(i)
  criteria2[i] <- sum(is.na(logReturnsRaw[i]))
}
criteria2 <- criteria2/2520
summary(criteria2)
which(criteria2>0.05)

criteria3 <- c(0)
logReturnsFiltered <- data.frame(logReturnsRaw[,1])
start = 2
for(i in 2:length(logReturnsRaw)){
  print(i)
  criteria3[i] <- sum(is.na(logReturnsRaw[,i]))/2520
  if(criteria3[i] <= 0.05){
    logReturnsFiltered[,start] = logReturnsRaw[,i]
    names(logReturnsFiltered)[start]=names(logReturnsRaw)[i]
    start = start +1
  }
}

volumeRaw<-c()
for(i in 2:length(logReturnsFiltered)){
  print(i)
  volumeRaw[i-1] <- paste0(names(logReturnsFiltered)[i], ".csv")
}
volumeFiltered <- read.csv(volumeRaw[1])
volumeFiltered <- data.frame(volumeFiltered$date, volumeFiltered$volume)
volumeFiltered <- volumeFiltered[1:2521,]
substring <- substr(volumeRaw[1], start = 1, stop = nchar(volumeRaw[1])-4)
colnames(volumeFiltered)[2] <- substring
for (i in 2:length(volumeRaw)){
  print(i)
  temp <- read.csv(volumeRaw[i])
  temp <- data.frame(temp$date, temp$volume)
  substring <- substr(volumeRaw[i], start = 1, stop = nchar(volumeRaw[i])-4)
  colnames(temp)[2] <- substring
  if(nrow(temp)>=2521){
    temp <- temp[1:2521,]
    volumeFiltered <- merge(x=volumeFiltered, y=temp, by.x = "volumeFiltered.date",by.y = "temp.date", all.x = TRUE, all.y = TRUE)
  }else{
    volumeFiltered <- merge(x=volumeFiltered, y=temp, by.x = "volumeFiltered.date",by.y = "temp.date", all.x = TRUE, all.y = TRUE)
  }
}
which(volumeFiltered[,1]=="2019-04-18")
which(volumeFiltered[,1]=="2009-04-16")
volumeFiltered.1 <- volumeFiltered[123:2642,]

volumeExt <- c()
criteria4 <- c()
start <- 1
for(i in 2:length(volumeFiltered.1)){
  print(i)
  criteria4[i-1] <- length(volumeFiltered.1[,i][volumeFiltered.1[,i] < 1000])/2520
  if(criteria4[i-1] > 0.05){
    volumeExt[start] = names(volumeFiltered.1)[i]
    start = start +1
  }
}
logReturnsTemp <- logReturnsFiltered[, !(names(logReturnsFiltered)%in% volumeExt )] 

#Step 3: Cleaning of Data and Replacing of Outliers---------------------------------------------------------------------
logReturnsTemp1 <- data.frame(logReturnsTemp[-1])
for(i in 1: ncol(logReturnsTemp1)){
  print(i)
  logReturnsTemp1[i] <- na.interp(logReturnsTemp1[i])
}
logReturnsTemp2 <- data.matrix(logReturnsTemp1)
logReturnsCleaned<-logReturnsTemp2

#The end result is 3258 stocks which have appropriate data for analysis

#NOTE: We find that tsclean is not appropriate as it replaces data for ~33 stocks with static, arbitrary values
#The use of tsclean will lead to scenarios where asset selection generates portfolios that seemingly outperform
#the S&P 500 index from inception due to the inclusion of erroneous data 
#For completeness, we include the following code for the reader to confirm the claim
#logReturnsCleaned <- matrix(nrow = nrow(logReturnsTemp2), ncol = ncol(logReturnsTemp2))
#colnames(logReturnsCleaned)=colnames(logReturnsTemp2)
#for(i in 1: 3258){
#  print(i)
#  logReturnsCleaned[,i] <- tsclean(logReturnsTemp2[,i])
#}
#check <- logReturnsCleaned == logReturnsTemp2
#The ~1% in the computation below equates to ~33 stocks with static, market-outperforming returns
#sum(check!=TRUE)/(2520*3258)

#We include a dates matrix for further use
dates<-unlist(logReturnsFiltered[1])
dates<-matrix(dates,nrow=2520,ncol=1)
dates<-as.Date(dates)
names<-data.frame(names(logReturnsTemp1))
names[1][[1]][[2]]

#Step 4: Rolling Window Exercise---------------------------------------------------------------------------------------
#Rolling Window with Sample Size(T) = 2520, Size(m) = 252, Forecast Window (h) = 20
#NOTE: For big theta (computing time) reasons, the rolling window exercise is divided into 3 main sections.
#I elaborate on the segmented rolling window process below to demonstrate that it is NOT forward looking.

#Section I: Asset selection with in-sample data from i-251:i
#E.g.: If start date is 2010-04-16, we look at data from 2009-04-16:2010-04-16 to make decisions for 2010-04-17:2010-05-09
#NOTE: As the rolling window is segmented, section I generates a 114X30 matrix of stocks for each 20-day rolling window.
#In practice, you would compute a 1X30 matrix of stocks based on last 252 days, which will be your asset choices for next 20 days

#Section I.i: Portfolio Optimization by inputing priors from an ARIMA Model and an ARMA-GARCH Model to a Black-Litterman Model
#NOTE: This step is appropriately skipped for the equal weight portfolio

#Section II: Calculate Actual Returns
#NOTE: We generate actual returns by enlarging the asset matrix to 2260X30 (2260 days, every 20 days with the same set of assets)
#We then extract log returns for the 2260 days, and compile returns back into a 113X30 matrix (113 windows)
#We set the end date of our exercise to 2019-04-08 (2512nd day), which is why we have 113 windows of 20 days
#In practice, you would compute returns daily with a new list of 30 stocks every 20 days

#Section III: Compare with S&P 500
#S&P 500 log returns are computed for the 2260 days, and a time serires is generated to compare the actual returns of our
#portfolios against the S&P 500.

#Step 4.1: Section I: Asset Selection-----------------------------------------------------------------------------------
#i) Linear Regression Coefficient Analysis: For each 252 days in sample, we run a linear regression against time
#and obtain b coefficients for all 3258 assets in our filtered universe. 
#We then obtain the top 240 stocks with highest coefficients

#ii) High Sharpe Ratio: For the 240 stocks, we obtain the top 120 with highest Sharpe Ratios to minimize risky returns

#iii) Low Beta: For the 120 stocks, we then obtain the top 60 with lowest Beta to ensure that they are shielded from
#exaggerated market movements

#iv) Low Correlation: For the 60 stocks, we then obtain a 60X60 correlation matrix and obtain
#the top 30 with lowest-correlated stocks to shield against negative trends

#The code is self-explanatory with objects that are appropriately named
#Set working directory to wherever you stored SP500new.csv (must be different folder from full_history)
setwd("~/Biao Huan/Columbia/Academics/Summer 2019 (Final)/Statistical Methods for Finance/SH/fh_20190420")
SP500<-read.csv("SP500new.csv")
SP500<-SP500[37920:35401,c(5)]
for(i in 1:2520){
  print(i)
  if(i==2520) break
  SP500[i]<-log(SP500[i]/SP500[i+1])
}
SP500[2520]<-0
incorrectDates<-dates[2520:1]
SP500<-cbind(incorrectDates,SP500)
SP500<-SP500[2520:1,]
SP500<-as.matrix(SP500)

count=0 
loopcount=1
index<-matrix(c(1:3258), nrow=1, ncol=3258)
for(i in 232:2520){
  print(i)
  if(count==20){
    topCoefficients<-c()
    for(j in 1:3258){
      coef<-cbind(logReturnsCleaned[(i-251):(i),j],matrix(c(1:252),nrow=252,ncol=1))
      coef2<-lm(coef[,1]~coef[,2])
      coef3<-coef2$coefficients[2]
      topCoefficients<-cbind(topCoefficients,coef3)
    }
    topCoefficients<-matrix(topCoefficients,nrow=1,ncol=3258)
    orderedCoef<-rbind(index,topCoefficients)
    orderedCoef<-data.frame(orderedCoef)
    orderedCoef<-orderedCoef[order(orderedCoef[2,],decreasing=FALSE)]
    topCoefStocks<-c(as.integer(orderedCoef[1,1:240]))
    topCoefStocksReturns<-logReturnsCleaned[(i-251):(i),topCoefStocks]
    criteriaSharpe<-(colMeans(topCoefStocksReturns))/(GMCM:::colSds(topCoefStocksReturns))
    orderedCriteriaSharpe<-rbind(topCoefStocks,criteriaSharpe)
    orderedCriteriaSharpe<-data.frame(orderedCriteriaSharpe)
    orderedCriteriaSharpe<-orderedCriteriaSharpe[order(orderedCriteriaSharpe[2,],decreasing=TRUE)]
    topSharpeStocks<-c(as.integer(orderedCriteriaSharpe[1,1:120]))
    topSharpeStocksReturns<-logReturnsCleaned[(i-251):(i),topSharpeStocks]
    topSharpeStocksBeta<-cov(topSharpeStocksReturns)/sd(SP500[(i-251):(i),2])
    orderedCriteriaBeta<-rbind(matrix(topSharpeStocks,nrow=1,ncol=120),matrix(topSharpeStocksBeta,nrow=1,ncol=120))
    orderedCriteriaBeta<-data.frame(orderedCriteriaBeta)
    orderedCriteriaBeta<-orderedCriteriaBeta[order(orderedCriteriaBeta[2,],decreasing=FALSE)]    
    topBetaStocks<-c(as.integer(orderedCriteriaBeta[1,1:60]))
    topBetaStocksReturns<-logReturnsCleaned[1:252,topBetaStocks]
    z<-cor(topBetaStocksReturns)
    z[lower.tri(z,diag=TRUE)]=NA  #This section generates the 60X60 correlation matrix
    z=as.data.frame(as.table(z))  
    z=na.omit(z)  
    z=z[order(-abs(z$Freq),decreasing=TRUE),]  
    lowestCor<-matrix(nrow=1,ncol=1)
    for(h in 1:nrow(z)){
      temp1<-as.matrix(z[h,1])
      temp2<-as.matrix(z[h,2])
      temp3<-rbind(temp1,temp2)
      lowestCor<-rbind(lowestCor,temp3)
    }
    lowestCor<-unique(lowestCor) #As the values are obtained by pair-wise comparison, we delete repetitions
    index2<-matrix(nrow=1,ncol=1)
    for(k in 2:31){
      temp<-which(colnames(logReturnsCleaned) == lowestCor[k]) 
      index2<-rbind(index2,temp)
    }
    index2<-index2[2:31]
    if(loopcount==1){
      topStocks<-matrix(c(as.integer(index2)),nrow=1,ncol=30)
    }else{
      tempTopStocks<-matrix(c(as.integer(index2)),nrow=1,ncol=30)
      topStocks<-rbind(topStocks,tempTopStocks)
    }
    loopcount=loopcount+1
    count=0
  }
  count=count+1
} 
dim(topStocks)

#Step 4.2: Calculation of Returns and Plotting--------------------------------------------------------------------------
#Equal Portfolio #######################################################################################################

#Section II (Equal): We calculate the actual returns for our asset selection. This is not a forward-looking process for
#the reasons stated above, which we direct the reader to re-read in the note under Step 4.2
#We calculate a 1/N portfolio (N=30) first to see how it compares with the S&P 500

#This creates a matrix of weights for calculation of transaction costs
netMatrix<-matrix(0,nrow=2260,ncol=3258)
count=20
loopcount=1
for(i in 0:2260){
  print(i)
  if(count==20){
    for(h in 1:20){
      netMatrix[i+h,c(topStocks[loopcount,])]=1/30
    }
    count=0
    loopcount=loopcount+1
  }
  count=count+1
  if(loopcount==114){
    break
  }
}
dim(netMatrix)
sum(abs(netMatrix[21,]-netMatrix[20,]))

#We now compute net price weights for the computation of transaction costs
colnames(pricesRaw2)[2]
which(colnames(pricesRaw2)==colnames(logReturnsCleaned)[1])
pricesOrdered<-matrix(nrow=2521,ncol=3258)
for(i in 1:3258){
  print(i)
  num<-which(colnames(pricesRaw2)==colnames(logReturnsCleaned)[i])
  pricesOrdered[,i]<-pricesRaw2[1:2521,num]
}
colnames(pricesOrdered)=colnames(logReturnsCleaned)
pricesTemp1 <- data.frame(pricesOrdered)
for(i in 1: ncol(pricesTemp1)){
  print(i)
  pricesTemp1[i] <- na.interp(pricesTemp1[i])
}
pricesTemp2 <- data.matrix(pricesTemp1)
pricesCleaned<-pricesTemp2[2:2521,]
pricesWrongOrdered<-pricesTemp2[2521:1,]
simpleScale<-(pricesWrongOrdered[-2521,]/pricesWrongOrdered[-1,])
simpleScale<-simpleScale[2520:1,]
pricesCleaned[2,3]/pricesCleaned[1,3]
simpleScale[2,3]

#We then compute net price weights 
enlargedWeightsXTS<-cbind(data.frame(as.Date(dates[253:2512])),netMatrix)
enlargedWeightsXTS<-xts(enlargedWeightsXTS[,2:3259],order.by=enlargedWeightsXTS[,1])
rawLogReturnsXTS<-cbind(data.frame(as.Date(dates[(253):(2512)])),logReturnsCleaned[253:2512,])
rawLogReturnsXTS<-xts(rawLogReturnsXTS[,2:3259],order.by=rawLogReturnsXTS[,1])
rawLogReturnsXTS[1,]

equalWeightsXTS<-cbind(data.frame(as.Date(dates[(253):(2512)])),netMatrix)
equalWeightsXTS<-xts(equalWeightsXTS[,2:3259],order.by=equalWeightsXTS[,1])
outEqual <- Return.portfolio(R = rawLogReturnsXTS, weights = equalWeightsXTS, rebalance_on = "daily",verbose = T)
bopEqual <- outEqual$BOP.Weight
eopEqual <- outEqual$EOP.Weight
fEqual<-abs(bopEqual-eopEqual)
turnoverEqual=sum(fEqual)/2260
turnoverEqual  

#This calculates actual returns with transaction costs

sum(abs(as.matrix(eopEqual[(((loopcount-1)*20)+1),])-as.matrix(eopEqual[((loopcount-1)*20),])))
myPortfolioEqual<-outEqual$returns
myPortfolioEqual2<-myPortfolioEqual
for(i in 2:2259){
  print(i)
  temp<-sum(abs(as.matrix(eopEqual[i,])-as.matrix(eopEqual[i-1,])))
  myPortfolioEqual2[i]=(1-0.0005*temp)*(1+myPortfolioEqual[i])-1
}

#We obtain a matrix of stock names to check our results and identity the types of assets
stockNames<-c()
for(i in 1:113){
  tempStockNames<-matrix(colnames(logReturnsTemp1)[topStocks[i,]],nrow=1,ncol=30)
  stockNames<-rbind(stockNames,tempStockNames)
}
stockNames[80,]

write(stockNames, "stockNames.csv")

#Section III (Equal): We now plot our actual results and compare them to the S&P 500
#This is a time series for the equal portfolio less transaction costs
myPortfolioEqualXTS<-myPortfolioEqual
myPortfolioEqualXTS[2259,]
cumEqualPortfolioXTS<-cumsum(myPortfolioEqualXTS)
cumEqualPortfolioXTS[2259,]
plot(cumEqualPortfolioXTS, main="Equally Weighted Portfolio",xlab="Time",ylab="logreturns*1000" )

#This is a time series for the equal portfolio with transaction costs
myPortfolioEqualXTS2<-myPortfolioEqual2
myPortfolioEqualXTS2[2259,]
cumEqualPortfolioXTS2<-cumsum(myPortfolioEqualXTS2)
cumEqualPortfolioXTS2[2259,]
plot(cumEqualPortfolioXTS2, main="Equally Weighted Portfolio With Transaction Costs",xlab="Time",ylab="logreturns*1000" )

cumEqualPortfolioXTSMerged<-merge.xts(cumEqualPortfolioXTS,cumEqualPortfolioXTS2)

#Section III (S&P 500): Now we plot the returns against the S&P 500
SP500XTS<-cbind(data.frame(as.Date(dates[253:2511])),as.numeric(SP500[253:2511,2]))
SP500XTS<-xts(SP500XTS[,2],order.by=SP500XTS[,1])
cumSP500XTS<-cumsum(SP500XTS)
cumSP500XTS[2259,]
plot(cumSP500XTS, main="S&P 500",xlab="Time",ylab="logreturns*1000" )
allTwo<-merge.xts(cumSP500XTS,cumEqualPortfolioXTS)
allThree<-merge.xts(cumSP500XTS,cumEqualPortfolioXTSMerged)
plot(allTwo, main="S&P 500 and Equal Weights",xlab="Time",ylab="logreturns*1000")
plot(allThree, main="All Portfolios",xlab="Time",ylab="logreturns*1000")
#We see that, as expected, transaction costs for a 20-day rolling window exercise reduces actual returns
#for an "outperforming" portfolio towards the market performance

#ARIMA-BL Portfolio #####################################################################################################
#Section I.1.1 (ARIMA): ARIMA for Priors, Black-Litterman Model for Portfolio Optimization
#Here we use an ARIMA model to forecast time t+1:t+20 log Returns for our 30 stocks in each rolling window
#We then compile our forecasted data to be used for calculation of priors in the Black-Litterman Model
#NOTE: In practice, if we calculating the weights today, we would only have forecast data for the next 20 days
#The segmented rolling window exercise means that we will have forecast data for all 2260 days,
#But a close examination of the procedure in Section I.1.2 (ARIMA) will confirm that the process is not forward looking
count=0
loopcount=1
for(i in 252:2520){
  print(i)
  if(count==20){
    tempReturns<-data.frame(logReturnsCleaned[(i-251):(i),c(topStocks[loopcount,])])
    tempReturns<-cbind(data.frame(as.Date(dates)),tempReturns)
    tempReturnsXTS<-xts(tempReturns[ ,2], order.by=tempReturns[,1])
    tempFit<-auto.arima(tempReturnsXTS)
    predictedReturns<-tempFit %>% forecast(h=20)
    predictedReturns<-data.frame(predictedReturns)
    for(k in 2:30){
      print(k+100)
      tempReturnsXTS<-xts(tempReturns[ ,(k+1)], order.by=tempReturns[,1])
      tempFit<-auto.arima(tempReturnsXTS)
      predictedReturns<-tempFit %>% forecast(h=20)
      predictedReturns<-data.frame(predictedReturns)
      tempPredictedReturns<-predictedReturns
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
totalReturns
predictedTotalReturns<-matrix(unlist(totalReturns),nrow=2260,ncol=30)

#Section I.1.2 (ARIMA): Input the ARIMA Priors into Black-Litterman Model
#Here, we use forecasted 20-day log returns to calculate weights for the next 20 days using the Black-Litterman Model
#A Close examination of the if loop will show that this process is not forward looking.
count=20
loopcount=1
for(i in 0:2260){
  print(i)
  if(count==20){
    annualizedOm<-cov(predictedTotalReturns[(i+1):(i+20),])*252/20
    annualizedQ<-colMeans(predictedTotalReturns[(i+1):(i+20),])*252
    annualizedMiu<-colMeans(logReturnsCleaned[(i+1):(i+252),c(topStocks[loopcount,])])
    annualizedSigma<-cov(logReturnsCleaned[(i+1):(i+252),c(topStocks[loopcount,])])
    aMiu<-matrix(annualizedMiu,nrow=30,ncol=1)
    aSigma<-as.matrix(annualizedSigma)
    aQ<-matrix(annualizedQ,nrow=30,ncol=1)
    aOm<-as.matrix(annualizedOm)
    aP<-diag(x=1,nrow=30,ncol=30)
    miuBL<-solve(solve(aSigma)+t(aP)%*%aOm%*%aP)%*%(solve(aSigma)%*%aMiu+t(aP)%*%aOm%*%aQ)
    sigmaBL<-solve(solve(aSigma)+t(aP)%*%t(aOm)%*%aP)
    arimaBLPortfolio<-efficient.portfolio(er=miuBL,cov.mat = sigmaBL, target.return = 0)
    tempArimaBLWeights<-matrix(unlist(arimaBLPortfolio[4]),nrow=1,ncol=30)
    if(loopcount==1){
      arimaBLWeights <- tempArimaBLWeights
    }else{
      arimaBLWeights<-rbind(arimaBLWeights,tempArimaBLWeights)
    }
    count=0
    loopcount=loopcount+1
  }
  count=count+1
  if(i==2259){
    break
  }
}
dim(arimaBLWeights)

#Section II (ARIMA): We calculate the actual returns for our asset selection. This is not a forward-looking process for
#the reasons stated above, which we direct the reader to re-read in the note under Step 4.2

#This creates a matrix of weights for calculation of transaction costs
netMatrixARIMA<-matrix(0,nrow=2260,ncol=3258)
count=20
loopcount=1
for(i in 0:2260){
  print(i)
  if(count==20){
    for(h in 1:20){
      netMatrixARIMA[i+h,c(topStocks[loopcount,])]=arimaBLWeights[loopcount,]
    }
    count=0
    loopcount=loopcount+1
  }
  count=count+1
  if(loopcount==114){
    break
  }
}
dim(netMatrixARIMA)

#We write our table of weights and view it offline to check that we do indeed have a desired 2260X3258 matrix of weights
write.csv(netMatrixARIMA,"netMatrixARIMA.csv")

#We then compute net price weights 
ARIMAWeightsXTS<-cbind(data.frame(as.Date(dates[(253):(2512)])),netMatrixARIMA)
ARIMAWeightsXTS<-xts(ARIMAWeightsXTS[,2:3259],order.by=ARIMAWeightsXTS[,1])
outARIMA <- Return.portfolio(R = rawLogReturnsXTS, weights = ARIMAWeightsXTS, rebalance_on = "daily",verbose = T)
bopARIMA <- outARIMA$BOP.Weight
eopARIMA <- outARIMA$EOP.Weight
fARIMA<-abs(bopARIMA-eopARIMA)
turnoverARIMA=sum(fARIMA)/2260
turnoverARIMA  

#This calculates actual returns with transaction costs

myPortfolioARIMA<-outARIMA$returns
myPortfolioARIMA2<-myPortfolioARIMA
for(i in 2:2259){
  print(i)
  temp<-sum(abs(as.matrix(eopARIMA[i,])-as.matrix(eopARIMA[i-1,])))
  myPortfolioARIMA2[i]=(1-0.0005*temp)*(1+myPortfolioARIMA[i])-1
}

#Section III (ARIMA): We now plot our actual results and compare them to the S&P 500
#This is a time series for the ARIMA portfolio less transaction costs
myPortfolioARIMAXTS<-myPortfolioARIMA
myPortfolioARIMAXTS[2259,]
cumARIMAPortfolioXTS<-cumsum(myPortfolioARIMAXTS)
cumARIMAPortfolioXTS[2259,]
plot(cumARIMAPortfolioXTS, main="ARIMA-BL Portfolio",xlab="Time",ylab="logreturns*1000" )

#This is a time series for the equal portfolio with transaction costs
myPortfolioARIMAXTS2<-myPortfolioARIMA2
myPortfolioARIMAXTS2[2259,]
cumARIMAPortfolioXTS2<-cumsum(myPortfolioARIMAXTS2)
cumARIMAPortfolioXTS2[2259,]
plot(cumARIMAPortfolioXTS2, main="ARIMA-BL Portfolio With Transaction Costs",xlab="Time",ylab="logreturns*1000" )
allThreeARIMA<-merge.xts(cumSP500XTS,cumARIMAPortfolioXTS,cumARIMAPortfolioXTS2)
plot(allThreeARIMA, main="S&P 500 and ARIMA Weights",xlab="Time",ylab="logreturns*1000")

cumARIMAPortfolioXTSMerged<-merge.xts(cumARIMAPortfolioXTS,cumARIMAPortfolioXTS2)

allFive<-merge.xts(allThree,cumARIMAPortfolioXTSMerged)
plot(allFive, main="All Portfolios",xlab="Time",ylab="logreturns*1000")

#ARMA-GARCH-BL Portfolio ###############################################################################################
#Section I.1.1 (ARMA-GARCH): ARMA-GARCH for Priors, Black-Litterman Model for Portfolio Optimization
#Here we use an ARMA-GARCH model to forecast time t+1:t+20 log Returns for our 30 stocks in each rolling window
#We then compile our forecasted data to be used for calculation of priors in the Black-Litterman Model
#NOTE: In practice, if we calculating the weights today, we would only have forecast data for the next 20 days
#The segmented rolling window exercise means that we will have forecast data for all 2260 days,
#But a close examination of the procedure in Section I.1.2 (ARMA-GARCH) will confirm that the process is not forward looking
count=20
loopcount=1
for(i in 252:2520){
  if(count==20){
    print(i)
    model <- ugarchspec(variance.model=list(model='sGARCH', garchOrder=c(1, 1)), mean.model=list(armaOrder=c(1, 1)), distribution.model='std')
    tempReturns<-data.frame(logReturnsCleaned[(i-251):(i),which(netMatrix[loopcount,]>0)])
    tempReturns<-cbind(data.frame(as.Date(dates[(i-251):(i)])),tempReturns)
    tempReturnsXTS<-xts(tempReturns[ ,2], order.by=tempReturns[,1])
    tryCatch({model_fit <- ugarchfit(spec=model, data=tempReturnsXTS, solver='solnp')},
             warning=function(e) model_fit <- ugarchfit(spec=model, data=tempReturnsXTS, solver='hybrid'))
    tempFitAGarch<- ugarchforecast(model_fit, data = NULL, n.ahead = 20, n.roll= 0, out.sample = 0)
    predReturnsAGarch<-fitted(tempFitAGarch)
    predReturnsAGarch<-data.frame(predReturnsAGarch)
    for(k in 2:30){
      print(k+100)
      tempReturnsXTS<-xts(tempReturns[ ,(k+1)], order.by=tempReturns[,1])
      tryCatch({model_fit <- ugarchfit(spec=model, data=tempReturnsXTS, solver='solnp')},
               warning=function(e) model_fit <- ugarchfit(spec=model, data=tempReturnsXTS, solver='hybrid'))
      tempFitAGarch<- ugarchforecast(model_fit, data = NULL, n.ahead = 20, n.roll= 0, out.sample = 0)
      tempPredReturnsAGarch<-fitted(tempFitAGarch)
      tempPredReturnsAGarch<-data.frame(tempPredReturnsAGarch)
      predReturnsAGarch<-cbind(predReturnsAGarch,tempPredReturnsAGarch)
    }
      if(loopcount==1){
        totalReturnsAGarch2<-matrix(predReturnsAGarch,nrow=20,ncol=30)
      }else{
        tempTotalRet<-matrix(predReturnsAGarch,nrow=20,ncol=30)
        totalReturnsAGarch2<-rbind(totalReturnsAGarch2,tempTotalRet)
      }
      loopcount=loopcount+1
      count=0
  }
  count=count+1
}
loopcount
dim(totalReturnsAGarch2)
count=20
loopcount=1
for(i in 1952:2520){
  if(count==20){
    print(i)
    model <- ugarchspec(variance.model=list(model='sGARCH', garchOrder=c(1, 1)), mean.model=list(armaOrder=c(1, 1)), distribution.model='std')
    tempReturns<-data.frame(logReturnsCleaned[(i-251):(i),which(netMatrix[loopcount,]>0)])
    tempReturns<-cbind(data.frame(as.Date(dates[(i-251):(i)])),tempReturns)
    tempReturnsXTS<-xts(tempReturns[ ,2], order.by=tempReturns[,1])
    tryCatch({model_fit <- ugarchfit(spec=model, data=tempReturnsXTS, solver='solnp')},
             warning=function(e) model_fit <- ugarchfit(spec=model, data=tempReturnsXTS, solver='hybrid'))
    tempFitAGarch<- ugarchforecast(model_fit, data = NULL, n.ahead = 20, n.roll= 0, out.sample = 0)
    predReturnsAGarch<-fitted(tempFitAGarch)
    predReturnsAGarch<-data.frame(predReturnsAGarch)
    for(k in 2:30){
      print(k+100)
      tempReturnsXTS<-xts(tempReturns[ ,(k+1)], order.by=tempReturns[,1])
      tryCatch({model_fit <- ugarchfit(spec=model, data=tempReturnsXTS, solver='solnp')},
               warning=function(e) model_fit <- ugarchfit(spec=model, data=tempReturnsXTS, solver='hybrid'))
      tempFitAGarch<- ugarchforecast(model_fit, data = NULL, n.ahead = 20, n.roll= 0, out.sample = 0)
      tempPredReturnsAGarch<-fitted(tempFitAGarch)
      tempPredReturnsAGarch<-data.frame(tempPredReturnsAGarch)
      predReturnsAGarch<-cbind(predReturnsAGarch,tempPredReturnsAGarch)
    }
    if(loopcount==1){
      totalReturnsAGarch<-rbind(totalReturnsAGarch2,tempTotalRet)
    }else{
      tempTotalRet<-matrix(predReturnsAGarch,nrow=20,ncol=30)
      totalReturnsAGarch<-rbind(totalReturnsAGarch,tempTotalRet)
    }
    loopcount=loopcount+1
    count=0
  }
  count=count+1
}
totalReturnsAGarch3<-totalReturnsAGarch[1:2260,]
dim(totalReturnsAGarch3)
predictedTotalReturnsAGarch<-matrix(unlist(totalReturnsAGarch3),nrow=2260,ncol=30)

#Section I.1.2 (ARMA-GARCH): Input the ARMA-GARCH Priors into Black-Litterman Model
#Here, we use forecasted 20-day log returns to calculate weights for the next 20 days using the Black-Litterman Model
#A Close examination of the if loop will show that this process is not forward looking.
count=20
loopcount=1
for(i in 0:2260){
  print(i)
  if(count==20){
    annualizedOm<-cov(predictedTotalReturnsAGarch[(i+1):(i+20),])*252/20
    annualizedQ<-colMeans(predictedTotalReturnsAGarch[(i+1):(i+20),])*252
    annualizedMiu<-colMeans(logReturnsCleaned[(i+1):(i+252),c(topStocks[loopcount,])])
    annualizedSigma<-cov(logReturnsCleaned[(i+1):(i+252),c(topStocks[loopcount,])])
    aMiu<-matrix(annualizedMiu,nrow=30,ncol=1)
    aSigma<-as.matrix(annualizedSigma)
    aQ<-matrix(annualizedQ,nrow=30,ncol=1)
    aOm<-as.matrix(annualizedOm)
    aP<-diag(x=1,nrow=30,ncol=30)
    miuBL<-solve(solve(aSigma)+t(aP)%*%aOm%*%aP)%*%(solve(aSigma)%*%aMiu+t(aP)%*%aOm%*%aQ)
    sigmaBL<-solve(solve(aSigma)+t(aP)%*%t(aOm)%*%aP)
    armaBLPortfolio<-efficient.portfolio(er=miuBL,cov.mat = sigmaBL, target.return = 0)
    tempArmaBLWeights<-matrix(unlist(armaBLPortfolio[4]),nrow=1,ncol=30)
    if(loopcount==1){
      armaBLWeights <- tempArmaBLWeights
    }else{
      armaBLWeights<-rbind(armaBLWeights,tempArmaBLWeights)
    }
    count=0
    loopcount=loopcount+1
  }
  count=count+1
  if(i==2259){
    break
  }
}
dim(armaBLWeights)

#Section II (ARMA-GARCH): We calculate the actual returns for our asset selection. This is not a forward-looking process for
#the reasons stated above, which we direct the reader to re-read in the note under Step 4.2

#This creates a matrix of weights for calculation of transaction costs
netMatrixARMA<-matrix(0,nrow=2260,ncol=3258)
count=20
loopcount=1
for(i in 0:2260){
  print(i)
  if(count==20){
    for(h in 1:20){
      netMatrixARMA[i+h,c(topStocks[loopcount,])]=armaBLWeights[loopcount,]
    }
    count=0
    loopcount=loopcount+1
  }
  count=count+1
  if(loopcount==114){
    break
  }
}
dim(netMatrixARMA)

#We write our table of weights and view it offline to check that we do indeed have a desired 2260X3258 matrix of weights
write.csv(netMatrixARMA,"netMatrixARMA.csv")

#We then compute net price weights 
ARMAWeightsXTS<-cbind(data.frame(as.Date(dates[(253):(2512)])),netMatrixARMA)
ARMAWeightsXTS<-xts(ARMAWeightsXTS[,2:3259],order.by=ARMAWeightsXTS[,1])
outARMA <- Return.portfolio(R = rawLogReturnsXTS, weights = ARMAWeightsXTS, rebalance_on = "daily",verbose = T)
bopARMA <- outARMA$BOP.Weight
eopARMA <- outARMA$EOP.Weight
fARMA<-abs(bopARMA-eopARMA)
turnoverARMA=sum(fARMA)/2260
turnoverARMA  

#This calculates actual returns with transaction costs
myPortfolioARMA<-outARMA$returns
myPortfolioARMA2<-myPortfolioARMA
for(i in 2:2259){
  print(i)
  temp<-sum(abs(as.matrix(eopARMA[i,])-as.matrix(eopARMA[i-1,])))
  myPortfolioARMA2[i]=(1-0.0005*temp)*(1+myPortfolioARMA[i])-1
}

#Section III (ARMA-GARCH): We now plot our actual results and compare them to the S&P 500
#This is a time series for the ARMA-GARCH portfolio less transaction costs
myPortfolioARMAXTS<-myPortfolioARMA
myPortfolioARMAXTS[2259,]
cumARMAPortfolioXTS<-cumsum(myPortfolioARMAXTS)
cumARMAPortfolioXTS[2259,]
plot(cumARMAPortfolioXTS, main="ARMA-GARCH-BL Portfolio",xlab="Time",ylab="logreturns*1000" )

#This is a time series for the ARMA-GARCH portfolio with transaction costs
myPortfolioARMAXTS2<-myPortfolioARMA2
myPortfolioARMAXTS2[2259,]
cumARMAPortfolioXTS2<-cumsum(myPortfolioARMAXTS2)
cumARMAPortfolioXTS2[2259,]
plot(cumARMAPortfolioXTS2, main="ARMA-GARCH-BL Portfolio With Transaction Costs",xlab="Time",ylab="logreturns*1000" )
allThreeARMA<-merge.xts(cumSP500XTS,cumARMAPortfolioXTS,cumARMAPortfolioXTS2)
plot(allThreeARMA, main="S&P 500 and ARMA Weights",xlab="Time",ylab="logreturns*1000")

cumARMAPortfolioXTSMerged<-merge.xts(cumARMAPortfolioXTS,cumARMAPortfolioXTS2)

allSeven<-merge.xts(allFive,cumARMAPortfolioXTSMerged)
plot(allSeven, main="All Portfolios",xlab="Time",ylab="logreturns*1000")

#ARFIMA Portfolio ###############################################################################################
#Section I.1.1 (ARFIMA): ARFIMA for Priors, Black-Litterman Model for Portfolio Optimization
#Here we use an ARFIMA model with frac diff 0.6 to forecast time t+1:t+20 log Returns for our 30 stocks in each rolling window
#We then compile our forecasted data to be used for calculation of priors in the Black-Litterman Model
#NOTE: In practice, if we calculating the weights today, we would only have forecast data for the next 20 days
#The segmented rolling window exercise means that we will have forecast data for all 2260 days,
#But a close examination of the procedure in Section I.1.2 (ARFIMA) will confirm that the process is not forward looking
count=0
loopcount=1
for(i in 252:2520){
  print(i)
  if(count==20){
    i=252
    tempReturnsARFIMA<-data.frame(logReturnsCleaned[(i-251):(i),c(topStocks[loopcount,])])
    tempReturnsARFIMA<-cbind(data.frame(as.Date(dates[(i-251):(i)])),tempReturnsARFIMA)
    tempReturnsXTSARFIMA<-tempReturnsARFIMA[ ,2]
    tempFitARFIMA<-arfima(tempReturnsXTSARFIMA, order=c(1,0,1))
    predictedReturns<-sim_from_fitted(20,tempFitARFIMA)
    predictedReturnsARFIMA<-data.frame(predictedReturns)
    for(k in 2:30){
      print(k+100)
      tempReturnsXTSARFIMA<-tempReturnsARFIMA[ ,(k+1)]
      tempFitARFIMA<-arfima(tempReturnsXTSARFIMA, order=c(1,0,1))
      predictedReturns<-sim_from_fitted(20,tempFitARFIMA)
      predictedReturns<-data.frame(predictedReturns)
      tempPredictedReturnsARFIMA<-predictedReturns
      tempPredictedReturnsARFIMA<-data.frame(tempPredictedReturnsARFIMA)
      predictedReturnsARFIMA<-cbind(predictedReturnsARFIMA,tempPredictedReturnsARFIMA)
    }
    if(loopcount==1){
      totalReturnsARFIMA<-matrix(predictedReturnsARFIMA,nrow=20,ncol=30)
    }else{
      tempTotalReturnsARFIMA<-matrix(predictedReturnsARFIMA,nrow=20,ncol=30)
      totalReturnsARFIMA<-rbind(totalReturnsARFIMA,tempTotalReturnsARFIMA)
    }
    loopcount=loopcount+1
    count=0
  }
  count=count+1
} 
totalReturnsARFIMA
predictedTotalReturnsARFIMA<-matrix(unlist(totalReturnsARFIMA),nrow=2260,ncol=30)

#Section I.1.2 (ARFIMA): Input the ARFIMA Priors into Black-Litterman Model
#Here, we use forecasted 20-day log returns to calculate weights for the next 20 days using the Black-Litterman Model
#A Close examination of the if loop will show that this process is not forward looking.
count=20
loopcount=1
for(i in 0:2260){
  print(i)
  if(count==20){
    annualizedOm<-cov(predictedTotalReturnsARFIMA[(i+1):(i+20),])*252/20
    annualizedQ<-colMeans(predictedTotalReturnsARFIMA[(i+1):(i+20),])*252
    annualizedMiu<-colMeans(logReturnsCleaned[(i+1):(i+252),c(topStocks[loopcount,])])
    annualizedSigma<-cov(logReturnsCleaned[(i+1):(i+252),c(topStocks[loopcount,])])
    aMiu<-matrix(annualizedMiu,nrow=30,ncol=1)
    aSigma<-as.matrix(annualizedSigma)
    aQ<-matrix(annualizedQ,nrow=30,ncol=1)
    aOm<-as.matrix(annualizedOm)
    aP<-diag(x=1,nrow=30,ncol=30)
    miuBL<-solve(solve(aSigma)+t(aP)%*%aOm%*%aP)%*%(solve(aSigma)%*%aMiu+t(aP)%*%aOm%*%aQ)
    sigmaBL<-solve(solve(aSigma)+t(aP)%*%t(aOm)%*%aP)
    arfimaBLPortfolio<-efficient.portfolio(er=miuBL,cov.mat = sigmaBL, target.return = 0)
    tempArfimaBLWeights<-matrix(unlist(arfimaBLPortfolio[4]),nrow=1,ncol=30)
    if(loopcount==1){
      arfimaBLWeights <- tempArfimaBLWeights
    }else{
      arfimaBLWeights<-rbind(arfimaBLWeights,tempArfimaBLWeights)
    }
    count=0
    loopcount=loopcount+1
  }
  count=count+1
  if(i==2259){
    break
  }
}
dim(arfimaBLWeights)

#Section II (ARFIMA): We calculate the actual returns for our asset selection. This is not a forward-looking process for
#the reasons stated above, which we direct the reader to re-read in the note under Step 4.2

#This creates a matrix of weights for calculation of transaction costs
netMatrixARFIMA<-matrix(0,nrow=2260,ncol=3258)
count=20
loopcount=1
for(i in 0:2260){
  print(i)
  if(count==20){
    for(h in 1:20){
      netMatrixARFIMA[i+h,c(topStocks[loopcount,])]=arfimaBLWeights[loopcount,]
    }
    count=0
    loopcount=loopcount+1
  }
  count=count+1
  if(loopcount==114){
    break
  }
}
dim(netMatrixARFIMA)

#We write our table of weights and view it offline to check that we do indeed have a desired 2260X3258 matrix of weights
write.csv(netMatrixARFIMA,"netMatrixARFIMA.csv")

#We then compute net price weights 
ARFIMAWeightsXTS<-cbind(data.frame(as.Date(dates[(253):(2512)])),netMatrixARFIMA)
ARFIMAWeightsXTS<-xts(ARFIMAWeightsXTS[,2:3259],order.by=ARFIMAWeightsXTS[,1])
outARFIMA <- Return.portfolio(R = rawLogReturnsXTS, weights = ARFIMAWeightsXTS, rebalance_on = "daily",verbose = T)
bopARFIMA <- outARFIMA$BOP.Weight
eopARFIMA <- outARFIMA$EOP.Weight
fARFIMA<-abs(bopARFIMA-eopARFIMA)
turnoverARFIMA=sum(fARFIMA)/2260
turnoverARFIMA  

#This calculates actual returns with transaction costs
myPortfolioARFIMA<-outARFIMA$returns
myPortfolioARFIMA2<-myPortfolioARFIMA
for(i in 2:2259){
  print(i)
  temp<-sum(abs(as.matrix(eopARFIMA[i,])-as.matrix(eopARFIMA[i-1,])))
  myPortfolioARFIMA2[i]=(1-0.0005*temp)*(1+myPortfolioARFIMA[i])-1
}

#Section III (ARFIMA): We now plot our actual results and compare them to the S&P 500
#This is a time series for the ARFIMA portfolio less transaction costs
myPortfolioARFIMAXTS<-myPortfolioARFIMA
myPortfolioARFIMAXTS[2259,]
cumARFIMAPortfolioXTS<-cumsum(myPortfolioARFIMAXTS)
cumARFIMAPortfolioXTS[2259,]
plot(cumARFIMAPortfolioXTS, main="ARFIMA-GARCH-BL Portfolio",xlab="Time",ylab="logreturns*1000" )

#This is a time series for the ARFIMA portfolio with transaction costs
myPortfolioARFIMAXTS2<-myPortfolioARFIMA2
myPortfolioARFIMAXTS2[2259,]
cumARFIMAPortfolioXTS2<-cumsum(myPortfolioARFIMAXTS2)
cumARFIMAPortfolioXTS2[2259,]
plot(cumARFIMAPortfolioXTS2, main="ARFIMA-GARCH-BL Portfolio With Transaction Costs",xlab="Time",ylab="logreturns*1000" )
allThreeARFIMA<-merge.xts(cumSP500XTS,cumARFIMAPortfolioXTS,cumARFIMAPortfolioXTS2)
plot(allThreeARFIMA, main="S&P 500 and ARFIMA Weights",xlab="Time",ylab="logreturns*1000")

cumARFIMAPortfolioXTSMerged<-merge.xts(cumARFIMAPortfolioXTS,cumARFIMAPortfolioXTS2)
allNine<-merge.xts(allSeven,cumARFIMAPortfolioXTSMerged)
plot(allNine, main="All Portfolios",xlab="Time",ylab="logreturns*1000")

#Step 5: Evaluating the Portfolio--------------------------------------------------------------------------------------
#Step 5.1 (Equal)-------------------------------------------------------------------------------------------------------
#Section I: Without Transaction Costs
meanReturnEqual<-252 * mean(myPortfolioEqual)
volatilityEqual<-sqrt(252) * sd(myPortfolioEqual)
sharpeRatioEqual<-SharpeRatio.annualized(myPortfolioEqualXTS)
sortinoRatioEqual<-sqrt(252) * PerformanceAnalytics::SortinoRatio(myPortfolioEqualXTS)
maxDrawdownEqual<- -1 * PerformanceAnalytics::maxDrawdown(myPortfolioEqualXTS, geometric = FALSE)
turnoverEqualAnn<-turnoverEqual*252/20

#Section II: With Transaction Costs
meanReturnEqual2<-252 * mean(myPortfolioEqual2)
volatilityEqual2<-sqrt(252) * sd(myPortfolioEqual2)
sharpeRatioEqual2<-SharpeRatio.annualized(myPortfolioEqualXTS2)
sortinoRatioEqual2<-sqrt(252) * PerformanceAnalytics::SortinoRatio(myPortfolioEqualXTS2)
maxDrawdownEqual2<- -1 * PerformanceAnalytics::maxDrawdown(myPortfolioEqualXTS2, geometric = FALSE)
turnoverEqualAnn2<-turnoverEqual*252/20

#Step 5.2 (S&P 500)-------------------------------------------------------------------------------------------------------
meanReturnSP500<-252 * mean(SP500XTS)
volatilitySP500<-sqrt(252) * sd(SP500XTS)
sharpeRatioSP500<-SharpeRatio.annualized(SP500XTS)
sortinoRatioSP500<-sqrt(252) * PerformanceAnalytics::SortinoRatio(SP500XTS)
maxDrawdownSP500<- -1 * PerformanceAnalytics::maxDrawdown(SP500XTS, geometric = FALSE)
turnoverSP500<-0

#Step 5.3 (ARIMA)-------------------------------------------------------------------------------------------------------
#Section I: Without Transaction Costs
meanReturnARIMA<-252 * mean(myPortfolioARIMA)
volatilityARIMA<-sqrt(252) * sd(myPortfolioARIMA)
sharpeRatioARIMA<-SharpeRatio.annualized(myPortfolioARIMAXTS)
sortinoRatioARIMA<-sqrt(252) * PerformanceAnalytics::SortinoRatio(myPortfolioARIMAXTS)
maxDrawdownARIMA<- -1 * PerformanceAnalytics::maxDrawdown(myPortfolioARIMAXTS, geometric = FALSE)
turnoverARIMAAnn<-turnoverARIMA*252/20

#Section II: With Transaction Costs
meanReturnARIMA2<-252 * mean(myPortfolioARIMA2)
volatilityARIMA2<-sqrt(252) * sd(myPortfolioARIMA2)
sharpeRatioARIMA2<-SharpeRatio.annualized(myPortfolioARIMAXTS2)
sortinoRatioARIMA2<-sqrt(252) * PerformanceAnalytics::SortinoRatio(myPortfolioARIMAXTS2)
maxDrawdownARIMA2<- -1 * PerformanceAnalytics::maxDrawdown(myPortfolioARIMAXTS2, geometric = FALSE)
turnoverARIMAAnn2<-turnoverARIMA*252/20

#Step 5.4 (ARMA-GARCH)-------------------------------------------------------------------------------------------------------
#Section I: Without Transaction Costs
meanReturnARMA<-252 * mean(myPortfolioARMA)
volatilityARMA<-sqrt(252) * sd(myPortfolioARMA)
sharpeRatioARMA<-SharpeRatio.annualized(myPortfolioARMAXTS)
sortinoRatioARMA<-sqrt(252) * PerformanceAnalytics::SortinoRatio(myPortfolioARMAXTS)
maxDrawdownARMA<- -1 * PerformanceAnalytics::maxDrawdown(myPortfolioARMAXTS, geometric = FALSE)
turnoverARMAAnn<-turnoverARMA*252/20

#Section II: With Transaction Costs
meanReturnARMA2<-252 * mean(myPortfolioARMA2)
volatilityARMA2<-sqrt(252) * sd(myPortfolioARMA2)
sharpeRatioARMA2<-SharpeRatio.annualized(myPortfolioARMAXTS2)
sortinoRatioARMA2<-sqrt(252) * PerformanceAnalytics::SortinoRatio(myPortfolioARMAXTS2)
maxDrawdownARMA2<- -1 * PerformanceAnalytics::maxDrawdown(myPortfolioARMAXTS2, geometric = FALSE)
turnoverARMAAnn2<-turnoverARMA*252/20

#Step 5.5 (ARFIMA)-------------------------------------------------------------------------------------------------------
#Section I: Without Transaction Costs
meanReturnARFIMA<-252 * mean(myPortfolioARFIMA)
volatilityARFIMA<-sqrt(252) * sd(myPortfolioARFIMA)
sharpeRatioARFIMA<-SharpeRatio.annualized(myPortfolioARFIMAXTS)
sortinoRatioARFIMA<-sqrt(252) * PerformanceAnalytics::SortinoRatio(myPortfolioARFIMAXTS)
maxDrawdownARFIMA<- -1 * PerformanceAnalytics::maxDrawdown(myPortfolioARFIMAXTS, geometric = FALSE)
turnoverARFIMAAnn<-turnoverARFIMA*252/20

#Section II: With Transaction Costs
meanReturnARFIMA2<-252 * mean(myPortfolioARFIMA2)
volatilityARFIMA2<-sqrt(252) * sd(myPortfolioARFIMA2)
sharpeRatioARFIMA2<-SharpeRatio.annualized(myPortfolioARFIMAXTS2)
sortinoRatioARFIMA2<-sqrt(252) * PerformanceAnalytics::SortinoRatio(myPortfolioARFIMAXTS2)
maxDrawdownARFIMA2<- -1 * PerformanceAnalytics::maxDrawdown(myPortfolioARFIMAXTS2, geometric = FALSE)
turnoverARFIMAAnn2<-turnoverARFIMA*252/20

#Tabling the Data
print("The following are all annualized")
myData <- matrix(c(meanReturnEqual,volatilityEqual,sharpeRatioEqual,sortinoRatioEqual,maxDrawdownEqual,
                   turnoverEqualAnn,meanReturnEqual2,volatilityEqual2,sharpeRatioEqual2,sortinoRatioEqual2,
                   maxDrawdownEqual2,turnoverEqualAnn2,meanReturnARIMA,volatilityARIMA,sharpeRatioARIMA,
                   sortinoRatioARIMA,maxDrawdownARIMA,turnoverARIMAAnn,meanReturnARIMA2,volatilityARIMA2,sharpeRatioARIMA2,
                   sortinoRatioARIMA2,maxDrawdownARIMA2,turnoverARIMAAnn2,meanReturnARMA,volatilityARMA,
                   sharpeRatioARMA,sortinoRatioARMA,maxDrawdownARMA,turnoverARMAAnn,meanReturnARMA2,volatilityARMA2,
                   sharpeRatioARMA2,sortinoRatioARMA2,maxDrawdownARMA2,turnoverARMAAnn2,meanReturnSP500,
                   volatilitySP500,sharpeRatioSP500,sortinoRatioSP500,maxDrawdownSP500,turnoverSP500,meanReturnARFIMA,volatilityARFIMA,
                   sharpeRatioARFIMA,sortinoRatioARFIMA,maxDrawdownARFIMA,turnoverARFIMAAnn,meanReturnARFIMA2,volatilityARFIMA2,
                   sharpeRatioARFIMA2,sortinoRatioARFIMA2,maxDrawdownARFIMA2,turnoverARFIMAAnn2),
                   nrow=9,ncol=6,byrow=TRUE)
colnames(myData) <- c("Mean Returns","Volatility","Sharpe Ratio","Sortino Ratio","Max Drawdown","Turnover")
rownames(myData) <- c("Equal Weights","Equal Weights w T Cost","ARIMA BL Model","ARIMA BL Model w T Cost",
                     "ARMA-GARCH BL Model","ARMA-GARCH BL Model w T Cost","S&P 500","ARFIMA BL Model", "ARFIMA BL Model w T Cost")
myData <- as.table(myData)
myData
write.csv(myData,"myData.csv")
