---
title: "Standard and Poor's 500 - Index Volatility"
author: "Laurie Harris"
date: "April 3, 2017"
output: html_document
---


##Data Analysis Available
This report and accompaning analysis files are available at www.github.com/llh1000/Assignment11

##Standard and Poor's 500 Index
The Standard and Poors 500 Index (SNP500) is widely used a benchmark for the United States stock market.  Analyzing patterns and trends in this index can offer insight into the US stock market and national economy. We will examine the volatility of the SNP500 using three decay factors.

##Volatility Analysis and Computations
The data for the SNP 500 was obtained by getting the historical quotes beginning 1/2/1991 through 3/27/2017.  The returns were computed using logged data and the volatility was calculated using those results.  For the volatility estimates we have selected three different decay factors including 10, 30, and 100.  


```r
#install package if not already installed
#install.packages("tseries")

#invoke tseries package with library command
library(tseries)

#download data of SNP 500
SNPdata <- get.hist.quote("^gspc",quote="Close")

#calculate log returns
SNPret <- log(lag(SNPdata))-log(SNPdata)

#Calculate volatility measurement for logged returns
SNPvol <- sd(SNPret)*sqrt(250)*100

#Use function for volatility calculations
getVol <- function(d, logrets) {
var = 0
lam = 0
varlist <- c()
for (r in logrets) {
lam = lam*(1 - 1/d) + 1
var = (1 - 1/lam)*var + (1/lam)*r^2
varlist <- c(varlist, var)
}
sqrt(varlist)
}

#Calculate volatility over entire series using three different decay factors (10,30,100)
volest <- getVol(10,SNPret)
volest2 <- getVol(30,SNPret)
volest3 <- getVol(100,SNPret)

#plot volatility using the three different decay factors and save plot for report output
jpeg("volplot.jpg")
plot(volest,type="l",xlab = "Index", ylab = "Volatility Estimate")
lines(volest2,type="l",col="red")
lines(volest3,type="l",col="blue")
legend("topleft",lty=1, col=c(1,"black","red","blue"),
       c(expression(decay==10), expression(decay==30),
         expression(decay==100)),pch=1)
dev.off()
```

##Volitility Estimates
The plot below shows the three volatility estimates graphically.

![](volplot.jpg)
