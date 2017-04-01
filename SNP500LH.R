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
plot(volest, type="l",xlab = "Index", ylab = "Volatility Estimate")
lines(volest2,type="l",col="red")
lines(volest3,type="l",col="blue")
legend("topleft",lty=1, col=c(1,"black","red","blue"),
       c(expression(decay==10), expression(decay==30),
         expression(decay==100)),pch=1)
dev.off()

#render markdown file
library(knitr)
library(rmarkdown)
rmarkdown::render("SNP500Report.md")