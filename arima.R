library(quantmod)
library(forecast)

setwd("~/Desktop/Finance/UBCFA_Trading_Competition")
con = file("names.txt",open="r")
symbol = readLines(con)

foresee <- function(name, days) {
  curStock <- getSymbols(name,auto.assign = F)
  price = curStock[,4]
  price = price[(length(price)-10*days):length(price)]
  
  ts_price = ts(price, frequency=1)
  fit = auto.arima(ts_price,stepwise=F,approximation=F)
  fc = forecast(fit,h=days,level=c(75,85,95))
  
  plot(fc)
  title(sub=name,xlab = "Day",ylab = "Price")
}

setwd("~/Desktop/Finance/UBCFA_Trading_Competition/arima_plots_tse")
for (i in 1:length(symbol)) {
  jpeg(file = paste(symbol[i], ".jpeg", sep=""))
  foresee(symbol[i],10)
  dev.off()
}


pdf(file = "~/Desktop/Finance/UBCFA_Trading_Competition/arima_plots_tse/nov22.pdf")
par(mfrow=c(2,1))
for (i in 1:length(symbol)) {
  foresee(symbol[i],10)
}
dev.off()





curStock <- getSymbols("HBM.TO",auto.assign = F)
price = curStock[,4]
price = price[(length(price)-10*10):(length(price))]

ts_price = ts(price, frequency=1)
fit = auto.arima(ts_price,stepwise=F,approximation=F)
fc = forecast(fit,h=10,level=c(75,85,95))

plot(fc)
title(sub=name,xlab = "Day",ylab = "Price")

