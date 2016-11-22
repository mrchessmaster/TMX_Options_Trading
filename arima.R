library(quantmod)
library(forecast)

con = file("symbols.txt",open="r")
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

for (i in 1:length(symbol)) {
  foresee(symbol[i],10)
}


