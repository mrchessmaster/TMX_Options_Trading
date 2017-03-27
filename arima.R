library(quantmod)
library(forecast)
library(RCurl)
library(dplyr)
library(stringr)

setwd("~/Desktop/Finance/UBCFA_Trading_Competition")
#con = file("names.txt",open="r")
#symbol = readLines(con)
url = "ftp://ftp.nasdaqtrader.com/SymbolDirectory/otherlisted.txt"
filenames <- getURL(url)
rows <- strsplit(filenames,"\r\n")
rows <- as.data.frame(rows)
rows <- as.data.frame(rows[-c(1,5175),])
names(rows)[1] = "temp"
rows$temp = as.character(rows$temp)
dat <- str_split_fixed(rows$temp,"\\|",8)



curStock <- getSymbols("HIG",auto.assign = F)
price = curStock[,4]
price = price[(length(price)-10*3):length(price)]

ts_price = ts(price, frequency=1)
fit = auto.arima(ts_price,stepwise=F,approximation=F)
fc = forecast(fit,h=3,level=c(75,85,95))
fc$lower[3,1]


#foresee no graph
foresee_no_graph <- function(name,days) {
  curStock <- getSymbols(name,auto.assign = F)
  price = curStock[,4]
  if (10*days < length(price)) {
    price = price[(length(price)-10*days):length(price)]
  } else {
    price = price[1:length(price)]
  }
  curPrice = as.numeric(price[length(price)])
  
  ts_price = ts(price, frequency=1)
  fit = auto.arima(ts_price,stepwise=F,approximation=F)
  fc = forecast(fit,h=days,level=c(75,85,95))
  
  if (as.numeric(fc$lower[days,1]) >= curPrice) {
    if (fc$mean[days] >= 1.05*curPrice) {
      return(name)
    }
  }
  return("")
}

#foresee all no graph
goodStocks <- c()
for (i in dat[,1]) {
  if (foresee_no_graph(i,7) != "") 
    goodStocks <- append(goodStocks, foresee_no_graph(i,7))
}




# with graph
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


# individual plots JPEG:
setwd("~/Desktop/Finance/UBCFA_Trading_Competition/arima_plots_tse")
for (i in 1:length(symbol)) {
  jpeg(file = paste(symbol[i], ".jpeg", sep=""))
  foresee(symbol[i],10)
  dev.off()
}


# PDF with all plots
# symbol <- c("ECA.TO","VET.TO","TCW.TO","TCK-B.TO", "POU.TO", "VRX.TO", "NGD.TO","HBM.TO","POW.TO","GWO.TO","SLF.TO")
pdf(file = "~/Desktop/Finance/UBCFA_Trading_Competition/arima_plots_tse/nov22_7days.pdf")
par(mfrow=c(2,1))
for (i in 1:length(symbol)) {
  foresee(symbol[i],7)
}
dev.off()




#------ useless

curStock <- getSymbols("HBM.TO",auto.assign = F)
price = curStock[,4]
price = price[(length(price)-10*10):(length(price))]

ts_price = ts(price, frequency=1)
fit = auto.arima(ts_price,stepwise=F,approximation=F)
fc = forecast(fit,h=10,level=c(75,85,95))

plot(fc)
title(sub=name,xlab = "Day",ylab = "Price")

