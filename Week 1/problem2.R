setwd("~/GitHub/AnalyticsEdge_week1_problem2")

## reading the data
IBM <- read.csv("IBMStock.csv")
GE <- read.csv("GEStock.csv")
ProcterGamble <- read.csv("ProcterGambleStock.csv")
CocaCola <- read.csv("CocaColaStock.csv")
Boeing <- read.csv("BoeingStock.csv")

## 1.1
IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

## 1.2
summary(IBM)
summary(GE)

## 1.4 What is the mean stock price of IBM over this time period?
mean(IBM$StockPrice)

## 1.5 What is the minimum stock price of General Electric (GE) over this time period?
min(GE$StockPrice)

## 1.6 What is the maximum stock price of Coca-Cola over this time period?
max(CocaCola$StockPrice)

## 1.7 What is the median stock price of Boeing over this time period?
median(Boeing$StockPrice)

## 1.8 What is the standard deviation of the stock price of Procter & Gamble over this time period?
sd(ProcterGamble$StockPrice)

## 1.9 plot the Date on the x-axis and the StockPrice on the y-axis, for Coca-Cola
plot(CocaCola$Date, CocaCola$StockPrice, type="l")

##2.2 In March of 2000, the technology bubble burst, and a stock market crash occurred. According to this plot, which company's stock dropped more?
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col="red")
abline(v=as.Date(c("2000-03-01")), lwd=2)

## 2.3 Around 1983, the stock for one of these companies (Coca-Cola or Procter and Gamble) was going up, while the other was going down. Which one was going up?
##     In the time period shown in the plot, which stock generally has lower values?
abline(v=as.Date(c("1983-06-01")), lwd=2)

## 3.1 Which stock fell the most right after the technology bubble burst in March 2000?
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], col="blue")
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], col="orange")
lines(IBM$Date[301:432], IBM$StockPrice[301:432], col="green")
lines(GE$Date[301:432], GE$StockPrice[301:432], col="purple")
abline(v=as.Date(c("2000-03-01")), lwd=2)

## 3.3 Comparing September 1997 to November 1997, which companies saw a decreasing trend in their stock price?
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], col="blue")
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], col="orange")
lines(IBM$Date[301:432], IBM$StockPrice[301:432], col="green")
lines(GE$Date[301:432], GE$StockPrice[301:432], col="purple")
abline(v=as.Date(c("1997-09-01")), lwd=1)
abline(v=as.Date(c("1997-11-30")), lwd=1)

## 4.1 In which months has IBM historically had a higher stock price (on average)?
tapply(IBM$StockPrice - mean(IBM$StockPrice), months(IBM$Date), sum)

## 4.2 General Electric and Coca-Cola both have their highest average stock price in the same month. Which month is this?
tapply(CocaCola$StockPrice - mean(CocaCola$StockPrice), months(CocaCola$Date), sum)
tapply(GE$StockPrice - mean(GE$StockPrice), months(GE$Date), sum)