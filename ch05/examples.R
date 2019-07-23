# 5.1
library(xts)
download.file("http://bit.ly/2ikMUxn",
              destfile="F-F_Research_Data_Factors.zip", mode='wb')
unzip("F-F_Research_Data_Factors.zip")
ff <- read.delim('F-F_Research_Data_Factors.txt',
                 sep="", nrows=1067,
                 header=FALSE, skip=4, stringsAsFactors=FALSE)

names(ff) <- c("Date", "MKT", "SMB", "HML", "RF")
summary(ff)

ff.3 <-ff[,2:4]
dates.1 <-as.yearmon(as.character(ff$Date), "%Y%m")
ff.dates <-xts(as.matrix(ff.3), as.Date(dates.1))
summary(ff)

library(PerformanceAnalytics)
data(managers)
ret <-to.monthly(managers[,c(8,10)], indexAt='firstof', OHLC=F)
diff <-ret[,1] - ret[,2]
colnames(diff) <-c("SP500")
summary(diff)

dates <- as.Date(index(diff))
subset <- ff.dates[paste0(dates)] * 0.01
ff.factors <- cbind(subset, diff)
summary(ff.factors)

fit <-lm(SP500 ~ ., data = ff.factors)
summary(fit)

estimate <- fit$coefficients[1] +
  fit$coefficient[2] * ff.factors$MKT +
  fit$coefficient[3] * ff.factors$SMB +
  fit$coefficient[4] * ff.factors$HML

plot(as.numeric(diff), 
     as.numeric(estimate),
     main="Fama French 3-factor model for S&P 500",
     ylab="estimated premia", xlab="historical premia")
grid()


library(PerformanceAnalytics)
data(managers)
ret <- na.omit(managers[,1:6])
summary(ret)

fit <- factanal(ret, factors=1)
fit

pc <- prcomp(ret)
plot(pc)
print(summary(pc), digits=3)

wgt <- apply(pc$rotation, 2, function(x) x/sum(x))
round(wgt, 4)
sum(wgt[,1]) == 1
wgt

library(quantmod)
symbols <- c("SPY", "AGG", "EFA", "EEM", "OIL", "QQQ", "GOOG")
getSymbols(symbols, src="yahoo", auto.assign=TRUE)
prices <- do.call(merge, lapply(symbols, function(x) Cl(get(x))))
colnames(prices) <- symbols
summary(prices)

ret <- na.omit(ROC(prices['2007-12-31::2016-12-31'],
                   1, "discrete", na.pad = FALSE))
summary(ret)

fit <- prcomp(ret)
summary(fit)

round(fit$rotation, 3)

wgt <- apply(fit$rotation, 2, function(x) x/sum(x))
round(wgt[,1], 2)