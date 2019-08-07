# 12.1
library(quantmod)
symbols <- c("SPY", "AGG", "VEA", "EDV", "VGLT", "VGSH")

getSymbols(symbols, src = "yahoo")
prices <- do.call(merge,
                  lapply(symbols, function(x) Cl(get(x))))
colnames(prices) <- symbols
summary(prices)

tickers <- c("RU1000TR", "DGS10", "DTWEXM", "MSIALLP")
getSymbols(tickers, src="FRED")

ret <- to.monthly(RU1000TR, indexAt='firstof', OHLC=FALSE)
ret <- na.omit(cbind(DGS10, ret))
ret <- na.omit(cbind(DTWEXM, ret))
ret <- na.omit(cbind(MSIALLP, ret))
ret <- ret["1986::2016"]

ret < -ROC(ret, 1, "discrete", na.pad=FALSE)
summary(ret)

library(Quandl) 
oil <- Quandl("NSE/OIL",,type="xts",
              start_date="2013-01-01", collapse="monthly")
summary(oil)