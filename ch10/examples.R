# 10.1
library(vars)
library(quantmod)

tickers <-c("WILL5000INDFC", "MCOILWTICO")
getSymbols(tickers, src="FRED")

ret <- to.monthly(WILL5000INDFC, indexAt='firstof', OHLC=FALSE)
ret <- na.omit(cbind(MCOILWTICO, ret))
ret <- ret["1986::2016"]

ret <-ROC(ret,1, "discrete", na.pad=FALSE)
colnames(ret) <-c("oil", "stocks")
summary(ret)

lag <-VARselect(ret)
lag

model <-VAR(ret*100, p=lag$selection[1])
model

irf.oil.stocks <- irf(model, impulse="oil", response="stocks",
                      boot=TRUE, n.ahead=12, cumulative=FALSE)
irf.oil.stocks
plot(irf.oil.stocks, main="Impulse Response From Oil")
irf.oil.stocks$irf
irf.oil.stocks$Upper
irf.oil.stocks$Lower

summary(irf.oil.stocks$irf$oil)      # 1 std dev shock
summary(irf.oil.stocks$irf$oil * 2)  # 2 std dev shock
summary(irf.oil.stocks$irf$oil * -1) # -1 std dev shock

tickers <-c("WILL5000INDFC", "MZMV")
getSymbols(tickers, src="FRED")

ret <- to.monthly(WILL5000INDFC, indexAt='firstof', OHLC=FALSE)
ret <- na.omit(cbind(MZMV, ret))
ret <- ret["1975::2018"]

ret <-ROC(ret,1, "discrete", na.pad=FALSE)
colnames(ret) <-c("velocity-MZM", "stocks")
summary(ret)

lag <-VARselect(ret)
lag

model <-VAR(ret*100, p=lag$selection[1])
model
plot(irf(model, impulse="velocity.MZM", response="stocks",
         boot=TRUE, n.ahead=12, cumulative=FALSE),
     main="Impulse Response From Change in Money Supply")