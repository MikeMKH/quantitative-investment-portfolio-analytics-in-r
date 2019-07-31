# 8.2
library(FRAPO)
library(quantmod)
symbols <- c("SPY", "AGG", "EFA", "EEM", "GLD")
getSymbols(symbols, src = "yahoo", auto.assign=T)
prices <- do.call(merge, lapply(symbols, function(x) Cl(get(x))))
colnames(prices) <-symbols 

returns <-na.omit(ROC(prices['2007-12-31::2017-12-31'], 
                      1, "continuous", na.pad = FALSE))
summary(returns)
covar <- cov(returns)
risk.parity.w <- PERC(covar)
risk.parity.w

detach("package:FRAPO", unload=TRUE)
library(quantmod)
library(PortfolioAnalytics)
library(ROI)
library(quadprog)
library(ROI.plugin.quadprog)
funds <- colnames(returns)
port <- portfolio.spec(asset=funds)
port <- add.constraint(portfolio=port, type="leverage",
                       min_sum=0.99, max_sum=1.01)
port <- add.constraint(portfolio=port, type="long_only")
port <- add.objective(portfolio=port, type="risk_budget",
                      min_concentration=TRUE, name="StdDev")

port.w <- optimize.portfolio(R=returns, portfolio=port,
                             optimize_method="quadprog", trace=T)

round(extractWeights(port.w)*100,3)

# 8.3
library(quantmod)
library(PerformanceAnalytics)

getSymbols("VT", src = "yahoo", auto.assign=T)
vt <- Cl(VT["2010-12-31::2015-12-31"])
summary(vt)

vol.target.w <-function(x, y) {
  a <-ROC(x, 1, "discrete", na.pad=F)
  b <-rollapply(a, width=90, FUN=sd) * sqrt(252)
  c <-as.matrix(y/b)
  d <-1/NCOL(x)
  e <-as.xts(ifelse(c >d, d, c))
  cash <-as.xts(1-(apply(e, MARGIN=1, FUN=sum)))
  f <-merge(e, cash)
}

vt.5per.vol  <- na.omit(vol.target.w(vt,0.05)) 
vt.10per.vol <- na.omit(vol.target.w(vt,0.10))

w.5per  <- to.period(vt.5per.vol, period='months', indexAt='lastof',
                     OHLC=FALSE)
w.10per <- to.period(vt.10per.vol, period='months', indexAt='lastof', 
                     OHLC=FALSE)

vt.cash.ret <-cbind(ROC(vt, 1, "discrete", na.pad=FALSE), 0)
colnames(vt.cash.ret) <-c("vt", "cash")

port.w.5per  <- Return.portfolio(vt.cash.ret, weights=w.5per,
                                 wealth.index=TRUE, verbose=TRUE)$wealthindex
colnames(port.w.5per) <- c("vol.target.5per")

port.w.10per <- Return.portfolio(vt.cash.ret, weights=w.10per,
                                wealth.index=TRUE, verbose=TRUE)$wealthindex
colnames(port.w.10per) <- c("vol.target.10per")

vt.port <-cumprod(1 + vt.cash.ret$vt["2011-06-01/"])
summary(vt.port)

port <-cbind(vt.port, port.w.10per, port.w.5per)
matplot(port, main = "VT vs. Volatility Target Strategies",
        ylab = "", xlab = "",  type = "l", lwd = 2,
        lty = c(1, 1, 3), col=c("black", "gray50", "black"))

legend("bottomright", ncol=1, lwd=2, c(colnames(port)),
       col=c("black", "gray50", "black"), lty=c(1, 1, 3))

port.ret <- ROC(port, 1, "discrete", na.pad=FALSE)
port.vol <-apply(port.ret, 2, sd) * sqrt(252)
port.vol
