# 2.1
library(PerformanceAnalytics)
data("managers")
sp500.ret <- managers$`SP500 TR`

plot(sp500.ret * 100,
     main="S&P 500: 1-month % returns",
     ylab="return", xlab="date")

sp500.index <- cumprod(1 + sp500.ret)
plot(sp500.index,
     main="S&P 500 Index",
     ylab="price", xlab="date")

sp500.index <-cumprod(c(100, 1 + sp500.ret))
plot(sp500.index,
     main="S&P 500 Index",
     ylab="price", xlab="date")

library(TTR)
sp500.ret <-ROC(sp500.index, 1, "discrete", na.pad=FALSE)

data.test <-cbind(managers$SP500, sp500.ret)
colnames(data.test)[2] <-c("sp500.ret.ttr")
head(data.test)

sp500.12mo.ret <-ROC(sp500.index, n=12, "discrete", na.pad=FALSE)

# 2.2
sp500.10yrT.ret <-managers[,8:9] * 100
matplot(sp500.10yrT.ret,
        main="S&P 500 vs. 10-Year Treasury",
        ylab="% return", type='l')

# 2.3
vol <-apply(managers[,8:9], MARGIN=2, FUN=sd)
vol.annualized <-vol * sqrt(12)
vol.roll <-na.omit(
  rollapply(data=managers[,8:9], width=12, FUN=sd) * sqrt(12))
matplot(vol.roll,
        main="S&P 500 vs. 10-Year Treasury",
        ylab="rolling sd", type='l')

corr.matrix <-cor(na.omit(managers[,8:9]))
corr.matrix

# 2.4
ret <- managers$`US 10Y TR`
round(StdDev(ret), 5)
round(StdDev(ret, clean=c("geltner")), 5)
round(StdDev.annualized(ret), 5)
round(SharpeRatio.annualized(ret, Rf=0.01/12), 3)

# 2.5
round(SortinoRatio(ret), 3)

# 2.6
CAPM.beta(Ra=managers$`EDHEC LS EQ`, Rb=managers$`US 10Y TR`)
CAPM.beta(Ra=managers$`EDHEC LS EQ`, Rb=managers$`SP500 TR`)

coef(lm(formula=managers$EDHEC ~ managers$SP500))

# 2.9
table.Drawdowns(managers$SP500, top=3)
table.Drawdowns(managers$`US 10Y TR`, top=3)

library(timeSeries)
par(mfrow=c(2,1))

dd.history <- drawdowns(timeSeries(managers$SP500))
plot(dd.history * 100,
     main="S&P 500 Drawdowns: 1996-2006",
     ylab="drawdown %", type="l")

dd.history <- drawdowns(timeSeries(managers$`US 10Y TR`))
plot(dd.history * 100,
     main="US 10yr Tresuary Drawdowns: 1996-2006",
     ylab="drawdown %", type="l")