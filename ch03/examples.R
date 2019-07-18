# 3.1
library(PerformanceAnalytics)
data(managers)
summary(managers)

sp500.10yrT.ret <-managers[,8:9]

wgt <-c(0.70, 0.30)
port.rebal <- Return.portfolio(sp500.10yrT.ret, rebalance_on="monthly",
                               weights=wgt, wealth.index=TRUE)

port.norebal <- Return.portfolio(sp500.10yrT.ret,
                                 weights=wgt,wealth.index=TRUE)

mat <- cbind(port.rebal, port.norebal)
colnames(mat) <-c("Rebal", "No Rebal")

matplot(mat,
        main="Wealth Indexes",
        ylab="",
        xlab="",
        type="l",
        lty=1,
        col=c("black", "blue"))
legend(lty=c(1),"topleft",
       c(colnames(mat)),col=c("black", "blue"))
legend("bottomright", c("Dec. 31, 1995 = 1.0"))

library(TTR)
ret <- ROC(mat, n=1, type="discrete", na.pad=FALSE)
port.ret <- Return.annualized(ret , scale=12)
port.sd <- apply(ret , 2, sd) * sqrt(12)
port.sr <- SharpeRatio.annualized(ret)
port.sortino <- SortinoRatio(ret)

risk.ret <- round(rbind(port.ret, port.sd,
                        port.sr, port.sortino), 3)
rownames(risk.ret) <- c("returns:",
                        "volatility:",
                        "Sharpe Ratio:",
                        "Sortino Ratio")
risk.ret

# 3.2
prices <- apply(managers[,8:9], 2, function(x) cumprod(1+x))
prices.xts <- xts(prices,  order.by=index(managers[,8:9]))
sig.1 <- prices.xts / rollmean(prices.xts, k=10, align=c("right"))
sig.2 <- apply(sig.1, 2, function(x) ifelse(x >1, yes=1, no=0))
sig.3 <- xts(sig.2, as.Date(index(tail(managers, nrow(sig.2)))))
sig.lag <- na.omit(lag(sig.3, k=1))

strategy.ret <- na.omit(managers[,8:9] * sig.lag)
strategy.wi <- cumprod(1 + strategy.ret)
strategy.wgt <- sweep(strategy.wi, MARGIN=2, STATS=wgt, FUN="*")
taa.wi <- xts(apply(strategy.wgt, 1, sum),
              as.Date(index(strategy.wi)))

wi.3.ret <- na.omit(ROC(cbind(port.rebal, port.norebal, taa.wi),
                        1, "discrete"))
wi.3 <- xts(apply(wi.3.ret, 2, function(x) cumprod(1+x)),
            as.Date(index(wi.3.ret)))
colnames(wi.3) <- c("Rebal", "No Rebal", "Tactical")
wi.3

matplot(wi.3,
        main="Wealth Indexes",
        ylab="",
        xlab="",    
        type="l",
        col=c("black", "blue", "red"))
legend(lty=c(1,1,3),"topleft", 
       c(colnames(wi.3)), 
       col=c("black", "blue", "red"))
legend("bottomright", c("Nov. 30, 1996 = 1.0"))

wi.3.monthly.ret <- ROC(wi.3, n=1, type="discrete", na.pad=FALSE)
wi.3.annl.ret <- Return.annualized(wi.3.monthly.ret , scale=12)
wi.3.sd <- apply(wi.3.monthly.ret , 2, sd) * sqrt(12)
wi.3.sr <- SharpeRatio.annualized(wi.3.monthly.ret)
wi.3.sortino <- SortinoRatio(wi.3.monthly.ret)

risk.ret.wi.3 <-round(
  rbind(wi.3.annl.ret, wi.3.sd, wi.3.sr, wi.3.sortino), 4)
rownames(risk.ret.wi.3) <-c("return:",
                            "volatility:",
                            "Sharpe Ratio:",
                            "Sortino Ratio")
risk.ret.wi.3