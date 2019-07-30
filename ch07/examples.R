# 7.1
alpha <- c(0.05, 0.01)
VaR.1 <- qnorm(p=alpha, mean=0.07, sd=0.15) 
VaR.1

VaR.1.alt <-(0.07 + 0.15 * qnorm(p=alpha))
VaR.1.alt

VaR.1 == VaR.1.alt

prob.1 <-pnorm(VaR.1[1], mean=0.07, sd=0.15)
prob.1

es.1 <-(0.07 + 0.15 * dnorm(qnorm(alpha))/alpha)
es.1

alpha <-seq(0.05, 0.001, length=1000)
conf.l <-1-alpha
VaR.a <-(0.07 + 0.15 * qnorm(alpha))
ES.a <-(0.07 + 0.15 * dnorm(qnorm(alpha))/(alpha)) * -1
VaR.ES.a <-c(VaR.a, ES.a)

plot(conf.l, VaR.a, type="l", 
     main="Value at Risk & Expected Shortfall",
     cex.main=0.9, col="black",
     ylim=range(VaR.ES.a),
     xlab="confidence level", ylab="loss", lwd=2)
grid()
lines(conf.l, ES.a, lty=2, col="black", lwd=2)
legend("bottomleft", legend=c("VaR","ES"), col="black",
       lty=c(1, 2))

library(MASS)
library(PerformanceAnalytics)
data(managers)
ret <-managers$SP500
summary(ret)

fit.norm <-fitdistr(ret, "normal") 
alpha <-0.05
mu <- fit.norm$estimate[1]
sigma <-fit.norm$estimate[2]
fit.norm

VaR.norm <-(mu + sigma * qnorm(alpha))
names(VaR.norm) <-c("VaR-norm")
VaR.norm

fit.t <-fitdistr(ret, "t")
alpha <-0.05
mu <- fit.t$estimate[1]
sigma <-fit.t$estimate[2] / 100
nu <-fit.t$estimate[3]
fit.t

VaR.t <-sigma * qt(df=nu, p=0.05)
names(VaR.t) <-c("VaR-t")
VaR.t

hist(ret, breaks=50,
     freq=FALSE, 
     main="Distribution of S&P 500 Returns")
alpha <-0.05

abline(v=VaR.norm, lwd=2, lty=1)
abline(v=VaR.t, lwd=2, lty=3)

legend("topleft", legend=c("VaR.norm","VaR.t"), col="black",
       lty=c(1, 2))
box()

chart.BarVaR(ret, legend.loc="topleft",
             methods = c("HistoricalES"),
             main="S&P 500 Returns and ES",  
             lty = c(2),
             legend.cex = 0.8,width=12)

dev.off()

# 7.2
library(quantmod)
library(PerformanceAnalytics)
symbols <-c("VFINX", "VFITX")
getSymbols(symbols, src='yahoo', from = '1991-12-31')
prices <- do.call(merge, lapply(symbols, function(x) Cl(get(x))))
colnames(prices) <-c(symbols)
summary(prices)

ret <-ROC(prices["/2017"], 1, "discrete", na.pad = F)
summary(ret)

w = c(0.60, 0.40)
port <- Return.portfolio(ret, rebalance_on = "years", weights = w,
                         wealth.index = TRUE, verbose = TRUE)
port.ret <-ROC(port$wealthindex, 1, "discrete", na.pad = F)

hist(port.ret,
     freq = F,
     breaks = 100,
     col = 'gray95',
     xlab = "return",
     main = "Distribution of 60/40 Returns",
     panel.first = grid() )
curve(dnorm(x, mean(port.ret), sd(port.ret)),
      add = TRUE, col = "black", lwd = 2)
legend("topright", c("empirical", "normal"), fill = c("gray95", "black"))
box()

plot.ecdf(as.numeric(port.ret), main = "CDF Chart",
          xlab = "return", col="gray50", lwd = 2, panel.first = grid() )
curve(pnorm(x, mean(port.ret), sd(port.ret) ),
      add = TRUE, lwd = 2, col='black')
mtext(side = 5, "based on daily returns for 60/40 strategy", line = 0.2)
legend("topleft",c("empirical","normal"), fill = c("gray","black"))
abline(v=0, h=0.5, lty=2)
box()

plot.ecdf(as.numeric(port.ret),
          main = "Left Tail: CDF Chart", xlab = "return",
          xlim = c(-0.05, -0.01), ylim = c(0, 0.04),
          lwd = 2, cex.points=1.5, verticals=TRUE,
          panel.first = grid())
curve(pnorm(x, mean(port.ret), sd(port.ret)), 
      add = TRUE, lwd = 2, lty = 2, col = 'black')
legend("topleft",c("empirical","normal"), lty=c(1, 2), bg="white")
box()

stats::qqnorm(port.ret, panel.first = grid() ); qqline(port.ret, col = 'black')
stats::qqline(port.ret, col = 'black')
abline(h = -0.01, lty=2)

library(fExtremes)
gpd.fit <- gpdFit(as.numeric(port.ret), u=-0.01) 
gpd.fit
tailRisk(gpd.fit, prob=0.99)

var.hist <-PerformanceAnalytics::VaR(port.ret, p = 0.99,
                                     method = c("historical"))
es.hist <-PerformanceAnalytics::ES(port.ret, p = 0.99,
                                   method = c("historical"))
var.hist
es.hist

library(fExtremes)
gpd.fit <-gpdFit(as.numeric(port.ret), u=-0.01) 

set.seed(83)
gpd.sim <-rgpd(1000000, xi = gpd.fit@fit$par.ests[1],
               beta = gpd.fit@fit$par.ests[2])
summary(gpd.sim)

library(MASS)
truehist(gpd.sim * -1,
         main = paste("Simulated Left-Tail Density Histogram"),
         ylim = c(0, 1), xlim = c(-0.06, -0.035),
         xlab = c("estimated daily loss"),
         col = "gray", panel.first = grid())
box()