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