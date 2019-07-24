# 6.1
library(PerformanceAnalytics)
data(managers)
ret <- managers$SP500
summary(ret)

set.seed(994)
ret.sim.1 <- sample(ret, replace=TRUE)
summary(ret.sim.1)

ret.all <-cbind(ret, as.numeric(ret.sim.1))
matplot(ret.all, type='l')

set.seed(645)
ret.sim.2 <-sample(as.numeric(ret), size=1000, replace=TRUE)
summary(ret.sim.2)

library(PerformanceAnalytics)
data(managers)
ret <-managers[,8:10]
summary(ret)

set.seed(44)
ret.sim <-apply(ret, 2, function(x) sample(x, replace=TRUE))
ret.sim.1 <-xts(ret.sim, order.by=as.Date(index(ret)))
summary(ret.sim.1)
matplot(ret.sim.1, type='l')

wgt <-c(0.6, 0.3, 0.1)
f <-function(i, j) {
  r <-apply(i, 2, function(x) sample(x, replace=TRUE))
  r.1 <-xts(r, as.Date(index(i)))
  
  p <-Return.portfolio(r.1, rebalance_on="years",
                       weights=j, wealth.index=TRUE)
}

set.seed(48)
port.sim.1 <-f(ret, wgt)
summary(port.sim.1)
matplot(port.sim.1, type='l')

set.seed(48)
port.sim <-do.call(cbind, lapply(1:5, function(x) f(ret, wgt)))
summary(port.sim)
matplot(port.sim, type='l')

port.actual <-Return.portfolio(ret, rebalance_on="years",
                               weights=wgt, wealth.index=TRUE)
summary(port.actual)
matplot(port.actual, type='l')

port.all <-cbind(port.actual, port.sim)
matplot(port.all[,2:6], col="gray", type='l',
        ylab="", main="Wealth Indexes")
par(new=TRUE)
matplot(port.all[,1], col="black", type='l',
        yaxt="n", xaxt="n", ylab="", lwd=2)
legend("bottomright", c("simulated", "actual"), fill=c("gray", "black"))

# 6.2
set.seed(401)
ret <- rnorm(100, mean=0.05, sd=0.10)
prices <-cumprod(1 + ret)
summary(ret)
summary(prices)

prices <-cumprod(c(66.8, 1+ret))
summary(prices)

library(PerformanceAnalytics)
data(managers)
mean <-apply(managers[,8], 2, mean)
sd <-apply(managers[,8], 2, sd)

set.seed(49)
sim <-rnorm(100, mean=mean, sd=sd) 
summary(sim)
matplot(sim, type='l')

set.seed(49)
sim.1 <-xts(rnorm(nrow(managers[,8]), mean=mean, sd=sd),
            as.Date(index(managers[,8])))
summary(sim.1)
matplot(sim.1, type='l')

library(PerformanceAnalytics)
library(MASS)
set.seed(782)
assets.sim <-mvrnorm(n=nrow(managers[,8:10]),
                     mu=colMeans(managers[,8:10]),
                     Sigma=var(managers[,8:10]))
summary(assets.sim)

colMeans(managers[,8:10])
colMeans(assets.sim)

apply(managers[,8:10], 2, sd)
apply(assets.sim, 2, sd)
summary(assets.sim)
matplot(assets.sim, type='l')