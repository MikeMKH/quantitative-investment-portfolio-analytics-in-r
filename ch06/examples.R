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

# 6.3
library(quantmod)
library(MASS)
library(PerformanceAnalytics)
data("managers")
ret <- managers[,8]
ret.t.model <- fitdistr(ret, "t")
ret.t.model

set.seed(23)
t.ret <- rt(100000, df=13) * 0.01
summary(t.ret)
matplot(t.ret, type='l')

set.seed(77)
norm.ret <- rnorm(100000, mean=0.05/252, sd=0.15/sqrt(252))
summary(norm.ret)
matplot(norm.ret, type='l')

library(PerformanceAnalytics)
t.kurt <- kurtosis(t.ret)
norm.kurt <- kurtosis(norm.ret)

# t is larger than normal
t.kurt
norm.kurt

set.seed(77)
norm.ret <- rnorm(100, mean=0.05/252, sd=0.15/sqrt(252))
prices <- cumprod(1+norm.ret)
plot(prices,type='l', main="Wealth Index")

library(mvtnorm)
library(QRM)
library(PerformanceAnalytics)
data(managers)
fit <- fit.mst(managers[,8:10])
mu <- fit$mu
sigma <- as.matrix(fit$Sigma)
nu <- fit$df
n <- nrow(managers[,8:10]) 
fit

set.seed(198)
sim.dat <- rmvt(n=n, sigma=sigma, df=nu, delta=mu)
sim.dat <- xts(sim.dat, as.Date(index(managers[,8:10])))
summary(sim.dat)
matplot(sim.dat, type='l')

# 6.4
library(PerformanceAnalytics)
data(managers)
ret <- managers[,c(1,3,4,8)]
colnames(ret) <-c("HF-A", "HF-B", "HF-C", "SP500")
summary(ret)
matplot(ret, type='l')

fit <- lm(ret$SP500 ~ ., data=ret)
fit

f <-function(model, indices, x) {
  i <- indices[x,]
  o <- lm(model, data=i)
  return(coef(o))
}

library(boot)
set.seed(61)
results <- boot(data=ret, statistic=f, R=1000, equation=SP500 ~ .)
results

results.ci <-boot.ci(results, type="basic", index=2)
results.ci
plot(results, index=2)

# boot examples
library(boot)
?city
summary(city)
pairs(city)

ratio <- function(d, w) sum(d$x * w) / sum(d$u * w)
city.boot <- boot(city, ratio, R=99, stype="w", sim="ordinary")
city.boot
city.ci <- boot.ci(city.boot, conf=c(.90, .95),
                   type=c("norm", "basic", "perc", "bca"))
city.ci
boot.array(city.boot)

perm.cor <- function(d, i) cor(d$x, d$u[i])
city.perm <- boot(city, perm.cor, R=99, sim="permutation")
city.perm
city.perm.ci <- boot.ci(city.perm, conf=c(.90, .95),
                        type=c("norm", "basic", "perc"))
city.perm.ci
boot.array(city.perm, indices=TRUE)

?gravity
summary(gravity)
pairs(gravity)

diff.means <- function(d, f) {
  n <- nrow(d)
  gp1 <- 1:table(as.numeric(d$series))[1]
  m1 <- sum(d[gp1,1] * f[gp1])/sum(f[gp1])
  m2 <- sum(d[-gp1,1] * f[-gp1])/sum(f[-gp1])
  ss1 <- sum(d[gp1,1]^2 * f[gp1]) - (m1 *  m1 * sum(f[gp1]))
  ss2 <- sum(d[-gp1,1]^2 * f[-gp1]) - (m2 *  m2 * sum(f[-gp1]))
  c(m1 - m2, (ss1 + ss2)/(sum(f) - 2))
}
g <- gravity[as.numeric(gravity[,2]) >= 7,]
g.boot <- boot(g, diff.means, R=99, stype="f", strata=g[,2])
g.boot
g.ci <- boot.ci(g.boot, type=c("stud", "norm"))
g.ci
