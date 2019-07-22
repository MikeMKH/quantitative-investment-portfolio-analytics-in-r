# 4.1
library(PerformanceAnalytics)
data(managers)
?managers

returns <- na.omit(managers[,5:10])
summary(returns)

library(quadprog)
cov.mat <- cov(returns)
n <- ncol(cov.mat)
zero.mat <- array(0, dim=c(n,1))
one.mat <- matrix (1, nrow=n)
bvec.1 <- 1
meq.1 <- 1

port <-solve.QP(Dmat=cov.mat, dvec=zero.mat, Amat=one.mat,
                bvec=bvec.1, meq=meq.1)
port.wgt <- round(as.matrix(port$solution) * 100, digits=2)
rownames(port.wgt) <-c(names(returns))
colnames(port.wgt) <-c("MVP % weights")
port.wgt

one.zero.diagonal <- cbind(1, diag(n))
bvec.1.vector <- as.matrix(c(1, rep(0, n)))

port.noshort <- solve.QP(Dmat=cov.mat, dvec=zero.mat, Amat=one.zero.diagonal, 
                         bvec=bvec.1.vector, meq=meq.1)
port.noshort.wgt <- round(as.matrix(port.noshort$solution) * 100, digits=2)
rownames(port.noshort.wgt) <-c(names(returns))
colnames(port.noshort.wgt) <-c("MVP % weights")
port.noshort.wgt

one.zero.diagonal.lmt <- cbind(1, diag(n), 1*diag(n), -1*diag(n))
min.wgt <-rep(0.15, n)
max.wgt <-rep(0.50, n)

bvec.1.vector.lmt <- c(1, rep(0, n), min.wgt, -max.wgt)

port.noshort.lmt <-solve.QP(Dmat=cov.mat, dvec=zero.mat, meq=meq.1,
                            Amat=one.zero.diagonal.lmt,
                            bvec=bvec.1.vector.lmt)
port.noshort.lmt.wgt <- round(
  as.matrix(port.noshort.lmt$solution) * 100,  digits=2)
rownames(port.noshort.lmt.wgt) <-c(names(returns))
colnames(port.noshort.lmt.wgt) <-c("MVP % weights")
port.noshort.lmt.wgt

port.wgt
port.noshort.wgt
port.noshort.lmt.wgt

# 4.2
library(xts)
library(TTR)
library(fPortfolio)
library(fAssets)
library(DEoptimR)
library(timeSeries)

data("managers")
?managers
summary(managers)

returns <- as.timeSeries(na.omit(managers[, 5:10]))
n <-ncol(returns)
lmt <- c("minW[1:n]=0.15", "maxW[1:n]=0.5")

constraints <- minvariancePortfolio(returns, constraints=lmt)
constraints
round(constraints@portfolio@portfolio$weights * 100, 2)

eff <- portfolioFrontier(returns)
eff

lmt <- c("minW[1:n]=0.10", "maxW[1:n]=0.5",
         "minsumW[c(5:6)]=0.30", "maxsumW[c(5:6)]=0.45")

constraints <- minvariancePortfolio(returns, constraints = lmt)
round(constraints@portfolio@portfolio$weights * 100, 2)