# 9.1
library(quantmod)
symbols <- c("VTI", "BSV", "BLV", "VEA", "VIG", "VWELX")
getSymbols(symbols, src = "yahoo", auto.assign=T)
prices <- do.call(merge, lapply(symbols, function(x) Cl(get(x))))
colnames(prices) <- symbols 

returns <- na.omit(ROC(prices['1999-12-31::2017-12-31'], 
                       1, "continuous", na.pad = FALSE))
returns <- to.monthly(returns, indexAt='firstof', OHLC=FALSE)

fit <-lm(returns$VWELX~., data=returns)
summary(fit) # very interesting results

# FactorAnalytics does not seem to have the functions used in the chapter