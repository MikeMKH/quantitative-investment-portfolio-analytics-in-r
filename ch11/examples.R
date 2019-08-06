# 11.1
library(PerformanceAnalytics)
data(managers)
ret <- managers$`US 10Y TR` * 100

plot(ret, main="US Treasury 10 year: 1-month % returns",
     ylab="return", xlab="date")

dev.off()

dates.1 <-as.Date(index(ret))
dates.2 <-seq(first(dates.1), last(dates.1), by = "3 month")
ticks <-pretty(range(ret))

plot(dates.1, ret, type='l', 
     main="US Treasury 10 year: monthly returns", cex.main=0.9,
     ylab="returns", xlab="", yaxt="n", xaxt="n",
     ylim = c(min(ticks), max(ticks)),
     panel.first=c(abline(v=dates.2, col = "gray", lty = 3),
                   abline(h=ticks, col = "gray", lty = 3)))
axis(side=2, las=2, at=ticks, lab=paste0(ticks, "%"))
axis.Date(side=1, dates.1, at=dates.2, format = "%b-%Y")
box()

# 11.2
library(ggplot2)
library(PerformanceAnalytics)
library(scales)
data(managers)

ret <- managers$`US 10Y TR`
ret <- to.monthly(ret, indexAt='firstof', OHLC=FALSE)
ret.df <- data.frame(ret)
ret.df$date <- as.Date(index(ret))
summary(ret)

ggplot(ret.df) +
  geom_line(aes(x=ret.df$date, y=US.10Y.TR)) +
  scale_x_date(breaks=date_breaks('1 year'),
               labels=date_format("%b-%Y")) +
  scale_y_continuous(labels=percent) +
  labs(title="US Treasury 10 year: monthly returns") +
  ylab(NULL) + 
  xlab(NULL) +
  theme(text=element_text(size=9))
