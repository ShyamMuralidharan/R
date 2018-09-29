milk <- read.csv("~/Datasets/milkproddata.csv")
head(milk)
tail(milk)
ts.m <- ts(data = milk[,2],start=c(1960,1),end=c(1970,12),frequency = 12)
ts.plot(ts.m)
cycle(ts.m)
plot(decompose(ts.m))
ts.plot(diff(log(ts.m))) # d = 1
pacf(diff(log(ts.m))) # AR(0) 
acf(diff(log(ts.m))) # MA(0)
ar.mod <- arima(log(ts.m),order = c(0,1,0),seasonal = c(0,1,0))
ar.pred <- predict(ar.mod,n.ahead = 2*12)
ts.plot(ts.m,2.718^ar.pred$pred,col=c(1,2))
library("Metrics")
mape(milk[109:132,2],2.718^ar.pred$pred) # MAPE
mae(milk[109:132,2],2.718^ar.pred$pred) # MAE
library(ggplot2)
ggplot(ts.m,aes(time(ts.m),ts.m)) + geom_line() + 
  geom_line(data=data.frame(ar.pred),
            aes(x=time(ar.pred$pred),2.718^pred),col='dodgerblue3',lty=2,lwd=1) + labs(x="Time",y="Production")

