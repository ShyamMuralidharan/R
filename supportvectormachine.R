library(ggplot2)
install.packages("hflights")
hf <- hflights::hflights

ggplot(hf,aes(TaxiIn,TaxiOut)) + geom_point()





ggplot(hf,aes(TaxiIn,TaxiOut,col=Origin)) + geom_point(alpha=0.3)
ggplot(hf,aes(ActualElapsedTime,fill=factor(Month)))+geom_histogram()
head(hf)