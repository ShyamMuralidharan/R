hf <- hflights::hflights
library(dplyr)
head(hf)
### Select
s <- select(hf,Year:DayOfWeek,ArrTime,starts_with("taxi"),
            ends_with("Num"),contains("Time"),contains("Delay"),
            UniqueCarrier,Distance)
### Filter 
f <- filter(s,DayOfWeek > 5, 
        UniqueCarrier %in% c("AA","OO","B6"))

unique(hf$UniqueCarrier)
### Summarise
summarise(f,count = n(),
          avg_delay= mean(ArrDelay+DepDelay,na.rm=T),
          UniqueCarrierCount = n_distinct(UniqueCarrier),
          DaysOfDep = n_distinct(DayOfWeek),
          avg_dist = mean(Distance,na.rm=T))
### Mutate
(hf<-mutate(hf,TotDelay = ArrDelay+DepDelay,
        Delayed = ifelse(TotDelay>0,T,F)) )
  
names(hf)

hf %>% group_by(UniqueCarrier) %>% summarise(count =n(),
                                                    avg_distance=mean(Distance)) 
hf %>% summarise(count = n())
hf %>% group_by(UniqueCarrier,Origin) %>% summarise(count=n()) %>%
  ggplot(aes(reorder(UniqueCarrier,count),count,fill=Origin))+geom_bar(stat="identity")
hf %>% group_by(Origin) %>%
                  summarise(count = n())

#### Data.table
hf %>% group_by(DayOfWeek) %>% summarise(count = n())
hf.dt <- data.table::data.table(hf)

hf.dt[,.N,by=UniqueCarrier]        
hf %>% group_by(UniqueCarrier) %>% summarise(count = n())        
hf.dt[,TotDelay := ArrDelay+DepDelay]
hf.dt[,lapply(.SD,mean),.SDcols = c("Distance"),
      by=.(UniqueCarrier,Origin)][order(Distance)]
microbenchmark::microbenchmark(DT =hf.dt[,lapply(.SD,mean),.SDcols = c("Distance"),
                                         by=.(UniqueCarrier,Origin)][order(Distance)],
                               DF = hf %>% group_by(UniqueCarrier,Origin) %>% 
                                 summarise(AvgDistance = mean(Distance)) %>% arrange(AvgDistance)
                               ,times = 300,unit="s")

hf.dt[,lapply(.SD[UniqueCarrier=="AA"],mean,na.rm=T),.SDcols="ArrDelay"]
class(lapply(hf,mean))
class(sapply(hf,mean,na.rm=T))

