library(data.table)
library(tidyr)
del <- fread("deliveries.csv") 
mat <- fread("matches.csv")
head(mat)
head(del)
names(del)[1] <- "id"
del$winner <- mat$winner
bat <- del[batting_team == "Delhi Daredevils",
           lapply(.SD,sum),by=.(batsman,id),
           .SDcols="total_runs"]
head(bat)
bat[order(-total_runs)] %>% spread(id,total_runs)
head(bat)
install.packages("dplyr")
library(dplyr)
mat_del <- inner_join(mat,del,by=c("id","winner"))
names(mat_del)
mat_del_mer <- merge(mat,del,by="id")
head(mat_del)


wss <- NULL
for ( i in 1:10){
  km <- kmeans(bat[,3],centers = i,nstart=20,iter.max = 15)
wss[i] <- km$tot.withinss
}
plot(1:10,wss,type='b')

km.f <- kmeans(bat[,3],centers = 3,nstart=20)
bat$cls <- km.f$cluster
head(bat)
km.f$centers

library(ggplot2)
bat %>% 
  ggplot(aes(batsman,col=factor(cls))) + 
  coord_polar(theta='x')

bat %>% ggplot(aes(id,total_runs,col=factor(cls))) + geom_point()
