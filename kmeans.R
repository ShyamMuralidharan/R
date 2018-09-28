## Unsupervised learning
library(ggplot2)
#### Display the contents #### 
ggplot(iris,aes(Petal.Length,Petal.Width))+
  geom_point()
km.ir <- kmeans(iris[,-5],centers=3,nstart = 20)
km.ir
ggplot(iris,aes(Petal.Length,Petal.Width))+
  geom_point(aes(col=factor(km.ir$cluster))) + geom_point(data=as.data.frame(km.ir$centers),
                            aes(Petal.Length,Petal.Width,
                              col=unique(factor(km.ir$cluster))),size=3)
wss <- NULL
for(i in 1:10){
  km.mod <- kmeans(iris[,-5],centers = i,nstart=20)
  wss[i]<- km.mod$tot.withinss
}
plot(1:10,wss,type="b")

km.ir$centers
iris$Cluster <- factor(km.ir$cluster)

iris[iris$Cluster==3,]

