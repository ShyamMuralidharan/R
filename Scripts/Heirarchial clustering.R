#### Remove the categorical column
iris.sc<- scale(iris[,-5])
### Create the Heirarchial clustering model
iris.hc <- hclust(d=dist(iris.sc),method = "complete")
#### Plot to idenfity the best cluster count and the place to "cut" the tree
plot(iris.hc)
### Mechanics to explain the model
temp.hc <- hclust(d=dist(c(1:10)))
plot(temp.hc)
a <- cutree(temp.hc,h=5)
abline(h=3.75,col="red")
### Cut by height 'h' or cut my clusters 'k'
hc.ir <- cutree(iris.hc,k=3)
table(hc.ir,km.ir$cluster)
