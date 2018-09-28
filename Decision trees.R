### Load Libraries
library(magrittr)
library(data.table)
library(rpart)
library(rpart.plot)
### reading the data
dev <- fread("DEV_SAMPLE.csv")
### DT CART Model
cont <- rpart.control(minsplit = 45,minbucket = 15,cp=0,xval=10)
tree <- rpart(Target~.,data=dev[,-1],control = cont,method = "class")
## Print and plot the tree
print(tree)
plot(tree)
rpart.plot(tree,cex=0.6)
pred <- predict(tree,newdata=dev,type="class")
table(Actual= dev$Target,predicted = pred)
cp <-printcp(tree)
ptree <- prune(tree,0.0016194332,"CP")
rpart.plot(ptree,cex=0.6)

treeProb <- predict(ptree,newdata=dev,type="prob")
treePred <- predict(ptree,newdata=dev,type="class")
head(treeProb)
head(dev)
g1 <- floor(treeProb[,2]+0.9)

caret::confusionMatrix(data=dev$Target,treePred)
caret::confusionMatrix(data=dev$Target,g1)
mean(dev$Target==1) * 100

library("ROCR")
p <- prediction(labels = dev$Target,predictions = treeProb[,2])
perf <- performance(p,measure="tpr",x.measure = "fpr")
plot(perf)
auc <- performance(p,"auc")
auc@y.values[[1]]


View(iris)
iris.tree <- rpart(Species~.,data=iris,control=rpart.control(minsplit = 9,minbucket = 3,cp=0,xval=10))
print(iris.tree)
rpart.plot(iris.tree)

iris.pred <- predict(iris.tree,newdata=iris,type="class")
iris.prob <- predict(iris.tree,newdata=iris,type="prob")

caret::confusionMatrix(iris.pred,iris$Species)
