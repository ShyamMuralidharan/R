library(caret)
library(e1071)
set.seed(100)
sam <- caTools::sample.split(iris$Species,SplitRatio = 0.7)
### Test and train split
train = iris[sam,]
test = iris[!sam,]
#### Model building
rfMod <- randomForest::randomForest(Species~.,data=train)
cartMod <- rpart::rpart(Species~.,data=train,control=rpart::rpart.control(minsplit=6,minbucket=3,xval=10))
svm.mod <- svm(Species~.,data=train,kernel="radial",gamma=10,cost=10)
##### Train validation
rfpred <- predict(rfMod,newdata=train,type="class")
cartpred <- predict(cartMod,newdata=train,type="class")
svmpred <- predict(svm.mod,newdata=train,type="class")
#### Confusion Matrix
rfcm.tr <- confusionMatrix(rfpred,train$Species)
cartcm.tr <- confusionMatrix(cartpred,train$Species)
svmpred.tr <- confusionMatrix(svmpred,train$Species)
acc.df <- data.frame(Models=c("Random Forest","CART","SVM"))
acc.df$Training <- c(rfcm.tr$overall["Accuracy"],cartcm.tr$overall["Accuracy"],svmpred.tr$overall["Accuracy"])
### Model accuracy table
library(ggplot2)
ggplot(acc.df,aes(Models,Training)) + geom_bar(stat="identity")