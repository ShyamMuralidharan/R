library(e1071)
plot(iris)
#### Choose specific columns to be modelled upon
col <- c("Petal.Length","Petal.Width","Species")
svm.mod <- svm(Species~.,data=iris[,col],kernel="radial",cost=10,gamma=10)
print(svm.mod)
plot(svm.mod,iris[,col])
svm.mod.l <- list()
# Variation in the cost and gamma by looping
for(i in 1:4){
  svm.mod.l[[i]] <- svm(Species~.,data=iris[,col],kernel="radial",cost=i*10,gamma=i*10)
  
}
### Accuracy extraction estimator
tmp<- NULL
for( i in 1:length(svm.mod.l)){
  p <- predict(svm.mod.l[[i]],newdata=iris,type="class")
  a <- caret::confusionMatrix(p,iris$Species)
  tmp[i]<- a$overall["Accuracy"]
}
#### Grid search for finding the best fit.
h1 <- expand.grid(cost=c(40,50,60,70,90,100),gamma=c(1,5,10),kernel="radial")
for(i in 1:nrow(h1)){
  ker <- h1$kernel[i]
  cos <- h1$cost[i]
  gam <- h1$gamma[i]
  svm.mod <- svm(Species~.,data=iris[sample(150,100),],kernel=ker,
                 cost=cos,
                 gamma=gam)
  p1 <- predict(svm.mod,newdata=iris[sample(150,100),])
  a <- caret::confusionMatrix(p1,iris$Species[sample(150,100)])
  tmp[i]<- a$overall["Accuracy"]
}
### Identifying the best parameters producing best accuracy
tmp
h1[which.max(tmp),]
