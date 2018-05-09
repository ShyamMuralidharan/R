library("randomForest")
###
dev<- read.csv("~/DEV_SAMPLE.csv")
RF <- randomForest(as.factor(Target) ~ ., data = dev[,-1],
                   ntree=100,
                   mtry = 3,
                   nodesize = 10,
                   importance=TRUE )
print(RF)
plot(RF)

tRF <- tuneRF(x = dev[,-c(1,2)], 
              y=as.factor(dev$Target), 
              mtryStart = 3, 
              ntreeTry=100, 
              stepFactor = 1.5, 
              improve = 0.0001, 
              trace=TRUE, plot = TRUE, 
              doBest = TRUE, 
              nodesize = 10, 
              importance=TRUE )

pred <- predict(tRF,newdata=dev,type = "class")

caret::confusionMatrix(data=dev$Target,pred)
caret::confusionMatrix(data=dev$Target,treePred)
head(dev)
