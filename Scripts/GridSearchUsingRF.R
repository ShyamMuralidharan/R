### Library
library(randomForest)
dev <- read.csv("DEV_SAMPLE.CSV")
hyper <- expand.grid(mtry=c(2:3),ntrees=c(10:12))
temp <- list()
for (i in 1:nrow(hyper)){
  mtry <- hyper$mtry[i]
  ntrees <- hyper$ntrees[i]
  RF <- randomForest(factor(dev$Target)~.,
                     data=dev[,-1],
                    mtry=mtry,
                    ntree=ntrees,
                    importance = T,
                    nodesize=10)
  temp[i]<- RF$err.rate[which.min(RF$err.rate[,1])]
}
hyper[which.min(temp),]
varImpPlot(RF)
