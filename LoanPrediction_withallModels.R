### Load the libraries ####
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(randomForest))
suppressPackageStartupMessages(library(rpart))
suppressPackageStartupMessages(library(e1071))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(Metrics))
suppressPackageStartupMessages(library(tidyr))
### load the data ####
lp.train <- fread("~/Loan Prediction/train.csv",stringsAsFactors = T)
lp.test <- fread("~/Loan Prediction/test.csv",stringsAsFactors = T)
##### Data Pre Processing and exploration #####
lp.train[,.N,by=.(Loan_Status,Loan_Amount_Term)][,Approval_rat := N/sum(N)][order(-Approval_rat)] %>% spread(Loan_Status,Approval_rat,)
### Model Building ###
tree.mod <- rpart(Loan_Status~.,data=lp.train[,-1],method="class",
                  control = rpart.control(minsplit = 15,minbucket = 5,cp=0,xval=10))
rpart.plot::rpart.plot(tree.mod,cex=0.5)
print(tree.mod)
colMeans(is.na(lp.train))
lp.train<- data.frame(lp.train)
ind<- NULL
for(i in 1:ncol(lp.train))
ind[i]<-is.numeric(lp.train[,i])

for ( i in 1:ncol(lp.train[,ind])){
  
  lp.train[which(is.na(lp.train[,i])==T),i]<- median(lp.train[,i],na.rm=T)
}
}
colMeans(is.na(lp.train))
str(lp.train)

rf <- randomForest(Loan_Status~.,data=(lp.train[,-1]),ntree=500,mtry=4,
                   importance=T)

plot(rf)
print(rf)
tRF <- tuneRF(x=lp.train$Loan_Status,
              y=lp.train[,c(-1,-13)],
              mtryStart =3,
              ntreeTry = 100,
              stepFactor = 1.5,
              improve = 0.0001,
              importance=T,
              trace = T,
              plot = T)

class(sapply(1:10,mean))
rf.p <- predict(rf,newdata=lp.train,type="class")
confusionMatrix(rf.p,lp.train$Loan_Status)
rf.t.p <- predict(rf,newdata=lp.test,type="class")
lp.train<- data.table(lp.train)
names(lp.train)
lp.train[,lapply(.SD,mean,na.rm=T),.SDcols="LoanAmount",by=Loan_Status]
