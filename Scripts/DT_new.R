nm  <- fread("nm-logit.csv")
nm[,c("no","Loyal"):= NULL]
head(nm)
library(rpart)
library(rpart.plot)
nm.con <- rpart.control(minsplit = 5,minbucket = 2,cp=0,xval=10)
nm.tree <- rpart(Loyalty~.,data = nm,method="class",control = nm.con)
print(nm.tree)
rpart.plot(nm.tree)

nm.pr <- predict(nm.tree,newdata=nm,type="class")
nm.pro <- predict(nm.tree,newdata=nm,type="prob")
g1 <- floor(nm.pro[,2]+0.3)
caret::confusionMatrix(nm.pr,nm$Loyalty)
caret::confusionMatrix(g1,nm$Loyalty)
nm.perf <- prediction(nm.pro[,2],labels=nm$Loyalty)
nm.plt <- performance(nm.perf,measure="tpr",x.measure = "fpr")
plot(nm.plt)
abline(0,1)
nm.auc <- performance(nm.perf,measure="auc")
nm.auc@y.values[[1]]
View(data.frame(nm,nm.pr))

tmp <- NULL
for (i in 1:10)
{
  g1 <- floor(nm.pro[,2]+i/10)
  a<- caret::confusionMatrix(g1,nm$Loyalty)
  tmp[i] <- a$overall["Accuracy"]
}

plot(1:10,tmp,type="o")
hp<- fread("houseprices (1).csv")
hp.reg <- rpart(Price~.,data=hp,method="anova")
print(hp.reg)
install.packages("Metrics")
hp.pre <- predict(hp.reg,newdata=hp)
mod <- lm(Price~.,data=hp)
mo.pr <- predict(mod,hp)
View(data.frame(hp[,1],CART=hp.pre,LR = mo.pr))
lm.rmse <- Metrics::rmse(actual = hp$Price,predicted = mo.pr)
cart.rmse <- Metrics::rmse(actual = hp$Price,predicted = hp.pre)
df <- data.frame(models = c("CART","LR"),result = c(cart.rmse,lm.rmse))
library(ggplot2)
ggplot(df,aes(models,result))+geom_bar(stat="identity")
