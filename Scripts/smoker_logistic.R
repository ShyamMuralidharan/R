#### Attaching the Library ####
library(caTools)
##### Reading the data ######
smkr <- read.csv("~/Downloads/smokerdt.csv")
head(smkr)
##### Training and testing split ######
smkr_dt <- smkr[1:10,]
smkr_test <- smkr[11,]
##### Converting numeric columns, except age ####
smkr_dt[,!names(sapply(smkr_dt,is.numeric)) %in% "age"] <- lapply(smkr_dt[,!names(sapply(smkr_dt,is.numeric)) %in% "age"],as.factor)
##### Model train and test split ####
ind <- sample.split(smkr_dt$smoker,SplitRatio = 0.7)
train <- smkr_dt[ind,]
test <- smkr_dt[!ind,]
###### Model Building #####
smkr_mod <- glm(smoker~.,data=train,family = "binomial")
summary(smkr_mod)
pred_train <- predict(smkr_mod,train,type='response')
caret::confusionMatrix(ifelse(pred_train>0.5,1,0),train$smoker)
pred_test <- pred_train <- predict(smkr_mod,test,type='response')
caret::confusionMatrix(ifelse(pred_test>0.5,1,0),test$smoker)
###### Testing Data #####
smkr_test[,!names(sapply(smkr_test,is.numeric)) %in% "age"] <- lapply(smkr_test[,!names(sapply(smkr_test,is.numeric)) %in% "age"],as.factor)
pred <- predict(smkr_mod,smkr_test,type='response')
smoker_predicted <- ifelse(pred>0.5,1,0)
smoker_predicted
