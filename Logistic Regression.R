nm <- read.csv("nm-logit.csv")
library(pscl)
library(lmtest)
library(pROC)

head(nm)
nm<- nm[,c(-1,-6)]
### Build the model
mod <- glm(Loyalty~Brand+Product+Shopping,data=nm,family = "binomial")
### Step 1: check for the log likelihood ratio
lrtest(mod)
### Step 2: Explore the pR2 and look specifically into the
###         McFadden value
pR2(mod)
### Step 3: Explore the summary table for statistical significance
###         of the variables
summary(mod)
### Step 4: Export the odds beta values of the model
exp(coef(mod))
# compute probability as odds/odds+1
exp(coef(mod))/(exp(coef(mod))+1)
### Step 5: Build the confusion Matrix/classification table
# pre steps to build the confusion matrix
pred <- predict(mod,newdata=nm,type="response")
head(pred)
pred_class <- floor(pred+0.5) ## 0.5 is called as the threshold value
# confusion matrix method 1
table(Actual = nm$Loyalty,Predicted=pred_class)
# confusion matrix method 2, the sophisticated way
caret::confusionMatrix(pred_class,nm$Loyalty)
## Step 6 Build the ROC plot
predi <- prediction(predictions = pred_class,labels = nm$Loyalty)
predi1 <- performance(predi,measure = "tpr",x.measure = "fpr")
plot(predi1)
abline(0,1)
auc <- performance(predi,"auc")
### Area under the curve
auc@y.values[[1]]
