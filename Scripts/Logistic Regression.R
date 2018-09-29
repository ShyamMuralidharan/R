nm <- read.csv("nm-logit.csv")
library(pscl)
library(lmtest)
library(pROC)
library(dplyr)
library(ggplot2)

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
s <- summary(mod)
### Step 4: Export the odds beta values of the model
exp(coef(mod))
# compute probability as odds/odds+1
a <- exp(coef(mod))/(exp(coef(mod))+1)
df <- data.frame(Odds=exp(coefficients(logit)),
                 PVal =b$coefficients[,"Pr(>|z|)"],
                 Prob = a)
### Create the band of varialble values based on the Odds and te Pr value
df <- df %>% mutate(choice=ifelse(Odds>1 & PVal <0.05,"MustUse",
                                  ifelse(Odds>1 & PVal <0.5,"Recommended",
                                         ifelse(Odds>1 & PVal <0.9,"Try","Ignore"))))
df %>% 
  ggplot(aes(choice)) + 
  geom_bar() + 
  geom_text(stat='count',aes(label=..count..),vjust=-1,size=3)
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
