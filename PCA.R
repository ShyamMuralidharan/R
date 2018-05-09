## Normalization
# (x-xmax)/(xmax-xmin)
### Create the PCA model
iris.pc<- prcomp(iris[,c(-5,-6)])
### Explore the names
names(iris.pc)
### Identify using the summary table to find the ideal number of
### principal components needed to explain max cum variance
summary(iris.pc)
### Use the rotation matrix to identify the loading of the components
### onto the relevant PC's
iris.pc$rotation
### Extract Std Dev from the matrix
sdev <- iris.pc$sdev
### Obtain variance
pr_var<- sdev^2
### Extract the proportionate of variance to which is used for the
### Scree plot to identify number of components needed
prp_var_exp <- pr_var/sum(pr_var)
### Use the biplot to identify components needed and also the need for
### Scaling
biplot(iris.pc,scale = T)
### Plot the Proportion of variance explained by the components
### Plot the cumulative variance explained by the components
par(mfrow=c(1,2))
plot(prp_var_exp,xlab="Principal Components",ylab="Variance explained",type="b")
plot(cumsum(prp_var_exp),xlab="Principal Components",ylab="Cumulative Variance explained",type="b")
### Applying the PCA to model on data
iris.t <- data.frame(Species = iris[,c(5)],iris.pc$x)
iris.t <- iris.t[,1:3] ### Extract only the first 2 components
### CART
library(rpart)
iris.p.mod <- rpart(Species~PC1+PC2,data=iris.t,method = "class")
### Convert the test data to the format of the PCA model
iris.p <- predict(iris.pc,newdata = iris[,-c(5,6)])
iris.p <- iris.p[,1:2]
iris.p <- as.data.frame(iris.p)
### Predict using the new model
pr <- predict(iris.p.mod,newdata = iris.p,type = "class")
head(pr)
### Accuracy estimates
library(caret)
confusionMatrix(pr,iris.t$Species)



