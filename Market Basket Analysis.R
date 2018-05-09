library(arules)
library(datasets)
### Load the groceries dataset
data("Groceries")
### If data is loaded from CSV file the following method is used
groc  <- read.csv("~/groceries.csv",header = F)
gro.t <- as(gro,"transactions")
View(groc)
### Ways of creating transactions
ecom <- data.table::fread("~/data.csv")
ecom <- ecom[1:10000]
### From an existing dataset
t <- split(ecom$Description,ecom$InvoiceNo)
head(t)
### Applicable for csv and existing data
ecom.t <- as(t,"transactions")
summary(ecom.t)

### Market Basket Analysis
gro<- Groceries
### Visualize the top frequeted items
itemFrequencyPlot(gro,topN=20,type="absolute")

# Get the rules with min constraints on support and 
# confidence
rules <- apriori(gro, 
                 parameter = list(supp = 0.001, conf = 0.8))

# Show the top 5 rules, but only 2 digits
summary(rules)
options(digits=2)
inspect(rules[1:5])

# Sorting the rules
rules<-sort(rules, by="confidence", decreasing=TRUE)
inspect(rules[1:5])
### Write the rules into a data.frame for easier viewing
rule.df <- as(rules,"data.frame")
View(rule.df)
### Set rule with maximum length to subset limited items
rules <- apriori(gro, 
                 parameter = list(supp = 0.001, 
                                  conf = 0.8,maxlen=3))


# Cleaning the rules
subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
redundant
rules.pruned <- rules[!redundant]
rules<-rules.pruned

## Mining rules based on a items with specific product
rules<-apriori(data=gro, parameter=list(supp=0.001,conf = 0.08), 
               appearance = list(default="rhs",lhs="whole milk"),
               control = list(verbose=F))
## Mining rules based on specific product with other items
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])

