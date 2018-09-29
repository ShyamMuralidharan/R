library(data.table)
library("hflights")
library(dplyr)
library(magrittr)
library(tidyr)
library(dplyr)
library(microbenchmark)
library(imputeMissings)
hflights <- read.csv("~/flight_dataset.csv")
hf.dt <- data.table(hflights)
hf <- hflights
########################### DATA.TABLE ########################
# DT[ i , j, by ]
##### Basic data manipulations ######
#  i - Subsetting in data.tables by rows
hf.dt[1]
hf.dt[1:10]
hf.dt[Origin=="IAH"]
hf.dt[Origin=="HOU"& Dest == "JFK"]
hf.dt[which(hf.dt$Month==2)]
#### j -  Column indexing/chosing ####
hf.dt[,"Origin"]
hf.dt[,c("UniqueCarrier","Distance")]
hf.dt[,list(FlightNum,TailNum)]
hf.dt[,.(AirTime,ActualElapsedTime)]
### by - the grouping condition
hf.dt[,mean(AirTime,na.rm=T),by=UniqueCarrier]
hf.dt[,.(SD_DepDelay = sd(DepDelay,na.rm=T)),by=UniqueCarrier] ## Performing arithmetic operations but by renaming the variable
hf.dt[,median(ArrDelay,na.rm=T),by=.(UniqueCarrier,Month)] ## multiple grouping condition
hf.dt[,mean(ArrDelay+DepDelay,na.rm=T),keyby=.(UniqueCarrier,year_month =paste0(Year,"-",Month))] # renaming a grouping variable
##### Functions specific to data.table for manipulation ####
# 1. .N - to count the number of observations
# 2. .SD & .SDcols - to choose specific columns
# 3. .I - row index
hf.dt[,.N,keyby=UniqueCarrier] # get the count of number of values in the uniquecarrier group
hf.dt[,.I,keyby=UniqueCarrier] # Add row numbers group wise.
hf.dt[,.I[1:2],keyby=UniqueCarrier] # Select only the first 2 rows from the group
hf.dt[,.SD,.SDcols=c("UniqueCarrier","AirTime")]
#### Creating new columns ####
hf.dt[,TotalDelay := ArrDelay + DepDelay] 
hf.dt[,`:=`(TotalTaxi = TaxiIn+TaxiOut ,
            GroundTime = ActualElapsedTime - AirTime)] ## To create multiple columns at the same time
hf.dt[,c("TotalTaxi","GroundTime"):= NULL] ### Dropping columns
#### Processing values in columns ####
hf.dt[,lapply(.SD,mean,na.rm=T),.SDcols=c("TotalDelay","AirTime"),by=UniqueCarrier]
hf.dt[,.(AvgDelay = lapply(.SD,mean,na.rm=T)),by=.(UniqueCarrier,Month)] ## There is an error here can anyone identify it ?
#### Faster processing using setkey #####
setkey(hf.dt,UniqueCarrier)
hf.dt[UniqueCarrier=="AA"]
###### Chaining #####
dt1 <- hf.dt[,GroundTime := ActualElapsedTime-AirTime]
dt1 <- dt1[Origin == "IAH"]
dt1[order(-GroundTime)]
## the easy way to do the steps above
hf.dt[,GroundTime := ActualElapsedTime-AirTime][Origin == "IAH"][order(-GroundTime)]
#### Comparison #####
microbenchmark(DT = hf.dt[Origin=="HOU",
                          lapply(.SD,mean,na.rm=T),by=UniqueCarrier,.SDcols=c("ArrDelay","DepDelay")],
               DF = hf %>% filter(Origin == "HOU") %>% group_by(UniqueCarrier) %>%
                 summarise(AvgDelay= mean(ArrDelay,na.rm=TRUE),AvgDepDelay = mean(DepDelay,na.rm=T)),times = 1000)
microbenchmark(DT = hf.dt[,TotDelay := ArrDelay+DepDelay],
               DF = hf %>% mutate(TotDelay = ArrDelay + DepDelay),times=1000)
############################## End of Data.table ###############################
#####                                                                     ######
#####                                                                     ######
######################### Data Manipulation using tidyr ########################
library(tidyr)
### Important functions of tidyr
# 1. spread() - replacement of cast from reshape2
# 2. gather() - replacement of melt from reshape2
# 3. unite()
# 4. separate()
### spread ####
(dt1 <- hf.dt[,.(AvgDelay = lapply(.SD,mean,na.rm=T)),.SDcols="TotDelay",by=.(UniqueCarrier,Month)])
spread(dt1,UniqueCarrier # Key  ### Spread the data from a long format
       , AvgDelay # Value       ### to a wide format Spread by the key-value pair
       )
### gather ####
spread(dt1,Month,AvgDelay) %>% 
  gather(key = Month, value = AvgDelay,`1`:`12`,-UniqueCarrier)
#### unite ####
dt2 <- unite(hf.dt,"Date",Year,Month,DayofMonth,sep = "-") %>% as.data.table()
#### separate ####
dt2<-separate(dt2,col = Date,into = c("Year","Month","DayofMonth"),sep="-")
############################## End of tidyr ####################################
#####                                                                     ######
#####                                                                     ######
##################### Missing value detection and imputation ###################
### imputation
# When we are dealing with real world data we will be dealing with a lot of missing 
# values. General thumb rule stands that if the missing data is <5 % then we can omit
# the missing data and proceed, if not we will have to process the missing values
# some of the common methods include mean/media and mode imputation, other complex
# techniques exist such as predictive imputation, following are examples of the same
# 1. kNN imputation
# 2. mice imputation
# 3. rpart imputation
library(imputeMissings) # created package for simple mean/median and randomForest imputation
hf <- hflights
summary(hf)
colMeans(is.na(hf)) * 100 ## Identify the % of missing values in every column

##### Since there is no pre loaded function for mode imputation we create our own
Mode <- function (x, na.rm) {
  xtab <- table(x)
  xmode <- names(which(xtab == max(xtab)))
  if (length(xmode) > 1) xmode <- xmode[1]
  return(xmode)
}
replaceNA <- function(data){
  for (var in names(data)) {
    if (class(data[,var]) %in% c("numeric","integer")) {
      data[is.na(data[,var]),var] <- mean(data[,var], na.rm = TRUE)
    } else if (class(data[,var]) %in% c("character", "factor")) {
      data[is.na(data[,var]),var] <- Mode(data[,var], na.rm = TRUE)
    }
  }
  return(data)
}
hf1$UniqueCarrier[sample(100)] <- NA ## Artificial NA value creation to test the function
hf1 <- replaceNA(hf)
colMeans(is.na(hf1))
hf2 <- imputeMissings::impute(hf,method="median/mode")
colMeans(is.na(hf2)) * 100 ### Check for missing value % now
hf <- hflights # resetting the data back to original
############################## End of imputation   #############################
#####                                                                     ######
#####                                                                     ######
############################# Outlier detection ################################


