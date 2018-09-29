library(dplyr)
library(tidyr)
library(lubridate)

## 5 Verbs of Data Manip
#1. filter(.data,condition)
#2. select(.data,var names) - additional functions = starts_with(),ends_with(),contains()
#3. summarise(.data, var_name=summary_function(var)) -  
## additional functions = n()-> counts total observations,n_distinct()->counts distinct observations
#4. mutate(.data, new_var_name= operation/function/manip)
#5. group_by(.data,grouping_condition1,grouping_condition2,...)

#### additional
#1. slice()
#2. arrange()


### EDA
summary(storms)
str(storms)
head(storms)
tail(storms)
names(storms)

### 1. Filter()
filter(storms,year == "2015")
### 2. select()
select(storms,hour:wind)
select(storms,ends_with("ter"),contains("cat"))
select(claim,contains("car"),ends_with("cat"))
### 3. summarise()
summarise(storms,mean(wind),ts_diamean = mean(ts_diameter,na.rm = T),tot=`mean(wind)`+ts_diamean)
summarise(storms,count = n(),count_distinct=n_distinct(wind))
### 4. mutate()
mutate(storms,day_hour = day+hour)
summary(mutate(storms,tota_dia = if(wind>20) ts_diameter+hu_diameter ))

### 5. group_by()
group_by(storms,day) %>% summarise(count = n()) %>% arrange(desc(count,day))

#### tidyr
storms1<- unite(storms,day_month_year,day,month,year,sep = "-")
storms1$day_month_year=dmy(storms1$day_month_year)
class(dmy(storms1$day_month_year))
names(storms1)
head(storms1)
names(storms)
storms2<- separate(storms1,day_month_year,c("day","month","year"),sep="-")
names(storms2)

