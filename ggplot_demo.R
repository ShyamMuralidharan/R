library(ggplot2)

ggplot(hf,aes(TaxiIn,TaxiOut)) + geom_point(alpha=0.2)
head(hf)
ggplot(hf.dt,aes(TotDelay,TaxiIn)) + geom_point()
### if the x variable is continuous then we use
### histogram, if categorical or discrete then bar
ggplot(hf.dt,aes(ArrTime,fill=Origin))+ geom_histogram()
head(hf.dt)
ggplot(hf.dt,aes(factor(Cancelled),fill=Origin))+
  geom_bar()
mean(hf$Cancelled==1)

hr <- read.csv("~/HR-em.csv")
hr.dt <- data.table(hr)
names(hr.dt)[1]<- "Age"
ggplot(hr.dt,aes(reorder(Department,DailyRate),DailyRate))+ 
  geom_bar(stat="identity")
unique(hr.dt)
ggplot(hr.dt,aes(reorder(PercentSalaryHike),PercentsalaryHike))+ geom_bar() +
  facet_grid(~Department)
unique(hr.dt$JobSatisfaction)

hr.dt[,.N,by=Department] %>% ggplot(aes(Department,N)) +
  geom_bar(stat="identity")

(a <- ggplot(hr.dt,aes(Age,MonthlyIncome,col=Department))+
  geom_point(position="jitter") +
  coord_cartesian(xlim=c(30,50),ylim=c(5000,10000)))

my_theme <- function(){
  panel.background =  element_rect(fill = "white")
  plot.background = element_rect(fill="gray")
  axis.title.x = element_text(face="italic")
}
# element_blank()
# element_line()
# element_rect()
# element_text()

hf.dt[,lapply(.SD,mean),.SDcols = c("Distance"),
      by=.(Origin,Dest)][order(-Distance)] %>% 
  ggplot(aes(Origin,Distance,fill=Dest)) + geom_bar(stat="identity")
