library(ggplot2)
library(data.table)
library(magrittr)
options(scipen = 1)
poke <- read.csv("~/Pokemon.csv")
hp <- read.csv("~/houseprices (1).csv")
## Facetting
ggplot(hp,aes(Living.Area,Price)) + 
  geom_point() + facet_wrap(~Bedrooms)
ggplot(poke,aes(Attack,Defense)) + geom_point() + facet_grid(~`Type 1`)
### Stat layer
ggplot(hp,aes(Living.Area,Price)) +
  geom_point() + stat_smooth(method = "lm")
### Coord layer
a <- ggplot(hp,aes(Living.Area,Price)) + geom_point()
names(a)
dim(hp)
class(a)
dim(a$data)
a$data
## Zooming with clipping
ggplot(hp,aes(Living.Area,Price)) + 
  geom_point() + 
    xlim(c(1000,2000)) + 
    ylim(c(1e+05,3e+05))
## Zooming without clipping
ggplot(hp,aes(Living.Area,Price)) + 
  geom_point() +
 scale_y_continuous(breaks = seq(0,500000,by = 50000))+
  coord_cartesian(xlim=c(1000,2000))
poke.dt <- data.table(poke)
head(poke.dt)
poke.dt[,.N,by=.(`Type 1`,Generation)] %>% 
  ggplot(aes(reorder(`Type 1`,N),N,fill=factor(Generation))) + 
  geom_bar(stat="identity") + xlab("Type") + ylab("Count") + 
  #coord_flip() + 
  scale_y_continuous(breaks = seq(0,90,by=15)) + my_theme()
  theme(panel.background = element_rect(fill="white"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 45))
  
my_theme <- function()
{
  theme(panel.background = element_rect(fill="white"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 45))
}
