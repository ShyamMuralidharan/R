library(ggplot2)
names(storms)
ggplot(storms,aes(x=year,y=wind)) +  geom_line()
       #Data  # Aesthetic layer     #geom layer

# binning = range/no. of bins
ggplot(storms,aes(wind,fill=category)) + geom_histogram(bins = 5,binwidth = 10,position = "fill")

# explaratory plots
# explanatory plots