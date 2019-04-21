#### rush hour in different quarters


library(ggplot2)
library(dplyr)
library(readr)
library(ggmap)
library(tidyr)
library(lubridate)
library(stringr)

data=read_csv("LA_metro.csv")

data3=data%>%
  group_by(hour, month,quarter,year)%>%
  summarise(count=n())


ggplot(data3,aes(x=factor(quarter), y=hour, fill=count))+
  geom_tile()+
  scale_fill_gradient(low="white", high="darkred")+
  facet_wrap(~year,nrow = 2)+
  xlab("Quarter")+
  ylab("Time")+
  ggtitle("Rush hours in different months between 2017-2018")+
  theme(plot.title = element_text(hjust = 0.5))


#### rush hour in different regions  
####5.3.1
data4=data%>%
  group_by(start_region,hour,month,quarter,year)%>%
  summarise(count=n())

ggplot(data4,aes(x=start_region, y=factor(hour), fill=count))+
  geom_tile()+
  scale_fill_gradient(low="white", high="darkred")+
  facet_wrap(~year,nrow = 2)+
  ggtitle(" Rush hours in different regions between 2017-2018")+
  xlab("Region")+
  ylab("Time")+
  theme(plot.title = element_text(hjust = 0.5))