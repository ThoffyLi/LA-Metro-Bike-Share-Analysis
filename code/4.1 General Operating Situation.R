library(ggplot2)
library(dplyr)
library(readr)
library(ggmap)
library(tidyr)
library(lubridate)
library(stringr)

data=read_csv("LA_metro.csv")

#### Compared with different quarters about frequency between 2017-2018
### 5.1

data1=data %>%
  group_by(year,quarter) %>%
  summarise(count=n())


data1$quarter=as.factor(data1$quarter)


data1$year=as.factor(data1$year)

ggplot(data1,aes(x=quarter,y=count,
                 group=as.factor(year),color= year))+
  geom_point()+
  geom_line(size = 1)+
  ggtitle("General Bike Usage Trend (by quarter)")+
  theme(plot.title = element_text(hjust = 0.5))+ylab('number of trips')



#### Compared with different months about frequency between 2017-2018
###5.1.2


data2=data %>%
  group_by(year,month) %>%
  summarise(count=n())

data2$month=as.factor(data2$month)
data2$year=as.factor(data2$year)
data2$count=as.numeric(data2$count)

ggplot(data2,aes(x=month,y=count,group=year,color = year))+
  geom_point()+
  geom_line(size = 1)+
  ggtitle("General Bike Usage Trend (by month)")+
  ylab('number of trips')+
  theme(plot.title = element_text(hjust = 0.5))




