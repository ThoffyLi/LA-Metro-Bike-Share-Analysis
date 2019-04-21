# Find out stations that are mostly short or superfluous of bikes

library(ggplot2)
library(dplyr)
library(lubridate)
library(shiny)

# Chinese -> English labels
Sys.setlocale("LC_TIME", "English")

dataset = read.csv("LA_metro.csv")


# 2017
data_2017 = dataset %>% filter(year==2017)
data_2017$start_station = as.factor(data_2017$start_station)
station_count = data_2017 %>% group_by(start_station) %>% summarise(start_count = n()) %>% arrange(start_station)

data_2017$end_station = as.factor(data_2017$end_station)
station_count2 = data_2017 %>% group_by(end_station) %>% summarise(end_count = n())%>% arrange(end_station)
station_count2 = station_count2[-68,]
station_count2 = station_count2[-68,]

station_count$end_count = station_count2$end_count
station_count$in_out_diff = station_count$end_count - station_count$start_count 
station_count = station_count %>% arrange(in_out_diff)
station_count = station_count %>% filter(in_out_diff >100 | in_out_diff <(-100))

station_count$outlier = ifelse(station_count$in_out_diff<(-1250),"low",ifelse(station_count$in_out_diff>1500,"high","normal"))
station_count %>% ggplot(aes(x = reorder(start_station,in_out_diff),y=in_out_diff))+
  geom_col(aes(fill = outlier),width = 0.7)+scale_fill_manual(values = c("green","red","lightblue"))+guides(fill = F)+
  labs(x = "station", y = "bikes net input", title = "2017 Stations' Bike Supply")+
  theme(axis.text.y = element_text(size = 7.5),plot.title = element_text(hjust = 0.5))+coord_flip()



#2018
data_2018 = dataset %>% filter(year==2018)
data_2018$start_station = as.factor(data_2018$start_station)
station_count = data_2018 %>% group_by(start_station) %>% summarise(start_count = n()) %>% arrange(start_station)

data_2018$end_station = as.factor(data_2018$end_station)
station_count2 = data_2018 %>% group_by(end_station) %>% summarise(end_count = n())%>% arrange(end_station)

station_count$end_count = station_count2$end_count
station_count$in_out_diff = station_count$end_count - station_count$start_count 
station_count = station_count %>% arrange(in_out_diff)
station_count = station_count %>% filter(in_out_diff >100 | in_out_diff <(-100))

station_count$outlier = ifelse(station_count$in_out_diff<(-2500),"low",ifelse(station_count$in_out_diff>2000,"high","normal"))
station_count %>% ggplot(aes(x = reorder(start_station,in_out_diff),y=in_out_diff))+
  geom_col(aes(fill = outlier),width = 0.7)+scale_fill_manual(values = c("green","red","lightblue"))+guides(fill = F)+
  labs(x = "station", y = "bikes net input", title = "2018 Stations' Bike Supply")+
  theme(axis.text.y = element_text(size = 7.5),plot.title = element_text(hjust = 0.5))+coord_flip()

