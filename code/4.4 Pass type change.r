#Passholder type analysis

library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)

data = read_csv("LA_metro.csv")

#2017+2018 number of different passholder type
data%>%group_by(passholder_type,year)%>%summarise(count=n())%>%
  ggplot(aes(x=passholder_type,y=count,fill=as.factor(year)))+
  geom_col(position=position_dodge())+ggtitle("Number of Different Passholder Type")+
  theme(plot.title = element_text(hjust = 0.5),legend.title = element_blank())


#2017 data
d1=data%>%filter(year(start_time)==2017)

##number of Walk-up changing by month
d1%>%filter(passholder_type == "Walk-up")%>%group_by(month = month(start_time, label = T, abbr = F))%>%summarise(count=n())%>%
  ggplot(aes(x=month,y=count,fill=month))+geom_col()+ggtitle("Number of Walk-up Holders for Each Month in 2017")+theme(plot.title = element_text(hjust = 0.5))

##number of One Day Pass changing by month
d1%>%filter(passholder_type == "One Day Pass")%>%group_by(month = month(start_time, label = T, abbr = F))%>%summarise(count=n())%>%
  ggplot(aes(x=month,y=count,fill=month))+geom_col()+ggtitle("number of One Day Pass holders for each month in 2017")+theme(plot.title = element_text(hjust = 0.5))

##number of Monthly Pass changing by month
d1%>%filter(passholder_type == "Monthly Pass")%>%group_by(month = month(start_time, label = T, abbr = F))%>%summarise(count=n())%>%
  ggplot(aes(x=month,y=count,fill=month))+geom_col()+ggtitle("number of Monthly Pass holders for each month in 2017")+theme(plot.title = element_text(hjust = 0.5))

##number of Flex Pass changing by month
d1%>%filter(passholder_type == "Flex Pass")%>%group_by(month = month(start_time, label = T, abbr = F))%>%summarise(count=n())%>%
  ggplot(aes(x=month,y=count,fill=month))+geom_col()+ggtitle("number of Flex Pass holders for each month in 2017")+theme(plot.title = element_text(hjust = 0.5))


#2018
d2=data%>%filter(year(start_time)==2018)

##number of Walk-up changing by month
d2%>%filter(passholder_type == "Walk-up")%>%group_by(month = month(start_time, label = T, abbr = F))%>%summarise(count=n())%>%
  ggplot(aes(x=month,y=count,fill=month))+geom_col()+ggtitle("number of Walk-up holders for each month in 2018")+theme(plot.title = element_text(hjust = 0.5))

##number of One Day Pass changing by month
d2%>%filter(passholder_type == "One Day Pass")%>%group_by(month = month(start_time, label = T, abbr = F))%>%summarise(count=n())%>%
  ggplot(aes(x=month,y=count,fill=month))+geom_col()+ggtitle("number of One Day Pass holders for each month in 2018")+theme(plot.title = element_text(hjust = 0.5))

##number of Monthly Pass changing by month
d2%>%filter(passholder_type == "Monthly Pass")%>%group_by(month = month(start_time, label = T, abbr = F))%>%summarise(count=n())%>%
  ggplot(aes(x=month,y=count,fill=month))+geom_col()+ggtitle("number of Monthly Pass holders for each month in 2018")+theme(plot.title = element_text(hjust = 0.5))

##number of Flex Pass changing by month
d2%>%filter(passholder_type == "Flex Pass")%>%group_by(month = month(start_time, label = T, abbr = F))%>%summarise(count=n())%>%
  ggplot(aes(x=month,y=count,fill=month))+geom_col()+ggtitle("number of Flex Pass holders for each month in 2018")+theme(plot.title = element_text(hjust = 0.5))





#Quarter 2017
##number of Walk-up changing by Quarters
d1%>%filter(year(start_time)==2017)%>%filter(passholder_type == "Walk-up")%>%group_by(quarter)%>%summarise(count=n())%>%
  ggplot(aes(x=quarter,y=count,fill=quarter))+geom_col()+ggtitle("number of Walk-up holders for each Quarter in 2017")+theme(plot.title = element_text(hjust = 0.5))

##number of One Day Pass changing by Quarters
d1%>%filter(year(start_time)==2017)%>%filter(passholder_type == "One Day Pass")%>%group_by(quarter)%>%summarise(count=n())%>%
  ggplot(aes(x=quarter,y=count,fill=quarter))+geom_col()+ggtitle("number of One Day Pass holders for each Quarter in 2017")+theme(plot.title = element_text(hjust = 0.5))

##number of Walk-up changing by Quarters
d1%>%filter(year(start_time)==2017)%>%filter(passholder_type == "Monthly Pass")%>%group_by(quarter)%>%summarise(count=n())%>%
  ggplot(aes(x=quarter,y=count,fill=quarter))+geom_col()+ggtitle("number of Monthly Pass holders for each Quarter in 2017")+theme(plot.title = element_text(hjust = 0.5))

##number of Flex Pass changing by Quarters
d1%>%filter(year(start_time)==2017)%>%filter(passholder_type == "Monthly Pass")%>%group_by(quarter)%>%summarise(count=n())%>%
  ggplot(aes(x=quarter,y=count,fill=quarter))+geom_col()+ggtitle("number of Flex Pass holders for each Quarter in 2017")+theme(plot.title = element_text(hjust = 0.5))



#Quarter 2018
##number of Walk-up changing by Quarters
d2%>%filter(year(start_time)==2018)%>%filter(passholder_type == "Walk-up")%>%group_by(quarter)%>%summarise(count=n())%>%
  ggplot(aes(x=quarter,y=count,fill=quarter))+geom_col()+ggtitle("number of Walk-up holders for each Quarter in 2018")+theme(plot.title = element_text(hjust = 0.5))

##number of One Day Pass changing by Quarters
d2%>%filter(year(start_time)==2018)%>%filter(passholder_type == "One Day Pass")%>%group_by(quarter)%>%summarise(count=n())%>%
  ggplot(aes(x=quarter,y=count,fill=quarter))+geom_col()+ggtitle("number of One Day Pass holders for each Quarter in 2018")+theme(plot.title = element_text(hjust = 0.5))

##number of Walk-up changing by Quarters
d2%>%filter(year(start_time)==2018)%>%filter(passholder_type == "Monthly Pass")%>%group_by(quarter)%>%summarise(count=n())%>%
  ggplot(aes(x=quarter,y=count,fill=quarter))+geom_col()+ggtitle("number of Monthly Pass holders for each Quarter in 2018")+theme(plot.title = element_text(hjust = 0.5))

##number of Flex Pass changing by Quarters
d2%>%filter(year(start_time)==2018)%>%filter(passholder_type == "Monthly Pass")%>%group_by(quarter)%>%summarise(count=n())%>%
  ggplot(aes(x=quarter,y=count,fill=quarter))+geom_col()+ggtitle("number of Flex Pass holders for each Quarter in 2018")+theme(plot.title = element_text(hjust = 0.5))

