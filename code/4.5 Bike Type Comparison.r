#YinWang_BikeTypeAnalysis_Summary Code

library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

data= read_csv("LA_metro.csv")

#number of trip 
  data%>%filter(year=="2018",quarter=="4")%>%group_by(bike_type)%>%summarise( count = n())%>%
    ggplot(aes(x=bike_type,y=count))+geom_col(fill="lightblue")+geom_text(aes(label = count),vjust=-0.3)+ggtitle("Number of trips")+theme(plot.title = element_text(hjust = 0.5))+xlab("Bike Type")
      # 2514 trips of electric bikes, 71353 trips of standard bikes in 2018Q4
#number of bikes
  data%>%filter(year=="2018",quarter=="4")%>%group_by(bike_type)%>%summarise( number = n_distinct(bike_id)) 
  # there were 8 electric bikes and 1265 standard bikes in 2018Q4  
  
#to see number of trips each month in 2018Q4
  data%>%filter(year=="2018",quarter=="4")%>%group_by(month = month(start_time, label = T, abbr = F))%>%summarise(count=n())%>%
    ggplot(aes(x=month,y=count))+geom_col(fill="lightblue")+ggtitle("Trips number in each month(2018Q4)")+geom_text(aes(label = count),vjust=-0.3)+theme(plot.title = element_text(hjust = 0.5))
  data%>%filter(bike_type=="electric")%>%group_by(month = month(start_time, label = T, abbr = F))%>%summarise(count=n())%>%
    ggplot(aes(x=month,y=count))+geom_col(fill="lightblue")+ggtitle("Trips number of electric bikes in each month(2018Q4)")+geom_text(aes(label = count),vjust=-0.3)+theme(plot.title = element_text(hjust = 0.5))
  data%>%filter(year=="2018",quarter=="4",bike_type=="standard")%>%group_by(month = month(start_time, label = T, abbr = F))%>%summarise(count=n())%>%
    ggplot(aes(x=month,y=count))+geom_col(fill="pink")+ggtitle("Trips number of standard bikes in each month(2018Q4)")+geom_text(aes(label = count),vjust=-0.3)+theme(plot.title = element_text(hjust = 0.5))
  
  
#using fluency of each electric bike (compare to standard bike usage fluency)
  data%>%filter(year=="2018",quarter=="4",bike_type=="electric")%>%group_by(bike_id)%>%summarise( count = n())%>%
    ggplot(aes(x=as.factor(bike_id),y=count))+geom_col(fill="lightblue")+geom_text(aes(label = count),vjust=-0.5)+ggtitle("How many times did each electric bike be used in 2018 Q4?")+
    xlab("Electric Bike ID")+theme(plot.title = element_text(hjust = 0.5))
  data%>%filter(year=="2018",quarter=="4",bike_type=="electric")%>%group_by(bike_id)%>%summarise( count = n())%>%summarise(mean(count))
  data%>%filter(year=="2018",quarter=="4",bike_type=="standard")%>%group_by(bike_id)%>%summarise( count = n())%>%summarise(mean(count))
  data%>%filter(year=="2018",quarter=="4",bike_type=="standard")%>%group_by(bike_id)%>%summarise( count = n())%>%arrange(-count)%>%head(8)%>%
    ggplot(aes(x=as.factor(bike_id),y=count))+geom_col(fill="pink")+geom_text(aes(label = count),vjust=-0.5)+ggtitle("How many times did standard bike be used in 2018 Q4? (top8)")+
    xlab("Standard Bike ID")+theme(plot.title = element_text(hjust = 0.5))

  
#to see trip duration distribution of each bike type
  #assume the comman duration is less than 100 minutes
  data%>%filter(year=="2018",quarter=="4",bike_type=="standard",duration<100)%>%group_by(duration)%>%summarise( count = n())%>%
    ggplot(aes(x=duration,y=count))+geom_col(fill = 'lightblue',color ='white',size = 0.8)+ggtitle("Duration distribution of standard bikes")+theme(plot.title = element_text(hjust = 0.5))
  data%>%filter(year=="2018",quarter=="4",bike_type=="electric",duration<100)%>%group_by(duration)%>%summarise( count = n())%>%
    ggplot(aes(x=duration,y=count))+geom_col(fill = 'lightblue',color ='white',size = 0.8)+ggtitle("Duration distribution of electric bikes")+theme(plot.title = element_text(hjust = 0.5))
  
  data%>%filter(year=="2018",quarter=="4",duration<100)%>%summarise( average=mean(duration))
  data%>%filter(year=="2018",quarter=="4",bike_type=="standard")%>%filter(duration<100)%>%summarise( average=mean(duration))
  data%>%filter(year=="2018",quarter=="4",bike_type=="electric")%>%filter(duration<100)%>%summarise( average=mean(duration))
  
  