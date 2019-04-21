library(dplyr)
library(ggplot2)
library(lubridate)
library(shiny)
library(stringr)

###READ DATA
data=read_csv("LA_metro.csv")

####create los angeles city map _ start station 

library(ggmap)
register_google(key="AIzaSyCHcc5V8brlMlxoQEhj0i4noN7EE0o-BrU")
location=c(-118.3, 34)
map1=get_map(location=location, source = "google", zoom=10, maptype = "terrain",color = "color")
la=ggmap(map1)

###LA 2-year station usage


data%>%
  filter(!is.na(start_region))%>%
  group_by(start_region,quarter)%>%
  summarise(count=n())%>%
  ggplot( aes(x=quarter, y=count, fill=start_region))+
  geom_col(position = "dodge")+
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")+
  xlab("Quarter")+
  ylab("Count")+
  ggtitle("Metro bike quarterly station usage by region")



hi=data%>%
  filter(start_station != 3000)%>%
  group_by(start_lon, start_lat, start_region, quarter)%>%
  summarise(count=n())



la+geom_point(data=hi,aes(x=start_lon, y=start_lat, size=count),alpha=0.5, color="blue4")+
  facet_wrap(~quarter,nrow = 2)+
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")+
  xlab("Longtitude")+
  ylab("Latitude")+
  ggtitle("Metro bike 2-year station usage")


#downtown la with start station map
map2=get_map(location=c(lon=-118.25, lat=34.05), source = "google", zoom=13, maptype = "terrain",color = "color")
dtmap=ggmap(map2)

hi_dtla=hi%>%
  filter(start_region=="DTLA")

dtmap+geom_point(data=hi_dtla,aes(x=start_lon, y=start_lat, size=count),alpha=0.5, color="blue4")+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom")+
  xlab("Longtitude")+
  ylab("Latitude")+
  ggtitle("Metro bike 2-year DTLA station usage")


dtmap+stat_density2d(data=hi_dtla,aes(x=start_lon, y=start_lat, fill=..level..), alpha=0.5,size=1.5,geom="polygon")+
  scale_fill_gradient(low="white", high="darkred")+
  theme_void()

##pasadena
Pasadena=get_map(location=c(lon=-118.15, lat=34.15), source = "google", zoom=13, maptype = "terrain",color = "color")
ggmap(Pasadena)

hi_pasadena=hi%>%
  filter(start_region=="Pasadena")

ggmap(Pasadena)+geom_point(data=hi_pasadena,aes(x=start_lon, y=start_lat, size=count),alpha=0.5, color="blue4")+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom")+
  xlab("Longtitude")+
  ylab("Latitude")+
  ggtitle("Metro bike 2-year Pasadena station usage")



##santa monica
map4=get_map(location=c(lon=-118.45, lat=34), source = "google", zoom=13, maptype = "terrain",color = "color")
santa=ggmap(map4)

hi_santa=hi%>%
  filter(start_region=="Santa Monica")

santa+geom_point(data=hi_santa,aes(x=start_lon, y=start_lat, size=count),alpha=0.5, color="blue4")+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom")+
  xlab("Longtitude")+
  ylab("Latitude")+
  ggtitle("Metro bike 2-year Santa Monica station usage")


##long beach
map5=get_map(location=c(lon=-118.27, lat=33.75), source = "google", zoom=13, maptype = "terrain",color = "color")
portla=ggmap(map5)

hi_port=hi%>%
  filter(start_region=="Port LA")

portla+geom_point(data=hi_port,aes(x=start_lon, y=start_lat, size=count),alpha=0.5, color="blue4")+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom")+
  xlab("Longtitude")+
  ylab("Latitude")+
  ggtitle("Metro bike 2-year Port of LA station usage")


### route map_ bike return
return_type=data%>%
  filter(!is.na(start_region)& !is.na(end_region))%>%
  mutate(return_type=ifelse(start_station==end_station,"Same station","Different stations"))%>%
  group_by(start_lon, start_lat, end_lat, end_lon,start_region,end_region,return_type, year)%>%
  summarise(count=n())

#same station vs different station
ggplot(return_type,aes(x=start_region,y=count,fill=return_type))+
  geom_col(position="dodge")+
  facet_wrap(~quarter, nrow=2)+
  xlab("Start Region")+
  ggtitle("Same-station return vs. Different-station return")+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom")

#different-station return: same region vs. different regions    
return_type=return_type%>%
  filter(return_type=="Different stations")%>%
  mutate(return_region=ifelse(start_region==end_region,"Same region","Different regions"))

return_dif_region=return_type%>%
  group_by(start_region, return_region)%>%
  summarise(count=n())

return_dif_region%>%
  filter(return_region=="Different regions")%>%
  ggplot(aes(x=start_region, y=count))+
  geom_col()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom")+
  ggtitle("Cross-region return by Start Region")+
  ylab("Count")+
  xlab("Start Region")

sum=function(x){
  y=return_dif_region$count[2*x]+return_dif_region$count[2*x-1]
  return(y)
}
sum(1)
sum=sum(c(1,1,2,2,3,3,4,4))
sum=data.frame(sum)
bind_cols(return_dif_region,sum)%>%
  mutate(percentage=count/sum)%>%
  ggplot(aes(x="", y=percentage, fill=return_region)) + geom_bar(stat="identity", width=1)+
  facet_wrap(~start_region, nrow=1)+
  coord_polar("y", start=0) + geom_text(aes(label = paste0(round(percentage*100), "%")), position = position_stack(vjust = 0.5))+
  theme_void()+  
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom")+
  ggtitle("Same-region return vs. Different-region return")

##route map for across region return
return_dif_region=return_type%>%
  filter(start_region != end_region)%>%
  group_by(start_region, end_region,start_lon, start_lat, end_lat, end_lon, year)%>%
  summarise(count=n())%>%
  mutate(route=paste(start_region,"-",end_region))


la+geom_curve(data=return_dif_region,aes(x=start_lon, y=start_lat,xend=end_lon, yend=end_lat, color=route))+
  geom_point(data = return_dif_region,aes(x=start_lon, y=start_lat),color="brown", alpha=0.5)+
  geom_point(data = return_dif_region,aes(x=end_lon, y=end_lat), color="pink", alpha=0.25)+
  coord_cartesian(ylim = c(33.63,34.5),xlim = c(-119,-117.6))+
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom")+
  ggtitle("Cross-region return route map")


###region shortage
d_start=return_dif_region%>%
  group_by(start_region, year)%>%
  summarise(count_start=n())

d_end=return_dif_region%>%
  group_by(end_region, year)%>%
  summarise(count_end=n())

left_join(d_start, d_end, by=c("year"="year","start_region"="end_region"))%>%
  mutate(number=count_end-count_start)%>%
  ggplot(aes(y=number, x=start_region, fill=as.factor(year)))+
  geom_col(position="dodge")+
  ylab("Number of Cross-region returned bikes")+
  xlab("Region")+
  coord_flip()+
  ggtitle("Regional Bike Shortage")+
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom")


