# Clean raw data(8 trip csv files and 1 station csv file)
# output clean data


library(dplyr)
library(ggplot2)
library(lubridate)
library(shiny)

# Chinese -> English labels
Sys.setlocale("LC_TIME", "English")


# read in raw data
q42018=read.csv("metro-bike-share-trips-2018-q4.csv")
q32018=read.csv("metro-bike-share-trips-2018-q3.csv")
q22018=read.csv("metro-bike-share-trips-2018-q2.csv")
q12018=read.csv("metro-bike-share-trips-2018-q1.csv")
q42017=read.csv("metro-bike-share-trips-2017-q4.csv")
q32017=read.csv("metro-bike-share-trips-2017-q3.csv")
q22017=read.csv("metro-bike-share-trips-2017-q2.csv")
q12017=read.csv("metro-bike-share-trips-2017-q1.csv")

# unify column names
colnames(q12017)[c(5,8)]=c("start_station", "end_station")

q12017$bike_id=as.numeric(q12017$bike_id)

q12017$start_time=mdy_hm(q12017$start_time)
q12017$end_time=mdy_hm(q12017$end_time)


# time dimension: minute
q12017$duration=
  q12017$end_time-q12017$start_time


q32017$start_time=mdy_hm(q32017$start_time)
q32017$end_time=mdy_hm(q32017$end_time)

q22017$start_time=ymd_hms(q22017$start_time)
q22017$end_time=ymd_hms(q22017$end_time)

q42017$start_time=ymd_hms(q42017$start_time)
q42017$end_time=ymd_hms(q42017$end_time)

q12018$start_time=ymd_hms(q12018$start_time)
q12018$end_time=ymd_hms(q12018$end_time)

q22018$start_time=ymd_hms(q22018$start_time)
q22018$end_time=ymd_hms(q22018$end_time)

q32018$start_time=ymd_hms(q32018$start_time)
q32018$end_time=ymd_hms(q32018$end_time)

q42018$start_time=ymd_hms(q42018$start_time)
q42018$end_time=ymd_hms(q42018$end_time)


# combine all data together
data=rbind(q12017,q22017,q32017,q42017,q12018,q22018,q32018)

data=data%>%
  mutate("bike_type"="standard")

data=rbind(q42018,data)   ###end of all data combining 



# deal with inaccurate data
  ## find that there is one row the start_station is not in LA, but with an 118 lon
  ##the station no is 3039, according to other records of 3039 station
  ##it is clearly an error, so that we modify the wrong data to make it alien with the others

data$start_lon=ifelse(data$start_station==3039, -118.3939, data$start_lon)
data$start_lat=ifelse(data$start_station==3039, 34.02448, data$start_lat)

s3000=data%>%
  filter(start_station==3000)

##clean end_station
data%>%
  filter(end_lon>=0)%>%
  group_by(end_station)%>%
  summarise(n())

data$end_lon=ifelse(data$end_station==4108,-118.2382, data$end_lon)
data$end_lat=ifelse(data$end_station==4108, 34.02589, data$end_lat)


#create new variable start_region
station=read.csv("metro-bike-share-stations-2019-01-07.csv")
levels(station$Region)

pasa_station=station%>%
  filter(Region=="Pasadena")

DTLA_satation=station%>%
  filter(Region=="DTLA")

port_station=station%>%
  filter(Region=="Port of LA")

venice_station=station%>%
  filter(Region=="Venice")

data=data%>%
  mutate(start_region=ifelse(start_station %in% pasa_station$Station_ID,"Pasadena",
                             ifelse(start_station %in% DTLA_satation$Station_ID,"DTLA",
                                    ifelse(start_station %in% port_station$Station_ID,"Port LA",
                                           ifelse(start_station %in% venice_station$Station_ID, "Santa Monica",NA)))))%>%
  mutate( end_region=ifelse(end_station %in% pasa_station$Station_ID,"Pasadena",
                            ifelse(end_station %in% DTLA_satation$Station_ID,"DTLA",
                                   ifelse(end_station %in% port_station$Station_ID,"Port LA",
                                          ifelse(end_station %in% venice_station$Station_ID, "Santa Monica",NA)))))

#set annual pass = flex pass
data$passholder_type = as.character(data$passholder_type)
data$passholder_type[data$passholder_type == "Annual Pass"] ="Flex Pass"
data$passholder_type = as.factor(data$passholder_type)
levels(data$passholder_type)

# extract weekday,quarter,year,hour,minute
data$hour = hour(data$start_time)
data$minute = minute(data$start_time)
data$year = year(data$start_time)
data$month = month(data$start_time)
data$quarter = ifelse(data$month>=10,4,ifelse(data$month>=7,3,ifelse(data$month>=4,2,1)))
data$weekday = wday(data$start_time,label = T)


write.csv(data, file="LA_metro.csv")
