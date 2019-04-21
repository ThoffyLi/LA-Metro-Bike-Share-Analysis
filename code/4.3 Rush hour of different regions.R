# Rush hour of different regions

library(ggplot2)
library(dplyr)
library(lubridate)
library(shiny)

dataset = read.csv("LA_metro.csv")

# general duration on workdays and weekends
   dataset$weekday = as.character(dataset$weekday)
   dataset$wtype = ifelse(dataset$weekday %in% c('Sat','Sun'),'weekend','workday')
   dataset %>% ggplot(aes(x = duration)) + 
     geom_line(aes(color = wtype),stat = 'density', size = 1,show.legend = F) +
     geom_density(aes(fill = wtype),color = NA, alpha = 0.2)+
     xlim(c(0,100))+ ggtitle("Trip duration: Workdays vs Weekends")+
     theme(plot.title = element_text(hjust = 0.5))
 
   

# the proportion of round trip in SM+Port vs Downtown+Pasadena
   # Santa Monica + Port LA
   data_smpo = dataset %>% filter(start_region %in% c('Santa Monica','Port LA'))
   data_smpo = data_smpo %>% group_by(trip_route_category) %>% summarise(count = n())

   myLabel_smpo = as.vector(data_smpo$trip_route_category)   
   myLabel_smpo = paste(myLabel_smpo, "(", round(data_smpo$count / sum(data_smpo$count) * 100, 2), "%)", sep = "")   
   
   myLabel2_smpo = c(myLabel_smpo[2],myLabel_smpo[1])
   
   data_smpo %>% ggplot(aes(x = "", y = count, fill = trip_route_category)) +
     geom_bar(stat = "identity", width = 1) +    
     coord_polar(theta = "y",start = 120) + 
     labs(x = "", y = "", title = "Trips - Santa Monica & Port LA") +
     theme(axis.ticks = element_blank(),plot.title = element_text(hjust = 0.5),axis.text = element_blank()) + 
     theme(legend.title = element_blank(), legend.position = "right") + 
     scale_fill_discrete(breaks = data_smpo$trip_route_category, labels = myLabel_smpo) + 
     geom_text(aes(y = count/10000 + c(0, cumsum(count)[-length(count)]) + 17220, x = sum(count)/100000 + 0.2, label = myLabel2_smpo), size = 4.5)
 
 
    
    # DTLA + Pasadena
   data_dtpa = dataset %>% filter(start_region %in% c('DTLA','Pasadena'))
   data_dtpa = data_dtpa %>% group_by(trip_route_category) %>% summarise(count = n())
   
   myLabel_dtpa = as.vector(data_dtpa$trip_route_category)   
   myLabel_dtpa = paste(myLabel_dtpa, "(", round(data_dtpa$count / sum(data_dtpa$count) * 100, 2), "%)", sep = "")   
   
   myLabel2_dtpa = c(myLabel_dtpa[2],myLabel_dtpa[1])
   
   data_dtpa %>% ggplot(aes(x = "", y = count, fill = trip_route_category)) +
     geom_bar(stat = "identity", width = 1) +    
     coord_polar(theta = "y",start = 120) + 
     labs(x = "", y = "", title = "Trips - DTLA & Pasadena") +
     theme(axis.ticks = element_blank(),plot.title = element_text(hjust = 0.5),axis.text = element_blank()) + 
     theme(legend.title = element_blank(), legend.position = "right") + 
     scale_fill_discrete(breaks = data_dtpa$trip_route_category, labels = myLabel_dtpa) + 
     geom_text(aes(y = count/100000 + c(0, cumsum(count)[-length(count)]-20000) + 26220, x = sum(count)/370000, label = myLabel2_dtpa), size = 4.3)
   
   
   







