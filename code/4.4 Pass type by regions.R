# Pass type analysis of different regions

library(ggplot2)
library(dplyr)
library(lubridate)
library(shiny)
 

dataset = read.csv("LA_metro.csv")

# DTLA pass type
DTLA_short = dataset %>% filter(start_region == 'DTLA' & duration<=30)

DTLA =DTLA_short %>% group_by(passholder_type) %>% summarise(count = n())


myLabel_DTLA = as.vector(DTLA$passholder_type)   
myLabel_DTLA = paste(myLabel_DTLA, "(", round(DTLA$count / sum(DTLA$count) * 100, 2), "%)", sep = "")   

myLabel2_DTLA = c(myLabel_DTLA[2],myLabel_DTLA[1])

DTLA %>% ggplot(aes(x = "", y = count, fill = passholder_type)) +
  geom_bar(stat = "identity", width = 1) +    
  coord_polar(theta = "y",start = 120) + 
  labs(x = "", y = "", title = "DTLA Pass Type for Short Trips") +
  theme(axis.ticks = element_blank(),plot.title = element_text(hjust = 0.5),axis.text = element_blank()) + 
  scale_fill_discrete(breaks = DTLA$passholder_type, labels = myLabel_DTLA)




# seaside pass type
SEA = dataset %>% filter(start_region %in% c('Santa Monica','Port LA'))

SEA =SEA %>% group_by(passholder_type) %>% summarise(count = n())


myLabel_SEA = as.vector(SEA$passholder_type)   
myLabel_SEA = paste(myLabel_SEA, "(", round(SEA$count / sum(SEA$count) * 100, 2), "%)", sep = "")   

myLabel2_SEA = c(myLabel_SEA[2],myLabel_SEA[1])

SEA %>% ggplot(aes(x = "", y = count, fill = passholder_type)) +
  geom_bar(stat = "identity", width = 1) +    
  coord_polar(theta = "y",start = 120) + 
  labs(x = "", y = "", title = "Seaside Pass Type for all Trips") +
  theme(axis.ticks = element_blank(),plot.title = element_text(hjust = 0.5),axis.text = element_blank()) + 
  scale_fill_discrete(breaks = SEA$passholder_type, labels = myLabel_SEA)










