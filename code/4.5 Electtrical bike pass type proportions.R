# The pass type and rush hour analysis of electrical bikes

library(ggplot2)
library(dplyr)
library(lubridate)

data = read.csv("LA_metro.csv")
dataset = data %>% filter(bike_type =="electric")


# total pass type proportions
data_elec =dataset %>% group_by(passholder_type) %>% summarise(count = n())

myLabel1 = as.vector(data_elec$passholder_type)   
myLabel1 = paste(myLabel1, "(", round(data_elec$count / sum(data_elec$count) * 100, 2), "%)", sep = "")   
myLabel2 = c(myLabel1[2],myLabel1[1])

data_elec %>% ggplot(aes(x = "", y = count, fill = passholder_type)) +
  geom_bar(stat = "identity", width = 1) +    
  coord_polar(theta = "y",start = 120) + 
  labs(x = "", y = "", title = "Pass Type of Electrical Bikes") +
  theme(axis.ticks = element_blank(),plot.title = element_text(hjust = 0.5),axis.text = element_blank()) + 
  scale_fill_discrete(breaks = data_elec$passholder_type, labels = myLabel1)




