library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)


# to draw the graph of the quantity of electric bike 
# that are borrowed and returned in each station
# to see which sation are most popular
data = read_csv('LA_metro.csv')

station = data %>% 
  filter(year == '2018' & quarter == '4'& bike_type == 'electric') %>% 
  select(start_station, end_station)

start = station %>% 
  select(start_station) %>% 
  group_by(start_station) %>% 
  summarise(start = n())

end = station %>% 
  select(end_station) %>% 
  group_by(end_station) %>% 
  summarise(end = n())

a = unique(station$start_station)
b = unique(station$end_station)
c = unique(c(a, b))
c = data.frame(c)
c$c = as.character(c$c)
start$start_station = as.character(start$start_station)
end$end_station = as.character(end$end_station)
c = left_join(c, start, by = c('c' = 'start_station'))
c = left_join(c, end, by = c('c' = 'end_station'))
c = c %>% 
  gather(start, end, key = 'status', value = 'count')

c %>% 
  ggplot(aes(x = c, y = count, fill = status)) + 
  geom_col(position = 'dodge') + 
  xlab('') + 
  ylab('') + 
  coord_flip() + 
  ggtitle('The amount of electric bike borrowed and returned in each station') + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
