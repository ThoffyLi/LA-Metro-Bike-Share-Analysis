# Shiny dashboard of rush hour~year/quarter/region


library(ggplot2)
library(dplyr)
library(lubridate)
library(shiny)

dataset = read.csv("LA_metro.csv")


# The User Interface
ui = fluidPage(
  sidebarLayout(
    # for input
    sidebarPanel(
      textInput(inputId = "title1", label = "Title1"),
      textInput(inputId = "title2", label = "Title2"),
      selectInput(inputId = "year", label = "Year",
                  choices = c('2017','2018','Total'),selected = 'Total'),
      selectInput(inputId = "quarter", label= "Quarter",
                  choices = c('1','2','3','4','Total'),selected = 'Total'),
      selectInput(inputId = "region", label= "Region",
                  choices = c(levels(as.factor(dataset$start_region)),'Total'),selected = 'Total')
    ),
    
    # for output
    mainPanel(
      plotOutput(outputId = "plot1"),
      plotOutput(outputId = "plot2")
      
    )
  )
)

# The server

server = function(input, output){
  
  #divide by half hour
  dataset$time = ifelse(dataset$minute<30,dataset$hour,dataset$hour+0.5) 
 

  output$plot1 = renderPlot({
    
    if(input$year == 'Total'){
      if(input$quarter == 'Total'){
         if(input$region =='Total'){
           genRH = dataset %>% group_by(weekday,as.factor(time)) %>% summarise(count = n())
         }
        else{
          genRH = dataset %>% filter(start_region ==input$region) %>% group_by(weekday,as.factor(time)) %>% summarise(count = n())
        }
      }
      else{
        if(input$region =='Total'){
          genRH = dataset %>%filter(quarter ==as.numeric(input$quarter)) %>%  group_by(weekday,as.factor(time)) %>% summarise(count = n())
        }
        else{
          genRH = dataset %>% filter(start_region ==input$region & quarter ==as.numeric(input$quarter)) %>% group_by(weekday,as.factor(time)) %>% summarise(count = n())
        }
        
      }
    }
    
    else{
      if(input$quarter == 'Total'){
        if(input$region == 'Total'){
          genRH = dataset %>% filter(year ==as.numeric(input$year)) %>% group_by(weekday,as.factor(time)) %>% summarise(count = n())
        }
        else{
          genRH = dataset %>% filter(year ==as.numeric(input$year)&start_region ==input$region) %>% group_by(weekday,as.factor(time)) %>% summarise(count = n())
        }
      }
      else{
        if(input$region == 'Total'){
          genRH = dataset %>% filter(quarter ==as.numeric(input$quarter) & year ==as.numeric(input$year)) %>% group_by(weekday,as.factor(time)) %>% summarise(count = n())
        }
        else{
          genRH = dataset %>% filter(quarter ==as.numeric(input$quarter) & year ==as.numeric(input$year)&start_region ==input$region) %>% group_by(weekday,as.factor(time)) %>% summarise(count = n())
        }
        
      }
    }
    
    colnames(genRH)[2] = "time"
    genRH$time = as.numeric(as.character(genRH$time))
    
    genRH %>% ggplot(aes(x = weekday, y = time, fill = count))+
      geom_tile()+scale_fill_gradient(low = "white", high = "darkred")+
      scale_y_continuous(breaks = seq(0,23,2))+
      xlab("Weekday")+ggtitle(input$title1)+
      theme(plot.title = element_text(hjust = 0.5))+
      scale_x_discrete(limits = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"))
    
    
  })
  
  
  output$plot2 = renderPlot({
    if(input$year == 'Total'){
       if(input$quarter == 'Total'){
         if(input$region == 'Total'){
           dataset = dataset
         }
         else{
           dataset = dataset %>% filter(start_region == input$region)
         }
      }
      
      else{
        
        if(input$region == 'Total'){
          dataset = dataset %>% filter(quarter == input$quarter)
        }
        else{
          dataset = dataset %>% filter(start_region == input$region & quarter == input$quarter)
        }
      }
    }
    
    else{
      if(input$quarter == 'Total'){
        if(input$region == 'Total'){
          dataset = dataset %>% filter(year == input$year)
        }
        else{
          dataset = dataset %>% filter(year == input$year & start_region == input$region)
        }
       }
      else{
        
        if(input$region == 'Total'){
          dataset = dataset %>% filter(year == input$year & quarter == input$quarter)
        }
        else{
          dataset = dataset %>% filter(year == input$year & start_region == input$region & quarter == input$quarter)
        }
        
      }
      
    }

    dataset %>% ggplot(aes(x = duration, y =..density..))+
      geom_histogram(fill = 'lightblue',color ='white',binwidth = 1,size = 0.8)+
      xlim(c(0,100))+ 
      xlab('Duration')+
      ylab('Density')+
      ggtitle(input$title2)+theme(plot.title = element_text(hjust = 0.5))
  })
  
}


# Combine and run the app
shinyApp(ui, server)