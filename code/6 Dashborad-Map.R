library(dplyr)
library(lubridate)
library(readr)
library(ggplot2)
library(shiny)
library(leaflet) 
library(shinydashboard)
library(stringr)

data=read_csv("LA_metro.csv")

data=data%>%
  mutate(quarter=(paste(year(data$start_time),"Q",quarter(data$start_time))))

##shiny app: location_start station usage

header=dashboardHeader(title = "Metro bike operation")

body=dashboardBody(fluidRow(column(width = 9,box(width = NULL, solidHeader = TRUE, leafletOutput("map", height = 500))),
                                                           column(width = 3, box(width = NULL, 
                                                                                 selectInput(inputId = "Time", label = "Select a time period", choices = c("Two Years", levels(as.factor(year(data$start_time))),levels(as.factor(data$quarter)))),
                                                                                 selectInput(inputId = "Location", label = "Select a Location", choices = c("LA","DTLA","Santa Monica"="SantaMonica","Pasadena","Port of LA"="PortofLA")),
                                                                                 selectInput(inputId = "Data",label = "Select a dataset", choices = c("Start Station"="start_station","End Station"="end_station"))
                                                           ))))



ui=dashboardPage(header, dashboardSidebar(disable = TRUE),body)


server=function(input, output){
  
  df=reactive({
    if(input$Data=="start_station")
      return(data%>%
               filter(start_station!=3000)%>%
               group_by(start_station,start_lon, start_lat,quarter,start_region,passholder_type, year)%>%
               summarise(count=n())%>%
               select(station=start_station, lon=start_lon, lat=start_lat,quarter,region=start_region,count,passholder_type,year))
    
    if(input$Data=="end_station")
      return(data%>%
               filter(end_station!=3000)%>%
               group_by(end_station, end_lon, end_lat,quarter, end_region,year)%>%
               summarise(count=n())%>%
               select(station=end_station, lon=end_lon, lat=end_lat,quarter,region=end_region,count,year))
    
  })
  

  hi=reactive({
    if(input$Time=="Two Years")
      return(df())
    
    if(str_detect(string = input$Time,pattern = "Q")==TRUE )
      return(df()[df()$quarter==input$Time,])
    
    if(input$Time=="2017")
      return(df()[df()$year==input$Time,])
    
    if(input$Time=="2018")
      return(df()[df()$year==input$Time,])
  })
  
  location=reactive({
    if(input$Location=="LA")
      return(c(-118.32, 33.9,10))
    
    if(input$Location=="DTLA")
      return(c(-118.25,34.061,12))
    
    if(input$Location=="SantaMonica")
      return(c(-118.45, 34,13))
    
    if(input$Location=="PortofLA")
      return(c(-118.27, 33.75,12))
    
    if(input$Location=="Pasadena")
      return(c(-118.15, 34.15,13))
  })
  
  
  output$map=renderLeaflet({
    pal=colorNumeric(
      palette = c("blue4","black"),
      domain = hi()$count)
    
    leaflet()%>%
      setView(lng =location()[1], lat=location()[2],zoom=location()[3])%>%
      addTiles()%>%
      addLegend(position = "bottomright",pal = pal, values = hi()$count)%>%
      addCircles(data=hi(),
                 lng =hi()$lon, lat = hi()$lat, radius =hi()$count/10,  color = ~pal(hi()$count), fillOpacity = 1,
                 popup = paste("Station:", hi()$station,"  Count:",hi()$count ))
    })
}


shinyApp(ui=ui, server = server)



 
