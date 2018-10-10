#packages
packages.used=c("shiny","shinydashboard","leaflet","dplyr","ggmap","lattice","scales","googleway","huxtable")
packages.needed=setdiff(packages.used, 
                        intersect(installed.packages()[,1], 
                                  packages.used))

if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE)
}

library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(ggmap)
library(lattice)
library(scales)
library(googleway)
library(huxtable)
library(plotly)
library(tmap)
library(tmaptools)
data('World')
source('../lib/register_google.R')

getwd()
register_google(key = 'AIzaSyAMMFd-5P1JoxxN0wn4APqya4L1VSAEBvw')

register_google(key = "AIzaSyAMMFd-5P1JoxxN0wn4APqya4L1VSAEBvw", account_type = "premium", day_limit = 100000)
#functions 

bikes <- read.csv('../data/citibikeStations2018.csv')

gardens <- read.csv('../data/Gardens.csv')
gardens <- gardens[!(is.na(gardens$Latitude)),]
bins <- read.csv('../data/bins.csv')
libraries <- read.csv('../data/Library.csv')
temp <- read.csv('../data/temp.csv', header = T)
temp$Date <- as.Date(temp$Date, "%Y-%m-%d")
sea <- read.csv('../data/sealevel1.csv', header = T)
sea$Date <- as.Date(sea$Date, '%Y-%m-%d')
co2 <- read.csv('../data/co2.csv', header = T)
co2$Date <- as.Date(co2$Date, "%Y-%m-%d")
so2 <- read.csv('../data/so2.csv', header = T)
pm <- read.csv('../data/pm.csv', header = T)

#server section 
server <- function(input, output, session) {
  #the goggle api key 
  
  api_key <- "AIzaSyAMMFd-5P1JoxxN0wn4APqya4L1VSAEBvw"
  map_key <- "AIzaSyAMMFd-5P1JoxxN0wn4APqya4L1VSAEBvw"
  
  #the chart section 
  #the temperature chart 
  output$temp <- renderPlotly({
   p <- ggplot(temp, aes(x=Date, y=Mean))+
     geom_line(col='orange')+
     geom_smooth(method='lm', col='steelblue', linetype=1)+
     labs(title='Global Temperature Changes', x= 'Year', y='Temperature Difference')
   p <- ggplotly(p)
   p
    
  })
  
  output$temptext <- renderText({
    "As you can see the chart, the global temperature increases hugely with years, especially recent decades."
  })
  
  # the sea level chart 
  
  output$sealevel <- renderPlotly({
    p <- ggplot(sea, aes(x=Date, y=change))+
      geom_line(col='steelblue')+
      geom_smooth(method='lm', col='red', linetype=1)+
      labs(title='The Sea level Changes', x= 'Year', y='Sea Level Difference')
    p <- ggplotly(p)
    p
    
  })
  
  output$sealeveltext <- renderText({
    "From this chart, the global sea level is increasing largely since 1900. Also, the shape of line is becoming bigger and bigger. "
  })
  
  
  
  #the co2 chart
  
  output$co2level <- renderPlotly({
    p <- ggplot(co2, aes(x=Date, y=Mean))+
      geom_line(col='steelblue')+
      geom_smooth(method='lm', col='red', linetype=1)+
      labs(title='The Global Carbon Dioxide Changes', x= 'Year', y='CO2 PPM')
    p <- ggplotly(p)
    p
    
  })
  output$co2text <- renderText({
    "From the graph, the CO2 is keeping going up with years, and the situation is also becoming worse and worse!"
  })
  
  
  # the so2 chart
  output$so2 <- renderPlotly({
    p <-  ggplot(so2, aes(x=Year, y=million, fill=Entity))+
      geom_area()+
      labs(title='The Global SO2 Emission Changes', x="Year",y="Millions Tonnes")
    
    p <- ggplotly(p)
    p
    
  })
  
  output$so2text <- renderText({
    "From this area chart, during this period of time,Europe, North America and Aisa made the 
    most contribution to the global SO2 emission, and the Aisa is becoming the biggest part during recent years."
    
  })
  
  
  # the pm2.5 chart 
  output$pm25 <- renderPlot({
    
  
    World <- append_data(World, pm[pm$Year==input$year,], key.shp='name', key.data='Country', ignore.na=T, ignore.duplicates = T)
    tm_shape(World)+
      tm_polygons("pm2.5")
    
  })
   output$pmtext <- renderText({
     "From this global heatmap, the PM2.5 of Aisa and Africa is becoming worse and worse, espcially the middle east, India and China."
     
   })
  
  
  
  
  
  
  # the bike map 
  output$bikes <- renderLeaflet({
    #the bike icon
    bikeIcon <- icons(
      iconUrl = "./www/bike1.png",
      iconWidth = 25, iconHeight = 20,
      iconAnchorX = 22, iconAnchorY = 94
    )
    #the bike map 
    leaflet(data=bikes)%>% 
      addTiles()%>%
      addMarkers( ~long, ~lat, popup = ~as.character(Name), icon = bikeIcon,
                  clusterOptions = markerClusterOptions())
    
    
    
  })
  
  #the Graden map 
  output$gardens <- renderLeaflet({
    greenLeafIcon <- makeIcon(
      iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-green.png",
      iconWidth = 20, iconHeight = 20,
      iconAnchorX = 20, iconAnchorY = 20
      
    )
    
    leaflet(data = gardens) %>% addTiles() %>%
      addMarkers(~Longitude, ~Latitude, icon = greenLeafIcon,popup = ~as.character(Name),
                 label = ~as.character(Name),clusterOptions = markerClusterOptions())
    
  })
  
  
  
  
  
  

  #the bins map 
  output$bins <- renderLeaflet({
    #the bins icon
    binIcon <- icons(
      iconUrl = "./www/bin.png",
      iconWidth = 25, iconHeight = 20,
      iconAnchorX = 22, iconAnchorY = 94)
    
    leaflet(data=bins)%>% 
      addTiles()%>% 
      addMarkers(~long, ~lat, popup = ~as.character(Address), icon=binIcon,
                 clusterOptions = markerClusterOptions() )
    
    
  })

  #the libraries map 
  output$libraries <- renderLeaflet({
    #the library icon
    libraryIcon <- icons(
      iconUrl = "./www/library.png",
      iconWidth = 25, iconHeight = 20,
      iconAnchorX = 22, iconAnchorY = 94)
    
    leaflet(data=libraries)%>% 
      addTiles()%>% 
      addMarkers(~long, ~lat, popup = ~as.character(Name), icon=libraryIcon,
                 clusterOptions = markerClusterOptions() )
    
    
  })
  
  

  
  # find the lat and long of address 
  output$mapOut <-renderPlot({
    map <- get_map(location =c(lon=-73.9851, lat=40.7589), maptype = 'roadmap',scale = 1, zoom = 12)
    mapPoints <- ggmap(map, extent = 'normal')
    mapPoints+coord_cartesian()
  })
  # the text
  output$coords <- renderText({
    paste0("Longitude=", round(input$plot_click$x,6), "\nLatitude=", round(input$plot_click$y,6), 
           "\nThe address is: ", revgeocode(c(input$plot_click$x,input$plot_click$y)))
  })
  
  
  # the recommend route and calories burned section
  df_route <- eventReactive(input$getRoute, {print("Getting route")
    
    o <- input$origin
    d <- input$destination
    return(data.frame(origin=o, destination=d, stringsAsFactors = F))})
  
  output$myMap <- renderGoogle_map({
    df <- df_route()
    print(df)
    if(df$origin==" "| df$destination==" ")
      return()
    res <- google_directions(
      key=api_key,
      origin = df$origin, 
      destination = df$destination,
      mode = 'bicycling'
    )
    
    df_route <- data.frame(route=res$routes$overview_polyline$points)
    google_map(key=map_key)%>% 
      add_polylines(data=df_route, polyline = 'route')
  })
  
  # the data about time, distance, calories 
  
  output$showdata <- renderText({
    two_dist <- mapdist(from=input$origin, to=input$destination, mode = 'bicycling')
    two_dist_driving <- mapdist(from=input$origin, to=input$destination, mode = 'driving')
    paste0("The distance is: ", round(two_dist$km/1.61,3)," miles.",
           "\n",
           "\nThe bicycling time is: ", round(two_dist$minutes,3), " minutes", 
           "\n",
           "\nThe driving time is:", round(two_dist_driving$minutes,3), " minutes",
           "\n",
           "\nThe Calories burned is: ", round((two_dist$minutes)*10,3), " calories, if you go bicycling", 
           "\n",
           "\nYou just reduce: ",round(((two_dist$km)/1.61)*440.95,2), " grams of carbon dioxide!!!!", 
           "\n",
           "\nThank You for saving the world!")
  })
  
  
  
  
}








