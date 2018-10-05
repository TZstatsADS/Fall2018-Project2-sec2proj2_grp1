
#packages 
library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(ggmap)
library(lattice)
library(scales)
library(googleway)
register_google(key = 'AIzaSyAz_yVSZuJDLZE79ouq6HkHQB8Yr3HsqZ4')

register_google(key = "AIzaSyAz_yVSZuJDLZE79ouq6HkHQB8Yr3HsqZ4", account_type = "premium", day_limit = 100000)
#functions 

setwd("C:/Users/jinxi/Documents/semester-2018/applied data science/proj/app")
bikes <- read.csv('./data/citibikeStations2018.csv')
hospital <- read.csv('./data/Hospital.csv')
gardens <- read.csv('./data/Gardens.csv')
gardens <- gardens[!(is.na(gardens$Latitude)),]
libraries <- read.csv('./data/Library.csv')
wifi <- read.csv('./data/wifi.csv')
bins <- read.csv('./data/bins.csv')


#server section 
server <- function(input, output, session) {
  #the goggle api key 
  
  api_key <- "AIzaSyAz_yVSZuJDLZE79ouq6HkHQB8Yr3HsqZ4"
  map_key <- "AIzaSyAz_yVSZuJDLZE79ouq6HkHQB8Yr3HsqZ4"
  
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
  
  
  
  
  
  
  
  
  #the hospitals map 
  output$hospitals <- renderLeaflet({
    # the hospital icon
    hospitalIcon <- icons(
      iconUrl = "./www/hospital.png",
      iconWidth = 25, iconHeight = 20,
      iconAnchorX = 22, iconAnchorY = 94
      
    )
    leaflet(data=hospital)%>% 
      addTiles()%>% 
      addMarkers(~Longitude, ~Latitude, popup = ~as.character(Name), icon=hospitalIcon,
                 label = ~as.character(Name))
    
  })
  #the wifi map 
  output$wifi <- renderLeaflet({
    #the wifi icon
    wifiIcon <- icons(
      iconUrl = "./www/wifi.png",
      iconWidth = 25, iconHeight = 20,
      iconAnchorX = 22, iconAnchorY = 94
    )
    #the wifi map 
    leaflet(data=wifi)%>% 
      addTiles()%>%
      addMarkers( ~long, ~lat, popup = ~as.character(Name), icon = wifiIcon,
                  clusterOptions = markerClusterOptions())
    
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
    paste0("Longitude=", input$plot_click$x, "\nLatitude=", input$plot_click$y, 
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
    paste0("The distance is: ", two_dist$km," kilometers.",
           "\nThe time is: ", two_dist$minutes, " minutes", 
           "\nThe Calories burned is: ", (two_dist$minutes)*10, " calories")
  })
  
  
  
  
}
