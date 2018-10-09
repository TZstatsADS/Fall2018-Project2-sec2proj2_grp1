
#packages 
library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(ggmap)
library(lattice)
library(scales)
library(googleway)
library(huxtable)
source('../lib/register_google.R')

#this is my own api key 
register_google(key = 'AIzaSyCq8VzY3P1qQvvCA5ERHk-ueMS8Q8LYo8A')

#you have to get your own api key to run the code 
#this is the link to get the api key https://developers.google.com/maps/documentation/javascript/get-api-key

register_google(key = "AIzaSyCq8VzY3P1qQvvCA5ERHk-ueMS8Q8LYo8A", account_type = "premium", day_limit = 100000)
#functions 

bikes <- read.csv('../data/citibikeStations2018.csv')
hospital <- read.csv('../data/Hospital.csv')
gardens <- read.csv('../data/Gardens.csv')
gardens <- gardens[!(is.na(gardens$Latitude)),]
libraries <- read.csv('../data/Library.csv')
wifi <- read.csv('../data/wifi.csv')
bins <- read.csv('../data/bins.csv')


#ui section 
ui <- dashboardPage( skin = 'green', # green lives 
                     #the header section
                     dashboardHeader(title = "How Riding Bikes Brings You a Green Life", titleWidth = 400,
                                     dropdownMenu(
                                       type = 'messages',
                                       messageItem( from ='Action ', message = "Build a green world for future", 
                                                    icon = icon('life-ring'))
                                     )),
                     
                     #the sidebar section 
                     
                     dashboardSidebar(
                       sidebarMenu(
                         #frist part: message for users 
                         menuItem("Message", tabName = 'message', icon = icon("leaf",class=NULL,lib="font-awesome")),
                         
                         #second part: the functions of app
                         menuItem("Green Lives", icon = icon('globe',lib='font-awesome'), 
                                  # the bikes map 
                                  menuSubItem('Find Bikes', tabName = 'bikes', icon = icon('bicycle',lib='font-awesome')),
                                  #the gardens map
                                  menuSubItem('Find Garden', tabName = 'gardens', icon = icon('tree', lib = 'font-awesome')),
                                  #the hospitals map
                                  menuSubItem('Find Hospital', tabName = 'hospitals',icon = icon('medkit', lib = 'font-awesome')),
                                  #the wifi map 
                                  menuSubItem('Find Wifi', tabName = 'wifi', icon = icon('wifi', lib = 'font-awesome')), 
                                  #the recycling bins map 
                                  menuSubItem('Find Recycling Bins', tabName = 'bins', icon =icon('trash',lib='font-awesome')),
                                  #the library map 
                                  menuSubItem('Find Library',tabName = 'libraries', icon = icon('book',lib='font-awesome'))
                         ), 
                         #third section: calculate the calories 
                         menuItem("Burning Calories", icon = icon('angellist', lib = 'font-awesome'), 
                                  menuSubItem('Find Address', tabName='address',icon=icon('map-marker',lib='font-awesome')),
                                  menuSubItem("Calories Burned", tabName = 'calories', icon = icon('location-arrow', lib='font-awesome'))
                         )
                         
                       )
                       
                     ),
                     
                     
                     
                     
                     dashboardBody(
                       
                       tabItems(
                         #message part: write down the messages for users to take action
                         tabItem(tabName = 'message',
                                 h1('Message for Users', align='center', style = "font-family: 'Lobster', cursive;
                                    font-weight: 500; line-height: 1.1; 
                                    color: #4d3a7d;"), 
                                 h4("Please write down your message here.XXXXXXXXXXXX"),
                                  img(src="green.png", height=140, width=400)
                                 
                                 ), 
                         #bike map, creat the bike map for users to find the nearest bike stations
                         tabItem(tabName = 'bikes', 
                                 h1('Citi Bike Map',  align='center',style=" font-family: 'Lobster', cursive;
                                    font-weight: 500;
                                    line-height: 1.1;
                                    color: #3d85c6;"), 
                                 leafletOutput('bikes', width = 1600, height = 800)), 
                         #gardens map, create te gardens for users to find the gardens 
                         tabItem(tabName = 'gardens', 
                                 h1('Garden Map', align='center',style=" font-family: 'Lobster', cursive;
                                    font-weight: 500;
                                    line-height: 1.1;
                                    color: #48ca3b;" ), 
                                 leafletOutput('gardens', width = 1600, height = 800)), 
                         #hospitals: create the hospitals for user to find the nearest hospitals 
                         tabItem(tabName ='hospitals', 
                                 h1('Hospital Map',align='center',style=" font-family: 'Lobster', cursive;
                                    font-weight: 500;
                                    line-height: 1.1;
                                    color: #ea9999;" ),
                                 leafletOutput('hospitals', width = 1600, height = 800)), 
                         #wifi map
                         tabItem(tabName = 'wifi', 
                                 h1('WIFI Map', align='center',style=" font-family: 'Lobster', cursive;
                                    font-weight: 500;
                                    line-height: 1.1;
                                    color: #ffe599;" ),
                                 leafletOutput('wifi', width = 1600, height = 800)),
                         #recycling map 
                         tabItem(tabName = 'bins', 
                                 h1('Recycling Bin Map', align='center',style=" font-family: 'Lobster', cursive;
                                    font-weight: 500;
                                    line-height: 1.1;
                                    color: #38761d;" ),
                                 leafletOutput('bins', width = 1600, height = 800)), 
                         #libraries map 
                         tabItem(tabName = 'libraries', 
                                 h1('Library Map', align='center',style=" font-family: 'Lobster', cursive;
                                    font-weight: 500;
                                    line-height: 1.1;
                                    color: #134f5c;" ),
                                 leafletOutput('libraries', width = 1600, height = 800)
                                 ), 
                         #Calories section
                         #find the address
                         tabItem(tabName = 'address',
                                 h1('Find the Location',align='center',style=" font-family: 'Lobster', cursive;
                                    font-weight: 500;
                                    line-height: 1.1;
                                    color: #00ff00;" ),
                                 fluidRow(
                                   box(title = "Attention", status = "warning", solidHeader = T, height = 10,width = 3,
                                       h3('click on this map, you will find the longitude and latitude of your starting point and your destination,',align='center'),
                                       h3(' then you paste and copy the address of starting point and destination in the "Calories Burned" section.', align='center'), 
                                       
                                       verbatimTextOutput(outputId = 'coords')
                                       
                                   ), 
                                   box(title = 'Please Click on Map', status = 'success',solidHeader = T,
                                       plotOutput(outputId = 'mapOut',
                                                  width = 1000, height=700, 
                                                  click = "plot_click")
                                   )
                                 )
                                 
                                 
                                 ),
                         #fuctions for burn calories 
                         
                         tabItem(tabName = 'calories', 
                                 h1('Burning Calories', align='center',style=" font-family: 'Lobster', cursive;
                                    font-weight: 500;
                                    line-height: 1.1;
                                    color: #ff0000;" ),
                                 fluidRow(
                                   
                                   box(title = "The Address", status = 'success',  width = 5, height = 5, solidHeader = T,
                                       textInput(inputId = 'origin', label = 'Origin', 
                                                 value = 'columbia university'), 
                                       textInput(inputId = 'destination', label = 'Destination', 
                                                 value='new york university'),
                                       actionButton(inputId = 'getRoute',label = 'Click Me'),
                                       verbatimTextOutput(outputId = 'showdata' )
                                   ),
                                   
                                   box(title='The Best Bicycling Route',status = 'success',solidHeader = T,
                                       google_mapOutput('myMap', width = 800, height = 600) 
                                       
                                   )
                                   
                                 )
                                 
                                 
                                 
                                 )
                         )
                         )
                     
                         )

