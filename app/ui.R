
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
library(plotly)
source('../lib/register_google.R')

#this is my own api key 
register_google(key = 'AIzaSyCq8VzY3P1qQvvCA5ERHk-ueMS8Q8LYo8A')

#you have to get your own api key to run the code 
#this is the link to get the api key https://developers.google.com/maps/documentation/javascript/get-api-key

register_google(key = "AIzaSyCq8VzY3P1qQvvCA5ERHk-ueMS8Q8LYo8A", account_type = "premium", day_limit = 100000)
#functions 

bikes <- read.csv('../data/citibikeStations2018.csv')

gardens <- read.csv('../data/Gardens.csv')
gardens <- gardens[!(is.na(gardens$Latitude)),]
bins <- read.csv('../data/bins.csv')
libraries <- read.csv('../data/Library.csv')
temp <- read.csv('../data/temp.csv', header = T)
temp$Date <- as.Date(temp$Date, "%m/%d/%Y")


#ui section 
ui <- dashboardPage( skin = 'green', # green lives 
                     #the header section
                     dashboardHeader(title = "How Citi Bikes Bring You a Green Life", titleWidth = 400,
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
                         
                         #the chart section; 
                         menuItem('The Situation',tabName = 'chart', icon = icon('envira', lib='font-awesome')),
                         
                         #second part: the functions of app
                         menuItem("Green Lives", icon = icon('globe',lib='font-awesome'), 
                                  # the bikes map 
                                  menuSubItem('Find Bikes', tabName = 'bikes', icon = icon('bicycle',lib='font-awesome')),
                                  #the gardens map
                                  menuSubItem('Find Garden', tabName = 'gardens', icon = icon('tree', lib = 'font-awesome')),
                                  
                                  #the recycling bins map 
                                  menuSubItem('Find Recycling Bins', tabName = 'bins', icon =icon('trash',lib='font-awesome')), 
                                  menuSubItem('Find Library',tabName = 'libraries', icon = icon('book',lib='font-awesome'))
                                  
                                  
                                  
                         ), 
                         #third section: calculate the calories 
                         menuItem("Burning Calories", icon = icon('angellist', lib = 'font-awesome'), 
                                  menuSubItem('Find Address', tabName='address',icon=icon('map-marker',lib='font-awesome')),
                                  menuSubItem("Calories Burned", tabName = 'calories', icon = icon('location-arrow', lib='font-awesome'))
                         ),
                         menuItem("Contact Us",tabName = 'contact', icon=icon("phone", lib = "font-awesome"))
                         
                       )
                       
                     ),
                     
                     
                     
                     
                     dashboardBody(
                       
                       tabItems(
                         #message part: write down the messages for users to take action
                         tabItem(tabName = 'message',
                                 h1('Message for Users', align='center', style = "font-family: 'Lobster', cursive;
                                    font-weight: 500; line-height: 1.1; 
                                    color: #4d3a7d;"), 
                                 h3("     As we all know, the environment is worsening at a fast pace. Greenhouse gases generated 
                                    by cars give rise to global warming. Glaciers are melting which causes the ocean levels to rise.
                                    Droughts are plaguing areas that once had plenty of water and crops are yielding less food. 
                                    Air pollution is making the living conditions on our planet even worse. Various environmental 
                                    issues pose a huge threat to people's health conditions. It's high time that we should take 
                                    actions to promote green lifestyles and that's why we release our web-based R Shiny tools."),
                                 h3("     We use the R shiny interactive maps to guide people to lead a green life efficiently with Citi bike. Our users can utilize 
                                     our product to explore the nearby bike stations to ride citi bikes. Our interface can also show 
                                     the gardens near you so that you can breathe fresh air. Nearby recycling bins will also be displayed 
                                     to encourage people to sort and reduce the waste. What's more, users also have access to the libraries 
                                     near them so they can read books there. All these things are environmentally friendly."),
                                 h3("     What's more, we add a fancy functionality 'Burning Calories' to our new product. You will be automatically 
                                    located after you click the map. The longitude and latitude of your starting point and destination will be shown.
                                    We will suggest the optimal route and calculate the calories burned along the way accordingly. "),
                                 h3("     Living green is important, and we want to empower you to do your part.Enjoy our tool and learn how to live an 
                                    environmentally friendly life. It's more important now than ever to act to solve our environmental problems. 
                                    Because these changes in our planet are serious, and without action, they're only going to get worse. "),
                                
                                  img(src="green.png", height=140, width=400, align='center')
                                 
                                 ), 
                         #the chart section; 
                         tabItem(tabName = 'chart', 
                                 h1('The Situation', align='center', style=" font-family: 'Lobster', cursive;
                                    font-weight: 500;
                                    line-height: 1.1;
                                    color: #ff0000;" ), 
                                 h3("The Global Changes:"), 
                                 tabsetPanel(
                                   tabPanel("The Temperature Changes", plotlyOutput(outputId = 'temp', height = '600px')), 
                                   tabPanel("The Sea Level Chnages", plotlyOutput(outputId = 'sealevel', height = '600px')), 
                                   tabPanel("The Trends in Atomspheric Carbon Dioxide", plotlyOutput(outputId = 'co2level', height = '600px'))
                                   
                                 )
                                 
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
                                 
                                 
                                 
                                 ),
                         tabItem(tabName = 'contact', 
                                 h1('Contact Us', align='center',style=" font-family: 'Lobster', cursive;
                                    font-weight: 500;
                                    line-height: 1.1;
                                    color: #00ff00;" ), 
                                 fluidRow(
                                   h2("We are Group 1"),
                                   h4("Xin Jin xj2215@columbia.edu"),
                                   br(),
                                   h4('Raymond Zhang rz2394@columbia.edu'), 
                                   br(), 
                                   h4("Yanzi Shen ys3088@columbia.edu")
                                 )
                                 
                                 )
                         )
                         )
                     
                         )

