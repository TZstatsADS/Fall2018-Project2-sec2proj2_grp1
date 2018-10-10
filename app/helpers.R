

register_google(key = 'AIzaSyAMMFd-5P1JoxxN0wn4APqya4L1VSAEBvw')


#functions 

bikes <- read.csv('data/citibikeStations2018.csv')
gardens <- read.csv('data/Gardens.csv')
gardens <- gardens[!(is.na(gardens$Latitude)),]
bins <- read.csv('data/bins.csv')
libraries <- read.csv('data/Library.csv')


temp <- read.csv('data/temp.csv', header = T)
temp$Date <- as.Date(temp$Date, "%Y-%m-%d")
sea <- read.csv('data/sealevel1.csv', header = T)
sea$Date <- as.Date(sea$Date, '%Y-%m-%d')
co2 <- read.csv('data/co2.csv', header = T)
co2$Date <- as.Date(co2$Date, "%Y-%m-%d")
so2 <- read.csv('data/so2.csv', header = T)
pm <- read.csv('data/pm.csv', header = T)

data('World')































