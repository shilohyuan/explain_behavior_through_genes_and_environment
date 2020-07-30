library(lubridate)
library(dplyr)
options("scipen"=100, "digits"=10)
degreesToRadians <- function(degrees) {
  radians <- degrees * pi / 180
  return(radians)
}

distanceInKmBetweenEarthCoordinates <- function(lat1, lon1, lat2, lon2) {
  earthRadiusKm <- 6371
  distance <- rep(NA,length(lat1))
  for (q in 1:length(lat1)) {
    #print(q)
    dLat <- degreesToRadians(lat2[q]-lat1[q])
    #print(dLat)
    dLon <- degreesToRadians(lon2[q]-lon1[q])
    #print(dLon)
    Lat1 <- degreesToRadians(lat1[q])
    #print(lat1)
    Lat2 <- degreesToRadians(lat2[q])
    #print(lat2)
    
    a <- (sin(dLat/2) * sin(dLat/2)) + (sin(dLon/2) * sin(dLon/2) * cos(Lat1) * cos(Lat2))
    #print(a)
    c <- 2 * atan2(sqrt(a), sqrt(1-a))
    #print(c)
    distance[q] <- earthRadiusKm * c
  }
  return(distance)
}

data <- readRDS("/mnt/workspace2/cotwins/raw1.rds")
data2 <- as.data.frame(data)
data3 <- data.frame()
list_IDs <- names(sort(table(data2$user_id)))
data2$time_track <- NA
data2$step <- NA
for (i in 1:length(list_IDs)) {
                                        #seperate into individuals
  print(i)
  inputInfo <- data2[which(data2$user_id == list_IDs[i]), ]
  inputInfo <- inputInfo[order(inputInfo[,8]),]
  k <- inputInfo$local_time
  reference <- seq(as.Date(k[1]), as.Date(k[length(k)]), "days")
  reference <- reference[which(reference %in% as.Date(k))]
  # on each day
  clean <- data.frame()
  for (j in 1:length(reference)) {
    individual <- inputInfo[which(as.Date(k) == reference[j]), ]
    individual1 <- individual[1:(length(individual$latitude)-1),]
    individual2 <- individual[2:length(individual$latitude),]
    individual$time_track[1:length(individual$latitude)-1] <- as.double(individual2$local_time - individual1$local_time, units = "secs")
    individual$step[1:length(individual$latitude)-1] <- distanceInKmBetweenEarthCoordinates(individual1$latitude, individual1$longitude, individual2$latitude, individual2$longitude)
    clean <- rbind(clean, individual)
  }
  data3 <- rbind(data3, clean)
}
# here is the first time when zero latitudes disappeared
length(data3$latitude)
data3 <- data3[-which(data3$accuracy < 0),]
data3 <- data3[-which(data3$latitude ==0),]

saveRDS(data3, '/mnt/workspace2/cotwins/data3.rds')
