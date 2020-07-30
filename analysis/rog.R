library(dplyr)

degreesToRadians <- function(degrees) {
  radians <- degrees * pi / 180
  return(radians)
}

distanceInKmBetweenEarthCoordinates <- function(lat1, lon1, lat2, lon2) {
  earthRadiusKm <- 6371
  
  dLat <- degreesToRadians(lat2-lat1)
  #print(dLat)
  dLon <- degreesToRadians(lon2-lon1)
  #print(dLon)
  lat1 <- degreesToRadians(lat1)
  #print(lat1)
  lat2 <- degreesToRadians(lat2)
  #print(lat2)
  
  a <- (sin(dLat/2) * sin(dLat/2)) + (sin(dLon/2) * sin(dLon/2) * cos(lat1) * cos(lat2))
  #print(a)
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  #print(c)
  distance <- earthRadiusKm * c
  
  return(distance)
}

df <- readRDS("/mnt/workspace2/cotwins/stayPoint_cleaned_travelRemoved_withAppType.rds")
perDay <- readRDS("/mnt/workspace2/cotwins/stayPoint_cleaned_travelRemoved_perDay.rds")
df <- select(df, user_id, latitude, longitude, arvT, levT, app_type)
df$user_id <- as.character(df$user_id)
sorted.IDs <- names(sort(table(df$user_id)))

head(df)
df$avLat <- NA
df$avLon <- NA
for (i in 1:length(sorted.IDs)){
    print(i)
    inputInfo <- df[which(df$user_id == sorted.IDs[i]),]
    k <- inputInfo$arvT
    reference <- seq(as.Date(k[1]), as.Date(k[length(k)]), "days")
    reference <- reference[which(reference %in% as.Date(k))]
    for (j in 1:length(reference)){
        sameDay <- inputInfo[which(as.Date(k) == reference[j]),]
        sameDay$avLat <- mean(sameDay$latitude)
        sameDay$avLon <- mean(sameDay$longitude)
        sameDay -> inputInfo[which(as.Date(k) == reference[j]),]
    }
    inputInfo -> df[which(df$user_id == sorted.IDs[i]),]
}


ID <- c()
dateAll <- as.Date(c(NA))
rogAll <- c()
for (i in 1:length(sorted.IDs)){
    print(i)
    inputInfo <- df[which(df$user_id == sorted.IDs[i]),]
    k <- inputInfo$arvT
    reference <- seq(as.Date(k[1]), as.Date(k[length(k)]), "days")
    reference <- reference[which(reference %in% as.Date(k))]
    dates <- as.Date(c(NA))
    rog <- c()
    for (j in 1:length(reference)) {
        sameDay <- inputInfo[which(as.Date(k) == reference[j]),]
        sameDay$new <- distanceInKmBetweenEarthCoordinates(sameDay$latitude, sameDay$longitude, sameDay$avLat, sameDay$avLon)
        rog <- c(rog,sqrt(sum(sameDay$new^2)))
        dates <- c(dates, reference[j])
        dates <- dates[complete.cases(dates)]
    }
    ID <- c(ID,rep(sorted.IDs[i], length(rog)))
    rogAll <- c(rogAll,rog)
    dateAll <- c(dateAll,dates)
}

dateAll <- dateAll[complete.cases(dateAll)]
perDay2 <- data.frame(ID = ID, days = dateAll, rog = rogAll)
saveRDS(perDay2,"perDay_rog.rds")    
        
