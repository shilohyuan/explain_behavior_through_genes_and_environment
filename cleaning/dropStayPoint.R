args = commandArgs(trailingOnly=TRUE) 
#Rscript --vanilla sillyScript.R iris.txt out.txt
#will create a string vector args which contains the entries iris.txt and out.txt. 
start <- args[1]
end <- args[2]
options(warn=-1) # turn global warning off

library(dplyr)
library(lubridate)
degreesToRadians <- function(degrees) {
  radians <- degrees * pi / 180
  return(radians)
}

distanceInKmBetweenEarthCoordinatesArray <- function(lat1, lon1, lat2, lon2) {
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
    
    a <- (sin(dLat/2) * sin(dLat/2)) + (sin(dLon/2) * sin(dLon/2) * cos(Lat1) * 
                                          cos(Lat2))
    #print(a)
    c <- 2 * atan2(sqrt(a), sqrt(1-a))
    #print(c)
    distance[q] <- earthRadiusKm * c
  }
  return(distance)
}

distanceInKmBetweenEarthCoordinates <- function(lat1, lon1, lat2, lon2) {
  earthRadiusKm <- 6371
  dLat <- degreesToRadians(lat2-lat1)
  dLon <- degreesToRadians(lon2-lon1)
  lat1 <- degreesToRadians(lat1)
  lat2 <- degreesToRadians(lat2)
  a <- (sin(dLat/2) * sin(dLat/2)) + (sin(dLon/2) * sin(dLon/2) * cos(lat1) * cos(lat2))
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  distance <- earthRadiusKm * c
  return(distance)
}


#output1 <- sprintf("500m_%s_to_%s.txt",start,end)
#write.table(t,file = output1,quote=FALSE,sep = "\t",row.names=F)
#for python plotting, and explaining to Scott
output1 <- sprintf("500m_%s_to_%s.rds",start,end)
#saveRDS(t,output1)


#output2 <- sprintf("200m_%s_to_%s.txt",start,end)
#write.table(t,file = output2,quote=FALSE,sep = "\t",row.names=F)
output2 <- sprintf("200m_%s_to_%s.rds",start,end)
#saveRDS(t,output2)

df <- readRDS(output2)
df <- readRDS('200m_1_to_575.rds')


length(df$latitude) 
df <- df[which(df$sdLat < .4),] 
df <- df[which(df$sdLon < .4),]
length(df$latitude)
df <- df[which(df$speed < 10),] 
length(df$latitude)                                       

#### for 200m, less stay points.
#### 6329 removed. 
#### since 200m and 500m doesn't behave very obviously different on map (driving and flying)
#### only incudes 200m for plotting purposes (raw, 200m-raw, 200m-clean (layers))
#### cleaning should be written on travel.rds (t - 200m)
# 1 - outside of denver
df$Denver <- 0


for (i in 1:length(df$latitude)){ 
  if (distanceInKmBetweenEarthCoordinates(39.7392,-104.9903,df$latitude[i],df$longitude[i]) > 1000){
    df$Denver[i] <- 1
  }
}


df$step <- NA
list_IDs <- names(sort(table(df$user_id)))
for (i in 1:length(list_IDs)) {
  #seperate into individuals
  individual <- df[which(df$user_id == list_IDs[i]), ]
  individual1 <- individual[1:(length(individual$latitude)-1),]
  individual2 <- individual[2:length(individual$latitude),]
  individual$step[1:length(individual$latitude)-1] <- distanceInKmBetweenEarthCoordinatesArray(individual1$latitude, individual1$longitude, individual2$latitude, individual2$longitude)
  individual -> df[which(df$user_id == list_IDs[i]), ]
}




for (l in 1:length(list_IDs)){    
    individual <- df[which(df$user_id == list_IDs[l]),]
    list <- which(individual$Denver == 1)
    if ((length(list) > 0) & (length(individual$latitude) > 2)){
        for (j in 1:length(list)){
            travel <- list[j]
            travel1 <- list[j] + 1
            travel2 <- list[j] + 2
            if (complete.cases(individual$latitude[travel]) & complete.cases(individual$step[travel1]) & complete.cases(individual$step[travel2])){
                if ((individual$step[travel1] > 100) | (individual$step[travel2] > 100)){
                    individual$latitude[travel:travel2] <- NA
                }
            }
        }
    }
    df[which(df$user_id == list_IDs[l]),] <- individual
}

df <- df[which(complete.cases(df$latitude)),]
length(df$latitude)
saveRDS(df,'stayPoint_cleaned.rds')

