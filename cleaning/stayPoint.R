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

stayPoint <- function(traj, thetaD, thetaT) {
  user_id <- c()
  latitude <- c()
  longitude <- c()
  arvT <- c(traj$local_time[1])
  levT <- c(traj$local_time[1])
  speed <- c()
  sdLat <- c()
  sdLon <- c()
  numPoint <- c()
  i <- 1
  j <- i + 1
  pointNum <- length(traj$user_id) # number of GPS points in traj
  while ((i < pointNum) & (j < pointNum)) {
    flag <- TRUE
    while ((j < pointNum) & flag) {
      distance <- distanceInKmBetweenEarthCoordinates(traj$latitude[i], traj$longitude[i],traj$latitude[j],traj$longitude[j])
      #print(distance)
      if (distance >= thetaD){
        time <- as.double(traj$local_time[j] - traj$local_time[i], units = "mins")
        #print(time)
        if ((time >= thetaT) & (time < 720)) {
          # calculating average latitude and longitude
          subStay <- traj[i:j,]
          #print(sd(subStay$latitude))
          #if ((sd(subStay$latitude) < .4) & (sd(subStay$longitude) < .4) & (mean(subStay$speed, na.rm=T) < 10)){
          #print(subStay)
          #print(subStay$user_id[1])
          user_id <- c(user_id, subStay$user_id[1])
          latitude <- c(latitude,mean(subStay$latitude))
          longitude <- c(longitude,mean(subStay$longitude))
          arvT <- c(arvT,subStay$local_time[1])
          levT <- c(levT, subStay$local_time[length(subStay$user_id)])
          speed <- c(speed, mean(subStay$speed, na.rm =T))
          sdLat <- c(sdLat,sd(subStay$latitude))
          sdLon <- c(sdLon,sd(subStay$longitude))
          numPoint <- c(numPoint, length(subStay$latitude))
          #}
        }
        i <- j 
        flag <- FALSE
        #print("breaking out of j-while loop")
      }
      else if (distance < thetaD)  {
        j <- j + 1
        #print(j)
      }
      }
  }
  s <- data.frame(user_id = user_id, latitude = latitude, longitude = longitude, arvT = arvT[-1], levT = levT[-1], speed = speed, sdLat = sdLat, sdLon = sdLon, numPoint = numPoint)
  
  return(s) # dataframe of stay points S = {s}
  #return(q)
}

q <- readRDS('data3_speedremoved.rds')
allID <- names(sort(table(q$user_id)))

chunk <- allID[start:end]
#print(chunk)
df <- q[which(q$user_id %in% chunk),]
# chunk it here once

#output <-sprintf("raw_%s_to%s.txt",start,end)
#write.table(df,file = output, quote = FALSE, sep="\t", row.names=F)
                                        # for python plotting
list_ID <- names(table(df$user_id))

thetaD <- .5
thetaT <- 30
t <- data.frame()
for (q in 1:length(list_ID)) {
  individual <- df[which(df$user_id == list_ID[q]),]
  set <- stayPoint(individual,thetaD,thetaT)
  t <- rbind(t,set)
}
# strintf function here making unique output names
# remember also update the 200m
output1 <- sprintf("500m_%s_to_%s.txt",start,end)
write.table(t,file = output1,quote=FALSE,sep = "\t",row.names=F)
output1 <- sprintf("500m_%s_to_%s.rds",start,end)
saveRDS(t,output1)

thetaD <- .2
thetaT <- 30
t <- data.frame()

for (q in 1:length(list_ID)) {
  individual <- df[which(df$user_id == list_ID[q]),]
  set <- stayPoint(individual,thetaD,thetaT)
  t <- rbind(t,set)
}
output2 <- sprintf("200m_%s_to_%s.txt",start,end)
write.table(t,file = output2,quote=FALSE,sep = "\t",row.names=F)
output2 <- sprintf("200m_%s_to_%s.rds",start,end)
saveRDS(t,output2)
