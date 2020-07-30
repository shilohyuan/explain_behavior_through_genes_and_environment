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

perPersonF <- function(data4, sorted.IDs) {
    #per Person
    nbDays <- rep(NA,length(sorted.IDs))
    appType <- rep(NA, length(sorted.IDs))
    nbTrip <- rep(NA,length(sorted.IDs))
    tripTime <- rep(NA,length(sorted.IDs))
    mileage <- rep(NA,length(sorted.IDs))
    totalDays <- rep(NA,length(sorted.IDs))
    
    for (i in 1:length(sorted.IDs)){ # drop 5 individual with 1 points
      print(i)
      #seperate into individuals
      inputInfo <- data4[which(data4$user_id == sorted.IDs[i]),]
      k <- inputInfo$sample_time
      reference <- seq(as.Date(k[1]), as.Date(k[length(k)]), "days")
      totalDays[i] <- length(reference) 
      reference <- reference[which(reference %in% as.Date(k))]
      # number of days 
      nbDays[i] <- length(reference)
      #"6577309868354245094" cannot find matching SVID in the id file
      # removed
      
      appType[i] <- inputInfo$app_type[1]
      tripTime[i] <- sum(inputInfo$time_track, na.rm = TRUE)
      nbTrip[i] <- length(inputInfo$latitude)
      mileage[i] <- sum(inputInfo$step, na.rm = TRUE)
    }
    
    perPerson <- data.frame(ID = sorted.IDs, tripTime = tripTime, nbDays = nbDays,totalDays = totalDays,nbTrip = nbTrip, appType = appType, mileage = mileage)
    saveRDS(perPerson, 'perPerson_raw.rds')
    perPerson$totalDays <- perPerson$nbDays/totalDays    
    perPerson$tripTime <- perPerson$tripTime/nbDays
    perPerson$tripTime <- perPerson$tripTime/60
    perPerson$tripTime <- perPerson$tripTime/60
    perPerson$mileage <- perPerson$mileage/nbDays
    perPerson$mileage <- perPerson$mileage/1000
    perPerson$nbTrip <- perPerson$nbTrip/nbDays

    return(perPerson)
}

perDayF <- function(perPerson, data4, sorted.IDs){
    # of days
    # daily Mileage
    # time track in a day
    # # of pts in a day
    
    IDs <- rep(NA, sum(perPerson$nbDays, na.rm = T))
    days <- as.Date(rep(NA, sum(perPerson$nbDays, na.rm = T)))
    appType <- rep(NA, sum(perPerson$nbDays, na.rm = T))
    nbTrip <- rep(NA, sum(perPerson$nbDays, na.rm = T))
    dM <- rep(NA, sum(perPerson$nbDays, na.rm = T))
    tripTime <- rep(NA,sum(perPerson$nbDays, na.rm = T))
    
    index <- 0
    for (i in 6:length(sorted.IDs)){
      print(i)
      #seperate into individuals
      inputInfo <- data4[which(data4$user_id == sorted.IDs[i]),]
      k <- inputInfo$sample_time
      reference <- seq(as.Date(k[1]), as.Date(k[length(k)]), "days")
      reference <- reference[which(reference %in% as.Date(k))]
      # on each day
      for (j in 1:length(reference)) { 
        index <- index + 1
        sameDay <- inputInfo[which(as.Date(k) == reference[j]),]
        #print(sameDay)
        IDs[index] <- sameDay$user_id[1]
        appType[index] <- sameDay$app_type[1]
        tripTime[index] <- sum(sameDay$time_track[1:length(sameDay$time_track)-1], na.rm = T)
        days[index] <- reference[j]
        nbTrip[index] <- length(sameDay$time_track)
        dM[index] <- sum(sameDay$step[1:length(sameDay$time_track)-1], na.rm = T)
        # this is helpful in calculating big gaps between days (how many big gaps there are)
        # tripTIme > 24 hours = 1440 mins
      }
    }
    perDay <- data.frame(ID = IDs, days = days, tripTime = tripTime, nbTrip = nbTrip, dailyMileage = dM, appType = appType)
    perDay$tripTime <- perDay$tripTime/3600
    perDay$dailyMileage <- perDay$dailyMileage/1000
    
    return(perDay)
}

my.summary <- function(x, na.rm=TRUE){
  result <- c(Min=min(x, na.rm=na.rm),
              FirstQuartile=quantile(x, 0.25, na.rm=na.rm),
              Median=median(x, na.rm=na.rm),
              Mean=mean(x, na.rm=na.rm),
              SD=sd(x, na.rm=na.rm),
              ThirdQuartile=quantile(x, 0.75, na.rm=na.rm),
              Max=max(x, na.rm=na.rm))
}

summaryF <- function(data4, perPerson, perDay){
    data4<- as.data.frame(data4)
    ind <- sapply(data4, is.numeric)
    q <- sapply(data4[, ind], my.summary)
    q <- q[,3:6]
    q <- as.data.frame(t(q))
    rownames(q) <- c("accuracy (m)","track interval (secs)","step (m)","speed (m/s)")
    colnames(q)[2] <- "1st Quartile"
    colnames(q)[6] <- "3rd Quartile"

    ind1 <- sapply(perPerson, is.numeric)
    k <- sapply(perPerson[, ind1], my.summary)
    k <- k[,2:5]
    k <- as.data.frame(t(k))
    colnames(k)[2] <- "1st Quartile"
    colnames(k)[6] <- "3rd Quartile"
    rownames(k) <- c("number of days (per person)","days available/total days","number of trips (per person)","daily Mileage (km) (per Person)")
 
    ind2 <- sapply(perDay, is.numeric)
    m <- sapply(perDay[, ind2], my.summary)
    m <- as.data.frame(t(m))
    colnames(m)[2] <- "1st Quartile"
    colnames(m)[6] <- "3rd Quartile"
    rownames(m) <- c("trip time (hours) (per Day)","number of trips (per Day)","daily Mileage (km) (per Day)")
    options("scipen"=100, "digits"=2)
    rawSummary <- rbind(q,k,m)
    return(rawSummary)
}

tableSummary <- function(rawSummary, rawSummaryAndroid, rawSummaryiOS) {
    collapse_rows_dt <- data.frame(C1 = c(rep("accuracy (m)", 3), rep("track interval (secs)", 3), rep('step (m)',3), rep("speed (m/s)",3), rep("number of days (per person)",3),rep('available/total (per person)',3),rep("number of points (per person)",3),rep("daily mileage (km) (per person)",3),rep("time tracked in UTC (hours) (per day)",3),rep("number of points (per day)",3),rep("daily mileage (km) (per day)",3)),
                                   C2 = sample(c(0,1), 33, replace = TRUE),
                                   C3 = sample(c(0,1), 33, replace = TRUE),
                                   C4 = sample(c(0,1), 33, replace = TRUE),
                                   C5 = sample(c(0,1), 33, replace = TRUE),
                                   C6 = sample(c(0,1), 33, replace = TRUE),
                                   C7 = sample(c(0,1), 33, replace = TRUE),
                                   C8 = sample(c(0,1), 33, replace = TRUE),
                                   C9 = sample(c(0,1), 33, replace = TRUE))
    index = 0
    collapse_rows_dt
    for (i in 1:11) {
      collapse_rows_dt[1 + index*3,3:9] <- rawSummary[i,]
      collapse_rows_dt[1 + index*3,2] <- "All"
      collapse_rows_dt[2 + index*3,3:9] <- rawSummaryAndroid[i,]
      collapse_rows_dt[2 + index*3,2] <- "Android"
      collapse_rows_dt[3 + index*3,3:9] <- rawSummaryiOS[i,]
      collapse_rows_dt[3 + index*3,2] <- "iOS" 
      index <- index + 1
    }
    
    colnames(collapse_rows_dt) <- c("Variables","Devices","Min","firstQuartile","Median","Mean","SD","thirdQuartile","Max")
    return(collapse_rows_dt)
}

args = commandArgs(trailingOnly =TRUE)
item <- args[1]
filename <- sprintf('/mnt/workspace2/cotwins/%s.rds',item)

df <- readRDS(filename)

list_IDs <- names(sort(table(df$user_id)))
perPersonAll <- perPersonF(df,list_IDs)
perDayAll <- perDayF(perPersonAll,df,list_IDs)

dfAndroid <- df[which(df$app_type == "android"),]
dfiOS <- df[which(df$app_type == "ios"),]
perPersonAndroid <- perPersonAll[which(perPersonAll$appType == "android"),]
perPersoniOS <- perPersonAll[which(perPersonAll$appType == "ios"),]
perDayAndroid <- perDayAll[which(perDayAll$appType == "android"),]
perDayiOS <- perDayAll[which(perDayAll$appType == "ios"),]
summaryAll <- summaryF(df, perPersonAll, perDayAll)
summaryAndroid <- summaryF(dfAndroid,perPersonAndroid,perDayAndroid)
summaryiOS <- summaryF(dfiOS,perPersoniOS,perDayiOS)
summary <- tableSummary(summaryAll, summaryAndroid, summaryiOS)

resultname <- sprintf('/mnt/workspace2/cotwins/%s_summary.rds',item)
saveRDS(summary, resultname)


