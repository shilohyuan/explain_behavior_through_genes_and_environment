library(dplyr)
splitHlaf <- readRDS("splitHalf_complete.rds")

df <- readRDS("/mnt/workspace2/cotwins/stayPoint_cleaned_travelRemoved_withAppType.rds")
perDay <- readRDS("/mnt/workspace2/cotwins/stayPoint_cleaned_travelRemoved_perDay.rds")
head(df)
df <- select(df, user_id, latitude, longitude, arvT, levT, app_type)
df$user_id <- as.character(df$user_id)
sorted.IDs <- names(sort(table(df$user_id)))

df$days <- as.Date(df$arvT)
df <- select(df,user_id, days, latitude, longitude, arvT, levT, app_type)
df$time_track <- as.double(df$levT - df$arvT, units = "secs")
m <- df[which(duplicated(df[,1:4]) | duplicated(df[,1:4],fromLast = TRUE)),]
m_IDs <- names(sort(table(m$user_id)))
for (i in 1:length(m_IDs)){
    inputInfo <- m[which(m$user_id == m_IDs[i]),]
    k <- inputInfo$arvT
    reference <- seq(as.Date(k[1]),as.Date(k[length(k)]),"days")
    reference <- reference[which(reference %in% as.Date(k))]
    for (j in 1:length(reference)) {
        sameDay <- inputInfo[which(as.Date(k) == reference[j]),]
        time_track <- sum(sameDay$time_track)
        sameDay$time_track <- NA
        sameDay$time_track[1] <- time_track
        inputInfo[which(as.Date(k) == reference[j]),]       <- sameDay
#        print(sameDay)
    }
    inputInfo -> m[which(m$user_id == m_IDs[i]),]
}
# df is updated with all the work on m
m ->  df[which(duplicated(df[,1:4]) | duplicated(df[,1:4],fromLast = TRUE)),]
df <- df[complete.cases(df$time_track),]
                                        #remove duplicated stay points
saveRDS(df,"200m_unique_stayPoint.rds")


ID <- c()
dateAll <- as.Date(c(NA))
entropyAll <- c()
for (i in 1:length(sorted.IDs)){
    print(i)
    inputInfo <- df[which(df$user_id == sorted.IDs[i]),]
    k <- inputInfo$arvT
    reference <- seq(as.Date(k[1]), as.Date(k[length(k)]), "days")
    reference <- reference[which(reference %in% as.Date(k))]
    dates <- as.Date(c(NA))
    entropy <- c()
    for (j in 1:length(reference)) {
        sameDay <- inputInfo[which(as.Date(k) == reference[j]),]
        timePercentage <- sameDay$time_track
        timeAll <- perDay$tripTime[which(as.character(perDay$ID) == sorted.IDs[i] & perDay$days == reference[j])]*3600
        fraction <- timePercentage/timeAll
        entropy <- c(entropy,-sum(fraction*log2(fraction)))
        dates <- c(dates, reference[j])
        dates <- dates[complete.cases(dates)]
    }
    ID <- c(ID,rep(sorted.IDs[i], length(entropy)))
    entropyAll <- c(entropyAll,entropy)
    dateAll <- c(dateAll,dates)
}

dateAll <- dateAll[complete.cases(dateAll)]
perDay2 <- data.frame(ID = ID, days = dateAll, entropy = entropyAll)
saveRDS(perDay2,"perDay_entropy.rds")    
