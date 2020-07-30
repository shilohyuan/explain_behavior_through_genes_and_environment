library(lubridate)
library(dplyr)

#need to change df <- readRDS('stayPoint_cleaned_perDay.rds')

df$wdays <- wday(df$days)
df$ID <- as.character(df$ID)


entropy <- readRDS("perDay_entropy.rds")
entropy$ID <- as.character(entropy$ID)
df <- df %>% right_join(entropy, by = c("ID","days"))
df$ID <- as.character(df$ID)

weekends <- df[which(df$wdays > 5),]
weekdays <- df[which(df$wdays < 6),]

splitHalfReliability <- function(weekdays){

    appType <- c()
    user_id <- c()
    user_id <- as.character(user_id)
    oddPlaces <- c()
    evenPlaces <- c()
    oddDistance <- c()
    evenDistance <- c()
    oddEntropy <- c()
    evenEntropy <- c()
    list_IDs <- names(sort(table(weekdays$ID)))

    for (i in 1:length(list_IDs)) {
        individual <- weekdays[which(weekdays$ID == list_IDs[i]),]
                                        #there are people with no weekdays data
        if (length(individual$ID) > 1){ #exclude individual with less than 2 days of data points
            individual1 <- individual[seq(1,length(individual$ID),2),]
            individual2 <- individual[seq(2,length(individual$ID),2),]
            user_id <- c(user_id, individual$ID[1])
            oddPlaces <- c(oddPlaces, mean(individual1$nbTrip, na.rm = T)) # number of places
            evenPlaces <- c(evenPlaces, mean(individual2$nbTrip, na.rm =T))
                                        # distance
            oddDistance <- c(oddDistance, mean(individual1$dailyMileage, na.rm=T))
            evenDistance <- c(evenDistance, mean(individual2$dailyMileage, na.rm=T))
            oddEntropy <- c(oddEntropy, mean(individual1$entropy,na.rm=T))
            evenEntropy <- c(evenEntropy, mean(individual2$entropy,na.rm=T))
            appType <- c(appType, individual$appType[1])
        }
    }
    splitHalf <- data.frame(ID = user_id, oddPlaces = oddPlaces, evenPlaces = evenPlaces, oddDistance=oddDistance, evenDistance=evenDistance, oddEntropy = oddEntropy, evenEntropy = evenEntropy, appType = appType)
    return(splitHalf)
}

wdays_splitHalf <- splitHalfReliability(weekdays)
wdays_splitHalf$wdays <- 1
wends_splitHalf <- splitHalfReliability(weekends)
wends_splitHalf$wdays <- 0

splitHalf <- rbind(wdays_splitHalf,wends_splitHalf)

#saveRDS(splitHalf, 'splitHalf_500removed.rds')


### twin correlation
#splitHalf <- readRDS('splitHalf_500removed.rds')
splitHalf$ID <- as.character(splitHalf$ID)
sorted.IDs <- names(sort(table(splitHalf$ID)))
splitHalf$ID <- as.numeric(splitHalf$ID)
dob <- readRDS('Robin_paper-entry_2-22-17_cleaned.rds')
referencePair <- read.csv(file ="id_mapping.csv", header = TRUE, sep = ",") # N(twin) = 335
                                        # selecting twinPairs where both twins are in the dataset
referencePair$T1 <- as.character(referencePair$T1)
referencePair$T2 <- as.character(referencePair$T2)

referencePair <- referencePair[which((referencePair$T1_alternate_id %in% as.numeric(sorted.IDs)) & (referencePair$T2_alternate_id %in% as.numeric(sorted.IDs))),] # N(twin) = 247
MZ <- referencePair[which(referencePair$bestzygos == "MZ"),] #N(MZ) = 110 #after N(MZ) = 84
DZ <- referencePair[which(referencePair$bestzygos == "DZ"),] #N(DZ) = 136 #after N(DZ) = 103
OS <- referencePair[which(referencePair$bestzygos == "OS"),] #N(OS) = 89  #after N(OS) = 60

SVID <-c(referencePair$T1, referencePair$T2)
ID <- c(referencePair$T1_alternate_id, referencePair$T2_alternate_id)
splitHalf$SVID <- NA

for (i in 1:length(splitHalf$ID)){
    if (splitHalf$ID[i] %in% ID){
    splitHalf$SVID[i] <- SVID[which(ID == splitHalf$ID[i])]
    }
    }
                                        # only assigned SVID to complete twin pairs
splitHalf_complete <- splitHalf[which(complete.cases(splitHalf$SVID)),]
head(splitHalf_complete)


dob <- select(dob, family, ID1, Birth_Year, Birth_Month, Birth_Day, ID2, Birth_Year2, Birth_Month2, Birth_Day2)
dob1 <- as.data.frame(select(dob, ID1, Birth_Year, Birth_Month, Birth_Day))
dob2 <- as.data.frame(select(dob, ID2, Birth_Year, Birth_Month, Birth_Day))
colnames(dob2) <- c("ID1", "Birth_Year","Birth_Month","Birth_Day")
dobAll <- rbind(dob1, dob2)
dobAll$DOB <- ymd(paste(dobAll$Birth_Year, dobAll$Birth_Month,dobAll$Birth_Day))
class(dobAll$ID1)

splitHalf_complete$dob <- as.Date(NA)
for (i in 1:length(splitHalf_complete$SVID)){
    splitHalf_complete$dob[i] <- dobAll$DOB[which(dobAll$ID1 == splitHalf_complete$SVID[i])]
}

splitHalf_complete$dob <- as.Date(splitHalf_complete$dob)
head(splitHalf_complete)

                                        # input is daily movement patterns with multiple days information
                                        # df (whole sample)
                                        # df[which(df$wdays < 6),] is the weekdays
                                        # df[which(df$wdays > 5),] is the weekends

getTwin <- function(df,y) {
    list_IDs <- unique(splitHalf_complete$ID)
    q <- length(list_IDs)
    twin <- data.frame(SVID = rep(NA,q) , allPlaces = rep(NA,q), before18Places= rep(NA,q), after18Places= rep(NA,q), allDistance=rep(NA,q), before18Distance=rep(NA,q), after18Distance=rep(NA,q))
    for (i in 1:length(list_IDs)){
        individual <- df[which(as.numeric(df$ID) == list_IDs[i]),]
        dob <- splitHalf_complete$dob[which(list_IDs[i] == splitHalf_complete$ID)]
        individualBefore18 <- individual[which(as.double(individual$days - dob, unit = "days") <= 6570),]
        individualAfter18 <- individual[which(as.double(individual$days - dob,unit = 'days') > 6570),]
        twin$SVID[i] <- splitHalf_complete$SVID[which(list_IDs[i] == splitHalf_complete$ID)]
        twin$allPlaces[i] <- mean(individual$nbTrip, na.rm=T)
        twin$before18Places[i] <- mean(individualBefore18$nbTrip, na.rm =T)
        twin$after18Places[i] <- mean(individualAfter18$nbTrip, na.rm = T)
        twin$allDistance[i] <- mean(individual$dailyMileage, na.rm=T)
        twin$before18Distance[i] <- mean(individualBefore18$dailyMileage, na.rm =T)
        twin$after18Distance[i] <- mean(individualAfter18$dailyMileage, na.rm=T)
    }
    
    twin$T1 <- twin$SVID
    twin$T2 <- twin$SVID
    referencePair <- select(referencePair, T1,T2, bestzygos)
    twin1 <- merge(referencePair, twin, by ="T1")
    twin1 <- select(twin1, T1, T2.x, bestzygos, allPlaces, before18Places, after18Places, allDistance, before18Distance, after18Distance) 
    twin2 <- merge(referencePair, twin, by ="T2")
    twin2 <- select(twin2, T1.x, T2, bestzygos, allPlaces, before18Places, after18Places, allDistance, before18Distance, after18Distance)
    appendix  <- switch(y, "all","wdays","wends")
    fName1 <- sprintf("twin1_%s.rds",appendix)
    fName2 <- sprintf("twin2_%s.rds",appendix)
    saveRDS(twin1,fName1)
    saveRDS(twin2,fName2)
}

getTwin(df,1)
getTwin(df[which(df$wdays < 6),],2)
getTwin(df[which(df$wdays > 5),],3)

    

    
