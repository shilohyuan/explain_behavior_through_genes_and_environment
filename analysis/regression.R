library(lubridate)
library(dplyr)

df <- readRDS('/mnt/workspace2/cotwins/stayPoint_cleaned_travelRemoved_perDay.rds')
df$wdays <- wday(df$days)

df$wdays[which(df$wdays < 6)] <- 0 # weekdays
df$wdays[which(df$wdays > 5)] <- 1 # weekends
#pdf('distance.pdf')
#hist(df$dailyMileage[which(df$dailyMileage < 1.00001)],breaks=100)
#dev.off()

sex <- readRDS('/mnt/workspace2/cotwins/Robin_paper-entry_2-22-17_cleaned.rds')
sex <- select(sex,ID1, Sex1)

referencePair <- read.csv(file ="/mnt/workspace2/cotwins/id_mapping.csv", header = TRUE, sep = ",")
referencePair <- as.data.frame(referencePair)
T1 <- select(referencePair,T1, T1_alternate_id, bestzygos)
T2 <- select(referencePair,T2, T2_alternate_id, bestzygos)
colnames(T2) <- c("T1","T1_alternate_id","bestzygos")
pair <- rbind(T1,T2)
T1$T1 <- as.character(T1$T1)
sex <- sex[match(T1$T1, sex$ID1),]
demo <- cbind(pair,sex)
demo <- select(demo, T1, T1_alternate_id, bestzygos, Sex1)
demo$T1 <- as.character(demo$T1)
demo$bestzygos <- as.character(demo$bestzygos)
sorted.IDs <- names(sort(table(df$ID)))

dob <- readRDS('/mnt/workspace2/cotwins/Robin_paper-entry_2-22-17_cleaned.rds')
dob <- select(dob, family, ID1, Birth_Year, Birth_Month, Birth_Day, ID2, Birth_Year2, Birth_Month2, Birth_Day2)
dob1 <- as.data.frame(select(dob, ID1, Birth_Year, Birth_Month, Birth_Day))
dob2 <- as.data.frame(select(dob, ID2, Birth_Year, Birth_Month, Birth_Day))
colnames(dob2) <- c("ID1", "Birth_Year","Birth_Month","Birth_Day")
dobAll <- rbind(dob1, dob2)
dobAll$DOB <- ymd(paste(dobAll$Birth_Year, dobAll$Birth_Month,dobAll$Birth_Day))
class(dobAll$ID1)

df$SVID <- NA
df$sex <- NA
df$bestzygos <- NA
df$dob <- as.Date(NA)
for (i in 1:length(sorted.IDs)){
    individual <- df[which(df$ID == sorted.IDs[i]),]
    individual$SVID <- demo$T1[match(as.numeric(sorted.IDs[i]),demo$T1_alternate_id)]
    individual$sex <- demo$Sex1[match(as.numeric(sorted.IDs[i]),demo$T1_alternate_id)]
    individual$bestzygos <- demo$bestzygos[match(as.numeric(sorted.IDs[i]),demo$T1_alternate_id)]
    individual$dob <- dobAll$DOB[match(individual$SVID[1],dobAll$ID1)]
    df[which(df$ID == sorted.IDs[i]),] <- individual
}
                                        # 10 of the location data does not have any matching demographic information



df$bestzygos[which(df$bestzygos == "MZ")] <- 1
df$bestzygos[which(df$bestzygos == "DZ")] <- 2
df$bestzygos[which(df$bestzygos == "OS")] <- 2

df$bestzygos <- as.numeric(df$bestzygos)

#pdf('nbPoint_lm.pdf')
#plot(lm(df$nbTrip ~ df$wdays + df$sex + df$bestzygos))
#dev.off()

df$appType <- as.character(df$appType)
df$appType[which(df$appType == "ios")] <- 0
df$appType[which(df$appType == "android")] <- 1
df$appType <- as.numeric(df$appType)
class(df$appType)



head(df)
entropy <- readRDS('/mnt/workspace2/cotwins/perDay_entropy.rds')
as.character(entropy$ID) -> entropy$ID
df <- cbind(entropy,df)

rog <- readRDS('/mnt/workspace2/cotwins/perDay_rog.rds')
as.character(rog$ID) -> rog$ID
df <- cbind(rog,df)
## calculate radius and read in radius


library(lubridate)
df$age <- as.double(df$days - df$dob)/365 #all divided by 365 days, in years
df$summer <- NA
df$summer[which(month(df$days) %in% c(5,6,7,8))] <- 1 # during summer 
df$summer[which(month(df$days) %in% c(1,2,3,4,9,10,11,12))] <- 0# during school year


scale <- as.numeric(min(df$age)) #14.21 rescaling to the youngests age
class(scale)
df$age_scale <- df$age - scale
                                        #make family ID
df$FID <- as.numeric(substr(df$SVID, 3,6))
head(df)
df$ID <- as.numeric(substr(df$SVID, 3,8))
class(df$FID)

saveRDS(df,'regression_4_5.rds')











