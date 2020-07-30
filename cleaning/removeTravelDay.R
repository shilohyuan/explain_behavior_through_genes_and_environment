library(lubridate)

df <- readRDS('stayPoint_cleaned_perDay.rds')
new <- df[which(df$dailyMileage > 200),]
new$ID <- as.character(new$ID)
head(new)
removeID <- names(sort(table(new$ID)))
removeID
sp <- readRDS('stayPoint_cleaned.rds')
head(sp)
for (i in 1:length(removeID)){
    print(i)
    individual <- sp[which(sp$user_id == removeID[i]),]
    days <- new$days[which(new$ID == removeID[i])]
    individual$latitude[which(date(individual$arvT) %in% days)] <- NA
    individual -> sp[which(sp$user_id == removeID[i]),]    
}

length(sp$latitude)
sp <- sp[which(complete.cases(sp$latitude)),]
length(sp$latitude)
saveRDS(sp,"stayPoint_cleaned_travelRemoved.rds")
