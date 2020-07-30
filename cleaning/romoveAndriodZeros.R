## use the dataset after cotwins/analyses/src/location/clean.R
## remove points with latitude 0.000 and longitude 0.000


## detele
## android devices also record locationw when individuals are not moving
## there are 0 block effec (cite that paper) observed in our dataset as well
## only the beginning of the activity and end of the acitivty (start of the next movement) were kept
## this step is called removing duplicated points as a way to deal with systematic error in the method section of the manuscript

df <- readRDS('data3.rds')
length(df$latitude) # 6,690,282
df$step <- df$step*1000
df$speed <- df$step/df$time_track

zero_block <- which(df$app_type == "android" & df$step == 0)
split_block <- split(zero_block, cumsum(c(1,diff(zero_block) != 1)))
split_block<- split_block[which(lengths(split_block) > 1)] #to make the for loop faster

delete <- c()
for (i in 1:length(split_block)) {
    #print(i)
    q <-  unlist(split_block[i],use.name=FALSE)
    q[1] <- NA
    delete <- c(delete,q)
}
delete <- c(delete[which(complete.cases(delete))])
df <- df[-delete,]
length(df$latitude) #1135577
saveRDS(df,"data3_speedremoved.rds")

## stayPoint.R
## random error (urban canyoninng errors)
## that involves signal jumpings were not treated but would be resolves
## in the calculation of stay points (stay within 200 meters for at least 30 mins
