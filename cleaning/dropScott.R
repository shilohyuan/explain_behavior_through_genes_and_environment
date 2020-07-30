library(dplyr)
library(lubridate)
locations <- readRDS('Michigan_DB_user_location_11_11_18.rds')
head(locations)
locations <- as.data.frame(locations)
class(locations$user_id) #character
class(locations$sample_time)
locations$sample_time <- ymd_hms(locations$sample_time)
locations <- locations %>% filter(sample_time > ymd('2015-01-02'), sample_time < ymd('2018-11-11'))

referencePair <- read.csv(file ="id_mapping.csv", header = TRUE, sep = ",")
referencePair <- as.data.frame(referencePair)
T1 <- select(referencePair,T1, T1_alternate_id, bestzygos)
T2 <- select(referencePair,T2, T2_alternate_id, bestzygos)
colnames(T2) <- c("T1","T1_alternate_id","bestzygos")
pair <- rbind(T1,T2)
head(pair)
class(pair$T1_alternate_id) #numeric

locations <- locations[which(as.numeric(locations$user_id) %in% pair$T1_alternate_id),]
length(locations$latitude) # 21,016,920
user_info <- readRDS('Michigan_DB_users_11_11_18.rds')
user_info <- select(user_info, alternate_id, app_type)
locations <- locations[!duplicated(locations[c('user_id', 'latitude', 'longitude', 'sample_time')]),]

length(locations$latitude)  #10,808,990
locations <- locations %>%
  select(id:timestamp_type) %>%
  filter(
    !is.na(longitude),
    !is.na(latitude),
    !is.na(creation_date),
    !is.na(sample_time),
    !is.na(user_id),
    !is.na(accuracy)) %>%
    mutate(creation_date = ymd_hms(creation_date), sample_time = ymd_hms(sample_time))
length(locations$latitude) #10,794,812
locations <- locations %>% filter(accuracy < 500)

length(locations$latitude) #9,433,385
locations <- left_join(locations, user_info, by = c('user_id' = 'alternate_id'))
locations <- filter(locations, app_type == 'android' | timestamp_type == 'iOS_UTC') #removing the ones without appType
length(locations$latitude)# 7,829,808
data <- locations
data <- select(data,user_id, latitude,longitude,sample_time,accuracy,sample_timezone,app_type) 
data$local_time <- data$sample_time + minutes(data$sample_timezone)
data2 <- data %>% distinct(user_id, local_time,.keep_all = TRUE)
length(data2) #7,825,884
saveRDS(data2,"raw1.rds")
