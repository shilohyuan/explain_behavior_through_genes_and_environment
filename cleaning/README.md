### Data Cleaning
```sh
dropScott.R
#remove administrator's data
#remove < 500m accuracy (including negative accuracy)
#remove points in the future
#remove points with any missing field (latitude, longitude, accuracy, appType)
#remove duplicated points

clean.R
#takes an .rds dataset(latitude, longitude, and time) as input
#outputs a dataset that organized by local time with step length derived
#also remove negative accuracy
#remove latitude == 0 (android devices systematic errors)

romoveAndriodZeros.R
#also removed blocks of 0 in android devices

stayPoint.R
#smooth out the signal jumping error
#code logic from Zheng et al., 2011
#example:
#Rscript stayPoint.R 1 588
# 200m_1_to_588.rds (for summary statistic)

dropStayPoint.R
# remove layover-type travel, travel by car, stay points with low accuracy
# output stayPoint_cleaned.rds

stayPoint_summary.R
# output stayPoint_cleaned_perDay.rds

removeTravelDay.R
# removing days with mileage > 200km
# need to use stayPoint_cleaned_perDay.rds to identify daily mileage > 200km
# then remove those days in the original file stayPoint_cleaned.rds and
# outputs stayPoint_cleaned_travelRemoved.rds

stayPoint_summary.R
# run as follows:
# Rscript stayPoint_summary.R stayPoint_cleaned_travelRemoved
# used again to generate the final stayPoint_cleaned_travelRemoved_summary.rds
# that could be used in generating a Table 1 in the paper

clean_summary.Rmd
# take in stayPoint_cleaned_travelRemoved_summary.rds and
# outputs a html table
```



