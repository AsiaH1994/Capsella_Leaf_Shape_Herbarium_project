#rm(list=ls())
library(FluMoDL)
library(dplyr)
library(fuzzyjoin)
library(weatherData)
library(rnoaa)
options(noaakey = "ifwilTHTRMxSsheNajCJqXNJTFbjAaBk")
library(rgdal)
library(zoo)
setwd("~/Documents")

#Required unique functions 
######### functions
citySwap = function(x, y, z){
  paste(x, y, x, z, sep = "")
}

#make a dataframe of the mean of tmax, tmin, prcp, including the ID number
citySwap2 = function(x, y, z, e, f, g, l, k){
  paste(x, y, z, l, k, x, e, x, f, x, g, sep = "")
}

#column names 
citySwap3 = function(x, y, z ){
  paste(y, x, z, sep = "")
}

#add min date 
citySwap_4 = function(x, y, z, j, h){
  paste(x, y, x, z, j, h, sep = "")
}

#one date and city column name, x = city2, i = city
citySwap4.5 = function(x, y, z, j, h, i){
  paste(x, y, i, z, j, h, sep = "")
}

#adding max and min dates 
citySwap5 = function(x, y, z, j, h, k, l, p){
  paste(x, y, j, z, h, k, l, p, sep = "")
}

## DOC year 
citySwap6 = function(x, y, z, j, l, k, p){
  paste(x, y, j, z, l, k, l, p, sep = "")
}

##paste into bind_rows()
citySwap7 = function(x, y, z){
  paste(x, y, z, sep = "")
}

#Prep Data
#replace "region" and "state" text with region and state of interest in all subsequent text. 

######## data prep
alabama_csv <- read.csv('alamaba_cities.csv')
alabama_csv$month <- match(alabama_csv$month, month.name)
alabama_csv$date <- paste(alabama_csv$year, alabama_csv$month, alabama_csv$day, sep="-")
strptime(alabama_csv$date,format="%Y-%m-%d")
alabama_csv$date <- as.Date(alabama_csv$date)
colnames(alabama_csv) <- c("id", 'id2', 'city_county_state', 'latitude', 'longitude', 'elevation', 'month', 'day', 'year', 'date')
alabama_csv$previous6mon <- as.Date(as.yearmon(as.Date(alabama_csv$date)) -.5, frac = 1)
head(alabama_csv)

alabama_csv <- alabama_csv[-c(19:34),]
## to use lapply 
city <- alabama_csv$id
city2 <- alabama_csv$id2
date <- alabama_csv$date
pyear <- (alabama_csv$date - 365)
p6mon <- alabama_csv$previous6mon
year <- alabama_csv$year
lennumber <- (1:18) #number of samples

## important data frames
df1 = list(city, city2, date)

df2 <- list(city, city2, p6mon, date)

df3 <- list(city2, lennumber)

df4 <- list(city, city2, pyear, date)

df5 <- list(city, city2, year)


#Find NOAA stations 
#if using for 1st time, use full code. Then save stations as ".txt".
#then filter by stations in neighboring states 
##NOAA stations - set up and filter by nearby states
stations <- ghcnd_stations()
write.table(stations, "ghcnd_stations.txt")

AGF_stations <- stations %>% filter(state == c("Al", "GA", "FL", "MS"))
write.csv2(AGF_stations, "AGF_stations.csv")

#after running the 1st time 
AGF_stations <- read.csv("AGF_stations.csv")

#find closests stations
## no data even 100 miles out. 
AL_clst <- meteo_nearby_stations(lat_lon_df = alabama_csv, station_data = AGF_stations, radius = 130, limit = 200)

?meteo_nearby_stations
### separate all cities 
cat(citySwap(unique(city), y = " <- (AL_clst$'", z = "')"), sep = "\n")

andalusia <- (AL_clst$'andalusia')
mobile <- (AL_clst$'mobile')
tuscaloosa <- (AL_clst$'tuscaloosa')
lauderdalecounty <- (AL_clst$'lauderdalecounty')
dothan <- (AL_clst$'dothan')
florence <- (AL_clst$'florence')
tuscumbia <- (AL_clst$'tuscumbia')
moulton <- (AL_clst$'moulton')
montgomery <- (AL_clst$'montgomery')
athens <- (AL_clst$'athens')

### separate all station id's (monitors) for each city
cat(citySwap(unique(city), y = "_monitors <- ", z = "$id"), sep = "\n")

andalusia_monitors <- andalusia$id
mobile_monitors <- mobile$id
tuscaloosa_monitors <- tuscaloosa$id
lauderdalecounty_monitors <- lauderdalecounty$id
dothan_monitors <- dothan$id
florence_monitors <- florence$id
tuscumbia_monitors <- tuscumbia$id
moulton_monitors <- moulton$id
montgomery_monitors <- montgomery$id
athens_monitors <- athens$id

### pull all observations for cities 
cat(citySwap(unique(city), y = "_obs <- meteo_pull_monitors(", z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')"), sep = "\n")

andalusia_obs <- meteo_pull_monitors(andalusia_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
mobile_obs <- meteo_pull_monitors(mobile_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
tuscaloosa_obs <- meteo_pull_monitors(tuscaloosa_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
lauderdalecounty_obs <- meteo_pull_monitors(lauderdalecounty_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
dothan_obs <- meteo_pull_monitors(dothan_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
florence_obs <- meteo_pull_monitors(florence_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
tuscumbia_obs <- meteo_pull_monitors(tuscumbia_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
moulton_obs <- meteo_pull_monitors(moulton_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
montgomery_obs <- meteo_pull_monitors(montgomery_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
athens_obs <- meteo_pull_monitors(athens_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')

### Part 1
### pull data by date and location - dates are coming from state_csv 
lapply(df1, cat(citySwap4.5(x = city2, y = "_obs_temps <- ", i = city, z = "_obs %>% filter(date == '", j = date , h = "')"), sep = "\n"))

andalusia1_obs_temps <- andalusia_obs %>% filter(date == '2017-12-30')
mobile1_obs_temps <- mobile_obs %>% filter(date == '1992-03-01')
tuscaloosa1_obs_temps <- tuscaloosa_obs %>% filter(date == '1975-02-06')
lauderdalecounty_obs_temps <- lauderdalecounty_obs %>% filter(date == '1969-03-26')
dothan_obs_temps <- dothan_obs %>% filter(date == '2011-12-11')
andalusia2_obs_temps <- andalusia_obs %>% filter(date == '2017-12-30')
florence_obs_temps <- florence_obs %>% filter(date == '1973-04-10')
tuscumbia_obs_temps <- tuscumbia_obs %>% filter(date == '1973-03-28')
tuscaloosa2_obs_temps <- tuscaloosa_obs %>% filter(date == '1955-03-26')
mobile2_obs_temps <- mobile_obs %>% filter(date == '2012-01-15')
mobile3_obs_temps <- mobile_obs %>% filter(date == '1988-03-19')
tuscaloosa3_obs_temps <- tuscaloosa_obs %>% filter(date == '1955-03-26')
moulton_obs_temps <- moulton_obs %>% filter(date == '2000-01-19')
montgomery_obs_temps <- montgomery_obs %>% filter(date == '2010-03-18')
mobile4_obs_temps <- mobile_obs %>% filter(date == '1992-02-29')
athens_obs_temps <- athens_obs %>% filter(date == '1959-03-19')
tuscaloosa4_obs_temps <- tuscaloosa_obs %>% filter(date == '1955-02-22')
tuscaloosa5_obs_temps <- tuscaloosa_obs %>% filter(date == '1957-02-07')

#combine into one data frame using bind_rows()
cat(citySwap7(city2, y = "_obs_temps", z = ","), sep = "")
AL_obs <- bind_rows(andalusia1_obs_temps,mobile1_obs_temps,tuscaloosa1_obs_temps,lauderdalecounty_obs_temps,dothan_obs_temps,andalusia2_obs_temps,florence_obs_temps,tuscumbia_obs_temps,tuscaloosa2_obs_temps,mobile2_obs_temps,mobile3_obs_temps,tuscaloosa3_obs_temps,moulton_obs_temps,montgomery_obs_temps,mobile4_obs_temps,athens_obs_temps,tuscaloosa4_obs_temps,tuscaloosa5_obs_temps)

#Merge the original state_csv dataframe and new state_obs dataframe 
AL_data <- merge.data.frame(alabama_csv, AL_obs, by = 'date')

## find the tavg for state data 
AL_data$tavg <- ((AL_data$tmax + AL_data$tmin) / 2)

### save data as xlsx file 
library("xlsx")
write.xlsx(AL_data, file = "alabama_data_total_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

### Part 2
#finding data from 6 months prior to date of collection
#ambient temps and precipitation may have affected plant/leaf growth and shape
lapply(df2, cat(citySwap5(x = city2, y = "_obs_6months <- meteo_pull_monitors(", j = city, 
                          z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '", h = p6mon, 
                          k = "', date_max = '", l = date, p ="')"), sep = "\n"))

andalusia1_obs_6months <- meteo_pull_monitors(andalusia_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2017-06-30', date_max = '2017-12-30')
mobile1_obs_6months <- meteo_pull_monitors(mobile_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1991-09-30', date_max = '1992-03-01')
tuscaloosa1_obs_6months <- meteo_pull_monitors(tuscaloosa_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1974-08-31', date_max = '1975-02-06')
lauderdalecounty_obs_6months <- meteo_pull_monitors(lauderdalecounty_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1968-09-30', date_max = '1969-03-26')
dothan_obs_6months <- meteo_pull_monitors(dothan_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2011-06-30', date_max = '2011-12-11')
andalusia2_obs_6months <- meteo_pull_monitors(andalusia_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2017-06-30', date_max = '2017-12-30')
florence_obs_6months <- meteo_pull_monitors(florence_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1972-10-31', date_max = '1973-04-10')
tuscumbia_obs_6months <- meteo_pull_monitors(tuscumbia_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1972-09-30', date_max = '1973-03-28')
tuscaloosa2_obs_6months <- meteo_pull_monitors(tuscaloosa_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1954-09-30', date_max = '1955-03-26')
mobile2_obs_6months <- meteo_pull_monitors(mobile_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2011-07-31', date_max = '2012-01-15')
mobile3_obs_6months <- meteo_pull_monitors(mobile_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1987-09-30', date_max = '1988-03-19')
tuscaloosa3_obs_6months <- meteo_pull_monitors(tuscaloosa_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1954-09-30', date_max = '1955-03-26')
moulton_obs_6months <- meteo_pull_monitors(moulton_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1999-07-31', date_max = '2000-01-19')
montgomery_obs_6months <- meteo_pull_monitors(montgomery_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2009-09-30', date_max = '2010-03-18')
mobile4_obs_6months <- meteo_pull_monitors(mobile_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1991-08-31', date_max = '1992-02-29')
athens_obs_6months <- meteo_pull_monitors(athens_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1958-09-30', date_max = '1959-03-19')
tuscaloosa4_obs_6months <- meteo_pull_monitors(tuscaloosa_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1954-08-31', date_max = '1955-02-22')
tuscaloosa5_obs_6months <- meteo_pull_monitors(tuscaloosa_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1956-08-31', date_max = '1957-02-07')

## make a dataframe of the mean of tmax, tmin, prcp of the previous 6 months 
lapply(df3, cat(citySwap2(x = city2,  y = "_6months", z = " <- data.frame(alabama_csv$id[", l= lennumber, k = "], (mean(", e = "_obs_6months$prcp, na.rm=TRUE)), (mean(", f = "_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(", g = "_obs_6months$tmin, na.rm=TRUE))) "), sep = "\n"))

andalusia1_6months <- data.frame(alabama_csv$id[1], (mean(andalusia1_obs_6months$prcp, na.rm=TRUE)), (mean(andalusia1_obs_6months$tmax, na.rm=TRUE)), 
                                 (mean(andalusia1_obs_6months$tmin, na.rm=TRUE))) 
mobile1_6months <- data.frame(alabama_csv$id[2], (mean(mobile1_obs_6months$prcp, na.rm=TRUE)), (mean(mobile1_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(mobile1_obs_6months$tmin, na.rm=TRUE))) 
tuscaloosa1_6months <- data.frame(alabama_csv$id[3], (mean(tuscaloosa1_obs_6months$prcp, na.rm=TRUE)), (mean(tuscaloosa1_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(tuscaloosa1_obs_6months$tmin, na.rm=TRUE))) 
lauderdalecounty_6months <- data.frame(alabama_csv$id[4], (mean(lauderdalecounty_obs_6months$prcp, na.rm=TRUE)), (mean(lauderdalecounty_obs_6months$tmax, na.rm=TRUE)), 
                                       (mean(lauderdalecounty_obs_6months$tmin, na.rm=TRUE))) 
dothan_6months <- data.frame(alabama_csv$id[5], (mean(dothan_obs_6months$prcp, na.rm=TRUE)), (mean(dothan_obs_6months$tmax, na.rm=TRUE)), 
                             (mean(dothan_obs_6months$tmin, na.rm=TRUE))) 
andalusia2_6months <- data.frame(alabama_csv$id[6], (mean(andalusia2_obs_6months$prcp, na.rm=TRUE)), (mean(andalusia2_obs_6months$tmax, na.rm=TRUE)), 
                                 (mean(andalusia2_obs_6months$tmin, na.rm=TRUE))) 
florence_6months <- data.frame(alabama_csv$id[7], (mean(florence_obs_6months$prcp, na.rm=TRUE)), (mean(florence_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(florence_obs_6months$tmin, na.rm=TRUE))) 
tuscumbia_6months <- data.frame(alabama_csv$id[8], (mean(tuscumbia_obs_6months$prcp, na.rm=TRUE)), (mean(tuscumbia_obs_6months$tmax, na.rm=TRUE)), 
                                (mean(tuscumbia_obs_6months$tmin, na.rm=TRUE))) 
tuscaloosa2_6months <- data.frame(alabama_csv$id[9], (mean(tuscaloosa2_obs_6months$prcp, na.rm=TRUE)), (mean(tuscaloosa2_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(tuscaloosa2_obs_6months$tmin, na.rm=TRUE))) 
mobile2_6months <- data.frame(alabama_csv$id[10], (mean(mobile2_obs_6months$prcp, na.rm=TRUE)), (mean(mobile2_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(mobile2_obs_6months$tmin, na.rm=TRUE))) 
mobile3_6months <- data.frame(alabama_csv$id[11], (mean(mobile3_obs_6months$prcp, na.rm=TRUE)), (mean(mobile3_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(mobile3_obs_6months$tmin, na.rm=TRUE))) 
tuscaloosa3_6months <- data.frame(alabama_csv$id[12], (mean(tuscaloosa3_obs_6months$prcp, na.rm=TRUE)), (mean(tuscaloosa3_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(tuscaloosa3_obs_6months$tmin, na.rm=TRUE))) 
moulton_6months <- data.frame(alabama_csv$id[13], (mean(moulton_obs_6months$prcp, na.rm=TRUE)), (mean(moulton_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(moulton_obs_6months$tmin, na.rm=TRUE))) 
montgomery_6months <- data.frame(alabama_csv$id[14], (mean(montgomery_obs_6months$prcp, na.rm=TRUE)), (mean(montgomery_obs_6months$tmax, na.rm=TRUE)), 
                                 (mean(montgomery_obs_6months$tmin, na.rm=TRUE))) 
mobile4_6months <- data.frame(alabama_csv$id[15], (mean(mobile4_obs_6months$prcp, na.rm=TRUE)), (mean(mobile4_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(mobile4_obs_6months$tmin, na.rm=TRUE))) 
athens_6months <- data.frame(alabama_csv$id[16], (mean(athens_obs_6months$prcp, na.rm=TRUE)), (mean(athens_obs_6months$tmax, na.rm=TRUE)), 
                             (mean(athens_obs_6months$tmin, na.rm=TRUE))) 
tuscaloosa4_6months <- data.frame(alabama_csv$id[17], (mean(tuscaloosa4_obs_6months$prcp, na.rm=TRUE)), (mean(tuscaloosa4_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(tuscaloosa4_obs_6months$tmin, na.rm=TRUE))) 
tuscaloosa5_6months <- data.frame(alabama_csv$id[18], (mean(tuscaloosa5_obs_6months$prcp, na.rm=TRUE)), (mean(tuscaloosa5_obs_6months$tmax, na.rm=TRUE)))

#rename col.names 
cat(citySwap3(city2, y = "colnames(", z = "_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')"), sep = "\n")

colnames(andalusia1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(mobile1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tuscaloosa1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(lauderdalecounty_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(dothan_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(andalusia2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(florence_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tuscumbia_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tuscaloosa2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(mobile2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(mobile3_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tuscaloosa3_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(moulton_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(montgomery_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(mobile4_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(athens_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tuscaloosa4_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tuscaloosa5_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')

## combine previous 6 months data using bind_rows
cat(citySwap7(city2, y = "_6months", z = ","), sep = "")
AL_6months <- bind_rows(andalusia1_6months,mobile1_6months,tuscaloosa1_6months,lauderdalecounty_6months,dothan_6months,andalusia2_6months,florence_6months,tuscumbia_6months,tuscaloosa2_6months,mobile2_6months,mobile3_6months,tuscaloosa3_6months,moulton_6months,montgomery_6months,mobile4_6months,athens_6months,tuscaloosa4_6months,tuscaloosa5_6months)

### save data as xlsx file 
write.xlsx(AL_6months, file = "AL_6months_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


### Part 3
#find data from previous year from date of collection 
lapply(df4, cat(citySwap5(x = city2, y = "_obs_year <- meteo_pull_monitors(", j = city, 
                          z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '", h= pyear, 
                          k = "', date_max = '", l = date, p ="')"), sep = "\n"))

andalusia1_obs_year <- meteo_pull_monitors(andalusia_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2016-12-30', date_max = '2017-12-30')
mobile1_obs_year <- meteo_pull_monitors(mobile_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1991-03-02', date_max = '1992-03-01')
tuscaloosa1_obs_year <- meteo_pull_monitors(tuscaloosa_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1974-02-06', date_max = '1975-02-06')
lauderdalecounty_obs_year <- meteo_pull_monitors(lauderdalecounty_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1968-03-26', date_max = '1969-03-26')
dothan_obs_year <- meteo_pull_monitors(dothan_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2010-12-11', date_max = '2011-12-11')
andalusia2_obs_year <- meteo_pull_monitors(andalusia_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2016-12-30', date_max = '2017-12-30')
florence_obs_year <- meteo_pull_monitors(florence_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1972-04-10', date_max = '1973-04-10')
tuscumbia_obs_year <- meteo_pull_monitors(tuscumbia_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1972-03-28', date_max = '1973-03-28')
tuscaloosa2_obs_year <- meteo_pull_monitors(tuscaloosa_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1954-03-26', date_max = '1955-03-26')
mobile2_obs_year <- meteo_pull_monitors(mobile_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2011-01-15', date_max = '2012-01-15')
mobile3_obs_year <- meteo_pull_monitors(mobile_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1987-03-20', date_max = '1988-03-19')
tuscaloosa3_obs_year <- meteo_pull_monitors(tuscaloosa_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1954-03-26', date_max = '1955-03-26')
moulton_obs_year <- meteo_pull_monitors(moulton_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1999-01-19', date_max = '2000-01-19')
montgomery_obs_year <- meteo_pull_monitors(montgomery_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2009-03-18', date_max = '2010-03-18')
mobile4_obs_year <- meteo_pull_monitors(mobile_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1991-03-01', date_max = '1992-02-29')
athens_obs_year <- meteo_pull_monitors(athens_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1958-03-19', date_max = '1959-03-19')
tuscaloosa4_obs_year <- meteo_pull_monitors(tuscaloosa_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1954-02-22', date_max = '1955-02-22')
tuscaloosa5_obs_year <- meteo_pull_monitors(tuscaloosa_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1956-02-08', date_max = '1957-02-07')

## make a data frame of the mean of tmax, tmin, prcp of the previous year before the date of collection 
lapply(df3, cat(citySwap2(x = city2,  y = "_previousyear", z = " <- data.frame(alabama_csv$id[", l= lennumber, 
                          k = "], (mean(", e = "_obs_year$prcp, na.rm=TRUE)), (mean(", 
                          f = "_obs_year$tmax, na.rm=TRUE)), (mean(", g = "_obs_year$tmin, na.rm=TRUE))) "), sep = "\n"))
andalusia1_previousyear <- data.frame(alabama_csv$id[1], (mean(andalusia1_obs_year$prcp, na.rm=TRUE)), (mean(andalusia1_obs_year$tmax, na.rm=TRUE)), (mean(andalusia1_obs_year$tmin, na.rm=TRUE))) 
mobile1_previousyear <- data.frame(alabama_csv$id[2], (mean(mobile1_obs_year$prcp, na.rm=TRUE)), (mean(mobile1_obs_year$tmax, na.rm=TRUE)), (mean(mobile1_obs_year$tmin, na.rm=TRUE))) 
tuscaloosa1_previousyear <- data.frame(alabama_csv$id[3], (mean(tuscaloosa1_obs_year$prcp, na.rm=TRUE)), (mean(tuscaloosa1_obs_year$tmax, na.rm=TRUE)), (mean(tuscaloosa1_obs_year$tmin, na.rm=TRUE))) 
lauderdalecounty_previousyear <- data.frame(alabama_csv$id[4], (mean(lauderdalecounty_obs_year$prcp, na.rm=TRUE)), (mean(lauderdalecounty_obs_year$tmax, na.rm=TRUE)), (mean(lauderdalecounty_obs_year$tmin, na.rm=TRUE))) 
dothan_previousyear <- data.frame(alabama_csv$id[5], (mean(dothan_obs_year$prcp, na.rm=TRUE)), (mean(dothan_obs_year$tmax, na.rm=TRUE)), (mean(dothan_obs_year$tmin, na.rm=TRUE))) 
andalusia2_previousyear <- data.frame(alabama_csv$id[6], (mean(andalusia2_obs_year$prcp, na.rm=TRUE)), (mean(andalusia2_obs_year$tmax, na.rm=TRUE)), (mean(andalusia2_obs_year$tmin, na.rm=TRUE))) 
florence_previousyear <- data.frame(alabama_csv$id[7], (mean(florence_obs_year$prcp, na.rm=TRUE)), (mean(florence_obs_year$tmax, na.rm=TRUE)), (mean(florence_obs_year$tmin, na.rm=TRUE))) 
tuscumbia_previousyear <- data.frame(alabama_csv$id[8], (mean(tuscumbia_obs_year$prcp, na.rm=TRUE)), (mean(tuscumbia_obs_year$tmax, na.rm=TRUE)), (mean(tuscumbia_obs_year$tmin, na.rm=TRUE))) 
tuscaloosa2_previousyear <- data.frame(alabama_csv$id[9], (mean(tuscaloosa2_obs_year$prcp, na.rm=TRUE)), (mean(tuscaloosa2_obs_year$tmax, na.rm=TRUE)), (mean(tuscaloosa2_obs_year$tmin, na.rm=TRUE))) 
mobile2_previousyear <- data.frame(alabama_csv$id[10], (mean(mobile2_obs_year$prcp, na.rm=TRUE)), (mean(mobile2_obs_year$tmax, na.rm=TRUE)), (mean(mobile2_obs_year$tmin, na.rm=TRUE))) 
mobile3_previousyear <- data.frame(alabama_csv$id[11], (mean(mobile3_obs_year$prcp, na.rm=TRUE)), (mean(mobile3_obs_year$tmax, na.rm=TRUE)), (mean(mobile3_obs_year$tmin, na.rm=TRUE))) 
tuscaloosa3_previousyear <- data.frame(alabama_csv$id[12], (mean(tuscaloosa3_obs_year$prcp, na.rm=TRUE)), (mean(tuscaloosa3_obs_year$tmax, na.rm=TRUE)), (mean(tuscaloosa3_obs_year$tmin, na.rm=TRUE))) 
moulton_previousyear <- data.frame(alabama_csv$id[13], (mean(moulton_obs_year$prcp, na.rm=TRUE)), (mean(moulton_obs_year$tmax, na.rm=TRUE)), (mean(moulton_obs_year$tmin, na.rm=TRUE))) 
montgomery_previousyear <- data.frame(alabama_csv$id[14], (mean(montgomery_obs_year$prcp, na.rm=TRUE)), (mean(montgomery_obs_year$tmax, na.rm=TRUE)), (mean(montgomery_obs_year$tmin, na.rm=TRUE))) 
mobile4_previousyear <- data.frame(alabama_csv$id[15], (mean(mobile4_obs_year$prcp, na.rm=TRUE)), (mean(mobile4_obs_year$tmax, na.rm=TRUE)), (mean(mobile4_obs_year$tmin, na.rm=TRUE))) 
athens_previousyear <- data.frame(alabama_csv$id[16], (mean(athens_obs_year$prcp, na.rm=TRUE)), (mean(athens_obs_year$tmax, na.rm=TRUE)), (mean(athens_obs_year$tmin, na.rm=TRUE))) 
tuscaloosa4_previousyear <- data.frame(alabama_csv$id[17], (mean(tuscaloosa4_obs_year$prcp, na.rm=TRUE)), (mean(tuscaloosa4_obs_year$tmax, na.rm=TRUE)), (mean(tuscaloosa4_obs_year$tmin, na.rm=TRUE))) 
tuscaloosa5_previousyear <- data.frame(alabama_csv$id[18], (mean(tuscaloosa5_obs_year$prcp, na.rm=TRUE)), (mean(tuscaloosa5_obs_year$tmax, na.rm=TRUE)), (mean(tuscaloosa5_obs_year$tmin, na.rm=TRUE)))

#rename col.names 
cat(citySwap3(city2, y = "colnames(", z = "_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')"), sep = "\n")

colnames(andalusia1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(mobile1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tuscaloosa1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(lauderdalecounty_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(dothan_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(andalusia2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(florence_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tuscumbia_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tuscaloosa2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(mobile2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(mobile3_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tuscaloosa3_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(moulton_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(montgomery_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(mobile4_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(athens_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tuscaloosa4_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tuscaloosa5_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')

## combine previous year before the date of collection data using bind_rows
cat(citySwap7(city2, y = "_previousyear", z = ","), sep = "")
AL_Pyear <- bind_rows(andalusia1_previousyear,mobile1_previousyear,tuscaloosa1_previousyear,lauderdalecounty_previousyear,dothan_previousyear,andalusia2_previousyear,florence_previousyear,tuscumbia_previousyear,tuscaloosa2_previousyear,mobile2_previousyear,mobile3_previousyear,tuscaloosa3_previousyear,moulton_previousyear,montgomery_previousyear,mobile4_previousyear,athens_previousyear,tuscaloosa4_previousyear,tuscaloosa5_previousyear)

# save data
write.xlsx(AL_Pyear, file = "AL_previous_year_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


### Part 4 
#Find data from current year of date of collection 
lapply(df5, cat(citySwap6(x = city2, y = "_obs_yearDOC <- meteo_pull_monitors(", j = city, 
                          z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '", l = year , 
                          k = "-01-01', date_max = '",  p ="-12-31')"), sep = "\n"))

andalusia1_obs_yearDOC <- meteo_pull_monitors(andalusia_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2017-01-01', date_max = '2017-12-31')
mobile1_obs_yearDOC <- meteo_pull_monitors(mobile_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1992-01-01', date_max = '1992-12-31')
tuscaloosa1_obs_yearDOC <- meteo_pull_monitors(tuscaloosa_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1975-01-01', date_max = '1975-12-31')
lauderdalecounty_obs_yearDOC <- meteo_pull_monitors(lauderdalecounty_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1969-01-01', date_max = '1969-12-31')
dothan_obs_yearDOC <- meteo_pull_monitors(dothan_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2011-01-01', date_max = '2011-12-31')
andalusia2_obs_yearDOC <- meteo_pull_monitors(andalusia_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2017-01-01', date_max = '2017-12-31')
florence_obs_yearDOC <- meteo_pull_monitors(florence_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1973-01-01', date_max = '1973-12-31')
tuscumbia_obs_yearDOC <- meteo_pull_monitors(tuscumbia_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1973-01-01', date_max = '1973-12-31')
tuscaloosa2_obs_yearDOC <- meteo_pull_monitors(tuscaloosa_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1955-01-01', date_max = '1955-12-31')
mobile2_obs_yearDOC <- meteo_pull_monitors(mobile_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2012-01-01', date_max = '2012-12-31')
mobile3_obs_yearDOC <- meteo_pull_monitors(mobile_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1988-01-01', date_max = '1988-12-31')
tuscaloosa3_obs_yearDOC <- meteo_pull_monitors(tuscaloosa_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1955-01-01', date_max = '1955-12-31')
moulton_obs_yearDOC <- meteo_pull_monitors(moulton_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2000-01-01', date_max = '2000-12-31')
montgomery_obs_yearDOC <- meteo_pull_monitors(montgomery_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2010-01-01', date_max = '2010-12-31')
mobile4_obs_yearDOC <- meteo_pull_monitors(mobile_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1992-01-01', date_max = '1992-12-31')
athens_obs_yearDOC <- meteo_pull_monitors(athens_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1959-01-01', date_max = '1959-12-31')
tuscaloosa4_obs_yearDOC <- meteo_pull_monitors(tuscaloosa_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1955-01-01', date_max = '1955-12-31')
tuscaloosa5_obs_yearDOC <- meteo_pull_monitors(tuscaloosa_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1957-01-01', date_max = '1957-12-31')

## make a dataframe of the mean of tmax, tmin, current year of the date of collection
lapply(dflist_nodates, cat(citySwap2(x = city2,  y = "_DOC", z = " <- data.frame(alabama_csv$id[", l= lennumber, k = "], (mean(", e = "_obs_yearDOC$prcp, na.rm=TRUE)), (mean(", f = "_obs_yearDOC$tmax, na.rm=TRUE)), (mean(", g = "_obs_yearDOC$tmin, na.rm=TRUE))) "), sep = "\n"))

andalusia1_DOC <- data.frame(alabama_csv$id[1], (mean(andalusia1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(andalusia1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(andalusia1_obs_yearDOC$tmin, na.rm=TRUE))) 
mobile1_DOC <- data.frame(alabama_csv$id[2], (mean(mobile1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(mobile1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(mobile1_obs_yearDOC$tmin, na.rm=TRUE))) 
tuscaloosa1_DOC <- data.frame(alabama_csv$id[3], (mean(tuscaloosa1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(tuscaloosa1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(tuscaloosa1_obs_yearDOC$tmin, na.rm=TRUE))) 
lauderdalecounty_DOC <- data.frame(alabama_csv$id[4], (mean(lauderdalecounty_obs_yearDOC$prcp, na.rm=TRUE)), (mean(lauderdalecounty_obs_yearDOC$tmax, na.rm=TRUE)), (mean(lauderdalecounty_obs_yearDOC$tmin, na.rm=TRUE))) 
dothan_DOC <- data.frame(alabama_csv$id[5], (mean(dothan_obs_yearDOC$prcp, na.rm=TRUE)), (mean(dothan_obs_yearDOC$tmax, na.rm=TRUE)), (mean(dothan_obs_yearDOC$tmin, na.rm=TRUE))) 
andalusia2_DOC <- data.frame(alabama_csv$id[6], (mean(andalusia2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(andalusia2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(andalusia2_obs_yearDOC$tmin, na.rm=TRUE))) 
florence_DOC <- data.frame(alabama_csv$id[7], (mean(florence_obs_yearDOC$prcp, na.rm=TRUE)), (mean(florence_obs_yearDOC$tmax, na.rm=TRUE)), (mean(florence_obs_yearDOC$tmin, na.rm=TRUE))) 
tuscumbia_DOC <- data.frame(alabama_csv$id[8], (mean(tuscumbia_obs_yearDOC$prcp, na.rm=TRUE)), (mean(tuscumbia_obs_yearDOC$tmax, na.rm=TRUE)), (mean(tuscumbia_obs_yearDOC$tmin, na.rm=TRUE))) 
tuscaloosa2_DOC <- data.frame(alabama_csv$id[9], (mean(tuscaloosa2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(tuscaloosa2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(tuscaloosa2_obs_yearDOC$tmin, na.rm=TRUE))) 
mobile2_DOC <- data.frame(alabama_csv$id[10], (mean(mobile2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(mobile2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(mobile2_obs_yearDOC$tmin, na.rm=TRUE))) 
mobile3_DOC <- data.frame(alabama_csv$id[11], (mean(mobile3_obs_yearDOC$prcp, na.rm=TRUE)), (mean(mobile3_obs_yearDOC$tmax, na.rm=TRUE)), (mean(mobile3_obs_yearDOC$tmin, na.rm=TRUE))) 
tuscaloosa3_DOC <- data.frame(alabama_csv$id[12], (mean(tuscaloosa3_obs_yearDOC$prcp, na.rm=TRUE)), (mean(tuscaloosa3_obs_yearDOC$tmax, na.rm=TRUE)), (mean(tuscaloosa3_obs_yearDOC$tmin, na.rm=TRUE))) 
moulton_DOC <- data.frame(alabama_csv$id[13], (mean(moulton_obs_yearDOC$prcp, na.rm=TRUE)), (mean(moulton_obs_yearDOC$tmax, na.rm=TRUE)), (mean(moulton_obs_yearDOC$tmin, na.rm=TRUE))) 
montgomery_DOC <- data.frame(alabama_csv$id[14], (mean(montgomery_obs_yearDOC$prcp, na.rm=TRUE)), (mean(montgomery_obs_yearDOC$tmax, na.rm=TRUE)), (mean(montgomery_obs_yearDOC$tmin, na.rm=TRUE))) 
mobile4_DOC <- data.frame(alabama_csv$id[15], (mean(mobile4_obs_yearDOC$prcp, na.rm=TRUE)), (mean(mobile4_obs_yearDOC$tmax, na.rm=TRUE)), (mean(mobile4_obs_yearDOC$tmin, na.rm=TRUE))) 
athens_DOC <- data.frame(alabama_csv$id[16], (mean(athens_obs_yearDOC$prcp, na.rm=TRUE)), (mean(athens_obs_yearDOC$tmax, na.rm=TRUE)), (mean(athens_obs_yearDOC$tmin, na.rm=TRUE))) 
tuscaloosa4_DOC <- data.frame(alabama_csv$id[17], (mean(tuscaloosa4_obs_yearDOC$prcp, na.rm=TRUE)), (mean(tuscaloosa4_obs_yearDOC$tmax, na.rm=TRUE)), (mean(tuscaloosa4_obs_yearDOC$tmin, na.rm=TRUE))) 
tuscaloosa5_DOC <- data.frame(alabama_csv$id[18], (mean(tuscaloosa5_obs_yearDOC$prcp, na.rm=TRUE)), (mean(tuscaloosa5_obs_yearDOC$tmax, na.rm=TRUE)), (mean(tuscaloosa5_obs_yearDOC$tmin, na.rm=TRUE)))

#rename col.names 
cat(citySwap3(city2, y = "colnames(", z = "_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')"), sep = "\n")

colnames(andalusia1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(mobile1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tuscaloosa1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(lauderdalecounty_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(dothan_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(andalusia2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(florence_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tuscumbia_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tuscaloosa2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(mobile2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(mobile3_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tuscaloosa3_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(moulton_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(montgomery_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(mobile4_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(athens_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tuscaloosa4_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tuscaloosa5_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')

## combine current year of the date of collection data using bind_rows
cat(citySwap7(city2, y = "_DOC", z = ","), sep = "")
AL_yearDOC <- bind_rows(andalusia1_DOC,mobile1_DOC,tuscaloosa1_DOC,lauderdalecounty_DOC,dothan_DOC,andalusia2_DOC,florence_DOC,tuscumbia_DOC,tuscaloosa2_DOC,mobile2_DOC,mobile3_DOC,tuscaloosa3_DOC,moulton_DOC,montgomery_DOC,mobile4_DOC,athens_DOC,tuscaloosa4_DOC,tuscaloosa5_DOC)

# save data
write.xlsx(AL_yearDOC, file = "AL_year_DOC_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
