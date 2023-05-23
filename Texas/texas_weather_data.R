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
TX_csv <- read.csv('texas_cities.csv')
TX_csv$month <- match(TX_csv$month, month.name)
TX_csv$date <- paste(TX_csv$year, TX_csv$month, TX_csv$day, sep="-")
strptime(TX_csv$date,format="%Y-%m-%d")
TX_csv$date <- as.Date(state_csv$date)
colnames(TX_csv) <- c("id", 'id2', 'city_county_state', 'latitude', 'longitude', 'elevation', 'month', 'day', 'year', 'date')
TX_csv$previous6mon <- as.Date(as.yearmon(as.Date(TX_csv$date)) -.5, frac = 1)
head(TX_csv)

## to use lapply 
city <- TX_csv$id
city2 <- TX_csv$id2
date <- TX_csv$date
pyear <- (TX_csv$date - 365)
p6mon <- TX_csv$previous6mon
year <- TX_csv$year
lennumber <- (1:x) #number of samples

## important data frames
df1 <- list(city, city2, date)

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

southwest_stations <- stations %>% filter(state == c("TX", "OK", "NM"))
write.csv2(southwest_stations, "southwest_stations.csv")

#after running the 1st time 
region_stations <- read.csv("great_lakes_stations.csv")


#find closests stations
TX_clst <- meteo_nearby_stations(lat_lon_df = TX_csv, station_data = southwest_stations, radius = 50, limit = 200)

### separate all cities 
cat(citySwap(unique(city), y = " <- (TX_clst$'", z = "')"), sep = "\n")

### separate all station id's (monitors) for each city
cat(citySwap(unique(city), y = "_monitors <- ", z = "$id"), sep = "\n")

### pull all observations for cities 
cat(citySwap(unique(city), y = "_obs <- meteo_pull_monitors(", z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')"), sep = "\n")

### Part 1
### pull data by date and location - dates are coming from state_csv 
lapply(df1, cat(citySwap4.5(x = city2, y = "_obs_temps <- ", i = city, z = "_obs %>% filter(date == '", j = date , h = "')"), sep = "\n"))


#combine into one data frame using bind_rows()
cat(citySwap7(city2, y = "_obs_temps", z = ","), sep = "")
TX_obs <- bind_rows()

#Merge the original state_csv dataframe and new state_obs dataframe 
TX_data <- merge.data.frame(TX_csv, TX_obs, by = 'date')

## find the tavg for state data 
TX_data$tavg <- ((TX_data$tmax + state_data$tmin) / 2)

### save data as xlsx file 
library("xlsx")
write.xlsx(TX_data, file = "state_data_total_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

### Part 2
#finding data from 6 months prior to date of collection
#ambient temps and precipitation may have affected plant/leaf growth and shape
lapply(df2, cat(citySwap5(x = city2, y = "_obs_6months <- meteo_pull_monitors(", j = city, 
                          z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '", h = p6mon, 
                          k = "', date_max = '", l = date, p ="')"), sep = "\n"))

## make a dataframe of the mean of tmax, tmin, prcp of the previous 6 months 
lapply(df3, cat(citySwap2(x = city2,  y = "_6months", z = " <- data.frame(TX_csv$id[", l= lennumber, k = "], (mean(", e = "_obs_6months$prcp, na.rm=TRUE)), (mean(", f = "_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(", g = "_obs_6months$tmin, na.rm=TRUE))) "), sep = "\n"))

#rename col.names 
cat(citySwap3(city2, y = "colnames(", z = "_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')"), sep = "\n")

## combine previous 6 months data using bind_rows
cat(citySwap7(city2, y = "_6months", z = ","), sep = "")
TX_6months <- bind_rows()

### save data as xlsx file 
write.xlsx(TX_6months, file = "TX_6months_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


### Part 3
#find data from previous year from date of collection 
lapply(df4, cat(citySwap5(x = city2, y = "_obs_year <- meteo_pull_monitors(", j = city, 
                          z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '", h= pyear, 
                          k = "', date_max = '", l = date, p ="')"), sep = "\n"))

## make a data frame of the mean of tmax, tmin, prcp of the previous year before the date of collection 
lapply(df3, cat(citySwap2(x = city2,  y = "_previousyear", z = " <- data.frame(TX_csv$id[", l= lennumber, 
                          k = "], (mean(", e = "_obs_year$prcp, na.rm=TRUE)), (mean(", 
                          f = "_obs_year$tmax, na.rm=TRUE)), (mean(", g = "_obs_year$tmin, na.rm=TRUE))) "), sep = "\n"))

#rename col.names 
cat(citySwap3(city2, y = "colnames(", z = "_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')"), sep = "\n")

## combine previous year before the date of collection data using bind_rows
cat(citySwap7(city2, y = "_previousyear", z = ","), sep = "")
TX_Pyear <- bind_rows()

# save data
write.xlsx(TX_Pyear, file = "TX_previous_year_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


### Part 4 
#Find data from current year of date of collection 
lapply(df5, cat(citySwap6(x = city2, y = "_obs_yearDOC <- meteo_pull_monitors(", j = city, 
                          z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '", l = year , 
                          k = "-01-01', date_max = '",  p ="-12-31')"), sep = "\n"))

## make a dataframe of the mean of tmax, tmin, current year of the date of collection
lapply(dflist_nodates, cat(citySwap2(x = city2,  y = "_DOC", z = " <- data.frame(TX_csv$id[", l= lennumber, k = "], (mean(", e = "_obs_yearDOC$prcp, na.rm=TRUE)), (mean(", f = "_obs_yearDOC$tmax, na.rm=TRUE)), (mean(", g = "_obs_yearDOC$tmin, na.rm=TRUE))) "), sep = "\n"))

#rename col.names 
cat(citySwap3(city2, y = "colnames(", z = "_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')"), sep = "\n")


## combine current year of the date of collection data using bind_rows
cat(citySwap7(city2, y = "_DOC", z = ","), sep = "")
TX_yearDOC <- bind_rows()

# save data
write.xlsx(TX_yearDOC, file = "TX_year_DOC_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)