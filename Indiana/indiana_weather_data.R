rm(list=ls())
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
IN_csv <- read.csv('indiana_cities.csv')
IN_csv$month <- match(IN_csv$month, month.name)
IN_csv$date <- paste(IN_csv$year, IN_csv$month, IN_csv$day, sep="-")
strptime(IN_csv$date,format="%Y-%m-%d")
IN_csv$date <- as.Date(IN_csv$date)
colnames(IN_csv) <- c("id", 'id2', 'city_county_state', 'latitude', 'longitude', 'elevation', 'month', 'day', 'year', 'date')
IN_csv$previous6mon <- as.Date(as.yearmon(as.Date(IN_csv$date)) -.5, frac = 1)
head(IN_csv)

## to use lapply 
city <- IN_csv$id
city2 <- IN_csv$id2
date <- IN_csv$date
pyear <- (IN_csv$date - 365)
p6mon <- IN_csv$previous6mon
year <- IN_csv$year
lennumber <- (1:22) #number of samples

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

great_lakes_stations <- stations %>% filter(state == c("OH", "IL", "IN", "MI", "WI"))
write.csv2(great_lakes_stations, "great_lakes_stations.csv")

#after running the 1st time 
great_lakes_stations <- read.csv("great_lakes_stations.csv")

#find closests stations
IN_clst <- meteo_nearby_stations(lat_lon_df = IN_csv, station_data = great_lakes_stations, radius = 50, limit = 200)

### separate all cities 
cat(citySwap(unique(city), y = " <- (IN_clst$'", z = "')"), sep = "\n")

bedford <- (IN_clst$'bedford')
sulivan <- (IN_clst$'sulivan')
richmond <- (IN_clst$'richmond')
monroecounty <- (IN_clst$'monroecounty')
southport <- (IN_clst$'southport')
decatur <- (IN_clst$'decatur')
wellscounty <- (IN_clst$'wellscounty')
rockville <- (IN_clst$'rockville')
bloomington <- (IN_clst$'bloomington')
huntington <- (IN_clst$'huntington')
evansville <- (IN_clst$'evansville')
allencounty <- (IN_clst$'allencounty')
morristown <- (IN_clst$'morristown')
rushville <- (IN_clst$'rushville')
bloomingdale <- (IN_clst$'bloomingdale')
warsaw <- (IN_clst$'warsaw')
mountvernon <- (IN_clst$'mountvernon')

### separate all station id's (monitors) for each city
cat(citySwap(unique(city), y = "_monitors <- ", z = "$id"), sep = "\n")

bedford_monitors <- bedford$id
sulivan_monitors <- sulivan$id
richmond_monitors <- richmond$id
monroecounty_monitors <- monroecounty$id
southport_monitors <- southport$id
decatur_monitors <- decatur$id
wellscounty_monitors <- wellscounty$id
rockville_monitors <- rockville$id
bloomington_monitors <- bloomington$id
huntington_monitors <- huntington$id
evansville_monitors <- evansville$id
allencounty_monitors <- allencounty$id
morristown_monitors <- morristown$id
rushville_monitors <- rushville$id
bloomingdale_monitors <- bloomingdale$id
warsaw_monitors <- warsaw$id
mountvernon_monitors <- mountvernon$id

### pull all observations for cities 
cat(citySwap(unique(city), y = "_obs <- meteo_pull_monitors(", z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')"), sep = "\n")

bedford_obs <- meteo_pull_monitors(bedford_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
sulivan_obs <- meteo_pull_monitors(sulivan_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
richmond_obs <- meteo_pull_monitors(richmond_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
monroecounty_obs <- meteo_pull_monitors(monroecounty_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
southport_obs <- meteo_pull_monitors(southport_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
decatur_obs <- meteo_pull_monitors(decatur_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
wellscounty_obs <- meteo_pull_monitors(wellscounty_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
rockville_obs <- meteo_pull_monitors(rockville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
bloomington_obs <- meteo_pull_monitors(bloomington_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
huntington_obs <- meteo_pull_monitors(huntington_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
evansville_obs <- meteo_pull_monitors(evansville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
allencounty_obs <- meteo_pull_monitors(allencounty_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
morristown_obs <- meteo_pull_monitors(morristown_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
rushville_obs <- meteo_pull_monitors(rushville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
bloomingdale_obs <- meteo_pull_monitors(bloomingdale_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
warsaw_obs <- meteo_pull_monitors(warsaw_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
mountvernon_obs <- meteo_pull_monitors(mountvernon_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')

### Part 1
### pull data by date and location - dates are coming from IN_csv 
lapply(df1, cat(citySwap4.5(x = city2, y = "_obs_temps <- ", i = city, z = "_obs %>% filter(date == '", j = date , h = "')"), sep = "\n"))

bedford1_obs_temps <- bedford_obs %>% filter(date == '1951-05-04')
sulivan_obs_temps <- sulivan_obs %>% filter(date == '1974-04-17')
richmond_obs_temps <- richmond_obs %>% filter(date == '1973-04-20')
monroecounty_obs_temps <- monroecounty_obs %>% filter(date == '1960-04-27')
southport1_obs_temps <- southport_obs %>% filter(date == '1935-04-27')
decatur1_obs_temps <- decatur_obs %>% filter(date == '1966-05-01')
wellscounty_obs_temps <- wellscounty_obs %>% filter(date == '1900-05-13')
southport2_obs_temps <- southport_obs %>% filter(date == '1935-04-27')
rockville_obs_temps <- rockville_obs %>% filter(date == '1980-10-25')
bloomington_obs_temps <- bloomington_obs %>% filter(date == '1961-04-23')
huntington1_obs_temps <- huntington_obs %>% filter(date == '1937-05-08')
evansville_obs_temps <- evansville_obs %>% filter(date == '1941-04-12')
allencounty_obs_temps <- allencounty_obs %>% filter(date == '1942-05-03')
morristown_obs_temps <- morristown_obs %>% filter(date == '1913-05-07')
huntington2_obs_temps <- huntington_obs %>% filter(date == '1946-04-28')
rushville_obs_temps <- rushville_obs %>% filter(date == '1979-05-11')
huntington3_obs_temps <- huntington_obs %>% filter(date == '1969-06-26')
bloomingdale_obs_temps <- bloomingdale_obs %>% filter(date == '1929-09-20')
decatur2_obs_temps <- decatur_obs %>% filter(date == '1905-04-30')
bedford2_obs_temps <- bedford_obs %>% filter(date == '1933-04-17')
warsaw_obs_temps <- warsaw_obs %>% filter(date == '1932-05-17')
mountvernon_obs_temps <- mountvernon_obs %>% filter(date == '2021-04-11')

#combine into one data frame using bind_rows()
cat(citySwap7(city2, y = "_obs_temps", z = ","), sep = "")
IN_obs <- bind_rows(bedford1_obs_temps,sulivan_obs_temps,richmond_obs_temps,southport1_obs_temps,decatur1_obs_temps,wellscounty_obs_temps,southport2_obs_temps,rockville_obs_temps,bloomington_obs_temps,huntington1_obs_temps,evansville_obs_temps,allencounty_obs_temps,morristown_obs_temps,huntington2_obs_temps,rushville_obs_temps,huntington3_obs_temps,bloomingdale_obs_temps,decatur2_obs_temps,bedford2_obs_temps,warsaw_obs_temps,mountvernon_obs_temps)

#Merge the original state_csv dataframe and new state_obs dataframe 
IN_data <- merge.data.frame(IN_csv, IN_obs, by = 'date')

## find the tavg for state data 
IN_data$tavg <- ((IN_data$tmax + IN_data$tmin) / 2)

### save data as xlsx file 
library("xlsx")
write.xlsx(IN_data, file = "IN_data_total_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

### Part 2
#finding data from 6 months prior to date of collection
#ambient temps and precipitation may have affected plant/leaf growth and shape
lapply(df2, cat(citySwap5(x = city2, y = "_obs_6months <- meteo_pull_monitors(", j = city, 
                          z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '", h = p6mon, 
                          k = "', date_max = '", l = date, p ="')"), sep = "\n"))

bedford1_obs_6months <- meteo_pull_monitors(bedford_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1950-11-30', date_max = '1951-05-04')
sulivan_obs_6months <- meteo_pull_monitors(sulivan_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1973-10-31', date_max = '1974-04-17')
richmond_obs_6months <- meteo_pull_monitors(richmond_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1972-10-31', date_max = '1973-04-20')
monroecounty_obs_6months <- meteo_pull_monitors(monroecounty_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1959-10-31', date_max = '1960-04-27')
southport1_obs_6months <- meteo_pull_monitors(southport_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1934-10-31', date_max = '1935-04-27')
decatur1_obs_6months <- meteo_pull_monitors(decatur_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1965-11-30', date_max = '1966-05-01')
wellscounty_obs_6months <- meteo_pull_monitors(wellscounty_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1899-11-30', date_max = '1900-05-13')
southport2_obs_6months <- meteo_pull_monitors(southport_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1934-10-31', date_max = '1935-04-27')
rockville_obs_6months <- meteo_pull_monitors(rockville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1980-04-30', date_max = '1980-10-25')
bloomington_obs_6months <- meteo_pull_monitors(bloomington_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1960-10-31', date_max = '1961-04-23')
huntington1_obs_6months <- meteo_pull_monitors(huntington_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1936-11-30', date_max = '1937-05-08')
evansville_obs_6months <- meteo_pull_monitors(evansville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1940-10-31', date_max = '1941-04-12')
allencounty_obs_6months <- meteo_pull_monitors(allencounty_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1941-11-30', date_max = '1942-05-03')
morristown_obs_6months <- meteo_pull_monitors(morristown_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1912-11-30', date_max = '1913-05-07')
huntington2_obs_6months <- meteo_pull_monitors(huntington_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1945-10-31', date_max = '1946-04-28')
rushville_obs_6months <- meteo_pull_monitors(rushville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1978-11-30', date_max = '1979-05-11')
huntington3_obs_6months <- meteo_pull_monitors(huntington_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1968-12-31', date_max = '1969-06-26')
bloomingdale_obs_6months <- meteo_pull_monitors(bloomingdale_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1929-03-31', date_max = '1929-09-20')
decatur2_obs_6months <- meteo_pull_monitors(decatur_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1904-10-31', date_max = '1905-04-30')
bedford2_obs_6months <- meteo_pull_monitors(bedford_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1932-10-31', date_max = '1933-04-17')
warsaw_obs_6months <- meteo_pull_monitors(warsaw_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1931-11-30', date_max = '1932-05-17')
mountvernon_obs_6months <- meteo_pull_monitors(mountvernon_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2020-10-31', date_max = '2021-04-11')

## make a dataframe of the mean of tmax, tmin, prcp of the previous 6 months 
lapply(df3, cat(citySwap2(x = city2,  y = "_6months", z = " <- data.frame(IN_csv$id[", l= lennumber, k = "], (mean(", e = "_obs_6months$prcp, na.rm=TRUE)), (mean(", f = "_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(", g = "_obs_6months$tmin, na.rm=TRUE))) "), sep = "\n"))

bedford1_6months <- data.frame(IN_csv$id[1], (mean(bedford1_obs_6months$prcp, na.rm=TRUE)), (mean(bedford1_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(bedford1_obs_6months$tmin, na.rm=TRUE))) 
sulivan_6months <- data.frame(IN_csv$id[2], (mean(sulivan_obs_6months$prcp, na.rm=TRUE)), (mean(sulivan_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(sulivan_obs_6months$tmin, na.rm=TRUE))) 
richmond_6months <- data.frame(IN_csv$id[3], (mean(richmond_obs_6months$prcp, na.rm=TRUE)), (mean(richmond_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(richmond_obs_6months$tmin, na.rm=TRUE))) 
monroecounty_6months <- data.frame(IN_csv$id[4], (mean(monroecounty_obs_6months$prcp, na.rm=TRUE)), (mean(monroecounty_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(monroecounty_obs_6months$tmin, na.rm=TRUE))) 
southport1_6months <- data.frame(IN_csv$id[5], (mean(southport1_obs_6months$prcp, na.rm=TRUE)), (mean(southport1_obs_6months$tmax, na.rm=TRUE)), 
                                 (mean(southport1_obs_6months$tmin, na.rm=TRUE))) 
decatur1_6months <- data.frame(IN_csv$id[6], (mean(decatur1_obs_6months$prcp, na.rm=TRUE)), (mean(decatur1_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(decatur1_obs_6months$tmin, na.rm=TRUE))) 
wellscounty_6months <- data.frame(IN_csv$id[7], (mean(wellscounty_obs_6months$prcp, na.rm=TRUE)), (mean(wellscounty_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(wellscounty_obs_6months$tmin, na.rm=TRUE))) 
southport2_6months <- data.frame(IN_csv$id[8], (mean(southport2_obs_6months$prcp, na.rm=TRUE)), (mean(southport2_obs_6months$tmax, na.rm=TRUE)), 
                                 (mean(southport2_obs_6months$tmin, na.rm=TRUE))) 
rockville_6months <- data.frame(IN_csv$id[9], (mean(rockville_obs_6months$prcp, na.rm=TRUE)), (mean(rockville_obs_6months$tmax, na.rm=TRUE)), 
                                (mean(rockville_obs_6months$tmin, na.rm=TRUE))) 
bloomington_6months <- data.frame(IN_csv$id[10], (mean(bloomington_obs_6months$prcp, na.rm=TRUE)), (mean(bloomington_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(bloomington_obs_6months$tmin, na.rm=TRUE))) 
huntington1_6months <- data.frame(IN_csv$id[11], (mean(huntington1_obs_6months$prcp, na.rm=TRUE)), (mean(huntington1_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(huntington1_obs_6months$tmin, na.rm=TRUE))) 
evansville_6months <- data.frame(IN_csv$id[12], (mean(evansville_obs_6months$prcp, na.rm=TRUE)), (mean(evansville_obs_6months$tmax, na.rm=TRUE)), 
                                 (mean(evansville_obs_6months$tmin, na.rm=TRUE))) 
allencounty_6months <- data.frame(IN_csv$id[13], (mean(allencounty_obs_6months$prcp, na.rm=TRUE)), (mean(allencounty_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(allencounty_obs_6months$tmin, na.rm=TRUE))) 
morristown_6months <- data.frame(IN_csv$id[14], (mean(morristown_obs_6months$prcp, na.rm=TRUE)), (mean(morristown_obs_6months$tmax, na.rm=TRUE)), 
                                 (mean(morristown_obs_6months$tmin, na.rm=TRUE))) 
huntington2_6months <- data.frame(IN_csv$id[15], (mean(huntington2_obs_6months$prcp, na.rm=TRUE)), (mean(huntington2_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(huntington2_obs_6months$tmin, na.rm=TRUE))) 
rushville_6months <- data.frame(IN_csv$id[16], (mean(rushville_obs_6months$prcp, na.rm=TRUE)), (mean(rushville_obs_6months$tmax, na.rm=TRUE)), 
                                (mean(rushville_obs_6months$tmin, na.rm=TRUE))) 
huntington3_6months <- data.frame(IN_csv$id[17], (mean(huntington3_obs_6months$prcp, na.rm=TRUE)), (mean(huntington3_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(huntington3_obs_6months$tmin, na.rm=TRUE))) 
bloomingdale_6months <- data.frame(IN_csv$id[18], (mean(bloomingdale_obs_6months$prcp, na.rm=TRUE)), (mean(bloomingdale_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(bloomingdale_obs_6months$tmin, na.rm=TRUE))) 
decatur2_6months <- data.frame(IN_csv$id[19], (mean(decatur2_obs_6months$prcp, na.rm=TRUE)), (mean(decatur2_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(decatur2_obs_6months$tmin, na.rm=TRUE))) 
bedford2_6months <- data.frame(IN_csv$id[20], (mean(bedford2_obs_6months$prcp, na.rm=TRUE)), (mean(bedford2_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(bedford2_obs_6months$tmin, na.rm=TRUE))) 
warsaw_6months <- data.frame(IN_csv$id[21], (mean(warsaw_obs_6months$prcp, na.rm=TRUE)), (mean(warsaw_obs_6months$tmax, na.rm=TRUE)), 
                             (mean(warsaw_obs_6months$tmin, na.rm=TRUE))) 
mountvernon_6months <- data.frame(IN_csv$id[22], (mean(mountvernon_obs_6months$prcp, na.rm=TRUE)), (mean(mountvernon_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(mountvernon_obs_6months$tmin, na.rm=TRUE)))

#rename col.names 
cat(citySwap3(city2, y = "colnames(", z = "_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')"), sep = "\n")

colnames(bedford1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(sulivan_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(richmond_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(monroecounty_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(southport1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(decatur1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(wellscounty_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(southport2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(rockville_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(bloomington_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(huntington1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(evansville_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(allencounty_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(morristown_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(huntington2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(rushville_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(huntington3_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(bloomingdale_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(decatur2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(bedford2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(warsaw_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(mountvernon_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')

## combine previous 6 months data using bind_rows
cat(citySwap7(city2, y = "_6months", z = ","), sep = "")
IN_6months <- bind_rows(bedford1_6months,sulivan_6months,richmond_6months,monroecounty_6months,southport1_6months,decatur1_6months,wellscounty_6months,southport2_6months,rockville_6months,bloomington_6months,huntington1_6months,evansville_6months,allencounty_6months,morristown_6months,huntington2_6months,rushville_6months,huntington3_6months,bloomingdale_6months,decatur2_6months,bedford2_6months,warsaw_6months,mountvernon_6months)

### save data as xlsx file 
write.xlsx(IN_6months, file = "IN_6months_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


### Part 3
#find data from previous year from date of collection 
lapply(df4, cat(citySwap5(x = city2, y = "_obs_year <- meteo_pull_monitors(", j = city, 
                          z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '", h= pyear, 
                          k = "', date_max = '", l = date, p ="')"), sep = "\n"))

bedford1_obs_year <- meteo_pull_monitors(bedford_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1950-05-04', date_max = '1951-05-04')
sulivan_obs_year <- meteo_pull_monitors(sulivan_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1973-04-17', date_max = '1974-04-17')
richmond_obs_year <- meteo_pull_monitors(richmond_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1972-04-20', date_max = '1973-04-20')
monroecounty_obs_year <- meteo_pull_monitors(monroecounty_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1959-04-28', date_max = '1960-04-27')
southport1_obs_year <- meteo_pull_monitors(southport_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1934-04-27', date_max = '1935-04-27')
decatur1_obs_year <- meteo_pull_monitors(decatur_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1965-05-01', date_max = '1966-05-01')
wellscounty_obs_year <- meteo_pull_monitors(wellscounty_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1899-05-13', date_max = '1900-05-13')
southport2_obs_year <- meteo_pull_monitors(southport_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1934-04-27', date_max = '1935-04-27')
rockville_obs_year <- meteo_pull_monitors(rockville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1979-10-26', date_max = '1980-10-25')
bloomington_obs_year <- meteo_pull_monitors(bloomington_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1960-04-23', date_max = '1961-04-23')
huntington1_obs_year <- meteo_pull_monitors(huntington_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1936-05-08', date_max = '1937-05-08')
evansville_obs_year <- meteo_pull_monitors(evansville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1940-04-12', date_max = '1941-04-12')
allencounty_obs_year <- meteo_pull_monitors(allencounty_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1941-05-03', date_max = '1942-05-03')
morristown_obs_year <- meteo_pull_monitors(morristown_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1912-05-07', date_max = '1913-05-07')
huntington2_obs_year <- meteo_pull_monitors(huntington_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1945-04-28', date_max = '1946-04-28')
rushville_obs_year <- meteo_pull_monitors(rushville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1978-05-11', date_max = '1979-05-11')
huntington3_obs_year <- meteo_pull_monitors(huntington_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1968-06-26', date_max = '1969-06-26')
bloomingdale_obs_year <- meteo_pull_monitors(bloomingdale_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1928-09-20', date_max = '1929-09-20')
decatur2_obs_year <- meteo_pull_monitors(decatur_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1904-04-30', date_max = '1905-04-30')
bedford2_obs_year <- meteo_pull_monitors(bedford_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1932-04-17', date_max = '1933-04-17')
warsaw_obs_year <- meteo_pull_monitors(warsaw_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1931-05-18', date_max = '1932-05-17')
mountvernon_obs_year <- meteo_pull_monitors(mountvernon_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2020-04-11', date_max = '2021-04-11')

## make a data frame of the mean of tmax, tmin, prcp of the previous year before the date of collection 
lapply(df3, cat(citySwap2(x = city2,  y = "_previousyear", z = " <- data.frame(IN_csv$id[", l= lennumber, 
                          k = "], (mean(", e = "_obs_year$prcp, na.rm=TRUE)), (mean(", 
                          f = "_obs_year$tmax, na.rm=TRUE)), (mean(", g = "_obs_year$tmin, na.rm=TRUE))) "), sep = "\n"))

bedford1_previousyear <- data.frame(IN_csv$id[1], (mean(bedford1_obs_year$prcp, na.rm=TRUE)), (mean(bedford1_obs_year$tmax, na.rm=TRUE)), (mean(bedford1_obs_year$tmin, na.rm=TRUE))) 
sulivan_previousyear <- data.frame(IN_csv$id[2], (mean(sulivan_obs_year$prcp, na.rm=TRUE)), (mean(sulivan_obs_year$tmax, na.rm=TRUE)), (mean(sulivan_obs_year$tmin, na.rm=TRUE))) 
richmond_previousyear <- data.frame(IN_csv$id[3], (mean(richmond_obs_year$prcp, na.rm=TRUE)), (mean(richmond_obs_year$tmax, na.rm=TRUE)), (mean(richmond_obs_year$tmin, na.rm=TRUE))) 
monroecounty_previousyear <- data.frame(IN_csv$id[4], (mean(monroecounty_obs_year$prcp, na.rm=TRUE)), (mean(monroecounty_obs_year$tmax, na.rm=TRUE)), (mean(monroecounty_obs_year$tmin, na.rm=TRUE))) 
southport1_previousyear <- data.frame(IN_csv$id[5], (mean(southport1_obs_year$prcp, na.rm=TRUE)), (mean(southport1_obs_year$tmax, na.rm=TRUE)), (mean(southport1_obs_year$tmin, na.rm=TRUE))) 
decatur1_previousyear <- data.frame(IN_csv$id[6], (mean(decatur1_obs_year$prcp, na.rm=TRUE)), (mean(decatur1_obs_year$tmax, na.rm=TRUE)), (mean(decatur1_obs_year$tmin, na.rm=TRUE))) 
wellscounty_previousyear <- data.frame(IN_csv$id[7], (mean(wellscounty_obs_year$prcp, na.rm=TRUE)), (mean(wellscounty_obs_year$tmax, na.rm=TRUE)), (mean(wellscounty_obs_year$tmin, na.rm=TRUE))) 
southport2_previousyear <- data.frame(IN_csv$id[8], (mean(southport2_obs_year$prcp, na.rm=TRUE)), (mean(southport2_obs_year$tmax, na.rm=TRUE)), (mean(southport2_obs_year$tmin, na.rm=TRUE))) 
rockville_previousyear <- data.frame(IN_csv$id[9], (mean(rockville_obs_year$prcp, na.rm=TRUE)), (mean(rockville_obs_year$tmax, na.rm=TRUE)), (mean(rockville_obs_year$tmin, na.rm=TRUE))) 
bloomington_previousyear <- data.frame(IN_csv$id[10], (mean(bloomington_obs_year$prcp, na.rm=TRUE)), (mean(bloomington_obs_year$tmax, na.rm=TRUE)), (mean(bloomington_obs_year$tmin, na.rm=TRUE))) 
huntington1_previousyear <- data.frame(IN_csv$id[11], (mean(huntington1_obs_year$prcp, na.rm=TRUE)), (mean(huntington1_obs_year$tmax, na.rm=TRUE)), (mean(huntington1_obs_year$tmin, na.rm=TRUE))) 
evansville_previousyear <- data.frame(IN_csv$id[12], (mean(evansville_obs_year$prcp, na.rm=TRUE)), (mean(evansville_obs_year$tmax, na.rm=TRUE)), (mean(evansville_obs_year$tmin, na.rm=TRUE))) 
allencounty_previousyear <- data.frame(IN_csv$id[13], (mean(allencounty_obs_year$prcp, na.rm=TRUE)), (mean(allencounty_obs_year$tmax, na.rm=TRUE)), (mean(allencounty_obs_year$tmin, na.rm=TRUE))) 
morristown_previousyear <- data.frame(IN_csv$id[14], (mean(morristown_obs_year$prcp, na.rm=TRUE)), (mean(morristown_obs_year$tmax, na.rm=TRUE)), (mean(morristown_obs_year$tmin, na.rm=TRUE))) 
huntington2_previousyear <- data.frame(IN_csv$id[15], (mean(huntington2_obs_year$prcp, na.rm=TRUE)), (mean(huntington2_obs_year$tmax, na.rm=TRUE)), (mean(huntington2_obs_year$tmin, na.rm=TRUE))) 
rushville_previousyear <- data.frame(IN_csv$id[16], (mean(rushville_obs_year$prcp, na.rm=TRUE)), (mean(rushville_obs_year$tmax, na.rm=TRUE)), (mean(rushville_obs_year$tmin, na.rm=TRUE))) 
huntington3_previousyear <- data.frame(IN_csv$id[17], (mean(huntington3_obs_year$prcp, na.rm=TRUE)), (mean(huntington3_obs_year$tmax, na.rm=TRUE)), (mean(huntington3_obs_year$tmin, na.rm=TRUE))) 
bloomingdale_previousyear <- data.frame(IN_csv$id[18], (mean(bloomingdale_obs_year$prcp, na.rm=TRUE)), (mean(bloomingdale_obs_year$tmax, na.rm=TRUE)), (mean(bloomingdale_obs_year$tmin, na.rm=TRUE))) 
decatur2_previousyear <- data.frame(IN_csv$id[19], (mean(decatur2_obs_year$prcp, na.rm=TRUE)), (mean(decatur2_obs_year$tmax, na.rm=TRUE)), (mean(decatur2_obs_year$tmin, na.rm=TRUE))) 
bedford2_previousyear <- data.frame(IN_csv$id[20], (mean(bedford2_obs_year$prcp, na.rm=TRUE)), (mean(bedford2_obs_year$tmax, na.rm=TRUE)), (mean(bedford2_obs_year$tmin, na.rm=TRUE))) 
warsaw_previousyear <- data.frame(IN_csv$id[21], (mean(warsaw_obs_year$prcp, na.rm=TRUE)), (mean(warsaw_obs_year$tmax, na.rm=TRUE)), (mean(warsaw_obs_year$tmin, na.rm=TRUE))) 
mountvernon_previousyear <- data.frame(IN_csv$id[22], (mean(mountvernon_obs_year$prcp, na.rm=TRUE)), (mean(mountvernon_obs_year$tmax, na.rm=TRUE)), (mean(mountvernon_obs_year$tmin, na.rm=TRUE)))

#rename col.names 
cat(citySwap3(city2, y = "colnames(", z = "_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')"), sep = "\n")

colnames(bedford1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(sulivan_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(richmond_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(monroecounty_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(southport1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(decatur1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(wellscounty_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(southport2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(rockville_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(bloomington_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(huntington1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(evansville_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(allencounty_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(morristown_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(huntington2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(rushville_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(huntington3_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(bloomingdale_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(decatur2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(bedford2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(warsaw_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(mountvernon_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')

## combine previous year before the date of collection data using bind_rows
cat(citySwap7(city2, y = "_previousyear", z = ","), sep = "")
IN_Pyear <- bind_rows(bedford1_previousyear,sulivan_previousyear,richmond_previousyear,monroecounty_previousyear,southport1_previousyear,decatur1_previousyear,wellscounty_previousyear,southport2_previousyear,rockville_previousyear,bloomington_previousyear,huntington1_previousyear,evansville_previousyear,allencounty_previousyear,morristown_previousyear,huntington2_previousyear,rushville_previousyear,huntington3_previousyear,bloomingdale_previousyear,decatur2_previousyear,bedford2_previousyear,warsaw_previousyear,mountvernon_previousyear)

# save data
write.xlsx(IN_Pyear, file = "IN_previous_year_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


### Part 4 
#Find data from current year of date of collection 
lapply(df5, cat(citySwap6(x = city2, y = "_obs_yearDOC <- meteo_pull_monitors(", j = city, 
                          z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '", l = year , 
                          k = "-01-01', date_max = '",  p ="-12-31')"), sep = "\n"))

bedford1_obs_yearDOC <- meteo_pull_monitors(bedford_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1951-01-01', date_max = '1951-12-31')
sulivan_obs_yearDOC <- meteo_pull_monitors(sulivan_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1974-01-01', date_max = '1974-12-31')
richmond_obs_yearDOC <- meteo_pull_monitors(richmond_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1973-01-01', date_max = '1973-12-31')
monroecounty_obs_yearDOC <- meteo_pull_monitors(monroecounty_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1960-01-01', date_max = '1960-12-31')
southport1_obs_yearDOC <- meteo_pull_monitors(southport_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1935-01-01', date_max = '1935-12-31')
decatur1_obs_yearDOC <- meteo_pull_monitors(decatur_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1966-01-01', date_max = '1966-12-31')
wellscounty_obs_yearDOC <- meteo_pull_monitors(wellscounty_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1900-01-01', date_max = '1900-12-31')
southport2_obs_yearDOC <- meteo_pull_monitors(southport_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1935-01-01', date_max = '1935-12-31')
rockville_obs_yearDOC <- meteo_pull_monitors(rockville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1980-01-01', date_max = '1980-12-31')
bloomington_obs_yearDOC <- meteo_pull_monitors(bloomington_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1961-01-01', date_max = '1961-12-31')
huntington1_obs_yearDOC <- meteo_pull_monitors(huntington_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1937-01-01', date_max = '1937-12-31')
evansville_obs_yearDOC <- meteo_pull_monitors(evansville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1941-01-01', date_max = '1941-12-31')
allencounty_obs_yearDOC <- meteo_pull_monitors(allencounty_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1942-01-01', date_max = '1942-12-31')
morristown_obs_yearDOC <- meteo_pull_monitors(morristown_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1913-01-01', date_max = '1913-12-31')
huntington2_obs_yearDOC <- meteo_pull_monitors(huntington_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1946-01-01', date_max = '1946-12-31')
rushville_obs_yearDOC <- meteo_pull_monitors(rushville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1979-01-01', date_max = '1979-12-31')
huntington3_obs_yearDOC <- meteo_pull_monitors(huntington_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1969-01-01', date_max = '1969-12-31')
bloomingdale_obs_yearDOC <- meteo_pull_monitors(bloomingdale_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1929-01-01', date_max = '1929-12-31')
decatur2_obs_yearDOC <- meteo_pull_monitors(decatur_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1905-01-01', date_max = '1905-12-31')
bedford2_obs_yearDOC <- meteo_pull_monitors(bedford_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1933-01-01', date_max = '1933-12-31')
warsaw_obs_yearDOC <- meteo_pull_monitors(warsaw_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1932-01-01', date_max = '1932-12-31')
mountvernon_obs_yearDOC <- meteo_pull_monitors(mountvernon_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2021-01-01', date_max = '2021-12-31')

## make a dataframe of the mean of tmax, tmin, current year of the date of collection
lapply(dflist_nodates, cat(citySwap2(x = city2,  y = "_DOC", z = " <- data.frame(IN_csv$id[", l= lennumber, k = "], (mean(", e = "_obs_yearDOC$prcp, na.rm=TRUE)), (mean(", f = "_obs_yearDOC$tmax, na.rm=TRUE)), (mean(", g = "_obs_yearDOC$tmin, na.rm=TRUE))) "), sep = "\n"))

bedford1_DOC <- data.frame(IN_csv$id[1], (mean(bedford1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(bedford1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(bedford1_obs_yearDOC$tmin, na.rm=TRUE))) 
sulivan_DOC <- data.frame(IN_csv$id[2], (mean(sulivan_obs_yearDOC$prcp, na.rm=TRUE)), (mean(sulivan_obs_yearDOC$tmax, na.rm=TRUE)), (mean(sulivan_obs_yearDOC$tmin, na.rm=TRUE))) 
richmond_DOC <- data.frame(IN_csv$id[3], (mean(richmond_obs_yearDOC$prcp, na.rm=TRUE)), (mean(richmond_obs_yearDOC$tmax, na.rm=TRUE)), (mean(richmond_obs_yearDOC$tmin, na.rm=TRUE))) 
monroecounty_DOC <- data.frame(IN_csv$id[4], (mean(monroecounty_obs_yearDOC$prcp, na.rm=TRUE)), (mean(monroecounty_obs_yearDOC$tmax, na.rm=TRUE)), (mean(monroecounty_obs_yearDOC$tmin, na.rm=TRUE))) 
southport1_DOC <- data.frame(IN_csv$id[5], (mean(southport1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(southport1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(southport1_obs_yearDOC$tmin, na.rm=TRUE))) 
decatur1_DOC <- data.frame(IN_csv$id[6], (mean(decatur1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(decatur1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(decatur1_obs_yearDOC$tmin, na.rm=TRUE))) 
wellscounty_DOC <- data.frame(IN_csv$id[7], (mean(wellscounty_obs_yearDOC$prcp, na.rm=TRUE)), (mean(wellscounty_obs_yearDOC$tmax, na.rm=TRUE)), (mean(wellscounty_obs_yearDOC$tmin, na.rm=TRUE))) 
southport2_DOC <- data.frame(IN_csv$id[8], (mean(southport2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(southport2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(southport2_obs_yearDOC$tmin, na.rm=TRUE))) 
rockville_DOC <- data.frame(IN_csv$id[9], (mean(rockville_obs_yearDOC$prcp, na.rm=TRUE)), (mean(rockville_obs_yearDOC$tmax, na.rm=TRUE)), (mean(rockville_obs_yearDOC$tmin, na.rm=TRUE))) 
bloomington_DOC <- data.frame(IN_csv$id[10], (mean(bloomington_obs_yearDOC$prcp, na.rm=TRUE)), (mean(bloomington_obs_yearDOC$tmax, na.rm=TRUE)), (mean(bloomington_obs_yearDOC$tmin, na.rm=TRUE))) 
huntington1_DOC <- data.frame(IN_csv$id[11], (mean(huntington1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(huntington1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(huntington1_obs_yearDOC$tmin, na.rm=TRUE))) 
evansville_DOC <- data.frame(IN_csv$id[12], (mean(evansville_obs_yearDOC$prcp, na.rm=TRUE)), (mean(evansville_obs_yearDOC$tmax, na.rm=TRUE)), (mean(evansville_obs_yearDOC$tmin, na.rm=TRUE))) 
allencounty_DOC <- data.frame(IN_csv$id[13], (mean(allencounty_obs_yearDOC$prcp, na.rm=TRUE)), (mean(allencounty_obs_yearDOC$tmax, na.rm=TRUE)), (mean(allencounty_obs_yearDOC$tmin, na.rm=TRUE))) 
morristown_DOC <- data.frame(IN_csv$id[14], (mean(morristown_obs_yearDOC$prcp, na.rm=TRUE)), (mean(morristown_obs_yearDOC$tmax, na.rm=TRUE)), (mean(morristown_obs_yearDOC$tmin, na.rm=TRUE))) 
huntington2_DOC <- data.frame(IN_csv$id[15], (mean(huntington2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(huntington2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(huntington2_obs_yearDOC$tmin, na.rm=TRUE))) 
rushville_DOC <- data.frame(IN_csv$id[16], (mean(rushville_obs_yearDOC$prcp, na.rm=TRUE)), (mean(rushville_obs_yearDOC$tmax, na.rm=TRUE)), (mean(rushville_obs_yearDOC$tmin, na.rm=TRUE))) 
huntington3_DOC <- data.frame(IN_csv$id[17], (mean(huntington3_obs_yearDOC$prcp, na.rm=TRUE)), (mean(huntington3_obs_yearDOC$tmax, na.rm=TRUE)), (mean(huntington3_obs_yearDOC$tmin, na.rm=TRUE))) 
bloomingdale_DOC <- data.frame(IN_csv$id[18], (mean(bloomingdale_obs_yearDOC$prcp, na.rm=TRUE)), (mean(bloomingdale_obs_yearDOC$tmax, na.rm=TRUE)), (mean(bloomingdale_obs_yearDOC$tmin, na.rm=TRUE))) 
decatur2_DOC <- data.frame(IN_csv$id[19], (mean(decatur2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(decatur2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(decatur2_obs_yearDOC$tmin, na.rm=TRUE))) 
bedford2_DOC <- data.frame(IN_csv$id[20], (mean(bedford2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(bedford2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(bedford2_obs_yearDOC$tmin, na.rm=TRUE))) 
warsaw_DOC <- data.frame(IN_csv$id[21], (mean(warsaw_obs_yearDOC$prcp, na.rm=TRUE)), (mean(warsaw_obs_yearDOC$tmax, na.rm=TRUE)), (mean(warsaw_obs_yearDOC$tmin, na.rm=TRUE))) 
mountvernon_DOC <- data.frame(IN_csv$id[22], (mean(mountvernon_obs_yearDOC$prcp, na.rm=TRUE)), (mean(mountvernon_obs_yearDOC$tmax, na.rm=TRUE)), (mean(mountvernon_obs_yearDOC$tmin, na.rm=TRUE))) 

#rename col.names 
cat(citySwap3(city2, y = "colnames(", z = "_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')"), sep = "\n")

colnames(bedford1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(sulivan_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(richmond_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(monroecounty_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(southport1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(decatur1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(wellscounty_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(southport2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(rockville_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(bloomington_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(huntington1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(evansville_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(allencounty_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(morristown_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(huntington2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(rushville_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(huntington3_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(bloomingdale_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(decatur2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(bedford2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(warsaw_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(mountvernon_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')

## combine current year of the date of collection data using bind_rows
cat(citySwap7(city2, y = "_DOC", z = ","), sep = "")
IN_yearDOC <- bind_rows(bedford1_DOC,sulivan_DOC,richmond_DOC,monroecounty_DOC,southport1_DOC,decatur1_DOC,wellscounty_DOC,southport2_DOC,rockville_DOC,bloomington_DOC,huntington1_DOC,evansville_DOC,allencounty_DOC,morristown_DOC,huntington2_DOC,rushville_DOC,huntington3_DOC,bloomingdale_DOC,decatur2_DOC,bedford2_DOC,warsaw_DOC,mountvernon_DOC)

# save data
write.xlsx(IN_yearDOC, file = "IN_year_DOC_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
