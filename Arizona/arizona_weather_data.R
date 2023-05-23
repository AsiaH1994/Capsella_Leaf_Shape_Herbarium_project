r#m(list=ls())
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
arizona_csv <- read.csv('arizona_cities.csv')
arizona_csv$month <- match(arizona_csv$month, month.name)
arizona_csv$date <- paste(arizona_csv$year, arizona_csv$month, arizona_csv$day, sep="-")
strptime(arizona_csv$date,format="%Y-%m-%d")
arizona_csv$date <- as.Date(arizona_csv$date)
colnames(arizona_csv) <- c("id", 'id2', 'city_county_state', 'latitude', 'longitude', 'elevation', 'month', 'day', 'year', 'date')
arizona_csv$previous6mon <- as.Date(as.yearmon(as.Date(arizona_csv$date)) -.5, frac = 1)
head(arizona_csv)

## to use lapply 
city <- arizona_csv$id
city2 <- arizona_csv$id2
date <- arizona_csv$date
pyear <- (arizona_csv$date - 365)
p6mon <- arizona_csv$previous6mon
year <- arizona_csv$year
lennumber <- (1:21) #number of samples

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

arizonanevada_stations <- stations %>% filter(state == c("AZ", "NV"))
write.csv2(arizonanevada_stations, "arizonanevada_stations.csv")

#after running the 1st time 
arizonanevada_stations <- read.csv("arizonanevada_stations.csv")


#find closests stations
arizona_clst <- meteo_nearby_stations(lat_lon_df = arizona_csv, station_data = arizonanevada_stations, radius = 50)

### separate all cities 
cat(citySwap(unique(city), y = " <- (arizona_clst$'", z = "')"), sep = "\n")

tucson <- (arizona_clst$'tucson')
crownking <- (arizona_clst$'crownking')
sunflower <- (arizona_clst$'sunflower')
tempe <- (arizona_clst$'tempe')
sedona <- (arizona_clst$'sedona')
cottonwood <- (arizona_clst$'cottonwood')
wheatfields <- (arizona_clst$'wheatfields')
roosevelt <- (arizona_clst$'roosevelt')
phoenix <- (arizona_clst$'phoenix')
queencreek <- (arizona_clst$'queencreek')
prescott <- (arizona_clst$'prescott')
cochise <- (arizona_clst$'cochise')
wikieup <- (arizona_clst$'wikieup')
sierraanchas <- (arizona_clst$'sierraanchas')

### separate all station id's (monitors) for each city
cat(citySwap(unique(city), y = "_monitors <- ", z = "$id"), sep = "\n")

tucson_monitors <- tucson$id
crownking_monitors <- crownking$id
sunflower_monitors <- sunflower$id
tempe_monitors <- tempe$id
sedona_monitors <- sedona$id
cottonwood_monitors <- cottonwood$id
wheatfields_monitors <- wheatfields$id
roosevelt_monitors <- roosevelt$id
phoenix_monitors <- phoenix$id
queencreek_monitors <- queencreek$id
prescott_monitors <- prescott$id
cochise_monitors <- cochise$id
wikieup_monitors <- wikieup$id
sierraanchas_monitors <- sierraanchas$id

### pull all observations for cities 
cat(citySwap(unique(city), y = "_obs <- meteo_pull_monitors(", z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')"), sep = "\n")

tucson_obs <- meteo_pull_monitors(tucson_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
crownking_obs <- meteo_pull_monitors(crownking_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
sunflower_obs <- meteo_pull_monitors(sunflower_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
tempe_obs <- meteo_pull_monitors(tempe_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
sedona_obs <- meteo_pull_monitors(sedona_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
cottonwood_obs <- meteo_pull_monitors(cottonwood_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
wheatfields_obs <- meteo_pull_monitors(wheatfields_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
roosevelt_obs <- meteo_pull_monitors(roosevelt_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
phoenix_obs <- meteo_pull_monitors(phoenix_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
queencreek_obs <- meteo_pull_monitors(queencreek_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
prescott_obs <- meteo_pull_monitors(prescott_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
cochise_obs <- meteo_pull_monitors(cochise_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
wikieup_obs <- meteo_pull_monitors(wikieup_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
sierraanchas_obs <- meteo_pull_monitors(sierraanchas_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')

### Part 1
### pull data by date and location - dates are coming from state_csv 
lapply(df1, cat(citySwap4.5(x = city2, y = "_obs_temps <- ", i = city, z = "_obs %>% filter(date == '", j = date , h = "')"), sep = "\n"))

tucson1_obs_temps <- tucson_obs %>% filter(date == '2018-04-04')
tucson2_obs_temps <- tucson_obs %>% filter(date == '2018-04-04')
tucson3_obs_temps <- tucson_obs %>% filter(date == '1993-03-05')
crownking_obs_temps <- crownking_obs %>% filter(date == '2002-12-31')
sunflower1_obs_temps <- sunflower_obs %>% filter(date == '1990-04-03')
tempe1_obs_temps <- tempe_obs %>% filter(date == '1932-02-09')
tempe2_obs_temps <- tempe_obs %>% filter(date == '1962-02-23')
sedona_obs_temps <- sedona_obs %>% filter(date == '1961-05-06')
cottonwood_obs_temps <- cottonwood_obs %>% filter(date == '2010-02-28')
wheatfields_obs_temps <- wheatfields_obs %>% filter(date == '1986-05-18')
roosevelt_obs_temps <- roosevelt_obs %>% filter(date == '2000-12-06')
phoenix1_obs_temps <- phoenix_obs %>% filter(date == '2014-02-01')
queencreek_obs_temps <- queencreek_obs %>% filter(date == '2001-02-16')
tempe3_obs_temps <- tempe_obs %>% filter(date == '2015-02-07')
prescott_obs_temps <- prescott_obs %>% filter(date == '2008-03-21')
cochise_obs_temps <- cochise_obs %>% filter(date == '2003-04-13')
wikieup_obs_temps <- wikieup_obs %>% filter(date == '1979-04-13')
phoenix2_obs_temps <- phoenix_obs %>% filter(date == '1979-03-09')
sunflower2_obs_temps <- sunflower_obs %>% filter(date == '1972-03-19')
tempe4_obs_temps <- tempe_obs %>% filter(date == '1951-01-16')
sierraanchas_obs_temps <- sierraanchas_obs %>% filter(date == '1978-03-21')

#combine into one data frame using bind_rows()
cat(citySwap7(city2, y = "_obs_temps", z = ","), sep = "")
arizona_obs <- bind_rows(tucson1_obs_temps,tucson2_obs_temps,tucson3_obs_temps,crownking_obs_temps,sunflower1_obs_temps,tempe1_obs_temps,tempe2_obs_temps,sedona_obs_temps,cottonwood_obs_temps,wheatfields_obs_temps,roosevelt_obs_temps,phoenix1_obs_temps,queencreek_obs_temps,tempe3_obs_temps,prescott_obs_temps,cochise_obs_temps,wikieup_obs_temps,phoenix2_obs_temps,sunflower2_obs_temps,tempe4_obs_temps,sierraanchas_obs_temps)

#Merge the original state_csv dataframe and new state_obs dataframe 
arizona_data <- merge.data.frame(arizona_csv, arizona_obs, by = 'date')

## find the tavg for state data 
arizona_data$tavg <- ((arizona_data$tmax + arizona_data$tmin) / 2)

### save data as xlsx file 
library("xlsx")
write.xlsx(arizona_data, file = "arizona_data_total_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

### Part 2
#finding data from 6 months prior to date of collection
#ambient temps and precipitation may have affected plant/leaf growth and shape
lapply(df2, cat(citySwap5(x = city2, y = "_obs_6months <- meteo_pull_monitors(", j = city, 
                          z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '", h = p6mon, 
                          k = "', date_max = '", l = date, p ="')"), sep = "\n"))

tucson1_obs_6months <- meteo_pull_monitors(tucson_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2017-10-31', date_max = '2018-04-04')
tucson2_obs_6months <- meteo_pull_monitors(tucson_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2017-10-31', date_max = '2018-04-04')
tucson3_obs_6months <- meteo_pull_monitors(tucson_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1992-09-30', date_max = '1993-03-05')
crownking_obs_6months <- meteo_pull_monitors(crownking_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2002-06-30', date_max = '2002-12-31')
sunflower1_obs_6months <- meteo_pull_monitors(sunflower_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1989-10-31', date_max = '1990-04-03')
tempe1_obs_6months <- meteo_pull_monitors(tempe_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1931-08-31', date_max = '1932-02-09')
tempe2_obs_6months <- meteo_pull_monitors(tempe_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1961-08-31', date_max = '1962-02-23')
sedona_obs_6months <- meteo_pull_monitors(sedona_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1960-11-30', date_max = '1961-05-06')
cottonwood_obs_6months <- meteo_pull_monitors(cottonwood_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2009-08-31', date_max = '2010-02-28')
wheatfields_obs_6months <- meteo_pull_monitors(wheatfields_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1985-11-30', date_max = '1986-05-18')
roosevelt_obs_6months <- meteo_pull_monitors(roosevelt_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2000-06-30', date_max = '2000-12-06')
phoenix1_obs_6months <- meteo_pull_monitors(phoenix_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2013-08-31', date_max = '2014-02-01')
queencreek_obs_6months <- meteo_pull_monitors(queencreek_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2000-08-31', date_max = '2001-02-16')
tempe3_obs_6months <- meteo_pull_monitors(tempe_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2014-08-31', date_max = '2015-02-07')
prescott_obs_6months <- meteo_pull_monitors(prescott_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2007-09-30', date_max = '2008-03-21')
cochise_obs_6months <- meteo_pull_monitors(cochise_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2002-10-31', date_max = '2003-04-13')
wikieup_obs_6months <- meteo_pull_monitors(wikieup_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1978-10-31', date_max = '1979-04-13')
phoenix2_obs_6months <- meteo_pull_monitors(phoenix_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1978-09-30', date_max = '1979-03-09')
sunflower2_obs_6months <- meteo_pull_monitors(sunflower_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1971-09-30', date_max = '1972-03-19')
tempe4_obs_6months <- meteo_pull_monitors(tempe_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1950-07-31', date_max = '1951-01-16')
sierraanchas_obs_6months <- meteo_pull_monitors(sierraanchas_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1977-09-30', date_max = '1978-03-21')

## make a dataframe of the mean of tmax, tmin, prcp of the previous 6 months 
lapply(df3, cat(citySwap2(x = city2,  y = "_6months", z = " <- data.frame(arizona_csv$id[", l= lennumber, k = "], (mean(", e = "_obs_6months$prcp, na.rm=TRUE)), (mean(", f = "_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(", g = "_obs_6months$tmin, na.rm=TRUE))) "), sep = "\n"))

tucson1_6months <- data.frame(arizona_csv$id[1], (mean(tucson1_obs_6months$prcp, na.rm=TRUE)), (mean(tucson1_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(tucson1_obs_6months$tmin, na.rm=TRUE))) 
tucson2_6months <- data.frame(arizona_csv$id[2], (mean(tucson2_obs_6months$prcp, na.rm=TRUE)), (mean(tucson2_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(tucson2_obs_6months$tmin, na.rm=TRUE))) 
tucson3_6months <- data.frame(arizona_csv$id[3], (mean(tucson3_obs_6months$prcp, na.rm=TRUE)), (mean(tucson3_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(tucson3_obs_6months$tmin, na.rm=TRUE))) 
crownking_6months <- data.frame(arizona_csv$id[4], (mean(crownking_obs_6months$prcp, na.rm=TRUE)), (mean(crownking_obs_6months$tmax, na.rm=TRUE)), 
                                (mean(crownking_obs_6months$tmin, na.rm=TRUE))) 
sunflower1_6months <- data.frame(arizona_csv$id[5], (mean(sunflower1_obs_6months$prcp, na.rm=TRUE)), (mean(sunflower1_obs_6months$tmax, na.rm=TRUE)), 
                                 (mean(sunflower1_obs_6months$tmin, na.rm=TRUE))) 
tempe1_6months <- data.frame(arizona_csv$id[6], (mean(tempe1_obs_6months$prcp, na.rm=TRUE)), (mean(tempe1_obs_6months$tmax, na.rm=TRUE)), 
                             (mean(tempe1_obs_6months$tmin, na.rm=TRUE))) 
tempe2_6months <- data.frame(arizona_csv$id[7], (mean(tempe2_obs_6months$prcp, na.rm=TRUE)), (mean(tempe2_obs_6months$tmax, na.rm=TRUE)), 
                             (mean(tempe2_obs_6months$tmin, na.rm=TRUE))) 
sedona_6months <- data.frame(arizona_csv$id[8], (mean(sedona_obs_6months$prcp, na.rm=TRUE)), (mean(sedona_obs_6months$tmax, na.rm=TRUE)), 
                             (mean(sedona_obs_6months$tmin, na.rm=TRUE))) 
cottonwood_6months <- data.frame(arizona_csv$id[9], (mean(cottonwood_obs_6months$prcp, na.rm=TRUE)), (mean(cottonwood_obs_6months$tmax, na.rm=TRUE)), 
                                 (mean(cottonwood_obs_6months$tmin, na.rm=TRUE))) 
wheatfields_6months <- data.frame(arizona_csv$id[10], (mean(wheatfields_obs_6months$prcp, na.rm=TRUE)), (mean(wheatfields_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(wheatfields_obs_6months$tmin, na.rm=TRUE))) 
roosevelt_6months <- data.frame(arizona_csv$id[11], (mean(roosevelt_obs_6months$prcp, na.rm=TRUE)), (mean(roosevelt_obs_6months$tmax, na.rm=TRUE)), 
                                (mean(roosevelt_obs_6months$tmin, na.rm=TRUE))) 
phoenix1_6months <- data.frame(arizona_csv$id[12], (mean(phoenix1_obs_6months$prcp, na.rm=TRUE)), (mean(phoenix1_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(phoenix1_obs_6months$tmin, na.rm=TRUE))) 
queencreek_6months <- data.frame(arizona_csv$id[13], (mean(queencreek_obs_6months$prcp, na.rm=TRUE)), (mean(queencreek_obs_6months$tmax, na.rm=TRUE)), 
                                 (mean(queencreek_obs_6months$tmin, na.rm=TRUE))) 
tempe3_6months <- data.frame(arizona_csv$id[14], (mean(tempe3_obs_6months$prcp, na.rm=TRUE)), (mean(tempe3_obs_6months$tmax, na.rm=TRUE)), 
                             (mean(tempe3_obs_6months$tmin, na.rm=TRUE))) 
prescott_6months <- data.frame(arizona_csv$id[15], (mean(prescott_obs_6months$prcp, na.rm=TRUE)), (mean(prescott_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(prescott_obs_6months$tmin, na.rm=TRUE))) 
cochise_6months <- data.frame(arizona_csv$id[16], (mean(cochise_obs_6months$prcp, na.rm=TRUE)), (mean(cochise_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(cochise_obs_6months$tmin, na.rm=TRUE))) 
wikieup_6months <- data.frame(arizona_csv$id[17], (mean(wikieup_obs_6months$prcp, na.rm=TRUE)), (mean(wikieup_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(wikieup_obs_6months$tmin, na.rm=TRUE))) 
phoenix2_6months <- data.frame(arizona_csv$id[18], (mean(phoenix2_obs_6months$prcp, na.rm=TRUE)), (mean(phoenix2_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(phoenix2_obs_6months$tmin, na.rm=TRUE))) 
sunflower2_6months <- data.frame(arizona_csv$id[19], (mean(sunflower2_obs_6months$prcp, na.rm=TRUE)), (mean(sunflower2_obs_6months$tmax, na.rm=TRUE)), 
                                 (mean(sunflower2_obs_6months$tmin, na.rm=TRUE))) 
tempe4_6months <- data.frame(arizona_csv$id[20], (mean(tempe4_obs_6months$prcp, na.rm=TRUE)), (mean(tempe4_obs_6months$tmax, na.rm=TRUE)), 
                             (mean(tempe4_obs_6months$tmin, na.rm=TRUE))) 
sierraanchas_6months <- data.frame(arizona_csv$id[21], (mean(sierraanchas_obs_6months$prcp, na.rm=TRUE)), (mean(sierraanchas_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(sierraanchas_obs_6months$tmin, na.rm=TRUE)))

#rename col.names 
cat(citySwap3(city2, y = "colnames(", z = "_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')"), sep = "\n")

colnames(tucson1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tucson2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tucson3_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(crownking_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(sunflower1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tempe1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tempe2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(sedona_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(cottonwood_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(wheatfields_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(roosevelt_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(phoenix1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(queencreek_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tempe3_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(prescott_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(cochise_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(wikieup_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(phoenix2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(sunflower2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tempe4_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(sierraanchas_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')

## combine previous 6 months data using bind_rows
cat(citySwap7(city2, y = "_6months", z = ","), sep = "")
arizona_6months <- bind_rows(tucson1_6months,tucson2_6months,tucson3_6months,crownking_6months,sunflower1_6months,tempe1_6months,tempe2_6months,sedona_6months,cottonwood_6months,wheatfields_6months,roosevelt_6months,phoenix1_6months,queencreek_6months,tempe3_6months,prescott_6months,cochise_6months,wikieup_6months,phoenix2_6months,sunflower2_6months,tempe4_6months,sierraanchas_6months)

### save data as xlsx file 
write.xlsx(arizona_6months, file = "arizona_6months_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


### Part 3
#find data from previous year from date of collection 
lapply(df4, cat(citySwap5(x = city2, y = "_obs_year <- meteo_pull_monitors(", j = city, 
                          z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '", h= pyear, 
                          k = "', date_max = '", l = date, p ="')"), sep = "\n"))

tucson1_obs_year <- meteo_pull_monitors(tucson_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2017-04-04', date_max = '2018-04-04')
tucson2_obs_year <- meteo_pull_monitors(tucson_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2017-04-04', date_max = '2018-04-04')
tucson3_obs_year <- meteo_pull_monitors(tucson_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1992-03-05', date_max = '1993-03-05')
crownking_obs_year <- meteo_pull_monitors(crownking_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2001-12-31', date_max = '2002-12-31')
sunflower1_obs_year <- meteo_pull_monitors(sunflower_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1989-04-03', date_max = '1990-04-03')
tempe1_obs_year <- meteo_pull_monitors(tempe_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1931-02-09', date_max = '1932-02-09')
tempe2_obs_year <- meteo_pull_monitors(tempe_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1961-02-23', date_max = '1962-02-23')
sedona_obs_year <- meteo_pull_monitors(sedona_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1960-05-06', date_max = '1961-05-06')
cottonwood_obs_year <- meteo_pull_monitors(cottonwood_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2009-02-28', date_max = '2010-02-28')
wheatfields_obs_year <- meteo_pull_monitors(wheatfields_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1985-05-18', date_max = '1986-05-18')
roosevelt_obs_year <- meteo_pull_monitors(roosevelt_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1999-12-07', date_max = '2000-12-06')
phoenix1_obs_year <- meteo_pull_monitors(phoenix_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2013-02-01', date_max = '2014-02-01')
queencreek_obs_year <- meteo_pull_monitors(queencreek_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2000-02-17', date_max = '2001-02-16')
tempe3_obs_year <- meteo_pull_monitors(tempe_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2014-02-07', date_max = '2015-02-07')
prescott_obs_year <- meteo_pull_monitors(prescott_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2007-03-22', date_max = '2008-03-21')
cochise_obs_year <- meteo_pull_monitors(cochise_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2002-04-13', date_max = '2003-04-13')
wikieup_obs_year <- meteo_pull_monitors(wikieup_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1978-04-13', date_max = '1979-04-13')
phoenix2_obs_year <- meteo_pull_monitors(phoenix_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1978-03-09', date_max = '1979-03-09')
sunflower2_obs_year <- meteo_pull_monitors(sunflower_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1971-03-20', date_max = '1972-03-19')
tempe4_obs_year <- meteo_pull_monitors(tempe_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1950-01-16', date_max = '1951-01-16')
sierraanchas_obs_year <- meteo_pull_monitors(sierraanchas_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1977-03-21', date_max = '1978-03-21')

## make a data frame of the mean of tmax, tmin, prcp of the previous year before the date of collection 
lapply(df3, cat(citySwap2(x = city2,  y = "_previousyear", z = " <- data.frame(arizona_csv$id[", l= lennumber, 
                          k = "], (mean(", e = "_obs_year$prcp, na.rm=TRUE)), (mean(", 
                          f = "_obs_year$tmax, na.rm=TRUE)), (mean(", g = "_obs_year$tmin, na.rm=TRUE))) "), sep = "\n"))

tucson1_previousyear <- data.frame(arizona_csv$id[1], (mean(tucson1_obs_year$prcp, na.rm=TRUE)), (mean(tucson1_obs_year$tmax, na.rm=TRUE)), (mean(tucson1_obs_year$tmin, na.rm=TRUE))) 
tucson2_previousyear <- data.frame(arizona_csv$id[2], (mean(tucson2_obs_year$prcp, na.rm=TRUE)), (mean(tucson2_obs_year$tmax, na.rm=TRUE)), (mean(tucson2_obs_year$tmin, na.rm=TRUE))) 
tucson3_previousyear <- data.frame(arizona_csv$id[3], (mean(tucson3_obs_year$prcp, na.rm=TRUE)), (mean(tucson3_obs_year$tmax, na.rm=TRUE)), (mean(tucson3_obs_year$tmin, na.rm=TRUE))) 
crownking_previousyear <- data.frame(arizona_csv$id[4], (mean(crownking_obs_year$prcp, na.rm=TRUE)), (mean(crownking_obs_year$tmax, na.rm=TRUE)), (mean(crownking_obs_year$tmin, na.rm=TRUE))) 
sunflower1_previousyear <- data.frame(arizona_csv$id[5], (mean(sunflower1_obs_year$prcp, na.rm=TRUE)), (mean(sunflower1_obs_year$tmax, na.rm=TRUE)), (mean(sunflower1_obs_year$tmin, na.rm=TRUE))) 
tempe1_previousyear <- data.frame(arizona_csv$id[6], (mean(tempe1_obs_year$prcp, na.rm=TRUE)), (mean(tempe1_obs_year$tmax, na.rm=TRUE)), (mean(tempe1_obs_year$tmin, na.rm=TRUE))) 
tempe2_previousyear <- data.frame(arizona_csv$id[7], (mean(tempe2_obs_year$prcp, na.rm=TRUE)), (mean(tempe2_obs_year$tmax, na.rm=TRUE)), (mean(tempe2_obs_year$tmin, na.rm=TRUE))) 
sedona_previousyear <- data.frame(arizona_csv$id[8], (mean(sedona_obs_year$prcp, na.rm=TRUE)), (mean(sedona_obs_year$tmax, na.rm=TRUE)), (mean(sedona_obs_year$tmin, na.rm=TRUE))) 
cottonwood_previousyear <- data.frame(arizona_csv$id[9], (mean(cottonwood_obs_year$prcp, na.rm=TRUE)), (mean(cottonwood_obs_year$tmax, na.rm=TRUE)), (mean(cottonwood_obs_year$tmin, na.rm=TRUE))) 
wheatfields_previousyear <- data.frame(arizona_csv$id[10], (mean(wheatfields_obs_year$prcp, na.rm=TRUE)), (mean(wheatfields_obs_year$tmax, na.rm=TRUE)), (mean(wheatfields_obs_year$tmin, na.rm=TRUE))) 
roosevelt_previousyear <- data.frame(arizona_csv$id[11], (mean(roosevelt_obs_year$prcp, na.rm=TRUE)), (mean(roosevelt_obs_year$tmax, na.rm=TRUE)), (mean(roosevelt_obs_year$tmin, na.rm=TRUE))) 
phoenix1_previousyear <- data.frame(arizona_csv$id[12], (mean(phoenix1_obs_year$prcp, na.rm=TRUE)), (mean(phoenix1_obs_year$tmax, na.rm=TRUE)), (mean(phoenix1_obs_year$tmin, na.rm=TRUE))) 
queencreek_previousyear <- data.frame(arizona_csv$id[13], (mean(queencreek_obs_year$prcp, na.rm=TRUE)), (mean(queencreek_obs_year$tmax, na.rm=TRUE)), (mean(queencreek_obs_year$tmin, na.rm=TRUE))) 
tempe3_previousyear <- data.frame(arizona_csv$id[14], (mean(tempe3_obs_year$prcp, na.rm=TRUE)), (mean(tempe3_obs_year$tmax, na.rm=TRUE)), (mean(tempe3_obs_year$tmin, na.rm=TRUE))) 
prescott_previousyear <- data.frame(arizona_csv$id[15], (mean(prescott_obs_year$prcp, na.rm=TRUE)), (mean(prescott_obs_year$tmax, na.rm=TRUE)), (mean(prescott_obs_year$tmin, na.rm=TRUE))) 
cochise_previousyear <- data.frame(arizona_csv$id[16], (mean(cochise_obs_year$prcp, na.rm=TRUE)), (mean(cochise_obs_year$tmax, na.rm=TRUE)), (mean(cochise_obs_year$tmin, na.rm=TRUE))) 
wikieup_previousyear <- data.frame(arizona_csv$id[17], (mean(wikieup_obs_year$prcp, na.rm=TRUE)), (mean(wikieup_obs_year$tmax, na.rm=TRUE)), (mean(wikieup_obs_year$tmin, na.rm=TRUE))) 
phoenix2_previousyear <- data.frame(arizona_csv$id[18], (mean(phoenix2_obs_year$prcp, na.rm=TRUE)), (mean(phoenix2_obs_year$tmax, na.rm=TRUE)), (mean(phoenix2_obs_year$tmin, na.rm=TRUE))) 
sunflower2_previousyear <- data.frame(arizona_csv$id[19], (mean(sunflower2_obs_year$prcp, na.rm=TRUE)), (mean(sunflower2_obs_year$tmax, na.rm=TRUE)), (mean(sunflower2_obs_year$tmin, na.rm=TRUE))) 
tempe4_previousyear <- data.frame(arizona_csv$id[20], (mean(tempe4_obs_year$prcp, na.rm=TRUE)), (mean(tempe4_obs_year$tmax, na.rm=TRUE)), (mean(tempe4_obs_year$tmin, na.rm=TRUE))) 
sierraanchas_previousyear <- data.frame(arizona_csv$id[21], (mean(sierraanchas_obs_year$prcp, na.rm=TRUE)), (mean(sierraanchas_obs_year$tmax, na.rm=TRUE)), (mean(sierraanchas_obs_year$tmin, na.rm=TRUE)))

#rename col.names 
cat(citySwap3(city2, y = "colnames(", z = "_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')"), sep = "\n")

colnames(tucson1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tucson2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tucson3_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(crownking_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(sunflower1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tempe1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tempe2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(sedona_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(cottonwood_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(wheatfields_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(roosevelt_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(phoenix1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(queencreek_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tempe3_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(prescott_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(cochise_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(wikieup_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(phoenix2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(sunflower2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tempe4_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(sierraanchas_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')

## combine previous year before the date of collection data using bind_rows
cat(citySwap7(city2, y = "_previousyear", z = ","), sep = "")
arizona_Pyear <- bind_rows(tucson1_previousyear,tucson2_previousyear,tucson3_previousyear,crownking_previousyear,sunflower1_previousyear,tempe1_previousyear,tempe2_previousyear,sedona_previousyear,cottonwood_previousyear,wheatfields_previousyear,roosevelt_previousyear,phoenix1_previousyear,queencreek_previousyear,tempe3_previousyear,prescott_previousyear,cochise_previousyear,wikieup_previousyear,phoenix2_previousyear,sunflower2_previousyear,tempe4_previousyear,sierraanchas_previousyear)

# save data
write.xlsx(arizona_Pyear, file = "arizona_previous_year_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


### Part 4 
#Find data from current year of date of collection 
lapply(df5, cat(citySwap6(x = city2, y = "_obs_yearDOC <- meteo_pull_monitors(", j = city, 
                          z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '", l = year , 
                          k = "-01-01', date_max = '",  p ="-12-31')"), sep = "\n"))

tucson1_obs_yearDOC <- meteo_pull_monitors(tucson_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2018-01-01', date_max = '2018-12-31')
tucson2_obs_yearDOC <- meteo_pull_monitors(tucson_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2018-01-01', date_max = '2018-12-31')
tucson3_obs_yearDOC <- meteo_pull_monitors(tucson_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1993-01-01', date_max = '1993-12-31')
crownking_obs_yearDOC <- meteo_pull_monitors(crownking_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2002-01-01', date_max = '2002-12-31')
sunflower1_obs_yearDOC <- meteo_pull_monitors(sunflower_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1990-01-01', date_max = '1990-12-31')
tempe1_obs_yearDOC <- meteo_pull_monitors(tempe_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1932-01-01', date_max = '1932-12-31')
tempe2_obs_yearDOC <- meteo_pull_monitors(tempe_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1962-01-01', date_max = '1962-12-31')
sedona_obs_yearDOC <- meteo_pull_monitors(sedona_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1961-01-01', date_max = '1961-12-31')
cottonwood_obs_yearDOC <- meteo_pull_monitors(cottonwood_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2010-01-01', date_max = '2010-12-31')
wheatfields_obs_yearDOC <- meteo_pull_monitors(wheatfields_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1986-01-01', date_max = '1986-12-31')
roosevelt_obs_yearDOC <- meteo_pull_monitors(roosevelt_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2000-01-01', date_max = '2000-12-31')
phoenix1_obs_yearDOC <- meteo_pull_monitors(phoenix_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2014-01-01', date_max = '2014-12-31')
queencreek_obs_yearDOC <- meteo_pull_monitors(queencreek_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2001-01-01', date_max = '2001-12-31')
tempe3_obs_yearDOC <- meteo_pull_monitors(tempe_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2015-01-01', date_max = '2015-12-31')
prescott_obs_yearDOC <- meteo_pull_monitors(prescott_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2008-01-01', date_max = '2008-12-31')
cochise_obs_yearDOC <- meteo_pull_monitors(cochise_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2003-01-01', date_max = '2003-12-31')
wikieup_obs_yearDOC <- meteo_pull_monitors(wikieup_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1979-01-01', date_max = '1979-12-31')
phoenix2_obs_yearDOC <- meteo_pull_monitors(phoenix_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1979-01-01', date_max = '1979-12-31')
sunflower2_obs_yearDOC <- meteo_pull_monitors(sunflower_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1972-01-01', date_max = '1972-12-31')
tempe4_obs_yearDOC <- meteo_pull_monitors(tempe_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1951-01-01', date_max = '1951-12-31')
sierraanchas_obs_yearDOC <- meteo_pull_monitors(sierraanchas_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1978-01-01', date_max = '1978-12-31')

## make a dataframe of the mean of tmax, tmin, current year of the date of collection
lapply(dflist_nodates, cat(citySwap2(x = city2,  y = "_DOC", z = " <- data.frame(arizona_csv$id[", l= lennumber, k = "], (mean(", e = "_obs_yearDOC$prcp, na.rm=TRUE)), (mean(", f = "_obs_yearDOC$tmax, na.rm=TRUE)), (mean(", g = "_obs_yearDOC$tmin, na.rm=TRUE))) "), sep = "\n"))

tucson1_DOC <- data.frame(arizona_csv$id[1], (mean(tucson1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(tucson1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(tucson1_obs_yearDOC$tmin, na.rm=TRUE))) 
tucson2_DOC <- data.frame(arizona_csv$id[2], (mean(tucson2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(tucson2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(tucson2_obs_yearDOC$tmin, na.rm=TRUE))) 
tucson3_DOC <- data.frame(arizona_csv$id[3], (mean(tucson3_obs_yearDOC$prcp, na.rm=TRUE)), (mean(tucson3_obs_yearDOC$tmax, na.rm=TRUE)), (mean(tucson3_obs_yearDOC$tmin, na.rm=TRUE))) 
crownking_DOC <- data.frame(arizona_csv$id[4], (mean(crownking_obs_yearDOC$prcp, na.rm=TRUE)), (mean(crownking_obs_yearDOC$tmax, na.rm=TRUE)), (mean(crownking_obs_yearDOC$tmin, na.rm=TRUE))) 
sunflower1_DOC <- data.frame(arizona_csv$id[5], (mean(sunflower1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(sunflower1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(sunflower1_obs_yearDOC$tmin, na.rm=TRUE))) 
tempe1_DOC <- data.frame(arizona_csv$id[6], (mean(tempe1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(tempe1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(tempe1_obs_yearDOC$tmin, na.rm=TRUE))) 
tempe2_DOC <- data.frame(arizona_csv$id[7], (mean(tempe2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(tempe2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(tempe2_obs_yearDOC$tmin, na.rm=TRUE))) 
sedona_DOC <- data.frame(arizona_csv$id[8], (mean(sedona_obs_yearDOC$prcp, na.rm=TRUE)), (mean(sedona_obs_yearDOC$tmax, na.rm=TRUE)), (mean(sedona_obs_yearDOC$tmin, na.rm=TRUE))) 
cottonwood_DOC <- data.frame(arizona_csv$id[9], (mean(cottonwood_obs_yearDOC$prcp, na.rm=TRUE)), (mean(cottonwood_obs_yearDOC$tmax, na.rm=TRUE)), (mean(cottonwood_obs_yearDOC$tmin, na.rm=TRUE))) 
wheatfields_DOC <- data.frame(arizona_csv$id[10], (mean(wheatfields_obs_yearDOC$prcp, na.rm=TRUE)), (mean(wheatfields_obs_yearDOC$tmax, na.rm=TRUE)), (mean(wheatfields_obs_yearDOC$tmin, na.rm=TRUE))) 
roosevelt_DOC <- data.frame(arizona_csv$id[11], (mean(roosevelt_obs_yearDOC$prcp, na.rm=TRUE)), (mean(roosevelt_obs_yearDOC$tmax, na.rm=TRUE)), (mean(roosevelt_obs_yearDOC$tmin, na.rm=TRUE))) 
phoenix1_DOC <- data.frame(arizona_csv$id[12], (mean(phoenix1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(phoenix1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(phoenix1_obs_yearDOC$tmin, na.rm=TRUE))) 
queencreek_DOC <- data.frame(arizona_csv$id[13], (mean(queencreek_obs_yearDOC$prcp, na.rm=TRUE)), (mean(queencreek_obs_yearDOC$tmax, na.rm=TRUE)), (mean(queencreek_obs_yearDOC$tmin, na.rm=TRUE))) 
tempe3_DOC <- data.frame(arizona_csv$id[14], (mean(tempe3_obs_yearDOC$prcp, na.rm=TRUE)), (mean(tempe3_obs_yearDOC$tmax, na.rm=TRUE)), (mean(tempe3_obs_yearDOC$tmin, na.rm=TRUE))) 
prescott_DOC <- data.frame(arizona_csv$id[15], (mean(prescott_obs_yearDOC$prcp, na.rm=TRUE)), (mean(prescott_obs_yearDOC$tmax, na.rm=TRUE)), (mean(prescott_obs_yearDOC$tmin, na.rm=TRUE))) 
cochise_DOC <- data.frame(arizona_csv$id[16], (mean(cochise_obs_yearDOC$prcp, na.rm=TRUE)), (mean(cochise_obs_yearDOC$tmax, na.rm=TRUE)), (mean(cochise_obs_yearDOC$tmin, na.rm=TRUE))) 
wikieup_DOC <- data.frame(arizona_csv$id[17], (mean(wikieup_obs_yearDOC$prcp, na.rm=TRUE)), (mean(wikieup_obs_yearDOC$tmax, na.rm=TRUE)), (mean(wikieup_obs_yearDOC$tmin, na.rm=TRUE))) 
phoenix2_DOC <- data.frame(arizona_csv$id[18], (mean(phoenix2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(phoenix2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(phoenix2_obs_yearDOC$tmin, na.rm=TRUE))) 
sunflower2_DOC <- data.frame(arizona_csv$id[19], (mean(sunflower2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(sunflower2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(sunflower2_obs_yearDOC$tmin, na.rm=TRUE))) 
tempe4_DOC <- data.frame(arizona_csv$id[20], (mean(tempe4_obs_yearDOC$prcp, na.rm=TRUE)), (mean(tempe4_obs_yearDOC$tmax, na.rm=TRUE)), (mean(tempe4_obs_yearDOC$tmin, na.rm=TRUE))) 
sierraanchas_DOC <- data.frame(arizona_csv$id[21], (mean(sierraanchas_obs_yearDOC$prcp, na.rm=TRUE)), (mean(sierraanchas_obs_yearDOC$tmax, na.rm=TRUE)), (mean(sierraanchas_obs_yearDOC$tmin, na.rm=TRUE)))

#rename col.names 
cat(citySwap3(city2, y = "colnames(", z = "_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')"), sep = "\n")

colnames(tucson1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tucson2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tucson3_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(crownking_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(sunflower1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tempe1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tempe2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(sedona_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(cottonwood_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(wheatfields_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(roosevelt_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(phoenix1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(queencreek_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tempe3_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(prescott_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(cochise_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(wikieup_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(phoenix2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(sunflower2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tempe4_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(sierraanchas_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')

## combine current year of the date of collection data using bind_rows
cat(citySwap7(city2, y = "_DOC", z = ","), sep = "")
arizona_yearDOC <- bind_rows(tucson1_DOC,tucson2_DOC,tucson3_DOC,crownking_DOC,sunflower1_DOC,tempe1_DOC,tempe2_DOC,sedona_DOC,cottonwood_DOC,wheatfields_DOC,roosevelt_DOC,phoenix1_DOC,queencreek_DOC,tempe3_DOC,prescott_DOC,cochise_DOC,wikieup_DOC,phoenix2_DOC,sunflower2_DOC,tempe4_DOC,sierraanchas_DOC)

# save data
write.xlsx(arizona_yearDOC, file = "arizona_year_DOC_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
