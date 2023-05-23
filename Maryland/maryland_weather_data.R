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
maryland_csv <- read.csv('maryland_cities.csv')
maryland_csv$month <- match(maryland_csv$month, month.name)
maryland_csv$date <- paste(maryland_csv$year, maryland_csv$month, maryland_csv$day, sep="-")
strptime(maryland_csv$date,format="%Y-%m-%d")
maryland_csv$date <- as.Date(maryland_csv$date)
colnames(maryland_csv) <- c("id", 'id2', 'city_county_state', 'latitude', 'longitude', 'elevation', 'month', 'day', 'year', 'date')
maryland_csv$previous6mon <- as.Date(as.yearmon(as.Date(maryland_csv$date)) -.5, frac = 1)
head(maryland_csv)

## to use lapply 
city <- maryland_csv$id
city2 <- maryland_csv$id2
date <- maryland_csv$date
pyear <- (maryland_csv$date - 365)
p6mon <- maryland_csv$previous6mon
year <- maryland_csv$year
lennumber <- (1:37) #number of samples

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
east_coast_stations <- stations %>% filter(state == c("VA", "MD", "DE"))

#find closests stations
maryland_clst <- meteo_nearby_stations(lat_lon_df = maryland_csv, station_data = east_coast_stations, radius = 50)

### separate all cities 
cat(citySwap(unique(city), y = " <- (maryland_clst$'", z = "')"), sep = "\n")

phoenix <- (maryland_clst$'phoenix')
carney <- (maryland_clst$'carney')
accokeek <- (maryland_clst$'accokeek')
galestown <- (maryland_clst$'galestown')
easton <- (maryland_clst$'easton')
silverspring <- (maryland_clst$'silverspring')
cockeysville <- (maryland_clst$'cockeysville')
preston <- (maryland_clst$'preston')
snowhill <- (maryland_clst$'snowhill')
townson <- (maryland_clst$'townson')
waddellscorner <- (maryland_clst$'waddellscorner')
uppermarlboro <- (maryland_clst$'uppermarlboro')
harmony <- (maryland_clst$'harmony')
owingmills <- (maryland_clst$'owingmills')
eastnewmarket <- (maryland_clst$'eastnewmarket')
collegepark <- (maryland_clst$'collegepark')
reistertown <- (maryland_clst$'reistertown')
ashland <- (maryland_clst$'ashland')
mandelasprings <- (maryland_clst$'mandelasprings')
lilypons <- (maryland_clst$'lilypons')
newcomb <- (maryland_clst$'newcomb')
baltimorecounty <- (maryland_clst$'baltimorecounty')
timomium <- (maryland_clst$'timomium')
laplata <- (maryland_clst$'laplata')
rosedale <- (maryland_clst$'rosedale')
ellicottcity <- (maryland_clst$'ellicottcity')
thumont <- (maryland_clst$'thumont')
kennedyville <- (maryland_clst$'kennedyville')
northbesthesda <- (maryland_clst$'northbesthesda')
gaithersburg <- (maryland_clst$'gaithersburg')

### separate all station id's (monitors) for each city
cat(citySwap(unique(city), y = "_monitors <- ", z = "$id"), sep = "\n")

phoenix_monitors <- phoenix$id
carney_monitors <- carney$id
accokeek_monitors <- accokeek$id
galestown_monitors <- galestown$id
easton_monitors <- easton$id
silverspring_monitors <- silverspring$id
cockeysville_monitors <- cockeysville$id
preston_monitors <- preston$id
snowhill_monitors <- snowhill$id
townson_monitors <- townson$id
waddellscorner_monitors <- waddellscorner$id
uppermarlboro_monitors <- uppermarlboro$id
harmony_monitors <- harmony$id
owingmills_monitors <- owingmills$id
eastnewmarket_monitors <- eastnewmarket$id
collegepark_monitors <- collegepark$id
reistertown_monitors <- reistertown$id
ashland_monitors <- ashland$id
mandelasprings_monitors <- mandelasprings$id
lilypons_monitors <- lilypons$id
newcomb_monitors <- newcomb$id
baltimorecounty_monitors <- baltimorecounty$id
timomium_monitors <- timomium$id
laplata_monitors <- laplata$id
rosedale_monitors <- rosedale$id
ellicottcity_monitors <- ellicottcity$id
thumont_monitors <- thumont$id
kennedyville_monitors <- kennedyville$id
northbesthesda_monitors <- northbesthesda$id
gaithersburg_monitors <- gaithersburg$id

### pull all observations for cities 
cat(citySwap(unique(city), y = "_obs <- meteo_pull_monitors(", z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')"), sep = "\n")

phoenix_obs <- meteo_pull_monitors(phoenix_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
carney_obs <- meteo_pull_monitors(carney_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
accokeek_obs <- meteo_pull_monitors(accokeek_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
galestown_obs <- meteo_pull_monitors(galestown_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
easton_obs <- meteo_pull_monitors(easton_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
silverspring_obs <- meteo_pull_monitors(silverspring_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
cockeysville_obs <- meteo_pull_monitors(cockeysville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
preston_obs <- meteo_pull_monitors(preston_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
snowhill_obs <- meteo_pull_monitors(snowhill_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
townson_obs <- meteo_pull_monitors(townson_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
waddellscorner_obs <- meteo_pull_monitors(waddellscorner_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
uppermarlboro_obs <- meteo_pull_monitors(uppermarlboro_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
harmony_obs <- meteo_pull_monitors(harmony_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
owingmills_obs <- meteo_pull_monitors(owingmills_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
eastnewmarket_obs <- meteo_pull_monitors(eastnewmarket_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
collegepark_obs <- meteo_pull_monitors(collegepark_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
reistertown_obs <- meteo_pull_monitors(reistertown_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
ashland_obs <- meteo_pull_monitors(ashland_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
mandelasprings_obs <- meteo_pull_monitors(mandelasprings_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
lilypons_obs <- meteo_pull_monitors(lilypons_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
newcomb_obs <- meteo_pull_monitors(newcomb_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
baltimorecounty_obs <- meteo_pull_monitors(baltimorecounty_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
timomium_obs <- meteo_pull_monitors(timomium_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
laplata_obs <- meteo_pull_monitors(laplata_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
rosedale_obs <- meteo_pull_monitors(rosedale_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
ellicottcity_obs <- meteo_pull_monitors(ellicottcity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
thumont_obs <- meteo_pull_monitors(thumont_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
kennedyville_obs <- meteo_pull_monitors(kennedyville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
northbesthesda_obs <- meteo_pull_monitors(northbesthesda_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
gaithersburg_obs <- meteo_pull_monitors(gaithersburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')

### Part 1
### pull data by date and location - dates are coming from state_csv 
lapply(df1, cat(citySwap4.5(x = city2, y = "_obs_temps <- ", i = city, z = "_obs %>% filter(date == '", j = date , h = "')"), sep = "\n"))

phoenix_obs_temps <- phoenix_obs %>% filter(date == '1981-04-29')
carney1_obs_temps <- carney_obs %>% filter(date == '1970-04-25')
carney2_obs_temps <- carney_obs %>% filter(date == '1970-04-25')
carney3_obs_temps <- carney_obs %>% filter(date == '1970-04-25')
accokeek_obs_temps <- accokeek_obs %>% filter(date == '1985-04-20')
galestown_obs_temps <- galestown_obs %>% filter(date == '2015-04-21')
easton_obs_temps <- easton_obs %>% filter(date == '2010-03-27')
silverspring1_obs_temps <- silverspring_obs %>% filter(date == '1910-04-10')
cockeysville_obs_temps <- cockeysville_obs %>% filter(date == '1970-05-12')
preston1_obs_temps <- preston_obs %>% filter(date == '2004-12-12')
preston2_obs_temps <- preston_obs %>% filter(date == '2015-12-02')
snowhill1_obs_temps <- snowhill_obs %>% filter(date == '2006-04-02')
townson1_obs_temps <- townson_obs %>% filter(date == '1980-04-30')
waddellscorner_obs_temps <- waddellscorner_obs %>% filter(date == '2005-04-15')
uppermarlboro_obs_temps <- uppermarlboro_obs %>% filter(date == '1990-03-29')
harmony_obs_temps <- harmony_obs %>% filter(date == '2010-11-27')
owingmills_obs_temps <- owingmills_obs %>% filter(date == '1977-06-23')
eastnewmarket_obs_temps <- eastnewmarket_obs %>% filter(date == '2005-04-24')
collegepark_obs_temps <- collegepark_obs %>% filter(date == '1990-04-18')
townson2_obs_temps <- townson_obs %>% filter(date == '1980-04-24')
snowhill2_obs_temps <- snowhill_obs %>% filter(date == '2006-04-02')
reistertown_obs_temps <- reistertown_obs %>% filter(date == '1980-04-26')
#ashland_obs_temps <- ashland_obs %>% filter(date == 'NA')
mandelasprings_obs_temps <- mandelasprings_obs %>% filter(date == '2007-05-01')
lilypons_obs_temps <- lilypons_obs %>% filter(date == '1980-04-21')
townson3_obs_temps <- townson_obs %>% filter(date == '1981-04-29')
newcomb_obs_temps <- newcomb_obs %>% filter(date == '2013-03-29')
baltimorecounty_obs_temps <- baltimorecounty_obs %>% filter(date == '1974-04-20')
timomium_obs_temps <- timomium_obs %>% filter(date == '1976-06-05')
laplata_obs_temps <- laplata_obs %>% filter(date == '1952-03-28')
rosedale_obs_temps <- rosedale_obs %>% filter(date == '1991-04-05')
#ellicottcity_obs_temps <- ellicottcity_obs %>% filter(date == 'NA')
thumont_obs_temps <- thumont_obs %>% filter(date == '1973-04-29')
kennedyville_obs_temps <- kennedyville_obs %>% filter(date == '1965-05-04')
silverspring2_obs_temps <- silverspring_obs %>% filter(date == '1977-05-06')
northbesthesda_obs_temps <- northbesthesda_obs %>% filter(date == '1973-04-12')
gaithersburg_obs_temps <- gaithersburg_obs %>% filter(date == '1981-05-09')

#combine into one data frame using bind_rows()
cat(citySwap7(city2, y = "_obs_temps", z = ","), sep = "")
maryland_obs <- bind_rows(phoenix_obs_temps,carney1_obs_temps,carney2_obs_temps,carney3_obs_temps,accokeek_obs_temps,galestown_obs_temps,easton_obs_temps,silverspring1_obs_temps,cockeysville_obs_temps,preston1_obs_temps,preston2_obs_temps,snowhill1_obs_temps,townson1_obs_temps,waddellscorner_obs_temps,uppermarlboro_obs_temps,harmony_obs_temps,owingmills_obs_temps,eastnewmarket_obs_temps,collegepark_obs_temps,townson2_obs_temps,snowhill2_obs_temps,reistertown_obs_temps,mandelasprings_obs_temps,lilypons_obs_temps,townson3_obs_temps,newcomb_obs_temps,baltimorecounty_obs_temps,timomium_obs_temps,laplata_obs_temps,rosedale_obs_temps,thumont_obs_temps,kennedyville_obs_temps,silverspring2_obs_temps,northbesthesda_obs_temps,gaithersburg_obs_temps)

#Merge the original state_csv dataframe and new state_obs dataframe 
maryland_data <- merge.data.frame(maryland_csv, maryland_obs, by = 'date')

## find the tavg for state data 
maryland_data$tavg <- ((maryland_data$tmax + maryland_data$tmin) / 2)

### save data as xlsx file 
library("xlsx")
write.xlsx(maryland_data, file = "maryland_data_total_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

### Part 2
#finding data from 6 months prior to date of collection
#ambient temps and precipitation may have affected plant/leaf growth and shape
lapply(df2, cat(citySwap5(x = city2, y = "_obs_6months <- meteo_pull_monitors(", j = city, 
                          z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '", h = p6mon, 
                          k = "', date_max = '", l = date, p ="')"), sep = "\n"))

phoenix_obs_6months <- meteo_pull_monitors(phoenix_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1980-10-31', date_max = '1981-04-29')
carney1_obs_6months <- meteo_pull_monitors(carney_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1969-10-31', date_max = '1970-04-25')
carney2_obs_6months <- meteo_pull_monitors(carney_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1969-10-31', date_max = '1970-04-25')
carney3_obs_6months <- meteo_pull_monitors(carney_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1969-10-31', date_max = '1970-04-25')
accokeek_obs_6months <- meteo_pull_monitors(accokeek_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1984-10-31', date_max = '1985-04-20')
galestown_obs_6months <- meteo_pull_monitors(galestown_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2014-10-31', date_max = '2015-04-21')
easton_obs_6months <- meteo_pull_monitors(easton_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2009-09-30', date_max = '2010-03-27')
silverspring1_obs_6months <- meteo_pull_monitors(silverspring_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1909-10-31', date_max = '1910-04-10')
cockeysville_obs_6months <- meteo_pull_monitors(cockeysville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1969-11-30', date_max = '1970-05-12')
preston1_obs_6months <- meteo_pull_monitors(preston_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2004-06-30', date_max = '2004-12-12')
preston2_obs_6months <- meteo_pull_monitors(preston_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2015-06-30', date_max = '2015-12-02')
snowhill1_obs_6months <- meteo_pull_monitors(snowhill_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2005-10-31', date_max = '2006-04-02')
townson1_obs_6months <- meteo_pull_monitors(townson_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1979-10-31', date_max = '1980-04-30')
waddellscorner_obs_6months <- meteo_pull_monitors(waddellscorner_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2004-10-31', date_max = '2005-04-15')
uppermarlboro_obs_6months <- meteo_pull_monitors(uppermarlboro_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1989-09-30', date_max = '1990-03-29')
harmony_obs_6months <- meteo_pull_monitors(harmony_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2010-05-31', date_max = '2010-11-27')
owingmills_obs_6months <- meteo_pull_monitors(owingmills_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1976-12-31', date_max = '1977-06-23')
eastnewmarket_obs_6months <- meteo_pull_monitors(eastnewmarket_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2004-10-31', date_max = '2005-04-24')
collegepark_obs_6months <- meteo_pull_monitors(collegepark_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1989-10-31', date_max = '1990-04-18')
townson2_obs_6months <- meteo_pull_monitors(townson_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1979-10-31', date_max = '1980-04-24')
snowhill2_obs_6months <- meteo_pull_monitors(snowhill_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2005-10-31', date_max = '2006-04-02')
reistertown_obs_6months <- meteo_pull_monitors(reistertown_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1979-10-31', date_max = '1980-04-26')
#ashland_obs_6months <- meteo_pull_monitors(ashland_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = 'NA', date_max = 'NA')
mandelasprings_obs_6months <- meteo_pull_monitors(mandelasprings_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2006-11-30', date_max = '2007-05-01')
lilypons_obs_6months <- meteo_pull_monitors(lilypons_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1979-10-31', date_max = '1980-04-21')
townson3_obs_6months <- meteo_pull_monitors(townson_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1980-10-31', date_max = '1981-04-29')
newcomb_obs_6months <- meteo_pull_monitors(newcomb_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2012-09-30', date_max = '2013-03-29')
baltimorecounty_obs_6months <- meteo_pull_monitors(baltimorecounty_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1973-10-31', date_max = '1974-04-20')
timomium_obs_6months <- meteo_pull_monitors(timomium_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1975-12-31', date_max = '1976-06-05')
laplata_obs_6months <- meteo_pull_monitors(laplata_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1951-09-30', date_max = '1952-03-28')
rosedale_obs_6months <- meteo_pull_monitors(rosedale_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1990-10-31', date_max = '1991-04-05')
#ellicottcity_obs_6months <- meteo_pull_monitors(ellicottcity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = 'NA', date_max = 'NA')
thumont_obs_6months <- meteo_pull_monitors(thumont_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1972-10-31', date_max = '1973-04-29')
kennedyville_obs_6months <- meteo_pull_monitors(kennedyville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1964-11-30', date_max = '1965-05-04')
silverspring2_obs_6months <- meteo_pull_monitors(silverspring_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1976-11-30', date_max = '1977-05-06')
northbesthesda_obs_6months <- meteo_pull_monitors(northbesthesda_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1972-10-31', date_max = '1973-04-12')
gaithersburg_obs_6months <- meteo_pull_monitors(gaithersburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1980-11-30', date_max = '1981-05-09')

## make a dataframe of the mean of tmax, tmin, prcp of the previous 6 months 
lapply(df3, cat(citySwap2(x = city2,  y = "_6months", z = " <- data.frame(maryland_csv$id[", l= lennumber, k = "], (mean(", e = "_obs_6months$prcp, na.rm=TRUE)), (mean(", f = "_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(", g = "_obs_6months$tmin, na.rm=TRUE))) "), sep = "\n"))

phoenix_6months <- data.frame(maryland_csv$id[1], (mean(phoenix_obs_6months$prcp, na.rm=TRUE)), (mean(phoenix_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(phoenix_obs_6months$tmin, na.rm=TRUE))) 
carney1_6months <- data.frame(maryland_csv$id[2], (mean(carney1_obs_6months$prcp, na.rm=TRUE)), (mean(carney1_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(carney1_obs_6months$tmin, na.rm=TRUE))) 
carney2_6months <- data.frame(maryland_csv$id[3], (mean(carney2_obs_6months$prcp, na.rm=TRUE)), (mean(carney2_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(carney2_obs_6months$tmin, na.rm=TRUE))) 
carney3_6months <- data.frame(maryland_csv$id[4], (mean(carney3_obs_6months$prcp, na.rm=TRUE)), (mean(carney3_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(carney3_obs_6months$tmin, na.rm=TRUE))) 
accokeek_6months <- data.frame(maryland_csv$id[5], (mean(accokeek_obs_6months$prcp, na.rm=TRUE)), (mean(accokeek_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(accokeek_obs_6months$tmin, na.rm=TRUE))) 
galestown_6months <- data.frame(maryland_csv$id[6], (mean(galestown_obs_6months$prcp, na.rm=TRUE)), (mean(galestown_obs_6months$tmax, na.rm=TRUE)), 
                                (mean(galestown_obs_6months$tmin, na.rm=TRUE))) 
easton_6months <- data.frame(maryland_csv$id[7], (mean(easton_obs_6months$prcp, na.rm=TRUE)), (mean(easton_obs_6months$tmax, na.rm=TRUE)), 
                             (mean(easton_obs_6months$tmin, na.rm=TRUE))) 
silverspring1_6months <- data.frame(maryland_csv$id[8], (mean(silverspring1_obs_6months$prcp, na.rm=TRUE)), (mean(silverspring1_obs_6months$tmax, na.rm=TRUE)), 
                                    (mean(silverspring1_obs_6months$tmin, na.rm=TRUE))) 
cockeysville_6months <- data.frame(maryland_csv$id[9], (mean(cockeysville_obs_6months$prcp, na.rm=TRUE)), (mean(cockeysville_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(cockeysville_obs_6months$tmin, na.rm=TRUE))) 
preston1_6months <- data.frame(maryland_csv$id[10], (mean(preston1_obs_6months$prcp, na.rm=TRUE)), (mean(preston1_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(preston1_obs_6months$tmin, na.rm=TRUE))) 
preston2_6months <- data.frame(maryland_csv$id[11], (mean(preston2_obs_6months$prcp, na.rm=TRUE)), (mean(preston2_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(preston2_obs_6months$tmin, na.rm=TRUE))) 
snowhill1_6months <- data.frame(maryland_csv$id[12], (mean(snowhill1_obs_6months$prcp, na.rm=TRUE)), (mean(snowhill1_obs_6months$tmax, na.rm=TRUE)), 
                                (mean(snowhill1_obs_6months$tmin, na.rm=TRUE))) 
townson1_6months <- data.frame(maryland_csv$id[13], (mean(townson1_obs_6months$prcp, na.rm=TRUE)), (mean(townson1_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(townson1_obs_6months$tmin, na.rm=TRUE))) 
waddellscorner_6months <- data.frame(maryland_csv$id[14], (mean(waddellscorner_obs_6months$prcp, na.rm=TRUE)), (mean(waddellscorner_obs_6months$tmax, na.rm=TRUE)), 
                                     (mean(waddellscorner_obs_6months$tmin, na.rm=TRUE))) 
uppermarlboro_6months <- data.frame(maryland_csv$id[15], (mean(uppermarlboro_obs_6months$prcp, na.rm=TRUE)), (mean(uppermarlboro_obs_6months$tmax, na.rm=TRUE)), 
                                    (mean(uppermarlboro_obs_6months$tmin, na.rm=TRUE))) 
harmony_6months <- data.frame(maryland_csv$id[16], (mean(harmony_obs_6months$prcp, na.rm=TRUE)), (mean(harmony_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(harmony_obs_6months$tmin, na.rm=TRUE))) 
owingmills_6months <- data.frame(maryland_csv$id[17], (mean(owingmills_obs_6months$prcp, na.rm=TRUE)), (mean(owingmills_obs_6months$tmax, na.rm=TRUE)), 
                                 (mean(owingmills_obs_6months$tmin, na.rm=TRUE))) 
eastnewmarket_6months <- data.frame(maryland_csv$id[18], (mean(eastnewmarket_obs_6months$prcp, na.rm=TRUE)), (mean(eastnewmarket_obs_6months$tmax, na.rm=TRUE)), 
                                    (mean(eastnewmarket_obs_6months$tmin, na.rm=TRUE))) 
collegepark_6months <- data.frame(maryland_csv$id[19], (mean(collegepark_obs_6months$prcp, na.rm=TRUE)), (mean(collegepark_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(collegepark_obs_6months$tmin, na.rm=TRUE))) 
townson2_6months <- data.frame(maryland_csv$id[20], (mean(townson2_obs_6months$prcp, na.rm=TRUE)), (mean(townson2_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(townson2_obs_6months$tmin, na.rm=TRUE))) 
snowhill2_6months <- data.frame(maryland_csv$id[21], (mean(snowhill2_obs_6months$prcp, na.rm=TRUE)), (mean(snowhill2_obs_6months$tmax, na.rm=TRUE)), 
                                (mean(snowhill2_obs_6months$tmin, na.rm=TRUE))) 
reistertown_6months <- data.frame(maryland_csv$id[22], (mean(reistertown_obs_6months$prcp, na.rm=TRUE)), (mean(reistertown_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(reistertown_obs_6months$tmin, na.rm=TRUE))) 
mandelasprings_6months <- data.frame(maryland_csv$id[24], (mean(mandelasprings_obs_6months$prcp, na.rm=TRUE)), (mean(mandelasprings_obs_6months$tmax, na.rm=TRUE)), 
                                     (mean(mandelasprings_obs_6months$tmin, na.rm=TRUE))) 
lilypons_6months <- data.frame(maryland_csv$id[25], (mean(lilypons_obs_6months$prcp, na.rm=TRUE)), (mean(lilypons_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(lilypons_obs_6months$tmin, na.rm=TRUE))) 
townson3_6months <- data.frame(maryland_csv$id[26], (mean(townson3_obs_6months$prcp, na.rm=TRUE)), (mean(townson3_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(townson3_obs_6months$tmin, na.rm=TRUE))) 
newcomb_6months <- data.frame(maryland_csv$id[27], (mean(newcomb_obs_6months$prcp, na.rm=TRUE)), (mean(newcomb_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(newcomb_obs_6months$tmin, na.rm=TRUE))) 
baltimorecounty_6months <- data.frame(maryland_csv$id[28], (mean(baltimorecounty_obs_6months$prcp, na.rm=TRUE)), (mean(baltimorecounty_obs_6months$tmax, na.rm=TRUE)), 
                                      (mean(baltimorecounty_obs_6months$tmin, na.rm=TRUE))) 
timomium_6months <- data.frame(maryland_csv$id[29], (mean(timomium_obs_6months$prcp, na.rm=TRUE)), (mean(timomium_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(timomium_obs_6months$tmin, na.rm=TRUE))) 
laplata_6months <- data.frame(maryland_csv$id[30], (mean(laplata_obs_6months$prcp, na.rm=TRUE)), (mean(laplata_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(laplata_obs_6months$tmin, na.rm=TRUE))) 
rosedale_6months <- data.frame(maryland_csv$id[31], (mean(rosedale_obs_6months$prcp, na.rm=TRUE)), (mean(rosedale_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(rosedale_obs_6months$tmin, na.rm=TRUE))) 
thumont_6months <- data.frame(maryland_csv$id[33], (mean(thumont_obs_6months$prcp, na.rm=TRUE)), (mean(thumont_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(thumont_obs_6months$tmin, na.rm=TRUE))) 
kennedyville_6months <- data.frame(maryland_csv$id[34], (mean(kennedyville_obs_6months$prcp, na.rm=TRUE)), (mean(kennedyville_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(kennedyville_obs_6months$tmin, na.rm=TRUE))) 
silverspring2_6months <- data.frame(maryland_csv$id[35], (mean(silverspring2_obs_6months$prcp, na.rm=TRUE)), (mean(silverspring2_obs_6months$tmax, na.rm=TRUE)), 
                                    (mean(silverspring2_obs_6months$tmin, na.rm=TRUE))) 
northbesthesda_6months <- data.frame(maryland_csv$id[36], (mean(northbesthesda_obs_6months$prcp, na.rm=TRUE)), (mean(northbesthesda_obs_6months$tmax, na.rm=TRUE)), 
                                     (mean(northbesthesda_obs_6months$tmin, na.rm=TRUE))) 
gaithersburg_6months <- data.frame(maryland_csv$id[37], (mean(gaithersburg_obs_6months$prcp, na.rm=TRUE)), (mean(gaithersburg_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(gaithersburg_obs_6months$tmin, na.rm=TRUE)))

#rename col.names 
cat(citySwap3(city2, y = "colnames(", z = "_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')"), sep = "\n")

colnames(phoenix_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(carney1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(carney2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(carney3_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(accokeek_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(galestown_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(easton_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(silverspring1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(cockeysville_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(preston1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(preston2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(snowhill1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(townson1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(waddellscorner_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(uppermarlboro_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(harmony_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(owingmills_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(eastnewmarket_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(collegepark_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(townson2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(snowhill2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(reistertown_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(mandelasprings_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(lilypons_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(townson3_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(newcomb_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(baltimorecounty_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(timomium_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(laplata_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(rosedale_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(thumont_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(kennedyville_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(silverspring2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(northbesthesda_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(gaithersburg_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')

## combine previous 6 months data using bind_rows
cat(citySwap7(city2, y = "_6months", z = ","), sep = "")
maryland_6months <- bind_rows(phoenix_6months,carney1_6months,carney2_6months,carney3_6months,accokeek_6months,galestown_6months,easton_6months,silverspring1_6months,cockeysville_6months,preston1_6months,preston2_6months,
                           snowhill1_6months,townson1_6months,waddellscorner_6months,uppermarlboro_6months,harmony_6months,owingmills_6months,eastnewmarket_6months,collegepark_6months,townson2_6months,snowhill2_6months,
                           reistertown_6months,mandelasprings_6months,lilypons_6months,townson3_6months,newcomb_6months,baltimorecounty_6months,timomium_6months,laplata_6months,rosedale_6months,
                           thumont_6months,kennedyville_6months,silverspring2_6months,northbesthesda_6months,gaithersburg_6months,)

### save data as xlsx file 
write.xlsx(maryland_6months, file = "maryland_6months_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


### Part 3
#find data from previous year from date of collection 
lapply(df4, cat(citySwap5(x = city2, y = "_obs_year <- meteo_pull_monitors(", j = city, 
                          z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '", h= pyear, 
                          k = "', date_max = '", l = date, p ="')"), sep = "\n"))

phoenix_obs_year <- meteo_pull_monitors(phoenix_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1980-04-29', date_max = '1981-04-29')
carney1_obs_year <- meteo_pull_monitors(carney_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1969-04-25', date_max = '1970-04-25')
carney2_obs_year <- meteo_pull_monitors(carney_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1969-04-25', date_max = '1970-04-25')
carney3_obs_year <- meteo_pull_monitors(carney_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1969-04-25', date_max = '1970-04-25')
accokeek_obs_year <- meteo_pull_monitors(accokeek_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1984-04-20', date_max = '1985-04-20')
galestown_obs_year <- meteo_pull_monitors(galestown_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2014-04-21', date_max = '2015-04-21')
easton_obs_year <- meteo_pull_monitors(easton_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2009-03-27', date_max = '2010-03-27')
silverspring1_obs_year <- meteo_pull_monitors(silverspring_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1909-04-10', date_max = '1910-04-10')
cockeysville_obs_year <- meteo_pull_monitors(cockeysville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1969-05-12', date_max = '1970-05-12')
preston1_obs_year <- meteo_pull_monitors(preston_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2003-12-13', date_max = '2004-12-12')
preston2_obs_year <- meteo_pull_monitors(preston_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2014-12-02', date_max = '2015-12-02')
snowhill1_obs_year <- meteo_pull_monitors(snowhill_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2005-04-02', date_max = '2006-04-02')
townson1_obs_year <- meteo_pull_monitors(townson_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1979-05-01', date_max = '1980-04-30')
waddellscorner_obs_year <- meteo_pull_monitors(waddellscorner_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2004-04-15', date_max = '2005-04-15')
uppermarlboro_obs_year <- meteo_pull_monitors(uppermarlboro_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1989-03-29', date_max = '1990-03-29')
harmony_obs_year <- meteo_pull_monitors(harmony_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2009-11-27', date_max = '2010-11-27')
owingmills_obs_year <- meteo_pull_monitors(owingmills_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1976-06-23', date_max = '1977-06-23')
eastnewmarket_obs_year <- meteo_pull_monitors(eastnewmarket_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2004-04-24', date_max = '2005-04-24')
collegepark_obs_year <- meteo_pull_monitors(collegepark_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1989-04-18', date_max = '1990-04-18')
townson2_obs_year <- meteo_pull_monitors(townson_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1979-04-25', date_max = '1980-04-24')
snowhill2_obs_year <- meteo_pull_monitors(snowhill_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2005-04-02', date_max = '2006-04-02')
reistertown_obs_year <- meteo_pull_monitors(reistertown_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1979-04-27', date_max = '1980-04-26')
#ashland_obs_year <- meteo_pull_monitors(ashland_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = 'NA', date_max = 'NA')
mandelasprings_obs_year <- meteo_pull_monitors(mandelasprings_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2006-05-01', date_max = '2007-05-01')
lilypons_obs_year <- meteo_pull_monitors(lilypons_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1979-04-22', date_max = '1980-04-21')
townson3_obs_year <- meteo_pull_monitors(townson_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1980-04-29', date_max = '1981-04-29')
newcomb_obs_year <- meteo_pull_monitors(newcomb_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2012-03-29', date_max = '2013-03-29')
baltimorecounty_obs_year <- meteo_pull_monitors(baltimorecounty_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1973-04-20', date_max = '1974-04-20')
timomium_obs_year <- meteo_pull_monitors(timomium_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1975-06-06', date_max = '1976-06-05')
laplata_obs_year <- meteo_pull_monitors(laplata_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1951-03-29', date_max = '1952-03-28')
rosedale_obs_year <- meteo_pull_monitors(rosedale_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1990-04-05', date_max = '1991-04-05')
#ellicottcity_obs_year <- meteo_pull_monitors(ellicottcity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = 'NA', date_max = 'NA')
thumont_obs_year <- meteo_pull_monitors(thumont_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1972-04-29', date_max = '1973-04-29')
kennedyville_obs_year <- meteo_pull_monitors(kennedyville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1964-05-04', date_max = '1965-05-04')
silverspring2_obs_year <- meteo_pull_monitors(silverspring_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1976-05-06', date_max = '1977-05-06')
northbesthesda_obs_year <- meteo_pull_monitors(northbesthesda_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1972-04-12', date_max = '1973-04-12')
gaithersburg_obs_year <- meteo_pull_monitors(gaithersburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1980-05-09', date_max = '1981-05-09')

## make a data frame of the mean of tmax, tmin, prcp of the previous year before the date of collection 
lapply(df3, cat(citySwap2(x = city2,  y = "_previousyear", z = " <- data.frame(maryland_csv$id[", l= lennumber, 
                          k = "], (mean(", e = "_obs_year$prcp, na.rm=TRUE)), (mean(", 
                          f = "_obs_year$tmax, na.rm=TRUE)), (mean(", g = "_obs_year$tmin, na.rm=TRUE))) "), sep = "\n"))

phoenix_previousyear <- data.frame(maryland_csv$id[1], (mean(phoenix_obs_year$prcp, na.rm=TRUE)), (mean(phoenix_obs_year$tmax, na.rm=TRUE)), (mean(phoenix_obs_year$tmin, na.rm=TRUE))) 
carney1_previousyear <- data.frame(maryland_csv$id[2], (mean(carney1_obs_year$prcp, na.rm=TRUE)), (mean(carney1_obs_year$tmax, na.rm=TRUE)), (mean(carney1_obs_year$tmin, na.rm=TRUE))) 
carney2_previousyear <- data.frame(maryland_csv$id[3], (mean(carney2_obs_year$prcp, na.rm=TRUE)), (mean(carney2_obs_year$tmax, na.rm=TRUE)), (mean(carney2_obs_year$tmin, na.rm=TRUE))) 
carney3_previousyear <- data.frame(maryland_csv$id[4], (mean(carney3_obs_year$prcp, na.rm=TRUE)), (mean(carney3_obs_year$tmax, na.rm=TRUE)), (mean(carney3_obs_year$tmin, na.rm=TRUE))) 
accokeek_previousyear <- data.frame(maryland_csv$id[5], (mean(accokeek_obs_year$prcp, na.rm=TRUE)), (mean(accokeek_obs_year$tmax, na.rm=TRUE)), (mean(accokeek_obs_year$tmin, na.rm=TRUE))) 
galestown_previousyear <- data.frame(maryland_csv$id[6], (mean(galestown_obs_year$prcp, na.rm=TRUE)), (mean(galestown_obs_year$tmax, na.rm=TRUE)), (mean(galestown_obs_year$tmin, na.rm=TRUE))) 
easton_previousyear <- data.frame(maryland_csv$id[7], (mean(easton_obs_year$prcp, na.rm=TRUE)), (mean(easton_obs_year$tmax, na.rm=TRUE)), (mean(easton_obs_year$tmin, na.rm=TRUE))) 
silverspring1_previousyear <- data.frame(maryland_csv$id[8], (mean(silverspring1_obs_year$prcp, na.rm=TRUE)), (mean(silverspring1_obs_year$tmax, na.rm=TRUE)), (mean(silverspring1_obs_year$tmin, na.rm=TRUE))) 
cockeysville_previousyear <- data.frame(maryland_csv$id[9], (mean(cockeysville_obs_year$prcp, na.rm=TRUE)), (mean(cockeysville_obs_year$tmax, na.rm=TRUE)), (mean(cockeysville_obs_year$tmin, na.rm=TRUE))) 
preston1_previousyear <- data.frame(maryland_csv$id[10], (mean(preston1_obs_year$prcp, na.rm=TRUE)), (mean(preston1_obs_year$tmax, na.rm=TRUE)), (mean(preston1_obs_year$tmin, na.rm=TRUE))) 
preston2_previousyear <- data.frame(maryland_csv$id[11], (mean(preston2_obs_year$prcp, na.rm=TRUE)), (mean(preston2_obs_year$tmax, na.rm=TRUE)), (mean(preston2_obs_year$tmin, na.rm=TRUE))) 
snowhill1_previousyear <- data.frame(maryland_csv$id[12], (mean(snowhill1_obs_year$prcp, na.rm=TRUE)), (mean(snowhill1_obs_year$tmax, na.rm=TRUE)), (mean(snowhill1_obs_year$tmin, na.rm=TRUE))) 
townson1_previousyear <- data.frame(maryland_csv$id[13], (mean(townson1_obs_year$prcp, na.rm=TRUE)), (mean(townson1_obs_year$tmax, na.rm=TRUE)), (mean(townson1_obs_year$tmin, na.rm=TRUE))) 
waddellscorner_previousyear <- data.frame(maryland_csv$id[14], (mean(waddellscorner_obs_year$prcp, na.rm=TRUE)), (mean(waddellscorner_obs_year$tmax, na.rm=TRUE)), (mean(waddellscorner_obs_year$tmin, na.rm=TRUE))) 
uppermarlboro_previousyear <- data.frame(maryland_csv$id[15], (mean(uppermarlboro_obs_year$prcp, na.rm=TRUE)), (mean(uppermarlboro_obs_year$tmax, na.rm=TRUE)), (mean(uppermarlboro_obs_year$tmin, na.rm=TRUE))) 
harmony_previousyear <- data.frame(maryland_csv$id[16], (mean(harmony_obs_year$prcp, na.rm=TRUE)), (mean(harmony_obs_year$tmax, na.rm=TRUE)), (mean(harmony_obs_year$tmin, na.rm=TRUE))) 
owingmills_previousyear <- data.frame(maryland_csv$id[17], (mean(owingmills_obs_year$prcp, na.rm=TRUE)), (mean(owingmills_obs_year$tmax, na.rm=TRUE)), (mean(owingmills_obs_year$tmin, na.rm=TRUE))) 
eastnewmarket_previousyear <- data.frame(maryland_csv$id[18], (mean(eastnewmarket_obs_year$prcp, na.rm=TRUE)), (mean(eastnewmarket_obs_year$tmax, na.rm=TRUE)), (mean(eastnewmarket_obs_year$tmin, na.rm=TRUE))) 
collegepark_previousyear <- data.frame(maryland_csv$id[19], (mean(collegepark_obs_year$prcp, na.rm=TRUE)), (mean(collegepark_obs_year$tmax, na.rm=TRUE)), (mean(collegepark_obs_year$tmin, na.rm=TRUE))) 
townson2_previousyear <- data.frame(maryland_csv$id[20], (mean(townson2_obs_year$prcp, na.rm=TRUE)), (mean(townson2_obs_year$tmax, na.rm=TRUE)), (mean(townson2_obs_year$tmin, na.rm=TRUE))) 
snowhill2_previousyear <- data.frame(maryland_csv$id[21], (mean(snowhill2_obs_year$prcp, na.rm=TRUE)), (mean(snowhill2_obs_year$tmax, na.rm=TRUE)), (mean(snowhill2_obs_year$tmin, na.rm=TRUE))) 
reistertown_previousyear <- data.frame(maryland_csv$id[22], (mean(reistertown_obs_year$prcp, na.rm=TRUE)), (mean(reistertown_obs_year$tmax, na.rm=TRUE)), (mean(reistertown_obs_year$tmin, na.rm=TRUE))) 
mandelasprings_previousyear <- data.frame(maryland_csv$id[24], (mean(mandelasprings_obs_year$prcp, na.rm=TRUE)), (mean(mandelasprings_obs_year$tmax, na.rm=TRUE)), (mean(mandelasprings_obs_year$tmin, na.rm=TRUE))) 
lilypons_previousyear <- data.frame(maryland_csv$id[25], (mean(lilypons_obs_year$prcp, na.rm=TRUE)), (mean(lilypons_obs_year$tmax, na.rm=TRUE)), (mean(lilypons_obs_year$tmin, na.rm=TRUE))) 
townson3_previousyear <- data.frame(maryland_csv$id[26], (mean(townson3_obs_year$prcp, na.rm=TRUE)), (mean(townson3_obs_year$tmax, na.rm=TRUE)), (mean(townson3_obs_year$tmin, na.rm=TRUE))) 
newcomb_previousyear <- data.frame(maryland_csv$id[27], (mean(newcomb_obs_year$prcp, na.rm=TRUE)), (mean(newcomb_obs_year$tmax, na.rm=TRUE)), (mean(newcomb_obs_year$tmin, na.rm=TRUE))) 
baltimorecounty_previousyear <- data.frame(maryland_csv$id[28], (mean(baltimorecounty_obs_year$prcp, na.rm=TRUE)), (mean(baltimorecounty_obs_year$tmax, na.rm=TRUE)), (mean(baltimorecounty_obs_year$tmin, na.rm=TRUE))) 
timomium_previousyear <- data.frame(maryland_csv$id[29], (mean(timomium_obs_year$prcp, na.rm=TRUE)), (mean(timomium_obs_year$tmax, na.rm=TRUE)), (mean(timomium_obs_year$tmin, na.rm=TRUE))) 
laplata_previousyear <- data.frame(maryland_csv$id[30], (mean(laplata_obs_year$prcp, na.rm=TRUE)), (mean(laplata_obs_year$tmax, na.rm=TRUE)), (mean(laplata_obs_year$tmin, na.rm=TRUE))) 
rosedale_previousyear <- data.frame(maryland_csv$id[31], (mean(rosedale_obs_year$prcp, na.rm=TRUE)), (mean(rosedale_obs_year$tmax, na.rm=TRUE)), (mean(rosedale_obs_year$tmin, na.rm=TRUE))) 
thumont_previousyear <- data.frame(maryland_csv$id[33], (mean(thumont_obs_year$prcp, na.rm=TRUE)), (mean(thumont_obs_year$tmax, na.rm=TRUE)), (mean(thumont_obs_year$tmin, na.rm=TRUE))) 
kennedyville_previousyear <- data.frame(maryland_csv$id[34], (mean(kennedyville_obs_year$prcp, na.rm=TRUE)), (mean(kennedyville_obs_year$tmax, na.rm=TRUE)), (mean(kennedyville_obs_year$tmin, na.rm=TRUE))) 
silverspring2_previousyear <- data.frame(maryland_csv$id[35], (mean(silverspring2_obs_year$prcp, na.rm=TRUE)), (mean(silverspring2_obs_year$tmax, na.rm=TRUE)), (mean(silverspring2_obs_year$tmin, na.rm=TRUE))) 
northbesthesda_previousyear <- data.frame(maryland_csv$id[36], (mean(northbesthesda_obs_year$prcp, na.rm=TRUE)), (mean(northbesthesda_obs_year$tmax, na.rm=TRUE)), (mean(northbesthesda_obs_year$tmin, na.rm=TRUE))) 
gaithersburg_previousyear <- data.frame(maryland_csv$id[37], (mean(gaithersburg_obs_year$prcp, na.rm=TRUE)), (mean(gaithersburg_obs_year$tmax, na.rm=TRUE)), (mean(gaithersburg_obs_year$tmin, na.rm=TRUE))) 

#rename col.names 
cat(citySwap3(city2, y = "colnames(", z = "_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')"), sep = "\n")

colnames(phoenix_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(carney1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(carney2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(carney3_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(accokeek_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(galestown_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(easton_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(silverspring1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(cockeysville_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(preston1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(preston2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(snowhill1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(townson1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(waddellscorner_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(uppermarlboro_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(harmony_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(owingmills_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(eastnewmarket_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(collegepark_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(townson2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(snowhill2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(reistertown_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
#colnames(ashland_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(mandelasprings_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(lilypons_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(townson3_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(newcomb_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(baltimorecounty_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(timomium_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(laplata_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(rosedale_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
#colnames(ellicottcity_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(thumont_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(kennedyville_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(silverspring2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(northbesthesda_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(gaithersburg_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')

## combine previous year before the date of collection data using bind_rows
cat(citySwap7(city2, y = "_previousyear", z = ","), sep = "")
maryland_Pyear <- bind_rows(phoenix_previousyear,carney1_previousyear,carney2_previousyear,carney3_previousyear,accokeek_previousyear,galestown_previousyear,easton_previousyear,silverspring1_previousyear,cockeysville_previousyear,preston1_previousyear,preston2_previousyear,snowhill1_previousyear,townson1_previousyear,waddellscorner_previousyear,uppermarlboro_previousyear,harmony_previousyear,owingmills_previousyear,eastnewmarket_previousyear,collegepark_previousyear,townson2_previousyear,snowhill2_previousyear,reistertown_previousyear,mandelasprings_previousyear,lilypons_previousyear,townson3_previousyear,newcomb_previousyear,baltimorecounty_previousyear,timomium_previousyear,laplata_previousyear,rosedale_previousyear,thumont_previousyear,kennedyville_previousyear,silverspring2_previousyear,northbesthesda_previousyear,gaithersburg_previousyear)

# save data
write.xlsx(maryland_Pyear, file = "maryland_previous_year_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


### Part 4 
#Find data from current year of date of collection 
lapply(df5, cat(citySwap6(x = city2, y = "_obs_yearDOC <- meteo_pull_monitors(", j = city, 
                          z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '", l = year , 
                          k = "-01-01', date_max = '",  p ="-12-31')"), sep = "\n"))

phoenix_obs_yearDOC <- meteo_pull_monitors(phoenix_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1981-01-01', date_max = '1981-12-31')
carney1_obs_yearDOC <- meteo_pull_monitors(carney_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1970-01-01', date_max = '1970-12-31')
carney2_obs_yearDOC <- meteo_pull_monitors(carney_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1970-01-01', date_max = '1970-12-31')
carney3_obs_yearDOC <- meteo_pull_monitors(carney_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1970-01-01', date_max = '1970-12-31')
accokeek_obs_yearDOC <- meteo_pull_monitors(accokeek_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1985-01-01', date_max = '1985-12-31')
galestown_obs_yearDOC <- meteo_pull_monitors(galestown_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2015-01-01', date_max = '2015-12-31')
easton_obs_yearDOC <- meteo_pull_monitors(easton_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2010-01-01', date_max = '2010-12-31')
silverspring1_obs_yearDOC <- meteo_pull_monitors(silverspring_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1910-01-01', date_max = '1910-12-31')
cockeysville_obs_yearDOC <- meteo_pull_monitors(cockeysville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1970-01-01', date_max = '1970-12-31')
preston1_obs_yearDOC <- meteo_pull_monitors(preston_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2004-01-01', date_max = '2004-12-31')
preston2_obs_yearDOC <- meteo_pull_monitors(preston_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2015-01-01', date_max = '2015-12-31')
snowhill1_obs_yearDOC <- meteo_pull_monitors(snowhill_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2006-01-01', date_max = '2006-12-31')
townson1_obs_yearDOC <- meteo_pull_monitors(townson_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1980-01-01', date_max = '1980-12-31')
waddellscorner_obs_yearDOC <- meteo_pull_monitors(waddellscorner_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2005-01-01', date_max = '2005-12-31')
uppermarlboro_obs_yearDOC <- meteo_pull_monitors(uppermarlboro_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1990-01-01', date_max = '1990-12-31')
harmony_obs_yearDOC <- meteo_pull_monitors(harmony_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2010-01-01', date_max = '2010-12-31')
owingmills_obs_yearDOC <- meteo_pull_monitors(owingmills_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1977-01-01', date_max = '1977-12-31')
eastnewmarket_obs_yearDOC <- meteo_pull_monitors(eastnewmarket_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2005-01-01', date_max = '2005-12-31')
collegepark_obs_yearDOC <- meteo_pull_monitors(collegepark_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1990-01-01', date_max = '1990-12-31')
townson2_obs_yearDOC <- meteo_pull_monitors(townson_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1980-01-01', date_max = '1980-12-31')
snowhill2_obs_yearDOC <- meteo_pull_monitors(snowhill_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2006-01-01', date_max = '2006-12-31')
reistertown_obs_yearDOC <- meteo_pull_monitors(reistertown_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1980-01-01', date_max = '1980-12-31')
ashland_obs_yearDOC <- meteo_pull_monitors(ashland_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1973-01-01', date_max = '1973-12-31')
mandelasprings_obs_yearDOC <- meteo_pull_monitors(mandelasprings_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2007-01-01', date_max = '2007-12-31')
lilypons_obs_yearDOC <- meteo_pull_monitors(lilypons_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1980-01-01', date_max = '1980-12-31')
townson3_obs_yearDOC <- meteo_pull_monitors(townson_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1981-01-01', date_max = '1981-12-31')
newcomb_obs_yearDOC <- meteo_pull_monitors(newcomb_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2013-01-01', date_max = '2013-12-31')
baltimorecounty_obs_yearDOC <- meteo_pull_monitors(baltimorecounty_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1974-01-01', date_max = '1974-12-31')
timomium_obs_yearDOC <- meteo_pull_monitors(timomium_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1976-01-01', date_max = '1976-12-31')
laplata_obs_yearDOC <- meteo_pull_monitors(laplata_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1952-01-01', date_max = '1952-12-31')
rosedale_obs_yearDOC <- meteo_pull_monitors(rosedale_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1991-01-01', date_max = '1991-12-31')
ellicottcity_obs_yearDOC <- meteo_pull_monitors(ellicottcity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1964-01-01', date_max = '1964-12-31')
thumont_obs_yearDOC <- meteo_pull_monitors(thumont_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1973-01-01', date_max = '1973-12-31')
kennedyville_obs_yearDOC <- meteo_pull_monitors(kennedyville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1965-01-01', date_max = '1965-12-31')
silverspring2_obs_yearDOC <- meteo_pull_monitors(silverspring_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1977-01-01', date_max = '1977-12-31')
northbesthesda_obs_yearDOC <- meteo_pull_monitors(northbesthesda_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1973-01-01', date_max = '1973-12-31')
gaithersburg_obs_yearDOC <- meteo_pull_monitors(gaithersburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1981-01-01', date_max = '1981-12-31')

## make a dataframe of the mean of tmax, tmin, current year of the date of collection
lapply(dflist_nodates, cat(citySwap2(x = city2,  y = "_DOC", z = " <- data.frame(maryland_csv$id[", l= lennumber, k = "], (mean(", e = "_obs_yearDOC$prcp, na.rm=TRUE)), (mean(", f = "_obs_yearDOC$tmax, na.rm=TRUE)), (mean(", g = "_obs_yearDOC$tmin, na.rm=TRUE))) "), sep = "\n"))

phoenix_DOC <- data.frame(maryland_csv$id[1], (mean(phoenix_obs_yearDOC$prcp, na.rm=TRUE)), (mean(phoenix_obs_yearDOC$tmax, na.rm=TRUE)), (mean(phoenix_obs_yearDOC$tmin, na.rm=TRUE))) 
carney1_DOC <- data.frame(maryland_csv$id[2], (mean(carney1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(carney1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(carney1_obs_yearDOC$tmin, na.rm=TRUE))) 
carney2_DOC <- data.frame(maryland_csv$id[3], (mean(carney2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(carney2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(carney2_obs_yearDOC$tmin, na.rm=TRUE))) 
carney3_DOC <- data.frame(maryland_csv$id[4], (mean(carney3_obs_yearDOC$prcp, na.rm=TRUE)), (mean(carney3_obs_yearDOC$tmax, na.rm=TRUE)), (mean(carney3_obs_yearDOC$tmin, na.rm=TRUE))) 
accokeek_DOC <- data.frame(maryland_csv$id[5], (mean(accokeek_obs_yearDOC$prcp, na.rm=TRUE)), (mean(accokeek_obs_yearDOC$tmax, na.rm=TRUE)), (mean(accokeek_obs_yearDOC$tmin, na.rm=TRUE))) 
galestown_DOC <- data.frame(maryland_csv$id[6], (mean(galestown_obs_yearDOC$prcp, na.rm=TRUE)), (mean(galestown_obs_yearDOC$tmax, na.rm=TRUE)), (mean(galestown_obs_yearDOC$tmin, na.rm=TRUE))) 
easton_DOC <- data.frame(maryland_csv$id[7], (mean(easton_obs_yearDOC$prcp, na.rm=TRUE)), (mean(easton_obs_yearDOC$tmax, na.rm=TRUE)), (mean(easton_obs_yearDOC$tmin, na.rm=TRUE))) 
silverspring1_DOC <- data.frame(maryland_csv$id[8], (mean(silverspring1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(silverspring1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(silverspring1_obs_yearDOC$tmin, na.rm=TRUE))) 
cockeysville_DOC <- data.frame(maryland_csv$id[9], (mean(cockeysville_obs_yearDOC$prcp, na.rm=TRUE)), (mean(cockeysville_obs_yearDOC$tmax, na.rm=TRUE)), (mean(cockeysville_obs_yearDOC$tmin, na.rm=TRUE))) 
preston1_DOC <- data.frame(maryland_csv$id[10], (mean(preston1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(preston1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(preston1_obs_yearDOC$tmin, na.rm=TRUE))) 
preston2_DOC <- data.frame(maryland_csv$id[11], (mean(preston2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(preston2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(preston2_obs_yearDOC$tmin, na.rm=TRUE))) 
snowhill1_DOC <- data.frame(maryland_csv$id[12], (mean(snowhill1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(snowhill1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(snowhill1_obs_yearDOC$tmin, na.rm=TRUE))) 
townson1_DOC <- data.frame(maryland_csv$id[13], (mean(townson1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(townson1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(townson1_obs_yearDOC$tmin, na.rm=TRUE))) 
waddellscorner_DOC <- data.frame(maryland_csv$id[14], (mean(waddellscorner_obs_yearDOC$prcp, na.rm=TRUE)), (mean(waddellscorner_obs_yearDOC$tmax, na.rm=TRUE)), (mean(waddellscorner_obs_yearDOC$tmin, na.rm=TRUE))) 
uppermarlboro_DOC <- data.frame(maryland_csv$id[15], (mean(uppermarlboro_obs_yearDOC$prcp, na.rm=TRUE)), (mean(uppermarlboro_obs_yearDOC$tmax, na.rm=TRUE)), (mean(uppermarlboro_obs_yearDOC$tmin, na.rm=TRUE))) 
harmony_DOC <- data.frame(maryland_csv$id[16], (mean(harmony_obs_yearDOC$prcp, na.rm=TRUE)), (mean(harmony_obs_yearDOC$tmax, na.rm=TRUE)), (mean(harmony_obs_yearDOC$tmin, na.rm=TRUE))) 
owingmills_DOC <- data.frame(maryland_csv$id[17], (mean(owingmills_obs_yearDOC$prcp, na.rm=TRUE)), (mean(owingmills_obs_yearDOC$tmax, na.rm=TRUE)), (mean(owingmills_obs_yearDOC$tmin, na.rm=TRUE))) 
eastnewmarket_DOC <- data.frame(maryland_csv$id[18], (mean(eastnewmarket_obs_yearDOC$prcp, na.rm=TRUE)), (mean(eastnewmarket_obs_yearDOC$tmax, na.rm=TRUE)), (mean(eastnewmarket_obs_yearDOC$tmin, na.rm=TRUE))) 
collegepark_DOC <- data.frame(maryland_csv$id[19], (mean(collegepark_obs_yearDOC$prcp, na.rm=TRUE)), (mean(collegepark_obs_yearDOC$tmax, na.rm=TRUE)), (mean(collegepark_obs_yearDOC$tmin, na.rm=TRUE))) 
townson2_DOC <- data.frame(maryland_csv$id[20], (mean(townson2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(townson2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(townson2_obs_yearDOC$tmin, na.rm=TRUE))) 
snowhill2_DOC <- data.frame(maryland_csv$id[21], (mean(snowhill2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(snowhill2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(snowhill2_obs_yearDOC$tmin, na.rm=TRUE))) 
reistertown_DOC <- data.frame(maryland_csv$id[22], (mean(reistertown_obs_yearDOC$prcp, na.rm=TRUE)), (mean(reistertown_obs_yearDOC$tmax, na.rm=TRUE)), (mean(reistertown_obs_yearDOC$tmin, na.rm=TRUE))) 
mandelasprings_DOC <- data.frame(maryland_csv$id[24], (mean(mandelasprings_obs_yearDOC$prcp, na.rm=TRUE)), (mean(mandelasprings_obs_yearDOC$tmax, na.rm=TRUE)), (mean(mandelasprings_obs_yearDOC$tmin, na.rm=TRUE))) 
lilypons_DOC <- data.frame(maryland_csv$id[25], (mean(lilypons_obs_yearDOC$prcp, na.rm=TRUE)), (mean(lilypons_obs_yearDOC$tmax, na.rm=TRUE)), (mean(lilypons_obs_yearDOC$tmin, na.rm=TRUE))) 
townson3_DOC <- data.frame(maryland_csv$id[26], (mean(townson3_obs_yearDOC$prcp, na.rm=TRUE)), (mean(townson3_obs_yearDOC$tmax, na.rm=TRUE)), (mean(townson3_obs_yearDOC$tmin, na.rm=TRUE))) 
newcomb_DOC <- data.frame(maryland_csv$id[27], (mean(newcomb_obs_yearDOC$prcp, na.rm=TRUE)), (mean(newcomb_obs_yearDOC$tmax, na.rm=TRUE)), (mean(newcomb_obs_yearDOC$tmin, na.rm=TRUE))) 
baltimorecounty_DOC <- data.frame(maryland_csv$id[28], (mean(baltimorecounty_obs_yearDOC$prcp, na.rm=TRUE)), (mean(baltimorecounty_obs_yearDOC$tmax, na.rm=TRUE)), (mean(baltimorecounty_obs_yearDOC$tmin, na.rm=TRUE))) 
timomium_DOC <- data.frame(maryland_csv$id[29], (mean(timomium_obs_yearDOC$prcp, na.rm=TRUE)), (mean(timomium_obs_yearDOC$tmax, na.rm=TRUE)), (mean(timomium_obs_yearDOC$tmin, na.rm=TRUE))) 
laplata_DOC <- data.frame(maryland_csv$id[30], (mean(laplata_obs_yearDOC$prcp, na.rm=TRUE)), (mean(laplata_obs_yearDOC$tmax, na.rm=TRUE)), (mean(laplata_obs_yearDOC$tmin, na.rm=TRUE))) 
rosedale_DOC <- data.frame(maryland_csv$id[31], (mean(rosedale_obs_yearDOC$prcp, na.rm=TRUE)), (mean(rosedale_obs_yearDOC$tmax, na.rm=TRUE)), (mean(rosedale_obs_yearDOC$tmin, na.rm=TRUE))) 
thumont_DOC <- data.frame(maryland_csv$id[33], (mean(thumont_obs_yearDOC$prcp, na.rm=TRUE)), (mean(thumont_obs_yearDOC$tmax, na.rm=TRUE)), (mean(thumont_obs_yearDOC$tmin, na.rm=TRUE))) 
kennedyville_DOC <- data.frame(maryland_csv$id[34], (mean(kennedyville_obs_yearDOC$prcp, na.rm=TRUE)), (mean(kennedyville_obs_yearDOC$tmax, na.rm=TRUE)), (mean(kennedyville_obs_yearDOC$tmin, na.rm=TRUE))) 
silverspring2_DOC <- data.frame(maryland_csv$id[35], (mean(silverspring2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(silverspring2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(silverspring2_obs_yearDOC$tmin, na.rm=TRUE))) 
northbesthesda_DOC <- data.frame(maryland_csv$id[36], (mean(northbesthesda_obs_yearDOC$prcp, na.rm=TRUE)), (mean(northbesthesda_obs_yearDOC$tmax, na.rm=TRUE)), (mean(northbesthesda_obs_yearDOC$tmin, na.rm=TRUE))) 
gaithersburg_DOC <- data.frame(maryland_csv$id[37], (mean(gaithersburg_obs_yearDOC$prcp, na.rm=TRUE)), (mean(gaithersburg_obs_yearDOC$tmax, na.rm=TRUE)), (mean(gaithersburg_obs_yearDOC$tmin, na.rm=TRUE)))

#rename col.names 
cat(citySwap3(city2, y = "colnames(", z = "_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')"), sep = "\n")

colnames(baltimorecounty_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(carney1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(carney2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(carney3_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(cockeysville_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(owingmills_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(phoenix_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(reistertown_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(rosedale_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(timomium_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(townson1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(townson2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(townson3_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(harmony_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(preston1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(preston2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(laplata_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(eastnewmarket_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(galestown_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(waddellscorner_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(lilypons_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(thumont_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
#colnames(ellicottcity_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(kennedyville_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
#colnames(ashland_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(gaithersburg_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(northbesthesda_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(silverspring1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(silverspring2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(accokeek_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(collegepark_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(uppermarlboro_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(easton_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(newcomb_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(mandelasprings_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(snowhill1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(snowhill2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')

## combine current year of the date of collection data using bind_rows
cat(citySwap7(city2, y = "_DOC", z = ","), sep = "")
maryland_yearDOC <- bind_rows(phoenix_DOC,carney1_DOC,carney2_DOC,carney3_DOC,accokeek_DOC,galestown_DOC,easton_DOC,silverspring1_DOC,cockeysville_DOC,preston1_DOC,preston2_DOC,snowhill1_DOC,townson1_DOC,waddellscorner_DOC,uppermarlboro_DOC,harmony_DOC,owingmills_DOC,eastnewmarket_DOC,collegepark_DOC,townson2_DOC,snowhill2_DOC,reistertown_DOC,mandelasprings_DOC,lilypons_DOC,townson3_DOC,newcomb_DOC,baltimorecounty_DOC,timomium_DOC,laplata_DOC,rosedale_DOC,thumont_DOC,kennedyville_DOC,silverspring2_DOC,northbesthesda_DOC,gaithersburg_DOC)

# save data
write.xlsx(maryland_yearDOC, file = "maryland_year_DOC_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


