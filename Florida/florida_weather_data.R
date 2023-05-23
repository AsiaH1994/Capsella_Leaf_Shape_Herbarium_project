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
FL_csv <- read.csv('florida_cities.csv')
FL_csv$month <- match(FL_csv$month, month.name)
FL_csv$date <- paste(FL_csv$year, FL_csv$month, FL_csv$day, sep="-")
strptime(FL_csv$date,format="%Y-%m-%d")
FL_csv$date <- as.Date(FL_csv$date)
colnames(FL_csv) <- c("id", 'id2', 'city_county_state', 'latitude', 'longitude', 'elevation', 'month', 'day', 'year', 'date')
FL_csv$previous6mon <- as.Date(as.yearmon(as.Date(FL_csv$date)) -.5, frac = 1)
head(FL_csv)

## to use lapply 
city <- FL_csv$id
city2 <- FL_csv$id2
date <- FL_csv$date
pyear <- (FL_csv$date - 365)
p6mon <- FL_csv$previous6mon
year <- FL_csv$year
lennumber <- (1:33) #number of samples

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

South_stations <- stations %>% filter(state == c("FL", "GA", "AL"))
write.csv2(South_stations, "South_stations.csv")

#after running the 1st time 
#region_stations <- read.csv("great_lakes_stations.csv")


#find closests stations
FL_clst <- meteo_nearby_stations(lat_lon_df = FL_csv, station_data = South_stations, radius = 50, limit = 200)

### separate all cities 
cat(citySwap(unique(city), y = " <- (FL_clst$'", z = "')"), sep = "\n")

tallahassee <- (FL_clst$'tallahassee')
homestead <- (FL_clst$'homestead')
monticello <- (FL_clst$'monticello')
plantcity <- (FL_clst$'plantcity')
pensacola <- (FL_clst$'pensacola')
gainesville <- (FL_clst$'gainesville')
staugustine <- (FL_clst$'staugustine')
marianna <- (FL_clst$'marianna')
blountstown <- (FL_clst$'blountstown')
highsprings <- (FL_clst$'highsprings')
milton <- (FL_clst$'milton')
jacksonville <- (FL_clst$'jacksonville')

### separate all station id's (monitors) for each city
cat(citySwap(unique(city), y = "_monitors <- ", z = "$id"), sep = "\n")

tallahassee_monitors <- tallahassee$id
homestead_monitors <- homestead$id
monticello_monitors <- monticello$id
plantcity_monitors <- plantcity$id
pensacola_monitors <- pensacola$id
gainesville_monitors <- gainesville$id
staugustine_monitors <- staugustine$id
marianna_monitors <- marianna$id
blountstown_monitors <- blountstown$id
highsprings_monitors <- highsprings$id
milton_monitors <- milton$id
jacksonville_monitors <- jacksonville$id

### pull all observations for cities 
cat(citySwap(unique(city), y = "_obs <- meteo_pull_monitors(", z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')"), sep = "\n")

tallahassee_obs <- meteo_pull_monitors(tallahassee_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
homestead_obs <- meteo_pull_monitors(homestead_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
monticello_obs <- meteo_pull_monitors(monticello_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
plantcity_obs <- meteo_pull_monitors(plantcity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
pensacola_obs <- meteo_pull_monitors(pensacola_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
gainesville_obs <- meteo_pull_monitors(gainesville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
staugustine_obs <- meteo_pull_monitors(staugustine_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
marianna_obs <- meteo_pull_monitors(marianna_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
blountstown_obs <- meteo_pull_monitors(blountstown_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
highsprings_obs <- meteo_pull_monitors(highsprings_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
milton_obs <- meteo_pull_monitors(milton_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
jacksonville_obs <- meteo_pull_monitors(jacksonville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')

### Part 1
### pull data by date and location - dates are coming from state_csv 
lapply(df1, cat(citySwap4.5(x = city2, y = "_obs_temps <- ", i = city, z = "_obs %>% filter(date == '", j = date , h = "')"), sep = "\n"))

tallahassee1_obs_temps <- tallahassee_obs %>% filter(date == '1957-03-14')
homestead1_obs_temps <- homestead_obs %>% filter(date == '1997-12-02')
tallahassee2_obs_temps <- tallahassee_obs %>% filter(date == '1957-03-14')
monticello_obs_temps <- monticello_obs %>% filter(date == '1958-03-24')
tallahassee3_obs_temps <- tallahassee_obs %>% filter(date == '1967-03-02')
tallahassee4_obs_temps <- tallahassee_obs %>% filter(date == '1957-03-14')
tallahassee5_obs_temps <- tallahassee_obs %>% filter(date == '1957-03-14')
plantcity_obs_temps <- plantcity_obs %>% filter(date == '1989-04-04')
pensacola1_obs_temps <- pensacola_obs %>% filter(date == '1975-01-30')
gainesville1_obs_temps <- gainesville_obs %>% filter(date == '1967-01-03')
staugustine_obs_temps <- staugustine_obs %>% filter(date == '2004-04-14')
tallahassee6_obs_temps <- tallahassee_obs %>% filter(date == '1957-03-14')
pensacola2_obs_temps <- pensacola_obs %>% filter(date == '1977-02-19')
gainesville2_obs_temps <- gainesville_obs %>% filter(date == '1979-03-03')
marianna1_obs_temps <- marianna_obs %>% filter(date == '1957-02-09')
homestead2_obs_temps <- homestead_obs %>% filter(date == '1997-11-02')
pensacola3_obs_temps <- pensacola_obs %>% filter(date == '1992-03-01')
gainesville3_obs_temps <- gainesville_obs %>% filter(date == '2002-03-13')
homestead3_obs_temps <- homestead_obs %>% filter(date == '1997-12-02')
pensacola4_obs_temps <- pensacola_obs %>% filter(date == '2001-04-02')
homestead4_obs_temps <- homestead_obs %>% filter(date == '1997-11-02')
blountstown_obs_temps <- blountstown_obs %>% filter(date == '2008-02-24')
pensacola5_obs_temps <- pensacola_obs %>% filter(date == '2011-03-23')
marianna2_obs_temps <- marianna_obs %>% filter(date == '1957-02-09')
pensacola6_obs_temps <- pensacola_obs %>% filter(date == '2002-12-20')
highsprings_obs_temps <- highsprings_obs %>% filter(date == '2013-03-02')
pensacola7_obs_temps <- pensacola_obs %>% filter(date == '1999-02-28')
pensacola8_obs_temps <- pensacola_obs %>% filter(date == '1996-03-17')
pensacola9_obs_temps <- pensacola_obs %>% filter(date == '1990-02-17')
pensacola10_obs_temps <- pensacola_obs %>% filter(date == '1989-01-29')
pensacola11_obs_temps <- pensacola_obs %>% filter(date == '1986-03-03')
milton_obs_temps <- milton_obs %>% filter(date == '1990-02-24')
jacksonville_obs_temps <- jacksonville_obs %>% filter(date == '2003-02-09')

#combine into one data frame using bind_rows()
cat(citySwap7(city2, y = "_obs_temps", z = ","), sep = "")
FL_obs <- bind_rows(tallahassee1_obs_temps,homestead1_obs_temps,tallahassee2_obs_temps,monticello_obs_temps,tallahassee3_obs_temps,tallahassee4_obs_temps,tallahassee5_obs_temps,plantcity_obs_temps,pensacola1_obs_temps,gainesville1_obs_temps,staugustine_obs_temps,tallahassee6_obs_temps,pensacola2_obs_temps,gainesville2_obs_temps,marianna1_obs_temps,homestead2_obs_temps,pensacola3_obs_temps,gainesville3_obs_temps,homestead3_obs_temps,pensacola4_obs_temps,homestead4_obs_temps,blountstown_obs_temps,pensacola5_obs_temps,marianna2_obs_temps,pensacola6_obs_temps,highsprings_obs_temps,pensacola7_obs_temps,pensacola8_obs_temps,pensacola9_obs_temps,pensacola10_obs_temps,pensacola11_obs_temps,milton_obs_temps,jacksonville_obs_temps)

#Merge the original state_csv dataframe and new state_obs dataframe 
FL_data <- merge.data.frame(FL_csv, FL_obs, by = 'date')

## find the tavg for state data 
FL_data$tavg <- ((FL_data$tmax + FL_data$tmin) / 2)

### save data as xlsx file 
library("xlsx")
write.xlsx(FL_data, file = "FL_data_total_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

### Part 2
#finding data from 6 months prior to date of collection
#ambient temps and precipitation may have affected plant/leaf growth and shape
lapply(df2, cat(citySwap5(x = city2, y = "_obs_6months <- meteo_pull_monitors(", j = city, 
                          z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '", h = p6mon, 
                          k = "', date_max = '", l = date, p ="')"), sep = "\n"))

tallahassee1_obs_6months <- meteo_pull_monitors(tallahassee_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1956-09-30', date_max = '1957-03-14')
homestead1_obs_6months <- meteo_pull_monitors(homestead_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1997-06-30', date_max = '1997-12-02')
tallahassee2_obs_6months <- meteo_pull_monitors(tallahassee_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1956-09-30', date_max = '1957-03-14')
monticello_obs_6months <- meteo_pull_monitors(monticello_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1957-09-30', date_max = '1958-03-24')
tallahassee3_obs_6months <- meteo_pull_monitors(tallahassee_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1966-09-30', date_max = '1967-03-02')
tallahassee4_obs_6months <- meteo_pull_monitors(tallahassee_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1956-09-30', date_max = '1957-03-14')
tallahassee5_obs_6months <- meteo_pull_monitors(tallahassee_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1956-09-30', date_max = '1957-03-14')
plantcity_obs_6months <- meteo_pull_monitors(plantcity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1988-10-31', date_max = '1989-04-04')
pensacola1_obs_6months <- meteo_pull_monitors(pensacola_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1974-07-31', date_max = '1975-01-30')
gainesville1_obs_6months <- meteo_pull_monitors(gainesville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1966-07-31', date_max = '1967-01-03')
staugustine_obs_6months <- meteo_pull_monitors(staugustine_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2003-10-31', date_max = '2004-04-14')
tallahassee6_obs_6months <- meteo_pull_monitors(tallahassee_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1956-09-30', date_max = '1957-03-14')
pensacola2_obs_6months <- meteo_pull_monitors(pensacola_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1976-08-31', date_max = '1977-02-19')
gainesville2_obs_6months <- meteo_pull_monitors(gainesville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1978-09-30', date_max = '1979-03-03')
marianna1_obs_6months <- meteo_pull_monitors(marianna_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1956-08-31', date_max = '1957-02-09')
homestead2_obs_6months <- meteo_pull_monitors(homestead_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1997-05-31', date_max = '1997-11-02')
pensacola3_obs_6months <- meteo_pull_monitors(pensacola_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1991-09-30', date_max = '1992-03-01')
gainesville3_obs_6months <- meteo_pull_monitors(gainesville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2001-09-30', date_max = '2002-03-13')
homestead3_obs_6months <- meteo_pull_monitors(homestead_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1997-06-30', date_max = '1997-12-02')
pensacola4_obs_6months <- meteo_pull_monitors(pensacola_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2000-10-31', date_max = '2001-04-02')
homestead4_obs_6months <- meteo_pull_monitors(homestead_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1997-05-31', date_max = '1997-11-02')
blountstown_obs_6months <- meteo_pull_monitors(blountstown_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2007-08-31', date_max = '2008-02-24')
pensacola5_obs_6months <- meteo_pull_monitors(pensacola_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2010-09-30', date_max = '2011-03-23')
marianna2_obs_6months <- meteo_pull_monitors(marianna_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1956-08-31', date_max = '1957-02-09')
pensacola6_obs_6months <- meteo_pull_monitors(pensacola_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2002-06-30', date_max = '2002-12-20')
highsprings_obs_6months <- meteo_pull_monitors(highsprings_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2012-09-30', date_max = '2013-03-02')
pensacola7_obs_6months <- meteo_pull_monitors(pensacola_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1998-08-31', date_max = '1999-02-28')
pensacola8_obs_6months <- meteo_pull_monitors(pensacola_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1995-09-30', date_max = '1996-03-17')
pensacola9_obs_6months <- meteo_pull_monitors(pensacola_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1989-08-31', date_max = '1990-02-17')
pensacola10_obs_6months <- meteo_pull_monitors(pensacola_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1988-07-31', date_max = '1989-01-29')
pensacola11_obs_6months <- meteo_pull_monitors(pensacola_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1985-09-30', date_max = '1986-03-03')
milton_obs_6months <- meteo_pull_monitors(milton_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1989-08-31', date_max = '1990-02-24')
jacksonville_obs_6months <- meteo_pull_monitors(jacksonville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2002-08-31', date_max = '2003-02-09')

## make a dataframe of the mean of tmax, tmin, prcp of the previous 6 months 
lapply(df3, cat(citySwap2(x = city2,  y = "_6months", z = " <- data.frame(FL_csv$id[", l= lennumber, k = "], (mean(", e = "_obs_6months$prcp, na.rm=TRUE)), (mean(", f = "_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(", g = "_obs_6months$tmin, na.rm=TRUE))) "), sep = "\n"))

tallahassee1_6months <- data.frame(FL_csv$id[1], (mean(tallahassee1_obs_6months$prcp, na.rm=TRUE)), (mean(tallahassee1_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(tallahassee1_obs_6months$tmin, na.rm=TRUE))) 
homestead1_6months <- data.frame(FL_csv$id[2], (mean(homestead1_obs_6months$prcp, na.rm=TRUE)), (mean(homestead1_obs_6months$tmax, na.rm=TRUE)), 
                                 (mean(homestead1_obs_6months$tmin, na.rm=TRUE))) 
tallahassee2_6months <- data.frame(FL_csv$id[3], (mean(tallahassee2_obs_6months$prcp, na.rm=TRUE)), (mean(tallahassee2_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(tallahassee2_obs_6months$tmin, na.rm=TRUE))) 
monticello_6months <- data.frame(FL_csv$id[4], (mean(monticello_obs_6months$prcp, na.rm=TRUE)), (mean(monticello_obs_6months$tmax, na.rm=TRUE)), 
                                 (mean(monticello_obs_6months$tmin, na.rm=TRUE))) 
tallahassee3_6months <- data.frame(FL_csv$id[5], (mean(tallahassee3_obs_6months$prcp, na.rm=TRUE)), (mean(tallahassee3_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(tallahassee3_obs_6months$tmin, na.rm=TRUE))) 
tallahassee4_6months <- data.frame(FL_csv$id[6], (mean(tallahassee4_obs_6months$prcp, na.rm=TRUE)), (mean(tallahassee4_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(tallahassee4_obs_6months$tmin, na.rm=TRUE))) 
tallahassee5_6months <- data.frame(FL_csv$id[7], (mean(tallahassee5_obs_6months$prcp, na.rm=TRUE)), (mean(tallahassee5_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(tallahassee5_obs_6months$tmin, na.rm=TRUE))) 
plantcity_6months <- data.frame(FL_csv$id[8], (mean(plantcity_obs_6months$prcp, na.rm=TRUE)), (mean(plantcity_obs_6months$tmax, na.rm=TRUE)), 
                                (mean(plantcity_obs_6months$tmin, na.rm=TRUE))) 
pensacola1_6months <- data.frame(FL_csv$id[9], (mean(pensacola1_obs_6months$prcp, na.rm=TRUE)), (mean(pensacola1_obs_6months$tmax, na.rm=TRUE)), 
                                 (mean(pensacola1_obs_6months$tmin, na.rm=TRUE))) 
gainesville1_6months <- data.frame(FL_csv$id[10], (mean(gainesville1_obs_6months$prcp, na.rm=TRUE)), (mean(gainesville1_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(gainesville1_obs_6months$tmin, na.rm=TRUE))) 
staugustine_6months <- data.frame(FL_csv$id[11], (mean(staugustine_obs_6months$prcp, na.rm=TRUE)), (mean(staugustine_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(staugustine_obs_6months$tmin, na.rm=TRUE))) 
tallahassee6_6months <- data.frame(FL_csv$id[12], (mean(tallahassee6_obs_6months$prcp, na.rm=TRUE)), (mean(tallahassee6_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(tallahassee6_obs_6months$tmin, na.rm=TRUE))) 
pensacola2_6months <- data.frame(FL_csv$id[13], (mean(pensacola2_obs_6months$prcp, na.rm=TRUE)), (mean(pensacola2_obs_6months$tmax, na.rm=TRUE)), 
                                 (mean(pensacola2_obs_6months$tmin, na.rm=TRUE))) 
gainesville2_6months <- data.frame(FL_csv$id[14], (mean(gainesville2_obs_6months$prcp, na.rm=TRUE)), (mean(gainesville2_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(gainesville2_obs_6months$tmin, na.rm=TRUE))) 
marianna1_6months <- data.frame(FL_csv$id[15], (mean(marianna1_obs_6months$prcp, na.rm=TRUE)), (mean(marianna1_obs_6months$tmax, na.rm=TRUE)), 
                                (mean(marianna1_obs_6months$tmin, na.rm=TRUE))) 
homestead2_6months <- data.frame(FL_csv$id[16], (mean(homestead2_obs_6months$prcp, na.rm=TRUE)), (mean(homestead2_obs_6months$tmax, na.rm=TRUE)), 
                                 (mean(homestead2_obs_6months$tmin, na.rm=TRUE))) 
pensacola3_6months <- data.frame(FL_csv$id[17], (mean(pensacola3_obs_6months$prcp, na.rm=TRUE)), (mean(pensacola3_obs_6months$tmax, na.rm=TRUE)), 
                                 (mean(pensacola3_obs_6months$tmin, na.rm=TRUE))) 
gainesville3_6months <- data.frame(FL_csv$id[18], (mean(gainesville3_obs_6months$prcp, na.rm=TRUE)), (mean(gainesville3_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(gainesville3_obs_6months$tmin, na.rm=TRUE))) 
homestead3_6months <- data.frame(FL_csv$id[19], (mean(homestead3_obs_6months$prcp, na.rm=TRUE)), (mean(homestead3_obs_6months$tmax, na.rm=TRUE)), 
                                 (mean(homestead3_obs_6months$tmin, na.rm=TRUE))) 
pensacola4_6months <- data.frame(FL_csv$id[20], (mean(pensacola4_obs_6months$prcp, na.rm=TRUE)), (mean(pensacola4_obs_6months$tmax, na.rm=TRUE)), 
                                 (mean(pensacola4_obs_6months$tmin, na.rm=TRUE))) 
homestead4_6months <- data.frame(FL_csv$id[21], (mean(homestead4_obs_6months$prcp, na.rm=TRUE)), (mean(homestead4_obs_6months$tmax, na.rm=TRUE)), 
                                 (mean(homestead4_obs_6months$tmin, na.rm=TRUE))) 
blountstown_6months <- data.frame(FL_csv$id[22], (mean(blountstown_obs_6months$prcp, na.rm=TRUE)), (mean(blountstown_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(blountstown_obs_6months$tmin, na.rm=TRUE))) 
pensacola5_6months <- data.frame(FL_csv$id[23], (mean(pensacola5_obs_6months$prcp, na.rm=TRUE)), (mean(pensacola5_obs_6months$tmax, na.rm=TRUE)), 
                                 (mean(pensacola5_obs_6months$tmin, na.rm=TRUE))) 
marianna2_6months <- data.frame(FL_csv$id[24], (mean(marianna2_obs_6months$prcp, na.rm=TRUE)), (mean(marianna2_obs_6months$tmax, na.rm=TRUE)), 
                                (mean(marianna2_obs_6months$tmin, na.rm=TRUE))) 
pensacola6_6months <- data.frame(FL_csv$id[25], (mean(pensacola6_obs_6months$prcp, na.rm=TRUE)), (mean(pensacola6_obs_6months$tmax, na.rm=TRUE)), 
                                 (mean(pensacola6_obs_6months$tmin, na.rm=TRUE))) 
highsprings_6months <- data.frame(FL_csv$id[26], (mean(highsprings_obs_6months$prcp, na.rm=TRUE)), (mean(highsprings_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(highsprings_obs_6months$tmin, na.rm=TRUE))) 
pensacola7_6months <- data.frame(FL_csv$id[27], (mean(pensacola7_obs_6months$prcp, na.rm=TRUE)), (mean(pensacola7_obs_6months$tmax, na.rm=TRUE)), 
                                 (mean(pensacola7_obs_6months$tmin, na.rm=TRUE))) 
pensacola8_6months <- data.frame(FL_csv$id[28], (mean(pensacola8_obs_6months$prcp, na.rm=TRUE)), (mean(pensacola8_obs_6months$tmax, na.rm=TRUE)), 
                                 (mean(pensacola8_obs_6months$tmin, na.rm=TRUE))) 
pensacola9_6months <- data.frame(FL_csv$id[29], (mean(pensacola9_obs_6months$prcp, na.rm=TRUE)), (mean(pensacola9_obs_6months$tmax, na.rm=TRUE)), 
                                 (mean(pensacola9_obs_6months$tmin, na.rm=TRUE))) 
pensacola10_6months <- data.frame(FL_csv$id[30], (mean(pensacola10_obs_6months$prcp, na.rm=TRUE)), (mean(pensacola10_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(pensacola10_obs_6months$tmin, na.rm=TRUE))) 
pensacola11_6months <- data.frame(FL_csv$id[31], (mean(pensacola11_obs_6months$prcp, na.rm=TRUE)), (mean(pensacola11_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(pensacola11_obs_6months$tmin, na.rm=TRUE))) 
milton_6months <- data.frame(FL_csv$id[32], (mean(milton_obs_6months$prcp, na.rm=TRUE)), (mean(milton_obs_6months$tmax, na.rm=TRUE)), 
                             (mean(milton_obs_6months$tmin, na.rm=TRUE))) 
jacksonville_6months <- data.frame(FL_csv$id[33], (mean(jacksonville_obs_6months$prcp, na.rm=TRUE)), (mean(jacksonville_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(jacksonville_obs_6months$tmin, na.rm=TRUE)))

#rename col.names 
cat(citySwap3(city2, y = "colnames(", z = "_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')"), sep = "\n")

colnames(tallahassee1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(homestead1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tallahassee2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(monticello_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tallahassee3_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tallahassee4_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tallahassee5_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(plantcity_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(pensacola1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(gainesville1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(staugustine_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tallahassee6_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(pensacola2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(gainesville2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(marianna1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(homestead2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(pensacola3_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(gainesville3_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(homestead3_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(pensacola4_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(homestead4_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(blountstown_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(pensacola5_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(marianna2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(pensacola6_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(highsprings_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(pensacola7_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(pensacola8_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(pensacola9_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(pensacola10_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(pensacola11_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(milton_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(jacksonville_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')

## combine previous 6 months data using bind_rows
cat(citySwap7(city2, y = "_6months", z = ","), sep = "")
FL_6months <- bind_rows(tallahassee1_6months,homestead1_6months,tallahassee2_6months,monticello_6months,tallahassee3_6months,tallahassee4_6months,tallahassee5_6months,plantcity_6months,pensacola1_6months,gainesville1_6months,staugustine_6months,tallahassee6_6months,pensacola2_6months,gainesville2_6months,marianna1_6months,homestead2_6months,pensacola3_6months,gainesville3_6months,homestead3_6months,pensacola4_6months,homestead4_6months,blountstown_6months,pensacola5_6months,marianna2_6months,pensacola6_6months,highsprings_6months,pensacola7_6months,pensacola8_6months,pensacola9_6months,pensacola10_6months,pensacola11_6months,milton_6months,jacksonville_6months)

### save data as xlsx file 
write.xlsx(FL_6months, file = "FL_6months_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


### Part 3
#find data from previous year from date of collection 
lapply(df4, cat(citySwap5(x = city2, y = "_obs_year <- meteo_pull_monitors(", j = city, 
                          z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '", h= pyear, 
                          k = "', date_max = '", l = date, p ="')"), sep = "\n"))

tallahassee1_obs_year <- meteo_pull_monitors(tallahassee_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1956-03-14', date_max = '1957-03-14')
homestead1_obs_year <- meteo_pull_monitors(homestead_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1996-12-02', date_max = '1997-12-02')
tallahassee2_obs_year <- meteo_pull_monitors(tallahassee_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1956-03-14', date_max = '1957-03-14')
monticello_obs_year <- meteo_pull_monitors(monticello_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1957-03-24', date_max = '1958-03-24')
tallahassee3_obs_year <- meteo_pull_monitors(tallahassee_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1966-03-02', date_max = '1967-03-02')
tallahassee4_obs_year <- meteo_pull_monitors(tallahassee_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1956-03-14', date_max = '1957-03-14')
tallahassee5_obs_year <- meteo_pull_monitors(tallahassee_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1956-03-14', date_max = '1957-03-14')
plantcity_obs_year <- meteo_pull_monitors(plantcity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1988-04-04', date_max = '1989-04-04')
pensacola1_obs_year <- meteo_pull_monitors(pensacola_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1974-01-30', date_max = '1975-01-30')
gainesville1_obs_year <- meteo_pull_monitors(gainesville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1966-01-03', date_max = '1967-01-03')
staugustine_obs_year <- meteo_pull_monitors(staugustine_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2003-04-15', date_max = '2004-04-14')
tallahassee6_obs_year <- meteo_pull_monitors(tallahassee_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1956-03-14', date_max = '1957-03-14')
pensacola2_obs_year <- meteo_pull_monitors(pensacola_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1976-02-20', date_max = '1977-02-19')
gainesville2_obs_year <- meteo_pull_monitors(gainesville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1978-03-03', date_max = '1979-03-03')
marianna1_obs_year <- meteo_pull_monitors(marianna_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1956-02-10', date_max = '1957-02-09')
homestead2_obs_year <- meteo_pull_monitors(homestead_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1996-11-02', date_max = '1997-11-02')
pensacola3_obs_year <- meteo_pull_monitors(pensacola_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1991-03-02', date_max = '1992-03-01')
gainesville3_obs_year <- meteo_pull_monitors(gainesville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2001-03-13', date_max = '2002-03-13')
homestead3_obs_year <- meteo_pull_monitors(homestead_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1996-12-02', date_max = '1997-12-02')
pensacola4_obs_year <- meteo_pull_monitors(pensacola_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2000-04-02', date_max = '2001-04-02')
homestead4_obs_year <- meteo_pull_monitors(homestead_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1996-11-02', date_max = '1997-11-02')
blountstown_obs_year <- meteo_pull_monitors(blountstown_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2007-02-24', date_max = '2008-02-24')
pensacola5_obs_year <- meteo_pull_monitors(pensacola_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2010-03-23', date_max = '2011-03-23')
marianna2_obs_year <- meteo_pull_monitors(marianna_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1956-02-10', date_max = '1957-02-09')
pensacola6_obs_year <- meteo_pull_monitors(pensacola_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2001-12-20', date_max = '2002-12-20')
highsprings_obs_year <- meteo_pull_monitors(highsprings_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2012-03-02', date_max = '2013-03-02')
pensacola7_obs_year <- meteo_pull_monitors(pensacola_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1998-02-28', date_max = '1999-02-28')
pensacola8_obs_year <- meteo_pull_monitors(pensacola_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1995-03-18', date_max = '1996-03-17')
pensacola9_obs_year <- meteo_pull_monitors(pensacola_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1989-02-17', date_max = '1990-02-17')
pensacola10_obs_year <- meteo_pull_monitors(pensacola_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1988-01-30', date_max = '1989-01-29')
pensacola11_obs_year <- meteo_pull_monitors(pensacola_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1985-03-03', date_max = '1986-03-03')
milton_obs_year <- meteo_pull_monitors(milton_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1989-02-24', date_max = '1990-02-24')
jacksonville_obs_year <- meteo_pull_monitors(jacksonville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2002-02-09', date_max = '2003-02-09')

## make a data frame of the mean of tmax, tmin, prcp of the previous year before the date of collection 
lapply(df3, cat(citySwap2(x = city2,  y = "_previousyear", z = " <- data.frame(FL_csv$id[", l= lennumber, 
                          k = "], (mean(", e = "_obs_year$prcp, na.rm=TRUE)), (mean(", 
                          f = "_obs_year$tmax, na.rm=TRUE)), (mean(", g = "_obs_year$tmin, na.rm=TRUE))) "), sep = "\n"))

tallahassee1_previousyear <- data.frame(FL_csv$id[1], (mean(tallahassee1_obs_year$prcp, na.rm=TRUE)), (mean(tallahassee1_obs_year$tmax, na.rm=TRUE)), (mean(tallahassee1_obs_year$tmin, na.rm=TRUE))) 
homestead1_previousyear <- data.frame(FL_csv$id[2], (mean(homestead1_obs_year$prcp, na.rm=TRUE)), (mean(homestead1_obs_year$tmax, na.rm=TRUE)), (mean(homestead1_obs_year$tmin, na.rm=TRUE))) 
tallahassee2_previousyear <- data.frame(FL_csv$id[3], (mean(tallahassee2_obs_year$prcp, na.rm=TRUE)), (mean(tallahassee2_obs_year$tmax, na.rm=TRUE)), (mean(tallahassee2_obs_year$tmin, na.rm=TRUE))) 
monticello_previousyear <- data.frame(FL_csv$id[4], (mean(monticello_obs_year$prcp, na.rm=TRUE)), (mean(monticello_obs_year$tmax, na.rm=TRUE)), (mean(monticello_obs_year$tmin, na.rm=TRUE))) 
tallahassee3_previousyear <- data.frame(FL_csv$id[5], (mean(tallahassee3_obs_year$prcp, na.rm=TRUE)), (mean(tallahassee3_obs_year$tmax, na.rm=TRUE)), (mean(tallahassee3_obs_year$tmin, na.rm=TRUE))) 
tallahassee4_previousyear <- data.frame(FL_csv$id[6], (mean(tallahassee4_obs_year$prcp, na.rm=TRUE)), (mean(tallahassee4_obs_year$tmax, na.rm=TRUE)), (mean(tallahassee4_obs_year$tmin, na.rm=TRUE))) 
tallahassee5_previousyear <- data.frame(FL_csv$id[7], (mean(tallahassee5_obs_year$prcp, na.rm=TRUE)), (mean(tallahassee5_obs_year$tmax, na.rm=TRUE)), (mean(tallahassee5_obs_year$tmin, na.rm=TRUE))) 
plantcity_previousyear <- data.frame(FL_csv$id[8], (mean(plantcity_obs_year$prcp, na.rm=TRUE)), (mean(plantcity_obs_year$tmax, na.rm=TRUE)), (mean(plantcity_obs_year$tmin, na.rm=TRUE))) 
pensacola1_previousyear <- data.frame(FL_csv$id[9], (mean(pensacola1_obs_year$prcp, na.rm=TRUE)), (mean(pensacola1_obs_year$tmax, na.rm=TRUE)), (mean(pensacola1_obs_year$tmin, na.rm=TRUE))) 
gainesville1_previousyear <- data.frame(FL_csv$id[10], (mean(gainesville1_obs_year$prcp, na.rm=TRUE)), (mean(gainesville1_obs_year$tmax, na.rm=TRUE)), (mean(gainesville1_obs_year$tmin, na.rm=TRUE))) 
staugustine_previousyear <- data.frame(FL_csv$id[11], (mean(staugustine_obs_year$prcp, na.rm=TRUE)), (mean(staugustine_obs_year$tmax, na.rm=TRUE)), (mean(staugustine_obs_year$tmin, na.rm=TRUE))) 
tallahassee6_previousyear <- data.frame(FL_csv$id[12], (mean(tallahassee6_obs_year$prcp, na.rm=TRUE)), (mean(tallahassee6_obs_year$tmax, na.rm=TRUE)), (mean(tallahassee6_obs_year$tmin, na.rm=TRUE))) 
pensacola2_previousyear <- data.frame(FL_csv$id[13], (mean(pensacola2_obs_year$prcp, na.rm=TRUE)), (mean(pensacola2_obs_year$tmax, na.rm=TRUE)), (mean(pensacola2_obs_year$tmin, na.rm=TRUE))) 
gainesville2_previousyear <- data.frame(FL_csv$id[14], (mean(gainesville2_obs_year$prcp, na.rm=TRUE)), (mean(gainesville2_obs_year$tmax, na.rm=TRUE)), (mean(gainesville2_obs_year$tmin, na.rm=TRUE))) 
marianna1_previousyear <- data.frame(FL_csv$id[15], (mean(marianna1_obs_year$prcp, na.rm=TRUE)), (mean(marianna1_obs_year$tmax, na.rm=TRUE)), (mean(marianna1_obs_year$tmin, na.rm=TRUE))) 
homestead2_previousyear <- data.frame(FL_csv$id[16], (mean(homestead2_obs_year$prcp, na.rm=TRUE)), (mean(homestead2_obs_year$tmax, na.rm=TRUE)), (mean(homestead2_obs_year$tmin, na.rm=TRUE))) 
pensacola3_previousyear <- data.frame(FL_csv$id[17], (mean(pensacola3_obs_year$prcp, na.rm=TRUE)), (mean(pensacola3_obs_year$tmax, na.rm=TRUE)), (mean(pensacola3_obs_year$tmin, na.rm=TRUE))) 
gainesville3_previousyear <- data.frame(FL_csv$id[18], (mean(gainesville3_obs_year$prcp, na.rm=TRUE)), (mean(gainesville3_obs_year$tmax, na.rm=TRUE)), (mean(gainesville3_obs_year$tmin, na.rm=TRUE))) 
homestead3_previousyear <- data.frame(FL_csv$id[19], (mean(homestead3_obs_year$prcp, na.rm=TRUE)), (mean(homestead3_obs_year$tmax, na.rm=TRUE)), (mean(homestead3_obs_year$tmin, na.rm=TRUE))) 
pensacola4_previousyear <- data.frame(FL_csv$id[20], (mean(pensacola4_obs_year$prcp, na.rm=TRUE)), (mean(pensacola4_obs_year$tmax, na.rm=TRUE)), (mean(pensacola4_obs_year$tmin, na.rm=TRUE))) 
homestead4_previousyear <- data.frame(FL_csv$id[21], (mean(homestead4_obs_year$prcp, na.rm=TRUE)), (mean(homestead4_obs_year$tmax, na.rm=TRUE)), (mean(homestead4_obs_year$tmin, na.rm=TRUE))) 
blountstown_previousyear <- data.frame(FL_csv$id[22], (mean(blountstown_obs_year$prcp, na.rm=TRUE)), (mean(blountstown_obs_year$tmax, na.rm=TRUE)), (mean(blountstown_obs_year$tmin, na.rm=TRUE))) 
pensacola5_previousyear <- data.frame(FL_csv$id[23], (mean(pensacola5_obs_year$prcp, na.rm=TRUE)), (mean(pensacola5_obs_year$tmax, na.rm=TRUE)), (mean(pensacola5_obs_year$tmin, na.rm=TRUE))) 
marianna2_previousyear <- data.frame(FL_csv$id[24], (mean(marianna2_obs_year$prcp, na.rm=TRUE)), (mean(marianna2_obs_year$tmax, na.rm=TRUE)), (mean(marianna2_obs_year$tmin, na.rm=TRUE))) 
pensacola6_previousyear <- data.frame(FL_csv$id[25], (mean(pensacola6_obs_year$prcp, na.rm=TRUE)), (mean(pensacola6_obs_year$tmax, na.rm=TRUE)), (mean(pensacola6_obs_year$tmin, na.rm=TRUE))) 
highsprings_previousyear <- data.frame(FL_csv$id[26], (mean(highsprings_obs_year$prcp, na.rm=TRUE)), (mean(highsprings_obs_year$tmax, na.rm=TRUE)), (mean(highsprings_obs_year$tmin, na.rm=TRUE))) 
pensacola7_previousyear <- data.frame(FL_csv$id[27], (mean(pensacola7_obs_year$prcp, na.rm=TRUE)), (mean(pensacola7_obs_year$tmax, na.rm=TRUE)), (mean(pensacola7_obs_year$tmin, na.rm=TRUE))) 
pensacola8_previousyear <- data.frame(FL_csv$id[28], (mean(pensacola8_obs_year$prcp, na.rm=TRUE)), (mean(pensacola8_obs_year$tmax, na.rm=TRUE)), (mean(pensacola8_obs_year$tmin, na.rm=TRUE))) 
pensacola9_previousyear <- data.frame(FL_csv$id[29], (mean(pensacola9_obs_year$prcp, na.rm=TRUE)), (mean(pensacola9_obs_year$tmax, na.rm=TRUE)), (mean(pensacola9_obs_year$tmin, na.rm=TRUE))) 
pensacola10_previousyear <- data.frame(FL_csv$id[30], (mean(pensacola10_obs_year$prcp, na.rm=TRUE)), (mean(pensacola10_obs_year$tmax, na.rm=TRUE)), (mean(pensacola10_obs_year$tmin, na.rm=TRUE))) 
pensacola11_previousyear <- data.frame(FL_csv$id[31], (mean(pensacola11_obs_year$prcp, na.rm=TRUE)), (mean(pensacola11_obs_year$tmax, na.rm=TRUE)), (mean(pensacola11_obs_year$tmin, na.rm=TRUE))) 
milton_previousyear <- data.frame(FL_csv$id[32], (mean(milton_obs_year$prcp, na.rm=TRUE)), (mean(milton_obs_year$tmax, na.rm=TRUE)), (mean(milton_obs_year$tmin, na.rm=TRUE))) 
jacksonville_previousyear <- data.frame(FL_csv$id[33], (mean(jacksonville_obs_year$prcp, na.rm=TRUE)), (mean(jacksonville_obs_year$tmax, na.rm=TRUE)), (mean(jacksonville_obs_year$tmin, na.rm=TRUE)))

#rename col.names 
cat(citySwap3(city2, y = "colnames(", z = "_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')"), sep = "\n")

colnames(tallahassee1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(homestead1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tallahassee2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(monticello_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tallahassee3_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tallahassee4_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tallahassee5_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(plantcity_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(pensacola1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(gainesville1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(staugustine_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tallahassee6_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(pensacola2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(gainesville2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(marianna1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(homestead2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(pensacola3_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(gainesville3_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(homestead3_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(pensacola4_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(homestead4_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(blountstown_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(pensacola5_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(marianna2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(pensacola6_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(highsprings_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(pensacola7_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(pensacola8_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(pensacola9_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(pensacola10_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(pensacola11_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(milton_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(jacksonville_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')

## combine previous year before the date of collection data using bind_rows
cat(citySwap7(city2, y = "_previousyear", z = ","), sep = "")
FL_Pyear <- bind_rows(tallahassee1_previousyear,homestead1_previousyear,tallahassee2_previousyear,monticello_previousyear,tallahassee3_previousyear,tallahassee4_previousyear,tallahassee5_previousyear,plantcity_previousyear,pensacola1_previousyear,gainesville1_previousyear,staugustine_previousyear,tallahassee6_previousyear,pensacola2_previousyear,gainesville2_previousyear,marianna1_previousyear,homestead2_previousyear,pensacola3_previousyear,gainesville3_previousyear,homestead3_previousyear,pensacola4_previousyear,homestead4_previousyear,blountstown_previousyear,pensacola5_previousyear,marianna2_previousyear,pensacola6_previousyear,highsprings_previousyear,pensacola7_previousyear,pensacola8_previousyear,pensacola9_previousyear,pensacola10_previousyear,pensacola11_previousyear,milton_previousyear,jacksonville_previousyear)

# save data
write.xlsx(FL_Pyear, file = "FL_previous_year_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


### Part 4 
#Find data from current year of date of collection 
lapply(df5, cat(citySwap6(x = city2, y = "_obs_yearDOC <- meteo_pull_monitors(", j = city, 
                          z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '", l = year , 
                          k = "-01-01', date_max = '",  p ="-12-31')"), sep = "\n"))

tallahassee1_obs_yearDOC <- meteo_pull_monitors(tallahassee_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1957-01-01', date_max = '1957-12-31')
homestead1_obs_yearDOC <- meteo_pull_monitors(homestead_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1997-01-01', date_max = '1997-12-31')
tallahassee2_obs_yearDOC <- meteo_pull_monitors(tallahassee_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1957-01-01', date_max = '1957-12-31')
monticello_obs_yearDOC <- meteo_pull_monitors(monticello_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1958-01-01', date_max = '1958-12-31')
tallahassee3_obs_yearDOC <- meteo_pull_monitors(tallahassee_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1967-01-01', date_max = '1967-12-31')
tallahassee4_obs_yearDOC <- meteo_pull_monitors(tallahassee_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1957-01-01', date_max = '1957-12-31')
tallahassee5_obs_yearDOC <- meteo_pull_monitors(tallahassee_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1957-01-01', date_max = '1957-12-31')
plantcity_obs_yearDOC <- meteo_pull_monitors(plantcity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1989-01-01', date_max = '1989-12-31')
pensacola1_obs_yearDOC <- meteo_pull_monitors(pensacola_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1975-01-01', date_max = '1975-12-31')
gainesville1_obs_yearDOC <- meteo_pull_monitors(gainesville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1967-01-01', date_max = '1967-12-31')
staugustine_obs_yearDOC <- meteo_pull_monitors(staugustine_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2004-01-01', date_max = '2004-12-31')
tallahassee6_obs_yearDOC <- meteo_pull_monitors(tallahassee_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1957-01-01', date_max = '1957-12-31')
pensacola2_obs_yearDOC <- meteo_pull_monitors(pensacola_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1977-01-01', date_max = '1977-12-31')
gainesville2_obs_yearDOC <- meteo_pull_monitors(gainesville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1979-01-01', date_max = '1979-12-31')
marianna1_obs_yearDOC <- meteo_pull_monitors(marianna_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1957-01-01', date_max = '1957-12-31')
homestead2_obs_yearDOC <- meteo_pull_monitors(homestead_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1997-01-01', date_max = '1997-12-31')
pensacola3_obs_yearDOC <- meteo_pull_monitors(pensacola_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1992-01-01', date_max = '1992-12-31')
gainesville3_obs_yearDOC <- meteo_pull_monitors(gainesville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2002-01-01', date_max = '2002-12-31')
homestead3_obs_yearDOC <- meteo_pull_monitors(homestead_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1997-01-01', date_max = '1997-12-31')
pensacola4_obs_yearDOC <- meteo_pull_monitors(pensacola_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2001-01-01', date_max = '2001-12-31')
homestead4_obs_yearDOC <- meteo_pull_monitors(homestead_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1997-01-01', date_max = '1997-12-31')
blountstown_obs_yearDOC <- meteo_pull_monitors(blountstown_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2008-01-01', date_max = '2008-12-31')
pensacola5_obs_yearDOC <- meteo_pull_monitors(pensacola_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2011-01-01', date_max = '2011-12-31')
marianna2_obs_yearDOC <- meteo_pull_monitors(marianna_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1957-01-01', date_max = '1957-12-31')
pensacola6_obs_yearDOC <- meteo_pull_monitors(pensacola_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2002-01-01', date_max = '2002-12-31')
highsprings_obs_yearDOC <- meteo_pull_monitors(highsprings_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2013-01-01', date_max = '2013-12-31')
pensacola7_obs_yearDOC <- meteo_pull_monitors(pensacola_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1999-01-01', date_max = '1999-12-31')
pensacola8_obs_yearDOC <- meteo_pull_monitors(pensacola_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1996-01-01', date_max = '1996-12-31')
pensacola9_obs_yearDOC <- meteo_pull_monitors(pensacola_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1990-01-01', date_max = '1990-12-31')
pensacola10_obs_yearDOC <- meteo_pull_monitors(pensacola_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1989-01-01', date_max = '1989-12-31')
pensacola11_obs_yearDOC <- meteo_pull_monitors(pensacola_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1986-01-01', date_max = '1986-12-31')
milton_obs_yearDOC <- meteo_pull_monitors(milton_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1990-01-01', date_max = '1990-12-31')
jacksonville_obs_yearDOC <- meteo_pull_monitors(jacksonville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2003-01-01', date_max = '2003-12-31')

## make a dataframe of the mean of tmax, tmin, current year of the date of collection
lapply(dflist_nodates, cat(citySwap2(x = city2,  y = "_DOC", z = " <- data.frame(FL_csv$id[", l= lennumber, k = "], (mean(", e = "_obs_yearDOC$prcp, na.rm=TRUE)), (mean(", f = "_obs_yearDOC$tmax, na.rm=TRUE)), (mean(", g = "_obs_yearDOC$tmin, na.rm=TRUE))) "), sep = "\n"))

tallahassee1_DOC <- data.frame(FL_csv$id[1], (mean(tallahassee1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(tallahassee1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(tallahassee1_obs_yearDOC$tmin, na.rm=TRUE))) 
homestead1_DOC <- data.frame(FL_csv$id[2], (mean(homestead1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(homestead1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(homestead1_obs_yearDOC$tmin, na.rm=TRUE))) 
tallahassee2_DOC <- data.frame(FL_csv$id[3], (mean(tallahassee2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(tallahassee2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(tallahassee2_obs_yearDOC$tmin, na.rm=TRUE))) 
monticello_DOC <- data.frame(FL_csv$id[4], (mean(monticello_obs_yearDOC$prcp, na.rm=TRUE)), (mean(monticello_obs_yearDOC$tmax, na.rm=TRUE)), (mean(monticello_obs_yearDOC$tmin, na.rm=TRUE))) 
tallahassee3_DOC <- data.frame(FL_csv$id[5], (mean(tallahassee3_obs_yearDOC$prcp, na.rm=TRUE)), (mean(tallahassee3_obs_yearDOC$tmax, na.rm=TRUE)), (mean(tallahassee3_obs_yearDOC$tmin, na.rm=TRUE))) 
tallahassee4_DOC <- data.frame(FL_csv$id[6], (mean(tallahassee4_obs_yearDOC$prcp, na.rm=TRUE)), (mean(tallahassee4_obs_yearDOC$tmax, na.rm=TRUE)), (mean(tallahassee4_obs_yearDOC$tmin, na.rm=TRUE))) 
tallahassee5_DOC <- data.frame(FL_csv$id[7], (mean(tallahassee5_obs_yearDOC$prcp, na.rm=TRUE)), (mean(tallahassee5_obs_yearDOC$tmax, na.rm=TRUE)), (mean(tallahassee5_obs_yearDOC$tmin, na.rm=TRUE))) 
plantcity_DOC <- data.frame(FL_csv$id[8], (mean(plantcity_obs_yearDOC$prcp, na.rm=TRUE)), (mean(plantcity_obs_yearDOC$tmax, na.rm=TRUE)), (mean(plantcity_obs_yearDOC$tmin, na.rm=TRUE))) 
pensacola1_DOC <- data.frame(FL_csv$id[9], (mean(pensacola1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(pensacola1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(pensacola1_obs_yearDOC$tmin, na.rm=TRUE))) 
gainesville1_DOC <- data.frame(FL_csv$id[10], (mean(gainesville1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(gainesville1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(gainesville1_obs_yearDOC$tmin, na.rm=TRUE))) 
staugustine_DOC <- data.frame(FL_csv$id[11], (mean(staugustine_obs_yearDOC$prcp, na.rm=TRUE)), (mean(staugustine_obs_yearDOC$tmax, na.rm=TRUE)), (mean(staugustine_obs_yearDOC$tmin, na.rm=TRUE))) 
tallahassee6_DOC <- data.frame(FL_csv$id[12], (mean(tallahassee6_obs_yearDOC$prcp, na.rm=TRUE)), (mean(tallahassee6_obs_yearDOC$tmax, na.rm=TRUE)), (mean(tallahassee6_obs_yearDOC$tmin, na.rm=TRUE))) 
pensacola2_DOC <- data.frame(FL_csv$id[13], (mean(pensacola2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(pensacola2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(pensacola2_obs_yearDOC$tmin, na.rm=TRUE))) 
gainesville2_DOC <- data.frame(FL_csv$id[14], (mean(gainesville2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(gainesville2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(gainesville2_obs_yearDOC$tmin, na.rm=TRUE))) 
marianna1_DOC <- data.frame(FL_csv$id[15], (mean(marianna1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(marianna1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(marianna1_obs_yearDOC$tmin, na.rm=TRUE))) 
homestead2_DOC <- data.frame(FL_csv$id[16], (mean(homestead2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(homestead2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(homestead2_obs_yearDOC$tmin, na.rm=TRUE))) 
pensacola3_DOC <- data.frame(FL_csv$id[17], (mean(pensacola3_obs_yearDOC$prcp, na.rm=TRUE)), (mean(pensacola3_obs_yearDOC$tmax, na.rm=TRUE)), (mean(pensacola3_obs_yearDOC$tmin, na.rm=TRUE))) 
gainesville3_DOC <- data.frame(FL_csv$id[18], (mean(gainesville3_obs_yearDOC$prcp, na.rm=TRUE)), (mean(gainesville3_obs_yearDOC$tmax, na.rm=TRUE)), (mean(gainesville3_obs_yearDOC$tmin, na.rm=TRUE))) 
homestead3_DOC <- data.frame(FL_csv$id[19], (mean(homestead3_obs_yearDOC$prcp, na.rm=TRUE)), (mean(homestead3_obs_yearDOC$tmax, na.rm=TRUE)), (mean(homestead3_obs_yearDOC$tmin, na.rm=TRUE))) 
pensacola4_DOC <- data.frame(FL_csv$id[20], (mean(pensacola4_obs_yearDOC$prcp, na.rm=TRUE)), (mean(pensacola4_obs_yearDOC$tmax, na.rm=TRUE)), (mean(pensacola4_obs_yearDOC$tmin, na.rm=TRUE))) 
homestead4_DOC <- data.frame(FL_csv$id[21], (mean(homestead4_obs_yearDOC$prcp, na.rm=TRUE)), (mean(homestead4_obs_yearDOC$tmax, na.rm=TRUE)), (mean(homestead4_obs_yearDOC$tmin, na.rm=TRUE))) 
blountstown_DOC <- data.frame(FL_csv$id[22], (mean(blountstown_obs_yearDOC$prcp, na.rm=TRUE)), (mean(blountstown_obs_yearDOC$tmax, na.rm=TRUE)), (mean(blountstown_obs_yearDOC$tmin, na.rm=TRUE))) 
pensacola5_DOC <- data.frame(FL_csv$id[23], (mean(pensacola5_obs_yearDOC$prcp, na.rm=TRUE)), (mean(pensacola5_obs_yearDOC$tmax, na.rm=TRUE)), (mean(pensacola5_obs_yearDOC$tmin, na.rm=TRUE))) 
marianna2_DOC <- data.frame(FL_csv$id[24], (mean(marianna2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(marianna2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(marianna2_obs_yearDOC$tmin, na.rm=TRUE))) 
pensacola6_DOC <- data.frame(FL_csv$id[25], (mean(pensacola6_obs_yearDOC$prcp, na.rm=TRUE)), (mean(pensacola6_obs_yearDOC$tmax, na.rm=TRUE)), (mean(pensacola6_obs_yearDOC$tmin, na.rm=TRUE))) 
highsprings_DOC <- data.frame(FL_csv$id[26], (mean(highsprings_obs_yearDOC$prcp, na.rm=TRUE)), (mean(highsprings_obs_yearDOC$tmax, na.rm=TRUE)), (mean(highsprings_obs_yearDOC$tmin, na.rm=TRUE))) 
pensacola7_DOC <- data.frame(FL_csv$id[27], (mean(pensacola7_obs_yearDOC$prcp, na.rm=TRUE)), (mean(pensacola7_obs_yearDOC$tmax, na.rm=TRUE)), (mean(pensacola7_obs_yearDOC$tmin, na.rm=TRUE))) 
pensacola8_DOC <- data.frame(FL_csv$id[28], (mean(pensacola8_obs_yearDOC$prcp, na.rm=TRUE)), (mean(pensacola8_obs_yearDOC$tmax, na.rm=TRUE)), (mean(pensacola8_obs_yearDOC$tmin, na.rm=TRUE))) 
pensacola9_DOC <- data.frame(FL_csv$id[29], (mean(pensacola9_obs_yearDOC$prcp, na.rm=TRUE)), (mean(pensacola9_obs_yearDOC$tmax, na.rm=TRUE)), (mean(pensacola9_obs_yearDOC$tmin, na.rm=TRUE))) 
pensacola10_DOC <- data.frame(FL_csv$id[30], (mean(pensacola10_obs_yearDOC$prcp, na.rm=TRUE)), (mean(pensacola10_obs_yearDOC$tmax, na.rm=TRUE)), (mean(pensacola10_obs_yearDOC$tmin, na.rm=TRUE))) 
pensacola11_DOC <- data.frame(FL_csv$id[31], (mean(pensacola11_obs_yearDOC$prcp, na.rm=TRUE)), (mean(pensacola11_obs_yearDOC$tmax, na.rm=TRUE)), (mean(pensacola11_obs_yearDOC$tmin, na.rm=TRUE))) 
milton_DOC <- data.frame(FL_csv$id[32], (mean(milton_obs_yearDOC$prcp, na.rm=TRUE)), (mean(milton_obs_yearDOC$tmax, na.rm=TRUE)), (mean(milton_obs_yearDOC$tmin, na.rm=TRUE))) 
jacksonville_DOC <- data.frame(FL_csv$id[33], (mean(jacksonville_obs_yearDOC$prcp, na.rm=TRUE)), (mean(jacksonville_obs_yearDOC$tmax, na.rm=TRUE)), (mean(jacksonville_obs_yearDOC$tmin, na.rm=TRUE))) 

#rename col.names 
cat(citySwap3(city2, y = "colnames(", z = "_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')"), sep = "\n")

colnames(tallahassee1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(homestead1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tallahassee2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(monticello_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tallahassee3_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tallahassee4_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tallahassee5_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(plantcity_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(pensacola1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(gainesville1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(staugustine_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tallahassee6_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(pensacola2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(gainesville2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(marianna1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(homestead2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(pensacola3_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(gainesville3_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(homestead3_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(pensacola4_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(homestead4_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(blountstown_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(pensacola5_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(marianna2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(pensacola6_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(highsprings_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(pensacola7_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(pensacola8_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(pensacola9_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(pensacola10_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(pensacola11_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(milton_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(jacksonville_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')


## combine current year of the date of collection data using bind_rows
cat(citySwap7(city2, y = "_DOC", z = ","), sep = "")
FL_yearDOC <- bind_rows(tallahassee1_DOC,homestead1_DOC,tallahassee2_DOC,monticello_DOC,tallahassee3_DOC,tallahassee4_DOC,tallahassee5_DOC,plantcity_DOC,pensacola1_DOC,gainesville1_DOC,staugustine_DOC,tallahassee6_DOC,pensacola2_DOC,gainesville2_DOC,marianna1_DOC,homestead2_DOC,pensacola3_DOC,gainesville3_DOC,homestead3_DOC,pensacola4_DOC,homestead4_DOC,blountstown_DOC,pensacola5_DOC,marianna2_DOC,pensacola6_DOC,highsprings_DOC,pensacola7_DOC,pensacola8_DOC,pensacola9_DOC,pensacola10_DOC,pensacola11_DOC,milton_DOC,jacksonville_DOC)

# save data
write.xlsx(FL_yearDOC , file = "FL_year_DOC_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
