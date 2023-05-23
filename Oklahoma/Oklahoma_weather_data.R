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
OK_csv <- read.csv('oklahoma_cities.csv')
OK_csv$month <- match(OK_csv$month, month.name)
OK_csv$date <- paste(OK_csv$year, OK_csv$month, OK_csv$day, sep="-")
strptime(OK_csv$date,format="%Y-%m-%d")
OK_csv$date <- as.Date(OK_csv$date)
colnames(OK_csv) <- c("id", 'id2', 'city_county_state', 'latitude', 'longitude', 'elevation', 'month', 'day', 'year', 'date')
OK_csv$previous6mon <- as.Date(as.yearmon(as.Date(OK_csv$date)) -.5, frac = 1)
head(OK_csv)

## to use lapply 
city <- OK_csv$id
city2 <- OK_csv$id2
date <- OK_csv$date
pyear <- (OK_csv$date - 365)
p6mon <- OK_csv$previous6mon
year <- OK_csv$year
lennumber <- (1:17) #number of samples

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

midwest2_stations <- stations %>% filter(state == c("OK", "TX", "KS", "MO"))
write.csv2(midwest2_stations, "midwest2_stations.csv")

#after running the 1st time 
midwest2_stations <- read.csv("midwest2_stations.csv")


#find closests stations
OK_clst <- meteo_nearby_stations(lat_lon_df = OK_csv, station_data = midwest2_stations, radius = 50, limit = 200)

### separate all cities 
cat(citySwap(unique(city), y = " <- (OK_clst$'", z = "')"), sep = "\n")

summers <- (OK_clst$'summers')
stillwater <- (OK_clst$'stillwater')
oklahomacity <- (OK_clst$'oklahomacity')
riverbottom <- (OK_clst$'riverbottom')
hennessey <- (OK_clst$'hennessey')
kingfisher <- (OK_clst$'kingfisher')
bristow <- (OK_clst$'bristow')
norman <- (OK_clst$'norman')
sulphur <- (OK_clst$'sulphur')
drippingsprings <- (OK_clst$'drippingsprings')
peggs <- (OK_clst$'peggs')
shawnee <- (OK_clst$'shawnee')
alva <- (OK_clst$'alva')

### separate all station id's (monitors) for each city
cat(citySwap(unique(city), y = "_monitors <- ", z = "$id"), sep = "\n")

summers_monitors <- summers$id
stillwater_monitors <- stillwater$id
oklahomacity_monitors <- oklahomacity$id
riverbottom_monitors <- riverbottom$id
hennessey_monitors <- hennessey$id
kingfisher_monitors <- kingfisher$id
bristow_monitors <- bristow$id
norman_monitors <- norman$id
sulphur_monitors <- sulphur$id
drippingsprings_monitors <- drippingsprings$id
peggs_monitors <- peggs$id
shawnee_monitors <- shawnee$id
alva_monitors <- alva$id

### pull all observations for cities 
cat(citySwap(unique(city), y = "_obs <- meteo_pull_monitors(", z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')"), sep = "\n")

summers_obs <- meteo_pull_monitors(summers_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
stillwater_obs <- meteo_pull_monitors(stillwater_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
oklahomacity_obs <- meteo_pull_monitors(oklahomacity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
riverbottom_obs <- meteo_pull_monitors(riverbottom_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
hennessey_obs <- meteo_pull_monitors(hennessey_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
kingfisher_obs <- meteo_pull_monitors(kingfisher_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
bristow_obs <- meteo_pull_monitors(bristow_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
norman_obs <- meteo_pull_monitors(norman_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
sulphur_obs <- meteo_pull_monitors(sulphur_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
drippingsprings_obs <- meteo_pull_monitors(drippingsprings_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
peggs_obs <- meteo_pull_monitors(peggs_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
shawnee_obs <- meteo_pull_monitors(shawnee_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
alva_obs <- meteo_pull_monitors(alva_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')

### Part 1
### pull data by date and location - dates are coming from state_csv 
lapply(df1, cat(citySwap4.5(x = city2, y = "_obs_temps <- ", i = city, z = "_obs %>% filter(date == '", j = date , h = "')"), sep = "\n"))

summers_obs_temps <- summers_obs %>% filter(date == '1958-04-22')
stillwater_obs_temps <- stillwater_obs %>% filter(date == '1940-04-10')
oklahomacity1_obs_temps <- oklahomacity_obs %>% filter(date == '1937-04-01')
riverbottom1_obs_temps <- riverbottom_obs %>% filter(date == '1959-04-05')
hennessey_obs_temps <- hennessey_obs %>% filter(date == '1914-03-20')
kingfisher_obs_temps <- kingfisher_obs %>% filter(date == '1913-04-26')
oklahomacity2_obs_temps <- oklahomacity_obs %>% filter(date == '1992-03-30')
bristow_obs_temps <- bristow_obs %>% filter(date == '1933-04-03')
norman_obs_temps <- norman_obs %>% filter(date == '2007-05-15')
sulphur_obs_temps <- sulphur_obs %>% filter(date == '1936-03-25')
oklahomacity3_obs_temps <- oklahomacity_obs %>% filter(date == '2001-03-29')
drippingsprings_obs_temps <- drippingsprings_obs %>% filter(date == '1956-04-22')
oklahomacity4_obs_temps <- oklahomacity_obs %>% filter(date == '2001-04-01')
peggs_obs_temps <- peggs_obs %>% filter(date == '1958-05-11')
riverbottom2_obs_temps <- riverbottom_obs %>% filter(date == '1959-04-05')
shawnee_obs_temps <- shawnee_obs %>% filter(date == '1963-03-23')
alva_obs_temps <- alva_obs %>% filter(date == '1970-11-03')

#combine into one data frame using bind_rows()
cat(citySwap7(city2, y = "_obs_temps", z = ","), sep = "")
OK_obs <- bind_rows(summers_obs_temps,stillwater_obs_temps,oklahomacity1_obs_temps,riverbottom1_obs_temps,hennessey_obs_temps,kingfisher_obs_temps,oklahomacity2_obs_temps,bristow_obs_temps,norman_obs_temps,sulphur_obs_temps,oklahomacity3_obs_temps,drippingsprings_obs_temps,oklahomacity4_obs_temps,peggs_obs_temps,riverbottom2_obs_temps,shawnee_obs_temps,alva_obs_temps)

#Merge the original state_csv dataframe and new state_obs dataframe 
OK_data <- merge.data.frame(OK_csv, OK_obs, by = 'date')

## find the tavg for state data 
OK_data$tavg <- ((OK_data$tmax + OK_data$tmin) / 2)

### save data as xlsx file 
library("xlsx")
write.xlsx(OK_data, file = "OK_data_total_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

### Part 2
#finding data from 6 months prior to date of collection
#ambient temps and precipitation may have affected plant/leaf growth and shape
lapply(df2, cat(citySwap5(x = city2, y = "_obs_6months <- meteo_pull_monitors(", j = city, 
                          z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '", h = p6mon, 
                          k = "', date_max = '", l = date, p ="')"), sep = "\n"))

summers_obs_6months <- meteo_pull_monitors(summers_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1957-10-31', date_max = '1958-04-22')
stillwater_obs_6months <- meteo_pull_monitors(stillwater_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1939-10-31', date_max = '1940-04-10')
oklahomacity1_obs_6months <- meteo_pull_monitors(oklahomacity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1936-10-31', date_max = '1937-04-01')
riverbottom1_obs_6months <- meteo_pull_monitors(riverbottom_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1958-10-31', date_max = '1959-04-05')
hennessey_obs_6months <- meteo_pull_monitors(hennessey_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1913-09-30', date_max = '1914-03-20')
kingfisher_obs_6months <- meteo_pull_monitors(kingfisher_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1912-10-31', date_max = '1913-04-26')
oklahomacity2_obs_6months <- meteo_pull_monitors(oklahomacity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1991-09-30', date_max = '1992-03-30')
bristow_obs_6months <- meteo_pull_monitors(bristow_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1932-10-31', date_max = '1933-04-03')
norman_obs_6months <- meteo_pull_monitors(norman_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2006-11-30', date_max = '2007-05-15')
sulphur_obs_6months <- meteo_pull_monitors(sulphur_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1935-09-30', date_max = '1936-03-25')
oklahomacity3_obs_6months <- meteo_pull_monitors(oklahomacity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2000-09-30', date_max = '2001-03-29')
drippingsprings_obs_6months <- meteo_pull_monitors(drippingsprings_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1955-10-31', date_max = '1956-04-22')
oklahomacity4_obs_6months <- meteo_pull_monitors(oklahomacity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2000-10-31', date_max = '2001-04-01')
peggs_obs_6months <- meteo_pull_monitors(peggs_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1957-11-30', date_max = '1958-05-11')
riverbottom2_obs_6months <- meteo_pull_monitors(riverbottom_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1958-10-31', date_max = '1959-04-05')
shawnee_obs_6months <- meteo_pull_monitors(shawnee_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1962-09-30', date_max = '1963-03-23')
alva_obs_6months <- meteo_pull_monitors(alva_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1970-05-31', date_max = '1970-11-03')

## make a dataframe of the mean of tmax, tmin, prcp of the previous 6 months 
lapply(df3, cat(citySwap2(x = city2,  y = "_6months", z = " <- data.frame(OK_csv$id[", l= lennumber, k = "], (mean(", e = "_obs_6months$prcp, na.rm=TRUE)), (mean(", f = "_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(", g = "_obs_6months$tmin, na.rm=TRUE))) "), sep = "\n"))

summers_6months <- data.frame(OK_csv$id[1], (mean(summers_obs_6months$prcp, na.rm=TRUE)), (mean(summers_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(summers_obs_6months$tmin, na.rm=TRUE))) 
stillwater_6months <- data.frame(OK_csv$id[2], (mean(stillwater_obs_6months$prcp, na.rm=TRUE)), (mean(stillwater_obs_6months$tmax, na.rm=TRUE)), 
                                 (mean(stillwater_obs_6months$tmin, na.rm=TRUE))) 
oklahomacity1_6months <- data.frame(OK_csv$id[3], (mean(oklahomacity1_obs_6months$prcp, na.rm=TRUE)), (mean(oklahomacity1_obs_6months$tmax, na.rm=TRUE)), 
                                    (mean(oklahomacity1_obs_6months$tmin, na.rm=TRUE))) 
riverbottom1_6months <- data.frame(OK_csv$id[4], (mean(riverbottom1_obs_6months$prcp, na.rm=TRUE)), (mean(riverbottom1_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(riverbottom1_obs_6months$tmin, na.rm=TRUE))) 
hennessey_6months <- data.frame(OK_csv$id[5], (mean(hennessey_obs_6months$prcp, na.rm=TRUE)), (mean(hennessey_obs_6months$tmax, na.rm=TRUE)), 
                                (mean(hennessey_obs_6months$tmin, na.rm=TRUE))) 
kingfisher_6months <- data.frame(OK_csv$id[6], (mean(kingfisher_obs_6months$prcp, na.rm=TRUE)), (mean(kingfisher_obs_6months$tmax, na.rm=TRUE)), 
                                 (mean(kingfisher_obs_6months$tmin, na.rm=TRUE))) 
oklahomacity2_6months <- data.frame(OK_csv$id[7], (mean(oklahomacity2_obs_6months$prcp, na.rm=TRUE)), (mean(oklahomacity2_obs_6months$tmax, na.rm=TRUE)), 
                                    (mean(oklahomacity2_obs_6months$tmin, na.rm=TRUE))) 
bristow_6months <- data.frame(OK_csv$id[8], (mean(bristow_obs_6months$prcp, na.rm=TRUE)), (mean(bristow_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(bristow_obs_6months$tmin, na.rm=TRUE))) 
norman_6months <- data.frame(OK_csv$id[9], (mean(norman_obs_6months$prcp, na.rm=TRUE)), (mean(norman_obs_6months$tmax, na.rm=TRUE)), 
                             (mean(norman_obs_6months$tmin, na.rm=TRUE))) 
sulphur_6months <- data.frame(OK_csv$id[10], (mean(sulphur_obs_6months$prcp, na.rm=TRUE)), (mean(sulphur_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(sulphur_obs_6months$tmin, na.rm=TRUE))) 
oklahomacity3_6months <- data.frame(OK_csv$id[11], (mean(oklahomacity3_obs_6months$prcp, na.rm=TRUE)), (mean(oklahomacity3_obs_6months$tmax, na.rm=TRUE)), 
                                    (mean(oklahomacity3_obs_6months$tmin, na.rm=TRUE))) 
drippingsprings_6months <- data.frame(OK_csv$id[12], (mean(drippingsprings_obs_6months$prcp, na.rm=TRUE)), (mean(drippingsprings_obs_6months$tmax, na.rm=TRUE)), 
                                      (mean(drippingsprings_obs_6months$tmin, na.rm=TRUE))) 
oklahomacity4_6months <- data.frame(OK_csv$id[13], (mean(oklahomacity4_obs_6months$prcp, na.rm=TRUE)), (mean(oklahomacity4_obs_6months$tmax, na.rm=TRUE)), 
                                    (mean(oklahomacity4_obs_6months$tmin, na.rm=TRUE))) 
peggs_6months <- data.frame(OK_csv$id[14], (mean(peggs_obs_6months$prcp, na.rm=TRUE)), (mean(peggs_obs_6months$tmax, na.rm=TRUE)), 
                            (mean(peggs_obs_6months$tmin, na.rm=TRUE))) 
riverbottom2_6months <- data.frame(OK_csv$id[15], (mean(riverbottom2_obs_6months$prcp, na.rm=TRUE)), (mean(riverbottom2_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(riverbottom2_obs_6months$tmin, na.rm=TRUE))) 
shawnee_6months <- data.frame(OK_csv$id[16], (mean(shawnee_obs_6months$prcp, na.rm=TRUE)), (mean(shawnee_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(shawnee_obs_6months$tmin, na.rm=TRUE))) 
alva_6months <- data.frame(OK_csv$id[17], (mean(alva_obs_6months$prcp, na.rm=TRUE)), (mean(alva_obs_6months$tmax, na.rm=TRUE)), 
                           (mean(alva_obs_6months$tmin, na.rm=TRUE)))

#rename col.names 
cat(citySwap3(city2, y = "colnames(", z = "_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')"), sep = "\n")

colnames(summers_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(stillwater_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(oklahomacity1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(riverbottom1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(hennessey_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(kingfisher_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(oklahomacity2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(bristow_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(norman_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(sulphur_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(oklahomacity3_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(drippingsprings_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(oklahomacity4_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(peggs_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(riverbottom2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(shawnee_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(alva_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')

## combine previous 6 months data using bind_rows
cat(citySwap7(city2, y = "_6months", z = ","), sep = "")
OK_6months <- bind_rows(summers_6months,stillwater_6months,oklahomacity1_6months,riverbottom1_6months,hennessey_6months,kingfisher_6months,oklahomacity2_6months,bristow_6months,norman_6months,sulphur_6months,oklahomacity3_6months,drippingsprings_6months,oklahomacity4_6months,peggs_6months,riverbottom2_6months,shawnee_6months,alva_6months)

### save data as xlsx file 
write.xlsx(OK_6months, file = "OK_6months_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


### Part 3
#find data from previous year from date of collection 
lapply(df4, cat(citySwap5(x = city2, y = "_obs_year <- meteo_pull_monitors(", j = city, 
                          z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '", h= pyear, 
                          k = "', date_max = '", l = date, p ="')"), sep = "\n"))

summers_obs_year <- meteo_pull_monitors(summers_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1957-04-22', date_max = '1958-04-22')
stillwater_obs_year <- meteo_pull_monitors(stillwater_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1939-04-11', date_max = '1940-04-10')
oklahomacity1_obs_year <- meteo_pull_monitors(oklahomacity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1936-04-01', date_max = '1937-04-01')
riverbottom1_obs_year <- meteo_pull_monitors(riverbottom_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1958-04-05', date_max = '1959-04-05')
hennessey_obs_year <- meteo_pull_monitors(hennessey_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1913-03-20', date_max = '1914-03-20')
kingfisher_obs_year <- meteo_pull_monitors(kingfisher_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1912-04-26', date_max = '1913-04-26')
oklahomacity2_obs_year <- meteo_pull_monitors(oklahomacity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1991-03-31', date_max = '1992-03-30')
bristow_obs_year <- meteo_pull_monitors(bristow_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1932-04-03', date_max = '1933-04-03')
norman_obs_year <- meteo_pull_monitors(norman_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2006-05-15', date_max = '2007-05-15')
sulphur_obs_year <- meteo_pull_monitors(sulphur_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1935-03-26', date_max = '1936-03-25')
oklahomacity3_obs_year <- meteo_pull_monitors(oklahomacity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2000-03-29', date_max = '2001-03-29')
drippingsprings_obs_year <- meteo_pull_monitors(drippingsprings_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1955-04-23', date_max = '1956-04-22')
oklahomacity4_obs_year <- meteo_pull_monitors(oklahomacity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2000-04-01', date_max = '2001-04-01')
peggs_obs_year <- meteo_pull_monitors(peggs_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1957-05-11', date_max = '1958-05-11')
riverbottom2_obs_year <- meteo_pull_monitors(riverbottom_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1958-04-05', date_max = '1959-04-05')
shawnee_obs_year <- meteo_pull_monitors(shawnee_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1962-03-23', date_max = '1963-03-23')
alva_obs_year <- meteo_pull_monitors(alva_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1969-11-03', date_max = '1970-11-03')

## make a data frame of the mean of tmax, tmin, prcp of the previous year before the date of collection 
lapply(df3, cat(citySwap2(x = city2,  y = "_previousyear", z = " <- data.frame(OK_csv$id[", l= lennumber, 
                          k = "], (mean(", e = "_obs_year$prcp, na.rm=TRUE)), (mean(", 
                          f = "_obs_year$tmax, na.rm=TRUE)), (mean(", g = "_obs_year$tmin, na.rm=TRUE))) "), sep = "\n"))

summers_previousyear <- data.frame(OK_csv$id[1], (mean(summers_obs_year$prcp, na.rm=TRUE)), (mean(summers_obs_year$tmax, na.rm=TRUE)), (mean(summers_obs_year$tmin, na.rm=TRUE))) 
stillwater_previousyear <- data.frame(OK_csv$id[2], (mean(stillwater_obs_year$prcp, na.rm=TRUE)), (mean(stillwater_obs_year$tmax, na.rm=TRUE)), (mean(stillwater_obs_year$tmin, na.rm=TRUE))) 
oklahomacity1_previousyear <- data.frame(OK_csv$id[3], (mean(oklahomacity1_obs_year$prcp, na.rm=TRUE)), (mean(oklahomacity1_obs_year$tmax, na.rm=TRUE)), (mean(oklahomacity1_obs_year$tmin, na.rm=TRUE))) 
riverbottom1_previousyear <- data.frame(OK_csv$id[4], (mean(riverbottom1_obs_year$prcp, na.rm=TRUE)), (mean(riverbottom1_obs_year$tmax, na.rm=TRUE)), (mean(riverbottom1_obs_year$tmin, na.rm=TRUE))) 
hennessey_previousyear <- data.frame(OK_csv$id[5], (mean(hennessey_obs_year$prcp, na.rm=TRUE)), (mean(hennessey_obs_year$tmax, na.rm=TRUE)), (mean(hennessey_obs_year$tmin, na.rm=TRUE))) 
kingfisher_previousyear <- data.frame(OK_csv$id[6], (mean(kingfisher_obs_year$prcp, na.rm=TRUE)), (mean(kingfisher_obs_year$tmax, na.rm=TRUE)), (mean(kingfisher_obs_year$tmin, na.rm=TRUE))) 
oklahomacity2_previousyear <- data.frame(OK_csv$id[7], (mean(oklahomacity2_obs_year$prcp, na.rm=TRUE)), (mean(oklahomacity2_obs_year$tmax, na.rm=TRUE)), (mean(oklahomacity2_obs_year$tmin, na.rm=TRUE))) 
bristow_previousyear <- data.frame(OK_csv$id[8], (mean(bristow_obs_year$prcp, na.rm=TRUE)), (mean(bristow_obs_year$tmax, na.rm=TRUE)), (mean(bristow_obs_year$tmin, na.rm=TRUE))) 
norman_previousyear <- data.frame(OK_csv$id[9], (mean(norman_obs_year$prcp, na.rm=TRUE)), (mean(norman_obs_year$tmax, na.rm=TRUE)), (mean(norman_obs_year$tmin, na.rm=TRUE))) 
sulphur_previousyear <- data.frame(OK_csv$id[10], (mean(sulphur_obs_year$prcp, na.rm=TRUE)), (mean(sulphur_obs_year$tmax, na.rm=TRUE)), (mean(sulphur_obs_year$tmin, na.rm=TRUE))) 
oklahomacity3_previousyear <- data.frame(OK_csv$id[11], (mean(oklahomacity3_obs_year$prcp, na.rm=TRUE)), (mean(oklahomacity3_obs_year$tmax, na.rm=TRUE)), (mean(oklahomacity3_obs_year$tmin, na.rm=TRUE))) 
drippingsprings_previousyear <- data.frame(OK_csv$id[12], (mean(drippingsprings_obs_year$prcp, na.rm=TRUE)), (mean(drippingsprings_obs_year$tmax, na.rm=TRUE)), (mean(drippingsprings_obs_year$tmin, na.rm=TRUE))) 
oklahomacity4_previousyear <- data.frame(OK_csv$id[13], (mean(oklahomacity4_obs_year$prcp, na.rm=TRUE)), (mean(oklahomacity4_obs_year$tmax, na.rm=TRUE)), (mean(oklahomacity4_obs_year$tmin, na.rm=TRUE))) 
peggs_previousyear <- data.frame(OK_csv$id[14], (mean(peggs_obs_year$prcp, na.rm=TRUE)), (mean(peggs_obs_year$tmax, na.rm=TRUE)), (mean(peggs_obs_year$tmin, na.rm=TRUE))) 
riverbottom2_previousyear <- data.frame(OK_csv$id[15], (mean(riverbottom2_obs_year$prcp, na.rm=TRUE)), (mean(riverbottom2_obs_year$tmax, na.rm=TRUE)), (mean(riverbottom2_obs_year$tmin, na.rm=TRUE))) 
shawnee_previousyear <- data.frame(OK_csv$id[16], (mean(shawnee_obs_year$prcp, na.rm=TRUE)), (mean(shawnee_obs_year$tmax, na.rm=TRUE)), (mean(shawnee_obs_year$tmin, na.rm=TRUE))) 
alva_previousyear <- data.frame(OK_csv$id[17], (mean(alva_obs_year$prcp, na.rm=TRUE)), (mean(alva_obs_year$tmax, na.rm=TRUE)), (mean(alva_obs_year$tmin, na.rm=TRUE)))

#rename col.names 
cat(citySwap3(city2, y = "colnames(", z = "_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')"), sep = "\n")

colnames(summers_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(stillwater_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(oklahomacity1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(riverbottom1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(hennessey_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(kingfisher_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(oklahomacity2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(bristow_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(norman_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(sulphur_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(oklahomacity3_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(drippingsprings_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(oklahomacity4_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(peggs_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(riverbottom2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(shawnee_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(alva_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')

## combine previous year before the date of collection data using bind_rows
cat(citySwap7(city2, y = "_previousyear", z = ","), sep = "")
OK_Pyear <- bind_rows(summers_previousyear,stillwater_previousyear,oklahomacity1_previousyear,riverbottom1_previousyear,hennessey_previousyear,kingfisher_previousyear,oklahomacity2_previousyear,bristow_previousyear,norman_previousyear,sulphur_previousyear,oklahomacity3_previousyear,drippingsprings_previousyear,oklahomacity4_previousyear,peggs_previousyear,riverbottom2_previousyear,shawnee_previousyear,alva_previousyear)

# save data
write.xlsx(OK_Pyear, file = "OK_previous_year_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


### Part 4 
#Find data from current year of date of collection 
lapply(df5, cat(citySwap6(x = city2, y = "_obs_yearDOC <- meteo_pull_monitors(", j = city, 
                          z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '", l = year , 
                          k = "-01-01', date_max = '",  p ="-12-31')"), sep = "\n"))

summers_obs_yearDOC <- meteo_pull_monitors(summers_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1958-01-01', date_max = '1958-12-31')
stillwater_obs_yearDOC <- meteo_pull_monitors(stillwater_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1940-01-01', date_max = '1940-12-31')
oklahomacity1_obs_yearDOC <- meteo_pull_monitors(oklahomacity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1937-01-01', date_max = '1937-12-31')
riverbottom1_obs_yearDOC <- meteo_pull_monitors(riverbottom_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1959-01-01', date_max = '1959-12-31')
hennessey_obs_yearDOC <- meteo_pull_monitors(hennessey_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1914-01-01', date_max = '1914-12-31')
kingfisher_obs_yearDOC <- meteo_pull_monitors(kingfisher_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1913-01-01', date_max = '1913-12-31')
oklahomacity2_obs_yearDOC <- meteo_pull_monitors(oklahomacity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1992-01-01', date_max = '1992-12-31')
bristow_obs_yearDOC <- meteo_pull_monitors(bristow_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1933-01-01', date_max = '1933-12-31')
norman_obs_yearDOC <- meteo_pull_monitors(norman_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2007-01-01', date_max = '2007-12-31')
sulphur_obs_yearDOC <- meteo_pull_monitors(sulphur_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1936-01-01', date_max = '1936-12-31')
oklahomacity3_obs_yearDOC <- meteo_pull_monitors(oklahomacity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2001-01-01', date_max = '2001-12-31')
drippingsprings_obs_yearDOC <- meteo_pull_monitors(drippingsprings_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1956-01-01', date_max = '1956-12-31')
oklahomacity4_obs_yearDOC <- meteo_pull_monitors(oklahomacity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2001-01-01', date_max = '2001-12-31')
peggs_obs_yearDOC <- meteo_pull_monitors(peggs_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1958-01-01', date_max = '1958-12-31')
riverbottom2_obs_yearDOC <- meteo_pull_monitors(riverbottom_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1959-01-01', date_max = '1959-12-31')
shawnee_obs_yearDOC <- meteo_pull_monitors(shawnee_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1963-01-01', date_max = '1963-12-31')
alva_obs_yearDOC <- meteo_pull_monitors(alva_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1970-01-01', date_max = '1970-12-31')

## make a dataframe of the mean of tmax, tmin, current year of the date of collection
lapply(dflist_nodates, cat(citySwap2(x = city2,  y = "_DOC", z = " <- data.frame(OK_csv$id[", l= lennumber, k = "], (mean(", e = "_obs_yearDOC$prcp, na.rm=TRUE)), (mean(", f = "_obs_yearDOC$tmax, na.rm=TRUE)), (mean(", g = "_obs_yearDOC$tmin, na.rm=TRUE))) "), sep = "\n"))

summers_DOC <- data.frame(OK_csv$id[1], (mean(summers_obs_yearDOC$prcp, na.rm=TRUE)), (mean(summers_obs_yearDOC$tmax, na.rm=TRUE)), (mean(summers_obs_yearDOC$tmin, na.rm=TRUE))) 
stillwater_DOC <- data.frame(OK_csv$id[2], (mean(stillwater_obs_yearDOC$prcp, na.rm=TRUE)), (mean(stillwater_obs_yearDOC$tmax, na.rm=TRUE)), (mean(stillwater_obs_yearDOC$tmin, na.rm=TRUE))) 
oklahomacity1_DOC <- data.frame(OK_csv$id[3], (mean(oklahomacity1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(oklahomacity1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(oklahomacity1_obs_yearDOC$tmin, na.rm=TRUE))) 
riverbottom1_DOC <- data.frame(OK_csv$id[4], (mean(riverbottom1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(riverbottom1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(riverbottom1_obs_yearDOC$tmin, na.rm=TRUE))) 
hennessey_DOC <- data.frame(OK_csv$id[5], (mean(hennessey_obs_yearDOC$prcp, na.rm=TRUE)), (mean(hennessey_obs_yearDOC$tmax, na.rm=TRUE)), (mean(hennessey_obs_yearDOC$tmin, na.rm=TRUE))) 
kingfisher_DOC <- data.frame(OK_csv$id[6], (mean(kingfisher_obs_yearDOC$prcp, na.rm=TRUE)), (mean(kingfisher_obs_yearDOC$tmax, na.rm=TRUE)), (mean(kingfisher_obs_yearDOC$tmin, na.rm=TRUE))) 
oklahomacity2_DOC <- data.frame(OK_csv$id[7], (mean(oklahomacity2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(oklahomacity2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(oklahomacity2_obs_yearDOC$tmin, na.rm=TRUE))) 
bristow_DOC <- data.frame(OK_csv$id[8], (mean(bristow_obs_yearDOC$prcp, na.rm=TRUE)), (mean(bristow_obs_yearDOC$tmax, na.rm=TRUE)), (mean(bristow_obs_yearDOC$tmin, na.rm=TRUE))) 
norman_DOC <- data.frame(OK_csv$id[9], (mean(norman_obs_yearDOC$prcp, na.rm=TRUE)), (mean(norman_obs_yearDOC$tmax, na.rm=TRUE)), (mean(norman_obs_yearDOC$tmin, na.rm=TRUE))) 
sulphur_DOC <- data.frame(OK_csv$id[10], (mean(sulphur_obs_yearDOC$prcp, na.rm=TRUE)), (mean(sulphur_obs_yearDOC$tmax, na.rm=TRUE)), (mean(sulphur_obs_yearDOC$tmin, na.rm=TRUE))) 
oklahomacity3_DOC <- data.frame(OK_csv$id[11], (mean(oklahomacity3_obs_yearDOC$prcp, na.rm=TRUE)), (mean(oklahomacity3_obs_yearDOC$tmax, na.rm=TRUE)), (mean(oklahomacity3_obs_yearDOC$tmin, na.rm=TRUE))) 
drippingsprings_DOC <- data.frame(OK_csv$id[12], (mean(drippingsprings_obs_yearDOC$prcp, na.rm=TRUE)), (mean(drippingsprings_obs_yearDOC$tmax, na.rm=TRUE)), (mean(drippingsprings_obs_yearDOC$tmin, na.rm=TRUE))) 
oklahomacity4_DOC <- data.frame(OK_csv$id[13], (mean(oklahomacity4_obs_yearDOC$prcp, na.rm=TRUE)), (mean(oklahomacity4_obs_yearDOC$tmax, na.rm=TRUE)), (mean(oklahomacity4_obs_yearDOC$tmin, na.rm=TRUE))) 
peggs_DOC <- data.frame(OK_csv$id[14], (mean(peggs_obs_yearDOC$prcp, na.rm=TRUE)), (mean(peggs_obs_yearDOC$tmax, na.rm=TRUE)), (mean(peggs_obs_yearDOC$tmin, na.rm=TRUE))) 
riverbottom2_DOC <- data.frame(OK_csv$id[15], (mean(riverbottom2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(riverbottom2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(riverbottom2_obs_yearDOC$tmin, na.rm=TRUE))) 
shawnee_DOC <- data.frame(OK_csv$id[16], (mean(shawnee_obs_yearDOC$prcp, na.rm=TRUE)), (mean(shawnee_obs_yearDOC$tmax, na.rm=TRUE)), (mean(shawnee_obs_yearDOC$tmin, na.rm=TRUE))) 
alva_DOC <- data.frame(OK_csv$id[17], (mean(alva_obs_yearDOC$prcp, na.rm=TRUE)), (mean(alva_obs_yearDOC$tmax, na.rm=TRUE)), (mean(alva_obs_yearDOC$tmin, na.rm=TRUE)))

#rename col.names 
cat(citySwap3(city2, y = "colnames(", z = "_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')"), sep = "\n")

colnames(summers_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(stillwater_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(oklahomacity1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(riverbottom1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(hennessey_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(kingfisher_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(oklahomacity2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(bristow_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(norman_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(sulphur_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(oklahomacity3_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(drippingsprings_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(oklahomacity4_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(peggs_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(riverbottom2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(shawnee_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(alva_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')

## combine current year of the date of collection data using bind_rows
cat(citySwap7(city2, y = "_DOC", z = ","), sep = "")
OK_yearDOC <- bind_rows(summers_DOC,stillwater_DOC,oklahomacity1_DOC,riverbottom1_DOC,hennessey_DOC,kingfisher_DOC,oklahomacity2_DOC,bristow_DOC,norman_DOC,sulphur_DOC,oklahomacity3_DOC,drippingsprings_DOC,oklahomacity4_DOC,peggs_DOC,riverbottom2_DOC,shawnee_DOC,alva_DOC)

# save data
write.xlsx(OK_yearDOC, file = "OK_year_DOC_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
