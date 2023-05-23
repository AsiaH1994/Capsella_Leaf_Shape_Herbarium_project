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
OH_csv <- read.csv('ohio_cities.csv')
OH_csv$month <- match(OH_csv$month, month.name)
OH_csv$date <- paste(OH_csv$year, OH_csv$month, OH_csv$day, sep="-")
strptime(OH_csv$date,format="%Y-%m-%d")
OH_csv$date <- as.Date(OH_csv$date)
colnames(OH_csv) <- c("id", 'id2', 'city_county_state', 'latitude', 'longitude', 'elevation', 'month', 'day', 'year', 'date')
OH_csv$previous6mon <- as.Date(as.yearmon(as.Date(OH_csv$date)) -.5, frac = 1)
head(OH_csv)

OH_csv[8,9] <- 1979

OH_csv <- OH_csv[,-(11)]

## to use lapply 
city <- OH_csv$id
city2 <- OH_csv$id2
date <- OH_csv$date
pyear <- (OH_csv$date - 365)
p6mon <- OH_csv$previous6mon
year <- OH_csv$year
lennumber <- (1:8) #number of samples

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
OH_clst <- meteo_nearby_stations(lat_lon_df = OH_csv, station_data = great_lakes_stations, radius = 50, limit = 200)

### separate all cities 
cat(citySwap(unique(city), y = " <- (OH_clst$'", z = "')"), sep = "\n")

hamilton <- (OH_clst$'hamilton')
columbus <- (OH_clst$'columbus')
coshocton <- (OH_clst$'coshocton')
lebanon <- (OH_clst$'lebanon')
ironton <- (OH_clst$'ironton')
oxford <- (OH_clst$'oxford')
stclairsville <- (OH_clst$'stclairsville')

### separate all station id's (monitors) for each city
cat(citySwap(unique(city), y = "_monitors <- ", z = "$id"), sep = "\n")

hamilton_monitors <- hamilton$id
columbus_monitors <- columbus$id
coshocton_monitors <- coshocton$id
lebanon_monitors <- lebanon$id
ironton_monitors <- ironton$id
oxford_monitors <- oxford$id
stclairsville_monitors <- stclairsville$id

### pull all observations for cities 
cat(citySwap(unique(city), y = "_obs <- meteo_pull_monitors(", z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')"), sep = "\n")


hamilton_obs <- meteo_pull_monitors(hamilton_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
columbus_obs <- meteo_pull_monitors(columbus_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
coshocton_obs <- meteo_pull_monitors(coshocton_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
lebanon_obs <- meteo_pull_monitors(lebanon_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
ironton_obs <- meteo_pull_monitors(ironton_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
oxford_obs <- meteo_pull_monitors(oxford_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
stclairsville_obs <- meteo_pull_monitors(stclairsville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')

### Part 1
### pull data by date and location - dates are coming from state_csv 
lapply(df1, cat(citySwap4.5(x = city2, y = "_obs_temps <- ", i = city, z = "_obs %>% filter(date == '", j = date , h = "')"), sep = "\n"))

hamilton1_obs_temps <- hamilton_obs %>% filter(date == '1990-04-05')
columbus_obs_temps <- columbus_obs %>% filter(date == '2005-05-05')
coshocton_obs_temps <- coshocton_obs %>% filter(date == '1942-04-08')
hamilton2_obs_temps <- hamilton_obs %>% filter(date == '1990-04-05')
lebanon_obs_temps <- lebanon_obs %>% filter(date == '1992-04-27')
ironton_obs_temps <- ironton_obs %>% filter(date == '1984-04-27')
oxford_obs_temps <- oxford_obs %>% filter(date == '1958-05-04')
stclairsville_obs_temps <- stclairsville_obs %>% filter(date == '1979-04-29')

#combine into one data frame using bind_rows()
cat(citySwap7(city2, y = "_obs_temps", z = ","), sep = "")
OH_obs <- bind_rows(hamilton1_obs_temps,columbus_obs_temps,coshocton_obs_temps,hamilton2_obs_temps,lebanon_obs_temps,ironton_obs_temps,oxford_obs_temps,stclairsville_obs_temps)

#Merge the original state_csv dataframe and new state_obs dataframe 
OH_data <- merge.data.frame(OH_csv, OH_obs, by = 'date')

## find the tavg for state data 
OH_data$tavg <- ((OH_data$tmax + OH_data$tmin) / 2)

### save data as xlsx file 
library("xlsx")
write.xlsx(OH_data, file = "OH_data_total_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

### Part 2
#finding data from 6 months prior to date of collection
#ambient temps and precipitation may have affected plant/leaf growth and shape
lapply(df2, cat(citySwap5(x = city2, y = "_obs_6months <- meteo_pull_monitors(", j = city, 
                          z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '", h = p6mon, 
                          k = "', date_max = '", l = date, p ="')"), sep = "\n"))

hamilton1_obs_6months <- meteo_pull_monitors(hamilton_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1989-10-31', date_max = '1990-04-05')
columbus_obs_6months <- meteo_pull_monitors(columbus_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2004-11-30', date_max = '2005-05-05')
coshocton_obs_6months <- meteo_pull_monitors(coshocton_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1941-10-31', date_max = '1942-04-08')
hamilton2_obs_6months <- meteo_pull_monitors(hamilton_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1989-10-31', date_max = '1990-04-05')
lebanon_obs_6months <- meteo_pull_monitors(lebanon_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1991-10-31', date_max = '1992-04-27')
ironton_obs_6months <- meteo_pull_monitors(ironton_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1983-10-31', date_max = '1984-04-27')
oxford_obs_6months <- meteo_pull_monitors(oxford_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1957-11-30', date_max = '1958-05-04')
stclairsville_obs_6months <- meteo_pull_monitors(stclairsville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1978-10-31', date_max = '1979-04-29')

## make a dataframe of the mean of tmax, tmin, prcp of the previous 6 months 
lapply(df3, cat(citySwap2(x = city2,  y = "_6months", z = " <- data.frame(OH_csv$id[", l= lennumber, k = "], (mean(", e = "_obs_6months$prcp, na.rm=TRUE)), (mean(", f = "_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(", g = "_obs_6months$tmin, na.rm=TRUE))) "), sep = "\n"))

hamilton1_6months <- data.frame(OH_csv$id[1], (mean(hamilton1_obs_6months$prcp, na.rm=TRUE)), (mean(hamilton1_obs_6months$tmax, na.rm=TRUE)), 
                                (mean(hamilton1_obs_6months$tmin, na.rm=TRUE))) 
columbus_6months <- data.frame(OH_csv$id[2], (mean(columbus_obs_6months$prcp, na.rm=TRUE)), (mean(columbus_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(columbus_obs_6months$tmin, na.rm=TRUE))) 
coshocton_6months <- data.frame(OH_csv$id[3], (mean(coshocton_obs_6months$prcp, na.rm=TRUE)), (mean(coshocton_obs_6months$tmax, na.rm=TRUE)), 
                                (mean(coshocton_obs_6months$tmin, na.rm=TRUE))) 
hamilton2_6months <- data.frame(OH_csv$id[4], (mean(hamilton2_obs_6months$prcp, na.rm=TRUE)), (mean(hamilton2_obs_6months$tmax, na.rm=TRUE)), 
                                (mean(hamilton2_obs_6months$tmin, na.rm=TRUE))) 
lebanon_6months <- data.frame(OH_csv$id[5], (mean(lebanon_obs_6months$prcp, na.rm=TRUE)), (mean(lebanon_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(lebanon_obs_6months$tmin, na.rm=TRUE))) 
ironton_6months <- data.frame(OH_csv$id[6], (mean(ironton_obs_6months$prcp, na.rm=TRUE)), (mean(ironton_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(ironton_obs_6months$tmin, na.rm=TRUE))) 
oxford_6months <- data.frame(OH_csv$id[7], (mean(oxford_obs_6months$prcp, na.rm=TRUE)), (mean(oxford_obs_6months$tmax, na.rm=TRUE)), 
                             (mean(oxford_obs_6months$tmin, na.rm=TRUE))) 
stclairsville_6months <- data.frame(OH_csv$id[8], (mean(stclairsville_obs_6months$prcp, na.rm=TRUE)), (mean(stclairsville_obs_6months$tmax, na.rm=TRUE)), 
                                    (mean(stclairsville_obs_6months$tmin, na.rm=TRUE)))

#rename col.names 
cat(citySwap3(city2, y = "colnames(", z = "_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')"), sep = "\n")

colnames(hamilton1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(columbus_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(coshocton_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(hamilton2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(lebanon_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(ironton_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(oxford_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(stclairsville_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')

## combine previous 6 months data using bind_rows
cat(citySwap7(city2, y = "_6months", z = ","), sep = "")
OH_6months <- bind_rows(hamilton1_6months,columbus_6months,coshocton_6months,hamilton2_6months,lebanon_6months,ironton_6months,oxford_6months,stclairsville_6months)

### save data as xlsx file 
write.xlsx(OH_6months, file = "OH_6months_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


### Part 3
#find data from previous year from date of collection 
lapply(df4, cat(citySwap5(x = city2, y = "_obs_year <- meteo_pull_monitors(", j = city, 
                          z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '", h= pyear, 
                          k = "', date_max = '", l = date, p ="')"), sep = "\n"))

hamilton1_obs_year <- meteo_pull_monitors(hamilton_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1989-04-05', date_max = '1990-04-05')
columbus_obs_year <- meteo_pull_monitors(columbus_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2004-05-05', date_max = '2005-05-05')
coshocton_obs_year <- meteo_pull_monitors(coshocton_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1941-04-08', date_max = '1942-04-08')
hamilton2_obs_year <- meteo_pull_monitors(hamilton_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1989-04-05', date_max = '1990-04-05')
lebanon_obs_year <- meteo_pull_monitors(lebanon_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1991-04-28', date_max = '1992-04-27')
ironton_obs_year <- meteo_pull_monitors(ironton_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1983-04-28', date_max = '1984-04-27')
oxford_obs_year <- meteo_pull_monitors(oxford_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1957-05-04', date_max = '1958-05-04')
stclairsville_obs_year <- meteo_pull_monitors(stclairsville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1978-04-29', date_max = '1979-04-29')

## make a data frame of the mean of tmax, tmin, prcp of the previous year before the date of collection 
lapply(df3, cat(citySwap2(x = city2,  y = "_previousyear", z = " <- data.frame(OH_csv$id[", l= lennumber, 
                          k = "], (mean(", e = "_obs_year$prcp, na.rm=TRUE)), (mean(", 
                          f = "_obs_year$tmax, na.rm=TRUE)), (mean(", g = "_obs_year$tmin, na.rm=TRUE))) "), sep = "\n"))

hamilton1_previousyear <- data.frame(OH_csv$id[1], (mean(hamilton1_obs_year$prcp, na.rm=TRUE)), (mean(hamilton1_obs_year$tmax, na.rm=TRUE)), (mean(hamilton1_obs_year$tmin, na.rm=TRUE))) 
columbus_previousyear <- data.frame(OH_csv$id[2], (mean(columbus_obs_year$prcp, na.rm=TRUE)), (mean(columbus_obs_year$tmax, na.rm=TRUE)), (mean(columbus_obs_year$tmin, na.rm=TRUE))) 
coshocton_previousyear <- data.frame(OH_csv$id[3], (mean(coshocton_obs_year$prcp, na.rm=TRUE)), (mean(coshocton_obs_year$tmax, na.rm=TRUE)), (mean(coshocton_obs_year$tmin, na.rm=TRUE))) 
hamilton2_previousyear <- data.frame(OH_csv$id[4], (mean(hamilton2_obs_year$prcp, na.rm=TRUE)), (mean(hamilton2_obs_year$tmax, na.rm=TRUE)), (mean(hamilton2_obs_year$tmin, na.rm=TRUE))) 
lebanon_previousyear <- data.frame(OH_csv$id[5], (mean(lebanon_obs_year$prcp, na.rm=TRUE)), (mean(lebanon_obs_year$tmax, na.rm=TRUE)), (mean(lebanon_obs_year$tmin, na.rm=TRUE))) 
ironton_previousyear <- data.frame(OH_csv$id[6], (mean(ironton_obs_year$prcp, na.rm=TRUE)), (mean(ironton_obs_year$tmax, na.rm=TRUE)), (mean(ironton_obs_year$tmin, na.rm=TRUE))) 
oxford_previousyear <- data.frame(OH_csv$id[7], (mean(oxford_obs_year$prcp, na.rm=TRUE)), (mean(oxford_obs_year$tmax, na.rm=TRUE)), (mean(oxford_obs_year$tmin, na.rm=TRUE))) 
stclairsville_previousyear <- data.frame(OH_csv$id[8], (mean(stclairsville_obs_year$prcp, na.rm=TRUE)), (mean(stclairsville_obs_year$tmax, na.rm=TRUE)), (mean(stclairsville_obs_year$tmin, na.rm=TRUE))) 

#rename col.names 
cat(citySwap3(city2, y = "colnames(", z = "_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')"), sep = "\n")

colnames(hamilton1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(columbus_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(coshocton_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(hamilton2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(lebanon_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(ironton_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(oxford_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(stclairsville_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')

## combine previous year before the date of collection data using bind_rows
cat(citySwap7(city2, y = "_previousyear", z = ","), sep = "")
OH_Pyear <- bind_rows(hamilton1_previousyear,columbus_previousyear,coshocton_previousyear,hamilton2_previousyear,lebanon_previousyear,ironton_previousyear,oxford_previousyear,stclairsville_previousyear)

# save data
write.xlsx(OH_Pyear, file = "OH_previous_year_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


### Part 4 
#Find data from current year of date of collection 
lapply(df5, cat(citySwap6(x = city2, y = "_obs_yearDOC <- meteo_pull_monitors(", j = city, 
                          z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '", l = year , 
                          k = "-01-01', date_max = '",  p ="-12-31')"), sep = "\n"))

hamilton1_obs_yearDOC <- meteo_pull_monitors(hamilton_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1990-01-01', date_max = '1990-12-31')
columbus_obs_yearDOC <- meteo_pull_monitors(columbus_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2005-01-01', date_max = '2005-12-31')
coshocton_obs_yearDOC <- meteo_pull_monitors(coshocton_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1942-01-01', date_max = '1942-12-31')
hamilton2_obs_yearDOC <- meteo_pull_monitors(hamilton_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1990-01-01', date_max = '1990-12-31')
lebanon_obs_yearDOC <- meteo_pull_monitors(lebanon_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1992-01-01', date_max = '1992-12-31')
ironton_obs_yearDOC <- meteo_pull_monitors(ironton_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1984-01-01', date_max = '1984-12-31')
oxford_obs_yearDOC <- meteo_pull_monitors(oxford_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1958-01-01', date_max = '1958-12-31')
stclairsville_obs_yearDOC <- meteo_pull_monitors(stclairsville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1979-01-01', date_max = '1979-12-31')

## make a dataframe of the mean of tmax, tmin, current year of the date of collection
lapply(dflist_nodates, cat(citySwap2(x = city2,  y = "_DOC", z = " <- data.frame(OH_csv$id[", l= lennumber, k = "], (mean(", e = "_obs_yearDOC$prcp, na.rm=TRUE)), (mean(", f = "_obs_yearDOC$tmax, na.rm=TRUE)), (mean(", g = "_obs_yearDOC$tmin, na.rm=TRUE))) "), sep = "\n"))

hamilton1_DOC <- data.frame(OH_csv$id[1], (mean(hamilton1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(hamilton1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(hamilton1_obs_yearDOC$tmin, na.rm=TRUE))) 
columbus_DOC <- data.frame(OH_csv$id[2], (mean(columbus_obs_yearDOC$prcp, na.rm=TRUE)), (mean(columbus_obs_yearDOC$tmax, na.rm=TRUE)), (mean(columbus_obs_yearDOC$tmin, na.rm=TRUE))) 
coshocton_DOC <- data.frame(OH_csv$id[3], (mean(coshocton_obs_yearDOC$prcp, na.rm=TRUE)), (mean(coshocton_obs_yearDOC$tmax, na.rm=TRUE)), (mean(coshocton_obs_yearDOC$tmin, na.rm=TRUE))) 
hamilton2_DOC <- data.frame(OH_csv$id[4], (mean(hamilton2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(hamilton2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(hamilton2_obs_yearDOC$tmin, na.rm=TRUE))) 
lebanon_DOC <- data.frame(OH_csv$id[5], (mean(lebanon_obs_yearDOC$prcp, na.rm=TRUE)), (mean(lebanon_obs_yearDOC$tmax, na.rm=TRUE)), (mean(lebanon_obs_yearDOC$tmin, na.rm=TRUE))) 
ironton_DOC <- data.frame(OH_csv$id[6], (mean(ironton_obs_yearDOC$prcp, na.rm=TRUE)), (mean(ironton_obs_yearDOC$tmax, na.rm=TRUE)), (mean(ironton_obs_yearDOC$tmin, na.rm=TRUE))) 
oxford_DOC <- data.frame(OH_csv$id[7], (mean(oxford_obs_yearDOC$prcp, na.rm=TRUE)), (mean(oxford_obs_yearDOC$tmax, na.rm=TRUE)), (mean(oxford_obs_yearDOC$tmin, na.rm=TRUE))) 
stclairsville_DOC <- data.frame(OH_csv$id[8], (mean(stclairsville_obs_yearDOC$prcp, na.rm=TRUE)), (mean(stclairsville_obs_yearDOC$tmax, na.rm=TRUE)), (mean(stclairsville_obs_yearDOC$tmin, na.rm=TRUE)))

#rename col.names 
cat(citySwap3(city2, y = "colnames(", z = "_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')"), sep = "\n")

colnames(hamilton1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(columbus_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(coshocton_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(hamilton2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(lebanon_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(ironton_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(oxford_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(stclairsville_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')

## combine current year of the date of collection data using bind_rows
cat(citySwap7(city2, y = "_DOC", z = ","), sep = "")
OH_yearDOC <- bind_rows(hamilton1_DOC,columbus_DOC,coshocton_DOC,hamilton2_DOC,lebanon_DOC,ironton_DOC,oxford_DOC,stclairsville_DOC)

# save data
write.xlsx(OH_yearDOC, file = "OH_year_DOC_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
