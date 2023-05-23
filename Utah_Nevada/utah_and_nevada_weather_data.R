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
UN_csv <- read.csv('utah_nevada_cities.csv')
UN_csv$month <- match(UN_csv$month, month.name)
UN_csv$date <- paste(UN_csv$year, UN_csv$month, UN_csv$day, sep="-")
strptime(UN_csv$date,format="%Y-%m-%d")
UN_csv$date <- as.Date(UN_csv$date)
colnames(state_csv) <- c("id", 'id2', 'city_county_state', 'latitude', 'longitude', 'elevation', 'month', 'day', 'year', 'date')
UN_csv$previous6mon <- as.Date(as.yearmon(as.Date(UN_csv$date)) -.5, frac = 1)
head(UN_csv)

## to use lapply 
city <- UN_csv$id
city2 <- UN_csv$id2
date <- UN_csv$date
pyear <- (UN_csv$date - 365)
p6mon <- UN_csv$previous6mon
year <- UN_csv$year
lennumber <- (1:23) #number of samples

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

UN_stations <- stations %>% filter(state == c("UT", "NV"))
write.csv2(region_stations, "UN_stations.csv")

#after running the 1st time 
UN_stations <- read.csv("great_lakes_stations.csv")


#find closests stations
UN_clst <- meteo_nearby_stations(lat_lon_df = UN_csv, station_data = UN_stations, radius = 50)

### separate all cities 
cat(citySwap(unique(city), y = " <- (UN_clst$'", z = "')"), sep = "\n")

eureka <- (UN_clst$'eureka')
cache <- (UN_clst$'cache')
millcreek <- (UN_clst$'millcreek')
herriman <- (UN_clst$'herriman')
saltlakecity <- (UN_clst$'saltlakecity')
northlogan <- (UN_clst$'northlogan')
murray <- (UN_clst$'murray')
tooele <- (UN_clst$'tooele')
springville <- (UN_clst$'springville')
moab <- (UN_clst$'moab')
provo <- (UN_clst$'provo')
pleasantgrove <- (UN_clst$'pleasantgrove')
fallon <- (UN_clst$'fallon')
welcome <- (UN_clst$'welcome')

### separate all station id's (monitors) for each city
cat(citySwap(unique(city), y = "_monitors <- ", z = "$id"), sep = "\n")

eureka_monitors <- eureka$id
cache_monitors <- cache$id
millcreek_monitors <- millcreek$id
herriman_monitors <- herriman$id
saltlakecity_monitors <- saltlakecity$id
northlogan_monitors <- northlogan$id
murray_monitors <- murray$id
tooele_monitors <- tooele$id
springville_monitors <- springville$id
moab_monitors <- moab$id
provo_monitors <- provo$id
pleasantgrove_monitors <- pleasantgrove$id
fallon_monitors <- fallon$id
welcome_monitors <- welcome$id

### pull all observations for cities 
cat(citySwap(unique(city), y = "_obs <- meteo_pull_monitors(", z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')"), sep = "\n")

eureka_obs <- meteo_pull_monitors(eureka_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
cache_obs <- meteo_pull_monitors(cache_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
millcreek_obs <- meteo_pull_monitors(millcreek_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
herriman_obs <- meteo_pull_monitors(herriman_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
saltlakecity_obs <- meteo_pull_monitors(saltlakecity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
northlogan_obs <- meteo_pull_monitors(northlogan_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
murray_obs <- meteo_pull_monitors(murray_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
tooele_obs <- meteo_pull_monitors(tooele_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
springville_obs <- meteo_pull_monitors(springville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
moab_obs <- meteo_pull_monitors(moab_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
provo_obs <- meteo_pull_monitors(provo_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
pleasantgrove_obs <- meteo_pull_monitors(pleasantgrove_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
fallon_obs <- meteo_pull_monitors(fallon_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
welcome_obs <- meteo_pull_monitors(welcome_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')

### Part 1
### pull data by date and location - dates are coming from state_csv 
lapply(df1, cat(citySwap4.5(x = city2, y = "_obs_temps <- ", i = city, z = "_obs %>% filter(date == '", j = date , h = "')"), sep = "\n"))

eureka1_obs_temps <- eureka_obs %>% filter(date == '2019-06-14')
cache_obs_temps <- cache_obs %>% filter(date == '1967-05-07')
eureka2_obs_temps <- eureka_obs %>% filter(date == '1995-05-24')
millcreek_obs_temps <- millcreek_obs %>% filter(date == '1967-04-13')
herriman_obs_temps <- herriman_obs %>% filter(date == '1995-05-06')
saltlakecity1_obs_temps <- saltlakecity_obs %>% filter(date == '1880-06-10')
saltlakecity2_obs_temps <- saltlakecity_obs %>% filter(date == '1995-05-06')
saltlakecity3_obs_temps <- saltlakecity_obs %>% filter(date == '1880-06-10')
northlogan1_obs_temps <- northlogan_obs %>% filter(date == '1964-05-22')
eureka3_obs_temps <- eureka_obs %>% filter(date == '1995-05-24')
murray_obs_temps <- murray_obs %>% filter(date == '1917-05-25')
tooele_obs_temps <- tooele_obs %>% filter(date == '1979-05-12')
springville1_obs_temps <- springville_obs %>% filter(date == '2020-06-24')
northlogan2_obs_temps <- northlogan_obs %>% filter(date == '1998-04-18')
moab_obs_temps <- moab_obs %>% filter(date == '1986-09-14')
provo1_obs_temps <- provo_obs %>% filter(date == '1967-06-13')
pleasantgrove_obs_temps <- pleasantgrove_obs %>% filter(date == '2005-04-14')
provo2_obs_temps <- provo_obs %>% filter(date == '1966-04-21')
saltlakecity4_obs_temps <- saltlakecity_obs %>% filter(date == '1995-05-04')
provo3_obs_temps <- provo_obs %>% filter(date == '1938-04-28')
springville2_obs_temps <- springville_obs %>% filter(date == '1932-05-22')
fallon_obs_temps <- fallon_obs %>% filter(date == '1978-04-20')
welcome_obs_temps <- welcome_obs %>% filter(date == '1941-05-23')

#combine into one data frame using bind_rows()
cat(citySwap7(city2, y = "_obs_temps", z = ","), sep = "")
UN_obs <- bind_rows(eureka1_obs_temps,cache_obs_temps,eureka2_obs_temps,millcreek_obs_temps,herriman_obs_temps,saltlakecity1_obs_temps,saltlakecity2_obs_temps,saltlakecity3_obs_temps,northlogan1_obs_temps,eureka3_obs_temps,murray_obs_temps,tooele_obs_temps,springville1_obs_temps,northlogan2_obs_temps,moab_obs_temps,provo1_obs_temps,pleasantgrove_obs_temps,provo2_obs_temps,saltlakecity4_obs_temps,provo3_obs_temps,springville2_obs_temps,fallon_obs_temps,welcome_obs_temps)

#Merge the original state_csv dataframe and new state_obs dataframe 
UN_data <- merge.data.frame(UN_csv, UN_obs, by = 'date')

## find the tavg for state data 
UN_data$tavg <- ((UN_data$tmax + UN_data$tmin) / 2)

### save data as xlsx file 
library("xlsx")
write.xlsx(UN_data, file = "UN_data_total_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

### Part 2
#finding data from 6 months prior to date of collection
#ambient temps and precipitation may have affected plant/leaf growth and shape
lapply(df2, cat(citySwap5(x = city2, y = "_obs_6months <- meteo_pull_monitors(", j = city, 
                          z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '", h = p6mon, 
                          k = "', date_max = '", l = date, p ="')"), sep = "\n"))
eureka1_obs_6months <- meteo_pull_monitors(eureka_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2018-12-31', date_max = '2019-06-14')
cache_obs_6months <- meteo_pull_monitors(cache_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1966-11-30', date_max = '1967-05-07')
eureka2_obs_6months <- meteo_pull_monitors(eureka_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1994-11-30', date_max = '1995-05-24')
millcreek_obs_6months <- meteo_pull_monitors(millcreek_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1966-10-31', date_max = '1967-04-13')
herriman_obs_6months <- meteo_pull_monitors(herriman_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1994-11-30', date_max = '1995-05-06')
saltlakecity1_obs_6months <- meteo_pull_monitors(saltlakecity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1879-12-31', date_max = '1880-06-10')
saltlakecity2_obs_6months <- meteo_pull_monitors(saltlakecity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1994-11-30', date_max = '1995-05-06')
saltlakecity3_obs_6months <- meteo_pull_monitors(saltlakecity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1879-12-31', date_max = '1880-06-10')
northlogan1_obs_6months <- meteo_pull_monitors(northlogan_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1963-11-30', date_max = '1964-05-22')
eureka3_obs_6months <- meteo_pull_monitors(eureka_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1994-11-30', date_max = '1995-05-24')
murray_obs_6months <- meteo_pull_monitors(murray_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1916-11-30', date_max = '1917-05-25')
tooele_obs_6months <- meteo_pull_monitors(tooele_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1978-11-30', date_max = '1979-05-12')
springville1_obs_6months <- meteo_pull_monitors(springville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2019-12-31', date_max = '2020-06-24')
northlogan2_obs_6months <- meteo_pull_monitors(northlogan_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1997-10-31', date_max = '1998-04-18')
moab_obs_6months <- meteo_pull_monitors(moab_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1986-03-31', date_max = '1986-09-14')
provo1_obs_6months <- meteo_pull_monitors(provo_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1966-12-31', date_max = '1967-06-13')
pleasantgrove_obs_6months <- meteo_pull_monitors(pleasantgrove_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2004-10-31', date_max = '2005-04-14')
provo2_obs_6months <- meteo_pull_monitors(provo_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1965-10-31', date_max = '1966-04-21')
saltlakecity4_obs_6months <- meteo_pull_monitors(saltlakecity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1994-11-30', date_max = '1995-05-04')
provo3_obs_6months <- meteo_pull_monitors(provo_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1937-10-31', date_max = '1938-04-28')
springville2_obs_6months <- meteo_pull_monitors(springville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1931-11-30', date_max = '1932-05-22')
fallon_obs_6months <- meteo_pull_monitors(fallon_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1977-10-31', date_max = '1978-04-20')
welcome_obs_6months <- meteo_pull_monitors(welcome_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1940-11-30', date_max = '1941-05-23')

## make a dataframe of the mean of tmax, tmin, prcp of the previous 6 months 
lapply(df3, cat(citySwap2(x = city2,  y = "_6months", z = " <- data.frame(UN_csv$id[", l= lennumber, k = "], (mean(", e = "_obs_6months$prcp, na.rm=TRUE)), (mean(", f = "_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(", g = "_obs_6months$tmin, na.rm=TRUE))) "), sep = "\n"))

eureka1_6months <- data.frame(UN_csv$id[1], (mean(eureka1_obs_6months$prcp, na.rm=TRUE)), (mean(eureka1_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(eureka1_obs_6months$tmin, na.rm=TRUE))) 
cache_6months <- data.frame(UN_csv$id[2], (mean(cache_obs_6months$prcp, na.rm=TRUE)), (mean(cache_obs_6months$tmax, na.rm=TRUE)), 
                            (mean(cache_obs_6months$tmin, na.rm=TRUE))) 
eureka2_6months <- data.frame(UN_csv$id[3], (mean(eureka2_obs_6months$prcp, na.rm=TRUE)), (mean(eureka2_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(eureka2_obs_6months$tmin, na.rm=TRUE))) 
millcreek_6months <- data.frame(UN_csv$id[4], (mean(millcreek_obs_6months$prcp, na.rm=TRUE)), (mean(millcreek_obs_6months$tmax, na.rm=TRUE)), 
                                (mean(millcreek_obs_6months$tmin, na.rm=TRUE))) 
herriman_6months <- data.frame(UN_csv$id[5], (mean(herriman_obs_6months$prcp, na.rm=TRUE)), (mean(herriman_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(herriman_obs_6months$tmin, na.rm=TRUE))) 
saltlakecity1_6months <- data.frame(UN_csv$id[6], (mean(saltlakecity1_obs_6months$prcp, na.rm=TRUE)), (mean(saltlakecity1_obs_6months$tmax, na.rm=TRUE)), 
                                    (mean(saltlakecity1_obs_6months$tmin, na.rm=TRUE))) 
saltlakecity2_6months <- data.frame(UN_csv$id[7], (mean(saltlakecity2_obs_6months$prcp, na.rm=TRUE)), (mean(saltlakecity2_obs_6months$tmax, na.rm=TRUE)), 
                                    (mean(saltlakecity2_obs_6months$tmin, na.rm=TRUE))) 
saltlakecity3_6months <- data.frame(UN_csv$id[8], (mean(saltlakecity3_obs_6months$prcp, na.rm=TRUE)), (mean(saltlakecity3_obs_6months$tmax, na.rm=TRUE)), 
                                    (mean(saltlakecity3_obs_6months$tmin, na.rm=TRUE))) 
northlogan1_6months <- data.frame(UN_csv$id[9], (mean(northlogan1_obs_6months$prcp, na.rm=TRUE)), (mean(northlogan1_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(northlogan1_obs_6months$tmin, na.rm=TRUE))) 
eureka3_6months <- data.frame(UN_csv$id[10], (mean(eureka3_obs_6months$prcp, na.rm=TRUE)), (mean(eureka3_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(eureka3_obs_6months$tmin, na.rm=TRUE))) 
murray_6months <- data.frame(UN_csv$id[11], (mean(murray_obs_6months$prcp, na.rm=TRUE)), (mean(murray_obs_6months$tmax, na.rm=TRUE)), 
                             (mean(murray_obs_6months$tmin, na.rm=TRUE))) 
tooele_6months <- data.frame(UN_csv$id[12], (mean(tooele_obs_6months$prcp, na.rm=TRUE)), (mean(tooele_obs_6months$tmax, na.rm=TRUE)), 
                             (mean(tooele_obs_6months$tmin, na.rm=TRUE))) 
springville1_6months <- data.frame(UN_csv$id[13], (mean(springville1_obs_6months$prcp, na.rm=TRUE)), (mean(springville1_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(springville1_obs_6months$tmin, na.rm=TRUE))) 
northlogan2_6months <- data.frame(UN_csv$id[14], (mean(northlogan2_obs_6months$prcp, na.rm=TRUE)), (mean(northlogan2_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(northlogan2_obs_6months$tmin, na.rm=TRUE))) 
moab_6months <- data.frame(UN_csv$id[15], (mean(moab_obs_6months$prcp, na.rm=TRUE)), (mean(moab_obs_6months$tmax, na.rm=TRUE)), 
                           (mean(moab_obs_6months$tmin, na.rm=TRUE))) 
provo1_6months <- data.frame(UN_csv$id[16], (mean(provo1_obs_6months$prcp, na.rm=TRUE)), (mean(provo1_obs_6months$tmax, na.rm=TRUE)), 
                             (mean(provo1_obs_6months$tmin, na.rm=TRUE))) 
pleasantgrove_6months <- data.frame(UN_csv$id[17], (mean(pleasantgrove_obs_6months$prcp, na.rm=TRUE)), (mean(pleasantgrove_obs_6months$tmax, na.rm=TRUE)), 
                                    (mean(pleasantgrove_obs_6months$tmin, na.rm=TRUE))) 
provo2_6months <- data.frame(UN_csv$id[18], (mean(provo2_obs_6months$prcp, na.rm=TRUE)), (mean(provo2_obs_6months$tmax, na.rm=TRUE)), 
                             (mean(provo2_obs_6months$tmin, na.rm=TRUE))) 
saltlakecity4_6months <- data.frame(UN_csv$id[19], (mean(saltlakecity4_obs_6months$prcp, na.rm=TRUE)), (mean(saltlakecity4_obs_6months$tmax, na.rm=TRUE)), 
                                    (mean(saltlakecity4_obs_6months$tmin, na.rm=TRUE))) 
provo3_6months <- data.frame(UN_csv$id[20], (mean(provo3_obs_6months$prcp, na.rm=TRUE)), (mean(provo3_obs_6months$tmax, na.rm=TRUE)), 
                             (mean(provo3_obs_6months$tmin, na.rm=TRUE))) 
springville2_6months <- data.frame(UN_csv$id[21], (mean(springville2_obs_6months$prcp, na.rm=TRUE)), (mean(springville2_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(springville2_obs_6months$tmin, na.rm=TRUE))) 
fallon_6months <- data.frame(UN_csv$id[22], (mean(fallon_obs_6months$prcp, na.rm=TRUE)), (mean(fallon_obs_6months$tmax, na.rm=TRUE)), 
                             (mean(fallon_obs_6months$tmin, na.rm=TRUE))) 
welcome_6months <- data.frame(UN_csv$id[23], (mean(welcome_obs_6months$prcp, na.rm=TRUE)), (mean(welcome_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(welcome_obs_6months$tmin, na.rm=TRUE)))

#rename col.names 
cat(citySwap3(city2, y = "colnames(", z = "_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')"), sep = "\n")

colnames(eureka1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(cache_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(eureka2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(millcreek_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(herriman_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(saltlakecity1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(saltlakecity2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(saltlakecity3_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(northlogan1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(eureka3_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(murray_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tooele_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(springville1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(northlogan2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(moab_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(provo1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(pleasantgrove_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(provo2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(saltlakecity4_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(provo3_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(springville2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(fallon_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(welcome_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')

## combine previous 6 months data using bind_rows
cat(citySwap7(city2, y = "_6months", z = ","), sep = "")
UN_6months <- bind_rows(eureka1_6months,cache_6months,eureka2_6months,millcreek_6months,herriman_6months,saltlakecity1_6months,saltlakecity2_6months,saltlakecity3_6months,northlogan1_6months,eureka3_6months,murray_6months,tooele_6months,springville1_6months,northlogan2_6months,moab_6months,provo1_6months,pleasantgrove_6months,provo2_6months,saltlakecity4_6months,provo3_6months,springville2_6months,fallon_6months,welcome_6months)

### save data as xlsx file 
write.xlsx(UN_6months, file = "UN_6months_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


### Part 3
#find data from previous year from date of collection 
lapply(df4, cat(citySwap5(x = city2, y = "_obs_year <- meteo_pull_monitors(", j = city, 
                          z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '", h= pyear, 
                          k = "', date_max = '", l = date, p ="')"), sep = "\n"))

eureka1_obs_year <- meteo_pull_monitors(eureka_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2018-06-14', date_max = '2019-06-14')
cache_obs_year <- meteo_pull_monitors(cache_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1966-05-07', date_max = '1967-05-07')
eureka2_obs_year <- meteo_pull_monitors(eureka_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1994-05-24', date_max = '1995-05-24')
millcreek_obs_year <- meteo_pull_monitors(millcreek_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1966-04-13', date_max = '1967-04-13')
herriman_obs_year <- meteo_pull_monitors(herriman_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1994-05-06', date_max = '1995-05-06')
saltlakecity1_obs_year <- meteo_pull_monitors(saltlakecity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1879-06-11', date_max = '1880-06-10')
saltlakecity2_obs_year <- meteo_pull_monitors(saltlakecity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1994-05-06', date_max = '1995-05-06')
saltlakecity3_obs_year <- meteo_pull_monitors(saltlakecity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1879-06-11', date_max = '1880-06-10')
northlogan1_obs_year <- meteo_pull_monitors(northlogan_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1963-05-23', date_max = '1964-05-22')
eureka3_obs_year <- meteo_pull_monitors(eureka_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1994-05-24', date_max = '1995-05-24')
murray_obs_year <- meteo_pull_monitors(murray_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1916-05-25', date_max = '1917-05-25')
tooele_obs_year <- meteo_pull_monitors(tooele_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1978-05-12', date_max = '1979-05-12')
springville1_obs_year <- meteo_pull_monitors(springville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2019-06-25', date_max = '2020-06-24')
northlogan2_obs_year <- meteo_pull_monitors(northlogan_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1997-04-18', date_max = '1998-04-18')
moab_obs_year <- meteo_pull_monitors(moab_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1985-09-14', date_max = '1986-09-14')
provo1_obs_year <- meteo_pull_monitors(provo_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1966-06-13', date_max = '1967-06-13')
pleasantgrove_obs_year <- meteo_pull_monitors(pleasantgrove_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2004-04-14', date_max = '2005-04-14')
provo2_obs_year <- meteo_pull_monitors(provo_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1965-04-21', date_max = '1966-04-21')
saltlakecity4_obs_year <- meteo_pull_monitors(saltlakecity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1994-05-04', date_max = '1995-05-04')
provo3_obs_year <- meteo_pull_monitors(provo_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1937-04-28', date_max = '1938-04-28')
springville2_obs_year <- meteo_pull_monitors(springville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1931-05-23', date_max = '1932-05-22')
fallon_obs_year <- meteo_pull_monitors(fallon_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1977-04-20', date_max = '1978-04-20')
welcome_obs_year <- meteo_pull_monitors(welcome_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1940-05-23', date_max = '1941-05-23')


## make a data frame of the mean of tmax, tmin, prcp of the previous year before the date of collection 
lapply(df3, cat(citySwap2(x = city2,  y = "_previousyear", z = " <- data.frame(UN_csv$id[", l= lennumber, 
                          k = "], (mean(", e = "_obs_year$prcp, na.rm=TRUE)), (mean(", 
                          f = "_obs_year$tmax, na.rm=TRUE)), (mean(", g = "_obs_year$tmin, na.rm=TRUE))) "), sep = "\n"))

eureka1_previousyear <- data.frame(UN_csv$id[1], (mean(eureka1_obs_year$prcp, na.rm=TRUE)), (mean(eureka1_obs_year$tmax, na.rm=TRUE)), (mean(eureka1_obs_year$tmin, na.rm=TRUE))) 
cache_previousyear <- data.frame(UN_csv$id[2], (mean(cache_obs_year$prcp, na.rm=TRUE)), (mean(cache_obs_year$tmax, na.rm=TRUE)), (mean(cache_obs_year$tmin, na.rm=TRUE))) 
eureka2_previousyear <- data.frame(UN_csv$id[3], (mean(eureka2_obs_year$prcp, na.rm=TRUE)), (mean(eureka2_obs_year$tmax, na.rm=TRUE)), (mean(eureka2_obs_year$tmin, na.rm=TRUE))) 
millcreek_previousyear <- data.frame(UN_csv$id[4], (mean(millcreek_obs_year$prcp, na.rm=TRUE)), (mean(millcreek_obs_year$tmax, na.rm=TRUE)), (mean(millcreek_obs_year$tmin, na.rm=TRUE))) 
herriman_previousyear <- data.frame(UN_csv$id[5], (mean(herriman_obs_year$prcp, na.rm=TRUE)), (mean(herriman_obs_year$tmax, na.rm=TRUE)), (mean(herriman_obs_year$tmin, na.rm=TRUE))) 
saltlakecity1_previousyear <- data.frame(UN_csv$id[6], (mean(saltlakecity1_obs_year$prcp, na.rm=TRUE)), (mean(saltlakecity1_obs_year$tmax, na.rm=TRUE)), (mean(saltlakecity1_obs_year$tmin, na.rm=TRUE))) 
saltlakecity2_previousyear <- data.frame(UN_csv$id[7], (mean(saltlakecity2_obs_year$prcp, na.rm=TRUE)), (mean(saltlakecity2_obs_year$tmax, na.rm=TRUE)), (mean(saltlakecity2_obs_year$tmin, na.rm=TRUE))) 
saltlakecity3_previousyear <- data.frame(UN_csv$id[8], (mean(saltlakecity3_obs_year$prcp, na.rm=TRUE)), (mean(saltlakecity3_obs_year$tmax, na.rm=TRUE)), (mean(saltlakecity3_obs_year$tmin, na.rm=TRUE))) 
northlogan1_previousyear <- data.frame(UN_csv$id[9], (mean(northlogan1_obs_year$prcp, na.rm=TRUE)), (mean(northlogan1_obs_year$tmax, na.rm=TRUE)), (mean(northlogan1_obs_year$tmin, na.rm=TRUE))) 
eureka3_previousyear <- data.frame(UN_csv$id[10], (mean(eureka3_obs_year$prcp, na.rm=TRUE)), (mean(eureka3_obs_year$tmax, na.rm=TRUE)), (mean(eureka3_obs_year$tmin, na.rm=TRUE))) 
murray_previousyear <- data.frame(UN_csv$id[11], (mean(murray_obs_year$prcp, na.rm=TRUE)), (mean(murray_obs_year$tmax, na.rm=TRUE)), (mean(murray_obs_year$tmin, na.rm=TRUE))) 
tooele_previousyear <- data.frame(UN_csv$id[12], (mean(tooele_obs_year$prcp, na.rm=TRUE)), (mean(tooele_obs_year$tmax, na.rm=TRUE)), (mean(tooele_obs_year$tmin, na.rm=TRUE))) 
springville1_previousyear <- data.frame(UN_csv$id[13], (mean(springville1_obs_year$prcp, na.rm=TRUE)), (mean(springville1_obs_year$tmax, na.rm=TRUE)), (mean(springville1_obs_year$tmin, na.rm=TRUE))) 
northlogan2_previousyear <- data.frame(UN_csv$id[14], (mean(northlogan2_obs_year$prcp, na.rm=TRUE)), (mean(northlogan2_obs_year$tmax, na.rm=TRUE)), (mean(northlogan2_obs_year$tmin, na.rm=TRUE))) 
moab_previousyear <- data.frame(UN_csv$id[15], (mean(moab_obs_year$prcp, na.rm=TRUE)), (mean(moab_obs_year$tmax, na.rm=TRUE)), (mean(moab_obs_year$tmin, na.rm=TRUE))) 
provo1_previousyear <- data.frame(UN_csv$id[16], (mean(provo1_obs_year$prcp, na.rm=TRUE)), (mean(provo1_obs_year$tmax, na.rm=TRUE)), (mean(provo1_obs_year$tmin, na.rm=TRUE))) 
pleasantgrove_previousyear <- data.frame(UN_csv$id[17], (mean(pleasantgrove_obs_year$prcp, na.rm=TRUE)), (mean(pleasantgrove_obs_year$tmax, na.rm=TRUE)), (mean(pleasantgrove_obs_year$tmin, na.rm=TRUE))) 
provo2_previousyear <- data.frame(UN_csv$id[18], (mean(provo2_obs_year$prcp, na.rm=TRUE)), (mean(provo2_obs_year$tmax, na.rm=TRUE)), (mean(provo2_obs_year$tmin, na.rm=TRUE))) 
saltlakecity4_previousyear <- data.frame(UN_csv$id[19], (mean(saltlakecity4_obs_year$prcp, na.rm=TRUE)), (mean(saltlakecity4_obs_year$tmax, na.rm=TRUE)), (mean(saltlakecity4_obs_year$tmin, na.rm=TRUE))) 
provo3_previousyear <- data.frame(UN_csv$id[20], (mean(provo3_obs_year$prcp, na.rm=TRUE)), (mean(provo3_obs_year$tmax, na.rm=TRUE)), (mean(provo3_obs_year$tmin, na.rm=TRUE))) 
springville2_previousyear <- data.frame(UN_csv$id[21], (mean(springville2_obs_year$prcp, na.rm=TRUE)), (mean(springville2_obs_year$tmax, na.rm=TRUE)), (mean(springville2_obs_year$tmin, na.rm=TRUE))) 
fallon_previousyear <- data.frame(UN_csv$id[22], (mean(fallon_obs_year$prcp, na.rm=TRUE)), (mean(fallon_obs_year$tmax, na.rm=TRUE)), (mean(fallon_obs_year$tmin, na.rm=TRUE))) 
welcome_previousyear <- data.frame(UN_csv$id[23], (mean(welcome_obs_year$prcp, na.rm=TRUE)), (mean(welcome_obs_year$tmax, na.rm=TRUE)), (mean(welcome_obs_year$tmin, na.rm=TRUE)))

#rename col.names 
cat(citySwap3(city2, y = "colnames(", z = "_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')"), sep = "\n")

colnames(eureka1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(cache_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(eureka2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(millcreek_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(herriman_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(saltlakecity1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(saltlakecity2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(saltlakecity3_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(northlogan1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(eureka3_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(murray_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tooele_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(springville1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(northlogan2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(moab_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(provo1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(pleasantgrove_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(provo2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(saltlakecity4_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(provo3_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(springville2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(fallon_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(welcome_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')

## combine previous year before the date of collection data using bind_rows
cat(citySwap7(city2, y = "_previousyear", z = ","), sep = "")
UN_Pyear <- bind_rows(eureka1_previousyear,cache_previousyear,eureka2_previousyear,millcreek_previousyear,herriman_previousyear,saltlakecity1_previousyear,saltlakecity2_previousyear,saltlakecity3_previousyear,northlogan1_previousyear,eureka3_previousyear,murray_previousyear,tooele_previousyear,springville1_previousyear,northlogan2_previousyear,moab_previousyear,provo1_previousyear,pleasantgrove_previousyear,provo2_previousyear,saltlakecity4_previousyear,provo3_previousyear,springville2_previousyear,fallon_previousyear,welcome_previousyear)

# save data
write.xlsx(UN_Pyear, file = "UN_previous_year_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


### Part 4 
#Find data from current year of date of collection 
lapply(df5, cat(citySwap6(x = city2, y = "_obs_yearDOC <- meteo_pull_monitors(", j = city, 
                          z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '", l = year , 
                          k = "-01-01', date_max = '",  p ="-12-31')"), sep = "\n"))

eureka1_obs_yearDOC <- meteo_pull_monitors(eureka_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2019-01-01', date_max = '2019-12-31')
cache_obs_yearDOC <- meteo_pull_monitors(cache_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1967-01-01', date_max = '1967-12-31')
eureka2_obs_yearDOC <- meteo_pull_monitors(eureka_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1995-01-01', date_max = '1995-12-31')
millcreek_obs_yearDOC <- meteo_pull_monitors(millcreek_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1967-01-01', date_max = '1967-12-31')
herriman_obs_yearDOC <- meteo_pull_monitors(herriman_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1995-01-01', date_max = '1995-12-31')
saltlakecity1_obs_yearDOC <- meteo_pull_monitors(saltlakecity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1880-01-01', date_max = '1880-12-31')
saltlakecity2_obs_yearDOC <- meteo_pull_monitors(saltlakecity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1995-01-01', date_max = '1995-12-31')
saltlakecity3_obs_yearDOC <- meteo_pull_monitors(saltlakecity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1880-01-01', date_max = '1880-12-31')
northlogan1_obs_yearDOC <- meteo_pull_monitors(northlogan_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1964-01-01', date_max = '1964-12-31')
eureka3_obs_yearDOC <- meteo_pull_monitors(eureka_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1995-01-01', date_max = '1995-12-31')
murray_obs_yearDOC <- meteo_pull_monitors(murray_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1917-01-01', date_max = '1917-12-31')
tooele_obs_yearDOC <- meteo_pull_monitors(tooele_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1979-01-01', date_max = '1979-12-31')
springville1_obs_yearDOC <- meteo_pull_monitors(springville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2020-01-01', date_max = '2020-12-31')
northlogan2_obs_yearDOC <- meteo_pull_monitors(northlogan_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1998-01-01', date_max = '1998-12-31')
moab_obs_yearDOC <- meteo_pull_monitors(moab_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1986-01-01', date_max = '1986-12-31')
provo1_obs_yearDOC <- meteo_pull_monitors(provo_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1967-01-01', date_max = '1967-12-31')
pleasantgrove_obs_yearDOC <- meteo_pull_monitors(pleasantgrove_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2005-01-01', date_max = '2005-12-31')
provo2_obs_yearDOC <- meteo_pull_monitors(provo_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1966-01-01', date_max = '1966-12-31')
saltlakecity4_obs_yearDOC <- meteo_pull_monitors(saltlakecity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1995-01-01', date_max = '1995-12-31')
provo3_obs_yearDOC <- meteo_pull_monitors(provo_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1938-01-01', date_max = '1938-12-31')
springville2_obs_yearDOC <- meteo_pull_monitors(springville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1932-01-01', date_max = '1932-12-31')
fallon_obs_yearDOC <- meteo_pull_monitors(fallon_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1978-01-01', date_max = '1978-12-31')
welcome_obs_yearDOC <- meteo_pull_monitors(welcome_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1941-01-01', date_max = '1941-12-31')

## make a dataframe of the mean of tmax, tmin, current year of the date of collection
lapply(dflist_nodates, cat(citySwap2(x = city2,  y = "_DOC", z = " <- data.frame(UN_csv$id[", l= lennumber, k = "], (mean(", e = "_obs_yearDOC$prcp, na.rm=TRUE)), (mean(", f = "_obs_yearDOC$tmax, na.rm=TRUE)), (mean(", g = "_obs_yearDOC$tmin, na.rm=TRUE))) "), sep = "\n"))

eureka1_DOC <- data.frame(UN_csv$id[1], (mean(eureka1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(eureka1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(eureka1_obs_yearDOC$tmin, na.rm=TRUE))) 
cache_DOC <- data.frame(UN_csv$id[2], (mean(cache_obs_yearDOC$prcp, na.rm=TRUE)), (mean(cache_obs_yearDOC$tmax, na.rm=TRUE)), (mean(cache_obs_yearDOC$tmin, na.rm=TRUE))) 
eureka2_DOC <- data.frame(UN_csv$id[3], (mean(eureka2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(eureka2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(eureka2_obs_yearDOC$tmin, na.rm=TRUE))) 
millcreek_DOC <- data.frame(UN_csv$id[4], (mean(millcreek_obs_yearDOC$prcp, na.rm=TRUE)), (mean(millcreek_obs_yearDOC$tmax, na.rm=TRUE)), (mean(millcreek_obs_yearDOC$tmin, na.rm=TRUE))) 
herriman_DOC <- data.frame(UN_csv$id[5], (mean(herriman_obs_yearDOC$prcp, na.rm=TRUE)), (mean(herriman_obs_yearDOC$tmax, na.rm=TRUE)), (mean(herriman_obs_yearDOC$tmin, na.rm=TRUE))) 
saltlakecity1_DOC <- data.frame(UN_csv$id[6], (mean(saltlakecity1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(saltlakecity1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(saltlakecity1_obs_yearDOC$tmin, na.rm=TRUE))) 
saltlakecity2_DOC <- data.frame(UN_csv$id[7], (mean(saltlakecity2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(saltlakecity2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(saltlakecity2_obs_yearDOC$tmin, na.rm=TRUE))) 
saltlakecity3_DOC <- data.frame(UN_csv$id[8], (mean(saltlakecity3_obs_yearDOC$prcp, na.rm=TRUE)), (mean(saltlakecity3_obs_yearDOC$tmax, na.rm=TRUE)), (mean(saltlakecity3_obs_yearDOC$tmin, na.rm=TRUE))) 
northlogan1_DOC <- data.frame(UN_csv$id[9], (mean(northlogan1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(northlogan1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(northlogan1_obs_yearDOC$tmin, na.rm=TRUE))) 
eureka3_DOC <- data.frame(UN_csv$id[10], (mean(eureka3_obs_yearDOC$prcp, na.rm=TRUE)), (mean(eureka3_obs_yearDOC$tmax, na.rm=TRUE)), (mean(eureka3_obs_yearDOC$tmin, na.rm=TRUE))) 
murray_DOC <- data.frame(UN_csv$id[11], (mean(murray_obs_yearDOC$prcp, na.rm=TRUE)), (mean(murray_obs_yearDOC$tmax, na.rm=TRUE)), (mean(murray_obs_yearDOC$tmin, na.rm=TRUE))) 
tooele_DOC <- data.frame(UN_csv$id[12], (mean(tooele_obs_yearDOC$prcp, na.rm=TRUE)), (mean(tooele_obs_yearDOC$tmax, na.rm=TRUE)), (mean(tooele_obs_yearDOC$tmin, na.rm=TRUE))) 
springville1_DOC <- data.frame(UN_csv$id[13], (mean(springville1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(springville1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(springville1_obs_yearDOC$tmin, na.rm=TRUE))) 
northlogan2_DOC <- data.frame(UN_csv$id[14], (mean(northlogan2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(northlogan2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(northlogan2_obs_yearDOC$tmin, na.rm=TRUE))) 
moab_DOC <- data.frame(UN_csv$id[15], (mean(moab_obs_yearDOC$prcp, na.rm=TRUE)), (mean(moab_obs_yearDOC$tmax, na.rm=TRUE)), (mean(moab_obs_yearDOC$tmin, na.rm=TRUE))) 
provo1_DOC <- data.frame(UN_csv$id[16], (mean(provo1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(provo1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(provo1_obs_yearDOC$tmin, na.rm=TRUE))) 
pleasantgrove_DOC <- data.frame(UN_csv$id[17], (mean(pleasantgrove_obs_yearDOC$prcp, na.rm=TRUE)), (mean(pleasantgrove_obs_yearDOC$tmax, na.rm=TRUE)), (mean(pleasantgrove_obs_yearDOC$tmin, na.rm=TRUE))) 
provo2_DOC <- data.frame(UN_csv$id[18], (mean(provo2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(provo2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(provo2_obs_yearDOC$tmin, na.rm=TRUE))) 
saltlakecity4_DOC <- data.frame(UN_csv$id[19], (mean(saltlakecity4_obs_yearDOC$prcp, na.rm=TRUE)), (mean(saltlakecity4_obs_yearDOC$tmax, na.rm=TRUE)), (mean(saltlakecity4_obs_yearDOC$tmin, na.rm=TRUE))) 
provo3_DOC <- data.frame(UN_csv$id[20], (mean(provo3_obs_yearDOC$prcp, na.rm=TRUE)), (mean(provo3_obs_yearDOC$tmax, na.rm=TRUE)), (mean(provo3_obs_yearDOC$tmin, na.rm=TRUE))) 
springville2_DOC <- data.frame(UN_csv$id[21], (mean(springville2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(springville2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(springville2_obs_yearDOC$tmin, na.rm=TRUE))) 
fallon_DOC <- data.frame(UN_csv$id[22], (mean(fallon_obs_yearDOC$prcp, na.rm=TRUE)), (mean(fallon_obs_yearDOC$tmax, na.rm=TRUE)), (mean(fallon_obs_yearDOC$tmin, na.rm=TRUE))) 
welcome_DOC <- data.frame(UN_csv$id[23], (mean(welcome_obs_yearDOC$prcp, na.rm=TRUE)), (mean(welcome_obs_yearDOC$tmax, na.rm=TRUE)), (mean(welcome_obs_yearDOC$tmin, na.rm=TRUE)))

#rename col.names 
cat(citySwap3(city2, y = "colnames(", z = "_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')"), sep = "\n")

colnames(eureka1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(cache_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(eureka2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(millcreek_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(herriman_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(saltlakecity1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(saltlakecity2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(saltlakecity3_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(northlogan1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(eureka3_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(murray_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tooele_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(springville1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(northlogan2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(moab_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(provo1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(pleasantgrove_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(provo2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(saltlakecity4_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(provo3_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(springville2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(fallon_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(welcome_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')

## combine current year of the date of collection data using bind_rows
cat(citySwap7(city2, y = "_DOC", z = ","), sep = "")
UN_yearDOC <- bind_rows(eureka1_DOC,cache_DOC,eureka2_DOC,millcreek_DOC,herriman_DOC,saltlakecity1_DOC,saltlakecity2_DOC,saltlakecity3_DOC,northlogan1_DOC,eureka3_DOC,murray_DOC,tooele_DOC,springville1_DOC,northlogan2_DOC,moab_DOC,provo1_DOC,pleasantgrove_DOC,provo2_DOC,saltlakecity4_DOC,provo3_DOC,springville2_DOC,fallon_DOC,welcome_DOC)

# save data
write.xlsx(UN_yearDOC, file = "UN_year_DOC_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
