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
GA_csv <- read.csv('georgia_cities.csv')
GA_csv$month <- match(GA_csv$month, month.name)
GA_csv$date <- paste(GA_csv$year, GA_csv$month, GA_csv$day, sep="-")
strptime(GA_csv$date,format="%Y-%m-%d")
GA_csv$date <- as.Date(GA_csv$date)
colnames(GA_csv) <- c("id", 'id2', 'city_county_state', 'latitude', 'longitude', 'elevation', 'month', 'day', 'year', 'date')
GA_csv$previous6mon <- as.Date(as.yearmon(as.Date(GA_csv$date)) -.5, frac = 1)
head(GA_csv)

## to use lapply 
city <- GA_csv$id
city2 <- GA_csv$id2
date <- GA_csv$date
pyear <- (GA_csv$date - 365)
p6mon <- GA_csv$previous6mon
year <- GA_csv$year
lennumber <- (1:25) #number of samples

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

south2_stations <- stations %>% filter(state == c("FL", "GA", "AL", "TN", "SC"))
write.csv2(south2_stations, "south2_stations.csv")

#after running the 1st time 
south2_stations <- read.csv("south2_stations.csv")


#find closests stations
GA_clst <- meteo_nearby_stations(lat_lon_df = GA_csv, station_data = south2_stations, radius = 50, limit = 200)

### separate all cities 
cat(citySwap(unique(city), y = " <- (GA_clst$'", z = "')"), sep = "\n")

madison <- (GA_clst$'madison')
darien <- (GA_clst$'darien')
statesboro <- (GA_clst$'statesboro')
newton <- (GA_clst$'newton')
athens <- (GA_clst$'athens')
bowman <- (GA_clst$'bowman')
gray <- (GA_clst$'gray')
dalton <- (GA_clst$'dalton')
milledgeville <- (GA_clst$'milledgeville')
cochran <- (GA_clst$'cochran')
macon <- (GA_clst$'macon')
quitman <- (GA_clst$'quitman')
colquit <- (GA_clst$'colquit')
rome <- (GA_clst$'rome')

### separate all station id's (monitors) for each city
cat(citySwap(unique(city), y = "_monitors <- ", z = "$id"), sep = "\n")

madison_monitors <- madison$id
darien_monitors <- darien$id
statesboro_monitors <- statesboro$id
newton_monitors <- newton$id
athens_monitors <- athens$id
bowman_monitors <- bowman$id
gray_monitors <- gray$id
dalton_monitors <- dalton$id
milledgeville_monitors <- milledgeville$id
cochran_monitors <- cochran$id
macon_monitors <- macon$id
quitman_monitors <- quitman$id
colquit_monitors <- colquit$id
rome_monitors <- rome$id

### pull all observations for cities 
cat(citySwap(unique(city), y = "_obs <- meteo_pull_monitors(", z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')"), sep = "\n")

madison_obs <- meteo_pull_monitors(madison_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
darien_obs <- meteo_pull_monitors(darien_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
statesboro_obs <- meteo_pull_monitors(statesboro_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
newton_obs <- meteo_pull_monitors(newton_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
athens_obs <- meteo_pull_monitors(athens_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
bowman_obs <- meteo_pull_monitors(bowman_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
gray_obs <- meteo_pull_monitors(gray_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
dalton_obs <- meteo_pull_monitors(dalton_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
milledgeville_obs <- meteo_pull_monitors(milledgeville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
cochran_obs <- meteo_pull_monitors(cochran_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
macon_obs <- meteo_pull_monitors(macon_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
quitman_obs <- meteo_pull_monitors(quitman_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
colquit_obs <- meteo_pull_monitors(colquit_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
rome_obs <- meteo_pull_monitors(rome_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')

### Part 1
### pull data by date and location - dates are coming from state_csv 
lapply(df1, cat(citySwap4.5(x = city2, y = "_obs_temps <- ", i = city, z = "_obs %>% filter(date == '", j = date , h = "')"), sep = "\n"))


madison_obs_temps <- madison_obs %>% filter(date == '1991-03-31')
darien1_obs_temps <- darien_obs %>% filter(date == '1958-03-24')
statesboro1_obs_temps <- statesboro_obs %>% filter(date == '1963-03-16')
newton_obs_temps <- newton_obs %>% filter(date == '2017-03-02')
athens1_obs_temps <- athens_obs %>% filter(date == '1964-04-01')
bowman_obs_temps <- bowman_obs %>% filter(date == '1978-03-25')
athens2_obs_temps <- athens_obs %>% filter(date == '1929-04-03')
gray_obs_temps <- gray_obs %>% filter(date == '1965-04-11')
dalton_obs_temps <- dalton_obs %>% filter(date == '1968-05-05')
darien2_obs_temps <- darien_obs %>% filter(date == '1959-12-29')
darien3_obs_temps <- darien_obs %>% filter(date == '1958-03-24')
milledgeville_obs_temps <- milledgeville_obs %>% filter(date == '1937-04-03')
athens3_obs_temps <- athens_obs %>% filter(date == '1911-04-14')
cochran1_obs_temps <- cochran_obs %>% filter(date == '1972-03-25')
macon_obs_temps <- macon_obs %>% filter(date == '1979-10-01')
cochran2_obs_temps <- cochran_obs %>% filter(date == '2010-03-07')
cochran3_obs_temps <- cochran_obs %>% filter(date == '2010-03-07')
quitman1_obs_temps <- quitman_obs %>% filter(date == '1966-04-17')
quitman2_obs_temps <- quitman_obs %>% filter(date == '1966-04-17')
quitman3_obs_temps <- quitman_obs %>% filter(date == '1966-04-17')
statesboro2_obs_temps <- statesboro_obs %>% filter(date == '1963-03-16')
colquit_obs_temps <- colquit_obs %>% filter(date == '1976-03-13')
athens4_obs_temps <- athens_obs %>% filter(date == '1917-04-01')
rome1_obs_temps <- rome_obs %>% filter(date == '1936-04-02')
rome2_obs_temps <- rome_obs %>% filter(date == '1938-02-12')

#combine into one data frame using bind_rows()
cat(citySwap7(city2, y = "_obs_temps", z = ","), sep = "")
GA_obs <- bind_rows(madison_obs_temps,darien1_obs_temps,statesboro1_obs_temps,newton_obs_temps,athens1_obs_temps,bowman_obs_temps,athens2_obs_temps,gray_obs_temps,dalton_obs_temps,darien2_obs_temps,darien3_obs_temps,milledgeville_obs_temps,athens3_obs_temps,cochran1_obs_temps,macon_obs_temps,cochran2_obs_temps,cochran3_obs_temps,quitman1_obs_temps,quitman2_obs_temps,quitman3_obs_temps,statesboro2_obs_temps,colquit_obs_temps,athens4_obs_temps,rome1_obs_temps,rome2_obs_temps)

#Merge the original state_csv dataframe and new state_obs dataframe 
GA_data <- merge.data.frame(GA_csv, GA_obs, by = 'date')

## find the tavg for state data 
GA_data$tavg <- ((GA_data$tmax + GA_data$tmin) / 2)

### save data as xlsx file 
library("xlsx")
write.xlsx(GA_data, file = "GA_data_total_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

### Part 2
#finding data from 6 months prior to date of collection
#ambient temps and precipitation may have affected plant/leaf growth and shape
lapply(df2, cat(citySwap5(x = city2, y = "_obs_6months <- meteo_pull_monitors(", j = city, 
                          z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '", h = p6mon, 
                          k = "', date_max = '", l = date, p ="')"), sep = "\n"))

madison_obs_6months <- meteo_pull_monitors(madison_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1990-09-30', date_max = '1991-03-31')
darien1_obs_6months <- meteo_pull_monitors(darien_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1957-09-30', date_max = '1958-03-24')
statesboro1_obs_6months <- meteo_pull_monitors(statesboro_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1962-09-30', date_max = '1963-03-16')
newton_obs_6months <- meteo_pull_monitors(newton_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2016-09-30', date_max = '2017-03-02')
athens1_obs_6months <- meteo_pull_monitors(athens_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1963-10-31', date_max = '1964-04-01')
bowman_obs_6months <- meteo_pull_monitors(bowman_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1977-09-30', date_max = '1978-03-25')
athens2_obs_6months <- meteo_pull_monitors(athens_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1928-10-31', date_max = '1929-04-03')
gray_obs_6months <- meteo_pull_monitors(gray_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1964-10-31', date_max = '1965-04-11')
dalton_obs_6months <- meteo_pull_monitors(dalton_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1967-11-30', date_max = '1968-05-05')
darien2_obs_6months <- meteo_pull_monitors(darien_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1959-06-30', date_max = '1959-12-29')
darien3_obs_6months <- meteo_pull_monitors(darien_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1957-09-30', date_max = '1958-03-24')
milledgeville_obs_6months <- meteo_pull_monitors(milledgeville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1936-10-31', date_max = '1937-04-03')
athens3_obs_6months <- meteo_pull_monitors(athens_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1910-10-31', date_max = '1911-04-14')
cochran1_obs_6months <- meteo_pull_monitors(cochran_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1971-09-30', date_max = '1972-03-25')
macon_obs_6months <- meteo_pull_monitors(macon_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1979-04-30', date_max = '1979-10-01')
cochran2_obs_6months <- meteo_pull_monitors(cochran_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2009-09-30', date_max = '2010-03-07')
cochran3_obs_6months <- meteo_pull_monitors(cochran_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2009-09-30', date_max = '2010-03-07')
quitman1_obs_6months <- meteo_pull_monitors(quitman_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1965-10-31', date_max = '1966-04-17')
quitman2_obs_6months <- meteo_pull_monitors(quitman_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1965-10-31', date_max = '1966-04-17')
quitman3_obs_6months <- meteo_pull_monitors(quitman_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1965-10-31', date_max = '1966-04-17')
statesboro2_obs_6months <- meteo_pull_monitors(statesboro_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1962-09-30', date_max = '1963-03-16')
colquit_obs_6months <- meteo_pull_monitors(colquit_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1975-09-30', date_max = '1976-03-13')
athens4_obs_6months <- meteo_pull_monitors(athens_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1916-10-31', date_max = '1917-04-01')
rome1_obs_6months <- meteo_pull_monitors(rome_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1935-10-31', date_max = '1936-04-02')
rome2_obs_6months <- meteo_pull_monitors(rome_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1937-08-31', date_max = '1938-02-12')

## make a dataframe of the mean of tmax, tmin, prcp of the previous 6 months 
lapply(df3, cat(citySwap2(x = city2,  y = "_6months", z = " <- data.frame(GA_csv$id[", l= lennumber, k = "], (mean(", e = "_obs_6months$prcp, na.rm=TRUE)), (mean(", f = "_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(", g = "_obs_6months$tmin, na.rm=TRUE))) "), sep = "\n"))

madison_6months <- data.frame(GA_csv$id[1], (mean(madison_obs_6months$prcp, na.rm=TRUE)), (mean(madison_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(madison_obs_6months$tmin, na.rm=TRUE))) 
darien1_6months <- data.frame(GA_csv$id[2], (mean(darien1_obs_6months$prcp, na.rm=TRUE)), (mean(darien1_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(darien1_obs_6months$tmin, na.rm=TRUE))) 
statesboro1_6months <- data.frame(GA_csv$id[3], (mean(statesboro1_obs_6months$prcp, na.rm=TRUE)), (mean(statesboro1_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(statesboro1_obs_6months$tmin, na.rm=TRUE))) 
newton_6months <- data.frame(GA_csv$id[4], (mean(newton_obs_6months$prcp, na.rm=TRUE)), (mean(newton_obs_6months$tmax, na.rm=TRUE)), 
                             (mean(newton_obs_6months$tmin, na.rm=TRUE))) 
athens1_6months <- data.frame(GA_csv$id[5], (mean(athens1_obs_6months$prcp, na.rm=TRUE)), (mean(athens1_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(athens1_obs_6months$tmin, na.rm=TRUE))) 
bowman_6months <- data.frame(GA_csv$id[6], (mean(bowman_obs_6months$prcp, na.rm=TRUE)), (mean(bowman_obs_6months$tmax, na.rm=TRUE)), 
                             (mean(bowman_obs_6months$tmin, na.rm=TRUE))) 
athens2_6months <- data.frame(GA_csv$id[7], (mean(athens2_obs_6months$prcp, na.rm=TRUE)), (mean(athens2_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(athens2_obs_6months$tmin, na.rm=TRUE))) 
gray_6months <- data.frame(GA_csv$id[8], (mean(gray_obs_6months$prcp, na.rm=TRUE)), (mean(gray_obs_6months$tmax, na.rm=TRUE)), 
                           (mean(gray_obs_6months$tmin, na.rm=TRUE))) 
dalton_6months <- data.frame(GA_csv$id[9], (mean(dalton_obs_6months$prcp, na.rm=TRUE)), (mean(dalton_obs_6months$tmax, na.rm=TRUE)), 
                             (mean(dalton_obs_6months$tmin, na.rm=TRUE))) 
darien2_6months <- data.frame(GA_csv$id[10], (mean(darien2_obs_6months$prcp, na.rm=TRUE)), (mean(darien2_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(darien2_obs_6months$tmin, na.rm=TRUE))) 
darien3_6months <- data.frame(GA_csv$id[11], (mean(darien3_obs_6months$prcp, na.rm=TRUE)), (mean(darien3_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(darien3_obs_6months$tmin, na.rm=TRUE))) 
milledgeville_6months <- data.frame(GA_csv$id[12], (mean(milledgeville_obs_6months$prcp, na.rm=TRUE)), (mean(milledgeville_obs_6months$tmax, na.rm=TRUE)), 
                                    (mean(milledgeville_obs_6months$tmin, na.rm=TRUE))) 
athens3_6months <- data.frame(GA_csv$id[13], (mean(athens3_obs_6months$prcp, na.rm=TRUE)), (mean(athens3_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(athens3_obs_6months$tmin, na.rm=TRUE))) 
cochran1_6months <- data.frame(GA_csv$id[14], (mean(cochran1_obs_6months$prcp, na.rm=TRUE)), (mean(cochran1_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(cochran1_obs_6months$tmin, na.rm=TRUE))) 
macon_6months <- data.frame(GA_csv$id[15], (mean(macon_obs_6months$prcp, na.rm=TRUE)), (mean(macon_obs_6months$tmax, na.rm=TRUE)), 
                            (mean(macon_obs_6months$tmin, na.rm=TRUE))) 
cochran2_6months <- data.frame(GA_csv$id[16], (mean(cochran2_obs_6months$prcp, na.rm=TRUE)), (mean(cochran2_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(cochran2_obs_6months$tmin, na.rm=TRUE))) 
cochran3_6months <- data.frame(GA_csv$id[17], (mean(cochran3_obs_6months$prcp, na.rm=TRUE)), (mean(cochran3_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(cochran3_obs_6months$tmin, na.rm=TRUE))) 
quitman1_6months <- data.frame(GA_csv$id[18], (mean(quitman1_obs_6months$prcp, na.rm=TRUE)), (mean(quitman1_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(quitman1_obs_6months$tmin, na.rm=TRUE))) 
quitman2_6months <- data.frame(GA_csv$id[19], (mean(quitman2_obs_6months$prcp, na.rm=TRUE)), (mean(quitman2_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(quitman2_obs_6months$tmin, na.rm=TRUE))) 
quitman3_6months <- data.frame(GA_csv$id[20], (mean(quitman3_obs_6months$prcp, na.rm=TRUE)), (mean(quitman3_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(quitman3_obs_6months$tmin, na.rm=TRUE))) 
statesboro2_6months <- data.frame(GA_csv$id[21], (mean(statesboro2_obs_6months$prcp, na.rm=TRUE)), (mean(statesboro2_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(statesboro2_obs_6months$tmin, na.rm=TRUE))) 
colquit_6months <- data.frame(GA_csv$id[22], (mean(colquit_obs_6months$prcp, na.rm=TRUE)), (mean(colquit_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(colquit_obs_6months$tmin, na.rm=TRUE))) 
athens4_6months <- data.frame(GA_csv$id[23], (mean(athens4_obs_6months$prcp, na.rm=TRUE)), (mean(athens4_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(athens4_obs_6months$tmin, na.rm=TRUE))) 
rome1_6months <- data.frame(GA_csv$id[24], (mean(rome1_obs_6months$prcp, na.rm=TRUE)), (mean(rome1_obs_6months$tmax, na.rm=TRUE)), 
                            (mean(rome1_obs_6months$tmin, na.rm=TRUE))) 
rome2_6months <- data.frame(GA_csv$id[25], (mean(rome2_obs_6months$prcp, na.rm=TRUE)), (mean(rome2_obs_6months$tmax, na.rm=TRUE)), 
                            (mean(rome2_obs_6months$tmin, na.rm=TRUE)))

#rename col.names 
cat(citySwap3(city2, y = "colnames(", z = "_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')"), sep = "\n")

colnames(madison_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(darien1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(statesboro1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(newton_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(athens1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(bowman_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(athens2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(gray_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(dalton_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(darien2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(darien3_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(milledgeville_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(athens3_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(cochran1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(macon_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(cochran2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(cochran3_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(quitman1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(quitman2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(quitman3_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(statesboro2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(colquit_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(athens4_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(rome1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(rome2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')

## combine previous 6 months data using bind_rows
cat(citySwap7(city2, y = "_6months", z = ","), sep = "")
GA_6months <- bind_rows(madison_6months,darien1_6months,statesboro1_6months,newton_6months,athens1_6months,bowman_6months,athens2_6months,gray_6months,dalton_6months,darien2_6months,darien3_6months,milledgeville_6months,athens3_6months,cochran1_6months,macon_6months,cochran2_6months,cochran3_6months,quitman1_6months,quitman2_6months,quitman3_6months,statesboro2_6months,colquit_6months,athens4_6months,rome1_6months,rome2_6months)

### save data as xlsx file 
write.xlsx(GA_6months, file = "GA_6months_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


### Part 3
#find data from previous year from date of collection 
lapply(df4, cat(citySwap5(x = city2, y = "_obs_year <- meteo_pull_monitors(", j = city, 
                          z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '", h= pyear, 
                          k = "', date_max = '", l = date, p ="')"), sep = "\n"))

madison_obs_year <- meteo_pull_monitors(madison_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1990-03-31', date_max = '1991-03-31')
darien1_obs_year <- meteo_pull_monitors(darien_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1957-03-24', date_max = '1958-03-24')
statesboro1_obs_year <- meteo_pull_monitors(statesboro_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1962-03-16', date_max = '1963-03-16')
newton_obs_year <- meteo_pull_monitors(newton_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2016-03-02', date_max = '2017-03-02')
athens1_obs_year <- meteo_pull_monitors(athens_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1963-04-02', date_max = '1964-04-01')
bowman_obs_year <- meteo_pull_monitors(bowman_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1977-03-25', date_max = '1978-03-25')
athens2_obs_year <- meteo_pull_monitors(athens_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1928-04-03', date_max = '1929-04-03')
gray_obs_year <- meteo_pull_monitors(gray_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1964-04-11', date_max = '1965-04-11')
dalton_obs_year <- meteo_pull_monitors(dalton_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1967-05-06', date_max = '1968-05-05')
darien2_obs_year <- meteo_pull_monitors(darien_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1958-12-29', date_max = '1959-12-29')
darien3_obs_year <- meteo_pull_monitors(darien_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1957-03-24', date_max = '1958-03-24')
milledgeville_obs_year <- meteo_pull_monitors(milledgeville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1936-04-03', date_max = '1937-04-03')
athens3_obs_year <- meteo_pull_monitors(athens_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1910-04-14', date_max = '1911-04-14')
cochran1_obs_year <- meteo_pull_monitors(cochran_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1971-03-26', date_max = '1972-03-25')
macon_obs_year <- meteo_pull_monitors(macon_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1978-10-01', date_max = '1979-10-01')
cochran2_obs_year <- meteo_pull_monitors(cochran_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2009-03-07', date_max = '2010-03-07')
cochran3_obs_year <- meteo_pull_monitors(cochran_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2009-03-07', date_max = '2010-03-07')
quitman1_obs_year <- meteo_pull_monitors(quitman_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1965-04-17', date_max = '1966-04-17')
quitman2_obs_year <- meteo_pull_monitors(quitman_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1965-04-17', date_max = '1966-04-17')
quitman3_obs_year <- meteo_pull_monitors(quitman_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1965-04-17', date_max = '1966-04-17')
statesboro2_obs_year <- meteo_pull_monitors(statesboro_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1962-03-16', date_max = '1963-03-16')
colquit_obs_year <- meteo_pull_monitors(colquit_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1975-03-14', date_max = '1976-03-13')
athens4_obs_year <- meteo_pull_monitors(athens_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1916-04-01', date_max = '1917-04-01')
rome1_obs_year <- meteo_pull_monitors(rome_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1935-04-03', date_max = '1936-04-02')
rome2_obs_year <- meteo_pull_monitors(rome_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1937-02-12', date_max = '1938-02-12')

## make a data frame of the mean of tmax, tmin, prcp of the previous year before the date of collection 
lapply(df3, cat(citySwap2(x = city2,  y = "_previousyear", z = " <- data.frame(GA_csv$id[", l= lennumber, 
                          k = "], (mean(", e = "_obs_year$prcp, na.rm=TRUE)), (mean(", 
                          f = "_obs_year$tmax, na.rm=TRUE)), (mean(", g = "_obs_year$tmin, na.rm=TRUE))) "), sep = "\n"))

madison_previousyear <- data.frame(GA_csv$id[1], (mean(madison_obs_year$prcp, na.rm=TRUE)), (mean(madison_obs_year$tmax, na.rm=TRUE)), (mean(madison_obs_year$tmin, na.rm=TRUE))) 
darien1_previousyear <- data.frame(GA_csv$id[2], (mean(darien1_obs_year$prcp, na.rm=TRUE)), (mean(darien1_obs_year$tmax, na.rm=TRUE)), (mean(darien1_obs_year$tmin, na.rm=TRUE))) 
statesboro1_previousyear <- data.frame(GA_csv$id[3], (mean(statesboro1_obs_year$prcp, na.rm=TRUE)), (mean(statesboro1_obs_year$tmax, na.rm=TRUE)), (mean(statesboro1_obs_year$tmin, na.rm=TRUE))) 
newton_previousyear <- data.frame(GA_csv$id[4], (mean(newton_obs_year$prcp, na.rm=TRUE)), (mean(newton_obs_year$tmax, na.rm=TRUE)), (mean(newton_obs_year$tmin, na.rm=TRUE))) 
athens1_previousyear <- data.frame(GA_csv$id[5], (mean(athens1_obs_year$prcp, na.rm=TRUE)), (mean(athens1_obs_year$tmax, na.rm=TRUE)), (mean(athens1_obs_year$tmin, na.rm=TRUE))) 
bowman_previousyear <- data.frame(GA_csv$id[6], (mean(bowman_obs_year$prcp, na.rm=TRUE)), (mean(bowman_obs_year$tmax, na.rm=TRUE)), (mean(bowman_obs_year$tmin, na.rm=TRUE))) 
athens2_previousyear <- data.frame(GA_csv$id[7], (mean(athens2_obs_year$prcp, na.rm=TRUE)), (mean(athens2_obs_year$tmax, na.rm=TRUE)), (mean(athens2_obs_year$tmin, na.rm=TRUE))) 
gray_previousyear <- data.frame(GA_csv$id[8], (mean(gray_obs_year$prcp, na.rm=TRUE)), (mean(gray_obs_year$tmax, na.rm=TRUE)), (mean(gray_obs_year$tmin, na.rm=TRUE))) 
dalton_previousyear <- data.frame(GA_csv$id[9], (mean(dalton_obs_year$prcp, na.rm=TRUE)), (mean(dalton_obs_year$tmax, na.rm=TRUE)), (mean(dalton_obs_year$tmin, na.rm=TRUE))) 
darien2_previousyear <- data.frame(GA_csv$id[10], (mean(darien2_obs_year$prcp, na.rm=TRUE)), (mean(darien2_obs_year$tmax, na.rm=TRUE)), (mean(darien2_obs_year$tmin, na.rm=TRUE))) 
darien3_previousyear <- data.frame(GA_csv$id[11], (mean(darien3_obs_year$prcp, na.rm=TRUE)), (mean(darien3_obs_year$tmax, na.rm=TRUE)), (mean(darien3_obs_year$tmin, na.rm=TRUE))) 
milledgeville_previousyear <- data.frame(GA_csv$id[12], (mean(milledgeville_obs_year$prcp, na.rm=TRUE)), (mean(milledgeville_obs_year$tmax, na.rm=TRUE)), (mean(milledgeville_obs_year$tmin, na.rm=TRUE))) 
athens3_previousyear <- data.frame(GA_csv$id[13], (mean(athens3_obs_year$prcp, na.rm=TRUE)), (mean(athens3_obs_year$tmax, na.rm=TRUE)), (mean(athens3_obs_year$tmin, na.rm=TRUE))) 
cochran1_previousyear <- data.frame(GA_csv$id[14], (mean(cochran1_obs_year$prcp, na.rm=TRUE)), (mean(cochran1_obs_year$tmax, na.rm=TRUE)), (mean(cochran1_obs_year$tmin, na.rm=TRUE))) 
macon_previousyear <- data.frame(GA_csv$id[15], (mean(macon_obs_year$prcp, na.rm=TRUE)), (mean(macon_obs_year$tmax, na.rm=TRUE)), (mean(macon_obs_year$tmin, na.rm=TRUE))) 
cochran2_previousyear <- data.frame(GA_csv$id[16], (mean(cochran2_obs_year$prcp, na.rm=TRUE)), (mean(cochran2_obs_year$tmax, na.rm=TRUE)), (mean(cochran2_obs_year$tmin, na.rm=TRUE))) 
cochran3_previousyear <- data.frame(GA_csv$id[17], (mean(cochran3_obs_year$prcp, na.rm=TRUE)), (mean(cochran3_obs_year$tmax, na.rm=TRUE)), (mean(cochran3_obs_year$tmin, na.rm=TRUE))) 
quitman1_previousyear <- data.frame(GA_csv$id[18], (mean(quitman1_obs_year$prcp, na.rm=TRUE)), (mean(quitman1_obs_year$tmax, na.rm=TRUE)), (mean(quitman1_obs_year$tmin, na.rm=TRUE))) 
quitman2_previousyear <- data.frame(GA_csv$id[19], (mean(quitman2_obs_year$prcp, na.rm=TRUE)), (mean(quitman2_obs_year$tmax, na.rm=TRUE)), (mean(quitman2_obs_year$tmin, na.rm=TRUE))) 
quitman3_previousyear <- data.frame(GA_csv$id[20], (mean(quitman3_obs_year$prcp, na.rm=TRUE)), (mean(quitman3_obs_year$tmax, na.rm=TRUE)), (mean(quitman3_obs_year$tmin, na.rm=TRUE))) 
statesboro2_previousyear <- data.frame(GA_csv$id[21], (mean(statesboro2_obs_year$prcp, na.rm=TRUE)), (mean(statesboro2_obs_year$tmax, na.rm=TRUE)), (mean(statesboro2_obs_year$tmin, na.rm=TRUE))) 
colquit_previousyear <- data.frame(GA_csv$id[22], (mean(colquit_obs_year$prcp, na.rm=TRUE)), (mean(colquit_obs_year$tmax, na.rm=TRUE)), (mean(colquit_obs_year$tmin, na.rm=TRUE))) 
athens4_previousyear <- data.frame(GA_csv$id[23], (mean(athens4_obs_year$prcp, na.rm=TRUE)), (mean(athens4_obs_year$tmax, na.rm=TRUE)), (mean(athens4_obs_year$tmin, na.rm=TRUE))) 
rome1_previousyear <- data.frame(GA_csv$id[24], (mean(rome1_obs_year$prcp, na.rm=TRUE)), (mean(rome1_obs_year$tmax, na.rm=TRUE)), (mean(rome1_obs_year$tmin, na.rm=TRUE))) 
rome2_previousyear <- data.frame(GA_csv$id[25], (mean(rome2_obs_year$prcp, na.rm=TRUE)), (mean(rome2_obs_year$tmax, na.rm=TRUE)), (mean(rome2_obs_year$tmin, na.rm=TRUE)))

#rename col.names 
cat(citySwap3(city2, y = "colnames(", z = "_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')"), sep = "\n")

colnames(madison_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(darien1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(statesboro1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(newton_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(athens1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(bowman_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(athens2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(gray_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(dalton_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(darien2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(darien3_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(milledgeville_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(athens3_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(cochran1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(macon_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(cochran2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(cochran3_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(quitman1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(quitman2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(quitman3_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(statesboro2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(colquit_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(athens4_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(rome1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(rome2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')

## combine previous year before the date of collection data using bind_rows
cat(citySwap7(city2, y = "_previousyear", z = ","), sep = "")
GA_Pyear <- bind_rows(madison_previousyear,darien1_previousyear,statesboro1_previousyear,newton_previousyear,athens1_previousyear,bowman_previousyear,athens2_previousyear,gray_previousyear,dalton_previousyear,darien2_previousyear,darien3_previousyear,milledgeville_previousyear,athens3_previousyear,cochran1_previousyear,macon_previousyear,cochran2_previousyear,cochran3_previousyear,quitman1_previousyear,quitman2_previousyear,quitman3_previousyear,statesboro2_previousyear,colquit_previousyear,athens4_previousyear,rome1_previousyear,rome2_previousyear)

# save data
write.xlsx(GA_Pyear, file = "GA_previous_year_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


### Part 4 
#Find data from current year of date of collection 
lapply(df5, cat(citySwap6(x = city2, y = "_obs_yearDOC <- meteo_pull_monitors(", j = city, 
                          z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '", l = year , 
                          k = "-01-01', date_max = '",  p ="-12-31')"), sep = "\n"))

madison_obs_yearDOC <- meteo_pull_monitors(madison_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1991-01-01', date_max = '1991-12-31')
darien1_obs_yearDOC <- meteo_pull_monitors(darien_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1958-01-01', date_max = '1958-12-31')
statesboro1_obs_yearDOC <- meteo_pull_monitors(statesboro_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1963-01-01', date_max = '1963-12-31')
newton_obs_yearDOC <- meteo_pull_monitors(newton_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2017-01-01', date_max = '2017-12-31')
athens1_obs_yearDOC <- meteo_pull_monitors(athens_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1964-01-01', date_max = '1964-12-31')
bowman_obs_yearDOC <- meteo_pull_monitors(bowman_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1978-01-01', date_max = '1978-12-31')
athens2_obs_yearDOC <- meteo_pull_monitors(athens_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1929-01-01', date_max = '1929-12-31')
gray_obs_yearDOC <- meteo_pull_monitors(gray_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1965-01-01', date_max = '1965-12-31')
dalton_obs_yearDOC <- meteo_pull_monitors(dalton_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1968-01-01', date_max = '1968-12-31')
darien2_obs_yearDOC <- meteo_pull_monitors(darien_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1959-01-01', date_max = '1959-12-31')
darien3_obs_yearDOC <- meteo_pull_monitors(darien_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1958-01-01', date_max = '1958-12-31')
milledgeville_obs_yearDOC <- meteo_pull_monitors(milledgeville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1937-01-01', date_max = '1937-12-31')
athens3_obs_yearDOC <- meteo_pull_monitors(athens_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1911-01-01', date_max = '1911-12-31')
cochran1_obs_yearDOC <- meteo_pull_monitors(cochran_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1972-01-01', date_max = '1972-12-31')
macon_obs_yearDOC <- meteo_pull_monitors(macon_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1979-01-01', date_max = '1979-12-31')
cochran2_obs_yearDOC <- meteo_pull_monitors(cochran_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2010-01-01', date_max = '2010-12-31')
cochran3_obs_yearDOC <- meteo_pull_monitors(cochran_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2010-01-01', date_max = '2010-12-31')
quitman1_obs_yearDOC <- meteo_pull_monitors(quitman_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1966-01-01', date_max = '1966-12-31')
quitman2_obs_yearDOC <- meteo_pull_monitors(quitman_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1966-01-01', date_max = '1966-12-31')
quitman3_obs_yearDOC <- meteo_pull_monitors(quitman_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1966-01-01', date_max = '1966-12-31')
statesboro2_obs_yearDOC <- meteo_pull_monitors(statesboro_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1963-01-01', date_max = '1963-12-31')
colquit_obs_yearDOC <- meteo_pull_monitors(colquit_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1976-01-01', date_max = '1976-12-31')
athens4_obs_yearDOC <- meteo_pull_monitors(athens_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1917-01-01', date_max = '1917-12-31')
rome1_obs_yearDOC <- meteo_pull_monitors(rome_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1936-01-01', date_max = '1936-12-31')
rome2_obs_yearDOC <- meteo_pull_monitors(rome_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1938-01-01', date_max = '1938-12-31')

## make a dataframe of the mean of tmax, tmin, current year of the date of collection
lapply(dflist_nodates, cat(citySwap2(x = city2,  y = "_DOC", z = " <- data.frame(GA_csv$id[", l= lennumber, k = "], (mean(", e = "_obs_yearDOC$prcp, na.rm=TRUE)), (mean(", f = "_obs_yearDOC$tmax, na.rm=TRUE)), (mean(", g = "_obs_yearDOC$tmin, na.rm=TRUE))) "), sep = "\n"))

madison_DOC <- data.frame(GA_csv$id[1], (mean(madison_obs_yearDOC$prcp, na.rm=TRUE)), (mean(madison_obs_yearDOC$tmax, na.rm=TRUE)), (mean(madison_obs_yearDOC$tmin, na.rm=TRUE))) 
darien1_DOC <- data.frame(GA_csv$id[2], (mean(darien1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(darien1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(darien1_obs_yearDOC$tmin, na.rm=TRUE))) 
statesboro1_DOC <- data.frame(GA_csv$id[3], (mean(statesboro1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(statesboro1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(statesboro1_obs_yearDOC$tmin, na.rm=TRUE))) 
newton_DOC <- data.frame(GA_csv$id[4], (mean(newton_obs_yearDOC$prcp, na.rm=TRUE)), (mean(newton_obs_yearDOC$tmax, na.rm=TRUE)), (mean(newton_obs_yearDOC$tmin, na.rm=TRUE))) 
athens1_DOC <- data.frame(GA_csv$id[5], (mean(athens1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(athens1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(athens1_obs_yearDOC$tmin, na.rm=TRUE))) 
bowman_DOC <- data.frame(GA_csv$id[6], (mean(bowman_obs_yearDOC$prcp, na.rm=TRUE)), (mean(bowman_obs_yearDOC$tmax, na.rm=TRUE)), (mean(bowman_obs_yearDOC$tmin, na.rm=TRUE))) 
athens2_DOC <- data.frame(GA_csv$id[7], (mean(athens2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(athens2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(athens2_obs_yearDOC$tmin, na.rm=TRUE))) 
gray_DOC <- data.frame(GA_csv$id[8], (mean(gray_obs_yearDOC$prcp, na.rm=TRUE)), (mean(gray_obs_yearDOC$tmax, na.rm=TRUE)), (mean(gray_obs_yearDOC$tmin, na.rm=TRUE))) 
dalton_DOC <- data.frame(GA_csv$id[9], (mean(dalton_obs_yearDOC$prcp, na.rm=TRUE)), (mean(dalton_obs_yearDOC$tmax, na.rm=TRUE)), (mean(dalton_obs_yearDOC$tmin, na.rm=TRUE))) 
darien2_DOC <- data.frame(GA_csv$id[10], (mean(darien2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(darien2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(darien2_obs_yearDOC$tmin, na.rm=TRUE))) 
darien3_DOC <- data.frame(GA_csv$id[11], (mean(darien3_obs_yearDOC$prcp, na.rm=TRUE)), (mean(darien3_obs_yearDOC$tmax, na.rm=TRUE)), (mean(darien3_obs_yearDOC$tmin, na.rm=TRUE))) 
milledgeville_DOC <- data.frame(GA_csv$id[12], (mean(milledgeville_obs_yearDOC$prcp, na.rm=TRUE)), (mean(milledgeville_obs_yearDOC$tmax, na.rm=TRUE)), (mean(milledgeville_obs_yearDOC$tmin, na.rm=TRUE))) 
athens3_DOC <- data.frame(GA_csv$id[13], (mean(athens3_obs_yearDOC$prcp, na.rm=TRUE)), (mean(athens3_obs_yearDOC$tmax, na.rm=TRUE)), (mean(athens3_obs_yearDOC$tmin, na.rm=TRUE))) 
cochran1_DOC <- data.frame(GA_csv$id[14], (mean(cochran1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(cochran1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(cochran1_obs_yearDOC$tmin, na.rm=TRUE))) 
macon_DOC <- data.frame(GA_csv$id[15], (mean(macon_obs_yearDOC$prcp, na.rm=TRUE)), (mean(macon_obs_yearDOC$tmax, na.rm=TRUE)), (mean(macon_obs_yearDOC$tmin, na.rm=TRUE))) 
cochran2_DOC <- data.frame(GA_csv$id[16], (mean(cochran2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(cochran2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(cochran2_obs_yearDOC$tmin, na.rm=TRUE))) 
cochran3_DOC <- data.frame(GA_csv$id[17], (mean(cochran3_obs_yearDOC$prcp, na.rm=TRUE)), (mean(cochran3_obs_yearDOC$tmax, na.rm=TRUE)), (mean(cochran3_obs_yearDOC$tmin, na.rm=TRUE))) 
quitman1_DOC <- data.frame(GA_csv$id[18], (mean(quitman1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(quitman1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(quitman1_obs_yearDOC$tmin, na.rm=TRUE))) 
quitman2_DOC <- data.frame(GA_csv$id[19], (mean(quitman2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(quitman2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(quitman2_obs_yearDOC$tmin, na.rm=TRUE))) 
quitman3_DOC <- data.frame(GA_csv$id[20], (mean(quitman3_obs_yearDOC$prcp, na.rm=TRUE)), (mean(quitman3_obs_yearDOC$tmax, na.rm=TRUE)), (mean(quitman3_obs_yearDOC$tmin, na.rm=TRUE))) 
statesboro2_DOC <- data.frame(GA_csv$id[21], (mean(statesboro2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(statesboro2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(statesboro2_obs_yearDOC$tmin, na.rm=TRUE))) 
colquit_DOC <- data.frame(GA_csv$id[22], (mean(colquit_obs_yearDOC$prcp, na.rm=TRUE)), (mean(colquit_obs_yearDOC$tmax, na.rm=TRUE)), (mean(colquit_obs_yearDOC$tmin, na.rm=TRUE))) 
athens4_DOC <- data.frame(GA_csv$id[23], (mean(athens4_obs_yearDOC$prcp, na.rm=TRUE)), (mean(athens4_obs_yearDOC$tmax, na.rm=TRUE)), (mean(athens4_obs_yearDOC$tmin, na.rm=TRUE))) 
rome1_DOC <- data.frame(GA_csv$id[24], (mean(rome1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(rome1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(rome1_obs_yearDOC$tmin, na.rm=TRUE))) 
rome2_DOC <- data.frame(GA_csv$id[25], (mean(rome2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(rome2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(rome2_obs_yearDOC$tmin, na.rm=TRUE)))

#rename col.names 
cat(citySwap3(city2, y = "colnames(", z = "_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')"), sep = "\n")

colnames(madison_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(darien1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(statesboro1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(newton_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(athens1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(bowman_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(athens2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(gray_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(dalton_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(darien2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(darien3_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(milledgeville_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(athens3_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(cochran1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(macon_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(cochran2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(cochran3_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(quitman1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(quitman2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(quitman3_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(statesboro2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(colquit_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(athens4_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(rome1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(rome2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')

## combine current year of the date of collection data using bind_rows
cat(citySwap7(city2, y = "_DOC", z = ","), sep = "")
GA_yearDOC <- bind_rows(madison_DOC,darien1_DOC,statesboro1_DOC,newton_DOC,athens1_DOC,bowman_DOC,athens2_DOC,gray_DOC,dalton_DOC,darien2_DOC,darien3_DOC,milledgeville_DOC,athens3_DOC,cochran1_DOC,macon_DOC,cochran2_DOC,cochran3_DOC,quitman1_DOC,quitman2_DOC,quitman3_DOC,statesboro2_DOC,colquit_DOC,athens4_DOC,rome1_DOC,rome2_DOC)

# save data
write.xlsx(GA_yearDOC, file = "GA_year_DOC_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
