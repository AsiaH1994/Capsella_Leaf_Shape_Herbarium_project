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
PNF_csv <- read.csv('PNF_cities.csv')
PNF_csv$month <- match(PNF_csv$month, month.name)
PNF_csv[c(3,11,13,15,23,24),7] <- "4"
PNF_csv$date <- paste(PNF_csv$year, PNF_csv$month, PNF_csv$day, sep="-")
strptime(PNF_csv$date,format="%Y-%m-%d")
PNF_csv$date <- as.Date(PNF_csv$date)
colnames(PNF_csv) <- c("id", 'id2', 'city_county_state', 'latitude', 'longitude', 'elevation', 'month', 'day', 'year', 'date')
PNF_csv$previous6mon <- as.Date(as.yearmon(as.Date(PNF_csv$date)) -.5, frac = 1)
head(PNF_csv)

##remove ashford - having issues for some reason
PNF_csv <- PNF_csv[-(6),]

## to use lapply 
city <- PNF_csv$id
city2 <- PNF_csv$id2
date <- PNF_csv$date
pyear <- (PNF_csv$date - 365)
p6mon <- PNF_csv$previous6mon
year <- PNF_csv$year
lennumber <- (1:28) #number of samples

## important data frames
df1 <-  list(city, city2, date)

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

PNF_stations <- stations %>% filter(state == c("OR", "WA", "MT", "NE", "ID"))
write.csv2(PNF_stations, "PNF_stations.csv")

#after running the 1st time 
#region_stations <- read.csv("great_lakes_stations.csv")


#find closests stations
PNF_clst <- meteo_nearby_stations(lat_lon_df = PNF_csv, station_data = PNF_stations, radius = 50, limit = 200)

### separate all cities 
cat(citySwap(unique(city), y = " <- (PNF_clst$'", z = "')"), sep = "\n")

glasgow <- (PNF_clst$'glasgow')
wallawalla <- (PNF_clst$'wallawalla')
caldwell <- (PNF_clst$'caldwell')
hastings <- (PNF_clst$'hastings')
corvallis <- (PNF_clst$'corvallis')
pullman <- (PNF_clst$'pullman')
boise <- (PNF_clst$'boise')
anacortes <- (PNF_clst$'anacortes')
pocatello <- (PNF_clst$'pocatello')
cheney <- (PNF_clst$'cheney')
chadron <- (PNF_clst$'chadron')
mullen <- (PNF_clst$'mullen')
gering <- (PNF_clst$'gering')
crawford <- (PNF_clst$'crawford')
littlebluetownship <- (PNF_clst$'littlebluetownship')
browkenbow <- (PNF_clst$'browkenbow')
jerome <- (PNF_clst$'jerome')
neligh <- (PNF_clst$'neligh')
bannercounty <- (PNF_clst$'bannercounty')
almotacreek <- (PNF_clst$'almotacreek')
longpine <- (PNF_clst$'longpine')
albin <- (PNF_clst$'albin')
alliance <- (PNF_clst$'alliance')

### separate all station id's (monitors) for each city
cat(citySwap(unique(city), y = "_monitors <- ", z = "$id"), sep = "\n")

glasgow_monitors <- glasgow$id
wallawalla_monitors <- wallawalla$id
caldwell_monitors <- caldwell$id
hastings_monitors <- hastings$id
corvallis_monitors <- corvallis$id
pullman_monitors <- pullman$id
boise_monitors <- boise$id
anacortes_monitors <- anacortes$id
pocatello_monitors <- pocatello$id
cheney_monitors <- cheney$id
chadron_monitors <- chadron$id
mullen_monitors <- mullen$id
gering_monitors <- gering$id
crawford_monitors <- crawford$id
littlebluetownship_monitors <- littlebluetownship$id
browkenbow_monitors <- browkenbow$id
jerome_monitors <- jerome$id
neligh_monitors <- neligh$id
bannercounty_monitors <- bannercounty$id
almotacreek_monitors <- almotacreek$id
longpine_monitors <- longpine$id
albin_monitors <- albin$id
alliance_monitors <- alliance$id

### pull all observations for cities 
cat(citySwap(unique(city), y = "_obs <- meteo_pull_monitors(", z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')"), sep = "\n")

glasgow_obs <- meteo_pull_monitors(glasgow_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
wallawalla_obs <- meteo_pull_monitors(wallawalla_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
caldwell_obs <- meteo_pull_monitors(caldwell_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
hastings_obs <- meteo_pull_monitors(hastings_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
corvallis_obs <- meteo_pull_monitors(corvallis_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
pullman_obs <- meteo_pull_monitors(pullman_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
boise_obs <- meteo_pull_monitors(boise_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
anacortes_obs <- meteo_pull_monitors(anacortes_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
pocatello_obs <- meteo_pull_monitors(pocatello_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
cheney_obs <- meteo_pull_monitors(cheney_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
chadron_obs <- meteo_pull_monitors(chadron_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
mullen_obs <- meteo_pull_monitors(mullen_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
gering_obs <- meteo_pull_monitors(gering_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
crawford_obs <- meteo_pull_monitors(crawford_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
littlebluetownship_obs <- meteo_pull_monitors(littlebluetownship_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
browkenbow_obs <- meteo_pull_monitors(browkenbow_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
jerome_obs <- meteo_pull_monitors(jerome_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
neligh_obs <- meteo_pull_monitors(neligh_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
bannercounty_obs <- meteo_pull_monitors(bannercounty_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
almotacreek_obs <- meteo_pull_monitors(almotacreek_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
longpine_obs <- meteo_pull_monitors(longpine_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
albin_obs <- meteo_pull_monitors(albin_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
alliance_obs <- meteo_pull_monitors(alliance_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')

### Part 1
### pull data by date and location - dates are coming from state_csv 
lapply(df1, cat(citySwap4.5(x = city2, y = "_obs_temps <- ", i = city, z = "_obs %>% filter(date == '", j = date , h = "')"), sep = "\n"))

glasgow_obs_temps <- glasgow_obs %>% filter(date == '2011-05-10')
wallawalla_obs_temps <- wallawalla_obs %>% filter(date == '1963-03-21')
caldwell_obs_temps <- caldwell_obs %>% filter(date == '1959-04-02')
hastings_obs_temps <- hastings_obs %>% filter(date == '1978-06-03')
corvallis_obs_temps <- corvallis_obs %>% filter(date == '1943-03-19')
pullman_obs_temps <- pullman_obs %>% filter(date == '1965-03-12')
boise_obs_temps <- boise_obs %>% filter(date == '1911-05-06')
anacortes_obs_temps <- anacortes_obs %>% filter(date == '1965-06-11')
pocatello_obs_temps <- pocatello_obs %>% filter(date == '1952-09-25')
cheney_obs_temps <- cheney_obs %>% filter(date == '1965-04-15')
chadron1_obs_temps <- chadron_obs %>% filter(date == '1983-03-14')
chadron2_obs_temps <- chadron_obs %>% filter(date == '1977-04-16')
chadron3_obs_temps <- chadron_obs %>% filter(date == '2007-05-16')
mullen1_obs_temps <- mullen_obs %>% filter(date == '1985-04-10')
mullen2_obs_temps <- mullen_obs %>% filter(date == '1986-05-29')
gering_obs_temps <- gering_obs %>% filter(date == '1976-05-17')
crawford_obs_temps <- crawford_obs %>% filter(date == '1978-05-15')
littlebluetownship_obs_temps <- littlebluetownship_obs %>% filter(date == '1990-05-21')
browkenbow_obs_temps <- browkenbow_obs %>% filter(date == '1927-05-10')
jerome_obs_temps <- jerome_obs %>% filter(date == '1995-05-08')
neligh_obs_temps <- neligh_obs %>% filter(date == '1896-05-02')
bannercounty_obs_temps <- bannercounty_obs %>% filter(date == '1985-04-28')
almotacreek_obs_temps <- almotacreek_obs %>% filter(date == '1975-04-20')
longpine_obs_temps <- longpine_obs %>% filter(date == '1987-05-02')
albin_obs_temps <- albin_obs %>% filter(date == '1977-06-03')
alliance_obs_temps <- alliance_obs %>% filter(date == '1975-06-06')
chadron4_obs_temps <- chadron_obs %>% filter(date == '1976-06-22')
chadron5_obs_temps <- chadron_obs %>% filter(date == '1970-06-09')

#combine into one data frame using bind_rows()
cat(citySwap7(city2, y = "_obs_temps", z = ","), sep = "")
PNW_obs <- bind_rows(glasgow_obs_temps,wallawalla_obs_temps,caldwell_obs_temps,hastings_obs_temps,corvallis_obs_temps,pullman_obs_temps,boise_obs_temps,anacortes_obs_temps,pocatello_obs_temps,cheney_obs_temps,chadron1_obs_temps,chadron2_obs_temps,chadron3_obs_temps,mullen1_obs_temps,mullen2_obs_temps,gering_obs_temps,crawford_obs_temps,littlebluetownship_obs_temps,browkenbow_obs_temps,jerome_obs_temps,neligh_obs_temps,bannercounty_obs_temps,almotacreek_obs_temps,longpine_obs_temps,albin_obs_temps,alliance_obs_temps,chadron4_obs_temps,chadron5_obs_temps)

#Merge the original state_csv dataframe and new state_obs dataframe 
PNW_data <- merge.data.frame(PNF_csv, PNW_obs, by = 'date')

## find the tavg for state data 
PNW_data$tavg <- ((PNW_data$tmax + PNW_data$tmin) / 2)

### save data as xlsx file 
library("xlsx")
write.xlsx(PNW_data, file = "PNW_data_total_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

### Part 2
#finding data from 6 months prior to date of collection
#ambient temps and precipitation may have affected plant/leaf growth and shape
lapply(df2, cat(citySwap5(x = city2, y = "_obs_6months <- meteo_pull_monitors(", j = city, 
                          z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '", h = p6mon, 
                          k = "', date_max = '", l = date, p ="')"), sep = "\n"))

glasgow_obs_6months <- meteo_pull_monitors(glasgow_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2010-11-30', date_max = '2011-05-10')
wallawalla_obs_6months <- meteo_pull_monitors(wallawalla_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1962-09-30', date_max = '1963-03-21')
caldwell_obs_6months <- meteo_pull_monitors(caldwell_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1958-10-31', date_max = '1959-04-02')
hastings_obs_6months <- meteo_pull_monitors(hastings_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1977-12-31', date_max = '1978-06-03')
corvallis_obs_6months <- meteo_pull_monitors(corvallis_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1942-09-30', date_max = '1943-03-19')
pullman_obs_6months <- meteo_pull_monitors(pullman_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1964-09-30', date_max = '1965-03-12')
boise_obs_6months <- meteo_pull_monitors(boise_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1910-11-30', date_max = '1911-05-06')
anacortes_obs_6months <- meteo_pull_monitors(anacortes_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1964-12-31', date_max = '1965-06-11')
pocatello_obs_6months <- meteo_pull_monitors(pocatello_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1952-03-31', date_max = '1952-09-25')
cheney_obs_6months <- meteo_pull_monitors(cheney_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1964-10-31', date_max = '1965-04-15')
chadron1_obs_6months <- meteo_pull_monitors(chadron_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1982-09-30', date_max = '1983-03-14')
chadron2_obs_6months <- meteo_pull_monitors(chadron_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1976-10-31', date_max = '1977-04-16')
chadron3_obs_6months <- meteo_pull_monitors(chadron_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2006-11-30', date_max = '2007-05-16')
mullen1_obs_6months <- meteo_pull_monitors(mullen_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1984-10-31', date_max = '1985-04-10')
mullen2_obs_6months <- meteo_pull_monitors(mullen_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1985-11-30', date_max = '1986-05-29')
gering_obs_6months <- meteo_pull_monitors(gering_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1975-11-30', date_max = '1976-05-17')
crawford_obs_6months <- meteo_pull_monitors(crawford_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1977-11-30', date_max = '1978-05-15')
littlebluetownship_obs_6months <- meteo_pull_monitors(littlebluetownship_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1989-11-30', date_max = '1990-05-21')
browkenbow_obs_6months <- meteo_pull_monitors(browkenbow_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1926-11-30', date_max = '1927-05-10')
jerome_obs_6months <- meteo_pull_monitors(jerome_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1994-11-30', date_max = '1995-05-08')
neligh_obs_6months <- meteo_pull_monitors(neligh_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1895-11-30', date_max = '1896-05-02')
bannercounty_obs_6months <- meteo_pull_monitors(bannercounty_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1984-10-31', date_max = '1985-04-28')
almotacreek_obs_6months <- meteo_pull_monitors(almotacreek_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1974-10-31', date_max = '1975-04-20')
longpine_obs_6months <- meteo_pull_monitors(longpine_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1986-11-30', date_max = '1987-05-02')
albin_obs_6months <- meteo_pull_monitors(albin_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1976-12-31', date_max = '1977-06-03')
alliance_obs_6months <- meteo_pull_monitors(alliance_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1974-12-31', date_max = '1975-06-06')
chadron4_obs_6months <- meteo_pull_monitors(chadron_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1975-12-31', date_max = '1976-06-22')
chadron5_obs_6months <- meteo_pull_monitors(chadron_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1969-12-31', date_max = '1970-06-09')

## make a dataframe of the mean of tmax, tmin, prcp of the previous 6 months 
lapply(df3, cat(citySwap2(x = city2,  y = "_6months", z = " <- data.frame(PNF_csv$id[", l= lennumber, k = "], (mean(", e = "_obs_6months$prcp, na.rm=TRUE)), (mean(", f = "_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(", g = "_obs_6months$tmin, na.rm=TRUE))) "), sep = "\n"))
glasgow_6months <- data.frame(PNF_csv$id[1], (mean(glasgow_obs_6months$prcp, na.rm=TRUE)), (mean(glasgow_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(glasgow_obs_6months$tmin, na.rm=TRUE))) 
wallawalla_6months <- data.frame(PNF_csv$id[2], (mean(wallawalla_obs_6months$prcp, na.rm=TRUE)), (mean(wallawalla_obs_6months$tmax, na.rm=TRUE)), 
                                 (mean(wallawalla_obs_6months$tmin, na.rm=TRUE))) 
caldwell_6months <- data.frame(PNF_csv$id[3], (mean(caldwell_obs_6months$prcp, na.rm=TRUE)), (mean(caldwell_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(caldwell_obs_6months$tmin, na.rm=TRUE))) 
hastings_6months <- data.frame(PNF_csv$id[4], (mean(hastings_obs_6months$prcp, na.rm=TRUE)), (mean(hastings_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(hastings_obs_6months$tmin, na.rm=TRUE))) 
corvallis_6months <- data.frame(PNF_csv$id[5], (mean(corvallis_obs_6months$prcp, na.rm=TRUE)), (mean(corvallis_obs_6months$tmax, na.rm=TRUE)), 
                                (mean(corvallis_obs_6months$tmin, na.rm=TRUE))) 
pullman_6months <- data.frame(PNF_csv$id[6], (mean(pullman_obs_6months$prcp, na.rm=TRUE)), (mean(pullman_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(pullman_obs_6months$tmin, na.rm=TRUE))) 
boise_6months <- data.frame(PNF_csv$id[7], (mean(boise_obs_6months$prcp, na.rm=TRUE)), (mean(boise_obs_6months$tmax, na.rm=TRUE)), 
                            (mean(boise_obs_6months$tmin, na.rm=TRUE))) 
anacortes_6months <- data.frame(PNF_csv$id[8], (mean(anacortes_obs_6months$prcp, na.rm=TRUE)), (mean(anacortes_obs_6months$tmax, na.rm=TRUE)), 
                                (mean(anacortes_obs_6months$tmin, na.rm=TRUE))) 
pocatello_6months <- data.frame(PNF_csv$id[9], (mean(pocatello_obs_6months$prcp, na.rm=TRUE)), (mean(pocatello_obs_6months$tmax, na.rm=TRUE)), 
                                (mean(pocatello_obs_6months$tmin, na.rm=TRUE))) 
cheney_6months <- data.frame(PNF_csv$id[10], (mean(cheney_obs_6months$prcp, na.rm=TRUE)), (mean(cheney_obs_6months$tmax, na.rm=TRUE)), 
                             (mean(cheney_obs_6months$tmin, na.rm=TRUE))) 
chadron1_6months <- data.frame(PNF_csv$id[11], (mean(chadron1_obs_6months$prcp, na.rm=TRUE)), (mean(chadron1_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(chadron1_obs_6months$tmin, na.rm=TRUE))) 
chadron2_6months <- data.frame(PNF_csv$id[12], (mean(chadron2_obs_6months$prcp, na.rm=TRUE)), (mean(chadron2_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(chadron2_obs_6months$tmin, na.rm=TRUE))) 
chadron3_6months <- data.frame(PNF_csv$id[13], (mean(chadron3_obs_6months$prcp, na.rm=TRUE)), (mean(chadron3_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(chadron3_obs_6months$tmin, na.rm=TRUE))) 
mullen1_6months <- data.frame(PNF_csv$id[14], (mean(mullen1_obs_6months$prcp, na.rm=TRUE)), (mean(mullen1_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(mullen1_obs_6months$tmin, na.rm=TRUE))) 
mullen2_6months <- data.frame(PNF_csv$id[15], (mean(mullen2_obs_6months$prcp, na.rm=TRUE)), (mean(mullen2_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(mullen2_obs_6months$tmin, na.rm=TRUE))) 
gering_6months <- data.frame(PNF_csv$id[16], (mean(gering_obs_6months$prcp, na.rm=TRUE)), (mean(gering_obs_6months$tmax, na.rm=TRUE)), 
                             (mean(gering_obs_6months$tmin, na.rm=TRUE))) 
crawford_6months <- data.frame(PNF_csv$id[17], (mean(crawford_obs_6months$prcp, na.rm=TRUE)), (mean(crawford_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(crawford_obs_6months$tmin, na.rm=TRUE))) 
littlebluetownship_6months <- data.frame(PNF_csv$id[18], (mean(littlebluetownship_obs_6months$prcp, na.rm=TRUE)), (mean(littlebluetownship_obs_6months$tmax, na.rm=TRUE)), 
                                         (mean(littlebluetownship_obs_6months$tmin, na.rm=TRUE))) 
browkenbow_6months <- data.frame(PNF_csv$id[19], (mean(browkenbow_obs_6months$prcp, na.rm=TRUE)), (mean(browkenbow_obs_6months$tmax, na.rm=TRUE)), 
                                 (mean(browkenbow_obs_6months$tmin, na.rm=TRUE))) 
jerome_6months <- data.frame(PNF_csv$id[20], (mean(jerome_obs_6months$prcp, na.rm=TRUE)), (mean(jerome_obs_6months$tmax, na.rm=TRUE)), 
                             (mean(jerome_obs_6months$tmin, na.rm=TRUE))) 
neligh_6months <- data.frame(PNF_csv$id[21], (mean(neligh_obs_6months$prcp, na.rm=TRUE)), (mean(neligh_obs_6months$tmax, na.rm=TRUE)), 
                             (mean(neligh_obs_6months$tmin, na.rm=TRUE))) 
bannercounty_6months <- data.frame(PNF_csv$id[22], (mean(bannercounty_obs_6months$prcp, na.rm=TRUE)), (mean(bannercounty_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(bannercounty_obs_6months$tmin, na.rm=TRUE))) 
almotacreek_6months <- data.frame(PNF_csv$id[23], (mean(almotacreek_obs_6months$prcp, na.rm=TRUE)), (mean(almotacreek_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(almotacreek_obs_6months$tmin, na.rm=TRUE))) 
longpine_6months <- data.frame(PNF_csv$id[24], (mean(longpine_obs_6months$prcp, na.rm=TRUE)), (mean(longpine_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(longpine_obs_6months$tmin, na.rm=TRUE))) 
albin_6months <- data.frame(PNF_csv$id[25], (mean(albin_obs_6months$prcp, na.rm=TRUE)), (mean(albin_obs_6months$tmax, na.rm=TRUE)), 
                            (mean(albin_obs_6months$tmin, na.rm=TRUE))) 
alliance_6months <- data.frame(PNF_csv$id[26], (mean(alliance_obs_6months$prcp, na.rm=TRUE)), (mean(alliance_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(alliance_obs_6months$tmin, na.rm=TRUE))) 
chadron4_6months <- data.frame(PNF_csv$id[27], (mean(chadron4_obs_6months$prcp, na.rm=TRUE)), (mean(chadron4_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(chadron4_obs_6months$tmin, na.rm=TRUE))) 
chadron5_6months <- data.frame(PNF_csv$id[28], (mean(chadron5_obs_6months$prcp, na.rm=TRUE)), (mean(chadron5_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(chadron5_obs_6months$tmin, na.rm=TRUE)))


#rename col.names 
cat(citySwap3(city2, y = "colnames(", z = "_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')"), sep = "\n")

colnames(glasgow_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(wallawalla_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(caldwell_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(hastings_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(corvallis_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(pullman_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(boise_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(anacortes_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(pocatello_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(cheney_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(chadron1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(chadron2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(chadron3_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(mullen1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(mullen2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(gering_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(crawford_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(littlebluetownship_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(browkenbow_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(jerome_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(neligh_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(bannercounty_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(almotacreek_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(longpine_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(albin_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(alliance_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(chadron4_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(chadron5_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')

## combine previous 6 months data using bind_rows
cat(citySwap7(city2, y = "_6months", z = ","), sep = "")
PNW_6months <- bind_rows(glasgow_6months,wallawalla_6months,caldwell_6months,hastings_6months,corvallis_6months,pullman_6months,boise_6months,anacortes_6months,pocatello_6months,cheney_6months,chadron1_6months,chadron2_6months,chadron3_6months,mullen1_6months,mullen2_6months,gering_6months,crawford_6months,littlebluetownship_6months,browkenbow_6months,jerome_6months,neligh_6months,bannercounty_6months,almotacreek_6months,longpine_6months,albin_6months,alliance_6months,chadron4_6months,chadron5_6months)

### save data as xlsx file 
write.xlsx(PNW_6months, file = "PNW_6months_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


### Part 3
#find data from previous year from date of collection 
lapply(df4, cat(citySwap5(x = city2, y = "_obs_year <- meteo_pull_monitors(", j = city, 
                          z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '", h= pyear, 
                          k = "', date_max = '", l = date, p ="')"), sep = "\n"))

glasgow_obs_year <- meteo_pull_monitors(glasgow_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2010-05-10', date_max = '2011-05-10')
wallawalla_obs_year <- meteo_pull_monitors(wallawalla_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1962-03-21', date_max = '1963-03-21')
caldwell_obs_year <- meteo_pull_monitors(caldwell_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1958-04-02', date_max = '1959-04-02')
hastings_obs_year <- meteo_pull_monitors(hastings_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1977-06-03', date_max = '1978-06-03')
corvallis_obs_year <- meteo_pull_monitors(corvallis_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1942-03-19', date_max = '1943-03-19')
pullman_obs_year <- meteo_pull_monitors(pullman_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1964-03-12', date_max = '1965-03-12')
boise_obs_year <- meteo_pull_monitors(boise_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1910-05-06', date_max = '1911-05-06')
anacortes_obs_year <- meteo_pull_monitors(anacortes_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1964-06-11', date_max = '1965-06-11')
pocatello_obs_year <- meteo_pull_monitors(pocatello_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1951-09-26', date_max = '1952-09-25')
cheney_obs_year <- meteo_pull_monitors(cheney_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1964-04-15', date_max = '1965-04-15')
chadron1_obs_year <- meteo_pull_monitors(chadron_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1982-03-14', date_max = '1983-03-14')
chadron2_obs_year <- meteo_pull_monitors(chadron_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1976-04-16', date_max = '1977-04-16')
chadron3_obs_year <- meteo_pull_monitors(chadron_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2006-05-16', date_max = '2007-05-16')
mullen1_obs_year <- meteo_pull_monitors(mullen_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1984-04-10', date_max = '1985-04-10')
mullen2_obs_year <- meteo_pull_monitors(mullen_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1985-05-29', date_max = '1986-05-29')
gering_obs_year <- meteo_pull_monitors(gering_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1975-05-18', date_max = '1976-05-17')
crawford_obs_year <- meteo_pull_monitors(crawford_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1977-05-15', date_max = '1978-05-15')
littlebluetownship_obs_year <- meteo_pull_monitors(littlebluetownship_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1989-05-21', date_max = '1990-05-21')
browkenbow_obs_year <- meteo_pull_monitors(browkenbow_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1926-05-10', date_max = '1927-05-10')
jerome_obs_year <- meteo_pull_monitors(jerome_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1994-05-08', date_max = '1995-05-08')
neligh_obs_year <- meteo_pull_monitors(neligh_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1895-05-03', date_max = '1896-05-02')
bannercounty_obs_year <- meteo_pull_monitors(bannercounty_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1984-04-28', date_max = '1985-04-28')
almotacreek_obs_year <- meteo_pull_monitors(almotacreek_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1974-04-20', date_max = '1975-04-20')
longpine_obs_year <- meteo_pull_monitors(longpine_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1986-05-02', date_max = '1987-05-02')
albin_obs_year <- meteo_pull_monitors(albin_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1976-06-03', date_max = '1977-06-03')
alliance_obs_year <- meteo_pull_monitors(alliance_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1974-06-06', date_max = '1975-06-06')
chadron4_obs_year <- meteo_pull_monitors(chadron_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1975-06-23', date_max = '1976-06-22')
chadron5_obs_year <- meteo_pull_monitors(chadron_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1969-06-09', date_max = '1970-06-09')

## make a data frame of the mean of tmax, tmin, prcp of the previous year before the date of collection 
lapply(df3, cat(citySwap2(x = city2,  y = "_previousyear", z = " <- data.frame(PNF_csv$id[", l= lennumber, 
                          k = "], (mean(", e = "_obs_year$prcp, na.rm=TRUE)), (mean(", 
                          f = "_obs_year$tmax, na.rm=TRUE)), (mean(", g = "_obs_year$tmin, na.rm=TRUE))) "), sep = "\n"))

glasgow_previousyear <- data.frame(PNF_csv$id[1], (mean(glasgow_obs_year$prcp, na.rm=TRUE)), (mean(glasgow_obs_year$tmax, na.rm=TRUE)), (mean(glasgow_obs_year$tmin, na.rm=TRUE))) 
wallawalla_previousyear <- data.frame(PNF_csv$id[2], (mean(wallawalla_obs_year$prcp, na.rm=TRUE)), (mean(wallawalla_obs_year$tmax, na.rm=TRUE)), (mean(wallawalla_obs_year$tmin, na.rm=TRUE))) 
caldwell_previousyear <- data.frame(PNF_csv$id[3], (mean(caldwell_obs_year$prcp, na.rm=TRUE)), (mean(caldwell_obs_year$tmax, na.rm=TRUE)), (mean(caldwell_obs_year$tmin, na.rm=TRUE))) 
hastings_previousyear <- data.frame(PNF_csv$id[4], (mean(hastings_obs_year$prcp, na.rm=TRUE)), (mean(hastings_obs_year$tmax, na.rm=TRUE)), (mean(hastings_obs_year$tmin, na.rm=TRUE))) 
corvallis_previousyear <- data.frame(PNF_csv$id[5], (mean(corvallis_obs_year$prcp, na.rm=TRUE)), (mean(corvallis_obs_year$tmax, na.rm=TRUE)), (mean(corvallis_obs_year$tmin, na.rm=TRUE))) 
pullman_previousyear <- data.frame(PNF_csv$id[6], (mean(pullman_obs_year$prcp, na.rm=TRUE)), (mean(pullman_obs_year$tmax, na.rm=TRUE)), (mean(pullman_obs_year$tmin, na.rm=TRUE))) 
boise_previousyear <- data.frame(PNF_csv$id[7], (mean(boise_obs_year$prcp, na.rm=TRUE)), (mean(boise_obs_year$tmax, na.rm=TRUE)), (mean(boise_obs_year$tmin, na.rm=TRUE))) 
anacortes_previousyear <- data.frame(PNF_csv$id[8], (mean(anacortes_obs_year$prcp, na.rm=TRUE)), (mean(anacortes_obs_year$tmax, na.rm=TRUE)), (mean(anacortes_obs_year$tmin, na.rm=TRUE))) 
pocatello_previousyear <- data.frame(PNF_csv$id[9], (mean(pocatello_obs_year$prcp, na.rm=TRUE)), (mean(pocatello_obs_year$tmax, na.rm=TRUE)), (mean(pocatello_obs_year$tmin, na.rm=TRUE))) 
cheney_previousyear <- data.frame(PNF_csv$id[10], (mean(cheney_obs_year$prcp, na.rm=TRUE)), (mean(cheney_obs_year$tmax, na.rm=TRUE)), (mean(cheney_obs_year$tmin, na.rm=TRUE))) 
chadron1_previousyear <- data.frame(PNF_csv$id[11], (mean(chadron1_obs_year$prcp, na.rm=TRUE)), (mean(chadron1_obs_year$tmax, na.rm=TRUE)), (mean(chadron1_obs_year$tmin, na.rm=TRUE))) 
chadron2_previousyear <- data.frame(PNF_csv$id[12], (mean(chadron2_obs_year$prcp, na.rm=TRUE)), (mean(chadron2_obs_year$tmax, na.rm=TRUE)), (mean(chadron2_obs_year$tmin, na.rm=TRUE))) 
chadron3_previousyear <- data.frame(PNF_csv$id[13], (mean(chadron3_obs_year$prcp, na.rm=TRUE)), (mean(chadron3_obs_year$tmax, na.rm=TRUE)), (mean(chadron3_obs_year$tmin, na.rm=TRUE))) 
mullen1_previousyear <- data.frame(PNF_csv$id[14], (mean(mullen1_obs_year$prcp, na.rm=TRUE)), (mean(mullen1_obs_year$tmax, na.rm=TRUE)), (mean(mullen1_obs_year$tmin, na.rm=TRUE))) 
mullen2_previousyear <- data.frame(PNF_csv$id[15], (mean(mullen2_obs_year$prcp, na.rm=TRUE)), (mean(mullen2_obs_year$tmax, na.rm=TRUE)), (mean(mullen2_obs_year$tmin, na.rm=TRUE))) 
gering_previousyear <- data.frame(PNF_csv$id[16], (mean(gering_obs_year$prcp, na.rm=TRUE)), (mean(gering_obs_year$tmax, na.rm=TRUE)), (mean(gering_obs_year$tmin, na.rm=TRUE))) 
crawford_previousyear <- data.frame(PNF_csv$id[17], (mean(crawford_obs_year$prcp, na.rm=TRUE)), (mean(crawford_obs_year$tmax, na.rm=TRUE)), (mean(crawford_obs_year$tmin, na.rm=TRUE))) 
littlebluetownship_previousyear <- data.frame(PNF_csv$id[18], (mean(littlebluetownship_obs_year$prcp, na.rm=TRUE)), (mean(littlebluetownship_obs_year$tmax, na.rm=TRUE)), (mean(littlebluetownship_obs_year$tmin, na.rm=TRUE))) 
browkenbow_previousyear <- data.frame(PNF_csv$id[19], (mean(browkenbow_obs_year$prcp, na.rm=TRUE)), (mean(browkenbow_obs_year$tmax, na.rm=TRUE)), (mean(browkenbow_obs_year$tmin, na.rm=TRUE))) 
jerome_previousyear <- data.frame(PNF_csv$id[20], (mean(jerome_obs_year$prcp, na.rm=TRUE)), (mean(jerome_obs_year$tmax, na.rm=TRUE)), (mean(jerome_obs_year$tmin, na.rm=TRUE))) 
neligh_previousyear <- data.frame(PNF_csv$id[21], (mean(neligh_obs_year$prcp, na.rm=TRUE)), (mean(neligh_obs_year$tmax, na.rm=TRUE)), (mean(neligh_obs_year$tmin, na.rm=TRUE))) 
bannercounty_previousyear <- data.frame(PNF_csv$id[22], (mean(bannercounty_obs_year$prcp, na.rm=TRUE)), (mean(bannercounty_obs_year$tmax, na.rm=TRUE)), (mean(bannercounty_obs_year$tmin, na.rm=TRUE))) 
almotacreek_previousyear <- data.frame(PNF_csv$id[23], (mean(almotacreek_obs_year$prcp, na.rm=TRUE)), (mean(almotacreek_obs_year$tmax, na.rm=TRUE)), (mean(almotacreek_obs_year$tmin, na.rm=TRUE))) 
longpine_previousyear <- data.frame(PNF_csv$id[24], (mean(longpine_obs_year$prcp, na.rm=TRUE)), (mean(longpine_obs_year$tmax, na.rm=TRUE)), (mean(longpine_obs_year$tmin, na.rm=TRUE))) 
albin_previousyear <- data.frame(PNF_csv$id[25], (mean(albin_obs_year$prcp, na.rm=TRUE)), (mean(albin_obs_year$tmax, na.rm=TRUE)), (mean(albin_obs_year$tmin, na.rm=TRUE))) 
alliance_previousyear <- data.frame(PNF_csv$id[26], (mean(alliance_obs_year$prcp, na.rm=TRUE)), (mean(alliance_obs_year$tmax, na.rm=TRUE)), (mean(alliance_obs_year$tmin, na.rm=TRUE))) 
chadron4_previousyear <- data.frame(PNF_csv$id[27], (mean(chadron4_obs_year$prcp, na.rm=TRUE)), (mean(chadron4_obs_year$tmax, na.rm=TRUE)), (mean(chadron4_obs_year$tmin, na.rm=TRUE))) 
chadron5_previousyear <- data.frame(PNF_csv$id[28], (mean(chadron5_obs_year$prcp, na.rm=TRUE)), (mean(chadron5_obs_year$tmax, na.rm=TRUE)), (mean(chadron5_obs_year$tmin, na.rm=TRUE)))

#rename col.names 
cat(citySwap3(city2, y = "colnames(", z = "_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')"), sep = "\n")

colnames(glasgow_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(wallawalla_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(caldwell_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(hastings_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(corvallis_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(pullman_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(boise_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(anacortes_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(pocatello_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(cheney_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(chadron1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(chadron2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(chadron3_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(mullen1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(mullen2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(gering_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(crawford_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(littlebluetownship_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(browkenbow_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(jerome_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(neligh_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(bannercounty_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(almotacreek_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(longpine_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(albin_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(alliance_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(chadron4_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(chadron5_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')

## combine previous year before the date of collection data using bind_rows
cat(citySwap7(city2, y = "_previousyear", z = ","), sep = "")
PNW_Pyear <- bind_rows(glasgow_previousyear,wallawalla_previousyear,caldwell_previousyear,hastings_previousyear,corvallis_previousyear,pullman_previousyear,boise_previousyear,anacortes_previousyear,pocatello_previousyear,cheney_previousyear,chadron1_previousyear,chadron2_previousyear,chadron3_previousyear,mullen1_previousyear,mullen2_previousyear,gering_previousyear,crawford_previousyear,littlebluetownship_previousyear,browkenbow_previousyear,jerome_previousyear,neligh_previousyear,bannercounty_previousyear,almotacreek_previousyear,longpine_previousyear,albin_previousyear,alliance_previousyear,chadron4_previousyear,chadron5_previousyear)

# save data
write.xlsx(PNW_Pyear, file = "PNW_previous_year_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

### Part 4 
#Find data from current year of date of collection 
lapply(df5, cat(citySwap6(x = city2, y = "_obs_yearDOC <- meteo_pull_monitors(", j = city, 
                          z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '", l = year , 
                          k = "-01-01', date_max = '",  p ="-12-31')"), sep = "\n"))

glasgow_obs_yearDOC <- meteo_pull_monitors(glasgow_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2011-01-01', date_max = '2011-12-31')
wallawalla_obs_yearDOC <- meteo_pull_monitors(wallawalla_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1963-01-01', date_max = '1963-12-31')
caldwell_obs_yearDOC <- meteo_pull_monitors(caldwell_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1959-01-01', date_max = '1959-12-31')
hastings_obs_yearDOC <- meteo_pull_monitors(hastings_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1978-01-01', date_max = '1978-12-31')
corvallis_obs_yearDOC <- meteo_pull_monitors(corvallis_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1943-01-01', date_max = '1943-12-31')
pullman_obs_yearDOC <- meteo_pull_monitors(pullman_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1965-01-01', date_max = '1965-12-31')
boise_obs_yearDOC <- meteo_pull_monitors(boise_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1911-01-01', date_max = '1911-12-31')
anacortes_obs_yearDOC <- meteo_pull_monitors(anacortes_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1965-01-01', date_max = '1965-12-31')
pocatello_obs_yearDOC <- meteo_pull_monitors(pocatello_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1952-01-01', date_max = '1952-12-31')
cheney_obs_yearDOC <- meteo_pull_monitors(cheney_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1965-01-01', date_max = '1965-12-31')
chadron1_obs_yearDOC <- meteo_pull_monitors(chadron_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1983-01-01', date_max = '1983-12-31')
chadron2_obs_yearDOC <- meteo_pull_monitors(chadron_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1977-01-01', date_max = '1977-12-31')
chadron3_obs_yearDOC <- meteo_pull_monitors(chadron_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2007-01-01', date_max = '2007-12-31')
mullen1_obs_yearDOC <- meteo_pull_monitors(mullen_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1985-01-01', date_max = '1985-12-31')
mullen2_obs_yearDOC <- meteo_pull_monitors(mullen_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1986-01-01', date_max = '1986-12-31')
gering_obs_yearDOC <- meteo_pull_monitors(gering_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1976-01-01', date_max = '1976-12-31')
crawford_obs_yearDOC <- meteo_pull_monitors(crawford_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1978-01-01', date_max = '1978-12-31')
littlebluetownship_obs_yearDOC <- meteo_pull_monitors(littlebluetownship_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1990-01-01', date_max = '1990-12-31')
browkenbow_obs_yearDOC <- meteo_pull_monitors(browkenbow_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1927-01-01', date_max = '1927-12-31')
jerome_obs_yearDOC <- meteo_pull_monitors(jerome_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1995-01-01', date_max = '1995-12-31')
neligh_obs_yearDOC <- meteo_pull_monitors(neligh_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1896-01-01', date_max = '1896-12-31')
bannercounty_obs_yearDOC <- meteo_pull_monitors(bannercounty_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1985-01-01', date_max = '1985-12-31')
almotacreek_obs_yearDOC <- meteo_pull_monitors(almotacreek_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1975-01-01', date_max = '1975-12-31')
longpine_obs_yearDOC <- meteo_pull_monitors(longpine_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1987-01-01', date_max = '1987-12-31')
albin_obs_yearDOC <- meteo_pull_monitors(albin_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1977-01-01', date_max = '1977-12-31')
alliance_obs_yearDOC <- meteo_pull_monitors(alliance_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1975-01-01', date_max = '1975-12-31')
chadron4_obs_yearDOC <- meteo_pull_monitors(chadron_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1976-01-01', date_max = '1976-12-31')
chadron5_obs_yearDOC <- meteo_pull_monitors(chadron_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1970-01-01', date_max = '1970-12-31')

## make a dataframe of the mean of tmax, tmin, current year of the date of collection
lapply(dflist_nodates, cat(citySwap2(x = city2,  y = "_DOC", z = " <- data.frame(PNF_csv$id[", l= lennumber, k = "], (mean(", e = "_obs_yearDOC$prcp, na.rm=TRUE)), (mean(", f = "_obs_yearDOC$tmax, na.rm=TRUE)), (mean(", g = "_obs_yearDOC$tmin, na.rm=TRUE))) "), sep = "\n"))

glasgow_DOC <- data.frame(PNF_csv$id[1], (mean(glasgow_obs_yearDOC$prcp, na.rm=TRUE)), (mean(glasgow_obs_yearDOC$tmax, na.rm=TRUE)), (mean(glasgow_obs_yearDOC$tmin, na.rm=TRUE))) 
wallawalla_DOC <- data.frame(PNF_csv$id[2], (mean(wallawalla_obs_yearDOC$prcp, na.rm=TRUE)), (mean(wallawalla_obs_yearDOC$tmax, na.rm=TRUE)), (mean(wallawalla_obs_yearDOC$tmin, na.rm=TRUE))) 
caldwell_DOC <- data.frame(PNF_csv$id[3], (mean(caldwell_obs_yearDOC$prcp, na.rm=TRUE)), (mean(caldwell_obs_yearDOC$tmax, na.rm=TRUE)), (mean(caldwell_obs_yearDOC$tmin, na.rm=TRUE))) 
hastings_DOC <- data.frame(PNF_csv$id[4], (mean(hastings_obs_yearDOC$prcp, na.rm=TRUE)), (mean(hastings_obs_yearDOC$tmax, na.rm=TRUE)), (mean(hastings_obs_yearDOC$tmin, na.rm=TRUE))) 
corvallis_DOC <- data.frame(PNF_csv$id[5], (mean(corvallis_obs_yearDOC$prcp, na.rm=TRUE)), (mean(corvallis_obs_yearDOC$tmax, na.rm=TRUE)), (mean(corvallis_obs_yearDOC$tmin, na.rm=TRUE))) 
pullman_DOC <- data.frame(PNF_csv$id[6], (mean(pullman_obs_yearDOC$prcp, na.rm=TRUE)), (mean(pullman_obs_yearDOC$tmax, na.rm=TRUE)), (mean(pullman_obs_yearDOC$tmin, na.rm=TRUE))) 
boise_DOC <- data.frame(PNF_csv$id[7], (mean(boise_obs_yearDOC$prcp, na.rm=TRUE)), (mean(boise_obs_yearDOC$tmax, na.rm=TRUE)), (mean(boise_obs_yearDOC$tmin, na.rm=TRUE))) 
anacortes_DOC <- data.frame(PNF_csv$id[8], (mean(anacortes_obs_yearDOC$prcp, na.rm=TRUE)), (mean(anacortes_obs_yearDOC$tmax, na.rm=TRUE)), (mean(anacortes_obs_yearDOC$tmin, na.rm=TRUE))) 
pocatello_DOC <- data.frame(PNF_csv$id[9], (mean(pocatello_obs_yearDOC$prcp, na.rm=TRUE)), (mean(pocatello_obs_yearDOC$tmax, na.rm=TRUE)), (mean(pocatello_obs_yearDOC$tmin, na.rm=TRUE))) 
cheney_DOC <- data.frame(PNF_csv$id[10], (mean(cheney_obs_yearDOC$prcp, na.rm=TRUE)), (mean(cheney_obs_yearDOC$tmax, na.rm=TRUE)), (mean(cheney_obs_yearDOC$tmin, na.rm=TRUE))) 
chadron1_DOC <- data.frame(PNF_csv$id[11], (mean(chadron1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(chadron1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(chadron1_obs_yearDOC$tmin, na.rm=TRUE))) 
chadron2_DOC <- data.frame(PNF_csv$id[12], (mean(chadron2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(chadron2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(chadron2_obs_yearDOC$tmin, na.rm=TRUE))) 
chadron3_DOC <- data.frame(PNF_csv$id[13], (mean(chadron3_obs_yearDOC$prcp, na.rm=TRUE)), (mean(chadron3_obs_yearDOC$tmax, na.rm=TRUE)), (mean(chadron3_obs_yearDOC$tmin, na.rm=TRUE))) 
mullen1_DOC <- data.frame(PNF_csv$id[14], (mean(mullen1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(mullen1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(mullen1_obs_yearDOC$tmin, na.rm=TRUE))) 
mullen2_DOC <- data.frame(PNF_csv$id[15], (mean(mullen2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(mullen2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(mullen2_obs_yearDOC$tmin, na.rm=TRUE))) 
gering_DOC <- data.frame(PNF_csv$id[16], (mean(gering_obs_yearDOC$prcp, na.rm=TRUE)), (mean(gering_obs_yearDOC$tmax, na.rm=TRUE)), (mean(gering_obs_yearDOC$tmin, na.rm=TRUE))) 
crawford_DOC <- data.frame(PNF_csv$id[17], (mean(crawford_obs_yearDOC$prcp, na.rm=TRUE)), (mean(crawford_obs_yearDOC$tmax, na.rm=TRUE)), (mean(crawford_obs_yearDOC$tmin, na.rm=TRUE))) 
littlebluetownship_DOC <- data.frame(PNF_csv$id[18], (mean(littlebluetownship_obs_yearDOC$prcp, na.rm=TRUE)), (mean(littlebluetownship_obs_yearDOC$tmax, na.rm=TRUE)), (mean(littlebluetownship_obs_yearDOC$tmin, na.rm=TRUE))) 
browkenbow_DOC <- data.frame(PNF_csv$id[19], (mean(browkenbow_obs_yearDOC$prcp, na.rm=TRUE)), (mean(browkenbow_obs_yearDOC$tmax, na.rm=TRUE)), (mean(browkenbow_obs_yearDOC$tmin, na.rm=TRUE))) 
jerome_DOC <- data.frame(PNF_csv$id[20], (mean(jerome_obs_yearDOC$prcp, na.rm=TRUE)), (mean(jerome_obs_yearDOC$tmax, na.rm=TRUE)), (mean(jerome_obs_yearDOC$tmin, na.rm=TRUE))) 
neligh_DOC <- data.frame(PNF_csv$id[21], (mean(neligh_obs_yearDOC$prcp, na.rm=TRUE)), (mean(neligh_obs_yearDOC$tmax, na.rm=TRUE)), (mean(neligh_obs_yearDOC$tmin, na.rm=TRUE))) 
bannercounty_DOC <- data.frame(PNF_csv$id[22], (mean(bannercounty_obs_yearDOC$prcp, na.rm=TRUE)), (mean(bannercounty_obs_yearDOC$tmax, na.rm=TRUE)), (mean(bannercounty_obs_yearDOC$tmin, na.rm=TRUE))) 
almotacreek_DOC <- data.frame(PNF_csv$id[23], (mean(almotacreek_obs_yearDOC$prcp, na.rm=TRUE)), (mean(almotacreek_obs_yearDOC$tmax, na.rm=TRUE)), (mean(almotacreek_obs_yearDOC$tmin, na.rm=TRUE))) 
longpine_DOC <- data.frame(PNF_csv$id[24], (mean(longpine_obs_yearDOC$prcp, na.rm=TRUE)), (mean(longpine_obs_yearDOC$tmax, na.rm=TRUE)), (mean(longpine_obs_yearDOC$tmin, na.rm=TRUE))) 
albin_DOC <- data.frame(PNF_csv$id[25], (mean(albin_obs_yearDOC$prcp, na.rm=TRUE)), (mean(albin_obs_yearDOC$tmax, na.rm=TRUE)), (mean(albin_obs_yearDOC$tmin, na.rm=TRUE))) 
alliance_DOC <- data.frame(PNF_csv$id[26], (mean(alliance_obs_yearDOC$prcp, na.rm=TRUE)), (mean(alliance_obs_yearDOC$tmax, na.rm=TRUE)), (mean(alliance_obs_yearDOC$tmin, na.rm=TRUE))) 
chadron4_DOC <- data.frame(PNF_csv$id[27], (mean(chadron4_obs_yearDOC$prcp, na.rm=TRUE)), (mean(chadron4_obs_yearDOC$tmax, na.rm=TRUE)), (mean(chadron4_obs_yearDOC$tmin, na.rm=TRUE))) 
chadron5_DOC <- data.frame(PNF_csv$id[28], (mean(chadron5_obs_yearDOC$prcp, na.rm=TRUE)), (mean(chadron5_obs_yearDOC$tmax, na.rm=TRUE)), (mean(chadron5_obs_yearDOC$tmin, na.rm=TRUE)))

#rename col.names 
cat(citySwap3(city2, y = "colnames(", z = "_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')"), sep = "\n")



## combine current year of the date of collection data using bind_rows
cat(citySwap7(city2, y = "_DOC", z = ","), sep = "")
PNW_yearDOC <- bind_rows(glasgow_DOC,wallawalla_DOC,caldwell_DOC,hastings_DOC,corvallis_DOC,pullman_DOC,boise_DOC,anacortes_DOC,pocatello_DOC,cheney_DOC,chadron1_DOC,chadron2_DOC,chadron3_DOC,mullen1_DOC,mullen2_DOC,gering_DOC,crawford_DOC,littlebluetownship_DOC,browkenbow_DOC,jerome_DOC,neligh_DOC,bannercounty_DOC,almotacreek_DOC,longpine_DOC,albin_DOC,alliance_DOC,chadron4_DOC,chadron5_DOC)

# save data
write.xlsx(PNW_yearDOC, file = "PNW_year_DOC_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
