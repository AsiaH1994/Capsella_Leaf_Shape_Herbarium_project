rm(list=ls())
library(FluMoDL)
library(dplyr)
library(fuzzyjoin)
library(weatherData)
library(rnoaa)
options(noaakey = "ifwilTHTRMxSsheNajCJqXNJTFbjAaBk")
library(rgdal)
library(zoo)
library("xlsx")
setwd("~/Documents")

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

######## data prep 
#### Delaware data prep 
delaware_csv <- read.csv('delaware_cities.csv')
delaware_csv$month <- match(delaware_csv$month, month.name)
delaware_csv$date <- paste(delaware_csv$year, delaware_csv$month, delaware_csv$day, sep="-")
strptime(delaware_csv$date,format="%Y-%m-%d")
delaware_csv$date <- as.Date(delaware_csv$date)
colnames(delaware_csv) <- c("id", 'id2', 'city_county_state', 'latitude', 'longitude', 'elevation', 'month', 'day', 'year', 'date')
delaware_csv$previous6mon <- as.Date(as.yearmon(as.Date(delaware_csv$date)) -.5, frac = 1)
head(delaware_csv)
##added this code to know what is the city 
city <- delaware_csv$id
city2 <- delaware_csv$id2
date <- delaware_csv$date
pyear <- (delaware_csv$date - 365)
p6mon <- delaware_csv$previous6mon
year <- delaware_csv$year
lennumber <- (1:14) #number of samples

## important data frames
df1 = list(city, city2, date)

df2 <- list(city, city2, p6mon, date)

df3 <- list(city2, lennumber)

df4 <- list(city, city2, pyear, date)

df5 <- list(city, city2, year)

#stations 
stations <- ghcnd_stations()
east_coast_station <- stations %>% filter(state == c("VA", "MD", "DE"))
#write.csv2(east_coast_station, "east_coast_stations.csv")
east_coast_station <- read.csv("east_coast_stations.csv")

### find closests stations
delaware_clst <- meteo_nearby_stations(lat_lon_df = delaware_csv, station_data = east_coast_station, radius = 50)

### separate all cities 
cat(citySwap(unique(city), y = " <- (delaware_clst$'", z = "')"), sep = "\n")

newark <- (delaware_clst$'newark')
dagsboro <- (delaware_clst$'dagsboro')
elsmere <- (delaware_clst$'elsmere')
wilmington <- (delaware_clst$'wilmington')
newcastlecounty <- (delaware_clst$'newcastlecounty')
dover <- (delaware_clst$'dover')
seaford <- (delaware_clst$'seaford')
marydel <- (delaware_clst$'marydel')
delawarecity <- (delaware_clst$'delawarecity')

### separate all station id's (monitors) for each city
cat(citySwap(unique(city), y = "_monitors <- ", z = "$id"), sep = "\n")

newark_monitors <- newark$id
dagsboro_monitors <- dagsboro$id
elsmere_monitors <- elsmere$id
wilmington_monitors <- wilmington$id
newcastlecounty_monitors <- newcastlecounty$id
dover_monitors <- dover$id
seaford_monitors <- seaford$id
marydel_monitors <- marydel$id
delawarecity_monitors <- delawarecity$id

### pull all observations for cities
cat(citySwap(unique(city), y = "_obs <- meteo_pull_monitors(", z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')"), sep = "\n")

newark_obs <- meteo_pull_monitors(newark_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
dagsboro_obs <- meteo_pull_monitors(dagsboro_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
elsmere_obs <- meteo_pull_monitors(elsmere_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
wilmington_obs <- meteo_pull_monitors(wilmington_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
newcastlecounty_obs <- meteo_pull_monitors(newcastlecounty_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
dover_obs <- meteo_pull_monitors(dover_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
seaford_obs <- meteo_pull_monitors(seaford_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
marydel_obs <- meteo_pull_monitors(marydel_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
delawarecity_obs <- meteo_pull_monitors(delawarecity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')

################ data for date of collection ###############
# pull data by date and location - dates are coming from state_csv
lapply(df1, cat(citySwap4.5(x = city2, y = "_obs_temps <- ", i = city, z = "_obs %>% filter(date == '", j = date , h = "')"), sep = "\n"))

newark1_obs_temps <- newark_obs %>% filter(date == '1965-04-19')
dagsboro_obs_temps <- dagsboro_obs %>% filter(date == '2011-04-23')
elsmere_obs_temps <- elsmere_obs %>% filter(date == '1868-06-24')
wilmington1_obs_temps <- wilmington_obs %>% filter(date == '1933-05-10')
newcastlecounty_obs_temps <- newcastlecounty_obs %>% filter(date == '1932-05-07')
dover1_obs_temps <- dover_obs %>% filter(date == '1984-10-22')
wilmington2_obs_temps <- wilmington_obs %>% filter(date == '1898-06-03')
dover2_obs_temps <- dover_obs %>% filter(date == '1984-07-17')
seaford_obs_temps <- seaford_obs %>% filter(date == '1927-04-14')
newark2_obs_temps <- newark_obs %>% filter(date == '1969-04-18')
newark3_obs_temps <- newark_obs %>% filter(date == '1896-04-28')
marydel_obs_temps <- marydel_obs %>% filter(date == '1980-05-02')
delawarecity_obs_temps <- delawarecity_obs %>% filter(date == '1967-04-29')
dover3_obs_temps <- dover_obs %>% filter(date == '1984-10-23')

## combine using bind_rows
cat(citySwap7(city2, y = "_obs_temps", z = ","), sep = "")
delaware_obs <- bind_rows(newark1_obs_temps,dagsboro_obs_temps,elsmere_obs_temps,wilmington1_obs_temps,newcastlecounty_obs_temps,dover1_obs_temps,wilmington2_obs_temps,dover2_obs_temps,seaford_obs_temps,newark2_obs_temps,newark3_obs_temps,marydel_obs_temps,delawarecity_obs_temps,dover3_obs_temps)

## merge with state_csv
delaware_data <- merge.data.frame(delaware_csv, delaware_obs, by = 'date')

## find the tavg for state data 
delaware_data$tavg <- ((delaware_data$tmax + delaware_data$tmin) / 2)

### save data as xlsx file 
write.xlsx(delaware_data, file = "delaware_data_total_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

################# data for previous 6 months ##############

## pull data for the previous 6 months before the date of collection
lapply(df2, cat(citySwap5(x = city2, y = "_obs_6months <- meteo_pull_monitors(", j = city, 
                          z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '", h = p6mon, 
                          k = "', date_max = '", l = date, p ="')"), sep = "\n"))

newark1_obs_6months <- meteo_pull_monitors(newark_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1964-10-31', date_max = '1965-04-19')
dagsboro_obs_6months <- meteo_pull_monitors(dagsboro_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2010-10-31', date_max = '2011-04-23')
elsmere_obs_6months <- meteo_pull_monitors(elsmere_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1867-12-31', date_max = '1868-06-24')
wilmington1_obs_6months <- meteo_pull_monitors(wilmington_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1932-11-30', date_max = '1933-05-10')
newcastlecounty_obs_6months <- meteo_pull_monitors(newcastlecounty_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1931-11-30', date_max = '1932-05-07')
dover1_obs_6months <- meteo_pull_monitors(dover_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1984-04-30', date_max = '1984-10-22')
wilmington2_obs_6months <- meteo_pull_monitors(wilmington_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1897-12-31', date_max = '1898-06-03')
dover2_obs_6months <- meteo_pull_monitors(dover_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1984-01-31', date_max = '1984-07-17')
seaford_obs_6months <- meteo_pull_monitors(seaford_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1926-10-31', date_max = '1927-04-14')
newark2_obs_6months <- meteo_pull_monitors(newark_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1968-10-31', date_max = '1969-04-18')
newark3_obs_6months <- meteo_pull_monitors(newark_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1895-10-31', date_max = '1896-04-28')
marydel_obs_6months <- meteo_pull_monitors(marydel_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1979-11-30', date_max = '1980-05-02')
delawarecity_obs_6months <- meteo_pull_monitors(delawarecity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1966-10-31', date_max = '1967-04-29')
dover3_obs_6months <- meteo_pull_monitors(dover_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1984-04-30', date_max = '1984-10-23')

## make a dataframe of the mean of tmax, tmin, prcp of the previous 6 months 
lapply(df3, cat(citySwap2(x = city2,  y = "_6months", z = " <- data.frame(delaware_csv$id[", l= lennumber, k = "], (mean(", e = "_obs_6months$prcp, na.rm=TRUE)), (mean(", f = "_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(", g = "_obs_6months$tmin, na.rm=TRUE))) "), sep = "\n"))

newark1_6months <- data.frame(delaware_csv$id[1], (mean(newark1_obs_6months$prcp, na.rm=TRUE)), (mean(newark1_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(newark1_obs_6months$tmin, na.rm=TRUE))) 
dagsboro_6months <- data.frame(delaware_csv$id[2], (mean(dagsboro_obs_6months$prcp, na.rm=TRUE)), (mean(dagsboro_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(dagsboro_obs_6months$tmin, na.rm=TRUE))) 
elsmere_6months <- data.frame(delaware_csv$id[3], (mean(elsmere_obs_6months$prcp, na.rm=TRUE)), (mean(elsmere_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(elsmere_obs_6months$tmin, na.rm=TRUE))) 
wilmington1_6months <- data.frame(delaware_csv$id[4], (mean(wilmington1_obs_6months$prcp, na.rm=TRUE)), (mean(wilmington1_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(wilmington1_obs_6months$tmin, na.rm=TRUE))) 
newcastlecounty_6months <- data.frame(delaware_csv$id[5], (mean(newcastlecounty_obs_6months$prcp, na.rm=TRUE)), (mean(newcastlecounty_obs_6months$tmax, na.rm=TRUE)), 
                                      (mean(newcastlecounty_obs_6months$tmin, na.rm=TRUE))) 
dover1_6months <- data.frame(delaware_csv$id[6], (mean(dover1_obs_6months$prcp, na.rm=TRUE)), (mean(dover1_obs_6months$tmax, na.rm=TRUE)), 
                             (mean(dover1_obs_6months$tmin, na.rm=TRUE))) 
wilmington2_6months <- data.frame(delaware_csv$id[7], (mean(wilmington2_obs_6months$prcp, na.rm=TRUE)), (mean(wilmington2_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(wilmington2_obs_6months$tmin, na.rm=TRUE))) 
dover2_6months <- data.frame(delaware_csv$id[8], (mean(dover2_obs_6months$prcp, na.rm=TRUE)), (mean(dover2_obs_6months$tmax, na.rm=TRUE)), 
                             (mean(dover2_obs_6months$tmin, na.rm=TRUE))) 
seaford_6months <- data.frame(delaware_csv$id[9], (mean(seaford_obs_6months$prcp, na.rm=TRUE)), (mean(seaford_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(seaford_obs_6months$tmin, na.rm=TRUE))) 
newark2_6months <- data.frame(delaware_csv$id[10], (mean(newark2_obs_6months$prcp, na.rm=TRUE)), (mean(newark2_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(newark2_obs_6months$tmin, na.rm=TRUE))) 
newark3_6months <- data.frame(delaware_csv$id[11], (mean(newark3_obs_6months$prcp, na.rm=TRUE)), (mean(newark3_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(newark3_obs_6months$tmin, na.rm=TRUE))) 
marydel_6months <- data.frame(delaware_csv$id[12], (mean(marydel_obs_6months$prcp, na.rm=TRUE)), (mean(marydel_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(marydel_obs_6months$tmin, na.rm=TRUE))) 
delawarecity_6months <- data.frame(delaware_csv$id[13], (mean(delawarecity_obs_6months$prcp, na.rm=TRUE)), (mean(delawarecity_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(delawarecity_obs_6months$tmin, na.rm=TRUE))) 
dover3_6months <- data.frame(delaware_csv$id[14], (mean(dover3_obs_6months$prcp, na.rm=TRUE)), (mean(dover3_obs_6months$tmax, na.rm=TRUE)), 
                             (mean(dover3_obs_6months$tmin, na.rm=TRUE)))

#rename column names 
cat(citySwap3(city2, y = "colnames(", z = "_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')"), sep = "\n")

colnames(newark1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(dagsboro_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(elsmere_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(wilmington1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(newcastlecounty_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(dover1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(wilmington2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(dover2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(seaford_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(newark2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(newark3_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(marydel_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(delawarecity_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(dover3_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')

## combine previous 6 months data using bind_rows
cat(citySwap7(city2, y = "_6months", z = ","), sep = "")
delaware_6months <- bind_rows(newark1_6months,dagsboro_6months,elsmere_6months,wilmington1_6months,newcastlecounty_6months,dover1_6months,wilmington2_6months,dover2_6months,seaford_6months,newark2_6months,newark3_6months,marydel_6months,delawarecity_6months,dover3_6months)


## find the tavg for state data 
delaware_6months$tavg <- ((delaware_6months$tmax + delaware_6months$tmin) / 2)

delaware_6months$tavg <- delaware_6months$tavg *0.1
delaware_6months$tmax_avg <- delaware_6months$tmax_avg *0.1
delaware_6months$tmin_avg <- delaware_6months$tmin_avg*0.1
delaware_6months$prcp_avg <- delaware_6months$prcp_avg *0.1

### save data as xlsx file 
write.xlsx(delaware_6months, file = "delaware_6months_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

################ date from previous year from date of collection #############
lapply(df4, cat(citySwap5(x = city2, y = "_obs_year <- meteo_pull_monitors(", j = city, 
                          z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '", h= pyear, 
                          k = "', date_max = '", l = date, p ="')"), sep = "\n"))

newark1_obs_year <- meteo_pull_monitors(newark_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1964-04-19', date_max = '1965-04-19')
dagsboro_obs_year <- meteo_pull_monitors(dagsboro_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2010-04-23', date_max = '2011-04-23')
elsmere_obs_year <- meteo_pull_monitors(elsmere_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1867-06-25', date_max = '1868-06-24')
wilmington1_obs_year <- meteo_pull_monitors(wilmington_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1932-05-10', date_max = '1933-05-10')
newcastlecounty_obs_year <- meteo_pull_monitors(newcastlecounty_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1931-05-08', date_max = '1932-05-07')
dover1_obs_year <- meteo_pull_monitors(dover_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1983-10-23', date_max = '1984-10-22')
wilmington2_obs_year <- meteo_pull_monitors(wilmington_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1897-06-03', date_max = '1898-06-03')
dover2_obs_year <- meteo_pull_monitors(dover_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1983-07-18', date_max = '1984-07-17')
seaford_obs_year <- meteo_pull_monitors(seaford_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1926-04-14', date_max = '1927-04-14')
newark2_obs_year <- meteo_pull_monitors(newark_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1968-04-18', date_max = '1969-04-18')
newark3_obs_year <- meteo_pull_monitors(newark_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1895-04-29', date_max = '1896-04-28')
marydel_obs_year <- meteo_pull_monitors(marydel_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1979-05-03', date_max = '1980-05-02')
delawarecity_obs_year <- meteo_pull_monitors(delawarecity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1966-04-29', date_max = '1967-04-29')
dover3_obs_year <- meteo_pull_monitors(dover_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1983-10-24', date_max = '1984-10-23')

## make a dataframe of the mean of tmax, tmin, prcp of the previous year before the date of collection 
lapply(df3, cat(citySwap2(x = city2,  y = "_previousyear", z = " <- data.frame(delaware_csv$id[", l= lennumber, 
                          k = "], (mean(", e = "_obs_year$prcp, na.rm=TRUE)), (mean(", 
                          f = "_obs_year$tmax, na.rm=TRUE)), (mean(", g = "_obs_year$tmin, na.rm=TRUE))) "), sep = "\n"))

newark1_previousyear <- data.frame(delaware_csv$id[1], (mean(newark1_obs_year$prcp, na.rm=TRUE)), (mean(newark1_obs_year$tmax, na.rm=TRUE)), (mean(newark1_obs_year$tmin, na.rm=TRUE))) 
dagsboro_previousyear <- data.frame(delaware_csv$id[2], (mean(dagsboro_obs_year$prcp, na.rm=TRUE)), (mean(dagsboro_obs_year$tmax, na.rm=TRUE)), (mean(dagsboro_obs_year$tmin, na.rm=TRUE))) 
elsmere_previousyear <- data.frame(delaware_csv$id[3], (mean(elsmere_obs_year$prcp, na.rm=TRUE)), (mean(elsmere_obs_year$tmax, na.rm=TRUE)), (mean(elsmere_obs_year$tmin, na.rm=TRUE))) 
wilmington1_previousyear <- data.frame(delaware_csv$id[4], (mean(wilmington1_obs_year$prcp, na.rm=TRUE)), (mean(wilmington1_obs_year$tmax, na.rm=TRUE)), (mean(wilmington1_obs_year$tmin, na.rm=TRUE))) 
newcastlecounty_previousyear <- data.frame(delaware_csv$id[5], (mean(newcastlecounty_obs_year$prcp, na.rm=TRUE)), (mean(newcastlecounty_obs_year$tmax, na.rm=TRUE)), (mean(newcastlecounty_obs_year$tmin, na.rm=TRUE))) 
dover1_previousyear <- data.frame(delaware_csv$id[6], (mean(dover1_obs_year$prcp, na.rm=TRUE)), (mean(dover1_obs_year$tmax, na.rm=TRUE)), (mean(dover1_obs_year$tmin, na.rm=TRUE))) 
wilmington2_previousyear <- data.frame(delaware_csv$id[7], (mean(wilmington2_obs_year$prcp, na.rm=TRUE)), (mean(wilmington2_obs_year$tmax, na.rm=TRUE)), (mean(wilmington2_obs_year$tmin, na.rm=TRUE))) 
dover2_previousyear <- data.frame(delaware_csv$id[8], (mean(dover2_obs_year$prcp, na.rm=TRUE)), (mean(dover2_obs_year$tmax, na.rm=TRUE)), (mean(dover2_obs_year$tmin, na.rm=TRUE))) 
seaford_previousyear <- data.frame(delaware_csv$id[9], (mean(seaford_obs_year$prcp, na.rm=TRUE)), (mean(seaford_obs_year$tmax, na.rm=TRUE)), (mean(seaford_obs_year$tmin, na.rm=TRUE))) 
newark2_previousyear <- data.frame(delaware_csv$id[10], (mean(newark2_obs_year$prcp, na.rm=TRUE)), (mean(newark2_obs_year$tmax, na.rm=TRUE)), (mean(newark2_obs_year$tmin, na.rm=TRUE))) 
newark3_previousyear <- data.frame(delaware_csv$id[11], (mean(newark3_obs_year$prcp, na.rm=TRUE)), (mean(newark3_obs_year$tmax, na.rm=TRUE)), (mean(newark3_obs_year$tmin, na.rm=TRUE))) 
marydel_previousyear <- data.frame(delaware_csv$id[12], (mean(marydel_obs_year$prcp, na.rm=TRUE)), (mean(marydel_obs_year$tmax, na.rm=TRUE)), (mean(marydel_obs_year$tmin, na.rm=TRUE))) 
delawarecity_previousyear <- data.frame(delaware_csv$id[13], (mean(delawarecity_obs_year$prcp, na.rm=TRUE)), (mean(delawarecity_obs_year$tmax, na.rm=TRUE)), (mean(delawarecity_obs_year$tmin, na.rm=TRUE))) 
dover3_previousyear <- data.frame(delaware_csv$id[14], (mean(dover3_obs_year$prcp, na.rm=TRUE)), (mean(dover3_obs_year$tmax, na.rm=TRUE)), (mean(dover3_obs_year$tmin, na.rm=TRUE))) 

#rename col.names 
cat(citySwap3(city2, y = "colnames(", z = "_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')"), sep = "\n")

colnames(newark1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(dagsboro_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(elsmere_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(wilmington1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(newcastlecounty_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(dover1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(wilmington2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(dover2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(seaford_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(newark2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(newark3_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(marydel_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(delawarecity_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(dover3_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')

## combine previous year before the date of collection data using bind_rows
cat(citySwap7(city2, y = "_previousyear", z = ","), sep = "")
delaware_Pyear <- bind_rows(newark1_previousyear,dagsboro_previousyear,elsmere_previousyear,wilmington1_previousyear,newcastlecounty_previousyear,dover1_previousyear,wilmington2_previousyear,dover2_previousyear,seaford_previousyear,newark2_previousyear,newark3_previousyear,marydel_previousyear,delawarecity_previousyear,dover3_previousyear)

# save data
write.xlsx(delaware_Pyear, file = "delaware_previous_year_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

################## data from year of date of collection ################
## pull data for the current year of the date of collection
lapply(df5, cat(citySwap6(x = city2, y = "_obs_yearDOC <- meteo_pull_monitors(", j = city, 
                          z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '", l = year , 
                          k = "-01-01', date_max = '",  p ="-12-31')"), sep = "\n"))

newark1_obs_yearDOC <- meteo_pull_monitors(newark_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1965-01-01', date_max = '1965-12-31')
dagsboro_obs_yearDOC <- meteo_pull_monitors(dagsboro_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2011-01-01', date_max = '2011-12-31')
elsmere_obs_yearDOC <- meteo_pull_monitors(elsmere_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1868-01-01', date_max = '1868-12-31')
wilmington1_obs_yearDOC <- meteo_pull_monitors(wilmington_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1933-01-01', date_max = '1933-12-31')
newcastlecounty_obs_yearDOC <- meteo_pull_monitors(newcastlecounty_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1932-01-01', date_max = '1932-12-31')
dover1_obs_yearDOC <- meteo_pull_monitors(dover_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1984-01-01', date_max = '1984-12-31')
wilmington2_obs_yearDOC <- meteo_pull_monitors(wilmington_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1898-01-01', date_max = '1898-12-31')
dover2_obs_yearDOC <- meteo_pull_monitors(dover_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1984-01-01', date_max = '1984-12-31')
seaford_obs_yearDOC <- meteo_pull_monitors(seaford_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1927-01-01', date_max = '1927-12-31')
newark2_obs_yearDOC <- meteo_pull_monitors(newark_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1969-01-01', date_max = '1969-12-31')
newark3_obs_yearDOC <- meteo_pull_monitors(newark_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1896-01-01', date_max = '1896-12-31')
marydel_obs_yearDOC <- meteo_pull_monitors(marydel_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1980-01-01', date_max = '1980-12-31')
delawarecity_obs_yearDOC <- meteo_pull_monitors(delawarecity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1967-01-01', date_max = '1967-12-31')
dover3_obs_yearDOC <- meteo_pull_monitors(dover_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1984-01-01', date_max = '1984-12-31')

## make a dataframe of the mean of tmax, tmin, current year of the date of collection
lapply(df3, cat(citySwap2(x = city2,  y = "_DOC", z = " <- data.frame(delaware_csv$id[", l= lennumber, k = "], (mean(", e = "_obs_yearDOC$prcp, na.rm=TRUE)), (mean(", f = "_obs_yearDOC$tmax, na.rm=TRUE)), (mean(", g = "_obs_yearDOC$tmin, na.rm=TRUE))) "), sep = "\n"))

newark1_DOC <- data.frame(delaware_csv$id[1], (mean(newark1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(newark1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(newark1_obs_yearDOC$tmin, na.rm=TRUE))) 
dagsboro_DOC <- data.frame(delaware_csv$id[2], (mean(dagsboro_obs_yearDOC$prcp, na.rm=TRUE)), (mean(dagsboro_obs_yearDOC$tmax, na.rm=TRUE)), (mean(dagsboro_obs_yearDOC$tmin, na.rm=TRUE))) 
elsmere_DOC <- data.frame(delaware_csv$id[3], (mean(elsmere_obs_yearDOC$prcp, na.rm=TRUE)), (mean(elsmere_obs_yearDOC$tmax, na.rm=TRUE)), (mean(elsmere_obs_yearDOC$tmin, na.rm=TRUE))) 
wilmington1_DOC <- data.frame(delaware_csv$id[4], (mean(wilmington1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(wilmington1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(wilmington1_obs_yearDOC$tmin, na.rm=TRUE))) 
newcastlecounty_DOC <- data.frame(delaware_csv$id[5], (mean(newcastlecounty_obs_yearDOC$prcp, na.rm=TRUE)), (mean(newcastlecounty_obs_yearDOC$tmax, na.rm=TRUE)), (mean(newcastlecounty_obs_yearDOC$tmin, na.rm=TRUE))) 
dover1_DOC <- data.frame(delaware_csv$id[6], (mean(dover1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(dover1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(dover1_obs_yearDOC$tmin, na.rm=TRUE))) 
wilmington2_DOC <- data.frame(delaware_csv$id[7], (mean(wilmington2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(wilmington2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(wilmington2_obs_yearDOC$tmin, na.rm=TRUE))) 
dover2_DOC <- data.frame(delaware_csv$id[8], (mean(dover2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(dover2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(dover2_obs_yearDOC$tmin, na.rm=TRUE))) 
seaford_DOC <- data.frame(delaware_csv$id[9], (mean(seaford_obs_yearDOC$prcp, na.rm=TRUE)), (mean(seaford_obs_yearDOC$tmax, na.rm=TRUE)), (mean(seaford_obs_yearDOC$tmin, na.rm=TRUE))) 
newark2_DOC <- data.frame(delaware_csv$id[10], (mean(newark2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(newark2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(newark2_obs_yearDOC$tmin, na.rm=TRUE))) 
newark3_DOC <- data.frame(delaware_csv$id[11], (mean(newark3_obs_yearDOC$prcp, na.rm=TRUE)), (mean(newark3_obs_yearDOC$tmax, na.rm=TRUE)), (mean(newark3_obs_yearDOC$tmin, na.rm=TRUE))) 
marydel_DOC <- data.frame(delaware_csv$id[12], (mean(marydel_obs_yearDOC$prcp, na.rm=TRUE)), (mean(marydel_obs_yearDOC$tmax, na.rm=TRUE)), (mean(marydel_obs_yearDOC$tmin, na.rm=TRUE))) 
delawarecity_DOC <- data.frame(delaware_csv$id[13], (mean(delawarecity_obs_yearDOC$prcp, na.rm=TRUE)), (mean(delawarecity_obs_yearDOC$tmax, na.rm=TRUE)), (mean(delawarecity_obs_yearDOC$tmin, na.rm=TRUE))) 
dover3_DOC <- data.frame(delaware_csv$id[14], (mean(dover3_obs_yearDOC$prcp, na.rm=TRUE)), (mean(dover3_obs_yearDOC$tmax, na.rm=TRUE)), (mean(dover3_obs_yearDOC$tmin, na.rm=TRUE))) 

#rename col.names 
cat(citySwap3(city2, y = "colnames(", z = "_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')"), sep = "\n")

colnames(newark1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(dagsboro_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(elsmere_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(wilmington1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(newcastlecounty_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(dover1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(wilmington2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(dover2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(seaford_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(newark2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(newark3_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(marydel_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(delawarecity_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(dover3_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')

## combine current year of the date of collection data using bind_rows
cat(citySwap7(city2, y = "_DOC", z = ","), sep = "")
delaware_yearDOC <- bind_rows(newark1_DOC,dagsboro_DOC,elsmere_DOC,wilmington1_DOC,newcastlecounty_DOC,dover1_DOC,wilmington2_DOC,dover2_DOC,seaford_DOC,newark2_DOC,newark3_DOC,marydel_DOC,delawarecity_DOC,dover3_DOC)

# save data
write.xlsx(delaware_yearDOC, file = "delaware_year_DOC_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

