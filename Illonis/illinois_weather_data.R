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
IL_csv <- read.csv('illinois_cities.csv')
IL_csv$month <- match(IL_csv$month, month.name)
IL_csv$date <- paste(IL_csv$year, IL_csv$month, IL_csv$day, sep="-")
strptime(IL_csv$date,format="%Y-%m-%d")
IL_csv$date <- as.Date(IL_csv$date)
colnames(IL_csv) <- c("id", 'id2', 'city_county_state', 'latitude', 'longitude', 'elevation', 'month', 'day', 'year', 'date')
IL_csv$previous6mon <- as.Date(as.yearmon(as.Date(IL_csv$date)) -.5, frac = 1)
head(IL_csv)

## to use lapply 
city <- IL_csv$id
city2 <- IL_csv$id2
date <- IL_csv$date
pyear <- (IL_csv$date - 365)
p6mon <- IL_csv$previous6mon
year <- IL_csv$year
lennumber <- (1:15) #number of samples

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
IL_clst <- meteo_nearby_stations(lat_lon_df = IL_csv, station_data = great_lakes_stations, radius = 50, limit = 200)

### separate all cities 
cat(citySwap(unique(city), y = " <- (IL_clst$'", z = "')"), sep = "\n")

fairbury <- (IL_clst$'fairbury')
parkridge <- (IL_clst$'parkridge')
momence <- (IL_clst$'momence')
starkcounty <- (IL_clst$'starkcounty')
lacon <- (IL_clst$'lacon')
joilet <- (IL_clst$'joilet')
champaign <- (IL_clst$'champaign')
naperville <- (IL_clst$'naperville')
charleston <- (IL_clst$'charleston')
chicago <- (IL_clst$'chicago')
dekalb <- (IL_clst$'dekalb')

### separate all station id's (monitors) for each city
cat(citySwap(unique(city), y = "_monitors <- ", z = "$id"), sep = "\n")

fairbury_monitors <- fairbury$id
parkridge_monitors <- parkridge$id
momence_monitors <- momence$id
starkcounty_monitors <- starkcounty$id
lacon_monitors <- lacon$id
joilet_monitors <- joilet$id
champaign_monitors <- champaign$id
naperville_monitors <- naperville$id
charleston_monitors <- charleston$id
chicago_monitors <- chicago$id
dekalb_monitors <- dekalb$id

### pull all observations for cities 
cat(citySwap(unique(city), y = "_obs <- meteo_pull_monitors(", z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')"), sep = "\n")

fairbury_obs <- meteo_pull_monitors(fairbury_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
parkridge_obs <- meteo_pull_monitors(parkridge_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
momence_obs <- meteo_pull_monitors(momence_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
starkcounty_obs <- meteo_pull_monitors(starkcounty_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
lacon_obs <- meteo_pull_monitors(lacon_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
joilet_obs <- meteo_pull_monitors(joilet_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
champaign_obs <- meteo_pull_monitors(champaign_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
naperville_obs <- meteo_pull_monitors(naperville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
charleston_obs <- meteo_pull_monitors(charleston_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
chicago_obs <- meteo_pull_monitors(chicago_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
dekalb_obs <- meteo_pull_monitors(dekalb_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')

### Part 1
### pull data by date and location - dates are coming from state_csv 
lapply(df1, cat(citySwap4.5(x = city2, y = "_obs_temps <- ", i = city, z = "_obs %>% filter(date == '", j = date , h = "')"), sep = "\n"))

fairbury_obs_temps <- fairbury_obs %>% filter(date == '1999-05-02')
parkridge_obs_temps <- parkridge_obs %>% filter(date == '1907-05-15')
momence_obs_temps <- momence_obs %>% filter(date == '1975-09-15')
starkcounty_obs_temps <- starkcounty_obs %>% filter(date == '1907-05-26')
lacon_obs_temps <- lacon_obs %>% filter(date == '1981-04-27')
joilet_obs_temps <- joilet_obs %>% filter(date == '1975-05-10')
champaign_obs_temps <- champaign_obs %>% filter(date == '1966-05-15')
naperville1_obs_temps <- naperville_obs %>% filter(date == '1998-05-02')
naperville2_obs_temps <- naperville_obs %>% filter(date == '1970-04-24')
charleston1_obs_temps <- charleston_obs %>% filter(date == '1965-05-10')
chicago1_obs_temps <- chicago_obs %>% filter(date == '1967-06-02')
charleston2_obs_temps <- charleston_obs %>% filter(date == '1946-04-07')
chicago2_obs_temps <- chicago_obs %>% filter(date == '1960-06-22')
dekalb_obs_temps <- dekalb_obs %>% filter(date == '1895-05-10')
chicago3_obs_temps <- chicago_obs %>% filter(date == '1898-05-12')

#combine into one data frame using bind_rows()
cat(citySwap7(city2, y = "_obs_temps", z = ","), sep = "")
IL_obs <- bind_rows(fairbury_obs_temps,parkridge_obs_temps,momence_obs_temps,starkcounty_obs_temps,lacon_obs_temps,joilet_obs_temps,champaign_obs_temps,naperville1_obs_temps,naperville2_obs_temps,charleston1_obs_temps,chicago1_obs_temps,charleston2_obs_temps,chicago2_obs_temps,dekalb_obs_temps,chicago3_obs_temps)

#Merge the original state_csv dataframe and new state_obs dataframe 
IL_data <- merge.data.frame(IL_csv, IL_obs, by = 'date')

## find the tavg for state data 
IL_data$tavg <- ((IL_data$tmax + IL_data$tmin) / 2)

### save data as xlsx file 
library("xlsx")
write.xlsx(IL_data, file = "IL_data_total_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

### Part 2
#finding data from 6 months prior to date of collection
#ambient temps and precipitation may have affected plant/leaf growth and shape
lapply(df2, cat(citySwap5(x = city2, y = "_obs_6months <- meteo_pull_monitors(", j = city, 
                          z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '", h = p6mon, 
                          k = "', date_max = '", l = date, p ="')"), sep = "\n"))

fairbury_obs_6months <- meteo_pull_monitors(fairbury_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1998-11-30', date_max = '1999-05-02')
parkridge_obs_6months <- meteo_pull_monitors(parkridge_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1906-11-30', date_max = '1907-05-15')
momence_obs_6months <- meteo_pull_monitors(momence_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1975-03-31', date_max = '1975-09-15')
starkcounty_obs_6months <- meteo_pull_monitors(starkcounty_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1906-11-30', date_max = '1907-05-26')
lacon_obs_6months <- meteo_pull_monitors(lacon_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1980-10-31', date_max = '1981-04-27')
joilet_obs_6months <- meteo_pull_monitors(joilet_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1974-11-30', date_max = '1975-05-10')
champaign_obs_6months <- meteo_pull_monitors(champaign_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1965-11-30', date_max = '1966-05-15')
naperville1_obs_6months <- meteo_pull_monitors(naperville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1997-11-30', date_max = '1998-05-02')
naperville2_obs_6months <- meteo_pull_monitors(naperville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1969-10-31', date_max = '1970-04-24')
charleston1_obs_6months <- meteo_pull_monitors(charleston_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1964-11-30', date_max = '1965-05-10')
chicago1_obs_6months <- meteo_pull_monitors(chicago_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1966-12-31', date_max = '1967-06-02')
charleston2_obs_6months <- meteo_pull_monitors(charleston_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1945-10-31', date_max = '1946-04-07')
chicago2_obs_6months <- meteo_pull_monitors(chicago_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1959-12-31', date_max = '1960-06-22')
dekalb_obs_6months <- meteo_pull_monitors(dekalb_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1894-11-30', date_max = '1895-05-10')
chicago3_obs_6months <- meteo_pull_monitors(chicago_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1897-11-30', date_max = '1898-05-12')

## make a dataframe of the mean of tmax, tmin, prcp of the previous 6 months 
lapply(df3, cat(citySwap2(x = city2,  y = "_6months", z = " <- data.frame(IL_csv$id[", l= lennumber, k = "], (mean(", e = "_obs_6months$prcp, na.rm=TRUE)), (mean(", f = "_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(", g = "_obs_6months$tmin, na.rm=TRUE))) "), sep = "\n"))

fairbury_6months <- data.frame(IL_csv$id[1], (mean(fairbury_obs_6months$prcp, na.rm=TRUE)), (mean(fairbury_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(fairbury_obs_6months$tmin, na.rm=TRUE))) 
parkridge_6months <- data.frame(IL_csv$id[2], (mean(parkridge_obs_6months$prcp, na.rm=TRUE)), (mean(parkridge_obs_6months$tmax, na.rm=TRUE)), 
                                (mean(parkridge_obs_6months$tmin, na.rm=TRUE))) 
momence_6months <- data.frame(IL_csv$id[3], (mean(momence_obs_6months$prcp, na.rm=TRUE)), (mean(momence_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(momence_obs_6months$tmin, na.rm=TRUE))) 
starkcounty_6months <- data.frame(IL_csv$id[4], (mean(starkcounty_obs_6months$prcp, na.rm=TRUE)), (mean(starkcounty_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(starkcounty_obs_6months$tmin, na.rm=TRUE))) 
lacon_6months <- data.frame(IL_csv$id[5], (mean(lacon_obs_6months$prcp, na.rm=TRUE)), (mean(lacon_obs_6months$tmax, na.rm=TRUE)), 
                            (mean(lacon_obs_6months$tmin, na.rm=TRUE))) 
joilet_6months <- data.frame(IL_csv$id[6], (mean(joilet_obs_6months$prcp, na.rm=TRUE)), (mean(joilet_obs_6months$tmax, na.rm=TRUE)), 
                             (mean(joilet_obs_6months$tmin, na.rm=TRUE))) 
champaign_6months <- data.frame(IL_csv$id[7], (mean(champaign_obs_6months$prcp, na.rm=TRUE)), (mean(champaign_obs_6months$tmax, na.rm=TRUE)), 
                                (mean(champaign_obs_6months$tmin, na.rm=TRUE))) 
naperville1_6months <- data.frame(IL_csv$id[8], (mean(naperville1_obs_6months$prcp, na.rm=TRUE)), (mean(naperville1_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(naperville1_obs_6months$tmin, na.rm=TRUE))) 
naperville2_6months <- data.frame(IL_csv$id[9], (mean(naperville2_obs_6months$prcp, na.rm=TRUE)), (mean(naperville2_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(naperville2_obs_6months$tmin, na.rm=TRUE))) 
charleston1_6months <- data.frame(IL_csv$id[10], (mean(charleston1_obs_6months$prcp, na.rm=TRUE)), (mean(charleston1_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(charleston1_obs_6months$tmin, na.rm=TRUE))) 
chicago1_6months <- data.frame(IL_csv$id[11], (mean(chicago1_obs_6months$prcp, na.rm=TRUE)), (mean(chicago1_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(chicago1_obs_6months$tmin, na.rm=TRUE))) 
charleston2_6months <- data.frame(IL_csv$id[12], (mean(charleston2_obs_6months$prcp, na.rm=TRUE)), (mean(charleston2_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(charleston2_obs_6months$tmin, na.rm=TRUE))) 
chicago2_6months <- data.frame(IL_csv$id[13], (mean(chicago2_obs_6months$prcp, na.rm=TRUE)), (mean(chicago2_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(chicago2_obs_6months$tmin, na.rm=TRUE))) 
dekalb_6months <- data.frame(IL_csv$id[14], (mean(dekalb_obs_6months$prcp, na.rm=TRUE)), (mean(dekalb_obs_6months$tmax, na.rm=TRUE)), 
                             (mean(dekalb_obs_6months$tmin, na.rm=TRUE))) 
chicago3_6months <- data.frame(IL_csv$id[15], (mean(chicago3_obs_6months$prcp, na.rm=TRUE)), (mean(chicago3_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(chicago3_obs_6months$tmin, na.rm=TRUE))) 

#rename col.names 
cat(citySwap3(city2, y = "colnames(", z = "_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')"), sep = "\n")

colnames(fairbury_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(parkridge_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(momence_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(starkcounty_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(lacon_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(joilet_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(champaign_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(naperville1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(naperville2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(charleston1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(chicago1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(charleston2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(chicago2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(dekalb_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(chicago3_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')

## combine previous 6 months data using bind_rows
cat(citySwap7(city2, y = "_6months", z = ","), sep = "")
IL_6months <- bind_rows(fairbury_6months,parkridge_6months,momence_6months,starkcounty_6months,lacon_6months,joilet_6months,champaign_6months,naperville1_6months,naperville2_6months,charleston1_6months,chicago1_6months,charleston2_6months,chicago2_6months,dekalb_6months,chicago3_6months)

### save data as xlsx file 
write.xlsx(IL_6months, file = "IL_6months_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


### Part 3
#find data from previous year from date of collection 
lapply(df4, cat(citySwap5(x = city2, y = "_obs_year <- meteo_pull_monitors(", j = city, 
                          z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '", h= pyear, 
                          k = "', date_max = '", l = date, p ="')"), sep = "\n"))

fairbury_obs_year <- meteo_pull_monitors(fairbury_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1998-05-02', date_max = '1999-05-02')
parkridge_obs_year <- meteo_pull_monitors(parkridge_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1906-05-15', date_max = '1907-05-15')
momence_obs_year <- meteo_pull_monitors(momence_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1974-09-15', date_max = '1975-09-15')
starkcounty_obs_year <- meteo_pull_monitors(starkcounty_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1906-05-26', date_max = '1907-05-26')
lacon_obs_year <- meteo_pull_monitors(lacon_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1980-04-27', date_max = '1981-04-27')
joilet_obs_year <- meteo_pull_monitors(joilet_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1974-05-10', date_max = '1975-05-10')
champaign_obs_year <- meteo_pull_monitors(champaign_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1965-05-15', date_max = '1966-05-15')
naperville1_obs_year <- meteo_pull_monitors(naperville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1997-05-02', date_max = '1998-05-02')
naperville2_obs_year <- meteo_pull_monitors(naperville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1969-04-24', date_max = '1970-04-24')
charleston1_obs_year <- meteo_pull_monitors(charleston_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1964-05-10', date_max = '1965-05-10')
chicago1_obs_year <- meteo_pull_monitors(chicago_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1966-06-02', date_max = '1967-06-02')
charleston2_obs_year <- meteo_pull_monitors(charleston_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1945-04-07', date_max = '1946-04-07')
chicago2_obs_year <- meteo_pull_monitors(chicago_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1959-06-23', date_max = '1960-06-22')
dekalb_obs_year <- meteo_pull_monitors(dekalb_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1894-05-10', date_max = '1895-05-10')
chicago3_obs_year <- meteo_pull_monitors(chicago_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1897-05-12', date_max = '1898-05-12')

## make a data frame of the mean of tmax, tmin, prcp of the previous year before the date of collection 
lapply(df3, cat(citySwap2(x = city2,  y = "_previousyear", z = " <- data.frame(IL_csv$id[", l= lennumber, 
                          k = "], (mean(", e = "_obs_year$prcp, na.rm=TRUE)), (mean(", 
                          f = "_obs_year$tmax, na.rm=TRUE)), (mean(", g = "_obs_year$tmin, na.rm=TRUE))) "), sep = "\n"))

fairbury_previousyear <- data.frame(IL_csv$id[1], (mean(fairbury_obs_year$prcp, na.rm=TRUE)), (mean(fairbury_obs_year$tmax, na.rm=TRUE)), (mean(fairbury_obs_year$tmin, na.rm=TRUE))) 
parkridge_previousyear <- data.frame(IL_csv$id[2], (mean(parkridge_obs_year$prcp, na.rm=TRUE)), (mean(parkridge_obs_year$tmax, na.rm=TRUE)), (mean(parkridge_obs_year$tmin, na.rm=TRUE))) 
momence_previousyear <- data.frame(IL_csv$id[3], (mean(momence_obs_year$prcp, na.rm=TRUE)), (mean(momence_obs_year$tmax, na.rm=TRUE)), (mean(momence_obs_year$tmin, na.rm=TRUE))) 
starkcounty_previousyear <- data.frame(IL_csv$id[4], (mean(starkcounty_obs_year$prcp, na.rm=TRUE)), (mean(starkcounty_obs_year$tmax, na.rm=TRUE)), (mean(starkcounty_obs_year$tmin, na.rm=TRUE))) 
lacon_previousyear <- data.frame(IL_csv$id[5], (mean(lacon_obs_year$prcp, na.rm=TRUE)), (mean(lacon_obs_year$tmax, na.rm=TRUE)), (mean(lacon_obs_year$tmin, na.rm=TRUE))) 
joilet_previousyear <- data.frame(IL_csv$id[6], (mean(joilet_obs_year$prcp, na.rm=TRUE)), (mean(joilet_obs_year$tmax, na.rm=TRUE)), (mean(joilet_obs_year$tmin, na.rm=TRUE))) 
champaign_previousyear <- data.frame(IL_csv$id[7], (mean(champaign_obs_year$prcp, na.rm=TRUE)), (mean(champaign_obs_year$tmax, na.rm=TRUE)), (mean(champaign_obs_year$tmin, na.rm=TRUE))) 
naperville1_previousyear <- data.frame(IL_csv$id[8], (mean(naperville1_obs_year$prcp, na.rm=TRUE)), (mean(naperville1_obs_year$tmax, na.rm=TRUE)), (mean(naperville1_obs_year$tmin, na.rm=TRUE))) 
naperville2_previousyear <- data.frame(IL_csv$id[9], (mean(naperville2_obs_year$prcp, na.rm=TRUE)), (mean(naperville2_obs_year$tmax, na.rm=TRUE)), (mean(naperville2_obs_year$tmin, na.rm=TRUE))) 
charleston1_previousyear <- data.frame(IL_csv$id[10], (mean(charleston1_obs_year$prcp, na.rm=TRUE)), (mean(charleston1_obs_year$tmax, na.rm=TRUE)), (mean(charleston1_obs_year$tmin, na.rm=TRUE))) 
chicago1_previousyear <- data.frame(IL_csv$id[11], (mean(chicago1_obs_year$prcp, na.rm=TRUE)), (mean(chicago1_obs_year$tmax, na.rm=TRUE)), (mean(chicago1_obs_year$tmin, na.rm=TRUE))) 
charleston2_previousyear <- data.frame(IL_csv$id[12], (mean(charleston2_obs_year$prcp, na.rm=TRUE)), (mean(charleston2_obs_year$tmax, na.rm=TRUE)), (mean(charleston2_obs_year$tmin, na.rm=TRUE))) 
chicago2_previousyear <- data.frame(IL_csv$id[13], (mean(chicago2_obs_year$prcp, na.rm=TRUE)), (mean(chicago2_obs_year$tmax, na.rm=TRUE)), (mean(chicago2_obs_year$tmin, na.rm=TRUE))) 
dekalb_previousyear <- data.frame(IL_csv$id[14], (mean(dekalb_obs_year$prcp, na.rm=TRUE)), (mean(dekalb_obs_year$tmax, na.rm=TRUE)), (mean(dekalb_obs_year$tmin, na.rm=TRUE))) 
chicago3_previousyear <- data.frame(IL_csv$id[15], (mean(chicago3_obs_year$prcp, na.rm=TRUE)), (mean(chicago3_obs_year$tmax, na.rm=TRUE)), (mean(chicago3_obs_year$tmin, na.rm=TRUE))) 

#rename col.names 
cat(citySwap3(city2, y = "colnames(", z = "_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')"), sep = "\n")

colnames(fairbury_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(parkridge_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(momence_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(starkcounty_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(lacon_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(joilet_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(champaign_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(naperville1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(naperville2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(charleston1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(chicago1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(charleston2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(chicago2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(dekalb_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(chicago3_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')

## combine previous year before the date of collection data using bind_rows
cat(citySwap7(city2, y = "_previousyear", z = ","), sep = "")
IL_Pyear <- bind_rows(fairbury_previousyear,parkridge_previousyear,momence_previousyear,starkcounty_previousyear,lacon_previousyear,joilet_previousyear,champaign_previousyear,naperville1_previousyear,naperville2_previousyear,charleston1_previousyear,chicago1_previousyear,charleston2_previousyear,chicago2_previousyear,dekalb_previousyear,chicago3_previousyear)

# save data
write.xlsx(IL_Pyear, file = "IL_previous_year_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


### Part 4 
#Find data from current year of date of collection 
lapply(df5, cat(citySwap6(x = city2, y = "_obs_yearDOC <- meteo_pull_monitors(", j = city, 
                          z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '", l = year , 
                          k = "-01-01', date_max = '",  p ="-12-31')"), sep = "\n"))

fairbury_obs_yearDOC <- meteo_pull_monitors(fairbury_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1999-01-01', date_max = '1999-12-31')
parkridge_obs_yearDOC <- meteo_pull_monitors(parkridge_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1907-01-01', date_max = '1907-12-31')
momence_obs_yearDOC <- meteo_pull_monitors(momence_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1975-01-01', date_max = '1975-12-31')
starkcounty_obs_yearDOC <- meteo_pull_monitors(starkcounty_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1907-01-01', date_max = '1907-12-31')
lacon_obs_yearDOC <- meteo_pull_monitors(lacon_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1981-01-01', date_max = '1981-12-31')
joilet_obs_yearDOC <- meteo_pull_monitors(joilet_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1975-01-01', date_max = '1975-12-31')
champaign_obs_yearDOC <- meteo_pull_monitors(champaign_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1966-01-01', date_max = '1966-12-31')
naperville1_obs_yearDOC <- meteo_pull_monitors(naperville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1998-01-01', date_max = '1998-12-31')
naperville2_obs_yearDOC <- meteo_pull_monitors(naperville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1970-01-01', date_max = '1970-12-31')
charleston1_obs_yearDOC <- meteo_pull_monitors(charleston_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1965-01-01', date_max = '1965-12-31')
chicago1_obs_yearDOC <- meteo_pull_monitors(chicago_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1967-01-01', date_max = '1967-12-31')
charleston2_obs_yearDOC <- meteo_pull_monitors(charleston_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1946-01-01', date_max = '1946-12-31')
chicago2_obs_yearDOC <- meteo_pull_monitors(chicago_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1960-01-01', date_max = '1960-12-31')
dekalb_obs_yearDOC <- meteo_pull_monitors(dekalb_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1895-01-01', date_max = '1895-12-31')
chicago3_obs_yearDOC <- meteo_pull_monitors(chicago_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1898-01-01', date_max = '1898-12-31')

## make a dataframe of the mean of tmax, tmin, current year of the date of collection
lapply(dflist_nodates, cat(citySwap2(x = city2,  y = "_DOC", z = " <- data.frame(IL_csv$id[", l= lennumber, k = "], (mean(", e = "_obs_yearDOC$prcp, na.rm=TRUE)), (mean(", f = "_obs_yearDOC$tmax, na.rm=TRUE)), (mean(", g = "_obs_yearDOC$tmin, na.rm=TRUE))) "), sep = "\n"))

fairbury_DOC <- data.frame(IL_csv$id[1], (mean(fairbury_obs_yearDOC$prcp, na.rm=TRUE)), (mean(fairbury_obs_yearDOC$tmax, na.rm=TRUE)), (mean(fairbury_obs_yearDOC$tmin, na.rm=TRUE))) 
parkridge_DOC <- data.frame(IL_csv$id[2], (mean(parkridge_obs_yearDOC$prcp, na.rm=TRUE)), (mean(parkridge_obs_yearDOC$tmax, na.rm=TRUE)), (mean(parkridge_obs_yearDOC$tmin, na.rm=TRUE))) 
momence_DOC <- data.frame(IL_csv$id[3], (mean(momence_obs_yearDOC$prcp, na.rm=TRUE)), (mean(momence_obs_yearDOC$tmax, na.rm=TRUE)), (mean(momence_obs_yearDOC$tmin, na.rm=TRUE))) 
starkcounty_DOC <- data.frame(IL_csv$id[4], (mean(starkcounty_obs_yearDOC$prcp, na.rm=TRUE)), (mean(starkcounty_obs_yearDOC$tmax, na.rm=TRUE)), (mean(starkcounty_obs_yearDOC$tmin, na.rm=TRUE))) 
lacon_DOC <- data.frame(IL_csv$id[5], (mean(lacon_obs_yearDOC$prcp, na.rm=TRUE)), (mean(lacon_obs_yearDOC$tmax, na.rm=TRUE)), (mean(lacon_obs_yearDOC$tmin, na.rm=TRUE))) 
joilet_DOC <- data.frame(IL_csv$id[6], (mean(joilet_obs_yearDOC$prcp, na.rm=TRUE)), (mean(joilet_obs_yearDOC$tmax, na.rm=TRUE)), (mean(joilet_obs_yearDOC$tmin, na.rm=TRUE))) 
champaign_DOC <- data.frame(IL_csv$id[7], (mean(champaign_obs_yearDOC$prcp, na.rm=TRUE)), (mean(champaign_obs_yearDOC$tmax, na.rm=TRUE)), (mean(champaign_obs_yearDOC$tmin, na.rm=TRUE))) 
naperville1_DOC <- data.frame(IL_csv$id[8], (mean(naperville1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(naperville1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(naperville1_obs_yearDOC$tmin, na.rm=TRUE))) 
naperville2_DOC <- data.frame(IL_csv$id[9], (mean(naperville2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(naperville2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(naperville2_obs_yearDOC$tmin, na.rm=TRUE))) 
charleston1_DOC <- data.frame(IL_csv$id[10], (mean(charleston1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(charleston1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(charleston1_obs_yearDOC$tmin, na.rm=TRUE))) 
chicago1_DOC <- data.frame(IL_csv$id[11], (mean(chicago1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(chicago1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(chicago1_obs_yearDOC$tmin, na.rm=TRUE))) 
charleston2_DOC <- data.frame(IL_csv$id[12], (mean(charleston2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(charleston2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(charleston2_obs_yearDOC$tmin, na.rm=TRUE))) 
chicago2_DOC <- data.frame(IL_csv$id[13], (mean(chicago2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(chicago2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(chicago2_obs_yearDOC$tmin, na.rm=TRUE))) 
dekalb_DOC <- data.frame(IL_csv$id[14], (mean(dekalb_obs_yearDOC$prcp, na.rm=TRUE)), (mean(dekalb_obs_yearDOC$tmax, na.rm=TRUE)), (mean(dekalb_obs_yearDOC$tmin, na.rm=TRUE))) 
chicago3_DOC <- data.frame(IL_csv$id[15], (mean(chicago3_obs_yearDOC$prcp, na.rm=TRUE)), (mean(chicago3_obs_yearDOC$tmax, na.rm=TRUE)), (mean(chicago3_obs_yearDOC$tmin, na.rm=TRUE))) 

#rename col.names 
cat(citySwap3(city2, y = "colnames(", z = "_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')"), sep = "\n")

colnames(fairbury_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(parkridge_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(momence_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(starkcounty_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(lacon_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(joilet_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(champaign_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(naperville1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(naperville2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(charleston1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(chicago1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(charleston2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(chicago2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(dekalb_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(chicago3_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')

## combine current year of the date of collection data using bind_rows
cat(citySwap7(city2, y = "_DOC", z = ","), sep = "")
IL_yearDOC <- bind_rows(fairbury_DOC,parkridge_DOC,momence_DOC,starkcounty_DOC,lacon_DOC,joilet_DOC,champaign_DOC,naperville1_DOC,naperville2_DOC,charleston1_DOC,chicago1_DOC,charleston2_DOC,chicago2_DOC,dekalb_DOC,chicago3_DOC)

# save data
write.xlsx(IL_yearDOC, file = "IL_year_DOC_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
