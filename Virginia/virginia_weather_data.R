#rm(list=ls())
library(FluMoDL)
library(dplyr)
library(fuzzyjoin)
library(weatherData)
library(rnoaa)
options(noaakey = "ifwilTHTRMxSsheNajCJqXNJTFbjAaBk")
library(rgdal)
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

#add one date 
citySwap_4 = function(x, y, z, j, h){
  paste(x, y, x, z, j, h, sep = "")
}

#one date and city column name, x = city2, i = city
citySwap4.5 = function(x, y, z, j, h, i){
  paste(x, y, i, z, j, h, sep = "")
}

#adding max and min dates 
citySwap5 = function(x, y, z, j, h, k, l, p){
  paste(x, y, j, z, h, k, l, p, sep = '')
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
#### Wisconsin data prep 
virginia_csv <- read.csv('virginia_cities.csv')
virginia_csv$month <- match(virginia_csv$month, month.name)
virginia_csv$date <- paste(virginia_csv$year, virginia_csv$month, virginia_csv$day, sep="-")
strptime(virginia_csv$date,format="%Y-%m-%d")
virginia_csv$date <- as.Date(virginia_csv$date)
colnames(virginia_csv) <- c("id", 'id2', 'city_county_state', 'latitude', 'longitude', 'elevation', 'month', 'day', 'year', 'date')
head(virginia_csv)
##added this code to know what is the city 
city <- virginia_csv$id
city2 <- virginia_csv$id2
lennumber <- (1:76) #number of samples

##NOAA stations - set up and filter by nearby states
stations <- ghcnd_stations()
#write.table(stations, "ghcnd_stations.txt")

east_coast_station <- stations %>% filter(state == c("VA", "MD", "DE"))
write.csv2(east_coast_station, "east_coast_stations.csv")

#after running the 1st time 
east_coast_station <- read.csv("east_coast_stations.csv")

### find closests stations
virginia_clst <- meteo_nearby_stations(lat_lon_df = virginia_csv, station_data = east_coast_station, radius = 50)

### separate all cities 
cat(citySwap(unique(city), y = " <- (virginia_clst$'", z = "')"), sep = "\n")

richmond <- (virginia_clst$'richmond')
norfolk <- (virginia_clst$'norfolk')
middlesexcounty <- (virginia_clst$'middlesexcounty')
suffolkcity <- (virginia_clst$'suffolkcity')
hopewell <- (virginia_clst$'hopewell')
charoltesville <- (virginia_clst$'charoltesville')
radford <- (virginia_clst$'radford')
williamsburg <- (virginia_clst$'williamsburg')
alexandria <- (virginia_clst$'alexandria')
fredericksburg <- (virginia_clst$'fredericksburg')
harrisonburg <- (virginia_clst$'harrisonburg')
pocomokecity <- (virginia_clst$'pocomokecity')
roanokecounty <- (virginia_clst$'roanokecounty')
winchester <- (virginia_clst$'winchester')
alexandriacity <- (virginia_clst$'alexandriacity')
emporia <- (virginia_clst$'emporia')
lynchburg <- (virginia_clst$'lynchburg')
accomac <- (virginia_clst$'accomac')
leesburg <- (virginia_clst$'leesburg')
reston <- (virginia_clst$'reston')
staunton <- (virginia_clst$'staunton')
virginiabeach <- (virginia_clst$'virginiabeach')
exmore <- (virginia_clst$'exmore')
newportnews <- (virginia_clst$'newportnews')
franlkin <- (virginia_clst$'franlkin')
hampton <- (virginia_clst$'hampton')

### separate all station id's (monitors) for each city
cat(citySwap(unique(city), y = "_monitors <- ", z = "$id"), sep = "\n")

richmond_monitors <- richmond$id
norfolk_monitors <- norfolk$id
middlesexcounty_monitors <- middlesexcounty$id
suffolkcity_monitors <- suffolkcity$id
hopewell_monitors <- hopewell$id
charoltesville_monitors <- charoltesville$id
radford_monitors <- radford$id
williamsburg_monitors <- williamsburg$id
alexandria_monitors <- alexandria$id
fredericksburg_monitors <- fredericksburg$id
harrisonburg_monitors <- harrisonburg$id
pocomokecity_monitors <- pocomokecity$id
roanokecounty_monitors <- roanokecounty$id
winchester_monitors <- winchester$id
alexandriacity_monitors <- alexandriacity$id
emporia_monitors <- emporia$id
lynchburg_monitors <- lynchburg$id
accomac_monitors <- accomac$id
leesburg_monitors <- leesburg$id
reston_monitors <- reston$id
staunton_monitors <- staunton$id
virginiabeach_monitors <- virginiabeach$id
exmore_monitors <- exmore$id
newportnews_monitors <- newportnews$id
franlkin_monitors <- franlkin$id
hampton_monitors <- hampton$id

### pull all observations for cities
cat(citySwap(unique(city), y = "_obs <- meteo_pull_monitors(", z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')"), sep = "\n")

richmond_obs <- meteo_pull_monitors(richmond_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
norfolk_obs <- meteo_pull_monitors(norfolk_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
middlesexcounty_obs <- meteo_pull_monitors(middlesexcounty_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
suffolkcity_obs <- meteo_pull_monitors(suffolkcity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
hopewell_obs <- meteo_pull_monitors(hopewell_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
charoltesville_obs <- meteo_pull_monitors(charoltesville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
radford_obs <- meteo_pull_monitors(radford_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
williamsburg_obs <- meteo_pull_monitors(williamsburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
alexandria_obs <- meteo_pull_monitors(alexandria_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
fredericksburg_obs <- meteo_pull_monitors(fredericksburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
harrisonburg_obs <- meteo_pull_monitors(harrisonburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
pocomokecity_obs <- meteo_pull_monitors(pocomokecity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
roanokecounty_obs <- meteo_pull_monitors(roanokecounty_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
winchester_obs <- meteo_pull_monitors(winchester_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
alexandriacity_obs <- meteo_pull_monitors(alexandriacity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
emporia_obs <- meteo_pull_monitors(emporia_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
lynchburg_obs <- meteo_pull_monitors(lynchburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
accomac_obs <- meteo_pull_monitors(accomac_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
leesburg_obs <- meteo_pull_monitors(leesburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
reston_obs <- meteo_pull_monitors(reston_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
staunton_obs <- meteo_pull_monitors(staunton_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
virginiabeach_obs <- meteo_pull_monitors(virginiabeach_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
exmore_obs <- meteo_pull_monitors(exmore_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
newportnews_obs <- meteo_pull_monitors(newportnews_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
franlkin_obs <- meteo_pull_monitors(franlkin_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
hampton_obs <- meteo_pull_monitors(hampton_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')

#make df list using list()
df1 = list(city, city2, date)
date = virginia_csv$date
lapply(df1, cat(citySwap4.5(x = city2, y = "_obs_temps <- ", i = city, z = "_obs %>% filter(date == '", j = date , h = "')"), sep = "\n"))

richmond1_obs_temps <- richmond_obs %>% filter(date == '1970-05-09')
norfolk1_obs_temps <- norfolk_obs %>% filter(date == '1975-03-31')
richmond2_obs_temps <- richmond_obs %>% filter(date == '1976-03-08')
richmond3_obs_temps <- richmond_obs %>% filter(date == '1942-05-03')
middlesexcounty_obs_temps <- middlesexcounty_obs %>% filter(date == '1932-04-29')
richmond4_obs_temps <- richmond_obs %>% filter(date == '1978-04-02')
richmond5_obs_temps <- richmond_obs %>% filter(date == '1932-04-25')
richmond6_obs_temps <- richmond_obs %>% filter(date == '1935-03-29')
richmond7_obs_temps <- richmond_obs %>% filter(date == '1931-04-23')
richmond7.5_obs_temps <- richmond_obs %>% filter(date == '1932-04-01')
richmond8_obs_temps <- richmond_obs %>% filter(date == '1931-05-20')
#richmond9_obs_temps <- richmond_obs %>% filter(date == 'NA')
#richmond10_obs_temps <- richmond_obs %>% filter(date == 'NA')
suffolkcity1_obs_temps <- suffolkcity_obs %>% filter(date == '1979-04-01')
richmond11_obs_temps <- richmond_obs %>% filter(date == '1978-04-16')
hopewell1_obs_temps <- hopewell_obs %>% filter(date == '1978-04-17')
richmond12_obs_temps <- richmond_obs %>% filter(date == '1973-04-13')
richmond13_obs_temps <- richmond_obs %>% filter(date == '1971-04-29')
charoltesville1_obs_temps <- charoltesville_obs %>% filter(date == '1971-04-29')
radford1_obs_temps <- radford_obs %>% filter(date == '1981-03-27')
williamsburg_obs_temps <- williamsburg_obs %>% filter(date == '1999-11-18')
alexandria1_obs_temps <- alexandria_obs %>% filter(date == '1939-04-24')
fredericksburg1_obs_temps <- fredericksburg_obs %>% filter(date == '1969-04-09')
radford2_obs_temps <- radford_obs %>% filter(date == '1978-05-17')
#radford3_obs_temps <- radford_obs %>% filter(date == 'NA')
hopewell2_obs_temps <- hopewell_obs %>% filter(date == '1930-05-09')
williamsburg2_obs_temps <- williamsburg_obs %>% filter(date == '1967-04-13')
harrisonburg1_obs_temps <- harrisonburg_obs %>% filter(date == '2002-04-12')
suffolkcity2_obs_temps <- suffolkcity_obs %>% filter(date == '2009-03-09')
fredericksburg2_obs_temps <- fredericksburg_obs %>% filter(date == '1999-05-11')
richmond14_obs_temps <- richmond_obs %>% filter(date == '1970-05-09')
harrisonburg2_obs_temps <- harrisonburg_obs %>% filter(date == '1962-04-02')
pocomokecity_obs_temps <- pocomokecity_obs %>% filter(date == '1938-05-22')
roanokecounty_obs_temps <- roanokecounty_obs %>% filter(date == '1979-06-12')
williamsburg3_obs_temps <- williamsburg_obs %>% filter(date == '1975-05-26')
winchester1_obs_temps <- winchester_obs %>% filter(date == '1904-05-30')
alexandriacity_obs_temps <- alexandriacity_obs %>% filter(date == '1972-04-06')
harrisonburg2_obs_temps <- harrisonburg_obs %>% filter(date == '1983-03-22')
winchester2_obs_temps <- winchester_obs %>% filter(date == '1980-08-17')
emporia_obs_temps <- emporia_obs %>% filter(date == '1979-03-31')
lynchburg1_obs_temps <- lynchburg_obs %>% filter(date == '1984-04-10')
norfolk1_obs_temps <- norfolk_obs %>% filter(date == '1975-03-31')
norfolk2_obs_temps <- norfolk_obs %>% filter(date == '1975-03-31')
accomac_obs_temps <- accomac_obs %>% filter(date == '2011-04-03')
norfolk3_obs_temps <- norfolk_obs %>% filter(date == '1969-03-09')
harrisonburg3_obs_temps <- harrisonburg_obs %>% filter(date == '1984-05-02')
lynchburg2_obs_temps <- lynchburg_obs %>% filter(date == '1979-04-21')
winchester3_obs_temps <- winchester_obs %>% filter(date == '1971-05-08')
harrisonburg4_obs_temps <- harrisonburg_obs %>% filter(date == '1980-04-23')
lynchburg3_obs_temps <- lynchburg_obs %>% filter(date == '1984-04-10')
leesburg_obs_temps <- leesburg_obs %>% filter(date == '1969-04-24')
lynchburg4_obs_temps <- lynchburg_obs %>% filter(date == '2011-04-03')
norfolk4_obs_temps <- norfolk_obs %>% filter(date == '1970-04-04')
richmond15_obs_temps <- richmond_obs %>% filter(date == '1979-05-04')
reston_obs_temps <- reston_obs %>% filter(date == '1971-04-13')
harrisonburg5_obs_temps <- harrisonburg_obs %>% filter(date == '1984-04-22')
norfolk5_obs_temps <- norfolk_obs %>% filter(date == '1970-04-04')
staunton_obs_temps <- staunton_obs %>% filter(date == '1978-08-10')
charoltesville2_obs_temps <- charoltesville_obs %>% filter(date == '1978-05-03')
harrisonburg6_obs_temps <- harrisonburg_obs %>% filter(date == '1976-04-20')
norfolk6_obs_temps <- norfolk_obs %>% filter(date == '1978-03-31')
fredericksburg3_obs_temps <- fredericksburg_obs %>% filter(date == '1978-04-30')
harrisonburg7_obs_temps <- harrisonburg_obs %>% filter(date == '2002-04-13')
norfolk7_obs_temps <- norfolk_obs %>% filter(date == '2004-03-25')
winchester4_obs_temps <- winchester_obs %>% filter(date == '2011-05-05')
virginiabeach1_obs_temps <- virginiabeach_obs %>% filter(date == '1978-03-22')
exmore_obs_temps <- exmore_obs %>% filter(date == '1935-10-11')
williamsburg4_obs_temps <- williamsburg_obs %>% filter(date == '1939-02-20')
harrisonburg8_obs_temps <- harrisonburg_obs %>% filter(date == '1984-04-25')
newportnews_obs_temps <- newportnews_obs %>% filter(date == '1979-04-08')
franlkin_obs_temps <- franlkin_obs %>% filter(date == '1939-04-06')
charoltesville3_obs_temps <- charoltesville_obs %>% filter(date == '1971-05-01')
norfolk8_obs_temps <- norfolk_obs %>% filter(date == '1966-04-01')
hampton_obs_temps <- hampton_obs %>% filter(date == '1887-04-07')
virginiabeach2_obs_temps <- virginiabeach_obs %>% filter(date == '1966-04-23')
harrisonburg9_obs_temps <- harrisonburg_obs %>% filter(date == '1976-04-07')


## combine using bind_rows
cat(citySwap7(city2, y = "_obs_temps", z = ","), sep = "")
virginia_obs <- bind_rows(richmond1_obs_temps,norfolk1_obs_temps,richmond2_obs_temps,richmond3_obs_temps,middlesexcounty_obs_temps,richmond4_obs_temps,richmond5_obs_temps,richmond6_obs_temps,richmond7_obs_temps,richmond7.5_obs_temps,richmond8_obs_temps,suffolkcity1_obs_temps,richmond11_obs_temps,hopewell1_obs_temps,richmond12_obs_temps,richmond13_obs_temps,charoltesville1_obs_temps,radford1_obs_temps,williamsburg_obs_temps,alexandria1_obs_temps,fredericksburg1_obs_temps,radford2_obs_temps,hopewell2_obs_temps,williamsburg2_obs_temps,harrisonburg1_obs_temps,suffolkcity2_obs_temps,fredericksburg2_obs_temps,richmond14_obs_temps,harrisonburg2_obs_temps,pocomokecity_obs_temps,roanokecounty_obs_temps,williamsburg3_obs_temps,winchester1_obs_temps,alexandriacity_obs_temps,harrisonburg2_obs_temps,winchester2_obs_temps,emporia_obs_temps,lynchburg1_obs_temps,norfolk1_obs_temps,norfolk2_obs_temps,accomac_obs_temps,norfolk3_obs_temps,harrisonburg3_obs_temps,lynchburg2_obs_temps,winchester3_obs_temps,harrisonburg4_obs_temps,lynchburg3_obs_temps,leesburg_obs_temps,lynchburg4_obs_temps,norfolk4_obs_temps,richmond15_obs_temps,reston_obs_temps,harrisonburg5_obs_temps,norfolk5_obs_temps,staunton_obs_temps,charoltesville2_obs_temps,harrisonburg6_obs_temps,norfolk6_obs_temps,fredericksburg3_obs_temps,harrisonburg7_obs_temps,norfolk7_obs_temps,winchester4_obs_temps,virginiabeach1_obs_temps,exmore_obs_temps,williamsburg4_obs_temps,harrisonburg8_obs_temps,newportnews_obs_temps,franlkin_obs_temps,charoltesville3_obs_temps,norfolk8_obs_temps,hampton_obs_temps,virginiabeach2_obs_temps,harrisonburg9_obs_temps)

## merge with state_csv
virginia_data <- merge.data.frame(virginia_csv, virginia_obs, by = 'date')

## find the tavg for state data 
#### finding tavg
# general code: state_data$tavg <- ((state_data$tmax + state_data$tmin) / 2)
virginia_data$tavg <- ((virginia_data$tmax + virginia_data$tmin) / 2)

### save data as xlsx file 
library("xlsx")
write.xlsx(virginia_data, file = "virginia_data_total_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

### Find the previous 6 months and add to original state_csv dataframe 
library(zoo)
virginia_csv$previous6mon <- as.Date(as.yearmon(as.Date(virginia_csv$date)) -.5, frac = 1)

## pull data for the previous 6 months before the date of collection
p6mon <- virginia_csv$previous6mon
df2 <- list(city, city2, p6mon, date)
lapply(df2, cat(citySwap5(x = city2, y = "_obs_6months <- meteo_pull_monitors(", j = city, 
                                      z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '", h = p6mon, 
                                      k = "', date_max = '", l = date, p ="')"), sep = "\n"))

richmond1_obs_6months <- meteo_pull_monitors(richmond_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1969-11-30', date_max = '1970-05-09')
norfolk1_obs_6months <- meteo_pull_monitors(norfolk_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1974-09-30', date_max = '1975-03-31')
richmond2_obs_6months <- meteo_pull_monitors(richmond_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1975-09-30', date_max = '1976-03-08')
richmond3_obs_6months <- meteo_pull_monitors(richmond_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1941-11-30', date_max = '1942-05-03')
middlesexcounty_obs_6months <- meteo_pull_monitors(middlesexcounty_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1931-10-31', date_max = '1932-04-29')
richmond4_obs_6months <- meteo_pull_monitors(richmond_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1977-10-31', date_max = '1978-04-02')
richmond5_obs_6months <- meteo_pull_monitors(richmond_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1931-10-31', date_max = '1932-04-25')
richmond6_obs_6months <- meteo_pull_monitors(richmond_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1934-09-30', date_max = '1935-03-29')
richmond7_obs_6months <- meteo_pull_monitors(richmond_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1930-10-31', date_max = '1931-04-23')
richmond7.5_obs_6months <- meteo_pull_monitors(richmond_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1931-10-31', date_max = '1932-04-01')
richmond8_obs_6months <- meteo_pull_monitors(richmond_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1930-11-30', date_max = '1931-05-20')
suffolkcity1_obs_6months <- meteo_pull_monitors(suffolkcity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1978-10-31', date_max = '1979-04-01')
richmond11_obs_6months <- meteo_pull_monitors(richmond_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1977-10-31', date_max = '1978-04-16')
hopewell1_obs_6months <- meteo_pull_monitors(hopewell_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1977-10-31', date_max = '1978-04-17')
richmond12_obs_6months <- meteo_pull_monitors(richmond_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1972-10-31', date_max = '1973-04-13')
richmond13_obs_6months <- meteo_pull_monitors(richmond_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1970-10-31', date_max = '1971-04-29')
charoltesville1_obs_6months <- meteo_pull_monitors(charoltesville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1970-10-31', date_max = '1971-04-29')
radford1_obs_6months <- meteo_pull_monitors(radford_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1980-09-30', date_max = '1981-03-27')
williamsburg_obs_6months <- meteo_pull_monitors(williamsburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1999-05-31', date_max = '1999-11-18')
alexandria1_obs_6months <- meteo_pull_monitors(alexandria_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1938-10-31', date_max = '1939-04-24')
fredericksburg1_obs_6months <- meteo_pull_monitors(fredericksburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1968-10-31', date_max = '1969-04-09')
radford2_obs_6months <- meteo_pull_monitors(radford_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1977-11-30', date_max = '1978-05-17')
hopewell2_obs_6months <- meteo_pull_monitors(hopewell_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1929-11-30', date_max = '1930-05-09')
williamsburg2_obs_6months <- meteo_pull_monitors(williamsburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1966-10-31', date_max = '1967-04-13')
harrisonburg1_obs_6months <- meteo_pull_monitors(harrisonburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2001-10-31', date_max = '2002-04-12')
suffolkcity2_obs_6months <- meteo_pull_monitors(suffolkcity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2008-09-30', date_max = '2009-03-09')
fredericksburg2_obs_6months <- meteo_pull_monitors(fredericksburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1998-11-30', date_max = '1999-05-11')
richmond14_obs_6months <- meteo_pull_monitors(richmond_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1969-11-30', date_max = '1970-05-09')
harrisonburg2_obs_6months <- meteo_pull_monitors(harrisonburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1961-10-31', date_max = '1962-04-02')
pocomokecity_obs_6months <- meteo_pull_monitors(pocomokecity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1937-11-30', date_max = '1938-05-22')
roanokecounty_obs_6months <- meteo_pull_monitors(roanokecounty_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1978-12-31', date_max = '1979-06-12')
williamsburg3_obs_6months <- meteo_pull_monitors(williamsburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1974-11-30', date_max = '1975-05-26')
winchester1_obs_6months <- meteo_pull_monitors(winchester_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1903-11-30', date_max = '1904-05-30')
alexandriacity_obs_6months <- meteo_pull_monitors(alexandriacity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1971-10-31', date_max = '1972-04-06')
harrisonburg2_obs_6months <- meteo_pull_monitors(harrisonburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1982-09-30', date_max = '1983-03-22')
winchester2_obs_6months <- meteo_pull_monitors(winchester_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1980-02-29', date_max = '1980-08-17')
emporia_obs_6months <- meteo_pull_monitors(emporia_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1978-09-30', date_max = '1979-03-31')
lynchburg1_obs_6months <- meteo_pull_monitors(lynchburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1983-10-31', date_max = '1984-04-10')
norfolk1_obs_6months <- meteo_pull_monitors(norfolk_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1974-09-30', date_max = '1975-03-31')
norfolk2_obs_6months <- meteo_pull_monitors(norfolk_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1974-09-30', date_max = '1975-03-31')
accomac_obs_6months <- meteo_pull_monitors(accomac_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2010-10-31', date_max = '2011-04-03')
norfolk3_obs_6months <- meteo_pull_monitors(norfolk_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1968-09-30', date_max = '1969-03-09')
harrisonburg3_obs_6months <- meteo_pull_monitors(harrisonburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1983-11-30', date_max = '1984-05-02')
lynchburg2_obs_6months <- meteo_pull_monitors(lynchburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1978-10-31', date_max = '1979-04-21')
winchester3_obs_6months <- meteo_pull_monitors(winchester_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1970-11-30', date_max = '1971-05-08')
harrisonburg4_obs_6months <- meteo_pull_monitors(harrisonburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1979-10-31', date_max = '1980-04-23')
lynchburg3_obs_6months <- meteo_pull_monitors(lynchburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1983-10-31', date_max = '1984-04-10')
leesburg_obs_6months <- meteo_pull_monitors(leesburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1968-10-31', date_max = '1969-04-24')
lynchburg4_obs_6months <- meteo_pull_monitors(lynchburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2010-10-31', date_max = '2011-04-03')
norfolk4_obs_6months <- meteo_pull_monitors(norfolk_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1969-10-31', date_max = '1970-04-04')
richmond15_obs_6months <- meteo_pull_monitors(richmond_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1978-11-30', date_max = '1979-05-04')
reston_obs_6months <- meteo_pull_monitors(reston_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1970-10-31', date_max = '1971-04-13')
harrisonburg5_obs_6months <- meteo_pull_monitors(harrisonburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1983-10-31', date_max = '1984-04-22')
norfolk5_obs_6months <- meteo_pull_monitors(norfolk_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1969-10-31', date_max = '1970-04-04')
staunton_obs_6months <- meteo_pull_monitors(staunton_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1978-02-28', date_max = '1978-08-10')
charoltesville2_obs_6months <- meteo_pull_monitors(charoltesville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1977-11-30', date_max = '1978-05-03')
harrisonburg6_obs_6months <- meteo_pull_monitors(harrisonburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1975-10-31', date_max = '1976-04-20')
norfolk6_obs_6months <- meteo_pull_monitors(norfolk_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1977-09-30', date_max = '1978-03-31')
fredericksburg3_obs_6months <- meteo_pull_monitors(fredericksburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1977-10-31', date_max = '1978-04-30')
harrisonburg7_obs_6months <- meteo_pull_monitors(harrisonburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2001-10-31', date_max = '2002-04-13')
norfolk7_obs_6months <- meteo_pull_monitors(norfolk_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2003-09-30', date_max = '2004-03-25')
winchester4_obs_6months <- meteo_pull_monitors(winchester_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2010-11-30', date_max = '2011-05-05')
virginiabeach1_obs_6months <- meteo_pull_monitors(virginiabeach_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1977-09-30', date_max = '1978-03-22')
exmore_obs_6months <- meteo_pull_monitors(exmore_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1935-04-30', date_max = '1935-10-11')
williamsburg4_obs_6months <- meteo_pull_monitors(williamsburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1938-08-31', date_max = '1939-02-20')
harrisonburg8_obs_6months <- meteo_pull_monitors(harrisonburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1983-10-31', date_max = '1984-04-25')
newportnews_obs_6months <- meteo_pull_monitors(newportnews_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1978-10-31', date_max = '1979-04-08')
franlkin_obs_6months <- meteo_pull_monitors(franlkin_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1938-10-31', date_max = '1939-04-06')
charoltesville3_obs_6months <- meteo_pull_monitors(charoltesville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1970-11-30', date_max = '1971-05-01')
norfolk8_obs_6months <- meteo_pull_monitors(norfolk_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1965-10-31', date_max = '1966-04-01')
hampton_obs_6months <- meteo_pull_monitors(hampton_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1886-10-31', date_max = '1887-04-07')
virginiabeach2_obs_6months <- meteo_pull_monitors(virginiabeach_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1965-10-31', date_max = '1966-04-23')
harrisonburg9_obs_6months <- meteo_pull_monitors(harrisonburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1975-10-31', date_max = '1976-04-07')

## make a dataframe of the mean of tmax, tmin, prcp of the previous 6 months 

df3 <- list(city2, lennumber)
lapply(df3, cat(citySwap2(x = city2,  y = "_6months", z = " <- data.frame(virginia_csv$id[", l= lennumber, k = "], (mean(", e = "_obs_6months$prcp, na.rm=TRUE)), (mean(", f = "_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(", g = "_obs_6months$tmin, na.rm=TRUE))) "), sep = "\n"))
richmond1_6months <- data.frame(virginia_csv$id[1], (mean(richmond1_obs_6months$prcp, na.rm=TRUE)), (mean(richmond1_obs_6months$tmax, na.rm=TRUE)), 
                                (mean(richmond1_obs_6months$tmin, na.rm=TRUE))) 
norfolk1_6months <- data.frame(virginia_csv$id[2], (mean(norfolk1_obs_6months$prcp, na.rm=TRUE)), (mean(norfolk1_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(norfolk1_obs_6months$tmin, na.rm=TRUE))) 
richmond2_6months <- data.frame(virginia_csv$id[3], (mean(richmond2_obs_6months$prcp, na.rm=TRUE)), (mean(richmond2_obs_6months$tmax, na.rm=TRUE)), 
                                (mean(richmond2_obs_6months$tmin, na.rm=TRUE))) 
richmond3_6months <- data.frame(virginia_csv$id[4], (mean(richmond3_obs_6months$prcp, na.rm=TRUE)), (mean(richmond3_obs_6months$tmax, na.rm=TRUE)), 
                                (mean(richmond3_obs_6months$tmin, na.rm=TRUE))) 
middlesexcounty_6months <- data.frame(virginia_csv$id[5], (mean(middlesexcounty_obs_6months$prcp, na.rm=TRUE)), (mean(middlesexcounty_obs_6months$tmax, na.rm=TRUE)), 
                                      (mean(middlesexcounty_obs_6months$tmin, na.rm=TRUE))) 
richmond4_6months <- data.frame(virginia_csv$id[6], (mean(richmond4_obs_6months$prcp, na.rm=TRUE)), (mean(richmond4_obs_6months$tmax, na.rm=TRUE)), 
                                (mean(richmond4_obs_6months$tmin, na.rm=TRUE))) 
richmond5_6months <- data.frame(virginia_csv$id[7], (mean(richmond5_obs_6months$prcp, na.rm=TRUE)), (mean(richmond5_obs_6months$tmax, na.rm=TRUE)), 
                                (mean(richmond5_obs_6months$tmin, na.rm=TRUE))) 
richmond6_6months <- data.frame(virginia_csv$id[8], (mean(richmond6_obs_6months$prcp, na.rm=TRUE)), (mean(richmond6_obs_6months$tmax, na.rm=TRUE)), 
                                (mean(richmond6_obs_6months$tmin, na.rm=TRUE))) 
richmond7_6months <- data.frame(virginia_csv$id[9], (mean(richmond7_obs_6months$prcp, na.rm=TRUE)), (mean(richmond7_obs_6months$tmax, na.rm=TRUE)), 
                                (mean(richmond7_obs_6months$tmin, na.rm=TRUE))) 
richmond7.5_6months <- data.frame(virginia_csv$id[10], (mean(richmond7.5_obs_6months$prcp, na.rm=TRUE)), (mean(richmond7.5_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(richmond7.5_obs_6months$tmin, na.rm=TRUE))) 
richmond8_6months <- data.frame(virginia_csv$id[11], (mean(richmond8_obs_6months$prcp, na.rm=TRUE)), (mean(richmond8_obs_6months$tmax, na.rm=TRUE)), 
                                (mean(richmond8_obs_6months$tmin, na.rm=TRUE))) 
suffolkcity1_6months <- data.frame(virginia_csv$id[14], (mean(suffolkcity1_obs_6months$prcp, na.rm=TRUE)), (mean(suffolkcity1_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(suffolkcity1_obs_6months$tmin, na.rm=TRUE))) 
richmond11_6months <- data.frame(virginia_csv$id[15], (mean(richmond11_obs_6months$prcp, na.rm=TRUE)), (mean(richmond11_obs_6months$tmax, na.rm=TRUE)), 
                                 (mean(richmond11_obs_6months$tmin, na.rm=TRUE))) 
hopewell1_6months <- data.frame(virginia_csv$id[16], (mean(hopewell1_obs_6months$prcp, na.rm=TRUE)), (mean(hopewell1_obs_6months$tmax, na.rm=TRUE)), 
                                (mean(hopewell1_obs_6months$tmin, na.rm=TRUE))) 
richmond12_6months <- data.frame(virginia_csv$id[17], (mean(richmond12_obs_6months$prcp, na.rm=TRUE)), (mean(richmond12_obs_6months$tmax, na.rm=TRUE)), 
                                 (mean(richmond12_obs_6months$tmin, na.rm=TRUE))) 
richmond13_6months <- data.frame(virginia_csv$id[18], (mean(richmond13_obs_6months$prcp, na.rm=TRUE)), (mean(richmond13_obs_6months$tmax, na.rm=TRUE)), 
                                 (mean(richmond13_obs_6months$tmin, na.rm=TRUE))) 
charoltesville1_6months <- data.frame(virginia_csv$id[19], (mean(charoltesville1_obs_6months$prcp, na.rm=TRUE)), (mean(charoltesville1_obs_6months$tmax, na.rm=TRUE)), 
                                      (mean(charoltesville1_obs_6months$tmin, na.rm=TRUE))) 
radford1_6months <- data.frame(virginia_csv$id[20], (mean(radford1_obs_6months$prcp, na.rm=TRUE)), (mean(radford1_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(radford1_obs_6months$tmin, na.rm=TRUE))) 
williamsburg_6months <- data.frame(virginia_csv$id[21], (mean(williamsburg_obs_6months$prcp, na.rm=TRUE)), (mean(williamsburg_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(williamsburg_obs_6months$tmin, na.rm=TRUE))) 
alexandria1_6months <- data.frame(virginia_csv$id[22], (mean(alexandria1_obs_6months$prcp, na.rm=TRUE)), (mean(alexandria1_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(alexandria1_obs_6months$tmin, na.rm=TRUE))) 
fredericksburg1_6months <- data.frame(virginia_csv$id[23], (mean(fredericksburg1_obs_6months$prcp, na.rm=TRUE)), (mean(fredericksburg1_obs_6months$tmax, na.rm=TRUE)), 
                                      (mean(fredericksburg1_obs_6months$tmin, na.rm=TRUE))) 
radford2_6months <- data.frame(virginia_csv$id[24], (mean(radford2_obs_6months$prcp, na.rm=TRUE)), (mean(radford2_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(radford2_obs_6months$tmin, na.rm=TRUE))) 
hopewell2_6months <- data.frame(virginia_csv$id[26], (mean(hopewell2_obs_6months$prcp, na.rm=TRUE)), (mean(hopewell2_obs_6months$tmax, na.rm=TRUE)), 
                                (mean(hopewell2_obs_6months$tmin, na.rm=TRUE))) 
williamsburg2_6months <- data.frame(virginia_csv$id[27], (mean(williamsburg2_obs_6months$prcp, na.rm=TRUE)), (mean(williamsburg2_obs_6months$tmax, na.rm=TRUE)), 
                                    (mean(williamsburg2_obs_6months$tmin, na.rm=TRUE))) 
harrisonburg1_6months <- data.frame(virginia_csv$id[28], (mean(harrisonburg1_obs_6months$prcp, na.rm=TRUE)), (mean(harrisonburg1_obs_6months$tmax, na.rm=TRUE)), 
                                    (mean(harrisonburg1_obs_6months$tmin, na.rm=TRUE))) 
suffolkcity2_6months <- data.frame(virginia_csv$id[29], (mean(suffolkcity2_obs_6months$prcp, na.rm=TRUE)), (mean(suffolkcity2_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(suffolkcity2_obs_6months$tmin, na.rm=TRUE))) 
fredericksburg2_6months <- data.frame(virginia_csv$id[30], (mean(fredericksburg2_obs_6months$prcp, na.rm=TRUE)), (mean(fredericksburg2_obs_6months$tmax, na.rm=TRUE)), 
                                      (mean(fredericksburg2_obs_6months$tmin, na.rm=TRUE))) 
richmond14_6months <- data.frame(virginia_csv$id[31], (mean(richmond14_obs_6months$prcp, na.rm=TRUE)), (mean(richmond14_obs_6months$tmax, na.rm=TRUE)), 
                                 (mean(richmond14_obs_6months$tmin, na.rm=TRUE))) 
harrisonburg2_6months <- data.frame(virginia_csv$id[32], (mean(harrisonburg2_obs_6months$prcp, na.rm=TRUE)), (mean(harrisonburg2_obs_6months$tmax, na.rm=TRUE)), 
                                    (mean(harrisonburg2_obs_6months$tmin, na.rm=TRUE))) 
pocomokecity_6months <- data.frame(virginia_csv$id[33], (mean(pocomokecity_obs_6months$prcp, na.rm=TRUE)), (mean(pocomokecity_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(pocomokecity_obs_6months$tmin, na.rm=TRUE))) 
roanokecounty_6months <- data.frame(virginia_csv$id[34], (mean(roanokecounty_obs_6months$prcp, na.rm=TRUE)), (mean(roanokecounty_obs_6months$tmax, na.rm=TRUE)), 
                                    (mean(roanokecounty_obs_6months$tmin, na.rm=TRUE))) 
williamsburg3_6months <- data.frame(virginia_csv$id[35], (mean(williamsburg3_obs_6months$prcp, na.rm=TRUE)), (mean(williamsburg3_obs_6months$tmax, na.rm=TRUE)), 
                                    (mean(williamsburg3_obs_6months$tmin, na.rm=TRUE))) 
winchester1_6months <- data.frame(virginia_csv$id[36], (mean(winchester1_obs_6months$prcp, na.rm=TRUE)), (mean(winchester1_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(winchester1_obs_6months$tmin, na.rm=TRUE))) 
alexandriacity_6months <- data.frame(virginia_csv$id[37], (mean(alexandriacity_obs_6months$prcp, na.rm=TRUE)), (mean(alexandriacity_obs_6months$tmax, na.rm=TRUE)), 
                                     (mean(alexandriacity_obs_6months$tmin, na.rm=TRUE))) 
harrisonburg2_6months <- data.frame(virginia_csv$id[38], (mean(harrisonburg2_obs_6months$prcp, na.rm=TRUE)), (mean(harrisonburg2_obs_6months$tmax, na.rm=TRUE)), 
                                    (mean(harrisonburg2_obs_6months$tmin, na.rm=TRUE))) 
winchester2_6months <- data.frame(virginia_csv$id[39], (mean(winchester2_obs_6months$prcp, na.rm=TRUE)), (mean(winchester2_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(winchester2_obs_6months$tmin, na.rm=TRUE))) 
emporia_6months <- data.frame(virginia_csv$id[40], (mean(emporia_obs_6months$prcp, na.rm=TRUE)), (mean(emporia_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(emporia_obs_6months$tmin, na.rm=TRUE))) 
lynchburg1_6months <- data.frame(virginia_csv$id[41], (mean(lynchburg1_obs_6months$prcp, na.rm=TRUE)), (mean(lynchburg1_obs_6months$tmax, na.rm=TRUE)), 
                                 (mean(lynchburg1_obs_6months$tmin, na.rm=TRUE))) 
norfolk1_6months <- data.frame(virginia_csv$id[42], (mean(norfolk1_obs_6months$prcp, na.rm=TRUE)), (mean(norfolk1_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(norfolk1_obs_6months$tmin, na.rm=TRUE))) 
norfolk2_6months <- data.frame(virginia_csv$id[43], (mean(norfolk2_obs_6months$prcp, na.rm=TRUE)), (mean(norfolk2_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(norfolk2_obs_6months$tmin, na.rm=TRUE))) 
accomac_6months <- data.frame(virginia_csv$id[44], (mean(accomac_obs_6months$prcp, na.rm=TRUE)), (mean(accomac_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(accomac_obs_6months$tmin, na.rm=TRUE))) 
norfolk3_6months <- data.frame(virginia_csv$id[45], (mean(norfolk3_obs_6months$prcp, na.rm=TRUE)), (mean(norfolk3_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(norfolk3_obs_6months$tmin, na.rm=TRUE))) 
harrisonburg3_6months <- data.frame(virginia_csv$id[46], (mean(harrisonburg3_obs_6months$prcp, na.rm=TRUE)), (mean(harrisonburg3_obs_6months$tmax, na.rm=TRUE)), 
                                    (mean(harrisonburg3_obs_6months$tmin, na.rm=TRUE))) 
lynchburg2_6months <- data.frame(virginia_csv$id[47], (mean(lynchburg2_obs_6months$prcp, na.rm=TRUE)), (mean(lynchburg2_obs_6months$tmax, na.rm=TRUE)), 
                                 (mean(lynchburg2_obs_6months$tmin, na.rm=TRUE))) 
winchester3_6months <- data.frame(virginia_csv$id[48], (mean(winchester3_obs_6months$prcp, na.rm=TRUE)), (mean(winchester3_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(winchester3_obs_6months$tmin, na.rm=TRUE))) 
harrisonburg4_6months <- data.frame(virginia_csv$id[49], (mean(harrisonburg4_obs_6months$prcp, na.rm=TRUE)), (mean(harrisonburg4_obs_6months$tmax, na.rm=TRUE)), 
                                    (mean(harrisonburg4_obs_6months$tmin, na.rm=TRUE))) 
lynchburg3_6months <- data.frame(virginia_csv$id[50], (mean(lynchburg3_obs_6months$prcp, na.rm=TRUE)), (mean(lynchburg3_obs_6months$tmax, na.rm=TRUE)), 
                                 (mean(lynchburg3_obs_6months$tmin, na.rm=TRUE))) 
leesburg_6months <- data.frame(virginia_csv$id[51], (mean(leesburg_obs_6months$prcp, na.rm=TRUE)), (mean(leesburg_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(leesburg_obs_6months$tmin, na.rm=TRUE))) 
lynchburg4_6months <- data.frame(virginia_csv$id[52], (mean(lynchburg4_obs_6months$prcp, na.rm=TRUE)), (mean(lynchburg4_obs_6months$tmax, na.rm=TRUE)), 
                                 (mean(lynchburg4_obs_6months$tmin, na.rm=TRUE))) 
norfolk4_6months <- data.frame(virginia_csv$id[53], (mean(norfolk4_obs_6months$prcp, na.rm=TRUE)), (mean(norfolk4_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(norfolk4_obs_6months$tmin, na.rm=TRUE))) 
richmond15_6months <- data.frame(virginia_csv$id[54], (mean(richmond15_obs_6months$prcp, na.rm=TRUE)), (mean(richmond15_obs_6months$tmax, na.rm=TRUE)), 
                                 (mean(richmond15_obs_6months$tmin, na.rm=TRUE))) 
reston_6months <- data.frame(virginia_csv$id[55], (mean(reston_obs_6months$prcp, na.rm=TRUE)), (mean(reston_obs_6months$tmax, na.rm=TRUE)), 
                             (mean(reston_obs_6months$tmin, na.rm=TRUE))) 
harrisonburg5_6months <- data.frame(virginia_csv$id[56], (mean(harrisonburg5_obs_6months$prcp, na.rm=TRUE)), (mean(harrisonburg5_obs_6months$tmax, na.rm=TRUE)), 
                                    (mean(harrisonburg5_obs_6months$tmin, na.rm=TRUE))) 
norfolk5_6months <- data.frame(virginia_csv$id[57], (mean(norfolk5_obs_6months$prcp, na.rm=TRUE)), (mean(norfolk5_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(norfolk5_obs_6months$tmin, na.rm=TRUE))) 
staunton_6months <- data.frame(virginia_csv$id[58], (mean(staunton_obs_6months$prcp, na.rm=TRUE)), (mean(staunton_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(staunton_obs_6months$tmin, na.rm=TRUE))) 
charoltesville2_6months <- data.frame(virginia_csv$id[59], (mean(charoltesville2_obs_6months$prcp, na.rm=TRUE)), (mean(charoltesville2_obs_6months$tmax, na.rm=TRUE)), 
                                      (mean(charoltesville2_obs_6months$tmin, na.rm=TRUE))) 
harrisonburg6_6months <- data.frame(virginia_csv$id[60], (mean(harrisonburg6_obs_6months$prcp, na.rm=TRUE)), (mean(harrisonburg6_obs_6months$tmax, na.rm=TRUE)), 
                                    (mean(harrisonburg6_obs_6months$tmin, na.rm=TRUE))) 
norfolk6_6months <- data.frame(virginia_csv$id[61], (mean(norfolk6_obs_6months$prcp, na.rm=TRUE)), (mean(norfolk6_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(norfolk6_obs_6months$tmin, na.rm=TRUE))) 
fredericksburg3_6months <- data.frame(virginia_csv$id[62], (mean(fredericksburg3_obs_6months$prcp, na.rm=TRUE)), (mean(fredericksburg3_obs_6months$tmax, na.rm=TRUE)), 
                                      (mean(fredericksburg3_obs_6months$tmin, na.rm=TRUE))) 
harrisonburg7_6months <- data.frame(virginia_csv$id[63], (mean(harrisonburg7_obs_6months$prcp, na.rm=TRUE)), (mean(harrisonburg7_obs_6months$tmax, na.rm=TRUE)), 
                                    (mean(harrisonburg7_obs_6months$tmin, na.rm=TRUE))) 
norfolk7_6months <- data.frame(virginia_csv$id[64], (mean(norfolk7_obs_6months$prcp, na.rm=TRUE)), (mean(norfolk7_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(norfolk7_obs_6months$tmin, na.rm=TRUE))) 
winchester4_6months <- data.frame(virginia_csv$id[65], (mean(winchester4_obs_6months$prcp, na.rm=TRUE)), (mean(winchester4_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(winchester4_obs_6months$tmin, na.rm=TRUE))) 
virginiabeach1_6months <- data.frame(virginia_csv$id[66], (mean(virginiabeach1_obs_6months$prcp, na.rm=TRUE)), (mean(virginiabeach1_obs_6months$tmax, na.rm=TRUE)), 
                                     (mean(virginiabeach1_obs_6months$tmin, na.rm=TRUE))) 
exmore_6months <- data.frame(virginia_csv$id[67], (mean(exmore_obs_6months$prcp, na.rm=TRUE)), (mean(exmore_obs_6months$tmax, na.rm=TRUE)), 
                             (mean(exmore_obs_6months$tmin, na.rm=TRUE))) 
williamsburg4_6months <- data.frame(virginia_csv$id[68], (mean(williamsburg4_obs_6months$prcp, na.rm=TRUE)), (mean(williamsburg4_obs_6months$tmax, na.rm=TRUE)), 
                                    (mean(williamsburg4_obs_6months$tmin, na.rm=TRUE))) 
harrisonburg8_6months <- data.frame(virginia_csv$id[69], (mean(harrisonburg8_obs_6months$prcp, na.rm=TRUE)), (mean(harrisonburg8_obs_6months$tmax, na.rm=TRUE)), 
                                    (mean(harrisonburg8_obs_6months$tmin, na.rm=TRUE))) 
newportnews_6months <- data.frame(virginia_csv$id[70], (mean(newportnews_obs_6months$prcp, na.rm=TRUE)), (mean(newportnews_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(newportnews_obs_6months$tmin, na.rm=TRUE))) 
franlkin_6months <- data.frame(virginia_csv$id[71], (mean(franlkin_obs_6months$prcp, na.rm=TRUE)), (mean(franlkin_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(franlkin_obs_6months$tmin, na.rm=TRUE))) 
charoltesville3_6months <- data.frame(virginia_csv$id[72], (mean(charoltesville3_obs_6months$prcp, na.rm=TRUE)), (mean(charoltesville3_obs_6months$tmax, na.rm=TRUE)), 
                                      (mean(charoltesville3_obs_6months$tmin, na.rm=TRUE))) 
norfolk8_6months <- data.frame(virginia_csv$id[73], (mean(norfolk8_obs_6months$prcp, na.rm=TRUE)), (mean(norfolk8_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(norfolk8_obs_6months$tmin, na.rm=TRUE))) 
hampton_6months <- data.frame(virginia_csv$id[74], (mean(hampton_obs_6months$prcp, na.rm=TRUE)), (mean(hampton_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(hampton_obs_6months$tmin, na.rm=TRUE))) 
virginiabeach2_6months <- data.frame(virginia_csv$id[75], (mean(virginiabeach2_obs_6months$prcp, na.rm=TRUE)), (mean(virginiabeach2_obs_6months$tmax, na.rm=TRUE)), 
                                     (mean(virginiabeach2_obs_6months$tmin, na.rm=TRUE))) 
harrisonburg9_6months <- data.frame(virginia_csv$id[76], (mean(harrisonburg9_obs_6months$prcp, na.rm=TRUE)), (mean(harrisonburg9_obs_6months$tmax, na.rm=TRUE)), 
                                    (mean(harrisonburg9_obs_6months$tmin, na.rm=TRUE)))
#rename col.names 
cat(citySwap3(city2, y = "colnames(", z = "_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')"), sep = "\n")

colnames(richmond1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(norfolk1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(richmond2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(richmond3_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(middlesexcounty_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(richmond4_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(richmond5_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(richmond6_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(richmond7_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(richmond7.5_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(richmond8_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(suffolkcity1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(richmond11_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(hopewell1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(richmond12_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(richmond13_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(charoltesville1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(radford1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(williamsburg_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(alexandria1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(fredericksburg1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(radford2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(hopewell2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(williamsburg2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(harrisonburg1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(suffolkcity2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(fredericksburg2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(richmond14_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(harrisonburg2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(pocomokecity_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(roanokecounty_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(williamsburg3_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(winchester1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(alexandriacity_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(harrisonburg2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(winchester2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(emporia_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(lynchburg1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(norfolk1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(norfolk2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(accomac_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(norfolk3_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(harrisonburg3_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(lynchburg2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(winchester3_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(harrisonburg4_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(lynchburg3_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(leesburg_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(lynchburg4_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(norfolk4_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(richmond15_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(reston_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(harrisonburg5_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(norfolk5_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(staunton_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(charoltesville2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(harrisonburg6_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(norfolk6_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(fredericksburg3_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(harrisonburg7_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(norfolk7_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(winchester4_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(virginiabeach1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(exmore_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(williamsburg4_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(harrisonburg8_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(newportnews_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(franlkin_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(charoltesville3_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(norfolk8_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(hampton_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(virginiabeach2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(harrisonburg9_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')

## combine previous 6 months data using bind_rows
cat(citySwap7(city2, y = "_6months", z = ","), sep = "")
virginia_6months <- bind_rows(richmond1_6months,norfolk1_6months,richmond2_6months,richmond3_6months,middlesexcounty_6months,richmond4_6months,richmond5_6months,richmond6_6months,richmond7_6months,richmond7.5_6months,richmond8_6months,suffolkcity1_6months,richmond11_6months,hopewell1_6months,richmond12_6months,richmond13_6months,charoltesville1_6months,radford1_6months,williamsburg_6months,alexandria1_6months,fredericksburg1_6months,radford2_6months,hopewell2_6months,williamsburg2_6months,harrisonburg1_6months,suffolkcity2_6months,fredericksburg2_6months,richmond14_6months,harrisonburg2_6months,pocomokecity_6months,roanokecounty_6months,williamsburg3_6months,winchester1_6months,alexandriacity_6months,harrisonburg2_6months,winchester2_6months,emporia_6months,lynchburg1_6months,norfolk1_6months,norfolk2_6months,accomac_6months,norfolk3_6months,harrisonburg3_6months,lynchburg2_6months,winchester3_6months,harrisonburg4_6months,lynchburg3_6months,leesburg_6months,lynchburg4_6months,norfolk4_6months,richmond15_6months,reston_6months,harrisonburg5_6months,norfolk5_6months,staunton_6months,charoltesville2_6months,harrisonburg6_6months,norfolk6_6months,fredericksburg3_6months,harrisonburg7_6months,norfolk7_6months,winchester4_6months,virginiabeach1_6months,exmore_6months,williamsburg4_6months,harrisonburg8_6months,newportnews_6months,franlkin_6months,charoltesville3_6months,norfolk8_6months,hampton_6months,virginiabeach2_6months,harrisonburg9_6months)

### save data as xlsx file 
write.xlsx(virginia_6months, file = "virginia_6months_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

## pull data for the previous year before the date of collection
#pyear <- (virginia_csv$date - 365)
#df4 <- list(city, city2, pyear, date)
lapply(df4, cat(citySwap5(x = city2, y = "_obs_year <- meteo_pull_monitors(", j = city, 
                                      z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '", h= pyear, 
                                      k = "', date_max = '", l = date, p ="')"), sep = "\n"))

richmond1_obs_year <- meteo_pull_monitors(richmond_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1969-05-09', date_max = '1970-05-09')
norfolk1_obs_year <- meteo_pull_monitors(norfolk_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1974-03-31', date_max = '1975-03-31')
richmond2_obs_year <- meteo_pull_monitors(richmond_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1975-03-09', date_max = '1976-03-08')
richmond3_obs_year <- meteo_pull_monitors(richmond_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1941-05-03', date_max = '1942-05-03')
middlesexcounty_obs_year <- meteo_pull_monitors(middlesexcounty_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1931-04-30', date_max = '1932-04-29')
richmond4_obs_year <- meteo_pull_monitors(richmond_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1977-04-02', date_max = '1978-04-02')
richmond5_obs_year <- meteo_pull_monitors(richmond_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1931-04-26', date_max = '1932-04-25')
richmond6_obs_year <- meteo_pull_monitors(richmond_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1934-03-29', date_max = '1935-03-29')
richmond7_obs_year <- meteo_pull_monitors(richmond_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1930-04-23', date_max = '1931-04-23')
richmond7.5_obs_year <- meteo_pull_monitors(richmond_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1931-04-02', date_max = '1932-04-01')
richmond8_obs_year <- meteo_pull_monitors(richmond_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1930-05-20', date_max = '1931-05-20')
suffolkcity1_obs_year <- meteo_pull_monitors(suffolkcity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1978-04-01', date_max = '1979-04-01')
richmond11_obs_year <- meteo_pull_monitors(richmond_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1977-04-16', date_max = '1978-04-16')
hopewell1_obs_year <- meteo_pull_monitors(hopewell_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1977-04-17', date_max = '1978-04-17')
richmond12_obs_year <- meteo_pull_monitors(richmond_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1972-04-13', date_max = '1973-04-13')
richmond13_obs_year <- meteo_pull_monitors(richmond_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1970-04-29', date_max = '1971-04-29')
charoltesville1_obs_year <- meteo_pull_monitors(charoltesville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1970-04-29', date_max = '1971-04-29')
radford1_obs_year <- meteo_pull_monitors(radford_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1980-03-27', date_max = '1981-03-27')
williamsburg_obs_year <- meteo_pull_monitors(williamsburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1998-11-18', date_max = '1999-11-18')
alexandria1_obs_year <- meteo_pull_monitors(alexandria_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1938-04-24', date_max = '1939-04-24')
fredericksburg1_obs_year <- meteo_pull_monitors(fredericksburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1968-04-09', date_max = '1969-04-09')
radford2_obs_year <- meteo_pull_monitors(radford_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1977-05-17', date_max = '1978-05-17')
hopewell2_obs_year <- meteo_pull_monitors(hopewell_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1929-05-09', date_max = '1930-05-09')
williamsburg2_obs_year <- meteo_pull_monitors(williamsburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1966-04-13', date_max = '1967-04-13')
harrisonburg1_obs_year <- meteo_pull_monitors(harrisonburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2001-04-12', date_max = '2002-04-12')
suffolkcity2_obs_year <- meteo_pull_monitors(suffolkcity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2008-03-09', date_max = '2009-03-09')
fredericksburg2_obs_year <- meteo_pull_monitors(fredericksburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1998-05-11', date_max = '1999-05-11')
richmond14_obs_year <- meteo_pull_monitors(richmond_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1969-05-09', date_max = '1970-05-09')
harrisonburg2_obs_year <- meteo_pull_monitors(harrisonburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1961-04-02', date_max = '1962-04-02')
pocomokecity_obs_year <- meteo_pull_monitors(pocomokecity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1937-05-22', date_max = '1938-05-22')
roanokecounty_obs_year <- meteo_pull_monitors(roanokecounty_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1978-06-12', date_max = '1979-06-12')
williamsburg3_obs_year <- meteo_pull_monitors(williamsburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1974-05-26', date_max = '1975-05-26')
winchester1_obs_year <- meteo_pull_monitors(winchester_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1903-05-31', date_max = '1904-05-30')
alexandriacity_obs_year <- meteo_pull_monitors(alexandriacity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1971-04-07', date_max = '1972-04-06')
harrisonburg2_obs_year <- meteo_pull_monitors(harrisonburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1982-03-22', date_max = '1983-03-22')
winchester2_obs_year <- meteo_pull_monitors(winchester_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1979-08-18', date_max = '1980-08-17')
emporia_obs_year <- meteo_pull_monitors(emporia_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1978-03-31', date_max = '1979-03-31')
lynchburg1_obs_year <- meteo_pull_monitors(lynchburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1983-04-11', date_max = '1984-04-10')
norfolk1_obs_year <- meteo_pull_monitors(norfolk_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1974-03-31', date_max = '1975-03-31')
norfolk2_obs_year <- meteo_pull_monitors(norfolk_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1974-03-31', date_max = '1975-03-31')
accomac_obs_year <- meteo_pull_monitors(accomac_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2010-04-03', date_max = '2011-04-03')
norfolk3_obs_year <- meteo_pull_monitors(norfolk_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1968-03-09', date_max = '1969-03-09')
harrisonburg3_obs_year <- meteo_pull_monitors(harrisonburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1983-05-03', date_max = '1984-05-02')
lynchburg2_obs_year <- meteo_pull_monitors(lynchburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1978-04-21', date_max = '1979-04-21')
winchester3_obs_year <- meteo_pull_monitors(winchester_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1970-05-08', date_max = '1971-05-08')
harrisonburg4_obs_year <- meteo_pull_monitors(harrisonburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1979-04-24', date_max = '1980-04-23')
lynchburg3_obs_year <- meteo_pull_monitors(lynchburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1983-04-11', date_max = '1984-04-10')
leesburg_obs_year <- meteo_pull_monitors(leesburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1968-04-24', date_max = '1969-04-24')
lynchburg4_obs_year <- meteo_pull_monitors(lynchburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2010-04-03', date_max = '2011-04-03')
norfolk4_obs_year <- meteo_pull_monitors(norfolk_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1969-04-04', date_max = '1970-04-04')
richmond15_obs_year <- meteo_pull_monitors(richmond_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1978-05-04', date_max = '1979-05-04')
reston_obs_year <- meteo_pull_monitors(reston_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1970-04-13', date_max = '1971-04-13')
harrisonburg5_obs_year <- meteo_pull_monitors(harrisonburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1983-04-23', date_max = '1984-04-22')
norfolk5_obs_year <- meteo_pull_monitors(norfolk_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1969-04-04', date_max = '1970-04-04')
staunton_obs_year <- meteo_pull_monitors(staunton_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1977-08-10', date_max = '1978-08-10')
charoltesville2_obs_year <- meteo_pull_monitors(charoltesville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1977-05-03', date_max = '1978-05-03')
harrisonburg6_obs_year <- meteo_pull_monitors(harrisonburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1975-04-21', date_max = '1976-04-20')
norfolk6_obs_year <- meteo_pull_monitors(norfolk_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1977-03-31', date_max = '1978-03-31')
fredericksburg3_obs_year <- meteo_pull_monitors(fredericksburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1977-04-30', date_max = '1978-04-30')
harrisonburg7_obs_year <- meteo_pull_monitors(harrisonburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2001-04-13', date_max = '2002-04-13')
norfolk7_obs_year <- meteo_pull_monitors(norfolk_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2003-03-26', date_max = '2004-03-25')
winchester4_obs_year <- meteo_pull_monitors(winchester_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2010-05-05', date_max = '2011-05-05')
virginiabeach1_obs_year <- meteo_pull_monitors(virginiabeach_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1977-03-22', date_max = '1978-03-22')
exmore_obs_year <- meteo_pull_monitors(exmore_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1934-10-11', date_max = '1935-10-11')
williamsburg4_obs_year <- meteo_pull_monitors(williamsburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1938-02-20', date_max = '1939-02-20')
harrisonburg8_obs_year <- meteo_pull_monitors(harrisonburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1983-04-26', date_max = '1984-04-25')
newportnews_obs_year <- meteo_pull_monitors(newportnews_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1978-04-08', date_max = '1979-04-08')
franlkin_obs_year <- meteo_pull_monitors(franlkin_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1938-04-06', date_max = '1939-04-06')
charoltesville3_obs_year <- meteo_pull_monitors(charoltesville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1970-05-01', date_max = '1971-05-01')
norfolk8_obs_year <- meteo_pull_monitors(norfolk_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1965-04-01', date_max = '1966-04-01')
hampton_obs_year <- meteo_pull_monitors(hampton_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1886-04-07', date_max = '1887-04-07')
virginiabeach2_obs_year <- meteo_pull_monitors(virginiabeach_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1965-04-23', date_max = '1966-04-23')
harrisonburg9_obs_year <- meteo_pull_monitors(harrisonburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1975-04-08', date_max = '1976-04-07')

## make a dataframe of the mean of tmax, tmin, prcp of the previous year before the date of collection 

lapply(df3, cat(citySwap2(x = city2,  y = "_previousyear", z = " <- data.frame(virginia_csv$id[", l= lennumber, k = "], (mean(", e = "_obs_year$prcp, na.rm=TRUE)), (mean(", f = "_obs_year$tmax, na.rm=TRUE)), (mean(", g = "_obs_year$tmin, na.rm=TRUE))) "), sep = "\n"))

richmond1_previousyear <- data.frame(virginia_csv$id[1], (mean(richmond1_obs_year$prcp, na.rm=TRUE)), (mean(richmond1_obs_year$tmax, na.rm=TRUE)), (mean(richmond1_obs_year$tmin, na.rm=TRUE))) 
norfolk1_previousyear <- data.frame(virginia_csv$id[2], (mean(norfolk1_obs_year$prcp, na.rm=TRUE)), (mean(norfolk1_obs_year$tmax, na.rm=TRUE)), (mean(norfolk1_obs_year$tmin, na.rm=TRUE))) 
richmond2_previousyear <- data.frame(virginia_csv$id[3], (mean(richmond2_obs_year$prcp, na.rm=TRUE)), (mean(richmond2_obs_year$tmax, na.rm=TRUE)), (mean(richmond2_obs_year$tmin, na.rm=TRUE))) 
richmond3_previousyear <- data.frame(virginia_csv$id[4], (mean(richmond3_obs_year$prcp, na.rm=TRUE)), (mean(richmond3_obs_year$tmax, na.rm=TRUE)), (mean(richmond3_obs_year$tmin, na.rm=TRUE))) 
middlesexcounty_previousyear <- data.frame(virginia_csv$id[5], (mean(middlesexcounty_obs_year$prcp, na.rm=TRUE)), (mean(middlesexcounty_obs_year$tmax, na.rm=TRUE)), (mean(middlesexcounty_obs_year$tmin, na.rm=TRUE))) 
richmond4_previousyear <- data.frame(virginia_csv$id[6], (mean(richmond4_obs_year$prcp, na.rm=TRUE)), (mean(richmond4_obs_year$tmax, na.rm=TRUE)), (mean(richmond4_obs_year$tmin, na.rm=TRUE))) 
richmond5_previousyear <- data.frame(virginia_csv$id[7], (mean(richmond5_obs_year$prcp, na.rm=TRUE)), (mean(richmond5_obs_year$tmax, na.rm=TRUE)), (mean(richmond5_obs_year$tmin, na.rm=TRUE))) 
richmond6_previousyear <- data.frame(virginia_csv$id[8], (mean(richmond6_obs_year$prcp, na.rm=TRUE)), (mean(richmond6_obs_year$tmax, na.rm=TRUE)), (mean(richmond6_obs_year$tmin, na.rm=TRUE))) 
richmond7_previousyear <- data.frame(virginia_csv$id[9], (mean(richmond7_obs_year$prcp, na.rm=TRUE)), (mean(richmond7_obs_year$tmax, na.rm=TRUE)), (mean(richmond7_obs_year$tmin, na.rm=TRUE))) 
richmond7.5_previousyear <- data.frame(virginia_csv$id[10], (mean(richmond7.5_obs_year$prcp, na.rm=TRUE)), (mean(richmond7.5_obs_year$tmax, na.rm=TRUE)), (mean(richmond7.5_obs_year$tmin, na.rm=TRUE))) 
richmond8_previousyear <- data.frame(virginia_csv$id[11], (mean(richmond8_obs_year$prcp, na.rm=TRUE)), (mean(richmond8_obs_year$tmax, na.rm=TRUE)), (mean(richmond8_obs_year$tmin, na.rm=TRUE))) 
suffolkcity1_previousyear <- data.frame(virginia_csv$id[14], (mean(suffolkcity1_obs_year$prcp, na.rm=TRUE)), (mean(suffolkcity1_obs_year$tmax, na.rm=TRUE)), (mean(suffolkcity1_obs_year$tmin, na.rm=TRUE))) 
richmond11_previousyear <- data.frame(virginia_csv$id[15], (mean(richmond11_obs_year$prcp, na.rm=TRUE)), (mean(richmond11_obs_year$tmax, na.rm=TRUE)), (mean(richmond11_obs_year$tmin, na.rm=TRUE))) 
hopewell1_previousyear <- data.frame(virginia_csv$id[16], (mean(hopewell1_obs_year$prcp, na.rm=TRUE)), (mean(hopewell1_obs_year$tmax, na.rm=TRUE)), (mean(hopewell1_obs_year$tmin, na.rm=TRUE))) 
richmond12_previousyear <- data.frame(virginia_csv$id[17], (mean(richmond12_obs_year$prcp, na.rm=TRUE)), (mean(richmond12_obs_year$tmax, na.rm=TRUE)), (mean(richmond12_obs_year$tmin, na.rm=TRUE))) 
richmond13_previousyear <- data.frame(virginia_csv$id[18], (mean(richmond13_obs_year$prcp, na.rm=TRUE)), (mean(richmond13_obs_year$tmax, na.rm=TRUE)), (mean(richmond13_obs_year$tmin, na.rm=TRUE))) 
charoltesville1_previousyear <- data.frame(virginia_csv$id[19], (mean(charoltesville1_obs_year$prcp, na.rm=TRUE)), (mean(charoltesville1_obs_year$tmax, na.rm=TRUE)), (mean(charoltesville1_obs_year$tmin, na.rm=TRUE))) 
radford1_previousyear <- data.frame(virginia_csv$id[20], (mean(radford1_obs_year$prcp, na.rm=TRUE)), (mean(radford1_obs_year$tmax, na.rm=TRUE)), (mean(radford1_obs_year$tmin, na.rm=TRUE))) 
williamsburg_previousyear <- data.frame(virginia_csv$id[21], (mean(williamsburg_obs_year$prcp, na.rm=TRUE)), (mean(williamsburg_obs_year$tmax, na.rm=TRUE)), (mean(williamsburg_obs_year$tmin, na.rm=TRUE))) 
alexandria1_previousyear <- data.frame(virginia_csv$id[22], (mean(alexandria1_obs_year$prcp, na.rm=TRUE)), (mean(alexandria1_obs_year$tmax, na.rm=TRUE)), (mean(alexandria1_obs_year$tmin, na.rm=TRUE))) 
fredericksburg1_previousyear <- data.frame(virginia_csv$id[23], (mean(fredericksburg1_obs_year$prcp, na.rm=TRUE)), (mean(fredericksburg1_obs_year$tmax, na.rm=TRUE)), (mean(fredericksburg1_obs_year$tmin, na.rm=TRUE))) 
radford2_previousyear <- data.frame(virginia_csv$id[24], (mean(radford2_obs_year$prcp, na.rm=TRUE)), (mean(radford2_obs_year$tmax, na.rm=TRUE)), (mean(radford2_obs_year$tmin, na.rm=TRUE))) 
hopewell2_previousyear <- data.frame(virginia_csv$id[26], (mean(hopewell2_obs_year$prcp, na.rm=TRUE)), (mean(hopewell2_obs_year$tmax, na.rm=TRUE)), (mean(hopewell2_obs_year$tmin, na.rm=TRUE))) 
williamsburg2_previousyear <- data.frame(virginia_csv$id[27], (mean(williamsburg2_obs_year$prcp, na.rm=TRUE)), (mean(williamsburg2_obs_year$tmax, na.rm=TRUE)), (mean(williamsburg2_obs_year$tmin, na.rm=TRUE))) 
harrisonburg1_previousyear <- data.frame(virginia_csv$id[28], (mean(harrisonburg1_obs_year$prcp, na.rm=TRUE)), (mean(harrisonburg1_obs_year$tmax, na.rm=TRUE)), (mean(harrisonburg1_obs_year$tmin, na.rm=TRUE))) 
suffolkcity2_previousyear <- data.frame(virginia_csv$id[29], (mean(suffolkcity2_obs_year$prcp, na.rm=TRUE)), (mean(suffolkcity2_obs_year$tmax, na.rm=TRUE)), (mean(suffolkcity2_obs_year$tmin, na.rm=TRUE))) 
fredericksburg2_previousyear <- data.frame(virginia_csv$id[30], (mean(fredericksburg2_obs_year$prcp, na.rm=TRUE)), (mean(fredericksburg2_obs_year$tmax, na.rm=TRUE)), (mean(fredericksburg2_obs_year$tmin, na.rm=TRUE))) 
richmond14_previousyear <- data.frame(virginia_csv$id[31], (mean(richmond14_obs_year$prcp, na.rm=TRUE)), (mean(richmond14_obs_year$tmax, na.rm=TRUE)), (mean(richmond14_obs_year$tmin, na.rm=TRUE))) 
harrisonburg2_previousyear <- data.frame(virginia_csv$id[32], (mean(harrisonburg2_obs_year$prcp, na.rm=TRUE)), (mean(harrisonburg2_obs_year$tmax, na.rm=TRUE)), (mean(harrisonburg2_obs_year$tmin, na.rm=TRUE))) 
pocomokecity_previousyear <- data.frame(virginia_csv$id[33], (mean(pocomokecity_obs_year$prcp, na.rm=TRUE)), (mean(pocomokecity_obs_year$tmax, na.rm=TRUE)), (mean(pocomokecity_obs_year$tmin, na.rm=TRUE))) 
roanokecounty_previousyear <- data.frame(virginia_csv$id[34], (mean(roanokecounty_obs_year$prcp, na.rm=TRUE)), (mean(roanokecounty_obs_year$tmax, na.rm=TRUE)), (mean(roanokecounty_obs_year$tmin, na.rm=TRUE))) 
williamsburg3_previousyear <- data.frame(virginia_csv$id[35], (mean(williamsburg3_obs_year$prcp, na.rm=TRUE)), (mean(williamsburg3_obs_year$tmax, na.rm=TRUE)), (mean(williamsburg3_obs_year$tmin, na.rm=TRUE))) 
winchester1_previousyear <- data.frame(virginia_csv$id[36], (mean(winchester1_obs_year$prcp, na.rm=TRUE)), (mean(winchester1_obs_year$tmax, na.rm=TRUE)), (mean(winchester1_obs_year$tmin, na.rm=TRUE))) 
alexandriacity_previousyear <- data.frame(virginia_csv$id[37], (mean(alexandriacity_obs_year$prcp, na.rm=TRUE)), (mean(alexandriacity_obs_year$tmax, na.rm=TRUE)), (mean(alexandriacity_obs_year$tmin, na.rm=TRUE))) 
harrisonburg2_previousyear <- data.frame(virginia_csv$id[38], (mean(harrisonburg2_obs_year$prcp, na.rm=TRUE)), (mean(harrisonburg2_obs_year$tmax, na.rm=TRUE)), (mean(harrisonburg2_obs_year$tmin, na.rm=TRUE))) 
winchester2_previousyear <- data.frame(virginia_csv$id[39], (mean(winchester2_obs_year$prcp, na.rm=TRUE)), (mean(winchester2_obs_year$tmax, na.rm=TRUE)), (mean(winchester2_obs_year$tmin, na.rm=TRUE))) 
emporia_previousyear <- data.frame(virginia_csv$id[40], (mean(emporia_obs_year$prcp, na.rm=TRUE)), (mean(emporia_obs_year$tmax, na.rm=TRUE)), (mean(emporia_obs_year$tmin, na.rm=TRUE))) 
lynchburg1_previousyear <- data.frame(virginia_csv$id[41], (mean(lynchburg1_obs_year$prcp, na.rm=TRUE)), (mean(lynchburg1_obs_year$tmax, na.rm=TRUE)), (mean(lynchburg1_obs_year$tmin, na.rm=TRUE))) 
norfolk1_previousyear <- data.frame(virginia_csv$id[42], (mean(norfolk1_obs_year$prcp, na.rm=TRUE)), (mean(norfolk1_obs_year$tmax, na.rm=TRUE)), (mean(norfolk1_obs_year$tmin, na.rm=TRUE))) 
norfolk2_previousyear <- data.frame(virginia_csv$id[43], (mean(norfolk2_obs_year$prcp, na.rm=TRUE)), (mean(norfolk2_obs_year$tmax, na.rm=TRUE)), (mean(norfolk2_obs_year$tmin, na.rm=TRUE))) 
accomac_previousyear <- data.frame(virginia_csv$id[44], (mean(accomac_obs_year$prcp, na.rm=TRUE)), (mean(accomac_obs_year$tmax, na.rm=TRUE)), (mean(accomac_obs_year$tmin, na.rm=TRUE))) 
norfolk3_previousyear <- data.frame(virginia_csv$id[45], (mean(norfolk3_obs_year$prcp, na.rm=TRUE)), (mean(norfolk3_obs_year$tmax, na.rm=TRUE)), (mean(norfolk3_obs_year$tmin, na.rm=TRUE))) 
harrisonburg3_previousyear <- data.frame(virginia_csv$id[46], (mean(harrisonburg3_obs_year$prcp, na.rm=TRUE)), (mean(harrisonburg3_obs_year$tmax, na.rm=TRUE)), (mean(harrisonburg3_obs_year$tmin, na.rm=TRUE))) 
lynchburg2_previousyear <- data.frame(virginia_csv$id[47], (mean(lynchburg2_obs_year$prcp, na.rm=TRUE)), (mean(lynchburg2_obs_year$tmax, na.rm=TRUE)), (mean(lynchburg2_obs_year$tmin, na.rm=TRUE))) 
winchester3_previousyear <- data.frame(virginia_csv$id[48], (mean(winchester3_obs_year$prcp, na.rm=TRUE)), (mean(winchester3_obs_year$tmax, na.rm=TRUE)), (mean(winchester3_obs_year$tmin, na.rm=TRUE))) 
harrisonburg4_previousyear <- data.frame(virginia_csv$id[49], (mean(harrisonburg4_obs_year$prcp, na.rm=TRUE)), (mean(harrisonburg4_obs_year$tmax, na.rm=TRUE)), (mean(harrisonburg4_obs_year$tmin, na.rm=TRUE))) 
lynchburg3_previousyear <- data.frame(virginia_csv$id[50], (mean(lynchburg3_obs_year$prcp, na.rm=TRUE)), (mean(lynchburg3_obs_year$tmax, na.rm=TRUE)), (mean(lynchburg3_obs_year$tmin, na.rm=TRUE))) 
leesburg_previousyear <- data.frame(virginia_csv$id[51], (mean(leesburg_obs_year$prcp, na.rm=TRUE)), (mean(leesburg_obs_year$tmax, na.rm=TRUE)), (mean(leesburg_obs_year$tmin, na.rm=TRUE))) 
lynchburg4_previousyear <- data.frame(virginia_csv$id[52], (mean(lynchburg4_obs_year$prcp, na.rm=TRUE)), (mean(lynchburg4_obs_year$tmax, na.rm=TRUE)), (mean(lynchburg4_obs_year$tmin, na.rm=TRUE))) 
norfolk4_previousyear <- data.frame(virginia_csv$id[53], (mean(norfolk4_obs_year$prcp, na.rm=TRUE)), (mean(norfolk4_obs_year$tmax, na.rm=TRUE)), (mean(norfolk4_obs_year$tmin, na.rm=TRUE))) 
richmond15_previousyear <- data.frame(virginia_csv$id[54], (mean(richmond15_obs_year$prcp, na.rm=TRUE)), (mean(richmond15_obs_year$tmax, na.rm=TRUE)), (mean(richmond15_obs_year$tmin, na.rm=TRUE))) 
reston_previousyear <- data.frame(virginia_csv$id[55], (mean(reston_obs_year$prcp, na.rm=TRUE)), (mean(reston_obs_year$tmax, na.rm=TRUE)), (mean(reston_obs_year$tmin, na.rm=TRUE))) 
harrisonburg5_previousyear <- data.frame(virginia_csv$id[56], (mean(harrisonburg5_obs_year$prcp, na.rm=TRUE)), (mean(harrisonburg5_obs_year$tmax, na.rm=TRUE)), (mean(harrisonburg5_obs_year$tmin, na.rm=TRUE))) 
norfolk5_previousyear <- data.frame(virginia_csv$id[57], (mean(norfolk5_obs_year$prcp, na.rm=TRUE)), (mean(norfolk5_obs_year$tmax, na.rm=TRUE)), (mean(norfolk5_obs_year$tmin, na.rm=TRUE))) 
staunton_previousyear <- data.frame(virginia_csv$id[58], (mean(staunton_obs_year$prcp, na.rm=TRUE)), (mean(staunton_obs_year$tmax, na.rm=TRUE)), (mean(staunton_obs_year$tmin, na.rm=TRUE))) 
charoltesville2_previousyear <- data.frame(virginia_csv$id[59], (mean(charoltesville2_obs_year$prcp, na.rm=TRUE)), (mean(charoltesville2_obs_year$tmax, na.rm=TRUE)), (mean(charoltesville2_obs_year$tmin, na.rm=TRUE))) 
harrisonburg6_previousyear <- data.frame(virginia_csv$id[60], (mean(harrisonburg6_obs_year$prcp, na.rm=TRUE)), (mean(harrisonburg6_obs_year$tmax, na.rm=TRUE)), (mean(harrisonburg6_obs_year$tmin, na.rm=TRUE))) 
norfolk6_previousyear <- data.frame(virginia_csv$id[61], (mean(norfolk6_obs_year$prcp, na.rm=TRUE)), (mean(norfolk6_obs_year$tmax, na.rm=TRUE)), (mean(norfolk6_obs_year$tmin, na.rm=TRUE))) 
fredericksburg3_previousyear <- data.frame(virginia_csv$id[62], (mean(fredericksburg3_obs_year$prcp, na.rm=TRUE)), (mean(fredericksburg3_obs_year$tmax, na.rm=TRUE)), (mean(fredericksburg3_obs_year$tmin, na.rm=TRUE))) 
harrisonburg7_previousyear <- data.frame(virginia_csv$id[63], (mean(harrisonburg7_obs_year$prcp, na.rm=TRUE)), (mean(harrisonburg7_obs_year$tmax, na.rm=TRUE)), (mean(harrisonburg7_obs_year$tmin, na.rm=TRUE))) 
norfolk7_previousyear <- data.frame(virginia_csv$id[64], (mean(norfolk7_obs_year$prcp, na.rm=TRUE)), (mean(norfolk7_obs_year$tmax, na.rm=TRUE)), (mean(norfolk7_obs_year$tmin, na.rm=TRUE))) 
winchester4_previousyear <- data.frame(virginia_csv$id[65], (mean(winchester4_obs_year$prcp, na.rm=TRUE)), (mean(winchester4_obs_year$tmax, na.rm=TRUE)), (mean(winchester4_obs_year$tmin, na.rm=TRUE))) 
virginiabeach1_previousyear <- data.frame(virginia_csv$id[66], (mean(virginiabeach1_obs_year$prcp, na.rm=TRUE)), (mean(virginiabeach1_obs_year$tmax, na.rm=TRUE)), (mean(virginiabeach1_obs_year$tmin, na.rm=TRUE))) 
exmore_previousyear <- data.frame(virginia_csv$id[67], (mean(exmore_obs_year$prcp, na.rm=TRUE)), (mean(exmore_obs_year$tmax, na.rm=TRUE)), (mean(exmore_obs_year$tmin, na.rm=TRUE))) 
williamsburg4_previousyear <- data.frame(virginia_csv$id[68], (mean(williamsburg4_obs_year$prcp, na.rm=TRUE)), (mean(williamsburg4_obs_year$tmax, na.rm=TRUE)), (mean(williamsburg4_obs_year$tmin, na.rm=TRUE))) 
harrisonburg8_previousyear <- data.frame(virginia_csv$id[69], (mean(harrisonburg8_obs_year$prcp, na.rm=TRUE)), (mean(harrisonburg8_obs_year$tmax, na.rm=TRUE)), (mean(harrisonburg8_obs_year$tmin, na.rm=TRUE))) 
newportnews_previousyear <- data.frame(virginia_csv$id[70], (mean(newportnews_obs_year$prcp, na.rm=TRUE)), (mean(newportnews_obs_year$tmax, na.rm=TRUE)), (mean(newportnews_obs_year$tmin, na.rm=TRUE))) 
franlkin_previousyear <- data.frame(virginia_csv$id[71], (mean(franlkin_obs_year$prcp, na.rm=TRUE)), (mean(franlkin_obs_year$tmax, na.rm=TRUE)), (mean(franlkin_obs_year$tmin, na.rm=TRUE))) 
charoltesville3_previousyear <- data.frame(virginia_csv$id[72], (mean(charoltesville3_obs_year$prcp, na.rm=TRUE)), (mean(charoltesville3_obs_year$tmax, na.rm=TRUE)), (mean(charoltesville3_obs_year$tmin, na.rm=TRUE))) 
norfolk8_previousyear <- data.frame(virginia_csv$id[73], (mean(norfolk8_obs_year$prcp, na.rm=TRUE)), (mean(norfolk8_obs_year$tmax, na.rm=TRUE)), (mean(norfolk8_obs_year$tmin, na.rm=TRUE))) 
hampton_previousyear <- data.frame(virginia_csv$id[74], (mean(hampton_obs_year$prcp, na.rm=TRUE)), (mean(hampton_obs_year$tmax, na.rm=TRUE)), (mean(hampton_obs_year$tmin, na.rm=TRUE))) 
virginiabeach2_previousyear <- data.frame(virginia_csv$id[75], (mean(virginiabeach2_obs_year$prcp, na.rm=TRUE)), (mean(virginiabeach2_obs_year$tmax, na.rm=TRUE)), (mean(virginiabeach2_obs_year$tmin, na.rm=TRUE))) 
harrisonburg9_previousyear <- data.frame(virginia_csv$id[76], (mean(harrisonburg9_obs_year$prcp, na.rm=TRUE)), (mean(harrisonburg9_obs_year$tmax, na.rm=TRUE)), (mean(harrisonburg9_obs_year$tmin, na.rm=TRUE)))

#rename col.names 

#cat(citySwap3(city2, y = "colnames(", z = "_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')"), sep = "\n")
colnames(richmond1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(norfolk1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(richmond2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(richmond3_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(middlesexcounty_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(richmond4_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(richmond5_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(richmond6_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(richmond7_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(richmond7.5_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(richmond8_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(suffolkcity1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(richmond11_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(hopewell1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(richmond12_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(richmond13_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(charoltesville1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(radford1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(williamsburg_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(alexandria1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(fredericksburg1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(radford2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(hopewell2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(williamsburg2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(harrisonburg1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(suffolkcity2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(fredericksburg2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(richmond14_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(harrisonburg2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(pocomokecity_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(roanokecounty_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(williamsburg3_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(winchester1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(alexandriacity_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(harrisonburg2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(winchester2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(emporia_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(lynchburg1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(norfolk1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(norfolk2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(accomac_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(norfolk3_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(harrisonburg3_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(lynchburg2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(winchester3_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(harrisonburg4_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(lynchburg3_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(leesburg_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(lynchburg4_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(norfolk4_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(richmond15_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(reston_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(harrisonburg5_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(norfolk5_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(staunton_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(charoltesville2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(harrisonburg6_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(norfolk6_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(fredericksburg3_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(harrisonburg7_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(norfolk7_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(winchester4_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(virginiabeach1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(exmore_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(williamsburg4_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(harrisonburg8_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(newportnews_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(franlkin_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(charoltesville3_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(norfolk8_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(hampton_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(virginiabeach2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(harrisonburg9_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
## combine previous year before the date of collection data using bind_rows

#at(citySwap7(city2, y = "_previousyear", z = ","), sep = "")
virginia_Pyear <- bind_rows(richmond1_previousyear,norfolk1_previousyear,richmond2_previousyear,richmond3_previousyear,middlesexcounty_previousyear,richmond4_previousyear,richmond5_previousyear,richmond6_previousyear,richmond7_previousyear,richmond7.5_previousyear,richmond8_previousyear,suffolkcity1_previousyear,richmond11_previousyear,hopewell1_previousyear,richmond12_previousyear,richmond13_previousyear,charoltesville1_previousyear,radford1_previousyear,williamsburg_previousyear,alexandria1_previousyear,fredericksburg1_previousyear,radford2_previousyear,hopewell2_previousyear,williamsburg2_previousyear,harrisonburg1_previousyear,suffolkcity2_previousyear,fredericksburg2_previousyear,richmond14_previousyear,harrisonburg2_previousyear,pocomokecity_previousyear,roanokecounty_previousyear,williamsburg3_previousyear,winchester1_previousyear,alexandriacity_previousyear,harrisonburg2_previousyear,winchester2_previousyear,emporia_previousyear,lynchburg1_previousyear,norfolk1_previousyear,norfolk2_previousyear,accomac_previousyear,norfolk3_previousyear,harrisonburg3_previousyear,lynchburg2_previousyear,winchester3_previousyear,harrisonburg4_previousyear,lynchburg3_previousyear,leesburg_previousyear,lynchburg4_previousyear,norfolk4_previousyear,richmond15_previousyear,reston_previousyear,harrisonburg5_previousyear,norfolk5_previousyear,staunton_previousyear,charoltesville2_previousyear,harrisonburg6_previousyear,norfolk6_previousyear,fredericksburg3_previousyear,harrisonburg7_previousyear,norfolk7_previousyear,winchester4_previousyear,virginiabeach1_previousyear,exmore_previousyear,williamsburg4_previousyear,harrisonburg8_previousyear,newportnews_previousyear,franlkin_previousyear,charoltesville3_previousyear,norfolk8_previousyear,hampton_previousyear,virginiabeach2_previousyear,harrisonburg9_previousyear)

# save data
write.xlsx(virginia_Pyear, file = "virginia_previous_year_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

## pull data for the current year of the date of collection
#years <- virginia_csv$year
#df5 <- list(city, city2, years)
lapply(df5, cat(citySwap6(x = city2, y = "_obs_yearDOC <- meteo_pull_monitors(", j = city, 
                               z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '", l = years , 
                               k = "-01-01', date_max = '",  p ="-12-31')"), sep = "\n"))
richmond1_obs_yearDOC <- meteo_pull_monitors(richmond_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1970-01-01', date_max = '1970-12-31')
norfolk1_obs_yearDOC <- meteo_pull_monitors(norfolk_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1975-01-01', date_max = '1975-12-31')
richmond2_obs_yearDOC <- meteo_pull_monitors(richmond_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1976-01-01', date_max = '1976-12-31')
richmond3_obs_yearDOC <- meteo_pull_monitors(richmond_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1942-01-01', date_max = '1942-12-31')
middlesexcounty_obs_yearDOC <- meteo_pull_monitors(middlesexcounty_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1932-01-01', date_max = '1932-12-31')
richmond4_obs_yearDOC <- meteo_pull_monitors(richmond_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1978-01-01', date_max = '1978-12-31')
richmond5_obs_yearDOC <- meteo_pull_monitors(richmond_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1932-01-01', date_max = '1932-12-31')
richmond6_obs_yearDOC <- meteo_pull_monitors(richmond_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1935-01-01', date_max = '1935-12-31')
richmond7_obs_yearDOC <- meteo_pull_monitors(richmond_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1931-01-01', date_max = '1931-12-31')
richmond7.5_obs_yearDOC <- meteo_pull_monitors(richmond_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1932-01-01', date_max = '1932-12-31')
richmond8_obs_yearDOC <- meteo_pull_monitors(richmond_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1931-01-01', date_max = '1931-12-31')
suffolkcity1_obs_yearDOC <- meteo_pull_monitors(suffolkcity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1979-01-01', date_max = '1979-12-31')
richmond11_obs_yearDOC <- meteo_pull_monitors(richmond_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1978-01-01', date_max = '1978-12-31')
hopewell1_obs_yearDOC <- meteo_pull_monitors(hopewell_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1978-01-01', date_max = '1978-12-31')
richmond12_obs_yearDOC <- meteo_pull_monitors(richmond_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1973-01-01', date_max = '1973-12-31')
richmond13_obs_yearDOC <- meteo_pull_monitors(richmond_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1971-01-01', date_max = '1971-12-31')
charoltesville1_obs_yearDOC <- meteo_pull_monitors(charoltesville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1971-01-01', date_max = '1971-12-31')
radford1_obs_yearDOC <- meteo_pull_monitors(radford_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1981-01-01', date_max = '1981-12-31')
williamsburg_obs_yearDOC <- meteo_pull_monitors(williamsburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1999-01-01', date_max = '1999-12-31')
alexandria1_obs_yearDOC <- meteo_pull_monitors(alexandria_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1939-01-01', date_max = '1939-12-31')
fredericksburg1_obs_yearDOC <- meteo_pull_monitors(fredericksburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1969-01-01', date_max = '1969-12-31')
radford2_obs_yearDOC <- meteo_pull_monitors(radford_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1978-01-01', date_max = '1978-12-31')
hopewell2_obs_yearDOC <- meteo_pull_monitors(hopewell_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1930-01-01', date_max = '1930-12-31')
williamsburg2_obs_yearDOC <- meteo_pull_monitors(williamsburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1967-01-01', date_max = '1967-12-31')
harrisonburg1_obs_yearDOC <- meteo_pull_monitors(harrisonburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2002-01-01', date_max = '2002-12-31')
suffolkcity2_obs_yearDOC <- meteo_pull_monitors(suffolkcity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2009-01-01', date_max = '2009-12-31')
fredericksburg2_obs_yearDOC <- meteo_pull_monitors(fredericksburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1999-01-01', date_max = '1999-12-31')
richmond14_obs_yearDOC <- meteo_pull_monitors(richmond_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1970-01-01', date_max = '1970-12-31')
harrisonburg2_obs_yearDOC <- meteo_pull_monitors(harrisonburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1962-01-01', date_max = '1962-12-31')
pocomokecity_obs_yearDOC <- meteo_pull_monitors(pocomokecity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1938-01-01', date_max = '1938-12-31')
roanokecounty_obs_yearDOC <- meteo_pull_monitors(roanokecounty_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1979-01-01', date_max = '1979-12-31')
williamsburg3_obs_yearDOC <- meteo_pull_monitors(williamsburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1975-01-01', date_max = '1975-12-31')
winchester1_obs_yearDOC <- meteo_pull_monitors(winchester_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1904-01-01', date_max = '1904-12-31')
alexandriacity_obs_yearDOC <- meteo_pull_monitors(alexandriacity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1972-01-01', date_max = '1972-12-31')
harrisonburg2_obs_yearDOC <- meteo_pull_monitors(harrisonburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1983-01-01', date_max = '1983-12-31')
winchester2_obs_yearDOC <- meteo_pull_monitors(winchester_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1980-01-01', date_max = '1980-12-31')
emporia_obs_yearDOC <- meteo_pull_monitors(emporia_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1979-01-01', date_max = '1979-12-31')
lynchburg1_obs_yearDOC <- meteo_pull_monitors(lynchburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1984-01-01', date_max = '1984-12-31')
norfolk1_obs_yearDOC <- meteo_pull_monitors(norfolk_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1975-01-01', date_max = '1975-12-31')
norfolk2_obs_yearDOC <- meteo_pull_monitors(norfolk_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1975-01-01', date_max = '1975-12-31')
accomac_obs_yearDOC <- meteo_pull_monitors(accomac_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2011-01-01', date_max = '2011-12-31')
norfolk3_obs_yearDOC <- meteo_pull_monitors(norfolk_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1969-01-01', date_max = '1969-12-31')
harrisonburg3_obs_yearDOC <- meteo_pull_monitors(harrisonburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1984-01-01', date_max = '1984-12-31')
lynchburg2_obs_yearDOC <- meteo_pull_monitors(lynchburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1979-01-01', date_max = '1979-12-31')
winchester3_obs_yearDOC <- meteo_pull_monitors(winchester_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1971-01-01', date_max = '1971-12-31')
harrisonburg4_obs_yearDOC <- meteo_pull_monitors(harrisonburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1980-01-01', date_max = '1980-12-31')
lynchburg3_obs_yearDOC <- meteo_pull_monitors(lynchburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1984-01-01', date_max = '1984-12-31')
leesburg_obs_yearDOC <- meteo_pull_monitors(leesburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1969-01-01', date_max = '1969-12-31')
lynchburg4_obs_yearDOC <- meteo_pull_monitors(lynchburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2011-01-01', date_max = '2011-12-31')
norfolk4_obs_yearDOC <- meteo_pull_monitors(norfolk_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1970-01-01', date_max = '1970-12-31')
richmond15_obs_yearDOC <- meteo_pull_monitors(richmond_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1979-01-01', date_max = '1979-12-31')
reston_obs_yearDOC <- meteo_pull_monitors(reston_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1971-01-01', date_max = '1971-12-31')
harrisonburg5_obs_yearDOC <- meteo_pull_monitors(harrisonburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1984-01-01', date_max = '1984-12-31')
norfolk5_obs_yearDOC <- meteo_pull_monitors(norfolk_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1970-01-01', date_max = '1970-12-31')
staunton_obs_yearDOC <- meteo_pull_monitors(staunton_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1978-01-01', date_max = '1978-12-31')
charoltesville2_obs_yearDOC <- meteo_pull_monitors(charoltesville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1978-01-01', date_max = '1978-12-31')
harrisonburg6_obs_yearDOC <- meteo_pull_monitors(harrisonburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1976-01-01', date_max = '1976-12-31')
norfolk6_obs_yearDOC <- meteo_pull_monitors(norfolk_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1978-01-01', date_max = '1978-12-31')
fredericksburg3_obs_yearDOC <- meteo_pull_monitors(fredericksburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1978-01-01', date_max = '1978-12-31')
harrisonburg7_obs_yearDOC <- meteo_pull_monitors(harrisonburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2002-01-01', date_max = '2002-12-31')
norfolk7_obs_yearDOC <- meteo_pull_monitors(norfolk_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2004-01-01', date_max = '2004-12-31')
winchester4_obs_yearDOC <- meteo_pull_monitors(winchester_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '2011-01-01', date_max = '2011-12-31')
virginiabeach1_obs_yearDOC <- meteo_pull_monitors(virginiabeach_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1978-01-01', date_max = '1978-12-31')
exmore_obs_yearDOC <- meteo_pull_monitors(exmore_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1935-01-01', date_max = '1935-12-31')
williamsburg4_obs_yearDOC <- meteo_pull_monitors(williamsburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1939-01-01', date_max = '1939-12-31')
harrisonburg8_obs_yearDOC <- meteo_pull_monitors(harrisonburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1984-01-01', date_max = '1984-12-31')
newportnews_obs_yearDOC <- meteo_pull_monitors(newportnews_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1979-01-01', date_max = '1979-12-31')
franlkin_obs_yearDOC <- meteo_pull_monitors(franlkin_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1939-01-01', date_max = '1939-12-31')
charoltesville3_obs_yearDOC <- meteo_pull_monitors(charoltesville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1971-01-01', date_max = '1971-12-31')
norfolk8_obs_yearDOC <- meteo_pull_monitors(norfolk_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1966-01-01', date_max = '1966-12-31')
hampton_obs_yearDOC <- meteo_pull_monitors(hampton_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1887-01-01', date_max = '1887-12-31')
virginiabeach2_obs_yearDOC <- meteo_pull_monitors(virginiabeach_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1966-01-01', date_max = '1966-12-31')
harrisonburg9_obs_yearDOC <- meteo_pull_monitors(harrisonburg_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1976-01-01', date_max = '1976-12-31')

## make a dataframe of the mean of tmax, tmin, current year of the date of collection
lapply(df3, cat(citySwap2(x = city2,  y = "_DOC", z = " <- data.frame(virginia_csv$id[", l= lennumber, k = "], (mean(", e = "_obs_yearDOC$prcp, na.rm=TRUE)), (mean(", f = "_obs_yearDOC$tmax, na.rm=TRUE)), (mean(", g = "_obs_yearDOC$tmin, na.rm=TRUE))) "), sep = "\n"))

richmond1_DOC <- data.frame(virginia_csv$id[1], (mean(richmond1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(richmond1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(richmond1_obs_yearDOC$tmin, na.rm=TRUE))) 
norfolk1_DOC <- data.frame(virginia_csv$id[2], (mean(norfolk1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(norfolk1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(norfolk1_obs_yearDOC$tmin, na.rm=TRUE))) 
richmond2_DOC <- data.frame(virginia_csv$id[3], (mean(richmond2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(richmond2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(richmond2_obs_yearDOC$tmin, na.rm=TRUE))) 
richmond3_DOC <- data.frame(virginia_csv$id[4], (mean(richmond3_obs_yearDOC$prcp, na.rm=TRUE)), (mean(richmond3_obs_yearDOC$tmax, na.rm=TRUE)), (mean(richmond3_obs_yearDOC$tmin, na.rm=TRUE))) 
middlesexcounty_DOC <- data.frame(virginia_csv$id[5], (mean(middlesexcounty_obs_yearDOC$prcp, na.rm=TRUE)), (mean(middlesexcounty_obs_yearDOC$tmax, na.rm=TRUE)), (mean(middlesexcounty_obs_yearDOC$tmin, na.rm=TRUE))) 
richmond4_DOC <- data.frame(virginia_csv$id[6], (mean(richmond4_obs_yearDOC$prcp, na.rm=TRUE)), (mean(richmond4_obs_yearDOC$tmax, na.rm=TRUE)), (mean(richmond4_obs_yearDOC$tmin, na.rm=TRUE))) 
richmond5_DOC <- data.frame(virginia_csv$id[7], (mean(richmond5_obs_yearDOC$prcp, na.rm=TRUE)), (mean(richmond5_obs_yearDOC$tmax, na.rm=TRUE)), (mean(richmond5_obs_yearDOC$tmin, na.rm=TRUE))) 
richmond6_DOC <- data.frame(virginia_csv$id[8], (mean(richmond6_obs_yearDOC$prcp, na.rm=TRUE)), (mean(richmond6_obs_yearDOC$tmax, na.rm=TRUE)), (mean(richmond6_obs_yearDOC$tmin, na.rm=TRUE))) 
richmond7_DOC <- data.frame(virginia_csv$id[9], (mean(richmond7_obs_yearDOC$prcp, na.rm=TRUE)), (mean(richmond7_obs_yearDOC$tmax, na.rm=TRUE)), (mean(richmond7_obs_yearDOC$tmin, na.rm=TRUE))) 
richmond7.5_DOC <- data.frame(virginia_csv$id[10], (mean(richmond7.5_obs_yearDOC$prcp, na.rm=TRUE)), (mean(richmond7.5_obs_yearDOC$tmax, na.rm=TRUE)), (mean(richmond7.5_obs_yearDOC$tmin, na.rm=TRUE))) 
richmond8_DOC <- data.frame(virginia_csv$id[11], (mean(richmond8_obs_yearDOC$prcp, na.rm=TRUE)), (mean(richmond8_obs_yearDOC$tmax, na.rm=TRUE)), (mean(richmond8_obs_yearDOC$tmin, na.rm=TRUE))) 
suffolkcity1_DOC <- data.frame(virginia_csv$id[14], (mean(suffolkcity1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(suffolkcity1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(suffolkcity1_obs_yearDOC$tmin, na.rm=TRUE))) 
richmond11_DOC <- data.frame(virginia_csv$id[15], (mean(richmond11_obs_yearDOC$prcp, na.rm=TRUE)), (mean(richmond11_obs_yearDOC$tmax, na.rm=TRUE)), (mean(richmond11_obs_yearDOC$tmin, na.rm=TRUE))) 
hopewell1_DOC <- data.frame(virginia_csv$id[16], (mean(hopewell1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(hopewell1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(hopewell1_obs_yearDOC$tmin, na.rm=TRUE))) 
richmond12_DOC <- data.frame(virginia_csv$id[17], (mean(richmond12_obs_yearDOC$prcp, na.rm=TRUE)), (mean(richmond12_obs_yearDOC$tmax, na.rm=TRUE)), (mean(richmond12_obs_yearDOC$tmin, na.rm=TRUE))) 
richmond13_DOC <- data.frame(virginia_csv$id[18], (mean(richmond13_obs_yearDOC$prcp, na.rm=TRUE)), (mean(richmond13_obs_yearDOC$tmax, na.rm=TRUE)), (mean(richmond13_obs_yearDOC$tmin, na.rm=TRUE))) 
charoltesville1_DOC <- data.frame(virginia_csv$id[19], (mean(charoltesville1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(charoltesville1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(charoltesville1_obs_yearDOC$tmin, na.rm=TRUE))) 
radford1_DOC <- data.frame(virginia_csv$id[20], (mean(radford1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(radford1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(radford1_obs_yearDOC$tmin, na.rm=TRUE))) 
williamsburg_DOC <- data.frame(virginia_csv$id[21], (mean(williamsburg_obs_yearDOC$prcp, na.rm=TRUE)), (mean(williamsburg_obs_yearDOC$tmax, na.rm=TRUE)), (mean(williamsburg_obs_yearDOC$tmin, na.rm=TRUE))) 
alexandria1_DOC <- data.frame(virginia_csv$id[22], (mean(alexandria1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(alexandria1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(alexandria1_obs_yearDOC$tmin, na.rm=TRUE))) 
fredericksburg1_DOC <- data.frame(virginia_csv$id[23], (mean(fredericksburg1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(fredericksburg1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(fredericksburg1_obs_yearDOC$tmin, na.rm=TRUE))) 
radford2_DOC <- data.frame(virginia_csv$id[24], (mean(radford2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(radford2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(radford2_obs_yearDOC$tmin, na.rm=TRUE))) 
hopewell2_DOC <- data.frame(virginia_csv$id[26], (mean(hopewell2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(hopewell2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(hopewell2_obs_yearDOC$tmin, na.rm=TRUE))) 
williamsburg2_DOC <- data.frame(virginia_csv$id[27], (mean(williamsburg2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(williamsburg2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(williamsburg2_obs_yearDOC$tmin, na.rm=TRUE))) 
harrisonburg1_DOC <- data.frame(virginia_csv$id[28], (mean(harrisonburg1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(harrisonburg1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(harrisonburg1_obs_yearDOC$tmin, na.rm=TRUE))) 
suffolkcity2_DOC <- data.frame(virginia_csv$id[29], (mean(suffolkcity2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(suffolkcity2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(suffolkcity2_obs_yearDOC$tmin, na.rm=TRUE))) 
fredericksburg2_DOC <- data.frame(virginia_csv$id[30], (mean(fredericksburg2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(fredericksburg2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(fredericksburg2_obs_yearDOC$tmin, na.rm=TRUE))) 
richmond14_DOC <- data.frame(virginia_csv$id[31], (mean(richmond14_obs_yearDOC$prcp, na.rm=TRUE)), (mean(richmond14_obs_yearDOC$tmax, na.rm=TRUE)), (mean(richmond14_obs_yearDOC$tmin, na.rm=TRUE))) 
harrisonburg2_DOC <- data.frame(virginia_csv$id[32], (mean(harrisonburg2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(harrisonburg2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(harrisonburg2_obs_yearDOC$tmin, na.rm=TRUE))) 
pocomokecity_DOC <- data.frame(virginia_csv$id[33], (mean(pocomokecity_obs_yearDOC$prcp, na.rm=TRUE)), (mean(pocomokecity_obs_yearDOC$tmax, na.rm=TRUE)), (mean(pocomokecity_obs_yearDOC$tmin, na.rm=TRUE))) 
roanokecounty_DOC <- data.frame(virginia_csv$id[34], (mean(roanokecounty_obs_yearDOC$prcp, na.rm=TRUE)), (mean(roanokecounty_obs_yearDOC$tmax, na.rm=TRUE)), (mean(roanokecounty_obs_yearDOC$tmin, na.rm=TRUE))) 
williamsburg3_DOC <- data.frame(virginia_csv$id[35], (mean(williamsburg3_obs_yearDOC$prcp, na.rm=TRUE)), (mean(williamsburg3_obs_yearDOC$tmax, na.rm=TRUE)), (mean(williamsburg3_obs_yearDOC$tmin, na.rm=TRUE))) 
winchester1_DOC <- data.frame(virginia_csv$id[36], (mean(winchester1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(winchester1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(winchester1_obs_yearDOC$tmin, na.rm=TRUE))) 
alexandriacity_DOC <- data.frame(virginia_csv$id[37], (mean(alexandriacity_obs_yearDOC$prcp, na.rm=TRUE)), (mean(alexandriacity_obs_yearDOC$tmax, na.rm=TRUE)), (mean(alexandriacity_obs_yearDOC$tmin, na.rm=TRUE))) 
harrisonburg2_DOC <- data.frame(virginia_csv$id[38], (mean(harrisonburg2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(harrisonburg2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(harrisonburg2_obs_yearDOC$tmin, na.rm=TRUE))) 
winchester2_DOC <- data.frame(virginia_csv$id[39], (mean(winchester2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(winchester2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(winchester2_obs_yearDOC$tmin, na.rm=TRUE))) 
emporia_DOC <- data.frame(virginia_csv$id[40], (mean(emporia_obs_yearDOC$prcp, na.rm=TRUE)), (mean(emporia_obs_yearDOC$tmax, na.rm=TRUE)), (mean(emporia_obs_yearDOC$tmin, na.rm=TRUE))) 
lynchburg1_DOC <- data.frame(virginia_csv$id[41], (mean(lynchburg1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(lynchburg1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(lynchburg1_obs_yearDOC$tmin, na.rm=TRUE))) 
norfolk1_DOC <- data.frame(virginia_csv$id[42], (mean(norfolk1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(norfolk1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(norfolk1_obs_yearDOC$tmin, na.rm=TRUE))) 
norfolk2_DOC <- data.frame(virginia_csv$id[43], (mean(norfolk2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(norfolk2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(norfolk2_obs_yearDOC$tmin, na.rm=TRUE))) 
accomac_DOC <- data.frame(virginia_csv$id[44], (mean(accomac_obs_yearDOC$prcp, na.rm=TRUE)), (mean(accomac_obs_yearDOC$tmax, na.rm=TRUE)), (mean(accomac_obs_yearDOC$tmin, na.rm=TRUE))) 
norfolk3_DOC <- data.frame(virginia_csv$id[45], (mean(norfolk3_obs_yearDOC$prcp, na.rm=TRUE)), (mean(norfolk3_obs_yearDOC$tmax, na.rm=TRUE)), (mean(norfolk3_obs_yearDOC$tmin, na.rm=TRUE))) 
harrisonburg3_DOC <- data.frame(virginia_csv$id[46], (mean(harrisonburg3_obs_yearDOC$prcp, na.rm=TRUE)), (mean(harrisonburg3_obs_yearDOC$tmax, na.rm=TRUE)), (mean(harrisonburg3_obs_yearDOC$tmin, na.rm=TRUE))) 
lynchburg2_DOC <- data.frame(virginia_csv$id[47], (mean(lynchburg2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(lynchburg2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(lynchburg2_obs_yearDOC$tmin, na.rm=TRUE))) 
winchester3_DOC <- data.frame(virginia_csv$id[48], (mean(winchester3_obs_yearDOC$prcp, na.rm=TRUE)), (mean(winchester3_obs_yearDOC$tmax, na.rm=TRUE)), (mean(winchester3_obs_yearDOC$tmin, na.rm=TRUE))) 
harrisonburg4_DOC <- data.frame(virginia_csv$id[49], (mean(harrisonburg4_obs_yearDOC$prcp, na.rm=TRUE)), (mean(harrisonburg4_obs_yearDOC$tmax, na.rm=TRUE)), (mean(harrisonburg4_obs_yearDOC$tmin, na.rm=TRUE))) 
lynchburg3_DOC <- data.frame(virginia_csv$id[50], (mean(lynchburg3_obs_yearDOC$prcp, na.rm=TRUE)), (mean(lynchburg3_obs_yearDOC$tmax, na.rm=TRUE)), (mean(lynchburg3_obs_yearDOC$tmin, na.rm=TRUE))) 
leesburg_DOC <- data.frame(virginia_csv$id[51], (mean(leesburg_obs_yearDOC$prcp, na.rm=TRUE)), (mean(leesburg_obs_yearDOC$tmax, na.rm=TRUE)), (mean(leesburg_obs_yearDOC$tmin, na.rm=TRUE))) 
lynchburg4_DOC <- data.frame(virginia_csv$id[52], (mean(lynchburg4_obs_yearDOC$prcp, na.rm=TRUE)), (mean(lynchburg4_obs_yearDOC$tmax, na.rm=TRUE)), (mean(lynchburg4_obs_yearDOC$tmin, na.rm=TRUE))) 
norfolk4_DOC <- data.frame(virginia_csv$id[53], (mean(norfolk4_obs_yearDOC$prcp, na.rm=TRUE)), (mean(norfolk4_obs_yearDOC$tmax, na.rm=TRUE)), (mean(norfolk4_obs_yearDOC$tmin, na.rm=TRUE))) 
richmond15_DOC <- data.frame(virginia_csv$id[54], (mean(richmond15_obs_yearDOC$prcp, na.rm=TRUE)), (mean(richmond15_obs_yearDOC$tmax, na.rm=TRUE)), (mean(richmond15_obs_yearDOC$tmin, na.rm=TRUE))) 
reston_DOC <- data.frame(virginia_csv$id[55], (mean(reston_obs_yearDOC$prcp, na.rm=TRUE)), (mean(reston_obs_yearDOC$tmax, na.rm=TRUE)), (mean(reston_obs_yearDOC$tmin, na.rm=TRUE))) 
harrisonburg5_DOC <- data.frame(virginia_csv$id[56], (mean(harrisonburg5_obs_yearDOC$prcp, na.rm=TRUE)), (mean(harrisonburg5_obs_yearDOC$tmax, na.rm=TRUE)), (mean(harrisonburg5_obs_yearDOC$tmin, na.rm=TRUE))) 
norfolk5_DOC <- data.frame(virginia_csv$id[57], (mean(norfolk5_obs_yearDOC$prcp, na.rm=TRUE)), (mean(norfolk5_obs_yearDOC$tmax, na.rm=TRUE)), (mean(norfolk5_obs_yearDOC$tmin, na.rm=TRUE))) 
staunton_DOC <- data.frame(virginia_csv$id[58], (mean(staunton_obs_yearDOC$prcp, na.rm=TRUE)), (mean(staunton_obs_yearDOC$tmax, na.rm=TRUE)), (mean(staunton_obs_yearDOC$tmin, na.rm=TRUE))) 
charoltesville2_DOC <- data.frame(virginia_csv$id[59], (mean(charoltesville2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(charoltesville2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(charoltesville2_obs_yearDOC$tmin, na.rm=TRUE))) 
harrisonburg6_DOC <- data.frame(virginia_csv$id[60], (mean(harrisonburg6_obs_yearDOC$prcp, na.rm=TRUE)), (mean(harrisonburg6_obs_yearDOC$tmax, na.rm=TRUE)), (mean(harrisonburg6_obs_yearDOC$tmin, na.rm=TRUE))) 
norfolk6_DOC <- data.frame(virginia_csv$id[61], (mean(norfolk6_obs_yearDOC$prcp, na.rm=TRUE)), (mean(norfolk6_obs_yearDOC$tmax, na.rm=TRUE)), (mean(norfolk6_obs_yearDOC$tmin, na.rm=TRUE))) 
fredericksburg3_DOC <- data.frame(virginia_csv$id[62], (mean(fredericksburg3_obs_yearDOC$prcp, na.rm=TRUE)), (mean(fredericksburg3_obs_yearDOC$tmax, na.rm=TRUE)), (mean(fredericksburg3_obs_yearDOC$tmin, na.rm=TRUE))) 
harrisonburg7_DOC <- data.frame(virginia_csv$id[63], (mean(harrisonburg7_obs_yearDOC$prcp, na.rm=TRUE)), (mean(harrisonburg7_obs_yearDOC$tmax, na.rm=TRUE)), (mean(harrisonburg7_obs_yearDOC$tmin, na.rm=TRUE))) 
norfolk7_DOC <- data.frame(virginia_csv$id[64], (mean(norfolk7_obs_yearDOC$prcp, na.rm=TRUE)), (mean(norfolk7_obs_yearDOC$tmax, na.rm=TRUE)), (mean(norfolk7_obs_yearDOC$tmin, na.rm=TRUE))) 
winchester4_DOC <- data.frame(virginia_csv$id[65], (mean(winchester4_obs_yearDOC$prcp, na.rm=TRUE)), (mean(winchester4_obs_yearDOC$tmax, na.rm=TRUE)), (mean(winchester4_obs_yearDOC$tmin, na.rm=TRUE))) 
virginiabeach1_DOC <- data.frame(virginia_csv$id[66], (mean(virginiabeach1_obs_yearDOC$prcp, na.rm=TRUE)), (mean(virginiabeach1_obs_yearDOC$tmax, na.rm=TRUE)), (mean(virginiabeach1_obs_yearDOC$tmin, na.rm=TRUE))) 
exmore_DOC <- data.frame(virginia_csv$id[67], (mean(exmore_obs_yearDOC$prcp, na.rm=TRUE)), (mean(exmore_obs_yearDOC$tmax, na.rm=TRUE)), (mean(exmore_obs_yearDOC$tmin, na.rm=TRUE))) 
williamsburg4_DOC <- data.frame(virginia_csv$id[68], (mean(williamsburg4_obs_yearDOC$prcp, na.rm=TRUE)), (mean(williamsburg4_obs_yearDOC$tmax, na.rm=TRUE)), (mean(williamsburg4_obs_yearDOC$tmin, na.rm=TRUE))) 
harrisonburg8_DOC <- data.frame(virginia_csv$id[69], (mean(harrisonburg8_obs_yearDOC$prcp, na.rm=TRUE)), (mean(harrisonburg8_obs_yearDOC$tmax, na.rm=TRUE)), (mean(harrisonburg8_obs_yearDOC$tmin, na.rm=TRUE))) 
newportnews_DOC <- data.frame(virginia_csv$id[70], (mean(newportnews_obs_yearDOC$prcp, na.rm=TRUE)), (mean(newportnews_obs_yearDOC$tmax, na.rm=TRUE)), (mean(newportnews_obs_yearDOC$tmin, na.rm=TRUE))) 
franlkin_DOC <- data.frame(virginia_csv$id[71], (mean(franlkin_obs_yearDOC$prcp, na.rm=TRUE)), (mean(franlkin_obs_yearDOC$tmax, na.rm=TRUE)), (mean(franlkin_obs_yearDOC$tmin, na.rm=TRUE))) 
charoltesville3_DOC <- data.frame(virginia_csv$id[72], (mean(charoltesville3_obs_yearDOC$prcp, na.rm=TRUE)), (mean(charoltesville3_obs_yearDOC$tmax, na.rm=TRUE)), (mean(charoltesville3_obs_yearDOC$tmin, na.rm=TRUE))) 
norfolk8_DOC <- data.frame(virginia_csv$id[73], (mean(norfolk8_obs_yearDOC$prcp, na.rm=TRUE)), (mean(norfolk8_obs_yearDOC$tmax, na.rm=TRUE)), (mean(norfolk8_obs_yearDOC$tmin, na.rm=TRUE))) 
hampton_DOC <- data.frame(virginia_csv$id[74], (mean(hampton_obs_yearDOC$prcp, na.rm=TRUE)), (mean(hampton_obs_yearDOC$tmax, na.rm=TRUE)), (mean(hampton_obs_yearDOC$tmin, na.rm=TRUE))) 
virginiabeach2_DOC <- data.frame(virginia_csv$id[75], (mean(virginiabeach2_obs_yearDOC$prcp, na.rm=TRUE)), (mean(virginiabeach2_obs_yearDOC$tmax, na.rm=TRUE)), (mean(virginiabeach2_obs_yearDOC$tmin, na.rm=TRUE))) 
harrisonburg9_DOC <- data.frame(virginia_csv$id[76], (mean(harrisonburg9_obs_yearDOC$prcp, na.rm=TRUE)), (mean(harrisonburg9_obs_yearDOC$tmax, na.rm=TRUE)), (mean(harrisonburg9_obs_yearDOC$tmin, na.rm=TRUE)))

#rename col.names 
cat(citySwap3(city2, y = "colnames(", z = "_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')"), sep = "\n")

colnames(richmond1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(norfolk1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(richmond2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(richmond3_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(middlesexcounty_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(richmond4_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(richmond5_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(richmond6_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(richmond7_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(richmond7.5_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(richmond8_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(suffolkcity1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(richmond11_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(hopewell1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(richmond12_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(richmond13_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(charoltesville1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(radford1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(williamsburg_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(alexandria1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(fredericksburg1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(radford2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(hopewell2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(williamsburg2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(harrisonburg1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(suffolkcity2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(fredericksburg2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(richmond14_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(harrisonburg2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(pocomokecity_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(roanokecounty_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(williamsburg3_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(winchester1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(alexandriacity_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(harrisonburg2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(winchester2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(emporia_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(lynchburg1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(norfolk1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(norfolk2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(accomac_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(norfolk3_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(harrisonburg3_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(lynchburg2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(winchester3_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(harrisonburg4_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(lynchburg3_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(leesburg_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(lynchburg4_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(norfolk4_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(richmond15_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(reston_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(harrisonburg5_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(norfolk5_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(staunton_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(charoltesville2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(harrisonburg6_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(norfolk6_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(fredericksburg3_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(harrisonburg7_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(norfolk7_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(winchester4_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(virginiabeach1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(exmore_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(williamsburg4_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(harrisonburg8_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(newportnews_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(franlkin_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(charoltesville3_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(norfolk8_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(hampton_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(virginiabeach2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(harrisonburg9_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')

## combine current year of the date of collection data using bind_rows
cat(citySwap7(city2, y = "_DOC", z = ","), sep = "")
virginia_yearDOC <- bind_rows(richmond1_DOC,norfolk1_DOC,richmond2_DOC,richmond3_DOC,middlesexcounty_DOC,richmond4_DOC,richmond5_DOC,richmond6_DOC,richmond7_DOC,richmond7.5_DOC,richmond8_DOC,suffolkcity1_DOC,richmond11_DOC,hopewell1_DOC,richmond12_DOC,richmond13_DOC,charoltesville1_DOC,radford1_DOC,williamsburg_DOC,alexandria1_DOC,fredericksburg1_DOC,radford2_DOC,hopewell2_DOC,williamsburg2_DOC,harrisonburg1_DOC,suffolkcity2_DOC,fredericksburg2_DOC,richmond14_DOC,harrisonburg2_DOC,pocomokecity_DOC,roanokecounty_DOC,williamsburg3_DOC,winchester1_DOC,alexandriacity_DOC,harrisonburg2_DOC,winchester2_DOC,emporia_DOC,lynchburg1_DOC,norfolk1_DOC,norfolk2_DOC,accomac_DOC,norfolk3_DOC,harrisonburg3_DOC,lynchburg2_DOC,winchester3_DOC,harrisonburg4_DOC,lynchburg3_DOC,leesburg_DOC,lynchburg4_DOC,norfolk4_DOC,richmond15_DOC,reston_DOC,harrisonburg5_DOC,norfolk5_DOC,staunton_DOC,charoltesville2_DOC,harrisonburg6_DOC,norfolk6_DOC,fredericksburg3_DOC,harrisonburg7_DOC,norfolk7_DOC,winchester4_DOC,virginiabeach1_DOC,exmore_DOC,williamsburg4_DOC,harrisonburg8_DOC,newportnews_DOC,franlkin_DOC,charoltesville3_DOC,norfolk8_DOC,hampton_DOC,virginiabeach2_DOC,harrisonburg9_DOC)

# save data
write.xlsx(virginia_yearDOC, file = "virginia_year_DOC_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
