#rm(list=ls())
library(FluMoDL)
library(dplyr)
library(fuzzyjoin)
library(weatherData)
library(rnoaa)
options(noaakey = "ifwilTHTRMxSsheNajCJqXNJTFbjAaBk")
library(rgdal)
library("xlsx")
setwd("~/Documents")

######### functions
citySwap = function(x, y, z){
  paste(x, y, x, z, sep = "")
}

#make a dataframe of the mean of tmax, tmin, prcp, including the ID number
citySwap2 = function(x, y, z, e, f, g, l, k){
  paste(x, y, x, z, l, k, x, e, x, f, x, g, sep = "")
}

#column names 
citySwap3 = function(x, y, z ){
  paste(y, x, z, sep = "")
}

#add min date 
citySwap_4 = function(x, y, z, j, h){
  paste(x, y, x, z, j, h, sep = "")
}

#adding max and min dates 
citySwap5 = function(x, y, z, j, h, k, l, p){
  paste(x, y, j, z, h, k, l, p, sep = "")
}

######## data prep 
#### Wisconsin data prep 
wisconsin_csv <- read.csv('wisconsin_cities.csv')
wisconsin_csv$month <- match(wisconsin_csv$month, month.name)
wisconsin_csv$date <- paste(wisconsin_csv$year, wisconsin_csv$month, wisconsin_csv$day, sep="-")
strptime(wisconsin_csv$date,format="%Y-%m-%d")
wisconsin_csv$date <- as.Date(wisconsin_csv$date)
colnames(wisconsin_csv) <- c("id", 'id2', 'city_county_state', 'latitude', 'longitude', 'elevation', 'month', 'day', 'year', 'date')
head(wisconsin_csv)
##added this code to know what is the city 
city <- wisconsin_csv$id
city2 <- wisconsin_csv$id2

##NOAA stations - set up and filter by nearby states
#stations <- ghcnd_stations()
#great_lakes_stations <- stations %>% filter(state == c("MI", "OH", "IL", "IN", "WI"))
great_lakes_stations <- read.csv("great_lakes_stations.csv")

### find closests stations
## state = state
## city = id 
#general code: state_clst <- meteo_nearby_stations(lat_lon_df = state_csv, station_data = great_lakes_stations, radius = 50)
#too big to write so leave as is. 
wisconsin_clst <- meteo_nearby_stations(lat_lon_df = wisconsin_csv, station_data = great_lakes_stations, radius = 50)

### separate all cities 
#general code: city <- (state_clst$city)
#city <- (wisconsin_clst$city)

cat(citySwap(unique(city), y = " <- (wisconsin_clst$'", z = "')"), sep = "\n")

madison <- (wisconsin_clst$'madison')
stevespoint <- (wisconsin_clst$'stevespoint')
lacrosse <- (wisconsin_clst$'lacrosse')
riverfalls <- (wisconsin_clst$'riverfalls')
viroqua <- (wisconsin_clst$'viroqua')
newlondon <- (wisconsin_clst$'newlondon')
brodhead <- (wisconsin_clst$'brodhead')
medford <- (wisconsin_clst$'medford')
cornell <- (wisconsin_clst$'cornell')
lakemills <- (wisconsin_clst$'lakemills')
boscobel <- (wisconsin_clst$'boscobel')
waupan <- (wisconsin_clst$'waupan')
wentbend <- (wisconsin_clst$'wentbend')
elkhom <- (wisconsin_clst$'elkhom')
waukesha <- (wisconsin_clst$'waukesha')
verona <- (wisconsin_clst$'verona')
rockcounty <- (wisconsin_clst$'rockcounty')
saukcity <- (wisconsin_clst$'saukcity')
dodgeville <- (wisconsin_clst$'dodgeville')
tworivers <- (wisconsin_clst$'tworivers')
wisconsinrapids <- (wisconsin_clst$'wisconsinrapids')
wausau <- (wisconsin_clst$'wausau')
utica <- (wisconsin_clst$'utica')
sturgeonbay <- (wisconsin_clst$'sturgeonbay')
barron <- (wisconsin_clst$'barron')
wetboro <- (wisconsin_clst$'wetboro')
ithica <- (wisconsin_clst$'ithica')
beloit <- (wisconsin_clst$'beloit')
jefferson <- (wisconsin_clst$'jefferson')
union <- (wisconsin_clst$'union')
albany <- (wisconsin_clst$'albany')
oshkosh <- (wisconsin_clst$'oshkosh')
spirit <- (wisconsin_clst$'spirit')

### separate all station id's (monitors) for each city
# general code: city_monitors <- city$id
#x_monitors <- x$id

cat(citySwap(unique(city), y = "_monitors <- ", z = "$id"), sep = "\n")

madison_monitors <- madison$id
stevespoint_monitors <- stevespoint$id
lacrosse_monitors <- lacrosse$id
riverfalls_monitors <- riverfalls$id
viroqua_monitors <- viroqua$id
newlondon_monitors <- newlondon$id
brodhead_monitors <- brodhead$id
medford_monitors <- medford$id
cornell_monitors <- cornell$id
lakemills_monitors <- lakemills$id
boscobel_monitors <- boscobel$id
waupan_monitors <- waupan$id
wentbend_monitors <- wentbend$id
elkhom_monitors <- elkhom$id
waukesha_monitors <- waukesha$id
verona_monitors <- verona$id
rockcounty_monitors <- rockcounty$id
saukcity_monitors <- saukcity$id
dodgeville_monitors <- dodgeville$id
tworivers_monitors <- tworivers$id
wisconsinrapids_monitors <- wisconsinrapids$id
wausau_monitors <- wausau$id
utica_monitors <- utica$id
sturgeonbay_monitors <- sturgeonbay$id
barron_monitors <- barron$id
wetboro_monitors <- wetboro$id
ithica_monitors <- ithica$id
beloit_monitors <- beloit$id
jefferson_monitors <- jefferson$id
union_monitors <- union$id
albany_monitors <- albany$id
oshkosh_monitors <- oshkosh$id
spirit_monitors <- spirit$id

### pull all observations for cities 
# general code: city_obs <- meteo_pull_monitors(city_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = " ")
#x_obs <- meteo_pull_monitors(x_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = " ")

cat(citySwap(unique(city), y = "_obs <- meteo_pull_monitors(", z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')"), sep = "\n")

madison_obs <- meteo_pull_monitors(madison_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
stevespoint_obs <- meteo_pull_monitors(stevespoint_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
lacrosse_obs <- meteo_pull_monitors(lacrosse_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
riverfalls_obs <- meteo_pull_monitors(riverfalls_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
viroqua_obs <- meteo_pull_monitors(viroqua_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
newlondon_obs <- meteo_pull_monitors(newlondon_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
brodhead_obs <- meteo_pull_monitors(brodhead_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
medford_obs <- meteo_pull_monitors(medford_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
cornell_obs <- meteo_pull_monitors(cornell_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
lakemills_obs <- meteo_pull_monitors(lakemills_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
boscobel_obs <- meteo_pull_monitors(boscobel_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
waupan_obs <- meteo_pull_monitors(waupan_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
wentbend_obs <- meteo_pull_monitors(wentbend_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
elkhom_obs <- meteo_pull_monitors(elkhom_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
waukesha_obs <- meteo_pull_monitors(waukesha_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
verona_obs <- meteo_pull_monitors(verona_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
saukcity_obs <- meteo_pull_monitors(saukcity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
dodgeville_obs <- meteo_pull_monitors(dodgeville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
tworivers_obs <- meteo_pull_monitors(tworivers_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
wisconsinrapids_obs <- meteo_pull_monitors(wisconsinrapids_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
wausau_obs <- meteo_pull_monitors(wausau_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
utica_obs <- meteo_pull_monitors(utica_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
sturgeonbay_obs <- meteo_pull_monitors(sturgeonbay_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
barron_obs <- meteo_pull_monitors(barron_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
wetboro_obs <- meteo_pull_monitors(wetboro_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
ithica_obs <- meteo_pull_monitors(ithica_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
beloit_obs <- meteo_pull_monitors(beloit_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
jefferson_obs <- meteo_pull_monitors(jefferson_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
union_obs <- meteo_pull_monitors(union_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
albany_obs <- meteo_pull_monitors(albany_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
oshkosh_obs <- meteo_pull_monitors(oshkosh_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')
spirit_obs <- meteo_pull_monitors(spirit_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1920-01-01')

### pull data by date and location - dates are coming from state_csv 
# general code: city_obs_temps <- city_obs %>% filter(date == " ")
#x_obs_temps <- x_obs %>% filter(date == ' ')

cat(citySwap(city2, y = "_obs_temps <- ", z = "_obs %>% filter(date == ' ')"), sep = "\n")

madison1_obs_temps <- madison_obs %>% filter(date == '1978-05-16')
madison2_obs_temps <- madison_obs %>% filter(date == '1978-05-16')
stevespoint_obs_temps <- stevespoint_obs %>% filter(date == '1981-10-19')
lacrosse_obs_temps <- lacrosse_obs %>% filter(date == '1974-06-14')
riverfalls_obs_temps <- riverfalls_obs %>% filter(date == '1972-05-05')
viroqua_obs_temps <- viroqua_obs %>% filter(date == '1956-05-30')
newlondon1_obs_temps <- newlondon_obs %>% filter(date == '1977-10-09')
medford_obs_temps <- medford_obs %>% filter(date == '1947-07-26')
cornell_obs_temps <- cornell_obs %>% filter(date == '1958-07-25')
lakemills_obs_temps <- lakemills_obs %>% filter(date == '1857-06-05')
boscobel_obs_temps <- boscobel_obs %>% filter(date == '1975-05-26')
waupan_obs_temps <- waupan_obs %>% filter(date == '1965-06-06')
madison4_obs_temps <- madison_obs %>% filter(date == '1877-05-25')
wentbend_obs_temps <- wentbend_obs %>% filter(date == '1969-05-10')
elkhom_obs_temps <- elkhom_obs %>% filter(date == '1938-05-09')
waukesha_obs_temps <- waukesha_obs %>% filter(date == '1959-06-10')
verona_obs_temps <- verona_obs %>% filter(date == '1998-12-05')
saukcity1_obs_temps <- saukcity_obs %>% filter(date == '1960-05-19')
madison5_obs_temps <- madison_obs %>% filter(date == '1881-06-14')
dodgeville1_obs_temps <- dodgeville_obs %>% filter(date == '1970-08-14')
newlondon2_obs_temps <- newlondon_obs %>% filter(date == '1955-06-08')
tworivers_obs_temps <- tworivers_obs %>% filter(date == '1964-09-27')
wisconsinrapids_obs_temps <- wisconsinrapids_obs %>% filter(date == '1891-06-03')
wausau_obs_temps <- wausau_obs %>% filter(date == '1961-07-18')
madison6_obs_temps <- madison_obs %>% filter(date == '1886-05-26')
saukcity2_obs_temps <- saukcity_obs %>% filter(date == '1937-05-15')
utica_obs_temps <- utica_obs %>% filter(date == '1956-06-20')
sturgeonbay_obs_temps <- sturgeonbay_obs %>% filter(date == '1959-10-02')
barron_obs_temps <- barron_obs %>% filter(date == '1888-05-10')
wetboro_obs_temps <- wetboro_obs %>% filter(date == '1993-07-18')
ithica_obs_temps <- ithica_obs %>% filter(date == '1969-04-27')
beloit_obs_temps <- beloit_obs %>% filter(date == '1932-05-07')
dodgeville2_obs_temps <- dodgeville_obs %>% filter(date == '1958-05-13')
jefferson_obs_temps <- jefferson_obs %>% filter(date == '1984-05-13')
union_obs_temps <- union_obs %>% filter(date == '1961-05-01')
albany_obs_temps <- albany_obs %>% filter(date == '1965-05-14')
oshkosh_obs_temps <- oshkosh_obs %>% filter(date == '1965-05-20')
spirit_obs_temps <- spirit_obs %>% filter(date == '1947-06-06')
madison7_obs_temps <- madison_obs %>% filter(date == '1953-06-16')

## combine using bind_rows
wisconsin_obs <- bind_rows(madison1_obs_temps, madison2_obs_temps, stevespoint_obs_temps, lacrosse_obs_temps, 
riverfalls_obs_temps, viroqua_obs_temps, newlondon1_obs_temps, medford_obs_temps, cornell_obs_temps, 
lakemills_obs_temps, boscobel_obs_temps, waupan_obs_temps, madison4_obs_temps, wentbend_obs_temps, elkhom_obs_temps, 
waukesha_obs_temps, verona_obs_temps, saukcity1_obs_temps,  madison5_obs_temps, dodgeville1_obs_temps, 
newlondon2_obs_temps, tworivers_obs_temps, wisconsinrapids_obs_temps, wausau_obs_temps, madison6_obs_temps,
saukcity2_obs_temps, utica_obs_temps, sturgeonbay_obs_temps, barron_obs_temps, wetboro_obs_temps, 
ithica_obs_temps, beloit_obs_temps, dodgeville2_obs_temps, jefferson_obs_temps, union_obs_temps, albany_obs_temps, 
oshkosh_obs_temps, spirit_obs_temps, madison7_obs_temps)

## merge with state_csv
#general code: state_data <- merge.data.frame(state_csv, state_obs, by = 'date')
wisconsin_data <- merge.data.frame(wisconsin_csv, wisconsin_obs, by = 'date')

## find the tavg for state data 
#### finding tavg
# general code: state_data$tavg <- ((state_data$tmax + state_data$tmin) / 2)
wisconsin_data$tavg <- ((wisconsin_data$tmax + wisconsin_data$tmin) / 2)

### save data as xlsx file 
library("xlsx")
write.xlsx(wisconsin_data, file = "wisconsin_data_total_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

### Find the previous 6 months and add to original state_csv dataframe 
#install.packages("zoo")
library(zoo)
#general code: state_csv$previous6mon <- as.Date(as.yearmon(as.Date(state_csv$date)) -.5, frac = 1)
wisconsin_csv$previous6mon <- as.Date(as.yearmon(as.Date(wisconsin_csv$date)) -.5, frac = 1)

## pull data for the previous 6 months before the date of collection
# general code: city_obs_6months <- meteo_pull_monitors(city_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = " ", date_max = " " )
#x_obs_6months <- meteo_pull_monitors(x_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = ' ', date_max = ' ' )

cat(citySwap(city2, y = "_obs_6months <- meteo_pull_monitors(", z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '', date_max = '')"), sep = "\n")

madison1_obs_6months <- meteo_pull_monitors(madison_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1977-11-30', date_max = '1978-05-16' )
madison2_obs_6months <- meteo_pull_monitors(madison_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1977-11-30', date_max = '1978-05-16' )
stevespoint_obs_6months <- meteo_pull_monitors(stevespoint_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1981-04-30', date_max = '1981-10-19')
lacrosse_obs_6months <- meteo_pull_monitors(lacrosse_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1973-12-31', date_max = '1974-06-14')
riverfalls_obs_6months <- meteo_pull_monitors(riverfalls_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1971-11-30', date_max = '1972-05-05')
viroqua_obs_6months <- meteo_pull_monitors(viroqua_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1955-11-30', date_max = '1956-05-30')
newlondon1_obs_6months <- meteo_pull_monitors(newlondon_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1977-04-30', date_max = '1977-10-09')
brodhead_obs_6months <- meteo_pull_monitors(brodhead_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = 'N/A', date_max = 'N/A' )
medford_obs_6months <- meteo_pull_monitors(medford_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1947-01-31', date_max = '1947-07-26
' )
cornell_obs_6months <- meteo_pull_monitors(cornell_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1958-01-31', date_max = '1958-07-25')
lakemills_obs_6months <- meteo_pull_monitors(lakemills_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1856-12-31', date_max = '1857-06-05')
boscobel_obs_6months <- meteo_pull_monitors(boscobel_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1974-11-30', date_max = '1975-05-26')
waupan_obs_6months <- meteo_pull_monitors(waupan_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1964-12-31', date_max = '1965-06-06')
madison4_obs_6months <- meteo_pull_monitors(madison_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1876-11-30', date_max = '1877-05-25')
wentbend_obs_6months <- meteo_pull_monitors(wentbend_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1968-11-30', date_max = '1969-05-10')
elkhom_obs_6months <- meteo_pull_monitors(elkhom_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1937-11-30', date_max = '1938-05-09')
waukesha_obs_6months <- meteo_pull_monitors(waukesha_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1958-12-31', date_max = '1959-06-10')
verona_obs_6months <- meteo_pull_monitors(verona_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1998-06-30', date_max = '1998-12-05')
rockcounty_obs_6months <- meteo_pull_monitors(rockcounty_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = 'N/A', date_max = 'N/A' )
saukcity1_obs_6months <- meteo_pull_monitors(saukcity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1959-11-30', date_max = '1960-05-19
')
madison5_obs_6months <- meteo_pull_monitors(madison_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1880-12-31', date_max = '1881-06-14')
dodgeville1_obs_6months <- meteo_pull_monitors(dodgeville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1970-02-28', date_max = '1970-08-14')
newlondon2_obs_6months <- meteo_pull_monitors(newlondon_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1954-12-31', date_max = '1955-06-08')
tworivers_obs_6months <- meteo_pull_monitors(tworivers_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1964-03-31', date_max = '1964-09-27')
wisconsinrapids_obs_6months <- meteo_pull_monitors(wisconsinrapids_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1890-12-31', date_max = '1891-06-03')
wausau_obs_6months <- meteo_pull_monitors(wausau_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1961-01-31', date_max = '1961-07-18
')
madison6_obs_6months <- meteo_pull_monitors(madison_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1885-11-30', date_max = '1886-05-26')
saukcity2_obs_6months <- meteo_pull_monitors(saukcity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1936-11-30', date_max = '1937-05-15')
utica_obs_6months <- meteo_pull_monitors(utica_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1955-12-31', date_max = '1956-06-20')
sturgeonbay_obs_6months <- meteo_pull_monitors(sturgeonbay_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1959-04-30', date_max = '1959-10-02')
barron_obs_6months <- meteo_pull_monitors(barron_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1887-11-30', date_max = '1888-05-10
')
wetboro_obs_6months <- meteo_pull_monitors(wetboro_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1993-01-31', date_max = '1993-07-18')
ithica_obs_6months <- meteo_pull_monitors(ithica_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1968-10-31', date_max = '1969-04-27
')
beloit_obs_6months <- meteo_pull_monitors(beloit_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1931-11-30', date_max = '1932-05-07')
dodgeville2_obs_6months <- meteo_pull_monitors(dodgeville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1957-11-30', date_max = '1958-05-13')
jefferson_obs_6months <- meteo_pull_monitors(jefferson_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1983-11-30', date_max = '1984-05-13')
union_obs_6months <- meteo_pull_monitors(union_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1960-11-30', date_max = '1961-05-01')
albany_obs_6months <- meteo_pull_monitors(albany_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1964-11-30', date_max = '1965-05-14')
oshkosh_obs_6months <- meteo_pull_monitors(oshkosh_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1964-11-30', date_max = '1965-05-20')
spirit_obs_6months <- meteo_pull_monitors(spirit_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1946-12-31', date_max = '1947-06-06')
madison7_obs_6months <- meteo_pull_monitors(madison_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1952-12-31', date_max = '1953-06-16')

## make a dataframe of the mean of tmax, tmin, prcp of the previous 6 months 
#previous6months_x <- data.frame(wisconsin_csv$id[8], 
                                   #(mean(x_obs_6months$prcp, na.rm=TRUE)), 
                                   #(mean(x_obs_6months$tmax, na.rm=TRUE)), 
                                   #(mean(x_obs_6months$tmin, na.rm=TRUE)))

## make list of dataframes without dates
lennumber <- (1:41)
dflist_nodates <- list(city, city2, lennumber)

citySwap2 = function(x, y, z, e, f, g, l, k){
  paste(x, y, z, l, k, x, e, x, f, x, g, sep = "")
}

lapply(dflist_nodates, cat(citySwap2(x = city2,  y = "_6months", z = " <- data.frame(wisconsin_csv$id[", l= lennumber, k = "], 
                                   (mean(", e = "_obs_6months$prcp, na.rm=TRUE)), 
                                   (mean(", f = "_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(", g = "_obs_6months$tmin, na.rm=TRUE))) "), sep = "\n"))

madison1_6months <- data.frame(wisconsin_csv$id[1], 
                               (mean(madison1_obs_6months$prcp, na.rm=TRUE)), 
                               (mean(madison1_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(madison1_obs_6months$tmin, na.rm=TRUE))) 
madison2_6months <- data.frame(wisconsin_csv$id[2], 
                               (mean(madison2_obs_6months$prcp, na.rm=TRUE)), 
                               (mean(madison2_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(madison2_obs_6months$tmin, na.rm=TRUE))) 
stevespoint_6months <- data.frame(wisconsin_csv$id[3], 
                                  (mean(stevespoint_obs_6months$prcp, na.rm=TRUE)), 
                                  (mean(stevespoint_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(stevespoint_obs_6months$tmin, na.rm=TRUE))) 
lacrosse_6months <- data.frame(wisconsin_csv$id[4], 
                               (mean(lacrosse_obs_6months$prcp, na.rm=TRUE)), 
                               (mean(lacrosse_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(lacrosse_obs_6months$tmin, na.rm=TRUE))) 
riverfalls_6months <- data.frame(wisconsin_csv$id[5], 
                                 (mean(riverfalls_obs_6months$prcp, na.rm=TRUE)), 
                                 (mean(riverfalls_obs_6months$tmax, na.rm=TRUE)), 
                                 (mean(riverfalls_obs_6months$tmin, na.rm=TRUE))) 
viroqua_6months <- data.frame(wisconsin_csv$id[6], 
                              (mean(viroqua_obs_6months$prcp, na.rm=TRUE)), 
                              (mean(viroqua_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(viroqua_obs_6months$tmin, na.rm=TRUE))) 
newlondon1_6months <- data.frame(wisconsin_csv$id[7], 
                                 (mean(newlondon1_obs_6months$prcp, na.rm=TRUE)), 
                                 (mean(newlondon1_obs_6months$tmax, na.rm=TRUE)), 
                                 (mean(newlondon1_obs_6months$tmin, na.rm=TRUE))) 
medford_6months <- data.frame(wisconsin_csv$id[9], 
                              (mean(medford_obs_6months$prcp, na.rm=TRUE)), 
                              (mean(medford_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(medford_obs_6months$tmin, na.rm=TRUE))) 
cornell_6months <- data.frame(wisconsin_csv$id[10], 
                              (mean(cornell_obs_6months$prcp, na.rm=TRUE)), 
                              (mean(cornell_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(cornell_obs_6months$tmin, na.rm=TRUE))) 
lakemills_6months <- data.frame(wisconsin_csv$id[11], 
                                (mean(lakemills_obs_6months$prcp, na.rm=TRUE)), 
                                (mean(lakemills_obs_6months$tmax, na.rm=TRUE)), 
                                (mean(lakemills_obs_6months$tmin, na.rm=TRUE))) 
boscobel_6months <- data.frame(wisconsin_csv$id[12], 
                               (mean(boscobel_obs_6months$prcp, na.rm=TRUE)), 
                               (mean(boscobel_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(boscobel_obs_6months$tmin, na.rm=TRUE))) 
waupan_6months <- data.frame(wisconsin_csv$id[13], 
                             (mean(waupan_obs_6months$prcp, na.rm=TRUE)), 
                             (mean(waupan_obs_6months$tmax, na.rm=TRUE)), 
                             (mean(waupan_obs_6months$tmin, na.rm=TRUE))) 
madison4_6months <- data.frame(wisconsin_csv$id[14], 
                               (mean(madison4_obs_6months$prcp, na.rm=TRUE)), 
                               (mean(madison4_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(madison4_obs_6months$tmin, na.rm=TRUE))) 
wentbend_6months <- data.frame(wisconsin_csv$id[15], 
                               (mean(wentbend_obs_6months$prcp, na.rm=TRUE)), 
                               (mean(wentbend_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(wentbend_obs_6months$tmin, na.rm=TRUE))) 
elkhom_6months <- data.frame(wisconsin_csv$id[16], 
                             (mean(elkhom_obs_6months$prcp, na.rm=TRUE)), 
                             (mean(elkhom_obs_6months$tmax, na.rm=TRUE)), 
                             (mean(elkhom_obs_6months$tmin, na.rm=TRUE))) 
waukesha_6months <- data.frame(wisconsin_csv$id[17], 
                               (mean(waukesha_obs_6months$prcp, na.rm=TRUE)), 
                               (mean(waukesha_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(waukesha_obs_6months$tmin, na.rm=TRUE))) 
verona_6months <- data.frame(wisconsin_csv$id[18], 
                             (mean(verona_obs_6months$prcp, na.rm=TRUE)), 
                             (mean(verona_obs_6months$tmax, na.rm=TRUE)), 
                             (mean(verona_obs_6months$tmin, na.rm=TRUE))) 
saukcity1_6months <- data.frame(wisconsin_csv$id[20], 
                                (mean(saukcity1_obs_6months$prcp, na.rm=TRUE)), 
                                (mean(saukcity1_obs_6months$tmax, na.rm=TRUE)), 
                                (mean(saukcity1_obs_6months$tmin, na.rm=TRUE))) 
madison5_6months <- data.frame(wisconsin_csv$id[21], 
                               (mean(madison5_obs_6months$prcp, na.rm=TRUE)), 
                               (mean(madison5_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(madison5_obs_6months$tmin, na.rm=TRUE))) 
dodgeville1_6months <- data.frame(wisconsin_csv$id[22], 
                                  (mean(dodgeville1_obs_6months$prcp, na.rm=TRUE)), 
                                  (mean(dodgeville1_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(dodgeville1_obs_6months$tmin, na.rm=TRUE))) 
newlondon2_6months <- data.frame(wisconsin_csv$id[23], 
                                 (mean(newlondon2_obs_6months$prcp, na.rm=TRUE)), 
                                 (mean(newlondon2_obs_6months$tmax, na.rm=TRUE)), 
                                 (mean(newlondon2_obs_6months$tmin, na.rm=TRUE))) 
tworivers_6months <- data.frame(wisconsin_csv$id[24], 
                                (mean(tworivers_obs_6months$prcp, na.rm=TRUE)), 
                                (mean(tworivers_obs_6months$tmax, na.rm=TRUE)), 
                                (mean(tworivers_obs_6months$tmin, na.rm=TRUE))) 
wisconsinrapids_6months <- data.frame(wisconsin_csv$id[25], 
                                      (mean(wisconsinrapids_obs_6months$prcp, na.rm=TRUE)), 
                                      (mean(wisconsinrapids_obs_6months$tmax, na.rm=TRUE)), 
                                      (mean(wisconsinrapids_obs_6months$tmin, na.rm=TRUE))) 
wausau_6months <- data.frame(wisconsin_csv$id[26], 
                             (mean(wausau_obs_6months$prcp, na.rm=TRUE)), 
                             (mean(wausau_obs_6months$tmax, na.rm=TRUE)), 
                             (mean(wausau_obs_6months$tmin, na.rm=TRUE))) 
madison6_6months <- data.frame(wisconsin_csv$id[27], 
                               (mean(madison6_obs_6months$prcp, na.rm=TRUE)), 
                               (mean(madison6_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(madison6_obs_6months$tmin, na.rm=TRUE))) 
saukcity2_6months <- data.frame(wisconsin_csv$id[28], 
                                (mean(saukcity2_obs_6months$prcp, na.rm=TRUE)), 
                                (mean(saukcity2_obs_6months$tmax, na.rm=TRUE)), 
                                (mean(saukcity2_obs_6months$tmin, na.rm=TRUE))) 
utica_6months <- data.frame(wisconsin_csv$id[29], 
                            (mean(utica_obs_6months$prcp, na.rm=TRUE)), 
                            (mean(utica_obs_6months$tmax, na.rm=TRUE)), 
                            (mean(utica_obs_6months$tmin, na.rm=TRUE))) 
sturgeonbay_6months <- data.frame(wisconsin_csv$id[30], 
                                  (mean(sturgeonbay_obs_6months$prcp, na.rm=TRUE)), 
                                  (mean(sturgeonbay_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(sturgeonbay_obs_6months$tmin, na.rm=TRUE))) 
barron_6months <- data.frame(wisconsin_csv$id[31], 
                             (mean(barron_obs_6months$prcp, na.rm=TRUE)), 
                             (mean(barron_obs_6months$tmax, na.rm=TRUE)), 
                             (mean(barron_obs_6months$tmin, na.rm=TRUE))) 
wetboro_6months <- data.frame(wisconsin_csv$id[32], 
                              (mean(wetboro_obs_6months$prcp, na.rm=TRUE)), 
                              (mean(wetboro_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(wetboro_obs_6months$tmin, na.rm=TRUE))) 
ithica_6months <- data.frame(wisconsin_csv$id[33], 
                             (mean(ithica_obs_6months$prcp, na.rm=TRUE)), 
                             (mean(ithica_obs_6months$tmax, na.rm=TRUE)), 
                             (mean(ithica_obs_6months$tmin, na.rm=TRUE))) 
beloit_6months <- data.frame(wisconsin_csv$id[34], 
                             (mean(beloit_obs_6months$prcp, na.rm=TRUE)), 
                             (mean(beloit_obs_6months$tmax, na.rm=TRUE)), 
                             (mean(beloit_obs_6months$tmin, na.rm=TRUE))) 
dodgeville2_6months <- data.frame(wisconsin_csv$id[35], 
                                  (mean(dodgeville2_obs_6months$prcp, na.rm=TRUE)), 
                                  (mean(dodgeville2_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(dodgeville2_obs_6months$tmin, na.rm=TRUE))) 
jefferson_6months <- data.frame(wisconsin_csv$id[36], 
                                (mean(jefferson_obs_6months$prcp, na.rm=TRUE)), 
                                (mean(jefferson_obs_6months$tmax, na.rm=TRUE)), 
                                (mean(jefferson_obs_6months$tmin, na.rm=TRUE))) 
union_6months <- data.frame(wisconsin_csv$id[37], 
                            (mean(union_obs_6months$prcp, na.rm=TRUE)), 
                            (mean(union_obs_6months$tmax, na.rm=TRUE)), 
                            (mean(union_obs_6months$tmin, na.rm=TRUE))) 
albany_6months <- data.frame(wisconsin_csv$id[38], 
                             (mean(albany_obs_6months$prcp, na.rm=TRUE)), 
                             (mean(albany_obs_6months$tmax, na.rm=TRUE)), 
                             (mean(albany_obs_6months$tmin, na.rm=TRUE))) 
oshkosh_6months <- data.frame(wisconsin_csv$id[39], 
                              (mean(oshkosh_obs_6months$prcp, na.rm=TRUE)), 
                              (mean(oshkosh_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(oshkosh_obs_6months$tmin, na.rm=TRUE))) 
spirit_6months <- data.frame(wisconsin_csv$id[40], 
                             (mean(spirit_obs_6months$prcp, na.rm=TRUE)), 
                             (mean(spirit_obs_6months$tmax, na.rm=TRUE)), 
                             (mean(spirit_obs_6months$tmin, na.rm=TRUE))) 
madison7_6months <- data.frame(wisconsin_csv$id[41], 
                               (mean(madison7_obs_6months$prcp, na.rm=TRUE)), 
                               (mean(madison7_obs_6months$tmax, na.rm=TRUE)), 
                               (mean(madison7_obs_6months$tmin, na.rm=TRUE)))


#rename col.names 
#colnames(x_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')

cat(citySwap3(city2, y = "colnames(", z = "_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')"), sep = "\n")

colnames(madison1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(madison2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(stevespoint_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(lacrosse_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(riverfalls_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(viroqua_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(newlondon1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(medford_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(cornell_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(lakemills_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(boscobel_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(waupan_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(madison4_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(wentbend_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(elkhom_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(waukesha_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(verona_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(saukcity1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(madison5_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(dodgeville1_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(newlondon2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tworivers_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(wisconsinrapids_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(wausau_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(madison6_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(saukcity2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(utica_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(sturgeonbay_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(barron_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(wetboro_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(ithica_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(beloit_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(dodgeville2_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(jefferson_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(union_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(albany_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(oshkosh_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(spirit_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(madison7_6months) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')

## combine previous 6 months data using bind_rows
wisconsin_6months <- bind_rows(madison1_6months,  
                           madison2_6months,
                           stevespoint_6months, 
                           lacrosse_6months, 
                           riverfalls_6months, 
                           viroqua_6months,
                           newlondon1_6months, 
                           medford_6months,
                           cornell_6months,
                           lakemills_6months,
                           boscobel_6months, 
                           waupan_6months, 
                           madison4_6months,
                           wentbend_6months,
                           elkhom_6months, 
                           waukesha_6months,
                           verona_6months, 
                           saukcity1_6months,
                           madison5_6months,
                           dodgeville1_6months, 
                           newlondon2_6months,
                           tworivers_6months,
                           wisconsinrapids_6months, 
                           wausau_6months, 
                           madison6_6months,
                           saukcity2_6months,  
                           utica_6months,
                           sturgeonbay_6months,
                           barron_6months,
                           wetboro_6months,
                           ithica_6months,
                           beloit_6months,
                           dodgeville2_6months, 
                           jefferson_6months,
                           union_6months,  
                           albany_6months, 
                           oshkosh_6months, 
                           spirit_6months,
                           madison7_6months)

### save data as xlsx file 
write.xlsx(wisconsin_6months, file = "wisconsin_6months_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

## pull data for the previous year before the date of collection
# general code: city_obs_year <- meteo_pull_monitors(city_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = " ", date_max = " " )
#x_obs_year <- meteo_pull_monitors(x_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = " ", date_max = ' ' )

date_max <- wisconsin_csv$date
date_min <- (wisconsin_csv$date - 365)
dflist_withdates <- list(city, city2, date_max, date_min)

lapply(dflist_withdates, cat(citySwap(x = city2, y = "_obs_year <- meteo_pull_monitors(", j = city, 
                                      z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '", h = date_min, 
                                      k = "', date_max = '", l = date_max, p ="')"), sep = "\n"))

madison1_obs_year <- meteo_pull_monitors(madison_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1977-05-16', date_max = '1978-05-16')
madison2_obs_year <- meteo_pull_monitors(madison_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1977-05-16', date_max = '1978-05-16')
stevespoint_obs_year <- meteo_pull_monitors(stevespoint_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1980-10-19', date_max = '1981-10-19')
lacrosse_obs_year <- meteo_pull_monitors(lacrosse_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1973-06-14', date_max = '1974-06-14')
riverfalls_obs_year <- meteo_pull_monitors(riverfalls_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1971-05-06', date_max = '1972-05-05')
viroqua_obs_year <- meteo_pull_monitors(viroqua_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1955-05-31', date_max = '1956-05-30')
newlondon1_obs_year <- meteo_pull_monitors(newlondon_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1976-10-09', date_max = '1977-10-09')
medford_obs_year <- meteo_pull_monitors(medford_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1946-07-26', date_max = '1947-07-26')
cornell_obs_year <- meteo_pull_monitors(cornell_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1957-07-25', date_max = '1958-07-25')
lakemills_obs_year <- meteo_pull_monitors(lakemills_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1856-06-05', date_max = '1857-06-05')
boscobel_obs_year <- meteo_pull_monitors(boscobel_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1974-05-26', date_max = '1975-05-26')
waupan_obs_year <- meteo_pull_monitors(waupan_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1964-06-06', date_max = '1965-06-06')
madison4_obs_year <- meteo_pull_monitors(madison_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1876-05-25', date_max = '1877-05-25')
wentbend_obs_year <- meteo_pull_monitors(wentbend_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1968-05-10', date_max = '1969-05-10')
elkhom_obs_year <- meteo_pull_monitors(elkhom_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1937-05-09', date_max = '1938-05-09')
waukesha_obs_year <- meteo_pull_monitors(waukesha_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1958-06-10', date_max = '1959-06-10')
verona_obs_year <- meteo_pull_monitors(verona_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1997-12-05', date_max = '1998-12-05')
saukcity1_obs_year <- meteo_pull_monitors(saukcity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1959-05-20', date_max = '1960-05-19')
madison5_obs_year <- meteo_pull_monitors(madison_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1880-06-14', date_max = '1881-06-14')
dodgeville1_obs_year <- meteo_pull_monitors(dodgeville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1969-08-14', date_max = '1970-08-14')
newlondon2_obs_year <- meteo_pull_monitors(newlondon_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1954-06-08', date_max = '1955-06-08')
tworivers_obs_year <- meteo_pull_monitors(tworivers_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1963-09-28', date_max = '1964-09-27')
wisconsinrapids_obs_year <- meteo_pull_monitors(wisconsinrapids_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1890-06-03', date_max = '1891-06-03')
wausau_obs_year <- meteo_pull_monitors(wausau_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1960-07-18', date_max = '1961-07-18')
madison6_obs_year <- meteo_pull_monitors(madison_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1885-05-26', date_max = '1886-05-26')
saukcity2_obs_year <- meteo_pull_monitors(saukcity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1936-05-15', date_max = '1937-05-15')
utica_obs_year <- meteo_pull_monitors(utica_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1955-06-21', date_max = '1956-06-20')
sturgeonbay_obs_year <- meteo_pull_monitors(sturgeonbay_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1958-10-02', date_max = '1959-10-02')
barron_obs_year <- meteo_pull_monitors(barron_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1887-05-11', date_max = '1888-05-10')
wetboro_obs_year <- meteo_pull_monitors(wetboro_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1992-07-18', date_max = '1993-07-18')
ithica_obs_year <- meteo_pull_monitors(ithica_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1968-04-27', date_max = '1969-04-27')
beloit_obs_year <- meteo_pull_monitors(beloit_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1931-05-08', date_max = '1932-05-07')
dodgeville2_obs_year <- meteo_pull_monitors(dodgeville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1957-05-13', date_max = '1958-05-13')
jefferson_obs_year <- meteo_pull_monitors(jefferson_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1983-05-14', date_max = '1984-05-13')
union_obs_year <- meteo_pull_monitors(union_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1960-05-01', date_max = '1961-05-01')
albany_obs_year <- meteo_pull_monitors(albany_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1964-05-14', date_max = '1965-05-14')
oshkosh_obs_year <- meteo_pull_monitors(oshkosh_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1964-05-20', date_max = '1965-05-20')
spirit_obs_year <- meteo_pull_monitors(spirit_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1946-06-06', date_max = '1947-06-06')
madison7_obs_year <- meteo_pull_monitors(madison_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1952-06-16', date_max = '1953-06-16')


## make a dataframe of the mean of tmax, tmin, prcp of the previous year before the date of collection 
#previousyear_x <- data.frame(wisconsin_csv$id[8], 
                                #(mean(x_obs_year$prcp, na.rm=TRUE)), 
                                #(mean(x_obs_year$tmax, na.rm=TRUE)), 
                                #(mean(x_obs_year$tmin, na.rm=TRUE))) 

lapply(dflist_nodates, cat(citySwap2(x = city2,  y = "_previousyear", z = " <- data.frame(wisconsin_csv$id[", l= lennumber, k = "], 
                                   (mean(", e = "_obs_6months$prcp, na.rm=TRUE)), 
                                   (mean(", f = "_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(", g = "_obs_6months$tmin, na.rm=TRUE))) "), sep = "\n"))
madison1_previousyear <- data.frame(wisconsin_csv$id[1], 
                                    (mean(madison1_obs_6months$prcp, na.rm=TRUE)), 
                                    (mean(madison1_obs_6months$tmax, na.rm=TRUE)), 
                                    (mean(madison1_obs_6months$tmin, na.rm=TRUE))), 
madison2_previousyear <- data.frame(wisconsin_csv$id[2], 
                                    (mean(madison2_obs_6months$prcp, na.rm=TRUE)), 
                                    (mean(madison2_obs_6months$tmax, na.rm=TRUE)), 
                                    (mean(madison2_obs_6months$tmin, na.rm=TRUE))), 
stevespoint_previousyear <- data.frame(wisconsin_csv$id[3], 
                                       (mean(stevespoint_obs_6months$prcp, na.rm=TRUE)), 
                                       (mean(stevespoint_obs_6months$tmax, na.rm=TRUE)), 
                                       (mean(stevespoint_obs_6months$tmin, na.rm=TRUE))), 
lacrosse_previousyear <- data.frame(wisconsin_csv$id[4], 
                                    (mean(lacrosse_obs_6months$prcp, na.rm=TRUE)), 
                                    (mean(lacrosse_obs_6months$tmax, na.rm=TRUE)), 
                                    (mean(lacrosse_obs_6months$tmin, na.rm=TRUE))), 
riverfalls_previousyear <- data.frame(wisconsin_csv$id[5], 
                                      (mean(riverfalls_obs_6months$prcp, na.rm=TRUE)), 
                                      (mean(riverfalls_obs_6months$tmax, na.rm=TRUE)), 
                                      (mean(riverfalls_obs_6months$tmin, na.rm=TRUE))), 
viroqua_previousyear <- data.frame(wisconsin_csv$id[6], 
                                   (mean(viroqua_obs_6months$prcp, na.rm=TRUE)), 
                                   (mean(viroqua_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(viroqua_obs_6months$tmin, na.rm=TRUE))), 
newlondon1_previousyear <- data.frame(wisconsin_csv$id[7], 
                                      (mean(newlondon1_obs_6months$prcp, na.rm=TRUE)), 
                                      (mean(newlondon1_obs_6months$tmax, na.rm=TRUE)), 
                                      (mean(newlondon1_obs_6months$tmin, na.rm=TRUE))), 
medford_previousyear <- data.frame(wisconsin_csv$id[9], 
                                   (mean(medford_obs_6months$prcp, na.rm=TRUE)), 
                                   (mean(medford_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(medford_obs_6months$tmin, na.rm=TRUE))), 
cornell_previousyear <- data.frame(wisconsin_csv$id[10], 
                                   (mean(cornell_obs_6months$prcp, na.rm=TRUE)), 
                                   (mean(cornell_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(cornell_obs_6months$tmin, na.rm=TRUE))), 
lakemills_previousyear <- data.frame(wisconsin_csv$id[11], 
                                     (mean(lakemills_obs_6months$prcp, na.rm=TRUE)), 
                                     (mean(lakemills_obs_6months$tmax, na.rm=TRUE)), 
                                     (mean(lakemills_obs_6months$tmin, na.rm=TRUE))), 
boscobel_previousyear <- data.frame(wisconsin_csv$id[12], 
                                    (mean(boscobel_obs_6months$prcp, na.rm=TRUE)), 
                                    (mean(boscobel_obs_6months$tmax, na.rm=TRUE)), 
                                    (mean(boscobel_obs_6months$tmin, na.rm=TRUE))), 
waupan_previousyear <- data.frame(wisconsin_csv$id[13], 
                                  (mean(waupan_obs_6months$prcp, na.rm=TRUE)), 
                                  (mean(waupan_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(waupan_obs_6months$tmin, na.rm=TRUE))), 
madison4_previousyear <- data.frame(wisconsin_csv$id[14], 
                                    (mean(madison4_obs_6months$prcp, na.rm=TRUE)), 
                                    (mean(madison4_obs_6months$tmax, na.rm=TRUE)), 
                                    (mean(madison4_obs_6months$tmin, na.rm=TRUE))), 
wentbend_previousyear <- data.frame(wisconsin_csv$id[15], 
                                    (mean(wentbend_obs_6months$prcp, na.rm=TRUE)), 
                                    (mean(wentbend_obs_6months$tmax, na.rm=TRUE)), 
                                    (mean(wentbend_obs_6months$tmin, na.rm=TRUE))), 
elkhom_previousyear <- data.frame(wisconsin_csv$id[16], 
                                  (mean(elkhom_obs_6months$prcp, na.rm=TRUE)), 
                                  (mean(elkhom_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(elkhom_obs_6months$tmin, na.rm=TRUE))), 
waukesha_previousyear <- data.frame(wisconsin_csv$id[17], 
                                    (mean(waukesha_obs_6months$prcp, na.rm=TRUE)), 
                                    (mean(waukesha_obs_6months$tmax, na.rm=TRUE)), 
                                    (mean(waukesha_obs_6months$tmin, na.rm=TRUE))), 
verona_previousyear <- data.frame(wisconsin_csv$id[18], 
                                  (mean(verona_obs_6months$prcp, na.rm=TRUE)), 
                                  (mean(verona_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(verona_obs_6months$tmin, na.rm=TRUE))), 
saukcity1_previousyear <- data.frame(wisconsin_csv$id[20], 
                                     (mean(saukcity1_obs_6months$prcp, na.rm=TRUE)), 
                                     (mean(saukcity1_obs_6months$tmax, na.rm=TRUE)), 
                                     (mean(saukcity1_obs_6months$tmin, na.rm=TRUE))), 
madison5_previousyear <- data.frame(wisconsin_csv$id[21], 
                                    (mean(madison5_obs_6months$prcp, na.rm=TRUE)), 
                                    (mean(madison5_obs_6months$tmax, na.rm=TRUE)), 
                                    (mean(madison5_obs_6months$tmin, na.rm=TRUE))), 
dodgeville1_previousyear <- data.frame(wisconsin_csv$id[22], 
                                       (mean(dodgeville1_obs_6months$prcp, na.rm=TRUE)), 
                                       (mean(dodgeville1_obs_6months$tmax, na.rm=TRUE)), 
                                       (mean(dodgeville1_obs_6months$tmin, na.rm=TRUE))), 
newlondon2_previousyear <- data.frame(wisconsin_csv$id[23], 
                                      (mean(newlondon2_obs_6months$prcp, na.rm=TRUE)), 
                                      (mean(newlondon2_obs_6months$tmax, na.rm=TRUE)), 
                                      (mean(newlondon2_obs_6months$tmin, na.rm=TRUE))), 
tworivers_previousyear <- data.frame(wisconsin_csv$id[24], 
                                     (mean(tworivers_obs_6months$prcp, na.rm=TRUE)), 
                                     (mean(tworivers_obs_6months$tmax, na.rm=TRUE)), 
                                     (mean(tworivers_obs_6months$tmin, na.rm=TRUE))), 
wisconsinrapids_previousyear <- data.frame(wisconsin_csv$id[25], 
                                           (mean(wisconsinrapids_obs_6months$prcp, na.rm=TRUE)), 
                                           (mean(wisconsinrapids_obs_6months$tmax, na.rm=TRUE)), 
                                           (mean(wisconsinrapids_obs_6months$tmin, na.rm=TRUE))), 
wausau_previousyear <- data.frame(wisconsin_csv$id[26], 
                                  (mean(wausau_obs_6months$prcp, na.rm=TRUE)), 
                                  (mean(wausau_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(wausau_obs_6months$tmin, na.rm=TRUE))), 
madison6_previousyear <- data.frame(wisconsin_csv$id[27], 
                                    (mean(madison6_obs_6months$prcp, na.rm=TRUE)), 
                                    (mean(madison6_obs_6months$tmax, na.rm=TRUE)), 
                                    (mean(madison6_obs_6months$tmin, na.rm=TRUE))), 
saukcity2_previousyear <- data.frame(wisconsin_csv$id[28], 
                                     (mean(saukcity2_obs_6months$prcp, na.rm=TRUE)), 
                                     (mean(saukcity2_obs_6months$tmax, na.rm=TRUE)), 
                                     (mean(saukcity2_obs_6months$tmin, na.rm=TRUE))), 
utica_previousyear <- data.frame(wisconsin_csv$id[29], 
                                 (mean(utica_obs_6months$prcp, na.rm=TRUE)), 
                                 (mean(utica_obs_6months$tmax, na.rm=TRUE)), 
                                 (mean(utica_obs_6months$tmin, na.rm=TRUE))), 
sturgeonbay_previousyear <- data.frame(wisconsin_csv$id[30], 
                                       (mean(sturgeonbay_obs_6months$prcp, na.rm=TRUE)), 
                                       (mean(sturgeonbay_obs_6months$tmax, na.rm=TRUE)), 
                                       (mean(sturgeonbay_obs_6months$tmin, na.rm=TRUE))), 
barron_previousyear <- data.frame(wisconsin_csv$id[31], 
                                  (mean(barron_obs_6months$prcp, na.rm=TRUE)), 
                                  (mean(barron_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(barron_obs_6months$tmin, na.rm=TRUE))), 
wetboro_previousyear <- data.frame(wisconsin_csv$id[32], 
                                   (mean(wetboro_obs_6months$prcp, na.rm=TRUE)), 
                                   (mean(wetboro_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(wetboro_obs_6months$tmin, na.rm=TRUE))), 
ithica_previousyear <- data.frame(wisconsin_csv$id[33], 
                                  (mean(ithica_obs_6months$prcp, na.rm=TRUE)), 
                                  (mean(ithica_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(ithica_obs_6months$tmin, na.rm=TRUE))), 
beloit_previousyear <- data.frame(wisconsin_csv$id[34], 
                                  (mean(beloit_obs_6months$prcp, na.rm=TRUE)), 
                                  (mean(beloit_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(beloit_obs_6months$tmin, na.rm=TRUE))), 
dodgeville2_previousyear <- data.frame(wisconsin_csv$id[35], 
                                       (mean(dodgeville2_obs_6months$prcp, na.rm=TRUE)), 
                                       (mean(dodgeville2_obs_6months$tmax, na.rm=TRUE)), 
                                       (mean(dodgeville2_obs_6months$tmin, na.rm=TRUE))), 
jefferson_previousyear <- data.frame(wisconsin_csv$id[36], 
                                     (mean(jefferson_obs_6months$prcp, na.rm=TRUE)), 
                                     (mean(jefferson_obs_6months$tmax, na.rm=TRUE)), 
                                     (mean(jefferson_obs_6months$tmin, na.rm=TRUE))), 
union_previousyear <- data.frame(wisconsin_csv$id[37], 
                                 (mean(union_obs_6months$prcp, na.rm=TRUE)), 
                                 (mean(union_obs_6months$tmax, na.rm=TRUE)), 
                                 (mean(union_obs_6months$tmin, na.rm=TRUE))), 
albany_previousyear <- data.frame(wisconsin_csv$id[38], 
                                  (mean(albany_obs_6months$prcp, na.rm=TRUE)), 
                                  (mean(albany_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(albany_obs_6months$tmin, na.rm=TRUE))), 
oshkosh_previousyear <- data.frame(wisconsin_csv$id[39], 
                                   (mean(oshkosh_obs_6months$prcp, na.rm=TRUE)), 
                                   (mean(oshkosh_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(oshkosh_obs_6months$tmin, na.rm=TRUE))), 
spirit_previousyear <- data.frame(wisconsin_csv$id[40], 
                                  (mean(spirit_obs_6months$prcp, na.rm=TRUE)), 
                                  (mean(spirit_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(spirit_obs_6months$tmin, na.rm=TRUE))), 
madison7_previousyear <- data.frame(wisconsin_csv$id[41], 
                                    (mean(madison7_obs_6months$prcp, na.rm=TRUE)), 
                                    (mean(madison7_obs_6months$tmax, na.rm=TRUE)), 
                                    (mean(madison7_obs_6months$tmin, na.rm=TRUE)))

#rename col.names 
#colnames(previousyear_x) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")

cat(citySwap3(city2, y = "colnames(", z = "_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')"), sep = "\n")

colnames(madison1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(madison2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(stevespoint_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(lacrosse_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(riverfalls_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(viroqua_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(newlondon1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(medford_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(cornell_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(lakemills_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(boscobel_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(waupan_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(madison4_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(wentbend_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(elkhom_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(waukesha_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(verona_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(saukcity1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(madison5_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(dodgeville1_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(newlondon2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tworivers_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(wisconsinrapids_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(wausau_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(madison6_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(saukcity2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(utica_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(sturgeonbay_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(barron_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(wetboro_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(ithica_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(beloit_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(dodgeville2_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(jefferson_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(union_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(albany_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(oshkosh_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(spirit_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(madison7_previousyear) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')

## combine previous year before the date of collection data using bind_rows
wisconsin_Pyear <- bind_rows(madison1_previousyear, 
                             madison2_previousyear, 
                             stevespoint_previousyear, 
                             lacrosse_previousyear, 
                             riverfalls_previousyear, 
                             viroqua_previousyear, 
                             newlondon1_previousyear, 
                             medford_previousyear, 
                             cornell_previousyear, 
                             lakemills_previousyear, 
                             boscobel_previousyear, 
                             waupan_previousyear, 
                             madison4_previousyear, 
                             wentbend_previousyear, 
                             elkhom_previousyear, 
                             waukesha_previousyear, 
                             verona_previousyear, 
                             saukcity1_previousyear, 
                             madison5_previousyear, 
                             dodgeville1_previousyear, 
                             newlondon2_previousyear, 
                             tworivers_previousyear, 
                             wisconsinrapids_previousyear, 
                             wausau_previousyear, 
                             madison6_previousyear, 
                             saukcity2_previousyear, 
                             utica_previousyear, 
                             sturgeonbay_previousyear, 
                             barron_previousyear, 
                             wetboro_previousyear, 
                             ithica_previousyear, 
                             beloit_previousyear, 
                             dodgeville2_previousyear, 
                             jefferson_previousyear, 
                             union_previousyear, 
                             albany_previousyear, 
                             oshkosh_previousyear, 
                             spirit_previousyear, 
                             madison7_previousyear)

# save data
write.xlsx(wisconsin_Pyear, file = "wisconsin_previous_year_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

## pull data for the current year of the date of collection
# general code: city_obs_yearDOC <- meteo_pull_monitors(city_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = " ", date_max = " " )
#x_obs_yearDOC <- meteo_pull_monitors(x_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = ' ', date_max = ' ')

years <- wisconsin_csv$year
df_list3 <- list(city, city2, years)

lapply(df_list3, cat(citySwap6(x = city2, y = "_obs_yearDOC <- meteo_pull_monitors(", j = city, 
                               z = "_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '", l = years , 
                               k = "-01-01', date_max = '",  p ="-12-31')"), sep = "\n"))

madison1_obs_yearDOC <- meteo_pull_monitors(madison_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1978-01-01', date_max = '1978-12-31')
madison2_obs_yearDOC <- meteo_pull_monitors(madison_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1978-01-01', date_max = '1978-12-31')
stevespoint_obs_yearDOC <- meteo_pull_monitors(stevespoint_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1981-01-01', date_max = '1981-12-31')
lacrosse_obs_yearDOC <- meteo_pull_monitors(lacrosse_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1974-01-01', date_max = '1974-12-31')
riverfalls_obs_yearDOC <- meteo_pull_monitors(riverfalls_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1972-01-01', date_max = '1972-12-31')
viroqua_obs_yearDOC <- meteo_pull_monitors(viroqua_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1956-01-01', date_max = '1956-12-31')
newlondon1_obs_yearDOC <- meteo_pull_monitors(newlondon_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1977-01-01', date_max = '1977-12-31')
brodhead_obs_yearDOC <- meteo_pull_monitors(brodhead_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1964-01-01', date_max = '1964-12-31')
medford_obs_yearDOC <- meteo_pull_monitors(medford_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1947-01-01', date_max = '1947-12-31')
cornell_obs_yearDOC <- meteo_pull_monitors(cornell_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1958-01-01', date_max = '1958-12-31')
lakemills_obs_yearDOC <- meteo_pull_monitors(lakemills_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1857-01-01', date_max = '1857-12-31')
boscobel_obs_yearDOC <- meteo_pull_monitors(boscobel_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1975-01-01', date_max = '1975-12-31')
waupan_obs_yearDOC <- meteo_pull_monitors(waupan_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1965-01-01', date_max = '1965-12-31')
madison4_obs_yearDOC <- meteo_pull_monitors(madison_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1877-01-01', date_max = '1877-12-31')
wentbend_obs_yearDOC <- meteo_pull_monitors(wentbend_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1969-01-01', date_max = '1969-12-31')
elkhom_obs_yearDOC <- meteo_pull_monitors(elkhom_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1938-01-01', date_max = '1938-12-31')
waukesha_obs_yearDOC <- meteo_pull_monitors(waukesha_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1959-01-01', date_max = '1959-12-31')
verona_obs_yearDOC <- meteo_pull_monitors(verona_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1998-01-01', date_max = '1998-12-31')
rockcounty_obs_yearDOC <- meteo_pull_monitors(rockcounty_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1889-01-01', date_max = '1889-12-31')
saukcity1_obs_yearDOC <- meteo_pull_monitors(saukcity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1960-01-01', date_max = '1960-12-31')
madison5_obs_yearDOC <- meteo_pull_monitors(madison_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1881-01-01', date_max = '1881-12-31')
dodgeville1_obs_yearDOC <- meteo_pull_monitors(dodgeville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1970-01-01', date_max = '1970-12-31')
newlondon2_obs_yearDOC <- meteo_pull_monitors(newlondon_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1955-01-01', date_max = '1955-12-31')
tworivers_obs_yearDOC <- meteo_pull_monitors(tworivers_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1964-01-01', date_max = '1964-12-31')
wisconsinrapids_obs_yearDOC <- meteo_pull_monitors(wisconsinrapids_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1891-01-01', date_max = '1891-12-31')
wausau_obs_yearDOC <- meteo_pull_monitors(wausau_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1961-01-01', date_max = '1961-12-31')
madison6_obs_yearDOC <- meteo_pull_monitors(madison_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1886-01-01', date_max = '1886-12-31')
saukcity2_obs_yearDOC <- meteo_pull_monitors(saukcity_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1937-01-01', date_max = '1937-12-31')
utica_obs_yearDOC <- meteo_pull_monitors(utica_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1956-01-01', date_max = '1956-12-31')
sturgeonbay_obs_yearDOC <- meteo_pull_monitors(sturgeonbay_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1959-01-01', date_max = '1959-12-31')
barron_obs_yearDOC <- meteo_pull_monitors(barron_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1888-01-01', date_max = '1888-12-31')
wetboro_obs_yearDOC <- meteo_pull_monitors(wetboro_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1993-01-01', date_max = '1993-12-31')
ithica_obs_yearDOC <- meteo_pull_monitors(ithica_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1969-01-01', date_max = '1969-12-31')
beloit_obs_yearDOC <- meteo_pull_monitors(beloit_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1932-01-01', date_max = '1932-12-31')
dodgeville2_obs_yearDOC <- meteo_pull_monitors(dodgeville_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1958-01-01', date_max = '1958-12-31')
jefferson_obs_yearDOC <- meteo_pull_monitors(jefferson_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1984-01-01', date_max = '1984-12-31')
union_obs_yearDOC <- meteo_pull_monitors(union_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1961-01-01', date_max = '1961-12-31')
albany_obs_yearDOC <- meteo_pull_monitors(albany_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1965-01-01', date_max = '1965-12-31')
oshkosh_obs_yearDOC <- meteo_pull_monitors(oshkosh_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1965-01-01', date_max = '1965-12-31')
spirit_obs_yearDOC <- meteo_pull_monitors(spirit_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1947-01-01', date_max = '1947-12-31')
madison7_obs_yearDOC <- meteo_pull_monitors(madison_monitors, var = c('TMAX', 'TMIN', 'PRCP'), date_min = '1953-01-01', date_max = '1953-12-31')

## make a dataframe of the mean of tmax, tmin, current year of the date of collection
#previousyearDOC_x <- data.frame(wisconsin_csv$id[], 
                                   #(mean(x_obs_yearDOC$prcp, na.rm=TRUE)), 
                                   #(mean(x_obs_yearDOC$tmax, na.rm=TRUE)), 
                                   #(mean(x_obs_yearDOC$tmin, na.rm=TRUE))) 


lapply(dflist_nodates, cat(citySwap2(x = city2,  y = "_DOC", z = " <- data.frame(wisconsin_csv$id[", l= lennumber, k = "], 
                                   (mean(", e = "_obs_6months$prcp, na.rm=TRUE)), 
                                   (mean(", f = "_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(", g = "_obs_6months$tmin, na.rm=TRUE))) "), sep = "\n"))
madison1_DOC <- data.frame(wisconsin_csv$id[1], 
                           (mean(madison1_obs_6months$prcp, na.rm=TRUE)), 
                           (mean(madison1_obs_6months$tmax, na.rm=TRUE)), 
                           (mean(madison1_obs_6months$tmin, na.rm=TRUE))) 
madison2_DOC <- data.frame(wisconsin_csv$id[2], 
                           (mean(madison2_obs_6months$prcp, na.rm=TRUE)), 
                           (mean(madison2_obs_6months$tmax, na.rm=TRUE)), 
                           (mean(madison2_obs_6months$tmin, na.rm=TRUE))) 
stevespoint_DOC <- data.frame(wisconsin_csv$id[3], 
                              (mean(stevespoint_obs_6months$prcp, na.rm=TRUE)), 
                              (mean(stevespoint_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(stevespoint_obs_6months$tmin, na.rm=TRUE))) 
lacrosse_DOC <- data.frame(wisconsin_csv$id[4], 
                           (mean(lacrosse_obs_6months$prcp, na.rm=TRUE)), 
                           (mean(lacrosse_obs_6months$tmax, na.rm=TRUE)), 
                           (mean(lacrosse_obs_6months$tmin, na.rm=TRUE))) 
riverfalls_DOC <- data.frame(wisconsin_csv$id[5], 
                             (mean(riverfalls_obs_6months$prcp, na.rm=TRUE)), 
                             (mean(riverfalls_obs_6months$tmax, na.rm=TRUE)), 
                             (mean(riverfalls_obs_6months$tmin, na.rm=TRUE))) 
viroqua_DOC <- data.frame(wisconsin_csv$id[6], 
                          (mean(viroqua_obs_6months$prcp, na.rm=TRUE)), 
                          (mean(viroqua_obs_6months$tmax, na.rm=TRUE)), 
                          (mean(viroqua_obs_6months$tmin, na.rm=TRUE))) 
newlondon1_DOC <- data.frame(wisconsin_csv$id[7], 
                             (mean(newlondon1_obs_6months$prcp, na.rm=TRUE)), 
                             (mean(newlondon1_obs_6months$tmax, na.rm=TRUE)), 
                             (mean(newlondon1_obs_6months$tmin, na.rm=TRUE))) 
medford_DOC <- data.frame(wisconsin_csv$id[9], 
                          (mean(medford_obs_6months$prcp, na.rm=TRUE)), 
                          (mean(medford_obs_6months$tmax, na.rm=TRUE)), 
                          (mean(medford_obs_6months$tmin, na.rm=TRUE))) 
cornell_DOC <- data.frame(wisconsin_csv$id[10], 
                          (mean(cornell_obs_6months$prcp, na.rm=TRUE)), 
                          (mean(cornell_obs_6months$tmax, na.rm=TRUE)), 
                          (mean(cornell_obs_6months$tmin, na.rm=TRUE))) 
lakemills_DOC <- data.frame(wisconsin_csv$id[11], 
                            (mean(lakemills_obs_6months$prcp, na.rm=TRUE)), 
                            (mean(lakemills_obs_6months$tmax, na.rm=TRUE)), 
                            (mean(lakemills_obs_6months$tmin, na.rm=TRUE))) 
boscobel_DOC <- data.frame(wisconsin_csv$id[12], 
                           (mean(boscobel_obs_6months$prcp, na.rm=TRUE)), 
                           (mean(boscobel_obs_6months$tmax, na.rm=TRUE)), 
                           (mean(boscobel_obs_6months$tmin, na.rm=TRUE))) 
waupan_DOC <- data.frame(wisconsin_csv$id[13], 
                         (mean(waupan_obs_6months$prcp, na.rm=TRUE)), 
                         (mean(waupan_obs_6months$tmax, na.rm=TRUE)), 
                         (mean(waupan_obs_6months$tmin, na.rm=TRUE))) 
madison4_DOC <- data.frame(wisconsin_csv$id[14], 
                           (mean(madison4_obs_6months$prcp, na.rm=TRUE)), 
                           (mean(madison4_obs_6months$tmax, na.rm=TRUE)), 
                           (mean(madison4_obs_6months$tmin, na.rm=TRUE))) 
wentbend_DOC <- data.frame(wisconsin_csv$id[15], 
                           (mean(wentbend_obs_6months$prcp, na.rm=TRUE)), 
                           (mean(wentbend_obs_6months$tmax, na.rm=TRUE)), 
                           (mean(wentbend_obs_6months$tmin, na.rm=TRUE))) 
elkhom_DOC <- data.frame(wisconsin_csv$id[16], 
                         (mean(elkhom_obs_6months$prcp, na.rm=TRUE)), 
                         (mean(elkhom_obs_6months$tmax, na.rm=TRUE)), 
                         (mean(elkhom_obs_6months$tmin, na.rm=TRUE))) 
waukesha_DOC <- data.frame(wisconsin_csv$id[17], 
                           (mean(waukesha_obs_6months$prcp, na.rm=TRUE)), 
                           (mean(waukesha_obs_6months$tmax, na.rm=TRUE)), 
                           (mean(waukesha_obs_6months$tmin, na.rm=TRUE))) 
verona_DOC <- data.frame(wisconsin_csv$id[18], 
                         (mean(verona_obs_6months$prcp, na.rm=TRUE)), 
                         (mean(verona_obs_6months$tmax, na.rm=TRUE)), 
                         (mean(verona_obs_6months$tmin, na.rm=TRUE))) 
saukcity1_DOC <- data.frame(wisconsin_csv$id[20], 
                            (mean(saukcity1_obs_6months$prcp, na.rm=TRUE)), 
                            (mean(saukcity1_obs_6months$tmax, na.rm=TRUE)), 
                            (mean(saukcity1_obs_6months$tmin, na.rm=TRUE))) 
madison5_DOC <- data.frame(wisconsin_csv$id[21], 
                           (mean(madison5_obs_6months$prcp, na.rm=TRUE)), 
                           (mean(madison5_obs_6months$tmax, na.rm=TRUE)), 
                           (mean(madison5_obs_6months$tmin, na.rm=TRUE))) 
dodgeville1_DOC <- data.frame(wisconsin_csv$id[22], 
                              (mean(dodgeville1_obs_6months$prcp, na.rm=TRUE)), 
                              (mean(dodgeville1_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(dodgeville1_obs_6months$tmin, na.rm=TRUE))) 
newlondon2_DOC <- data.frame(wisconsin_csv$id[23], 
                             (mean(newlondon2_obs_6months$prcp, na.rm=TRUE)), 
                             (mean(newlondon2_obs_6months$tmax, na.rm=TRUE)), 
                             (mean(newlondon2_obs_6months$tmin, na.rm=TRUE))) 
tworivers_DOC <- data.frame(wisconsin_csv$id[24], 
                            (mean(tworivers_obs_6months$prcp, na.rm=TRUE)), 
                            (mean(tworivers_obs_6months$tmax, na.rm=TRUE)), 
                            (mean(tworivers_obs_6months$tmin, na.rm=TRUE))) 
wisconsinrapids_DOC <- data.frame(wisconsin_csv$id[25], 
                                  (mean(wisconsinrapids_obs_6months$prcp, na.rm=TRUE)), 
                                  (mean(wisconsinrapids_obs_6months$tmax, na.rm=TRUE)), 
                                  (mean(wisconsinrapids_obs_6months$tmin, na.rm=TRUE))) 
wausau_DOC <- data.frame(wisconsin_csv$id[26], 
                         (mean(wausau_obs_6months$prcp, na.rm=TRUE)), 
                         (mean(wausau_obs_6months$tmax, na.rm=TRUE)), 
                         (mean(wausau_obs_6months$tmin, na.rm=TRUE))) 
madison6_DOC <- data.frame(wisconsin_csv$id[27], 
                           (mean(madison6_obs_6months$prcp, na.rm=TRUE)), 
                           (mean(madison6_obs_6months$tmax, na.rm=TRUE)), 
                           (mean(madison6_obs_6months$tmin, na.rm=TRUE))) 
saukcity2_DOC <- data.frame(wisconsin_csv$id[28], 
                            (mean(saukcity2_obs_6months$prcp, na.rm=TRUE)), 
                            (mean(saukcity2_obs_6months$tmax, na.rm=TRUE)), 
                            (mean(saukcity2_obs_6months$tmin, na.rm=TRUE))) 
utica_DOC <- data.frame(wisconsin_csv$id[29], 
                        (mean(utica_obs_6months$prcp, na.rm=TRUE)), 
                        (mean(utica_obs_6months$tmax, na.rm=TRUE)), 
                        (mean(utica_obs_6months$tmin, na.rm=TRUE))) 
sturgeonbay_DOC <- data.frame(wisconsin_csv$id[30], 
                              (mean(sturgeonbay_obs_6months$prcp, na.rm=TRUE)), 
                              (mean(sturgeonbay_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(sturgeonbay_obs_6months$tmin, na.rm=TRUE))) 
barron_DOC <- data.frame(wisconsin_csv$id[31], 
                         (mean(barron_obs_6months$prcp, na.rm=TRUE)), 
                         (mean(barron_obs_6months$tmax, na.rm=TRUE)), 
                         (mean(barron_obs_6months$tmin, na.rm=TRUE))) 
wetboro_DOC <- data.frame(wisconsin_csv$id[32], 
                          (mean(wetboro_obs_6months$prcp, na.rm=TRUE)), 
                          (mean(wetboro_obs_6months$tmax, na.rm=TRUE)), 
                          (mean(wetboro_obs_6months$tmin, na.rm=TRUE))) 
ithica_DOC <- data.frame(wisconsin_csv$id[33], 
                         (mean(ithica_obs_6months$prcp, na.rm=TRUE)), 
                         (mean(ithica_obs_6months$tmax, na.rm=TRUE)), 
                         (mean(ithica_obs_6months$tmin, na.rm=TRUE))) 
beloit_DOC <- data.frame(wisconsin_csv$id[34], 
                         (mean(beloit_obs_6months$prcp, na.rm=TRUE)), 
                         (mean(beloit_obs_6months$tmax, na.rm=TRUE)), 
                         (mean(beloit_obs_6months$tmin, na.rm=TRUE))) 
dodgeville2_DOC <- data.frame(wisconsin_csv$id[35], 
                              (mean(dodgeville2_obs_6months$prcp, na.rm=TRUE)), 
                              (mean(dodgeville2_obs_6months$tmax, na.rm=TRUE)), 
                              (mean(dodgeville2_obs_6months$tmin, na.rm=TRUE))) 
jefferson_DOC <- data.frame(wisconsin_csv$id[36], 
                            (mean(jefferson_obs_6months$prcp, na.rm=TRUE)), 
                            (mean(jefferson_obs_6months$tmax, na.rm=TRUE)), 
                            (mean(jefferson_obs_6months$tmin, na.rm=TRUE))) 
union_DOC <- data.frame(wisconsin_csv$id[37], 
                        (mean(union_obs_6months$prcp, na.rm=TRUE)), 
                        (mean(union_obs_6months$tmax, na.rm=TRUE)), 
                        (mean(union_obs_6months$tmin, na.rm=TRUE))) 
albany_DOC <- data.frame(wisconsin_csv$id[38], 
                         (mean(albany_obs_6months$prcp, na.rm=TRUE)), 
                         (mean(albany_obs_6months$tmax, na.rm=TRUE)), 
                         (mean(albany_obs_6months$tmin, na.rm=TRUE))) 
oshkosh_DOC <- data.frame(wisconsin_csv$id[39], 
                          (mean(oshkosh_obs_6months$prcp, na.rm=TRUE)), 
                          (mean(oshkosh_obs_6months$tmax, na.rm=TRUE)), 
                          (mean(oshkosh_obs_6months$tmin, na.rm=TRUE))) 
spirit_DOC <- data.frame(wisconsin_csv$id[40], 
                         (mean(spirit_obs_6months$prcp, na.rm=TRUE)), 
                         (mean(spirit_obs_6months$tmax, na.rm=TRUE)), 
                         (mean(spirit_obs_6months$tmin, na.rm=TRUE))) 
madison7_DOC <- data.frame(wisconsin_csv$id[41], 
                           (mean(madison7_obs_6months$prcp, na.rm=TRUE)), 
                           (mean(madison7_obs_6months$tmax, na.rm=TRUE)), 
                           (mean(madison7_obs_6months$tmin, na.rm=TRUE)))

#rename col.names 
#colnames(previousyearDOC_x) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')

cat(citySwap3(city2, y = "colnames(", z = "_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')"), sep = "\n")

colnames(madison1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(madison2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(stevespoint_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(lacrosse_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(riverfalls_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(viroqua_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(newlondon1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(medford_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(cornell_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(lakemills_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(boscobel_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(waupan_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(madison4_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(wentbend_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(elkhom_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(waukesha_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(verona_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(saukcity1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(madison5_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(dodgeville1_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(newlondon2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(tworivers_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(wisconsinrapids_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(wausau_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(madison6_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(saukcity2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(utica_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(sturgeonbay_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(barron_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(wetboro_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(ithica_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(beloit_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(dodgeville2_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(jefferson_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(union_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(albany_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(oshkosh_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(spirit_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(madison7_DOC) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')

##paste into bind_rows()
citySwap7 = function(x, y, z){
  paste(x, y, z, sep = "")
}

## combine current year of the date of collection data using bind_rows
cat(citySwap7(city2, y = "_DOC", z = ","), sep = "")
wisconsin_yearDOC <- bind_rows(madison1_DOC,madison2_DOC,stevespoint_DOC,lacrosse_DOC,
                           riverfalls_DOC,viroqua_DOC,newlondon1_DOC,medford_DOC,cornell_DOC,lakemills_DOC,
                           boscobel_DOC,waupan_DOC,madison4_DOC,wentbend_DOC,elkhom_DOC,waukesha_DOC,
                           verona_DOC,saukcity1_DOC,madison5_DOC,dodgeville1_DOC,newlondon2_DOC,tworivers_DOC,
                           wisconsinrapids_DOC,wausau_DOC,madison6_DOC,saukcity2_DOC,utica_DOC,sturgeonbay_DOC,
                           barron_DOC,wetboro_DOC,ithica_DOC,beloit_DOC,dodgeville2_DOC,jefferson_DOC,union_DOC,
                           albany_DOC,oshkosh_DOC,spirit_DOC,madison7_DOC)


write.xlsx(wisconsin_yearDOC, file = "wisconsin_year_DOC_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
