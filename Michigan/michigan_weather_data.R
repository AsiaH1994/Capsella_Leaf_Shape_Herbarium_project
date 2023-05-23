#rm(list=ls())
library(FluMoDL)
library(dplyr)
library(fuzzyjoin)
library(weatherData)
library(rnoaa)
options(noaakey = "ifwilTHTRMxSsheNajCJqXNJTFbjAaBk")
library(rgdal)
setwd("~/Documents")

######### michgian samples - add date in y-m-d format 
michigan_csv <- read.csv("michigan_samples.csv")
michigan_csv$month <- match(michigan_csv$month, month.name)
michigan_csv$date <- paste(michigan_csv$year, michigan_csv$month, michigan_csv$day, sep="-")
strptime(michigan_csv$date,format="%Y-%m-%d")
michigan_csv$date <- as.Date(michigan_csv$date)
head(michigan_csv)
colnames(michigan_csv) <- c("id", 'city_county_state', 'latitude', 'longitude', 'elevation', 'month', 'day', 'year', 'date')

##NOAA stations 
stations <- ghcnd_stations()
great_lakes_stations <- stations %>% filter(state == c("MI", "OH", "IL", "IN", "WI"))

### closests stations 
great_lakes <- meteo_nearby_stations(lat_lon_df = michigan_csv, station_data = great_lakes_stations, radius = 50)

##### 1 city
albion <- (great_lakes$Albion)

#### pull monitors for 1 city 
albion_monitors <- albion$id

#### pull observations   
albion_obs <- meteo_pull_monitors(albion_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "1910-01-01")
albion_test <- albion_obs %>% filter(date == "1927-5-3") #successful 

### separate all cities  
alpena <- (great_lakes$Alpena)
annarbor <- (great_lakes$`Ann Arbor`)
beaverisland <- (great_lakes$`Beaver Island`)
bigrapids <- (great_lakes$`Big Rapids`)
charlevoixcounty <- (great_lakes$`Charlevoix County`)
eastlansing <- (great_lakes$`East Lansing`)
fremont <- (great_lakes$Fremont)
grandrapids <- (great_lakes$`Grand Rapids`)
hart <- (great_lakes$Hart)
hudsonville <- (great_lakes$Hudsonville)
kalamazoo <- (great_lakes$Kalamazoo)
mtpleasant <- (great_lakes$`Mt. Pleasant`)
muskegon <- (great_lakes$Muskegon)
otsego <- (great_lakes$Otsego)
porthuron <- (great_lakes$`Port Huron`)
richmond <- (great_lakes$Richmond)
springfield <- (great_lakes$Springfield)
tawascity <- (great_lakes$`Tawas City`)
williamston <- (great_lakes$Williamston)
wyoming <- (great_lakes$Wyoming)
ypsilanti <- (great_lakes$Ypsilanti)

#### monitors for all cities 
alpena_monitors <- alpena$id
annarbor_monitors <- annarbor$id
beaverisland_monitors <- beaverisland$id
bigrapids_monitors <- bigrapids$id
charlevoixcounty_monitors <- charlevoixcounty$id
eastlansing_monitors <- eastlansing$id
fremont_monitors <- fremont$id
grandrapids_monitors <- grandrapids$id
hart_monitors <- hart$id
hudsonville_monitors <- hudsonville$id
kalamazoo_monitors <- kalamazoo$id
mtpleasant_monitors <- mtpleasant$id
muskegon_monitors <- muskegon$id
otsego_monitors <- otsego$id
porthuron_monitors <- porthuron$id
richmond_monitors <- richmond$id
springfield_monitors <- springfield$id
tawascity_monitors <- tawascity$id
williamston_monitors <- williamston$id
wyoming_monitors <- wyoming$id
ypsilanti_monitors <- ypsilanti$id

### pull all observations 
alpena_obs <- meteo_pull_monitors(alpena_monitors, var = c("TMAX", "TMIN", "TAVG", "PRCP"), date_min = "1910-01-01")
annarbor_obs <- meteo_pull_monitors(annarbor_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "1910-01-01")
beaverisland_obs <- meteo_pull_monitors(beaverisland_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "1910-01-01")
bigrapids_obs <- meteo_pull_monitors(bigrapids_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "1910-01-01")
charlevoixcounty_obs <- meteo_pull_monitors(charlevoixcounty_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "1910-01-01")
eastlansing_obs <- meteo_pull_monitors(eastlansing_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "1910-01-01")
fremont_obs <- meteo_pull_monitors(fremont_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "1910-01-01")
grandrapids_obs <- meteo_pull_monitors(grandrapids_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "1910-01-01")
hart_obs <- meteo_pull_monitors(hart_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "1910-01-01")
hudsonville_obs <- meteo_pull_monitors(hudsonville_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "1910-01-01")
kalamazoo_obs <- meteo_pull_monitors(kalamazoo_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "1910-01-01")
mtpleasant_obs <- meteo_pull_monitors(mtpleasant_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "1910-01-01")
muskegon_obs <- meteo_pull_monitors(muskegon_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "1910-01-01")
otsego_obs <- meteo_pull_monitors(otsego_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "1910-01-01")
porthuron_obs <- meteo_pull_monitors(porthuron_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "1910-01-01")
richmond_obs <- meteo_pull_monitors(richmond_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "1910-01-01")
springfield_obs <- meteo_pull_monitors(springfield_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "1910-01-01")
tawascity_obs <- meteo_pull_monitors(tawascity_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "1910-01-01")
williamston_obs <- meteo_pull_monitors(williamston_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "1910-01-01")
wyoming_obs <- meteo_pull_monitors(wyoming_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "1910-01-01")
ypsilanti_obs <- meteo_pull_monitors(ypsilanti_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "1910-01-01")

### spot check
aplena_test <- alpena_obs %>% filter(date == "1972-5-26") # successful 
 
### pull data by date and location - dates are coming from michigan_csv 
albion_obs_temps <- albion_obs %>% filter(date == "1926-11-30")
alpena_temps <- alpena_obs %>% filter(date == "1971-11-30")
annarbor_temps <- annarbor_obs %>% filter(date == "1861-5-7") # not here - expected 
beaverisland_temps <- beaverisland_obs %>% filter(date == "1958-6-26")
bigrapids_temps1 <- bigrapids_obs %>% filter(date == "1998-5-4")
bigrapids_temps2 <- bigrapids_obs %>% filter(date == "2002-5-11")
charlevoixcounty_temps <- charlevoixcounty_obs %>% filter(date == "1985-10-5")
eastlansing_temps <- eastlansing_obs %>% filter(date == "1938-5-13")
fremont_temps <- fremont_obs %>% filter(date == "2006-5-1")
grandrapids_temps1 <- grandrapids_obs %>% filter(date == "1975-5-9")
grandrapids_temps2 <- grandrapids_obs %>% filter(date == "1965-5-17")
grandrapids_temps3 <- grandrapids_obs %>% filter(date == "1972-5-16")
grandrapids_temps4 <- grandrapids_obs %>% filter(date == "1967-5-15")
hart_temps <- hart_obs %>% filter(date == "2003-5-26")
hudsonville_temps1 <- hudsonville_obs %>% filter(date == "1954-5-7")
hudsonville_temps2 <- hudsonville_obs %>% filter(date == "1967-5-17")
kalamazoo_temps <- kalamazoo_obs %>% filter(date == "1973-4-8")
mtpleasant_temps <- mtpleasant_obs %>% filter(date == "1997-9-18")
muskegon_temps <- muskegon_obs %>% filter(date == "1976-5-8")
otsego_temps <- otsego_obs %>% filter(date == "2017-6-11")
porthuron_temps <- porthuron_obs %>% filter(date == "1892-6-20")
richmond_temps <- richmond_obs %>% filter(date == "1958-6-18")
springfield_temps <- springfield_obs %>% filter(date == "1994-4-27")
tawascity_temps <- tawascity_obs %>% filter(date == "1992-5-12")
williamston_temps <- williamston_obs %>% filter(date == "2021-9-26")
wyoming_temps <- wyoming_obs %>% filter(date == "1966-5-22")
ypsilanti_temps <- ypsilanti_obs %>% filter(date == "1916-5-17")

###combine into 1 dataframe
df1 <- bind_rows(annarbor_temps, beaverisland_temps, bigrapids_temps1) ## this works so do with all df's 
michigan_obs <- bind_rows(albion_obs_temps, alpena_temps, annarbor_temps, beaverisland_temps, bigrapids_temps1, bigrapids_temps2, 
                          charlevoixcounty_temps, eastlansing_temps, fremont_temps, grandrapids_temps1, 
                          grandrapids_temps2, grandrapids_temps3, grandrapids_temps4, hart_temps, 
                          hudsonville_temps1, hudsonville_temps2, kalamazoo_temps, mtpleasant_temps, muskegon_temps, 
                          otsego_temps, porthuron_temps, richmond_temps, springfield_temps, tawascity_temps, 
                          williamston_temps, wyoming_temps, ypsilanti_temps)

#### merge with original michigan_csv - merging my id is not working 
#bind_rows = Can't combine `..1$date` <character> and `..2$date` <date>.
michigan_data <- merge.data.frame(michigan_csv, michigan_obs, by = 'date')

### writing to excel 
install.packages("xlsx")
library("xlsx")
write.xlsx(michigan_data, file = "michigan_data_total_weather.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
#write.csv(Michigan_data, file = "michigan_data.csv", row.names = TRUE)
## this worked. needs to be cleaned up but can be done in excel 

#### finding tavg 
michigan_data$tavg <- ((michigan_data$tmax + michigan_data$tmin) / 2)
###TMAX is measured in tenths of degrees Celcius.
##this is data for the collection date. Next is to find data for the year (DOC), previous year, and 6 months before date of collection. 

### yearly data
gsoy_data<- ncdc_datatypes('GSOY') # this is not what I need 

#data for previous year of collection 
beaverisland_obs_year <- meteo_pull_monitors(beaverisland_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "1957-06-26", date_max = "1958-06-26" )
##this works, data is not missing - duplicate data includes NAs. 1 station had a whole years data. 
##find the mean of each value as a list. 
previousyear_beaverisland <- data.frame(michigan_csv$id[16], (mean(beaverisland_obs_year$prcp, na.rm=TRUE)), (mean(beaverisland_obs_year$tmax, na.rm=TRUE)), 
                          (mean(beaverisland_obs_year$tmin, na.rm=TRUE))) 
colnames(previousyear_beaverisland) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")

### to get previous 6 months in dataframe 
install.packages("zoo")
library(zoo)
michigan_data$previous6mon <- as.Date(as.yearmon(as.Date(michigan_data$date)) -.5, frac = 1)
##also do this in orginal csv for ease of use 
michigan_csv$previous6mon <- as.Date(as.yearmon(as.Date(michigan_csv$date)) -.5, frac = 1)

###finding data for previous 6 months 
beaverisland_obs_6months <- meteo_pull_monitors(beaverisland_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "1957-12-31", date_max = "1958-06-26" )

###find data for all for previous year
albion_obs_year <- meteo_pull_monitors(albion_monitors, var = c("TMAX", "TMIN", "PRCP"), 
                                       date_min = "1926-05-03", date_max = "1927-05-03" )
alpena_obs_year <- meteo_pull_monitors(alpena_monitors, var = c("TMAX", "TMIN", "PRCP"), 
                                       date_min = "1970-11-30", date_max = "1971-11-30" )
bigrapids1_obs_year <- meteo_pull_monitors(bigrapids_monitors, var = c("TMAX", "TMIN", "PRCP"), 
                                           date_min = "1997-5-4", date_max = "1998-5-4" )
bigrapids2_obs_year <- meteo_pull_monitors(bigrapids_monitors, var = c("TMAX", "TMIN", "PRCP"), 
                                           date_min = "2001-5-11", date_max = "2002-5-11" )
charlevoixcounty_obs_year <- meteo_pull_monitors(charlevoixcounty_monitors, var = c("TMAX", "TMIN", "PRCP"), 
                                                 date_min = "1984-10-5", date_max = "1985-10-5" )
eastlansing_obs_year <- meteo_pull_monitors(eastlansing_monitors, var = c("TMAX", "TMIN", "PRCP"), 
                                            date_min = "1937-5-13", date_max = "1938-5-13" )
fremont_obs_year <- meteo_pull_monitors(fremont_monitors, var = c("TMAX", "TMIN", "PRCP"), 
                                        date_min = "2005-5-1", date_max = "2006-5-1" )
grandrapids1_obs_year <- meteo_pull_monitors(grandrapids_monitors, var = c("TMAX", "TMIN", "PRCP"), 
                                             date_min = "1974-5-9", date_max = "1975-5-9" )
grandrapids2_obs_year <- meteo_pull_monitors(grandrapids_monitors, var = c("TMAX", "TMIN", "PRCP"), 
                                             date_min = "1964-5-17", date_max = "1965-5-17" )
grandrapids3_obs_year <- meteo_pull_monitors(grandrapids_monitors, var = c("TMAX", "TMIN", "PRCP"), 
                                             date_min = "1971-5-16", date_max = "1972-5-16" )
grandrapids4_obs_year <- meteo_pull_monitors(grandrapids_monitors, var = c("TMAX", "TMIN", "PRCP"), 
                                             date_min = "1966-5-15", date_max = "1967-5-15" )
hart_obs_year <- meteo_pull_monitors(hart_monitors, var = c("TMAX", "TMIN", "PRCP"), 
                                     date_min = "2002-5-26", date_max = "2003-5-26" )
hudsonville1_obs_year <- meteo_pull_monitors(hudsonville_monitors, var = c("TMAX", "TMIN", "PRCP"), 
                                             date_min = "1953-5-7", date_max = "1954-5-7" )
hudsonville2_obs_year <- meteo_pull_monitors(hudsonville_monitors, var = c("TMAX", "TMIN", "PRCP"), 
                                             date_min = "1966-5-17", date_max = "1967-5-17" )
kalamazoo_obs_year <- meteo_pull_monitors(kalamazoo_monitors, var = c("TMAX", "TMIN", "PRCP"), 
                                          date_min = "1972-4-8", date_max = "1973-4-8" )
mtpleasant_obs_year <- meteo_pull_monitors(mtpleasant_monitors, var = c("TMAX", "TMIN", "PRCP"), 
                                           date_min = "1996-9-18", date_max = "1997-9-18" )
muskegon_obs_year <- meteo_pull_monitors(muskegon_monitors, var = c("TMAX", "TMIN", "PRCP"), 
                                         date_min = "1975-5-8", date_max = "1976-5-8" )
otsego_obs_year <- meteo_pull_monitors(otsego_monitors, var = c("TMAX", "TMIN", "PRCP"), 
                                       date_min = "2016-6-11", date_max = "2017-6-11" )
richmond_obs_year <- meteo_pull_monitors(richmond_monitors, var = c("TMAX", "TMIN", "PRCP"), 
                                         date_min = "1957-6-18", date_max = "1958-6-18" )
springfield_obs_year <- meteo_pull_monitors(springfield_monitors, var = c("TMAX", "TMIN", "PRCP"), 
                                            date_min = "1993-4-27", date_max = "1994-4-27" )
tawascity_obs_year <- meteo_pull_monitors(tawascity_monitors, var = c("TMAX", "TMIN", "PRCP"), 
                                          date_min = "1991-5-12", date_max = "1992-5-12" )
williamston_obs_year <- meteo_pull_monitors(williamston_monitors, var = c("TMAX", "TMIN", "PRCP"), 
                                            date_min = "2020-9-26", date_max = "2021-9-26" )
wyoming_obs_year <- meteo_pull_monitors(wyoming_monitors, var = c("TMAX", "TMIN", "PRCP"), 
                                        date_min = "1965-5-22", date_max = "1966-5-22" )
ypsilanti_obs_year <- meteo_pull_monitors(ypsilanti_monitors, var = c("TMAX", "TMIN", "PRCP"), 
                                          date_min = "1915-5-17", date_max = "1916-5-17" )

##### making data frames of previous year data 
previousyear_albion <- data.frame(michigan_csv$id[8], 
                                  (mean(albion_obs_year$prcp, na.rm=TRUE)), 
                                  (mean(albion_obs_year$tmax, na.rm=TRUE)), 
                                        (mean(albion_obs_year$tmin, na.rm=TRUE))) 
colnames(previousyear_albion) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")
previousyear_alpena <- data.frame(michigan_csv$id[24], 
                                  (mean(alpena_obs_year$prcp, na.rm=TRUE)), 
                                  (mean(alpena_obs_year$tmax, na.rm=TRUE)), 
                                  (mean(alpena_obs_year$tmin, na.rm=TRUE)))
colnames(previousyear_alpena) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")
previousyear_bigrapids1 <- data.frame(michigan_csv$id[21], 
                                      (mean(bigrapids1_obs_year$prcp, na.rm=TRUE)), 
                                      (mean(bigrapids1_obs_year$tmax, na.rm=TRUE)), 
                                  (mean(bigrapids1_obs_year$tmin, na.rm=TRUE)))
colnames(previousyear_bigrapids1) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")
previousyear_bigrapids2 <- data.frame(michigan_csv$id[22], 
                                      (mean(bigrapids2_obs_year$prcp, na.rm=TRUE)), 
                                      (mean(bigrapids2_obs_year$tmax, na.rm=TRUE)), 
                                  (mean(bigrapids2_obs_year$tmin, na.rm=TRUE)))
colnames(previousyear_bigrapids2) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")
previousyear_charlevoixcounty <- data.frame(michigan_csv$id[5], 
                                            (mean(charlevoixcounty_obs_year$prcp, na.rm=TRUE)), 
                                            (mean(charlevoixcounty_obs_year$tmax, na.rm=TRUE)), 
                                  (mean(charlevoixcounty_obs_year$tmin, na.rm=TRUE)))
colnames(previousyear_charlevoixcounty) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")
previousyear_eastlansing <- data.frame(michigan_csv$id[20], 
                                       (mean(eastlansing_obs_year$prcp, na.rm=TRUE)), 
                                       (mean(eastlansing_obs_year$tmax, na.rm=TRUE)), 
                                  (mean(eastlansing_obs_year$tmin, na.rm=TRUE)))
colnames(previousyear_eastlansing) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")
previousyear_fremont <- data.frame(michigan_csv$id[12], 
                                   (mean(fremont_obs_year$prcp, na.rm=TRUE)), 
                                   (mean(fremont_obs_year$tmax, na.rm=TRUE)), 
                                       (mean(fremont_obs_year$tmin, na.rm=TRUE)))
colnames(previousyear_fremont) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")
previousyear_grandrapids1 <- data.frame(michigan_csv$id[9], 
                                        (mean(grandrapids1_obs_year$prcp, na.rm=TRUE)), 
                                        (mean(grandrapids1_obs_year$tmax, na.rm=TRUE)), 
                                       (mean(grandrapids1_obs_year$tmin, na.rm=TRUE)))
colnames(previousyear_grandrapids1) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")
previousyear_grandrapids2 <- data.frame(michigan_csv$id[13], 
                                        (mean(grandrapids2_obs_year$prcp, na.rm=TRUE)), 
                                        (mean(grandrapids2_obs_year$tmax, na.rm=TRUE)), 
                                       (mean(grandrapids2_obs_year$tmin, na.rm=TRUE)))
colnames(previousyear_grandrapids2) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")
previousyear_grandrapids3 <- data.frame(michigan_csv$id[14], 
                                        (mean(grandrapids3_obs_year$prcp, na.rm=TRUE)),
                                        (mean(grandrapids3_obs_year$tmax, na.rm=TRUE)), 
                                       (mean(grandrapids3_obs_year$tmin, na.rm=TRUE)))
colnames(previousyear_grandrapids3) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")
previousyear_grandrapids4 <- data.frame(michigan_csv$id[15], 
                                        (mean(grandrapids4_obs_year$prcp, na.rm=TRUE)), 
                                        (mean(grandrapids4_obs_year$tmax, na.rm=TRUE)), 
                                       (mean(grandrapids4_obs_year$tmin, na.rm=TRUE)))
colnames(previousyear_grandrapids4) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")
previousyear_hart <- data.frame(michigan_csv$id[1], 
                                        (mean(hart_obs_year$prcp, na.rm=TRUE)), 
                                        (mean(hart_obs_year$tmax, na.rm=TRUE)), 
                                        (mean(hart_obs_year$tmin, na.rm=TRUE)))
colnames(previousyear_hart) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")
previousyear_hudsonville1 <- data.frame(michigan_csv$id[2], 
                                        (mean(hudsonville1_obs_year$prcp, na.rm=TRUE)), 
                                        (mean(hudsonville1_obs_year$tmax, na.rm=TRUE)), 
                                        (mean(hudsonville1_obs_year$tmin, na.rm=TRUE)))
colnames(previousyear_hudsonville1) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")
previousyear_hudsonville2 <- data.frame(michigan_csv$id[4], 
                                        (mean(hudsonville2_obs_year$prcp, na.rm=TRUE)), 
                                        (mean(hudsonville2_obs_year$tmax, na.rm=TRUE)), 
                                        (mean(hudsonville2_obs_year$tmin, na.rm=TRUE)))
colnames(previousyear_hudsonville2) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")
previousyear_kalamazoo <- data.frame(michigan_csv$id[10],
                                     (mean(kalamazoo_obs_year$prcp, na.rm=TRUE)), 
                                     (mean(kalamazoo_obs_year$tmax, na.rm=TRUE)), 
                                        (mean(kalamazoo_obs_year$tmin, na.rm=TRUE)))
colnames(previousyear_kalamazoo) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")
previousyear_mtpleasant <- data.frame(michigan_csv$id[3], 
                                      (mean(mtpleasant_obs_year$prcp, na.rm=TRUE)), 
                                      (mean(mtpleasant_obs_year$tmax, na.rm=TRUE)), 
                                        (mean(mtpleasant_obs_year$tmin, na.rm=TRUE)))
colnames(previousyear_mtpleasant) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")
previousyear_muskegon <- data.frame(michigan_csv$id[7], 
                                    (mean(muskegon_obs_year$prcp, na.rm=TRUE)), 
                                    (mean(muskegon_obs_year$tmax, na.rm=TRUE)), 
                                      (mean(muskegon_obs_year$tmin, na.rm=TRUE)))
colnames(previousyear_muskegon) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")
previousyear_otsego <- data.frame(michigan_csv$id[27], 
                                  (mean(otsego_obs_year$prcp, na.rm=TRUE)), 
                                  (mean(otsego_obs_year$tmax, na.rm=TRUE)), 
                                      (mean(otsego_obs_year$tmin, na.rm=TRUE)))
colnames(previousyear_otsego) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")
previousyear_richmond <- data.frame(michigan_csv$id[17], 
                                    (mean(richmond_obs_year$prcp, na.rm=TRUE)), 
                                    (mean(richmond_obs_year$tmax, na.rm=TRUE)), 
                                      (mean(richmond_obs_year$tmin, na.rm=TRUE)))
colnames(previousyear_richmond) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")
previousyear_springfield <- data.frame(michigan_csv$id[25], 
                                       (mean(springfield_obs_year$prcp, na.rm=TRUE)), 
                                       (mean(springfield_obs_year$tmax, na.rm=TRUE)), 
                                      (mean(springfield_obs_year$tmin, na.rm=TRUE)))
colnames(previousyear_springfield) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")
previousyear_tawascity <- data.frame(michigan_csv$id[26], 
                                     (mean(tawascity_obs_year$prcp, na.rm=TRUE)), 
                                     (mean(tawascity_obs_year$tmax, na.rm=TRUE)), 
                                      (mean(tawascity_obs_year$tmin, na.rm=TRUE)))
colnames(previousyear_tawascity) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")
previousyear_williamston <- data.frame(michigan_csv$id[6], 
                                       (mean(williamston_obs_year$prcp, na.rm=TRUE)), 
                                       (mean(williamston_obs_year$tmax, na.rm=TRUE)), 
                                     (mean(williamston_obs_year$tmin, na.rm=TRUE)))
colnames(previousyear_williamston) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")
previousyear_wyoming <- data.frame(michigan_csv$id[11], 
                                   (mean(wyoming_obs_year$prcp, na.rm=TRUE)), 
                                   (mean(wyoming_obs_year$tmax, na.rm=TRUE)), 
                                     (mean(wyoming_obs_year$tmin, na.rm=TRUE)))
colnames(previousyear_wyoming) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")
previousyear_ypsilanti <- data.frame(michigan_csv$id[18], 
                                     (mean(ypsilanti_obs_year$prcp, na.rm=TRUE)), 
                                     (mean(ypsilanti_obs_year$tmax, na.rm=TRUE)), 
                                     (mean(ypsilanti_obs_year$tmin, na.rm=TRUE)))
colnames(previousyear_ypsilanti) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")

### combine all df's for previous year data
michigan_previous_year_climate <- bind_rows(previousyear_albion, previousyear_alpena, previousyear_beaverisland,
                                            previousyear_bigrapids1, previousyear_bigrapids2, previousyear_charlevoixcounty, 
                                            previousyear_eastlansing, previousyear_fremont, previousyear_grandrapids1, previousyear_grandrapids2,
                                            previousyear_grandrapids3, previousyear_grandrapids4, previousyear_hart, previousyear_hudsonville1, 
                                            previousyear_hudsonville2, previousyear_kalamazoo, previousyear_mtpleasant, previousyear_mtpleasant, 
                                            previousyear_muskegon, previousyear_otsego, previousyear_richmond, previousyear_springfield,
                                            previousyear_tawascity, previousyear_williamston, previousyear_wyoming, previousyear_ypsilanti)

#save data
#write.csv(michigan_previous_year_climate, file = "michigan_previous_year_climate.csv", row.names = TRUE)

write.xlsx(michigan_previous_year_climate, file = "michigan_previous_year_climate.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

###find data for all for previous 6 months 
albion_obs_6months <- meteo_pull_monitors(albion_monitors, var = c("TMAX", "TMIN", "PRCP"), 
                                       date_min = "1926-11-30", date_max = "1927-05-03" )
alpena_obs_6months <- meteo_pull_monitors(alpena_monitors, var = c("TMAX", "TMIN", "PRCP"), 
                                       date_min = "1971-11-30", date_max = "1971-11-30" )
bigrapids1_6months <- meteo_pull_monitors(bigrapids_monitors, var = c("TMAX", "TMIN", "PRCP"), 
                                           date_min = "1997-11-30", date_max = "1998-5-4" )
bigrapids2_obs_6months <- meteo_pull_monitors(bigrapids_monitors, var = c("TMAX", "TMIN", "PRCP"), 
                                           date_min = "2001-11-30", date_max = "2002-5-11")
charlevoixcounty_obs_6months <- meteo_pull_monitors(charlevoixcounty_monitors, var = c("TMAX", "TMIN", "PRCP"), 
                                                 date_min = "1985-04-30", date_max = "1985-10-5" )
eastlansing_obs_6months <- meteo_pull_monitors(eastlansing_monitors, var = c("TMAX", "TMIN", "PRCP"), 
                                            date_min = "1937-11-30", date_max = "1938-5-13" )
fremont_obs_6months <- meteo_pull_monitors(fremont_monitors, var = c("TMAX", "TMIN", "PRCP"), 
                                        date_min = "2005-11-30", date_max = "2006-5-1" )
grandrapids1_obs_6months <- meteo_pull_monitors(grandrapids_monitors, var = c("TMAX", "TMIN", "PRCP"), 
                                             date_min = "1974-11-30", date_max = "1975-5-9" )
grandrapids2_obs_6months <- meteo_pull_monitors(grandrapids_monitors, var = c("TMAX", "TMIN", "PRCP"), 
                                             date_min = "1964-11-30", date_max = "1965-5-17" )
grandrapids3_obs_6months <- meteo_pull_monitors(grandrapids_monitors, var = c("TMAX", "TMIN", "PRCP"), 
                                             date_min = "1971-11-30", date_max = "1972-5-16" )
grandrapids4_obs_6months <- meteo_pull_monitors(grandrapids_monitors, var = c("TMAX", "TMIN", "PRCP"), 
                                             date_min = "1966-11-30", date_max = "1967-5-15" )
hart_obs_6months <- meteo_pull_monitors(hart_monitors, var = c("TMAX", "TMIN", "PRCP"), 
                                     date_min = "2002-11-30", date_max = "2003-5-26" )
hudsonville1_obs_6months <- meteo_pull_monitors(hudsonville_monitors, var = c("TMAX", "TMIN", "PRCP"), 
                                             date_min = "1953-11-30", date_max = "1954-5-7" )
hudsonville2_obs_6months <- meteo_pull_monitors(hudsonville_monitors, var = c("TMAX", "TMIN", "PRCP"), 
                                             date_min = "1966-11-30", date_max = "1967-5-17" )
kalamazoo_obs_6months <- meteo_pull_monitors(kalamazoo_monitors, var = c("TMAX", "TMIN", "PRCP"), 
                                          date_min = "1972-10-31", date_max = "1973-4-8" )
mtpleasant_obs_6months <- meteo_pull_monitors(mtpleasant_monitors, var = c("TMAX", "TMIN", "PRCP"), 
                                           date_min = "1997-03-31", date_max = "1997-9-18" )
muskegon_obs_6months <- meteo_pull_monitors(muskegon_monitors, var = c("TMAX", "TMIN", "PRCP"), 
                                         date_min = "1975-11-30", date_max = "1976-5-8" )
otsego_obs_6months <- meteo_pull_monitors(otsego_monitors, var = c("TMAX", "TMIN", "PRCP"), 
                                       date_min = "2016-12-31", date_max = "2017-6-11" )
richmond_obs_6months <- meteo_pull_monitors(richmond_monitors, var = c("TMAX", "TMIN", "PRCP"), 
                                         date_min = "1957-12-31", date_max = "1958-6-18" )
springfield_obs_6months <- meteo_pull_monitors(springfield_monitors, var = c("TMAX", "TMIN", "PRCP"), 
                                            date_min = "1993-10-31", date_max = "1994-4-27" )
tawascity_obs_6months <- meteo_pull_monitors(tawascity_monitors, var = c("TMAX", "TMIN", "PRCP"), 
                                          date_min = "1991-11-30", date_max = "1992-5-12" )
williamston_obs_6months <- meteo_pull_monitors(williamston_monitors, var = c("TMAX", "TMIN", "PRCP"), 
                                            date_min = "2021-03-31", date_max = "2021-9-26" )
wyoming_obs_6months <- meteo_pull_monitors(wyoming_monitors, var = c("TMAX", "TMIN", "PRCP"), 
                                        date_min = "1965-11-30", date_max = "1966-5-22" )
ypsilanti_obs_6months <- meteo_pull_monitors(ypsilanti_monitors, var = c("TMAX", "TMIN", "PRCP"), 
                                          date_min = "1915-11-30", date_max = "1916-5-17" )

## make a dataframe of the mean of tmax, tmin, prcp of the previous 6 months 
previous6months_albion <- data.frame(michigan_csv$id[8], 
                                   (mean(albion_obs_6months$prcp, na.rm=TRUE)), 
                                   (mean(albion_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(albion_obs_6months$tmin, na.rm=TRUE))) 
previous6months_alpena <- data.frame(michigan_csv$id[24], 
                                   (mean(alpena_obs_6months$prcp, na.rm=TRUE)), 
                                   (mean(alpena_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(alpena_obs_6months$tmin, na.rm=TRUE)))
previous6months_bigrapids1 <- data.frame(michigan_csv$id[19], 
                                   (mean(bigrapids1_6months$prcp, na.rm=TRUE)), 
                                   (mean(bigrapids1_6months$tmax, na.rm=TRUE)), 
                                   (mean(bigrapids1_6months$tmin, na.rm=TRUE)))
previous6months_bigrapids2 <- data.frame(michigan_csv$id[22], 
                                   (mean(bigrapids2_obs_6months$prcp, na.rm=TRUE)), 
                                   (mean(bigrapids2_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(bigrapids2_obs_6months$tmin, na.rm=TRUE)))
previous6months_charlevoixcounty <- data.frame(michigan_csv$id[5], 
                                   (mean(charlevoixcounty_obs_6months$prcp, na.rm=TRUE)), 
                                   (mean(charlevoixcounty_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(charlevoixcounty_obs_6months$tmin, na.rm=TRUE)))
previous6months_eastlansing <- data.frame(michigan_csv$id[20], 
                                               (mean(eastlansing_obs_6months$prcp, na.rm=TRUE)), 
                                               (mean(eastlansing_obs_6months$tmax, na.rm=TRUE)), 
                                               (mean(eastlansing_obs_6months$tmin, na.rm=TRUE)))
previous6months_fremont <- data.frame(michigan_csv$id[12], 
                                   (mean(fremont_obs_6months$prcp, na.rm=TRUE)), 
                                   (mean(fremont_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(fremont_obs_6months$tmin, na.rm=TRUE)))
previous6months_grandrapids1 <- data.frame(michigan_csv$id[9], 
                                   (mean(grandrapids1_obs_6months$prcp, na.rm=TRUE)), 
                                   (mean(grandrapids1_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(grandrapids1_obs_6months$tmin, na.rm=TRUE)))
previous6months_grandrapids2 <- data.frame(michigan_csv$id[13], 
                                   (mean(grandrapids2_obs_6months$prcp, na.rm=TRUE)), 
                                   (mean(grandrapids2_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(grandrapids2_obs_6months$tmin, na.rm=TRUE)))
previous6months_grandrapids3 <- data.frame(michigan_csv$id[14], 
                                   (mean(grandrapids3_obs_6months$prcp, na.rm=TRUE)), 
                                   (mean(grandrapids3_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(grandrapids3_obs_6months$tmin, na.rm=TRUE)))
previous6months_grandrapids4 <- data.frame(michigan_csv$id[15], 
                                   (mean(grandrapids4_obs_6months$prcp, na.rm=TRUE)), 
                                   (mean(grandrapids4_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(grandrapids4_obs_6months$tmin, na.rm=TRUE)))
previous6months_hart <- data.frame(michigan_csv$id[1], 
                                   (mean(hart_obs_6months$prcp, na.rm=TRUE)), 
                                   (mean(hart_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(hart_obs_6months$tmin, na.rm=TRUE)))
previous6months_hudsonville1 <- data.frame(michigan_csv$id[2], 
                                   (mean(hudsonville1_obs_6months$prcp, na.rm=TRUE)), 
                                   (mean(hudsonville1_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(hudsonville1_obs_6months$tmin, na.rm=TRUE)))
previous6months_hudsonville2 <- data.frame(michigan_csv$id[4], 
                                   (mean(hudsonville2_obs_6months$prcp, na.rm=TRUE)), 
                                   (mean(hudsonville2_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(hudsonville2_obs_6months$tmin, na.rm=TRUE)))
previous6months_kalamazoo <- data.frame(michigan_csv$id[10], 
                                   (mean(kalamazoo_obs_6months$prcp, na.rm=TRUE)), 
                                   (mean(kalamazoo_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(kalamazoo_obs_6months$tmin, na.rm=TRUE)))
previous6months_mtpleasant <- data.frame(michigan_csv$id[3], 
                                   (mean(mtpleasant_obs_6months$prcp, na.rm=TRUE)), 
                                   (mean(mtpleasant_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(mtpleasant_obs_6months$tmin, na.rm=TRUE)))
previous6months_muskegon <- data.frame(michigan_csv$id[7], 
                                   (mean(muskegon_obs_6months$prcp, na.rm=TRUE)), 
                                   (mean(muskegon_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(muskegon_obs_6months$tmin, na.rm=TRUE)))
previous6months_otsego <- data.frame(michigan_csv$id[27], 
                                   (mean(otsego_obs_6months$prcp, na.rm=TRUE)), 
                                   (mean(otsego_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(otsego_obs_6months$tmin, na.rm=TRUE)))
previous6months_richmond <- data.frame(michigan_csv$id[17], 
                                   (mean(richmond_obs_6months$prcp, na.rm=TRUE)), 
                                   (mean(richmond_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(richmond_obs_6months$tmin, na.rm=TRUE)))
previous6months_springfield <- data.frame(michigan_csv$id[25], 
                                   (mean(springfield_obs_6months$prcp, na.rm=TRUE)), 
                                   (mean(springfield_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(springfield_obs_6months$tmin, na.rm=TRUE)))
previous6months_tawascity <- data.frame(michigan_csv$id[26], 
                                   (mean(tawascity_obs_6months$prcp, na.rm=TRUE)), 
                                   (mean(tawascity_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(tawascity_obs_6months$tmin, na.rm=TRUE)))
previous6months_williamston <- data.frame(michigan_csv$id[6], 
                                   (mean(williamston_obs_6months$prcp, na.rm=TRUE)), 
                                   (mean(williamston_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(williamston_obs_6months$tmin, na.rm=TRUE)))
previous6months_wyoming <- data.frame(michigan_csv$id[11], 
                                   (mean(wyoming_obs_6months$prcp, na.rm=TRUE)), 
                                   (mean(wyoming_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(wyoming_obs_6months$tmin, na.rm=TRUE)))
previous6months_ypsilanti <- data.frame(michigan_csv$id[18], 
                                   (mean(ypsilanti_obs_6months$prcp, na.rm=TRUE)), 
                                   (mean(ypsilanti_obs_6months$tmax, na.rm=TRUE)), 
                                   (mean(ypsilanti_obs_6months$tmin, na.rm=TRUE)))

#rename col.names 
colnames(previous6months_albion) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")
colnames(previous6months_alpena) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")
colnames(previous6months_bigrapids1) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")
colnames(previous6months_bigrapids2) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")
colnames(previous6months_charlevoixcounty) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")
colnames(previous6months_eastlansing) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")
colnames(previous6months_fremont) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")
colnames(previous6months_grandrapids1) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")
colnames(previous6months_grandrapids2) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")
colnames(previous6months_grandrapids3) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")
colnames(previous6months_grandrapids4) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")
colnames(previous6months_hart) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")
colnames(previous6months_hudsonville1) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")
colnames(previous6months_hudsonville2) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")
colnames(previous6months_kalamazoo) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")
colnames(previous6months_mtpleasant) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")
colnames(previous6months_muskegon) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")
colnames(previous6months_otsego) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")
colnames(previous6months_richmond) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")
colnames(previous6months_tawascity) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")
colnames(previous6months_williamston) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")
colnames(previous6months_wyoming) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")
colnames(previous6months_ypsilanti) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")
colnames(previous6months_springfield) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")

## combine previous 6 months data using bind_rows
michigan_6months <- bind_rows(previous6months_albion, previous6months_alpena, previous6months_bigrapids1, previous6months_bigrapids2,
                              previous6months_charlevoixcounty, previous6months_eastlansing, previous6months_fremont, previous6months_grandrapids1, 
                              previous6months_grandrapids2, previous6months_grandrapids3, previous6months_grandrapids4, previous6months_hart,
                              previous6months_hudsonville1, previous6months_hudsonville1, previous6months_hudsonville2, previous6months_kalamazoo,
                              previous6months_mtpleasant, previous6months_muskegon, previous6months_otsego, previous6months_richmond, 
                              previous6months_springfield, previous6months_tawascity, previous6months_williamston, previous6months_wyoming,
                              previous6months_ypsilanti)

#save data
#write.csv(michigan_6months, file = "michigan_6months_climate.csv", row.names = TRUE)

write.xlsx(michigan_6months, file = "michigan_6months_climate.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

## pull data for the current year of the date of collection
# general code: city_obs_yearDOC <- meteo_pull_monitors(city_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "", date_max = "" )
albion_obs_yearDOC <- meteo_pull_monitors(albion_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "1927-01-01", date_max = "1927-12-31" )
alpena_obs_yearDOC <- meteo_pull_monitors(alpena_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "1972-01-01", date_max = "1972-12-31" )
bigrapids1_obs_yearDOC <- meteo_pull_monitors(bigrapids_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "1998-01-01", date_max = "1998-12-31" )
bigrapids2_obs_yearDOC <- meteo_pull_monitors(bigrapids_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "2002-01-01", date_max = "2002-12-31" )
charlevoixcounty_obs_yearDOC <- meteo_pull_monitors(charlevoixcounty_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "1985-01-01", date_max = "1985-12-31" )
eastlansing_obs_yearDOC <- meteo_pull_monitors(eastlansing_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "1938-01-01", date_max = "1938-12-31" )
fremont_obs_yearDOC <- meteo_pull_monitors(fremont_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "2006-01-01", date_max = "2006-12-31" )
grandrapids1_obs_yearDOC <- meteo_pull_monitors(grandrapids_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "1975-01-01", date_max = "1975-12-31" )
grandrapids2_obs_yearDOC <- meteo_pull_monitors(grandrapids_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "1965-01-01", date_max = "1965-12-31" )
grandrapids3_obs_yearDOC <- meteo_pull_monitors(grandrapids_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "1972-01-01", date_max = "1972-12-31" )
grandrapids4_obs_yearDOC <- meteo_pull_monitors(grandrapids_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "1967-01-01", date_max = "1967-12-31" )
hart_obs_yearDOC <- meteo_pull_monitors(hart_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "2003-01-01", date_max = "2003-12-31" )
hudsonville1_obs_yearDOC <- meteo_pull_monitors(hudsonville_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "1954-01-01", date_max = "1954-12-31" )
hudsonville2_obs_yearDOC <- meteo_pull_monitors(hudsonville_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "1967-01-01", date_max = "1967-12-31" )
kalamazoo_obs_yearDOC <- meteo_pull_monitors(kalamazoo_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "1973-01-01", date_max = "1973-12-31" )
mtpleasant_obs_yearDOC <- meteo_pull_monitors(mtpleasant_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "1997-01-01", date_max = "1997-12-31" )
muskegon_obs_yearDOC <- meteo_pull_monitors(muskegon_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "1976-01-01", date_max = "1976-12-31" )
otsego_obs_yearDOC <- meteo_pull_monitors(otsego_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "2017-01-01", date_max = "2017-12-31" )
richmond_obs_yearDOC <- meteo_pull_monitors(richmond_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "1958-01-01", date_max = "1958-12-31" )
tawascity_obs_yearDOC <- meteo_pull_monitors(tawascity_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "1992-01-01", date_max = "1992-12-31" )
williamston_obs_yearDOC <- meteo_pull_monitors(williamston_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "2021-01-01", date_max = "2021-12-31" )
wyoming_obs_yearDOC <- meteo_pull_monitors(wyoming_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "1966-01-01", date_max = "1966-12-31" )
ypsilanti_obs_yearDOC <- meteo_pull_monitors(ypsilanti_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "1916-01-01", date_max = "1916-12-31" )
springfield_obs_yearDOC <- meteo_pull_monitors(springfield_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "1994-01-01", date_max = "1994-12-31" )
beaverisland_obs_yearDOC <- meteo_pull_monitors(beaverisland_monitors, var = c("TMAX", "TMIN", "PRCP"), date_min = "1958-06-26-01-01", date_max = "1958-06-26-12-31" )

## make a dataframe of the mean of tmax, tmin, current year of the date of collection
city2 <- michigan_csv$id

citySwap2 = function(x, y, z, e, f, g){
  paste(x, y, x, z, x, e, x, f, x, g, sep = "")
}

cat(citySwap2(city2, y = "previousyearDOC_", z = " <- data.frame(michigan_csv$id[], 
                                   (mean(", e = "_obs_yearDOC$prcp, na.rm=TRUE)), 
                                   (mean(", f = "_obs_yearDOC$tmax, na.rm=TRUE)), 
                                   (mean(", g = "_obs_yearDOC$tmin, na.rm=TRUE)))"), sep = "\n")

previousyearDOC_hart <- data.frame(michigan_csv$id[1], 
                                   (mean(hart_obs_yearDOC$prcp, na.rm=TRUE)), 
                                   (mean(hart_obs_yearDOC$tmax, na.rm=TRUE)), 
                                   (mean(hart_obs_yearDOC$tmin, na.rm=TRUE)))
previousyearDOC_hudsonville1 <- data.frame(michigan_csv$id[2], 
                                          (mean(hudsonville1_obs_yearDOC$prcp, na.rm=TRUE)), 
                                          (mean(hudsonville1_obs_yearDOC$tmax, na.rm=TRUE)), 
                                          (mean(hudsonville1_obs_yearDOC$tmin, na.rm=TRUE)))
previousyearDOC_mtpleasant <- data.frame(michigan_csv$id[3], 
                                           (mean(mtpleasant_obs_yearDOC$prcp, na.rm=TRUE)), 
                                           (mean(mtpleasant_obs_yearDOC$tmax, na.rm=TRUE)), 
                                           (mean(mtpleasant_obs_yearDOC$tmin, na.rm=TRUE)))
previousyearDOC_hudsonville2 <- data.frame(michigan_csv$id[4], 
                                          (mean(hudsonville2_obs_yearDOC$prcp, na.rm=TRUE)), 
                                          (mean(hudsonville2_obs_yearDOC$tmax, na.rm=TRUE)), 
                                          (mean(hudsonville2_obs_yearDOC$tmin, na.rm=TRUE)))
previousyearDOC_charlevoixcounty <- data.frame(michigan_csv$id[5], 
                                                (mean(charlevoixcounty_obs_yearDOC$prcp, na.rm=TRUE)), 
                                                (mean(charlevoixcounty_obs_yearDOC$tmax, na.rm=TRUE)), 
                                                (mean(charlevoixcounty_obs_yearDOC$tmin, na.rm=TRUE)))
previousyearDOC_williamston <- data.frame(michigan_csv$id[6], 
                                          (mean(williamston_obs_yearDOC$prcp, na.rm=TRUE)), 
                                          (mean(williamston_obs_yearDOC$tmax, na.rm=TRUE)), 
                                          (mean(williamston_obs_yearDOC$tmin, na.rm=TRUE)))
previousyearDOC_muskegon <- data.frame(michigan_csv$id[7], 
                                       (mean(muskegon_obs_yearDOC$prcp, na.rm=TRUE)), 
                                       (mean(muskegon_obs_yearDOC$tmax, na.rm=TRUE)), 
                                       (mean(muskegon_obs_yearDOC$tmin, na.rm=TRUE)))
previousyearDOC_albion <- data.frame(michigan_csv$id[8], 
                                     (mean(albion_obs_yearDOC$prcp, na.rm=TRUE)), 
                                     (mean(albion_obs_yearDOC$tmax, na.rm=TRUE)), 
                                     (mean(albion_obs_yearDOC$tmin, na.rm=TRUE)))
previousyearDOC_grandrapids1 <- data.frame(michigan_csv$id[9], 
                                           (mean(grandrapids1_obs_yearDOC$prcp, na.rm=TRUE)), 
                                           (mean(grandrapids1_obs_yearDOC$tmax, na.rm=TRUE)), 
                                           (mean(grandrapids1_obs_yearDOC$tmin, na.rm=TRUE)))
previousyearDOC_kalamazoo <- data.frame(michigan_csv$id[10], 
                                        (mean(kalamazoo_obs_yearDOC$prcp, na.rm=TRUE)), 
                                        (mean(kalamazoo_obs_yearDOC$tmax, na.rm=TRUE)), 
                                        (mean(kalamazoo_obs_yearDOC$tmin, na.rm=TRUE)))
previousyearDOC_wyoming <- data.frame(michigan_csv$id[11], 
                                      (mean(wyoming_obs_yearDOC$prcp, na.rm=TRUE)), 
                                      (mean(wyoming_obs_yearDOC$tmax, na.rm=TRUE)), 
                                      (mean(wyoming_obs_yearDOC$tmin, na.rm=TRUE)))
previousyearDOC_fremont <- data.frame(michigan_csv$id[12], 
                                      (mean(fremont_obs_yearDOC$prcp, na.rm=TRUE)), 
                                      (mean(fremont_obs_yearDOC$tmax, na.rm=TRUE)), 
                                      (mean(fremont_obs_yearDOC$tmin, na.rm=TRUE)))
previousyearDOC_grandrapids2 <- data.frame(michigan_csv$id[13], 
                                           (mean(grandrapids2_obs_yearDOC$prcp, na.rm=TRUE)), 
                                           (mean(grandrapids2_obs_yearDOC$tmax, na.rm=TRUE)), 
                                           (mean(grandrapids2_obs_yearDOC$tmin, na.rm=TRUE)))
previousyearDOC_grandrapids3 <- data.frame(michigan_csv$id[14], 
                                           (mean(grandrapids3_obs_yearDOC$prcp, na.rm=TRUE)), 
                                           (mean(grandrapids3_obs_yearDOC$tmax, na.rm=TRUE)), 
                                           (mean(grandrapids3_obs_yearDOC$tmin, na.rm=TRUE)))
previousyearDOC_grandrapids4 <- data.frame(michigan_csv$id[15], 
                                           (mean(grandrapids4_obs_yearDOC$prcp, na.rm=TRUE)), 
                                           (mean(grandrapids4_obs_yearDOC$tmax, na.rm=TRUE)), 
                                           (mean(grandrapids4_obs_yearDOC$tmin, na.rm=TRUE)))
previousyearDOC_beaverisland <- data.frame(michigan_csv$id[16], 
                                            (mean(beaverisland_obs_yearDOC$prcp, na.rm=TRUE)), 
                                            (mean(beaverisland_obs_yearDOC$tmax, na.rm=TRUE)), 
                                            (mean(beaverisland_obs_yearDOC$tmin, na.rm=TRUE)))
previousyearDOC_richmond <- data.frame(michigan_csv$id[17], 
                                       (mean(richmond_obs_yearDOC$prcp, na.rm=TRUE)), 
                                       (mean(richmond_obs_yearDOC$tmax, na.rm=TRUE)), 
                                       (mean(richmond_obs_yearDOC$tmin, na.rm=TRUE)))
previousyearDOC_ypsilanti <- data.frame(michigan_csv$id[18], 
                                        (mean(ypsilanti_obs_yearDOC$prcp, na.rm=TRUE)), 
                                        (mean(ypsilanti_obs_yearDOC$tmax, na.rm=TRUE)), 
                                        (mean(ypsilanti_obs_yearDOC$tmin, na.rm=TRUE)))

previousyearDOC_eastlansing <- data.frame(michigan_csv$id[20], 
                                           (mean(eastlansing_obs_yearDOC$prcp, na.rm=TRUE)), 
                                           (mean(eastlansing_obs_yearDOC$tmax, na.rm=TRUE)), 
                                           (mean(eastlansing_obs_yearDOC$tmin, na.rm=TRUE)))
previousyearDOC_bigrapids1 <- data.frame(michigan_csv$id[21], 
                                         (mean(bigrapids1_obs_yearDOC$prcp, na.rm=TRUE)), 
                                         (mean(bigrapids1_obs_yearDOC$tmax, na.rm=TRUE)), 
                                         (mean(bigrapids1_obs_yearDOC$tmin, na.rm=TRUE)))
previousyearDOC_bigrapids2 <- data.frame(michigan_csv$id[22], 
                                         (mean(bigrapids2_obs_yearDOC$prcp, na.rm=TRUE)), 
                                         (mean(bigrapids2_obs_yearDOC$tmax, na.rm=TRUE)), 
                                         (mean(bigrapids2_obs_yearDOC$tmin, na.rm=TRUE)))
previousyearDOC_alpena <- data.frame(michigan_csv$id[24], 
                                     (mean(alpena_obs_yearDOC$prcp, na.rm=TRUE)), 
                                     (mean(alpena_obs_yearDOC$tmax, na.rm=TRUE)), 
                                     (mean(alpena_obs_yearDOC$tmin, na.rm=TRUE)))
previousyearDOC_springfield <- data.frame(michigan_csv$id[25], 
                                          (mean(springfield_obs_yearDOC$prcp, na.rm=TRUE)), 
                                          (mean(springfield_obs_yearDOC$tmax, na.rm=TRUE)), 
                                          (mean(springfield_obs_yearDOC$tmin, na.rm=TRUE)))
previousyearDOC_tawascity <- data.frame(michigan_csv$id[26], 
                                         (mean(tawascity_obs_yearDOC$prcp, na.rm=TRUE)), 
                                         (mean(tawascity_obs_yearDOC$tmax, na.rm=TRUE)), 
                                         (mean(tawascity_obs_yearDOC$tmin, na.rm=TRUE)))
previousyearDOC_otsego <- data.frame(michigan_csv$id[27], 
                                     (mean(otsego_obs_yearDOC$prcp, na.rm=TRUE)), 
                                     (mean(otsego_obs_yearDOC$tmax, na.rm=TRUE)), 
                                     (mean(otsego_obs_yearDOC$tmin, na.rm=TRUE)))
#rename col.names 
colnames(previousyearDOC_city) <- c('id',"prcp_avg", "tmax_avg", "tmin_avg")

cat(citySwap3(city2, y = "colnames(previousyearDOC_", z = ") <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')"), sep = "\n")

colnames(previousyearDOC_hart) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(previousyearDOC_hudsonville1) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(previousyearDOC_mtpleasant) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(previousyearDOC_hudsonville2) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(previousyearDOC_charlevoixcounty) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(previousyearDOC_williamston) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(previousyearDOC_muskegon) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(previousyearDOC_albion) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(previousyearDOC_grandrapids1) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(previousyearDOC_kalamazoo) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(previousyearDOC_wyoming) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(previousyearDOC_fremont) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(previousyearDOC_grandrapids2) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(previousyearDOC_grandrapids3) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(previousyearDOC_grandrapids4) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(previousyearDOC_beaverisland) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(previousyearDOC_richmond) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(previousyearDOC_ypsilanti) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(previousyearDOC_eastlansing) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(previousyearDOC_bigrapids1) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(previousyearDOC_bigrapids2) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(previousyearDOC_alpena) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(previousyearDOC_springfield) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(previousyearDOC_tawascity) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')
colnames(previousyearDOC_otsego) <- c('id','prcp_avg', 'tmax_avg', 'tmin_avg')


## combine current year of the date of collection data using bind_rows
michigan_yearDOC <- bind_rows(previousyearDOC_albion, previousyearDOC_alpena, previousyearDOC_charlevoixcounty, previousyearDOC_beaverisland,
                              previousyearDOC_bigrapids1, previousyearDOC_bigrapids2, previousyearDOC_eastlansing, previousyearDOC_fremont, 
                              previousyearDOC_grandrapids1, previousyearDOC_grandrapids2, previousyearDOC_grandrapids3, previousyearDOC_grandrapids4,
                              previousyearDOC_hart, previousyearDOC_hudsonville1, previousyearDOC_hudsonville2, previousyearDOC_kalamazoo, 
                              previousyearDOC_mtpleasant, previousyearDOC_muskegon, previousyearDOC_otsego, previousyearDOC_richmond, 
                              previousyearDOC_springfield, previousyearDOC_tawascity, previousyearDOC_williamston, previousyearDOC_wyoming,
                              previousyearDOC_ypsilanti)

write.xlsx(michigan_yearDOC, file = "michigan_yearDOC_climate.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
