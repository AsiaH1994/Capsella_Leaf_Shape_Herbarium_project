#rm(list=ls())
library("xlsx")
library(dplyr)

### functions
filtering = function(x, y, z){
  paste(x, y, x, z, sep = "")
}

meandata = function(x, y, e, f, g, l, k, z){
  paste(x, y, l, k, x, e, x, f, x, g, x, z, sep = "")
}

collnames = function(x, y, z ){
  paste(y, x, z, sep = "")
}

bindrowsfun = function(x, y, z){
  paste(x, y, z, sep = "")
}

## total weather data - go from 1/10 a degree to degree
## then find average of all data for that city 
STtotal <- read.csv("ST_data_total_weather.csv")

##make prcp, tmax, tmin, and tavg numeric

STtotal$prcp <- as.numeric(STtotal$prcp)
STtotal$tmax <- as.numeric(STtotal$tmax)
STtotal$tmin <- as.numeric(STtotal$tmin)
STtotal$tavg <- as.numeric(STtotal$tavg)

## multiply by 0.1 or divide by 10 to make 1 degree 

STtotal$prcp <- STtotal$prcp/10
STtotal$tmax <- STtotal$tmax/10
STtotal$tmin <- STtotal$tmin/10
STtotal$tavg <- STtotal$tavg/10

#pull data 
id2 <- unique(STtotal$id2)
cat(filtering(id2, y =  "<- filter(STtotal, id2 == '", z = "')"), sep = "\n")

### find the mean for each 
lennumber = (1:20)
df <- list(id2, lennumber)

lapply(df, cat(meandata(x = id2,  y = "_data <- data.frame((id2[", l= lennumber, k = "]), (mean(", e = "$prcp, na.rm=TRUE)), (mean(", f = "$tmax, na.rm=TRUE)), 
                              (mean(", g = "$tmin, na.rm=TRUE)), (mean(", z = "$tavg, na.rm=TRUE)))"), sep = "\n"))


#column names 
cat(collnames(unique(id2), y = "colnames(", z = "_data) <- c('id', 'prcp_avg','tmax_avg', 'tmin_avg', 'tavg_avg')"), sep = "\n")


##paste into bind_rows()
cat(bindrowsfun(unique(id2), y = "_data", z = ","), sep = "")
STtotal_obs <- bind_rows()

### save data as xlsx file 
write.xlsx(STtotal_obs, file = "STtotal_obs.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)




