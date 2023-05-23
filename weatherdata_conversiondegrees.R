rm(list=ls())
library("xlsx")
library(dplyr)

####### alabama #########

al_sixmonths <- read.csv("AL_6months_weather.csv")

al_sixmonths$prcp_avg <- as.numeric(al_sixmonths$prcp_avg)
al_sixmonths$tmax_avg <- as.numeric(al_sixmonths$tmax_avg)
al_sixmonths$tmin_avg <- as.numeric(al_sixmonths$tmin_avg)

al_sixmonths$prcp_avg <- al_sixmonths$prcp_avg *0.1
al_sixmonths$tmax_avg <- al_sixmonths$tmax_avg *0.1
al_sixmonths$tmin_avg <- al_sixmonths$tmin_avg *0.1

## find the tavg for state data 
al_sixmonths$tavg <- ((al_sixmonths$tmax + al_sixmonths$tmin) / 2)

### save data as xlsx file 
write.xlsx(al_sixmonths, file = "al_sixmonths.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

al_previousyear <- read.csv("AL_previous_year_weather.csv")

al_previousyear$prcp_avg <- as.numeric(al_previousyear$prcp_avg)
al_previousyear$tmax_avg <- as.numeric(al_previousyear$tmax_avg)
al_previousyear$tmin_avg <- as.numeric(al_previousyear$tmin_avg)

al_previousyear$prcp_avg <- al_previousyear$prcp_avg *0.1
al_previousyear$tmax_avg <- al_previousyear$tmax_avg *0.1
al_previousyear$tmin_avg <- al_previousyear$tmin_avg *0.1

## find the tavg for state data 
al_previousyear$tavg <- ((al_previousyear$tmax + al_previousyear$tmin) / 2)

### save data as xlsx file 
write.xlsx(al_previousyear, file = "al_previousyear.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


al_doc <- read.csv("AL_year_DOC_weather.csv")

al_doc$prcp_avg <- as.numeric(al_doc$prcp_avg)
al_doc$tmax_avg <- as.numeric(al_doc$tmax_avg)
al_doc$tmin_avg <- as.numeric(al_doc$tmin_avg)

al_doc$prcp_avg <- al_doc$prcp_avg *0.1
al_doc$tmax_avg <- al_doc$tmax_avg *0.1
al_doc$tmin_avg <- al_doc$tmin_avg *0.1

## find the tavg for state data 
al_doc$tavg <- ((al_doc$tmax + al_doc$tmin) / 2)

### save data as xlsx file 
write.xlsx(al_doc, file = "al_doc.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

######### delaware #########

DE_sixmonths <- read.csv("delaware_6months_weather.csv")
DE_previousyear <- read.csv("delaware_previous_year_weather.csv")
DE_doc <- read.csv("delaware_year_DOC_weather.csv")

DE_sixmonths$prcp_avg <- as.numeric(DE_sixmonths$prcp_avg)
DE_sixmonths$tmax_avg <- as.numeric(DE_sixmonths$tmax_avg)
DE_sixmonths$tmin_avg <- as.numeric(DE_sixmonths$tmin_avg)

DE_sixmonths$prcp_avg <- DE_sixmonths$prcp_avg *0.1
DE_sixmonths$tmax_avg <- DE_sixmonths$tmax_avg *0.1
DE_sixmonths$tmin_avg <- DE_sixmonths$tmin_avg *0.1

## find the tavg for state data 
DE_sixmonths$tavg <- ((DE_sixmonths$tmax + DE_sixmonths$tmin) / 2)

DE_previousyear$prcp_avg <- as.numeric(DE_previousyear$prcp_avg)
DE_previousyear$tmax_avg <- as.numeric(DE_previousyear$tmax_avg)
DE_previousyear$tmin_avg <- as.numeric(DE_previousyear$tmin_avg)

DE_previousyear$prcp_avg <- DE_previousyear$prcp_avg *0.1
DE_previousyear$tmax_avg <- DE_previousyear$tmax_avg *0.1
DE_previousyear$tmin_avg <- DE_previousyear$tmin_avg *0.1

## find the tavg for state data 
DE_previousyear$tavg <- ((DE_previousyear$tmax + DE_previousyear$tmin) / 2)

### save data as xlsx file 
write.xlsx(DE_previousyear, file = "DE_previousyear.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

DE_doc$prcp_avg <- as.numeric(DE_doc$prcp_avg)
DE_doc$tmax_avg <- as.numeric(DE_doc$tmax_avg)
DE_doc$tmin_avg <- as.numeric(DE_doc$tmin_avg)

DE_doc$prcp_avg <- DE_doc$prcp_avg *0.1
DE_doc$tmax_avg <- DE_doc$tmax_avg *0.1
DE_doc$tmin_avg <- DE_doc$tmin_avg *0.1

## find the tavg for state data 
DE_doc$tavg <- ((DE_doc$tmax + DE_doc$tmin) / 2)

### save data as xlsx file 
write.xlsx(DE_doc, file = "DE_doc.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

######## florida #########

FL_sixmonths <- read.csv("FL_6months_weather.csv")
FL_previousyear <- read.csv("FL_previous_year_weather.csv")
FL_doc <- read.csv("FL_year_DOC_weather.csv")

FL_sixmonths$prcp_avg <- as.numeric(FL_sixmonths$prcp_avg)
FL_sixmonths$tmax_avg <- as.numeric(FL_sixmonths$tmax_avg)
FL_sixmonths$tmin_avg <- as.numeric(FL_sixmonths$tmin_avg)

FL_sixmonths$prcp_avg <- FL_sixmonths$prcp_avg *0.1
FL_sixmonths$tmax_avg <- FL_sixmonths$tmax_avg *0.1
FL_sixmonths$tmin_avg <- FL_sixmonths$tmin_avg *0.1


## find the tavg for state data 
FL_sixmonths$tavg <- ((FL_sixmonths$tmax + FL_sixmonths$tmin) / 2)

### save data as xlsx file 
write.xlsx(FL_sixmonths, file = "FL_sixmonths.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

FL_previousyear$prcp_avg <- as.numeric(FL_previousyear$prcp_avg)
FL_previousyear$tmax_avg <- as.numeric(FL_previousyear$tmax_avg)
FL_previousyear$tmin_avg <- as.numeric(FL_previousyear$tmin_avg)

FL_previousyear$prcp_avg <- FL_previousyear$prcp_avg *0.1
FL_previousyear$tmax_avg <- FL_previousyear$tmax_avg *0.1
FL_previousyear$tmin_avg <- FL_previousyear$tmin_avg *0.1

## find the tavg for state data 
FL_previousyear$tavg <- ((FL_previousyear$tmax + FL_previousyear$tmin) / 2)

### save data as xlsx file 
write.xlsx(FL_previousyear, file = "FL_previousyear.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

FL_doc$prcp_avg <- as.numeric(FL_doc$prcp_avg)
FL_doc$tmax_avg <- as.numeric(FL_doc$tmax_avg)
FL_doc$tmin_avg <- as.numeric(FL_doc$tmin_avg)

FL_doc$prcp_avg <- FL_doc$prcp_avg *0.1
FL_doc$tmax_avg <- FL_doc$tmax_avg *0.1
FL_doc$tmin_avg <- FL_doc$tmin_avg *0.1

## find the tavg for state data 
FL_doc$tavg <- ((FL_doc$tmax + FL_doc$tmin) / 2)

### save data as xlsx file 
write.xlsx(FL_doc, file = "FL_doc.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

####### georgia #########

GA_sixmonths <- read.csv("GA_6months_weather.csv")
GA_previousyear <- read.csv("GA_previous_year_weather.csv")
GA_doc <- read.csv("GA_year_DOC_weather.csv")

GA_sixmonths$prcp_avg <- as.numeric(GA_sixmonths$prcp_avg)
GA_sixmonths$tmax_avg <- as.numeric(GA_sixmonths$tmax_avg)
GA_sixmonths$tmin_avg <- as.numeric(GA_sixmonths$tmin_avg)

GA_sixmonths$prcp_avg <- GA_sixmonths$prcp_avg *0.1
GA_sixmonths$tmax_avg <- GA_sixmonths$tmax_avg *0.1
GA_sixmonths$tmin_avg <- GA_sixmonths$tmin_avg *0.1


## find the tavg for state data 
GA_sixmonths$tavg <- ((GA_sixmonths$tmax + GA_sixmonths$tmin) / 2)

### save data as xlsx file 
write.xlsx(GA_sixmonths, file = "GA_sixmonths.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

GA_previousyear$prcp_avg <- as.numeric(GA_previousyear$prcp_avg)
GA_previousyear$tmax_avg <- as.numeric(GA_previousyear$tmax_avg)
GA_previousyear$tmin_avg <- as.numeric(GA_previousyear$tmin_avg)

GA_previousyear$prcp_avg <- GA_previousyear$prcp_avg *0.1
GA_previousyear$tmax_avg <- GA_previousyear$tmax_avg *0.1
GA_previousyear$tmin_avg <- GA_previousyear$tmin_avg *0.1

## find the tavg for state data 
GA_previousyear$tavg <- ((GA_previousyear$tmax + GA_previousyear$tmin) / 2)

### save data as xlsx file 
write.xlsx(GA_previousyear, file = "GA_previousyear.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

GA_doc$prcp_avg <- as.numeric(GA_doc$prcp_avg)
GA_doc$tmax_avg <- as.numeric(GA_doc$tmax_avg)
GA_doc$tmin_avg <- as.numeric(GA_doc$tmin_avg)

GA_doc$prcp_avg <- GA_doc$prcp_avg *0.1
GA_doc$tmax_avg <- GA_doc$tmax_avg *0.1
GA_doc$tmin_avg <- GA_doc$tmin_avg *0.1

## find the tavg for state data 
GA_doc$tavg <- ((GA_doc$tmax + GA_doc$tmin) / 2)

### save data as xlsx file 
write.xlsx(GA_doc, file = "GA_doc.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

###### illionis ########

IL_sixmonths <- read.csv("IL_6months_weather.csv")
IL_previousyear <- read.csv("IL_previous_year_weather.csv")
IL_doc <- read.csv("IL_year_DOC_weather.csv")

IL_sixmonths$prcp_avg <- as.numeric(IL_sixmonths$prcp_avg)
IL_sixmonths$tmax_avg <- as.numeric(IL_sixmonths$tmax_avg)
IL_sixmonths$tmin_avg <- as.numeric(IL_sixmonths$tmin_avg)

IL_sixmonths$prcp_avg <- IL_sixmonths$prcp_avg *0.1
IL_sixmonths$tmax_avg <- IL_sixmonths$tmax_avg *0.1
IL_sixmonths$tmin_avg <- IL_sixmonths$tmin_avg *0.1


## find the tavg for state data 
IL_sixmonths$tavg <- ((IL_sixmonths$tmax + IL_sixmonths$tmin) / 2)

### save data as xlsx file 
write.xlsx(IL_sixmonths, file = "IL_sixmonths.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

IL_previousyear$prcp_avg <- as.numeric(IL_previousyear$prcp_avg)
IL_previousyear$tmax_avg <- as.numeric(IL_previousyear$tmax_avg)
IL_previousyear$tmin_avg <- as.numeric(IL_previousyear$tmin_avg)

IL_previousyear$prcp_avg <- IL_previousyear$prcp_avg *0.1
IL_previousyear$tmax_avg <- IL_previousyear$tmax_avg *0.1
IL_previousyear$tmin_avg <- IL_previousyear$tmin_avg *0.1

## find the tavg for state data 
IL_previousyear$tavg <- ((IL_previousyear$tmax + IL_previousyear$tmin) / 2)

### save data as xlsx file 
write.xlsx(IL_previousyear, file = "IL_previousyear.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

IL_doc$prcp_avg <- as.numeric(IL_doc$prcp_avg)
IL_doc$tmax_avg <- as.numeric(IL_doc$tmax_avg)
IL_doc$tmin_avg <- as.numeric(IL_doc$tmin_avg)

IL_doc$prcp_avg <- IL_doc$prcp_avg *0.1
IL_doc$tmax_avg <- IL_doc$tmax_avg *0.1
IL_doc$tmin_avg <- IL_doc$tmin_avg *0.1

## find the tavg for state data 
IL_doc$tavg <- ((IL_doc$tmax + IL_doc$tmin) / 2)

### save data as xlsx file 
write.xlsx(IL_doc, file = "IL_doc.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

######### indiana #########

IN_sixmonths <- read.csv("IN_6months_weather.csv")
IN_previousyear <- read.csv("IN_previous_year_weather.csv")
IN_doc <- read.csv("IN_year_DOC_weather.csv")

IN_sixmonths$prcp_avg <- as.numeric(IN_sixmonths$prcp_avg)
IN_sixmonths$tmax_avg <- as.numeric(IN_sixmonths$tmax_avg)
IN_sixmonths$tmin_avg <- as.numeric(IN_sixmonths$tmin_avg)

IN_sixmonths$prcp_avg <- IN_sixmonths$prcp_avg *0.1
IN_sixmonths$tmax_avg <- IN_sixmonths$tmax_avg *0.1
IN_sixmonths$tmin_avg <- IN_sixmonths$tmin_avg *0.1


## find the tavg for state data 
IN_sixmonths$tavg <- ((IN_sixmonths$tmax + IN_sixmonths$tmin) / 2)

### save data as xlsx file 
write.xlsx(IN_sixmonths, file = "IN_sixmonths.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

IN_previousyear$prcp_avg <- as.numeric(IN_previousyear$prcp_avg)
IN_previousyear$tmax_avg <- as.numeric(IN_previousyear$tmax_avg)
IN_previousyear$tmin_avg <- as.numeric(IN_previousyear$tmin_avg)

IN_previousyear$prcp_avg <- IN_previousyear$prcp_avg *0.1
IN_previousyear$tmax_avg <- IN_previousyear$tmax_avg *0.1
IN_previousyear$tmin_avg <- IN_previousyear$tmin_avg *0.1

## find the tavg for state data 
IN_previousyear$tavg <- ((IN_previousyear$tmax + IN_previousyear$tmin) / 2)

### save data as xlsx file 
write.xlsx(IN_previousyear, file = "IN_previousyear.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

IN_doc$prcp_avg <- as.numeric(IN_doc$prcp_avg)
IN_doc$tmax_avg <- as.numeric(IN_doc$tmax_avg)
IN_doc$tmin_avg <- as.numeric(IN_doc$tmin_avg)

IN_doc$prcp_avg <- IN_doc$prcp_avg *0.1
IN_doc$tmax_avg <- IN_doc$tmax_avg *0.1
IN_doc$tmin_avg <- IN_doc$tmin_avg *0.1

## find the tavg for state data 
IN_doc$tavg <- ((IN_doc$tmax + IN_doc$tmin) / 2)

### save data as xlsx file 
write.xlsx(IN_doc, file = "IN_doc.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

###### maryland ########
MY_sixmonths <- read.csv("maryland_6months_weather.csv")
MY_previousyear <- read.csv("maryland_previous_year_weather.csv")
MY_doc <- read.csv("maryland_year_DOC_weather.csv")

MY_sixmonths$prcp_avg <- as.numeric(MY_sixmonths$prcp_avg)
MY_sixmonths$tmax_avg <- as.numeric(MY_sixmonths$tmax_avg)
MY_sixmonths$tmin_avg <- as.numeric(MY_sixmonths$tmin_avg)

MY_sixmonths$prcp_avg <- MY_sixmonths$prcp_avg *0.1
MY_sixmonths$tmax_avg <- MY_sixmonths$tmax_avg *0.1
MY_sixmonths$tmin_avg <- MY_sixmonths$tmin_avg *0.1


## find the tavg for state data 
MY_sixmonths$tavg <- ((MY_sixmonths$tmax + MY_sixmonths$tmin) / 2)

### save data as xlsx file 
write.xlsx(MY_sixmonths, file = "MY_sixmonths.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

MY_previousyear$prcp_avg <- as.numeric(MY_previousyear$prcp_avg)
MY_previousyear$tmax_avg <- as.numeric(MY_previousyear$tmax_avg)
MY_previousyear$tmin_avg <- as.numeric(MY_previousyear$tmin_avg)

MY_previousyear$prcp_avg <- MY_previousyear$prcp_avg *0.1
MY_previousyear$tmax_avg <- MY_previousyear$tmax_avg *0.1
MY_previousyear$tmin_avg <- MY_previousyear$tmin_avg *0.1

## find the tavg for state data 
MY_previousyear$tavg <- ((MY_previousyear$tmax + MY_previousyear$tmin) / 2)

### save data as xlsx file 
write.xlsx(MY_previousyear, file = "MY_previousyear.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

MY_doc$prcp_avg <- as.numeric(MY_doc$prcp_avg)
MY_doc$tmax_avg <- as.numeric(MY_doc$tmax_avg)
MY_doc$tmin_avg <- as.numeric(MY_doc$tmin_avg)

MY_doc$prcp_avg <- MY_doc$prcp_avg *0.1
MY_doc$tmax_avg <- MY_doc$tmax_avg *0.1
MY_doc$tmin_avg <- MY_doc$tmin_avg *0.1

## find the tavg for state data 
MY_doc$tavg <- ((MY_doc$tmax + MY_doc$tmin) / 2)

### save data as xlsx file 
write.xlsx(MY_doc, file = "MY_doc.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

##### Wisconsin ########

WI_sixmonths <- read.csv("wisconsin_6months_weather.csv")
WI_previousyear <- read.csv("wisconsin_previous_year_weather.csv")
WI_doc <- read.csv("wisconsin_year_DOC_weather.csv")

WI_sixmonths$prcp_avg <- as.numeric(WI_sixmonths$prcp_avg)
WI_sixmonths$tmax_avg <- as.numeric(WI_sixmonths$tmax_avg)
WI_sixmonths$tmin_avg <- as.numeric(WI_sixmonths$tmin_avg)

WI_sixmonths$prcp_avg <- WI_sixmonths$prcp_avg *0.1
WI_sixmonths$tmax_avg <- WI_sixmonths$tmax_avg *0.1
WI_sixmonths$tmin_avg <- WI_sixmonths$tmin_avg *0.1


## find the tavg for state data 
WI_sixmonths$tavg <- ((WI_sixmonths$tmax + WI_sixmonths$tmin) / 2)

### save data as xlsx file 
write.xlsx(WI_sixmonths, file = "WI_sixmonths.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

WI_previousyear$prcp_avg <- as.numeric(WI_previousyear$prcp_avg)
WI_previousyear$tmax_avg <- as.numeric(WI_previousyear$tmax_avg)
WI_previousyear$tmin_avg <- as.numeric(WI_previousyear$tmin_avg)

WI_previousyear$prcp_avg <- WI_previousyear$prcp_avg *0.1
WI_previousyear$tmax_avg <- WI_previousyear$tmax_avg *0.1
WI_previousyear$tmin_avg <- WI_previousyear$tmin_avg *0.1

## find the tavg for state data 
WI_previousyear$tavg <- ((WI_previousyear$tmax + WI_previousyear$tmin) / 2)

### save data as xlsx file 
write.xlsx(WI_previousyear, file = "WI_previousyear.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

WI_doc$prcp_avg <- as.numeric(WI_doc$prcp_avg)
WI_doc$tmax_avg <- as.numeric(WI_doc$tmax_avg)
WI_doc$tmin_avg <- as.numeric(WI_doc$tmin_avg)

WI_doc$prcp_avg <- WI_doc$prcp_avg *0.1
WI_doc$tmax_avg <- WI_doc$tmax_avg *0.1
WI_doc$tmin_avg <- WI_doc$tmin_avg *0.1

## find the tavg for state data 
WI_doc$tavg <- ((WI_doc$tmax + WI_doc$tmin) / 2)

### save data as xlsx file 
write.xlsx(WI_doc, file = "WI_doc.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


###### Virginia #######

VI_sixmonths <- read.csv("virginia_6months_weather.csv")
VI_previousyear <- read.csv("virginia_previous_year_weather.csv")
VI_doc <- read.csv("virginia_year_DOC_weather.csv")

VI_sixmonths$prcp_avg <- as.numeric(VI_sixmonths$prcp_avg)
VI_sixmonths$tmax_avg <- as.numeric(VI_sixmonths$tmax_avg)
VI_sixmonths$tmin_avg <- as.numeric(VI_sixmonths$tmin_avg)

VI_sixmonths$prcp_avg <- VI_sixmonths$prcp_avg *0.1
VI_sixmonths$tmax_avg <- VI_sixmonths$tmax_avg *0.1
VI_sixmonths$tmin_avg <- VI_sixmonths$tmin_avg *0.1


## find the tavg for state data 
VI_sixmonths$tavg <- ((VI_sixmonths$tmax + VI_sixmonths$tmin) / 2)

### save data as xlsx file 
write.xlsx(VI_sixmonths, file = "VI_sixmonths.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

VI_previousyear$prcp_avg <- as.numeric(VI_previousyear$prcp_avg)
VI_previousyear$tmax_avg <- as.numeric(VI_previousyear$tmax_avg)
VI_previousyear$tmin_avg <- as.numeric(VI_previousyear$tmin_avg)

VI_previousyear$prcp_avg <- VI_previousyear$prcp_avg *0.1
VI_previousyear$tmax_avg <- VI_previousyear$tmax_avg *0.1
VI_previousyear$tmin_avg <- VI_previousyear$tmin_avg *0.1

## find the tavg for state data 
VI_previousyear$tavg <- ((VI_previousyear$tmax + VI_previousyear$tmin) / 2)

### save data as xlsx file 
write.xlsx(VI_previousyear, file = "VI_previousyear.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

VI_doc$prcp_avg <- as.numeric(VI_doc$prcp_avg)
VI_doc$tmax_avg <- as.numeric(VI_doc$tmax_avg)
VI_doc$tmin_avg <- as.numeric(VI_doc$tmin_avg)

VI_doc$prcp_avg <- VI_doc$prcp_avg *0.1
VI_doc$tmax_avg <- VI_doc$tmax_avg *0.1
VI_doc$tmin_avg <- VI_doc$tmin_avg *0.1

## find the tavg for state data 
VI_doc$tavg <- ((VI_doc$tmax + VI_doc$tmin) / 2)

### save data as xlsx file 
write.xlsx(VI_doc, file = "VI_doc.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

######## Texas ########

TX_sixmonths <- read.csv("TX_6months_weather.csv")
TX_previousyear <- read.csv("TX_previous_year_weather.csv")
TX_doc <- read.csv("TX_year_DOC_weather.csv")

TX_sixmonths$prcp_avg <- as.numeric(TX_sixmonths$prcp_avg)
TX_sixmonths$tmax_avg <- as.numeric(TX_sixmonths$tmax_avg)
TX_sixmonths$tmin_avg <- as.numeric(TX_sixmonths$tmin_avg)

TX_sixmonths$prcp_avg <- TX_sixmonths$prcp_avg *0.1
TX_sixmonths$tmax_avg <- TX_sixmonths$tmax_avg *0.1
TX_sixmonths$tmin_avg <- TX_sixmonths$tmin_avg *0.1


## find the tavg for state data 
TX_sixmonths$tavg <- ((TX_sixmonths$tmax + TX_sixmonths$tmin) / 2)

### save data as xlsx file 
write.xlsx(TX_sixmonths, file = "TX_sixmonths.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

TX_previousyear$prcp_avg <- as.numeric(TX_previousyear$prcp_avg)
TX_previousyear$tmax_avg <- as.numeric(TX_previousyear$tmax_avg)
TX_previousyear$tmin_avg <- as.numeric(TX_previousyear$tmin_avg)

TX_previousyear$prcp_avg <- TX_previousyear$prcp_avg *0.1
TX_previousyear$tmax_avg <- TX_previousyear$tmax_avg *0.1
TX_previousyear$tmin_avg <- TX_previousyear$tmin_avg *0.1

## find the tavg for state data 
TX_previousyear$tavg <- ((TX_previousyear$tmax + TX_previousyear$tmin) / 2)

### save data as xlsx file 
write.xlsx(TX_previousyear, file = "TX_previousyear.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

TX_doc$prcp_avg <- as.numeric(TX_doc$prcp_avg)
TX_doc$tmax_avg <- as.numeric(TX_doc$tmax_avg)
TX_doc$tmin_avg <- as.numeric(TX_doc$tmin_avg)

TX_doc$prcp_avg <- TX_doc$prcp_avg *0.1
TX_doc$tmax_avg <- TX_doc$tmax_avg *0.1
TX_doc$tmin_avg <- TX_doc$tmin_avg *0.1

## find the tavg for state data 
TX_doc$tavg <- ((TX_doc$tmax + TX_doc$tmin) / 2)

### save data as xlsx file 
write.xlsx(TX_doc, file = "TX_doc.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

######## Michigan ########

MI_sixmonths <- read.csv("michigan_6months_climate.csv")
MI_previousyear <- read.csv("michigan_previous_year_climate.csv")
MI_doc <- read.csv("michigan_yearDOC_climate.csv")

MI_sixmonths$prcp_avg <- as.numeric(MI_sixmonths$prcp_avg)
MI_sixmonths$tmax_avg <- as.numeric(MI_sixmonths$tmax_avg)
MI_sixmonths$tmin_avg <- as.numeric(MI_sixmonths$tmin_avg)

MI_sixmonths$prcp_avg <- MI_sixmonths$prcp_avg *0.1
MI_sixmonths$tmax_avg <- MI_sixmonths$tmax_avg *0.1
MI_sixmonths$tmin_avg <- MI_sixmonths$tmin_avg *0.1


## find the tavg for state data 
MI_sixmonths$tavg <- ((MI_sixmonths$tmax + MI_sixmonths$tmin) / 2)

### save data as xlsx file 
write.xlsx(MI_sixmonths, file = "MI_sixmonths.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

MI_previousyear$prcp_avg <- as.numeric(MI_previousyear$prcp_avg)
MI_previousyear$tmax_avg <- as.numeric(MI_previousyear$tmax_avg)
MI_previousyear$tmin_avg <- as.numeric(MI_previousyear$tmin_avg)

MI_previousyear$prcp_avg <- MI_previousyear$prcp_avg *0.1
MI_previousyear$tmax_avg <- MI_previousyear$tmax_avg *0.1
MI_previousyear$tmin_avg <- MI_previousyear$tmin_avg *0.1

## find the tavg for state data 
MI_previousyear$tavg <- ((MI_previousyear$tmax + MI_previousyear$tmin) / 2)

### save data as xlsx file 
write.xlsx(MI_previousyear, file = "MI_previousyear.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

MI_doc$prcp_avg <- as.numeric(MI_doc$prcp_avg)
MI_doc$tmax_avg <- as.numeric(MI_doc$tmax_avg)
MI_doc$tmin_avg <- as.numeric(MI_doc$tmin_avg)

MI_doc$prcp_avg <- MI_doc$prcp_avg *0.1
MI_doc$tmax_avg <- MI_doc$tmax_avg *0.1
MI_doc$tmin_avg <- MI_doc$tmin_avg *0.1

## find the tavg for state data 
MI_doc$tavg <- ((MI_doc$tmax + MI_doc$tmin) / 2)

### save data as xlsx file 
write.xlsx(MI_doc, file = "MI_doc.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

######## Ohio ########

OH_sixmonths <- read.csv("OH_6months_weather.csv")
OH_previousyear <- read.csv("OH_previous_year_weather.csv")
OH_doc <- read.csv("OH_year_DOC_weather.csv")

OH_sixmonths$prcp_avg <- as.numeric(OH_sixmonths$prcp_avg)
OH_sixmonths$tmax_avg <- as.numeric(OH_sixmonths$tmax_avg)
OH_sixmonths$tmin_avg <- as.numeric(OH_sixmonths$tmin_avg)

OH_sixmonths$prcp_avg <- OH_sixmonths$prcp_avg *0.1
OH_sixmonths$tmax_avg <- OH_sixmonths$tmax_avg *0.1
OH_sixmonths$tmin_avg <- OH_sixmonths$tmin_avg *0.1


## find the tavg for state data 
OH_sixmonths$tavg <- ((OH_sixmonths$tmax + OH_sixmonths$tmin) / 2)

### save data as xlsx file 
write.xlsx(OH_sixmonths, file = "OH_sixmonths.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

OH_previousyear$prcp_avg <- as.numeric(OH_previousyear$prcp_avg)
OH_previousyear$tmax_avg <- as.numeric(OH_previousyear$tmax_avg)
OH_previousyear$tmin_avg <- as.numeric(OH_previousyear$tmin_avg)

OH_previousyear$prcp_avg <- OH_previousyear$prcp_avg *0.1
OH_previousyear$tmax_avg <- OH_previousyear$tmax_avg *0.1
OH_previousyear$tmin_avg <- OH_previousyear$tmin_avg *0.1

## find the tavg for state data 
OH_previousyear$tavg <- ((OH_previousyear$tmax + OH_previousyear$tmin) / 2)

### save data as xlsx file 
write.xlsx(OH_previousyear, file = "OH_previousyear.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

OH_doc$prcp_avg <- as.numeric(OH_doc$prcp_avg)
OH_doc$tmax_avg <- as.numeric(OH_doc$tmax_avg)
OH_doc$tmin_avg <- as.numeric(OH_doc$tmin_avg)

OH_doc$prcp_avg <- OH_doc$prcp_avg *0.1
OH_doc$tmax_avg <- OH_doc$tmax_avg *0.1
OH_doc$tmin_avg <- OH_doc$tmin_avg *0.1

## find the tavg for state data 
OH_doc$tavg <- ((OH_doc$tmax + OH_doc$tmin) / 2)

### save data as xlsx file 
write.xlsx(OH_doc, file = "OH_doc.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

######## Oklahoma ########

OK_sixmonths <- read.csv("OK_6months_weather.csv")
OK_previousyear <- read.csv("OK_previous_year_weather.csv")
OK_doc <- read.csv("OK_year_DOC_weather.csv")

OK_sixmonths$prcp_avg <- as.numeric(OK_sixmonths$prcp_avg)
OK_sixmonths$tmax_avg <- as.numeric(OK_sixmonths$tmax_avg)
OK_sixmonths$tmin_avg <- as.numeric(OK_sixmonths$tmin_avg)

OK_sixmonths$prcp_avg <- OK_sixmonths$prcp_avg *0.1
OK_sixmonths$tmax_avg <- OK_sixmonths$tmax_avg *0.1
OK_sixmonths$tmin_avg <- OK_sixmonths$tmin_avg *0.1


## find the tavg for state data 
OK_sixmonths$tavg <- ((OK_sixmonths$tmax + OK_sixmonths$tmin) / 2)

### save data as xlsx file 
write.xlsx(OK_sixmonths, file = "OK_sixmonths.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

OK_previousyear$prcp_avg <- as.numeric(OK_previousyear$prcp_avg)
OK_previousyear$tmax_avg <- as.numeric(OK_previousyear$tmax_avg)
OK_previousyear$tmin_avg <- as.numeric(OK_previousyear$tmin_avg)

OK_previousyear$prcp_avg <- OK_previousyear$prcp_avg *0.1
OK_previousyear$tmax_avg <- OK_previousyear$tmax_avg *0.1
OK_previousyear$tmin_avg <- OK_previousyear$tmin_avg *0.1

## find the tavg for state data 
OK_previousyear$tavg <- ((OK_previousyear$tmax + OK_previousyear$tmin) / 2)

### save data as xlsx file 
write.xlsx(OK_previousyear, file = "OK_previousyear.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

OK_doc$prcp_avg <- as.numeric(OK_doc$prcp_avg)
OK_doc$tmax_avg <- as.numeric(OK_doc$tmax_avg)
OK_doc$tmin_avg <- as.numeric(OK_doc$tmin_avg)

OK_doc$prcp_avg <- OK_doc$prcp_avg *0.1
OK_doc$tmax_avg <- OK_doc$tmax_avg *0.1
OK_doc$tmin_avg <- OK_doc$tmin_avg *0.1

## find the tavg for state data 
OK_doc$tavg <- ((OK_doc$tmax + OK_doc$tmin) / 2)

### save data as xlsx file 
write.xlsx(OK_doc, file = "OK_doc.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

######### Utah Nevada ##########

UN_sixmonths <- read.csv("UN_6months_weather.csv")
UN_previousyear <- read.csv("UN_previous_year_weather.csv")
UN_doc <- read.csv("UN_year_DOC_weather.csv")

UN_sixmonths$prcp_avg <- as.numeric(UN_sixmonths$prcp_avg)
UN_sixmonths$tmax_avg <- as.numeric(UN_sixmonths$tmax_avg)
UN_sixmonths$tmin_avg <- as.numeric(UN_sixmonths$tmin_avg)

UN_sixmonths$prcp_avg <- UN_sixmonths$prcp_avg *0.1
UN_sixmonths$tmax_avg <- UN_sixmonths$tmax_avg *0.1
UN_sixmonths$tmin_avg <- UN_sixmonths$tmin_avg *0.1


## find the tavg for state data 
UN_sixmonths$tavg <- ((UN_sixmonths$tmax + UN_sixmonths$tmin) / 2)

### save data as xlsx file 
write.xlsx(UN_sixmonths, file = "UN_sixmonths.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

UN_previousyear$prcp_avg <- as.numeric(UN_previousyear$prcp_avg)
UN_previousyear$tmax_avg <- as.numeric(UN_previousyear$tmax_avg)
UN_previousyear$tmin_avg <- as.numeric(UN_previousyear$tmin_avg)

UN_previousyear$prcp_avg <- UN_previousyear$prcp_avg *0.1
UN_previousyear$tmax_avg <- UN_previousyear$tmax_avg *0.1
UN_previousyear$tmin_avg <- UN_previousyear$tmin_avg *0.1

## find the tavg for state data 
UN_previousyear$tavg <- ((UN_previousyear$tmax + UN_previousyear$tmin) / 2)

### save data as xlsx file 
write.xlsx(UN_previousyear, file = "UN_previousyear.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

UN_doc$prcp_avg <- as.numeric(UN_doc$prcp_avg)
UN_doc$tmax_avg <- as.numeric(UN_doc$tmax_avg)
UN_doc$tmin_avg <- as.numeric(UN_doc$tmin_avg)

UN_doc$prcp_avg <- UN_doc$prcp_avg *0.1
UN_doc$tmax_avg <- UN_doc$tmax_avg *0.1
UN_doc$tmin_avg <- UN_doc$tmin_avg *0.1

## find the tavg for state data 
UN_doc$tavg <- ((UN_doc$tmax + UN_doc$tmin) / 2)

### save data as xlsx file 
write.xlsx(UN_doc, file = "UN_doc.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


########## PNW ########

PNW_sixmonths <- read.csv("PNW_6months_weather.csv")
PNW_previousyear <- read.csv("PNW_previous_year_weather.csv")
PNW_doc <- read.csv("PNW_year_DOC_weather.csv")

PNW_sixmonths$prcp_avg <- as.numeric(PNW_sixmonths$prcp_avg)
PNW_sixmonths$tmax_avg <- as.numeric(PNW_sixmonths$tmax_avg)
PNW_sixmonths$tmin_avg <- as.numeric(PNW_sixmonths$tmin_avg)

PNW_sixmonths$prcp_avg <- PNW_sixmonths$prcp_avg *0.1
PNW_sixmonths$tmax_avg <- PNW_sixmonths$tmax_avg *0.1
PNW_sixmonths$tmin_avg <- PNW_sixmonths$tmin_avg *0.1


## find the tavg for state data 
PNW_sixmonths$tavg <- ((PNW_sixmonths$tmax + PNW_sixmonths$tmin) / 2)

### save data as xlsx file 
write.xlsx(PNW_sixmonths, file = "PNW_sixmonths.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

PNW_previousyear$prcp_avg <- as.numeric(PNW_previousyear$prcp_avg)
PNW_previousyear$tmax_avg <- as.numeric(PNW_previousyear$tmax_avg)
PNW_previousyear$tmin_avg <- as.numeric(PNW_previousyear$tmin_avg)

PNW_previousyear$prcp_avg <- PNW_previousyear$prcp_avg *0.1
PNW_previousyear$tmax_avg <- PNW_previousyear$tmax_avg *0.1
PNW_previousyear$tmin_avg <- PNW_previousyear$tmin_avg *0.1

## find the tavg for state data 
PNW_previousyear$tavg <- ((PNW_previousyear$tmax + PNW_previousyear$tmin) / 2)

### save data as xlsx file 
write.xlsx(PNW_previousyear, file = "PNW_previousyear.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

PNW_doc$prcp_avg <- as.numeric(PNW_doc$prcp_avg)
PNW_doc$tmax_avg <- as.numeric(PNW_doc$tmax_avg)
PNW_doc$tmin_avg <- as.numeric(PNW_doc$tmin_avg)

PNW_doc$prcp_avg <- PNW_doc$prcp_avg *0.1
PNW_doc$tmax_avg <- PNW_doc$tmax_avg *0.1
PNW_doc$tmin_avg <- PNW_doc$tmin_avg *0.1

## find the tavg for state data 
PNW_doc$tavg <- ((PNW_doc$tmax + PNW_doc$tmin) / 2)

### save data as xlsx file 
write.xlsx(PNW_doc, file = "PNW_doc.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
