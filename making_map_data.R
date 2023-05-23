##making map of data
#install.packages("tidyverse")
#install.packages("sf")
#install.packages("mapview")
library(tidyverse)
library(sf)
library(mapview)
library(dplyr)
install.packages('factoextra')
library(factoextra)
library(ggplot2)
install.packages("ggbiplot")
setwd("~/Documents/")

## new york maps 
nyc_plants <- read_csv("collection_data_for_map.csv")
mapview(nyc_plants, xcol = "longitude", ycol = "latitude", crs = 4269, grid = FALSE)
flowering <- nyc_plants %>% filter(flowered== "yes")
mapview(flowering, xcol = "longitude", ycol = "latitude", crs = 4269, grid = FALSE)
planted <- nyc_plants %>% filter(grown== "yes")
mapview(planted, xcol = "longitude", ycol = "latitude", crs = 4269, grid = FALSE)
mapview(nyc_plants, xcol = "longitude", ycol = "latitude", zcol = "grown", legend = TRUE,crs = 4269, grid = FALSE)


#texas maps
texas_plants <- read_csv("texasm_map_data.csv")
texas_plants <- as.data.frame(texas_plants)
texas_plants$latitude <- as.numeric(texas_plants$latitude)
texas_plants$longitude <- as.numeric(texas_plants$longitude)
texas_plants <- na.omit(texas_plants)
mapview(texas_plants, xcol = "longitude", ycol = "latitude", crs = 4269, grid = FALSE)
mapview(texas_plants, xcol = "longitude", ycol = "latitude", zcol = "Type", crs = 4269, grid = FALSE)


####Arizona maps
arizona_maps <- read_csv("Arizona_maps.csv")
arizona_maps <- as.data.frame(arizona_maps)
arizona_maps$Latitude <- as.numeric(arizona_maps$Latitude)
arizona_maps$Longitude <- as.numeric(arizona_maps$Longitude)
arizona_maps <- na.omit(arizona_maps)
mapview(arizona_maps, xcol = "Longitude", ycol = "Latitude", crs = 4269, grid = FALSE)

##partial map
partial_maps <- read_csv("Partial_map_list_notype.csv")
partial_maps$Latitude <- as.numeric(partial_maps$Latitude)
partial_maps$Longitude <- as.numeric(partial_maps$Longitude)
partial_maps <- na.omit(partial_maps)
mapview(partial_maps, xcol = "Longitude", ycol = "Latitude", crs = 4269, grid = FALSE)

#map by type few states
map_type <- read.csv("map_by_type.csv")
map_type$Latitude <- as.numeric(map_type$Latitude)
map_type$Longitude <- as.numeric(map_type$Longitude)
map_type <- na.omit(map_type)
mapview(map_type, xcol = "Longitude", ycol = "Latitude", crs = 4269, grid = FALSE)
mapview(map_type, xcol = "Longitude", ycol = "Latitude", zcol = "Type", crs = 4269, grid = FALSE)
mapview(map_type, xcol = "Longitude", ycol = "Latitude", zcol = "Year", crs = 4269, grid = FALSE)


nyc_plants <- read_csv("four_temp_exp_genotypes_map.csv")
mapview(nyc_plants, xcol = "longitude", ycol = "latitude", zcol = "ID", crs = 4269, grid = FALSE)
png(filename = "fourgenotypestempexp.png")
dev.off()

total.map <- na.omit(total_data)
mapview(total.map, xcol = "longitude", ycol = "latitude", zcol = "climate_region", legend = TRUE,crs = 4269, grid = FALSE)

#palette function to get colors 
mapviewOptions(vector.palette = colorRampPalette(RColorBrewer::brewer.pal(9, "Reds")))




