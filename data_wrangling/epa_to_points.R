library(sp)
library(rgdal)
library(rgeos)
library(leaflet)
library(raster)
library(gstat)
library(tmap)
library(tidyverse)
library(lubridate)
library("spdplyr")
library(velox)

# setwd("~/Downloads/idw data")

### files, names should change by variable
pm25.2014 <- read_csv("daily_88101_2014.csv")
pm25.2015 <- read_csv("daily_88101_2015.csv")
pm25.2016 <- read_csv("daily_88101_2016.csv")
pm25.2017 <- read_csv("daily_88101_2017.csv")
pm25.2018 <- read_csv("daily_88101_2018.csv")

pm25 <- rbind(pm25.2014, pm25.2015, pm25.2016, pm25.2017, pm25.2018)

pm25$id <- paste(pm25$`State Code`, pm25$`County Code`, pm25$`Site Num`, sep="")

# km.grid <- readOGR("Km_Grid")
# new.crs <- proj4string(km.grid)
new.crs <- CRS("+init=epsg:4326")

lac <- readOGR("LargeAreaCounties")

#Set lat/lon
coordinates(pm25) <- pm25[,c("Longitude", "Latitude")]

#Set projection to WSG84
proj4string(pm25) <- CRS("+init=epsg:4269")
pm25 <- spTransform(pm25, new.crs)

area.pm25 <- pm25[lac,]


area.pm25$moyr <- dmy(paste(01, month(area.pm25$ `Date Local`), year(area.pm25$`Date Local`), sep = "-"))


### Monthly Aggregation
area.pm25.monthly <- area.pm25@data %>% 
  group_by(id, moyr, Latitude, Longitude) %>%
  summarise(Monthly_Mean = base::mean(`Arithmetic Mean`))

mos <- 1:12
yrs <- 2014:2018
names <- c()

for(i in 1:length(yrs)) {
  this.yr <- paste("PM25", mos, substring(yrs[i], 3, 4), sep = "_")
  names <- c(names, this.yr)
}
names <- make.names(names)
names
# names(lac) <- names
# lac@data
# writeOGR(lac, ".", "PM25_Counties_Monthly", driver = "ESRI Shapefile")

name.converter <- function(name) {
  if (!is.na(as.integer(substr(name, 1, 1)))) {
    return (
      as.integer(substr(name, 1, 4)) * 100 +
        as.integer(substr(name, 6, 7))
    )
  }
  else {
    split <- strsplit(name, "_")[[1]]
    return (
      200000 +
        as.integer(split[3]) * 100 +
        as.integer(split[2])
    )
  }
}

area.pm25.monthly$moyr <- names[match(sapply(area.pm25.monthly$moyr, name.converter),
                                      sapply(names, name.converter))]

pm25.monthly.wide <- area.pm25.monthly %>% 
  pivot_wider(
    names_from = moyr,
    values_from = Monthly_Mean
  )

coordinates(area.pm25.monthly) <- area.pm25.monthly[,c("Longitude", "Latitude")]
proj4string(area.pm25.monthly) <- new.crs

coordinates(pm25.monthly.wide) <- pm25.monthly.wide[,c("Longitude", "Latitude")]
proj4string(pm25.monthly.wide) <- new.crs

writeOGR(pm25.monthly.wide, "PM25_Sensors_Monthly.geojson", "pm25.monthly.wide", driver = "GeoJSON", overwrite_layer = TRUE)