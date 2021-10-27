library(raster)
library(dplyr)
library(tidyr)
library(gstat)
library(stringr)
library(lubridate)
library(sf)
library(tmap)

# setwd("~/Downloads")

state_raster <- stack('ps+road/ps.grd')[[1]] # change this lol
county <- stack("Master_Raster/Master_Raster.grd")
mod.data <- rbind(read.csv("faa_2014_2016_week.csv"),
                 read.csv("faa_2017_2018_week.csv")) %>% 
  rename(moyr = week)
geometry <- mod.data[,c("lon", "lat")] %>% 
  split(seq(nrow(mod.data))) %>% 
  lapply(FUN=function(z) st_point(as.matrix(z))) %>% 
  st_sfc(crs=4326)
points <- st_sf(mod.data[, !(colnames(mod.data) %in% c("X", "station", "lon", "lat"))], geometry=geometry, crs=4326) %>%
  st_transform(crs=32616)
xy <- as.matrix(mod.data[,c("lon", "lat")]) %>%
  st_multipoint() %>%
  st_sfc(crs=4326) %>%
  st_transform(crs=32616) %>%
  st_coordinates()
mod.data$Raster.Cell <- cellFromXY(state_raster, xy[, -3])
mod.data <- mod.data %>%
  dplyr::select(Raster.Cell, mslp, tmpf, p01i, moyr, lon, lat)

# group by month
to.month <- function(name) {
  time <- strptime(paste0(name, "-1"), format=paste0("%Y-%W-%w"))
  return(paste(month(time), year(time) - 2000, sep="_"))
}
mod.data$month <- sapply(mod.data$moyr, to.month)

# group by quarter
to.quarter <- function(name) {
  split <- strsplit(name, "-")[[1]]
  year <- as.numeric(split[1]) - 2000
  quarter <- max(1, floor((as.numeric(split[2]) - 1) / 13) + 1)
  return(paste(quarter, year, sep="_"))
}
mod.data$qtr <- sapply(mod.data$moyr, to.quarter)
mod.data <- mod.data %>%
  rename(
    Pressure = mslp,
    Temp = tmpf,
    Precip = p01i
  ) %>%
  dplyr::select(!moyr)
month.mod.data <- mod.data %>%
  group_by(lon, lat, month) %>%
  summarise(
    Pressure = mean(Pressure, na.rm = TRUE),
    Temp = mean(Temp, na.rm = TRUE),
    Precip = mean(Precip, na.rm = TRUE)
  )
month.wide.mod.data <- month.mod.data %>%
  pivot_wider(
    names_from = month,
    values_from = c(Pressure, Temp, Precip)
  )
month.sf.mod.data <- month.wide.mod.data %>%
  st_as_sf(
    coords = c("lon", "lat"),
    crs = 4326
  )
month.is.21 <- rowSums(as.data.frame(raster::extract(county, month.sf.mod.data)), na.rm = TRUE) > 0
month.mod.data.21 <- month.sf.mod.data[month.is.21, ]
st_write(month.mod.data.21, "FAA_Monthly.geojson")

quarter.mod.data <- mod.data %>%
  group_by(lon, lat, qtr) %>%
  summarise(
    Pressure = mean(Pressure, na.rm = TRUE),
    Temp = mean(Temp, na.rm = TRUE),
    Precip = mean(Precip, na.rm = TRUE)
  )
# convert to wide
wide.mod.data <- quarter.mod.data %>%
  pivot_wider(
    names_from = qtr,
    values_from = c(Pressure, Temp, Precip)
  )
# convert to geojson
sf.mod.data <- wide.mod.data %>%
  st_as_sf(
    coords = c("lon", "lat"),
    crs = 4326
  )
is.21 <- rowSums(as.data.frame(raster::extract(county, sf.mod.data)), na.rm = TRUE) > 0
mod.data.21 <- sf.mod.data[is.21, ]
st_write(mod.data.21, "FAA_Quarterly.geojson")

master <- stack(state_raster)
num.cells = ncell(state_raster)
period.names = unique(mod.data$moyr)
num.periods = length(period.names) 
period.vec = 0:num.periods

# For Loop to separate out the data in different Raster Layers
start.row = 1
for(i in 1:num.periods){
  # Extract Data for Period i
  period.data = mod.data %>% filter(moyr == period.names[i])
  
  # Create a df containing all Raster.cell Numbers
  df = data.frame(1:num.cells)
  colnames(df) = "Raster.Cell"
  
  # Left join data of period i to raste cell df, this accounts for missing cell values
  full.data = left_join(df, period.data) %>% dplyr::select(-moyr)
  
  # Insert Period i full data into RasterLayer
  master[[i]] = full.data$p01i
  names(master)[i] <- paste(period.names[i], "p01i", sep="-")
}

interpolate <- function(layer) {
  cells <- as.data.frame(rasterToPoints(state_raster))
  df_1 <- left_join(cells, as.data.frame(rasterToPoints(layer)))
  ## Joining, by = c("x", "y")
  coordinates(df_1) <- ~x+y
  proj4string(df_1) <- CRS("+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs")
  layername <- names(layer)
  
  valid = !is.na(df_1[, layername]@data[, 1])
  predictions = idw(get(layername)~1, 
                    locations=df_1[valid,,drop=FALSE],
                    newdata=df_1[!valid,,drop=FALSE],
                    nmax=8,
                    idp=0.5)
  newdata = df_1[, layername]@data[, 1]
  newdata[!valid] = predictions$var1.pred
  r1 <- rasterFromXYZ(cbind(as.data.frame(df_1)[, c("x", "y")], newdata))
  r1 <- resample(r1, state_raster)
  r1_df <- as.data.frame(r1, xy=TRUE)
  r1_df$Raster.Cell <- cellFromXY(r1, as.matrix(r1_df[,c("x", "y")]))
  dates <- as.integer(str_split_fixed(substr(layername, 2, str_length(layername)), coll("."), 3))
  r1_df$year <- dates[1] - 2000
  r1_df$week <- dates[2]
  r1_df <- r1_df %>% dplyr::select(-x, -y) %>% rename(p01i = newdata)
  return (r1_df)
}

interpolate_to_raster <- function(layer) {
  cells <- as.data.frame(rasterToPoints(state_raster))
  df_1 <- left_join(cells, as.data.frame(rasterToPoints(layer)))
  ## Joining, by = c("x", "y")
  coordinates(df_1) <- ~x+y
  proj4string(df_1) <- CRS("+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs")
  layername <- names(layer)
  
  valid = !is.na(df_1[, layername]@data[, 1])
  predictions = idw(get(layername)~1, 
                    locations=df_1[valid,,drop=FALSE],
                    newdata=df_1[!valid,,drop=FALSE],
                    nmax=8,
                    idp=0.5)
  newdata = df_1[, layername]@data[, 1]
  newdata[!valid] = predictions$var1.pred
  r1 <- rasterFromXYZ(cbind(as.data.frame(df_1)[, c("x", "y")], newdata))
  r1 <- resample(r1, state_raster)
  return (r1)
}

# interpolated <- lapply(1:dim(master)[3], FUN=function(z) interpolate(master[[z]])) %>%
#   bind_rows() # needs splitting?
# interpolated <- lapply(1:88, FUN=function(z) interpolate(master[[z]])) %>%
#   bind_rows() # needs splitting?

out <- stack(master[[1]])

for (idx in 1:nlayers(master)) {
  print(idx)
  out[[idx]] <- interpolate_to_raster(master[[idx]])
}

names(out) <- names(master)

writeRaster(out[[88 + 88 + 1:88]], "4st_p01i_177-264.grd")

test <- stack(master[[1:4]])
for (i in 0:3) {
  test[[i + 1]] <- interpolated[interpolated$week == i, ]$p01i
}
tm_shape(test) + tm_raster(alpha=1, style="cont") + tm_shape(points[points$moyr == "2014-00", ]) + tm_dots()

write.csv(interpolated, "4st_p01i_1-88.csv", row.names=FALSE)

mslp.reproj = projectRaster(out, crs=crs("+init=epsg:4326"))

county <- stack("Master_Raster/Master_Raster.grd")

mslp.21 <- resample(mslp.reproj, county[[1]]) %>%
  mask(county[[1]])

writeRaster(mslp.21, "21_mslp.grd")


out <- stack(c(
  stack("4st_mslp_1-88"),
  stack("4st_mslp_89-176"),
  stack("4st_mslp_177-264")
))

county_2 <- stack("idw data/master_updated.grd")

to.quarter <- function(name) {
  split <- strsplit(name, "\\.")[[1]]
  year <- as.numeric(substr(split[1], 2, 5)) - 2000
  variable <- switch(split[3],
                     "mslp" = "Pressure",
                     "tmpf" = "Temp",
                     "p01i" = "Precip"
                     )
  quarter <- max(1, floor((as.numeric(split[2]) - 1) / 13) + 1)
  return(paste(variable, quarter, year, sep="_"))
}

# mslp
quarter.names <- sapply(names(mslp.21), to.quarter)
quarter.names <- factor(quarter.names, levels=unique(quarter.names))

new.quarters.mslp <- stackApply(mslp.21, quarter.names, fun=mean) %>%
  stack()

names(new.quarters.mslp) <- levels(quarter.names)

# temp
quarter.names <- sapply(names(temp.21), to.quarter)
quarter.names <- factor(quarter.names, levels=unique(quarter.names))

new.quarters.temp <- stackApply(temp.21, quarter.names, fun=mean) %>%
  stack()

names(new.quarters.temp) <- levels(quarter.names)

#p01i
quarter.names <- sapply(names(p01i.21), to.quarter)
quarter.names <- factor(quarter.names, levels=unique(quarter.names))

new.quarters.p01i <- stackApply(p01i.21, quarter.names, fun=mean) %>%
  stack()

names(new.quarters.p01i) <- levels(quarter.names)

# replacing layers
master.old <- county_2[[which(!names(county_2) %in% names(new.quarters.mslp))]]
master.new <- addLayer(master.old, new.quarters.mslp)
master.old <- master.new[[which(!names(master.new) %in% names(new.quarters.temp))]]
master.new <- addLayer(master.old, new.quarters.temp)
master.old <- master.new[[which(!names(master.new) %in% names(new.quarters.p01i))]]
master.new <- addLayer(master.old, new.quarters.p01i)

writeRaster(master.new, "master_20210930.grd")
