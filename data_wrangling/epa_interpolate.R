library(sf)
library(raster)
library(tidyr)
library(dplyr)
library(gstat)

setwd("~/Downloads/idw data")

# _m is monthly, _q is quarterly
# this nomenclature will persist through the code
points_m <- st_read("all_points.geojson")
points_q <- st_read("all_points_quarter.geojson")

# base rasters
county <- stack("../Master_Raster/Master_Raster.grd")
master <- stack(county[[1]])

points_m$cell <- cellFromXY(master, st_coordinates(points_m))
points_q$cell <- cellFromXY(master, st_coordinates(points_q))

# two sensors fall in the same grid cell; we will take the mean
points_m <- points_m %>%
  group_by(cell) %>%
  summarise_all(mean)

points_q <- points_q %>%
  group_by(cell) %>%
  summarise_all(mean)

cols_m <- which(grepl("_", colnames(points_m)))
cols_q <- which(grepl("_", colnames(points_q)))

cell_col_m <- which(colnames(points_m) == "cell")
cell_col_q <- which(colnames(points_q) == "cell")

num.cells <- ncell(master)

# only run one of the next 2 loops (plus name statement after)
# loop to insert monthly data into raster
for(i in 1:length(cols_m)){
  # Extract Data for Period i
  period.data <- points_m[, c(cols_m[i], cell_col_m)] %>%
    st_drop_geometry()
  
  # Create a df containing all Raster.cell Numbers
  df <- data.frame(1:num.cells)
  colnames(df) <- "cell"
  
  # Left join data of period i to raster cell df, this accounts for missing cell values
  full.data <- left_join(df, period.data)
  
  # Insert Period i full data into RasterLayer
  master[[i]] <- full.data[, -1]
}
names(master) <- colnames(points_m)[cols_m]

# loop to insert quarterly data into raster
for(i in 1:length(cols_q)){
  # Extract Data for Period i
  period.data <- points_q[, c(cols_q[i], cell_col_q)] %>%
    st_drop_geometry()
  
  # Create a df containing all Raster.cell Numbers
  df <- data.frame(1:num.cells)
  colnames(df) <- "cell"
  
  # Left join data of period i to raster cell df, this accounts for missing cell values
  full.data <- left_join(df, period.data)
  
  # Insert Period i full data into RasterLayer
  master[[i]] <- full.data[, -1]
  names(master)[i] <- colnames(period.data)[1]
}
names(master) <- colnames(points_q)[cols_q]

interpolate_to_raster <- function(layer) {
  cells <- as.data.frame(rasterToPoints(county[[1]]))
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
  r1 <- resample(r1, county[[1]])
  return (r1)
}

out <- stack(master[[1]])

for (idx in 1:nlayers(master)) {
  print(idx)
  out[[idx]] <- interpolate_to_raster(master[[idx]])
}

plot(out[[1:12]])

master_old <- stack("Master_Raster/master_raster.grd")

# choose _m or _q based on loop choice
# rename, and combine with old master raster (_q only)
names(out) <- colnames(points_m)[cols_m]
writeRaster(out, "epa_monthly.grd")

names(out) <- colnames(points_q)[cols_q]
master_old <- master_old[[which(!names(master_old) %in% names(out))]]
master_new <- addLayer(master_old, out)
writeRaster(master_new, "master_20211028.grd")
