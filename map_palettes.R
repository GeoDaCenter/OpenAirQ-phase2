library(raster)
library(classInt)
library(tidyr)
library(tmap)
library(rosm)

# tmap_mode("view")

setwd("C:\\Users\\Andrew\\Documents\\Work\\OpenAirQ-models")
# combine 3 model rasters
all_raster <- stack(c(stack("21counties/nn_21_base.grd"),
                      stack("21counties/nn_21_outlier.grd"),
                      stack("21counties/nn_21_spatialcv.grd")))
# pull all cell values from raster
all_values <- as.vector(sapply(1:dim(all_raster)[3], FUN=function(z) values(all_raster[[z]])[!is.na(values(all_raster[[z]]))]))
range <- c(min(all_values),
           max(all_values))
# downsampled jenks breaks
breaks <- classIntervals(c(range, sample(all_values, 1000)), n=20, style="fisher")$brks

# viridis color palettes
pal <- hcl.colors(255, pal="turku")
breaks_pal <- hcl.colors(20, pal="turku")

# condensed viridis color palette; move end point inside the range
semi_pal <- c(hcl.colors(100, pal="turku"), rep(pal[length(pal)], 155))

# means map
# create means rasters
r_means <- stack(c(mean(all_raster[[1:54]], na.rm=TRUE),
                  mean(all_raster[[1:54 + 54]], na.rm=TRUE),
                  mean(all_raster[[1:54 + 54 + 54]], na.rm=TRUE)))

base_year_means <- stack(c(mean(all_raster[[1:10]], na.rm=TRUE),
                           mean(all_raster[[11:21]], na.rm=TRUE),
                           mean(all_raster[[22:31]], na.rm=TRUE),
                           mean(all_raster[[32:42]], na.rm=TRUE),
                           mean(all_raster[[43:54]], na.rm=TRUE)))
names(base_year_means) <- c(2014, 2015, 2016, 2017, 2018)

# pull all cell values
means_values <- as.vector(sapply(1:dim(r_means)[3], FUN=function(z) values(r_means[[z]])[!is.na(values(r_means[[z]]))]))
means_range <- c(min(means_values),
                 max(means_values))
# downsampled jenks breaks
means_breaks <- classIntervals(c(means_range, sample(means_values, 1000)), n=20, style="fisher")$brks

basemap <- tm_shape(osm.raster(r_means)) + tm_rgb()

# means
names(r_means) <- c("base", "with outliers", "with spatial cv")
# plot(r_means, col=pal, zlim=means_range) # continuous
# plot(r_means, col=breaks_pal, breaks=means_breaks) # jenks

# months (2014)
# order: continuous, truncated-range continuous, jenks
# base model
# plot(all_raster[[1:10]], col=pal, zlim=range)
# plot(all_raster[[1:10]], col=semi_pal, zlim=range)
# plot(all_raster[[1:10]], col=breaks_pal, breaks=breaks)
# # outlier-containing model
# plot(all_raster[[1:10 + 54]], col=pal, zlim=range)
# plot(all_raster[[1:10 + 54]], col=semi_pal, zlim=range)
# plot(all_raster[[1:10 + 54]], col=breaks_pal, breaks=breaks)
# # spatial-cv model
# plot(all_raster[[1:10 + 54 + 54]], col=pal, zlim=range)
# plot(all_raster[[1:10 + 54 + 54]], col=semi_pal, zlim=range)
# plot(all_raster[[1:10 + 54 + 54]], col=breaks_pal, breaks=breaks)
# jenks, with artificially enhanced boundary at 10
# plot(all_raster[[1:10 + 54 + 54]], col=hcl.colors(25, rev=T), breaks=sort(c(seq(9.998, 10.002, by=0.001), breaks)))

# tmap
basemap + tm_shape(r_means) + tm_raster(alpha=0.7, palette=semi_pal, style="cont", title="") # means
basemap + tm_shape(base_year_means) + tm_raster(alpha=0.7, palette=semi_pal, style="cont", title="") # annual means (base model)
tm_shape(all_raster[[1:10]]) + tm_raster(alpha=0.7, palette=semi_pal, style="cont", title="") + tm_facets(nrow=3) # base
tm_shape(all_raster[[1:10 + 54]]) + tm_raster(alpha=0.7, palette=semi_pal, style="cont", title="") + tm_facets(nrow=3) # outlier
tm_shape(all_raster[[1:10 + 54 + 54]]) + tm_raster(alpha=0.7, palette=semi_pal, style="cont", title="") + tm_facets(nrow=3) # spatial cv
