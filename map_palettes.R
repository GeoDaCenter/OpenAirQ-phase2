library(raster)
library(classInt)
library(tidyr)

# setwd("Desktop/OpenAirQ-models")
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
pal <- hcl.colors(255, rev=T)
breaks_pal <- hcl.colors(20, rev=T)

# means map
# create means rasters
r_means <- stack(c(mean(all_raster[[1:54]], na.rm=TRUE),
                  mean(all_raster[[1:54 + 54]], na.rm=TRUE),
                  mean(all_raster[[1:54 + 54 + 54]], na.rm=TRUE)))
# pull all cell values
means_values <- as.vector(sapply(1:dim(r_means)[3], FUN=function(z) values(r_means[[z]])[!is.na(values(r_means[[z]]))]))
means_range <- c(min(means_values),
                 max(means_values))
# downsampled jenks breaks
means_breaks <- classIntervals(c(means_range, sample(means_values, 1000)), n=20, style="fisher")$brks

# means
names(r_means) <- c("base", "with outliers", "with spatial cv")
plot(r_means, col=pal, zlim=means_range) # continuous
plot(r_means, col=breaks_pal, breaks=means_breaks) # jenks

# months (2014)
# continuous
plot(all_raster[[1:10]], col=pal, zlim=range)
plot(all_raster[[1:10 + 54]], col=pal, zlim=range)
plot(all_raster[[1:10 + 54 + 54]], col=pal, zlim=range)
# jenks
plot(all_raster[[1:10]], col=breaks_pal, breaks=breaks)
plot(all_raster[[1:10 + 54]], col=breaks_pal, breaks=breaks)
plot(all_raster[[1:10 + 54 + 54]], col=breaks_pal, breaks=breaks)
# jenks, with artificially enhanced boundary at 10
plot(all_raster[[1:10 + 54 + 54]], col=hcl.colors(25, rev=T), breaks=sort(c(seq(9.998, 10.002, by=0.001), breaks)))
