library(rgdal)
library(sp)
library(raster)

# ------------------------------------------------------------------------------
sh <- readOGR(dsn= "Shapefile", layer = "land_boundary")
rs <- raster("Environment/Elevation.tif")
plot(rs)
plot(sh, add=T)


# projections
BEHRMANN <- CRS("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
WGS84 <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
