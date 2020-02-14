---
title: "Useful R commands - Global biodiversity patterns"
author: "Petr Keil"
date: "February 13, 2020"
output: 
  html_document: 
    highlight: pygments
    keep_md: yes
    number_sections: yes
    theme: cerulean
    toc: yes
---



# Libraries

For GIS operations


```r
library(raster) # for raster data
library(sp)     # for vector data
library(rgdal)  # for projections
```

For statistics and data visualization

```r
library(randomForest) # for random forest analysis
library(ggplot2) # for pretty graphics
library(broom)   # for preparation of shapefiles for ggplot2
```

# Projections

We will work with **Behrmann** equal-area projection. The other useful projection is
the **WGS84**, which is what Lat and Lon is.


```r
BEHRMANN <- CRS("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
WGS84 <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
```

# File operations


```r
list.files()
file.copy()
```

# Rasters

Loading raster


```r
x <- raster("path to your raster")
```

Plotting raster x


```r
plot(x)
```

Getting all values from a raster


```r
x[]
```

Replacing values in raster x


```r
x[x > 0] <- 1 # replace all values greater than 0 by 1
```

Summing up all values in single raster x


```r
sum(x[])
```

Summing corresponding values in two rasters x1 and x2 to produce a third raster


```r
sum(x1, x2)
```

Getting spatial coordinates (x and y) from a raster x


```r
coordinates(x)
```

Coarsening-up raster x


```r
aggregate(x, fact = 2) # here by the factor of 2
```

Stacking rasters x1 and x2 and other rasters


```r
stack(x1, x2, ...)
```

# Shapefiles

Load a shapefile


```r
readOGR(dsn= "Shapefile directory", layer = "Shapefile name")
```

# Analysis 

## Exploring relationships in the data


```r
pairs()
```

## Explaining ```y``` by ```x```

Generalized linear models (GLM)


```r
M <- glm(y ~ x, data = yourdata)
summary(M)
termplot(M, partial.resid=TRUE, se=TRUE)
```

Random forests


```r
RF <- randomForest(y ~ x, data = yourdata)
RF
summary(RF)
varImpPlot(RF)
partialPlot(RF, pred.data=yourdata, x.var="your variable")
```
