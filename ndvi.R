library(raster)
library(rgdal)
library(RColorBrewer)
library(tidyverse)

data<- stack(c("B2.tif", "B3.tif", "B4.tif", "B5.tif"))

ndvi<- (data$B5 - data$B4) / (data$B5 + data$B4)

plot(ndvi)

  datan<- as.data.frame(getValues(ndvi))

datan<- datan %>% rename(ndvi =`getValues(ndvi)`)
hist(ndvi)

x<- raster(ncol= 10, nrow= 10)

projection(x)<- "+proj=utm +zone=48 +datum=WGS84"
x

plot(x)
