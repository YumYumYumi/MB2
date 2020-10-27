#random numbers
runif(10, 0, 1)
sample(10,100,replace=T)
rnorm(3)

#quit
q() 
q("n") # exit R without saving 

summary()
plot()
str()
head()
ls()
rm()
getwd()
setwd()

union()
intersect() 
setdiff()

# Enter the monthly average precipitation values of Germany values directly into R:
prec_avg <- c(56, 46, 50, 53, 69, 83, 80, 62, 55, 60, 63)
plot(prec_avg)
plot(prec_avg, pch = 19, cex =2, col ="#00ff0060")
lines(lowess(prec_avg, f=.02))

###############################
## create a (random) set of numbers or two lists
## plot your data on x-y plot (scatterplot)
## change the plot setting (color, size)
## change the data for the plot e.g. log transformed
## try other plots e.g. boxplots
#################################

library(raster)
germany <- getData("GADM", country = "DEU", level =2) #get country borders, 
# other country codes can be found in the manual
plot(germany) 
prec <- getData("worldclim", var="prec", res=.5, lon = 10, lat =51) #get precipitation data, 
# lon and lat only needed for res =.5
plot(prec) #plot the precipitation 

prec_ger1 <- crop(prec, germany) # crop precipitation to extent of germany
spplot(prec_ger1)
prec_ger2 <- mask(prec_ger1, germany) #mask prec to shape of germany 
spplot(prec_ger2)
# germany borders: a vector file
# precipitation: a raster file
# spatial cropping: crop() commands reduces the extent
# spatial masking: mask() masks out all values outside the border
# spatial and non-spatial data need to be used in parallel


prec_avg <- cellStats(prec_ger2, stat = "mean")
# extract precipiation average of germany, other statistics possible as well

### task if you are already done: plot the precipiation, 
### re-run the code for different data and countries