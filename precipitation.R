library(raster)
korea<- getData("GADM", country = "KOR", level =2) #get country borders, 
# other country codes can be found in the manual
plot(korea) 
prec_kor <- getData("worldclim", var="prec", res=.5, lon = 127, lat =37) #get precipitation data, 
# lon and lat only needed for res =.5
plot(prec_kor) #plot the precipitation 

prec_kor1 <- crop(prec, korea) # crop precipitation to extent of germany
spplot(prec_kor1)
prec_kor2 <- mask(prec_kor1, korea) #mask prec to shape of germany 
spplot(prec_kor2)
prec_kor2 <- mask(prec_kor1, korea, inverse =T)
spplot(prec_kor2)

