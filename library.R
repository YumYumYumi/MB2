packageNames <- c("plyr", "ggplot2","rgeos", "maptools", "scales", 
                  "raster", "sf", "sp", "rgdal", "ggradar", "ggspatial",
                  "dplyr", "RStoolbox")
lapply(packageNames, library, character.only=TRUE)
