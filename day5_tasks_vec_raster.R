###################################
## 2. Tasks (yeah) 
####################################
# use the raster and vector data of “Steigerwald”
# plot raster and vector data in one map
# explore other mapping options and aim to generate new visual information

## data import (raster.tif)######################################################
S2_2018 <- raster("./day5_zip/S2_2018_04_20.tif")
S2_2018_br <- brick("./day5_zip/S2_2018_04_20.tif")
S2_2017 <- raster("./day5_zip/S2_2017_06_19.tif")
S2_2017_br <- brick("./day5_zip/S2_2017_06_19.tif")
S1_2016_VV <- raster("./day5_zip/S1A_IW_GRDH_1SDV_20161002_013314_015387_VV.tif")
S1_2016_VV_br <- brick("./day5_zip/S1A_IW_GRDH_1SDV_20161002_013314_015387_VV.tif")
S1_2015_VH <- raster("./day5_zip/S1A_IW_GRDH_1SDV_20150306_004914_00621B_VH.tif")
S1_2015_VH_br <- brick("./day5_zip/S1A_IW_GRDH_1SDV_20150306_004914_00621B_VH.tif")
SRTM_br <- brick("./day5_zip/SRTM.tif")
#################################################################################

## data import (vec. shp)########################################################
#points
DFD_LUCAS_LC <- read_sf("./day5_zip/DFD_LUCAS_LC.shp") #Pnt
ggplot() + geom_sf(alpha = 0.5, data =DFD_LUCAS_LC)
# 02_osm ##########################################
road <- read_sf("./day5_zip/02_osm/Roads.shp") 
road <- read_sf("./day5_zip/02_osm", layer = "Roads") #line
W_B_Ansbach <- read_sf("./day5_zip/02_osm/Wuerzburg_Bayreuth_Ansbach.shp") #pnt
waterWays <- read_sf("./day5_zip/02_osm/Water_ways.shp") #lines
villagesNearForest <- read_sf("./day5_zip/02_osm/villages_near_forest.shp") #pnt
station <- read_sf("./day5_zip/02_osm", layer = "Station") #pnt
landuse <- read_sf("./day5_zip/02_osm", layer = "Landuse") #pol
buildings <- read_sf("./day5_zip/02_osm", layer = "Buildings") #pol

# 03_forest#######################################
  # 01_forest_structure  # they are multipoints 
reas_station_str <- read_sf("./day5_zip/03_forest/01_forest_structure", 
                            layer = "research_station_Steigerwald_forest_structure")
For_station_str <- read_sf("./day5_zip/03_forest/01_forest_structure", 
                           layer = "Forschungsstation_Steigerwald_Waldstruktur")
ggplot() + geom_sf(alpha = 0.5, data =For_station_str )
  # 02_forest_border # they are polygons
fst_border_utm32N <- read_sf("./day5_zip/03_forest/02_forest_border", 
                             layer = "forest_border_utm32N")
fst_border <- read_sf("./day5_zip/03_forest/02_forest_border", 
                      layer = "forest_border")
ggplot() + geom_sf(alpha = 0.5, data =fst_border )

# 06_birds ########################################## (pnts)
Ueb_Voeg_utm <- read_sf("./day5_zip/06_birds", 
                        layer = "UebungsdatenVoegel261Einhektarplots_utm")
Ueb_Voeg <- read_sf("./day5_zip/06_birds", 
                    layer = "UebungsdatenVoegel261Einhektarplots")
ggplot() + geom_sf(alpha = 0.5, data = Ueb_Voeg)

S2_2018.df <- data.frame(coordinates(S2_2018_br), getValues(S2_2018_br))
#################################################################################

coordinates(SRTM)
extent(SRTM)
extent(S2_2017_br)
###
ggplot() + ggRGB(img = S2_2018_br, 3,2,1, 
                 stretch =  'hist', 
                 ggLayer = T) +
  geom_sf(alpha = 0.8, data = road)

####
plot(S2_2018_br)
plot(SRTM_br)
plot(S1_2015_VH_br)
####

ggR(S2_2018_br, 1:6, geom_raster = T)
ggRGB(S2_2018_br, 3, 2, 1, stretch = "log")

ggR(S2_2018_br, stretch = "lin", geom_raster = T, ggLayer = T) + 
  scale_fill_gradient() +
  geom_sf(fst_border)

ggplot(lsat.df) + geom_raster(aes(x=x, y=y, fill =B3_dn)) 

##############
ggplot() + 
  #geom_sf(alpha = 0.9, data =border_utm32) +
  geom_sf(alpha = 0.3, data = road) +
  #geom_sf(alpha = 0.3, data = W_B_Ansbach) + 
  geom_sf(alpha = 0.3, data = waterWays) +
  #geom_sf(alpha = 0.3, data = villagesNearForest) +
  geom_sf(alpha = 0.3, data = station) +
  geom_sf(alpha = 0.3, data = landuse, aes(color = fclass)) + 
  geom_sf(alpha = 0.3, data = buildings)#

ggplot() +  ggRGB(img = S2_2018_br, 4,3,2, 
                  stretch =  'hist', 
                  ggLayer = T) +
  geom_sf(alpha = 0.1, data = landuse, aes(color = fclass)) #+ 
  #geom_sf_label(data = landuse, aes(label = fclass))

#put crs
st_crs() <- st_crs()

######################################## 
## Yo Let's make a (maybe) reasonable map yeh ## 
########################################

ggplot() +  ggRGB(img = S2_2018_br, 5,4,3, 
                  stretch =  'lin', 
                  ggLayer = T) +
  geom_sf(alpha = 0.4, data = road, color = "green") + 
  geom_sf(alpha = 0.4, data = waterWays, color = "blue") 

###############################################################################
#crs check, just in case 
fst_border_utm32N_crop <- fst_border_utm32N %>% st_transform(st_crs(road))
st_crs(road) == st_crs(fst_border_utm32N_crop)
#clip polygon by another polygon 
fst_border_utm32N_crop <- st_crop(fst_border_utm32N$geometry, road$geometry)
road_mask <- road %>% st_transform(st_crs(fst_border_utm32N))
st_crs()
road_mask <- st_crop(road$geometry, fst_border_utm32N$geometry)


Ueb_Voeg_utm_crop <- st_crop(Ueb_Voeg_utm$geometry, landuse$geometry )
#Ueb_V_inter <- st_intersection(Ueb_Voeg_utm$geometry, landuse$geometry)

extent <- extent(landuse)
cropped <- Ueb_Voeg_utm %>% st_transform(st_crs(landuse))
st_crs(landuse) == st_crs(cropped)
extent_sf <- st_set_crs(st_as_sf(as(extent, "SpatialPolygons")), 4326)
cropped <- st_intersection(Ueb_Voeg_utm, extent)

# yes richtig 
st_crs(Ueb_Voeg_utm)
S2_2018_crop <- crop(S2_2018_br, extent(road))
S2_2018_masked <- mask(S2_2018_crop, fst_border_utm32N)
S2_2018_masked_cropped<- crop(S2_2018_masked, extent(road))



class(Ueb_Voeg_utm_crop)
structure(Ueb_Voeg_utm_crop)
structure(Ueb_Voeg_utm)
Ueb_Voeg_utm$geometry
head(Ueb_Voeg_utm_crop)
Ueb_Voeg_utm_crop[[1]]

######
library(ggdark)
ggplot() +  ggRGB(img = S2_2018_masked_cropped, 5,4,3, 
                  stretch =  'lin', 
                  ggLayer = T) + 
  geom_sf(data = fst_border_utm32N_crop, alpha = 0.2 , fill= NA, color = "grey", size = 1) + #fill = NA
  geom_sf(alpha = 0.3, data = road) + # dark_theme_gray() 
  geom_sf(alpha = 0.8, data = Ueb_Voeg_utm_crop, fill = "red", color = "red") +
  dark_theme_bw(base_family = "Fira Sans Condensed Light", base_size = 14)+
  theme(plot.title = element_text(family = "Fira Sans Condensed"),
        plot.background = element_rect(fill = "grey10"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank())


# Idee: masking , so drop the cities. it is ugly 


##
ggplot() +    ggR(SRTM_br, stretch = "lin", geom_raster = T, ggLayer = T) +
  geom_sf(alpha = .4, data = For_station_str, color = "brown", aes(size = Buche, geometry = geometry)) +
  #geom_sf(alpha = .3, data = For_station_str, color = "green", aes(size = Kiefer)) +
  geom_sf(alpha = 0.9, data = fst_border_utm32N, fill = NA) 
#theme_void()

###
df <- fortify(For_station_str) 
f1 <- For_station_str
f2<- st_drop_geometry(f1)
# https://github.com/r-spatial/sf/issues/231
f3 <- unlist(st_geometry(f1)) %>%  
  matrix(ncol=2,byrow=TRUE) %>% 
  as_tibble() %>% 
  setNames(c("lon","lat"))
f3 <- do.call(rbind, st_geometry(f1)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))
df <- cbind(f2,f3)
###
# https://www.r-graph-gallery.com/330-bubble-map-with-ggplot2.html
# https://stackoverflow.com/questions/59219334/sf-map-with-bubble-chart-size-included-in-legend

##########################################
#### Schwarzerle Karte ######################
##########################################
ggplot() +    #ggR(S1_2016_VV_br, stretch = "lin", geom_raster = T, ggLayer = T) +
  geom_sf(alpha = 0.9, data = fst_border_utm32N, fill = NA) +
  geom_sf(data=waterWays, color = "blue") +
  geom_point(data = df, aes(x=lon, y =lat, size=Schwarzerl, color =Schwarzerl, alpha =Schwarzerl), shape = 20, stroke = F) + 
  scale_size_continuous(range=c(1,12), trans = "log10") +
  scale_alpha_continuous(trans="log10", range=c(0.1, .9)) +
  scale_color_viridis(option="plasma", trans="log10") +
  theme_minimal() +
  blank() +
  labs(
    title = "Distribution of Schwarzerle in Steigerwald"
  ) + 
  theme(
    text = element_text(color = '#22211d')
  ) +
  north(fst_border_utm32N) #+ 
  #scalebar(fst_border_utm32N, dist = 10, st.size = 2, dist_unit = "km", transform = T, , model = "WGS84")
  #geom_sf(alpha = .4, data = For_station_str, color = "brown", aes(size = Buche, geometry = geometry)) +

  

###############################################
## Buntspecht mit masking / 
##### dplyr 
#################################################

extent(S2_2018_crop)
extent_ras <- extent(607680, 615680, 5527320, 5535320)
extent_sf <- st_set_crs(st_as_sf(as(extent_ras, "SpatialPolygons")), 32632)
cropped <- st_intersection(Ueb_Voeg_utm, extent_sf)

st_crs(Ueb_Voeg_utm)
st_crs(extent_sf)

ggplot() +  ggRGB(img = S2_2018_masked_cropped, 3,2,1, 
                  stretch =  'lin', 
                  ggLayer = T,
                  alpha = .7) + 
  geom_sf(data = fst_border_utm32N_crop, alpha = 0.2 , fill= NA, color = "grey", size = 1) + #fill = NA
  #geom_sf(alpha = 0.3, data = road_mask) + # dark_theme_gray() 
  geom_sf(alpha = 0.7, data = cropped, fill = "red", color = "eef033", 
          aes(size=Buntspecht )) +
  dark_theme_bw(base_family = "Fira Sans Condensed Light", base_size = 8)+
  theme(plot.title = element_text(family = "Fira Sans Condensed"),
        plot.background = element_rect(fill = "grey20"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey40", size = 0.2),
        panel.grid.minor = element_line(color = "grey40", size = 0.2),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank()) 

######### using dplyr 
cropped %>% 
  filter(Buntspecht > 0) %>% 
  ggplot() +  ggRGB(img = S2_2018_masked_cropped, 3,2,1, 
                    stretch =  'lin', 
                    ggLayer = T,
                    alpha = .7) + 
  geom_sf(data = fst_border_utm32N_crop, alpha = 0.2 , fill= NA, color = "grey", size = 1) + #fill = NA
  #geom_sf(alpha = 0.3, data = road_mask) + # dark_theme_gray() 
  geom_sf(alpha = 0.8, color = "#eef033", 
          aes(size=Buntspecht )) +
  dark_theme_bw(base_family = "Fira Sans Condensed Light", base_size = 8)+
  theme(plot.title = element_text(family = "Fira Sans Condensed"),
        plot.background = element_rect(fill = "grey20"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey40", size = 0.2),
        panel.grid.minor = element_line(color = "grey40", size = 0.2),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank()) + 
  labs(
    title = "The number of Buntspecht in Steigerwald"
  )
###################


#########################################################
##### Baumtyp Karte ##############
#############################################
df <- cbind(f2,f3)
df <- df[, -c(1, 21)]
#df$max <- apply(df[, c(1:15)], 1, max)
df$typ <- colnames(df)[apply(df[, c(1:11)], 1, which.max)]

ggplot() +    #ggR(S1_2016_VV_br, stretch = "lin", geom_raster = T, ggLayer = T) +
  geom_sf(alpha = 0.9, data = fst_border_utm32N, fill = NA) +
  geom_point(data = df, aes(x=lon, y =lat, color =typ), shape = 18)+ 
  scale_color_manual(values = c("#50555c", "#e76900", "#c833c9", "yellow", "#9c9894", "cyan",
                                "#9c9894", "green", "red", "60dba6", "8360db")) +
  theme_minimal() + 
  labs( 
    title = "Tree types in Steigerwald"
    ) + 
  north(fst_border_utm32N, symbol = 4)
    
  # scale_size_continuous(range=c(1,12), trans = "log") +
  # scale_alpha_continuous(trans="log", range=c(0.1, .9)) +
  # scale_color_viridis(option="plasma", trans="log") 
#geom_sf(alpha = .4, data = For_station_str, color = "brown", aes(size = Buche, geometry = geometry)) +


