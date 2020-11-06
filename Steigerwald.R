##########################################################
library(RCurl)
df <- read.csv("https://raw.githubusercontent.com/wegmann/R_data/master/Steigerwald_sample_points_all_data_subset_withNames.csv")
df <- df[ , -1]
library(tidyverse)
dfwhat <- read_csv("sample_points_all_subset_withNames.csv")
df <- read_csv("https://raw.githubusercontent.com/wegmann/R_data/master/Steigerwald_sample_points_all_data_subset_withNames.csv")

#write.csv(Your DataFrame,"Path where you'd like to export the DataFrame\\File Name.csv", row.names = FALSE)

df
head(df)
tail(df)
summary(df)
plot(df)
str(df)
names(df)
dim(df)
class(df)
levels(df)

# select one column (using different ways)
# just select the Sentinel data (S2.*)
# select the second last column
# select all columns but just first 10 rows
# just select LUCAS LC and SRTM

df$LUCAS_LC 
df[ ,2]

df[ ,3:12] 

df[ ,length(df)-1]
df[1:10, ]
df[ ,c(2,13)]  # just select LUCAS LC and SRTM
###############################################################################

## tasks to do 
# just plot SRTM values above a predefined NDVI value
plot(df$SRTM, df$TimeScan.NDVIavg) #Timescan
plot(df$SRTM, df$L8.ndvi) #L8
plot(df$SRTM, df$L7.ndvi) #L7
plot(df$SRTM, df$MOD.ndvi) #MOD

######################ggplot 
library(ggplot2)
theme_set(
  theme_minimal() +
    theme(legend.position = "right")
)

ggplot(df, aes(x=SRTM, y=TimeScan.NDVIavg, color=LCname)) +
  scale_color_manual(values=c("#CC79A7","#D55E00", "green", "#52854C", "black", "red", "#0072B2", "#56B4E9")) +
  geom_point() + stat_ellipse()

ggplot(df, aes(x=SRTM, y=L8.ndvi, color=LCname)) +
  scale_color_manual(values=c("#CC79A7","#D55E00", "green", "#52854C", "black", "red", "#0072B2", "#56B4E9")) +
  geom_point() + stat_ellipse()

ggplot(df, aes(x=SRTM, y=L7.ndvi, color=LCname)) +
  scale_color_manual(values=c("#CC79A7","#D55E00", "green", "#52854C", "black", "red", "#0072B2", "#56B4E9")) +
  geom_point() + stat_ellipse()

ggplot(df, aes(x=SRTM, y=MOD.ndvi, color=LCname)) +
  scale_color_manual(values=c("#CC79A7","#D55E00", "green", "#52854C", "black", "red", "#0072B2", "#56B4E9")) +
  geom_point() + stat_ellipse()

range(df$SRTM)

#######################################################################
# just plot NDVI values for SRTM values less than Y and landcover equal to X
unique(df$LCname)
df_t1 <- df[which(df$LCname == "shrubland"),]
plot(df_t1$SRTM, df_t1$TimeScan.NDVIavg) 
plot(df_t1$SRTM, df_t1$L8.ndvi)
plot(df_t1$SRTM, df_t1$L7.ndvi)
df_t1 <- df[which(df$LCname == "urban"),]
df_t1 <- df[which(df$LCname == "cropland"),]
df_t1 <- df[which(df$LCname == "broadleaf_woodland"),] ###
df_t1 <- df[which(df$LCname == "conferious_woodland"),] ###
df_t1 <- df[which(df$LCname == "grassland"),]
df_t1 <- df[which(df$LCname == "water"),] ###
df_t1 <- df[which(df$LCname == "wetland"),] #####
###################################################
df_t1 <- df[which(df$SRTM < 450 & df$LCname == "broadleaf_woodland"),]
ggplot(df_t1, aes(SRTM, L8.ndvi)) + geom_point(color = "#CC79A7") 
df_t1 <- df[which(df$SRTM < 420 & df$LCname == "conferious_woodland"),]
ggplot(df_t1, aes(SRTM, L8.ndvi)) + geom_point(color = "#D55E00") 
df_t1 <- df[which(df$SRTM < 500 & df$LCname == "water"),]
ggplot(df_t1, aes(SRTM, L8.ndvi)) + geom_point(color = "#0072B2") 
df_t1 <- df[which(df$SRTM < 500 & df$LCname == "wetland"),]
ggplot(df_t1, aes(SRTM, L8.ndvi)) + geom_point(color = "#56B4E9") 
plot(df_t1$SRTM, df_t1$L8.ndvi) 
###################################################
df %>% 
  filter(SRTM < 450 & LCname == "broadleaf_woodland") %>%
  ggplot(aes(SRTM, L8.ndvi)) + geom_point(color = "#CC79A7") 

df %>% 
  filter(SRTM < 420 & LCname == "conferious_woodland") %>%
  ggplot(aes(SRTM, L8.ndvi)) + geom_point(color = "#D55E00") 

df %>% 
  filter(SRTM < 400 & LCname == "water") %>%
  ggplot(aes(SRTM, L8.ndvi)) + geom_point(color = "#0072B2") 

df %>% 
  filter(SRTM < 500 & LCname == "wetland") %>%
  ggplot(aes(SRTM, L8.ndvi)) + geom_point(color = "#56B4E9") 

###################################################
# create a new data frame with all entries but only corresponding NDVI values above 0.5
# 1. possibility
df_L7ndvi_above_05 <- df %>% 
  filter(L7.ndvi > 0.5) 

df_L8ndvi_above_05 <-  df %>% 
  filter(L8.ndvi > 0.5) 

df_tsndvi_above_05 <- df %>% 
  filter(TimeScan.NDVIavg > 0.5) 

df_MOD_above_05 <-  df %>% 
  filter(MOD.ndvi > 0.5) 

# 2. possibility
df_L7ndvi_above_05 <- df[which(df$L7.ndvi > 0.5), ]
df_L8ndvi_above_05 <- df[which(df$L8.ndvi > 0.5), ]
df_tsndvi_above_05 <- df[which(df$TimeScan.NDVIavg > 0.5), ]
df_MOD_above_05 <- df[which(df$MOD.ndvi > 0.5), ]

#3. possiblity 
df_L7ndvi_above_05 <- subset(df, L7.ndvi > 0.5) 
df_L8ndvi_above_05 <- subset(df, L8.ndvi > 0.5)
df_tsndvi_above_05 <- subset(df, TimeScan.NDVIavg > 0.5)
df_MOD_above_05 <- subset(df, MOD.ndvi > 0.5)

#########################################################
# select data where LC values below x or above y
# 1. possibility
df_Lucas_3_6 <- df %>% 
  filter(LUCAS_LC < 6 & LUCAS_LC > 3) 

# 2. possibility
df_Lucas_3_6 <- df[which(df$LUCAS_LC < 6 & df$LUCAS_LC > 3), ]

#3. possiblity 
df_Lucas_3_6 <- subset(df, LUCAS_LC < 6 & LUCAS_LC > 3) 
#############################################################
# just select LUCAS LC and SRTM where NDVI larger equal than x
# 1. possibility
df2 <- df %>% 
  filter(L7.ndvi == 0.3 | L7.ndvi > 0.3) %>% 
  select(LUCAS_LC, SRTM) 

# 2. possibility
df2 <- subset(df, L7.ndvi > 0.3 | L7.ndvi == 0.3 )
df2 <- df2[ ,c(2,13)]

#3. possiblity 
df2 <- df[df$L7.ndvi == 0.3 | df$L7.ndvi > 0.3, ]
df2 <- df2[ ,c(2,13)]


################################################

remotes::install_github("mkearney/mizzourahmd")
