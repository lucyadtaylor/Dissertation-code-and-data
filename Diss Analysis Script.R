# clear the memory space in rstudio for efficiency
rm(list = ls())
gc()
#load packages
library(readr)
library(raster)
library(dismo)
library(tmap)
library(rgdal)
library(sf)
library(rJava)
library(pscl)
library(RColorBrewer)
library(spatstat)
library(maptools)

#DATA CLEANING
#read in California shapefile
cal <- read_sf("f067d2f7-7950-4e16-beba-8972d811599c2020329-1-18infjv.25og.shp")

#load in environmental data and SVI
#precipitation
#first import all files in a single folder as a list 
f <- list.files(path="/Users/lucytaylor/Library/Mobile Documents/com~apple~CloudDocs/Masters/Dissertation/dissertation/wc2.1_2.5m_prec_2010-2018", pattern='tif$', full.names=TRUE)
prec <- lapply(f, raster)
prec_stack <- stack(f)
prec_stack

#apply just to the extent of california
r <- crop(prec_stack, extent(cal))
cal_prec <- mask(r, cal)

#plot the first layer
plot(cal_prec[[1]])

#NDVI
n <- list.files(path="/Users/lucytaylor/Library/Mobile Documents/com~apple~CloudDocs/Masters/Dissertation/dissertation/diss/NDVI-2", pattern='tif$', full.names=TRUE)
NDVI <- lapply(n, raster)
NDVI_stack <- stack(n)

#crop to the extent of California
r4 <- crop(NDVI_stack, extent(cal))
cal_NDVI <- mask(r4, cal)

plot(cal_NDVI[[1]],
     main="NDVI")

#Evapotranspiration
e <- list.files(path="/Users/lucytaylor/Library/Mobile Documents/com~apple~CloudDocs/Masters/Dissertation/dissertation/diss/Evapo-transpiration", pattern='tif$', full.names=TRUE)
evap <- lapply(e, raster)
evap_stack <- stack(e)

#crop to the extent of California
r5 <- crop(evap_stack, extent(cal))
cal_evap <- mask(r5, cal)

#plot evapotranspiration
plot(cal_evap[[1]],
     main="Evapotranspiration")

#MaxTemp
t <- list.files(path="/Users/lucytaylor/Library/Mobile Documents/com~apple~CloudDocs/Masters/Dissertation/dissertation/diss/wc2.1_2.5m_tmax_2010-2018", pattern='tif$', full.names=TRUE)
maxtemp <- lapply(t, raster)
maxtemp_stack <- stack(t)

#crop to the extent of California
r3 <- crop(maxtemp_stack, extent(cal))
cal_maxtemp <- mask(r3, cal)

#plot max temp
plot(cal_maxtemp [[1]],
     main="maximum temperature")

#elevation 
elevation <- raster("wc2.1_10m_elev.tif")

r2 <- crop(elevation, extent(cal))
cal_elevation <- mask(r2, cal)

#plot elevation
plot(cal_elevation)

#SVI 
S <- list.files(path="/Users/lucytaylor/Library/Mobile Documents/com~apple~CloudDocs/Masters/Dissertation/dissertation/diss/SVI", pattern='tif$', full.names=TRUE)
SVI <- lapply(S, raster)
SVI_stack <- stack(SVI)

#crop to the extent of California
r7 <- crop(SVI_stack, extent(cal))
cal_SVI <- mask(r7, cal)

#plot SVI
plot(cal_SVI)

#2016 WI[D,J,F] SP[M,A,M] SU[J,J,A] AU[S,O,N] - 4 rasters
#2017 WI[D,J,F] SP[M,A,M] SU[J,J,A] AU[S,O,N] - 4 rasters
#2018 WI[D,J,F] SP[M,A,M] SU[J,J,A] AU[S,O,N] - 4 rasters

#derive seasonal raster to account for seasonality
#mean values by season temperature
mxtem_wi_15_16 <- calc(stack(cal_maxtemp[[1]], cal_maxtemp[[2]]), fun = mean, na.rm = T)
mxtem_sp_16 <- calc(stack(cal_maxtemp[[3]], cal_maxtemp[[4]], cal_maxtemp[[5]]), fun = mean, na.rm = T)
mxtem_su_16 <- calc(stack(cal_maxtemp[[6]], cal_maxtemp[[7]], cal_maxtemp[[8]]), fun = mean, na.rm = T)
mxtem_au_16 <- calc(stack(cal_maxtemp[[9]], cal_maxtemp[[10]], cal_maxtemp[[11]]), fun = mean, na.rm = T)
mxtem_wi_16_17 <- calc(stack(cal_maxtemp[[12]], cal_maxtemp[[13]], cal_maxtemp[[14]]), fun = mean, na.rm = T)
mxtem_sp_17 <- calc(stack(cal_maxtemp[[15]], cal_maxtemp[[16]], cal_maxtemp[[17]]), fun = mean, na.rm = T)
mxtem_su_17 <- calc(stack(cal_maxtemp[[18]], cal_maxtemp[[19]], cal_maxtemp[[20]]), fun = mean, na.rm = T)
mxtem_au_17 <- calc(stack(cal_maxtemp[[21]], cal_maxtemp[[22]], cal_maxtemp[[23]]), fun = mean, na.rm = T)
mxtem_wi_17_18 <- calc(stack(cal_maxtemp[[24]], cal_maxtemp[[25]], cal_maxtemp[[26]]), fun = mean, na.rm = T)
mxtem_sp_18 <- calc(stack(cal_maxtemp[[27]], cal_maxtemp[[28]], cal_maxtemp[[29]]), fun = mean, na.rm = T)
mxtem_su_18 <- calc(stack(cal_maxtemp[[30]], cal_maxtemp[[31]], cal_maxtemp[[32]]), fun = mean, na.rm = T)
mxtem_au_18 <- calc(stack(cal_maxtemp[[33]], cal_maxtemp[[34]], cal_maxtemp[[35]]), fun = mean, na.rm = T)

#mean values by season precipitation
prec_wi_15_16 <- calc(stack(cal_prec[[1]], cal_prec[[2]]), fun = mean, na.rm = T)
prec_sp_16 <- calc(stack(cal_prec[[3]], cal_prec[[4]], cal_prec[[5]]), fun = mean, na.rm = T)
prec_su_16 <- calc(stack(cal_prec[[6]], cal_prec[[7]], cal_prec[[8]]), fun = mean, na.rm = T)
prec_au_16 <- calc(stack(cal_prec[[9]], cal_prec[[10]], cal_prec[[11]]), fun = mean, na.rm = T)
prec_wi_16_17 <- calc(stack(cal_prec[[12]], cal_prec[[13]], cal_prec[[14]]), fun = mean, na.rm = T)
prec_sp_17 <- calc(stack(cal_prec[[15]], cal_prec[[16]], cal_prec[[17]]), fun = mean, na.rm = T)
prec_su_17 <- calc(stack(cal_prec[[18]], cal_prec[[19]], cal_prec[[20]]), fun = mean, na.rm = T)
prec_au_17 <- calc(stack(cal_prec[[21]], cal_prec[[22]], cal_prec[[23]]), fun = mean, na.rm = T)
prec_wi_17_18 <- calc(stack(cal_prec[[24]], cal_prec[[25]], cal_prec[[26]]), fun = mean, na.rm = T)
prec_sp_18 <- calc(stack(cal_prec[[27]], cal_prec[[28]], cal_prec[[29]]), fun = mean, na.rm = T)
prec_su_18 <- calc(stack(cal_prec[[30]], cal_prec[[31]], cal_prec[[32]]), fun = mean, na.rm = T)
prec_au_18 <- calc(stack(cal_prec[[33]], cal_prec[[34]], cal_prec[[35]]), fun = mean, na.rm = T)

#mean values by season evapotranspiration
evap_wi_15_16 <- calc(stack(cal_evap[[1]], cal_evap[[2]]), fun = mean, na.rm = T)
evap_sp_16 <- calc(stack(cal_evap[[3]], cal_evap[[4]], cal_evap[[5]]), fun = mean, na.rm = T)
evap_su_16 <- calc(stack(cal_evap[[6]], cal_evap[[7]], cal_evap[[8]]), fun = mean, na.rm = T)
evap_au_16 <- calc(stack(cal_evap[[9]], cal_evap[[10]], cal_evap[[11]]), fun = mean, na.rm = T)
evap_wi_16_17 <- calc(stack(cal_evap[[12]], cal_evap[[13]], cal_evap[[14]]), fun = mean, na.rm = T)
evap_sp_17 <- calc(stack(cal_evap[[15]], cal_evap[[16]], cal_evap[[17]]), fun = mean, na.rm = T)
evap_su_17 <- calc(stack(cal_evap[[18]], cal_evap[[19]], cal_evap[[20]]), fun = mean, na.rm = T)
evap_au_17 <- calc(stack(cal_evap[[21]], cal_evap[[22]], cal_evap[[23]]), fun = mean, na.rm = T)
evap_wi_17_18 <- calc(stack(cal_evap[[24]], cal_evap[[25]], cal_evap[[26]]), fun = mean, na.rm = T)
evap_sp_18 <- calc(stack(cal_evap[[27]], cal_evap[[28]], cal_evap[[29]]), fun = mean, na.rm = T)
evap_su_18 <- calc(stack(cal_evap[[30]], cal_evap[[31]], cal_evap[[32]]), fun = mean, na.rm = T)
evap_au_18 <- calc(stack(cal_evap[[33]], cal_evap[[34]], cal_evap[[35]]), fun = mean, na.rm = T)

#rename ndvi 
ndvi_wi_15_16 <- cal_NDVI[[1]]
ndvi_sp_16 <- cal_NDVI[[2]]
ndvi_su_16 <- cal_NDVI[[3]]
ndvi_au_16 <- cal_NDVI[[4]]
ndvi_wi_16_17 <- cal_NDVI[[5]]
ndvi_sp_17 <- cal_NDVI[[6]]
ndvi_su_17 <- cal_NDVI[[7]]
ndvi_au_17 <- cal_NDVI[[8]]
ndvi_wi_17_18 <- cal_NDVI[[9]]
ndvi_sp_18 <- cal_NDVI[[10]]
ndvi_su_18 <- cal_NDVI[[11]]
ndvi_au_18 <- cal_NDVI[[12]]

#split SVI values 
#use 2016 for 2017 as well as after visualising, not much difference over a year 
svi_16_17 <- cal_SVI[[1]]
svi_18 <- cal_SVI[[2]]

cal_county <- read_sf("CA_counties.shp")
#plot SVI
tm_shape(svi_18)+
  tm_raster(title = "SVI", palette = '-RdYlGn', style ='cont')+
  tm_layout(main.title = "2018", main.title.position = c(0.2, 0.7), title.size=4, legend.text.size = 1.05, 
            legend.position = c(0.65, 0.55), legend.height= -0.3, legend.title.size = 1.5, frame='white')+
  tm_scale_bar(position=c(0.02, 0.02), text.size = 1, breaks = c(0, 100, 200, 300))+
  tm_compass(north = 0,type = 'arrow', position = c('right', 'top'), text.size = 0.9)+
  tm_shape(cal_county)+tm_polygons(alpha = 0, border.col = "black")

#standardize all raster to dimension of approx. 5000m

#use max temp as the template 
RasterTemplate <- raster(nrow=dim(mxtem_wi_15_16[[1]])[1], ncol=dim(mxtem_wi_15_16[[1]])[2], crs=crs(mxtem_wi_15_16[[1]]), extent(mxtem_wi_15_16[[1]]))

#max temp
mxtem_wi_15_16_r <- resample(mxtem_wi_15_16, RasterTemplate, method = "bilinear")
mxtem_sp_16_r <- resample(mxtem_sp_16, RasterTemplate, method = "bilinear")
mxtem_su_16_r <- resample(mxtem_su_16, RasterTemplate, method = "bilinear")
mxtem_au_16_r <- resample(mxtem_au_16, RasterTemplate, method = "bilinear")
mxtem_wi_16_17_r <- resample(mxtem_wi_16_17, RasterTemplate, method = "bilinear")
mxtem_sp_17_r <- resample(mxtem_sp_17, RasterTemplate, method = "bilinear")
mxtem_su_17_r <- resample(mxtem_su_17, RasterTemplate, method = "bilinear")
mxtem_au_17_r <- resample(mxtem_au_17, RasterTemplate, method = "bilinear")
mxtem_wi_17_18_r <- resample(mxtem_wi_17_18, RasterTemplate, method = "bilinear")
mxtem_sp_18_r <- resample(mxtem_sp_18, RasterTemplate, method = "bilinear")
mxtem_su_18_r <- resample(mxtem_su_18, RasterTemplate, method = "bilinear")
mxtem_au_18_r <- resample(mxtem_au_18, RasterTemplate, method = "bilinear")

#prec
prec_wi_15_16_r <- resample(prec_wi_15_16, RasterTemplate, method = "bilinear")
prec_sp_16_r <- resample(prec_sp_16, RasterTemplate, method = "bilinear")
prec_su_16_r <- resample(prec_su_16, RasterTemplate, method = "bilinear")
prec_au_16_r <- resample(prec_au_16, RasterTemplate, method = "bilinear")
prec_wi_16_17_r <- resample(prec_wi_16_17, RasterTemplate, method = "bilinear")
prec_sp_17_r <- resample(prec_sp_17, RasterTemplate, method = "bilinear")
prec_su_17_r <- resample(prec_su_17, RasterTemplate, method = "bilinear")
prec_au_17_r <- resample(prec_au_17, RasterTemplate, method = "bilinear")
prec_wi_17_18_r <- resample(prec_wi_17_18, RasterTemplate, method = "bilinear")
prec_sp_18_r <- resample(prec_sp_18, RasterTemplate, method = "bilinear")
prec_su_18_r <- resample(prec_su_18, RasterTemplate, method = "bilinear")
prec_au_18_r <- resample(prec_au_18, RasterTemplate, method = "bilinear")

#evap
evap_wi_15_16_r <- resample(evap_wi_15_16, RasterTemplate, method = "bilinear")
evap_sp_16_r <- resample(evap_sp_16, RasterTemplate, method = "bilinear")
evap_su_16_r <- resample(evap_su_16, RasterTemplate, method = "bilinear")
evap_au_16_r <- resample(evap_au_16, RasterTemplate, method = "bilinear")
evap_wi_16_17_r <- resample(evap_wi_16_17, RasterTemplate, method = "bilinear")
evap_sp_17_r <- resample(evap_sp_17, RasterTemplate, method = "bilinear")
evap_su_17_r <- resample(evap_su_17, RasterTemplate, method = "bilinear")
evap_au_17_r <- resample(evap_au_17, RasterTemplate, method = "bilinear")
evap_wi_17_18_r <- resample(evap_wi_17_18, RasterTemplate, method = "bilinear")
evap_sp_18_r <- resample(evap_sp_18, RasterTemplate, method = "bilinear")
evap_su_18_r <- resample(evap_su_18, RasterTemplate, method = "bilinear")
evap_au_18_r <- resample(evap_au_18, RasterTemplate, method = "bilinear")

#ndvi
ndvi_wi_15_16_r <- resample(ndvi_wi_15_16, RasterTemplate, method = "bilinear")
ndvi_sp_16_r <- resample(ndvi_sp_16, RasterTemplate, method = "bilinear")
ndvi_su_16_r <- resample(ndvi_su_16, RasterTemplate, method = "bilinear")
ndvi_au_16_r <- resample(ndvi_au_16, RasterTemplate, method = "bilinear")
ndvi_wi_16_17_r <- resample(ndvi_wi_16_17, RasterTemplate, method = "bilinear")
ndvi_sp_17_r <- resample(ndvi_sp_17, RasterTemplate, method = "bilinear")
ndvi_su_17_r <- resample(ndvi_su_17, RasterTemplate, method = "bilinear")
ndvi_au_17_r <- resample(ndvi_au_17, RasterTemplate, method = "bilinear")
ndvi_wi_17_18_r <- resample(ndvi_wi_17_18, RasterTemplate, method = "bilinear")
ndvi_sp_18_r <- resample(ndvi_sp_18, RasterTemplate, method = "bilinear")
ndvi_su_18_r <- resample(ndvi_su_18, RasterTemplate, method = "bilinear")
ndvi_au_18_r <- resample(ndvi_au_18, RasterTemplate, method = "bilinear")

#elev
elev_all_r <- resample(cal_elevation, RasterTemplate, method ="bilinear")

# svi
svi_16_17_r <- resample(svi_16_17, RasterTemplate, method ="bilinear")
svi_18_r <- resample(svi_18, RasterTemplate, method ="bilinear")

#stack all raster in order according to season type
cal_win_15_16 <- stack(mxtem_wi_15_16_r, prec_wi_15_16_r, evap_wi_15_16_r, ndvi_wi_15_16_r, elev_all_r, svi_16_17_r)
cal_sp_16 <- stack(mxtem_sp_16_r, prec_sp_16_r, evap_sp_16_r, ndvi_sp_16_r, elev_all_r, svi_16_17_r)
cal_su_16 <- stack(mxtem_su_16_r, prec_su_16_r, evap_su_16_r, ndvi_su_16_r, elev_all_r, svi_16_17_r)
cal_au_16 <- stack(mxtem_au_16_r, prec_au_16_r, evap_au_16_r, ndvi_au_16_r, elev_all_r, svi_16_17_r)
cal_win_16_17 <- stack(mxtem_wi_16_17_r, prec_wi_16_17_r, evap_wi_16_17_r, ndvi_wi_16_17_r, elev_all_r, svi_16_17_r)
cal_sp_17 <- stack(mxtem_sp_17_r, prec_sp_17_r, evap_sp_17_r, ndvi_sp_17_r, elev_all_r, svi_16_17_r)
cal_su_17 <- stack(mxtem_su_17_r, prec_su_17_r, evap_su_17_r, ndvi_su_17_r, elev_all_r, svi_16_17_r)
cal_au_17 <- stack(mxtem_au_17_r, prec_au_17_r, evap_au_17_r, ndvi_au_17_r, elev_all_r, svi_16_17_r)
cal_win_17_18 <- stack(mxtem_wi_17_18_r, prec_wi_17_18_r, evap_wi_17_18_r, ndvi_wi_17_18_r, elev_all_r, svi_18_r)
cal_sp_18 <- stack(mxtem_sp_18_r, prec_sp_18_r, evap_sp_18_r, ndvi_sp_18_r, elev_all_r, svi_18_r)
cal_su_18 <- stack(mxtem_su_18_r, prec_su_18_r, evap_su_18_r, ndvi_su_18_r, elev_all_r, svi_18_r)
cal_au_18 <- stack(mxtem_au_18_r, prec_au_18_r, evap_au_18_r, ndvi_au_18_r, elev_all_r, svi_18_r)


#FIRE DATA CLEANING 
#load fire point datasets
fire_2016 <- read.csv("cal_firepoints_2016.csv", header = TRUE, sep =",")
fire_2017 <- read.csv("cal_firepoints_2017.csv", header = TRUE, sep =",")
fire_2018 <- read.csv("cal_firepoints_2018.csv", header = TRUE, sep =",")

#check confidence 
table(fire_2016$confidence)
table(fire_2017$confidence)
table(fire_2018$confidence)

#check type
table(fire_2016$type)
table(fire_2017$type)
table(fire_2018$type)

#reduce the dataset to 'h' and 'n' confidence values and type '0' and '2' 
fire_2016 <- fire_2016[(fire_2016$confidence == "h"|fire_2016$confidence == "n") & (fire_2016$type == 0|fire_2016$type == 2),]

#reduce the dataset to 'h' and 'n' confidence values and type '0' and '2' 
fire_2017 <- fire_2017[(fire_2017$confidence == "h"|fire_2017$confidence == "n") & (fire_2017$type == 0|fire_2017$type == 2),]

#reduce the dataset to 'h' and 'n' confidence values and type '0' and '2' 
fire_2018 <- fire_2018[(fire_2018$confidence == "h"|fire_2018$confidence == "n") & (fire_2018$type == 0|fire_2018$type == 2),]

#TRY TO DO KERNEL DENSITY
#data for KD

#loading in hotspot datasets 
tw16 <- read_csv('viirs-snpp_2016_United_States.csv')
tw17 <- read_csv('viirs-snpp_2017_United_States.csv')
tw18 <- read_csv('viirs-snpp_2018_United_States.csv')

#read california shapefile
cal <- read_sf("f067d2f7-7950-4e16-beba-8972d811599c2020329-1-18infjv.25og.shp")

#inspect california shapefile
tm_shape(cal)+
  tm_polygons()

st_crs(cal)

#making each a spatial dataframe 
# Convert data frame to sf object
#2016
tw16_s <- st_as_sf(x = tw16, 
                   coords = c("longitude", "latitude"),
                   crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
#select rows where type is 0 & 2 and confidence is high
tw16_s <- tw16_s %>% dplyr::filter(type == 0&2) %>% dplyr::filter(confidence == 'h')

#2016
tw16_sCAL <- tw16_s[cal,]

#2017
tw17_s <- st_as_sf(x = tw17, 
                   coords = c("longitude", "latitude"),
                   crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#select rows where type is 0 & 2 and confidence is high
tw17_s <- tw17_s %>% dplyr::filter(type == 0&2) %>% dplyr::filter(confidence == 'h')

#2017
tw17_sCAL <- tw17_s[cal,]

#2018
tw18_s <- st_as_sf(x = tw18, 
                   coords = c("longitude", "latitude"),
                   crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#select rows where type is 0 & 2 and confidence is high
tw18_s <- tw18_s %>% dplyr::filter(type == 0&2) %>% dplyr::filter(confidence == 'h')

#2018
tw18_sCAL <- tw18_s[cal,]

## create a window for spatstat to create density
california <- st_read("cb_2018_us_state_5m.shp")
california <- california %>%
  dplyr::filter(., NAME=="California")

state_sf <- st_as_sf(california)
state_flat <- st_transform(state_sf, crs = 2163)
plot(california)

state_owin <- as.owin(as_Spatial(state_flat))
plot(state_owin)

tw16_fire <- st_transform(tw16_sCAL, crs = 2163)
tw16_fire <- st_as_sf(tw16_fire)

tw16_fire_co <- tw16_fire %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2])

X <- tw16_fire_co[["lon"]]
Y <- tw16_fire_co[["lat"]]

tw16_fire.ppp <- ppp(x=X,
                     y=Y,
                     window=state_owin)

#palette for plot
colfunc <- colorRampPalette(c("forest green", "yellow", "orange", "red"))

tw16_fire.ppp %>%
  density(., sigma=20000) %>%
  plot(col=colfunc, main="2016")

#2017
tw17_fire <- st_transform(tw17_sCAL, crs = 2163)
tw17_fire <- st_as_sf(tw17_fire)

tw17_fire_co <- tw17_fire %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2])

X <- tw17_fire_co[["lon"]]
Y <- tw17_fire_co[["lat"]]

tw17_fire.ppp <- ppp(x=X,
                     y=Y,
                     window=state_owin)

tw17_fire.ppp %>%
  density(., sigma=20000) %>%
  plot(col=colfunc, main="2017")

tw18_fire <- st_transform(tw18_sCAL, crs = 2163)
tw18_fire <- st_as_sf(tw18_fire)

tw18_fire_co <- tw18_fire %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2])

X <- tw18_fire_co[["lon"]]
Y <- tw18_fire_co[["lat"]]

tw18_fire.ppp <- ppp(x=X,
                     y=Y,
                     window=state_owin)


tw18_fire.ppp %>%
  density(., sigma=20000) %>%
  plot(col=colfunc, main="2018")

#append files into one file
fire_data <- rbind(fire_2016, fire_2017, fire_2018)

#change date from yyyy/mm/dd to dd/mm/yyyy for easy separation
fire_data$acq_date <- strftime(fire_data$acq_date, "%d/%m/%Y")
fire_data$yyyy <- substr(fire_data$acq_date, 7, 10)
fire_data$mm <- substr(fire_data$acq_date, 4, 5)
fire_data$handle <- paste(fire_data$mm, "-", fire_data$yyyy, sep="")

# N = 194498

table(fire_data$yyyy)   
# 2016  2017  2018 
# 35820 75065 83613

#separate into seasons 
# 2016
fire_wi_15_16 <- fire_data[fire_data$handle == "01-2016"| fire_data$handle == "02-2016",]
fire_sp_16 <- fire_data[fire_data$handle == "03-2016"| fire_data$handle == "04-2016"| fire_data$handle == "05-2016",]
fire_su_16 <- fire_data[fire_data$handle == "06-2016"| fire_data$handle == "07-2016"| fire_data$handle == "08-2016",]
fire_au_16 <- fire_data[fire_data$handle == "09-2016"| fire_data$handle == "10-2016"| fire_data$handle == "11-2016",]

#calculate density value 
fire_15_16_win_den <- (1462/35820) * 1000
fire_16_sp_den <- (2954/35820) * 1000
fire_16_su_den <- (19227/35820) * 1000
fire_16_au_den <- (11407/35820) * 1000


# 2017
fire_wi_16_17 <- fire_data[fire_data$handle == "12-2016" | fire_data$handle == "01-2017"| fire_data$handle == "02-2017",]
fire_sp_17 <- fire_data[fire_data$handle == "03-2017"| fire_data$handle == "04-2017"| fire_data$handle == "05-2017",]
fire_su_17 <- fire_data[fire_data$handle == "06-2017"| fire_data$handle == "07-2017"| fire_data$handle == "08-2017",]
fire_au_17 <- fire_data[fire_data$handle == "09-2017"| fire_data$handle == "10-2017"| fire_data$handle == "11-2017",]

#calculate density value 
fire_16_17_win_den <- (1667/35820) * 1000
fire_17_sp_den <- (3826/35820) * 1000
fire_17_su_den <- (27998/35820) * 1000
fire_17_au_den <- (31473/35820) * 1000


# 2018
fire_wi_17_18 <- fire_data[fire_data$handle == "12-2017" | fire_data$handle == "01-2018"| fire_data$handle == "02-2018",]
fire_sp_18 <- fire_data[fire_data$handle == "03-2018"| fire_data$handle == "04-2018"| fire_data$handle == "05-2018",]
fire_su_18 <- fire_data[fire_data$handle == "06-2018"| fire_data$handle == "07-2018"| fire_data$handle == "08-2018",]
fire_au_18 <- fire_data[fire_data$handle == "09-2018"| fire_data$handle == "10-2018"| fire_data$handle == "11-2018",]

#calculate density value 
fire_17_18_win_den <- (14186/35820) * 1000
fire_18_sp_den <- (3176/35820) * 1000
fire_18_su_den <- (56570/35820) * 1000
fire_18_au_den <- (19181/35820) * 1000

#extract environmental values on to points for each fire season file
# 2016
# win_15_16
# reduce columns 
fire_wi_15_16 <- fire_wi_15_16[,c(1,2,10,14,18)]
# rename 5th column 
colnames(fire_wi_15_16)[5] <- "datemmyyyy"
# declare data to be spatial dataset
coordinates(fire_wi_15_16)= ~ longitude+latitude
# rename band names the raster stack
names(cal_win_15_16) <- c("maxtemp", "precip", "dryness", "ndvi", "elevation", "svi")
#update the file by extracting the raster values
cal_fire_wi_15_16 <- data.frame(fire_wi_15_16, raster::extract(cal_win_15_16, fire_wi_15_16))
#drop optional column
cal_fire_wi_15_16 <- cal_fire_wi_15_16[,-6]

# repeat for other files

# sp_16
View(fire_sp_16)
# reduce columns 
fire_sp_16 <- fire_sp_16[,c(1,2,10,14,18)]
# rename 5th column 
colnames(fire_sp_16)[5] <- "datemmyyyy"
# declare data to be spatial dataset
coordinates(fire_sp_16)= ~ longitude+latitude
# rename band names the raster stack
names(cal_sp_16) <- c("maxtemp", "precip", "dryness", "ndvi", "elevation", "svi")
# update the file by extracting the raster values
cal_fire_sp_16 <- data.frame(fire_sp_16, raster::extract(cal_sp_16, fire_sp_16))
# drop optional column
cal_fire_sp_16 <- cal_fire_sp_16[,-6]

# su_16
View(fire_su_16)
# reduce columns 
fire_su_16 <- fire_su_16[,c(1,2,10,14,18)]
# rename 5th column 
colnames(fire_su_16)[5] <- "datemmyyyy"
# declare data to be spatial dataset
coordinates(fire_su_16)= ~ longitude+latitude 
# rename band names the raster stack
names(cal_su_16) <- c("maxtemp", "precip", "dryness", "ndvi", "elevation", "svi")
# update the file by extracting the raster values
cal_fire_su_16 <- data.frame(fire_su_16, raster::extract(cal_su_16, fire_su_16))
# drop optional column
cal_fire_su_16 <- cal_fire_su_16[,-6]

# au_16
View(fire_au_16)
# reduce columns 
fire_au_16 <- fire_au_16[,c(1,2,10,14,18)]
# rename 5th column 
colnames(fire_au_16)[5] <- "datemmyyyy"
# declare data to be spatial dataset
coordinates(fire_au_16)= ~ longitude+latitude 
# rename band names the raster stack
names(cal_au_16) <- c("maxtemp", "precip", "dryness", "ndvi", "elevation", "svi")
# update the file by extracting the raster values
cal_fire_au_16 <- data.frame(fire_au_16, raster::extract(cal_au_16, fire_au_16))
# drop optional column
cal_fire_au_16 <- cal_fire_au_16[,-6]

#2017
# win_16_17
# reduce columns 
fire_wi_16_17 <- fire_wi_16_17[,c(1,2,10,14,18)]
# rename 5th column 
colnames(fire_wi_16_17)[5] <- "datemmyyyy"
# declare data to be spatial dataset
coordinates(fire_wi_16_17)= ~ longitude+latitude
# rename band names the raster stack
names(cal_win_16_17) <- c("maxtemp", "precip", "dryness", "ndvi", "elevation", "svi")
# update the file by extracting the raster values
cal_fire_wi_16_17 <- data.frame(fire_wi_16_17, raster::extract(cal_win_16_17, fire_wi_16_17))
# drop optional column
cal_fire_wi_16_17 <- cal_fire_wi_16_17[,-6]

# sp_17
View(fire_sp_17)
# reduce columns 
fire_sp_17 <- fire_sp_17[,c(1,2,10,14,18)]
# rename 5th column 
colnames(fire_sp_17)[5] <- "datemmyyyy"
# declare data to be spatial dataset
coordinates(fire_sp_17)= ~ longitude+latitude 
# rename band names the raster stack
names(cal_sp_17) <- c("maxtemp", "precip", "dryness", "ndvi", "elevation", "svi")
# update the file by extracting the raster values
cal_fire_sp_17 <- data.frame(fire_sp_17, raster::extract(cal_sp_17, fire_sp_17))
# drop optional column
cal_fire_sp_17 <- cal_fire_sp_17[,-6]

# su_17
View(fire_su_17)
# reduce columns 
fire_su_17 <- fire_su_17[,c(1,2,10,14,18)]
# rename 5th column 
colnames(fire_su_17)[5] <- "datemmyyyy"
# declare data to be spatial dataset
coordinates(fire_su_17)= ~ longitude+latitude 
# rename band names the raster stack
names(cal_su_17) <- c("maxtemp", "precip", "dryness", "ndvi", "elevation", "svi")
# update the file by extracting the raster values
cal_fire_su_17 <- data.frame(fire_su_17, raster::extract(cal_su_17, fire_su_17))
# drop optional column
cal_fire_su_17 <- cal_fire_su_17[,-6]

# au_17
View(fire_au_17)
# reduce columns 
fire_au_17 <- fire_au_17[,c(1,2,10,14,18)]
# rename 5th column 
colnames(fire_au_17)[5] <- "datemmyyyy"
# declare data to be spatial dataset
coordinates(fire_au_17)= ~ longitude+latitude
# rename band names the raster stack
names(cal_au_17) <- c("maxtemp", "precip", "dryness", "ndvi", "elevation", "svi")
# update the file by extracting the raster values
cal_fire_au_17 <- data.frame(fire_au_17, raster::extract(cal_au_17, fire_au_17))
# drop optional column
cal_fire_au_17 <- cal_fire_au_17[,-6]

#2018
# win_17_18
# reduce columns 
fire_wi_17_18 <- fire_wi_17_18[,c(1,2,10,14,18)]
# rename 5th column 
colnames(fire_wi_17_18)[5] <- "datemmyyyy"
# declare data to be spatial dataset
coordinates(fire_wi_17_18)= ~ longitude+latitude 
# rename band names the raster stack
names(cal_win_17_18) <- c("maxtemp", "precip", "dryness", "ndvi", "elevation", "svi")
# update the file by extracting the raster values
cal_fire_wi_17_18 <- data.frame(fire_wi_17_18, raster::extract(cal_win_17_18, fire_wi_17_18))
# drop optional column
cal_fire_wi_17_18 <- cal_fire_wi_17_18[,-6]

# sp_18
View(fire_sp_18)
# reduce columns 
fire_sp_18 <- fire_sp_18[,c(1,2,10,14,18)]
# rename 5th column 
colnames(fire_sp_18)[5] <- "datemmyyyy"
# declare data to be spatial dataset
coordinates(fire_sp_18)= ~ longitude+latitude 
# rename band names the raster stack
names(cal_sp_18) <- c("maxtemp", "precip", "dryness", "ndvi", "elevation", "svi")
# update the file by extracting the raster values
cal_fire_sp_18 <- data.frame(fire_sp_18, raster::extract(cal_sp_18, fire_sp_18))
# drop optional column
cal_fire_sp_18 <- cal_fire_sp_18[,-6]

# su_18
View(fire_su_18)
# reduce columns 
fire_su_18 <- fire_su_18[,c(1,2,10,14,18)]
# rename 5th column 
colnames(fire_su_18)[5] <- "datemmyyyy"
# declare data to be spatial dataset
coordinates(fire_su_18)= ~ longitude+latitude 
# rename band names the raster stack
names(cal_su_18) <- c("maxtemp", "precip", "dryness", "ndvi", "elevation", "svi")
# update the file by extracting the raster values
cal_fire_su_18 <- data.frame(fire_su_18, raster::extract(cal_su_18, fire_su_18))
# drop optional column
cal_fire_su_18 <- cal_fire_su_18[,-6]

# au_18
View(fire_au_18)
# reduce columns 
fire_au_18 <- fire_au_18[,c(1,2,10,14,18)]
# rename 5th column 
colnames(fire_au_18)[5] <- "datemmyyyy"
# declare data to be spatial dataset
coordinates(fire_au_18)= ~ longitude+latitude
# rename band names the raster stack
names(cal_au_18) <- c("maxtemp", "precip", "dryness", "ndvi", "elevation", "svi")
# update the file by extracting the raster values
cal_fire_au_18 <- data.frame(fire_au_18, raster::extract(cal_au_18, fire_au_18))
# drop optional column
cal_fire_au_18 <- cal_fire_au_18[,-6]


#RUN ANALYSIS

#MaxEnt Analysis & logistic regression modeling

# prepare data for maxent and logistic regression modelling
# Winter 2015-16
occ_cal_fire_wi_15_16 <- cal_fire_wi_15_16[,-c(3:11)]

# get the same random sample for training and testing
set.seed(20000430)

#Use K fold to split into test and train
group <- kfold(occ_cal_fire_wi_15_16, 5)
occ_train_15_16 <- occ_cal_fire_wi_15_16[group != 1, ]
occ_test_15_16 <- occ_cal_fire_wi_15_16[group == 1, ]
coordinates(occ_train_15_16)= ~ longitude+latitude
coordinates(occ_test_15_16)= ~ longitude+latitude

# extracting env conditions for training occ from the raster
# stack; a data frame is returned (i.e multiple columns)
presvals_15_16 <- raster::extract(cal_win_15_16, occ_train_15_16)
# env conditions for testing occ
presvals_test_15_16 <- raster::extract(cal_win_15_16, occ_test_15_16)

# create background points twice the size of train data
set.seed(20000430)
bg_15_16 <- as.data.frame(randomPoints(cal_win_15_16, 2*length(occ_train_15_16)))
coordinates(bg_15_16)= ~ x+y
# extracting env conditions for background
absvals_15_16 <- raster::extract(cal_win_15_16, bg_15_16)
# create vector for 1s and 0s for presvals and absvals respectively
pres_abs_15_16 <- c(rep(1, nrow(presvals_15_16)), rep(0, nrow(absvals_15_16)))
# for maxent analysis
pder_final_15_16 <- as.data.frame(rbind(presvals_15_16, absvals_15_16))
# for logistic regression analysis
logistic_final_15_16 <- as.data.frame(cbind(pres_abs_15_16, pder_final_15_16))
colnames(logistic_final_15_16)[1] <- "wildfire"

#logistic regression model
log.model_15_16 <- glm(wildfire ~ maxtemp + precip + dryness + ndvi + elevation + svi, family = binomial(link = "logit"), data = logistic_final_15_16)
summary(log.model_15_16)           # p-values 
exp(coefficients(log.model_15_16)) # converts to odds ratios

#extract McFadden's R2
pR2(log.model_15_16)

Sys.setenv(JAVA_HOME='/Library/Internet Plug-Ins/JavaAppletPlugin.plugin/Contents/Home')
#get wd to save the outputs 
getwd()

#load maxent 
maxent()
# maxent analysis
# train Maxent with tabular data
model_15_16 <- maxent(x=pder_final_15_16, ## env conditions
							p=pres_abs_15_16,   ## 1:presence or 0:absence
							path=paste0("/Users/lucytaylor/Library/Mobile Documents/com~apple~CloudDocs/Masters/Dissertation/dissertation/diss"), ## folder for maxent output; 
							# results in a file
							args=c("responsecurves") ## parameter specification
							)

# see output in internet browser 
model_15_16

# perform spatial prediction using training output from maxent model
cal_pred_15_16 <- predict(model_15_16, cal_win_15_16)
plot(cal_pred_15_16, main="Predicted Probability (in Winter 2015/16)")

#model evaluation
# using 'training data' to evaluate p & a are dataframe/s
# (the p and a are the training presence and background
# points)
mod_eval_train_15_16 <- dismo::evaluate(p = presvals_15_16 , a = absvals_15_16, model = model_15_16)
print(mod_eval_train_15_16)

mod_eval_test_15_16 <- dismo::evaluate(p = presvals_test_15_16, a = absvals_15_16, model = model_15_16)
print(mod_eval_test_15_16)  #training AUC may be higher than testing AUC

# calculate thresholds of models
thd1_wi_15_16 <- threshold(mod_eval_train_15_16, "spec_sens")  # highest TSS

# plotting points that are above the previously calculated
# thresholded value
plot(cal_pred_15_16 >= thd1_wi_15_16, main="Predicted threshold extents of wildfires [in Winter 2015-16]")


# map of probability 
tm_shape(cal_pred_15_16)+
  tm_raster(title = "Predicted probability", palette = '-RdYlGn', style ='cont', breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0))+
  tm_layout(main.title = "Winter 2015/2016", main.title.position = c(0.2, 0.7), title.size=3, legend.text.size = 1.1, 
            legend.position = c(0.65, 0.55), legend.height= -0.3, legend.title.size = 1.1, frame='white')+
  tm_scale_bar(position=c(0.02, 0.02), text.size = 1, breaks = c(0, 100, 200, 300))+
  tm_compass(north = 0,type = 'arrow', position = c('right', 'top'), text.size = 0.9)

#make raster for above threshold
ab_thd1_wi_15_16 <- cal_pred_15_16 >= thd1_wi_15_16
# map of threshold 
tm_shape(ab_thd1_wi_15_16)+
  tm_raster(style = "cat", title = "Threshold", palette= c("grey", "red"), labels = c("Below", "Above"))+
  tm_layout(main.title = "Winter 2015/2016", main.title.position = c(0.2, 0.7), title.size=3, legend.text.size = 1.1, 
            legend.position = c(0.65, 0.55), legend.height= -0.2, legend.title.size = 1.2, frame='white')+
  tm_scale_bar(position=c(0.02, 0.02), text.size = 1, breaks = c(0, 100, 200, 300))+
  tm_compass(north = 0,type = 'arrow', position = c('right', 'top'), text.size = 0.9)+tm_shape(cal_county) + tm_polygons(alpha = 0, border.col = "black") 

# see threshold 
thd1_wi_15_16
# prep threshold total map 
cal_th_wi_15_16_cl <- c(cal_pred_15_16@data@min-1, 0.541, 0, 0.541, cal_pred_15_16@data@max+1, 1)
cal_th_wi_15_16_mat <- matrix(cal_th_wi_15_16_cl, ncol = 3, byrow = TRUE)
cal_th_wi_15_16_mat
cal_th_wi_15_16_recl <- reclassify(cal_pred_15_16, cal_th_wi_15_16_mat)

tm_shape(cal_th_wi_15_16_recl) + tm_raster(style = "cat", title = "Threshold", palette= c("grey", "red"), labels = c("Above threshold", "Below Threshold")) +
  tm_shape(cal) + tm_polygons(alpha = 0, border.col = "black") +
  tm_layout(frame = FALSE, legend.outside = TRUE)

cal_th_wi_15_16 <- cal_pred_15_16 >= thd1_wi_15_16
class(cal_th_wi_15_16)

#repeat for each season each year 

# prepare data for maxent and logistic regression modelling
# Spring 2016
occ_cal_fire_sp_16 <- cal_fire_sp_16[,-c(3:11)]

# get the same random sample for training and testing
set.seed(20000430)

#Use K fold to split into test and train
group <- kfold(occ_cal_fire_sp_16, 5)
occ_train_sp_16 <- occ_cal_fire_sp_16[group != 1, ]
occ_test_sp_16 <- occ_cal_fire_sp_16[group == 1, ]
coordinates(occ_train_sp_16)= ~ longitude+latitude
coordinates(occ_test_sp_16)= ~ longitude+latitude

# extracting env conditions for training occ from the raster
# stack; a data frame is returned (i.e multiple columns)
presvals_sp_16 <- raster::extract(cal_sp_16, occ_train_sp_16)
# env conditions for testing occ
presvals_test_sp_16 <- raster::extract(cal_sp_16, occ_test_sp_16)

# create background points twice the size of train data
set.seed(20000430)
bg_sp_16 <- as.data.frame(randomPoints(cal_sp_16, 2*length(occ_train_sp_16)))
coordinates(bg_sp_16)= ~ x+y
# extracting env conditions for background
absvals_sp_16 <- raster::extract(cal_sp_16, bg_sp_16)
# create vector for 1s and 0s for presvals and absvals respectively
pres_abs_sp_16 <- c(rep(1, nrow(presvals_sp_16)), rep(0, nrow(absvals_sp_16)))
# for maxent analysis
pder_final_sp_16 <- as.data.frame(rbind(presvals_sp_16, absvals_sp_16))
# for logistic regression analysis
logistic_final_sp_16 <- as.data.frame(cbind(pres_abs_sp_16, pder_final_sp_16))
colnames(logistic_final_sp_16)[1] <- "wildfire"

# logistic regression model 
log.model_sp_16 <- glm(wildfire ~ maxtemp + precip + dryness + ndvi + elevation + svi, family = binomial(link = "logit"), data = logistic_final_sp_16)
summary(log.model_sp_16)           # look here for p-values 
exp(coefficients(log.model_sp_16)) # converts to odds ratios 

#McFadden's pseudo-R2
pR2(log.model_sp_16)

# maxent analysis
# train Maxent with tabular data
model_sp_16 <- maxent(x=pder_final_sp_16, ## env conditions
											p=pres_abs_sp_16,   ## 1:presence or 0:absence
											path=paste0("/Users/lucytaylor/Library/Mobile Documents/com~apple~CloudDocs/Masters/Dissertation/dissertation/diss"), ## folder for maxent output; 
											#put in a file
											args=c("responsecurves") ## parameter specification
)

# see output in internet browser 
model_sp_16

# perform spatial prediction using training output from maxent model
cal_pred_sp_16 <- predict(model_sp_16, cal_sp_16)
plot(cal_pred_sp_16, main="Predicted Probability (in Spring 2016)")

### model evaluation for table 3
# using 'training data' to evaluate p & a are dataframe/s
# (the p and a are the training presence and background
# points)
mod_eval_train_sp_16 <- dismo::evaluate(p = presvals_sp_16 , a = absvals_sp_16, model = model_sp_16)
print(mod_eval_train_sp_16)

mod_eval_test_sp_16 <- dismo::evaluate(p = presvals_test_sp_16, a = absvals_sp_16, model = model_sp_16)
print(mod_eval_test_sp_16)  # training AUC may be higher than testing AUC

# calculate thresholds of models
thd1_sp_16 <- threshold(mod_eval_train_sp_16, "spec_sens")  # highest TSS

# plotting points that are above the previously calculated
# thresholded value
plot(cal_pred_sp_16 >= thd1_sp_16, main="Predicted threshold extents of Wildfires [in Spring 2016]")

#map of probability 
tm_shape(cal_pred_sp_16)+
  tm_raster(title = "Predicted probability", palette = '-RdYlGn', style ='cont', breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0))+
  tm_layout(main.title = "Spring 2016", main.title.position = c(0.2, 0.7), title.size=3, legend.text.size = 1.1, 
            legend.position = c(0.65, 0.55), legend.height= -0.3, legend.title.size = 1.1, frame='white')+
  tm_scale_bar(position=c(0.02, 0.02), text.size = 1, breaks = c(0, 100, 200, 300))+
  tm_compass(north = 0,type = 'arrow', position = c('right', 'top'), text.size = 0.9)

#make raster for above threshold
ab_thd1_sp_16 <- cal_pred_sp_16 >= thd1_sp_16
# map of threshold 
tm_shape(ab_thd1_sp_16)+
  tm_raster(style = "cat", title = "Threshold", palette= c("grey", "red"), labels = c("Below", "Above"))+
  tm_layout(main.title = "Spring 2016", main.title.position = c(0.2, 0.7), title.size=3, legend.text.size = 1.1, 
            legend.position = c(0.65, 0.55), legend.height= -0.2, legend.title.size = 1.2, frame='white')+
  tm_scale_bar(position=c(0.02, 0.02), text.size = 1, breaks = c(0, 100, 200, 300))+
  tm_compass(north = 0,type = 'arrow', position = c('right', 'top'), text.size = 0.9)+tm_shape(cal_county) + tm_polygons(alpha = 0, border.col = "black") 

# see threshold 
thd1_sp_16
# prep threshold total map 
cal_th_sp_16_cl <- c(cal_pred_sp_16@data@min-1, 0.431, 0, 0.431, cal_pred_sp_16@data@max+1, 1)
cal_th_sp_16_mat <- matrix(cal_th_sp_16_cl, ncol = 3, byrow = TRUE)
cal_th_sp_16_mat
cal_th_sp_16_recl <- reclassify(cal_pred_sp_16, cal_th_sp_16_mat)

tm_shape(cal_th_sp_16_recl) + tm_raster(style = "cat", title = "Threshold", palette= c("grey", "red"), labels = c("Above threshold", "Below Threshold")) +
  tm_shape(cal) + tm_polygons(alpha = 0, border.col = "black") +
  tm_layout(frame = FALSE, legend.outside = TRUE)

# prepare data for maxent and logistic regression modelling
# Summer 2016
occ_cal_fire_su_16 <- cal_fire_su_16[,-c(3:11)]

# get the same random sample for training and testing
set.seed(20000430)

#Use K fold to split into test and train
group <- kfold(occ_cal_fire_su_16, 5)
occ_train_su_16 <- occ_cal_fire_su_16[group != 1, ]
occ_test_su_16 <- occ_cal_fire_su_16[group == 1, ]
coordinates(occ_train_su_16)= ~ longitude+latitude
coordinates(occ_test_su_16)= ~ longitude+latitude

# extracting env conditions for training occ from the raster
# stack; a data frame is returned (i.e multiple columns)
presvals_su_16 <- raster::extract(cal_su_16, occ_train_su_16)
# env conditions for testing occ
presvals_test_su_16 <- raster::extract(cal_su_16, occ_test_su_16)

# create background points twice the size of train data
set.seed(20000430)
bg_su_16 <- as.data.frame(randomPoints(cal_su_16, 2*length(occ_train_su_16)))
coordinates(bg_su_16)= ~ x+y
# extracting env conditions for background
absvals_su_16 <- raster::extract(cal_su_16, bg_su_16)
# create vector for 1s and 0s for presvals and absvals respectively
pres_abs_su_16 <- c(rep(1, nrow(presvals_su_16)), rep(0, nrow(absvals_su_16)))
# for maxent analysis
pder_final_su_16 <- as.data.frame(rbind(presvals_su_16, absvals_su_16))
# for logistic regression analysis
logistic_final_su_16 <- as.data.frame(cbind(pres_abs_su_16, pder_final_su_16))
colnames(logistic_final_su_16)[1] <- "wildfire"

# logistic regression model 
log.model_su_16 <- glm(wildfire ~ maxtemp + precip + dryness + ndvi + elevation + svi, family = binomial(link = "logit"), data = logistic_final_su_16)
summary(log.model_su_16)           # p-values 
exp(coefficients(log.model_su_16)) # converts to odds ratios 

#McFadden's pseudo-R2
pR2(log.model_su_16)

# maxent analysis
# train Maxent with tabular data
model_su_16 <- maxent(x=pder_final_su_16, ## env conditions
											p=pres_abs_su_16,   ## 1:presence or 0:absence
											path=paste0("/Users/lucytaylor/Library/Mobile Documents/com~apple~CloudDocs/Masters/Dissertation/dissertation/diss"), ## folder for maxent output; 
											# results in a file, .
											args=c("responsecurves") ## parameter specification
)

# see output in internet browser 
model_su_16

# perform spatial prediction using training output from maxent model
cal_pred_su_16 <- predict(model_su_16, cal_su_16)
plot(cal_pred_su_16, main="Predicted Probability (in Summer 2016)")

### model evaluation 
# using 'training data' to evaluate p & a are dataframe/s
# (the p and a are the training presence and background
# points)
mod_eval_train_su_16 <- dismo::evaluate(p = presvals_su_16 , a = absvals_su_16, model = model_su_16)
print(mod_eval_train_su_16)

mod_eval_test_su_16 <- dismo::evaluate(p = presvals_test_su_16, a = absvals_su_16, model = model_su_16)
print(mod_eval_test_su_16)  # training AUC may be higher than testing AUC

# calculate thresholds of models
thd1_su_16 <- threshold(mod_eval_train_su_16, "spec_sens")  # highest TSS

# plotting points that are above the previously calculated
# threshold value
plot(cal_pred_su_16 >= thd1_su_16, main="Predicted threshold extents of Wildfires [in Summer 2016]")

tm_shape(cal_pred_su_16 >= thd1_su_16)+
  tm_raster()

#map of probability 
tm_shape(cal_pred_su_16)+
  tm_raster(title = "Predicted probability", palette = '-RdYlGn', style ='cont', breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0))+
  tm_layout(main.title = "Summer 2016", main.title.position = c(0.2, 0.7), title.size=3, legend.text.size = 1.1, 
            legend.position = c(0.65, 0.55), legend.height= -0.3, legend.title.size = 1.1, frame='white')+
  tm_scale_bar(position=c(0.02, 0.02), text.size = 1, breaks = c(0, 100, 200, 300))+
  tm_compass(north = 0,type = 'arrow', position = c('right', 'top'), text.size = 0.9)

#make raster for above threshold
ab_thd1_su_16 <- cal_pred_su_16 >= thd1_su_16
# map of threshold 
tm_shape(ab_thd1_su_16)+
  tm_raster(style = "cat", title = "Threshold", palette= c("grey", "red"), labels = c("Below", "Above"))+
  tm_layout(main.title = "Summer 2016", main.title.position = c(0.2, 0.7), title.size=3, legend.text.size = 1.1, 
            legend.position = c(0.65, 0.55), legend.height= -0.2, legend.title.size = 1.2, frame='white')+
  tm_scale_bar(position=c(0.02, 0.02), text.size = 1, breaks = c(0, 100, 200, 300))+
  tm_compass(north = 0,type = 'arrow', position = c('right', 'top'), text.size = 0.9)+tm_shape(cal_county) + tm_polygons(alpha = 0, border.col = "black") 

# see threshold 
thd1_su_16
# prep threshold total map 
cal_th_su_16_cl <- c(cal_pred_su_16@data@min-1, 0.141, 0, 0.141, cal_pred_su_16@data@max+1, 1)
cal_th_su_16_mat <- matrix(cal_th_su_16_cl, ncol = 3, byrow = TRUE)
cal_th_su_16_mat
cal_th_su_16_recl <- reclassify(cal_pred_su_16, cal_th_su_16_mat)

tm_shape(cal_th_su_16_recl) + tm_raster(style = "cat", title = "Threshold", palette= c("grey", "red"), labels = c("Above threshold", "Below Threshold")) +
  tm_shape(cal) + tm_polygons(alpha = 0, border.col = "black") +
  tm_layout(frame = FALSE, legend.outside = TRUE)


# prepare data for maxent and logistic regression modelling
# Autumn 2016
occ_cal_fire_au_16 <- cal_fire_au_16[,-c(3:11)]

# get the same random sample for training and testing
set.seed(20000430)

#Use K fold to split into test and train
group <- kfold(occ_cal_fire_au_16, 5)
occ_train_au_16 <- occ_cal_fire_au_16[group != 1, ]
occ_test_au_16 <- occ_cal_fire_au_16[group == 1, ]
coordinates(occ_train_au_16)= ~ longitude+latitude
coordinates(occ_test_au_16)= ~ longitude+latitude

# extracting env conditions for training occ from the raster
# stack; a data frame is returned (i.e multiple columns)
presvals_au_16 <- raster::extract(cal_au_16, occ_train_au_16)
# env conditions for testing occ
presvals_test_au_16 <- raster::extract(cal_au_16, occ_test_au_16)

# create background points twice the size of train data
set.seed(20000430)
bg_au_16 <- as.data.frame(randomPoints(cal_au_16, 2*length(occ_train_au_16)))
coordinates(bg_au_16)= ~ x+y
# extracting env conditions for background
absvals_au_16 <- raster::extract(cal_au_16, bg_au_16)
# create vector for 1s and 0s for presvals and absvals respectively
pres_abs_au_16 <- c(rep(1, nrow(presvals_au_16)), rep(0, nrow(absvals_au_16)))
# for maxent analysis
pder_final_au_16 <- as.data.frame(rbind(presvals_au_16, absvals_au_16))
# for logistic regression analysis
logistic_final_au_16 <- as.data.frame(cbind(pres_abs_au_16, pder_final_au_16))
colnames(logistic_final_au_16)[1] <- "wildfire"

# logistic regression model 
log.model_au_16 <- glm(wildfire ~ maxtemp + precip + dryness + ndvi + elevation + svi, family = binomial(link = "logit"), data = logistic_final_au_16)
summary(log.model_au_16)           # look here for p-values 
exp(coefficients(log.model_au_16)) # converts to odds ratios 

#McFadden's pseudo R2
pR2(log.model_au_16)

# maxent analysis
# train Maxent with tabular data
model_au_16 <- maxent(x=pder_final_au_16, ## env conditions
                      p=pres_abs_au_16,   ## 1:presence or 0:absence
                      path=paste0("/Users/lucytaylor/Library/Mobile Documents/com~apple~CloudDocs/Masters/Dissertation/dissertation/diss"), ## folder for maxent output; 
                      # put in file
                      args=c("responsecurves") ## parameter specification
)

# see output in internet browser 
model_au_16

# perform spatial prediction using training output from maxent model
cal_pred_au_16 <- predict(model_au_16, cal_au_16)
plot(cal_pred_au_16, main="Predicted Probability (in Autumn 2016)")

### model evaluation
# using 'training data' to evaluate p & a are dataframe/s
# (the p and a are the training presence and background
# points)
mod_eval_train_au_16 <- dismo::evaluate(p = presvals_au_16 , a = absvals_au_16, model = model_au_16)
print(mod_eval_train_au_16)

mod_eval_test_au_16 <- dismo::evaluate(p = presvals_test_au_16, a = absvals_au_16, model = model_au_16)
print(mod_eval_test_au_16)  # training AUC may be higher than testing AUC

# calculate thresholds of models
thd1_au_16 <- threshold(mod_eval_train_au_16, "spec_sens")  # highest TSS

# plotting points that are above the previously calculated
# thresholded value
plot(cal_pred_au_16 >= thd1_au_16, main="Predicted threshold extents of Wildfires [in Autumn 2016]")

#map of probability 
tm_shape(cal_pred_au_16)+
  tm_raster(title = "Predicted probability", palette = '-RdYlGn', style ='cont', breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0))+
  tm_layout(main.title = "Autumn 2016", main.title.position = c(0.2, 0.7), title.size=3, legend.text.size = 1.1, 
            legend.position = c(0.65, 0.55), legend.height= -0.3, legend.title.size = 1.1, frame='white')+
  tm_scale_bar(position=c(0.02, 0.02), text.size = 1, breaks = c(0, 100, 200, 300))+
  tm_compass(north = 0,type = 'arrow', position = c('right', 'top'), text.size = 0.9)

#make raster for above threshold
ab_thd1_au_16 <- cal_pred_au_16 >= thd1_au_16
# map of threshold 
tm_shape(ab_thd1_au_16)+
  tm_raster(style = "cat", title = "Threshold", palette= c("grey", "red"), labels = c("Below", "Above"))+
  tm_layout(main.title = "Autumn 2016", main.title.position = c(0.2, 0.7), title.size=3, legend.text.size = 1.1, 
            legend.position = c(0.65, 0.55), legend.height= -0.2, legend.title.size = 1.2, frame='white')+
  tm_scale_bar(position=c(0.02, 0.02), text.size = 1, breaks = c(0, 100, 200, 300))+
  tm_compass(north = 0,type = 'arrow', position = c('right', 'top'), text.size = 0.9)+tm_shape(cal_county) + tm_polygons(alpha = 0, border.col = "black") 

# see threshold 
thd1_au_16
# prep threshold total map 
cal_th_au_16_cl <- c(cal_pred_au_16@data@min-1, 0.364, 0, 0.364, cal_pred_au_16@data@max+1, 1)
cal_th_au_16_mat <- matrix(cal_th_au_16_cl, ncol = 3, byrow = TRUE)
cal_th_au_16_mat
cal_th_au_16_recl <- reclassify(cal_pred_au_16, cal_th_au_16_mat)

tm_shape(cal_th_au_16_recl) + tm_raster(style = "cat", title = "Threshold", palette= c("grey", "red"), labels = c("Above threshold", "Below Threshold")) +
  tm_shape(cal) + tm_polygons(alpha = 0, border.col = "black") +
  tm_layout(frame = FALSE, legend.outside = TRUE)

# prepare data for maxent and logistic regression modelling
# Winter 2016-17
occ_cal_fire_wi_16_17 <- cal_fire_wi_16_17[,-c(3:11)]

# get the same random sample for training and testing
set.seed(20000430)

#Use K fold to split into test and train
group <- kfold(occ_cal_fire_wi_16_17, 5)
occ_train_wi_16_17 <- occ_cal_fire_wi_16_17[group != 1, ]
occ_test_wi_16_17 <- occ_cal_fire_wi_16_17[group == 1, ]
coordinates(occ_train_wi_16_17)= ~ longitude+latitude
coordinates(occ_test_wi_16_17)= ~ longitude+latitude

# extracting env conditions for training occ from the raster
# stack; a data frame is returned (i.e multiple columns)
presvals_wi_16_17 <- raster::extract(cal_win_16_17, occ_train_wi_16_17)
# env conditions for testing occ
presvals_test_wi_16_17 <- raster::extract(cal_win_16_17, occ_test_wi_16_17)

# create background points twice the size of train data
set.seed(20000430)
bg_wi_16_17 <- as.data.frame(randomPoints(cal_win_16_17, 2*length(occ_train_wi_16_17)))
coordinates(bg_wi_16_17)= ~ x+y
# extracting env conditions for background
absvals_wi_16_17 <- raster::extract(cal_win_16_17, bg_wi_16_17)
# create vector for 1s and 0s for presvals and absvals respectively
pres_abs_wi_16_17 <- c(rep(1, nrow(presvals_wi_16_17)), rep(0, nrow(absvals_wi_16_17)))
# for maxent analysis
pder_final_wi_16_17 <- as.data.frame(rbind(presvals_wi_16_17, absvals_wi_16_17))
# for logistic regression analysis
logistic_final_wi_16_17 <- as.data.frame(cbind(pres_abs_wi_16_17, pder_final_wi_16_17))
colnames(logistic_final_wi_16_17)[1] <- "wildfire"

# logistic regression model 
log.model_wi_16_17 <- glm(wildfire ~ maxtemp + precip + dryness + ndvi + elevation + svi, family = binomial(link = "logit"), data = logistic_final_wi_16_17)
summary(log.model_wi_16_17)           # look here for p-values 
exp(coefficients(log.model_wi_16_17)) # converts to odds ratios 

#McFadden's pseudo-R2
pR2(log.model_wi_16_17)

# maxent analysis
# train Maxent with tabular data
model_wi_16_17 <- maxent(x=pder_final_wi_16_17, ## env conditions
                      p=pres_abs_wi_16_17,   ## 1:presence or 0:absence
                      path=paste0("/Users/lucytaylor/Library/Mobile Documents/com~apple~CloudDocs/Masters/Dissertation/dissertation/diss"), ## folder for maxent output; 
                      # put in file
                      args=c("responsecurves") ## parameter specification
)

# see output in internet browser 
model_wi_16_17

# perform spatial prediction using training output from maxent model
cal_pred_wi_16_17 <- predict(model_wi_16_17, cal_win_16_17)
plot(cal_pred_wi_16_17, main="Predicted Probability (in Winter 16-17)")

### model evaluation for table 3
# using 'training data' to evaluate p & a are dataframe/s
# (the p and a are the training presence and background
# points)
mod_eval_train_wi_16_17 <- dismo::evaluate(p = presvals_wi_16_17 , a = absvals_wi_16_17, model = model_wi_16_17)
print(mod_eval_train_wi_16_17)

mod_eval_test_wi_16_17 <- dismo::evaluate(p = presvals_test_wi_16_17, a = absvals_wi_16_17, model = model_wi_16_17)
print(mod_eval_test_wi_16_17)  # training AUC may be higher than testing AUC

# calculate thresholds of models
thd1_wi_16_17 <- threshold(mod_eval_train_wi_16_17, "spec_sens")  # highest TSS

# plotting points that are above the previously calculated
# thresholded value
plot(cal_pred_wi_16_17 >= thd1_wi_16_17, main="Predicted threshold extents of Wildfires [in Autumn 2016]")

#map of probability 
tm_shape(cal_pred_wi_16_17)+
  tm_raster(title = "Predicted probability", palette = '-RdYlGn', style ='cont', breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0))+
  tm_layout(main.title = "Winter 2016/2017", main.title.position = c(0.2, 0.7), title.size=3, legend.text.size = 1.1, 
            legend.position = c(0.65, 0.55), legend.height= -0.3, legend.title.size = 1.1, frame='white')+
  tm_scale_bar(position=c(0.02, 0.02), text.size = 1, breaks = c(0, 100, 200, 300))+
  tm_compass(north = 0,type = 'arrow', position = c('right', 'top'), text.size = 0.9)

#make raster for above threshold
ab_thd1_wi_16_17 <- cal_pred_wi_16_17 >= thd1_wi_16_17
# map of threshold 
tm_shape(ab_thd1_wi_16_17)+
  tm_raster(style = "cat", title = "Threshold", palette= c("grey", "red"), labels = c("Below", "Above"))+
  tm_layout(main.title = "Winter 2016/2017", main.title.position = c(0.2, 0.7), title.size=3, legend.text.size = 1.1, 
            legend.position = c(0.65, 0.55), legend.height= -0.2, legend.title.size = 1.2, frame='white')+
  tm_scale_bar(position=c(0.02, 0.02), text.size = 1, breaks = c(0, 100, 200, 300))+
  tm_compass(north = 0,type = 'arrow', position = c('right', 'top'), text.size = 0.9)+tm_shape(cal_county) + tm_polygons(alpha = 0, border.col = "black") 

# see threshold 
thd1_wi_16_17
# prep threshold total map 
cal_th_wi_16_17_cl <- c(cal_pred_wi_16_17@data@min-1, 0.573, 0, 0.573, cal_pred_wi_16_17@data@max+1, 1)
cal_th_wi_16_17_mat <- matrix(cal_th_wi_16_17_cl, ncol = 3, byrow = TRUE)
cal_th_wi_16_17_mat
cal_th_wi_16_17_recl <- reclassify(cal_pred_wi_16_17, cal_th_wi_16_17_mat)

tm_shape(cal_th_wi_16_17_recl) + tm_raster(style = "cat", title = "Threshold", palette= c("grey", "red"), labels = c("Above threshold", "Below Threshold")) +
  tm_shape(cal) + tm_polygons(alpha = 0, border.col = "black") +
  tm_layout(frame = FALSE, legend.outside = TRUE)

# prepare data for maxent and logistic regression modelling
# Spring 2017
occ_cal_fire_sp_17 <- cal_fire_sp_17[,-c(3:11)]

# get the same random sample for training and testing
set.seed(20000430)

#Use K fold to split into test and train
group <- kfold(occ_cal_fire_sp_17, 5)
occ_train_sp_17 <- occ_cal_fire_sp_17[group != 1, ]
occ_test_sp_17 <- occ_cal_fire_sp_17[group == 1, ]
coordinates(occ_train_sp_17)= ~ longitude+latitude
coordinates(occ_test_sp_17)= ~ longitude+latitude

# extracting env conditions for training occ from the raster
# stack; a data frame is returned (i.e multiple columns)
presvals_sp_17 <- raster::extract(cal_sp_17, occ_train_sp_17)
# env conditions for testing occ
presvals_test_sp_17 <- raster::extract(cal_sp_17, occ_test_sp_17)

# create background points twice the size of train data
set.seed(20000430)
bg_sp_17 <- as.data.frame(randomPoints(cal_sp_17, 2*length(occ_train_sp_17)))
coordinates(bg_sp_17)= ~ x+y
# extracting env conditions for background
absvals_sp_17 <- raster::extract(cal_sp_17, bg_sp_17)
# create vector for 1s and 0s for presvals and absvals respectively
pres_abs_sp_17 <- c(rep(1, nrow(presvals_sp_17)), rep(0, nrow(absvals_sp_17)))
# for maxent analysis
pder_final_sp_17 <- as.data.frame(rbind(presvals_sp_17, absvals_sp_17))
# for logistic regression analysis
logistic_final_sp_17 <- as.data.frame(cbind(pres_abs_sp_17, pder_final_sp_17))
colnames(logistic_final_sp_17)[1] <- "wildfire"

# logistic regression model
log.model_sp_17 <- glm(wildfire ~ maxtemp + precip + dryness + ndvi + elevation + svi, family = binomial(link = "logit"), data = logistic_final_sp_17)
summary(log.model_sp_17)           # look here for p-values 
exp(coefficients(log.model_sp_17)) # converts to odds ratios 

#McFadden's pseudo-R2
pR2(log.model_sp_17)

# maxent analysis
# train Maxent with tabular data
model_sp_17 <- maxent(x=pder_final_sp_17, ## env conditions
                         p=pres_abs_sp_17,   ## 1:presence or 0:absence
                         path=paste0("/Users/lucytaylor/Library/Mobile Documents/com~apple~CloudDocs/Masters/Dissertation/dissertation/diss"), ## folder for maxent output; 
                         # put in file
                         args=c("responsecurves") ## parameter specification
)

# see output in internet browser 
model_sp_17

# perform spatial prediction using training output from maxent model
cal_pred_sp_17 <- predict(model_sp_17, cal_sp_17)
plot(cal_pred_sp_17, main="Predicted Probability (in Spring 2017)")

### model evaluation for table 3
# using 'training data' to evaluate p & a are dataframe/s
# (the p and a are the training presence and background
# points)
mod_eval_train_sp_17 <- dismo::evaluate(p = presvals_sp_17 , a = absvals_sp_17, model = model_sp_17)
print(mod_eval_train_sp_17)

mod_eval_test_sp_17 <- dismo::evaluate(p = presvals_test_sp_17, a = absvals_sp_17, model = model_sp_17)
print(mod_eval_test_sp_17)  # training AUC may be higher than testing AUC

# calculate thresholds of models
thd1_sp_17 <- threshold(mod_eval_train_sp_17, "spec_sens")  # highest TSS

# plotting points that are above the previously calculated
# thresholded value
plot(cal_pred_sp_17 >= thd1_sp_17, main="Predicted threshold extents of Wildfires [in Spring 2017]")

#map of probability 
tm_shape(cal_pred_sp_17)+
  tm_raster(title = "Predicted probability", palette = '-RdYlGn', style ='cont', breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0))+
  tm_layout(main.title = "Spring 2017", main.title.position = c(0.2, 0.7), title.size=3, legend.text.size = 1.1, 
            legend.position = c(0.65, 0.55), legend.height= -0.3, legend.title.size = 1.1, frame='white')+
  tm_scale_bar(position=c(0.02, 0.02), text.size = 1, breaks = c(0, 100, 200, 300))+
  tm_compass(north = 0,type = 'arrow', position = c('right', 'top'), text.size = 0.9)

#make raster for above threshold
ab_thd1_sp_17 <- cal_pred_sp_17 >= thd1_sp_17
# map of threshold 
tm_shape(ab_thd1_sp_17)+
  tm_raster(style = "cat", title = "Threshold", palette= c("grey", "red"), labels = c("Below", "Above"))+
  tm_layout(main.title = "Spring 2017", main.title.position = c(0.2, 0.7), title.size=3, legend.text.size = 1.1, 
            legend.position = c(0.65, 0.55), legend.height= -0.2, legend.title.size = 1.2, frame='white')+
  tm_scale_bar(position=c(0.02, 0.02), text.size = 1, breaks = c(0, 100, 200, 300))+
  tm_compass(north = 0,type = 'arrow', position = c('right', 'top'), text.size = 0.9)+tm_shape(cal_county) + tm_polygons(alpha = 0, border.col = "black") 

# see threshold 
thd1_sp_17
# prep threshold total map 
cal_th_sp_17_cl <- c(cal_pred_sp_17@data@min-1, 0.424, 0, 0.424, cal_pred_sp_17@data@max+1, 1)
cal_th_sp_17_mat <- matrix(cal_th_sp_17_cl, ncol = 3, byrow = TRUE)
cal_th_sp_17_mat
cal_th_sp_17_recl <- reclassify(cal_pred_sp_17, cal_th_sp_17_mat)

tm_shape(cal_th_sp_17_recl) + tm_raster(style = "cat", title = "Threshold", palette= c("grey", "red"), labels = c("Above threshold", "Below Threshold")) +
  tm_shape(cal) + tm_polygons(alpha = 0, border.col = "black") +
  tm_layout(frame = FALSE, legend.outside = TRUE)

# prepare data for maxent and logistic regression modelling
# Summer 2017
occ_cal_fire_su_17 <- cal_fire_su_17[,-c(3:11)]

# get the same random sample for training and testing
set.seed(20000430)

#Use K fold to split into test and train
group <- kfold(occ_cal_fire_su_17, 5)
occ_train_su_17 <- occ_cal_fire_su_17[group != 1, ]
occ_test_su_17 <- occ_cal_fire_su_17[group == 1, ]
coordinates(occ_train_su_17)= ~ longitude+latitude
coordinates(occ_test_su_17)= ~ longitude+latitude

# extracting env conditions for training occ from the raster
# stack; a data frame is returned (i.e multiple columns)
presvals_su_17 <- raster::extract(cal_su_17, occ_train_su_17)
# env conditions for testing occ
presvals_test_su_17 <- raster::extract(cal_su_17, occ_test_su_17)

# create background points twice the size of train data
set.seed(20000430)
bg_su_17 <- as.data.frame(randomPoints(cal_su_17, 2*length(occ_train_su_17)))
coordinates(bg_su_17)= ~ x+y
# extracting env conditions for background
absvals_su_17 <- raster::extract(cal_su_17, bg_su_17)
# create vector for 1s and 0s for presvals and absvals respectively
pres_abs_su_17 <- c(rep(1, nrow(presvals_su_17)), rep(0, nrow(absvals_su_17)))
# for maxent analysis
pder_final_su_17 <- as.data.frame(rbind(presvals_su_17, absvals_su_17))
# for logistic regression analysis
logistic_final_su_17 <- as.data.frame(cbind(pres_abs_su_17, pder_final_su_17))
colnames(logistic_final_su_17)[1] <- "wildfire"

# logistic regression model 
log.model_su_17 <- glm(wildfire ~ maxtemp + precip + dryness + ndvi + elevation + svi, family = binomial(link = "logit"), data = logistic_final_su_17)
summary(log.model_su_17)           # p-values 
exp(coefficients(log.model_su_17)) # converts to odds ratios 

#McFadden's pseudo-R2
pR2(log.model_su_17)

# maxent analysis
# train Maxent with tabular data
model_su_17 <- maxent(x=pder_final_su_17, ## env conditions
                      p=pres_abs_su_17,   ## 1:presence or 0:absence
                      path=paste0("/Users/lucytaylor/Library/Mobile Documents/com~apple~CloudDocs/Masters/Dissertation/dissertation/diss"), ## folder for maxent output; 
                      # put in file
                      args=c("responsecurves") ## parameter specification
)

# see output in internet browser 
model_su_17

# perform spatial prediction using training output from maxent model
cal_pred_su_17 <- predict(model_su_17, cal_su_17)
plot(cal_pred_su_17, main="Predicted Probability (in Summer 2017)")

### model evaluation
# using 'training data' to evaluate p & a are dataframe/s
# (the p and a are the training presence and background
# points)
mod_eval_train_su_17 <- dismo::evaluate(p = presvals_su_17 , a = absvals_su_17, model = model_su_17)
print(mod_eval_train_su_17)

mod_eval_test_su_17 <- dismo::evaluate(p = presvals_test_su_17, a = absvals_su_17, model = model_su_17)
print(mod_eval_test_su_17)  # training AUC may be higher than testing AUC

# calculate thresholds of models
thd1_su_17 <- threshold(mod_eval_train_su_17, "spec_sens")  # highest TSS

# plotting points that are above the previously calculated
# thresholded value
plot(cal_pred_su_17 >= thd1_su_17, main="Predicted threshold extents of Wildfires [in Summer 2017]")

#map of probability 
tm_shape(cal_pred_su_17)+
  tm_raster(title = "Predicted probability", palette = '-RdYlGn', style ='cont', breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0))+
  tm_layout(main.title = "Summer 2017", main.title.position = c(0.2, 0.7), title.size=3, legend.text.size = 1.1, 
            legend.position = c(0.65, 0.55), legend.height= -0.3, legend.title.size = 1.1, frame='white')+
  tm_scale_bar(position=c(0.02, 0.02), text.size = 1, breaks = c(0, 100, 200, 300))+
  tm_compass(north = 0,type = 'arrow', position = c('right', 'top'), text.size = 0.9)

#make raster for above threshold
ab_thd1_su_17 <- cal_pred_su_17 >= thd1_su_17
# map of threshold 
tm_shape(ab_thd1_su_17)+
  tm_raster(style = "cat", title = "Threshold", palette= c("grey", "red"), labels = c("Below", "Above"))+
  tm_layout(main.title = "Summer 2017", main.title.position = c(0.2, 0.7), title.size=3, legend.text.size = 1.1, 
            legend.position = c(0.65, 0.55), legend.height= -0.2, legend.title.size = 1.2, frame='white')+
  tm_scale_bar(position=c(0.02, 0.02), text.size = 1, breaks = c(0, 100, 200, 300))+
  tm_compass(north = 0,type = 'arrow', position = c('right', 'top'), text.size = 0.9)+tm_shape(cal_county) + tm_polygons(alpha = 0, border.col = "black") 

# see threshold 
thd1_su_17
# prep threshold total map 
cal_th_su_17_cl <- c(cal_pred_su_17@data@min-1, 0.317, 0, 0.317, cal_pred_su_17@data@max+1, 1)
cal_th_su_17_mat <- matrix(cal_th_su_17_cl, ncol = 3, byrow = TRUE)
cal_th_su_17_mat
cal_th_su_17_recl <- reclassify(cal_pred_su_17, cal_th_su_17_mat)

tm_shape(cal_th_su_17_recl) + tm_raster(style = "cat", title = "Threshold", palette= c("grey", "red"), labels = c("Above threshold", "Below Threshold")) +
  tm_shape(cal) + tm_polygons(alpha = 0, border.col = "black") +
  tm_layout(frame = FALSE, legend.outside = TRUE)

# prepare data for maxent and logistic regression modelling
# Autumn 2017
occ_cal_fire_au_17 <- cal_fire_au_17[,-c(3:11)]

# get the same random sample for training and testing
set.seed(20000430)

#Use K fold to split into test and train
group <- kfold(occ_cal_fire_au_17, 5)
occ_train_au_17 <- occ_cal_fire_au_17[group != 1, ]
occ_test_au_17 <- occ_cal_fire_au_17[group == 1, ]
coordinates(occ_train_au_17)= ~ longitude+latitude
coordinates(occ_test_au_17)= ~ longitude+latitude

# extracting env conditions for training occ from the raster
# stack; a data frame is returned (i.e multiple columns)
presvals_au_17 <- raster::extract(cal_au_17, occ_train_au_17)
# env conditions for testing occ
presvals_test_au_17 <- raster::extract(cal_au_17, occ_test_au_17)

# create background points twice the size of train data
set.seed(20000430)
bg_au_17 <- as.data.frame(randomPoints(cal_au_17, 2*length(occ_train_au_17)))
coordinates(bg_au_17)= ~ x+y
# extracting env conditions for background
absvals_au_17 <- raster::extract(cal_au_17, bg_au_17)
# create vector for 1s and 0s for presvals and absvals respectively
pres_abs_au_17 <- c(rep(1, nrow(presvals_au_17)), rep(0, nrow(absvals_au_17)))
# for maxent analysis
pder_final_au_17 <- as.data.frame(rbind(presvals_au_17, absvals_au_17))
# for logistic regression analysis
logistic_final_au_17 <- as.data.frame(cbind(pres_abs_au_17, pder_final_au_17))
colnames(logistic_final_au_17)[1] <- "wildfire"

# logistic regression model 
log.model_au_17 <- glm(wildfire ~ maxtemp + precip + dryness + ndvi + elevation + svi, family = binomial(link = "logit"), data = logistic_final_au_17)
summary(log.model_au_17)           # look here for p-values 
exp(coefficients(log.model_au_17)) # converts to odds ratios 

#McFadden's pseudo-R2
pR2(log.model_au_17)

# maxent analysis
# train Maxent with tabular data
model_au_17 <- maxent(x=pder_final_au_17, ## env conditions
                      p=pres_abs_au_17,   ## 1:presence or 0:absence
                      path=paste0("/Users/lucytaylor/Library/Mobile Documents/com~apple~CloudDocs/Masters/Dissertation/dissertation/diss"), ## folder for maxent output; 
                      # put in file
                      args=c("responsecurves") ## parameter specification
)

# see output in internet browser 
model_au_17

# perform spatial prediction using training output from maxent model
cal_pred_au_17 <- predict(model_au_17, cal_au_17)
plot(cal_pred_au_17, main="Predicted Probability (in Autumn 2017)")

### model evaluation for table 3
# using 'training data' to evaluate p & a are dataframe/s
# (the p and a are the training presence and background
# points)
mod_eval_train_au_17 <- dismo::evaluate(p = presvals_au_17 , a = absvals_au_17, model = model_au_17)
print(mod_eval_train_au_17)

mod_eval_test_au_17 <- dismo::evaluate(p = presvals_test_au_17, a = absvals_au_17, model = model_au_17)
print(mod_eval_test_au_17)  # training AUC may be higher than testing AUC

# calculate thresholds of models
thd1_au_17 <- threshold(mod_eval_train_au_17, "spec_sens")  # highest TSS

# plotting points that are above the previously calculated
# thresholded value
plot(cal_pred_au_17 >= thd1_au_17, main="Predicted threshold extents of Wildfires [in Autumn 2017]")

#map of probability 
tm_shape(cal_pred_au_17)+
  tm_raster(title = "Predicted probability", palette = '-RdYlGn', style ='cont', breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0))+
  tm_layout(main.title = "Autumn 2017", main.title.position = c(0.2, 0.7), title.size=3, legend.text.size = 1.1, 
            legend.position = c(0.65, 0.55), legend.height= -0.3, legend.title.size = 1.1, frame='white')+
  tm_scale_bar(position=c(0.02, 0.02), text.size = 1, breaks = c(0, 100, 200, 300))+
  tm_compass(north = 0,type = 'arrow', position = c('right', 'top'), text.size = 0.9)

#make raster for above threshold
ab_thd1_au_17 <- cal_pred_au_17 >= thd1_au_17
# map of threshold 
tm_shape(ab_thd1_au_17)+
  tm_raster(style = "cat", title = "Threshold", palette= c("grey", "red"), labels = c("Below", "Above"))+
  tm_layout(main.title = "Autumn 2017", main.title.position = c(0.2, 0.7), title.size=3, legend.text.size = 1.1, 
            legend.position = c(0.65, 0.55), legend.height= -0.2, legend.title.size = 1.2, frame='white')+
  tm_scale_bar(position=c(0.02, 0.02), text.size = 1, breaks = c(0, 100, 200, 300))+
  tm_compass(north = 0,type = 'arrow', position = c('right', 'top'), text.size = 0.9)+tm_shape(cal_county) + tm_polygons(alpha = 0, border.col = "black") 

# see threshold 
thd1_au_17
# prep threshold total map 
cal_th_au_17_cl <- c(cal_pred_au_17@data@min-1, 0.354, 0, 0.354, cal_pred_au_17@data@max+1, 1)
cal_th_au_17_mat <- matrix(cal_th_au_17_cl, ncol = 3, byrow = TRUE)
cal_th_au_17_mat
cal_th_au_17_recl <- reclassify(cal_pred_au_17, cal_th_au_17_mat)

tm_shape(cal_th_au_17_recl) + tm_raster(style = "cat", title = "Threshold", palette= c("grey", "red"), labels = c("Above threshold", "Below Threshold")) +
  tm_shape(cal) + tm_polygons(alpha = 0, border.col = "black") +
  tm_layout(frame = FALSE, legend.outside = TRUE)

# prepare data for maxent and logistic regression modelling
# Winter 2017-18
occ_cal_fire_wi_17_18 <- cal_fire_wi_17_18[,-c(3:11)]

# get the same random sample for training and testing
set.seed(20000430)

#Use K fold to split into test and train
group <- kfold(occ_cal_fire_wi_17_18, 5)
occ_train_wi_17_18 <- occ_cal_fire_wi_17_18[group != 1, ]
occ_test_wi_17_18 <- occ_cal_fire_wi_17_18[group == 1, ]
coordinates(occ_train_wi_17_18)= ~ longitude+latitude
coordinates(occ_test_wi_17_18)= ~ longitude+latitude

# extracting env conditions for training occ from the raster
# stack; a data frame is returned (i.e multiple columns)
presvals_wi_17_18 <- raster::extract(cal_win_17_18, occ_train_wi_17_18)
# env conditions for testing occ
presvals_test_wi_17_18 <- raster::extract(cal_win_17_18, occ_test_wi_17_18)

# create background points twice the size of train data
set.seed(20000430)
bg_wi_17_18 <- as.data.frame(randomPoints(cal_win_17_18, 2*length(occ_train_wi_17_18)))
coordinates(bg_wi_17_18)= ~ x+y
# extracting env conditions for background
absvals_wi_17_18 <- raster::extract(cal_win_17_18, bg_wi_17_18)
# create vector for 1s and 0s for presvals and absvals respectively
pres_abs_wi_17_18 <- c(rep(1, nrow(presvals_wi_17_18)), rep(0, nrow(absvals_wi_17_18)))
# for maxent analysis
pder_final_wi_17_18 <- as.data.frame(rbind(presvals_wi_17_18, absvals_wi_17_18))
# for logistic regression analysis
logistic_final_wi_17_18 <- as.data.frame(cbind(pres_abs_wi_17_18, pder_final_wi_17_18))
colnames(logistic_final_wi_17_18)[1] <- "wildfire"

# logistic regression model 
log.model_wi_17_18 <- glm(wildfire ~ maxtemp + precip + dryness + ndvi + elevation + svi, family = binomial(link = "logit"), data = logistic_final_wi_17_18)
summary(log.model_wi_17_18)           # look here for p-values
exp(coefficients(log.model_wi_17_18)) # converts to odds ratios

#McFadden's pseudo-R2
pR2(log.model_wi_17_18)

# maxent analysis
# train Maxent with tabular data
model_wi_17_18 <- maxent(x=pder_final_wi_17_18, ## env conditions
                      p=pres_abs_wi_17_18,   ## 1:presence or 0:absence
                      path=paste0("/Users/lucytaylor/Library/Mobile Documents/com~apple~CloudDocs/Masters/Dissertation/dissertation/diss"), ## folder for maxent output; 
                      # put in file.
                      args=c("responsecurves") ## parameter specification
)

# see output in internet browser 
model_wi_17_18

# perform spatial prediction using training output from maxent model
cal_pred_wi_17_18 <- predict(model_wi_17_18, cal_win_17_18)
plot(cal_pred_wi_17_18, main="Predicted Probability (in Winter 2017/2018)")

### model evaluation for table 3
# using 'training data' to evaluate p & a are dataframe/s
# (the p and a are the training presence and background
# points)
mod_eval_train_wi_17_18 <- dismo::evaluate(p = presvals_wi_17_18 , a = absvals_wi_17_18, model = model_wi_17_18)
print(mod_eval_train_wi_17_18)

mod_eval_test_wi_17_18 <- dismo::evaluate(p = presvals_test_wi_17_18, a = absvals_wi_17_18, model = model_wi_17_18)
print(mod_eval_test_wi_17_18)  # training AUC may be higher than testing AUC

# calculate thresholds of models
thd1_wi_17_18 <- threshold(mod_eval_train_wi_17_18, "spec_sens")  # highest TSS

# plotting points that are above the previously calculated
# thresholded value
plot(cal_pred_wi_17_18 >= thd1_wi_17_18, main="Predicted threshold extents of Wildfires [in Winter 2017/2018]")

#map of probability 
tm_shape(cal_pred_wi_17_18)+
  tm_raster(title = "Predicted probability", palette = '-RdYlGn', style ='cont', breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0))+
  tm_layout(main.title = "Winter 2017/2018", main.title.position = c(0.2, 0.7), title.size=3, legend.text.size = 1.1, 
            legend.position = c(0.65, 0.55), legend.height= -0.3, legend.title.size = 1.1, frame='white')+
  tm_scale_bar(position=c(0.02, 0.02), text.size = 1, breaks = c(0, 100, 200, 300))+
  tm_compass(north = 0,type = 'arrow', position = c('right', 'top'), text.size = 0.9)

#make raster for above threshold
ab_thd1_wi_17_18 <- cal_pred_wi_17_18 >= thd1_wi_17_18
# map of threshold 
tm_shape(ab_thd1_wi_17_18)+
  tm_raster(style = "cat", title = "Threshold", palette= c("grey", "red"), labels = c("Below", "Above"))+
  tm_layout(main.title = "Winter 2017/2018", main.title.position = c(0.2, 0.7), title.size=3, legend.text.size = 1.1, 
            legend.position = c(0.65, 0.55), legend.height= -0.2, legend.title.size = 1.2, frame='white')+
  tm_scale_bar(position=c(0.02, 0.02), text.size = 1, breaks = c(0, 100, 200, 300))+
  tm_compass(north = 0,type = 'arrow', position = c('right', 'top'), text.size = 0.9)+tm_shape(cal_county) + tm_polygons(alpha = 0, border.col = "black") 

# see threshold 
thd1_wi_17_18
# prep threshold total map 
cal_th_wi_17_18_cl <- c(cal_pred_wi_17_18@data@min-1, 0.153, 0, 0.153, cal_pred_wi_17_18@data@max+1, 1)
cal_th_wi_17_18_mat <- matrix(cal_th_wi_17_18_cl, ncol = 3, byrow = TRUE)
cal_th_wi_17_18_mat
cal_th_wi_17_18_recl <- reclassify(cal_pred_wi_17_18, cal_th_wi_17_18_mat)

tm_shape(cal_th_wi_17_18_recl) + tm_raster(style = "cat", title = "Threshold", palette= c("grey", "red"), labels = c("Above threshold", "Below Threshold")) +
  tm_shape(cal) + tm_polygons(alpha = 0, border.col = "black") +
  tm_layout(frame = FALSE, legend.outside = TRUE)

# prepare data for maxent and logistic regression modelling
# Spring 2018
occ_cal_fire_sp_18 <- cal_fire_sp_18[,-c(3:11)]

# get the same random sample for training and testing
set.seed(20000430)

#Use K fold to split into test and train
group <- kfold(occ_cal_fire_sp_18, 5)
occ_train_sp_18 <- occ_cal_fire_sp_18[group != 1, ]
occ_test_sp_18 <- occ_cal_fire_sp_18[group == 1, ]
coordinates(occ_train_sp_18)= ~ longitude+latitude
coordinates(occ_test_sp_18)= ~ longitude+latitude

# extracting env conditions for training occ from the raster
# stack; a data frame is returned (i.e multiple columns)
presvals_sp_18 <- raster::extract(cal_sp_18, occ_train_sp_18)
# env conditions for testing occ
presvals_test_sp_18 <- raster::extract(cal_sp_18, occ_test_sp_18)

# create background points twice the size of train data
set.seed(20000430)
bg_sp_18 <- as.data.frame(randomPoints(cal_sp_18, 2*length(occ_train_sp_18)))
coordinates(bg_sp_18)= ~ x+y
# extracting env conditions for background
absvals_sp_18 <- raster::extract(cal_sp_18, bg_sp_18)
# create vector for 1s and 0s for presvals and absvals respectively
pres_abs_sp_18 <- c(rep(1, nrow(presvals_sp_18)), rep(0, nrow(absvals_sp_18)))
# for maxent analysis
pder_final_sp_18 <- as.data.frame(rbind(presvals_sp_18, absvals_sp_18))
# for logistic regression analysis
logistic_final_sp_18 <- as.data.frame(cbind(pres_abs_sp_18, pder_final_sp_18))
colnames(logistic_final_sp_18)[1] <- "wildfire"

# logistic regression model 
log.model_sp_18 <- glm(wildfire ~ maxtemp + precip + dryness + ndvi + elevation + svi, family = binomial(link = "logit"), data = logistic_final_sp_18)
summary(log.model_sp_18)           # look here for p-values 
exp(coefficients(log.model_sp_18)) # converts to odds ratios 

#McFadden's pseudo-R2
pR2(log.model_sp_18)

# maxent analysis
# train Maxent with tabular data
model_sp_18 <- maxent(x=pder_final_sp_18, ## env conditions
                      p=pres_abs_sp_18,   ## 1:presence or 0:absence
                      path=paste0("/Users/lucytaylor/Library/Mobile Documents/com~apple~CloudDocs/Masters/Dissertation/dissertation/diss"), ## folder for maxent output; 
                      # put in file
                      args=c("responsecurves") ## parameter specification
)

# see output in internet browser 
model_sp_18

# perform spatial prediction using training output from maxent model
cal_pred_sp_18 <- predict(model_sp_18, cal_sp_18)
plot(cal_pred_sp_18, main="Predicted Probability (in Spring 2018)")

### model evaluation for table 3
# using 'training data' to evaluate p & a are dataframe/s
# (the p and a are the training presence and background
# points)
mod_eval_train_sp_18 <- dismo::evaluate(p = presvals_sp_18 , a = absvals_sp_18, model = model_sp_18)
print(mod_eval_train_sp_18)

mod_eval_test_sp_18 <- dismo::evaluate(p = presvals_test_sp_18, a = absvals_sp_18, model = model_sp_18)
print(mod_eval_test_sp_18)  # training AUC may be higher than testing AUC

# calculate thresholds of models
thd1_sp_18 <- threshold(mod_eval_train_sp_18, "spec_sens")  # highest TSS

# plotting points that are above the previously calculated
# thresholded value
plot(cal_pred_sp_18 >= thd1_sp_18, main="Predicted threshold extents of Wildfires [in Spring 2018]")

#map of probability 
tm_shape(cal_pred_sp_18)+
  tm_raster(title = "Predicted probability", palette = '-RdYlGn', style ='cont', breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0))+
  tm_layout(main.title = "Spring 2018", main.title.position = c(0.2, 0.7), title.size=3, legend.text.size = 1.1, 
            legend.position = c(0.65, 0.55), legend.height= -0.3, legend.title.size = 1.1, frame='white')+
  tm_scale_bar(position=c(0.02, 0.02), text.size = 1, breaks = c(0, 100, 200, 300))+
  tm_compass(north = 0,type = 'arrow', position = c('right', 'top'), text.size = 0.9)

#make raster for above threshold
ab_thd1_sp_18 <- cal_pred_sp_18 >= thd1_sp_18
# map of threshold 
tm_shape(ab_thd1_sp_18)+
  tm_raster(style = "cat", title = "Threshold", palette= c("grey", "red"), labels = c("Below", "Above"))+
  tm_layout(main.title = "Spring 2018", main.title.position = c(0.2, 0.7), title.size=3, legend.text.size = 1.1, 
            legend.position = c(0.65, 0.55), legend.height= -0.2, legend.title.size = 1.2, frame='white')+
  tm_scale_bar(position=c(0.02, 0.02), text.size = 1, breaks = c(0, 100, 200, 300))+
  tm_compass(north = 0,type = 'arrow', position = c('right', 'top'), text.size = 0.9)+tm_shape(cal_county) + tm_polygons(alpha = 0, border.col = "black") 

# see threshold 
thd1_sp_18
# prep threshold total map 
cal_th_sp_18_cl <- c(cal_pred_sp_18@data@min-1, 0.457, 0, 0.457, cal_pred_sp_18@data@max+1, 1)
cal_th_sp_18_mat <- matrix(cal_th_sp_18_cl, ncol = 3, byrow = TRUE)
cal_th_sp_18_mat
cal_th_sp_18_recl <- reclassify(cal_pred_sp_18, cal_th_sp_18_mat)

tm_shape(cal_th_sp_18_recl) + tm_raster(style = "cat", title = "Threshold", palette= c("grey", "red"), labels = c("Above threshold", "Below Threshold")) +
  tm_shape(cal) + tm_polygons(alpha = 0, border.col = "black") +
  tm_layout(frame = FALSE, legend.outside = TRUE)

# prepare data for maxent and logistic regression modelling
# Summer 2018
occ_cal_fire_su_18 <- cal_fire_su_18[,-c(3:11)]

# get the same random sample for training and testing
set.seed(20000430)

#Use K fold to split into test and train
group <- kfold(occ_cal_fire_su_18, 5)
occ_train_su_18 <- occ_cal_fire_su_18[group != 1, ]
occ_test_su_18 <- occ_cal_fire_su_18[group == 1, ]
coordinates(occ_train_su_18)= ~ longitude+latitude
coordinates(occ_test_su_18)= ~ longitude+latitude

# extracting env conditions for training occ from the raster
# stack; a data frame is returned (i.e multiple columns)
presvals_su_18 <- raster::extract(cal_su_18, occ_train_su_18)
# env conditions for testing occ
presvals_test_su_18 <- raster::extract(cal_su_18, occ_test_su_18)

# create background points twice the size of train data
set.seed(20000430)
bg_su_18 <- as.data.frame(randomPoints(cal_su_18, 2*length(occ_train_su_18)))
coordinates(bg_su_18)= ~ x+y
# extracting env conditions for background
absvals_su_18 <- raster::extract(cal_su_18, bg_su_18)
# create vector for 1s and 0s for presvals and absvals respectively
pres_abs_su_18 <- c(rep(1, nrow(presvals_su_18)), rep(0, nrow(absvals_su_18)))
# for maxent analysis
pder_final_su_18 <- as.data.frame(rbind(presvals_su_18, absvals_su_18))
# for logistic regression analysis
logistic_final_su_18 <- as.data.frame(cbind(pres_abs_su_18, pder_final_su_18))
colnames(logistic_final_su_18)[1] <- "wildfire"

# logistic regression model 
log.model_su_18 <- glm(wildfire ~ maxtemp + precip + dryness + ndvi + elevation + svi, family = binomial(link = "logit"), data = logistic_final_su_18)
summary(log.model_su_18)           # look here for p-values
exp(coefficients(log.model_su_18)) # converts to odds ratios

#McFadden's pseudo-R2
pR2(log.model_su_18)

# maxent analysis
# train Maxent with tabular data
model_su_18 <- maxent(x=pder_final_su_18, ## env conditions
                      p=pres_abs_su_18,   ## 1:presence or 0:absence
                      path=paste0("/Users/lucytaylor/Library/Mobile Documents/com~apple~CloudDocs/Masters/Dissertation/dissertation/diss"), ## folder for maxent output; 
                      # put in file
                      args=c("responsecurves") ## parameter specification
)

# see output in internet browser 
model_su_18

# perform spatial prediction using training output from maxent model
cal_pred_su_18 <- predict(model_su_18, cal_su_18)
plot(cal_pred_su_18, main="Predicted Probability (in Summer 2018)")

### model evaluation 
# using 'training data' to evaluate p & a are dataframe/s
# (the p and a are the training presence and background
# points)
mod_eval_train_su_18 <- dismo::evaluate(p = presvals_su_18 , a = absvals_su_18, model = model_su_18)
print(mod_eval_train_su_18)

mod_eval_test_su_18 <- dismo::evaluate(p = presvals_test_su_18, a = absvals_su_18, model = model_su_18)
print(mod_eval_test_su_18)  # training AUC may be higher than testing AUC

# calculate thresholds of models
thd1_su_18 <- threshold(mod_eval_train_su_18, "spec_sens")  # highest TSS

# plotting points that are above the previously calculated
# thresholded value
plot(cal_pred_su_18 >= thd1_su_18, main="Predicted threshold extents of Wildfires [in Summer 2018]")

#map of probability 
tm_shape(cal_pred_su_18)+
  tm_raster(title = "Predicted probability", palette = '-RdYlGn', style ='cont', breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0))+
  tm_layout(main.title = "Summer 2018", main.title.position = c(0.2, 0.7), title.size=3, legend.text.size = 1.1, 
            legend.position = c(0.65, 0.55), legend.height= -0.3, legend.title.size = 1.1, frame='white')+
  tm_scale_bar(position=c(0.02, 0.02), text.size = 1, breaks = c(0, 100, 200, 300))+
  tm_compass(north = 0,type = 'arrow', position = c('right', 'top'), text.size = 0.9)

#make raster for above threshold
ab_thd1_su_18 <- cal_pred_su_18 >= thd1_su_18
# map of threshold 
tm_shape(ab_thd1_su_18)+
  tm_raster(style = "cat", title = "Threshold", palette= c("grey", "red"), labels = c("Below", "Above"))+
  tm_layout(main.title = "Summer 2018", main.title.position = c(0.2, 0.7), title.size=3, legend.text.size = 1.1, 
            legend.position = c(0.65, 0.55), legend.height= -0.2, legend.title.size = 1.2, frame='white')+
  tm_scale_bar(position=c(0.02, 0.02), text.size = 1, breaks = c(0, 100, 200, 300))+
  tm_compass(north = 0,type = 'arrow', position = c('right', 'top'), text.size = 0.9)+tm_shape(cal_county) + tm_polygons(alpha = 0, border.col = "black") 

# see threshold 
thd1_su_18
# prep threshold total map 
cal_th_su_18_cl <- c(cal_pred_su_18@data@min-1, 0.218, 0, 0.218, cal_pred_su_18@data@max+1, 1)
cal_th_su_18_mat <- matrix(cal_th_su_18_cl, ncol = 3, byrow = TRUE)
cal_th_su_18_mat
cal_th_su_18_recl <- reclassify(cal_pred_su_18, cal_th_su_18_mat)

tm_shape(cal_th_su_18_recl) + tm_raster(style = "cat", title = "Threshold", palette= c("grey", "red"), labels = c("Above threshold", "Below Threshold")) +
  tm_shape(cal) + tm_polygons(alpha = 0, border.col = "black") +
  tm_layout(frame = FALSE, legend.outside = TRUE)

# prepare data for maxent and logistic regression modelling
# Autumnn 2018
occ_cal_fire_au_18 <- cal_fire_au_18[,-c(3:11)]

# get the same random sample for training and testing
set.seed(20000430)

#Use K fold to split into test and train
group <- kfold(occ_cal_fire_au_18, 5)
occ_train_au_18 <- occ_cal_fire_au_18[group != 1, ]
occ_test_au_18 <- occ_cal_fire_au_18[group == 1, ]
coordinates(occ_train_au_18)= ~ longitude+latitude
coordinates(occ_test_au_18)= ~ longitude+latitude

# extracting env conditions for training occ from the raster
# stack; a data frame is returned (i.e multiple columns)
presvals_au_18 <- raster::extract(cal_au_18, occ_train_au_18)
# env conditions for testing occ
presvals_test_au_18 <- raster::extract(cal_au_18, occ_test_au_18)

# create background points twice the size of train data
set.seed(20000430)
bg_au_18 <- as.data.frame(randomPoints(cal_au_18, 2*length(occ_train_au_18)))
coordinates(bg_au_18)= ~ x+y
# extracting env conditions for background
absvals_au_18 <- raster::extract(cal_au_18, bg_au_18)
# create vector for 1s and 0s for presvals and absvals respectively
pres_abs_au_18 <- c(rep(1, nrow(presvals_au_18)), rep(0, nrow(absvals_au_18)))
# for maxent analysis
pder_final_au_18 <- as.data.frame(rbind(presvals_au_18, absvals_au_18))
# for logistic regression analysis
logistic_final_au_18 <- as.data.frame(cbind(pres_abs_au_18, pder_final_au_18))
colnames(logistic_final_au_18)[1] <- "wildfire"

# logistic regression model 
log.model_au_18 <- glm(wildfire ~ maxtemp + precip + dryness + ndvi + elevation + svi, family = binomial(link = "logit"), data = logistic_final_au_18)
summary(log.model_au_18)           # look here for p-values 
exp(coefficients(log.model_au_18)) # converts to odds ratios 

#McFadden's pseudo-R2
pR2(log.model_au_18)

# maxent analysis
# train Maxent with tabular data
model_au_18 <- maxent(x=pder_final_au_18, ## env conditions
                      p=pres_abs_au_18,   ## 1:presence or 0:absence
                      path=paste0("/Users/lucytaylor/Library/Mobile Documents/com~apple~CloudDocs/Masters/Dissertation/dissertation/diss"), ## folder for maxent output; 
                      # put in a file, 
                      args=c("responsecurves") ## parameter specification
)

# see output in internet browser 
model_au_18

# perform spatial prediction using training output from maxent model
cal_pred_au_18 <- predict(model_au_18, cal_au_18)
plot(cal_pred_au_18, main="Predicted Probability (in Autumn 2018)")

### model evaluation for table 3
# using 'training data' to evaluate p & a are dataframe/s
# (the p and a are the training presence and background
# points)
mod_eval_train_au_18 <- dismo::evaluate(p = presvals_au_18 , a = absvals_au_18, model = model_au_18)
print(mod_eval_train_au_18)

mod_eval_test_au_18 <- dismo::evaluate(p = presvals_test_au_18, a = absvals_au_18, model = model_au_18)
print(mod_eval_test_au_18)  # training AUC may be higher than testing AUC

# calculate thresholds of models
thd1_au_18 <- threshold(mod_eval_train_au_18, "spec_sens")  # highest TSS
thd1_au_18

?jackknife()
# plotting points that are above the previously calculated
# thresholded value
plot(cal_pred_au_18 >= thd1_au_18, main="Predicted threshold extents of Wildfires [in Autumn 2018]")

#map of probability 
tm_shape(cal_pred_au_18)+
  tm_raster(title = "Predicted probability", palette = '-RdYlGn', style ='cont', breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0))+
  tm_layout(main.title = "Autumn 2018", main.title.position = c(0.2, 0.7), title.size=3, legend.text.size = 1.1, 
            legend.position = c(0.65, 0.55), legend.height= -0.3, legend.title.size = 1.1, frame='white')+
  tm_scale_bar(position=c(0.02, 0.02), text.size = 1, breaks = c(0, 100, 200, 300))+
  tm_compass(north = 0,type = 'arrow', position = c('right', 'top'), text.size = 0.9)

#make raster for above threshold
ab_thd1_au_18 <- cal_pred_au_18 >= thd1_au_18
# map of threshold 
tm_shape(ab_thd1_au_18)+
  tm_raster(style = "cat", title = "Threshold", palette= c("grey", "red"), labels = c("Below", "Above"))+
  tm_layout(main.title = "Autumn 2018", main.title.position = c(0.2, 0.7), title.size=3, legend.text.size = 1.1, 
            legend.position = c(0.65, 0.55), legend.height= -0.2, legend.title.size = 1.2, frame='white')+
  tm_scale_bar(position=c(0.02, 0.02), text.size = 1, breaks = c(0, 100, 200, 300))+
  tm_compass(north = 0,type = 'arrow', position = c('right', 'top'), text.size = 0.9)+tm_shape(cal_county) + tm_polygons(alpha = 0, border.col = "black") 

# see threshold 
thd1_au_18
# prep threshold total map 
cal_th_au_18_cl <- c(cal_pred_au_18@data@min-1, 0.402, 0, 0.402, cal_pred_au_18@data@max+1, 1)
cal_th_au_18_mat <- matrix(cal_th_au_18_cl, ncol = 3, byrow = TRUE)
cal_th_au_18_mat
cal_th_au_18_recl <- reclassify(cal_pred_au_18, cal_th_au_18_mat)

tm_shape(cal_th_au_18_recl) + tm_raster(style = "cat", title = "Threshold", palette= c("grey", "red"), labels = c("Below threshold", "Above Threshold")) +
  tm_shape(cal) + tm_polygons(alpha = 0, border.col = "black") +
  tm_layout(frame = FALSE, legend.outside = TRUE)

#Calculate areas above the threshold for all of 2016
Ab_Threshold_16_Binary <- cal_th_wi_15_16_recl*cal_th_sp_16_recl*cal_th_su_16_recl*cal_th_au_16_recl

#plot areas
tm_shape(Ab_Threshold_16_Binary) + tm_raster(style = "cat", title = "Threshold", palette=c("grey", "red"), labels=c("Below", "Above"))+
  tm_layout(main.title = "2016", main.title.position = c(0.2, 0.7), title.size=3, legend.text.size = 1.1, 
           legend.position = c(0.65, 0.55), legend.height= -0.2, legend.title.size = 1.2, frame='white')+
  tm_scale_bar(position=c(0.02, 0.02), text.size = 1, breaks = c(0, 100, 200, 300))+
  tm_compass(north = 0,type = 'arrow', position = c('right', 'top'), text.size = 0.9)+tm_shape(cal_county) + tm_polygons(alpha = 0, border.col = "black") 

#Calculate areas above the threshold for all of 2017
Ab_Threshold_17_Binary <- cal_th_wi_16_17_recl*cal_th_sp_17_recl*cal_th_su_17_recl*cal_th_au_17_recl

#plot areas
tm_shape(Ab_Threshold_17_Binary) + tm_raster(style = "cat", title = "Threshold", palette=c("grey", "red"), labels=c("Below", "Above"))+
  tm_layout(main.title = "2017", main.title.position = c(0.2, 0.7), title.size=3, legend.text.size = 1.1, 
            legend.position = c(0.65, 0.55), legend.height= -0.2, legend.title.size = 1.2, frame='white')+
  tm_scale_bar(position=c(0.02, 0.02), text.size = 1, breaks = c(0, 100, 200, 300))+
  tm_compass(north = 0,type = 'arrow', position = c('right', 'top'), text.size = 0.9)+tm_shape(cal_county) + tm_polygons(alpha = 0, border.col = "black") 

#Calculate areas above the threshold for all of 2018
Ab_Threshold_18_Binary <- cal_th_wi_17_18_recl*cal_th_sp_18_recl*cal_th_su_18_recl*cal_th_au_18_recl

#plot areas
tm_shape(Ab_Threshold_18_Binary) + tm_raster(style = "cat", title = "Threshold", palette=c("grey", "red"), labels=c("Below", "Above"))+
  tm_layout(main.title = "2018", main.title.position = c(0.2, 0.7), title.size=3, legend.text.size = 1.1, 
            legend.position = c(0.65, 0.55), legend.height= -0.2, legend.title.size = 1.2, frame='white')+
  tm_scale_bar(position=c(0.02, 0.02), text.size = 1, breaks = c(0, 100, 200, 300))+
  tm_compass(north = 0,type = 'arrow', position = c('right', 'top'), text.size = 0.9)+tm_shape(cal_county) + tm_polygons(alpha = 0, border.col = "black") 

#Calculate areas above the threshold for all years
Ab_Threshold_all_Binary <- cal_th_wi_15_16_recl*cal_th_sp_16_recl*cal_th_su_16_recl*cal_th_au_16_recl*cal_th_wi_16_17_recl*cal_th_sp_17_recl*cal_th_su_17_recl*cal_th_au_17_recl*cal_th_wi_17_18_recl*cal_th_sp_18_recl*cal_th_su_18_recl*cal_th_au_18_recl

#plot areas #shows no areas left
tm_shape(Ab_Threshold_all_Binary) + tm_raster(style = "cat", title = "", palette=c("grey", "red"), labels=c("Above Threshold", "Below Threshold")) +
  tm_shape(cal) + tm_polygons(alpha = 0, border.col = "black") +
  tm_layout(frame = FALSE, legend.outside = TRUE)

#Autumn change of PP from 2016 to 2018
au_dif <- cal_pred_au_18 - cal_pred_au_16

#Plot the change with counties
tm_shape(au_dif)+
  tm_raster(title = "PP change", palette = '-RdYlGn', style ='cont', breaks = c(-1.0, -0.5, 0, 0.5, 1.0))+
  tm_layout(main.title = "Autumn", main.title.position = c(0.2, 0.7), title.size=3, legend.text.size = 1.1, 
            legend.position = c(0.65, 0.55), legend.height= -0.3, legend.title.size = 1.1, frame='white')+
  tm_scale_bar(position=c(0.02, 0.02), text.size = 1, breaks = c(0, 100, 200, 300))+
  tm_compass(north = 0,type = 'arrow', position = c('right', 'top'), text.size = 0.9)+tm_shape(cal_county) + tm_polygons(alpha = 0, border.col = "black") 

#Summer change of PP from 2016 to 2018
su_dif <- cal_pred_su_18 - cal_pred_su_16

#Plot the change with counties
tm_shape(su_dif)+
  tm_raster(title = "PP change", palette = '-RdYlGn', style ='cont', breaks = c(-1.0, -0.5, 0, 0.5, 1.0))+
  tm_layout(main.title = "Summer", main.title.position = c(0.2, 0.7), title.size=3, legend.text.size = 1.1, 
            legend.position = c(0.65, 0.55), legend.height= -0.3, legend.title.size = 1.1, frame='white')+
  tm_scale_bar(position=c(0.02, 0.02), text.size = 1, breaks = c(0, 100, 200, 300))+
  tm_compass(north = 0,type = 'arrow', position = c('right', 'top'), text.size = 0.9)+tm_shape(cal_county) + tm_polygons(alpha = 0, border.col = "black") 

#Spring change of PP from 2016 to 2018
sp_dif <- cal_pred_sp_18 - cal_pred_sp_16

#Plot the change with counties
tm_shape(sp_dif)+
  tm_raster(title = "PP change", palette = '-RdYlGn', style ='cont', breaks = c(-1.0, -0.5, 0, 0.5, 1.0))+
  tm_layout(main.title = "Spring", main.title.position = c(0.2, 0.7), title.size=3, legend.text.size = 1.1, 
            legend.position = c(0.65, 0.55), legend.height= -0.3, legend.title.size = 1.1, frame='white')+
  tm_scale_bar(position=c(0.02, 0.02), text.size = 1, breaks = c(0, 100, 200, 300))+
  tm_compass(north = 0,type = 'arrow', position = c('right', 'top'), text.size = 0.9)+tm_shape(cal_county) + tm_polygons(alpha = 0, border.col = "black") 

#Winter change of PP from 2016 to 2018
win_dif <- cal_pred_wi_17_18 - cal_pred_15_16

#Plot the change with counties
tm_shape(win_dif)+
  tm_raster(title = "PP change", palette = '-RdYlGn', style ='cont', breaks = c(-1.0, -0.5, 0, 0.5, 1.0))+
  tm_layout(main.title = "Winter", main.title.position = c(0.2, 0.7), title.size=3, legend.text.size = 1.1, 
            legend.position = c(0.65, 0.55), legend.height= -0.3, legend.title.size = 1.1, frame='white')+
  tm_scale_bar(position=c(0.02, 0.02), text.size = 1, breaks = c(0, 100, 200, 300))+
  tm_compass(north = 0,type = 'arrow', position = c('right', 'top'), text.size = 0.9)+tm_shape(cal_county) + tm_polygons(alpha = 0, border.col = "black") 

