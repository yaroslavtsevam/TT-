########################## NDVI verification ##################################

#ALways better to do on your own
library(sf)
library(raster)
library(rgdal)
library(rgeos)
library(RColorBrewer)

TTcanopies = read_sf("TT_canopies.geojson")
TTcanopies = TTcanopies[-7:-1,]

RUDN <- stack("Y:\\YandexDisk\\NDVI\\SATELITE_DATA\\011169772030_01\\011169772030_01_P001_MUL\\19JUN04090505-M2AS-011169772030_01_P001.TIF")
RUDN <- brick(RUDN)

TIM <- stack("Y:\\YandexDisk\\NDVI\\SATELITE_DATA\\011169772050_01\\011169772050_01_P001_MUL\\19JUN04090502-M2AS-011169772050_01_P001.TIF")
TIM <- brick(TIM)

CENTER = stack("Y:\\YandexDisk\\NDVI\\SATELITE_DATA\\PANSHARP\\SCHOOL+BOLOTNAYA+ZARYADYE-04-06-19.tif")
CNETER = brick(CENTER)

refl_NIR = function(nir){
  Lnir = nir*0.982*0.117971/1.004-3.752
  JD = 2458638.87853
  D = JD - 2451545.0
  g  = 357.529 + 0.98560028 * D
  d  = 1.00014 - 0.01671 * cos(g/180*pi) - 0.00014 * cos(2*g/180*pi)
  SunEl = 56.5
  phi = 90 - SunEl
  Enir = 1071.98
  Rnir = Lnir * d^2 * pi / (Enir*cos(phi/180*pi))
  return(Rnir)
} 

refl_RED = function(red){
  
  Lred = red*0.945*1.02/5.85-1.350
  JD = 2458638.87853
  D = JD - 2451545.0
  g  = 357.529 + 0.98560028 * D
  d  = 1.00014 - 0.01671 * cos(g/180*pi) - 0.00014 * cos(2*g/180*pi)
  SunEl = 56.5
  phi = 90 - SunEl
  Ered = 1555.11
  Rred = Lred * d^2 * pi / (Ered*cos(phi/180*pi))
  return(Rred)
} 

refl_NDVI = function(raster,nir_band,red_band){
  red = refl_RED(raster[[red_band]])
  nir = refl_NIR(raster[[nir_band]])
  ndvi = (nir-red)/(nir+red)
  return(ndvi)
}
center_ndvi = refl_NDVI(CENTER,4,3)  
rudn_ndvi = refl_NDVI(RUDN,4,3)  
tim_ndvi = refl_NDVI(TIM,4,3) 
timrudn_ndvi = raster::merge(rudn_ndvi,tim_ndvi)

writeRaster(rudn_ndvi, filename=file.path(getwd(), "rudn_ndvi.tif"), format="GTiff", overwrite=TRUE)
writeRaster(tim_ndvi, filename=file.path(getwd(), "tim_ndvi.tif"), format="GTiff", overwrite=TRUE)
writeRaster(center_ndvi, filename=file.path(getwd(), "center_ndvi.tif"), format="GTiff", overwrite=TRUE)
q85 = function(x){
  return(quantile(x,0.85))
}

TTndvi = TTcanopies %>% mutate(
  NDVIMean = raster::extract(timrudn_ndvi, TTcanopies, fun = mean, na.rm = TRUE),
  NDVIMax = raster::extract(timrudn_ndvi, TTcanopies, fun = max, na.rm = TRUE),
  NDVIMin = raster::extract(timrudn_ndvi, TTcanopies, fun = min, na.rm = TRUE),
) %>% as.data.frame() %>% dplyr::select(-geometry)




NDVI_TT_score = AllData %>% filter(doy == 155, id %in% TTndvi$id) %>% left_join(TTndvi , by="id")%>%
  filter(!is.na(NDVI))%>%
  mutate(av_score = abs(NDVI - NDVIMean), 
         min_score = abs(NDVI - NDVIMin),
         max_score=abs(NDVI-NDVIMax),
         hour_score1=abs(hour-12),
         hour_score2=abs(hour-11),
         hour_score3=abs(hour-6))%>%
  group_by(id)%>%summarise( hour_av = hour[which.min(av_score)],
                            hour_min = hour[which.min(min_score)],
                            hour_max = hour[which.min(max_score)], 
                            NDVI_tt_av = NDVI[which.min(av_score)],
                            NDVI_tt_min = NDVI[which.min(min_score)],
                            NDVI_tt_max = NDVI[which.min(max_score)],
                            NDVI_time = NDVI[which.min(hour_score1)],
                            NDVI_time2 = NDVI[which.min(hour_score2)],
                            NDVI_time3 = NDVI[which.min(hour_score3)],
                            NDVI_av = NDVIMean[which.min(av_score)],
                            NDVI_min = NDVIMin[which.min(min_score)],
                            NDVI_max = NDVIMax[which.min(max_score)],
                            NDVIMean = mean(NDVIMean))

NDVI_TT_score %>% as.data.frame()


NDVI_TT = AllData %>% filter(doy == 155, id %in% TTndvi$id%>%unique) %>%filter(!is.na(NDVI))%>%group_by(id)%>%
  summarise(mean = mean(NDVI, na.rm=T), 
            min = min(NDVI, na.rm = T),
            max = max(NDVI, na.rm = T))


TT = left_join(NDVI_TT,TTndvi, by="id")%>%as.data.frame()
plot(x=TT$max,y = TT$NDVIMax)

ggplot(data = NDVI_TT_score)+
  geom_point(aes(x=NDVI_tt_av, y=NDVI_av),color=1)+
  geom_point(aes(x=NDVI_tt_min, y=NDVI_min),color=2)+
  geom_point(aes(x=NDVI_tt_max, y=NDVI_max),color=3)


ggplot(data = NDVI_TT_score)+
  geom_smooth(aes(x=NDVI_tt_min, y=NDVI_min), method = lm, color=3)+
  geom_point(aes(x=NDVI_tt_min, y=NDVI_min),color=3)+
  xlab("NDVI по данными TreeTalker")+
  ylab("NDVI по данными спутника WorldView 3")+
  theme_bw()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"))





mean_mod = lm(data = NDVI_TT_score, NDVI_tt_av~NDVI_av) %>% summary
min_mod  = lm(data = NDVI_TT_score, NDVI_tt_min~NDVI_min)  %>% summary
max_mod  = lm(data = NDVI_TT_score, NDVI_tt_max~NDVI_max) %>% summary
mod  = lm(data = TT%>%filter (max!=1), max~NDVIMax) %>% summary

