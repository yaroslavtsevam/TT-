library("solaR")
library("sirad")




# Angstrom-Prescott solar radiation model - ap
#
# ap(days, lat, lon, extraT=NULL, A=NA, B=NA, SSD)
#
# days - 	Vector of class 'Date' of length n.
# lat -  Latitude in decimal degrees.
# lon -  Longitude in decimal degrees.
# extraT -  Optional. Vector of length n of extraterrestrial solar radiation [MJm-2]. If 'NULL' then it is calculated by the function. 
# Providing extraterrestrial solar radiation speeds up the computation
# A	 - Angstrom-Prescott model 'A' coefficient. If 'NA' then A is derived from the map of interpolated coefficients estimated from Meteosat 
# solar radiation data. See details.
# B  - Angstrom-Prescott model 'B' coefficient. If 'NA' then B is derived from the map of interpolated coefficients estimated from Meteosat 
# solar radiation data. See details.
# SSD	- Vector of length n containing sunshine duration [in hours].

require(zoo)
data(Metdata)
A <- 0.21
B <- 0.57
sunshine <- Metdata$meteo$SUNSHINE
days <- Metdata$meteo$DAY
lat <- Metdata$LATITUDE
lon <- Metdata$LONGITUDE
plot(zoo(ap(days=days,lat=lat,lon=lon,extraT=NULL, A=A,B=B,SSD=sunshine),order.by=days))




# Supit-Van Kappel solar radiation model calculates daily solar radiation based on daily cloud coverage and temperature range using Supit-Van Kappel model.
#
# su(days, lat, lon, extraT=NULL, A=NA, B=NA, C=NA, tmax, tmin, CC)
# days - Vector of class 'Date' of length n.
# lat	- Latitude in decimal degrees.
# lon	- Longitude in decimal degrees.
# extraT	-  Optional. Vector of length n of extraterrestrial solar radiation [MJm-2]. If 'NULL' then it is calculated by the function. 
# A - Supit-Van Kappel model coefficient 'A'. If 'NA' then A is derived from the map of interpolated coefficients estimated from Meteosat solar radiation data. 
# B - Supit-Van Kappel model coefficient 'B'. If 'NA' then B is derived from the map of interpolated coefficients estimated from Meteosat solar radiation data. 
# C - Supit-Van Kappel model coefficient 'C'. If 'NA' then C is derived from the map of interpolated coefficients estimated from Meteosat solar radiation data. 
# tmax	- Vector of length n containing daily maximum temperature [C].
# tmin	-Vector of length n containing daily minumum temperature [C].
# CC	 Vector of length n containing daily cloud coverage [octas].


data(Metdata)
tmax <- Metdata$meteo$TEMP_MAX
tmin <- Metdata$meteo$TEMP_MIN
cc <- Metdata$meteo$CLOUD_DAYTIME_TOTAL


dat = data %>% group_by(doy) %>% filter(Species=="TTR") %>% summarise(time = mean(time), tmax=max(tair), tmin=min(tair),rh=mean(rh, na.rm=T), 
                                                                      sunshine = sum((TTR_500c+TTR_450c)/(TTR_760c+TTR_730c)>2, na.rm = T)*1.5,
                                                                      vap_pres = mean(0.61094*exp(17.625*tair/(tair+243.04)),na.rm=T),
                                                                      lightness = sum(TTR_500c+TTR_450c+TTR_760c+TTR_730c+TTR_860c+TTR_810c+TTR_680c+
                                                                                        TTR_650c+TTR_610c+TTR_600c+TTR_570c+TTR_550c, na.rm = T))
dat = dat[1:121,]
days = dat[[2]] %>%  as.character() %>% str_trunc(10, "right", ellipsis = "")
tmax = dat$tmax
tmin = dat$tmin
vap_pres = dat$vap_pres
cc = dat$sunshine
lat <- 55.745199
lon <- 37.617533
height_sealev = 122
wind = rep(2,121)
Jpred = su(days=days, lat=lat, lon=lon, extraT=NULL, A=NA, B=NA, 
              C=NA, tmax=tmax, tmin=tmin, CC=cc)
tal <- cst(Jpred ,dayOfYear(days),radians(lat)) 

#'et0' estimates evapotranspiration based on FAO Penman-Monteith equation [mmd-1]

eT = et0(Tmax=tmax,Tmin=tmin, vap_pres=vap_pres,sol_rad=Jpred,tal=tal,z=height_sealev,
    uz=wind,meah=10,extraT=NA,days=days,lat=lat)

dat$eT = eT
dat$J = Jpred



plot(dat$time, dat$J)
plot(dat$time, dat$lightness)
plot(dat$time, dat$eT)

ggplotly(qplot(data$time, (data$TTR_500c+data$TTR_450c)/(data$TTR_760c+data$TTR_730c), ylim=c(0,50) ))

