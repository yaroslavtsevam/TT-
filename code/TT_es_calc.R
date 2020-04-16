source("code/TT_Moscow_data_collection.R")
library(pracma)
# for wind interpolation
library(zoo)

#Loading weather data 
AllData = BLTNdata[[2]]
Moscow_center_weather_19 <- read_delim("data/Moscow_center_weather_19.csv", 
                                       ";", escape_double = FALSE, col_types = cols(`Местное время в Москве (центр, Балчуг)` = col_datetime(format = "%d.%m.%Y %H:%M"), 
                                      Pa = col_double()), comment = "#", trim_ws = TRUE)
#Calculating wind speed at height 20m
MCW19 = Moscow_center_weather_19 %>% mutate(ff20 = Ff*log(20/.6)/log(10/.6))
MCW19 = MCW19 %>% select(T, Ff,ff20, RRR)
MCW19$time = Moscow_center_weather_19[[1]]
#Gap fill data
BLTN  = AllData %>% filter(Site=="BOLOTNAYA")
Bdata = BLTN %>% left_join(MCW19, by ="time")
Bdata$ff20 = c(na.approx(Bdata$ff20, x = index(Bdata$ff20),  na.rm = TRUE, maxgap = Inf), NA,NA)
Bdata$Ff = c(na.approx(Bdata$Ff, x = index(Bdata$Ff),  na.rm = TRUE, maxgap = Inf), NA, NA)
#Ricardo's version of aerodynamic resistance
Bdata = Bdata %>% mutate(r2 =log((20-20*0.67)/(20*.15))^2/(ff20*0.41^2))
#Aerodynamic resistance according to Tom and Eddy Pro
Bdata = Bdata %>% mutate(r1 =log(20-20*0.8)/(20*.1*ff20*0.41^2))
#	C = ρCp ∆T/r
Bdata = Bdata %>% mutate(C1 = 1.006*1.202*(TTair - nt1)/r1 )
Bdata = Bdata %>% mutate(C2 = 1.006*1.202*(TTair - nt1)/r2 )
#lambda_T = Flux * 2264.705 KJ/KG
Bdata$Flux[Bdata$Flux >100] = NA
Bdata = Bdata %>% mutate(lT = Flux * 2264.705/3600 )
Bdata = Bdata %>% mutate(E1 = (lT+C1)/.9 )
Bdata = Bdata %>% mutate(E2 = (lT+C2)/.9 )
Bdata = Bdata %>% mutate(G1 = 0.1*E1 )
Bdata = Bdata %>% mutate(G2 = 0.1*E2 )




names(dis_pred_table) = c("doy","dist_pred","id", "growth")
AllData = left_join(AllData, dis_pred_table, by=c("id","doy"))

AllData$growth[is.na(AllData$growth)] = 0
AllData$growth[is.infinite(AllData$growth)] = 0
AllData  = AllData %>% BEFadd( verboseFlag =  "con")
AllData = AllData %>% left_join(read_delim("data/Bolotnaya_growth.csv", delim=";"),by="id") 
AllData = AllData %>% mutate(biomas_stored = pi*tree_height*1/3*(growth/1000)*(d/100+growth/1000) * C_V*1000)

TTR  = AllData %>% filter(!is.na(TTair))







TTR = TTR %>% mutate( dTair = tair - TTair)

TTR = TTR %>% filter(Site %in% c("BOLOTNAYA","TROITSK"))
TTR = TTR %>% filter(Species != "TTR")
TTR$LAIb[is.infinite(TTR$LAIb)] = NA
# T dif

Tdif = TTR %>% group_by(Site, Species, doy)%>% summarise(dTairmax = max(dTair, na.rm = T), dTairmin = min(dTair,na.rm = T), dTairmean = mean(dTair, na.rm=T))
LAI = TTR %>% group_by(Site, Species, doy)%>% summarise(LAImax = max(LAIb, na.rm = T), LAImin = min(LAIb,na.rm = T), LAImean = mean(LAIb, na.rm=T))





ggplot(data = Tdif)+
  geom_point(aes(x=doy, y = dTairmax, color = Species ))+
  geom_line(aes(x=doy, y = dTairmax, color = Species ))+
  facet_wrap(~Site,nrow = 2)+
  theme_bw()


ggplot(data = Tdif %>% filter(Species %in% c("Tilia cordata","Betula pendula")))+
  geom_point(aes(x=doy, y = dTairmax, color = Site ))+
  geom_line(aes(x=doy, y = dTairmax, color = Site ))+
  facet_wrap(~Species,nrow = 3)+
  theme_bw()



ggplot(data = Tdif)+
  geom_point(aes(x=doy, y = dTairmin, color = Species ))+
  geom_line(aes(x=doy, y = dTairmin, color = Species ))+
  facet_wrap(~Site,nrow = 2)+
  theme_bw()


ggplot(data = Tdif %>% filter(Species %in% c("Tilia cordata","Betula pendula")))+
  geom_point(aes(x=doy, y = dTairmin, color = Site ))+
  geom_line(aes(x=doy, y = dTairmin, color = Site ))+
  facet_wrap(~Species,nrow = 3)+
  theme_bw()



ggplot(data = Tdif)+
  geom_point(aes(x=doy, y = dTairmean, color = Species ))+
  geom_line(aes(x=doy, y = dTairmean, color = Species ))+
  facet_wrap(~Site,nrow = 2)+
  theme_bw()


ggplot(data = Tdif %>% filter(Species %in% c("Tilia cordata","Betula pendula")))+
  geom_point(aes(x=doy, y = dTairmean, color = Site ))+
  geom_line(aes(x=doy, y = dTairmean, color = Site ))+
  facet_wrap(~Species,nrow = 3)+
  theme_bw()


#Energy

Bsum = Bdata %>% group_by(id, Species) %>% filter(Species != "TTR") %>% summarise(KJ = sum(E2, na.rm = T))

ggplot(data = Bsum)+
  geom_col(aes( x = id,y = KJ))+
  facet_wrap(~Species, ncol = 2, scale="free" )+
  #scale_y_continuous(limits=c(-.45,.5))+
  theme_bw()



write.csv(Bdata, file="data.csv")





#Growth

Growth  = TTR %>% group_by(Site, Species, id)%>% filter(volt >4)%>% summarise(growth = min(detrend(dist)) - first(detrend(dist)))
rudn_growth = read_delim("data//proximity_RUDN.csv", delim = ";")
rudn_growth  = rudn_growth %>% select(id,ds,de) %>% mutate(rgr = ds-de)
rudn_growth[is.na(rudn_growth)] = 0


AllData = AllData %>% left_join(rudn_growth, by="id")
AllData %>% filter(Site == "RUDN") %>% as.data.frame()
TTR$dist[is.infinite(TTR$dist)] = NA
gr_pred = TTR %>% filter(Site == "BOLOTNAYA",hour >20 | hour <6,  tair >20 , volt>4.05 ) %>% group_by(id, Species) %>%
  summarise(b= lm(dist~doy, na.action = na.exclude)[[1]][2])

AllData %>% filter(Site == "RUDN") %>% group_by(id, Species, VTA_score)%>% 
  summarise(rgr=mean(rgr), DBH = mean(DBH), rgrdb = mean(rgr)/mean(DBH)) %>%
  arrange(Species) %>% as.data.frame()

ggplot(data = AllData %>% filter(Site == "BOLOTNAYA") %>%filter(Species != "TTR")%>% group_by(id, Species)%>% summarise(kg=mean(biomas_stored)))+
  geom_col(aes( x = id,y = kg))+
  facet_wrap(~Species, ncol = 2, scale="free" )+
  #scale_y_continuous(limits=c(-.45,.5))+
  theme_bw()

AllData$rgr

ggplot(data = TTR %>% filter(Site == "BOLOTNAYA"))+
  geom_point(aes(x=doy, y = detrend(dist), color = id ))+
  #geom_line(aes(x=doy, y = dist, color = id ))+
  geom_smooth(aes(x=doy,y=detrend(dist), color = id),method="lm" ,size=.1)+
  facet_wrap(~Species,nrow = 3)+
  scale_y_continuous(limits=c(-1,10))+
  theme_bw()

ggplot(data = Growth %>% filter(Site=="BOLOTNAYA"))+
  geom_col(aes( x = id,y = growth, fill = Species ))+
  #geom_line(aes(x=doy, y = dist, color = id ))+
  #geom_smooth(aes(x=doy,y=detrend(dist), color = id),method="lm" ,size=.1)+
  facet_wrap(~Species,ncol = 2, scale="free" )+
  #scale_y_continuous(limits=c(-.45,.5))+
  theme_bw()
  

  
TTR$dist

#LAI



ggplot(data = LAI)+
  geom_point(aes(x=doy, y = LAImax, color = Species ))+
  geom_line(aes(x=doy, y = LAImax, color = Species ))+
  facet_wrap(~Site,nrow = 2)+
  theme_bw()


ggplot(data = LAI %>% filter(Species %in% c("Tilia cordata","Betula pendula")))+
  geom_point(aes(x=doy, y = LAImax, color = Site ))+
  geom_line(aes(x=doy, y = LAImax, color = Site ))+
  facet_wrap(~Species,nrow = 3)+
  theme_bw()



ggplot(data = LAI)+
  geom_point(aes(x=doy, y = LAImin, color = Species ))+
  geom_line(aes(x=doy, y = LAImin, color = Species ))+
  facet_wrap(~Site,nrow = 2)+
  theme_bw()


ggplot(data = LAI %>% filter(Species %in% c("Tilia cordata","Betula pendula")))+
  geom_point(aes(x=doy, y = LAImin, color = Site ))+
  geom_line(aes(x=doy, y = LAImin, color = Site ))+
  facet_wrap(~Species,nrow = 3)+
  theme_bw()



ggplot(data = LAI)+
  geom_point(aes(x=doy, y = LAImean, color = Species ))+
  geom_line(aes(x=doy, y = LAImean, color = Species ))+
  facet_wrap(~Site,nrow = 2)+
  theme_bw()


ggplot(data = LAI %>% filter(Species %in% c("Tilia cordata","Betula pendula")))+
  geom_point(aes(x=doy, y = LAImean, color = Site ))+
  geom_line(aes(x=doy, y = LAImean, color = Site ))+
  facet_wrap(~Species,nrow = 3)+
  theme_bw()
















TTR%>% group_by(id)%>%summarise(n = n()) %>% as.data.frame()

TT285 =  TTR%>% filter(id == "218A0077")


ggplot(data = TT285)+
  geom_point(aes(x=time, y=LAIb))+
  geom_line(aes(x=time, y=LAIb))

