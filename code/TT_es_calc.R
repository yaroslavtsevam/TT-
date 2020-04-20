source("code/TT_Moscow_data_collection.R")
library(pracma)
# for wind interpolation
library(zoo)
library(tidyquant)

#Loading weather data 
#AllData = BLTNdata[[2]]
BLTN  = AllData %>% filter(Site=="BOLOTNAYA")
BLTN  = BLTN %>% BEFadd( verboseFlag =  "con")
BLTN = BLTN %>% left_join(read_delim("data/Bolotnaya_growth.csv", delim=";"),by="id") 
BLTN = BLTN %>% mutate(biomas_stored = pi*tree_height*1/3*(growth/1000)*(d/100+growth/1000) * C_V*1000)

Moscow_center_weather_19 <- read_delim("data/Moscow_center_weather_19.csv",delim = ";",
                                       escape_double = FALSE, comment = "#", 
                            col_types = cols(Pa = col_double()),  trim_ws = TRUE) %>% 
                            rename(time ="Местное время в Москве (центр, Балчуг)" ) %>%
                            mutate(time = as_datetime(time,format = "%d.%m.%Y %H:%M"))
#Calculating wind speed at height 20m
MCW19 = Moscow_center_weather_19 %>% mutate(ff20 = Ff*log(20/.6)/log(10/.6))
MCW19 = MCW19 %>% select(time,"T",Po, Ff,ff20, RRR) %>% rename("sT" = "T")
#Gap fill data

Bdata = BLTN %>% ungroup() %>% left_join(MCW19, by ="time") %>% as.data.frame()

Bdata$ff20 = c(na.approx(Bdata$ff20, x = index(Bdata$ff20),  na.rm = TRUE, maxgap = Inf), rep(NA,8))
Bdata$Ff = c(na.approx(Bdata$Ff, x = index(Bdata$Ff),  na.rm = TRUE, maxgap = Inf), rep(NA,8))
Bdata$sT = c(na.approx(Bdata$sT, x = index(Bdata$sT),  na.rm = TRUE, maxgap = Inf), rep(NA,8))
#Aerodynamic resistance according to Tom and Eddy Pro
Bdata = Bdata %>% mutate(r =log((20-20*0.67)/(20*.15))^2/(ff20*0.41^2))
Bdata = Bdata %>% mutate(ra =log((20-20*0.67)/(20*0.0123))*log((20-20*0.67)/(20*.123))/(Ff*0.41^2))
#Bdata = Bdata %>% mutate(a_d = air_density(tair,rh, Po, 3.5) )
#	C = ρCp ∆T/r kW/m2
Bdata = Bdata %>% mutate(H = 1.006*1.202*(nt1 - TTair)/(r) )

Bdata$Flux[Bdata$Flux >100] = NA
#lambda_T = Flux * 2264.705 KJ
#/3600 in kWh 
Bdata = Bdata %>% mutate(L = Flux * 2264.705 /3600)
Bdata = Bdata %>% mutate(Rn = (L+H)/.9 )
Bdata = Bdata %>% mutate(G = 0.1*Rn )


  air_density = function(tc, rh, Po, z){
    # Calculate the saturation vapor pressure given the temperature(celsius)
    # Polynomial from Herman Wobus 
     eso=6.1078;
     c0=0.99999683
     c1=-0.90826951E-02
     c2=0.78736169E-04
     c3=-0.61117958E-06
     c4=0.43884187E-08
     c5=-0.29883885E-10
     c6=0.21874425E-12
     c7=-0.17892321E-14
     c8=0.11112018E-16
     c9=-0.30994571E-19
     pol=c0+tc*(c1+tc*(c2+tc*(c3+tc*(c4+tc*(c5+tc*(c6+tc*(c7+tc*(c8+tc*(c9)))))))))
     esmb=eso/pol^8
     # Calculate the vapor pressures (mb) given the ambient temperature (c) and dewpoint (c) 
     emb=esmb*rh/100;
     # Calculate the actual pressure (mb)from the altimeter setting (mb) and geopotential altitude (m)
     r  = 6369E3
     k1 = 0.190263;
     k2 = 8.417286E-5;
     # Convertin pressure quicksulver mm to Bar  
     p = ((Po*1.33322^k1)-(k2*((r*z)/(r+z))))^(1/k1)
     # Calculate the air density (kg/m3) from actual pressure (mb) vapor pressure (mb) and temp (c)
     Rv = 461.4964
     Rd = 287.0531
     tk = tc + 273.15
     pv = emb * 100
     pd = (p - emb)*100
     density = (pv/(Rv*tk)) + (pd/(Rd*tk))
     return(density)
  }
  
Bdata = Bdata %>%  filter(Species != "TTR")
TTR  = AllData %>% filter(!is.na(TTair))







TTR = TTR %>% mutate( dTair = tair - TTair)

TTR = TTR %>% filter(Site %in% c("BOLOTNAYA","TROITSK"))
TTR = TTR %>% filter(Species != "TTR")
TTR$LAIb[is.infinite(TTR$LAIb)] = NA
# T dif


LAI = TTR %>% group_by(Site, Species, doy)%>% summarise(LAImax = max(LAIb, na.rm = T), LAImin = min(LAIb,na.rm = T), LAImean = mean(LAIb, na.rm=T))


#######################################  Temperature #############################################

Tdif = TTR %>% group_by(Site, Species, doy) %>% 
  summarise(dTairmax = max(dTair, na.rm = T), dTairmin = min(dTair,na.rm = T), dTairmean = mean(dTair, na.rm=T))
Bdata = Bdata %>% mutate( dTair = tair - TTair)



# Example of couple of days timeseries
Sys.setlocale("LC_ALL","English")
ggplot(data=Bdata %>% filter(doy>222 & doy <229, Species != "TTR", id !="218A0248",id !="218A0248" ), aes(x = time, y = dTair))+
  geom_point(aes(color=Species))+
  #geom_line(aes(color=id, group = id))+
  geom_ma( aes(group =id, color = Species), n=3,size=.4,)+
  geom_ma(data = MCW19 %>% mutate(doy = yday(time)) %>% filter(doy>222 & doy <229),
          aes( x = time, y = sT-20),color ="black",linetype="42",size=1,alpha=.4, n=3)+
  geom_hline(aes(yintercept = 0))+
  scale_y_continuous(sec.axis =  sec_axis(~ . + 20))+
  scale_x_continuous(n.breaks = 7, trans="time")+
  facet_wrap(~Species, nrow = 2)+
  theme_bw()


# Per species main dynamics

Tdifn = Bdata %>% filter(hour<5 | hour >21) %>% filter(Species != "TTR") %>% group_by(Species,doy,id) %>% 
  summarise(dt = max(dTair,na.rm = T)) %>% group_by(Species,doy) %>% 
  summarise(dtm = mean(dt,na.rm = T), sdt = sd(dt,na.rm = T)) %>% 
  filter(Species != "TTR", sdt < 2) 

ggplot(data = Tdifn)+
  geom_point(aes(x=doy, y = dtm, color = Species ))+
  #geom_smooth(aes(x=doy, y = dtm, color = Species), span =4 )+
  geom_errorbar(aes(x=doy, ymin = dtm-2*sdt,ymax=dtm+2*sdt, color = Species),linetype="dashed")+
  geom_ma(aes(x=doy, y = dtm, color = Species), linetype="solid", n=7 )+
  facet_wrap(~Species,nrow = 2, scales = "free")+
  theme_bw()

# Diurnal temperature difference inside and outside canopy

Bdiurnal  = Bdata %>%  filter(Species != "TTR") %>% 
  mutate(month = month(time)) %>%filter(month < 11) %>% mutate(minute = minute(time))%>%
  group_by(month, hour, Species) %>% summarise(dT = mean(dTair, na.rm = T), sdT = sd(dTair, na.rm=T)) %>% as.data.frame()

Bdiurnal$hour[ceiling((Bdiurnal$hour-1)/3) == (Bdiurnal$hour-1)/3 ] = Bdiurnal$hour[ceiling((Bdiurnal$hour-1)/3) == (Bdiurnal$hour-1)/3 ]+.5  

ggplot(data = Bdiurnal)+
  geom_point(aes(x = hour, y = dT, color = Species), position = position_dodge(width = 1) )+
  geom_smooth(aes(x = hour, y = dT, color = Species, group = Species), se = F)+
  geom_errorbar(aes(x=hour, ymin = dT-sdT,ymax=dT+sdT, color = Species),linetype="dashed", 
                position = position_dodge(width = 1))+
  geom_hline(aes(yintercept=0))+
  facet_wrap(~month, nrow=2, scales = "free")+
  theme_bw()


# Maximum temperature difference inside day per species

Bvar = Bdata %>% group_by(doy,id, Species) %>% filter(!is.na(tair)) %>% filter(Species != "TTR") %>%
                summarise(Tvar = max(tair) - min(tair), TTvar = max(TTair)-min(TTair)) %>% 
                group_by(doy, Species) %>% summarise(dT = mean(Tvar), sTvar = sd(Tvar), 
                                                     dTT = mean(TTvar), sTTvar = sd(TTvar))

ggplot(data = Bvar)+
  geom_point(aes(x = doy, y = dT, color = Species) )+
  geom_smooth(aes(x = doy, y = dT, color = Species), se = F)+
  #geom_errorbar(aes(x=doy, ymin = dT-sTvar,ymax=dT+sTvar, color = Species),linetype="dashed", 
  #              position = position_dodge(width = 1))+
  geom_point(aes(x = doy, y = dTT), color = "black")+
  geom_smooth(aes(x = doy, y = dTT), color = "black", se = F)+
  #geom_errorbar(aes(x=doy, ymin = dTT-sTTvar,ymax=dTT+sTTvar), color = "black",linetype="dashed", 
  #              position = position_dodge(width = 1))+
  
  geom_hline(aes(yintercept=0))+
  facet_wrap(~Species, nrow=2)+
  theme_bw()


  
  
####################################    Energy   ##########################################################


##### Diurnal graph per month per species
  Bde = Bdata  %>% mutate(month = month(time))%>% group_by(month,hour,Species) %>% filter(Species != "TTR") %>% 
        summarise(Rn = sum(Rn, na.rm = T)*1.5, L = sum(L, na.rm=T)*1.5, G = sum(G, na.rm = T)*1.5, H = sum(H, na.rm = T)*1.5) %>%
    filter(!is.na(hour))
  
  Bde$hour[ceiling((Bde$hour-1)/3) == (Bde$hour-1)/3 ] = Bde$hour[ceiling((Bde$hour-1)/3) == (Bde$hour-1)/3 ]+.5
  
  ggplot(data = Bde%>% filter(Species != "TTR"))+
    geom_point(aes(x=hour, y = Rn), color = "red")+
    geom_point(aes(x=hour, y = L), color = "blue", shape = 2)+
    geom_point(aes(x=hour, y = H), color = "green", shape = 3)+
    geom_point(aes(x=hour, y = G), color="brown", shape = 4)+
    geom_smooth(aes(x=hour, y = Rn), color = "red", se = F)+
    geom_smooth(aes(x=hour, y = L), color = "blue",linetype="dotted", se = F)+
    geom_smooth(aes(x=hour, y = H), color = "green",linetype="dashed", se = F)+
    geom_smooth(aes(x=hour, y = G), color="brown", se = F)+
    facet_grid(month~Species, scales = "free")+
    theme_bw()

##### Stacked column per month
  Bsum = Bdata %>% mutate(month = month(time)) %>% filter(!is.na(month))%>%
    group_by(id,doy,month, Species) %>% filter(Species != "TTR") %>%
    summarise(Rn = sum(Rn, na.rm = T)*1.5, L = sum(L, na.rm=T)*1.5, 
              G = sum(G, na.rm = T)*1.5, H = sum(H, na.rm = T)*1.5, d = mean(d,na.rm = T))%>% 
              pivot_longer(cols=c(L,H,G), names_to = "heat_type", values_to = "energy") %>%
              group_by(month,id, Species, heat_type) %>% filter(!is.na(month))%>%
              summarise(E = sum(energy, na.rm = T), Rn = sum(Rn, na.rm = T)) 
  

  ggplot(data= Bsum)+
    geom_col(aes(x=id, y = E, fill = heat_type))+
    geom_errorbar(aes(x = id, ymin = Rn, ymax = Rn))+
    facet_grid(month~Species, scales = "free")+
    theme_bw()
  
 
  
  #  
  # ggplot(data  = Bdata %>% filter(Species != "TTR") %>% mutate(month = month(time)))+
  #   geom_point(aes(x=time,y = Rn ),alpha = .1, color = "red")+
  #   geom_smooth(aes(x=time,y = Rn),color = "red")+
  #   geom_point(aes(x=time,y = L), alpha = .1, color = "blue")+
  #   geom_smooth(aes(x=time,y = L), color = "red")+
  #   geom_point(aes(x=time,y = H), alpha = .1, color = "green")+
  #   geom_smooth(aes(x=time,y = H), color = "green")+
  #   geom_point(aes(x=time,y = G), alpha = .1, color = "brown")+
  #   geom_smooth(aes(x=time,y = G), color = "brown")+
  #   facet_wrap(~Species, scales = "free")+
  #   theme_bw()
  #   
  #   
  # 
  # ggplot(data = Bst)+
  #   geom_col(aes( x = id,y = Rn))+
  #   facet_wrap(~Species, ncol = 2, scale="free" )+
  #   #scale_y_continuous(limits=c(-.45,.5))+
  #   theme_bw()





write.csv(Bdata, file="data.csv")





###################                    Growth

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

##################################       LAI



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

