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

#Total biomass stored per season
ggplot(data = BLTN %>% filter(Site == "BOLOTNAYA") %>%filter(Species != "TTR")%>% group_by(id, Species)%>% summarise(kg=mean(biomas_stored)))+
  geom_col(aes( x = id,y = kg))+
  facet_wrap(~Species, ncol = 2, scale="free" )+
  #scale_y_continuous(limits=c(-.45,.5))+
  theme_bw()


#Artificial graph of biomass growth according to accumulated LAI
Bgr = Bdata %>%filter(Species != "TTR") %>% mutate(NDVIc =  replace(NDVIc, NDVIc > 1, 1) , NDVIc =  replace(NDVIc, NDVIc < -1, -1)) %>% 
  group_by(id, Species,doy) %>%   summarise(bio_proxy = quantile(NDVIc, 0.85,na.rm = T), kg=mean(biomas_stored), n=n()) %>% 
  mutate(bio_proxy = replace(bio_proxy,bio_proxy<0,0), bioproxy = cumsum(bio_proxy)) %>% 
  mutate(biomas_stored = kg*bioproxy/max(bioproxy)) %>% filter(id != "218A0186")

doy = rep(180:310,unique(Bgr$id)%>%length )
id = rep(unique(Bgr$id), rep(311-180,unique(Bgr$id)%>%length))


df = data.frame(id,doy) %>% left_join(Bgr, by=c("id","doy"))


df$biomas = 0
for( i in df$id %>% unique()){
  biomas = df$biomas_stored[df$id == i]
  Species = as.factor(df$Species[df$id == i])%>% levels
  df$Species[df$id == i] = Species
  print(biomas)
  biomas = na.approx(biomas, x = index(biomas),  na.rm = T, maxgap = Inf)
  print(biomas)
  df$biomas[df$id == i] = biomas
}
  

ggplot(data = df%>%filter(doy<300))+
  geom_point(aes(x=doy, y=biomas,  group=id), shape=3,size=.5, alpha=4/10)+
  geom_line(aes(x=doy, y=biomas,  group=id),size=.5)+
  #geom_ma(aes(x=doy, y=biomas, color =id), n=3, linetype=1, size=1)+
  facet_wrap(~Species, scales = "free")+
  theme_bw()



##################################       LAI


TTR = TTR %>% mutate( dTair = tair - TTair)

TTR = TTR %>% filter(Site %in% c("BOLOTNAYA","TROITSK"))
TTR = TTR %>% filter(Species != "TTR")
TTR$LAIb[is.infinite(TTR$LAIb)] = NA
# T dif

LAI = Bdata%>% filter(Species != "TTR")
LAI$LAIb[is.infinite(LAI$LAIb)] = NA
LAI = LAI %>% mutate(lightness = TTR_450c+TTR_500c+TTR_550c+TTR_570c+TTR_600c+TTR_650c+TTR_610c+
                       TTR_680c+TTR_730c+TTR_760c+TTR_810c+TTR_860c)
LAI = LAI %>% mutate(blueness = TTR_450c+TTR_500c)
LAI = LAI %>% mutate(blueness2 = (b_V_450+b_B_500)/(TTR_450+TTR_500c))
-log(LAI$blueness2) %>% summary
LAI = LAI %>% mutate(pPARic = TTR_450c*2.55+TTR_500c*.58+TTR_550c*.78+TTR_570c*.9+TTR_600c*.95+TTR_650c+
                         TTR_680c*.8+TTR_730c*.2+TTR_760c*.05)
LAI = LAI %>% mutate(pPARbc =  b_V_450c*2.55+b_B_500c*0.58+b_G_550c*0.78+b_Y_570c*0.9+b_O_600c*0.95+b_R_650c+
                         b_S_680c*0.8+b_T_730c*0.2+b_U_760c*0.05)
LAI = LAI %>% mutate(LAIparc = -log(pPARbc/pPARic)/3)
LAI = LAI %>% mutate(pPARi = TTR_450*2.55+TTR_500*.58+TTR_550*.78+TTR_570*.9+TTR_600*.95+TTR_650+
                       TTR_680*.8+TTR_730*.2+TTR_760*.05)
LAI = LAI %>% mutate(pPARb =  b_V_450*20.55+b_B_500*0.58+b_G_550*0.78+b_Y_570*0.9+b_O_600*0.95+b_R_650+
                       b_S_680*0.8+b_T_730*0.2+b_U_760*0.05)
LAI = LAI %>% mutate(LAIpar = -log(pPARb/pPARi)/3)
LAI = LAI %>% group_by(id)%>%mutate(nlightness = lightness /max(lightness, na.rm=T))
LAI = LAI %>% mutate(qlightness = order(lightness) /n())

#LAI = LAI %>% group_by( Species, id,doy)%>% summarise(b = mean(blueness2,na.rm=T),nl = max(nlightness,na.rm=T),
#                                                      q = mean(qlightness,na.rm=T),LAImax = max(LAIpar, na.rm = T),
#                                                        LAImin = min(LAIb,na.rm = T), LAImean = mean(LAIb, na.rm=T))

LAI$LAIparc[!is.na(LAI$LAIparc) & LAI$TTR_450c<1000] = LAI$LAIparc[!is.na(LAI$LAIparc) & LAI$TTR_450c<1000]+2
LAI$LAIparc[ LAI$hour != 13 ] = NA
LAI = LAI %>% mutate(month = month(time))%>% group_by(month) %>%
  mutate(se = sd(LAIparc, na.rm = T),m = mean(LAIparc, na.rm = T), ) %>% 
  mutate(LAIparc = replace(LAIparc,LAIparc < m-se,NA))

LAI = LAI %>% mutate(LAIparc = replace(LAIparc,doy >285 & LAIparc >1.9,0.5))
LAI = LAI %>% mutate(LAIparc = replace(LAIparc,LAIparc < 0.1,0.5))


# PAI dynamics

ggplot(data = LAI)+
  #geom_point(aes(x=doy, y = q, color = q ))+
  geom_point(aes(x=time, y = LAIparc, color = id ))+
  #geom_ma(aes(x=doy, y = LAIparc, color = nl ), n=7)+
  geom_smooth(aes(x=time, y = LAIparc, color = id ), span=.1, se=F)+
  #geom_point(aes(x=time, y = q, color = id ))+
  #geom_line(aes(x=doy, y = LAImax, color = Species ))+
  facet_wrap(~Species,nrow = 2)+
  theme_bw()


# LAI, WAI per species
PAI = LAI %>%group_by(id,Species) %>% summarise(PAI = mean(replace(LAIparc, doy>290,NA),na.rm=T), 
            WAI =mean(replace(LAIparc, doy<290,NA),na.rm=T))%>% mutate(LAI = PAI -WAI) %>%
            pivot_longer(cols = c("WAI","LAI"), names_to = "index_name", values_to = "index")%>% as.data.table()
PAI[17,5] = 0.52
PAI[18,5] = 2.8
ggplot(data = PAI )+
  geom_col(aes(x = id, y = index, fill = index_name))+
  facet_wrap(~Species, scales = "free")+
  theme_bw()


###################### Partciles absorption #############################
 

Cpm <- read_delim("data/Moscow_center_pm25.csv", ";", escape_double = FALSE, 
                    col_types = cols(time = col_datetime(format = "%d.%m.%Y %H:%M")), 
                    trim_ws = TRUE)

Cpm = Cpm %>% mutate(doy = yday(time),hour=hour(time)) # g10-6 m-3

LAI = LAI %>% group_by(id,doy) %>% mutate(PAI = mean(LAIparc,na.rm=T)) %>%
  left_join(Cpm, by = c("doy","hour")) %>% select(-time.y)

LAI = LAI %>% left_join(PAI %>% filter(index_name == "WAI")%>% select(id,index), by ="id") %>%
  rename(WAI  = index)

for( i in LAI$id %>% unique()){
  lai = LAI$PAI[LAI$id == i]
  #Species = as.factor(LAI$Species[LAI$id == i])%>% levels
  #df$Species[df$id == i] = Species
  #print(biomas)
  lai = na.approx(lai, x = index(lai),  na.rm = T, maxgap = Inf)
  print(biomas)
  LAI$PAI[LAI$id == i] = lai
}

Vdavg = 0.64
Vdmin = 0.25
Vdmax = 1
LAIpm10 = 6

LAI =  LAI %>% mutate(LAI = PAI-WAI) %>% 
               mutate(V_avg = Vdavg*(PAI)/(WAI + LAIpm10)) %>%
               mutate(V_min = Vdmin*(PAI)/(WAI + LAIpm10)) %>%
               mutate(V_max = Vdmax*(PAI)/(WAI + LAIpm10)) %>%
               mutate(P_avg =V_avg*pm10*0.036) %>%
               mutate(P_min =V_min*pm10*0.036) %>%
               mutate(P_max =V_max*pm10*0.036) %>% rename(time = time.x) # g m-3

pm10 = LAI %>% group_by(id,doy, Species) %>% 
  summarise(P_avg = sum(P_avg, na.rm = T)*1.5,P_min = sum(P_min, na.rm = T)*1.5,P_max = sum(P_max, na.rm = T)*1.5)

ggplot(data = pm10)+
  #geom_point(aes(x=doy, y = P_avg, color = id)) +
  #geom_smooth(aes(x=doy, y = P_avg, color = id)) + 
  geom_errorbar(aes(x=doy, y = P_avg,ymin=P_min,ymax=P_max, color = id), position = position_dodge(3))+
  geom_smooth(aes(x=doy, y = P_avg, color = id), se =F, span=1)+
  facet_wrap(~Species, scales = "free")+
  theme_bw()

pm10sum = pm10 %>% group_by(id, Species) %>% 
  summarise(P_max= sum(P_max,na.rm=T),P_avg= sum(P_avg,na.rm=T),P_min= sum(P_min,na.rm=T)) 
  


ggplot(data = pm10sum) +
  geom_crossbar(aes(x=id,  y= P_avg,ymin=P_min,ymax=P_max, color=Species))+
  facet_wrap(~Species, scales = "free")+
  theme_bw()


