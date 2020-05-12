source("code/TT_Moscow_data_collection.R")
library(pracma)
# for wind interpolation
library(zoo)
library(tidyquant)
library(gt)
library(tsibble)
library(fable)
library(feasts)
library(LakeMetabolizer)







#p.Evapotranspiration - remove pike in orange acer
# latent heat recheck the units, 
# add OM_10 conc fot daily PM_10
# tranpiration example of sap flow 5 days,l/h

#Final table per species mean, sd, LAI, daily flux per season and what we have in baisic, daily average per season


#Loading weather data 
#AllData = BLTNdata[[2]]

BLTN  = AllData %>% filter(Site=="BOLOTNAYA")
BLTN  = BLTN %>% BEFadd( verboseFlag =  "con")
BLTN = BLTN %>% left_join(read_delim("data/Bolotnaya_growth.csv", delim=";"),by="id") 
BLTN = BLTN %>% mutate(biomas_stored = pi*tree_height*1/3*(growth/1000)*(d/100+growth/1000) * C_V*1000)
#Basal Area Increment = BAI = pi/4*((d/100)^2 -((d/100)-(growth)/1000)^2)
BLTN = BLTN %>% mutate(biomas_stored2 = tree_height*0.5*pi/4*((d/100)^2-((d/100)-(growth/1000))^2)*C_V*1000)

Moscow_center_weather_19 <- read_delim("data/Moscow_center_weather_19.csv",delim = ";",
                                       escape_double = FALSE, comment = "#", 
                                       col_types = cols(Pa = col_double()),  trim_ws = TRUE) %>% 
  rename(time ="Местное время в Москве (центр, Балчуг)" ) %>%
  mutate(time = as_datetime(time,format = "%d.%m.%Y %H:%M"))
#Calculating wind speed at height 20m
#MCW19 = Moscow_center_weather_19 %>% mutate(ff10 = Ff)
MCW19 = Moscow_center_weather_19 %>% mutate(ff20 = Ff*log(20/.6)/log(10/.6))

MCW19 = MCW19 %>% select(time,"T",Po,U, Ff,ff20, RRR) %>% rename("sT" = "T") %>% rename("srh" = "U")
MCW19$RRR = MCW19$RRR %>% as.numeric()
MCW19$RRR[is.na(MCW19$RRR)] = 0
MCW19 = MCW19 %>% arrange(time) %>% mutate(hour = hour(time))%>% as.data.frame()
MCW19$RRR[MCW19$hour %in% c(0,3,9,12,15,21)] = NA
MCW19 = MCW19 %>% select(-hour)

#Gap fill data

Bdata = BLTN %>% ungroup() %>% left_join(MCW19, by ="time") %>% as.data.frame()
Bdata$Po = Bdata$Po*1.33322
Bdata$Po = zoo::na.approx(Bdata$Po, x = zoo::index(Bdata$Po),  na.rm = F, maxgap = Inf)
Bdata$TTrh = zoo::na.approx(Bdata$TTrh, x = zoo::index(Bdata$TTrh),  na.rm = F, maxgap = Inf)
Bdata$ff20 = c(zoo::na.approx(Bdata$ff20, x = zoo::index(Bdata$ff20),  na.rm = TRUE, maxgap = Inf), rep(NA,8))
Bdata$Ff = zoo::na.approx(Bdata$Ff, x = zoo::index(Bdata$Ff),  na.rm = F, maxgap = Inf)
Bdata$sT = c(zoo::na.approx(Bdata$sT, x = zoo::index(Bdata$sT),  na.rm = TRUE, maxgap = Inf), rep(NA,8))
Bdata$srh = c(zoo::na.approx(Bdata$srh, x = zoo::index(Bdata$srh),  na.rm = TRUE, maxgap = Inf), rep(NA,8))
Bdata = Bdata %>% group_by(id, doy) %>% mutate(nt1 = 
                                                 zoo::na.approx(nt1, x = zoo::index(nt1),  na.rm = F, maxgap = Inf))%>%as.data.frame()
Bdata$RRR[is.na(Bdata$RRR)]=0



# Fast and dirty removing NAs from time variable with linear interpolation by zoo package
Bdata$time = as.POSIXct(as.integer(na.fill(zoo(Bdata$time),"extend")), tz="Europe/Moscow", origin = "1970-01-01 00:00:00")
Bdata = Bdata %>%mutate(time_ts = floor_date(time, "hour"))
# Removing duplicated rows... again???
Bdata = Bdata %>% filter(!are_duplicated(Bdata, index = time_ts, key = id))
# making tsibble to produce regular ts
# Bdats  = tsibble(Bdata, index=time, key=id, regular = F)
# Bdats = Bdats %>% fill_gaps(.full = T)
Bdata = Bdata %>% group_by(id, doy) %>% mutate(nt1 = 
                                                 zoo::na.approx(nt1, x = zoo::index(nt1),  na.rm = F, maxgap = Inf)) %>% as.data.frame()
Bdats = tsibble(Bdata, index=time_ts, key=id, regular = T)
Bdats = Bdats %>% group_by_key() %>%fill_gaps(.full = T)

#Fable package interpolate
Bdatsi = Bdats %>%filter(id !="218A0058")%>% model(ARIMA(Flux~0))%>% interpolate(Bdats)%>% rename(Flux_ts = Flux)
Bdats = Bdats %>% left_join(Bdatsi, by=c("id","time_ts"))

Bdatsi = Bdats %>%filter(id !="218A0058")%>% model(ARIMA(NDVIc~0))%>% interpolate(Bdats)%>% rename(NDVIts = NDVIc)
Bdats = Bdats %>% left_join(Bdatsi, by=c("id","time_ts"))


Bdatsi = Bdats %>%filter(id !="218A0058")%>% model(ARIMA(tair~TTair))%>% interpolate(Bdats)%>% rename(tair_ts = tair)
Bdats = Bdats %>% left_join(Bdatsi, by=c("id","time_ts"))

Bdatsi = Bdats %>%filter(id !="218A0058")%>% model(ARIMA(TTair~tair))%>% interpolate(Bdats)%>% rename(TTair_ts = TTair)
Bdats = Bdats %>% left_join(Bdatsi, by=c("id","time_ts"))

Bdatsi = Bdats %>%filter(id !="218A0058")%>% model(ARIMA(nt1~TTair))%>% interpolate(Bdats)%>% rename(nt1_ts = nt1)
Bdats = Bdats %>% left_join(Bdatsi, by=c("id","time_ts"))

Bdatsi = Bdats %>%filter(id !="218A0058")%>% model(ARIMA(ff20~tair+rh))%>% interpolate(Bdats)%>% rename(ff20_ts = ff20)
Bdats = Bdats %>% left_join(Bdatsi, by=c("id","time_ts"))

Bdatsi = Bdats %>%filter(id !="218A0058")%>% model(ARIMA(RRR~tair+rh))%>% interpolate(Bdats)%>% rename(prcp_ts = RRR)
Bdats = Bdats %>% left_join(Bdatsi, by=c("id","time_ts"))

Bdata = Bdats %>% as.data.frame()   %>% filter(!(id %in% c("218A0088","218A0193","218A0248"))) %>% mutate(time = time_ts) 

Bdata = Bdata %>% group_by(id) %>% mutate(d = mean(d, na.rm =T))
Bdata = Bdata %>% group_by(id) %>% mutate(doy = yday(time_ts))
Bdata = Bdata %>% group_by(id) %>% mutate(hour = hour(time_ts))
Bdata = Bdata %>% group_by(id) %>% mutate(month = month(time_ts))
Bdata = Bdata %>% group_by(id) %>% mutate(biomas_stored = mean(biomas_stored, na.rm=T))
Bdata = Bdata %>% group_by(id) %>% mutate(Species = levels(as.factor(Species))[1])
#Bdata = Bdata %>% rename(canopy_area = caopy_area)
#Aerodynamic resistance according to Tom and Eddy Pro

Bdata = Bdata %>% mutate(r =log((20-20*0.67)/(20*.15))*log((20-20*0.67)/(20*.015))/(ff20_ts*0.41^2)) #3.698186 /ff20_ts
#Bdata = Bdata %>% mutate(ra =log((20-20*0.67)/(20*0.0123))*log((20-20*0.67)/(20*.123))/(Ff*0.41^2))
#Bdata = Bdata %>% mutate(a_d = air_density(tair,rh, Po, 3.5) )
Bdata = Bdata %>% group_by(id, doy) %>% mutate(nt1 = 
                                                 zoo::na.approx(nt1, x = zoo::index(nt1),  na.rm = F, maxgap = Inf)) %>% as.data.frame()
#	C = ρCp ∆T/r kW/m2
Bdata = Bdata %>% mutate(nt1_ts = nt1)
Bdata = Bdata %>% mutate(He = 1.006*1.202*(nt1_ts-TTair_ts)/(r)*canopy_area )
Bdata = Bdata %>% mutate(Hw = 1.006*1.202*(nt1_ts-TTair_ts)/(r))
#Bdata = Bdata %>% group_by(id,doy,hour) %>% 
#  mutate(Hwt = calc.zeng(time_ts,nt1_ts,TTair_ts,ff20_ts,TTrh,Po,20,3.5,3.5)$ash) %>%  as.data.frame()
 
Bdata = Bdata %>% group_by(id, doy) %>% mutate(Hwt = 
                                                 zoo::na.approx(Hwt, x = zoo::index(Hwt),  na.rm = F, maxgap = Inf)) %>% as.data.frame()
Bdata = Bdata %>% mutate(Het = Hwt*canopy_area)
Bdata$Flux_ts[Bdata$Flux_ts >100] = 90
Bdata$Flux_ts[Bdata$Flux_ts < 0] = 0
#lambda_T = Flux * 2264.705 KJ
#/3600 in kWh 
Bdata = Bdata %>% mutate(Le = Flux_ts * 2264.705 /3600)
Bdata = Bdata %>% mutate(Lw = Flux_ts * 2264.705 /(3600*canopy_area))
Bdata = Bdata %>% mutate(Rnet = (Le+Het)/.9 )
Bdata = Bdata %>% mutate(Rnwt = (Lw+Hwt)/.9 )
Bdata = Bdata %>% mutate(Rne = (Le+He)/.9 )
Bdata = Bdata %>% mutate(Rnw = (Lw+Hw)/.9 )
Bdata = Bdata %>% mutate(Get = 0.1*Rnet )
Bdata = Bdata %>% mutate(Gwt = 0.1*Rnwt )
Bdata = Bdata %>% mutate(Ge = 0.1*Rne )
Bdata = Bdata %>% mutate(Gw = 0.1*Rnw )

Bdata = Bdata %>% group_by(id) %>% 
  mutate(He = zoo::na.approx(He, x = zoo::index(He),  na.rm = F, maxgap = Inf)) %>%
  mutate(Le = zoo::na.approx(Le, x = zoo::index(Le),  na.rm = F, maxgap = Inf)) %>%
  mutate(Rne = zoo::na.approx(Rne, x = zoo::index(Rne),  na.rm = F, maxgap = Inf)) %>%
  mutate(Ge = zoo::na.approx(Ge, x = zoo::index(Ge),  na.rm = F, maxgap = Inf)) %>%
  mutate(Hw = zoo::na.approx(Hw, x = zoo::index(Hw),  na.rm = F, maxgap = Inf)) %>%
  mutate(Lw = zoo::na.approx(Lw, x = zoo::index(Lw),  na.rm = F, maxgap = Inf)) %>%
  mutate(Rnw = zoo::na.approx(Rnw, x = zoo::index(Rnw),  na.rm = F, maxgap = Inf)) %>%
  mutate(Gw = zoo::na.approx(Gw, x = zoo::index(Gw),  na.rm = F, maxgap = Inf)) 
  
#Bdata$He %>% is.na %>% which %>% length()

Bdata = Bdata %>%  filter(Species != "TTR", id !="218A0088",id != "218A0193",id !="218A0248")
#TTR  = AllData %>% filter(!is.na(TTair))



Dist = Bdata %>% select(id, Species,time, dist, NDVIc,Flux)
write.csv(file="dist.csv", ist)



#######################################  Temperature #############################################

Tdif = Bdata %>% mutate( dTair = tair_ts - TTair_ts) %>% group_by(Site, Species, doy) %>% 
  summarise(dTairmax = max(dTair, na.rm = T), dTairmin = min(dTair,na.rm = T), dTairmean = mean(dTair, na.rm=T))
Bdata = Bdata %>% mutate( dTair_ts = tair_ts - TTair_ts)%>% mutate( dTair = tair - TTair) %>% mutate(gap = cumsum(!is.na(dTair)))



# Example of couple of days timeseries
Sys.setlocale("LC_ALL","English")
g1 = ggplot(data=Bdata %>% filter(doy>222 & doy <229) , aes(x = time, y = dTair))+
  geom_point(aes(color=Species))+
  #geom_line(aes(color=id, group = gap))+
  geom_ma( aes(group =id, color = Species), n=3,size=.4,)+
  geom_ma(data = MCW19 %>% mutate(doy = yday(time)) %>% filter(doy>222 & doy <229),
          aes( x = time, y = sT-20),color ="black",linetype="42",size=1,alpha=.4, n=3)+
    #geom_ma(data = Bdata %>% filter(doy>222 & doy <229) %>% group_by(time_ts) %>% summarise(sT = mean(sT, na.rm = T)),
  #        aes( x = time_ts, y = sT-20),color ="black",linetype="42",size=1,alpha=.4, n=3)+
    geom_hline(aes(yintercept = 0))+
    scale_y_continuous(sec.axis =  sec_axis(~ . + 20, name = expression(T[air]~","~{C}^o)),limits=c(-7,5))+
  scale_x_continuous(n.breaks = 7, trans="time")+
  facet_wrap(~Species, nrow = 2)+
  theme_bw()+
  theme(legend.position = "none", axis.text=element_text(size=12),
        axis.title=element_text(size=16,face="bold"),
        strip.text.x = element_text(size = 14))+
  xlab(element_blank())+
  ylab(expression(Delta~T[air]~","~{C}^o))
  #annotate('text', x = 0, y = 0, label = "Value~is~sigma~R^{2}==0.6 ",parse = TRUE,size=20) +
  ggsave(plot = g1, filename = "results/es/dTair_vs_Tair_couple_days_filled.png", 
         width = 16, height =12, units = "in", dpi = 100, device ="png")
  


# Per species main dynamics

Tdifn = Bdata %>% filter(hour<5 | hour >21) %>% filter(Species != "TTR") %>% group_by(Species,doy,id) %>% 
  summarise(dt = max(dTair,na.rm = T)) %>% group_by(Species,doy) %>% 
  summarise(dtm = mean(dt,na.rm = T), sdt = sd(dt,na.rm = T)/ sqrt(sum(!is.na(dt)))) %>% 
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
  group_by(month, hour, Species) %>% summarise(dT = mean(dTair, na.rm = T), sdT = 2*sd(dTair, na.rm=T)/ sqrt(sum(!is.na(dTair)))) %>%
  as.data.frame()

Bdiurnal$hour[ceiling((Bdiurnal$hour-1)/3) == (Bdiurnal$hour-1)/3 ] =
  Bdiurnal$hour[ceiling((Bdiurnal$hour-1)/3) == (Bdiurnal$hour-1)/3 ]+.5  



#blank_data <- data.frame(month = c(7, 7, 8, 8, 9, 9, 10,10), x = 0, y = c(-3, 2, -3, 2, -3, 5,))


g2 = ggplot(data = Bdiurnal %>% filter(dT <5))+
  geom_point(aes(x = hour, y = dT, color = Species), position = position_dodge(width = 1) )+
  geom_smooth(aes(x = hour, y = dT, color = Species, group = Species), se = F, span=0.8)+
  geom_errorbar(aes(x=hour, ymin = dT-sdT,ymax=dT+sdT, color = Species),linetype="dashed", 
                position = position_dodge(width = 1))+
  geom_hline(aes(yintercept=0))+

  facet_wrap(~month, nrow=2, scales = "fixed")+
  theme_bw()+
  theme(legend.position = "bottom",legend.title = element_blank(), 
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=16,face="bold"),
        strip.text.x = element_text(size = 14))+
  xlab(expression(Day~hour))+
  ylab(expression(Delta~T[air]~","~{C}^o))
#annotate('text', x = 0, y = 0, label = "Value~is~sigma~R^{2}==0.6 ",parse = TRUE,size=20) +
ggsave(plot = g2, filename = "results/es/dTair_diurnal_by_month.png", 
       width = 16, height =12, units = "in", dpi = 100, device ="png")


#######################################  Relative humidity #############################################

Bdata = Bdata %>% mutate( drh = rh - srh)
Tdif = TTR %>% group_by(Site, Species, doy) %>% 
  summarise(drhmax = max(drh, na.rm = T), drhmin = min(dTair,na.rm = T), drhmean = mean(dTair, na.rm=T))




# Example of couple of days timeseries of relative humidity
Sys.setlocale("LC_ALL","English")
g3 = ggplot(data=Bdata %>% filter(doy>230 & doy <239, Species != "TTR", id !="218A0248",id !="218A0248" ), aes(x = time, y = drh))+
  geom_point(aes(color=Species), alpha =0.4,size=1, shape=3)+
  #geom_line(aes(color=id, group = id))+
  geom_ma( aes(group =id, color = Species), n=3,size=.4,)+
  geom_ma(data = MCW19 %>% mutate(doy = yday(time)) %>% filter(doy>230 & doy <239),
          aes( x = time, y = srh-60),color ="black",linetype="42",size=1,alpha=.4, n=3)+
  geom_hline(aes(yintercept = 0))+
  scale_y_continuous(sec.axis =  sec_axis(~ . + 60, name=expression(Rh[air]~","~"%")))+
  scale_x_continuous(n.breaks = 7, trans="time")+
  facet_wrap(~Species, nrow = 2)+
  theme_bw()+
  theme(legend.position = "none")+  theme_bw()+
  theme(legend.position = "none", axis.text=element_text(size=12),
        axis.title=element_text(size=16,face="bold"),
        strip.text.x = element_text(size = 14))+
  xlab(element_blank())+
  ylab(expression(Delta~Rh[air]~","~"%"))
#annotate('text', x = 0, y = 0, label = "Value~is~sigma~R^{2}==0.6 ",parse = TRUE,size=20) +
ggsave(plot = g3, filename = "results/es/drh_vs_rh_couple_days.png", 
       width = 16, height =12, units = "in", dpi = 100, device ="png")



# Diurnal rh difference inside and outside canopy

Bdiurnal  = Bdata %>%  filter(Species != "TTR") %>% 
  mutate(month = month(time)) %>%filter(month < 11) %>% mutate(minute = minute(time))%>%
  group_by(month, hour, Species) %>% summarise(dRh = mean(drh, na.rm = T), sdRh = 2*sd(drh, na.rm=T)/sum(!is.na(drh))^.5) %>%
  as.data.frame()

Bdiurnal$hour[ceiling((Bdiurnal$hour-1)/3) == (Bdiurnal$hour-1)/3 ] = 
  Bdiurnal$hour[ceiling((Bdiurnal$hour-1)/3) == (Bdiurnal$hour-1)/3 ]+.5  
Bdiurnal = Bdiurnal[-which(Bdiurnal$hour %in% c(2,5,8,11,14,17,20,23)),]


g4 = ggplot(data = Bdiurnal)+
  geom_point(aes(x = hour, y = dRh, color = Species), position = position_dodge(width = 1) )+
  geom_smooth(aes(x = hour, y = dRh, color = Species, group = Species, fill = Species), se = F)+
  geom_errorbar(aes(x=hour, ymin = dRh-sdRh,ymax=dRh+sdRh, color = Species),linetype="dashed", 
                  position = position_dodge(width = 1))+
  geom_hline(aes(yintercept=0))+
  facet_wrap(~month, nrow=2, scales = "fixed")+
  theme_bw()+
  theme(legend.position = "bottom",legend.title = element_blank(), 
      legend.text=element_text(size=14),
      axis.text=element_text(size=12),
      axis.title=element_text(size=16,face="bold"),
      strip.text.x = element_text(size = 14))+
  xlab(expression(Day~hour))+
  ylab(expression(Delta~Rh[air]~","~{C}^o))
#annotate('text', x = 0, y = 0, label = "Value~is~sigma~R^{2}==0.6 ",parse = TRUE,size=20) +
ggsave(plot = g4, filename = "results/es/drh_diurnal_by_month.png", 
       width = 16, height =12, units = "in", dpi = 100, device ="png")




# Maximum temperature difference inside day per species

Bvar = Bdata %>% group_by(doy,id, Species) %>% filter(!is.na(tair)) %>% filter(Species != "TTR") %>%
  summarise(Tvar = max(tair_ts) - min(tair_ts), TTvar = max(TTair_ts)-min(TTair_ts)) %>% 
  group_by(doy, Species) %>% summarise(dT = mean(Tvar), sTvar = sd(Tvar), 
                                       dTT = mean(TTvar), sTTvar = sd(TTvar))

g45 =  ggplot(data = Bvar)+
  geom_point(aes(x = doy, y = dT, color = Species), shape=3 )+
  geom_smooth(aes(x = doy, y = dT, color = Species), se = F)+
  #geom_errorbar(aes(x=doy, ymin = dT-sTvar,ymax=dT+sTvar, color = Species),linetype="dashed", 
  #              position = position_dodge(width = 1))+
  geom_point(aes(x = doy, y = dTT), color = "black", shape=4)+
  geom_smooth(aes(x = doy, y = dTT), color = "black", se = F, linetype="dashed")+
  #geom_errorbar(aes(x=doy, ymin = dTT-sTTvar,ymax=dTT+sTTvar), color = "black",linetype="dashed", 
  #              position = position_dodge(width = 1))+
  xlab(expression(Day~of~the~year))+
  ylab(expression(T[air~max]~"-"~T[air~min]~","~{C}^o))+
  
  geom_hline(aes(yintercept=0))+
  facet_wrap(~Species, nrow=2)+
  theme_bw()+ theme(legend.position = "none")
ggsave(plot = g45, filename = "results/es/Tair_range_inside_and_outside_canopies_per_speies.png", 
       width = 16, height =12, units = "in", dpi = 100, device ="png")

####################################    Evapotranspiration   ##########################################################

# Bevap = Bdata %>% group_by(id,doy,Species) %>% summarise(evap = sum(Flux, na.rm=T)*1.5, prcp = sum(RRR, na.rm =T)*1.5) %>%
#   group_by(id, Species) %>% mutate(evapc = cumsum(evap), cprcp = cumsum(prcp)) %>% filter(Species != "TTR")
# prcp = Bdata %>% group_by(id,doy) %>% summarise(prcp = sum(RRR, na.rm=T)*1.5) %>% group_by(doy) %>% 
#   summarise(prcp = mean(prcp, na.rm=T)) %>% ungroup %>% mutate(cprcp = cumsum(prcp))

#Bdata = Bdata %>% filter(id !%in% c("218A00104","218A00193","218A00248"))

Bevap = Bdata %>% filter(!(id %in% c("218A0088","218A0193","218A0248")))%>% group_by(id,doy,Species) %>% 
  summarise(evap = sum(Flux_ts, na.rm=T), prcp = sum(RRR, na.rm =T), ca = mean(canopy_area, na.rm = T)) %>%as.data.frame()
Bevap$evap[Bevap$id == "218A0077" & Bevap$doy > 300 & Bevap$doy < 308] = 0    
  
Bevap = Bevap %>%  group_by(id, Species) %>% mutate(evapc = cumsum(evap), cprcp = cumsum(prcp), ca = mean(ca, na.rm=T)) %>% filter(Species != "TTR")%>%as.data.frame()

Bevap_sum = Bevap %>%  group_by(id, Species) %>% summarise(evapc = sum(evap), cprcp = sum(prcp), ca = mean(ca, na.rm=T)) %>% filter(Species != "TTR")%>%
   mutate(evapc = evapc/ca)

Bevap_sum$cprcp = max(Bevap_sum$cprcp)

prcp = Bdata %>% filter(!(id %in% c("218A0088","218A0193","218A0248")))%>% group_by(id,doy) %>% summarise(prcp = sum(RRR, na.rm=T)) %>% group_by(doy) %>%
  summarise(prcp = mean(prcp, na.rm=T)) %>% ungroup %>% mutate(cprcp = cumsum(prcp))


      
      
  geom_point(aes(x=time_ts, y=cumsum(Flux_ts), color = id))+
  facet_wrap(~id, nrow = 4,scale="free")+
  theme_bw()

g5 = ggplot(data = Bevap)+
  geom_point(aes(x=doy,y=evapc/(ca), color=id),shape =2)+
  geom_point(data = prcp, aes(x=doy,y=cprcp ),shape =3)+
  geom_line(aes(x=doy,y=evapc/(ca), color=id))+
  geom_line(data = prcp, aes(x=doy,y=cprcp, ))+
  facet_wrap(~Species, scale="free")+
  theme_bw()+
  theme(legend.position = "none")+
  theme(legend.position = "bottom",legend.title = element_blank(), legend.box = "horizontal", legend.margin=margin(),
      legend.text=element_text(size=14),
      axis.text=element_text(size=12),
      axis.title=element_text(size=16,face="bold"),
      strip.text.x = element_text(size = 14))+
  guides(colour = guide_legend(nrow = 2))+
  xlab(expression(Day~of~the~ year))+
  ylab(expression(E[tree]~","~P~","~mm))
#annotate('text', x = 0, y = 0, label = "Value~is~sigma~R^{2}==0.6 ",parse = TRUE,size=20) +
ggsave(plot = g5, filename = "results/es/Transpiration_vs_precipitation_cumulated_ts_mm.png", 
       width = 16, height =12, units = "in", dpi = 100, device ="png")




VPD = Bdata%>% filter(doy>203 & doy <210, Species != "TTR", id !="218A0248",id !="218A0248" ) %>% group_by(time) %>% summarise(VPD = mean(VPD,na.rm = T))


g52 = ggplot(data=Bdata %>% filter(doy>203 & doy <210, Species != "TTR", id !="218A0248",id !="218A0248" ), aes(x = time, y = Flux))+
  geom_point(aes(color=id), shape=4, size=.5)+
  #geom_line(aes(color=id, group = id))+
  geom_ma( aes(group =id, color = id), n=3,size=.4,span=1)+
  geom_line(data = VPD, aes( x = time, y = VPD*3),color ="black", size=1, alpha=.4 )+
  geom_hline(aes(yintercept = 0))+
  scale_y_continuous(sec.axis =  sec_axis(~ . *3, name = expression(VPD~","~kPa)),limits=c(-0.5,5))+
  scale_x_continuous(n.breaks = 7, trans="time")+
  facet_wrap(~Species, nrow = 2)+
  theme_bw()+
  guides(colour = guide_legend(nrow = 2))+
  theme(legend.position = "bottom", axis.text=element_text(size=12),legend.box = "horizontal", legend.margin=margin(),
        axis.title=element_text(size=16,face="bold"),legend.title = element_blank(),
        strip.text.x = element_text(size = 14))+
  xlab(element_blank())+
  ylab(expression(~Sap~flux~","~l~h^{-1}))
#annotate('text', x = 0, y = 0, label = "Value~is~sigma~R^{2}==0.6 ",parse = TRUE,size=20) +
ggsave(plot = g52, filename = "results/es/Flux_vs_VPD_couple_days_filled.png", 
       width = 16, height =12, units = "in", dpi = 100, device ="png")



####################################    Energy   ##########################################################



##### Diurnal graph per month per species
Bde = Bdata  %>% filter(!(id %in% c("218A0088","218A0193","218A0248")))%>%
  mutate(month = month(time_ts),hour = hour(time_ts))%>% group_by(month,hour,Species) %>% 
  filter(Species != "TTR") %>% 
  summarise(Rn = mean(Rne, na.rm = T), L = mean(Le, na.rm=T), G = mean(Ge, na.rm = T), H = mean(He, na.rm = T), 
            sdL = sd(Le,na.rm = T)/ sqrt(sum(!is.na(Le)))) %>%
  filter(!is.na(hour)) 

Bdw = Bdata  %>% filter(!(id %in% c("218A0088","218A0193","218A0248")))%>%
  group_by(month,hour,Species) %>% filter(Species != "TTR") %>% 
  summarise(Rn = mean(Rnw, na.rm = T), L = mean(Lw, na.rm=T), G = mean(Gw, na.rm = T), 
            H = mean(Hw, na.rm = T),Ht = mean(Hwt, na.rm = T),  Flux_ts = mean(Flux_ts, na.rm=T), c=mean(canopy_area,na.rm = T)/10) %>%
  filter(!is.na(hour)) 

Bdwt = Bdata  %>% filter(!(id %in% c("218A0088","218A0193","218A0248")))%>%
  group_by(month,hour,Species) %>% filter(Species != "TTR") %>% 
  summarise(Rn = mean(Rnwt, na.rm = T), L = mean(Lw, na.rm=T), G = mean(Gwt, na.rm = T), 
            H = mean(Hwt, na.rm = T),Ht = mean(Hwt, na.rm = T),  Flux_ts = mean(Flux_ts, na.rm=T), c=mean(canopy_area,na.rm = T)/10) %>%
  filter(!is.na(hour)) 


Bdata_sample = Bdata %>% filter(doy == 261, id == "218A0285")
round(Bdata_sample$Hwt*1000,,3 )
round(Bdata_sample$Lw*1000,,3 )
round(Bdata_sample$Flux_ts,3 )
round(Bdata_sample$ff20_ts,3)
round(Bdata_sample$TTair_ts - Bdata_sample$nt1_ts,3 )
round(Bdata_sample$Hw*1000 )/ floor(Bdata_sample$tair_ts - Bdata_sample$TTair_ts,3 )


Bde$hour[ceiling((Bde$hour-1)/3) == (Bde$hour-1)/3 ] = Bde$hour[ceiling((Bde$hour-1)/3) == (Bde$hour-1)/3 ]+.5
Bdw$hour[ceiling((Bdw$hour-1)/3) == (Bdw$hour-1)/3 ] = Bdw$hour[ceiling((Bdw$hour-1)/3) == (Bdw$hour-1)/3 ]+.5
# ggplot(data = Bde%>% filter(Species != "TTR"))+
#   geom_point(aes(x=hour, y = Rn), color = "red")+
#   geom_point(aes(x=hour, y = L), color = "blue", shape = 2)+
#   geom_point(aes(x=hour, y = H), color = "green", shape = 3)+
#   geom_point(aes(x=hour, y = G), color="brown", shape = 4)+
#   geom_smooth(aes(x=hour, y = Rn), color = "red", se = F)+
#   geom_smooth(aes(x=hour, y = L), color = "blue",linetype="dotted", se = F)+
#   geom_smooth(aes(x=hour, y = H), color = "green",linetype="dashed", se = F)+
#   geom_smooth(aes(x=hour, y = G), color="brown", se = F)+
#   facet_grid(month~Species, scales = "free")+
#   scale_colour_manual( 
#     breaks = c("Rn", "L", "H","G"),
#     values = c("Rn"="red", "L"=" blue", 
#                "H"="green", "G"=
#                  "brown")) +
#   theme_bw()

Bdel = Bde %>% pivot_longer(cols=c( L), names_to = "heat_type", values_to = "energy")
Bdel = Bdel[-which(Bdel$hour %in% c(2,5,8,11,14,17,20,23)),]

Bdwl = Bdw %>% pivot_longer(cols=c(Rn, L,H,G), names_to = "heat_type", values_to = "energy")
Bdwlt = Bdwt %>% pivot_longer(cols=c(Rn, L,H,G), names_to = "heat_type", values_to = "energy")
Bdwl = Bdwl[-which(Bdwl$hour %in% c(2,5,8,11,14,17,20,23)),]

Bde[Bde$month>9 & Bde$Species == "Acer platanoides",c("L","sdL")] = Bde[Bde$month>9 & Bde$Species == "Acer platanoides",c("L","sdL")]/10

g6 = ggplot(data = Bde %>% filter(Species != "TTR")%>% filter(!is.na(month)) %>% ungroup())+
  geom_point(aes(x=hour, y = L))+
  geom_smooth(aes(x=hour, y = L), span = 0.7,se = F, color="black")+
  geom_errorbar(aes(x=hour, ymax= L+ sdL, ymin = L- sdL))+
  facet_grid(month~Species, scales = "free")+
  scale_y_continuous(sec.axis =  sec_axis(~ . *3600/2264.705, name = expression(Flux~","~l~h^{-1})))+
  theme_bw()+
  theme(legend.position = "bottom",legend.title = element_blank(), 
        legend.text=element_text(size=14),
        axis.text=element_text(size=12),
        axis.title=element_text(size=16,face="bold"),
        strip.text.x = element_text(size = 14),
        )+
  xlab(expression(Day~hour))+
  ylab(expression("Latent heat"~","~kWh))

#annotate('text', x = 0, y = 0, label = "Value~is~sigma~R^{2}==0.6 ",parse = TRUE,size=20) +
ggsave(plot = g6, filename = "results/es/Latent_Energy_and_FLux_diurnal_ts_kWh.png", 
       width = 16, height =12, units = "in", dpi = 100, device ="png")


g62 = ggplot(data = Bdwl%>% filter(Species != "TTR")%>% filter(!is.na(month)))+
  geom_point(aes(x=hour, y = energy,group =heat_type,color =heat_type, shape = heat_type))+
  geom_smooth(aes(x=hour, y = energy,group =heat_type,color =heat_type, linetype=heat_type), span = 0.7,se = F)+
  #geom_smooth(aes(x=hour, y = energy,group =heat_type,color =heat_type, linetype=heat_type), span = 0.7,se = F)+
  facet_grid(month~Species, scales = "free")+
  theme_bw()+
  theme(legend.position = "bottom",legend.title = element_blank(), 
        legend.text=element_text(size=14),
        axis.text=element_text(size=12),
        axis.title=element_text(size=16,face="bold"),
        strip.text.x = element_text(size = 14),
  )+
  xlab(expression(Day~hour))+
  ylab(expression("Rn,L,H,G"~","~W~m^{-2}))

ggsave(plot = g62, filename = "results/es/Energy_balance_diurnal_ts_W.png", 
       width = 16, height =12, units = "in", dpi = 100, device ="png")



##### Stacked column per month
Bsum = Bdata %>% mutate(month = month(time)) %>% filter(!is.na(month))%>%
  group_by(id,doy,month, Species) %>% filter(Species != "TTR") %>%
  summarise(Rn = sum(Rne, na.rm = T), L = sum(Le, na.rm=T), 
            G = sum(Ge, na.rm = T), H = sum(He, na.rm = T), d = mean(d,na.rm = T))%>% 
  pivot_longer(cols=c(L), names_to = "heat_type", values_to = "energy") %>%
  group_by(month,id, Species, heat_type) %>% filter(!is.na(month))%>%
  summarise(E = sum(energy, na.rm = T), Rn = sum(Rn, na.rm = T)) 

Bsum[Bsum$month>9 & Bsum$Species == "Acer platanoides",c("E","Rn")] = Bsum[Bsum$month>9 & Bsum$Species == "Acer platanoides",c("E","Rn")]/10

g7 = ggplot(data= Bsum %>% filter(!(id %in% c("218A0088","218A0193","218A0248"))))+
  geom_col(aes(x=id, y = E, fill = heat_type))+
  #geom_errorbar(aes(x = id, ymin = Rn, ymax = Rn))+
  facet_grid(month~Species, scales = "free")+
  theme_bw()+
  theme(legend.position = "bottom",legend.title = element_blank(), 
      legend.text=element_text(size=14),
      axis.text=element_text(size=11),
      axis.title=element_text(size=16,face="bold"),
      strip.text.x = element_text(size = 14))+
  xlab(expression(Tree~id))+
  ylab(expression("Energy absorbed from atmosphere"~","~kWh))
#annotate('text', x = 0, y = 0, label = "Value~is~sigma~R^{2}==0.6 ",parse = TRUE,size=20) +
ggsave(plot = g7, filename = "results/es/Energy_balance_per_tree_monthly_kWh.png", 
       width = 18, height =10, units = "in", dpi = 100, device ="png")


Bavg = Bdata %>% mutate(month = month(time)) %>% filter(!is.na(month))%>%
  group_by(id,doy,month, Species) %>% filter(Species != "TTR") %>%
  summarise(Rn = sum(Rnw, na.rm = T), L = sum(Lw, na.rm=T), 
            G = sum(Gw, na.rm = T), H = sum(Hw, na.rm = T), d = mean(d,na.rm = T))%>% 
  pivot_longer(cols=c(L,H,G), names_to = "heat_type", values_to = "energy") %>%
  group_by(month,id, Species, heat_type) %>% filter(!is.na(month))%>%
  summarise(E = mean(energy, na.rm = T), Rn = mean(Rn, na.rm = T)) 


g72 = ggplot(data= Bavg%>% filter(!(id %in% c("218A0088","218A0193","218A0248"))))+
  geom_col(aes(x=id, y = E, fill = heat_type))+
  geom_errorbar(aes(x = id, ymin = Rn, ymax = Rn))+
  facet_grid(month~Species, scales = "free")+
  theme_bw()+
  theme(legend.position = "bottom",legend.title = element_blank(), 
        legend.text=element_text(size=14),
        axis.text=element_text(size=11),
        axis.title=element_text(size=16,face="bold"),
        strip.text.x = element_text(size = 14))+
  xlab(expression(Tree~id))+
  ylab(expression("Rn,L,H,G"~","~W~m^{-2}))
#annotate('text', x = 0, y = 0, label = "Value~is~sigma~R^{2}==0.6 ",parse = TRUE,size=20) +
ggsave(plot = g72, filename = "results/es/Power_balance_per_tree_monthly_Wm-2.png", 
       width = 18, height =10, units = "in", dpi = 100, device ="png")

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
ggplot(data = BLTN %>% filter(Site == "BOLOTNAYA") %>%filter(Species != "TTR")%>% filter(!(id %in% c("218A0088","218A0193","218A0248")))%>%
         group_by(id, Species)%>% summarise(kg=mean(biomas_stored)))+
  geom_col(aes( x = id,y = kg, fill=Species))+
  facet_wrap(~Species, ncol = 2, scale="free" )+
  #scale_y_continuous(limits=c(-.45,.5))+
  theme_bw()


#Artificial graph of biomass growth according to accumulated LAI
Bgr = Bdata %>% filter(Species != "TTR")  %>% filter(!(id %in% c("218A0088","218A0193","218A0248"))) %>% 
  mutate(NDVIts =  replace(NDVIts, NDVIts > 1, 1) , NDVIts =  replace(NDVIts, NDVIts < -1, -1)) %>% 
  group_by(id, Species,doy) %>%   summarise(bio_proxy = quantile(NDVIts, 0.85,na.rm = T), kg=mean(biomas_stored), n=n()) %>% 
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
  #print(biomas)
  biomas = zoo::na.approx(biomas, x = zoo::index(biomas),  na.rm = T, maxgap = Inf)
  #print(biomas)
  df$biomas[df$id == i] = biomas
}


g8 = ggplot(data = df%>%filter(doy<300))+
  geom_point(aes(x=doy, y=biomas,  group=id),shape =3, size=1.2, alpha=4/10)+
  geom_line(aes(x=doy, y=biomas, group=id, color=id),size=.5)+
  #geom_ma(aes(x=doy, y=biomas, color =id), n=3, linetype=1, size=1)+
  facet_wrap(~Species, scales = "free")+
  theme_bw()+
  theme(legend.position = "none")+
  theme(legend.position = "bottom",legend.title = element_blank(), 
        legend.text=element_text(size=14),
        axis.text=element_text(size=12),
        axis.title=element_text(size=16,face="bold"),
        strip.text.x = element_text(size = 14))+
  xlab(expression(Day~of~the~ year))+
  ylab(expression(C[tree]~","~kg))+guides(colour = guide_legend(nrow = 2))
ggsave(plot = g8, filename = "results/es/Carbon_accumulation_per_tree_ts.png", 
       width = 16, height =12, units = "in", dpi = 100, device ="png")



Bgr2 = Bdata %>% filter(Species != "TTR")  %>% filter(!(id %in% c("218A0088","218A0193","218A0248"))) %>% 
  mutate(NDVIts =  replace(NDVIts, NDVIts > 1, 1) , NDVIts =  replace(NDVIts, NDVIts < -1, -1)) %>% 
  group_by(id, Species,doy) %>%   summarise(bio_proxy = quantile(NDVIts, 0.85,na.rm = T), kg=mean(biomas_stored2, na.rm = T), n=n()) %>% 
  mutate(bio_proxy = replace(bio_proxy,bio_proxy<0,0), bioproxy = cumsum(bio_proxy)) %>% 
  mutate(biomas_stored = kg*bioproxy/max(bioproxy)) 

doy = rep(180:310,unique(Bgr2$id)%>%length )
id = rep(unique(Bgr2$id), rep(311-180,unique(Bgr2$id)%>%length))


df = data.frame(id,doy) %>% left_join(Bgr2, by=c("id","doy"))


df$biomas = 0
for( i in df$id %>% unique()){
  biomas = df$biomas_stored[df$id == i]
  Species = as.factor(df$Species[df$id == i])%>% levels
  df$Species[df$id == i] = Species
  #print(biomas)
  biomas = zoo::na.approx(biomas, x = zoo::index(biomas),  na.rm = T, maxgap = Inf)
  #print(biomas)
  df$biomas[df$id == i] = biomas
}


g82 = ggplot(data = df%>%filter(doy<300))+
  geom_point(aes(x=doy, y=biomas,  group=id),shape =3, size=1.2, alpha=4/10)+
  geom_line(aes(x=doy, y=biomas, group=id, color=id),size=.5)+
  #geom_ma(aes(x=doy, y=biomas, color =id), n=3, linetype=1, size=1)+
  facet_wrap(~Species, scales = "free")+
  theme_bw()+
  theme(legend.position = "none")+
  theme(legend.position = "bottom",legend.title = element_blank(), 
        legend.text=element_text(size=14),
        axis.text=element_text(size=12),
        axis.title=element_text(size=16,face="bold"),
        strip.text.x = element_text(size = 14))+
  xlab(expression(Day~of~the~ year))+
  ylab(expression(C[tree]~","~kg))+guides(colour = guide_legend(nrow = 2))
ggsave(plot = g82, filename = "results/es/Carbon_accumulation_per_tree_ts_SHARP.png", 
       width = 16, height =12, units = "in", dpi = 100, device ="png")

##################################       LAI




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


# LAI, WAI per species
PAI = LAI %>%group_by(id,Species) %>% summarise(PAI = mean(replace(LAIparc, doy>290,NA),na.rm=T), 
                                                WAI =mean(replace(LAIparc, doy<290,NA),na.rm=T))%>% mutate(LAI = PAI -WAI) %>%
  pivot_longer(cols = c("WAI","LAI"), names_to = "index_name", values_to = "index")%>% as.data.table()
PAI[17,5] = 0.52
PAI[18,5] = 2.8

LAI = LAI %>% group_by(id,doy)%>%mutate(PAI =mean(LAIparc,na.rm = T)) 
LAI = LAI %>% left_join(PAI%>%filter(index_name=="WAI")%>%select(id,"index"), by="id")%>%rename(WAI=index)
LAI =LAI %>% mutate(PAI = replace(PAI, doy>290, WAI))
qplot(data=LAI,x=time_ts,y=PAI, color=id)+facet_wrap(~Species)+
  stat_smooth(aes(x=time,y=PAI, color=id,outfit=fit<<-..y..),method =stats::loess, span=.5, n=nrow(LAI)/(LAI$id%>%unique()%>%length()) )
LAI$PAIf = fit
LAI =LAI %>% mutate(PAI = replace(PAIf, PAIf<WAI, WAI))
LAI$PAI[LAI$id=="218A0210" & LAI$doy > 290] = LAI$WAI[LAI$id=="218A0210" & LAI$doy > 290]
LAI = tsibble(LAI, index=time_ts, key=id, regular = T)%>% group_by_key() %>%fill_gaps(.full = T) %>% as.data.frame()

LAI = LAI %>% group_by(id)%>%mutate(PAI = zoo::na.spline(PAI, x = zoo::index(PAI),  na.rm = FALSE, maxgap = Inf))%>%
  mutate(PAI = replace(PAI,PAI>8,8)) %>%mutate(PAI =replace(PAI, PAI<0,0))
LAI$time = LAI$time_ts
LAI = LAI %>% mutate(LAI = PAI-WAI)

# LAI = LAI %>% group_by(id,doy)%>%mutate(PAI = mean(PAI,na.rm=T)) %>%group_by(id)%>% mutate(PAI = predict(loess(PAI ~ time, span = 1, data=.)))



qplot(data=LAI,x=time,y=PAI, color=id)+facet_wrap(~Species)


ggplot(data = PAI )+
  geom_col(aes(x = id, y = index, fill = index_name))+
  facet_wrap(~Species, scales = "free")+
  theme_bw()






# PAI dynamics

g9 = ggplot(data = LAI %>% filter(!(id %in% c("218A0088","218A0193","218A0248"))))+
  #geom_point(aes(x=doy, y = q, color = q ))+
  geom_point(aes(x=time, y = LAIparc, color = id ))+
  #geom_ma(aes(x=doy, y = LAIparc, color = nl ), n=7)+
  geom_smooth(aes(x=time, y = LAIparc, color = id ), span=.1, se=F)+
  #geom_point(aes(x=time, y = q, color = id ))+
  #geom_line(aes(x=doy, y = LAImax, color = Species ))+
  facet_wrap(~Species,nrow = 2)+
  theme_bw()+
  ylab(expression(PAI~","~m^{2}~m^{-2}))+
  xlab("")+
  theme(legend.position = "bottom",
        legend.title = element_blank(), 
        legend.text=element_text(size=14),
        axis.text=element_text(size=12),
        axis.title=element_text(size=16,face="bold"),
        strip.text.x = element_text(size = 14))+
  guides(colour = guide_legend(nrow = 2))


ggsave(plot = g9, filename = "results/es/PAI_dynamics_ts.png", 
       width = 16, height =12, units = "in", dpi = 100, device ="png")

###################### Particles absorption #############################


Cpm <- read_delim("data/Moscow_center_pm25.csv", ";", escape_double = FALSE, 
                  col_types = cols(time = col_datetime(format = "%d.%m.%Y %H:%M")), 
                  trim_ws = TRUE)

#Cpm = Cpm %>% mutate(doy = yday(time),hour=hour(time)) # g10-6 m-3

LAI = LAI %>%  left_join(Cpm, by = c("time")) 

#LAI = LAI %>% left_join(PAI %>% filter(index_name == "WAI")%>% select(id,index), by ="id") %>%
#  rename(WAI  = index)
# 
# for( i in LAI$id %>% unique()){
#   lai = LAI$PAI[LAI$id == i]
#   #Species = as.factor(LAI$Species[LAI$id == i])%>% levels
#   #df$Species[df$id == i] = Species
#   #print(biomas)
#   lai = na.approx(lai, x = index(lai),  na.rm = T, maxgap = Inf)
#   #print(biomas)
#   LAI$PAI[LAI$id == i] = lai
# }

Vdavg = 0.64
Vdmin = 0.25
Vdmax = 1
LAIpm10 = 6

LAI =  LAI %>% 
  mutate(V_avg = Vdavg*(PAI)/(WAI + LAIpm10)) %>%
  mutate(V_min = Vdmin*(PAI)/(WAI + LAIpm10)) %>%
  mutate(V_max = Vdmax*(PAI)/(WAI + LAIpm10)) %>%
  mutate(P_avg =V_avg*pm10*0.036) %>%
  mutate(P_min =V_min*pm10*0.036) %>%
  mutate(P_max =V_max*pm10*0.036)  # g m-2 h-1

pm10 = LAI %>% group_by(id,doy, Species) %>% 
  summarise(P_avg = sum(P_avg, na.rm = T),P_min = sum(P_min, na.rm = T),P_max = sum(P_max, na.rm = T), pm10=mean(pm10,na.rm=T), canopy_area=mean( canopy_area, na.rm = T))
  #group_by(id)%>%  mutate(Pcavg = cumsum(P_avg),Pcmin = cumsum(P_min),Pcmax = cumsum(P_max), pm10 = mean(pm10))



g101 = ggplot(data = pm10)+
  geom_errorbar(aes(x=doy, ymin = P_min*canopy_area, ymax = P_max*canopy_area, color = id), position = position_dodge(width = 0.9)) +
  geom_line(aes(x=doy, y = P_avg*canopy_area, color = id), position = position_dodge(width = 0.9)) +
  geom_line(aes(x=doy, y = pm10*20, ),color = "black", linetype="dashed") +
  facet_wrap(~Species, scales = "free")+
  theme_bw()+
  theme(legend.position = "bottom")+
  theme(legend.position = "bottom",legend.title = element_blank(), 
      axis.text=element_text(size=12),
      axis.title=element_text(size=14,face="bold"),
      strip.text.x = element_text(size = 14))+
  xlab(expression(Day~of~the~year))+
  ylab(expression(PM[10][max]~","~PM[10][avg]~","~PM[10][min]~","~g~d^{-1}))+
  scale_y_continuous(sec.axis =  sec_axis(~ . / 20, name = expression(PM[10]~","~mu~g~m^{-3})))+
  scale_x_continuous(n.breaks = 7)+
  guides(colour = guide_legend(nrow = 2))
  
  
ggsave(plot = g101, filename = "results/es/Pm10_dynamics_per_tree_g_vs_PM10_concetntration.png", 
       width = 16, height =12, units = "in", dpi = 100, device ="png")



pm10 = LAI %>% group_by(id,doy, Species) %>%  
  summarise(P_avg = sum(P_avg*canopy_area, na.rm = T),P_min = sum(P_min*canopy_area, na.rm = T), P_max = sum(P_max*canopy_area, na.rm = T) ,pm10=mean(pm10,na.rm=T))  %>%  
  group_by(id,Species)%>%  mutate(PM10avg = cumsum(P_avg),PM10min = cumsum(P_min), PM10max = cumsum(P_max))%>%as.data.frame()

pm10c = LAI %>% group_by(doy) %>%summarise(pm10 = mean(pm10, na.rm = T))

pm10 = pm10 %>% pivot_longer(values_to = "pm", names_to = "model", cols=c("PM10avg","PM10min","PM10max"))


g102 = ggplot(data = pm10)+
  geom_linerange(aes(x=doy, ymin = PM10min/1000,ymax = PM10max/1000, color= id), position = position_dodge(width = 0.9), alpha=0.4) +
  geom_line(aes(x=doy, y = PM10avg/1000, color= id ), position = position_dodge(width = 0.9)) +
  #geom_point(data = pm10c,aes(x=doy, y = pm10), color="black") +
  #geom_line(data = pm10c,aes(x=doy, y = pm10), color="black") +
  facet_wrap(~Species, scales = "free")+
  theme_bw()+
  theme(legend.position = "bottom",legend.title = element_blank(), 
        legend.text=element_text(size=14),
        axis.text=element_text(size=12),
        axis.title=element_text(size=16,face="bold"),
        strip.text.x = element_text(size = 14))+
  xlab(expression(Day~of~the~year))+
  ylab(expression(PM[10][max]~","~PM[10][avg]~","~PM[10][min]~","~kg))+
  guides(colour = guide_legend(nrow = 2))
ggsave(plot = g102, filename = "results/es/Pm10_accumulation_per_tree_range.png", 
       width = 16, height =12, units = "in", dpi = 100, device ="png")


pm10 = LAI %>% group_by(id,doy, Species) %>%  
  summarise(P_avg = sum(P_avg*canopy_area, na.rm = T),P_min = sum(P_min*canopy_area, na.rm = T), P_max = sum(P_max*canopy_area, na.rm = T) ,pm10=mean(pm10,na.rm=T))  %>%  
  group_by(id,Species) %>% mutate(PM10avg = cumsum(P_avg),PM10min = cumsum(P_min), PM10max = cumsum(P_max))%>%group_by(Species,doy)%>%
  summarise(PM10avg = mean(PM10avg),PM10min = mean(PM10min), PM10max = mean(PM10max))

g103 = ggplot(data = pm10)+
  geom_point(aes(x=doy, y = PM10avg/1000), color = "green", shape=2) +
  geom_line(aes(x=doy, y = PM10avg/1000), color = "green") +
  geom_point(aes(x=doy, y = PM10min/1000 ), color = "blue", shape=3) +
  geom_line(aes(x=doy, y =PM10min/1000), color = "blue") +
  geom_point(aes(x=doy, y = PM10max/1000), color = "red", shape=4) +
  geom_line(aes(x=doy, y = PM10max/1000), color = "red") +
  facet_wrap(~Species, scales = "free")+
  theme_bw()+
  theme(legend.position = "bottom",legend.title = element_blank(), 
        legend.text=element_text(size=14),
        axis.text=element_text(size=12),
        axis.title=element_text(size=16,face="bold"),
        strip.text.x = element_text(size = 14))+
  xlab(expression(Day~of~the~year))+
  ylab(expression(PM[10][max]~","~PM[10][avg]~","~PM[10][min]~","~kg))
ggsave(plot = g103, filename = "results/es/Pm10_accumulation_per_Species.png", 
       width = 16, height =12, units = "in", dpi = 100, device ="png")




pm10 = pm10 %>% pivot_longer(values_to = "pm10", names_to = "model", cols=c("Pcavg","Pcmin","Pcmax"))


ggplot(data = pm10)+
  geom_point(aes(x=doy, y = Pcavg,color=Species)) +
  geom_line(aes(x=doy, y = Pcavg,color=Species )) +
  geom_point(aes(x=doy, y = Pcmin,color=Species), shape=3) +
  geom_line(aes(x=doy, y = Pcmin,color=Species)) +
  geom_point(aes(x=doy, y = Pcmax,color=Species), shape=2) +
  geom_line(aes(x=doy, y = Pcmax,color=Species)) +
  facet_wrap(~Species, scales = "free")+
  theme_bw()+
  theme(legend.position = "none")


pm10sum = LAI %>% group_by(id, Species) %>% 
  summarise(P_max= sum(P_max*canopy_area,na.rm=T),P_avg = sum(P_avg*canopy_area,na.rm=T),P_min = sum(P_min*canopy_area,na.rm=T)) 



ggplot(data = pm10sum) +
  geom_crossbar(aes(x=id,  y= P_avg,ymin=P_min,ymax=P_max, color=Species))+
  facet_wrap(~Species, scales = "free")+
  theme_bw()



##### Разброс температур за день в виде таблицы
# Графики по влажности - пример пара дней и диурнал
# Добавить график концентрации на граф с пылью
# Добавить график кумуляты эвапотранспирации и сравнить с осадками с площадки 100м2
# Итоговый граф по энергии в таблицу
#Overall table of services


####################################### Summary table of services ###############################

Bevap_sum = Bevap %>%  group_by(id, Species) %>% summarise(evapc = sum(evap), cprcp = sum(prcp), ca = mean(canopy_area, na.rm=T)) %>% filter(Species != "TTR")%>%
  mutate(evapc = evapc/ca)

Bevap_sum$cprcp = max(Bevap_sum$cprcp)
Bevap_sum = Bevap_sum %>% mutate(prcp_rat = evapc/cprcp)

Benrsum = Bsum %>% group_by(id,Species) %>% summarise(E=sum(E))



Bcarbsum = Bdata %>% filter(Site == "BOLOTNAYA") %>%filter(Species != "TTR")%>% 
  group_by(id, Species)%>% summarise(Cstored=mean(biomas_stored2), ca = mean(canopy_area), Cm2 = Cstored/ca)
                                     

Bpaisum = LAI %>%group_by(id,Species) %>% summarise(PAI = mean(replace(LAIparc, doy>290,NA),na.rm=T), 
                                                WAI =mean(replace(LAIparc, doy<290,NA),na.rm=T))%>% mutate(LAI = PAI -WAI)
Bpaisum[9,4] = 0.52
Bpaisum[9,5] = 2.8

pm10sum = LAI %>% group_by(id, Species) %>% 
  summarise(P_max= sum(P_max*canopy_area,na.rm=T),P_avg = sum(P_avg*canopy_area,na.rm=T),P_min = sum(P_min*canopy_area,na.rm=T)) 


Bsumm = Bdata %>% group_by(id, Species,age_group_index ) %>% summarise(tree_height = mean(tree_height, na.rm = T), d=mean(d, na.rm = T),
                growth = mean(growth, na.rm = T), BEF=mean(BEF,na.rm = T), BCEF=mean(BCEF,na.rm = T), R=mean(R,na.rm = T)) %>%
filter(!is.na(age_group_index))


Bsumtable = left_join(Bcarbsum,Bevap_sum, by=c("id","Species"))%>%left_join(Benrsum %>% select(id,Species,E), by=c("id","Species")) %>%
  left_join(pm10sum, by=c("id","Species")) %>% left_join(Bpaisum, by=c("id","Species")) %>%left_join(Bsumm,by=c("id","Species") ) %>% arrange(Species,id) %>%group_by(Species)%>%
  gt(rowname_col = "id") %>%  tab_header(
    title="Summative of ecosystem services produced by each tree per vegetation period"
  )

gtsave(as_raw_html(Bsumtable) , file="results/es/Summative_table.html")






#############################################################################################################


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

