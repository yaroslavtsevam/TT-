####################### SUMMARY TABLES GENERATOR ################################33


library(summarytools)

SummaryTableFlux = AllData%>%group_by(Site, Species, doy, id) %>% mutate(update = length(id))%>%
  summarise(sFlux = sum(Flux, na.rm=T), readings = mean(update), diam = mean(diam, na.rm=T)) %>%
  mutate(suFlux = sFlux*readings/24) %>% filter(!is.na(suFlux)) %>% filter(!is.infinite(suFlux))%>%
  group_by(Species)%>%filter(!out_of_two_sigma(suFlux))%>%
  group_by(Site, Species, doy) %>% summarise(Flux = mean(suFlux, na.rm=T), d = mean(diam))

SummaryTableFlux %>% as.data.frame %>% group_by(Site, Species) %>% 
  summarise(mean = mean(Flux, na.rm=T), sd =sd(Flux, na.rm=T), max=max(Flux, na.rm=T), min=min(Flux, na.rm=T) ) %>%
  as.data.frame()


t = SummaryTableFlux %>% filter(Site == "RUDN", Species == "Acer platanoides ")%>% as.data.frame()

summary(t)

SummaryTableNDVI = AllData %>% group_by(Site,Species,doy, id) %>% filter(NDVI > 0 & NDVI < 0.97) %>%
  summarise( dNDVI = mean(NDVI, na.rm=T))  %>% filter(!out_of_two_sigma(dNDVI))

SummaryTableNDVI = SummaryTableNDVI %>% as.data.frame() %>% group_by(Site, Species) %>% 
  summarise(mean = mean(dNDVI, na.rm=T), sd =sd(dNDVI, na.rm=T), 
            max=max(dNDVI, na.rm=T), min=min(dNDVI, na.rm=T) ) %>%
  as.data.frame()

SummaryTableNDVIm = SummaryTableNDVI %>% as.data.frame() %>% group_by(Site, Species,month) %>% 
  summarise(mean = mean(dNDVI, na.rm=T), sd =sd(dNDVI, na.rm=T), 
            max=max(dNDVI, na.rm=T), min=min(dNDVI, na.rm=T) ) %>%
  as.data.frame()




SummaryTableHz = AllData%>%group_by(Site, Species, doy, id) %>%filter(!out_of_two_sigma(Hz)) %>% 
  summarise(hz = mean((46000-Hz)/(Hz+46000)*50, na.rm=T)) %>%na.omit() %>%
  filter(!is.infinite(hz))


SummaryTableHz = SummaryTableHz %>% as.data.frame() %>% 
  group_by(Site, Species) %>%   summarise(mean = mean(hz, na.rm=T), 
                                          sd =sd(hz, na.rm=T), max=max(hz, na.rm=T), min=min(hz, na.rm=T) ) %>%
  as.data.frame()



SummaryTableNDVI = AllData %>% group_by(Site,Species,doy, id, ) %>% filter(NDVI > 0 & NDVI < 0.97) %>%  
  summarise( mNDVI = quantile(NDVI,0.9, na.rm=T), sNDVI = sd(NDVI, na.rm=T), Flux = quantile(Flux,0.9, na.rm=T), VTAscore = mean(VTAscore)) %>% 
  group_by(Site,Species) %>% 
  summarise( NDVI = mean(mNDVI, na.rm=T), sNDVI = sd(mNDVI, na.rm=T),mxNDVI = max(mNDVI, na.rm=T) ,mnNDVI = min(mNDVI, na.rm=T))

SummaryTableNDVI%>%as.data.frame()

SummaryTableVTA = SummaryTableNDVI %>% group_by(VTAscr,Species)%>%
  summarise(mxNDVI = mean(mxNDVI),mnNDVI = mean(mnNDVI),mNDVI = mean(mNDVI), mFlux=mean(mFlux))%>%
  arrange(Species)%>%as.data.frame()

SummaryTablePRI = AllData %>% group_by(Site,Species,doy, id) %>% filter(PRI > 0 & PRI < 0.97) %>%
  summarise( mPRI = quantile(PRI,0.2, na.rm=T),VTAscore = mean(VTAscore) ) %>% group_by(Site,Species, id) %>%
  summarise( mPRI = quantile(mPRI,0.2, na.rm=T),VTAscore = mean(VTAscore) )

models = SummaryTableNDVI %>% group_by(Species) %>% mutate(TTscr = log(mFlux/mNDVI))%>% na.exclude %>% 
  filter(!is.infinite(mFlux)) %>% filter(!out_of_two_sigma(TTscr)) %>%  summarise(R2 = cor(sNDVI,VTAscr)^2)



SummaryTableOptimalCritical = AllData %>% group_by(Site,Species,doy, id, age_group) %>% 
  filter(NDVI > 0 & NDVI < 0.97) %>% filter(month <9)%>% filter(!is.infinite(Flux))%>%
  summarise( optNDVI = quantile(NDVI,0.9, na.rm=T), critNDVI = quantile(NDVI,0.2, na.rm=T),
             optFlux = quantile(Flux,0.9, na.rm=T), critFlux = quantile(Flux,0.2, na.rm=T))%>%
  group_by(Species, age_group) %>% 
  summarise( optNDVI = mean(optNDVI, na.rm=T), critNDVI = mean(critNDVI, na.rm=T),
             optFlux = mean(optFlux, na.rm=T), critFlux = mean(critFlux,0.2, na.rm=T))

SummaryTableOptimalCritical %>%as.data.frame()



SummaryTableANOVA = AllData %>% group_by(Site,Species,doy, id, age_group, age_group_index) %>% 
  filter(NDVI > 0 & NDVI < 0.97) %>% filter(month <9)%>% filter(!is.infinite(Flux))%>%
  summarise( optNDVI = quantile(NDVI,0.9, na.rm=T), critNDVI = quantile(NDVI,0.2, na.rm=T),
             optFlux = quantile(Flux,0.9, na.rm=T), critFlux = quantile(Flux,0.2, na.rm=T))


SummaryTableANOVA = AllData %>% group_by(Site,Species,doy, id, age_group, age_group_index, VTAscore) %>% 
  filter(NDVI > 0 & NDVI < 0.97) %>% filter(month <9)%>% filter(!is.infinite(Flux))%>%
  summarise( optNDVI = quantile(NDVI,0.8, na.rm=T), critNDVI = quantile(NDVI,0.2, na.rm=T),
             optFlux = quantile(Flux,0.6, na.rm=T), critFlux = quantile(Flux,0.2, na.rm=T))

modFLux = lm(data = SummaryTableANOVA, optFlux~Species+VTAscore)
anova(lm(data = SummaryTableANOVA, optFlux~Species+VTAscore))
anova(lm(data = SummaryTableANOVA, optNDVI~Species+VTAscore))
anova(lm(data = SummaryTableANOVA, critFlux~Species+VTAscore))
anova(lm(data = SummaryTableANOVA, critNDVI~Species+VTAscore))

TablesGraph = SummaryTableANOVA %>% group_by(Species)%>%do(graph = 
                                                             ggplot(data = .)+
                                                             geom_boxplot(aes(y=optFlux, x = as.factor(age_group), ymax = 20))+
                                                             ylim(c(0,20))+
                                                             scale_color_lancet()+
                                                             theme_bw()
                                                           
)

TablesGraph[7,2][[1]]


### Summary  graph  Age vs Flux

SummaryTableANOVAm = AllData %>% filter(Species %in% c("Acer platanoides","Betula pendula","Larix sibirica", "Pinus sylvestris",
                                                       "Picea abies","Quercus robur","Tilia cordata")) %>% group_by(Site,Species,doy, id, age_group, age_group_index) %>% 
  filter(NDVI > 0 & NDVI < 0.97) %>% filter(month <9)%>% filter(!is.infinite(Flux))%>%
  summarise( optNDVI = quantile(NDVI,0.9, na.rm=T), critNDVI = quantile(NDVI,0.2, na.rm=T),
             optFlux = quantile(Flux,0.9, na.rm=T), critFlux = quantile(Flux,0.2, na.rm=T))%>%
  group_by(Species,age_group,age_group_index)%>%na.exclude()%>%
  summarise(Fluxm = mean(optFlux, na.rm=T),  Flux_er = (CI(optFlux)[1]-CI(optFlux)[3])/2, NDVIm = mean(optNDVI, na.rm=T), 
            NDVIer = (CI(optNDVI)[1]-CI(optNDVI)[3])/2,m = length(optFlux) )


ggplot(data = SummaryTableANOVAm%>%filter(Species %in% c("Acer platanoides","Betula Pendula","Larix sibirica",
                                                         "Pinus sylvestris","Picea abies","Quercus robur","Tilia cordata")))+
  geom_line(aes(y=Fluxm, x = age_group_index, group = Species, color = Species),position = position_dodge(width = 0.1), size=1)+
  geom_errorbar(aes(ymin=Fluxm-Flux_er, ymax=Fluxm+Flux_er ,
                    x=age_group_index, group=Species, color=Species),position = position_dodge(width = 0.1), size=1, width = .2) +
  
  ylim(c(0,20))+
  scale_color_lancet()+
  theme_bw()



### Summary  graph  VTA vs Flux

SummaryTableANOVAm = AllData %>% filter(Species %in% c("Acer platanoides","Betula pendula",
                                                       "Larix sibirica", "Pinus sylvestris","Picea abies","Quercus robur","Tilia cordata")) %>% 
  group_by(Site,Species,doy, id, VTAscore) %>% 
  filter(NDVI > 0 & NDVI < 0.97) %>% filter(month <9)%>% filter(!is.infinite(Flux))%>%
  summarise( optNDVI = quantile(NDVI,0.9, na.rm=T), critNDVI = quantile(NDVI,0.2, na.rm=T),
             optFlux = quantile(Flux,0.9, na.rm=T), critFlux = quantile(Flux,0.2, na.rm=T))%>%
  group_by(Species,VTAscore)%>%na.exclude()%>%
  summarise(Fluxm = mean(optFlux, na.rm=T),  Flux_er = (CI(optFlux)[1]-CI(optFlux)[3])/2, NDVIm = mean(optNDVI, na.rm=T), 
            NDVIer = (CI(optNDVI)[1]-CI(optNDVI)[3])/2,m = length(optFlux) )


ggplot(data = SummaryTableANOVAm%>%filter(Species %in% c("Acer platanoides","Betula pendula",
                                                         "Larix sibirica", "Pinus sylvestris","Picea abies","Quercus robur","Tilia cordata")))+
  geom_line(aes(y=Fluxm, x = VTAscore, group = Species, color = Species),position = position_dodge(width = 0.1), size=1)+
  geom_errorbar(aes(ymin=Fluxm-Flux_er, ymax=Fluxm+Flux_er ,
                    x=VTAscore, group=Species, color=Species),position = position_dodge(width = 0.1), size=1, width = .2) +
  
  ylim(c(0,5))+
  ylab("Скорость сокотечения л/ч")+
  xlab("Категория состояния дерева")+
  labs(color = "Виды")+
  scale_color_lancet()+
  theme_bw()

### Summary graph  VTA vs NDVI
ggplot(data = SummaryTableANOVAm%>%filter(Species %in% c("Acer platanoides","Betula pendula",
                                                         "Larix sibirica", "Pinus sylvestris","Picea abies","Quercus robur","Tilia cordata")))+
  geom_line(aes(y=NDVIm, x = VTAscore, group = Species, color = Species),position = position_dodge(width = 0.1), size=1)+
  geom_errorbar(aes(ymin=NDVIm-NDVIer, ymax=NDVIm+NDVIer ,
                    x=VTAscore, group=Species, color=Species),
                position = position_dodge(width = 0.1), size=1, width = .2)+
  ylim(c(0,1))+
  ylab("NDVI")+
  labs(color = "Виды")+
  xlab("Категория состояния дерева")+
  scale_color_lancet()+
  theme_bw()
