RUDN = AllData %>% filter(Site == "RUDN")


# 5 Species 5 Graphs

Sys.setlocale("LC_ALL","English")

ids = RUDN %>% mutate(month = month(time)) %>% filter(month==8, Flux < 12)%>% 
  group_by(Species,id)%>%summarise(n = n()) %>% group_by(Species) %>% summarise(ids = id[which.max(n)]) %>% select(ids)
ids =ids[[1]]
ids[1] = "218A0142"
ggplot(data = RUDN %>% mutate(month = month(time)) %>% filter(month==8, Flux < 7.5, id %in% ids))+
  geom_line(aes(x=time, y=Flux, color=id,shape=Species ))+
  geom_point(aes(x=time, y=Flux, color=id,shape=Species ), size=1)+
  facet_wrap(~Species, nrow=3)+
  theme_bw()


ggplot(data = RUDN %>% mutate(month = month(time)) %>% filter(month==8, NDVIc < 1,NDVIc > 0,hour>8,hour <15, id %in% ids))+
  geom_smooth(aes(x=time, y=NDVIc, color=id,shape=Species ))+
  geom_point(aes(x=time, y=NDVIc, color=id,shape=Species ), size=1)+
  facet_wrap(~Species, nrow=3)+
  theme_bw()


ggplot(data = RUDN %>% mutate(month = month(time)) %>% filter(month==8, id %in% ids))+
  geom_smooth(aes(x=time, y=phi+120, color=id,shape=Species ))+
  geom_point(aes(x=time, y=phi+120, color=id,shape=Species ), size=1)+
  facet_wrap(~Species, nrow=3, scales = "free")+
  theme_bw()

ggplot(data = RUDN %>% mutate(month = month(time)) %>% filter(month==8, id %in% ids))+
  #geom_smooth(aes(x=time, y=tair, color=id,shape=Species ))+
  geom_point(aes(x=time, y=tair, color=id,shape=Species ), size=1)+
  geom_line(aes(x=time, y=tair, color=id,shape=Species ), size=1)+
  facet_wrap(~Species, nrow=3)+
  theme_bw()

ggplot(data = RUDN %>% mutate(month = month(time)) %>% filter(month==8,id %in% ids))+
  geom_line(aes(x=time, y=VPD, color=id,shape=Species ))+
  geom_point(aes(x=time, y=VPD, color=id,shape=Species ), size=1)+
  facet_wrap(~Species, nrow=3, scales = "free")+
  theme_bw()



ggplot(data=RUDN %>% mutate(month = month(time)) %>% filter(id %in% c("218A0064","218A0142"), doy < 165, year<2020))+
  geom_point(aes(x=time, y=phi+120, color=id))+
  theme_bw()

c("218A0137")

ggplot()+
  geom_point(data = RUDN %>% filter(id == "218A0137", year <2020, doy > 200, doy <220), aes(x=time, y=Flux), color=2, size=1)+
  geom_line(data = RUDN %>% filter(id == "218A0137", year <2020, doy > 200, doy <220), aes(x=time, y=Flux), color=2)+
  geom_point(data = AllData %>% filter(Site == "SCHOOL1234", Species=="Populus nigra", year <2020, Flux <20,doy > 200, doy <220),
             aes(x=time, y=Flux),color=3, size=1)+
  geom_line(data = AllData %>% filter(Site == "SCHOOL1234", Species=="Populus nigra", year <2020, Flux <20,doy > 200, doy <220),
             aes(x=time, y=Flux),color=3)+
  theme_bw()
