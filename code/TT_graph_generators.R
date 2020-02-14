
writeFluxTSALg = function(AllData){
  
  plotbunch = AllData%>%group_by(load, Species, doy, id) %>% mutate(update = length(id))%>%
    summarise(sFlux = sum(Flux, na.rm=T), readings = mean(update), diam = mean(diam, na.rm=T), 
              time=mean(time,na.rm=T), load_score = mean(load_score,na.rm=T)) %>%
    mutate(suFlux = sFlux*readings/24) %>%
    group_by(load,Species, doy) %>% summarise(Flux = mean(suFlux, na.rm=T), d = mean(diam), 
                                              time=as.Date(mean(time,na.rm=T)), load_score = mean(load_score,na.rm=T)) %>% 
    group_by(Species)%>%
    do(Flux = ggplot(data = .)
       +geom_smooth(aes(x=time,y=Flux, group = load, color=load, fill = load))
       #+geom_ribbon(aes(x=time,y=Flux, group = load, color=load, se=FALSE))
       +geom_point(aes(x=time,y=Flux, group = load, color=load),size=.1)
       +scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 week",
                     limits = c(as.Date("01.06.2019", "%d.%m.%Y"), as.Date("01.10.2019", "%d.%m.%Y")), name = NULL)
       +scale_y_continuous( limits = c(0,75), name = expression(Скорость~" "~сокотечения~" "~л~д^-1))
       +scale_color_lancet(guide=FALSE)
       +scale_fill_lancet(name = "Уровень антропогенной нагрузки")
       +theme_bw()
       +theme(legend.position="bottom")
       
    )
  
  for(i in 1:nrow(plotbunch)){
    filename = paste("Load_anhtrop",plotbunch[i,1],names(plotbunch)[2],".png",sep="_")
    graph = plotbunch[i,2][[1]][[1]]
    ggsave(filename, graph,device = "png",width=8,height=6,units="in")
  } 
}

writeFluxTSALinSg = function(AllData){
  
  plotbunch = AllData%>%group_by(Site, insite_load,Species, doy, id) %>% mutate(update = length(id))%>%
    summarise(sFlux = sum(Flux, na.rm=T), readings = mean(update), diam = mean(diam, na.rm=T), 
              time=mean(time,na.rm=T), load_score = mean(insite_load_score,na.rm=T)) %>%
    mutate(suFlux = sFlux*readings/24) %>%
    group_by(Site,insite_load,Species, doy) %>% summarise(Flux = mean(suFlux, na.rm=T), d = mean(diam), 
                                                          time=as.Date(mean(time,na.rm=T)), load_score = mean(load_score,na.rm=T)) %>% 
    group_by(Site,Species)%>%
    do(Flux = ggplot(data = .)
       +geom_smooth(aes(x = time,y = Flux, group = insite_load, color = insite_load, fill = insite_load))
       #+geom_ribbon(aes(x=time,y=Flux, group = load, color=load, se=FALSE))
       +geom_point(aes(x=time,y=Flux, group = insite_load, color = insite_load),size = .1)
       +scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 week",
                     limits = c(as.Date("01.06.2019", "%d.%m.%Y"), as.Date("01.10.2019", "%d.%m.%Y")), name = NULL)
       +scale_y_continuous( limits = c(0,75), name = expression(Скорость~" "~сокотечения~" "~л~д^-1))
       +scale_color_lancet(guide=FALSE, )
       +scale_fill_lancet(name = "Уровень антропогенной нагрузки внутри участка")
       +theme_bw()
       +theme(legend.position="bottom")
       
    )
  
  for(i in 1:nrow(plotbunch)){
    filename = paste("Load_antr_in_site_",plotbunch[i,1],plotbunch[i,2],".png",sep="_")
    graph = plotbunch[i,3][[1]][[1]]
    ggsave(filename, graph,device = "png",width=8,height=6,units="in")
  } 
}

writeFluxTSg = function(data, sitename){
  plotbunch = data%>%group_by(Species, doy, id) %>% mutate(update = length(id))%>%
    summarise(sFlux = sum(Flux, na.rm=T), readings = mean(update), diam = mean(diam, na.rm=T), time=mean(time,na.rm=T)) %>%
    mutate(suFlux = sFlux*readings/24) %>%
    group_by(Species, doy) %>% summarise(Flux = mean(suFlux, na.rm=T), d = mean(diam), time=as.Date(mean(time,na.rm=T))) %>% group_by(Species)%>%
    do(Flux = ggplot(data = .)
       +geom_smooth(aes(x=time,y=Flux))
       +geom_point(aes(x=time,y=Flux),size=.1)
       +scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 week",
                     limits = c(as.Date("01.05.2019", "%d.%m.%Y"),NA), name = NULL)
       +scale_y_continuous( limits = c(0,75), name = expression(Скорость~" "~сокотечения~" "~л~д^-1))
       +theme_bw()
       
    )
  
  
  #sitename = "RUDN"
  for(i in 1:nrow(plotbunch)){
    filename = paste(sitename,plotbunch[i,1],names(plotbunch)[2],".png",sep="_")
    graph = plotbunch[i,2][[1]][[1]]
    ggsave(filename, graph,device = "png",width=5.83,height=4.13,units="in")
  } 
}




writeFluxTSTTg = function(data, sitename){
  plotbunch = data%>%group_by(Species, doy, id) %>% mutate(update = length(id))%>%
    summarise(sFlux = sum(Flux, na.rm=T), readings = mean(update), diam = mean(diam, na.rm=T) ,time=mean(time,na.rm=T)) %>%
    mutate(suFlux = sFlux*readings/24) %>%
    group_by(id, doy) %>% summarise(Flux = mean(suFlux, na.rm=T), d = mean(diam),time=mean(time,na.rm=T)) %>% group_by(id)%>%
    do(Flux = ggplot(data = .)
       +geom_smooth(aes(x=as.Date(time),y=Flux))
       +geom_point(aes(x=as.Date(time),y=Flux),size=.1)
       +scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 week",
                     limits = c(as.Date("01.05.2019", "%d.%m.%Y"),NA), name = NULL)
       +scale_y_continuous( limits = c(0,75), name = expression(Скорость~" "~сокотечения~" "~л~д^-1))
       +theme_bw()
       +theme_bw())
  
  
  #sitename = "RUDN"
  for(i in 1:nrow(plotbunch)){
    filename = paste(sitename,plotbunch[i,1],names(plotbunch)[2],".png",sep="_")
    graph = plotbunch[i,2][[1]][[1]]
    ggsave(filename, graph,device = "png",width=5.83,height=4.13,units="in")
  } 
}


#plotbunch[15,2][[1]]

writeFluxDiurnalg = function(data, sitename){
  months_n = factor(c("Ягварь","Февраль","Март","Апрель","Май","Июнь", "Июль","Август","Сентябрь", "Октябрь","Ноябрь","Декабрь"), 
                    levels = c("Ягварь","Февраль","Март","Апрель","Май","Июнь", "Июль","Август","Сентябрь", "Октябрь","Ноябрь","Декабрь"))
  data = data%>%mutate(month = month(time))
  plotbunch = data%>%group_by(Species, hour, month,id) %>% filter(month<11 & month>4)%>%
    summarise(Flux = mean(Flux, na.rm=T)) %>% group_by(Species) %>% 
    mutate(months = months_n[month]) %>%
    do(DiurnalFlux = ggplot(data = .)
       +geom_smooth(aes(x=hour,y=Flux))
       +geom_point(aes(x=hour,y=Flux),size=.1)
       +scale_x_continuous(limits = c(0,24), breaks =c(0,3,6,9,12,15,18,21) ,name = expression(Часы~суток))
       +scale_y_continuous( limits = c(0,10), name = expression(Скорость~" "~сокотечения~" "~л~ч^-1))
       +facet_wrap(~months,nrow=2,ncol=3,strip.position = "bottom" )
       +theme_bw())
  
  
  #sitename = "RUDN"
  for(i in 1:nrow(plotbunch)){
    filename = paste(names(plotbunch)[2],plotbunch[i,1],sitename,".png",sep="_")
    graph = plotbunch[i,2][[1]][[1]]
    ggsave(filename, graph,device = "png",width=5.83,height=4.13,units="in")
  } 
}

writeFluxDiurnalALg = function(AllData){
  
  
  months_n = factor(c("Ягварь","Февраль","Март","Апрель","Май","Июнь", "Июль","Август","Сентябрь", "Октябрь","Ноябрь","Декабрь"), 
                    levels = c("Ягварь","Февраль","Март","Апрель","Май","Июнь", "Июль","Август","Сентябрь", "Октябрь","Ноябрь","Декабрь"))
  
  AllData = AllData%>%mutate(month = month(time))
  plotbunch = AllData%>%group_by(load,Species, hour, month,id) %>% filter(month<11 & month>4)%>%
    summarise(Flux = mean(Flux, na.rm=T)) %>% group_by(Species) %>% 
    mutate(months = months_n[month]) %>%
    do(DiurnalFlux = ggplot(data = .)
       +geom_smooth(aes(x=hour,y=Flux, group = load, color=load, fill = load))
       +geom_point(aes(x=hour,y=Flux, group = load,color=load),size=.1)
       +scale_x_continuous(limits = c(0,24), breaks =c(0,3,6,9,12,15,18,21) ,name = expression(Часы~суток))
       +scale_y_continuous( limits = c(0,10), name = expression(Скорость~" "~сокотечения~" "~л~ч^-1))
       +facet_wrap(~months,nrow=2,ncol=3,strip.position = "bottom" )
       +scale_color_lancet(guide=FALSE)
       +scale_fill_lancet(name = "Уровень антропогенной нагрузки")
       +theme_bw()
       +theme(legend.position="bottom"))
  
  for(i in 1:nrow(plotbunch)){
    filename = paste("Load_anhtrop",plotbunch[i,1],names(plotbunch)[2],".png",sep="_")
    graph = plotbunch[i,2][[1]][[1]]
    ggsave(filename, graph,device = "png",width=8,height=6,units="in")
  } 
}


writeFluxDiurnalALinSg = function(AllData){
  
  
  months_n = factor(c("Ягварь","Февраль","Март","Апрель","Май","Июнь", "Июль","Август","Сентябрь", "Октябрь","Ноябрь","Декабрь"), 
                    levels = c("Ягварь","Февраль","Март","Апрель","Май","Июнь", "Июль","Август","Сентябрь", "Октябрь","Ноябрь","Декабрь"))
  
  AllData = AllData%>%mutate(month = month(time))
  plotbunch = AllData%>%group_by(insite_load,Site,Species, hour, month,id) %>% filter(month<11 & month>4)%>%
    summarise(Flux = mean(Flux, na.rm=T)) %>% group_by(Site,Species) %>% 
    mutate(months = months_n[month]) %>%
    do(DiurnalFlux = ggplot(data = .)
       +geom_smooth(aes(x=hour,y=Flux, group = insite_load, color=insite_load, fill = insite_load))
       +geom_point(aes(x=hour,y=Flux, group = insite_load,color=insite_load),size=.1)
       +scale_x_continuous(limits = c(0,24), breaks =c(0,3,6,9,12,15,18,21) ,name = expression(Часы~суток))
       +scale_y_continuous( limits = c(0,10), name = expression(Скорость~" "~сокотечения~" "~л~ч^-1))
       +facet_wrap(~months,nrow=2,ncol=3,strip.position = "bottom" )
       +scale_color_lancet(guide=FALSE)
       +scale_fill_lancet(name = "Уровень антропогенной нагрузки внутри участка")
       +theme_bw()
       +theme(legend.position="bottom"))
  
  for(i in 1:nrow(plotbunch)){
    filename = paste("Load_anhtrop",plotbunch[i,1],plotbunch[i,2],".png",sep="_")
    graph = plotbunch[i,3][[1]][[1]]
    ggsave(filename, graph,device = "png",width=8,height=6,units="in")
  } 
}

writeFluxDiurnalTTg = function(data, sitename){
  months_n = factor(c("Ягварь","Февраль","Март","Апрель","Май","Июнь", "Июль","Август","Сентябрь", "Октябрь","Ноябрь","Декабрь"), 
                    levels = c("Ягварь","Февраль","Март","Апрель","Май","Июнь", "Июль","Август","Сентябрь", "Октябрь","Ноябрь","Декабрь"))
  data = data%>%mutate(month = month(time))
  plotbunch = data%>%group_by(Species, hour, month,id) %>% filter(month<11 & month>4)%>%
    group_by(id) %>% mutate(months = months_n[month]) %>%
    do(DiurnalFlux = ggplot(data = .)
       +geom_smooth(aes(x=hour,y=Flux))
       +geom_point(aes(x=hour,y=Flux),size=.1)
       +scale_x_continuous(limits = c(0,24), breaks =c(0,3,6,9,12,15,18,21) ,name = expression(Часы~суток))
       +scale_y_continuous( limits = c(0,10), name = expression(Скорость~" "~сокотечения~" "~л~ч^-1))
       +facet_wrap(~months,nrow=2,ncol=3,strip.position = "bottom" )
       +theme_bw())
  
  
  #sitename = "RUDN"
  for(i in 1:nrow(plotbunch)){
    filename = paste(names(plotbunch)[2],plotbunch[i,1],sitename,".png",sep="_")
    graph = plotbunch[i,2][[1]][[1]]
    ggsave(filename, graph,device = "png",width=5.83,height=4.13,units="in")
  } 
}


#plotbunch[5,2][[1]]

writeNdviTSg = function(data, sitename){
  
  plotbunch = data%>%group_by(Species, doy, id) %>% filter(NDVI >0 & NDVI <1)%>%
    summarise(dNDVI = mean(NDVI, na.rm=T), ndvi_med = quantile(NDVI,0.5,na.rm=T),
              ndvi_max = quantile(NDVI,0.95,na.rm=T), ndvi_v = NDVI[which.max(VPD)],
              ndvi_l = NDVI[which.max(b_G_550)], time = mean(time, na.rm = T)) %>%na.omit()%>%filter(!out_of_two_sigma(dNDVI))%>%
    group_by(Species) %>%
    do(NDVI = ggplot(data = .)
       +geom_smooth(aes(x=as.Date(time),y=ndvi_max), method="loess", span=.8)
       +geom_point(aes(x=as.Date(time),y=ndvi_max),size=.1, alpha=1/10)
       +scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 week",
                     limits = c(as.Date("01.05.2019", "%d.%m.%Y"),as.Date("01.10.2019", "%d.%m.%Y")), name = NULL)
       +scale_y_continuous( limits = c(0,1), name = expression(Вегетационный~" "~индекс~" "~NDVI))
       +theme_bw())
  
  
  #sitename = "RUDN"
  for(i in 1:nrow(plotbunch)){
    filename = paste(names(plotbunch)[2],plotbunch[i,1],sitename,".png",sep="_")
    graph = plotbunch[i,2][[1]][[1]]
    ggsave(filename, graph,device = "png",width=5.83,height=4.13,units="in")
  } 
}

#plotbunch[5,2][[1]]
writeNdviTSTTg = function(data, sitename){
  
  plotbunch = data%>%group_by(Species, doy, id) %>% filter(NDVI >0 & NDVI <0.97)%>%
    summarise(dNDVI = mean(NDVI, na.rm=T), ndvi_med = quantile(NDVI,0.5,na.rm=T),
              ndvi_max = quantile(NDVI,0.9,na.rm=T), ndvi_v = NDVI[which.max(VPD)],
              ndvi_l = NDVI[which.max(b_G_550)], time = mean(time, na.rm = T)) %>%na.omit()%>%filter(!out_of_two_sigma(dNDVI))%>%
    group_by(id) %>%
    do(NDVI = ggplot(data = .)
       +geom_smooth(aes(x=as.Date(time),y=ndvi_max), method="loess", span=1)
       +geom_point(aes(x=as.Date(time),y=ndvi_max),size=.1, alpha=1/10)
       +scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 week",
                     limits = c(as.Date("01.05.2019", "%d.%m.%Y"),as.Date("01.10.2019", "%d.%m.%Y")), name = NULL)
       +scale_y_continuous( limits = c(0,1), name = expression(Вегетационный~" "~индекс~" "~NDVI))
       +theme_bw())
  
  
  #sitename = "RUDN"
  for(i in 1:nrow(plotbunch)){
    filename = paste(names(plotbunch)[2],plotbunch[i,1],sitename,".png",sep="_")
    graph = plotbunch[i,2][[1]][[1]]
    ggsave(filename, graph,device = "png",width=5.83,height=4.13,units="in")
  } 
}


writeNdviTSALg = function(data, sitename){
  
  plotbunch = data%>%group_by(load,Species, doy, id) %>% filter(NDVI >0 & NDVI <0.97)%>%
    summarise(dNDVI = mean(NDVI, na.rm=T), ndvi_med = quantile(NDVI,0.5,na.rm=T),
              ndvi_m = quantile(NDVI,0.9,na.rm=T), ndvi_v = NDVI[which.max(VPD)],
              ndvi_l = NDVI[which.max(b_G_550)], time = mean(time, na.rm = T)) %>%na.omit()%>%filter(!out_of_two_sigma(ndvi_m))%>%
    group_by(Species) %>%
    do(NDVI = ggplot(data = .)
       +geom_smooth(aes(x=as.Date(time),y=ndvi_m, color=load, fill = load), method="loess", span=.4)
       +geom_point(aes(x=as.Date(time),y=ndvi_m, color=load), size=.1, alpha=1/10)
       +scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 week",
                     limits = c(as.Date("01.06.2019", "%d.%m.%Y"),as.Date("01.10.2019", "%d.%m.%Y")), name = NULL)
       +scale_y_continuous( limits = c(0,1), name = expression(Вегетационный~" "~индекс~" "~NDVI))
       +scale_color_lancet(guide=FALSE)
       +scale_fill_lancet(name = "Уровень антропогенной нагрузки")
       +theme_bw()
       +theme(legend.position="bottom")
       
    )
  
  
  for(i in 1:nrow(plotbunch)){
    filename = paste("Load_anhtrop",plotbunch[i,1],names(plotbunch)[2],".png",sep="_")
    graph = plotbunch[i,2][[1]][[1]]
    ggsave(filename, graph,device = "png",width=8,height=6,units="in")
  } 
  
}


writeNdviTSALinSg = function(data, sitename){
  
  plotbunch = data%>%group_by(insite_load,Site,Species, doy, id) %>% filter(NDVI >0 & NDVI <0.97)%>%
    summarise(dNDVI = mean(NDVI, na.rm=T), ndvi_med = quantile(NDVI,0.5,na.rm=T),
              ndvi_m = quantile(NDVI,0.9,na.rm=T), ndvi_v = NDVI[which.max(VPD)],
              ndvi_l = NDVI[which.max(b_G_550)], time = mean(time, na.rm = T)) %>%na.omit()%>%filter(!out_of_two_sigma(ndvi_m))%>%
    group_by(Site,Species) %>%
    do(NDVI = ggplot(data = .)
       +geom_smooth(aes(x=as.Date(time),y=ndvi_m, color=insite_load, group  = insite_load, fill = insite_load),
                    method="loess", span=.9)
       +geom_point(aes(x=as.Date(time),y=ndvi_m, color=insite_load), size=.1, alpha=1/10)
       +scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 week",
                     limits = c(as.Date("01.06.2019", "%d.%m.%Y"),as.Date("01.10.2019", "%d.%m.%Y")), name = NULL)
       +scale_y_continuous( limits = c(0,1), name = expression(Вегетационный~" "~индекс~" "~NDVI))
       +scale_color_lancet(guide=FALSE)
       +scale_fill_lancet(name = "Уровень антропогенной нагрузки")
       +theme_bw()
       +theme(legend.position="bottom")
       
    )
  
  
  for(i in 1:nrow(plotbunch)){
    filename = paste("Load_anhtrop_in_site_NDVI",plotbunch[i,1],plotbunch[i,2],".png",sep="_")
    graph = plotbunch[i,3][[1]][[1]]
    ggsave(filename, graph,device = "png",width=8,height=6,units="in")
  } 
  
}



writePRITSALg = function(data, sitename){
  
  plotbunch = data%>%group_by(load,Species, doy, id) %>% filter(NDVI >0 & NDVI <0.97)%>%
    summarise(dNDVI = mean(NDVI, na.rm=T), ndvi_med = quantile(NDVI,0.5,na.rm=T),
              PRI_m = quantile(( b_O_600- b_B_500)/( b_O_600 + b_B_500),0.1,na.rm=T), ndvi_v = NDVI[which.max(VPD)],
              ndvi_l = NDVI[which.max(b_G_550)], time = mean(time, na.rm = T)) %>%na.omit()%>%filter(!out_of_two_sigma(PRI_m))%>%
    group_by(Species) %>%
    do(PRI = ggplot(data = .)
       +geom_smooth(aes(x=as.Date(time),y=PRI_m, color=load, fill = load), method="loess", span=.4)
       +geom_point(aes(x=as.Date(time),y=PRI_m, color=load), size=.1, alpha=1/10)
       +scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 week",
                     limits = c(as.Date("01.06.2019", "%d.%m.%Y"),as.Date("01.10.2019", "%d.%m.%Y")), name = NULL)
       +scale_y_continuous( limits = c(-0.25,0.25), name = expression(Минимальное~" "~значение~" "~индекса~" "~PRI~" "~за~" "~день))
       +scale_color_lancet(guide=FALSE)
       +scale_fill_lancet(name = "Уровень антропогенной нагрузки")
       +theme_bw()
       +theme(legend.position="bottom")
       
    )
  
  
  for(i in 1:nrow(plotbunch)){
    filename = paste("Load_anhtrop",plotbunch[i,1],names(plotbunch)[2],".png",sep="_")
    graph = plotbunch[i,2][[1]][[1]]
    ggsave(filename, graph,device = "png",width=8,height=6,units="in")
  } 
  
}


writePRITSALinSg = function(data, sitename){
  
  plotbunch = data%>%group_by(insite_load,Site,Species, doy, id) %>% filter(NDVI >0 & NDVI <0.97)%>%
    summarise(dNDVI = mean(NDVI, na.rm=T), ndvi_med = quantile(NDVI,0.5,na.rm=T),
              PRI_m = quantile(( b_O_600- b_B_500)/( b_O_600 + b_B_500),0.1,na.rm=T), ndvi_v = NDVI[which.max(VPD)],
              ndvi_l = NDVI[which.max(b_G_550)], time = mean(time, na.rm = T)) %>%na.omit()%>%filter(!out_of_two_sigma(PRI_m))%>%
    group_by(Site,Species) %>%
    do(PRI = ggplot(data = .)
       +geom_smooth(aes(x=as.Date(time),y=PRI_m, color=load, fill = load), method="loess", span=.4)
       +geom_point(aes(x=as.Date(time),y=PRI_m, color=load), size=.1, alpha=1/10)
       +scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 week",
                     limits = c(as.Date("01.06.2019", "%d.%m.%Y"),as.Date("01.10.2019", "%d.%m.%Y")), name = NULL)
       +scale_y_continuous( limits = c(-0.25,0.25), name = expression(Минимальное~" "~значение~" "~индекса~" "~PRI~" "~за~" "~день))
       +scale_color_lancet(guide=FALSE)
       +scale_fill_lancet(name = "Уровень антропогенной нагрузки внутри участка")
       +theme_bw()
       +theme(legend.position="bottom")
       
    )
  
  
  for(i in 1:nrow(plotbunch)){
    filename = paste("Load_anhtrop_insite_PRI_",plotbunch[i,1],plotbunch[i,2],".png",sep="_")
    graph = plotbunch[i,3][[1]][[1]]
    ggsave(filename, graph,device = "png",width=8,height=6,units="in")
  } 
  
}



#plotbunch[5,2][[1]]

writeAngleTSg = function(data, sitename){
  data$phi = data$phi+180
  data$psi = data$psi+180
  data$theta = data$theta+180
  plotbunch = data%>%group_by(Species, doy, id) %>% 
    summarise(angle = mean(psi, na.rm=T), time = mean(time, na.rm = T)) %>%na.omit()%>%filter(!out_of_two_sigma(angle))%>%
    group_by(Species) %>%
    do(angle = ggplot(data = .)
       +geom_smooth(aes(x=as.Date(time),y=angle), method="loess", span=.2)
       +geom_point(aes(x=as.Date(time),y=angle),size=.1, alpha=1/10)
       +scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 week",
                     limits = c(as.Date("01.05.2019", "%d.%m.%Y"),NA), name = NULL)
       +scale_y_continuous(  name = expression(Угол~" "~уклона~" "~дерева))
       +theme_bw())
  
  
  #sitename = "RUDN"
  for(i in 1:nrow(plotbunch)){
    filename = paste(names(plotbunch)[2],plotbunch[i,1],sitename,".png",sep="_")
    graph = plotbunch[i,2][[1]][[1]]
    ggsave(filename, graph,device = "png",width=5.83,height=4.13,units="in")
  } 
}



writeNHzTSg = function(data, sitename){
  
  plotbunch = data%>%group_by(Species, doy, id) %>% 
    summarise(W = mean((46000-Hz)/(Hz+46000)*50, na.rm=T),  time = mean(time, na.rm = T)) %>% na.omit() %>%
    group_by(Species) %>%
    do(NHz50 = ggplot(data = .)
       +geom_smooth(aes(x=as.Date(time),y= W), method="loess", span=.2)
       +geom_point (aes(x=as.Date(time), y= W, color=id), size=.1, alpha=1/3)
       +scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 week",
                     limits = c(as.Date("01.05.2019", "%d.%m.%Y"),NA), name = NULL)
       +scale_y_continuous(limits = c(0,50),  name = expression(Нормализованная~" "~влажность~" "~древесины))
       +theme_bw()
       +theme(legend.position = "none") )
  
  
  #sitename = "RUDN"
  for(i in 1:nrow(plotbunch)){
    filename = paste(names(plotbunch)[2],plotbunch[i,1],sitename,".png",sep="_")
    graph = plotbunch[i,2][[1]][[1]]
    ggsave(filename, graph,device = "png",width=5.83,height=4.13,units="in")
  } 
}



writeTairRhTSg = function(data, sitename){
  
  plotbunch = data%>%group_by(Species) %>%filter(!is.na(tair))%>%
    do(clim = ggplot(data = .)
       +geom_line(aes(x=as.Date(time),y=tair),size=.2, alpha=1/3)
       +geom_line(aes(x=as.Date(time),y=VPD*20),size=.2, color=3, alpha=1/3)
       +scale_y_continuous(name = expression(Температура~воздуха~" "~C),
                           sec.axis = sec_axis(~./50, name = "Отрицательное давление паров влаги, кПa", labels = function(b) { round(b * 1, 2)}),
                           limits = c(0,40))
       +scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 week",
                     limits = c(as.Date("01.05.2019", "%d.%m.%Y"),NA), name = NULL)
       +theme_bw()
    )
  
  #sitename = "RUDN"
  for(i in 1:nrow(plotbunch)){
    filename = paste(names(plotbunch)[2],plotbunch[i,1],sitename,".png",sep="_")
    graph = plotbunch[i,2][[1]][[1]]
    ggsave(filename, graph,device = "png",width=5.83,height=4.13,units="in")
  } 
}


writeTairRhTSAlg = function(data){
  
  plotbunch = data%>%group_by(Species) %>%filter(!is.na(tair))%>%
    do(clim = ggplot(data = .)
       +geom_point(aes(x=as.Date(time),y=tair, group = load, color = load),size=.2, alpha=1/3)
       +geom_smooth(aes(x=as.Date(time),y=tair, group = load, color=load, fill = load),method = "loess",span = .4)
       +geom_line(aes(x=as.Date(time),y=VPD*20, group = load),size=.3, color=5, alpha=1/5)
       +scale_y_continuous(name = expression(Температура~воздуха~" "~C),
                           sec.axis = sec_axis(~./50, name = "Отрицательное давление паров влаги, кПa", labels = function(b) { round(b * 1, 2)}),
                           limits = c(0,40))
       +scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 week",
                     limits = c(as.Date("01.05.2019", "%d.%m.%Y"),as.Date("01.10.2019", "%d.%m.%Y")), name = NULL)
       +scale_color_lancet(guide=FALSE)
       +scale_fill_lancet(name = "Уровень антропогенной нагрузки внутри участка")
       +theme_bw()
       +theme(legend.position="bottom")
    )
  
  #sitename = "RUDN"
  for(i in 1:nrow(plotbunch)){
    filename = paste("Ant_load",names(plotbunch)[2],plotbunch[i,1],".png",sep="_")
    graph = plotbunch[i,2][[1]][[1]]
    ggsave(filename, graph,device = "png",width=5.83,height=4.13,units="in")
  } 
}


#data=RUDNdata[[2]]
#plotbunch[1,2][[1]]


writeAngleDeviation = function(data, sitename){
  data = data %>% group_by(doy,id) %>% mutate(psi_d = mean(psi, na.rm = T),
                                              phi_d = mean(phi, na.rm = T), 
                                              theta_d = mean(theta, na.rm = T))
  
  data = data %>% group_by(Species) %>% mutate(mxt=max(theta_d-theta, na.rm=T),
                                               mnt=min(theta_d-theta, na.rm=T))
  data = data %>% group_by(doy, id) %>%
    mutate(dtheta  = (theta- theta_d)) %>% ungroup%>% group_by( Species) %>%
    mutate(mxt=max(dtheta , na.rm=T), mnt=min(dtheta , na.rm=T)) %>% group_by(doy, Species) %>%
    mutate(mdtheta = mean(dtheta)) 
  
  plotbunch = data%>%group_by(Species) %>%
    do(stability = ggplot(data = .)+
         geom_line(aes(x=as.Date(time),y=dtheta ), color=1, size=0.02)+
         geom_hline(aes(yintercept=mxt), color=2,linetype="dashed", size=.02)+
         geom_hline(aes(yintercept=mnt), color=2,linetype="dashed", size=.02)+
         scale_y_continuous(name = expression(Отклонение~от~среднесуточного~значения~угла~theta~','~градусы), limits=c(-5,5))+
         scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 week",
                      limits = c(as.Date("01.05.2019", "%d.%m.%Y"),NA), name = NULL)+
         theme_bw()
    )
  #geom_smooth(aes(x=time,y=theta_d-theta ), color=3, size=0.02, se=T, span=.1,method = "loess")+
  #scale_y_continuous(limits=c(-10,10))+
  
  for(i in 1:nrow(plotbunch)){
    filename = paste(names(plotbunch)[2],plotbunch[i,1],sitename,".png",sep="_")
    graph = plotbunch[i,2][[1]][[1]]
    ggsave(filename, graph,device = "png",width=5.83,height=4.13,units="in")
  } 
}

#data=RUDNdata[[2]]
#plotbunch[21,2][[1]]


writeAngleDeviationTT = function(data, sitename){
  data = data %>% group_by(doy,id) %>% mutate(psi_d = mean(psi, na.rm = T),
                                              phi_d = mean(phi, na.rm = T), 
                                              theta_d = mean(theta, na.rm = T)) %>%
    mutate(dtheta  = (theta- theta_d)) %>%ungroup()%>%filter(!out_of_two_sigma(dtheta))%>%
    filter(dtheta <5 & dtheta > -5)%>%group_by(id) %>% mutate(mxt=max(dtheta, na.rm=T),
                                                              mnt=min(dtheta, na.rm=T))
  
  plotbunch = data%>%group_by(id) %>% 
    do(stability = ggplot(data = .)+
         geom_line(aes(x=as.Date(time),y=dtheta ), color=1, size=0.02)+
         geom_hline(aes(yintercept=mean(mxt,na.rm = T)), color=2,linetype="dashed", size=.02)+
         geom_hline(aes(yintercept=mean(mnt,na.rm = T)), color=2,linetype="dashed", size=.02)+
         scale_y_continuous(name = expression(Отклонение~от~среднесуточного~значения~угла~theta~','~градусы), limits=c(-5,5))+
         scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 week",
                      limits = c(as.Date("01.05.2019", "%d.%m.%Y"),NA), name = NULL)+
         theme_bw()
    )
  #geom_smooth(aes(x=time,y=theta_d-theta ), color=3, size=0.02, se=T, span=.1,method = "loess")+
  #scale_y_continuous(limits=c(-10,10))+
  
  for(i in 1:nrow(plotbunch)){
    filename = paste(names(plotbunch)[2],plotbunch[i,1],sitename,".png",sep="_")
    graph = plotbunch[i,2][[1]][[1]]
    ggsave(filename, graph,device = "png",width=5.83,height=4.13,units="in")
  } 
}



#data=RUDNdata[[2]]
#plotbunch[21,2][[1]]

writeZ2TT = function(data, sitename,meteo_data){
  data = left_join(data,meteo_data, by=c("year","doy","hour"))
  data = data %>% group_by(doy,id) %>%
    mutate(z2 = max(gz2+gy2+gx2, na.rm = T),
           WS = max(WS, na.rm = T))
  
  plotbunch = data%>%group_by(id) %>%
    do(AnglStdDevWspeed = ggplot(data = .)+
         geom_line(aes(x=as.Date(time),y=z2 ), color=1, size=0.02)+
         geom_line(aes(x=as.Date(time),y=WS/1000000), color=2,linetype="dashed", size=.02)+
         scale_y_continuous(name = expression(Cуммы~стандартных~отклонений~цифрового~сигнала~акслерометра~по~трем~осям),
                            sec.axis = sec_axis(~.*1000000, name = "Максимальная скорость ветра за последние 3 часа, м с^-1", 
                                                labels = function(b) { round(b * 1, 2)}))+
         scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 week",
                      limits = c(as.Date("01.05.2019", "%d.%m.%Y"),NA), name = NULL)+
         theme_bw()
    )
  #geom_smooth(aes(x=time,y=theta_d-theta ), color=3, size=0.02, se=T, span=.1,method = "loess")+
  #scale_y_continuous(limits=c(-10,10))+
  
  for(i in 1:nrow(plotbunch)){
    filename = paste(names(plotbunch)[2],plotbunch[i,1],sitename,".png",sep="_")
    graph = plotbunch[i,2][[1]][[1]]
    ggsave(filename, graph,device = "png",width=5.83,height=4.13,units="in")
  } 
}

writeZ2TTh = function(data, sitename,meteo_data){
  data = left_join(data,meteo_data, by=c("year","doy","hour"))
  data = data %>% mutate(g2 = gz2+gy2+gx2)
  plotbunch = data%>%group_by(id) %>%
    do(g2vsWSpeed = ggplot(data = .)+
         geom_line(aes(x=as.Date(time),y=g2 ), color=1, size=0.02)+
         geom_line(aes(x=as.Date(time),y=WS/1000000), color=2,linetype="dashed", size=.02)+
         scale_y_continuous(name = expression(Cуммы~стандартных~отклонений~цифрового~сигнала~акслерометра~по~трем~осям),
                            sec.axis = sec_axis(~.*1000000, name = "Максимальная скорость ветра за последние 3 часа, м с^-1", 
                                                labels = function(b) { round(b * 1, 2)}))+
         scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 week",
                      limits = c(as.Date("01.05.2019", "%d.%m.%Y"),NA), name = NULL)+
         theme_bw()
    )
  #geom_smooth(aes(x=time,y=theta_d-theta ), color=3, size=0.02, se=T, span=.1,method = "loess")+
  #scale_y_continuous(limits=c(-10,10))+
  
  for(i in 1:nrow(plotbunch)){
    filename = paste(names(plotbunch)[2],plotbunch[i,1],sitename,".png",sep="_")
    graph = plotbunch[i,2][[1]][[1]]
    try(ggsave(filename, graph,device = "png",width=5.83,height=4.13,units="in"),T)
  } 
}


#data=RUDNdata[[2]]
#plotbunch[1,2][[1]]

writeZ2 = function(data, sitename,meteo_data){
  data = left_join(data,meteo_data, by=c("year","doy","hour"))
  data = data %>% group_by(doy,id,Species) %>%
    mutate(z2 = max(gz2+gy2+gx2, na.rm = T),
           WS = max(WS, na.rm = T))%>%
    group_by(doy,Species)%>%
    mutate(
      z2s = mean(z2, na.rm = T), 
      WSs = mean(WS, na.rm = T)
    )
  
  plotbunch = data%>%group_by(Species)%>%
    do(AnglStdDevWspeed = ggplot(data = .)+
         geom_line(aes(x=as.Date(time),y=z2s ), color=1, size=0.02)+
         geom_line(aes(x=as.Date(time),y=WSs/1000000), color=2,linetype="dashed", size=.02)+
         scale_y_continuous(name = expression(Cуммы~стандартных~отклонений~цифрового~сигнала~акслерометра~по~трем~осям),
                            sec.axis = sec_axis(~.*1000000, name = "Максимальная скорость ветра за последние 3 часа, м с^-1", 
                                                labels = function(b) { round(b * 1, 2)}))+
         scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 week",
                      limits = c(as.Date("01.05.2019", "%d.%m.%Y"),NA), name = NULL)+
         theme_bw()
    )
  #geom_smooth(aes(x=time,y=theta_d-theta ), color=3, size=0.02, se=T, span=.1,method = "loess")+
  #scale_y_continuous(limits=c(-10,10))+
  
  for(i in 1:nrow(plotbunch)){
    filename = paste(names(plotbunch)[2],plotbunch[i,1],sitename,".png",sep="_")
    graph = plotbunch[i,2][[1]][[1]]
    ggsave(filename, graph,device = "png",width=5.83,height=4.13,units="in")
  } 
}

writeZ2ALg = function(data,meteo_data){
  
  data = left_join(data,meteo_data, by=c("year","doy","hour"))
  data = data %>% group_by(doy,id,Species,load) %>%
    mutate(z2 = max(gz2+gy2+gx2, na.rm = T),
           WS = max(WS, na.rm = T))%>%
    group_by(doy,Species,load)%>%
    mutate(
      z2s = mean(z2, na.rm = T), 
      WSs = mean(WS, na.rm = T)
    )
  
  plotbunch = data%>%group_by(Species)%>%
    do(trembling = ggplot(data = .)+
         geom_line(aes(x=as.Date(time),y=z2s, color=load ), size=.5)+
         geom_line(aes(x=as.Date(time),y=WSs/5000000), color=2, size=.5)+
         scale_y_continuous(name = expression(Cуммы~стандартных~отклонений~сигнала~акслерометра~по~трем~осям),
                            limits = c(0,5*10^-6),
                            sec.axis = sec_axis(~.*5000000, name = expression(Максимальная~скорость~ветра~за~последние~"3 часа,"~м~с^-1), 
                                                labels = function(b) { round(b * 1, 2)}))+
         scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 week",
                      limits = c(as.Date("01.06.2019", "%d.%m.%Y"),as.Date("01.10.2019", "%d.%m.%Y")), name = NULL)+
         scale_color_lancet(name = "Уровень антропогенной нагрузки")+
         theme_bw()+
         theme(legend.position="bottom")
    )
  
  for(i in 1:nrow(plotbunch)){
    filename = paste("Load_anhtrop",plotbunch[i,1],names(plotbunch)[2],".png",sep="_")
    graph = plotbunch[i,2][[1]][[1]]
    ggsave(filename, graph,device = "png",width=8,height=6,units="in")
  } 
  
}

writeZ2ALinSg = function(data,meteo_data){
  
  #data = left_join(data,meteo_data, by=c("year","doy","hour"))
  data = data %>% group_by(doy,id,Site,Species,insite_load) %>%
    mutate(z2 = max(gz2+gy2+gx2, na.rm = T),
           WS = max(WS, na.rm = T))%>%
    group_by(doy,Species,Site,insite_load)%>%
    mutate(
      z2s = mean(z2, na.rm = T), 
      WSs = mean(WS, na.rm = T)
    )
  
  plotbunch = data%>%group_by(Site,Species)%>%
    do(trembling = ggplot(data = .)+
         geom_line(aes(x=as.Date(time),y=z2s, color=insite_load ), size=.5)+
         geom_line(aes(x=as.Date(time),y=WSs/5000000 ),linetype = 4, color=3, size=.2)+
         scale_y_continuous(name = expression(Cуммы~стандартных~отклонений~сигнала~акслерометра~по~трем~осям),
                            limits = c(0,5*10^-6),
                            sec.axis = sec_axis(~.*5000000, name = expression(Максимальная~скорость~ветра~за~последние~"3 часа,"~м~с^-1), 
                                                labels = function(b) { round(b * 1, 2)}))+
         scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 week",
                      limits = c(as.Date("01.06.2019", "%d.%m.%Y"),as.Date("01.10.2019", "%d.%m.%Y")), name = NULL)+
         scale_color_lancet(name = "Уровень антропогенной нагрузки")+
         theme_bw()+
         theme(legend.position="bottom")
    )
  
  for(i in 1:nrow(plotbunch)){
    filename = paste("Load_anhtrop_insite_tremble_",plotbunch[i,1],plotbunch[i,2],".png",sep="_")
    graph = plotbunch[i,3][[1]][[1]]
    ggsave(filename, graph,device = "png",width=8,height=6,units="in")
  } 
  
}


writeGrowthTTm = function(Alldata){
  months_n = factor(c("Ягварь","Февраль","Март","Апрель","Май","Июнь", "Июль","Август","Сентябрь", "Октябрь","Ноябрь","Декабрь"), 
                    levels = c("Ягварь","Февраль","Март","Апрель","Май","Июнь", "Июль","Август","Сентябрь", "Октябрь","Ноябрь","Декабрь"))
  for(sitename in Alldata$Site %>% unique){
    data  = Alldata %>% filter(Site == sitename)
    dat = data %>% group_by(id, month) %>% summarise(growth = max(growth, na.rm=T))
    plotbunch = dat%>%group_by(id) %>%
      do(Growth = ggplot(data = .)+
           geom_col(aes(x=months_n[month],y=growth ), color=1, size=0.02)+
           scale_y_continuous(name = expression(Прирост~даметра~ствола~", "~мм))+
           theme_bw()
      )
    #geom_smooth(aes(x=time,y=theta_d-theta ), color=3, size=0.02, se=T, span=.1,method = "loess")+
    #scale_y_continuous(limits=c(-10,10))+
    
    for(i in 1:nrow(plotbunch)){
      filename = paste(names(plotbunch)[2],plotbunch[i,1],sitename,".png",sep="_")
      graph = plotbunch[i,2][[1]][[1]]
      ggsave(filename, graph,device = "png",width=5.83,height=4.13,units="in")
    } 
  }
  
}


writeGrowthSpm = function(Alldata){
  months_n = factor(c("Ягварь","Февраль","Март","Апрель","Май","Июнь", "Июль","Август","Сентябрь", "Октябрь","Ноябрь","Декабрь"), 
                    levels = c("Ягварь","Февраль","Март","Апрель","Май","Июнь", "Июль","Август","Сентябрь", "Октябрь","Ноябрь","Декабрь"))
  for(sitename in Alldata$Site %>% unique){
    data  = Alldata %>% filter(Site == sitename)
    dat = data %>% group_by(id, Species,month) %>% 
      summarise(growth = max(growth, na.rm=T)) %>% group_by(Species,month) %>%
      summarise(growth = mean(growth, na.rm=T))
    plotbunch = dat%>%group_by(Species) %>%
      do(Growth = ggplot(data = .)+
           geom_col(aes(x=months_n[month],y=growth ), color=1, size=0.02)+
           scale_y_continuous(name = expression(Прирост~даметра~ствола~", "~мм))+
           scale_x_discrete(limits = c("Июнь","Июль","Август","Сентябрь"), name = NULL)+
           scale_color_lancet(guide=FALSE)+
           theme_bw()
      )
    #geom_smooth(aes(x=time,y=theta_d-theta ), color=3, size=0.02, se=T, span=.1,method = "loess")+
    #scale_y_continuous(limits=c(-10,10))+
    
    for(i in 1:nrow(plotbunch)){
      filename = paste(names(plotbunch)[2],plotbunch[i,1],sitename,".png",sep="_")
      graph = plotbunch[i,2][[1]][[1]]
      ggsave(filename, graph,device = "png",width=5.83,height=4.13,units="in")
    } 
  }
  
}


writGrowthALSpm = function(Alldata){
  
  months_n = factor(c("Ягварь","Февраль","Март","Апрель","Май","Июнь", "Июль","Август","Сентябрь", "Октябрь","Ноябрь","Декабрь"), 
                    levels = c("Ягварь","Февраль","Март","Апрель","Май","Июнь", "Июль","Август","Сентябрь", "Октябрь","Ноябрь","Декабрь"))
  data  = Alldata 
  dat = data %>% group_by(id, Species,load,month) %>% 
    summarise(growth = max(growth, na.rm=T)) %>% group_by(load,Species,month) %>%
    summarise(growth = mean(growth, na.rm=T))
  plotbunch = dat%>%group_by(Species) %>%
    do(Growth = ggplot(data = .)+
         geom_col(aes(x=months_n[month],y=growth, fill=load, group = load ), size=0.02, position = "dodge")+
         scale_y_continuous(name = expression(Прирост~даметра~ствола~", "~мм))+
         scale_x_discrete(limits = c("Июнь","Июль","Август","Сентябрь"), name = NULL)+
         scale_color_lancet(guide=FALSE)+
         scale_fill_lancet(name = "Уровень антропогенной нагрузки")+
         theme_bw()+
         theme(legend.position="bottom"))
  
  
  for(i in 1:nrow(plotbunch)){
    filename = paste("Load_anhtrop",plotbunch[i,1],names(plotbunch)[2],".png",sep="_")
    graph = plotbunch[i,2][[1]][[1]]
    ggsave(filename, graph,device = "png",width=8,height=6,units="in")
  } 
}  

writGrowthALSinSpm = function(Alldata){
  
  months_n = factor(c("Ягварь","Февраль","Март","Апрель","Май","Июнь", "Июль","Август","Сентябрь", "Октябрь","Ноябрь","Декабрь"), 
                    levels = c("Ягварь","Февраль","Март","Апрель","Май","Июнь", "Июль","Август","Сентябрь", "Октябрь","Ноябрь","Декабрь"))
  data  = Alldata 
  dat = data %>% group_by(id, Species,insite_load,Site,month) %>% 
    summarise(growth = max(growth, na.rm=T)) %>% group_by(insite_load,Site,Species,month) %>%
    summarise(growth = mean(growth, na.rm=T))
  plotbunch = dat%>%group_by(Site,Species) %>%
    do(Growth = ggplot(data = .)+
         geom_col(aes(x=months_n[month],y=growth, fill=insite_load, group = insite_load ), size=0.02, position = "dodge")+
         scale_y_continuous(name = expression(Прирост~даметра~ствола~", "~мм))+
         scale_x_discrete(limits = c("Июнь","Июль","Август","Сентябрь"), name = NULL)+
         scale_color_lancet(guide=FALSE)+
         scale_fill_lancet(name = "Уровень антропогенной нагрузки")+
         theme_bw()+
         theme(legend.position="bottom"))
  
  
  for(i in 1:nrow(plotbunch)){
    filename = paste("Load_anhtrop_insite_growth_",plotbunch[i,1],plotbunch[i,2],".png",sep="_")
    graph = plotbunch[i,3][[1]][[1]]
    ggsave(filename, graph,device = "png",width=8,height=6,units="in")
  } 
} 


# months_n = factor(c("Ягварь","Февраль","Март","Апрель","Май","Июнь", "Июль","Август","Сентябрь", "Октябрь","Ноябрь","Декабрь"), 
#                   levels = c("Ягварь","Февраль","Март","Апрель","Май","Июнь", "Июль","Август","Сентябрь", "Октябрь","Ноябрь","Декабрь"))
# 
#   AllData = AllData%>%mutate(month = month(time))
#   plotbunch = AllData%>%group_by(load,Species, hour, month,id) %>% filter(month<11 & month>4)%>%
#     summarise(Flux = mean(Flux, na.rm=T)) %>% group_by(Species) %>% 
#     mutate(months = months_n[month]) %>%
#     do(DiurnalFlux = ggplot(data = .)
#        +geom_smooth(aes(x=hour,y=Flux, group = load, color=load, fill = load))
#        +geom_point(aes(x=hour,y=Flux, group = load,color=load),size=.1)
#        +scale_x_continuous(limits = c(0,24), breaks =c(0,3,6,9,12,15,18,21) ,name = expression(Часы~суток))
#        +scale_y_continuous( limits = c(0,10), name = expression(Скорость~" "~сокотечения~" "~л~ч^-1))
#        +facet_wrap(~months,nrow=2,ncol=3,strip.position = "bottom" )
#        +scale_color_viridis_d(guide=FALSE)
#        +scale_fill_viridis_d(name = "Уровень антропогенной нагрузки")
#        +theme_bw()
#        +theme(legend.position="bottom"))





#plotbunch[6,2][[1]]




writeTairRhTSAlg(AllData)

writeGrowthTTm(AllData)
writeGrowthSpm(AllData)
writGrowthALSpm(AllData)
writGrowthALSinSpm(AllData)
writeZ2ALinSg(AllData, meteo_data)

writeFluxTSALg(AllData)
writeFluxTSALinSg(AllData)
writeFluxDiurnalALg(AllData)
writeFluxDiurnalALinSg (AllData)
writeNdviTSALg(AllData)
writeNdviTSALinSg(AllData)
writePRITSALg(AllData)
writePRITSALinSg(AllData)
writeZ2ALg(AllData, meteo_data)


writeFluxTSg(RUDNdata[[2]],"RUDN")
writeFluxTSg(TIMdata[[2]],"Timiryazev")
writeFluxTSg(BLTNdata[[2]],"BOLOTNAYA")
writeFluxTSg(HORTdata[[2]],"GARDEN")
writeFluxTSg(TRSKdata[[2]],"TROITSK")
writeFluxTSg(SCHLdata[[2]],"SCHOOL1234")
writeFluxTSg(SHERdata[[2]],"SCHERBINKA")


writeFluxTSTTg(RUDNdata[[2]],"RUDN")
writeFluxTSTTg(TIMdata[[2]],"Timiryazev")
writeFluxTSTTg(BLTNdata[[2]],"BOLOTNAYA")
writeFluxTSTTg(HORTdata[[2]],"GARDEN")
writeFluxTSTTg(TRSKdata[[2]],"TROITSK")
writeFluxTSTTg(SCHLdata[[2]],"SCHOOL1234")
writeFluxTSTTg(SHERdata[[2]],"SCHERBINKA")


writeFluxDiurnalg(RUDNdata[[2]],"RUDN")
writeFluxDiurnalg(TIMdata[[2]],"Timiryazev")
writeFluxDiurnalg(BLTNdata[[2]],"BOLOTNAYA")
writeFluxDiurnalg(HORTdata[[2]],"GARDEN")
writeFluxDiurnalg(TRSKdata[[2]],"TROITSK")
writeFluxDiurnalg(SCHLdata[[2]],"SCHOOL1234")
writeFluxDiurnalg(SHERdata[[2]],"SCHERBINKA")


writeFluxDiurnalTTg(RUDNdata[[2]],"RUDN")
writeFluxDiurnalTTg(TIMdata[[2]],"Timiryazev")
writeFluxDiurnalTTg(BLTNdata[[2]],"BOLOTNAYA")
writeFluxDiurnalTTg(HORTdata[[2]],"GARDEN")
writeFluxDiurnalTTg(TRSKdata[[2]],"TROITSK")
writeFluxDiurnalTTg(SCHLdata[[2]],"SCHOOL1234")
writeFluxDiurnalTTg(SHERdata[[2]],"SCHERBINKA")




writeNdviTSg(RUDNdata[[2]],"RUDN")
writeNdviTSg(TIMdata[[2]],"Timiryazev")
writeNdviTSg(BLTNdata[[2]],"BOLOTNAYA")
writeNdviTSg(HORTdata[[2]],"GARDEN")
writeNdviTSg(TRSKdata[[2]],"TROITSK")
writeNdviTSg(SCHLdata[[2]],"SCHOOL1234")
writeNdviTSg(SHERdata[[2]],"SCHERBINKA")


writeNdviTSTTg(RUDNdata[[2]],"RUDN")
writeNdviTSTTg(TIMdata[[2]],"Timiryazev")
writeNdviTSTTg(BLTNdata[[2]],"BOLOTNAYA")
writeNdviTSTTg(HORTdata[[2]],"GARDEN")
writeNdviTSTTg(TRSKdata[[2]],"TROITSK")
writeNdviTSTTg(SCHLdata[[2]],"SCHOOL1234")
writeNdviTSTTg(SHERdata[[2]],"SCHERBINKA")


writeAngleTSg(RUDNdata[[2]],"RUDN")
writeAngleTSg(TIMdata[[2]],"Timiryazev")
writeAngleTSg(BLTNdata[[2]],"BOLOTNAYA")
writeAngleTSg(HORTdata[[2]],"GARDEN")
writeAngleTSg(TRSKdata[[2]],"TROITSK")
writeAngleTSg(SCHLdata[[2]],"SCHOOL1234")
writeAngleTSg(SHERdata[[2]],"SCHERBINKA")

writeNHzTSg(RUDNdata[[2]],"RUDN")
writeNHzTSg(TIMdata[[2]],"Timiryazev")
writeNHzTSg(BLTNdata[[2]],"BOLOTNAYA")
writeNHzTSg(HORTdata[[2]],"GARDEN")
writeNHzTSg(TRSKdata[[2]],"TROITSK")
writeNHzTSg(SCHLdata[[2]],"SCHOOL1234")
writeNHzTSg(SHERdata[[2]],"SCHERBINKA")

writeTairRhTSg(RUDNdata[[2]],"RUDN")
writeTairRhTSg(TIMdata[[2]],"Timiryazev")
writeTairRhTSg(BLTNdata[[2]],"BOLOTNAYA")
writeTairRhTSg(HORTdata[[2]],"GARDEN")
writeTairRhTSg(TRSKdata[[2]],"TROITSK")
writeTairRhTSg(SCHLdata[[2]],"SCHOOL1234")
writeTairRhTSg(SHERdata[[2]],"SCHERBINKA")


writeAngleDeviation(RUDNdata[[2]],"RUDN")
writeAngleDeviation(TIMdata[[2]],"Timiryazev")
writeAngleDeviation(BLTNdata[[2]],"BOLOTNAYA")
writeAngleDeviation(HORTdata[[2]],"GARDEN")
writeAngleDeviation(TRSKdata[[2]],"TROITSK")
writeAngleDeviation(SCHLdata[[2]],"SCHOOL1234")
writeAngleDeviation(SHERdata[[2]],"SCHERBINKA")



writeAngleDeviationTT(RUDNdata[[2]],"RUDN")
writeAngleDeviationTT(TIMdata[[2]],"Timiryazev")
writeAngleDeviationTT(BLTNdata[[2]],"BOLOTNAYA")
writeAngleDeviationTT(HORTdata[[2]],"GARDEN")
writeAngleDeviationTT(TRSKdata[[2]],"TROITSK")
writeAngleDeviationTT(SCHLdata[[2]],"SCHOOL1234")
writeAngleDeviationTT(SHERdata[[2]],"SCHERBINKA")


writeZ2TT(RUDNdata[[2]],"RUDN", meteo_data)
writeZ2TT(TIMdata[[2]],"Timiryazev", meteo_data)
writeZ2TT(BLTNdata[[2]],"BOLOTNAYA", meteo_data)
writeZ2TT(HORTdata[[2]],"GARDEN", meteo_data)
writeZ2TT(TRSKdata[[2]],"TROITSK", meteo_data)
writeZ2TT(SCHLdata[[2]],"SCHOOL1234", meteo_data)
writeZ2TT(SHERdata[[2]],"SCHERBINKA", meteo_data)


sData  = RUDNdata[[2]]%>% filter(doy >219 & doy<226 ) %>%
  filter(id %in% c("218A0173","218A0155","218A0146","218A0230","218A0221",
                   "218A0218","218A0140","218A0139","218A0115","218A0110",
                   "218A0108","218A0106","218A0100","218A0085","218A0064"))







writeZ2TTh(sData,"RUDN", meteo_data)



writeZ2TTh(TIMdata[[2]],"Timiryazev", meteo_data)
writeZ2TTh(BLTNdata[[2]],"BOLOTNAYA", meteo_data)
writeZ2TTh(HORTdata[[2]],"GARDEN", meteo_data)
writeZ2TTh(TRSKdata[[2]],"TROITSK", meteo_data)
writeZ2TTh(SCHLdata[[2]],"SCHOOL1234", meteo_data)
writeZ2TTh(SHERdata[[2]],"SCHERBINKA", meteo_data)

writeZ2(RUDNdata[[2]],"RUDN", meteo_data)
writeZ2(TIMdata[[2]],"Timiryazev", meteo_data)
writeZ2(BLTNdata[[2]],"BOLOTNAYA", meteo_data)
writeZ2(HORTdata[[2]],"GARDEN", meteo_data)
writeZ2(TRSKdata[[2]],"TROITSK", meteo_data)
writeZ2(SCHLdata[[2]],"SCHOOL1234", meteo_data)
writeZ2(SHERdata[[2]],"SCHERBINKA", meteo_data)



