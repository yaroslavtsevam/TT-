library(ggplot2)

TTgraph_stability = function(data, wrap = T){
  graph = ggplot(data, aes(x=time,y=x))+
    geom_line( aes(x=time,y=phi), color=2,linetype="dashed", size=.02)+
    geom_line( aes(x=time,y=psi), color=3,linetype="dashed", size=.02)+
    geom_line( aes(x=time,y=gx2*20000000), color=1,linetype="dashed", size=.02)+
  scale_y_continuous(name = expression(Inclination~" "~degree))
  
  if(wrap){
    graph = graph + facet_wrap(~id,  nrow = 2, strip.position = "bottom")
  }  
  
  return(graph+theme_bw())
}


TTgraph_charge = function(data, wrap = T){
  graph = ggplot(data, aes(x=time,y=x))+
    geom_line( aes(x=time,y=volt), color=1,linetype="dashed", size=.02)+
    scale_y_continuous(name = expression(Inclination~" "~degree))
  if(wrap){
    graph = graph + facet_wrap(~id,  nrow = 2, strip.position = "bottom")
  }
  return(graph+theme_bw())
}

TTgraph_growth = function(data, wrap = T){
  graph = ggplot(data, aes(x=time,y=x))+
    geom_point( aes(x=time,y=dist5/10 ), color=2,linetype="dashed", size=.02)+
    geom_smooth( aes(x=time,y=dist5/10 ), color=1,span=2)+
    scale_y_continuous(name = expression(Growth~" "~mm))
    if(wrap){
      graph = graph + facet_wrap(~id,  nrow = 2, strip.position = "bottom")
    }
    
  return(graph+theme_bw())
}

TTgraph_PRI = function(data, wrap = T){
graph = ggplot(data)+
  geom_point( aes(x=time, y=PRI), size=.1 )+
  geom_smooth(aes(x=time, y=PRI), span =1)
  if(wrap){
    graph = graph + facet_wrap(~id,  nrow = 2, strip.position = "bottom")
  }

return(graph + theme_bw())
}


TTgraph_PRI2 = function(data, wrap = T){
  graph = ggplot(data)+
    geom_point( aes(x=time, y=PRI2), size=.1 )+
    geom_smooth(aes(x=time, y=PRI2), span =1)
  if(wrap){
    graph = graph + facet_wrap(~id,  nrow = 2, strip.position = "bottom")
  }
  
  return(graph + theme_bw())
}

TTgraph_NDVI = function(data, wrap = T){
  graph = ggplot(data)+
    geom_point( aes(x=time, y=NDVI), size=.1 )+
    geom_smooth(aes(x=time, y=NDVI), span =1)
  if(wrap){
    graph = graph + facet_wrap(~id,  nrow = 2, strip.position = "bottom")
  }
  
  return(graph + theme_bw())
}


TTgraph_sapflux = function(data, wrap = T){
  graph= ggplot(data, aes(x=time,y=u))+
  geom_line( aes(x=time,y=u*3600*(diam^1.8777)*0.755/10000), color=1,linetype="dashed", size=.02)+
  geom_line( aes(x=time,y=VPD*5), color=4,linetype="dashed", size=.02)+
  scale_y_continuous(name = expression(Sap~flow~" "~l~h^{-1}), sec.axis = sec_axis(~./5, name = "VPD, kPa", labels = function(b) { round(b * 1, 2)})) 
  if (any(names(data)=="t_u")){
    graph = graph + geom_line(aes(x=time,y=t_u*3600*(diam^1.8777)*0.755/10000), linetype="dashed", color = "red", size=1)
    }
  if(wrap){
    graph = graph + facet_wrap(~id,  nrow = 2, strip.position = "bottom")
  }
return(graph+theme_bw())
}

TTgraph_sapflux_cum = function(data, wrap = T){

  data = data%>% group_by(doy, id) %>% mutate(u_d = sum(u))
  data = data%>% group_by(doy, id) %>% mutate(vpd_a = mean(VPD))
  data = data%>% group_by(id) %>% mutate(u_cs = cumsum(u)) 
  if (any(names(data)=="t_u")){
    data = data%>% group_by(id) %>% mutate(t_u_cs = cumsum(t_u))  
  }
  
  if (any(names(data)=="t_u")){
    data_d = data %>% group_by(doy,id)%>% summarise(tud=sum(t_u), ud=sum(u), diam=mean(diam)) %>% 
      group_by(id) %>% mutate(u_cs = cumsum(ud)) %>% mutate(t_u_cs = cumsum(tud)) 
  } else {
    data_d = data %>% group_by(doy,id)%>% summarise( ud=sum(u), diam=mean(diam)) %>% 
      group_by(id) %>% mutate(u_cs = cumsum(ud)) 
  }
  
  
  
  graph = ggplot(data_d)+
  geom_line( aes(x=doy,y=u_cs*3600*(diam^1.8777)*0.755/10000), color=4,linetype="solid", size=1)+
  scale_y_continuous(name = expression(Cumulated~sap~flow~" "~l), sec.axis = sec_axis(~./10, name = "VPD, kPa", labels = function(b) { round(b * 1, 2)})) 
  if (any(names(data)=="t_u")){
    graph = graph + geom_line( aes(x=doy,y=t_u_cs*3600*(diam^1.8777)*0.755/10000), color=2,linetype="42", size=1)
  }
  if(wrap){
    graph = graph + facet_wrap(~id,  nrow = 2, strip.position = "bottom")
  }
return(graph+theme_bw())
}

TTgraph_climate = function(data, wrap = T){
  graph= ggplot(data)+
  geom_line( aes(x=time,y=tair), color=2,linetype="dashed", size=.02)+
  geom_line( aes(x=time,y=rh/2), color=4,linetype="dashed", size=.02)+
  scale_y_continuous(name = expression(Air~" "~temperature~" "~degree), sec.axis = sec_axis(~.*2, name = "Relative humidity, %", labels = function(b) { round(b * 1, 2)})) +
  if(wrap){
    graph = graph + facet_wrap(~id,  nrow = 2, strip.position = "bottom")
  }
  return(graph+theme_bw())
}
