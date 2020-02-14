
data  = AllData %>% filter(Site == "BOLOTNAYA")

TTR = AllData %>% filter(Species == "TTR")
 data$Species %>% unique()

tair_pic = ggplot(data=data)+
  geom_point(aes(x=as.Date(time),y=ta),color=2, size=.1, alpha=1/20)+
  geom_point(aes(x=as.Date(time),y=tair),color=3, size=.1, alpha=1/20)+
  geom_smooth(aes(x=as.Date(time),y=ta), model="loess", color=2,span=.01)+
  geom_smooth(aes(x=as.Date(time),y=tair), model="loess", color=3,span=.1)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 week",
               limits = c(as.Date("01.05.2019", "%d.%m.%Y"),NA), name = NULL)+
  scale_y_continuous( limits = c(0,33), name = expression(Температура~" "~воздуха~" "~С^o))+
  theme_bw()
ggsave("Comparison_of_tair_in_RUDNVvsMSU.png", tair_pic,device = "png",width=5.83,height=4.13,units="in")

rh_pic = ggplot(data = data)+
  geom_point(aes(x=as.Date(time),y=Rh),color=2, size=.1, alpha=1/20)+
  geom_point(aes(x=as.Date(time),y=rh),color=3, size=.1, alpha=1/20)+
  geom_smooth(aes(x=as.Date(time),y=Rh), model="loess", color=2,span=.01)+
  geom_smooth(aes(x=as.Date(time),y=rh), model="loess", color=3,span=.1)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 week",
               limits = c(as.Date("01.05.2019", "%d.%m.%Y"),NA), name = NULL)+
  scale_y_continuous( limits = c(0,100), name = expression(Влажность~" "~воздуха~" "~"%"))+
  theme_bw()

ggsave("Comparison_of_rh_in_RUDNVvsMSU.png", rh_pic,device = "png",width=5.83,height=4.13,units="in")
