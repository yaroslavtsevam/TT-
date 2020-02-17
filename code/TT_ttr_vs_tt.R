
data  = AllData %>% filter(Site == "BOLOTNAYA")

TTR = data %>% filter(Species == "TTR")
 data$Species %>% unique()

data$b_R_650c[data$b_R_650c<0] = 0
data$b_O_600c[data$b_O_600c<0] = 0
data$b_Y_570c[data$b_Y_570c<0] = 0
data$b_G_550c[data$b_G_550c<0] = 0
data$b_B_500c[data$b_B_500c<0] = 0
data$b_V_450c[data$b_V_450c<0] = 0
data$b_W_860c[data$b_W_860c<0] = 0
data$b_V_810c[data$b_V_810c<0] = 0
data$b_U_760c[data$b_U_760c<0] = 0
data$b_T_730c[data$b_T_730c<0] = 0
data$b_S_680c[data$b_S_680c<0] = 0
data$b_R_610c[data$b_R_610c<0] = 0


TTRdatasum  = data %>%filter(Species == "TTR") %>% group_by(Site,id, doy, hour) %>% summarise( TTair = mean(tair,na.rm = T),
                                                                   TTrh   = mean(rh,  na.rm = T),
                                                                   TTR_650c = mean(b_R_650c),
                                                                   TTR_600c = mean(b_O_600c),
                                                                   TTR_570c = mean(b_Y_570c),
                                                                   TTR_550c = mean(b_G_550c),
                                                                   TTR_500c = mean(b_B_500c),
                                                                   TTR_450c = mean(b_V_450c),
                                                                   TTR_860c = mean(b_W_860c),
                                                                   TTR_810c = mean(b_V_810c),
                                                                   TTR_760c = mean(b_U_760c),
                                                                   TTR_730c = mean(b_T_730c),
                                                                   TTR_680c = mean(b_S_680c),
                                                                   TTR_610c = mean(b_R_610c),
                                                                   TTR_650 = mean(b_R_650),
                                                                   TTR_600 = mean(b_O_600),
                                                                   TTR_570 = mean(b_Y_570),
                                                                   TTR_550 = mean(b_G_550),
                                                                   TTR_500 = mean(b_B_500),
                                                                   TTR_450 = mean(b_V_450),
                                                                   TTR_860 = mean(b_W_860),
                                                                   TTR_810 = mean(b_V_810),
                                                                   TTR_760 = mean(b_U_760),
                                                                   TTR_730 = mean(b_T_730),
                                                                   TTR_680 = mean(b_S_680),
                                                                   TTR_610 = mean(b_R_610))

data =data %>% left_join(TTRdatasum, by=c("Site","doy","hour"))
 

tair_pic = ggplot(data=data)+
  geom_point(aes(x=as.Date(time),y=TTair-tair, color = Species), size=.1, alpha=1/20)+
  geom_smooth(aes(x=as.Date(time),y=TTair-tair, color = Species), model="loess",span=.1)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 week",
               limits = c(as.Date("01.05.2019", "%d.%m.%Y"),NA), name = NULL)+
  scale_y_continuous(  name = expression(Температура~" "~воздуха~" "~С^o))+
  theme_bw()
ggsave("Comparison_of_TTR_vs_TT_Bolotnaya.png", tair_pic,device = "png",width=5.83,height=4.13,units="in")








 tair_pic = ggplot(data=data)+
  geom_point(aes(x=as.Date(time),y=tair, color = Species), size=.1, alpha=1/20)+
  geom_smooth(aes(x=as.Date(time),y=tair, color = Species), model="loess",span=.1)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 week",
               limits = c(as.Date("01.05.2019", "%d.%m.%Y"),NA), name = NULL)+
  scale_y_continuous( limits = c(0,33), name = expression(Температура~" "~воздуха~" "~С^o))+
  theme_bw()
ggsave("Comparison_of_TTR_vs_TT_Bolotnaya.png", tair_pic,device = "png",width=5.83,height=4.13,units="in")

rh_pic = ggplot(data=data)+
  geom_point(aes(x=as.Date(time),y=rh, color = Species), size=.1, alpha=1/20)+
  geom_smooth(aes(x=as.Date(time),y=rh, color = Species), model="loess",span=.1)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 week",
               limits = c(as.Date("01.05.2019", "%d.%m.%Y"),NA), name = NULL)+
  scale_y_continuous( limits = c(30,100), name = expression(Relative~" "~humidity~" "~С^o))+
  theme_bw()
ggsave("Comparison_of_TTR_vs_TT_Bolotnaya.png", tair_pic,device = "png",width=5.83,height=4.13,units="in")

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
