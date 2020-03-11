

#source("code\\TTcalc_site_3.R")
source("code/TTcalc_site_6.R")
library(ggpmisc)
library(extrafont)
library(Rmisc)

#loadfonts(device = "win", quiet = TRUE)


installation_start = 1556845000


#OLDTIM = TTcalc_site(c("http://naturetalkers.altervista.org/C1880020/ttcloud.txt"),
#                     1535637102,
#                     import_folder_name=NULL,
#                     first_imported_dates_reconstructed = F,
#                     "timold_desc.csv")
#export_site_to_excel(OLDTIM,sitename="TIMACAD_old",
#                     insert_file="TIMOLD_descr.xlsx")


RUDNdata=TTcalc_site(c("http://naturetalkers.altervista.org/C18A0031/ttcloud.txt",
                       "http://naturetalkers.altervista.org/C18A0025/ttcloud.txt"),
                     installation_start,
                     import_folder_name = "data/backup/RUDN",
                     first_imported_dates_reconstructed = F,
                     "data/full_TT_desc.csv",
                     "RUDN",
                     verbose = "con")
 
 TIMdata=TTcalc_site(c("http://naturetalkers.altervista.org/C18A0029/ttcloud.txt",
                      "http://naturetalkers.altervista.org/C18A0030/ttcloud.txt"),
                    installation_start,
                    import_folder_name = "data/backup/TIMR",
                    first_imported_dates_reconstructed = T,
                    "data/full_TT_desc.csv",
                    "TIMIRYAZEV")


BLTNdata=TTcalc_site("http://naturetalkers.altervista.org/C18A0024/ttcloud.txt",
                     installation_start,
                     import_folder_name = NULL,
                     first_imported_dates_reconstructed = F,
                     "data/full_TT_desc.csv",
                     "BOLOTNAYA",
                     verbose="mem")

SCHLdata=TTcalc_site("http://naturetalkers.altervista.org/C18A0023/ttcloud.txt",
                     installation_start,
                     import_folder_name = NULL,
                     first_imported_dates_reconstructed = F,
                     "data/full_TT_desc.csv",
                     "SCHOOL1234")

SHERdata=TTcalc_site("http://naturetalkers.altervista.org/C1870015/ttcloud.txt",
                     installation_start,
                     import_folder_name = NULL,
                     first_imported_dates_reconstructed = F,
                     "data/full_TT_desc.csv",
                     "SCHERBINKA",
                     verbose = "con")

GRDNdata  = TTcalc_site("http://naturetalkers.altervista.org/C18A0026/ttcloud.txt",
                        installation_start,
                        import_folder_name = NULL,
                        first_imported_dates_reconstructed = F,
                        "data/full_TT_desc.csv",
                        "GARDEN")



TRSKdata = TTcalc_site("http://naturetalkers.altervista.org/C18A0027/ttcloud.txt",
                       installation_start,
                       import_folder_name = NULL,
                       first_imported_dates_reconstructed = F,
                       "data/full_TT_desc.csv",
                       "TROITSK")

AllData = rbind(
  RUDNdata[[2]],
  TIMdata[[2]],
  BLTNdata[[2]],
  GRDNdata[[2]],
  TRSKdata[[2]],
  SCHLdata[[2]],
  SHERdata[[2]]
)


tdt = dataR %>% group_by(id,doy, Species) %>% summarise(LAIm = mean(LAIb, na.rm=T),LAIx = max(LAIb, na.rm=T) )

ggplot(data =tdt,aes(x=doy,y=LAIx ))+geom_point(aes(color=Species))+facet_wrap(vars(id),nrow = 4)

data = BLTNdata[[2]]

dataR = TTR_add(data, "con")
dataR$LAIb[is.infinite(dataR$LAIb)] = NA 
dataR %>% group_by(id) %>% summarise(LAIm = mean(LAIb, na.rm=T),LAIx = max(LAIb, na.rm=T) )
dataR$LAIb %>% summary






TT = SHERdata[[2]] %>% filter(id =="218A0274") 

TRSKdata[[2]]$id %>% unique()

plot(TRSKdata[[2]]$b_V_810,TRSKdata[[2]]$involt )
plot(TRSKdata[[2]]$time,TRSKdata[[2]]$t1 )
S=SHERdata[[2]]
ggplot(data=TT)+
  geom_point(aes(x=time,y=dist), colour="black")+
  geom_point(aes(x=time,y=volt), colour="blue")+
  facet_wrap(~id, nrow=4)+
  ylim(0,5)+
  xlim(0,1)

ggplot(data=TT%>%filter(doy>190))+
  geom_point(aes(x=time,y=dist, color=rh))+
  geom_line(aes(x=time,y=tair/5, color=rh),span=20)+
  facet_wrap(~id, nrow=4)+
  ylim(0,7)



TT = TRSKdata[[2]] %>% filter(id =="218A0117") 
TT$tair = TT$tair+273
mod1 = lm(dist~tair,data=TT)
summary(mod1)




#










  #Climat comparison graphs

data = RUDNdata[[2]]
data = left_join(data,meteo_data, by=c("year","doy","hour"))



###### CREATINF DESCRIPTION TABLE##############


AllDataSummary= AllData %>% group_by(SiteIndex, Species,age_group_index,load,insite_load,id)%>%
  summarise(d = mean(diam),DBH = mean(DBH),DTT = mean(DTT), height = mean(height), VTA = mean(VTAscore))


write.csv(AllDataSummary, file="DB_description.csv")

#AllData2 = AllData2%>%group_by(id)%>%mutate(growth =first(dist_pred)-dist_pred)

# ggplot(AllData2)+
#   geom_point(aes(x=time,y=growth), size=.1)+facet_wrap(~id)+ylim(0,10)+theme_bw()
##############################################################################








AllData%>%group_by(Site)%>%summarise(age  = mean(age_group))





AllData2 =AllData
AllData2 = AllData2 %>% as.data.frame()
for (i in 1:ncol(AllData2)){
  AllData2[ AllData2[,i] %>% is.infinite(),i] = NA  
}
AllData2$month = month(AllData2$time)
SumaryAllmData = AllData2 %>% group_by(doy,id) %>% mutate(psi_d = mean(psi, na.rm = T),
                                                        phi_d = mean(phi, na.rm = T), 
                                                        theta_d = mean(theta, na.rm = T))%>%
  ungroup() %>% group_by(doy, id) %>% mutate(
    dtheta  = (theta- theta_d),
    dphi  = (phi- phi_d),
    dpsi  = (psi- psi_d)) %>% ungroup() %>%
  mutate(g2 = gx2+gy2+gz2,
         mHz = max(Hz, na.rm = T))%>%
  mutate(Hzmoist = (mHz-Hz)/(mHz+Hz)*50) %>%
  group_by(id,Site,Species,month, ) %>% 
  summarise(
    DBH = mean(DBH, na.rm = T),
    dthetamav = mean(dtheta, na.rm = T),
    dphimav = mean(dphi, na.rm = T),
    dpsimav = mean(dpsi, na.rm = T),
    g2mav = mean(g2, na.rm = T),
    tairm = mean(tair, na.rm = T),
    rhm = mean(rh, na.rm = T),
    NDVImav = mean(NDVI, na.rm = T),
    PRImav = mean(PRI, na.rm = T),
    PSNDmav = mean(PSND, na.rm=T),
    CRI2mav = mean(CRI2, na.rm=T),
    Hzmositmav = mean(Hzmoist, na.rm = T),
    Fluxmav = mean(Flux, na.rm = T),
    tmav = mean(tair, na.rm = T),
    VPDmav = mean(VPD, na.rm = T),
    dthetamsd = sd(dtheta, na.rm = T),
    dphimsd = sd(dphi, na.rm = T),
    dpsimsd = sd(dpsi, na.rm = T),
    g2msd = sd(g2, na.rm = T),
    tairsd = sd(tair, na.rm = T),
    rhsd = sd(rh, na.rm = T),
    NDVImsd = sd(NDVI, na.rm = T),
    PRImsd = sd(PRI, na.rm = T),
    PSNDmsd = sd(PSND, na.rm=T),
    CRI2msd = sd(CRI2, na.rm=T),
    Hzmositmsd = sd(Hzmoist, na.rm = T),
    Fluxmsd = sd(Flux, na.rm = T),
    tmsd = sd(tair, na.rm = T),
    VPDmsd = sd(VPD, na.rm = T),
    dthetamq10 = quantile(dtheta,0.1, na.rm = T),
    dphimq10 = quantile(dphi,0.1, na.rm = T),
    dpsimq10 = quantile(dpsi,0.1, na.rm = T),
    g2mq10 = quantile(g2,0.1, na.rm = T),
    tairq10 = quantile(tair,0.1, na.rm = T),
    rhq10 = quantile(rh,0.1, na.rm = T),
    NDVImq10 = quantile(NDVI,0.1, na.rm = T),
     PRImsq10 = quantile(PRI,0.1, na.rm = T),
     PSNDmq10 = quantile(PSND,0.1, na.rm=T),
     CRI2mq10 = quantile(CRI2,0.1, na.rm=T),
     Hzmositmq10 = quantile(Hzmoist,0.1, na.rm = T),
     Fluxmq10 = quantile(Flux,0.1, na.rm = T),
     VPDmq10 = quantile(VPD,0.1, na.rm = T),
     tmq10 = quantile(tair,0.1, na.rm = T),
    dthetamq90 = quantile(dtheta,0.9, na.rm = T),
    dphimq90 = quantile(dphi,0.9, na.rm = T),
    dpsimq90 = quantile(dpsi,0.9, na.rm = T),
    tairq90 = quantile(tair,0.9, na.rm = T),
    rhq90 = quantile(rh,0.9, na.rm = T),
    g2mq90 = quantile(g2,0.9, na.rm = T),
     NDVImq90 = quantile(NDVI,0.9, na.rm = T),
     PRImq90 = quantile(PRI,0.9, na.rm = T),
     PSNDmq90 = quantile(PSND,0.9, na.rm=T),
     CRI2mq90 = quantile(CRI2,0.9, na.rm=T),
     Hzmositmq90 = quantile(Hzmoist,0.9, na.rm = T),
     Fluxmq90 = quantile(Flux,0.9, na.rm = T),
     tmq90 = quantile(tair,0.9, na.rm = T),
     VPDmq90 = quantile(VPD,0.9, na.rm = T),
    dthetammd = quantile(dtheta,0.5, na.rm = T),
    dphimmd = quantile(dphi,0.5, na.rm = T),
    dpsimmd = quantile(dpsi,0.5, na.rm = T),
    tairmd = quantile(tair,0.5, na.rm = T),
    rhmd = quantile(rh,0.5, na.rm = T),
    g2mmd = quantile(g2,0.5, na.rm = T),
     NDVImmd = quantile(NDVI,0.5, na.rm = T),
     PRImmd = quantile(PRI,0.5, na.rm = T),
     PSNDmmd = quantile(PSND,0.5, na.rm=T),
     CRI2mmd = quantile(CRI2,0.5, na.rm=T),
     Hzmositmmd = quantile(Hzmoist,0.1, na.rm = T),
     Fluxmmd = quantile(Flux,0.5, na.rm = T),
     VPDmmd = quantile(VPD,0.5, na.rm = T),
     tmsum = sum(tair/length(Flux)*720, na.rm = T),
     VPDmsum = sum(VPD/length(Flux)*720, na.rm = T),
     Fluxmsum = sum(Flux/length(Flux)*720, na.rm = T),
     #growth = mean(growth, na.rm=T)
)



SumaryAllyData = AllData2 %>% group_by(doy,id) %>% mutate(psi_d = mean(psi, na.rm = T),
                                                         phi_d = mean(phi, na.rm = T), 
                                                         theta_d = mean(theta, na.rm = T))%>%
  ungroup() %>% group_by(doy, id) %>% mutate(
    dtheta  = (theta- theta_d),
    dphi  = (phi- phi_d),
    dpsi  = (psi- psi_d)) %>% ungroup() %>%
  mutate(g2 = gx2+gy2+gz2,
         mHz = max(Hz, na.rm = T))%>%
  mutate(Hzmoist = (mHz-Hz)/(mHz+Hz)*50) %>%
  group_by(id,Site,Species) %>% 
  summarise(
    DBH = mean(DBH, na.rm = T),
    dthetamav = mean(dtheta, na.rm = T),
    dphimav = mean(dphi, na.rm = T),
    dpsimav = mean(dpsi, na.rm = T),
    g2mav = mean(g2, na.rm = T),
    tairm = mean(tair, na.rm = T),
    rhm = mean(rh, na.rm = T),
    NDVImav = mean(NDVI, na.rm = T),
    PRImav = mean(PRI, na.rm = T),
    PSNDmav = mean(PSND, na.rm=T),
    CRI2mav = mean(CRI2, na.rm=T),
    Hzmositmav = mean(Hzmoist, na.rm = T),
    Fluxmav = mean(Flux, na.rm = T),
    tmav = mean(tair, na.rm = T),
    VPDmav = mean(VPD, na.rm = T),
    dthetamsd = sd(dtheta, na.rm = T),
    dphimsd = sd(dphi, na.rm = T),
    dpsimsd = sd(dpsi, na.rm = T),
    g2msd = sd(g2, na.rm = T),
    tairsd = sd(tair, na.rm = T),
    rhsd = sd(rh, na.rm = T),
    NDVImsd = sd(NDVI, na.rm = T),
    PRImsd = sd(PRI, na.rm = T),
    PSNDmsd = sd(PSND, na.rm=T),
    CRI2msd = sd(CRI2, na.rm=T),
    Hzmositmsd = sd(Hzmoist, na.rm = T),
    Fluxmsd = sd(Flux, na.rm = T),
    tmsd = sd(tair, na.rm = T),
    VPDmsd = sd(VPD, na.rm = T),
    dthetamq10 = quantile(dtheta,0.1, na.rm = T),
    dphimq10 = quantile(dphi,0.1, na.rm = T),
    dpsimq10 = quantile(dpsi,0.1, na.rm = T),
    g2mq10 = quantile(g2,0.1, na.rm = T),
    tairq10 = quantile(tair,0.1, na.rm = T),
    rhq10 = quantile(rh,0.1, na.rm = T),
    NDVImq10 = quantile(NDVI,0.1, na.rm = T),
    PRImsq10 = quantile(PRI,0.1, na.rm = T),
    PSNDmq10 = quantile(PSND,0.1, na.rm=T),
    CRI2mq10 = quantile(CRI2,0.1, na.rm=T),
    Hzmositmq10 = quantile(Hzmoist,0.1, na.rm = T),
    Fluxmq10 = quantile(Flux,0.1, na.rm = T),
    VPDmq10 = quantile(VPD,0.1, na.rm = T),
    tmq10 = quantile(tair,0.1, na.rm = T),
    dthetamq90 = quantile(dtheta,0.9, na.rm = T),
    dphimq90 = quantile(dphi,0.9, na.rm = T),
    dpsimq90 = quantile(dpsi,0.9, na.rm = T),
    tairq90 = quantile(tair,0.9, na.rm = T),
    rhq90 = quantile(rh,0.9, na.rm = T),
    g2mq90 = quantile(g2,0.9, na.rm = T),
    NDVImq90 = quantile(NDVI,0.9, na.rm = T),
    PRImq90 = quantile(PRI,0.9, na.rm = T),
    PSNDmq90 = quantile(PSND,0.9, na.rm=T),
    CRI2mq90 = quantile(CRI2,0.9, na.rm=T),
    Hzmositmq90 = quantile(Hzmoist,0.9, na.rm = T),
    Fluxmq90 = quantile(Flux,0.9, na.rm = T),
    tmq90 = quantile(tair,0.9, na.rm = T),
    VPDmq90 = quantile(VPD,0.9, na.rm = T),
    dthetammd = quantile(dtheta,0.5, na.rm = T),
    dphimmd = quantile(dphi,0.5, na.rm = T),
    dpsimmd = quantile(dpsi,0.5, na.rm = T),
    tairmd = quantile(tair,0.5, na.rm = T),
    rhmd = quantile(rh,0.5, na.rm = T),
    g2mmd = quantile(g2,0.5, na.rm = T),
    NDVImmd = quantile(NDVI,0.5, na.rm = T),
    PRImmd = quantile(PRI,0.5, na.rm = T),
    PSNDmmd = quantile(PSND,0.5, na.rm=T),
    CRI2mmd = quantile(CRI2,0.5, na.rm=T),
    Hzmositmmd = quantile(Hzmoist,0.1, na.rm = T),
    Fluxmmd = quantile(Flux,0.5, na.rm = T),
    VPDmmd = quantile(VPD,0.5, na.rm = T),
    tmsum = sum(tair/length(Flux)*720, na.rm = T),
    VPDmsum = sum(VPD/length(Flux)*720, na.rm = T),
    Fluxmsum = sum(Flux/length(Flux)*720, na.rm = T),
    #growth = max(growth, na.rm=T)
  )

SumaryAllyData$month = 0


SummmaryData = dplyr::union(SumaryAllmData,SumaryAllyData)

SummmaryData = SummmaryData %>% arrange(id)


write.csv(SummmaryData, file="SummaryAllTTDatawGrowth.csv")

SumaryGrowthmDataSite = SummmaryData  %>% group_by(Species,Site,month) %>% summarise(
  mean = mean(growth, na.rm=T),
  sd = sd(growth, na.rm=T),
  max = quantile(growth,0.9, na.rm = T),
  median = quantile(growth,0.5, na.rm = T),
  min = quantile(growth,0.1, na.rm = T),
)

SumaryGrowthmData = SummmaryData  %>% group_by(Species,month) %>% summarise(
  mean = mean(growth, na.rm=T),
  sd = sd(growth, na.rm=T),
  max = quantile(growth,0.9, na.rm = T),
  median = quantile(growth,0.5, na.rm = T),
  min = quantile(growth,0.1, na.rm = T),
)




SumaryTairmDataSite = SummmaryData  %>% group_by(Species,Site,month) %>% summarise(
  mean = mean(tair, na.rm=T),
  sd = sd(tair, na.rm=T),
  max = quantile(tair,0.9, na.rm = T),
  median = quantile(tair,0.5, na.rm = T),
  min = quantile(tair,0.1, na.rm = T),
)

SumaryRhmData = SummmaryData  %>% group_by(Species,Site,month) %>% summarise(
  mean = mean(rh, na.rm=T),
  sd = sd(rh, na.rm=T),
  max = quantile(rh,0.9, na.rm = T),
  median = quantile(rh,0.5, na.rm = T),
  min = quantile(rh,0.1, na.rm = T),
)



write.csv(SumaryGrowthmDataSite, file="SumaryGrowthmDataSite.csv")
write.csv(SumaryGrowthmData, file="SumaryGrowthmData.csv")

SumaryGrowthmDataSite %>% as.data.frame()


TRSKdata[[2]]$id%>%unique()


  


########################## VTA PCA and CROCOR ##################################

#library(devtools)
#install_github("vqv/ggbiplot")

library(ggbiplot)
library(Hmisc)
library(corrplot)
library(stringr)


AllTTDataWgrowth <- read_csv("AllTTDataWgrowth.csv", 
                             col_types = cols(X1 = col_skip(), 
                             datetime = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                            serv_datetime = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
                            time = col_datetime(format = "%Y-%m-%d %H:%M:%S")))

names(SummaryWithVTA)
SummaryWithVTA = left_join(AllTTDataWgrowth, VTA, by="id")

SummWithVTA = SummaryWithVTA%>%group_by(id,leaves_quality,crown_density,crown_codominance,wounds,
            cracks,cavities,fungi,trunk_inclination,curvature,trunk_codominance,pruning,trunk_rot,
            trunk_cavities,outgrowthing,insects,epicormic_shoots,superficial_roots,raised_roots,winding_roots,
            roots_damages,asphalt, Species,Site,)%>%summarise(DBH=mean(DBH))
write.csv(SummWithVTA,file="SummWithVTA.csv")

VTA = read_delim("VTA.csv", ";", escape_double = FALSE,trim_ws = TRUE)
VTA$id = paste("218A",VTA$id, sep="")
VTA = VTA %>%mutate(score = rowMeans(.[2:23], na.rm = T))
VTAscore =VTA %>% as.data.frame %>% dplyr::select(id, score)
library(readxl)
TT_MOS_DESC <- read_excel("TT_MOS_DESC.xlsx", col_types = c("text", "text", "numeric", 
                          "numeric", "numeric", "text", "text", "numeric", "skip", "skip", "numeric"))
TT_DESC = left_join(TT_MOS_DESC,VTAscore, by = "id")
write.csv(TT_DESC, file="full_TT_desc.csv")


VTA_PCA = left_join(SumaryAllmData, VTA, by="id")
VTA_PCA.raw = VTA_PCA[,4:91]%>%select(-g2mmd)%>%select(-g2mq10)
VTA_PCA.raw$growth[is.infinite(VTA_PCA.raw$growth)] = 0
VTA_PCA.raw = na.exclude(VTA_PCA.raw) 

VTA.pca = prcomp(VTA_PCA.raw, center = TRUE, scale = TRUE)

ggbiplot(VTA.pca,groups = VTA.pca$month, obs.scale = 1, var.scale = 1, ellipse=TRUE, circle = TRUE) +
  scale_color_discrete(name = '') +theme_bw()+xlim(-5,5)

ggscreeplot(VTA.pca, type = c("pev", "cev"))


################################# CORELLOGRAMS #################################

VTA.cor = cor(VTA_PCA.raw )

col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = VTA.cor, col = col, symm = TRUE)

#ttleaf.sign.filtered = ttleaf.pca.filtered[,c(22:25,15:18,2,7,12)]
res2 <- rcorr(as.matrix(VTA_PCA.raw))

corrplot(res2$r, type="full", order="FPC", 
         p.mat = res2$P, sig.level = 0.01, insig = "blank")


out_of_two_sigma(data$Flux)

var_n =  na.exclude(data$Flux) > mean(na.exclude(data$Flux), na.rm=T) + 2*sd(as.vector(data$Flux), na.rm = T) | 
        na.exclude(data$Flux) < mean(na.exclude(data$Flux), na.rm=T) - 2*sd(na.exclude(data$Flux), na.rm=T)

class(data$Flux)
AllData$pulses



AllData$Hz%>%max


tt_imported = TRSKdata[[2]]
#tt_imported = tt_imported %>% mutate(same_time = time == datetime )
graph = ggplot(tt_imported )+
  geom_point( aes(x=time,y=volt, colour=imported%>%as.factor), size=.02)+
  facet_wrap(~id,  nrow = 6, strip.position = "bottom")
graph+theme_bw()


ttt = tt_imported %>% filter(id=="218A0204")

tt_acer1 = tt_imported %>% filter(Species == "Acer platanoides") %>% 
  filter(hour > 8 & hour < 17) %>% filter(b_O_600 > 5000)

tt_acer2 = BLTNdata[[2]] %>% filter(Species == "Acer platanoides") %>% 
  filter(hour > 8 & hour < 17) %>% filter(b_O_600 > 5000)

tt_acer3 = RUDNdata[[2]]  %>% filter(Species == "Acer platanoides ") %>% 
  filter(hour > 8 & hour < 17) %>% filter(b_O_600 > 5000)

tt_acer = rbind(tt_acer1,tt_acer2,tt_acer3)

+ex = tt_imported %>% filter(id == "218A0115")
#library(openxlsx)
write.xlsx(ex, file = "218A0115.xlsx")


data = RUDNdata[[2]]
data$Flux_f = two_sigma_weekly_flagging(data, Flux)
plots = data %>% group_by(Species) %>%
  do(
    plots = ggplot(data = .) + aes(x = time, y = Flux) +
      geom_smooth() + ggtitle(.$Species)
  )

plots$plots[1]


data2 = data %>% two_sigma_grouped_flagging(Flux, id, Species, "_fa")
data2$Flux_fa
ggplot(data = data%>%filter(Species == "Tilia cordata " ,!Flux_f, out_of_two_sigma(Flux)))+
  geom_point(aes(x=time, y=Flux))


plotbunch[3,2][[1]]


summ = data%>%group_by(Species, doy, id) %>% mutate(update = length(id))%>%
  summarise(sFlux = sum(Flux), readings = mean(update), diam = mean(diam, na.rm=T)) %>%
  mutate(suFlux = sFlux*readings/24)%>%
  group_by(Species, doy) %>% summarise(Flux = mean(suFlux, na.rm=T), d = mean(diam))

summ%>% as.data.frame()

ggplot(data = summ%>%filter(Species=="Acer platanoides "))+
  geom_smooth(aes(x=doy,y=Flux))+geom_point(aes(x=doy,y=Flux),size=.1)+ylim(0,100)+theme_bw()

####################### Sap flow velocity for Giovanna List ####################
tData  = TIMdata[[2]] 
l = list(c("218A0289","218A0298"),
         c("218A0143","218A0222","218A0218"),
         c("218A0061","218A0180"))



for(s in 1:length(l)){
  print(s)
  ttData = tData%>% filter(id %in% l[[s]]) %>% group_by(id,doy) %>% summarise(v = sum(u, na.rm=T)/24*length(u))
  graph = ggplot()
  for(i in 1:length( l[[s]] ) ){
    t = ttData %>% filter(id == l[[s]][i])
    graph = graph + geom_line(data = t, aes(x=doy,y=v, linetype=l[[s]][i]), color=i,size=.1)
  }
  graph = graph+theme_bw()+ylim(0,0.3)
  filename = c("Tillia.png","Quercus.png","Acer.png")[s]
  ggsave(filename, graph,device = "png",width=5.83,height=4.13,units="in")
  
}

for(s in 1:length(l)){
  print(s)
  ttData = tData%>% filter(id %in% l[[s]])%>%filter(!out_of_two_sigma(u)) 
  graph = ggplot()
  for(i in 1:length( l[[s]] ) ){
    t = ttData %>% filter(id == l[[s]][i])
    graph = graph + geom_line(data = t, aes(x=time,y=u), color=i,size=.1)
  }
  graph = graph+theme_bw()+ylim(0,0.05)
  filename = c("Tillia_h.png","Quercus_h.png","Acer_h.png")[s]
  ggsave(filename, graph,device = "png",width=5.83,height=4.13,units="in")
  
}

#################################FREE PROXIMITY################################
data = RUDNdata[[2]]
proximity_RUDN = read_delim("proximity_RUDN.csv",";", 
                            escape_double = FALSE, 
                            col_types = cols(X4 = col_skip(),
                                             X5 = col_skip(), X6 = col_skip(), 
                                             X7 = col_skip(), de = col_double()), 
                            trim_ws = TRUE)
data = left_join(data, proximity_RUDN, by="id")  

data$volt[which((round(data$dist13, digits=2) == data$ds & data$doy <160) | 
                  (round(data$dist13, digits =2) == data$de & data$doy >250 ))]
data$time[which((round(data$dist13, digits=2) == data$ds & data$doy <160) | 
                  (round(data$dist13, digits =2) == data$de & data$doy >250 ))]

pred = ggplot(data %>% filter(b_W_860 <10) %>% filter((volt > 3.99 & volt<4.05)))+
  geom_point(aes(x=doy,y=dist13 ), size=.1)+
  geom_smooth(aes(x=doy,y=dist13),method="loess" ,size=.1)+
  stat_smooth(aes(x=doy,y=dist13),method="loess" ,size=.1)+
  facet_wrap(~id)+ylim(0,50)+theme_bw()
ggplot_build(pred)$data[[2]]


#####LOOKING FOR PROBLEMS
sigma3_weekly_flaging  = function(data, var_name){
  #var_names = c("time",var_name)
  data = data %>% group_by(week) %>% mutate(s3flag = ) %>% 
}

RUDN = RUDNdata[[2]]


id2show = c("218A0100","218A0106","218A0109","218A0110","218A0155","218A0115",
            "218A0171","218A0060","218A0064","218A0141","218A0142","218A0114")
RUDN = RUDN %>%filter(doy>180)
RUDN = RUDN  %>% filter(id %in% id2show)
RUDN %>% group_by(id)%>% summarise(Sp = as.factor(Species)%>% levels)

RUDN = two_sigma_weekly_flagging(RUDN,u)
RUDN = two_sigma_weekly_flagging(RUDN,gz2)
RUDN = two_sigma_weekly_flagging(RUDN,psi)
RUDN = radiation_flagging(RUDN)
RUDN$NDVI[!RUDN$rad_flag]=NA
RUDN = RUDN %>% group_by(doy,id) %>% mutate(psi_d = mean(psi, na.rm = T),
                                            phi_d = mean(phi, na.rm = T), 
                                            theta_d = mean(theta, na.rm = T),
                                            ) %>% ungroup()


RUDN = RUDN %>% group_by(id) %>% mutate(mxt=max(theta_d-theta),
                                        mnt=min(theta_d-theta))


graph = ggplot(RUDN%>%filter(!u_f))+
  geom_step( aes(x=time,y=VPD/25), color=alpha(4,0.5), size=.02, )+
  geom_line( aes(x=time,y=u), size=.02)+
  geom_smooth( aes(x=time,y=NDVI/8), color=3, size=.02, span=1)+
  #geom_point( aes(x=time,y=NDVI/8), color=3, size=.02)+
  scale_y_continuous(name = expression(Sap~flow~density~" "~l~h^{-1}~m^{-2}),
                     sec.axis = sec_axis(~.*25, name = "VPD, kPa", labels = function(b) { round(b * 1, 2)}),
                     limits = c(0,0.15))+
  facet_wrap( vars(Species,id),  nrow = 4, strip.position = "bottom",labeller = label_bquote(.(Species) - .(id)))
graph+theme_bw()


graph = ggplot(RUDN)+
  facet_wrap( vars(Species,id),  nrow = 4, strip.position = "bottom",labeller = label_bquote(.(Species) - .(id)))+
  geom_line(aes(x=time,y=theta_d-theta ), color=1, size=0.02)+
  geom_hline(aes(yintercept=mxt), color=2,linetype="dashed", size=.02)+
  geom_hline(aes(yintercept=mnt), color=2,linetype="dashed", size=.02)+
  scale_y_continuous(name = expression(Deviation~from~daily~mean~of~theta~angle~','~degree))+
  theme_bw()
  #geom_smooth(aes(x=time,y=theta_d-theta ), color=3, size=0.02, se=T, span=.1,method = "loess")+
  #scale_y_continuous(limits=c(-10,10))+
  
graph


RUDN


RUDN = RUDNdata[[2]]
RUDN = RUDN %>%filter(doy>180)
RUDN %>% group_by(id)%>% summarise(Sp = as.factor(Species)%>% levels)
RUDN = two_sigma_weekly_flagging(RUDN,u)
RUDN = two_sigma_weekly_flagging(RUDN,gz2)
RUDN = two_sigma_weekly_flagging(RUDN,theta)
RUDN = radiation_flagging(RUDN)
RUDN$NDVI[!RUDN$rad_flag]=NA
RUDN = RUDN %>% group_by(doy,id) %>% mutate(psi_d = mean(psi, na.rm = T),
                                            phi_d = mean(phi, na.rm = T), 
                                            theta_d = mean(theta, na.rm = T),
) %>% ungroup()


HORT = HORTdata[[2]] %>% group_by(id) %>% mutate(mxt=max(theta_d-theta),
                                        mnt=min(theta_d-theta))


graph = ggplot(HORT%>%filter(!u_f))+
  geom_step( aes(x=time,y=VPD/25), color=alpha(4,0.5), size=.02, )+
  geom_line( aes(x=time,y=u), size=.02)+
  geom_smooth( aes(x=time,y=NDVI/8), color=3, size=.02, span=1)+
  #geom_point( aes(x=time,y=NDVI/8), color=3, size=.02)+
  scale_y_continuous(name = expression(Sap~flow~density~" "~l~h^{-1}~m^{-2}),
                     sec.axis = sec_axis(~.*25, name = "VPD, kPa", labels = function(b) { round(b * 1, 2)}),
                     limits = c(0,0.15))+
  facet_wrap( vars(Species,id),  nrow = 4, strip.position = "bottom",labeller = label_bquote(.(Species) - .(id)))
graph+theme_bw()


graph = ggplot(HORT%>%filter(!theta_f))+
  facet_wrap( vars(Species,id),  nrow = 2, strip.position = "bottom",labeller = label_bquote(.(Species) - .(id)))+
  geom_line(aes(x=time,y=theta_d-theta ), color=1, size=0.02)+
  geom_hline(aes(yintercept=mxt), color=2,linetype="dashed", size=.02)+
  geom_hline(aes(yintercept=mnt), color=2,linetype="dashed", size=.02)+
  scale_y_continuous(name = expression(Deviation~from~daily~mean~of~theta~angle~','~degree))+
  theme_bw()
#geom_smooth(aes(x=time,y=theta_d-theta ), color=3, size=0.02, se=T, span=.1,method = "loess")+
#scale_y_continuous(limits=c(-10,10))+

graph

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
                   limits = c(as.Date("01.05.2019", "%d.%m.%Y"), as.Date("01.10.2019", "%d.%m.%Y")), name = NULL)
     +scale_y_continuous( limits = c(0,75), name = expression(Скорость~" "~сокотечения~" "~л~д^-1))
     +scale_color_viridis_d(guide=FALSE)
     +scale_fill_viridis_d(name = "Уровень \nантропогенной\nнагрузки")
     +theme_bw()
     
  )
data$load_score%>%unique()
plotbunch[1,2][[1]]
plotbunch[2,2][[1]]
plotbunch[4,2][[1]]
plotbunch[8,2][[1]]
plotbunch[10,2][[1]]
plotbunch[11,2][[1]]
plotbunch[16,2][[1]]
plotbunch[20,2][[1]]





write.csv(file="acer_spectra.csv", tt_acer)
