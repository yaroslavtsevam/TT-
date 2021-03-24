source("code/TTcalc_site_7.R")
library(ggpmisc)
library(extrafont)
library(Rmisc)
library(timetk)
library(slider)
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
                     "data/TT_desc_20.csv",
                     "RUDN",
                     verbose = "file")

TIMdata=TTcalc_site(c("http://naturetalkers.altervista.org/C18A0029/ttcloud.txt",
                      "http://naturetalkers.altervista.org/C18A0030/ttcloud.txt"),
                    installation_start,
                    import_folder_name = "data/backup/TIMR",
                    first_imported_dates_reconstructed = T,
                    "data/TT_desc_20.csv",
                    "TIMIRYAZEV",
                    verbose = "file")


BLTNdata=TTcalc_site("http://naturetalkers.altervista.org/C18A0024/ttcloud.txt",
                     installation_start,
                     import_folder_name = NULL,
                     first_imported_dates_reconstructed = F,
                     "data/TT_desc_20.csv",
                     "BOLOTNAYA",
                     verbose="file")

SCHLdata=TTcalc_site("http://naturetalkers.altervista.org/C18A0023/ttcloud.txt",
                     installation_start,
                     import_folder_name = NULL,
                     first_imported_dates_reconstructed = F,
                     "data/TT_desc_20.csv",
                     "SCHOOL1234",
                     verbose = "file")

SHERdata=TTcalc_site("http://naturetalkers.altervista.org/C1870015/ttcloud.txt",
                     installation_start,
                     import_folder_name = NULL,
                     first_imported_dates_reconstructed = F,
                     "data/TT_desc_20.csv",
                     "SCHERBINKA",
                     verbose = "file")

GRDNdata  = TTcalc_site("http://naturetalkers.altervista.org/C18A0026/ttcloud.txt",
                        installation_start,
                        import_folder_name = NULL,
                        first_imported_dates_reconstructed = F,
                        "data/TT_desc_20.csv",
                        "GARDEN",
                        verbose = "file")


TRSKdata = TTcalc_site("http://naturetalkers.altervista.org/C18A0027/ttcloud.txt",
                       installation_start,
                       import_folder_name = NULL,
                       first_imported_dates_reconstructed = F,
                       "data/TT_desc_20.csv",
                       "TROITSK",
                       verbose = "file")

GBSSdata = TTcalc_site("http://naturetalkers.altervista.org/C18A0028/ttcloud.txt",
                       installation_start,
                       import_folder_name = NULL,
                       first_imported_dates_reconstructed = F,
                       "data/TT_desc_20.csv",
                       "GBS_STAB",
                       verbose = "con")

GBSMdata = TTcalc_site("http://naturetalkers.altervista.org/C18A0229/C18A0229_cloud_rec.txt",
                       installation_start,
                       import_folder_name = NULL,
                       first_imported_dates_reconstructed = F,
                       "data/TT_desc_20.csv",
                       "GBS_MOIST",
                       verbose = "con")

ELTSdata = TTcalc_site(c("http://naturetalkers.altervista.org/C18A0230/ttcloud.txt",
                         "http://naturetalkers.altervista.org/C1850003/ttcloud.txt"),
                       installation_start,
                       import_folder_name = NULL,
                       first_imported_dates_reconstructed = F,
                       "data/TT_desc_20.csv",
                       "ELETS",
                       verbose = "file")
GPdata    = TTcalc_site("http://naturetalkers.altervista.org/C1880221/ttcloud.txt",
                        installation_start,
                        import_folder_name = NULL,
                        first_imported_dates_reconstructed = F,
                        "data/TT_desc_20.csv",
                        "GORKYPARK",
                        verbose = "con")






AllData = rbind(
  RUDNdata[[2]],
  TIMdata[[2]],
  BLTNdata[[2]],
  GRDNdata[[2]],
  TRSKdata[[2]],
  SCHLdata[[2]],
  SHERdata[[2]],
  GBSSdata[[2]],
  ELTSdata[[2]],
  GPdata[[2]]
  
)


AllData = rbind(
  GBSMdata[[2]]
  
)


AllData$month = month(AllData$time)


AllDataRaw = rbind(
  RUDNdata[[1]],
  TIMdata[[1]],
  BLTNdata[[1]],
  HORTdata[[1]],
  TRSKdata[[1]],
  SCHLdata[[1]],
  SHERdata[[1]]
)


AllDataRaw = rbind(
  TRSKdata[[1]]
)


write.csv(AllData,"results//all_data.csv")
write.csv(AllDataRaw,"results//all_data_raw.csv")




########################### METEO FROM MSU #####################################



read_meteo = function(folder){
  #folder="MSU"
  meteo_files = dir(folder,recursive=T)
  all_meteo_data = data.frame()
  for (file in meteo_files){
    meteo_data <- read_table2(paste(folder,file,sep="/"), 
                              col_types = cols(    `Cр.напр.1` = col_skip(), 
                                                   `Cр.ск.1` = col_skip(), 
                                                   `Tпов.макс_3ч` = col_skip(), 
                                                   `Tпов.мин_3ч` = col_skip(), 
                                                   X46 = col_skip(), 
                                                   `Давление` = col_skip(), 
                                                   `Давление_ур.моря` = col_skip(), 
                                                   `Знач.бар.тенденции` = col_skip(), 
                                                   `МЭД` = col_skip(),
                                                   `Макс.скорость_3ч1` = col_double(), 
                                                   `Мгн.направление1` = col_skip(), 
                                                   `Порыв1` = col_double(),
                                                   `Порыв_2` = col_double(),
                                                   `Мгн.скорость1` = col_skip(), 
                                                   `Осадки_12ч` = col_double(), 
                                                   `Осадки_24ч` = col_skip(), 
                                                   `Осадки_Макс.инт.10мин` = col_skip(), 
                                                   `Осадки_Макс.инт.1ч` = col_skip(), 
                                                   `Осадки_Мин.инт.1ч` = col_skip(), 
                                                   `Отн.влажность` = col_double(), 
                                                   `Порыв1` = col_skip(), 
                                                   `Твозд.` = col_double(), 
                                                   `Твозд.макс_12ч` = col_skip(), 
                                                   `Твозд.макс_3ч` = col_skip(), 
                                                   `Твозд.мин_12ч` = col_skip(), 
                                                   `Твозд.мин_3ч` = col_skip(), 
                                                   `Твозд.ср_сутки` = col_skip(), 
                                                   `Точка_росы` = col_skip(), 
                                                   `Упруг.вод.пара(e)` = col_skip(), 
                                                   `Хар.бар.тенденции` = col_skip(), 
                                                   `время` = col_time(format = "%H:%M:%S"), 
                                                   `дата` = col_date(format = "%d.%m.%Y"), 
                                                   `местн.DT` = col_skip()), 
                              locale = locale(encoding = "WINDOWS-1251"))
    all_meteo_data = rbind(all_meteo_data, meteo_data)
  }
  
  all_meteo_data = all_meteo_data[,c(1:6,21)]
  names(all_meteo_data) = c("date","time","ta","Rh","WBS","WS","Pr")
  all_meteo_data  = all_meteo_data %>% 
    mutate(hour=hour(time), doy=yday(date), year = year(date)) 
  #data = left_join(data,all_meteo_data, by=c("year","doy","hour"))
  return(all_meteo_data)
  
}  


meteo_data  = read_meteo("data/meteo/MSU/2019") %>% group_by(year,doy,hour)%>%
  summarise(Rh = mean(Rh, na.rm = T),ta = mean(ta, na.rm = T), WBS=mean(WBS, na.rm = T),WS=mean(WS, na.rm = T), Pr=mean(as.double(Pr)/12, na.rm=T))

meteo_data_2  = read_meteo("data/meteo/MSU/2020") %>% group_by(year,doy,hour)%>%
  summarise(Rh = mean(Rh, na.rm = T),ta = mean(ta, na.rm = T), WBS=mean(WBS, na.rm = T), WS=mean(WS, na.rm = T), Pr=mean(as.double(Pr)/12, na.rm=T))

meteo_data = rbind(meteo_data,meteo_data_2)

AllData = left_join(AllData,meteo_data, by=c("year","doy","hour"))
#meteo_data$doy %>% max
#data$doy
#data$Rh%>%is.na()%>%which()%>%length()

#Climat comparison graphs

data = AllData %>% filter(Site=="RUDN")
data = left_join(data,meteo_data, by=c("year","doy","hour"))

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

rh_pic = ggplot(data = data%>%filter(rh>25))+
  geom_point(aes(x=as.Date(time),y=Rh),color=2, size=.1, alpha=1/20)+
  geom_point(aes(x=as.Date(time),y=rh),color=3, size=.1, alpha=1/20)+
  geom_smooth(aes(x=as.Date(time),y=Rh), model="loess", color=2,span=.01)+
  geom_smooth(aes(x=as.Date(time),y=rh), model="loess", color=3,span=.1)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 week",
               limits = c(as.Date("01.05.2019", "%d.%m.%Y"),NA), name = NULL)+
  scale_y_continuous( limits = c(0,100), name = expression("Влажность воздуха"~" %"))+
  theme_bw()+
  theme(text = element_text(family = "Arial"))

ggsave("Comparison_of_rh_in_RUDNVvsMSU.png", rh_pic,device = "png",width=8,height=6,units="in")


NDVI_pic = ggplot(data=data %>% filter(NDVI<0.98 & NDVI > 0, id %in% googRUDN))+
  geom_point(aes(x=as.Date(time),y=NDVI),color=3, size=.1, alpha=1/20)+
  geom_smooth(data=data %>% filter(NDVI<0.98 & NDVI > 0, id %in% googRUDN)%>% filter(year==2019),aes(x=as.Date(time),y=NDVI+.1), model="loess", color=3,span=1)+
  geom_smooth(data=data %>% filter(NDVI<0.98 & NDVI > 0, id %in% googRUDN)%>% filter(year==2020),aes(x=as.Date(time),y=NDVI+.1), model="loess", color=3,span=1)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 week",
               limits = c(as.Date("01.05.2019", "%d.%m.%Y"),NA), name = NULL)+
  scale_y_continuous( limits = c(0,1), name = expression(NDVI))+
  theme_bw()+
  theme(axis.text.x = element_text(size=11, angle=90, hjust=1), axis.text.y = element_text(size=11))
ggsave("NDVI_RUDN_2y.png", NDVI_pic,device = "png",width=5.83,height=4.13,units="in")

flux_dat = AllData%>%group_by(Species, doy, id,year) %>% filter(Site=="RUDN")%>%mutate(update = length(id))%>%
  summarise(Flux = sum(Fluxal, na.rm=T)/n()*24,  diam = mean(diam, na.rm=T), Pr = unique(Pr) %>% sum(),
            time=mean(time,na.rm=T))
flux_dat$Flux[flux_dat$Flux > 10 & flux_dat$doy > 300] = 0
flux_dat$Flux[flux_dat$Flux > 10 & flux_dat$doy < 60] = 0

FLUX_pic = ggplot(flux_dat %>% filter( Flux > 0), aes(x=as.Date(time), y = Flux))+
  geom_point(color=3, size=.1, alpha=1/10)+
  geom_smooth(data=flux_dat %>% filter(year==2019, Flux > 0 ),method = "gam", color=3)+
  geom_smooth(data=flux_dat %>% filter(year==2020, Flux > 0),method = "gam",color=3)+
  geom_smooth(data=flux_dat , aes(x=as.Date(time), y =Pr*20),method = "loess", span=.3)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 week",
                 limits = c(as.Date("01.05.2019", "%d.%m.%Y"),NA), name = NULL)+
  scale_y_continuous( limits = c(0,40),sec.axis = sec_axis(~., name = "7 дневное бегущее среднее дневных сумм осадков, мм"), name = expression(Скорость~" "~сокотечения~" "~"л"~"д"^-1))+
 theme_bw()+
theme(axis.text.x = element_text(size=11, angle=90, hjust=1), axis.text.y = element_text(size=11))

ggsave("FLUX_RUDN_2y_with_prcp.png", FLUX_pic,device = "png",width=5.83,height=4.13,units="in")


AllData$is_corn_lab = "Хвойные"
AllData$is_corn_lab[!AllData$is_corniferous]="Широколиственные"

AllData %>% filter(Site == "RUDN",year==2020) %>% 
  group_by(doy, Species, is_corn_lab,id) %>% summarise(time = mean(time,na.rm=T), Flux = sum(Flux2, na.rm = T)*24/n()) %>%
  ggplot(aes(x = as.Date(time), y = Flux))+
  geom_point(aes(color=Species), size=.2, alpha=2/10)+
  geom_smooth(aes(color=Species), span = 3, na.rm = T)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 week",
               limits = c(as.Date("01.05.2020", "%d.%m.%Y"),NA), name = NULL)+
  scale_y_continuous( limits = c(0,50), name = expression(Скорость~" "~сокотечения~" "~"л"~"д"^-1))+
  facet_wrap(~is_corn_lab)+
  theme_bw()

dat = AllData %>% filter(doy> 120 & doy< 300) %>% filter(Site == "RUDN", Fluxal>0 & Fluxal <60 ) %>% 
  group_by(doy, Species, is_corn_lab,id) %>% 
  summarise(time = mean(time,na.rm=T), Flux = sum(Fluxal, na.rm = T)*24/n(), year=year(time), month=month(time)) 
dat = AllData %>% filter(doy> 120 & doy< 300) %>% filter(Site == "TIMIRYAZEV", Fluxal>0 & Fluxal <60 ) %>% 
  group_by(doy, Species, is_corn_lab,id) %>% 
  summarise(time = mean(time,na.rm=T), Flux = sum(Fluxal, na.rm = T)*24/n(), year=year(time), month=month(time)) 

dat = AllData %>% filter(doy> 120 & doy< 300) %>% filter(Site == "BOLOTNAYA", Fluxal>0 & Fluxal <60 ) %>% 
  group_by(doy, Species, is_corn_lab,id) %>% 
  summarise(time = mean(time,na.rm=T), Flux = sum(Fluxal, na.rm = T)*24/n(), year=year(time), month=month(time)) 


  ggplot(dat)+
  geom_point(data = dat %>% filter(year==2019, month <10), aes(x = as.Date(time), y = Flux,color=Species), size=.2, alpha=2/10)+
  geom_smooth(data = dat%>% filter(year==2019,month <10), aes(x = as.Date(time), y = Flux,color=Species), span = 3, se=F)+
  geom_point(data = dat%>% filter(year==2020,month >3), aes(x = as.Date(time), y = Flux,color=Species), size=.2, alpha=2/10)+
  geom_smooth(data = dat%>% filter(year==2020,month >3), aes(x = as.Date(time), y = Flux,color=Species), span = 3, se=F)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 week",
               limits = NULL, name = NULL)+
  scale_y_continuous( limits = c(0,50), name = expression("Скорость сокотечения л/д"))+
  facet_grid(is_corn_lab~year, scales = "free")+
  theme_bw()+
  theme(legend.title = element_blank())

######################################RUDN
  AllData = AllData %>% mutate(doy = yday(time), year=year(time), month=month(time))
  
  dat = AllData %>% filter(doy> 120 & doy< 300) %>% filter(Site == "RUDN", NDVIc>0 & NDVIc <.98 ) %>% 
    group_by(doy, Species, is_corn_lab,id) %>% 
    summarise(time = mean(time,na.rm=T), NDVI = quantile(NDVIc,0.7,na.rm=T), year=year(time), month=month(time)) 
  
  dat = AllData %>% filter(doy> 120 & doy< 300) %>% filter(Site == "TIMIRYAZEV", NDVIc>0 & NDVIc <.98 ) %>% 
    group_by(doy, Species, is_corn_lab,id) %>% filter(!(Species=="Quercus robur" & year == 2020)) %>%
    summarise(time = mean(time,na.rm=T), NDVI = quantile(NDVIc,0.7,na.rm=T), year=year(time), month=month(time)) 
  
  dat = AllData %>% filter(doy> 120 & doy< 300) %>% filter(Site == "TROITSK", NDVIc>0 & NDVIc <.98 ) %>% 
    group_by(doy, Species, is_corn_lab,id) %>% 
    summarise(time = mean(time,na.rm=T), NDVI = quantile(NDVIc,0.7,na.rm=T), year=year(time), month=month(time)) 
  
  
  ggplot(dat)+
    geom_point(data = dat %>% filter(year==2019, month <10), aes(x = as.Date(time), y = NDVI,color=Species), size=.2, alpha=2/10)+
    geom_smooth(data = dat%>% filter(year==2019,month <10), aes(x = as.Date(time), y = NDVI,color=Species), span = 1, se=F)+
    geom_point(data = dat%>% filter(year==2020,month >4), aes(x = as.Date(time), y = NDVI,color=Species), size=.2, alpha=2/10)+
    geom_smooth(data = dat%>% filter(year==2020,month >4), aes(x = as.Date(time), y = NDVI,color=Species), span = 1, se=F)+
    scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 week",
                 limits = NULL, name = NULL)+
    scale_y_continuous( limits = c(0,1), name = expression("Скорость сокотечения л/д"))+
    facet_grid(is_corn_lab~year, scales = "free")+
    theme_bw()+
    theme(legend.title = element_blank())
  theme_bw()





googRUDN = AllData %>% group_by(id) %>% summarise(VND=var(NDVI, na.rm=T))%>%filter(VND < 0.1) %>% select(id) 
AllData$NDVI %>% summary
googRUDN=googRUDN[[1]]
new_VTA = read_delim(file="data/TT_desc_20_full.csv", delim=";") %>% select(Site,id,VTA_score_2020)
AllData = AllData %>% left_join(new_VTA, by=c("Site","id"))





#####################   AGE d#################################################

# ggplot(data = AllData %>% group_by(id, Species,Site)%>%summarise(DBH = mean(DBH)))+geom_histogram(aes(x= DBH))
# 
# AllData = AllData%>%mutate(AgeGroup = case_when(
#                                       DBH < 60 ~"Первая",
#                                       DBH > 60 & DBH < 90 ~"Вторая",
#                                       DBH > 90 & DBH < 120 ~"Третья",
#                                       DBH > 120 & DBH < 150 ~"Четвертая",
#                                       DBH > 150 & DBH < 180 ~"Пятая",
#                                       DBH > 180  ~"Шестая"),
#                            AgeGroup  = factor(AgeGroup, levels = c("Первая","Вторая","Третья","Четвертая","Пятая","Шестая")))

# AllData = AllData%>%mutate(load = case_when(
#                                       Site == "BOLOTNAYA"  | Site =="SCHOOL1234" ~ "Оч. высокий",
#                                       Site == "RUDN"  | Site == "SCHERBINKA" ~ "Высокий",
#                                       Site == "TROITSK"  | Site =="GARDEN" ~ "Средний",
#                                       Site == "TIMIRYAZEV" ~ "Слабый"),
#   load  = factor(load, levels = c("Оч. высокий","Высокий","Средний","Слабый")))
# 
# AllData = AllData%>%mutate(load_score = case_when(
#   Site == "BOLOTNAYA"  | Site == "SCHOOL1234" ~ 1,
#   Site == "RUDN"  | Site == "SCHERBINKA" ~ 2,
#   Site == "TROITSK" | Site == "GARDEN" ~ 3,
#   Site == "TIMIRYAZEV" ~ 4))
   
AllData = AllData%>%mutate(load = case_when(
  Site == "TIMIRYAZEV" ~ "Слабый",
  Site == "RUDN" | Site == "TROITSK"  | Site == "GARDEN" ~ "Средний",
  Site == "BOLOTNAYA"  | Site =="SCHOOL1234" | Site == "SCHERBINKA" ~ "Высокий"),
  load  = factor(load, levels = c("Слабый","Средний","Высокий")))

AllData = AllData%>%mutate(load_score = case_when(
  Site == "BOLOTNAYA"  | Site =="SCHOOL1234" | Site == "SCHERBINKA" ~ "3",
  Site == "RUDN" | Site == "TROITSK"  | Site == "GARDEN" ~ "2",
  Site == "TIMIRYAZEV" ~ "1")) 
   
AllData$load_score %>% unique()
AllData$insite_load = "высокий"
AllData$insite_load_score = 2
low_in_site_load_TTlist = c("062",100,102,107,108,110,113,114,115,118,119,122,123,124,129,135,137,
                             140,141,146,149,151,165,168,171,175,188,192,192,198,206,220,221,230,
                             233,238,240,245,283,"077","079","088",104,138,193,210,212,248,255,277,285,
                             "061","063","087","096",112,120,145,158,163,172,176,183,195,203,208,209,218,
                             219,226,246,249,256,259,263,279,282,287,289,291,293)
low_in_site_load_TTlist = paste("218A0",low_in_site_load_TTlist,sep="")                             
AllData$insite_load[AllData$id %in% low_in_site_load_TTlist] = "низкий"
AllData$insite_load_score[AllData$id %in% low_in_site_load_TTlist] = 1


########################### Correcting species names ####################################


AllData$Species[AllData$Species=="Quercus robur "]="Quercus robur"
AllData$Species[AllData$Species == "Acer platanoides "] = "Acer platanoides"
AllData$Species[AllData$Species == "Fraxinus "] = "Fraxinus excelsior"
AllData$Species[AllData$Species == "Quercus robur "] = "Quercus robur"
AllData$Species[AllData$Species == "Salix  alba"] = "Salix alba"
AllData$Species[AllData$Species == "Larix siberica "] = "Larix sibirica"
AllData$Species[AllData$Species == "Larix decidua"] = "Larix sibirica"
AllData$Species[AllData$Species == "Larix siberica"] = "Larix sibirica"
AllData$Species[AllData$Species == "Tilia cordata "] = "Tilia cordata"
AllData$Species[AllData$Species == "Acer  pseudoplatanus"] = "Acer platanoides"
AllData$Species %>% unique()
AllData$Site %>% unique()


library(ggTimeSeries)

AllData$year = year(AllData$time)
AllData$doy = yday(AllData$time)
AllData %>% filter(Site=="RUDN", Species=="Tilia cordata")%>%filter(year > 2018) %>%
  ggplot(aes(x=time, y=Flux, color=id ))+geom_point()+ylim(0,3)


AllData %>% filter(Site=="RUDN", id=="218A0141")%>%filter(year > 2018) %>%
  ggplot(aes(x=time, y=Flux, color=id ))+geom_point()+ylim(0,10)

AllData %>% filter(Site=="RUDN", id=="218A0151")%>%filter(year > 2018) %>%
  ggplot(aes(x=time, y=Flux, color=id ))+geom_point()+ylim(0,10)

AllData %>% filter(Site=="RUDN", Species=="Tilia cordata") %>% ungroup()%>% select(id)%>% unique()

AllData %>% filter(id %in% c("218A0060","218A0069","218A0090","218A0151","218A0141")) %>%
  filter(year ==2020, doy >180) %>% group_by(doy, id)%>% summarise (time=mean(time,na.rm = T),Fluxal=sum(Fluxal, na.rm = T)/n()*24)%>%
   ggplot(aes(x=time, y=Fluxal, color=id ))+geom_point(size=.1)+geom_smooth(aes(),method = "lm", se = F) + ylim(0,60)+theme_bw()
# Recalculate only by nt1 & nt2
AllData = AllData %>% mutate(diam = case_when(
    id =="218A0151" & year > 2019 & doy > 163 ~  95/pi,
    id =="218A0090" & year > 2019 & doy > 163 ~  93/pi,
    id =="218A0141" & year > 2019 & doy > 163 ~  82.3/pi, 
    TRUE ~ DBH / pi
  ))

AllData = AllData %>% group_by(id,year,doy) %>%
  mutate(dTal = nt2 - nt1, na.rm = T) %>%
  mutate(dTalm = max(dTal, na.rm=T)) %>%
  mutate(ual = 119*(10^-3)*(dTalm/dTal - 1)^1.231, na.rm = T) %>%
  mutate(Fluxal = ual*3600*(diam^1.8777)*0.755/10000, na.rm = T) %>%
  ungroup()

AllData = AllData %>% group_by(id,year) %>%
  mutate(ECf_Tb = -74.15*nt1+min(Ecf, na.rm=T)) %>%
  mutate(ECf_Tc = -70.2*nt1+min(Ecf)) %>% 
  mutate(dECfb = Ecf - ECf_Tb) %>%
  mutate(dECfc = Ecf - ECf_Tc) %>%
  mutate(dECfdb = (dECfb-min(dECfb, na.rm=T))/(max(dECfb,na.rm=T)-min(dECfb,na.rm=T))) %>%
  mutate(dECfdc = (dECfc-min(dECfc, na.rm=T))/(max(dECfc,na.rm=T)-min(dECfc,na.rm=T))) %>%
  mutate(VWWCb = -0.36*dECfdb + 0.45) %>%
  mutate(VWWCc = case_when(
    dECfdc > 0.95 ~ -1.3452*dECfdc + 1.422,
    dECfdc <= 0.95 ~ -0.0555*dECfdc + 0.195
  ))

################################ RUDN DRY VS WET
###################### IRRIGATION

AllData %>% filter(id %in% c("218A0090","218A0141")) %>%
  filter(year ==2020, doy >180 & doy <250) %>% group_by(doy, id)%>% 
  summarise (time=mean(time,na.rm = T),u=mean(ual, na.rm = T)/n()*24,Fluxal=sum(Fluxal, na.rm = T)/n()*24, VPD=mean(VPD,na.rm=T))%>%
  ggplot(aes(x=time, y=Fluxal, color=id ))+
  geom_step(size=.1)+geom_smooth(aes(),method = "lm", se = F) + 
  #geom_smooth(aes(),span=.1,size=.1, se = F)+
  #geom_smooth(aes(x=time, y=VPD*100, color=NULL), se=F, span=.2)+
  ylim(0,50)+theme_bw()






AllData %>% filter(id %in% c("218A0090","218A0141")) %>%
  filter(year ==2020, doy >180 & doy <250) %>% group_by(doy, id)%>% 
  summarise (time=mean(time,na.rm = T),Fluxal=sum(Fluxal, na.rm = T)/n()*24, NDVI=quantile(NDVI,0.9,na.rm=T))%>%
  ggplot(aes(x=time, y=NDVI, color=id ))+
  geom_point(size=.1)+geom_smooth(aes(), span=1,se = F) + 
  #geom_smooth(aes(),span=.1,size=.1, se = F)+
  #geom_smooth(aes(x=time, y=VPD*100, color=NULL), se=F, span=.2)+
  ylim(0,1)+theme_bw()


AllData %>% filter(id %in% c("218A0090","218A0141")) %>%
  filter(year ==2020, doy >180 & doy <250) %>% group_by(doy, id)%>% 
  summarise (time=mean(time,na.rm = T),Fluxal=sum(Fluxal, na.rm = T)/n()*24, NDVI=quantile(NDVI,0.9,na.rm=T))%>%as.data.frame()




###IRRIGATION
AllData %>% filter(id %in% c("218A0090")) %>%
  filter(year ==2020, doy >180 & doy <250) %>% 
  ggplot(aes(x=time, y=Pr))


#1*(16.2-10)*100/100*1000
# Near road, limited 
AllData %>% filter(id =="218A0141") %>%
  filter(year ==2020, doy >180) %>% group_by(doy, id)%>% 
  summarise (time=mean(time,na.rm = T),
             u=mean(ual, na.rm = T)/n()*24,
             Fluxal=sum(Fluxal, na.rm = T)/n()*24, 
             Pr = unique(Pr) %>% sum()*100) %>% group_by(id) %>%
            mutate(TR = cumsum(Fluxal),Pr_s = cumsum(Pr)) %>%
            mutate(WR = Pr_s + 6200-600+1304) %>%
            mutate(WB = (WR - TR)/100000*100 +10) %>%
  ggplot()+
  geom_line(aes(x=as.Date(time), y=WB*50), color=1, size=.7 ) + 
  geom_line(aes(x=as.Date(time), y=Pr_s), size=.7, color=4, linetype = 3) + 
  geom_line(aes(x=as.Date(time), y=TR),color = 3, size=.7, linetype = 3) + 
  geom_hline(aes(yintercept=500),color = 2, size=.7, linetype = 2) + 
  scale_x_date( date_minor_breaks = "1 day",
                limits = c(as.Date("01.07.2020", "%d.%m.%Y"),NA), name = NULL)+
  scale_y_continuous( name = "Транспирация, осадки, л",
                      sec.axis = sec_axis(~./50, name = "Усредненная по горизонтам объемная влажность почвы, %"),)+
  theme_bw()+
  theme(axis.text.x = element_text(size=11, angle=90, hjust=1), axis.text.y = element_text(size=12), axis.title.y = element_text(size=12))

#1*(13.5-10)*144/100*1000
AllData %>% filter(id =="218A0090") %>%
  filter(year ==2020, doy >180 & doy <250) %>% group_by(doy, id)%>% 
  summarise (time=mean(time,na.rm = T),
             u=mean(ual, na.rm = T)/n()*24,
             Fluxal=sum(Fluxal, na.rm = T)/n()*24, 
             Pr = unique(Pr) %>% sum()*144) %>% group_by(id) %>%
  mutate(TR = cumsum(Fluxal),Pr_s = cumsum(Pr)) %>%
  mutate(WR = (Pr_s + 5040-534+707))%>%
  mutate(WB = (WR - TR)/144000*100 +10) %>%
  ggplot()+
  geom_line(aes(x=as.Date(time), y=WB*50), color=1, size=.7 ) + 
  geom_line(aes(x=as.Date(time), y=Pr_s), size=.7, color=4, linetype = 3) + 
  geom_line(aes(x=as.Date(time), y=TR),color = 3, size=.7, linetype = 3) + 
  geom_hline(aes(yintercept=500),color = 2, size=.7, linetype = 2) + 
  scale_x_date(date_minor_breaks = "1 day",
               limits = c(as.Date("01.07.2020", "%d.%m.%Y"),NA), name = NULL)+
  scale_y_continuous( name = "Траспирация, осадки, л",
                      sec.axis = sec_axis(~./50, name = "Усредненная по горизонтам объемная влажность почвы, %"))+
  theme_bw()+
  theme(axis.text.x = element_text(size=11, angle=90, hjust=1), axis.text.y = element_text(size=12), axis.title.y = element_text(size=12))





ttc = AllData %>% filter(id %in% c("218A0069"), year ==2020)
ttc %>% ggplot()+geom_point(aes(x=time, y = Flux))+geom_point(aes(x=time, y = wood_moist))
mod = lm(data = ttc, Flux~VPD+wood_moist)
Fluxr= predict(mod)
ttc = ttc %>% mutate(Flux3 = Fluxr)
ttc %>% ggplot()+geom_point(aes(x=time, y = Flux2/10, color="red"))+geom_point(aes(x=time, y = wood_moist))+ylim(0,1)
ggplotly()

AllData %>% filter(id %in% c("218A0224")) %>%
  filter(year ==2020) %>% ggplot(aes(x=time, y=predict(mod), color=id ))+geom_point()+ylim(0,0.65)



AllData$













############################   GROWTH d#################################################

AddGrowth = function(data){
  data = data %>% group_by(month, id) %>%
    mutate(mmean = mean(dist3, na.rm=T),
           mmed=quantile(dist3,0.5,na.rm=T), 
           mvar = sd(dist3,na.rm = T),
           mq3 = quantile(dist3,0.9,na.rm=T),
           mq1 = quantile(dist3,0.1,na.rm=T)) %>% 
    ungroup() %>% group_by(week,id) %>%
    mutate(wmean = mean(dist3, na.rm=T),wmed=quantile(dist3,0.5,na.rm=T), wvar = sd(dist3,na.rm = T)) %>% 
    ungroup() %>% group_by(doy,id) %>%
    mutate(dmean = mean(dist3, na.rm=T),dmed=quantile(dist3,0.5,na.rm=T), dvar = sd(dist3,na.rm = T))
  
  m = data %>% group_by(year, id) %>%  filter(hour<7 | hour >18 & dmed < wmed) %>% filter(Species !="TTR")%>%
    do(model = lm(data =., slide_dbl(dist3,~ quantile(.x,0.5,na.rm = T), .before=8, .after=8, .step=1) ~ datetime))
  
  g = data %>% 
    group_by(id) %>% 
    nest %>% 
    inner_join(m) %>% 
    mutate(preds = map2(model, data, predict)) %>% 
    unnest(data,preds) %>% 
    select(id,datetime, preds)
  
  data = left_join(data,g, by=c("id","datetime"))
  data = data %>%group_by(year, id) %>% mutate(growth = max(preds, na.rm=T)-preds)
  return(data)
  
}

x = AddGrowth(data)





(gbs1$dist3 - quantile(gbs1$dist3,0.5,na.rm=T))^2

qplot(gbs1$datetime,gbs1$growth,  color=gbs1$doy%>%as.factor())+geom_line(aes(x=gbs1$datetime,y=gbs1$preds))
qplot(gbs1$datetime,slide_dbl(gbs1$dist3,~ quantile(.x,0.5,na.rm = T), .before=8, .after=8, .step=1) ,  color=gbs1$doy)+
  geom_smooth(aes(x=gbs1$datetime,y=slide_dbl(gbs1$dist3,~ quantile(.x,0.5,na.rm = T), .before=8, .after=8, .step=1)),method="lm")+
  geom_line(aes(x=gbs1$datetime,y=gbs1$dmed))+
  geom_line(aes(x=gbs1$datetime,y=gbs1$wmed))

# VTA = read_delim("VTA.csv", ";", escape_double = FALSE,trim_ws = TRUE)
# VTA$id = paste("218A",VTA$id, sep="")
# VTA = VTA %>%mutate(score = rowMeans(.[2:23], na.rm = T))
# VTAscore =VTA %>% as.data.frame %>% dplyr::select(id, score)
# 
# AllData = left_join(AllData,VTAscore, by="id")

#### Decipher age group

AllData$age_group_index = "I"
AllData$age_group_index[AllData$age_group == 2] = "II"
AllData$age_group_index[AllData$age_group == 3] = "III"
AllData$age_group_index[AllData$age_group == 4] = "IV"
AllData$age_group_index[AllData$age_group == 5] = "V"
AllData$age_group_index[AllData$age_group == 6] = "VI"

AllData$load_score = 1
AllData$load_score[AllData$load == "Low"] = 1
AllData$load_score[AllData$load == "Medium"] = 2
AllData$load_score[AllData$load == "High"] = 3




###### CREATINF DESCRIPTION TABLE##############

##### GOODWAN GOODWAN GOODWAN GOODWAN GOODWAN GOODWAN GOODWAN


goodwan_angles <- read_delim("data/goodwan/goodwan_angles.csv",
                             col_types=cols(
                               id = col_double(),
                               device = col_double(),
                               profile = col_double(),
                               timestamp_utc = col_datetime(format = ""),
                               Angle1 = col_double(),
                               Angle2 = col_double(),
                               Amplitude = col_double(),
                               Direction = col_double(),
                               X9 = col_logical()
                             ),  delim=";", escape_double = FALSE, trim_ws = TRUE)

referencetable=data.frame(
  TTid = c("218A0137","218A0137" ,"218A0102","218A0102", "218A0124","218A0124","218A0100","218A0100","218A0060","218A0060",
           "218A0090","218A0090","218A0064","218A0064", "218A0151","218A0151", "218A0084","218A0084","218A0141","218A0141"),
  device = c(2658, 2642,         2655,2647,                2654, 2648,          2653, 2644,            2652, 2649,
            586,2643,           2656,2646,                2659,2651,          2657, 2650,              587,2645 ),
  gw_type =c("incl", "e_incl","incl", "e_incl","incl", "e_incl","incl", "e_incl","incl", "e_incl","incl", "e_incl","incl", "e_incl",
             "incl", "e_incl", "incl", "e_incl", "incl", "e_incl") )
            
 
goodwan_angles = goodwan_angles %>% mutate(timestamp_utc = timestamp_utc + 3600*3) %>%
  mutate(year = year(timestamp_utc), doy = yday(timestamp_utc), hour = hour(timestamp_utc))

goodwan_angles   = goodwan_angles %>% left_join(referencetable, by="device")
goodwan_angles   = goodwan_angles %>% rename(id_gw = id) %>% rename(id = TTid)
goodwan_angles_m = goodwan_angles %>% group_by(year, doy, hour, id) %>%mutate(
  Angle1 = abs(Angle1),
  Angle2 = abs(Angle2),
  Amplitude = abs(Amplitude)
) %>% summarise(          Angle1 = max(Angle1, na.rm=T),
                          Angle2 = max(Angle2, na.rm=T),
                          time = mean(timestamp_utc, na.rm = T),
                          Amplitude = max(abs(Amplitude), na.rm=T))

####Looks like JSON files have other data


library(jsonlite)

gw_a2 = fromJSON("data/goodwan/goodwan_angles.json")
gw_a2 = cbind(gw_a2[,1:3],as.data.frame(gw_a2$data))

goodwan_angles = gw_a2 %>% mutate(timestamp_utc = as_datetime(timestamp_utc) + 3600*3) %>%
  mutate(year = year(timestamp_utc), doy = yday(timestamp_utc), hour = hour(timestamp_utc))
goodwan_angles   = goodwan_angles %>% rename(device= device_id)
goodwan_angles   = goodwan_angles %>% left_join(referencetable, by="device")
goodwan_angles   = goodwan_angles %>% rename(id = TTid)
goodwan_angles_m= goodwan_angles %>% group_by(year, doy, hour, id) %>%mutate(
  Angle1 = abs(Angle1),
  Angle2 = abs(Angle2),
  Amplitude = abs(Amplitude)
) %>% summarise(          Angle1 = max(Angle1, na.rm=T),
                          Angle2 = max(Angle2, na.rm=T),
                          time = mean(timestamp_utc, na.rm = T),
                          Amplitude = max(abs(Amplitude), na.rm=T))
goodwan_angles_m$Angle2[is.infinite(goodwan_angles_m$Angle2)]=NA
goodwan_angles_m$Amplitude[is.infinite(goodwan_angles_m$Amplitude)]=NA






goodwan_angles_m$Amplitude %>% unique()



RUDN = AllData %>% filter(Site == "RUDN", year==2020, doy > 210) %>% ungroup() %>%
  group_by(doy,id) %>% mutate(psi_d = mean(psi, na.rm = T),
                              phi_d = mean(phi, na.rm = T), 
                              theta_d = mean(theta, na.rm = T)) %>%
  mutate(dtheta  = (theta- theta_d), dpsi  = (psi- psi_d),dphi  = (phi- phi_d) ) %>%ungroup()%>%group_by(id) %>%
  mutate(mxt=max(dtheta, na.rm=T), mnt=min(dtheta, na.rm=T)) 
#%>% mutate (yhour = (doy-1)*24+hour)
#goodwan_angles_m = goodwan_angles_m %>% mutate (yhour = (doy-1)*24+hour) %>% ungroup() #%>% select(id,Angle1,Angle2,Amplitude,yhour)

goodwan_all = left_join(goodwan_angles_m ,RUDN, by = c("id","year","doy","hour"))


ggplot(goodwan_all %>% filter(doy>230 & doy <265,
                              id %in% c("218A0100","218A0102","218A0124","218A0137"), !is.na(phi)))+
geom_line(aes(x=time.x,y=abs(dtheta)*10),size=.1, color=3)+
geom_line(aes(x=time.x, y=Amplitude/2), color="red")+
geom_line(aes(x=time.x,y=WS),size=.1, color=4)+
geom_hline(aes(yintercept = 11), linetype="dashed",color= "red")+
#geom_line(aes(x=time.x,y=WBS),size=.1, color=5)+
ylab("Углы отклонения от вертикальной оси")+
  scale_y_continuous( name = "Скорость ветра, м/с",
                      sec.axis = sec_axis(~., name = "Углы отклонения, градусы"),
                      limits = c(0,14))+
xlab("")+
facet_wrap(~id)+theme_bw()+
  theme(axis.text.x = element_text(size=10, angle=90, hjust=1), axis.text.y = element_text(size=12), axis.title.y = element_text(size=12))

ggplot(goodwan_all %>% filter(doy>215 & doy <255, id !="218A0084",id !="218A0100"))+
geom_col(aes(x=time.x,y=WBS),size=.1, color=1, linetype="dotted", alpha=1/200)+  
geom_point(aes(x=time.x,y=abs(dtheta)*50),size=.1, color=1)+
  geom_point(aes(x=time.x,y=abs(dpsi)*50),size=.3, color=2)+
  geom_point(aes(x=time.x,y=abs(dphi)*50),size=.1, color=3)+
  xlab("")+
  scale_y_continuous( name = "Скорость ветра, м/с",
                      sec.axis = sec_axis(~./50, name = "Углы отклонения, градусы"),
                      limits = c(0,12))+
  facet_wrap(~id)+theme_bw()



ggplot(goodwan_all %>% filter(doy>225 & doy <235, id == "218A0124"))+
  geom_col(aes(x=time.x,y=WBS),size=.1, color=1, linetype="dotted", alpha=1/200)+  
  geom_point(aes(x=time.x,y=dtheta*50),size=.1, color=1)+
  geom_point(aes(x=time.x,y=dpsi*50),size=.3, color=2)+
  geom_point(aes(x=time.x,y=dphi*50),size=.1, color=3)+
  xlab("")+
  scale_y_continuous( name = "Скорость ветра, м/с",
                      sec.axis = sec_axis(~./50, name = "Углы отклонения, градусы"),
                      limits = c(0,10))+
  facet_wrap(~id)+theme_bw()

ggplot(goodwan_all)+geom_point(aes(x=WBS,y=psi),size=.1, color=2)+geom_point(aes(x=WBS,y=Amplitude),size=.1, color=3)+
 xlab("Максимальная скорость порывов ветра за час, м/с")+ylab("Амплитуда углов отклонения")+ylim(-2,2)+theme_bw()

goodwan_all$Angle2 %>% unique()
goodwan124 = goodwan_all %>% filter(id == "218A0124")

ggplot(goodwan124)+geom_point(aes(x=time.x,y=dpsi),size=.1, color=2)+geom_point(aes(x=time.x,y=Angle2),size=.1, color=3)+
  geom_point(aes(x=time.x,y=gz2*100000),size=.1, color=4)+geom_col(aes(x=time.x,y=WS),size=.1,alpha=1/10000, color=1)+ 
  facet_wrap(~id)



goodwan100$gz2 %>% summary()



goodwan_frequencies <- read_delim("data/goodwan/goodwan_frequencies.csv", 
                                  ";", escape_double = FALSE, trim_ws = TRUE)

gw586 = goodwan_frequencies %>% filter(device==586) 

gw586$Angle %>% summary

ggplot(gw586,aes(x=timestamp_utc,y=Angle))+geom_point()

goodwan_frq = goodwan_angles %>% mutate(timestamp_utc = timestamp_utc + 3600*3) %>%
  mutate(year = year(timestamp_utc), doy = yday(timestamp_utc), hour = hour(timestamp_utc))

goodwan_angles   = goodwan_angles %>% left_join(referencetable, by="device")











gw_2645 = gw_a2 %>% filter(device_id == 2645)
gw_2645

gw_f2 = fromJSON("data/goodwan/goodwan_frequencies.json")
gw_f2$components
gw_2659 = gw_f2 %>% filter(device_id == 2659)

gw_2645 = gw_a2 %>% filter(device_id == 2645)

ggplot(gw_2645)+geom_point(aes(x=as_datetime(timestamp_utc),y=data$Amplitude))
################################################################







AllDataSummary= AllData %>% group_by(SiteIndex, Species,age_group_index,load,insite_load,id)%>%
  summarise(d = mean(diam),DBH = mean(DBH),DTT = mean(DTT), height = mean(height), VTA = mean(VTAscore))


write.csv(AllDataSummary, file="results//DB_description.csv")

#AllData2 = AllData2%>%group_by(id)%>%mutate(growth =first(dist_pred)-dist_pred)

# ggplot(AllData2)+
#   geom_point(aes(x=time,y=growth), size=.1)+facet_wrap(~id)+ylim(0,10)+theme_bw()
##############################################################################

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
      filename = paste("results//pictures//2020","Load_anhtrop",plotbunch[i,1],names(plotbunch)[2],".png",sep="_")
      graph = plotbunch[i,2][[1]][[1]]
      ggsave(filename, graph,device = "png",width=8,height=6,units="in")
    } 
}

writeFluxTSALg(AllData)


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
    filename = paste("results//pictures//2020", "Load_antr_in_site_",plotbunch[i,1],plotbunch[i,2],".png",sep="_")
    graph = plotbunch[i,3][[1]][[1]]
    ggsave(filename, graph,device = "png",width=8,height=6,units="in")
  } 
}

writeFluxTSg = function(data, sitename){
  plotbunch = data%>%group_by(Species, doy, id) %>% mutate(update = length(id))%>%
    summarise(sFlux = sum(Fluxal, na.rm=T), readings = mean(update), diam = mean(diam, na.rm=T), time=mean(time,na.rm=T)) %>%
    mutate(suFlux = sFlux/n()*24) %>%
    group_by(Species, doy) %>% summarise(Flux = mean(suFlux, na.rm=T), d = mean(diam), time=as.Date(mean(time,na.rm=T))) %>% group_by(Species)%>%
    do(Flux = ggplot(data = .)
       +geom_smooth(aes(x=time,y=Flux))
       +geom_point(aes(x=time,y=Flux),size=.1)
       +scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 week",
                     limits = c(as.Date("01.06.2020", "%d.%m.%Y"),as.Date("01.10.2020", "%d.%m.%Y")), name = NULL)
       +scale_y_continuous( limits = c(0,75), name = expression(Скорость~" "~сокотечения~" "~л~д^-1))
       +theme_bw()

       )
  
  
  #sitename = "RUDN"
  for(i in 1:nrow(plotbunch)){
    filename = paste("results//pictures//2020",sitename,plotbunch[i,1],names(plotbunch)[2],".png",sep="_")
    graph = plotbunch[i,2][[1]][[1]]
    ggsave(filename, graph,device = "png",width=5.83,height=4.13,units="in")
  } 
}

writeFluxTSg(AllData,"RUDN")


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
    filename = paste("results//pictures//2020",sitename,plotbunch[i,1],names(plotbunch)[2],".png",sep="_")
    graph = plotbunch[i,2][[1]][[1]]
    ggsave( filename, graph,device = "png",width=5.83,height=4.13,units="in")
  } 
}


#plotbunch[15,2][[1]]

writeFluxDiurnalg = function(data, sitename){
  months_n = factor(c("Январь","Февраль","Март","Апрель","Май","Июнь", "Июль","Август","Сентябрь", "Октябрь","Ноябрь","Декабрь"), 
                    levels = c("Январь","Февраль","Март","Апрель","Май","Июнь", "Июль","Август","Сентябрь", "Октябрь","Ноябрь","Декабрь"))
  data = data%>%mutate(month = month(time))
  plotbunch = data%>%group_by(Species, hour, month,id,year) %>% filter(month<11 & month>4, Site==sitename)%>%
    summarise(Flux = mean(Flux, na.rm=T)) %>% group_by(Species) %>% 
    mutate(months = months_n[month]) %>%
    do(DiurnalFlux = ggplot(data = .)
       +geom_smooth(aes(x=hour,y=Flux, color=as.factor(year)))
       +geom_point(aes(x=hour,y=Flux,color=as.factor(year)),size=.1)
       +scale_x_continuous( breaks =c(0,3,6,9,12,15,18,21,24) ,name = expression(Часы~суток))
       +scale_y_continuous( limits = c(0,10), name = expression(Скорость~" "~сокотечения~" "~л~ч^-1))
       +facet_wrap(~months,nrow=2,ncol=3,strip.position = "bottom" )
       +theme_bw()
       +theme(legend.title = element_blank()))
  
  
  #sitename = "RUDN"
  for(i in 1:nrow(plotbunch)){
    filename = paste("results//pictures//diurnal//2020",names(plotbunch)[2],plotbunch[i,1],sitename,".png",sep="_")
    graph = plotbunch[i,2][[1]][[1]]
    ggsave( filename, graph,device = "png",width=5.83,height=4.13,units="in")
  } 
}

writeFluxDiurnalg(AllData, "RUDN")
writeFluxDiurnalg(AllData, "GARDEN")

writeFluxDiurnalALg = function(AllData){
  
  
  months_n = factor(c("Январь","Февраль","Март","Апрель","Май","Июнь", "Июль","Август","Сентябрь", "Октябрь","Ноябрь","Декабрь"), 
                    levels = c("Январь","Февраль","Март","Апрель","Май","Июнь", "Июль","Август","Сентябрь", "Октябрь","Ноябрь","Декабрь"))
  
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
    filename = paste("results//pictures//2020","Load_anhtrop",plotbunch[i,1],names(plotbunch)[2],".png",sep="_")
    graph = plotbunch[i,2][[1]][[1]]
    ggsave(filename, graph,device = "png",width=8,height=6,units="in")
  } 
}


writeFluxDiurnalALinSg = function(AllData){
  
  
  months_n = factor(c("Январь","Февраль","Март","Апрель","Май","Июнь", "Июль","Август","Сентябрь", "Октябрь","Ноябрь","Декабрь"), 
                    levels = c("Январь","Февраль","Март","Апрель","Май","Июнь", "Июль","Август","Сентябрь", "Октябрь","Ноябрь","Декабрь"))
  
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
    filename = paste("results//pictures//2020","Load_anhtrop",plotbunch[i,1],plotbunch[i,2],".png",sep="_")
    graph = plotbunch[i,3][[1]][[1]]
    ggsave(filename, graph,device = "png",width=8,height=6,units="in")
  } 
}

writeFluxDiurnalTTg = function(data, sitename){
  months_n = factor(c("Январь","Февраль","Март","Апрель","Май","Июнь", "Июль","Август","Сентябрь", "Октябрь","Ноябрь","Декабрь"), 
                    levels = c("Январь","Февраль","Март","Апрель","Май","Июнь", "Июль","Август","Сентябрь", "Октябрь","Ноябрь","Декабрь"))
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
    filename = paste("results//pictures//",names(plotbunch)[2],plotbunch[i,1],sitename,".png",sep="_")
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
    filename = paste("results//pictures//2020",names(plotbunch)[2],plotbunch[i,1],sitename,".png",sep="_")
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
                     limits = c(as.Date("01.06.2020", "%d.%m.%Y"),as.Date("01.10.2020", "%d.%m.%Y")), name = NULL)
       +scale_y_continuous( limits = c(0,1), name = expression(Вегетационный~" "~индекс~" "~NDVI))
       +theme_bw())
  
  
  #sitename = "RUDN"
  for(i in 1:nrow(plotbunch)){
    filename = paste("results//pictures//2020",names(plotbunch)[2],plotbunch[i,1],sitename,".png",sep="_")
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
      filename = paste("results//pictures//2020","Load_anhtrop",plotbunch[i,1],names(plotbunch)[2],".png",sep="_")
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
    filename = paste("results//pictures//2020","Load_anhtrop_in_site_NDVI",plotbunch[i,1],plotbunch[i,2],".png",sep="_")
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
    filename = paste("results//pictures//","Load_anhtrop",plotbunch[i,1],names(plotbunch)[2],".png",sep="_")
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
    filename = paste("results//pictures//","Load_anhtrop_insite_PRI_",plotbunch[i,1],plotbunch[i,2],".png",sep="_")
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
                     limits = c(as.Date("01.05.2020", "%d.%m.%Y"),NA), name = NULL)
       +scale_y_continuous(  name = expression(Угол~" "~уклона~" "~дерева))
       +theme_bw())
  
  
  #sitename = "RUDN"
  for(i in 1:nrow(plotbunch)){
    filename = paste("results//pictures//2020",names(plotbunch)[2],plotbunch[i,1],sitename,".png",sep="_")
    graph = plotbunch[i,2][[1]][[1]]
    ggsave(filename, graph,device = "png",width=5.83,height=4.13,units="in")
  } 
}

writeAngleTSg(AllData,"RUDN")

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
    filename = paste("results//pictures//2020",names(plotbunch)[2],plotbunch[i,1],sitename,".png",sep="_")
    graph = plotbunch[i,2][[1]][[1]]
    ggsave(filename, graph,device = "png",width=5.83,height=4.13,units="in")
  } 
}



writeTairRhTSg = function(data, sitename){
  
  plotbunch = data%>%group_by(Species) %>%filter(!is.na(tair))%>%
    do(clim = ggplot(data = .)
       +geom_line(aes(x=as.Date(time),y=tair),size=.3, alpha=1/3)
       +geom_line(aes(x=as.Date(time),y=VPD*20),size=.3, color=3, alpha=1/3)
       +scale_y_continuous(name = expression(Температура~воздуха~" "~C),
                          sec.axis = sec_axis(~./50, name = "Отрицательное давление паров влаги, кПa", labels = function(b) { round(b * 1, 2)}),
                          limits = c(0,40))
       +scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 week",
                     limits = c(as.Date("01.06.2020", "%d.%m.%Y"),NA), name = NULL)
      +theme_bw()
    )
  
  #sitename = "RUDN"
  for(i in 1:nrow(plotbunch)){
    filename = paste("results//pictures//2020",names(plotbunch)[2],plotbunch[i,1],sitename,".png",sep="_")
    graph = plotbunch[i,2][[1]][[1]]
    ggsave(filename, graph,device = "png",width=5.83,height=4.13,units="in")
  } 
}


writeTairRhTSAlg = function(data){
  
  plotbunch = data%>%group_by(Species) %>%filter(!is.na(tair))%>%
    do(clim = ggplot(data = .)
       +geom_point(aes(x=as.Date(time),y=tair, group = load, color = load),size=.3, alpha=1/3)
       +geom_smooth(aes(x=as.Date(time),y=tair, group = load, color=load, fill = load),method = "loess",span = .4)
       +geom_line(aes(x=as.Date(time),y=VPD*20, group = load),size=.3, color=5, alpha=1/5)
       +scale_y_continuous(name = expression(Температура~воздуха~" "~C),
                           sec.axis = sec_axis(~./50, name = "Отрицательное давление паров влаги, кПa", labels = function(b) { round(b * 1, 2)}),
                           limits = c(0,40))
       +scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 week",
                     limits = c(as.Date("01.06.2019", "%d.%m.%Y"),as.Date("01.10.2019", "%d.%m.%Y")), name = NULL)
       +scale_color_lancet(guide=FALSE)
       +scale_fill_lancet(name = "Уровень антропогенной нагрузки внутри участка")
       +theme_bw()
       +theme(legend.position="bottom")
    )
  
  #sitename = "RUDN"
  for(i in 1:nrow(plotbunch)){
    filename = paste("results//pictures//2020","Ant_load",names(plotbunch)[2],plotbunch[i,1],".png",sep="_")
    graph = plotbunch[i,2][[1]][[1]]
    ggsave(filename, graph,device = "png",width=5.83,height=4.13,units="in")
  } 
}


#data=RUDNdata[[2]]
#plotbunch[1,2][[1]]


writeAngleDeviation = function(data, sitename){
  data = data %>% filter(Site == sitename) %>% group_by(doy,id) %>% mutate(psi_d = mean(psi, na.rm = T),
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
  geom_line(aes(x=as.Date(time),y=WS ), color=3, size=0.02)+
  geom_hline(aes(yintercept=mxt), color=2,linetype="dashed", size=.02)+
  geom_hline(aes(yintercept=mnt), color=2,linetype="dashed", size=.02)+
  scale_y_continuous(name = expression(Отклонение~от~среднесуточного~значения~угла~theta~','~градусы),
                     limits=c(-15,15), sec.axis = sec_axis(~., name = "Максимальная скорость ветра за последние 3 часа, м/с", 
                        labels = function(b) { round(b * 1, 2)}))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 week",
                  limits = c(as.Date("01.07.2020", "%d.%m.%Y"),NA), name = NULL)+
  theme_bw()
    )
  
#geom_smooth(aes(x=time,y=theta_d-theta ), color=3, size=0.02, se=T, span=.1,method = "loess")+
#scale_y_continuous(limits=c(-10,10))+

  for(i in 1:nrow(plotbunch)){
    filename = paste("results//pictures//angles//",names(plotbunch)[2],plotbunch[i,1],sitename,".png",sep="_")
    graph = plotbunch[i,2][[1]][[1]]
    ggsave(filename, graph,device = "png",width=5.83,height=4.13,units="in")
  } 
}
writeAngleDeviation(AllData,"BOLOTNAYA")


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
    filename = paste("results//pictures//2020",names(plotbunch)[2],plotbunch[i,1],sitename,".png",sep="_")
    graph = plotbunch[i,2][[1]][[1]]
    ggsave(filename, graph,device = "png",width=5.83,height=4.13,units="in")
  } 
}



#data=RUDNdata[[2]]
#plotbunch[21,2][[1]]

writeZ2TT = function(data, sitename){
  data = data %>% filter(year==2020) %>% group_by(doy,id,Species) %>%
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
                      limits = c(as.Date("01.05.2020", "%d.%m.%Y"),NA), name = NULL)+
         theme_bw()
    )
  #geom_smooth(aes(x=time,y=theta_d-theta ), color=3, size=0.02, se=T, span=.1,method = "loess")+
  #scale_y_continuous(limits=c(-10,10))+
  
  for(i in 1:nrow(plotbunch)){
    filename = paste("results//pictures//stab//",names(plotbunch)[3],names(plotbunch)[2],plotbunch[i,1],sitename,".png",sep="_")
    graph = plotbunch[i,2][[1]][[1]]
    ggsave( filename, graph,device = "png",width=5.83,height=4.13,units="in")
  } 
}

writeZ2TT(AllData,"RUDN")


writeZ2TTh = function(data, sitename){
  data = data %>% mutate(g2 = gz2+gy2+gx2)
  plotbunch = data%>%group_by(id,Species) %>% filter(year=2020)%>%
    do(g2vsWSpeed = ggplot(data = .)+
         geom_line(aes(x=as.Date(time),y=g2 ), color=1, size=0.02)+
         geom_line(aes(x=as.Date(time),y=WS/1000000), color=2,linetype="dashed", size=.02)+
         scale_y_continuous(name = expression(Cуммы~стандартных~отклонений~цифрового~сигнала~акслерометра~по~трем~осям),
                            sec.axis = sec_axis(~.*1000000, name = "Максимальная скорость ветра за последние 3 часа, м с^-1", 
                                                labels = function(b) { round(b * 1, 2)}))+
         scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 week",
                      limits = c(as.Date("01.05.2020", "%d.%m.%Y"),NA), name = NULL)+
         theme_bw()
    )
  #geom_smooth(aes(x=time,y=theta_d-theta ), color=3, size=0.02, se=T, span=.1,method = "loess")+
  #scale_y_continuous(limits=c(-10,10))+
  
  for(i in 1:nrow(plotbunch)){
    filename = paste("results//pictures//stab//",names(plotbunch)[2],plotbunch[i,1],sitename,".png",sep="_")
    graph = plotbunch[i,2][[1]][[1]]
    try(ggsave(filename, graph,device = "png",width=5.83,height=4.13,units="in"),T)
  } 
}
writeZ2TTh(AllData, "RUDN")

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
    filename = paste("results//pictures//2020",names(plotbunch)[2],plotbunch[i,1],sitename,".png",sep="_")
    graph = plotbunch[i,2][[1]][[1]]
    ggsave(filename, graph,device = "png",width=5.83,height=4.13,units="in")
  } 
}

writeZ2ALg = function(data){
  
  #data = left_join(data,meteo_data, by=c("year","doy","hour"))
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
                      limits = c(as.Date("01.06.2020", "%d.%m.%Y"),as.Date("01.10.2020", "%d.%m.%Y")), name = NULL)+
        scale_color_lancet(name = "Уровень антропогенной нагрузки")+
        theme_bw()+
        theme(legend.position="bottom")
    )
  
  for(i in 1:nrow(plotbunch)){
    filename = paste("results//pictures//2020", "Load_anhtrop",plotbunch[i,1],names(plotbunch)[2],".png",sep="_")
    graph = plotbunch[i,2][[1]][[1]]
    ggsave(filename, graph,device = "png",width=8,height=6,units="in")
  } 
  
}


writeZ2ALg(AllData)



writeZ2ALinSg = function(data,meteo_data){
  
  #data = left_join(data,meteo_data, by=c("year","doy","hour"))
  data = data %>% group_by(doy,id,Site,Species,load) %>%
    mutate(z2 = max(gz2+gy2+gx2, na.rm = T),
           WS = max(WS, na.rm = T))%>%
    group_by(doy,Species,Site,load)%>%
    mutate(
      z2s = mean(z2, na.rm = T), 
      WSs = mean(WS, na.rm = T)
    )
  
  plotbunch = data%>%group_by(Site,Species)%>%
    do(trembling = ggplot(data = .)+
         geom_line(aes(x=as.Date(time),y=z2s, color=load ), size=.5)+
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
    filename = paste("results//pictures//vstab//", "Load_anhtrop_insite_tremble_",plotbunch[i,1],plotbunch[i,2],".png",sep="_")
    graph = plotbunch[i,3][[1]][[1]]
    ggsave(filename, graph,device = "png",width=8,height=6,units="in")
  } 
  
}
writeZ2ALinSg(AllData)

writeGrowthTTm = function(Alldata){
  months_n = factor(c("Январь","Февраль","Март","Апрель","Май","Июнь", "Июль","Август","Сентябрь", "Октябрь","Ноябрь","Декабрь"), 
                    levels = c("Январь","Февраль","Март","Апрель","Май","Июнь", "Июль","Август","Сентябрь", "Октябрь","Ноябрь","Декабрь"))
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
      filename = paste("results//pictures//growth//2020",names(plotbunch)[2],plotbunch[i,1],sitename,".png",sep="_")
      graph = plotbunch[i,2][[1]][[1]]
      ggsave(filename, graph,device = "png",width=5.83,height=4.13,units="in")
    } 
  }

}

writeGrowthTTm(AllData)

writeGrowthSpm = function(Alldata){
  months_n = factor(c("Январь","Февраль","Март","Апрель","Май","Июнь", "Июль","Август","Сентябрь", "Октябрь","Ноябрь","Декабрь"), 
                    levels = c("Январь","Февраль","Март","Апрель","Май","Июнь", "Июль","Август","Сентябрь", "Октябрь","Ноябрь","Декабрь"))
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
      filename = paste("results//pictures//growth//2020",names(plotbunch)[2],plotbunch[i,1],sitename,".png",sep="_")
      graph = plotbunch[i,2][[1]][[1]]
      ggsave(filename, graph,device = "png",width=5.83,height=4.13,units="in")
    } 
  }
  
}
writeGrowthTTm(AllData)


writGrowthALSpm = function(Alldata){
  
     months_n = factor(c("Январь","Февраль","Март","Апрель","Май","Июнь", "Июль","Август","Сентябрь", "Октябрь","Ноябрь","Декабрь"), 
                    levels = c("Январь","Февраль","Март","Апрель","Май","Июнь", "Июль","Август","Сентябрь", "Октябрь","Ноябрь","Декабрь"))
    data  = Alldata 
    dat = data %>% group_by(id, Species,load,month) %>% filter(year==2020) %>%
      summarise(growth = max(growth, na.rm=T)) %>% group_by(load,Species,month) %>%
      summarise(growth = mean(growth, na.rm=T)) %>% mutate(growth = case_when(growth<0 ~ 0, TRUE ~ growth))
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
    filename = paste("results//pictures//growth//","Load_anhtrop",plotbunch[i,1],names(plotbunch)[2],".png",sep="_")
    graph = plotbunch[i,2][[1]][[1]]
    ggsave(filename, graph,device = "png",width=8,height=6,units="in")
  } 
}  
writGrowthALSpm(AllData)

writGrowthALSinSpm = function(Alldata){
  
  months_n = factor(c("Январь","Февраль","Март","Апрель","Май","Июнь", "Июль","Август","Сентябрь", "Октябрь","Ноябрь","Декабрь"), 
                    levels = c("Январь","Февраль","Март","Апрель","Май","Июнь", "Июль","Август","Сентябрь", "Октябрь","Ноябрь","Декабрь"))
  data  = Alldata 
  dat = data %>% group_by(id, Species,load_score,Site,month) %>% filter(year == 2020) %>%
    summarise(growth = max(growth, na.rm=T)) %>% group_by(load_score,Site,Species,month) %>%
    summarise(growth = mean(growth, na.rm=T))
  plotbunch = dat%>%group_by(Site,Species) %>%
    do(Growth = ggplot(data = .)+
         geom_col(aes(x=months_n[month],y=growth, fill=load_score, group = load ), size=0.02, position = "dodge")+
         scale_y_continuous(name = expression(Прирост~даметра~ствола~", "~мм))+
         scale_x_discrete(limits = c("Июнь","Июль","Август","Сентябрь"), name = NULL)+
         scale_color_lancet(guide=FALSE)+
         scale_fill_lancet(name = "Уровень антропогенной нагрузки")+
         theme_bw()+
         theme(legend.position="bottom"))
  
  
  for(i in 1:nrow(plotbunch)){
    filename = paste("results//pictures//growth//","Load_anhtrop_insite_growth_",plotbunch[i,1],plotbunch[i,2],".png",sep="_")
    graph = plotbunch[i,3][[1]][[1]]
    ggsave(filename, graph,device = "png",width=8,height=6,units="in")
  } 
} 
writGrowthALSinSpm(AllData)

           # months_n = factor(c("Январь","Февраль","Март","Апрель","Май","Июнь", "Июль","Август","Сентябрь", "Октябрь","Ноябрь","Декабрь"), 
           #                   levels = c("Январь","Февраль","Март","Апрель","Май","Июнь", "Июль","Август","Сентябрь", "Октябрь","Ноябрь","Декабрь"))
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
writeTairRhTSAlg(AllData%>%filter(Site == "TROITSK"))

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


writeFluxTSg(GBSMdata[[2]] %>% 
                    filter(id %in% c("218A0098","21880213","21880215","218A0229","218A0162","218A0133"))  ,"GBS_MOIST_1")
writeFluxTSg(GBSMdata[[2]] %>% 
                    filter(id %in% c("218A0080","218A0135","218A0242","218A0299","218A0260","218A0184")) ,"GBS_MOIST_2")




writeFluxDiurnalg(GBSMdata[[2]] %>% 
                    filter(id %in% c("218A0098","21880213","21880215","218A0229","218A0162","218A0133"))  ,"GBS_MOIST_1")
writeFluxDiurnalg(GBSMdata[[2]] %>% 
                    filter(id %in% c("218A0080","218A0135","218A0242","218A0299","218A0260","218A0184")) ,"GBS_MOIST_2")



writeNdviTSTTg(GBSMdata[[2]] %>%filter(id %in% c("218A0098","21880213","21880215","218A0229","218A0162","218A0133"))  ,"GBS_MOIST_1")
writeNdviTSTTg(GBSMdata[[2]] %>% filter(id %in% c("218A0080","218A0135","218A0242","218A0299","218A0260","218A0184")) ,"GBS_MOIST_2")



writeTairRhTSg(GBSMdata[[2]] %>% 
                 filter(id %in% c("218A0098","21880213","21880215","218A0229","218A0162","218A0133"),
                        year == 2020)  ,"GBS_MOIST_1")
writeTairRhTSg(GBSMdata[[2]] %>% 
                 filter(id %in% c("218A0080","218A0135","218A0242","218A0299","218A0260","218A0184"),
                        year == 2020) ,"GBS_MOIST_2")





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






#  
#  Таблицы среднихз макс мин значений потока, ндви, для каждого вида усредненный
#  
#  !!!!Посчитать возраста по феногруппам!!!!!! Добавить колонку в первую главу
#  
#  NDVI глава 2 добавить таблицу
#  
#  Расписать кто высокий кто низкий
#  
#  Ответить на TT 259, 263, 279
#  
#  Имена участков
#  
#  Показать что NDVI работает для ВТА и валить на то, что 
#  
#  Внутри сайтовое сравнение антроп нагрузке
#  
#  DONE Ольгу пнуть по ее пунктам
#  
#  Добавить в финальные таблицы наши экстремальные значения 




AllData%>%group_by(Site)%>%summarise(age  = mean(age_group))





AllData$NDVIc[AllData$NDVIc>1]=NA
AllData$NDVIc[AllData$NDVIc< -1]=NA
AllData2 =AllData
AllData2 = AllData2 %>% as.data.frame()
for (i in 1:ncol(AllData2)){
  AllData2[ AllData2[,i] %>% is.infinite(),i] = NA  
}
AllData2$month = month(AllData2$time)

SumaryAllmData = AllData %>% group_by(year,doy,id) %>% 
  mutate(psi_d = mean(psi, na.rm = T),
         phi_d = mean(phi, na.rm = T), 
         theta_d = mean(theta, na.rm = T))%>%
  ungroup() %>% group_by(year,doy, id) %>% 
  mutate(
    dtheta  = (theta- theta_d),
    dphi  = (phi- phi_d),
    dpsi  = (psi- psi_d)) %>% 
  ungroup() %>%
  mutate(g2 = gx2+gy2+gz2) %>%
  group_by(id,Site,Species,month,year ) %>% 
  summarise(
    n=n(),
    DBH = mean(DBH, na.rm = T),
    VTA = unique(VTA_score),
    age = unique(age_group_index),
    load = unique(antrop_load),
    dthetamav = mean(dtheta, na.rm = T),
    dphi = mean(dphi, na.rm = T),
    dpsi = mean(dpsi, na.rm = T),
    g2 = mean(g2, na.rm = T),
    tair = mean(tair, na.rm = T),
    rh = mean(rh, na.rm = T),
    NDVI = mean(NDVIc, na.rm = T),
    PRI = mean(PRIc, na.rm = T),
    PSND = mean(PSNDc, na.rm=T),
    CRI2 = mean(CRI2c, na.rm=T),
    wood_moist = mean(wood_moist, na.rm = T),
    growth = mean(growth, na.rm=T),
    Flux = mean(Flux, na.rm = T),
    Flux2 = mean(Flux2, na.rm = T),
    t = mean(nt1, na.rm = T),
    VPD = mean(VPD, na.rm = T),
    dtheta_se = sd(dtheta, na.rm = T)/n()^.5,
    dphi_se = sd(dphi, na.rm = T)/n()^.5,
    dpsi_se = sd(dpsi, na.rm = T)/n()^.5,
    g2_se = sd(g2, na.rm = T)/n()^.5,
    tair_se = sd(tair, na.rm = T)/n()^.5,
    rh_se = sd(rh, na.rm = T)/n()^.5,
    NDVI_se = sd(NDVIc, na.rm = T)/n()^.5,
    PRI_se = sd(PRIc, na.rm = T)/n()^.5,
    PSND_se = sd(PSNDc, na.rm=T)/n()^.5,
    CRI2_se = sd(CRI2c, na.rm=T)/n()^.5,
    wood_moist_se = sd(wood_moist, na.rm = T)/n()^.5,
    growth_se = sd(growth, na.rm=T)/n()^.5,
    Flux_se = sd(Flux, na.rm = T)/n()^.5,
    t_se = sd(nt1, na.rm = T)/n()^.5,
    VPD_se = sd(VPD, na.rm = T)/n()^.5,
    dtheta_q10 = quantile(dtheta,0.1, na.rm = T),
    dphi_q10 = quantile(dphi,0.1, na.rm = T),
    dpsi_q10 = quantile(dpsi,0.1, na.rm = T),
    g2_q10 = quantile(g2,0.1, na.rm = T),
    tair_q10 = quantile(tair,0.1, na.rm = T),
    rh_q10 = quantile(rh,0.1, na.rm = T),
    NDVImq10 = quantile(NDVIc,0.1, na.rm = T),
     PRImsq10 = quantile(PRIc,0.1, na.rm = T),
     PSNDmq10 = quantile(PSNDc,0.1, na.rm=T),
     CRI2mq10 = quantile(CRI2c,0.1, na.rm=T),
    wood_moist_q10 = quantile(wood_moist,0.1, na.rm = T),
    growth_q10 = quantile(growth,0.1, na.rm=T),
     Flux_q10 = quantile(Flux,0.1, na.rm = T),
     VPD_q10 = quantile(VPD,0.1, na.rm = T),
     t_q10 = quantile(nt1,0.1, na.rm = T),
    dtheta_q90 = quantile(dtheta,0.9, na.rm = T),
    dphi_q90 = quantile(dphi,0.9, na.rm = T),
    dpsi_q90 = quantile(dpsi,0.9, na.rm = T),
    tair_q90 = quantile(tair,0.9, na.rm = T),
    rh_q90 = quantile(rh,0.9, na.rm = T),
    g2_q90 = quantile(g2,0.9, na.rm = T),
     NDVI_q90 = quantile(NDVIc,0.9, na.rm = T),
     PRI_q90 = quantile(PRIc,0.9, na.rm = T),
     PSND_q90 = quantile(PSNDc,0.9, na.rm=T),
     CRI2_q90 = quantile(CRI2c,0.9, na.rm=T),
    wood_moist_q90 = quantile(wood_moist,0.9, na.rm = T),
    growth_q90 = quantile(growth,0.9, na.rm=T),
     Flux_q90 = quantile(Flux,0.9, na.rm = T),
     t_q90 = quantile(nt1,0.9, na.rm = T),
     VPD_q90 = quantile(VPD,0.9, na.rm = T),
    dtheta_md = quantile(dtheta,0.5, na.rm = T),
    dphi_md = quantile(dphi,0.5, na.rm = T),
    dpsi_md = quantile(dpsi,0.5, na.rm = T),
    tair_md = quantile(tair,0.5, na.rm = T),
    rh_md = quantile(rh,0.5, na.rm = T),
    g2_md = quantile(g2,0.5, na.rm = T),
     NDVI_md = quantile(NDVIc,0.5, na.rm = T),
     PRI_md = quantile(PRIc,0.5, na.rm = T),
     PSND_md = quantile(PSNDc,0.5, na.rm=T),
     CRI2_md = quantile(CRI2c,0.5, na.rm=T),
     wood_moist_md = quantile(wood_moist,0.5, na.rm = T),
    growth_md = quantile(growth,0.5, na.rm=T),
     Flux_md = quantile(Flux,0.5, na.rm = T),
     VPD_md = quantile(VPD,0.5, na.rm = T),
     t_sum = sum(tair/length(Flux)*30, na.rm = T),
     Flux_sum = sum(Flux/length(Flux)*720, na.rm = T),
     
)


SumaryAllData = AllData2 %>% group_by(year,doy,id) %>% 
  mutate(psi_d = mean(psi, na.rm = T),
         phi_d = mean(phi, na.rm = T), 
         theta_d = mean(theta, na.rm = T))%>%
  ungroup() %>% group_by(year,doy, id) %>% 
  mutate(
    dtheta  = (theta- theta_d),
    dphi  = (phi- phi_d),
    dpsi  = (psi- psi_d)) %>% 
  ungroup() %>%
  mutate(g2 = gx2+gy2+gz2) %>%
  group_by(id,Site,Species,doy,year ) %>% 
  summarise(
    n=n(),
    DBH = mean(DBH, na.rm = T),
    VTA = unique(VTA_score),
    age = unique(age_group_index),
    load = unique(antrop_load),
    dthetamav = mean(dtheta, na.rm = T),
    dphi = mean(dphi, na.rm = T),
    dpsi = mean(dpsi, na.rm = T),
    g2 = mean(g2, na.rm = T),
    tair = mean(tair, na.rm = T),
    rh = mean(rh, na.rm = T),
    NDVI = mean(NDVIc, na.rm = T),
    PRI = mean(PRIc, na.rm = T),
    PSND = mean(PSNDc, na.rm=T),
    CRI2 = mean(CRI2c, na.rm=T),
    wood_moist = mean(wood_moist, na.rm = T),
    growth = mean(growth, na.rm=T),
    Fluxm = mean(Flux, na.rm = T),
    t = mean(nt1, na.rm = T),
    VPD = mean(VPD, na.rm = T),
    dtheta_se = sd(dtheta, na.rm = T)/n()^.5,
    dphi_se = sd(dphi, na.rm = T)/n()^.5,
    dpsi_se = sd(dpsi, na.rm = T)/n()^.5,
    g2_se = sd(g2, na.rm = T)/n()^.5,
    tair_se = sd(tair, na.rm = T)/n()^.5,
    rh_se = sd(rh, na.rm = T)/n()^.5,
    NDVI_se = sd(NDVIc, na.rm = T)/n()^.5,
    PRI_se = sd(PRIc, na.rm = T)/n()^.5,
    PSND_se = sd(PSNDc, na.rm=T)/n()^.5,
    CRI2_se = sd(CRI2c, na.rm=T)/n()^.5,
    wood_moist_se = sd(wood_moist, na.rm = T)/n()^.5,
    growth_se = sd(growth, na.rm=T)/n()^.5,
    Flux_se = sd(Flux, na.rm = T)/n()^.5,
    t_se = sd(nt1, na.rm = T)/n()^.5,
    VPD_se = sd(VPD, na.rm = T)/n()^.5,
    dtheta_q10 = quantile(dtheta,0.1, na.rm = T),
    dphi_q10 = quantile(dphi,0.1, na.rm = T),
    dpsi_q10 = quantile(dpsi,0.1, na.rm = T),
    g2_q10 = quantile(g2,0.1, na.rm = T),
    tair_q10 = quantile(tair,0.1, na.rm = T),
    rh_q10 = quantile(rh,0.1, na.rm = T),
    NDVImq10 = quantile(NDVIc,0.1, na.rm = T),
    PRImsq10 = quantile(PRIc,0.1, na.rm = T),
    PSNDmq10 = quantile(PSNDc,0.1, na.rm=T),
    CRI2mq10 = quantile(CRI2c,0.1, na.rm=T),
    wood_moist_q10 = quantile(wood_moist,0.1, na.rm = T),
    growth_q10 = quantile(growth,0.1, na.rm=T),
    Flux_q10 = quantile(Flux,0.1, na.rm = T),
    VPD_q10 = quantile(VPD,0.1, na.rm = T),
    t_q10 = quantile(nt1,0.1, na.rm = T),
    dtheta_q90 = quantile(dtheta,0.9, na.rm = T),
    dphi_q90 = quantile(dphi,0.9, na.rm = T),
    dpsi_q90 = quantile(dpsi,0.9, na.rm = T),
    tair_q90 = quantile(tair,0.9, na.rm = T),
    rh_q90 = quantile(rh,0.9, na.rm = T),
    g2_q90 = quantile(g2,0.9, na.rm = T),
    NDVI_q90 = quantile(NDVIc,0.9, na.rm = T),
    PRI_q90 = quantile(PRIc,0.9, na.rm = T),
    PSND_q90 = quantile(PSNDc,0.9, na.rm=T),
    CRI2_q90 = quantile(CRI2c,0.9, na.rm=T),
    wood_moist_q90 = quantile(wood_moist,0.9, na.rm = T),
    growth_q90 = quantile(growth,0.9, na.rm=T),
    Flux_q90 = quantile(Flux,0.9, na.rm = T),
    t_q90 = quantile(nt1,0.9, na.rm = T),
    VPD_q90 = quantile(VPD,0.9, na.rm = T),
    dtheta_md = quantile(dtheta,0.5, na.rm = T),
    dphi_md = quantile(dphi,0.5, na.rm = T),
    dpsi_md = quantile(dpsi,0.5, na.rm = T),
    tair_md = quantile(tair,0.5, na.rm = T),
    rh_md = quantile(rh,0.5, na.rm = T),
    g2_md = quantile(g2,0.5, na.rm = T),
    NDVI_md = quantile(NDVIc,0.5, na.rm = T),
    PRI_md = quantile(PRIc,0.5, na.rm = T),
    PSND_md = quantile(PSNDc,0.5, na.rm=T),
    CRI2_md = quantile(CRI2c,0.5, na.rm=T),
    wood_moist_md = quantile(wood_moist,0.5, na.rm = T),
    growth_md = quantile(growth,0.5, na.rm=T),
    Flux_md = quantile(Flux,0.5, na.rm = T),
    VPD_md = quantile(VPD,0.5, na.rm = T),
    Flux_sum = sum(Flux/length(Flux)*24, na.rm = T),
    
  )




View(SumaryAllData)
write.csv(SumaryAllData, file="results/2020/19-20_GBS_moist_daily_data.csv")
write.csv(SumaryAllmData, file="results/2020/19-20_GBS_moist_monthly_data.csv")

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

########################## NDVI verification ##################################

#ALways better to do on your own
library(sf)
library(raster)
library(rgdal)
library(rgeos)
library(RColorBrewer)

TTcanopies = read_sf("TT_canopies.geojson")
TTcanopies = TTcanopies[-7:-1,]

RUDN <- stack("Y:\\YandexDisk\\NDVI\\SATELITE_DATA\\011169772030_01\\011169772030_01_P001_MUL\\19JUN04090505-M2AS-011169772030_01_P001.TIF")
RUDN <- brick(RUDN)

TIM <- stack("Y:\\YandexDisk\\NDVI\\SATELITE_DATA\\011169772050_01\\011169772050_01_P001_MUL\\19JUN04090502-M2AS-011169772050_01_P001.TIF")
TIM <- brick(TIM)


refl_NIR = function(nir){
  Lnir = nir*0.982*0.117971/1.004-3.752
  JD = 2458638.87853
  D = JD - 2451545.0
  g  = 357.529 + 0.98560028 * D
  d  = 1.00014 - 0.01671 * cos(g/180*pi) - 0.00014 * cos(2*g/180*pi)
  SunEl = 56.5
  phi = 90 - SunEl
  Enir = 1071.98
  Rnir = Lnir * d^2 * pi / (Enir*cos(phi/180*pi))
  return(Rnir)
} 

refl_RED = function(red){
  
  Lred = red*0.945*1.02/5.85-1.350
  JD = 2458638.87853
  D = JD - 2451545.0
  g  = 357.529 + 0.98560028 * D
  d  = 1.00014 - 0.01671 * cos(g/180*pi) - 0.00014 * cos(2*g/180*pi)
  SunEl = 56.5
  phi = 90 - SunEl
  Ered = 1555.11
  Rred = Lred * d^2 * pi / (Ered*cos(phi/180*pi))
  return(Rred)
} 

refl_NDVI = function(raster,nir_band,red_band){
  red = refl_RED(raster[[red_band]])
  nir = refl_NIR(raster[[nir_band]])
  ndvi = (nir-red)/(nir+red)
  return(ndvi)
}

rudn_ndvi = refl_NDVI(RUDN,4,3)  
tim_ndvi = refl_NDVI(TIM,4,3) 
timrudn_ndvi = raster::merge(rudn_ndvi,tim_ndvi)

writeRaster(rudn_ndvi, filename=file.path(getwd(), "rudn_ndvi.tif"), format="GTiff", overwrite=TRUE)
writeRaster(tim_ndvi, filename=file.path(getwd(), "tim_ndvi.tif"), format="GTiff", overwrite=TRUE)

q85 = function(x){
  return(quantile(x,0.85))
}

TTndvi = TTcanopies %>% mutate(
  NDVIMean = raster::extract(timrudn_ndvi, TTcanopies, fun = mean, na.rm = TRUE),
  NDVIMax = raster::extract(timrudn_ndvi, TTcanopies, fun = max, na.rm = TRUE),
  NDVIMin = raster::extract(timrudn_ndvi, TTcanopies, fun = min, na.rm = TRUE),
) %>% as.data.frame() %>% dplyr::select(-geometry)




NDVI_TT_score = AllData %>% filter(doy == 155, id %in% TTndvi$id) %>% left_join(TTndvi , by="id")%>%
                               filter(!is.na(NDVI))%>%
                               mutate(av_score = abs(NDVI - NDVIMean), 
                                      min_score = abs(NDVI - NDVIMin),
                                      max_score=abs(NDVI-NDVIMax),
                                      hour_score1=abs(hour-12),
                                      hour_score2=abs(hour-11),
                                      hour_score3=abs(hour-6))%>%
                               group_by(id)%>%summarise( hour_av = hour[which.min(av_score)],
                                                         hour_min = hour[which.min(min_score)],
                                                         hour_max = hour[which.min(max_score)], 
                                                         NDVI_tt_av = NDVI[which.min(av_score)],
                                                         NDVI_tt_min = NDVI[which.min(min_score)],
                                                         NDVI_tt_max = NDVI[which.min(max_score)],
                                                         NDVI_time = NDVI[which.min(hour_score1)],
                                                         NDVI_time2 = NDVI[which.min(hour_score2)],
                                                         NDVI_time3 = NDVI[which.min(hour_score3)],
                                                         NDVI_av = NDVIMean[which.min(av_score)],
                                                         NDVI_min = NDVIMin[which.min(min_score)],
                                                         NDVI_max = NDVIMax[which.min(max_score)],
                                                         NDVIMean = mean(NDVIMean))

NDVI_TT_score %>% as.data.frame()


NDVI_TT = AllData %>% filter(doy == 155, id %in% TTndvi$id%>%unique) %>%filter(!is.na(NDVI))%>%group_by(id)%>%
  summarise(mean = mean(NDVI, na.rm=T), 
            min = min(NDVI, na.rm = T),
            max = max(NDVI, na.rm = T))
         

TT = left_join(NDVI_TT,TTndvi, by="id")%>%as.data.frame()
plot(x=TT$max,y = TT$NDVIMax)

ggplot(data = NDVI_TT_score)+
  geom_point(aes(x=NDVI_tt_av, y=NDVI_av),color=1)+
  geom_point(aes(x=NDVI_tt_min, y=NDVI_min),color=2)+
  geom_point(aes(x=NDVI_tt_max, y=NDVI_max),color=3)


ggplot(data = NDVI_TT_score)+
  geom_smooth(aes(x=NDVI_tt_min, y=NDVI_min), method = lm, color=3)+
  geom_point(aes(x=NDVI_tt_min, y=NDVI_min),color=3)+
  xlab("NDVI по данными TreeTalker")+
  ylab("NDVI по данными спутника WorldView 3")+
  theme_bw()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"))
  




  mean_mod = lm(data = NDVI_TT_score, NDVI_tt_av~NDVI_av) %>% summary
  min_mod  = lm(data = NDVI_TT_score, NDVI_tt_min~NDVI_min)  %>% summary
  max_mod  = lm(data = NDVI_TT_score, NDVI_tt_max~NDVI_max) %>% summary
  mod  = lm(data = TT%>%filter (max!=1), max~NDVIMax) %>% summary
  
 
  


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

VTA = read_delim("data/VTA.csv", ";", escape_double = FALSE,trim_ws = TRUE)
VTA$id = paste("218A",VTA$id, sep="")
VTA = VTA %>%mutate(score = rowMeans(.[2:23], na.rm = T))
VTAscore =VTA %>% as.data.frame %>% dplyr::select(id, score)
library(readxl)
TT_MOS_DESC <- read_excel("TT_MOS_DESC.xlsx", col_types = c("text", "text", "numeric", 
                          "numeric", "numeric", "text", "text", "numeric", "skip", "skip", "numeric"))
TT_DESC = left_join(TT_MOS_DESC,VTAscore, by = "id")
write.csv(TT_DESC, file="all_TT_desc.csv")


VTA_PCA = left_join(SumaryAllmData, VTA, by="id")

VTA_PCA.raw  =  VTA_PCA [,4:91]%>% filter(!is.na(VTA)) %>% select(-ends_with("se"))
VTA_PCA.raw$growth[is.infinite(VTA_PCA.raw$growth)] = 0
VTA_PCA.raw = na.exclude(VTA_PCA.raw) 

VTA.pca = prcomp(VTA_PCA.raw, center = TRUE, scale = TRUE)

ggbiplot(VTA.pca,groups = VTA.pca$month, obs.scale = 1, var.scale = 1, ellipse=TRUE, circle = TRUE) +
  scale_color_discrete(name = '') +theme_bw()+xlim(-5,5)

ggscreeplot(VTA.pca, type = c("pev", "cev"))


################################# CORELLOGRAMS #################################

VTA.cor = cor(VTA_PCA.raw)

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



SummaryTableNDVI = AllData %>% group_by(Site,Species,doy, id, ) %>% filter(NDVI > 0 & NDVI < 0.97) %>%  
  summarise( mNDVI = quantile(NDVI,0.9, na.rm=T), sNDVI = sd(NDVI, na.rm=T), Flux = quantile(Flux,0.9, na.rm=T), VTA_score = mean(VTA_score)) %>% 
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

SummaryTableOptimalCritical %>% as.data.frame()
  
  
  
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

TablesGraph[5,2][[1]]


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


ggplot(data = SummaryTableANOVAm%>%filter(Species %in% c("Acer platanoides","Betula pendula",
                                                         "Larix sibirica", "Pinus sylvestris","Picea abies","Quercus robur","Tilia cordata")))+
  geom_line(aes(y=NDVIm, x = VTAscore, group = Species, color = Species),position = position_dodge(width = 0.1), size=1)+
  geom_errorbar(aes(ymin=NDVIm-NDVIer, ymax=NDVIm+NDVIer ,
                    x=VTAscore, group=Species, color=Species),position = position_dodge(width = 0.1), size=1, width = .2) +
  
  ylim(c(0,1))+
  ylab("NDVI")+
  labs(color = "Виды")+
  xlab("Категория состояния дерева")+
  scale_color_lancet()+
  theme_bw()








 SummaryTableANOVA$optFlux %>% max(na.rm = T)

SummaryVTAvsNDVI = left_join(SummaryTableNDVI, models, by="Species") %>% 
                  filter(Species %in% c("Acer platanoides", "Betula pendula", "Fraxinus","Pinus sylvestris", "Quercus robur", "Tilia cordata"))%>%
  ungroup %>% select(id,Species, mNDVI,mFlux, VTAscr, R2) %>% mutate(TTscr = mFlux/ mNDVI)
                  
SummaryVTAvsNDVI %>%arrange(Species)%>% as.data.frame()

 summary(models[10,2][[1]][[1]])$r.squared
models[17,2][[1]][[1]]%>%summary


ggplot(data = SummaryTableNDVI )+geom_point(aes(x=VTAscr,y=mxNDVI - mnNDVI, color =age_group))
ggplot(data = SummaryTablePRI)+geom_point(aes(x=VTAscore,y=log(mPRI)))
PRImod = lm(data = SummaryTablePRI, VTAscore~log(mPRI))
summary(PRImod)
####################### TABLES GENERATOR ################################33


library(summarytools)

AllDataf$Species[AllDataf$Species == "Quercus robur "] = "Quercus robur"   
AllDataf$Species[AllDataf$Species == "Tilia cordata "] = "Tilia cordata"   

SummaryTableFlux = AllDataf %>% group_by(Site, Species, doy, id) %>% mutate(update = length(id))%>%
  summarise(sFlux = sum(Flux, na.rm=T), readings = mean(update), DBH = mean(DBH, na.rm=T)) %>%
  mutate(suFlux = sFlux * readings / 24) %>% filter(!is.na(suFlux)) %>% filter(!is.infinite(suFlux))%>%
  group_by(Species)%>%filter(!out_of_two_sigma(suFlux))%>%
  group_by(Site, Species, doy) %>% summarise(Flux = mean(suFlux, na.rm=T), DBH = mean(DBH))

SummaryTableFluxm = AllDataf %>% 
  group_by(Site, Species, doy, id, month, antrop_load, VTA_score, age_group_index)%>% mutate(update = length(id)) %>%
  summarise(sFlux = sum(Flux, na.rm=T), readings = mean(update), DBH = mean(DBH, na.rm=T)) %>%
  mutate(suFlux = sFlux * readings / 24) %>% filter(!is.na(suFlux)) %>% filter(!is.infinite(suFlux))%>%
  group_by(Species)%>%filter(!out_of_two_sigma(suFlux))%>%
  group_by(Site, Species,  month, antrop_load,  VTA_score, age_group_index, doy) %>% 
  summarise(Flux = mean(suFlux, na.rm=T), DBH = mean(DBH))





Flux_species_site = SummaryTableFlux %>% as.data.frame %>% group_by(Site, Species) %>%  filter(Flux>0) %>%
  summarise(mean = mean(Flux, na.rm=T), sd =sd(Flux, na.rm=T), max=quantile(Flux,.9, na.rm=T), min=min(Flux, na.rm=T) ) %>%
  as.data.frame()

Flux_soecies_site_month = SummaryTableFluxm %>% as.data.frame %>% group_by(Site, Species, month) %>% 
  summarise(mean = mean(Flux, na.rm=T), sd =sd(Flux, na.rm=T), max=quantile(Flux,.9, na.rm=T), min=min(Flux, na.rm=T) ) %>%
  as.data.frame()

Flux_species_load = SummaryTableFluxm %>% as.data.frame %>% group_by(Species, antrop_load, ) %>%  filter(Flux>0) %>% 
  summarise(mean = mean(Flux, na.rm=T), sd =sd(Flux, na.rm=T), max=quantile(Flux,.9, na.rm=T), min=min(Flux, na.rm=T) ) %>%
  as.data.frame()

Flux_species_load_month = SummaryTableFluxm %>% as.data.frame %>% group_by(Species,antrop_load,  month) %>% 
  summarise(mean = mean(Flux, na.rm=T), sd =sd(Flux, na.rm=T), max=quantile(Flux,.9, na.rm=T), min=min(Flux, na.rm=T) ) %>%
  as.data.frame()

Flux_species_vta = SummaryTableFluxm %>% as.data.frame %>% group_by(Species,VTA_score) %>% filter(Flux>0) %>%
  summarise(mean = mean(Flux, na.rm=T), sd = sd(Flux, na.rm = T), max = quantile(Flux,.9, na.rm = T), 
  min=min(Flux, na.rm=T) ) %>%
  as.data.frame()


Flux_species_vta_month = SummaryTableFluxm %>% as.data.frame %>% group_by(Species,VTA_score,  month) %>% 
  summarise(mean = mean(Flux, na.rm=T), sd = sd(Flux, na.rm=T), max=quantile(Flux,.9, na.rm=T), min=min(Flux, na.rm=T) ) %>%
  as.data.frame()

Flux_species_age = SummaryTableFluxm %>% as.data.frame %>% group_by(Species,age_group_index) %>% filter(Flux>0) %>%
  summarise(mean = mean(Flux, na.rm = T), sd = sd(Flux, na.rm=T), max=quantile(Flux,.9, na.rm=T), min=min(Flux, na.rm=T) ) %>%
  as.data.frame()

Flux_species_age_month = SummaryTableFluxm %>% as.data.frame %>% group_by(Species,age_group_index,  month) %>% 
  summarise(mean = mean(Flux, na.rm=T), sd = sd(Flux, na.rm=T), max=quantile(Flux,.9, na.rm=T), min=min(Flux, na.rm=T) ) %>%
  as.data.frame()



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
plots = data %>% group_by(Species) %>%)
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
RUDN = RUDN %>% filter(doy>180)
RUDN = RUDN  %>% filter(id %in% id2show)
RUDN %>% group_by(id) %>% summarise(Sp = as.factor(Species)%>% levels)

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



##################################### ZLATKA ELETS ############################################

Zl = AllData %>% filter(Site=="ELETS")
ggplot()+
  geom_point(data = Zl%>% filter(id=="21880205"), aes(x = time, y = Flux), color = 2, size =.3)+
  geom_line(data = Zl%>% filter(id=="21880205"), aes(x = time, y = Flux), color = 2, size =.1)+
  geom_smooth(data = Zl%>% filter(id=="21880205"), aes(x = time, y = Flux, color = id),span = .5,  size = .6, se = T)+
  geom_point(data =Zl%>% filter(id=="218A0195"), aes(x = time, y = Flux), color = 3, size = .2)+
  geom_line(data = Zl%>% filter(id=="218A0195"), aes(x = time, y = Flux), color = 3, size = .1)+
  geom_smooth(data = Zl%>% filter(id=="218A0195"), aes(x = time, y = Flux, color = id),span = .5,  size = .6, se = T)+
  ylim(0,8)+
  xlab("")+
  ylab("Скорость сокотечения, л/ч")+
  theme_bw()

ggplot()+
  geom_point(data = Zl%>% filter(id=="21880205"), aes(x=time, y=NDVIc), color=2, size=.1)+
  #geom_line(data = Zl%>% filter(id=="21880205"), aes(x=time, y=NDVIc), color=2, size=.1)+
  geom_smooth(data = Zl%>% filter(id=="21880205"), aes(x=time, y=NDVIc, color=id),span=3,method="loess",  size=.6, se=T)+
  geom_point(data =Zl%>% filter(id=="218A0195"), aes(x=time, y=NDVIc), color=3, size=.1)+
  #geom_line(data = Zl%>% filter(id=="218A0195"), aes(x=time, y=NDVIc), color=3, size=.1)+
  geom_smooth(data = Zl%>% filter(id=="218A0195"), aes(x=time, y=NDVIc, color=id),span=3,method="loess",  size=.6, se=T)+
  ylim(0,1)+
  xlab("")+
  ylab("NDVI")+
  theme_bw()

##################################### DB to EXCEL ########################################################


export_all_to_excel(c("SiteIndex","Species","age_group_index","load",
                      "insite_load","VTAscore",	"id",	"time",	"diam",
                      "tair","rh","VPD","theta","psi","phi","g2","nt1",	"NDVI","W","Flux"), 
                    AllData %>% filter(year >2019) )


  

