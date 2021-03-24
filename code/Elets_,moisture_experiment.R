source("code/TTcalc_site_7.R")

ELTSdata = TTcalc_site(c("http://naturetalkers.altervista.org/C18A0230/ttcloud.txt",
                         "http://naturetalkers.altervista.org/C1850003/ttcloud.txt"),
                       installation_start,
                       import_folder_name = NULL,
                       first_imported_dates_reconstructed = F,
                       "data/TT_desc_20.csv",
                       "ELETS",
                       verbose = "сon")

  ############################################################################################################
  #######################################                              #######################################
  #######################################  VERY DIRTY RECONCTRUCTION   #######################################
  #######################################                              #######################################
  ############################################################################################################


data_bc = data.frame()

for(file in list.files("data/backup/Elets_control/",pattern="*.csv")){
 temp =  read_delim(paste("data/backup/Elets_control/",file, sep="/"),
             col_names = FALSE, delim=";", skip=0)
  data_bc = rbind(data_bc, temp)
}


# CLOUD FILE WAS UPDATED WITH BACKED UP DATA ON THE SERVER 
#cloud = read_delim("data/backup/Elets_zlatka_atacked/C18A0030.csv", col_names = FALSE, delim=";", skip=0)
# 
# shift = c(0,cloud$X4[2:length(cloud$X4)]-cloud$X4[1:(length(cloud$X4)-1)])
# shiftc = cumsum(shift)
# sshift = c(F,shiftc[2:length(shiftc)]<=shiftc[1:(length(shiftc)-1)])
# shiftc[sshift] = shiftc[which(sshift)-1]+6
# sshift = c(F,shiftc[2:length(shiftc)] <= shiftc[1:(length(shiftc)-1)])
# shiftc[sshift] = shiftc[which(sshift)-1]+6
# shiftc = shiftc + 7200
# 
# 
# cloud$X1 = paste(as.character( format(cloud$X4[1]+shiftc %>% as_datetime,"%d.%m.%y %H:%M:%S") 
# ),cloud$X1,sep=",")
# cloud2 = read_delim("http://naturetalkers.altervista.org/C18A0230/ttcloud.txt", col_names = FALSE, delim=";", skip=0)
# 
# cloud3 = rbind(cloud,cloud2)
# write_delim(cloud3, path="C18A0230_cor.txt", delim=";", quote = F, na="")
cloud = read_delim("http://naturetalkers.altervista.org/C18A0229/C18A0229_cloud_rec.txt", col_names = FALSE, delim=";", skip=0)

# tt = read_delim("data/backup/Elets_control/21880217.txt",col_names = FALSE, delim=";", skip=0)
# tt$X2 = 1:length(tt$X2) %>% DecToHex()
# write_delim(tt, path="data/backup/Elets_control/21880217_.csv", delim=";", quote = F, na="")
# cloud = read_delim("http://naturetalkers.altervista.org/C1850003/ttcloud.txt", col_names = FALSE, delim=";", skip=0)
# cloud$X1 %>% unique() %>% sort()
# 
# data_bc$X1 = paste("10.05.21 12:55:19",data_bc$X1,sep=",")
# data_bc = rbind(data_bc, cloud)

#source variable will be created from that
#data_bc$s=i+1
data_bc$X1 = paste("10.05.21 12:55:19",data_bc$X1,sep=",")
data_bc$X12 = as.double(data_bc$X12)
#print(as.data.frame(data_bc))
tt_bc = TTBasicCalc(data_bc , verboseFlag = "con")


tt_bc = tt_bc %>% group_by(id) %>% arrange(rec_num) %>%
  mutate(recharge_flag =ischarged(.data))%>%
  mutate(charge = cumsum(recharge_flag))
tt_bc$charge = tt_bc$charge+1

tt_bc %>% filter(id == "21880217") %>% as.data.frame %>% ggplot(aes(x=rec_num, y = volt))+geom_point(aes(color=datetime))

ELTSdata[[2]] %>%filter(id == "218A0195")%>%  ggplot(aes(x=datetime,y=volt))+geom_point(aes(color=charge%>% as.factor))+ 
  geom_point(data = tt_bc%>% filter(id == "21880214"),aes(x=1:length(volt)+5,y=volt))+theme_bw() 
ggplotly()

ELTSdata[[2]] %>% ggplot(aes(x=1:length(volt),y=volt))+geom_point(aes(shape=as.factor(year(datetime)), color=id))

dat_cor = data.frame(
  id = c(21880201,
         21880214,21880217,
         21880203,21880214,21880217,21880209,21880212,21880201,
         21880203,21880214,21880217,21880209,21880212),
  start_shift = c(1591349400,
                  1593604800,1593604800,
                  1596196800,1596196800,1596196800,1596196800,1596196800,1598529600,
                  1598529600,1598529600,1598529600,1598529600,1598529600  ),
  charge = c(1,  2,2, 3,4,3,3,3,3,  4,5,4,4,4)
)








for( i in 1:nrow(dat_cor)){
  tt_bc$datetime[ tt_bc$charge == dat_cor$charge[i] & tt_bc$id == dat_cor$id[i] ][1] =
    tt_bc$datetime[ tt_bc$charge == dat_cor$charge[i] & tt_bc$id == dat_cor$id[i] ][1] + dat_cor$start_shift[i]
}

extrapolate_corr_dates = function(data){
  if(any(data$datetime > 1556845000)){
    print(data$id %>% unique())
    start_cor_date = min(data$datetime[data$datetime > 1556845000], na.rm=T)
    
    start_cor_date_index = which(data$datetime == start_cor_date)
    print(start_cor_date)
    print(start_cor_date_index)
    if(start_cor_date_index > 1){
      start_cor_date = start_cor_date - 60*90*(start_cor_date_index-1)
    }
    print(start_cor_date)
    
    data$datetime = start_cor_date+0:(length(data$datetime)-1)*60*90
    if(unique(data$id) == "21880217") {
      data$datetime = start_cor_date+0:(length(data$datetime)-1)*60*60 
    }
    print(max(data$datetime))
    return(data)
  } else {
    
    print("No correct dates to extrapolate")
    return(data)
  }
    

}
  


tt_cor = tt_bc %>% group_by(id,charge) %>% do(
  extrapolate_corr_dates(.)
) %>% ungroup()

#data_bc = data_bc[-26687,]
#data_bc = data_bc %>% filter(!(X1 %in% c("21880215","218A0242")))%>% as.data.frame()
#tt_cor = tt_cor %>% filter(!(id %in% c("21880215","218A0242")))%>% as.data.frame()
#data_bc$X4 = as.integer(tt_cor$datetime)%>%rep(,each=2)

# data_bc$gx    = as.numeric(data_bc$X12)/4096
# data_bc$gx2   = as.numeric(data_bc$X13)/16777216
# data_bc$gy    = as.numeric(data_bc$X14)/4096
# data_bc$gy2   = as.numeric(data_bc$X15)/16777216
# data_bc$gz    = as.numeric(data_bc$X16)/4096
# data_bc$gz2   = as.numeric(data_bc$X17)/16777216
# data_bc$type  = as.numeric(data_bc$X3)
data_bc = data.frame()

for(file in list.files("data/backup/Elets_control/",pattern="*.csv")){
  temp =  read_delim(paste("data/backup/Elets_control/",file, sep="/"),
                     col_names = FALSE, delim=";", skip=0)
  data_bc = rbind(data_bc, temp)
}
# cloud = read_delim("data/backup/GBS/C18A0229.txt", col_names = FALSE, delim=";", skip=0)
# 
# cloud$X1 %>% unique() %>% sort()
# cloud = cloud %>% filter(X1 %in% c("21880215","218A0242")) %>% arrange(X1)
# data_bc = rbind(data_bc, cloud)


data_bc$id    = as.character(data_bc$X1)
data_bc$rec_num = data_bc$X2 %>% HexToDec

data_bcj = left_join(data_bc,tt_cor%>%select(datetime,id,rec_num), by=c("id","rec_num"))

data_bcj$datetime[which(data_bcj$X3=="49")]=data_bcj$datetime[which(data_bcj$X3=="49")-1]
data_bcj$X4[!is.na(data_bcj$datetime)] = as.integer(data_bcj$datetime[!is.na(data_bcj$datetime)])


data_bcj = data_bcj %>% select(X1:X20)
data_bcj$X1 = paste(as.character( format((data_bcj$X4+HexToDec(data_bcj$X2)+3600*3) %>% as_datetime,"%d.%m.%y %H:%M:%S") 
                                 ),data_bcj$X1,sep=",")


data_bcj = data_bcj %>% filter(X4 > 1556845000)

ttt = TTBasicCalc(data_bcj , verboseFlag = "con")
ttt = remove_common_duplicates(ttt)
ttt = ttt %>% group_by(id) %>% mutate(rn = 1:length(volt))
ttt$years = year(ttt$serv_datetime)
ttt$doys  = yday(ttt$serv_datetime)
ttt$hours = hour(ttt$serv_datetime)


descr_file = "data/TT_desc_20.csv"
sitename = "ELETS"
verboseFlag = "сon"
SITE_list = suppressWarnings(suppressMessages(
  read_delim(descr_file,col_names = T, delim=",")))
if (!is.null(sitename)){
  SITE_list = SITE_list%>%filter(Site == sitename) 
  if(SITE_list %>% length <1) {
    fun_log(verboseFlag = verboseFlag, c(
      "Looks like you have error in site name.\n"
    ))
  }
}



ttt = ttt  %>% filter(id %in% SITE_list$id)
if(ttt %>% nrow <1) {
  fun_log(verboseFlag = verboseFlag, c(
    "Looks like you have error in site name.\n"
  ))
}
ttt$time = ttt$datetime



ttt$year = year(ttt$time)
ttt$week = week(ttt$time)
ttt$month = month(ttt$time)
ttt$doy = yday(ttt$time)
ttt$hour = hour(ttt$time)
ttt$min = minute(ttt$time)

ttt = ttt %>% group_by(id,year,doy) %>%
  mutate(dT = nt2 - t2, na.rm = T) %>%
  mutate(dTa = nt2 - t2 - nt1 + t1, na.rm = T) %>%
  mutate(dTm = max(dT, na.rm=T)) %>%
  mutate(dTam = max(dTa, na.rm=T)) %>%
  mutate(maxTd = max(dist), na.rm = T) %>%
  mutate(meanTd = mean(dist), na.rm = T) %>%
  mutate(minTd = min(dist), na.rm = T) %>%
  mutate(u = 119*(10^-3)*(dTm/dT - 1)^1.231, na.rm = T) %>% #l m-2 s-1
  mutate(u2 = 118.99*(10^-3)*(dTm/(nt2-t2-nt1+t1) - 1)^1.231, na.rm = T) %>% #l m-2 s-1)
  ungroup()
ttt = ttt %>% group_by(id,year) %>%
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
ttt = ttt %>% ungroup() %>% group_by(id) %>% 
  mutate(dist2 = dist /volt*max(volt, na.rm=T) )

ttt = left_join(ttt,SITE_list,by="id")
ttt = ttt %>% mutate(DBH = DBH / pi)
ttt = ttt %>%
  mutate(Flux = u*3600*(DBH^1.8777)*0.755/10000, na.rm = T) %>%
  mutate(Flux2 = u2*3600*(DBH^1.8777)*0.755/10000, na.rm = T)
fun_log(verboseFlag = verboseFlag, c("Adding growth assesment \n")) 
ttt = AddGrowth(ttt)
fun_log(verboseFlag = verboseFlag, c("Choosing correct wood moisture according to species\n"))
ttt = AddMoist(ttt)

notes = SITE_list$notes %>% str_split_fixed(fixed("|"), n = 2)
factor_table  = data.frame(id = SITE_list$id, atack_level = (notes[,1]),
                           pesticide = (notes[,2])) 
factor_table[factor_table == "low"] = "Участок 2"
factor_table[factor_table == "high"] = "Участок 1"
factor_table[factor_table == "arbojet"] = "Инъекции арбаджет"
factor_table[factor_table == "reference"] = "Контроль"
factor_table$atack_level = as.factor(factor_table$atack_level)
factor_table$pesticide = as.factor(factor_table$pesticide)

ELETS  = ELTSdata[[2]]
ELETS  = ELETS %>% filter(!(id %in% (ttt$id %>% unique())))
ELETS  = rbind(ELETS,ttt)
ELETS = left_join(ELETS, factor_table, by = "id")
write.csv(ELETS, file="Elets_zlatka_recovered.csv")

#ELETS$dECfb[ELETS$doy < 175] = NA
ELETS = ELETS %>% group_by(id,year) %>% 
  mutate(dECfdb = (dECfb-quantile(dECfb,0.01, na.rm=T))/(quantile(dECfb,0.99,na.rm=T)-quantile(dECfb,0.01,na.rm=T))) %>%
  mutate(VWWCb = -0.36*dECfdb + 0.45)

plot_ly(data = ELETS, x = ~datetime, y = ~PRI, color = ~id)
plot_ly(data = ELETS%>%filter(notes == "atacked"), x = ~datetime, y = ~VWWCb, color = ~id)
plot_ly(data = ELETS%>%filter(notes == "control"), x = ~datetime, y = ~VWWCb, color = ~id)





ggplot(data = ttt, aes(x = datetime, y = volt))+geom_point(aes(color=id))
ggplotly()
write_delim(data_bcj, path="C185003rec.csv", delim=";", quote = F, na="")

plot_ly(data = ELTSdata[[2]], x = ~time, y = ~tair, color = ~id)

plot_ly(data = ttt, x = ~datetime, y = ~tair, color = ~id)




plot_ly(data = ELETS, x = ~datetime, y = ~theta+180, color = ~id, facet_col= ~notes)

ggplot(data = ELETS %>% filter(VWWCb > 0), 
       aes(x = datetime, y = VWWCb*100, color = id)) +
  geom_point(alpha = 1/10) +
  geom_smooth() +
  ylab("Влажность, %")+
  facet_grid(atack_level ~ pesticide) +
  theme_bw()


ggplot(data = ELETS %>% filter(Flux2 < 10)%>% filter(!(id %in% c("21880203", "21880216"))), 
       aes(x = datetime, y = Flux2, color = id)) +
  geom_point(alpha = 1/10) +
  ylab("Сокотечение, л/ч")+
  geom_smooth() +
  facet_grid(atack_level ~ pesticide) +
  theme_bw()
 

ggplot(data = ELETS %>% filter(PRI2 < .5, PRI2 > -.1)%>% filter(!(id %in% c("21880210","21880217"))), 
       aes(x = datetime, y = PRI2, color = id)) +
  geom_point(alpha = 1/10) +
  
  geom_smooth() +
  facet_grid(atack_level ~ pesticide) +
  theme_bw()



ggplot(data = ELETS %>% filter(NDVI < 1, NDVI > 0)%>% filter(!(id %in% c("21880210","21880217"))), 
       aes(x = datetime, y = NDVI, color = id)) +
  geom_point(alpha = 1/10) +
  geom_hline(aes(yintercept = .45))+
  geom_smooth() +
  facet_grid(atack_level ~ pesticide) +
  theme_bw()
