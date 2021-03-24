source("code/TTcalc_site_7.R")

recharge_schedule_read = function(back_folders){
  
  if (list.files(back_folders,pattern="recharge_schedule.csv") %>% length() > 0)
  {
    schd = read_csv(file = paste(back_folders,"recharge_schedule.csv",sep=""))
  } else 
  { 
    warning("file not found")
    return(NULL)
  }
  schd = schd %>% group_by(id) %>% mutate(charge_date = dmy(charge_date)) %>%
    arrange(charge_date) %>% mutate(charge = 1:n())
  
  return(schd)
}

read_backup_folder_data = function(b_folder){
  data = data.frame()
  for(file in list.files(b_folder,pattern="*.txt")){
    temp =  read_delim(paste(b_folder,file, sep="/"),
                       col_names = FALSE, delim=";", skip=0)
    data = rbind(data, temp)
  }
  return(data)
}

extrapolate_corr_dates = function(data){
  #just a beautiful number before which there were no active TT+
  mp = check_measure_period(data, "con")
  if(any(data$datetime > 1520000000)){
    print(data$id %>% unique())
    start_cor_date = min(data$datetime[data$datetime > 1520000000], na.rm=T)
    start_cor_date_index = which(data$datetime == start_cor_date)
    print(start_cor_date)
    print(start_cor_date_index)
    if(start_cor_date_index > 1){
      start_cor_date = start_cor_date - mp*(start_cor_date_index-1)
    }
    print(start_cor_date)
    
    data$datetime = start_cor_date+0:(length(data$datetime)-1)* mp
    if(unique(data$id) == "21880217") {
      data$datetime = start_cor_date+0:(length(data$datetime)-1)* mp
    }
    print(max(data$datetime))
    return(data)
  } else {
    
    print("No correct dates to extrapolate")
    return(data)
  }
  
  
}

TTBasicCalcForDateRecon = function(data){
  data$X1 = paste("10.05.22 12:55:19",data$X1,sep=",")
  data$X12 = as.double(data$X12)
  
  bc = TTBasicCalc(data , verboseFlag = "con")
  
  
  bc = bc %>% group_by(id) %>% arrange(rec_num) %>%
    mutate(recharge_flag =ischarged(.data))%>%
    mutate(charge = cumsum(recharge_flag))
  bc$charge = bc$charge+1
  return(bc)
}


DateReconstrForNoCloud = function(data_bc, schedule){
  for(i in 1:nrow(schedule)){
    #Calculating hour of the sunset for day of recharge from recharge schedule file
    sunsethour = round(solartime::computeSunsetHourDoy(
      yday(schedule$charge_date[i]),
      latDeg = schedule$lat[i], 
      longDeg = schedule$lon[i], 
      timeZone = schedule$tz[i]),
      0)
    #Calculating day of recharge sunset in UNIX time format
    sunset_unix_time = schedule$charge_date[i] %>% as.POSIXct() %>% 
      as.integer() + sunsethour*60*60
    sunset_unix_time = as.POSIXct.numeric(sunset_unix_time, origin = "1970-01-01", 
                                          tz = "Europe/Moscow")
    # generating vector mask for iterated charge and id   
    i_mask = data_bc$charge == schedule$charge[i] & data_bc$id == schedule$id[i]
    
    #checking at which position from charge restart we had sunset this day
    # taking position of first measurement which is below 10 for blue band
    sunset_index = which(i_mask & data_bc$b_V_450 < 10)[1]
    
    #setting correct time for sunset on first day after recharge    
    data_bc$datetime[sunset_index] = sunset_unix_time
    
  }
  
  tt_cor = data_bc %>% group_by(id,charge) %>% do(
    extrapolate_corr_dates(.)
  ) %>% ungroup()
  
  return(tt_cor)
}
corectedBasicData = function(data_bc, data_basic_time_cor){
  data_bc$rec_num = HexToDec(data_bc$X2)
  # vector mask for 45 record
  record_mask = which(data_bc$rec_num %in% data_basic_time_cor$rec_num)
  # vector mask for 49 record
  spec_rec_mask = record_mask+1
  # removing mask positions which are not 49, due to wrong order
  spec_rec_mask = spec_rec_mask[-which(spec_rec_mask %in% record_mask)]
  # setting correct time for record 45
  data_bc$X4[record_mask] = data_basic_time_cor$datetime %>% as.numeric()
  # setting correct time for record 49
  data_bc$X4[spec_rec_mask] = data_bc$X4[spec_rec_mask-1]
  # reconstructing server 
  data_bc$X1 = paste(as.character( format((data_bc$X4 +
    HexToDec(data_bc$X2) + 3600*3) %>% as_datetime,"%d.%m.%y %H:%M:%S")),
    data_bc$X1,sep = ",")
  #removing rec_num column
  data_bc = data_bc[-21]
  
  return(data_bc)
}

back_folder = "data/backup/SPB_FI/"
data_bc = read_backup_folder_data(back_folder)
schedule = recharge_schedule_read(back_folder)
data_basic = TTBasicCalcForDateRecon(data_bc)
data_basic_time_cor = DateReconstrForNoCloud(data_basic,schedule)
data_bc_cor = corectedBasicData(data_bc, data_basic_time_cor)

#reading cloud data
cloud_data =  read_delim(file="http://naturetalkers.altervista.org/C1870012/ttcloud_old.txt", delim = ";", col_names = F)
#adding reconstructed data to cloud data
reconstr_cloud = rbind(cloud_data, data_bc_cor)


write_delim(reconstr_cloud, file = "reconstr_spb_cloud2.txt", delim=";", col_names = F)

ggplotly(ggplot(data = data_basic_time_cor,aes(x = datetime, y = tair, color=id)) + geom_point())  

SPBdata = TTcalc_site(c("http://naturetalkers.altervista.org/C1870012/ttcloud.txt"),
                      installation_start,
                      import_folder_name = NULL,
                      first_imported_dates_reconstructed = F,
                      "data/TT_desc_20.csv",
                      "SPB",
                      verbose = "con")

SPB = SPBdata[[2]]



plot_ly(data = reconstr_cloud, x = ~X4, y = ~X18)
plot_ly(data = data_basic_time_cor, x = ~datetime, y = ~volt, color = ~id, facet_col= ~charge)

plot_ly(data = SPB %>% filter(Flux2<100), x = ~time, y = ~dist3, color = ~id, facet_col= ~notes)
write.csv(SPB, file="spb_reconstr.csv")



SPB = SPB %>% as.data.frame()
for (i in 1:ncol(SPB)){
  SPB[ SPB[,i] %>% is.infinite(),i] = NA  
}
SPB$month = month(SPB$time)
SumarySPBdData = SPB %>% group_by(doy,id) %>% mutate(psi_d = mean(psi, na.rm = T),
                                                          phi_d = mean(phi, na.rm = T), 
                                                          theta_d = mean(theta, na.rm = T))%>%
  ungroup() %>% group_by(doy, id) %>% mutate(
    dtheta  = (theta- theta_d),
    dphi  = (phi- phi_d),
    dpsi  = (psi- psi_d)) %>% ungroup() %>%
  mutate(g2 = gx2+gy2+gz2,
         mHz = max(Hz, na.rm = T))%>%
  mutate(Hzmoist = (mHz-Hz)/(mHz+Hz)*50) %>%
  group_by(id,Site,Species,doy ) %>% 
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
