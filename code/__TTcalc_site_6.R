# Developmen log
# version 6
# What is new

# Added BEFdata for central Russia species
# Added LAI calculation according top Beer law
# Corrected variable filtering according to it position, now filtered by name
# Starting thinking to switch to dtplyr
# Switched from cat to special logging function
# Switching to foreach %dopar%
# Added function for correcting wrong time extrapolation according to other results
# Added function for finding correct dates according to pattern matching of tair
# Moisture added
# PRI updated according to its logic - compare wavelengths around green(550) in classical PRI its 530 and 570
# in our case it is 500 and 600. Previous version which used asimetrical interval 550-570 - called PRI2 
# Luca's spectral calibration added

# TODO prepare third version of results without this variables
# RRR, E2, G2, C2, ff20, DBH, DTT, installation_height, X1, Site, minTd, meanTd, maxTd, step_time, corr_time, serv_cor, imported, rn, wrong_server,zone, SiteIndex, na.rm, datetime,  rec_num, PRI,PRI2, MCARI,recharge_flag, charge , cor_dt, MTCI,CIg,CIr,CRI1,CRI2,SIPI,PSSR,PSND,PSRI,CARI,EVI2,VARI, b_O_600,b_Y_570,b_G_550,b_B_500,b_V_450, b_R_610,source,id_rec, gx, gy, gz, serv_datetime, wrong_time,X,incr, type, b_R_650,b_W_860,b_V_810,b_U_760,b_T_730,b_S_680, NDVI,EVI,Nr,Rr,Br,Gr

source("code//TT_graphs.R") 
library(dtplyr)
library(data.table)
library(tidyverse)
library(plyr)
library(lubridate)
library(DescTools)
library(plotly)
library(tsibble)
library(openxlsx)
library(stringr)
library(ggsci)
library(corrplot)
library(Hmisc)
library(plotly)
library(foreach)

#Logging function to log all activities into memory, to console or in file=====
message_log = "Starting data log"
fun_log = function(verboseFlag = "mem", message){
  
  if (verboseFlag == "con"){
    cat(message)
  }
  if (verboseFlag == "mem"){
    assign(message_log, c("message_log",paste(message,collapse=" ")), env = globalenv())
  } 
  if (verboseFlag == "file"){
    file_name = paste("logs//",Sys.Date(),"-calc-loging.txt", collapse = "" )
    write(paste(message,collapse = ""),file=file_name,append=TRUE)  
  }
  
} 






#Basic calculations=============================================================
TTBasicCalc = function(tdt, verboseFlag){
  fun_log(verboseFlag = verboseFlag, c("\n",
      "Basic calculation started, adding not algorithmic variables to table ",
      dim(tdt)[1],"x",dim(tdt)[2],"\n"))
  #Clearing unclear
  tdt[tdt == -Inf] = NA
  tdt[tdt ==  Inf] = NA

  #Separate different types of devices
  tdt2 = tdt %>% filter(X3 == 46 | X3 == 49)
  tdt = tdt %>% filter(X3 == 45 )
  tdt_40 = tdt %>% filter(X3 == 40)

  # Basic variables for type 45
  tdt$incr = 1:length(tdt$X1)
  tdt$datetime = tdt$X4 %>% as_datetime( tz="Europe/Moscow",
                                         origin = "1970-01-01 00:00:00")
  tdt$rec_num = tdt$X2 %>% HexToDec
  tdt$serv_datetime = tdt$X1 %>% str_trunc(17,"right", ellipsis = "") %>%
    dmy_hms(  tz="Europe/Rome") %>% as.integer %>%
    as.POSIXct(,origin = "1970-01-01 00:00:00", tz="Europe/Moscow")
  tdt$wrong_time=((tdt$datetime<157680000)|((tdt$datetime)>tdt$serv_datetime))
  tdt$id = tdt$X1 %>% as.character %>%
    str_trunc(8,"left",  ellipsis = "")
  tdt$type = tdt$X3 %>% as.double
  fun_log(verboseFlag = verboseFlag, c("Voltage and proximity sensor \n"))
  #Voltage and proximity sensor
  tdt$volt      = (1.1*131072 / tdt$X8)+0.65
  tdt$dist = 4.6728*(tdt$X7*1.1/tdt$X8)^(-1.274)
  tdt$pulses     = tdt$X7
  tdt$Hz     = tdt$X20


  #Climate
  tdt$rh    = tdt$X10
  tdt$tair  = tdt$X11/10
  tdt$VPD   = 0.6108*exp((17.27*tdt$tair)/(tdt$tair+265.5))*(1 - tdt$rh/100)
  #Accelerometer
  tdt$gx    = tdt$X12/4096
  tdt$gx2   = tdt$X13/16777216
  tdt$gy    = tdt$X14/4096
  tdt$gy2   = tdt$X15/16777216
  tdt$gz    = tdt$X16/4096
  tdt$gz2   = tdt$X17/16777216
  tdt$accel = (tdt$gx^2 + tdt$gy^2 + tdt$gz^2)^0.5
  tdt$theta = atan(tdt$gx/(tdt$gy^2+tdt$gz^2)^0.5) /pi * 180
  tdt$psi   = atan(tdt$gy/(tdt$gx^2+tdt$gz^2)^0.5) /pi * 180
  tdt$phi   = atan(tdt$gz/(tdt$gy^2+tdt$gx^2)^0.5) /pi * 180
  #Temoerature probes
  tdt$t1    = tdt$X5 /10
  tdt$nt1   = tdt$X6 /10
  tdt$t2    = tdt$X18/10
  tdt$nt2   = tdt$X19/10
  
  mx= 119.639 - (0.0420 * (tdt$t1)) - (0.00761 * tdt$X20) 
  y0=-209.9931
  a=37.1602	
  b=-2.2091	
  c=0.0557	
  d=-0.0005
  
  
  tdt$moist = y0+a*mx+b*mx^2+c*mx^3+d*mx^4
  
  
  if (any(names(tdt)=="s")){
    tdt$source  = tdt$s
  }


  # Removing duplicates only according to data variables - slow but robust
  # filtering only type 45 since left alignment - 46 type rows will be added
  # only to rows of 45 with the same rec_num
  #print(names(tdt))
  
  duplicated_data_row = tdt[,c("id","dist","rh","tair","VPD","gx","gx2","gy","gy2")] %>% duplicated %>% which
  fun_log(verboseFlag = verboseFlag, c("\n","Table had ",dim(tdt)[1]," rows, found ",
      duplicated_data_row %>% length, " rows to be removed.","\n" ))
  if(duplicated_data_row %>% length > 0){
    tdt = tdt[-duplicated_data_row,]
  }



  # Basic variables for type 46
  tdt2$datetime = tdt2$X4 %>% as_datetime( tz="Europe/Moscow",
                                           origin = "1970-01-01 00:00:00")
  tdt2$serv_datetime = tdt2$X1 %>% str_trunc(17,"right", ellipsis = "") %>%
    dmy_hms(  tz="Europe/Rome") %>% as.integer %>%
    as.POSIXct(,origin = "1970-01-01 00:00:00", tz="Europe/Moscow")
  tdt2$rec_num = tdt2$X2 %>% HexToDec
  tdt2$id = tdt2$X1 %>% as.character %>%
    str_trunc(8,"left",  ellipsis = "")

  # Calculating type 46 variables - spectometer
  tdt2$b_R_650 = tdt2$X16
  tdt2$b_O_600 = tdt2$X15
  tdt2$b_Y_570 = tdt2$X14
  tdt2$b_G_550 = tdt2$X13
  tdt2$b_B_500 = tdt2$X12
  tdt2$b_V_450 = tdt2$X11
  
  tdt2$b_W_860 = tdt2$X10
  tdt2$b_V_810 = tdt2$X9
  tdt2$b_U_760 = tdt2$X8
  tdt2$b_T_730 = tdt2$X7
  tdt2$b_S_680 = tdt2$X6
  tdt2$b_R_610 = tdt2$X5
  
  tdt2$b_R_650c = tdt2$b_R_650*0.7829+202.77
  tdt2$b_O_600c = tdt2$b_O_600*0.8654-328.08
  tdt2$b_Y_570c = tdt2$b_Y_570*1.0462-666.72
  tdt2$b_G_550c = tdt2$b_G_550*1.0546-842.1
  tdt2$b_B_500c = tdt2$b_B_500*0.6257-232.13
  tdt2$b_V_450c = tdt2$b_V_450*0.4562-212.62
  
  tdt2$b_W_860c = tdt2$b_W_860*0.5319+334.88
  tdt2$b_V_810c = tdt2$b_V_810*0.8414+91.58
  tdt2$b_U_760c = tdt2$b_U_760*1.4549-1012.5
  tdt2$b_T_730c = tdt2$b_T_730*1.6209-1511.2
  tdt2$b_S_680c = tdt2$b_S_680*1.5199-561.56
  tdt2$b_R_610c = tdt2$b_R_610*1.6699-312.45


  tdt2 = tdt2 %>% mutate( NDVI = (b_V_810 -  b_S_680)/(b_V_810 + b_S_680))
  tdt2 = tdt2 %>% mutate( EVI = 2.5*(b_V_810-b_S_680)/
                            (b_V_810 + 6*b_S_680 - 7.5*b_B_500 + 1))
  tdt2 = tdt2 %>% mutate( EVI2 = 2.4*(b_V_810 - b_S_680)/
                            (b_V_810 + b_S_680 + 1))
  tdt2 = tdt2 %>% mutate( VARI = (b_G_550-b_S_680)/(b_V_810+b_S_680-b_B_500))
  tdt2 = tdt2 %>% mutate( Nr = (b_V_810)/
                            (b_V_810 + b_R_610 + b_B_500 + b_G_550))
  tdt2 = tdt2 %>% mutate( Rr = (b_R_610 )/
                            (b_V_810 + b_R_610 + b_B_500 + b_G_550))
  tdt2 = tdt2 %>% mutate( Br = ( b_B_500)/
                            (b_V_810 + b_R_610 + b_B_500 + b_G_550))
  tdt2 = tdt2 %>% mutate( Gr = (b_G_550)/
                            (b_V_810 + b_R_610 + b_B_500 + b_G_550))
  tdt2 = tdt2 %>% mutate( SIPI = (b_V_810 -  b_V_450)/(b_V_810 -b_S_680))
  tdt2 = tdt2 %>% mutate( PSSR = (b_V_810 / b_R_650))
  tdt2 = tdt2 %>% mutate( PSND = (b_V_810 - b_R_650)/(b_V_810 + b_R_650))
  tdt2 = tdt2 %>% mutate( PSRI = (b_S_680 - b_B_500)/b_U_760)
  tdt2 = tdt2 %>% mutate( CARI= ((b_T_730-b_S_680)-0.2*(b_T_730-b_G_550)))
  tdt2 = tdt2 %>% mutate( MCARI= ((b_T_730-b_S_680)-0.2*(b_T_730-b_G_550))*
                            (b_T_730/b_S_680))
  tdt2 = tdt2 %>% mutate( MTCI = ((b_U_760 - b_T_730) /(b_T_730 - b_S_680)))
  tdt2 = tdt2 %>% mutate( CIg = ((b_W_860 - b_Y_570)/b_Y_570))
  tdt2 = tdt2 %>% mutate( CIr = ((b_W_860 - b_T_730)/b_T_730))
  tdt2 = tdt2 %>% mutate( CRI1 = (1/ b_B_500)-(1/b_G_550))
  tdt2 = tdt2 %>% mutate( CRI2 = (1/ b_B_500)-(1/b_T_730))
  tdt2 = tdt2 %>% mutate( PRI = (b_B_500 - b_O_600 )/(b_B_500 + b_O_600))
  tdt2 = tdt2 %>% mutate( PRI2 = (b_G_550- b_Y_570 )/(b_G_550 + b_Y_570))
  
  tdt2 = tdt2 %>% mutate( NDVIc = (b_V_810c -  b_S_680c)/(b_V_810c + b_S_680c))
  tdt2 = tdt2 %>% mutate( EVIc = 2.5*(b_V_810c-b_S_680c)/
                            (b_V_810c + 6*b_S_680c - 7.5*b_B_500c + 1))
  tdt2 = tdt2 %>% mutate( EVI2c = 2.4*(b_V_810c - b_S_680c)/
                            (b_V_810c + b_S_680c + 1))
  tdt2 = tdt2 %>% mutate( VARIc = (b_G_550c-b_S_680c)/(b_V_810c+b_S_680c-b_B_500c))
  tdt2 = tdt2 %>% mutate( Nrc = (b_V_810c)/
                            (b_V_810c + b_R_610c + b_B_500c + b_G_550c))
  tdt2 = tdt2 %>% mutate( Rrc = (b_R_610c )/
                            (b_V_810c + b_R_610c + b_B_500c + b_G_550c))
  tdt2 = tdt2 %>% mutate( Brc = ( b_B_500c)/
                            (b_V_810c + b_R_610c + b_B_500c + b_G_550c))
  tdt2 = tdt2 %>% mutate( Grc = (b_G_550c)/
                            (b_V_810c + b_R_610c + b_B_500c + b_G_550c))
  tdt2 = tdt2 %>% mutate( SIPIc = (b_V_810c -  b_V_450c)/(b_V_810c - b_S_680c))
  tdt2 = tdt2 %>% mutate( PSSRc = (b_V_810c / b_R_650c))
  tdt2 = tdt2 %>% mutate( PSNDc = (b_V_810c - b_R_650c)/(b_V_810c + b_R_650c))
  tdt2 = tdt2 %>% mutate( PSRIc = (b_S_680c - b_B_500c)/b_U_760c)
  tdt2 = tdt2 %>% mutate( CARIc = ((b_T_730c - b_S_680c)-0.2*(b_T_730c - b_G_550c)))
  tdt2 = tdt2 %>% mutate( MCARIc = ((b_T_730c - b_S_680c)-0.2*(b_T_730c - b_G_550c))*
                            (b_T_730c/b_S_680c))
  tdt2 = tdt2 %>% mutate( MTCIc = ((b_U_760c - b_T_730c) /(b_T_730c - b_S_680c)))
  tdt2 = tdt2 %>% mutate( CIgc = ((b_W_860c - b_Y_570c)/b_Y_570c))
  tdt2 = tdt2 %>% mutate( CIrc = ((b_W_860c - b_T_730c)/b_T_730c))
  tdt2 = tdt2 %>% mutate( CRI1c = (1/ b_B_500c)-(1/b_G_550c))
  tdt2 = tdt2 %>% mutate( CRI2c = (1/ b_B_500c)-(1/b_T_730c))
  tdt2 = tdt2 %>% mutate( PRIc = (b_B_500c - b_O_600c )/(b_B_500c + b_O_600c))
  tdt2 = tdt2 %>% mutate( PRI2c = (b_G_550c - b_Y_570c )/(b_G_550c + b_Y_570c))

  # To align type 45 and 46 producing correct unique index variable
  tdt   = tdt  %>% mutate(id_rec = paste(id,rec_num,sep=""))
  tdt2  = tdt2 %>% mutate(id_rec = paste(id,rec_num-1,sep=""))
  
  # Uniting rows of type 45 and 46
  tdt_a = left_join(tdt,tdt2, by="id_rec", copy=F)
  
  
  names(tdt_a)[names(tdt_a) == "datetime.x"] = "datetime"
  names(tdt_a)[names(tdt_a) == "rec_num.x"] = "rec_num"
  names(tdt_a)[names(tdt_a) == "serv_datetime.x"] = "serv_datetime"
  names(tdt_a)[names(tdt_a) == "id.x"] = "id"
  
  
  #Recalculating bands and indexes for type 40 since it has only 4 bands
  if(any(tdt_a$type == 40)){
    tdt_40 = tdt_a %>% filter(type == 40)
    tdt_40$b_R_650 = tdt_40$X24.x
    tdt_40$b_G_550 = tdt_40$X23.x
    tdt_40$b_B_500 = tdt_40$X22.x
    tdt_40$b_V_810 = tdt_40$X21.x
    tdt_40  = tdt_40 %>% mutate( NDVI = (b_V_810 -  b_R_650)/
                                   (b_V_810 + b_R_650) )
    tdt_40  = tdt_40 %>% mutate( EVI = 2.5*(b_V_810-b_R_650)/
                                   (b_V_810+6*b_R_650-7.5*b_B_500+1))
    tdt_40  = tdt_40 %>% mutate( VARI = (b_G_550-b_R_650)/
                                   (b_V_810+b_R_650-b_B_500))
    tdt_40  = tdt_40 %>% mutate( Nr = (b_V_810)/
                                   (b_V_810+b_R_650+b_B_500+b_G_550))
    tdt_40  = tdt_40 %>% mutate( Rr = (b_R_650 )/
                                   (b_V_810+b_R_650+b_B_500+b_G_550))
    tdt_40  = tdt_40 %>% mutate( Br = ( b_B_500)/
                                   (b_V_810+b_R_650+b_B_500+b_G_550))
    tdt_40  = tdt_40 %>% mutate( Gr = ( b_G_550)/
                                   (b_V_810+b_R_610+b_B_500+b_G_550))
    tdt_40  = tdt_40 %>% mutate( SIPI = (b_V_810 -  b_B_500)/
                                   (b_V_810 -b_R_650) )
    tdt_40  = tdt_40 %>% mutate( PSSR = (b_V_810 / b_R_650) )
    tdt_40  = tdt_40 %>% mutate( PSND = (b_V_810 - b_R_650)/
                                   (b_V_810 + b_R_650) )
    tdt_40  = tdt_40 %>% mutate( PSRI = (b_R_650 - b_B_500)/b_V_810)
    tdt_40  = tdt_40 %>% mutate( CARI = ((b_V_810-b_R_650)-
                                           0.2*(b_V_810-b_G_550)))
    tdt_40  = tdt_40 %>% mutate( MCARI = ((b_V_810-b_R_650)-
                                            0.2*(b_V_810-b_G_550))
                                 *(b_V_810/b_R_650))
    tdt_40  = tdt_40 %>% mutate( CRI1 = (1/ b_B_500)-(1/b_G_550))
    tdt_40  = tdt_40 %>% mutate( CRI2 = (1/ b_B_500)-(1/b_V_810))
    #tdt_a[tdt_a$type==40,] = tdt_40
  }
  #Removing unconverted variables
  tt_data = tdt_a %>% select(.,-ends_with(".x")) %>% select(., -ends_with(".y"))

  return(tt_data)
}


#Example input
#server = c("http://naturetalkers.altervista.org/C18A0031/ttcloud.txt",
#"http://naturetalkers.altervista.org/C18A0025/ttcloud.txt")
#installation_start = 1556845000
#import_folder_name = "./RUDN"


#Calculating charges============================================================
ischarged= function(data){
  if(length(data$volt)>2){
    charged = c(F,(data$volt[2:length(data$volt)] -
                     data$volt[1:(length(data$volt)-1)]) > 0.5)
  } else {
    charged = F
  }
  charged
}


#Dates correction and extrapolation - new, not used=============================
extrapolate_dates = function(data, timestep){
  #print(data)
  data$time = data$datetime
  if(any(data$cor_dt))
    if(length(data$cor_dt %>% na.omit)>0){
      if(any(data$cor_dt) & length(data$cor_dt)>1 ){
        start = which(data$cor_dt)[1]
        if(start > 1){
          #print("case 1")
          data$time = data$datetime[start]
          plus_time = (1:(length(data$cor_dt)-start-1))*timestep
          minus_time = (1:(start-1))*(-timestep)
          data$time = data$time[(start+1):length(data$time)]+plus_time
          data$time = data$time[1:(start-1)]+minus_time
        }else{
          #print("case 2")
          data$time = data$datetime[start]
          plus_time = (1:(length(data$time)-start))*timestep
          data$time[(start+1):length(data$time)] =
            data$time[(start+1):length(data$time)]+plus_time
          #print("error?")
        }
      } else {
        if(any(data$serv_cor) & length(data$cor_dt)>1){
          start = which(data$serv_cor)[1]
          if(start > 1){
            #print("case 4")
            plus_time = (1:(length(data$cor_dt)-start)*timestep)
            minus_time = (1:(start-1))*(-timestep)
            data$time = data$datetime[start]
            data$time = data$time[(start+1):length(data$time)]+plus_time
            data$time = data$time[1:(start-1)]+minus_time
          }else{
            #print("case 5")
            plus_time = (1:(length(data$cor_dt)-start)*timestep)
            data$time = data$serv_datetime[start]
            data$time[(start+1):length(data$time)] =
              data$time[(start+1):length(data$time)]+plus_time
          }
        }
      }
    }
  return(data)
}

#Flagging continious correct server time========================================
mark_continious_serv_time = function(data){
  #is.null(data) %>% print
  wst = which(!data$wrong_server)
  if (length(wst)>2){
    wst_cont = (wst[2:length(wst)]-wst[1:(length(wst)-1)]<2)[-1] &
      (wst[2:(length(wst)-1)]-wst[3:(length(wst))]>-2)
    data$wrong_server = T
    data$wrong_server[wst[c(F,wst_cont,F)]] = F
  }
  data$wrong_server = T
  return(data)
}

#Check measure pediod (step time) per one tt per charge=========================
check_measure_period = function(temp_data, verboseFlag){
  real_time = which(!temp_data$time%>%is.na)
  if(real_time%>% length >1){
    mt = temp_data$time[real_time]
    rti = real_time
    model = lm(mt~rti)
    step_time = plyr::round_any(summary(model)[[4]][2,1], 100)
    fun_log(verboseFlag = verboseFlag, c("Estimated step time is ",step_time))
    if(step_time < 4000) {step_time=3600} 
    if(step_time > 3999) {step_time=5400}
    fun_log(verboseFlag = verboseFlag, c(" will be used step time ", step_time, "\n"))
    
  } else {
    step_time = 3600
  }
  return(step_time)
}
#Time correction - good old=====================================================
extrapolate_tt_date = function(tt_one, verboseFlag){
  fun_log(verboseFlag = verboseFlag, c("Starting data extrapolation \n"))
  if(tt_one %>% nrow <2 ){
    #print(tt_one)
  }
  if(tt_one$volt %>% length > 2){
    fun_log(verboseFlag = verboseFlag, c("Starting time correction for ", tt_one$id %>% unique,"\n"))
    bat_grow = c(F,
                 tt_one$volt[2:(nrow(tt_one))] - tt_one$volt[1:(nrow(tt_one)-1)] > 0.3)
    fun_log(verboseFlag = verboseFlag, c("Found ",which(bat_grow) %>% length,"recharges","\n"))

    tt_one$charge = cumsum(bat_grow)+1
    fun_log(verboseFlag = verboseFlag, c("So, charges states are", tt_one$charge %>% unique(),"\n"))

    tt_one$time = NA
    tt_one$corr_time = 1
    foreach (ci = tt_one$charge %>% unique) %dopar% {

      #temp_data = tt_one[tt_one$charge == ci,]
      temp_data = tt_one %>% filter(charge == ci)
      temp_data$time[!temp_data$wrong_time] =
        temp_data$datetime[!temp_data$wrong_time]

      fun_log(verboseFlag = verboseFlag, c("Correct dates ",which(!temp_data$wrong_time) %>% length,
          "correct times ",which(!is.na(temp_data$time)) %>% length,"\n"))
      temp_data$corr_time[!temp_data$wrong_time] = 2

      check_serv_time_too_high_according_it_record = function(x, dt){

        # Check if record number of record with current serv_time is
        #  bigger of any record number of measurements with correct unix time
        #  which is less than current serv_time

        # Index of elemnt in vector of correct unix time whcih are
        # less than current serv_time
        corr_unix_time_index = which(dt$serv_datetime[x] >
                                       dt$datetime[!dt$wrong_time])
        #Correct unix time which are less than current serv_time
        corr_unix_time = dt$datetime[!dt$wrong_time[corr_unix_time_index]]
        # Indexes of corr_unix_time elemnts in whole datetime variable
        datetime_index = which(dt$datetime %in% corr_unix_time)
        # If any record numbers of measurement with correct unix time, which
        # is less than the current serv_time, is bigger of record number of
        # current serv_time than this serv_time is INCORRECT
        return (any(dt$rec_num[datetime_index] > dt$rec_num[x]))
      }

      #if (any(!temp_data$wrong_server)){
      #  cor_serv_records = which(!temp_data$wrong_server)
      #  wrong_serv_records = cor_serv_records[cor_serv_records %>%
      #                                          map_lgl(check_serv_time_too_high_according_it_record, dt = tt_one)]
      #  temp_data$wrong_server[wrong_serv_records] = T
      #}


      if(length(temp_data$serv_datetime[temp_data$wrong_server == F])>0){
        # if there is any correct date in charge period don't use serv_date
        # but for the case when datetime is wrong use correct server time
        if (!any(!temp_data$time%>%is.na)) {
          only_server_time_ok = temp_data$wrong_time ==
            T & temp_data$wrong_server == F
          temp_data$time[only_server_time_ok] =
            temp_data$serv_datetime[only_server_time_ok]
          temp_data$corr_time[only_server_time_ok] = 3
        }

      }
      temp_data$corr_time[temp_data$datetime <
                            157680000 & temp_data$time %>% is.na] = 4
      temp_data$lost_connect = F
      temp_data$lost_connect[temp_data$datetime <
                               157680000 & temp_data$time %>% is.na] = T



      real_time = which(!is.na(temp_data$time))
      step_time = check_measure_period(temp_data, verboseFlag)
      tt_one$step_time = step_time
      fun_log(verboseFlag = verboseFlag, c("Charge:",ci," step time:",step_time,
          ", correct dates found",real_time %>% length,"\n"))
      #If there are any correct time inside this data
      if (real_time %>% length > 1){

        #If last elemnt do not have correct time - set it as correct
        if (real_time[length(real_time)] != length(temp_data$time)) {
          real_time = c(real_time,length(temp_data$time))
        }

        #Extrapolating back from the first elemet with correct time
        foreach( i = (real_time[1]-1):1) %dopar% {
          temp_data$time[i] = temp_data$time[i+1]-step_time
        }

        fun_log(verboseFlag = verboseFlag, c("Found measurements with correct timestemp:",
            length(real_time)-1, "\n"))
        for( i in 1:(length(real_time)-1))  {
          if(real_time[i+1] == real_time[i]+1){
            next(i)
          }
          foreach( j = real_time[i]:(real_time[i+1]-2)) %dopar% {
            temp_data$time[j+1] = temp_data$time[j]+step_time
          }
        }
        tt_one = tt_one %>% as.data.frame()
        temp_data = temp_data %>% as.data.frame()
        tt_one[tt_one$charge == ci,] = temp_data
        fun_log(verboseFlag = verboseFlag, c(tt_one$id %>% unique(), " charge ", ci,"was filled","\n"))


      }

    }


    tt_one$time = tt_one$time %>% as.POSIXct(origin="1970-01-01 00:00:00",
                                             tz="Europe/Moscow") }
  else {
    tt_one$charge=1
    tt_one$time = NA
    
  }
  return(tt_one)
}


correct_extrap_date = function(tt_one, verboseFlag){
  fun_log(verboseFlag = verboseFlag, c("TT ", tt_one$id %>% unique, " fixing wrong corrections","\n"))
  #Check if date goes back in time with with growing row nunber
  ends = nrow(tt_one)
  bad_extrap_index =c(F,(tt_one$time[2:ends] - tt_one$time[1:(ends-1)])<0)
  # If tt data is very small in size previous step cana generate 
  # bad_extrap_index longer than the tt data itself, so NA produced
  # lets remove them
  bad_extrap_index = na.exclude(bad_extrap_index)
  real_bad_extrap_index = which(bad_extrap_index)
  foreach (bi = which(bad_extrap_index)) %dopar% {
    before_problem = bi - 1
    time_before_drop = tt_one$time[before_problem]
    real_problem = which(c(rep(F, bi),tt_one$time[bi:nrow(tt_one)] < time_before_drop))#wrong
    real_bad_extrap_index = c(real_bad_extrap_index,real_problem) 
  }
  bad_extrap_index = rep(F,length(tt_one$time))
  bad_extrap_index[real_bad_extrap_index] = T
  bad_extrap_index = bad_extrap_index[1:nrow(tt_one)] 
  #Set this times and datetimes to 0
  fun_log(verboseFlag = verboseFlag, c("Found ",which(bad_extrap_index)%>% length," bad extrapolation, fixing","\n" ))
  #print(bad_extrap_index)
  tt_one$time[bad_extrap_index]         = NA
  tt_one$wrong_time[bad_extrap_index]   = TRUE
  tt_one$wrong_server[bad_extrap_index] = TRUE
  #tt_one$datetime[bad_extrap_index] = as.POSIXct(0, origin="1970-01-01 00:00:00", tz="Europe/Moscow")
  #tt_one$serv_datetime[bad_extrap_index] = as.POSIXct(0, origin="1970-01-01 00:00:00", tz="Europe/Moscow")
  fun_log(verboseFlag = verboseFlag, c("Removed incorrect time indexes","\n"))
  #Restart extrapolation
  for (ci in tt_one$charge %>% unique){
    temp_data = tt_one %>% filter(charge == ci)
    if(!any(is.na(temp_data$time))) {
     fun_log(verboseFlag = verboseFlag, c("Nothing to correct in ",ci," charge, skipping","\n"))
     next(ci)
    }
    real_time = which(!is.na(temp_data$time))
    step_time = temp_data$step_time[1]
    fun_log(verboseFlag = verboseFlag, c("Charge:",ci," step time:",step_time,
         ", correct dates found",real_time %>% length,"\n"))
    #If there are any correct time inside this data
    if (real_time %>% length > 1){
       #If last elemnt do not have correct time - set it as correct
       if (real_time[length(real_time)] != length(temp_data$time)) {
         real_time = c(real_time,length(temp_data$time))
       }
       if (real_time[1]>2){
         #Extrapolating back from the first elemet with correct time
         fun_log(verboseFlag = verboseFlag, c("Filling backward\n"))
         for( i in (real_time[1]-1):1) {
           temp_data$time[i] = temp_data$time[i+1]-step_time
         }
       }
       fun_log(verboseFlag = verboseFlag, c("Found measurements with correct timestamp:",
           length(real_time)-1, "\n"))
      for( i in 1:(length(real_time)-1)) {
        if(real_time[i+1] == real_time[i]+1){
         next(i)
        }
        foreach( j = real_time[i]:(real_time[i+1]-2)) %dopar% {
         temp_data$time[j+1] = temp_data$time[j]+step_time
        }
      }
       
      tt_one[tt_one$charge == ci,] = temp_data
      fun_log(verboseFlag = verboseFlag, c(tt_one$id %>% unique(), " charge ", ci,"was filled","\n"))
     }
  }
  tt_one$time = tt_one$time %>% as.POSIXct(origin="1970-01-01 00:00:00",tz="Europe/Moscow")  
  return(tt_one)
}

correct_time_ts_shift_matching = function(data, verboseFlag){
    #data = tt_data_ec
    fun_log(verboseFlag = verboseFlag, c("Starting correct_time_ts_shift_matching \n"))
    abs.max.ccf <- function(a,b) {
      d <- ccf(a, b, plot=FALSE, lag.max=length(a)-5)
      cor <- d$acf[,,1]
      abscor <- abs(d$acf[,,1])
      lag <- d$lag[,,1]
      abs.cor.max <- abscor[which.max(abscor)]
      abs.cor.max.lag <- lag[which.max(abscor)]
      return(c( abs.cor.max, abs.cor.max.lag))
    }
    
    lag_table = data.frame()
    foreach(icharge = 1:6) %dopar% {
      for(iid in (data$id %>% unique)) {
        ttsi = data[data$id == iid & data$charge == icharge,]
        if(nrow(ttsi)<64){ next()}
        
        
          for(jid in (data$id %>% unique)) {
            if(jid == iid){next()}
            ttsj = data %>% filter(id == jid, charge == icharge)
            print(nrow(ttsj))
            if(nrow(ttsj)<64){ next() }
            print(all(is.na(ttsj$time)))
            if(all(is.na(ttsj$time))) { next()}
            cor_time = which(!is.na(ttsj$time)) %>% length
            tsi = ts(ttsi$tair, start=1, end = nrow(ttsi), frequency = 1)
            tsj = ts(ttsj$tair, start=1, end = nrow(ttsj), frequency = 1)
            lag = abs.max.ccf(tsi,tsj)
            lag_row = data.frame(iid, jid, icharge, cor=lag[1], lag = lag[2], cor_time)
            print("lag_row")
            print(lag_row)
            lag_table = rbind(lag_table, lag_row)
            print(names(lag_table))
          }

        }
    }
    print("end")
    print(lag_table)
    result_table = lag_table %>% group_by(iid, icharge) %>%  summarise(
      MaxCor = max(cor), jid = jid[which.max(cor)], lag = lag[which.max(cor)], 
      cor_time = cor_time[which.max(cor)]
    ) %>% as.data.frame()
    
    
    datac = data
     foreach(i = 1:nrow(result_table)) %do% {
       iid = result_table$iid[i]
       jid = result_table$jid[i]
       lag = result_table$lag[i]
       icharge = result_table$icharge[i]
       if(all(is.na(
         datac%>%filter(id == iid, charge == icharge) %>% select(time)))){
         
         subj = datac%>%filter(id == jid & charge == icharge)
         subi = datac%>%filter(id == iid & charge == icharge)
         time_index = match(subi$rn,subj$rn+lag)
         subi$time = subj$time[time_index]
         datac[datac$id == iid & datac$charge == icharge,] = subi
       }
       
     }
     fun_log(verboseFlag = verboseFlag, c("Starting  correct_extrap_date inside correct_time_ts_shift_matching \n"))
    dataec = datac  %>% group_by(id) %>%
      do(correct_extrap_date(., verboseFlag)) %>% as.data.frame
    
    fun_log(verboseFlag = verboseFlag, c("Stopped correct_time_ts_shift_matching \n"))
    return(dataec)

# ggplot()+
#   geom_point(data = filter(data, id=="218A0060",  charge == 1), aes(x= rn, y=tair), color = 1)+
#   geom_point(data = filter(data, id=="218A0178",  charge == 1), aes(x= rn-23, y=tair), color = 2)
# ggplot(data = dataec)+
#   geom_point(aes(x=rn, y=tair, color=charge,shape=is.na(time)),size=.1)+
#   facet_wrap(~id)
}




#Calculating everything for one site - returns two tables=======================
TTcalc_site = function(server,
                       installation_start, 
                       import_folder_name,
                       first_imported_dates_reconstructed,
                       descr_file,
                       sitename, 
                       verboseFlag){
  # Example of possible intput
  # server = c("http://naturetalkers.altervista.org/C18A0031/ttcloud.txt",
  #               "http://naturetalkers.altervista.org/C18A0025/ttcloud.txt")
  # installation_start = 1556845000
  # import_folder_name="RUDN"
  # first_imported_dates_reconstructed = F
  # descr_file = "rudn_desc.csv"      
  

  temp_serv_dataset = data.frame()
  for (serv_url in server) {
    i = which(server %in% serv_url)
    dest_file_name = paste("db_sync/dbsync-",sitename,"-",i,"-",Sys.Date(),".txt", sep="")
    
    if(!any(str_c("db_sync/",dir("db_sync/")) == dest_file_name) ){
      fun_log(verboseFlag = verboseFlag, c("No sync today - downloading file \n"))
      download.file(url = serv_url, destfile = dest_file_name, method = "curl",mode="character")
    } else {
      fun_log(verboseFlag = verboseFlag, c("Sync was done today, no download \n"))
    }
    temp_read = suppressWarnings(suppressMessages(
      read_delim(dest_file_name, col_names = FALSE, comment = "#",delim=";", col_types = "ccdddddddddddddddddd")))
      
    temp_read$s = i
    temp_serv_dataset = rbind( temp_serv_dataset,temp_read)
    fun_log(verboseFlag = verboseFlag, c(as.character(serv_url), "data read.","\n"))
  }
  
  tt_data = TTBasicCalc(temp_serv_dataset, verboseFlag)
  #tt_basic_data = lazy_dt(tt_data)
  #If there are duplicates - remove
  
  tt_data = tt_data[
            -(tt_data[,c("volt","dist","pulses","Hz","rh","tair",
               "VPD","gx","gx2","gy","gy2")] 
              %>% duplicated 
              %>% which), ]
  #Removing rows with corrupted id
  corrupted_id = unique(tt_data$id)[str_which(unique(tt_data$id),":")]
  if(corrupted_id %>% length() > 0){
    fun_log(verboseFlag = verboseFlag, c("Found corrupted id: ", corrupted_id, " removing \n"))
    tt_data = tt_data %>% filter(!(id %in% corrupted_id))
  }
  
  #If data for one TT came from different clouds, we will try to give it some
  #order according to appearance on server since row order and record numbers
  #on different clouds will be different.
  tt_data = tt_data %>% group_by(id) %>%
    arrange(serv_datetime) %>% as.data.frame
  #Adding flagging variable showing that this data was obtained from server
  tt_data$imported = F
  # TT sends record to cloud one by one, and clouds do this in same way,
  # so on server records should appear in correct order.Between them could be
  # gaps, but we can detect them by battery charge discontinuties
  #tt_data = tt_data %>% group_by(id) %>% mutate(nrec_num = 1:length(rec_num))

  fun_log(verboseFlag = verboseFlag, c("Checking if there are some back up data data? \n"))
  if(!is.null(import_folder_name)){
    fun_log(verboseFlag = verboseFlag, c("Yep, there are some \n"))
      data_bc = data.frame()
      import_folder_name = paste("",import_folder_name,sep="")
      
      
      for(file in list.files(import_folder_name,pattern="TT*")){
        temp = suppressWarnings(suppressMessages(
          read_delim(paste(import_folder_name,file, sep="/"),
                     col_names = FALSE, delim=";", skip=0)
        ))
        fun_log(verboseFlag = verboseFlag, c("Dimmension of the file", file,":",dim(temp)[1]," - rows, ",
            dim(temp)[2]," - columns.","\n"))
        data_bc = rbind(data_bc, temp)
        fun_log(verboseFlag = verboseFlag, c("Binded array size", dim(data_bc)))
      }
      fun_log(verboseFlag = verboseFlag, c("Binded array size", dim(data_bc)))
      first_column = paste("10.05.20 12:55:19",data_bc$X1, sep=",")
      #imported data have no server time,so we should add some
      data_bc$X1 = first_column 
      #source variable will be created from that
      data_bc$s=i+1
      #print(as.data.frame(data_bc))
      tt_bc = TTBasicCalc(data_bc , verboseFlag = verboseFlag)
    
      timestep = 3600
    
      #Adding flagging variable showing that this data was obtained from impport
      tt_bc$imported = T
      fun_log(verboseFlag = verboseFlag, c("Basic calculated array size", dim(tt_bc),"\n"))
      #Inserting exported(backuped) from TT data into server data,
      #Very slow and stupid way, but there were no time for elegance
      #
      tt_imported = data.frame()
      # logging cases into file or memory
      log_imp = data.frame(id = "first", starts = NA, ends = NA, 
                           var = 0, tt = 0,bc = 0,stringsAsFactors = FALSE)
      
      print(names(tt_bc))
      for (ids in unique(tt_data$id)){
        bc = tt_bc %>% filter(id == ids) %>% as.data.frame
        tt = tt_data %>% filter(id == ids) %>% as.data.frame
        fun_log(verboseFlag = verboseFlag, c("Data from the TT ", ids," imported has ",
            dim(bc)[1]," - rows, ",dim(bc)[2]," - columns.","\n" ))
        fun_log(verboseFlag = verboseFlag, c("Data from the TT ", ids," on server has ",
            dim(tt)[1]," - rows, ",dim(tt)[2]," - columns.","\n" ))
        fun_log(verboseFlag = verboseFlag, c("Starting import of directly extracted from TT data","\n"))
        u = tt
        #if(dim(tt_imported)[1]>0){
        if(length(tt$volt)>0){
          #print(ids)
          
          badnames = c("datetime","wrong_time","id","type","incr","rec_num",
                       "serv_datetime","dist","source","id_rec",
                       "b_O_600","b_Y_570","b_W_860","b_V_810","b_U_760","b_R_610",
                       "NDVI","EVI","EVI2","Nr","Rr","Br","Gr","PSSR","PSND",
                       "MTCI","PRI","b_R_650","b_G_550","b_B_500","b_V_450",
                       "b_T_730","b_S_680","VARI","SIPI","PSRI","CARI","MCARI",
                       "CIg","CIr","CRI1","CRI2","imported")
          names_for_compare = names(tt)[!(names(tt) %in% badnames)]
          frank=data.frame()
          logs=data.frame()

          if(dim(bc)[1]>10){ #if imported data from tt is to small in most cases it's crappy and better just to skip it
              matches = match(do.call("paste",tt[, names_for_compare]),
                              do.call("paste", bc[, names_for_compare]))
              msize = length(matches)
              edges  = c((is.na(matches[2:msize]) - is.na(matches[1:(msize-1)])),0)
              
              if(any(edges == -1) & any(edges == 1) ) {
                starts = which(edges == -1)
                ends = which(edges == 1)
                subset1 = 1:starts
                subset2 = ends:msize
                frank = rbind(tt[subset1,], bc, tt[subset2,])
                #logs = data.frame(id = as.character(ids), starts =  starts, ends = ends, 
                #                  var = 1, tt = dim(tt)[1],bc = dim(bc)[1],stringsAsFactors = FALSE)
                
                fun_log(verboseFlag = verboseFlag, c("Case 1 intersect, start is ", starts," end is ",ends,"bc size ",dim(bc)[1]," tt size ",dim(tt)[1],"\n"))
              } else {
                if(any(edges == -1)){
                  starts = which(edges==-1)
                  frank = rbind(tt[1:starts,], bc)
                  #logs = data.frame(id = as.character(ids), starts = starts, ends = NA, 
                  #                  var = 2, tt = dim(tt)[1],bc = dim(bc)[1],stringsAsFactors = FALSE)
                  fun_log(verboseFlag = verboseFlag, c("Case 2 add to end, start is ", starts,"bc size ",dim(bc)[1]," tt size ",dim(tt)[1],"\n"))
                }
                if(any(edges == 1)){
                  ends = which(edges==1)
                  frank = rbind( bc, tt[ends:msize,])
                  #logs = data.frame(id = as.character(ids), starts = NA, ends = ends, 
                  #                 var = 3, tt = dim(tt)[1],bc = dim(bc)[1],stringsAsFactors = FALSE)
                  fun_log(verboseFlag = verboseFlag, c("Case 3 add to begin, end is ",ends,"bc size ",dim(bc)[1]," tt size ",dim(tt)[1],"\n"))
                  
                  }    
              } 
              
            } else {
              frank = tt
              #logs = data.frame(id = as.character(ids), starts = NA, ends = NA, 
              #                  var = 0, tt = dim(tt)[1],bc = dim(bc)[1], stringsAsFactors = FALSE)
              fun_log(verboseFlag = verboseFlag, c("Case 0 no backup","\n"))
            }
            u=frank
            #matches %>% print
            #fun_log(verboseFlag = verboseFlag, c("Found ", matches %>% length,
            #    " matches between importing and server data for TT ", ids,"\n")
            
    
        } else {
          u = bc
          #logs = c(id = ids, starts = NA, ends = NA, var = 4)
          fun_log(verboseFlag = verboseFlag, c("Case 4, only backup result is ", dim(u)[1]))
        }
        tt_imported = rbind(tt_imported,u)
        #log_imp = rbind(log_imp,logs)
      
    } # end of cycling through ids instruction
  # if no import was done just take tt_data  
  #print(log_imp)
  } else {
    fun_log(verboseFlag = verboseFlag, c("Nope, nothing to import \n"))
    tt_imported = tt_data
  }
  tt_imported = tt_imported %>% group_by(id) %>% mutate(rn = 1:length(volt))

  #DONE Add flagging variable to mark source of data - imported from backup or
  #got directly from server
  #TODO Check what is wrong with Timiryazev
  #TODO Add to time extrapolation option to understand measurement time

  ########NOT USING NOW
  #Esoterical way to flag data before and after reseting -
  #if rec_num falls more than 1000 records - it means that this TT was reseted
  #(its magic because cloud was not reseted, but record numbers in
  # cloud still drops)
  #tt_data$before_reset = c(T, cumsum(tt_data$rec_num[2:length(tt_data$rec_num)]
  #-tt_data$rec_num[1:(length(tt_data$rec_num)-1)] < -1000) < 1 )

  #Finding last record number before reset and constructing new variable with
  #continuous record number
  #d = tt_data %>% filter(before_reset == T) %>% group_by(id)
  #%>% summarise (max = max(rec_num))
  #tt_data = left_join(tt_data,d, by="id")
  #tt_data = tt_data %>% group_by(id) %>%
  #mutate(nrec_num = if_else(before_reset, rec_num, rec_num+max))

  #Function to detect recharge of battery -  find rise on more than 0.5 volts


  tt_imported = as.data.frame(tt_imported)
  #print(head(tt_imported))
  # Marking server time wrong if there are more than one measurement per hour
  tt_imported$years = year(tt_imported$serv_datetime)
  tt_imported$doys  = yday(tt_imported$serv_datetime)
  tt_imported$hours = hour(tt_imported$serv_datetime)
  #tt_imported = lazy_dt(tt_imported)
  tt_imported  = tt_imported  %>% group_by(id,years,doys, hours) %>%
    mutate(wrong_server = length(doys)>1) %>%  as.data.frame
  #Marking recharge
  #tt_imported = lazy_dt(tt_imported)
  tt_imported = tt_imported %>% group_by(id) %>% arrange(rn) %>%
    mutate(recharge_flag =ischarged(.data))
  #Calculating charge cycles
  tt_imported = tt_imported %>% group_by(id) %>% arrange(rn) %>%
    mutate(charge = cumsum(recharge_flag))
  #Simple detect of clearly wrong datetimes
  tt_imported = tt_imported %>% group_by(id) %>% arrange(rn) %>%
    mutate(cor_dt = (datetime > min(serv_datetime)) & (datetime <
                                                         max(serv_datetime)))
  #Detecting correct server time - first assumption -
  #it should one measurement per hour
  tt_imported = tt_imported %>% group_by(id, years, doys,hours) %>%
    mutate(serv_cor = length(serv_datetime) < 2)
  timestep = 3600
  #tt_imported = tt_imported %>% group_by(id, charge) %>%
  #  do(extrapolate_dates(., timestep))%>% as.data.frame


  tt_imported = tt_imported %>% group_by(id) %>%
    do(mark_continious_serv_time(.)) %>% as.data.frame
  #tt_imported = lazy_dt(tt_imported)
  tt_imported = tt_imported %>% group_by(id) %>%
    do(mark_continious_serv_time(.)) %>% as.data.frame


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
  

  
  tt_imported = tt_imported  %>% filter(id %in% SITE_list$id)
  if(tt_imported %>% nrow <1) {
    fun_log(verboseFlag = verboseFlag, c(
      "Looks like you have error in site name.\n"
    ))
  }
  fun_log(verboseFlag = verboseFlag, c("Starting extrapolation of dates \n"))
  tt_data_e = tt_imported  %>% group_by(id) %>%
    do(extrapolate_tt_date(., verboseFlag)) %>% as.data.frame
  
  
  fun_log(verboseFlag = verboseFlag, c("Starting correction of extrapolated dates \n"))
  tt_data_ec = tt_data_e  %>% group_by(id) %>%
    do(correct_extrap_date(., verboseFlag)) %>% as.data.frame
  
  
  fun_log(verboseFlag = verboseFlag, c("Starting correct_time_ts_shift_matching of extrapolated dates \n"))
  #tt_data_ec = correct_time_ts_shift_matching(tt_data_ec, verboseFlag)
  
  #tt_data_e  = tt_imported
  tt_data_e  = tt_data_ec %>% select(-c(years,doys,hours))
  tt_data_e$year = year(tt_data_e$time)
  tt_data_e$week = week(tt_data_e$time)
  tt_data_e$doy = yday(tt_data_e$time)
  tt_data_e$hour = hour(tt_data_e$time)
  tt_data_e$min = minute(tt_data_e$time)

  tt_data_e = tt_data_e %>% group_by(id,year,doy) %>%
    mutate(dT = nt2 - t2, na.rm = T) %>%
    mutate(dTa = nt2 - t2 - nt1 + t1, na.rm = T) %>%
    mutate(dTm = max(dT, na.rm=T)) %>%
    mutate(dTam = max(dTa, na.rm=T)) %>%
    mutate(maxTd = max(dist), na.rm = T) %>%
    mutate(meanTd = mean(dist), na.rm = T) %>%
    mutate(minTd = min(dist), na.rm = T) %>%
    mutate(u = 119*(10^-3)*(dTm/dT - 1)^1.231, na.rm = T) #l m-2 s-1


  SITEdata = left_join(tt_data_e,SITE_list,by="id")
  SITEdata = SITEdata %>% mutate(diam = DBH / pi)
  SITEdata = SITEdata %>%
    mutate(Flux = u*3600*(diam^1.8777)*0.755/10000, na.rm = T)
  
  #SITEdata = BEFadd(SITEdata, verboseFlag)
  #Spectrometer calibration
  SITEdata  = TTR_add(SITEdata, verboseFlag)



  if(verboseFlag =="mem"){
    return(list(tt_imported, SITEdata,message_log))
  }else {
    return(list(tt_imported, SITEdata))
  }
  
  
  

}

#Adding Biomass calculation data ==============================================
# Biomass calculated based on IPCC 2006 formula C = [V * D * BEF] * (1 + R) * CF
# BCEF = BEF * D is taken from paper  doi:10.3390/f9060312 Dmitry Schepaschenko
# Improved Estimates of Biomass Expansion Factors for Russian Forests
# Big table of data from this paper is used in this function

BEFadd = function(data, verboseFlag){
  fun_log(verboseFlag = verboseFlag, c("Adding BEF data for biomass growth calculation \n"))
  data = AllData
  BEFdata = read_delim("data/BEF.csv", delim = ";")
  data = data %>% mutate(genus = str_split(Species, " ", simplify = T)[,1])
  data = data %>% mutate(age_group_indexes = recode(age_group_index, V = "IV", VI = "IV"))
  data = data %>% mutate(Genum = recode(genus, Fraxinus = "Other hard deciduous", Acer = "Other hard deciduous",
                                        Salix = "Other soft deciduous", Tilia = "Other soft deciduous"))
  data = left_join(data, BEFdata, by =c("Genum","zone","age_group_indexes")) 
  data = data %>% select(-genus,-age_group_indexes )
  
  return(data)  
  #Other hard deciduous Fraxinus, Acer
  #Other soft deciduous Salix, Tilia
}





#Adding TTR connected variables=================================================
#IMPORTANT - we are assuming that one site is a group of TTs installed on trees
#which could be assumed to be in a same conditions and that there is one TTR per site
TTR_add = function(data, verboseFlag){
  
  fun_log(verboseFlag = verboseFlag, c("Starting site TTR data calculation \n"))
  
  if(any(data$Species == "TTR")){
    names(data)[names(data) == "id.x"] = "id"
    
   
    
    data$b_R_650c[data$b_R_650c < 0] = 0
    data$b_O_600c[data$b_O_600c < 0] = 0
    data$b_Y_570c[data$b_Y_570c < 0] = 0
    data$b_G_550c[data$b_G_550c < 0] = 0
    data$b_B_500c[data$b_B_500c < 0] = 0
    data$b_V_450c[data$b_V_450c < 0] = 0
    data$b_W_860c[data$b_W_860c < 0] = 0
    data$b_V_810c[data$b_V_810c < 0] = 0
    data$b_U_760c[data$b_U_760c < 0] = 0
    data$b_T_730c[data$b_T_730c < 0] = 0
    data$b_S_680c[data$b_S_680c < 0] = 0
    data$b_R_610c[data$b_R_610c < 0] = 0
    
    
    TTRdatasum  = data %>%filter(Species == "TTR") %>% group_by(Site,doy,hour) %>% summarise( 
      TTair = mean(tair,na.rm = T),
      TTrh  = mean(rh,  na.rm = T),
      TTR_650c = mean(b_R_650c,na.rm = T),
      TTR_600c = mean(b_O_600c,na.rm = T),
      TTR_570c = mean(b_Y_570c,na.rm = T),
      TTR_550c = mean(b_G_550c,na.rm = T),
      TTR_500c = mean(b_B_500c,na.rm = T),
      TTR_450c = mean(b_V_450c,na.rm = T),
      TTR_860c = mean(b_W_860c,na.rm = T),
      TTR_810c = mean(b_V_810c,na.rm = T),
      TTR_760c = mean(b_U_760c,na.rm = T),
      TTR_730c = mean(b_T_730c,na.rm = T),
      TTR_680c = mean(b_S_680c,na.rm = T),
      TTR_610c = mean(b_R_610c,na.rm = T),
      TTR_650 = mean(b_R_650,na.rm = T),
      TTR_600 = mean(b_O_600,na.rm = T),
      TTR_570 = mean(b_Y_570,na.rm = T),
      TTR_550 = mean(b_G_550,na.rm = T),
      TTR_500 = mean(b_B_500,na.rm = T),
      TTR_450 = mean(b_V_450,na.rm = T),
      TTR_860 = mean(b_W_860,na.rm = T),
      TTR_810 = mean(b_V_810,na.rm = T),
      TTR_760 = mean(b_U_760,na.rm = T),
      TTR_730 = mean(b_T_730,na.rm = T),
      TTR_680 = mean(b_S_680,na.rm = T),
      TTR_610 = mean(b_R_610,na.rm = T))
    
    
  
    data = data %>% left_join(TTRdatasum, by=c("Site","doy","hour"))
  
    #LAI according to Beer-Law and light extinction coefficient, look into papers LAI folder
    K = 5.2 # light extinction coefficient
    data = data %>% mutate(LAInir = -log((b_V_810c+b_W_860c)/(TTR_860c+TTR_810c))/K)
    data = data %>% mutate(LAIb = -log((b_V_450c+b_B_500c)/(TTR_450c+TTR_500c))/K)
    return(data)
  } else {
    
    fun_log(verboseFlag = verboseFlag, c("Looks like your site dont have TTR, returning data without change \n")) 
    return(data)
  }
  
}





#Exporting site data to excel===================================================
export_all_to_excel = function(AllData) {
  var_list = c("time","id","Species","d","VTA_score","rec_num","tair","rh","VPD",
               "theta","psi","phi","gz2","nt1","NDVIc","EVIc","VARIc","PRIc","NDVI","EVI","VARI","PRI","Rr",
               "Br","Gr","Flux", "TTair","TTrh","LAIb","LAInir")
  
  AllData = AllData %>% mutate(g2 = gz2+gy2+gx2) 
  AllData = AllData %>% mutate(W = mean((46000-Hz)/(Hz+46000)*50, na.rm=T))
  
  foreach (site = AllData$SiteIndex %>% unique()) %dopar% {
    list_of_datasets = list()
    
    SITEdata = AllData%>%filter(SiteIndex == site)
    for (i in SITEdata$id %>% unique) {
      index = which(SITEdata$id %>% unique  == i)
      TT = SITEdata %>% filter(id == i)
      TT  = TT[,var_list]
      list_of_datasets[[i]] =  TT
      names(list_of_datasets)[index] = i
    }
    sitename = site
    write.xlsx(list_of_datasets, file = paste(sitename,".xlsx",sep=""))
    # dat = loadWorkbook( file = paste(sitename,".xlsx",sep=""))
    # desc = readWorkbook(insert_file, sheet=1)
    # addWorksheet(dat, "Пояснения")
    # writeData(dat,"Пояснения",desc)
    # saveWorkbook(dat, paste(sitename,".xlsx",sep=""), overwrite = TRUE)
  }



}


export_site_to_excel = function(site_object,sitename="site",
                                insert_file="RUDN_descr.xlsx") {
  var_list = c("time","id","Species","d","VTA_score","rec_num","tair","rh","VPD",
               "theta","psi","phi","gz2","nt1","NDVIc","EVIc","VARIc","PRIc","NDVI","EVI","VARI","PRI","Rr",
               "Br","Gr","Flux", "TTair","TTrh","LAIb","LAInir")
  list_of_datasets = list()
  
  SITEdata = site_object[[2]]
  foreach (i = SITEdata$id %>% unique) %dopar% {
    index = which(SITEdata$id %>% unique  == i)
    TT = SITEdata %>% filter(id == i)
    TT  = TT[,var_list]
    list_of_datasets[[i]] =  TT
    names(list_of_datasets)[index] = i
  }
  
  write.xlsx(list_of_datasets, file = paste(sitename,".xlsx",sep=""))
  dat = loadWorkbook( file = paste(sitename,".xlsx",sep=""))
  desc = readWorkbook(insert_file, sheet=1)
  addWorksheet(dat, "Пояснения")
  writeData(dat,"Пояснения",desc)
  saveWorkbook(dat, paste(sitename,".xlsx",sep=""), overwrite = TRUE)
}




#Exporting site data to csv folder\=============================================
export_site_to_csv_folder = function(site_object, export_folder="csv_export") {
  if(!dir.exists(export_folder)){
    dir.create(export_folder)
  }
  setwd(export_folder)

  for (i in 1:2){
    SITEdata = site_object[[i]]

    for (t in SITEdata$id %>% unique) {
      index = which(SITEdata$id %>% unique  == t)
      TT = SITEdata %>% filter(id == t)
      filename = paste(as.character(t),".csv",sep="")
      if(i==1){
        filename = paste("raw_",filename,sep="")
      }
      write.csv(TT,file = filename, sep=";", dec=".")
    }

  }
  setwd("..")
  return(NULL)
}
#========================================================================================================================
three_sigma_weekly_flagging = function(dt, var_name){

  dtg = dt %>% group_by(id,year,week)
  var_name   = enquo(var_name)
  var_name_f = paste0(quo_name(var_name),"_f", sep="", collapse = NULL)

  dtg = dtg %>% mutate(!!var_name_f :=
                         !!var_name > mean(!!var_name) + 3*sd(!!var_name) |
                         !!var_name < mean(!!var_name) - 3*sd(!!var_name))

  return(dtg %>% as.data.frame)
}
#========================================================================================================================
two_sigma_weekly_flagging = function(dt, var_name){

  dtg = dt %>% group_by(id,year,week)
  var_name   = enquo(var_name)
  var_name_f = paste0(quo_name(var_name),"_f", sep="", collapse = NULL)

  dtg = dtg %>% mutate(!!var_name_f :=
                         !!var_name > mean(!!var_name) + 2*sd(!!var_name) |
                         !!var_name < mean(!!var_name) - 2*sd(!!var_name))

  return(dtg %>% as.data.frame)
}
#===========Two sigma grouped flagging

two_sigma_grouped_flagging = function(dt, var_name, group_var1, group_var2,  suffix){
  group_var1 = enquo(group_var1)
  group_var2 = enquo(group_var2)
  dtg = dt %>% group_by(!!group_var1, !!group_var2)
  var_name   = enquo(var_name)
  var_name_f = paste0(quo_name(var_name),suffix, sep="", collapse = NULL)
  
  dtg = dtg %>% mutate(!!var_name_f :=
                         !!var_name > mean(!!var_name) + 2*sd(!!var_name) |
                         !!var_name < mean(!!var_name) - 2*sd(!!var_name))
  
  return(dtg %>% as.data.frame)
}
out_of_two_sigma = function(var){
  var[is.infinite(var)] = NA
  var_n =   var > mean(var, na.rm=T) + 2*sd(var, na.rm=T) |  var < mean(var, na.rm=T) - 2*sd(var, na.rm=T)
  return(var_n)
}



#===============================================================================
flagged = function(dt){
  vars = names(dt)[names(dt) %>% str_ends("_f")]
  for(var in vars){
    dt = dt %>% filter(!!var == FALSE)
  }
  return(dt)
}
#===============================================================================
radiation_flagging = function(dt){
  tt_data_e  = dt
  dtg = tt_data_e %>% mutate(radiation = b_V_810+b_B_500+b_G_550+b_R_650+b_R_610+
                               b_S_680+b_T_730+b_U_760+b_W_860+b_V_450+b_Y_570+b_O_600)
  dtg = dtg %>% group_by(id,year,doy) %>%
    mutate(rad_max = max(radiation, na.rm = T))
  dtg = dtg %>% mutate(is_max_hour = (radiation == rad_max)) %>% as.data.frame
  dtg$is_max_hour[is.na(dtg$is_max_hour)] = F
  dtg = dtg %>%group_by(id,year,doy) %>%
    mutate(max_hour = ifelse(is_max_hour %>% which %>% length > 0,
                             is_max_hour %>% which - 1, NA)) %>% as.data.frame
  dtg = dtg %>% group_by(id,year,week) %>%
    mutate(mean_max_hour =
             ifelse(max_hour %>% is.na %>% which %>% length < max_hour %>% length,
                    mean(max_hour, na.rm=T) %>% round(0) , NA))%>% as.data.frame

  dtg = dtg %>% group_by(id,year,doy) %>%
    mutate(rad_flag = hour > mean_max_hour - 4 & hour < mean_max_hour+4) %>%
    as.data.frame
  dt$rad_flag = dtg$rad_flag
  return(dt)
}




###########################TESTING$#############################################



#TODO remove record number			calculate solid angle from 3
#TODO angles +180
#TODO collect gz2 for all treetalker - build frequency destribution
#TODO and for given number calculate percentile
#TODO descibe nt1 as stem temperature
#TODO the same index for whole day
#TODO add yellowness

#TODO Flux
#TODO quantity	absolute values
#TODO quality 	curve
#TODO stomata closure
#TODO plot VPD vs FLUX
#TODO time of maximum flow
#TODO Fmax=gmaxVPD
#TODO Cumulative min max for all species
#TODO gmax = stomata conductance			R2


#TODO frequency distribution of growth to temperature
#TODO average to same temp per week
#TODO LUT for distance T-rh-day-night

#TODO gap fill - lut

#TODO compare TTR with TT data

##
##
##### Report preparation function - gapfilling and stuff



TTsite_repport = function(data){


}

