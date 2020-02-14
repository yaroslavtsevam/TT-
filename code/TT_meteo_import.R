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
                                                   `Мгн.скорость1` = col_skip(), 
                                                   `Осадки_12ч` = col_skip(), 
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
  
  all_meteo_data = all_meteo_data[,1:5]
  names(all_meteo_data) = c("date","time","ta","Rh","WS")
  all_meteo_data  = all_meteo_data %>% 
    mutate(hour=hour(time), doy=yday(date), year = year(date))
  #data = left_join(data,all_meteo_data, by=c("year","doy","hour"))
  return(all_meteo_data)
  
}  


meteo_data  = read_meteo("data/meteo/MSU") %>% group_by(year,doy,hour)%>%
  summarise(Rh = mean(Rh, na.rm = T),ta = mean(ta, na.rm = T), WS=mean(WS, na.rm = T))
#meteo_data$doy %>% max
#data = RUDNdata[[2]]
AllData = left_join(AllData,meteo_data, by=c("year","doy","hour"))
#data$doy
#data$Rh%>%is.na()%>%which()%>%length()