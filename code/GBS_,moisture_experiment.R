source("code/TTcalc_site_7.R")

############################################################################################################
#######################################                              #######################################
#######################################  VERY DIRTY RECONCTRUCTION   #######################################
#######################################                              #######################################
############################################################################################################


data_bc = data.frame()

for(file in list.files("data/backup/GBS/",pattern="*.csv")){
 temp =  read_delim(paste("data/backup/GBS",file, sep="/"),
             col_names = FALSE, delim=";", skip=0)
  data_bc = rbind(data_bc, temp)
}


cloud = read_delim("data/backup/GBS/C18A0229.txt", col_names = FALSE, delim=";", skip=0)

cloud$X1 %>% unique() %>% sort()

cloud = cloud %>% filter(X1 %in% c("21880215","218A0242")) %>% arrange(X1)


data_bc = rbind(data_bc, cloud)
data_bc$X1 = paste("10.05.21 12:55:19",data_bc$X1,sep=",")
#source variable will be created from that
data_bc$s=i+1
data_bc$X12 = as.double(data_bc$X12)
#print(as.data.frame(data_bc))
tt_bc = TTBasicCalc(data_bc , verboseFlag = "con")


tt_bc = tt_bc %>% group_by(id) %>% arrange(rec_num) %>%
  mutate(recharge_flag =ischarged(.data)) %>%
  mutate(charge = cumsum(recharge_flag))
tt_bc$charge = tt_bc$charge+1

 tt_cor %>% filter(id=="218A0135") %>% as.data.frame %>% ggplot(aes(x=datetime, y =volt, color= datetime))+geom_point()

GBSSdata[[2]] %>%filter(id=="218A0096")%>%  ggplot(aes(x=1:length(volt),y=volt))+geom_point(aes(color=datetime))+ 
  geom_point(data = tt_cor%>% filter(id=="21880204"),aes(x=1:length(volt),y=volt))+theme_bw() 
ggplotly()
dat_cor = data.frame(
  id = c("21880204","21880213","218A0080", "218A0098","218A0135","218A0162", "218A0184","218A0260","218A0267", "218A0299",
         "21880204","21880213","218A0080", "218A0098","218A0135","218A0162", "218A0184","218A0260","218A0267", "218A0299"),
  start_shift = c(1596461400,1596456000,1596456000, 1596456000,1596456000,1596456000, 1596456000,1596456000,1596456000, 1596456000,
                  1600349400,1600350000,1600350300, 1600350600,1600350900,1600351200, 1600351500,1600351800,1600352100, 1600352400  ),
  charge = c(2,2,2, 2,2,2, 2,2,2, 2,
             3,3,3, 3,3,3, 3,3,3, 3)
)


for( i in 1:nrow(dat_cor)){
  tt_bc$datetime[ tt_bc$charge == dat_cor$charge[i] & tt_bc$id == dat_cor$id[i] ][1] =
    tt_bc$datetime[ tt_bc$charge == dat_cor$charge[i] & tt_bc$id == dat_cor$id[i] ][1] + dat_cor$start_shift[i]
}

extrapolate_corr_dates = function(data){
  if(any(data$datetime > 1556845000)){
    start_cor_date = min(data$datetime[data$datetime > 1556845000], na.rm=T)
    
    start_cor_date_index = which(data$datetime == start_cor_date)
    print(start_cor_date)
    print(start_cor_date_index)
    if(start_cor_date_index > 1){
      start_cor_date = start_cor_date - 60*90*(start_cor_date_index-1)
    }
    print(start_cor_date)
    data$datetime = start_cor_date+0:(length(data$datetime)-1)*60*90
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

for(file in list.files("data/backup/GBS/",pattern="*.csv")){
  temp =  read_delim(paste("data/backup/GBS",file, sep="/"),
                     col_names = FALSE, delim=";", skip=0)
  data_bc = rbind(data_bc, temp)
}
cloud = read_delim("data/backup/GBS/C18A0229.txt", col_names = FALSE, delim=";", skip=0)
cloud$X1 %>% unique() %>% sort()
cloud = cloud %>% filter(X1 %in% c("21880215","218A0242")) %>% arrange(X1)
data_bc = rbind(data_bc, cloud)


data_bc$id    = as.character(data_bc$X1)
data_bc$rec_num = data_bc$X2 %>% HexToDec

data_bcj = left_join(data_bc,tt_cor%>%select(datetime,id,rec_num), by=c("id","rec_num"))

data_bcj$datetime[which(data_bcj$X3=="49")]=data_bcj$datetime[which(data_bcj$X3=="49")-1]
data_bcj$X4[!is.na(data_bcj$datetime)] = as.integer(data_bcj$datetime[!is.na(data_bcj$datetime)])


data_bcj = data_bcj %>% select(X1:X20)
data_bcj$X1 = paste(as.character( format((data_bcj$X4+HexToDec(data_bcj$X2)+3600*3) %>% as_datetime,"%d.%m.%y %H:%M:%S") 
                                 ),data_bcj$X1,sep=",")


data_bcj = data_bcj %>% filter(X4 > 1556845000)
write_delim(data_bcj, path="C18A0229_cloud_rec.txt", delim=";", quote = F, na="")

