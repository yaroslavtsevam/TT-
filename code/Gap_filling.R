source("code/TTcalc_site_7.R")
library(ggpmisc)
library(extrafont)
library(Rmisc)
library(timetk)
library(slider)
library(plotly)
library(TREX)
library(fable)
library(imputeTS)
library(progress)

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




BFG = function(SiteData){
  data = SiteData %>% ungroup()
  
  
  #Leaving only main calculated and raw variables
  
  data  = data %>% select(
    "id",
    "rec_num","datetime","serv_datetime","time","charge","year",# Time codes
    "b_R_650","b_O_600","b_Y_570","b_G_550","b_B_500","b_V_450",# Raw spectral
    "b_W_860","b_V_810","b_U_760","b_T_730","b_S_680","b_R_610",# Raw spectral
    "accel","theta","psi","phi","gx2","gy2","gz2","dist",       # Tree stability   
    "volt","pulses","Ecf","t1","t2","nt1","nt2","rh","tair",    # Raw sensors data
    "u","u2","growth","VPD","wood_moist"                        # Calculated data
  )
  
  
  # Filling implicit gaps, making them explicit NA filled and 
  # expand time step to 1 hour, from 1 hour and half
  
  # Filling gaps in time variable, to fullfill demands of tsibble package
  data$time = as.POSIXct(as.integer(zoo::na.fill(zoo::zoo(data$time),"extend")), 
                         tz = "Europe/Moscow", origin = "1970-01-01 00:00:00")
  data = data %>% group_by(id,year) %>% filter(!duplicated(time))
  data = data %>% mutate(time_ts = floor_date(time, "hour"))
  data = data %>% ungroup %>% filter(
    !are_duplicated(data, index = time_ts, key = id))
  # Converting to tsibble to gap fill and work with fable functions
  data = tsibble(data, index = time_ts, key = id, regular = T)
  data = data %>% group_by_key() %>% tsibble::fill_gaps(.full = TRUE)
  
  
  # Cleaning and removing outliers
  remove_nan_inf = function(x){
    x[is.infinite(x)] = NA
    x[is.nan(x)] = NA
    return(x)
  }
  data = data %>% map_df(remove_nan_inf)
  
  data$u[data$t1 < 1 | data$nt1 < 1 | data$t2 < 1 | data$nt2 < 1] = NA
  data$u2[data$t1 < 1 | data$nt1 < 1 | data$t2 < 1 | data$nt2 < 1] = NA
  data$tair[data$tair > 50 | data$tair < -19] = NA
  data$rh[data$rh > 100 | data$rh < 0] = NA
  
  
  dataframe2TREX = function(dat, longtitude){
    
    print("Preparing TREX")
   
    TREX = data %>% filter(!duplicated(time_ts)) %>% 
      mutate(value = nt2 - t2,year = as.integer(year(time_ts)),
             doy = as.integer(yday(time_ts)),
             hour = paste(as.character(hour(time_ts)),":00", sep = "")
      ) %>% select(year,doy,hour,value) %>% as.data.frame()
    TREX2 = tryCatch( is.trex(TREX,tz = "Europe/Moscow", time.format = "%H:%M", 
                solar.time = TRUE, long.deg = longtitude, ref.add = F, df = T),
             error = function(cond){print("skip TREX"); return(TREX2 = NULL)} 
             )
    
    return(TREX2) 
  }
  
  # Recalculating sap flux density with TREX
  sapflux_dr_TREX = function(df, longtitude){
    #Converting data to TREX format
    
    TX = dataframe2TREX(df, longtitude)
    
    
    #  There are several options to calculate dTmax in TREX, but double regression 
    #  looks better - using it
    if (!is.null(TX)) {
      
      output.max = tryCatch(
        tdm_dt.max(TX, methods = c("dr"), det.pd = TRUE, interpolate = FALSE,
        max.days = 10, df = TRUE), 
        error = function(cond){print("E"); return(NULL)})
      if( !is.null(output.max) ) {
      # aligning new dTmax with other data
        dT = output.max$daily_max.dr
        dT$doy = yday(dT$timestamp)
        dT = dT[,2:3]
        
        df$doy = yday(df$time_ts)
        df = left_join(df, dT, by = "doy")
        df = df %>% rename(dTm.mw = value)
        df = df %>% mutate(
          umw = 118.99*(10^-3)*(dTm.mw/(nt2 - t2 - nt1 + t1) - 1)^1.231)
      }
    }
    return(df)  
  }
  
  sumdat = data %>% ungroup() %>% group_by(id) %>% 
    summarise( across(where(is.numeric), ~sum(is.na(.x)))) %>%
    rowwise() %>%
    summarise(mx = max(c_across(b_R_650:u2)))
  ndat = data %>% ungroup() %>% group_by(id) %>% 
    summarise(n = n())
  sumdat = cbind(sumdat, ndat)
  sumdat = sumdat %>% mutate(ratio = mx/n)
  empty_id = sumdat$id[sumdat$ratio > .99]
  data = data %>% filter(!(id %in% empty_id))
  data =  data %>% as.data.frame %>% 
    split(.$id) %>% map_dfr( ~sapflux_dr_TREX(df = .,longtitude = 51))
  if("dTm.mw" %in% colnames(data)){
    data = data %>% select(-dTm.mw)  
  } else {
    return(data)
  }
  
  
  data$umw[data$umw < 0] = NA
  data$umw[data$umw > 1] = NA
  
  data$umw[data$u < 0] = NA
  data$umw[data$u > 1] = NA
  
  data$umw[data$u2 < 0] = NA
  data$umw[data$u2 > 1] = NA
  
  
  #Gap filling small gaps < 4
  #Function based on zoo package to interpolate linearly small gaps
  gapfill_zoo = function(x,n=4) {
    return(zoo::na.approx(x, x = zoo::index(x),  na.rm = F, maxgap = n))  
  }
  
  gapfill_zoo_time = function(x,n=4) {
    
    numf = zoo::na.approx(as.double(x), x = zoo::index(as.double(x)),
                          na.rm = F, maxgap = n)
    return(as.POSIXct(numf,tz = "Europe/Moscow",origin = "1970-01-01 00:00:00"))
    
  }
  
  
  two_sigma_filtering = function(x){
    if(sum(is.na(x)) == length(x)){return(x)}
    else {
      x[x > mean(x,na.rm=T) + 2*sd(x,na.rm=T) | 
          x < mean(x, na.rm = T) - 2*sd(x, na.rm = T)] = NA
    }
    
    return(x)
  }
  data = data %>% mutate(week = week(time_ts), month = month(time_ts))
  
  # First, removing weekly and monthly outliers and then filling small gaps
  
  data = data %>% ungroup %>% group_by(id,year,week) %>%  as_tibble() %>%
    mutate(across(where(is.numeric), ~two_sigma_filtering(.x)))
  
  
  data = data %>% ungroup %>% group_by(id,year,month) %>%  as_tibble() %>%
    mutate(across(where(is.numeric), ~two_sigma_filtering(.x)))
  
  
  data = data %>% ungroup() %>% group_by(id) %>%
    mutate(across(where(is.numeric), ~gapfill_zoo(.x , n=2)))
  
  
  
  # Filling gaps less than 24 hours according to time series structure - ARIMA
  
  impute_24h = function(x){
    if(length(x) - sum(is.na(x)) < 5) { return(x) }
    else{
      ts = as.ts(x)
      z = na_kalman(ts, model = "auto.arima", smooth = TRUE, maxgap = 24)
      
      return(as.vector(z))  
    }
    
  }
  
  total_impute = function(df){
    
    df = df %>% ungroup %>% group_by(id,year) %>%
      mutate(across(where(is.numeric), ~impute_24h(.x )))
    
    return(df)
  }
  
  data = data %>% total_impute()
  
  # Getting tair and rh from trees of the same Species for gap periods-+++
  impute_mean_by_species = function(df, descr_path = "data/TT_desc_20.csv"){

    descr = suppressWarnings(suppressMessages(
      read_delim(descr_path,col_names = T, delim = ",")))
    
    df = left_join(df %>% as.data.frame(),descr,by = "id")
    df = df %>% group_by(id,year) %>% filter(!duplicated(time_ts))
    df = df %>% mutate(time_ts = as.POSIXct(time_ts, tz = "Europe/Moscow",
                                            origin = "1970-01-01 00:00:00"))
    df =  tsibble(df, index = time_ts, key = c("id"), regular = T)
    
    mean_per_species  = df %>%
      group_by(Species) %>%
      summarise_all(
        mean,na.rm = TRUE
      )
    dataff = left_join(df,mean_per_species %>% 
                         select("time_ts","Species","tair","rh"), 
                       by = c("Species","time_ts"))
    dataff$tair.x[is.na(dataff$tair.x)] = dataff$tair.y[is.na(dataff$tair.x)]
    dataff$rh.x[is.na(dataff$rh.x)] = dataff$rh.y[is.na(dataff$rh.x)]
    
    df$rh = dataff$rh.x
    df$tair = dataff$tair.x
    df$VPD = 0.6108*exp((17.27 * df$tair)/(df$tair + 273.15))*(1 - df$rh/100)
    
    return(df)
  }
  
  
  data  = data %>% impute_mean_by_species()
  
  
  ######
  
  data = left_join(data %>% as.data.frame(),
                   descr %>% select(id,Species),
                   by = "id")
  
  
  return(data)
}


RUDN = RUDNdata[[2]]
#RUDNf = BFG(RUDN)
RUDNf = BFG(RUDNdata[[2]])
TIMf = BFG(TIMdata[[2]])
BLTNf = BFG(BLTNdata[[2]])
GRDNf = BFG(GRDNdata[[2]])
TRSKf = BFG(TRSKdata[[2]])
SCHLf = BFG(SCHLdata[[2]])
SHERf = BFG(SHERdata[[2]])
GBSf = BFG(GBSSdata[[2]])
ELTSf = BFG(ELTSdata[[2]])
GPf = BFG(GPdata[[2]])

AllDataf = rbind(RUDNf, TIMf,BLTNf,TRSKf,SCHLf,SHERf)
AllDataf = AllDataf %>% select(-Species.y)
AllDataf = AllDataf %>% select( -canopy_area,-status,-X1,  -serv_datetime)
AllDataf$datetime = AllDataf$time_ts
AllDataf = AllDataf %>% select(-time_ts)
AllDataf = AllDataf %>% select(-growth)
AllDataf = AllDataf %>% select(-charge, -pulses, -Ecf)
AllDataf = AllDataf %>% rename(Species = Species.x)
AllDataf = AllDataf %>% mutate(year = year(datetime), month= month(datetime), 
                               doy = yday(datetime), week = week(datetime))
AllDataf$dist = 0.000000008*(AllDataf$pulses)^2 - 0.0016*(AllDataf$pulses) + 89.032
AllDataf = AllDataf %>% group_by(id, year) %>% 
  mutate(growth = dist-min(dist, na.rm = T)) %>% ungroup()
AllDataf$umw[AllDataf$umw < 0] = 0
AllDataf = AllDataf %>% mutate(u2 = umw*3600*(DBH^1.8777)*0.755/10000) %>%
  rename(Flux = u2)
AllDataf$Flux[AllDataf$Flux < 0] = 0
AllDataf$Flux[AllDataf$Flux > 100] = NA



export_filled_site_to_excel = function(site_object,sitename="site",
                                insert_file="RUDN_descr.xlsx") {
  SITEdata = site_object
  list_of_datasets = list()
  
  for (i in SITEdata$id %>% unique){
    index = which(SITEdata$id %>% unique  == i)
    TT = SITEdata %>% filter(id == i)
    list_of_datasets[[i]] =  TT
    names(list_of_datasets)[index] = i
  }
  
  write.xlsx(list_of_datasets, file = paste(sitename,".xlsx",sep=""))
  dat = loadWorkbook( file = paste(sitename,".xlsx",sep = ""))
  desc = readWorkbook(insert_file, sheet=1)
  addWorksheet(dat, "Пояснения")
  writeData(dat,"Пояснения",desc)
  saveWorkbook(dat, paste(sitename,".xlsx",sep = ""), overwrite = TRUE)
}
sitelist = AllDataf$Site %>% unique
for(i in sitelist[6]){
  export_filled_site_to_excel(AllDataf %>% filter(Site == i),i)
}

AllDataf$b_R_650c = AllDataf$b_R_650*0.7829+202.77
AllDataf$b_O_600c = AllDataf$b_O_600*0.8654-328.08
AllDataf$b_Y_570c = AllDataf$b_Y_570*1.0462-666.72
AllDataf$b_G_550c = AllDataf$b_G_550*1.0546-842.1
AllDataf$b_B_500c = AllDataf$b_B_500*0.6257-232.13
AllDataf$b_V_450c = AllDataf$b_V_450*0.4562-212.62

AllDataf$b_W_860c = AllDataf$b_W_860*0.5319+334.88
AllDataf$b_V_810c = AllDataf$b_V_810*0.8414+91.58
AllDataf$b_U_760c = AllDataf$b_U_760*1.4549-1012.5
AllDataf$b_T_730c = AllDataf$b_T_730*1.6209-1511.2
AllDataf$b_S_680c = AllDataf$b_S_680*1.5199-561.56
AllDataf$b_R_610c = AllDataf$b_R_610*1.6699-312.45

write.csv(AllDataf, file = "AllDatafc.csv")


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

Flux_species_site_month = SummaryTableFluxm %>% as.data.frame %>% group_by(Site, Species, month) %>% 
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

AllDataf = AllDataf %>% mutate( NDVI = (b_V_810c - b_S_680c)/(b_V_810c + b_S_680c))

SummaryTableNDVI = AllDataf %>% ungroup() %>%
  group_by(Site, Species, doy, id, month, antrop_load, VTA_score, age_group_index) %>%
  filter(NDVI > 0 & NDVI < 0.97) %>%
  summarise( dNDVI = quantile(NDVI, 0.85,na.rm = T))

NDVI_site_species = SummaryTableNDVI %>% as.data.frame() %>% group_by(Site, Species) %>% 
  summarise(mean = mean(dNDVI, na.rm=T), sd =sd(dNDVI, na.rm=T), 
            max=max(dNDVI, na.rm=T), min=min(dNDVI, na.rm=T) ) %>%
  as.data.frame()

NDVI_site_species_month = SummaryTableNDVI %>% as.data.frame() %>% group_by(Site, Species, month) %>% 
  summarise(mean = mean(dNDVI, na.rm=T), sd =sd(dNDVI, na.rm=T), 
            max=max(dNDVI, na.rm=T), min=min(dNDVI, na.rm=T) ) %>%
  as.data.frame()

NDVI_age_species_month = SummaryTableNDVI %>% as.data.frame() %>% group_by(Species,age_group_index,  month) %>% 
  summarise(mean = mean(dNDVI, na.rm=T), sd =sd(dNDVI, na.rm=T), 
            max=max(dNDVI, na.rm=T), min=min(dNDVI, na.rm=T) ) %>%
  as.data.frame()

NDVI_age_species = SummaryTableNDVI %>% as.data.frame() %>% group_by(Species,age_group_index) %>% 
  summarise(mean = mean(dNDVI, na.rm=T), sd =sd(dNDVI, na.rm=T), 
            max=max(dNDVI, na.rm=T), min=min(dNDVI, na.rm=T) ) %>%
  as.data.frame()

NDVI_vta_species_month = SummaryTableNDVI %>% as.data.frame() %>% group_by(Species,VTA_score,  month) %>% 
  summarise(mean = mean(dNDVI, na.rm=T), sd =sd(dNDVI, na.rm=T), 
            max=max(dNDVI, na.rm=T), min=min(dNDVI, na.rm=T) ) %>%
  as.data.frame()

NDVI_vta_species = SummaryTableNDVI %>% as.data.frame() %>% group_by(Species,VTA_score) %>% 
  summarise(mean = mean(dNDVI, na.rm=T), sd =sd(dNDVI, na.rm=T), 
            max=max(dNDVI, na.rm=T), min=min(dNDVI, na.rm=T) ) %>%
  as.data.frame()

NDVI_load_species_month = SummaryTableNDVI %>% as.data.frame() %>% group_by(Species,antrop_load,  month) %>% 
  summarise(mean = mean(dNDVI, na.rm=T), sd =sd(dNDVI, na.rm=T), 
            max=max(dNDVI, na.rm=T), min=min(dNDVI, na.rm=T) ) %>%
  as.data.frame()

NDVI_load_species = SummaryTableNDVI %>% as.data.frame() %>% group_by(Species,antrop_load) %>% 
  summarise(mean = mean(dNDVI, na.rm=T), sd =sd(dNDVI, na.rm=T), 
            max=max(dNDVI, na.rm=T), min=min(dNDVI, na.rm=T) ) %>%
  as.data.frame()



SummaryTableMoist = AllDataf %>% ungroup() %>%
  group_by(Site, Species, doy, id, month, antrop_load, VTA_score, age_group_index) %>%
  summarise( dmoist = mean(wood_moist,na.rm = T))

moist_site_species = SummaryTableMoist %>% as.data.frame() %>% group_by(Site, Species) %>% 
  summarise(mean = mean(dmoist, na.rm=T), sd =sd(dmoist, na.rm=T), 
            max=max(dmoist, na.rm=T), min=min(dmoist, na.rm=T) ) %>%
  as.data.frame()

moist_site_species_month = SummaryTableMoist %>% as.data.frame() %>% group_by(Site, Species, month) %>% 
  summarise(mean = mean(dmoist, na.rm=T), sd =sd(dmoist, na.rm=T), 
            max=max(dmoist, na.rm=T), min=min(dmoist, na.rm=T) ) %>%
  as.data.frame()

moist_age_species = SummaryTableMoist %>% as.data.frame() %>% group_by(Species,age_group_index) %>% 
  summarise(mean = mean(dmoist, na.rm=T), sd =sd(dmoist, na.rm=T), 
            max=max(dmoist, na.rm=T), min=min(dmoist, na.rm=T) ) %>%
  as.data.frame()

moist_age_species_month = SummaryTableMoist %>% as.data.frame() %>% group_by( Species,age_group_index, month) %>% 
  summarise(mean = mean(dmoist, na.rm=T), sd =sd(dmoist, na.rm=T), 
            max=max(dmoist, na.rm=T), min=min(dmoist, na.rm=T) ) %>%
  as.data.frame()

moist_vta_species = SummaryTableMoist %>% as.data.frame() %>% group_by(Species,VTA_score) %>% 
  summarise(mean = mean(dmoist, na.rm=T), sd =sd(dmoist, na.rm=T), 
            max=max(dmoist, na.rm=T), min=min(dmoist, na.rm=T) ) %>%
  as.data.frame()

moist_vta_species_month = SummaryTableMoist %>% as.data.frame() %>% group_by( Species,VTA_score, month) %>% 
  summarise(mean = mean(dmoist, na.rm=T), sd =sd(dmoist, na.rm=T), 
            max=max(dmoist, na.rm=T), min=min(dmoist, na.rm=T) ) %>%
  as.data.frame()

moist_load_species = SummaryTableMoist %>% as.data.frame() %>% group_by(Species,antrop_load) %>% 
  summarise(mean = mean(dmoist, na.rm=T), sd =sd(dmoist, na.rm=T), 
            max=max(dmoist, na.rm=T), min=min(dmoist, na.rm=T) ) %>%
  as.data.frame()

moist_load_species_month = SummaryTableMoist %>% as.data.frame() %>% group_by( Species,antrop_load, month) %>% 
  summarise(mean = mean(dmoist, na.rm=T), sd =sd(dmoist, na.rm=T), 
            max=max(dmoist, na.rm=T), min=min(dmoist, na.rm=T) ) %>%
  as.data.frame()

SummaryTableGrow = AllDataf %>% ungroup() %>%
  group_by(Site, Species, doy, id, month, antrop_load, VTA_score, age_group_index) %>%
  summarise( growth = max(growth,na.rm = T))

SummaryTableGrow$growth = remove_nan_inf(SummaryTableGrow$growth)

grow_site_species = SummaryTableGrow %>% as.data.frame() %>% group_by(Site, Species) %>% 
  summarise(mean = mean(growth, na.rm=T), sd =sd(growth, na.rm=T), 
            max=max(growth, na.rm=T), min=min(growth, na.rm=T) ) %>%
  as.data.frame()

grow_site_species_month = SummaryTableGrow %>% as.data.frame() %>% group_by(Site, Species, month) %>% 
  summarise(mean = mean(growth, na.rm=T), sd =sd(growth, na.rm=T), 
            max=max(growth, na.rm=T), min=min(growth, na.rm=T) ) %>%
  as.data.frame()

grow_age_species = SummaryTableGrow %>% as.data.frame() %>% group_by(Species, age_group_index) %>% 
  summarise(mean = mean(growth, na.rm=T), sd =sd(growth, na.rm=T), 
            max=max(growth, na.rm=T), min=min(growth, na.rm=T) ) %>%
  as.data.frame()

grow_age_species_month = SummaryTableGrow %>% as.data.frame() %>% group_by(Species,age_group_index, month) %>% 
  summarise(mean = mean(growth, na.rm=T), sd =sd(growth, na.rm=T), 
            max=max(growth, na.rm=T), min=min(growth, na.rm=T) ) %>%
  as.data.frame()

grow_vta_species = SummaryTableGrow %>% as.data.frame() %>% group_by(Species, VTA_score) %>% 
  summarise(mean = mean(growth, na.rm=T), sd =sd(growth, na.rm=T), 
            max=max(growth, na.rm=T), min=min(growth, na.rm=T) ) %>%
  as.data.frame()

grow_vta_species_month = SummaryTableGrow %>% as.data.frame() %>% group_by(Species,VTA_score, month) %>% 
  summarise(mean = mean(growth, na.rm=T), sd =sd(growth, na.rm=T), 
            max=max(growth, na.rm=T), min=min(growth, na.rm=T) ) %>%
  as.data.frame()


grow_load_species = SummaryTableGrow %>% as.data.frame() %>% group_by(Species, antrop_load) %>% 
  summarise(mean = mean(growth, na.rm=T), sd =sd(growth, na.rm=T), 
            max=max(growth, na.rm=T), min=min(growth, na.rm=T) ) %>%
  as.data.frame()

grow_load_species_month = SummaryTableGrow %>% as.data.frame() %>% group_by(Species,antrop_load, month) %>% 
  summarise(mean = mean(growth, na.rm=T), sd =sd(growth, na.rm=T), 
            max=max(growth, na.rm=T), min=min(growth, na.rm=T) ) %>%
  as.data.frame()


sheets_list = list(
  Flux_species_site,Flux_species_site_month,Flux_species_load ,Flux_species_load_month,Flux_species_vta ,
  Flux_species_vta_month ,Flux_species_age ,Flux_species_age_month ,NDVI_site_species,NDVI_site_species_month,
  NDVI_age_species_month,NDVI_age_species,NDVI_vta_species_month,NDVI_vta_species,NDVI_load_species_month,
  NDVI_load_species,moist_site_species,moist_site_species_month,moist_age_species,moist_age_species_month,
  moist_vta_species,moist_vta_species_month,moist_load_species,moist_load_species_month,grow_site_species,
  grow_site_species_month, grow_age_species,grow_age_species_month,grow_vta_species,grow_vta_species_month,
  grow_load_species,grow_load_species_month 
)
write.xlsx(sheets_list, file = "Summaries.xlsx")
# 
# 
# temp = data  %>% filter(id %in% (data$id %>% unique())[3:7])
# 
# plot_ly( temp,x = ~time_ts, y = ~umw, color = ~id, type = "scatter") %>% 
#   add_lines() 
# 
# 
# 
# 
# temp = data %>% as.data.frame() %>% 
#   select(u2, tair, rh, time_ts,id, Species, b_R_650:b_R_610) %>%
#   mutate(time_ts = as.double(time_ts), id = id %>% as.factor(),
#          Species = as.factor(Species))
# 
# temp1 = temp %>% filter(id %in% unique(temp$id)[3:7])
# temp1 = temp1 %>% mutate(id = id %>% as.character() %>% as.factor()) 
# 
#  temp2 = temp %>% filter(id %in% unique(temp$id)[26:length(unique(temp$id))])
# temp2 = temp2 %>% mutate(id = id %>% as.character() %>% as.factor())
# 
# 
# temp1$id %>% unique()
# levels(temp1$id)
# library(doParallel)
# cl <- makeCluster(4)
# registerDoParallel(cl)
# tempf1 = missForest(temp1,  variablewise = T,
#                     verbose = T,parallelize = "variables")
# 
# 
# 
# rm(dataff)
# dataf = dataf %>% mutate(month = month(time_ts))
# m = lm(dataf$Flux ~ dataf$VPD)
# ggplot(dataf %>% filter(week(time_ts) %in% c(22:25),year == 2020), 
#        aes(x = time_ts, y = Flux, color = id)) + geom_point() + geom_line()+
#   geom_line(aes(x = time_ts, y = wood_moist*100, color = id))+
#   facet_wrap(~Species)
# 
# 
# flux_mod = function(a){lm(as.double(Flux) ~ wood_moist, data = a)}
# 
# fluxmodels = dataf %>% filter(month(time) %in% 4:9) %>%
#   filter(!is.na(year)) %>%
#   group_by(Species, year, month) %>% nest %>% 
#   #filter(!is.na(Site)) %>%
#   mutate(model = data %>% map(flux_mod))  %>%
#   mutate(resids = map2(data, model, modelr::add_residuals)) %>%
#   mutate(glance = map(model, broom::glance))
# 
# fluxmodels %>% unnest(glance)
# 
# TT141f = data %>% filter(id == "218A0141",  year(time_ts) == 2020, month %in% 5:7)
# 
# TREXF.dr = tdm_cal.sfd(output.max,make.plot = TRUE,df = TRUE, genus = "Tilia", 
#                        decimals = 5)
# 
# u2 = 118.99*(10^-3)*(dTm/(nt2-t2-nt1+t1) - 1)^1.231
# 
# output.max$max.pd
# 
# TT141f = dataf %>% filter(id == "218A0141",  year(time_ts) == 2020)
# TT141 = data %>% filter(id == "218A0141",  year(time_ts) == 2020)
# mean_per_species$VPD =  0.6108*exp((17.27*mean_per_species$tair)/(mean_per_species$tair+273.15))*(1-mean_per_species$rh/100)
# t = mean_per_species %>% 
#     filter(Species == "Tilia cordata", year(time_ts) == 2020) %>% 
#     select(VPD) %>% as.data.frame()
# TT141f$VPDm = t[,1]
# 
# plot_ly(data = TT141f, x = ~time_ts, y = ~u*50, type = "scatter") %>% 
#   add_lines() %>% add_lines(data = TT141f, x = ~time_ts, y = ~umw*50) 
# 
# 
# z = na_kalman(fluxts, model = "StructTS", smooth = TRUE)
# ggplot_na_imputations(fluxts,z)
# 
# 
# 
# 
# 
# 
# 
# 
# u_spline2 =  stats::spline( as.double(TT141f$time_ts), TT141f$u, method = "fmm",
#                             n = length(as.double(TT141f$time_ts)),
#                              xmin = min(as.double(TT141f$time_ts)),
#                             xmax = max(as.double(TT141f$time_ts)))
# u_spline = u_spline[[2]]
# u_spline2 = u_spline2[[2]]
# u_spline2[u_spline2 < 0] = 0
# u_spline2[u_spline2 > 30] = NA
# 
# TT141f$us2 = u_spline2
# 
# TT141ts = tsibble(TT141f, index = time_ts, key = c("id"), regular = T)
# TT141f = data %>% filter(id == "218A0141",  year(time_ts) == 2019)
# fit <- TT141ts[start(zx)[1]:end(zx)[1],] %>%
#   model(ARIMA(u ~ tair+wood_moist))
# TT141tsf = fit %>%  interpolate(TT141ts)
# 
# zx = na.contiguous(as.ts(TT141f$u))
# TT141tsm  = TT141ts %>% filter(start(zx)[1]:end(zx)[1]) %>%
#   model(fasster(u ~ VPD %S% (month) ), .safely = F) 
# TT141tsf = TT141tsm %>%  interpolate(TT141ts %>% filter(month %in% 4:9) )
# 
# plot_ly(data = TT141tsf , x = ~time_ts, y = ~u*50, type = "scatter") %>% 
#   add_lines(data = TT141tsf , x = ~time_ts, y = ~u*50) %>% 
#   add_lines(data = TT141ts , x = ~time_ts, y = ~VPD)
# 
# plot_ly(data = TT141ts , x = VPD, y = ~u*50, type = "scatter") 
# 
# 
# plot(dataf$time_ts,dataf$VPD)
# 
# 
# hist(data$Flux)
# 
# summary(m)
# 
# dataf %>% as.data.frame() %>% group_by(id)%>%
#   
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# data$time = as.POSIXct(as.integer(na.fill(zoo(data$time),"extend")), tz="Europe/Moscow", origin = "1970-01-01 00:00:00")
# data = data %>% filter(!duplicated(time))
# data = data %>% mutate(time_ts = floor_date(time, "hour"))
# data = data %>% filter(!are_duplicated(data, index = time_ts, key = id))
# # Removing duplicated rows... again???
# 
# # making tsibble to produce regular ts
# # Bdats  = tsibble(Bdata, index=time, key=id, regular = F)
# # Bdats = Bdats %>% fill_gaps(.full = T)
# data = tsibble(data, index=time_ts, key=id, regular = T)
# data = data %>% group_by_key() %>% tsibble::fill_gaps(.full = TRUE)
# 
# 
# RUDN %>% group_by(id) %>% summarise(gaps = is.na(b_R_650) %>% sum, n = n(), perc = gaps/n) %>% arrange(perc)
# 
# 
# 
# 
# 
# library(fable)
# 
# TT137 = data %>% filter(id == "218A0137") 
# TT137 = TT137 %>% filter(!is.na(time)) %>% filter(!duplicated(time)) %>% ungroup() %>%
#   as_tsibble( key = id, index = time)
# TT137f = TT137 %>% model(TSLM(b_R_650 ~ trend())) %>%
#   interpolate(TT137)
# 
# TT137f %>% autoplot(Flux)
# data %>% autoplot(Flux)
# 
# plot(data$time_ts, data$Flux)
# 
# olympic_complete <- olympic_running %>%
#   model(TSLM(Time ~ trend())) %>%
#   interpolate(olympic_running)
# 
# 
# #devtools::install_github("tidyverts/fasster")
# library(fasster)
# 
# TT283 = data %>% filter(id == "218A0283")
# TT283s = RUDN %>% filter(id == "218A0283")
# 
# TT283l  = TT283s %>% select(datetime, serv_datetime, charge, time,incr,rec_num)
# 
# 
# TT283 = TT283 %>% filter(!is.na(time)) %>% filter(!duplicated(time)) %>% ungroup() %>%
#   as_tsibble( key = id, index = time)
# TT283f = TT283 %>% model(TSLM(Flux ~ trend() + season(period = "day"))) %>%
#   interpolate(TT283)
# 
# TT283f %>% autoplot(Flux)
# TT283 %>% autoplot(Flux)
# 
# 
# library(tsibbledata)
# elecdemand_missing = tsibbledata::vic_elec
# 
# elec_misfit <- elecdemand_missing %>% 
#   fasster(log(Demand) ~ WorkDay %S% (trig(48, 16) + poly(1)) +  
#             Temperature + Temperature^2)
# 
# elec_fit = elec_misfit %>% interpolate()
# 
#   TT137f %>% autoplot(Flux)
# TT129 %>% autoplot(Flux)
# TT137$Flux %>% is.na() %>% sum
# 
# 
# ################### MICE
# 
# library(mice)
# 
# # result <- ampute(data = dataf %>% as.data.frame())
# # class(result)
# 
# imp1 <- mice(dataf %>% select(id,time_ts,b_O_600:b_R_610) %>%as.data.frame(), 
#              method = "rf", print = FALSE)
# 
# 
# 
# 
# 
# 
# 




