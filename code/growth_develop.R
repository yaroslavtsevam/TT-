library(runner)
library(spatialEco)
gd = AllData %>% filter(id %in% c( "218A0139","218A0142","218A0224","218A0122","218A0106","218A0147","218A0102","218A0280","218A0100","218A0151"),year == 2020)


ggplot(TIM %>% filter(year==2020, month >6), aes(x = time, y = dist3, color=id))+geom_point()
plot_ly(TRK %>% filter( doy > 1) , x = ~time, y = ~VWWCb, color = ~id)

  # dfn_night=dfn[dfn['a_650']==0]# select only night values
# dfn['growth_sensor_night']=dfn_night['growth_sensor_c']
# avg_g=dfn['growth_sensor_night'].median()# calculate median
# std_g=dfn['growth_sensor_night'].std()#calculate std
# 
# for j in range (0,len(dfn)):
#   if dfn.loc[j,'growth_sensor_night']>avg_g+2*std_g or dfn.loc[j,'growth_sensor_night']<avg_g-2*std_g: # correct for numbers outside +- 2*std
#   dfn.loc[j,'growth_sensor_night']=avg_g #replace outliers with median 
# dfn.growth_sensor_night.interpolate(method='pad',axis=0, inplace=True) #interpolation of missing data NaN
# dfn['growth_roll']=dfn.loc[0:len(dfn),'growth_sensor_night'].rolling(window=200).mean() # rolling mean of interpolated data
# dfn['growth_roll_diff']=dfn['growth_roll'].diff() # derivative (growth) of the distance
# dfn.loc[dfn.growth_roll_diff>0,'growth_roll_diff']=0  #to be applied in case we want non-negative growth
# 
# dfn['radial_growth']=-dfn['growth_roll_diff'].cumsum()

add_growth = function(Data, start_df){
  NewAllData = data.frame()
  for(site in Data$Site %>% unique())
  {
    print(site)
    start_doy = start_df$doy[start_df$site == site]
    Sitem = AllData %>% filter(Site == site) %>% ungroup() %>% 
      filter(year == 2020,dist3 < 5 & dist > 1, doy > start_doy, volt > 3.5, b_R_650 < 10 ) %>% 
      group_by(id, doy) %>% mutate(sdd = sd(dist3,na.rm = T)) %>%
      filter(sdd < .02) %>% ungroup() %>% group_by(id) %>%
      mutate(dist3 = two_sigma_filtering(dist3)) %>%
      mutate(distr = runner(dist3, k = 10, f = mean), timei = as.numeric(time)) %>%
      filter(!is.infinite(dist3), !is.na(dist3)) %>%
      nest(data = !id) %>%
      mutate(models = lapply(data, function(df) {
        tryCatch(loess(dist3 ~ timei, span = .7, data = df %>% as.data.frame()), 
                 error = function(cond){return(NA)})}))
    
    
    SITE = Data %>% filter(Site == site) %>% ungroup() %>% 
      mutate(timei = as.integer(time)) %>% nest(data = !id)
    
    SITE = left_join(SITE, Sitem %>% select(id, models))
    print("2")
    SITE = SITE %>% mutate(
      pred = tibble(map2(models,data,~tryCatch(augment(.x, newdata=.y), 
                                      error = function(cond){return(NA)})))) %>%
      select(id, pred) %>% unpack(pred) %>%  unnest() 
    if(".fitted" %in% colnames(SITE)){
      SITE = SITE %>%
      rename(distg = .fitted) %>% ungroup() %>% group_by(id) %>%
        mutate(gincr = difference(distg,1)) %>%
        mutate(gf = distg - dplyr::first(distg, default = min(distg, na.rm = T)))
      print("3")
      SITE$gincr[is.na(SITE$gincr) | SITE$gincr > 0 | abs(SITE$gincr) > 0.001 ] = 0
      SITE = SITE %>% mutate(gcs = -cumsum(gincr), growth = -gincr)
      print("4")
      NewAllData = rbind(NewAllData, SITE)
      
    } else(next())
  }
  return(NewAllData)
}

update_df = data.frame(
  site = c("RUDN","TIMIRYAZEV", "BOLOTNAYA","GARDEN",
           "TROITSK","SCHOOL1234","SCHERBINKA","GBS_STAB"),
  doy  = c(210,191,180,190,160,185,160 ,160) 
)


Gr = add_growth(AllDataf %>% filter(Site %in% update_df$site), update_df)

TRK$wood_moist = two_sigma_filtering(TRK$wood_moist)
TRK = TRK %>% ungroup %>% group_by(id) %>%
  mutate(wood_moist = two_sigma_filtering(wood_moist))





Gr$id %>% unique()

TIM = AllData %>% filter(Site == "TIMIRYAZEV")
BLT = AllData %>% filter(Site == "BOLOTNAYA")
GDN = AllData %>% filter(Site == "GARDEN")
TRK = AllData %>% filter(Site == "TROITSK")
SCL = AllData %>% filter(Site == "SCHOOL1234")
SCH = AllData %>% filter(Site == "SCHERBINKA")


isnight = AllData$b_R_650c == 0 # select only night values


dfn['growth_sensor_night'] = dfn_night['growth_sensor_c']