##
# This code is saved only for memorizing algorithms and corrections which was made to produce this data



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
AllData$insite_load = "high"
AllData$insite_load_score = 2
low_in_site_load_TTlist = c("062",100,102,107,108,110,113,114,115,118,119,122,123,124,129,135,137,
                            140,141,146,149,151,165,168,171,175,188,192,192,198,206,220,221,230,
                            233,238,240,245,283,"077","079","088",104,138,193,210,212,248,255,277,285,
                            "061","063","087","096",112,120,145,158,163,172,176,183,195,203,208,209,218,
                            219,226,246,249,256,259,263,279,282,287,289,291,293)
low_in_site_load_TTlist = paste("218A0",low_in_site_load_TTlist,sep="")                             
AllData$insite_load[AllData$id %in% low_in_site_load_TTlist] = "low"
AllData$insite_load_score[AllData$id %in% low_in_site_load_TTlist] = 1


########################### Correcting species names ####################################

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

############################   GROWTH d#################################################

dis_pred_table = data.frame()
for (ids in AllData$id %>% unique){
  
  dat = AllData%>%filter(id == ids)%>% 
    filter(b_W_860 <10) %>% filter((volt > 3.99 & volt<4.05))
  if(nrow(dat) < 10){next()}
  pred = ggplot_build(ggplot(dat)+
                        geom_smooth(aes(x=doy,y=dist13),method="lm" ,size=.1)+
                        stat_smooth(aes(x=doy,y=dist13),method="lm"))$data[[2]]
  pred_table = data.frame(round(pred$x, digits = 0), pred$y, id = ids, growth = max(pred$y) - pred$y)
  if(pred_table[1,2] < pred_table[length(pred_table[,1]),2]){
    pred_table[,2]=pred_table[1,2]
  }
  if(last(pred_table$growth) < first(pred_table$growth)){
    pred_table$growth=0
  }
  dis_pred_table = rbind(dis_pred_table, pred_table)
}
names(dis_pred_table) = c("doy","dist_pred","id", "growth")
AllData = left_join(AllData, dis_pred_table, by=c("id","doy"))
AllData$growth[is.na(AllData$growth)] = 0
AllData$growth[is.infinite(AllData$growth)] = 0



# VTA = read_delim("VTA.csv", ";", escape_double = FALSE,trim_ws = TRUE)
# VTA$id = paste("218A",VTA$id, sep="")
# VTA = VTA %>%mutate(score = rowMeans(.[2:23], na.rm = T))
# VTAscore =VTA %>% as.data.frame %>% dplyr::select(id, score)
# 
# AllData = left_join(AllData,VTAscore, by="id")


AllData$age_group_index = "I"
AllData$age_group_index[AllData$age_group == 2] = "II"
AllData$age_group_index[AllData$age_group == 3] = "III"
AllData$age_group_index[AllData$age_group == 4] = "IV"
AllData$age_group_index[AllData$age_group == 5] = "V"
AllData$age_group_index[AllData$age_group == 6] = "VI"



#################################################################################################################