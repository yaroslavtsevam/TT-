library(tidyverse)
library(openxlsx)
LAI <- read_csv("C:/Users/Alex/Downloads/LAI.csv")


names(LAI)%>% sort()

data  = LAI %>% select(id, time, month, doy,tair,prcp_ts,TTair, DBH, tree_height, growth, LAI, PAI, Flux, pm10) %>% 
        rename(prcp = prcp_ts, tair_canopy = tair, tair_outside = TTair ) %>% group_by(id,doy,month)%>%
          summarise(
            tair_canopy = mean(tair_canopy, na.rm = T),
            tair_outside = mean(tair_outside, na.rm = T),
            prcp = sum(prcp, na.rm = T),
            DBH = mean(DBH, na.rm = T),
            tree_height =  mean(tree_height, na.rm=T),
            growth = mean(growth, na.rm=T),
            LAI = mean(LAI, na.rm=T),
            PAI = mean(PAI, na.rm=T),
            Flux = sum(Flux, na.rm=T),
            pm10 = mean(pm10, na.rm=T)
          )


for (m in 8:10) {
  for (ids in unique(data$id)) {
    write.csv(data %>% filter(id == ids, month == m), file=paste(ids,"_",m,".csv", sep="" ))
  }
}

wb <- createWorkbook()


for (m in 8:10) {
  for (ids in unique(data$id)) {
    addWorksheet(wb, paste(ids,"_",m, sep = "" ))
    x = data %>% filter(id == ids, month == m)
    sheetname = paste(ids,"_",m, sep="" )
    writeData(wb, sheetname,x,startCol = 1, startRow = 1, rowNames = F)
  }
}

saveWorkbook(wb, "TT_ES_3MUGIS.xlsx", overwrite = TRUE)
