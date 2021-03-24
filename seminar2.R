#seminar 2
library(tidyverse)
library(readr)
library(sf)

data <- read_delim("data.csv", ";", escape_double = FALSE, 
                   col_types = cols(Tno = col_double()), 
                   trim_ws = TRUE)

data %>% as.data.frame
as.data.frame(data)

head(data %>% as.data.frame)
tail(data %>% as.data.frame)
print(data, n_extra = 1, n=5)

glimpse(data, width=1)
names(data)
data$`Pruning Index 5 = pruned within 5yrs  10 pruned between 5 and 10yrs`

library(skimr)
skim(data)


#select

data2 = data %>%
  select(N:`Crown Diameter (m)`)
data2


data %>%
  select(-starts_with("Total"))

data %>%
  select(contains("diameter"))%>% data.frame %>% head

data %>% select(-E)


#filter

data$Site %>% unique()

data %>%
  filter(
    Site %in% c("Bowers Ave","Ravensthorpe", "Plumstead Road"),
    Species != "Oak",
    N >= 2 & N <=4) %>% as.data.frame

#arrange

data %>%
  filter(
    Site %in% c("Bowers Ave","Ravensthorpe", "Plumstead Road"),
    Species != "Oak") %>%
  arrange(desc(`dbh (mm)`)) %>% as.data.frame()

#summarise

data %>%
  filter(Species == "Oak") %>%
  summarise(mean_diam = mean(`dbh (m)`), Sites = n_distinct(Site))

#group_by

data %>%
  group_by(Species) %>%
  dplyr::summarise(n=n(), max_diam = max(`dbh (m)`), Sites = n_distinct(Site))

data %>%
  group_by(Species,`Data Set      1=Norwich                0= Peterborough`) %>%
  dplyr::summarise(n = n(), max_diam = max(`dbh (m)`), mean_diam = mean(`dbh (m)`)) %>%
  ggplot()+
  geom_col(mapping = aes(x = Species, y = n))+
  facet_wrap(~`Data Set      1=Norwich                0= Peterborough`)+
  theme_bw()

#mutate

data = data %>%
  mutate(S = as.numeric(S))%>%
  mutate(error = `Predicted crown diamet using combined formulla`-`Crown Diameter (m)`)

#factors
library(forcats)
library(stringr)

names(data)
data$'Soil Code 1=sand and gravel 2= Clay 3=silt'

data = data %>%
  mutate(soil = as_factor(`Soil Code 1=sand and gravel 2= Clay 3=silt`)) %>%
  mutate(soil = fct_recode(soil,`sand and gravel` = "1", clay = "2",silt = "3"))

data$soil %>% as.integer()
data$soil %>% levels

data = data %>%
  dplyr::rename(geology=`Superfical Geology From British Geological Survey Geology of Britain Viewer`)

data = data %>%  
  mutate(is_river = geology %>% str_detect("River")) %>%
  mutate(soil = case_when(
    is_river & soil == "sand and gravel" ~ "river sand and gravel",
    is_river & soil == "clay" ~ "river clay",
    is_river & soil == "silt" ~ "river silt",
    TRUE ~ as.character(soil))) %>%
    mutate(soil = soil %>% fct_relevel("sand and gravel","river sand and gravel","clay",
                                     "river clay","silt","river silt")) %>%
    mutate(soil = soil %>% fct_drop)
  
#units
library(units)

data$`Ht (m)`

data = data %>% 
  mutate(Ht = as.numeric(`Ht (m)`))

units(data$Ht) = as_units("m")
data$time =1
units(data$time) = as_units("s")

#errors
library(errors)
options(errors.notation = "plus-minus", errors.digits=2)
data$`Predicted Crown Diameter` = set_errors(data$`Predicted Crown Diameter`, data$error)
data %>% as.data.frame %>% head

#dates
data$date = "20.07.19" 
data$date[data$`Data Set      1=Norwich                0= Peterborough` == 0] = "15.06.19"

data$date
library(lubridate)

data = data %>% 
  mutate( date = dmy(date))

data = data %>%
  mutate( year = year(date),
          month = month(date),
          day = day(date),
          doy = yday(date))

data %>% as.data.frame()

# Text cleaning
# https://en.wikipedia.org/wiki/Ordnance_Survey_National_Grid
library(stringr)
coord = str_replace_all(data$`Grid Reference`,' ','')
coord_N = str_trunc(coord, 12, "right", ellipsis = "") %>% str_trunc(5,"left", ellipsis = "")
coord_E = str_trunc(coord, 7, "right", ellipsis = "") %>% str_trunc( 5, "left", ellipsis = "")
quadr = str_trunc(coord, 2, "right", ellipsis = "")
table_c = data.frame(as.integer(coord_E), as.integer(coord_N),quadr)


names(table_c)=c("E", "N", "quadr")

head(table_c)

table_c = table_c %>% mutate(Easting_BC = case_when(
              quadr == TF ~ E +600000,
))
table_c = table_c %>% mutate(Northing_BC = case_when(
  quadr == TF ~ N +300000,
))


data$`Grid Reference`

table_c = na.exclude(table_c)

library(sf)

table_WGS = 
  table_c %>%
  st_as_sf(coords = c("Easting_BC", "Northing_BC"), crs = 27700) %>%
  st_transform(4326) %>%
  st_coordinates() %>% as.data.frame()


table_WGS = data.frame(Lat = table_WGS$Y, Lon = table_WGS$X)
table_WGS %>% head


data$`Grid Reference`[1]
table_c[1,]
table_WGS[1,]

coord = cbind(table_c,table_WGS)
head(coord)

data = cbind(data,table_c)
tail(data)
# ???
# Join!
library(dplyr)
data  = left_join(data, coord, by = c("Easting_BC", "Northing_BC"))
tail(data)

data = data %>% filter(!is.na(Lat))

library(ggspatial)
ggplot()+
  annotation_map_tile(zoomin = -1) +
  layer_spatial(data = data %>% st_as_sf(coords = c("Lon", "Lat"), crs = 4326))

#First analysis

data %>% select(Species,Ht, `dbh (m)`)%>% na.exclude()%>%
  group_by(Species) %>%
  dplyr::summarize(correlation = cor(Ht, `dbh (m)`), 
            determination = cor(Ht, `dbh (m)`)^2)

# Transform all to latin 
# maple - Acer platanoides, 
# Oak - Quercus robur,
# Silver birch - Betula pendula, 
# Sycamore - Platanus occidentalis


library(ggbiplot)
library(Hmisc)
library(corrplot)

problem = data %>% summarise_all(is.na) %>% summarise_all(sum) %>% as.vector()
names(problem)
problem_names = names(problem)[problem > 13]

problem = data %>% select_if(is_double) %>%
                   na.exclude() %>% summarise_all(var) %>% as.vector()

problem_var = names(problem)[problem < 1] 
problems = c(problem_var,problem_names)

data_pc = data %>% select(-(problems)) %>% select_if(is_double) %>% na.exclude()
data_t = data %>% select(-(problems))  %>% na.exclude()

data_pco = data_pc %>% prcomp(, center = TRUE, scale = TRUE)

ggbiplot(data_pco,ellipse=TRUE,  groups=data_t$Species, circle = 1, 
         obs.scale = 1)+theme_bw()+ theme(aspect.ratio = 1)

cor.full = cor(data_pc)

col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = cor.full, col = col, symm = TRUE)

data_pc2 = data_pc
names(data_pc2) = str_trunc(names(data_pc), 7,"right",ellipsis = "" )

res3 = rcorr(as.matrix(data_pc2))

corrplot(res3$r, type="upper", order="hclust", 
         p.mat = res3$P, sig.level = 0.01, insig = "blank")


# Models
library(broom)
library(units)
data_lm  = data %>% select(-(problem_names))
data_lm$dbh = set_units(as.double(data_lm$`dbh (m)`),"m")

Model_Ht_Dbh = lm(data = drop_units(data_lm), Ht  ~ dbh^2)
anova(Model_Ht_Dbh)
summary(Model_Ht_Dbh)
TD = tidy((Model_Ht_Dbh))
TD$estimate

library(kableExtra)
kbl(tidy((Model_Ht_Dbh))) %>% kable_styling()


# #################################
# Все переменные имеют корректный тип данных
# Повторяющиеся переменные убраны
# Из имен переменных убраны размерности
# Всем переменам заданы их реальные размерности
# Если какая-то переменная является ошибкой другой переменной, она должна быть убрана и добавлена в виде ошибки к основной переменной
# Категориальные переменные должны быть факторами
# Категории переменной из имени должны быть убраны
# Коды категориальных переменных заменены их категориями
# Должны быть созданы переменные координат(lat,lon) в британской системе координат(с учетом кодов квадратов) и в WGS84
# Виды должны быть переименованы на латыне
# #########################################

library(tidyverse)
library(units)
data <- read_delim("data.csv", ";", escape_double = FALSE, 
                   col_types = cols(Tno = col_double(), 
                                    `Age Index 1=Y 2=SM 3=EM 4=M` = col_integer(), 
                                    HR = col_character(), `% Variation` = col_double()),
                   locale = locale(decimal_mark = ","),
                   trim_ws = TRUE)


str(data)


# Убрать колонку из данных
data = data %>% select(-`dbh (mm)`, -HR)
data = data %>% select( -HR)

# Изменение имени переменной
data = data %>% rename(dbh = `dbh (m)`)


# Добавление размерности переменной
units(data$dbh) = as_units("m")
data %>% as.data.frame()

# Переделка переменной в категориальную
# 1 = Y, 2=SM, 3=EM, 4 =M

data = data %>% rename(age_index = `Age Index 1=Y 2=SM 3=EM 4=M`) 

data$age_index = as.factor(data$age_index)
levels(data$age_index) = c("Y","SM","EM","M")
data$age_index

data$age_index[data$age_index == "<NA>"]

as.integer(data$age_index)

# Проверить наличие пустых значений
sum(is.na(data$age_index))


# Переименовывание видов
#Oak - Quercus robur

data$Species[data$Species == "Oak"] = "Quercus robur"
data$Species[data$Species == "Quercus robur"] = "Oak"

data = data %>% mutate(Species = case_when(
                  Species == "Oak" ~ "Quercus robur",
                  Species == "Silver Birch" ~ "Betula pendula",
                  TRUE ~ Species
))
# Список уникальных*(без повторов) значений в переменной
data$Species %>% unique()
