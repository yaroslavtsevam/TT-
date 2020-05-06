library(readr)
cores_state <- read_delim("data/cores_state.csv",  ";", escape_double = FALSE, trim_ws = TRUE)
names(cores_state) = c("id","rot_ratio", "state")


cordata = AllData %>% filter( id  %in% cores_state$id) 
cordata = cordata %>% left_join(cores_state, by = "id") %>% as.data.frame()
cordata$state[cordata$state == "good"] = 1
cordata$state[cordata$state == "pith cavity"] = 2
cordata$state[cordata$state == "colored"] = 3
cordata$state[cordata$state == "cavity"] = 4
cordata$state = as.numeric(cordata$state)

cor_mat = cor(cordata[!is.na(cordata$u),c("state","rot_ratio", "accel","gz2","gy2","gx2","VTA_score","u")])
cordata$state %>% rank %>% summary

cordata$u
