boris_data <- read.csv("data/copy_RAW_Texas_BORISdata_20240110.csv")
ID_data <- read.csv(("data/copy_trial_ID_data_20240110.csv"))
boris_short <- boris_data[,c(1,5,7,8,12:14)]

trial = 1
ids = unique(ID_data$fish_ID)

site = unique(substr(boris_short$Observation.id, start=1, stop=4))

wesrows <- grep("WES|Wes", boris_short$Observation.id)
brrows <- grep("BR", boris_short$Observation.id)



for (i in 1:length(ids)){
  
}