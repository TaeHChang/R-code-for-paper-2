
###################################################
### Data extraction from global map ###############
###################################################


## terrestrial grid
library(sf)
library(rgdal)
library(raster)
library(rgeos)
library(dismo)
library(tidyverse)

library(doParallel)
cl<-makeCluster(2)
registerDoParallel(cl)
detectCores()
registerDoParallel(cores=12)
getDoParWorkers()

setwd("Hendra-Australia\\AUS_basemap\\final")
terrestrial.grid <- readOGR(dsn = "Basemap_clipped_numpoints_final.shp")
plot(terrestrial.grid)

ID <- terrestrial.grid@data %>% dplyr::select(id)
area_temp <- terrestrial.grid@data %>% dplyr::select(id, area)

###################################################
############### Maxent raster count ###############
###################################################

##################### Chiroptera

list_chiroptera_habitat_filtered <- readRDS(file = "Maxent\\Maxent_raster\\list_chiroptera_habitat_filtered.rds") 

a<-Sys.time()
chiroptera.mat<- foreach(i = 1:length(list_chiroptera_habitat_filtered),.combine = 'cbind',.multicombine = T,.packages = c("raster"),.errorhandling = 'pass')%:%
  foreach(j = 1:nrow(terrestrial.grid@data),.packages = c("raster"),.errorhandling = 'pass')%dopar%{
    cropped.grid<-crop(list_chiroptera_habitat_filtered[[i]], extent(terrestrial.grid@data$left[j],terrestrial.grid@data$right[j],terrestrial.grid@data$bottom[j],terrestrial.grid@data$top[j]))
    chiroptera.temp<-sum(getValues(cropped.grid),na.rm=T)
    return(chiroptera.temp)
  }
b<-Sys.time()
b-a

chiroptera.mat <- as.data.frame(chiroptera.mat)
chiroptera.mat <- cbind(ID, chiroptera.mat)

area_temp <- unique(area_temp)

chiroptera.mat_1 <- chiroptera.mat %>% 
  left_join(area_temp, by = c("id" = "id"))
chiroptera_mat_1 <- chiroptera.mat_1 %>% 
  mutate(pixels = area / 20000000)

chiroptera.mat_1 <- apply(chiroptera.mat_1,2,as.character)

write.csv(chiroptera.mat_1,"Maxent\\Species_mat\\chiroptera_mat.csv")
chiroptera_mat_1 <- read.csv("Maxent\\Species_mat\\chiroptera_mat.csv")

len <- length(colnames(chiroptera_mat_1))

for (i in 2:(len-2)){
  chiroptera_mat_1[i + (len-1)] <- (chiroptera_mat_1[i] > (chiroptera_mat_1[,"pixels"] / 2))
}

chiroptera_mat_1$count_chiroptera_prop50 <- rowSums(chiroptera_mat_1[,(len+1):(len + len - 3)])

chiroptera_prop50 <- chiroptera_mat_1 %>% 
  dplyr::select(id, count_chiroptera_prop50)


write.csv(chiroptera_prop50,"Maxent\\Species_mat\\chiroptera_prop50_count.csv")

########################### 25% 계산
chiroptera_mat_1<- read.csv("Maxent\\Species_mat\\chiroptera_mat.csv")

chiroptera_mat_1 <- chiroptera_mat_1 %>% 
  mutate(pixels = area / 20000000)

len <- length(colnames(chiroptera_mat_1))


for (i in 2:(len-2)){
  chiroptera_mat_1[i + (len-1)] <- (chiroptera_mat_1[i] > (chiroptera_mat_1[,"pixels"] / 4))
}

chiroptera_mat_1$count_chiroptera_prop25 <- rowSums(chiroptera_mat_1[,(len+1):(len + len - 3)])

chiroptera_prop25 <- chiroptera_mat_1 %>% 
  dplyr::select(id, count_chiroptera_prop25)

write.csv(chiroptera_prop25,"Maxent\\Species_mat\\chiroptera_prop25_count.csv")


########################### 75% 계산
chiroptera_mat_1<- read.csv("Maxent\\Species_mat\\chiroptera_mat.csv")

chiroptera_mat_1 <- chiroptera_mat_1 %>% 
  mutate(pixels = area / 20000000)

len <- length(colnames(chiroptera_mat_1))


for (i in 2:(len-2)){
  chiroptera_mat_1[i + (len-1)] <- (chiroptera_mat_1[i] > ((chiroptera_mat_1[,"pixels"] / 4) * 3))
}

chiroptera_mat_1$count_chiroptera_prop75 <- rowSums(chiroptera_mat_1[,(len+1):(len + len - 3)])

chiroptera_prop75 <- chiroptera_mat_1 %>% 
  dplyr::select(id, count_chiroptera_prop75)

write.csv(chiroptera_prop75,"Maxent\\Species_mat\\chiroptera_prop75_count.csv")






##################### Accipitriformes

list_accipitriformes_habitat_filtered <- readRDS(file = "Maxent\\Maxent_raster\\list_accipitriformes_habitat_filtered.rds") 

a<-Sys.time()
accipitriformes.mat<- foreach(i = 1:length(list_accipitriformes_habitat_filtered),.combine = 'cbind',.multicombine = T,.packages = c("raster"),.errorhandling = 'pass')%:%
  foreach(j = 1:nrow(terrestrial.grid@data),.packages = c("raster"),.errorhandling = 'pass')%dopar%{
    cropped.grid<-crop(list_accipitriformes_habitat_filtered[[i]], extent(terrestrial.grid@data$left[j],terrestrial.grid@data$right[j],terrestrial.grid@data$bottom[j],terrestrial.grid@data$top[j]))
    accipitriformes.temp<-sum(getValues(cropped.grid),na.rm=T)
    return(accipitriformes.temp)
  }
b<-Sys.time()
b-a

accipitriformes.mat <- as.data.frame(accipitriformes.mat)
accipitriformes.mat <- cbind(ID, accipitriformes.mat)

area_temp <- unique(area_temp)

accipitriformes.mat_1 <- accipitriformes.mat %>% 
  left_join(area_temp, by = c("id" = "id"))
accipitriformes_mat_1 <- accipitriformes.mat_1 %>% 
  mutate(pixels = area / 20000000)

accipitriformes.mat_1 <- apply(accipitriformes.mat_1,2,as.character)

write.csv(accipitriformes.mat_1,"Maxent\\Species_mat\\accipitriformes_mat.csv")
accipitriformes_mat_1 <- read.csv("Maxent\\Species_mat\\accipitriformes_mat.csv")

accipitriformes_mat_1 <- accipitriformes_mat_1 %>% 
  mutate(pixels = area / 20000000)

len <- length(colnames(accipitriformes_mat_1))

for (i in 2:(len-2)){
  accipitriformes_mat_1[i + (len-1)] <- (accipitriformes_mat_1[i] > (accipitriformes_mat_1[,"pixels"] / 2))
}

accipitriformes_mat_1$count_accipitriformes_prop50 <- rowSums(accipitriformes_mat_1[,(len+1):(len + len - 3)])

accipitriformes_prop50 <- accipitriformes_mat_1 %>% 
  dplyr::select(id, count_accipitriformes_prop50)


write.csv(accipitriformes_prop50,"Maxent\\Species_mat\\accipitriformes_prop50_count.csv")

########################### 25% 계산
accipitriformes_mat_1<- read.csv("Maxent\\Species_mat\\accipitriformes_mat.csv")

accipitriformes_mat_1 <- accipitriformes_mat_1 %>% 
  mutate(pixels = area / 20000000)

len <- length(colnames(accipitriformes_mat_1))


for (i in 2:(len-2)){
  accipitriformes_mat_1[i + (len-1)] <- (accipitriformes_mat_1[i] > (accipitriformes_mat_1[,"pixels"] / 4))
}

accipitriformes_mat_1$count_accipitriformes_prop25 <- rowSums(accipitriformes_mat_1[,(len+1):(len + len - 3)])

accipitriformes_prop25 <- accipitriformes_mat_1 %>% 
  dplyr::select(id, count_accipitriformes_prop25)

write.csv(accipitriformes_prop25,"Maxent\\Species_mat\\accipitriformes_prop25_count.csv")


########################### 75% 계산
accipitriformes_mat_1<- read.csv("Maxent\\Species_mat\\accipitriformes_mat.csv")

accipitriformes_mat_1 <- accipitriformes_mat_1 %>% 
  mutate(pixels = area / 20000000)

len <- length(colnames(accipitriformes_mat_1))


for (i in 2:(len-2)){
  accipitriformes_mat_1[i + (len-1)] <- (accipitriformes_mat_1[i] > ((accipitriformes_mat_1[,"pixels"] / 4) * 3))
}

accipitriformes_mat_1$count_accipitriformes_prop75 <- rowSums(accipitriformes_mat_1[,(len+1):(len + len - 3)])

accipitriformes_prop75 <- accipitriformes_mat_1 %>% 
  dplyr::select(id, count_accipitriformes_prop75)

write.csv(accipitriformes_prop75,"Maxent\\Species_mat\\accipitriformes_prop75_count.csv")




##################### Strigiformes

list_strigiformes_habitat_filtered <- readRDS(file = "Maxent\\Maxent_raster\\list_strigiformes_habitat_filtered.rds") 

a<-Sys.time()
strigiformes.mat<- foreach(i = 1:length(list_strigiformes_habitat_filtered),.combine = 'cbind',.multicombine = T,.packages = c("raster"),.errorhandling = 'pass')%:%
  foreach(j = 1:nrow(terrestrial.grid@data),.packages = c("raster"),.errorhandling = 'pass')%dopar%{
    cropped.grid<-crop(list_strigiformes_habitat_filtered[[i]], extent(terrestrial.grid@data$left[j],terrestrial.grid@data$right[j],terrestrial.grid@data$bottom[j],terrestrial.grid@data$top[j]))
    strigiformes.temp<-sum(getValues(cropped.grid),na.rm=T)
    return(strigiformes.temp)
  }
b<-Sys.time()
b-a

strigiformes.mat <- as.data.frame(strigiformes.mat)
strigiformes.mat <- cbind(ID, strigiformes.mat)

area_temp <- unique(area_temp)

strigiformes.mat_1 <- strigiformes.mat %>% 
  left_join(area_temp, by = c("id" = "id"))
strigiformes_mat_1 <- strigiformes.mat_1 %>% 
  mutate(pixels = area / 20000000)

strigiformes.mat_1 <- apply(strigiformes.mat_1,2,as.character)

write.csv(strigiformes.mat_1,"Maxent\\Species_mat\\strigiformes_mat.csv")
strigiformes_mat_1 <- read.csv("Maxent\\Species_mat\\strigiformes_mat.csv")

strigiformes_mat_1 <- strigiformes_mat_1 %>% 
  mutate(pixels = area / 20000000)

len <- length(colnames(strigiformes_mat_1))

for (i in 2:(len-2)){
  strigiformes_mat_1[i + (len-1)] <- (strigiformes_mat_1[i] > (strigiformes_mat_1[,"pixels"] / 2))
}

strigiformes_mat_1$count_strigiformes_prop50 <- rowSums(strigiformes_mat_1[,(len+1):(len + len - 3)])

strigiformes_prop50 <- strigiformes_mat_1 %>% 
  dplyr::select(id, count_strigiformes_prop50)


write.csv(strigiformes_prop50,"Maxent\\Species_mat\\strigiformes_prop50_count.csv")

########################### 25% 계산
strigiformes_mat_1<- read.csv("Maxent\\Species_mat\\strigiformes_mat.csv")

strigiformes_mat_1 <- strigiformes_mat_1 %>% 
  mutate(pixels = area / 20000000)

len <- length(colnames(strigiformes_mat_1))


for (i in 2:(len-2)){
  strigiformes_mat_1[i + (len-1)] <- (strigiformes_mat_1[i] > (strigiformes_mat_1[,"pixels"] / 4))
}

strigiformes_mat_1$count_strigiformes_prop25 <- rowSums(strigiformes_mat_1[,(len+1):(len + len - 3)])

strigiformes_prop25 <- strigiformes_mat_1 %>% 
  dplyr::select(id, count_strigiformes_prop25)

write.csv(strigiformes_prop25,"Maxent\\Species_mat\\strigiformes_prop25_count.csv")


########################### 75% 계산
strigiformes_mat_1<- read.csv("Maxent\\Species_mat\\strigiformes_mat.csv")

strigiformes_mat_1 <- strigiformes_mat_1 %>% 
  mutate(pixels = area / 20000000)

len <- length(colnames(strigiformes_mat_1))


for (i in 2:(len-2)){
  strigiformes_mat_1[i + (len-1)] <- (strigiformes_mat_1[i] > ((strigiformes_mat_1[,"pixels"] / 4) * 3))
}

strigiformes_mat_1$count_strigiformes_prop75 <- rowSums(strigiformes_mat_1[,(len+1):(len + len - 3)])

strigiformes_prop75 <- strigiformes_mat_1 %>% 
  dplyr::select(id, count_strigiformes_prop75)

write.csv(strigiformes_prop75,"Maxent\\Species_mat\\strigiformes_prop75_count.csv")





##################### Carnivora

list_carnivora_habitat_filtered <- readRDS(file = "Maxent\\Maxent_raster\\list_carnivora_habitat_filtered.rds") 

a<-Sys.time()
carnivora.mat<- foreach(i = 1:length(list_carnivora_habitat_filtered),.combine = 'cbind',.multicombine = T,.packages = c("raster"),.errorhandling = 'pass')%:%
  foreach(j = 1:nrow(terrestrial.grid@data),.packages = c("raster"),.errorhandling = 'pass')%dopar%{
    cropped.grid<-crop(list_carnivora_habitat_filtered[[i]], extent(terrestrial.grid@data$left[j],terrestrial.grid@data$right[j],terrestrial.grid@data$bottom[j],terrestrial.grid@data$top[j]))
    carnivora.temp<-sum(getValues(cropped.grid),na.rm=T)
    return(carnivora.temp)
  }
b<-Sys.time()
b-a

carnivora.mat <- as.data.frame(carnivora.mat)
carnivora.mat <- cbind(ID, carnivora.mat)

area_temp <- unique(area_temp)

carnivora.mat_1 <- carnivora.mat %>% 
  left_join(area_temp, by = c("id" = "id"))
carnivora_mat_1 <- carnivora.mat_1 %>% 
  mutate(pixels = area / 20000000)

carnivora.mat_1 <- apply(carnivora.mat_1,2,as.character)

write.csv(carnivora.mat_1,"Maxent\\Species_mat\\carnivora_mat.csv")
carnivora_mat_1 <- read.csv("Maxent\\Species_mat\\carnivora_mat.csv")

carnivora_mat_1 <- carnivora_mat_1 %>% 
  mutate(pixels = area / 20000000)

len <- length(colnames(carnivora_mat_1))

for (i in 2:(len-2)){
  carnivora_mat_1[i + (len-1)] <- (carnivora_mat_1[i] > (carnivora_mat_1[,"pixels"] / 2))
}

carnivora_mat_1$count_carnivora_prop50 <- rowSums(carnivora_mat_1[,(len+1):(len + len - 3)])

carnivora_prop50 <- carnivora_mat_1 %>% 
  dplyr::select(id, count_carnivora_prop50)


write.csv(carnivora_prop50,"Maxent\\Species_mat\\carnivora_prop50_count.csv")

########################### 25% 계산
carnivora_mat_1<- read.csv("Maxent\\Species_mat\\carnivora_mat.csv")

carnivora_mat_1 <- carnivora_mat_1 %>% 
  mutate(pixels = area / 20000000)

len <- length(colnames(carnivora_mat_1))


for (i in 2:(len-2)){
  carnivora_mat_1[i + (len-1)] <- (carnivora_mat_1[i] > (carnivora_mat_1[,"pixels"] / 4))
}

carnivora_mat_1$count_carnivora_prop25 <- rowSums(carnivora_mat_1[,(len+1):(len + len - 3)])

carnivora_prop25 <- carnivora_mat_1 %>% 
  dplyr::select(id, count_carnivora_prop25)

write.csv(carnivora_prop25,"Maxent\\Species_mat\\carnivora_prop25_count.csv")


########################### 75% 계산
carnivora_mat_1<- read.csv("Maxent\\Species_mat\\carnivora_mat.csv")

carnivora_mat_1 <- carnivora_mat_1 %>% 
  mutate(pixels = area / 20000000)

len <- length(colnames(carnivora_mat_1))


for (i in 2:(len-2)){
  carnivora_mat_1[i + (len-1)] <- (carnivora_mat_1[i] > ((carnivora_mat_1[,"pixels"] / 4) * 3))
}

carnivora_mat_1$count_carnivora_prop75 <- rowSums(carnivora_mat_1[,(len+1):(len + len - 3)])

carnivora_prop75 <- carnivora_mat_1 %>% 
  dplyr::select(id, count_carnivora_prop75)

write.csv(carnivora_prop75,"Maxent\\Species_mat\\carnivora_prop75_count.csv")





##################### Colubridae

list_colubridae_habitat_filtered <- readRDS(file = "Maxent\\Maxent_raster\\list_colubridae_habitat_filtered.rds") 

a<-Sys.time()
colubridae.mat<- foreach(i = 1:length(list_colubridae_habitat_filtered),.combine = 'cbind',.multicombine = T,.packages = c("raster"),.errorhandling = 'pass')%:%
  foreach(j = 1:nrow(terrestrial.grid@data),.packages = c("raster"),.errorhandling = 'pass')%dopar%{
    cropped.grid<-crop(list_colubridae_habitat_filtered[[i]], extent(terrestrial.grid@data$left[j],terrestrial.grid@data$right[j],terrestrial.grid@data$bottom[j],terrestrial.grid@data$top[j]))
    colubridae.temp<-sum(getValues(cropped.grid),na.rm=T)
    return(colubridae.temp)
  }
b<-Sys.time()
b-a

colubridae.mat <- as.data.frame(colubridae.mat)
colubridae.mat <- cbind(ID, colubridae.mat)

area_temp <- unique(area_temp)

colubridae.mat_1 <- colubridae.mat %>% 
  left_join(area_temp, by = c("id" = "id"))
colubridae_mat_1 <- colubridae.mat_1 %>% 
  mutate(pixels = area / 20000000)

colubridae.mat_1 <- apply(colubridae.mat_1,2,as.character)

write.csv(colubridae.mat_1,"Maxent\\Species_mat\\colubridae_mat.csv")
colubridae_mat_1 <- read.csv("Maxent\\Species_mat\\colubridae_mat.csv")

colubridae_mat_1 <- colubridae_mat_1 %>% 
  mutate(pixels = area / 20000000)

len <- length(colnames(colubridae_mat_1))

for (i in 2:(len-2)){
  colubridae_mat_1[i + (len-1)] <- (colubridae_mat_1[i] > (colubridae_mat_1[,"pixels"] / 2))
}

colubridae_mat_1$count_colubridae_prop50 <- rowSums(colubridae_mat_1[,(len+1):(len + len - 3)])

colubridae_prop50 <- colubridae_mat_1 %>% 
  dplyr::select(id, count_colubridae_prop50)


write.csv(colubridae_prop50,"Maxent\\Species_mat\\colubridae_prop50_count.csv")

########################### 25% 계산
colubridae_mat_1<- read.csv("Maxent\\Species_mat\\colubridae_mat.csv")

colubridae_mat_1 <- colubridae_mat_1 %>% 
  mutate(pixels = area / 20000000)

len <- length(colnames(colubridae_mat_1))


for (i in 2:(len-2)){
  colubridae_mat_1[i + (len-1)] <- (colubridae_mat_1[i] > (colubridae_mat_1[,"pixels"] / 4))
}

colubridae_mat_1$count_colubridae_prop25 <- rowSums(colubridae_mat_1[,(len+1):(len + len - 3)])

colubridae_prop25 <- colubridae_mat_1 %>% 
  dplyr::select(id, count_colubridae_prop25)

write.csv(colubridae_prop25,"Maxent\\Species_mat\\colubridae_prop25_count.csv")


########################### 75% 계산
colubridae_mat_1<- read.csv("Maxent\\Species_mat\\colubridae_mat.csv")

colubridae_mat_1 <- colubridae_mat_1 %>% 
  mutate(pixels = area / 20000000)

len <- length(colnames(colubridae_mat_1))


for (i in 2:(len-2)){
  colubridae_mat_1[i + (len-1)] <- (colubridae_mat_1[i] > ((colubridae_mat_1[,"pixels"] / 4) * 3))
}

colubridae_mat_1$count_colubridae_prop75 <- rowSums(colubridae_mat_1[,(len+1):(len + len - 3)])

colubridae_prop75 <- colubridae_mat_1 %>% 
  dplyr::select(id, count_colubridae_prop75)

write.csv(colubridae_prop75,"Maxent\\Species_mat\\colubridae_prop75_count.csv")





##################### Pythonidae

list_pythonidae_habitat_filtered <- readRDS(file = "Maxent\\Maxent_raster\\list_pythonidae_habitat_filtered.rds") 

a<-Sys.time()
pythonidae.mat<- foreach(i = 1:length(list_pythonidae_habitat_filtered),.combine = 'cbind',.multicombine = T,.packages = c("raster"),.errorhandling = 'pass')%:%
  foreach(j = 1:nrow(terrestrial.grid@data),.packages = c("raster"),.errorhandling = 'pass')%dopar%{
    cropped.grid<-crop(list_pythonidae_habitat_filtered[[i]], extent(terrestrial.grid@data$left[j],terrestrial.grid@data$right[j],terrestrial.grid@data$bottom[j],terrestrial.grid@data$top[j]))
    pythonidae.temp<-sum(getValues(cropped.grid),na.rm=T)
    return(pythonidae.temp)
  }
b<-Sys.time()
b-a

pythonidae.mat <- as.data.frame(pythonidae.mat)
pythonidae.mat <- cbind(ID, pythonidae.mat)

area_temp <- unique(area_temp)

pythonidae.mat_1 <- pythonidae.mat %>% 
  left_join(area_temp, by = c("id" = "id"))
pythonidae_mat_1 <- pythonidae.mat_1 %>% 
  mutate(pixels = area / 20000000)

pythonidae.mat_1 <- apply(pythonidae.mat_1,2,as.character)

write.csv(pythonidae.mat_1,"Maxent\\Species_mat\\pythonidae_mat.csv")
pythonidae_mat_1 <- read.csv("Maxent\\Species_mat\\pythonidae_mat.csv")

pythonidae_mat_1 <- pythonidae_mat_1 %>% 
  mutate(pixels = area / 20000000)

len <- length(colnames(pythonidae_mat_1))

for (i in 2:(len-2)){
  pythonidae_mat_1[i + (len-1)] <- (pythonidae_mat_1[i] > (pythonidae_mat_1[,"pixels"] / 2))
}

pythonidae_mat_1$count_pythonidae_prop50 <- rowSums(pythonidae_mat_1[,(len+1):(len + len - 3)])

pythonidae_prop50 <- pythonidae_mat_1 %>% 
  dplyr::select(id, count_pythonidae_prop50)


write.csv(pythonidae_prop50,"Maxent\\Species_mat\\pythonidae_prop50_count.csv")

########################### 25% 계산
pythonidae_mat_1<- read.csv("Maxent\\Species_mat\\pythonidae_mat.csv")

pythonidae_mat_1 <- pythonidae_mat_1 %>% 
  mutate(pixels = area / 20000000)

len <- length(colnames(pythonidae_mat_1))


for (i in 2:(len-2)){
  pythonidae_mat_1[i + (len-1)] <- (pythonidae_mat_1[i] > (pythonidae_mat_1[,"pixels"] / 4))
}

pythonidae_mat_1$count_pythonidae_prop25 <- rowSums(pythonidae_mat_1[,(len+1):(len + len - 3)])

pythonidae_prop25 <- pythonidae_mat_1 %>% 
  dplyr::select(id, count_pythonidae_prop25)

write.csv(pythonidae_prop25,"Maxent\\Species_mat\\pythonidae_prop25_count.csv")


########################### 75% 계산
pythonidae_mat_1<- read.csv("Maxent\\Species_mat\\pythonidae_mat.csv")

pythonidae_mat_1 <- pythonidae_mat_1 %>% 
  mutate(pixels = area / 20000000)

len <- length(colnames(pythonidae_mat_1))


for (i in 2:(len-2)){
  pythonidae_mat_1[i + (len-1)] <- (pythonidae_mat_1[i] > ((pythonidae_mat_1[,"pixels"] / 4) * 3))
}

pythonidae_mat_1$count_pythonidae_prop75 <- rowSums(pythonidae_mat_1[,(len+1):(len + len - 3)])

pythonidae_prop75 <- pythonidae_mat_1 %>% 
  dplyr::select(id, count_pythonidae_prop75)

write.csv(pythonidae_prop75,"Maxent\\Species_mat\\pythonidae_prop75_count.csv")







##################### Falconiformes

list_falconiformes_habitat_filtered <- readRDS(file = "Maxent\\Maxent_raster\\list_falconiformes_habitat_filtered.rds") 

a<-Sys.time()
falconiformes.mat<- foreach(i = 1:length(list_falconiformes_habitat_filtered),.combine = 'cbind',.multicombine = T,.packages = c("raster"),.errorhandling = 'pass')%:%
  foreach(j = 1:nrow(terrestrial.grid@data),.packages = c("raster"),.errorhandling = 'pass')%dopar%{
    cropped.grid<-crop(list_falconiformes_habitat_filtered[[i]], extent(terrestrial.grid@data$left[j],terrestrial.grid@data$right[j],terrestrial.grid@data$bottom[j],terrestrial.grid@data$top[j]))
    falconiformes.temp<-sum(getValues(cropped.grid),na.rm=T)
    return(falconiformes.temp)
  }
b<-Sys.time()
b-a

falconiformes.mat <- as.data.frame(falconiformes.mat)
falconiformes.mat <- cbind(ID, falconiformes.mat)

area_temp <- unique(area_temp)

falconiformes.mat_1 <- falconiformes.mat %>% 
  left_join(area_temp, by = c("id" = "id"))
falconiformes_mat_1 <- falconiformes.mat_1 %>% 
  mutate(pixels = area / 20000000)

falconiformes.mat_1 <- apply(falconiformes.mat_1,2,as.character)

write.csv(falconiformes.mat_1,"Maxent\\Species_mat\\falconiformes_mat.csv")
falconiformes_mat_1 <- read.csv("Maxent\\Species_mat\\falconiformes_mat.csv")

falconiformes_mat_1 <- falconiformes_mat_1 %>% 
  mutate(pixels = area / 20000000)

len <- length(colnames(falconiformes_mat_1))

for (i in 2:(len-2)){
  falconiformes_mat_1[i + (len-1)] <- (falconiformes_mat_1[i] > (falconiformes_mat_1[,"pixels"] / 2))
}

falconiformes_mat_1$count_falconiformes_prop50 <- rowSums(falconiformes_mat_1[,(len+1):(len + len - 3)])

falconiformes_prop50 <- falconiformes_mat_1 %>% 
  dplyr::select(id, count_falconiformes_prop50)


write.csv(falconiformes_prop50,"Maxent\\Species_mat\\falconiformes_prop50_count.csv")

########################### 25% 계산
falconiformes_mat_1<- read.csv("Maxent\\Species_mat\\falconiformes_mat.csv")

falconiformes_mat_1 <- falconiformes_mat_1 %>% 
  mutate(pixels = area / 20000000)

len <- length(colnames(falconiformes_mat_1))


for (i in 2:(len-2)){
  falconiformes_mat_1[i + (len-1)] <- (falconiformes_mat_1[i] > (falconiformes_mat_1[,"pixels"] / 4))
}

falconiformes_mat_1$count_falconiformes_prop25 <- rowSums(falconiformes_mat_1[,(len+1):(len + len - 3)])

falconiformes_prop25 <- falconiformes_mat_1 %>% 
  dplyr::select(id, count_falconiformes_prop25)

write.csv(falconiformes_prop25,"Maxent\\Species_mat\\falconiformes_prop25_count.csv")


########################### 75% 계산
falconiformes_mat_1<- read.csv("Maxent\\Species_mat\\falconiformes_mat.csv")

falconiformes_mat_1 <- falconiformes_mat_1 %>% 
  mutate(pixels = area / 20000000)

len <- length(colnames(falconiformes_mat_1))


for (i in 2:(len-2)){
  falconiformes_mat_1[i + (len-1)] <- (falconiformes_mat_1[i] > ((falconiformes_mat_1[,"pixels"] / 4) * 3))
}

falconiformes_mat_1$count_falconiformes_prop75 <- rowSums(falconiformes_mat_1[,(len+1):(len + len - 3)])

falconiformes_prop75 <- falconiformes_mat_1 %>% 
  dplyr::select(id, count_falconiformes_prop75)

write.csv(falconiformes_prop75,"Maxent\\Species_mat\\falconiformes_prop75_count.csv")
