
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

setwd("D:\\Environmental data\\Hendra-Australia\\AUS_basemap\\final")
terrestrial.grid <- readOGR(dsn = "Basemap_clipped_numpoints_final.shp")
plot(terrestrial.grid)

ID <- terrestrial.grid@data %>% dplyr::select(id)
area_temp <- terrestrial.grid@data %>% dplyr::select(id, area)

###################################################
############### Maxent raster count ###############
###################################################

##################### Chiroptera

list_chiroptera_habitat_filtered <- readRDS(file = "D:\\Environmental data\\Hendra-Australia\\Maxent\\Maxent_raster\\list_chiroptera_habitat_filtered.rds") 

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

area <- unique(area)

chiroptera.mat_1 <- chiroptera.mat %>% 
  left_join(area_temp, by = c("id" = "id"))
chiroptera_mat_1 <- chiroptera.mat_1 %>% 
  mutate(pixels = area / 20000000)

chiroptera.mat_1 <- apply(chiroptera.mat_1,2,as.character)

write.csv(chiroptera.mat_1,"D:\\Environmental data\\Hendra-Australia\\Maxent\\Species_mat\\chiroptera_mat.csv")
chiroptera_mat_1<- read.csv("D:\\Environmental data\\Hendra-Australia\\Maxent\\Species_mat\\chiroptera_mat.csv")

for (i in 2:50) {
  chiroptera_mat_1[i + 51] <- (chiroptera_mat_1[i] > (chiroptera_mat_1[,"pixels"] / 2))
}

chiroptera_mat_1$count_chiroptera_prop50 <- rowSums(chiroptera_mat_1[,53:101])

chiroptera_prop50 <- chiroptera_mat_1 %>% 
  dplyr::select(id, count_chiroptera_prop50)


write.csv(chiroptera_prop50,"D:\\Environmental data\\Hendra-Australia\\Maxent\\Species_mat\\chiroptera_prop50_count.csv")

########################### existence 계산
chiroptera_mat_1<- read.csv("D:\\Environmental data\\Hendra-Australia\\Maxent\\Species_mat\\chiroptera_mat.csv")

for (i in 2:50) {
  chiroptera_mat_1[i + 51] <- (chiroptera_mat_1[i] > 0) 
}

chiroptera_mat_1$count_chiroptera_existence <- rowSums(chiroptera_mat_1[,53:101])

chiroptera_existence <- chiroptera_mat_1 %>% 
  dplyr::select(id, count_chiroptera_existence)


##################### Accipitriformes

list_accipitriformes_habitat_filtered <- readRDS(file = "D:\\Environmental data\\Hendra-Australia\\Maxent\\Maxent_raster\\list_accipitriformes_habitat_filtered.rds") 

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


accipitriformes.mat_1 <- accipitriformes.mat %>% 
  left_join(area_temp, by = c("id" = "id"))
accipitriformes_mat_1 <- accipitriformes.mat_1 %>% 
  mutate(pixels = area / 20000000)

accipitriformes.mat_1 <- apply(accipitriformes.mat_1,2,as.character)

write.csv(accipitriformes.mat_1,"D:\\Environmental data\\Hendra-Australia\\Maxent\\Species_mat\\accipitriformes_mat.csv")
accipitriformes_mat_1<- read.csv("D:\\Environmental data\\Hendra-Australia\\Maxent\\Species_mat\\accipitriformes_mat.csv")

for (i in 2:17) {
  accipitriformes_mat_1[i + 18] <- (accipitriformes_mat_1[i] > (accipitriformes_mat_1[,"pixels"] / 2))
}

accipitriformes_mat_1$count_accipitriformes_prop50 <- rowSums(accipitriformes_mat_1[,20:35])

accipitriformes_prop50 <- accipitriformes_mat_1 %>% 
  dplyr::select(id, count_accipitriformes_prop50)


write.csv(accipitriformes_prop50,"D:\\Environmental data\\Hendra-Australia\\Maxent\\Species_mat\\accipitriformes_prop50_count.csv")

########################### existence 계산
accipitriformes_mat_1 <- read.csv("D:/Environmental data/Hendra-Australia/Maxent/Species_mat/accipitriformes_mat.csv")


for (i in 3:18) {
  accipitriformes_mat_1[i + 16] <- (accipitriformes_mat_1[i] > 0)
}

accipitriformes_mat_1$count_accipitriformes_existence <- rowSums(accipitriformes_mat_1[,19:34])

accipitriformes_existence <- accipitriformes_mat_1 %>% 
  dplyr::select(id, count_accipitriformes_existence)







##################### Strigiformes

list_strigiformes_habitat_filtered <- readRDS(file = "D:\\Environmental data\\Hendra-Australia\\Maxent\\Maxent_raster\\list_strigiformes_habitat_filtered.rds") 

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


strigiformes.mat_1 <- strigiformes.mat %>% 
  left_join(area_temp, by = c("id" = "id"))
strigiformes_mat_1 <- strigiformes.mat_1 %>% 
  mutate(pixels = area / 20000000)

strigiformes.mat_1 <- apply(strigiformes.mat_1,2,as.character)

write.csv(strigiformes.mat_1,"D:\\Environmental data\\Hendra-Australia\\Maxent\\Species_mat\\strigiformes_mat.csv")
strigiformes_mat_1<- read.csv("D:\\Environmental data\\Hendra-Australia\\Maxent\\Species_mat\\strigiformes_mat.csv")

for (i in 2:11) {
  strigiformes_mat_1[i + 12] <- (strigiformes_mat_1[i] > (strigiformes_mat_1[,"pixels"] / 2))
}

strigiformes_mat_1$count_strigiformes_prop50 <- rowSums(strigiformes_mat_1[,14:23])

strigiformes_prop50 <- strigiformes_mat_1 %>% 
  dplyr::select(id, count_strigiformes_prop50)


write.csv(strigiformes_prop50,"D:\\Environmental data\\Hendra-Australia\\Maxent\\Species_mat\\strigiformes_prop50_count.csv")

########################### existence 계산

strigiformes_mat_1<- read.csv("D:\\Environmental data\\Hendra-Australia\\Maxent\\Species_mat\\strigiformes_mat.csv")

for (i in 3:12) {
  strigiformes_mat_1[i + 11] <- (strigiformes_mat_1[i] > 0)
}

strigiformes_mat_1$count_strigiformes_existence <- rowSums(strigiformes_mat_1[,14:23])

strigiformes_existence <- strigiformes_mat_1 %>% 
  dplyr::select(id, count_strigiformes_existence)




##################### Carnivora

list_carnivora_habitat_filtered <- readRDS(file = "D:\\Environmental data\\Hendra-Australia\\Maxent\\Maxent_raster\\list_carnivora_habitat_filtered.rds") 

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


carnivora.mat_1 <- carnivora.mat %>% 
  left_join(area_temp, by = c("id" = "id"))
carnivora_mat_1 <- carnivora.mat_1 %>% 
  mutate(pixels = area / 20000000)

carnivora.mat_1 <- apply(carnivora.mat_1,2,as.character)

write.csv(carnivora.mat_1,"D:\\Environmental data\\Hendra-Australia\\Maxent\\Species_mat\\carnivora_mat.csv")
carnivora_mat_1<- read.csv("D:\\Environmental data\\Hendra-Australia\\Maxent\\Species_mat\\carnivora_mat.csv")

for (i in 2:6) {
  carnivora_mat_1[i + 7] <- (carnivora_mat_1[i] > (carnivora_mat_1[,"pixels"] / 2))
}

carnivora_mat_1$count_carnivora_prop50 <- rowSums(carnivora_mat_1[,9:13])

carnivora_prop50 <- carnivora_mat_1 %>% 
  dplyr::select(id, count_carnivora_prop50)


write.csv(carnivora_prop50,"D:\\Environmental data\\Hendra-Australia\\Maxent\\Species_mat\\carnivora_prop50_count.csv")

########################### existence 계산
carnivora_mat_1 <- read.csv("D:\\Environmental data\\Hendra-Australia\\Maxent\\Species_mat\\carnivora_mat.csv")

for (i in 3:7) {
  carnivora_mat_1[i + 6] <- (carnivora_mat_1[i] > 0)
}

carnivora_mat_1$count_carnivora_existence <- rowSums(carnivora_mat_1[,9:13])

carnivora_existence <- carnivora_mat_1 %>% 
  dplyr::select(id, count_carnivora_existence)








##################### colubridae

list_colubridae_habitat_filtered <- readRDS(file = "D:\\Environmental data\\Hendra-Australia\\Maxent\\Maxent_raster\\list_colubridae_habitat_filtered.rds") 

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


colubridae.mat_1 <- colubridae.mat %>% 
  left_join(area_temp, by = c("id" = "id"))
colubridae_mat_1 <- colubridae.mat_1 %>% 
  mutate(pixels = area / 20000000)

colubridae.mat_1 <- apply(colubridae.mat_1,2,as.character)

write.csv(colubridae.mat_1,"D:\\Environmental data\\Hendra-Australia\\Maxent\\Species_mat\\colubridae_mat.csv")
colubridae_mat_1<- read.csv("D:\\Environmental data\\Hendra-Australia\\Maxent\\Species_mat\\colubridae_mat.csv")

for (i in 2:8) {
  colubridae_mat_1[i + 9] <- (colubridae_mat_1[i] > (colubridae_mat_1[,"pixels"] / 2))
}

colubridae_mat_1$count_colubridae_prop50 <- rowSums(colubridae_mat_1[,11:17])

colubridae_prop50 <- colubridae_mat_1 %>% 
  dplyr::select(id, count_colubridae_prop50)


write.csv(colubridae_prop50,"D:\\Environmental data\\Hendra-Australia\\Maxent\\Species_mat\\colubridae_prop50_count.csv")

########################### existence 계산
colubridae_mat_1<- read.csv("D:\\Environmental data\\Hendra-Australia\\Maxent\\Species_mat\\colubridae_mat.csv")

for (i in 3:9) {
  colubridae_mat_1[i + 8] <- (colubridae_mat_1[i] > 0)
}

colubridae_mat_1$count_colubridae_existence <- rowSums(colubridae_mat_1[,11:17])

colubridae_existence <- colubridae_mat_1 %>% 
  dplyr::select(id, count_colubridae_existence)


existence_mat <- chiroptera_existence %>% 
  left_join(accipitriformes_existence, by = "id") %>% 
  left_join(strigiformes_existence, by = "id") %>% 
  left_join(carnivora_existence, by = "id") %>% 
  left_join(colubridae_existence, by = "id")

existence_mat <- existence_mat %>% 
  rename(ID = id,
         chiroptera_maxent_existence = count_chiroptera_existence,
         accipitriformes_maxent_existence = count_accipitriformes_existence,
         strigiformes_maxent_existence = count_strigiformes_existence,
         carnivora_maxent_existence = count_carnivora_existence,
         colubridae_maxent_existence = count_colubridae_existence)

###################################################
############### For IUCN polygons   ###############
###################################################

##############################################################
# Avian (Accipitriformes) Comprehensive
# Area 계산해서 0.5 이상만 ###############################################
##############################################################

setwd("D:\\Environmental data\\Hendra-Australia\\IUCN_polygon\\Accipitroformes")

polygon.accipitriformes <- readOGR(dsn = "data_0.shp")
crs(polygon.accipitriformes) #4326

#### maxent에서 사용한 것들만 list에 포함시키는 과정. 
library(readr)
accipi_list_maxent <- read.csv("D:\\Environmental data\\Hendra-Australia\\Maxent\\Species_list\\accipitriformes_species_over10.csv")

accipitriformes_list_filtered <- polygon.accipitriformes@data %>% 
  filter(BINOMIAL %in% accipi_list_maxent$Var1) %>% 
  dplyr::select(BINOMIAL)

accipitriformes_list_filtered <- unique(accipitriformes_list_filtered)

write.csv(accipitriformes_list_filtered, "D:\\Environmental data\\Hendra-Australia\\IUCN_polygon\\Species_list\\accipitriformes_list_maxent_filtered.csv")

list_filtered <- which(polygon.accipitriformes@data$BINOMIAL %in% accipitriformes_list_filtered$BINOMIAL)


#종의 위기 상황, 그리고 seasonal 하게 이동하는 습성을 가진 경우를 고려해서 adjust 한다. 
table(polygon.accipitriformes@data$PRESENCE) #1 Extant, 2 Probably Extant, 3 Possibly Exant, 4 Possibly Extinct, 5 Extinct / 1 에서 5로 커질수록 멸종에 가까워짐
table(polygon.accipitriformes@data$SEASONAL) #1 Resident, 2 breeding season, 3 non-breeding season, 4 passage, 5 seasonal occurence uncertain
table(polygon.accipitriformes@data$PRESENCE, polygon_accipitriformes@data$SEASONAL)

which(polygon.accipitriformes@data$PRESENCE %in% c(1,2))   # 
which(polygon.accipitriformes@data$PRESENCE %in% c(4,5))   # 
which(polygon.accipitriformes@data$SEASONAL ==1)           # resident인 경우만

Extant_resident <- which(polygon.accipitriformes@data$PRESENCE %in% c(1,2) & polygon.accipitriformes@data$SEASONAL ==1)    # 89 (Extant & resident)

### 그리고 여기에서 filtered list랑 extant, resident의 위치를 which로 찾은 벡터의 교집합을 만든다.

filtered <- intersect(list_filtered, Extant_resident)

#intersect를 계산한다.
library(rgeos)
intsct.accipitriformes <- gIntersects(polygon.accipitriformes, terrestrial.grid, byid=TRUE)
intsct.accipitriformes<-intsct.accipitriformes+1-1 #원래 매트릭스에는 TRUE, FALSE의 logical 값이 담겨 있는데, 이걸 숫자 0, 1로 변경.

#Extant, Resident만 골라서,
#바로 위에서 which로 id index로 찾은 위치를 기반으로 나누어서 추출한다. 
intsct_accipitriformes_extant_maxent <-intsct.accipitriformes[,filtered] +1 -1


length(colnames(intsct_accipitriformes_extant_maxent))         #18
length(unique(colnames(intsct_accipitriformes_extant_mexent))) #18

length(filtered)         #18

dim(intsct_accipitriformes_extant_maxent) # rows 682, columns 18

sum(is.na(intsct.accipitriformes.final))


##########################
### 위쪽은 약간 부족한 부분이, 겹치는 면적에 대한 계산이 안되었다.
### 아래의 int_area 붙은 부분은, QGIS에서 교차면적 기능을 사용해서 교차 영역 계산하고,
### 변수 계산기에서 각각의 부분에 대한 면적을 구한 것이다. 

accipitriformes_int_area <- readOGR(dsn = "D:\\Environmental data\\Hendra-Australia\\IUCN_polygon\\Intersection_area\\accipitriformes_int_area.shp")

## 여기서 extant랑 maxent 모델에서 쓴거만 list로 골라오기.
Extant_resident <- which(accipitriformes_int_area@data$LEGEND %in% "Extant (resident)") # (Extant & resident)
list_filtered <- which(accipitriformes_int_area@data$BINOMIAL %in% accipitriformes_list_filtered$BINOMIAL)

filtered <- intersect(list_filtered, Extant_resident)

accipitriformes_int_area_mat <- as.data.frame(accipitriformes_int_area@data)
accipitriformes_int_area_mat_extant_filtered <-accipitriformes_int_area_mat[filtered,] 


accipitriformes_int_area_mat_extant_filtered_1 <- accipitriformes_int_area_mat_extant_filtered %>%
  group_by(id, BINOMIAL) %>% 
  summarise(int_area_1 = sum(area_int))

area <- accipitriformes_int_area_mat_extant_filtered %>% 
  select(id, area)

area <- unique(area)

accipitriformes_int_area_mat_extant_filtered_1 <- accipitriformes_int_area_mat_extant_filtered_1 %>% 
  left_join(area, by = "id")

accipitriformes_int_area_mat_extant_filtered_1 <- accipitriformes_int_area_mat_extant_filtered_1 %>% 
  mutate(proportion = int_area_1/area) %>% 
  mutate(decision = case_when(proportion > 0.5 ~ 1,
                              proportion < 0.5 ~ 0)) %>% 
  select(id, BINOMIAL, decision)


accipitriformes_int_area_mat_extant_filtered_2 <- accipitriformes_int_area_mat_extant_filtered_1 %>%
  pivot_wider(names_from = BINOMIAL, values_from = decision)

accipitriformes_int_area_mat_extant_filtered_2[is.na(accipitriformes_int_area_mat_extant_filtered_2)] <- 0 
accipitriformes_int_area_mat_extant_filtered_2

write.csv(accipitriformes_int_area_mat_extant_filtered_2, "D:\\Environmental data\\Hendra-Australia\\IUCN_polygon\\Species_richness_mat\\accipitriformes_int_decision_extant_filtered.csv")

a <- accipitriformes_int_area_mat_extant_filtered_2[,-1] %>% 
  rowSums() %>% 
  cbind(accipitriformes_int_area_mat_extant_filtered_2[,"id"])

accipitirformes_count <- ID %>% 
  left_join(a)

write.csv(accipitirformes_count, "D:\\Environmental data\\Hendra-Australia\\IUCN_polygon\\Species_richness_mat\\accipitirformes_count_IUCN.csv")



##############################################################
# Avian (Strigiformes) Comprehensive 나중에 참고할거면 여기서 코드 가져가기.
# Area 계산해서 0.5 이상만 ###############################################
##############################################################
setwd("D:\\Environmental data\\Hendra-Australia\\IUCN_polygon\\Strigiformes")

polygon.strigiformes <- readOGR(dsn = "data_0.shp")

#### maxent에서 사용한 것들만 list에 포함시키는 과정. 
library(readr)

strigi_list_maxent <- read.csv("D:\\Environmental data\\Hendra-Australia\\Maxent\\Species_list\\strigiformes_species_over10.csv")

strigiformes_list_filtered <- polygon.strigiformes@data %>% 
  filter(BINOMIAL %in% strigi_list_maxent$Var1) %>% 
  dplyr::select(BINOMIAL)

strigiformes_list_filtered <- unique(strigiformes_list_filtered)

write.csv(strigiformes_list_filtered, "D:\\Environmental data\\Hendra-Australia\\IUCN_polygon\\Species_list\\strigiformes_list_maxent_filtered.csv")

##########################
### 위쪽은 약간 부족한 부분이, 겹치는 면적에 대한 계산이 안되었다.
### 아래의 int_area 붙은 파일은, QGIS에서 교차면적 기능을 사용해서 교차 영역 계산하고,
### 변수 계산기에서 각각의 부분에 대한 면적을 구한 것이다. 

strigiformes_int_area <- readOGR(dsn = "D:\\Environmental data\\Hendra-Australia\\IUCN_polygon\\Intersection_area\\strigiformes_int_area.shp")

## 여기서 extant랑 maxent 모델에서 쓴거만 list로 골라오기.
Extant_resident <- which(strigiformes_int_area@data$LEGEND %in% "Extant (resident)") # (Extant & resident)


list_filtered <- which(strigiformes_int_area@data$BINOMIAL %in% strigiformes_list_filtered$BINOMIAL)

filtered <- intersect(list_filtered, Extant_resident)

strigiformes_int_area_mat <- as.data.frame(strigiformes_int_area@data)
strigiformes_int_area_mat_extant_filtered <-strigiformes_int_area_mat[filtered,] 


strigiformes_int_area_mat_extant_filtered_1 <- strigiformes_int_area_mat_extant_filtered %>%
  group_by(id, BINOMIAL) %>% 
  summarise(int_area_1 = sum(int_area))

area <- strigiformes_int_area_mat_extant_filtered %>% 
  select(id, area)

area <- unique(area)

strigiformes_int_area_mat_extant_filtered_1 <- strigiformes_int_area_mat_extant_filtered_1 %>% 
  left_join(area, by = "id")

strigiformes_int_area_mat_extant_filtered_1 <- strigiformes_int_area_mat_extant_filtered_1 %>% 
  mutate(proportion = int_area_1/area) %>% 
  mutate(decision = case_when(proportion > 0.5 ~ 1,
                              proportion < 0.5 ~ 0)) %>% 
  select(id, BINOMIAL, decision)


strigiformes_int_area_mat_extant_filtered_2 <- strigiformes_int_area_mat_extant_filtered_1 %>%
  pivot_wider(names_from = BINOMIAL, values_from = decision) #여기서 이제 양쪽으로 넓은 데이터로 만들어서 count할 수 있도록 한다.

strigiformes_int_area_mat_extant_filtered_2[is.na(strigiformes_int_area_mat_extant_filtered_2)] <- 0 
strigiformes_int_area_mat_extant_filtered_2

write.csv(strigiformes_int_area_mat_extant_filtered_2, "D:\\Environmental data\\Hendra-Australia\\IUCN_polygon\\Species_richness_mat\\strigiformes_int_decision_extant_filtered.csv")

a <- strigiformes_int_area_mat_extant_filtered_2[,-1] %>% 
  rowSums() %>% 
  cbind(strigiformes_int_area_mat_extant_filtered_2[,"id"])

strigiformes_count <- ID %>% 
  left_join(a)

write.csv(strigiformes_count, "D:\\Environmental data\\Hendra-Australia\\IUCN_polygon\\Species_richness_mat\\strigiformes_count_IUCN.csv")


##############################################################
# Chiroptera Comprehensive
# Area 계산해서 0.5 이상만 ###############################################
##############################################################
setwd("D:\\Environmental data\\Hendra-Australia\\IUCN_polygon\\Chiroptera")

polygon.chiroptera <- readOGR(dsn = "data_0.shp")

#### maxent에서 사용한 것들만 list에 포함시키는 과정. 
library(readr)

chirop_list_maxent <- read.csv("D:\\Environmental data\\Hendra-Australia\\Maxent\\Species_list\\chiroptera_species_over10.csv")

chiroptera_list_filtered <- polygon.chiroptera@data %>% 
  filter(BINOMIAL %in% chirop_list_maxent$Var1) %>% 
  dplyr::select(BINOMIAL)

chiroptera_list_filtered <- unique(chiroptera_list_filtered)

write.csv(chiroptera_list_filtered, "D:\\Environmental data\\Hendra-Australia\\IUCN_polygon\\Species_list\\chiroptera_list_maxent_filtered.csv")

##########################
### 위쪽은 약간 부족한 부분이, 겹치는 면적에 대한 계산이 안되었다.
### 아래의 int_area 붙은 파일은, QGIS에서 교차면적 기능을 사용해서 교차 영역 계산하고,
### 변수 계산기에서 각각의 부분에 대한 면적을 구한 것이다. 

chiroptera_int_area <- readOGR(dsn = "D:\\Environmental data\\Hendra-Australia\\IUCN_polygon\\Intersection_area\\chiroptera_int_area.shp")

## 여기서 extant랑 maxent 모델에서 쓴거만 list로 골라오기.
Extant_resident <- which(chiroptera_int_area@data$LEGEND %in% "Extant (resident)") # (Extant & resident)


list_filtered <- which(chiroptera_int_area@data$BINOMIAL %in% chiroptera_list_filtered$BINOMIAL)

filtered <- intersect(list_filtered, Extant_resident)

chiroptera_int_area_mat <- as.data.frame(chiroptera_int_area@data)
chiroptera_int_area_mat_extant_filtered <-chiroptera_int_area_mat[filtered,] 


chiroptera_int_area_mat_extant_filtered_1 <- chiroptera_int_area_mat_extant_filtered %>%
  group_by(id, BINOMIAL) %>% 
  summarise(int_area_1 = sum(int_area))

area <- chiroptera_int_area_mat_extant_filtered %>% 
  select(id, area)

area <- unique(area)

chiroptera_int_area_mat_extant_filtered_1 <- chiroptera_int_area_mat_extant_filtered_1 %>% 
  left_join(area, by = "id")

chiroptera_int_area_mat_extant_filtered_1 <- chiroptera_int_area_mat_extant_filtered_1 %>% 
  mutate(proportion = int_area_1/area) %>% 
  mutate(decision = case_when(proportion > 0.5 ~ 1,
                              proportion < 0.5 ~ 0)) %>% 
  select(id, BINOMIAL, decision)


chiroptera_int_area_mat_extant_filtered_2 <- chiroptera_int_area_mat_extant_filtered_1 %>%
  pivot_wider(names_from = BINOMIAL, values_from = decision) #여기서 이제 양쪽으로 넓은 데이터로 만들어서 count할 수 있도록 한다.

chiroptera_int_area_mat_extant_filtered_2[is.na(chiroptera_int_area_mat_extant_filtered_2)] <- 0 
chiroptera_int_area_mat_extant_filtered_2

write.csv(chiroptera_int_area_mat_extant_filtered_2, "D:\\Environmental data\\Hendra-Australia\\IUCN_polygon\\Species_richness_mat\\chiroptera_int_decision_extant_filtered.csv")

a <- chiroptera_int_area_mat_extant_filtered_2[,-1] %>% 
  rowSums() %>% 
  cbind(chiroptera_int_area_mat_extant_filtered_2[,"id"])

chiroptera_count <- ID %>% 
  left_join(a)

write.csv(chiroptera_count, "D:\\Environmental data\\Hendra-Australia\\IUCN_polygon\\Species_richness_mat\\chiroptera_count_IUCN.csv")

##############################################################
# Carnivora Comprehensive
# Area 계산해서 0.5 이상만 ###############################################
##############################################################
setwd("D:\\Environmental data\\Hendra-Australia\\IUCN_polygon\\Carnivora")

polygon.carnivora <- readOGR(dsn = "data_0.shp")

#### maxent에서 사용한 것들만 list에 포함시키는 과정. 
library(readr)

carni_list_maxent <- read.csv("D:\\Environmental data\\Hendra-Australia\\Maxent\\Species_list\\carnivora_species_over10.csv")

carnivora_list_filtered <- polygon.carnivora@data %>% 
  filter(BINOMIAL %in% carni_list_maxent$Var1) %>% 
  dplyr::select(BINOMIAL)

carnivora_list_filtered <- unique(carnivora_list_filtered)

write.csv(carnivora_list_filtered, "D:\\Environmental data\\Hendra-Australia\\IUCN_polygon\\Species_list\\carnivora_list_maxent_filtered.csv")

##########################
### 위쪽은 약간 부족한 부분이, 겹치는 면적에 대한 계산이 안되었다.
### 아래의 int_area 붙은 파일은, QGIS에서 교차면적 기능을 사용해서 교차 영역 계산하고,
### 변수 계산기에서 각각의 부분에 대한 면적을 구한 것이다. 

carnivora_int_area <- readOGR(dsn = "D:\\Environmental data\\Hendra-Australia\\IUCN_polygon\\Intersection_area\\carnivora_int_area.shp")

## 여기서 extant랑 maxent 모델에서 쓴거만 list로 골라오기.
Extant_resident <- which(carnivora_int_area@data$LEGEND %in% "Extant (resident)") # (Extant & resident)


list_filtered <- which(carnivora_int_area@data$BINOMIAL %in% carnivora_list_filtered$BINOMIAL)

filtered <- intersect(list_filtered, Extant_resident)

carnivora_int_area_mat <- as.data.frame(carnivora_int_area@data)
carnivora_int_area_mat_extant_filtered <-carnivora_int_area_mat[filtered,] 


carnivora_int_area_mat_extant_filtered_1 <- carnivora_int_area_mat_extant_filtered %>%
  group_by(id, BINOMIAL) %>% 
  summarise(int_area_1 = sum(int_area))

area <- carnivora_int_area_mat_extant_filtered %>% 
  select(id, area)

area <- unique(area)

carnivora_int_area_mat_extant_filtered_1 <- carnivora_int_area_mat_extant_filtered_1 %>% 
  left_join(area, by = "id")

carnivora_int_area_mat_extant_filtered_1 <- carnivora_int_area_mat_extant_filtered_1 %>% 
  mutate(proportion = int_area_1/area) %>% 
  mutate(decision = case_when(proportion > 0.5 ~ 1,
                              proportion < 0.5 ~ 0)) %>% 
  select(id, BINOMIAL, decision)


carnivora_int_area_mat_extant_filtered_2 <- carnivora_int_area_mat_extant_filtered_1 %>%
  pivot_wider(names_from = BINOMIAL, values_from = decision) #여기서 이제 양쪽으로 넓은 데이터로 만들어서 count할 수 있도록 한다.

carnivora_int_area_mat_extant_filtered_2[is.na(carnivora_int_area_mat_extant_filtered_2)] <- 0 
carnivora_int_area_mat_extant_filtered_2

write.csv(carnivora_int_area_mat_extant_filtered_2, "D:\\Environmental data\\Hendra-Australia\\IUCN_polygon\\Species_richness_mat\\carnivora_int_decision_extant_filtered.csv")

a <- carnivora_int_area_mat_extant_filtered_2[,-1] %>% 
  rowSums() %>% 
  cbind(carnivora_int_area_mat_extant_filtered_2[,"id"])

carnivora_count <- ID %>% 
  left_join(a)

write.csv(carnivora_count, "D:\\Environmental data\\Hendra-Australia\\IUCN_polygon\\Species_richness_mat\\carnivora_count_IUCN.csv")

##############################################################
# Colubridae Comprehensive
# Area 계산해서 0.5 이상만 ###############################################
##############################################################
setwd("D:\\Environmental data\\Hendra-Australia\\IUCN_polygon\\Colubridae")

polygon.colubridae <- readOGR(dsn = "data_0.shp")

#### maxent에서 사용한 것들만 list에 포함시키는 과정. 
library(readr)

colu_list_maxent <- read.csv("D:\\Environmental data\\Hendra-Australia\\Maxent\\Species_list\\colubridae_species_over10.csv")

colubridae_list_filtered <- polygon.colubridae@data %>% 
  filter(BINOMIAL %in% colu_list_maxent$Var1) %>% 
  dplyr::select(BINOMIAL)

colubridae_list_filtered <- unique(colubridae_list_filtered)

write.csv(colubridae_list_filtered, "D:\\Environmental data\\Hendra-Australia\\IUCN_polygon\\Species_list\\colubridae_list_maxent_filtered.csv")

##########################
### 위쪽은 약간 부족한 부분이, 겹치는 면적에 대한 계산이 안되었다.
### 아래의 int_area 붙은 파일은, QGIS에서 교차면적 기능을 사용해서 교차 영역 계산하고,
### 변수 계산기에서 각각의 부분에 대한 면적을 구한 것이다. 

colubridae_int_area <- readOGR(dsn = "D:\\Environmental data\\Hendra-Australia\\IUCN_polygon\\Intersection_area\\colubridae_int_area.shp")

## 여기서 extant랑 maxent 모델에서 쓴거만 list로 골라오기.
Extant_resident <- which(colubridae_int_area@data$LEGEND %in% "Extant (resident)") # (Extant & resident)


list_filtered <- which(colubridae_int_area@data$BINOMIAL %in% colubridae_list_filtered$BINOMIAL)

filtered <- intersect(list_filtered, Extant_resident)

colubridae_int_area_mat <- as.data.frame(colubridae_int_area@data)
colubridae_int_area_mat_extant_filtered <-colubridae_int_area_mat[filtered,] 


colubridae_int_area_mat_extant_filtered_1 <- colubridae_int_area_mat_extant_filtered %>%
  group_by(id, BINOMIAL) %>% 
  summarise(int_area_1 = sum(int_area))

area <- colubridae_int_area_mat_extant_filtered %>% 
  select(id, area)

area <- unique(area)

colubridae_int_area_mat_extant_filtered_1 <- colubridae_int_area_mat_extant_filtered_1 %>% 
  left_join(area, by = "id")

colubridae_int_area_mat_extant_filtered_1 <- colubridae_int_area_mat_extant_filtered_1 %>% 
  mutate(proportion = int_area_1/area) %>% 
  mutate(decision = case_when(proportion > 0.5 ~ 1,
                              proportion < 0.5 ~ 0)) %>% 
  select(id, BINOMIAL, decision)


colubridae_int_area_mat_extant_filtered_2 <- colubridae_int_area_mat_extant_filtered_1 %>%
  pivot_wider(names_from = BINOMIAL, values_from = decision) #여기서 이제 양쪽으로 넓은 데이터로 만들어서 count할 수 있도록 한다.

colubridae_int_area_mat_extant_filtered_2[is.na(colubridae_int_area_mat_extant_filtered_2)] <- 0 
colubridae_int_area_mat_extant_filtered_2

write.csv(colubridae_int_area_mat_extant_filtered_2, "D:\\Environmental data\\Hendra-Australia\\IUCN_polygon\\Species_richness_mat\\colubridae_int_decision_extant_filtered.csv")

a <- colubridae_int_area_mat_extant_filtered_2[,-1] %>% 
  rowSums() %>% 
  cbind(colubridae_int_area_mat_extant_filtered_2[,"id"])

colubridae_count <- ID %>% 
  left_join(a)

write.csv(colubridae_count, "D:\\Environmental data\\Hendra-Australia\\IUCN_polygon\\Species_richness_mat\\colubridae_count_IUCN.csv")











