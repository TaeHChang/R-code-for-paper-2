### dismo 패키지를 활용한 Maxent 모델로 종 다양성 추정하기.
install.packages("rJava")
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_361')

library(raster)
library(rgdal)
library(maps)
library(dismo) 
library(maptools)
library(jsonlite)
library(tidyverse)
library(rJava)


library(doParallel)
cl<-makeCluster(2)
registerDoParallel(cl)
detectCores()
registerDoParallel(cores=32)
getDoParWorkers()



################### Creating biased background 

##################################################################### 예시부분
require(raster)
RR <- raster()
plot(RR)
RR <- disaggregate(RR, 10)
RR[] <- 1:ncell(RR)/ncell(RR)
RandomDismo <- dismo::randomPoints(mask = RR, n = 1000, prob = TRUE)

plot(RR, box = F, axes = F, legend = F)
points(RandomDismo, pch = 19, cex = 0.25, col = "blue")




library(dismo)
r <- raster(resolution=2)
values(r) <- 1:ncell(r)/ncell(r) ## 여기 이 부분에서 value로 GBIF 혹은 popluation의 값 넣어야 함.

plottab <- function(x) {
  d <- as.data.frame(table(x[,2]))
  d[,1] <- as.numeric(as.character(d[,1]))
  plot(d)
}

?dismo::randomPoints
pd1 <- dismo::randomPoints(r, n = 1000, prob = TRUE, lonlatCorrection=TRUE)
pd2 <- dismo::randomPoints(r, n = 1000, prob = FALSE, lonlatCorrection=TRUE)
pd3 <- dismo::randomPoints(r, n = 1000, prob = TRUE, lonlatCorrection=FALSE)
crs(r) <- "+proj=utm +zone=1"
pd4 <- dismo::randomPoints(r, n = 1000, prob = TRUE, lonlatCorrection=FALSE)

par(mfrow=c(2,2), mar=c(2,2,1,1))
plottab(pd1)
plottab(pd2)
plottab(pd3)
plottab(pd4)

par(mfrow=c(1,1))
plot(r, box = F, axes = F, legend = F)
points(pd1, pch = 19, cex = 0.25, col = "blue")
#####################################################################################################


############### 먼저, 빈 raster 만들고, study unit 크기에 맞게 masking 하기.
setwd("C:\\Users\\admin\\Desktop\\Hendra")
terrestrial.grid <- readOGR(dsn = "AUS_basemap\\final\\Basemap_clipped_numpoints_final.shp")
plot(terrestrial.grid)

ras <- raster(resolution=0.001)


ID <- terrestrial.grid@data %>% dplyr::select(id)
area_temp <- terrestrial.grid@data %>% dplyr::select(id, area)


# check raster projection
projection(ras)
# check vector projection
proj4string(terrestrial.grid)


# clip raster data with mask / mask는 시간 너무 오래걸려서 패쓰
#raster_clip <- mask(x = ras, mask = terrestrial.grid)

# create spatial extent from vector data
spatial_extent <- extent(terrestrial.grid)

# crop raster data using spatial extent / crop function이 훨씬 빠르고 나은듯
raster_crop <- crop(ras, spatial_extent)

plot(raster_crop)

library(readr)
chiroptera <- read_delim("Maxent\\Species_occurence/chiroptera.csv", 
                         delim = "\t", escape_double = FALSE, 
                         trim_ws = TRUE)

accipitriformes <- read_delim("Maxent\\Species_occurence/accipitriformes.csv", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE)

strigiformes <- read_delim("Maxent\\Species_occurence/strigiformes.csv", 
                           delim = "\t", escape_double = FALSE, 
                           trim_ws = TRUE)

carnivora <- read_delim("Maxent\\Species_occurence/carnivora.csv", 
                        delim = "\t", escape_double = FALSE, 
                        trim_ws = TRUE)

colubridae <- read_delim("Maxent\\Species_occurence/colubridae.csv", 
                         delim = "\t", escape_double = FALSE, 
                         trim_ws = TRUE)

total_GBIF <- rbind(chiroptera, accipitriformes, strigiformes, carnivora, colubridae)

total_species <- SpatialPoints(coords = total_GBIF[c("decimalLongitude", "decimalLatitude")], proj4string  = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))


# Define custom function to count points in each cell
count_points <- function(x, ...) {
  sum(!is.na(x))
}

# Rasterize points to create a raster mask
raster_mask <- rasterize(total_species, raster_crop, fun = count_points)

# Assign cell count values to the raster data
values(raster_crop) <- values(raster_mask)

plot(raster_crop)

raster_crop_mask <- mask(raster_crop, terrestrial.grid)

# Create a mask raster with NA values where raster_data has NA values
mask_raster <- raster_crop_mask
mask_raster[is.na(raster_crop_mask)] <- NA

# Mask raster data to exclude cells with NA values
masked_data <- mask(raster_crop_mask, mask_raster)
plot(raster_crop_mask)
plot(masked_data)

RandomDismo <- dismo::randomPoints(mask = masked_data, n = 10000, prob = TRUE, lonlatCorrection=TRUE)

plot(raster_crop_mask)
points(RandomDismo, pch = 19, cex = 0.25, col = "blue")

rm(raster_crop_mask)
rm(masked_data)
rm(total_species)
rm(total_GBIF)


save(RandomDismo, file = "RandomDismo.rda")
load(file = "RandomDismo.rda")


#############################################
############    chiroptera       ############
#############################################

library(readr)
chiroptera <- read_delim("Maxent\\Species_occurence/chiroptera.csv", 
                         delim = "\t", escape_double = FALSE, 
                         trim_ws = TRUE)

#각 species 별로 구분된 spatialpoints 객체를 모아 놓은 list이다. 
chiroptera %>% group_by(species) %>% group_map(~.x) %>%
  lapply(FUN = function(X) {
    SpatialPoints(coords = X[c("decimalLongitude", "decimalLatitude")], proj4string  = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  }
  ) -> sp_chiroptera_species

class(sp_chiroptera_species)
sp_chiroptera_species

#list 안에 총 56 종의 species가 있고, 각 species 별로 몇 개의 point가 찍혀 있는지 확인.
sp_chiroptera_species <- sp_chiroptera_species[unlist(lapply(sp_chiroptera_species, FUN = length)) > 10]
#point 숫자가 최소한 10보다는 큰 종만 골라서 다시 subset -> 49종


chiroptera_species <- as.data.frame(table(chiroptera$species))
chiroptera_species_over10 <- chiroptera_species %>% 
  dplyr::filter(Freq > 10)

write.csv(chiroptera_species_over10, "D:\\Environmental data\\Hendra-Australia\\Maxent\\Species_list\\chiroptera_species_over10.csv")

# load base map
setwd("AUS_basemap\\final")
terrestrial.grid <- readOGR(dsn = "Basemap_clipped_numpoints_final.shp")

#####loading world climate data #####
world_clm <- list.files(path = "Maxent\\wc2.1_2.5m_bio\\", 
                        pattern = ".tif$", full.names  = TRUE)

st_clm <- stack(world_clm)
cr_st_clm <- crop(st_clm, terrestrial.grid)
plot(cr_st_clm[[1]])

#####loading elevation data #####

elevation <- raster("Maxent\\wc2.1_2.5m_elev\\wc2.1_2.5m_elev.tif") 
elevation <- crop(elevation, terrestrial.grid)

plot(elevation)


#### making representative raster #### maxent 결과 담을 객체
rep_raster <- mask(raster( crs = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"), ext = extent(terrestrial.grid), resolution = res(cr_st_clm), vals = 1 ),terrestrial.grid)
plot(rep_raster)

list(cr_st_clm, elevation) ->list_env
lapply(list_env, FUN = raster::resample, y = rep_raster, method = "ngb") -> re_list_env





### parameter preparation
{
  prepPara <- function(userfeatures=NULL, #NULL=autofeature, could be any combination of # c("L", "Q", "H", "H", "P")
                       responsecurves=TRUE,
                       jackknife=TRUE,      
                       outputformat="logistic",
                       outputfiletype="asc", 
                       projectionlayers=NULL,
                       randomseed=FALSE,
                       removeDuplicates=TRUE,
                       betamultiplier=NULL,
                       biasfile=NULL,
                       testsamplesfile=NULL,
                       replicates=1,
                       replicatetype="crossvalidate",
                       writeplotdata=TRUE,
                       extrapolate=TRUE,
                       doclamp=TRUE,
                       beta_threshold=NULL,
                       beta_categorical=NULL,
                       beta_lqp=NULL,
                       beta_hinge=NULL,
                       applythresholdrule=NULL,
                       maximumiterations = 500,
                       convergencethreshold = 1.0E-5,
                       number_core = 1 
  ){
    #20 & 29-33 features, default is autofeature
    if(is.null(userfeatures)){
      args_out <- c("autofeature")
    } else {
      args_out <- c("noautofeature")
      if(grepl("L",userfeatures)) args_out <- c(args_out,"linear") else args_out <- c(args_out,"nolinear")
      if(grepl("Q",userfeatures)) args_out <- c(args_out,"quadratic") else args_out <- c(args_out,"noquadratic")
      if(grepl("H",userfeatures)) args_out <- c(args_out,"hinge") else args_out <- c(args_out,"nohinge")
      if(grepl("P",userfeatures)) args_out <- c(args_out,"product") else args_out <- c(args_out,"noproduct")
      if(grepl("T",userfeatures)) args_out <- c(args_out,"threshold") else args_out <- c(args_out,"nothreshold")
    }
    
    #1 
    if(responsecurves) args_out <- c(args_out,"responsecurves") else args_out <- c(args_out,"noresponsecurves")
    #2
    #if(picture) args_out <- c(args_out,"pictures") else args_out <- c(args_out,"nopictures")
    #3
    if(jackknife) args_out <- c(args_out,"jackknife") else args_out <- c(args_out,"nojackknife")
    #4
    args_out <- c(args_out,paste0("outputformat=",outputformat))
    #5
    args_out <- c(args_out,paste0("outputfiletype=",outputfiletype))
    #7
    if(!is.null(projectionlayers))    args_out <- c(args_out,paste0("projectionlayers=",projectionlayers))
    #10
    if(randomseed) args_out <- c(args_out,"randomseed") else args_out <- c(args_out,"norandomseed")
    #16
    if(removeDuplicates) args_out <- c(args_out,"removeduplicates") else args_out <- c(args_out,"noremoveduplicates")
    #20 & 53-56
    # check if negative
    betas <- c( betamultiplier,beta_threshold,beta_categorical,beta_lqp,beta_hinge)
    if(! is.null(betas) ){
      for(i in 1:length(betas)){
        if(betas[i] <0) stop("betamultiplier has to be positive")
      }
    }
    if (  !is.null(betamultiplier)  ){
      args_out <- c(args_out,paste0("betamultiplier=",betamultiplier))
    } else {
      if(!is.null(beta_threshold)) args_out <- c(args_out,paste0("beta_threshold=",beta_threshold))
      if(!is.null(beta_categorical)) args_out <- c(args_out,paste0("beta_categorical=",beta_categorical))
      if(!is.null(beta_lqp)) args_out <- c(args_out,paste0("beta_lqp=",beta_lqp))
      if(!is.null(beta_hinge)) args_out <- c(args_out,paste0("beta_hinge=",beta_hinge))
    }
    #22
    if(!is.null(biasfile))    args_out <- c(args_out,paste0("biasfile=",biasfile))
    #23
    if(!is.null(testsamplesfile))    args_out <- c(args_out,paste0("testsamplesfile=",testsamplesfile))
    #24&25
    replicates <- as.integer(replicates)
    if(replicates>1 ){
      args_out <- c(args_out,
                    paste0("replicates=",replicates),
                    paste0("replicatetype=",replicatetype) )
    }
    #37
    if(writeplotdata) args_out <- c(args_out,"writeplotdata") else args_out <- c(args_out,"nowriteplotdata")
    #39
    if(extrapolate) args_out <- c(args_out,"extrapolate") else args_out <- c(args_out,"noextrapolate")
    #42
    if(doclamp) args_out <- c(args_out,"doclamp") else args_out <- c(args_out,"nodoclamp")
    #60
    if(!is.null(applythresholdrule))    args_out <- c(args_out,paste0("applythresholdrule=",applythresholdrule))
    
    maximumiterations <- as.integer(maximumiterations)
    args_out <- c(args_out, paste0("maximumiterations=", maximumiterations))
    
    convergencethreshold <- as.integer(convergencethreshold)
    args_out <- c(args_out, paste0("convergencethreshold=", convergencethreshold))
    
    #number_core <- as.integer(number_core)
    #args_out <- c(args_out, paste0("threads=", number_core))
    
    return(args_out)
  }
}


stack(unlist(re_list_env)) ->st_env_r
names(st_env_r)

saveRDS(st_env_r, file = "D:\\Environmental data\\Hendra-Australia\\Maxent\\stack_env_raster.rds")
readRDS(file = "D:\\Environmental data\\Hendra-Australia\\Maxent\\stack_env_raster.rds") -> st_env_r




### 설명변수 중에서 correlation 있는거 제거하기.
corst_env_r <- raster::layerStats(st_env_r, 'pearson', na.rm = TRUE) #피어스 상관분석
corst_env_r <- corst_env_r[[1]] #상관분석 매트릭스만 뽑아 ..
corst_env_r <- as.data.frame(corst_env_r)
write.csv(corst_env_r, "D:\\Environmental data\\Hendra-Australia\\Maxent\\cor_2.csv")

st_env_r <- dropLayer(st_env_r, c("wc2.1_2.5m_bio_1", "wc2.1_2.5m_bio_10", "wc2.1_2.5m_bio_11", "wc2.1_2.5m_bio_12",
                                  "wc2.1_2.5m_bio_13", "wc2.1_2.5m_bio_14", "wc2.1_2.5m_bio_6", "wc2.1_2.5m_bio_3", "wc2.1_2.5m_bio_16",
                                  "wc2.1_2.5m_bio_17", "wc2.1_2.5m_bio_18", "wc2.1_2.5m_bio_7", "wc2.1_2.5m_bio_19",
                                  "wc2.1_2.5m_bio_5"))


saveRDS(st_env_r, file = "D:\\Environmental data\\Hendra-Australia\\Maxent\\stack_env_raster.rds")
st_env_r <- readRDS(file = "D:\\Environmental data\\Hendra-Australia\\Maxent\\stack_env_raster.rds")
## 최종적으로 bio layer 2, 4, 8, 9, 15, elevation만 남았음.


list_maxent_model =list(NA)
list_remain_var = list(NA)
for (i in 1:length(sp_chiroptera_species)) {
  
  sp_point_species = sp_chiroptera_species[[i]]
  
  exclude_var = NA
  var_num = 1:nlayers(st_env_r)
  min_contr_var = 0
  
  model0= maxent(x = st_env_r[[var_num]], p = sp_point_species,  silent = TRUE, a = RandomDismo,
                 args = prepPara(jackknife=TRUE, betamultiplier =1, replicates = 1),
                 removeDuplicates = FALSE,
                 path ="Maxent\\data" )
  min_var = which.min(model0@results[grepl("contribution$",rownames(model0@results))])
  min_contr_var = model0@results[grepl("contribution$",rownames(model0@results))][min_var]
  var_name = rownames(model0@results)[grepl("contribution$",rownames(model0@results))][min_var]
  exclude_var = substr(var_name,1, nchar(var_name)-13)
  var_num = which(!names(st_env_r) %in% exclude_var)
  
  while(min_contr_var <= 10){
    
    model0= maxent(st_env_r[[var_num]], sp_point_species,  silent = TRUE,
                   args = prepPara(jackknife=TRUE, betamultiplier =1, replicates = 1),
                   removeDuplicates = FALSE,
                   path ="Maxent\\data" )
    
    min_var = which.min(model0@results[grepl("contribution$",rownames(model0@results))])
    var_name = rownames(model0@results)[grepl("contribution$",rownames(model0@results))][min_var]
    min_contr_var = model0@results[grepl("contribution$",rownames(model0@results))][min_var]
    exclude_var = c(exclude_var, substr(var_name,1, nchar(var_name)-13))
    var_num = which(!names(st_env_r) %in% exclude_var)
  } 
  list_maxent_model[[i]]= model0
  list_remain_var[[i]] = names(st_env_r[[var_num]])
  
}


saveRDS(list_maxent_model, file = "Maxent\\Maxent_models\\list_maxent_model_chiroptera.rds")
readRDS(file = "Maxent\\Maxent_models\\list_maxent_model_chiroptera.rds") -> list_maxent_model_chiroptera
saveRDS(list_remain_var, file = "D:\\Environmental data\\Hendra-Australia\\Maxent\\Maxent_models\\list_remain_var_chiroptera.rds")
readRDS(file = "D:\\Environmental data\\Hendra-Australia\\Maxent\\Maxent_models\\list_remain_var_chiroptera.rds") -> list_remain_var_chiroptera


list_chiroptera_habitat =list(NA)
for (i in 1:length(list_maxent_model)) { 
  list_chiroptera_habitat[[i]] <- predict(list_maxent_model[[i]], st_env_r)
}

plot(list_chiroptera_habitat[[38]])

saveRDS(list_chiroptera_habitat, file = "Maxent\\Maxent_raster\\list_chiroptera_habitat.rds")
list_chiroptera_habitat <- readRDS(file = "Maxent\\Maxent_raster\\list_chiroptera_habitat.rds") 


######## evaluate/threshold

#### k-fold를 5로 잡아서 train과 test dateset 분리하기.
fold.list<-list()
length(list_maxent_model)
for(i in 1:length(list_maxent_model)){
  fold.list[[i]]<-kfold(sp_chiroptera_species[[i]],k=5)
}

fold.list

#### 각 모델 evaluate 하기
evaluate.mat<-list()
for(i in 1:length(list_maxent_model)){
  
  loc.test<-sp_chiroptera_species[[i]][fold.list[[i]] == 1,]
  
  bg <- randomPoints(st_env_r, length(loc.test))
  e1 <- evaluate(list_maxent_model[[i]], p=loc.test, a=bg, x=st_env_r)
  
  evaluate.mat[[i]]<-e1
}
evaluate.mat

#### evaluate 한 객체들에서 threshold 값만 빼내기
threshold.mat <- list()
for(i in 1:length(evaluate.mat)){
  t <- threshold(evaluate.mat[[i]])
  threshold.mat[[i]] <- t[["prevalence"]]
}
threshold.mat

#### 각 raster의 픽셀에 나타난 확률값들을 위에서 구한 threshold 기준으로 threshold를 넘으면 1, 넘지 않으면 0으로 하여 출현/비출현 구분.
list_chiroptera_habitat_filtered <- list_chiroptera_habitat
for(i in 1:length(list_chiroptera_habitat_filtered)){
  list_chiroptera_habitat_filtered[[i]][list_chiroptera_habitat_filtered[[i]] < threshold.mat[[i]]] <- 0 
  list_chiroptera_habitat_filtered[[i]][list_chiroptera_habitat_filtered[[i]] >= threshold.mat[[i]]] <- 1
}

plot(list_chiroptera_habitat_filtered[[38]]) 


saveRDS(list_chiroptera_habitat_filtered, file = "Maxent\\Maxent_raster\\list_chiroptera_habitat_filtered.rds")
list_chiroptera_habitat_filtered <- readRDS(file = "Maxent\\Maxent_raster\\list_chiroptera_habitat_filtered.rds") 





#############################################
############    Accipitriformes       ############
#############################################


library(readr)
accipitriformes <- read_delim("Maxent\\Species_occurence/accipitriformes.csv", 
                         delim = "\t", escape_double = FALSE, 
                         trim_ws = TRUE)

#각 species 별로 구분된 spatialpoints 객체를 모아 놓은 list이다. 
accipitriformes %>% group_by(species) %>% group_map(~.x) %>%
  lapply(FUN = function(X) {
    SpatialPoints(coords = X[c("decimalLongitude", "decimalLatitude")], proj4string  = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  }
  ) -> sp_accipitriformes_species

class(sp_accipitriformes_species)
sp_accipitriformes_species

#list 안에 총 17 종의 species가 있고, 각 species 별로 몇 개의 point가 찍혀 있는지 확인.
sp_accipitriformes_species <- sp_accipitriformes_species[unlist(lapply(sp_accipitriformes_species, FUN = length)) > 10]
#point 숫자가 최소한 10보다는 큰 종만 골라서 다시 subset -> 16종


accipitriformes_species <- as.data.frame(table(accipitriformes$species))
accipitriformes_species_over10 <- accipitriformes_species %>% 
  dplyr::filter(Freq > 10)

write.csv(accipitriformes_species_over10, "D:\\Environmental data\\Hendra-Australia\\Maxent\\Species_list\\accipitriformes_species_over10.csv")

# load base map
setwd("D:\\Environmental data\\Hendra-Australia\\AUS_basemap\\final")
terrestrial.grid <- readOGR(dsn = "Basemap_clipped_numpoints_final.shp")



#### making representative raster #### maxent 결과 담을 객체
rep_raster <- mask(raster( crs = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"), ext = extent(terrestrial.grid), resolution = res(cr_st_clm), vals = 1 ),terrestrial.grid)
plot(rep_raster)




### parameter preparation
{
  prepPara <- function(userfeatures=NULL, #NULL=autofeature, could be any combination of # c("L", "Q", "H", "H", "P")
                       responsecurves=TRUE,
                       jackknife=TRUE,      
                       outputformat="logistic",
                       outputfiletype="asc", 
                       projectionlayers=NULL,
                       randomseed=FALSE,
                       removeDuplicates=TRUE,
                       betamultiplier=NULL,
                       biasfile=NULL,
                       testsamplesfile=NULL,
                       replicates=1,
                       replicatetype="crossvalidate",
                       writeplotdata=TRUE,
                       extrapolate=TRUE,
                       doclamp=TRUE,
                       beta_threshold=NULL,
                       beta_categorical=NULL,
                       beta_lqp=NULL,
                       beta_hinge=NULL,
                       applythresholdrule=NULL,
                       maximumiterations = 500,
                       convergencethreshold = 1.0E-5,
                       number_core = 1 
  ){
    #20 & 29-33 features, default is autofeature
    if(is.null(userfeatures)){
      args_out <- c("autofeature")
    } else {
      args_out <- c("noautofeature")
      if(grepl("L",userfeatures)) args_out <- c(args_out,"linear") else args_out <- c(args_out,"nolinear")
      if(grepl("Q",userfeatures)) args_out <- c(args_out,"quadratic") else args_out <- c(args_out,"noquadratic")
      if(grepl("H",userfeatures)) args_out <- c(args_out,"hinge") else args_out <- c(args_out,"nohinge")
      if(grepl("P",userfeatures)) args_out <- c(args_out,"product") else args_out <- c(args_out,"noproduct")
      if(grepl("T",userfeatures)) args_out <- c(args_out,"threshold") else args_out <- c(args_out,"nothreshold")
    }
    
    #1 
    if(responsecurves) args_out <- c(args_out,"responsecurves") else args_out <- c(args_out,"noresponsecurves")
    #2
    #if(picture) args_out <- c(args_out,"pictures") else args_out <- c(args_out,"nopictures")
    #3
    if(jackknife) args_out <- c(args_out,"jackknife") else args_out <- c(args_out,"nojackknife")
    #4
    args_out <- c(args_out,paste0("outputformat=",outputformat))
    #5
    args_out <- c(args_out,paste0("outputfiletype=",outputfiletype))
    #7
    if(!is.null(projectionlayers))    args_out <- c(args_out,paste0("projectionlayers=",projectionlayers))
    #10
    if(randomseed) args_out <- c(args_out,"randomseed") else args_out <- c(args_out,"norandomseed")
    #16
    if(removeDuplicates) args_out <- c(args_out,"removeduplicates") else args_out <- c(args_out,"noremoveduplicates")
    #20 & 53-56
    # check if negative
    betas <- c( betamultiplier,beta_threshold,beta_categorical,beta_lqp,beta_hinge)
    if(! is.null(betas) ){
      for(i in 1:length(betas)){
        if(betas[i] <0) stop("betamultiplier has to be positive")
      }
    }
    if (  !is.null(betamultiplier)  ){
      args_out <- c(args_out,paste0("betamultiplier=",betamultiplier))
    } else {
      if(!is.null(beta_threshold)) args_out <- c(args_out,paste0("beta_threshold=",beta_threshold))
      if(!is.null(beta_categorical)) args_out <- c(args_out,paste0("beta_categorical=",beta_categorical))
      if(!is.null(beta_lqp)) args_out <- c(args_out,paste0("beta_lqp=",beta_lqp))
      if(!is.null(beta_hinge)) args_out <- c(args_out,paste0("beta_hinge=",beta_hinge))
    }
    #22
    if(!is.null(biasfile))    args_out <- c(args_out,paste0("biasfile=",biasfile))
    #23
    if(!is.null(testsamplesfile))    args_out <- c(args_out,paste0("testsamplesfile=",testsamplesfile))
    #24&25
    replicates <- as.integer(replicates)
    if(replicates>1 ){
      args_out <- c(args_out,
                    paste0("replicates=",replicates),
                    paste0("replicatetype=",replicatetype) )
    }
    #37
    if(writeplotdata) args_out <- c(args_out,"writeplotdata") else args_out <- c(args_out,"nowriteplotdata")
    #39
    if(extrapolate) args_out <- c(args_out,"extrapolate") else args_out <- c(args_out,"noextrapolate")
    #42
    if(doclamp) args_out <- c(args_out,"doclamp") else args_out <- c(args_out,"nodoclamp")
    #60
    if(!is.null(applythresholdrule))    args_out <- c(args_out,paste0("applythresholdrule=",applythresholdrule))
    
    maximumiterations <- as.integer(maximumiterations)
    args_out <- c(args_out, paste0("maximumiterations=", maximumiterations))
    
    convergencethreshold <- as.integer(convergencethreshold)
    args_out <- c(args_out, paste0("convergencethreshold=", convergencethreshold))
    
    #number_core <- as.integer(number_core)
    #args_out <- c(args_out, paste0("threads=", number_core))
    
    return(args_out)
  }
}



list_maxent_model =list(NA)
list_remain_var = list(NA)
for (i in 1:length(sp_accipitriformes_species)) {
  
  sp_point_species = sp_accipitriformes_species[[i]]
  
  exclude_var = NA
  var_num = 1:nlayers(st_env_r)
  min_contr_var = 0
  
  model0= maxent(st_env_r[[var_num]], sp_point_species,  silent = TRUE, a = RandomDismo,
                 args = prepPara(jackknife=TRUE, betamultiplier =1, replicates = 1),
                 removeDuplicates = FALSE,
                 path ="Maxent\\data" )
  min_var = which.min(model0@results[grepl("contribution$",rownames(model0@results))])
  min_contr_var = model0@results[grepl("contribution$",rownames(model0@results))][min_var]
  var_name = rownames(model0@results)[grepl("contribution$",rownames(model0@results))][min_var]
  exclude_var = substr(var_name,1, nchar(var_name)-13)
  var_num = which(!names(st_env_r) %in% exclude_var)
  
  while(min_contr_var <= 10){
    
    model0= maxent(st_env_r[[var_num]], sp_point_species,  silent = TRUE, a = RandomDismo,
                   args = prepPara(jackknife=TRUE, betamultiplier =1, replicates = 1),
                   removeDuplicates = FALSE,
                   path ="Maxent\\data" )
    
    min_var = which.min(model0@results[grepl("contribution$",rownames(model0@results))])
    var_name = rownames(model0@results)[grepl("contribution$",rownames(model0@results))][min_var]
    min_contr_var = model0@results[grepl("contribution$",rownames(model0@results))][min_var]
    exclude_var = c(exclude_var, substr(var_name,1, nchar(var_name)-13))
    var_num = which(!names(st_env_r) %in% exclude_var)
  } 
  list_maxent_model[[i]]= model0
  list_remain_var[[i]] = names(st_env_r[[var_num]])
  
}


saveRDS(list_maxent_model, file = "Maxent\\Maxent_models\\list_maxent_model_accipitriformes.rds")
readRDS(file = "D:\\Environmental data\\Hendra-Australia\\Maxent\\Maxent_models\\list_maxent_model_accipitriformes.rds") -> list_maxent_model_accipitriformes
saveRDS(list_remain_var, file = "D:\\Environmental data\\Hendra-Australia\\Maxent\\Maxent_models\\list_remain_var_accipitriformes.rds")
readRDS(file = "D:\\Environmental data\\Hendra-Australia\\Maxent\\Maxent_models\\list_remain_var_accipitriformes.rds") -> list_remain_var_accipitriformes


list_accipitriformes_habitat =list(NA)
for (i in 1:length(list_maxent_model)) { 
  list_accipitriformes_habitat[[i]] <- predict(list_maxent_model[[i]], st_env_r)
}

plot(list_accipitriformes_habitat[[1]])

saveRDS(list_accipitriformes_habitat, file = "D:\\Environmental data\\Hendra-Australia\\Maxent\\Maxent_raster\\list_accipitriformes_habitat.rds")
list_accipitriformes_habitat <- readRDS(file = "D:\\Environmental data\\Hendra-Australia\\Maxent\\Maxent_raster\\list_accipitriformes_habitat.rds") 


######## evaluate/threshold

#### k-fold를 5로 잡아서 train과 test dateset 분리하기.
fold.list<-list()
length(list_maxent_model)
for(i in 1:length(list_maxent_model)){
  fold.list[[i]]<-kfold(sp_accipitriformes_species[[i]],k=5)
}

fold.list

#### 각 모델 evaluate 하기
evaluate.mat<-list()
for(i in 1:length(list_maxent_model)){
  
  loc.test<-sp_accipitriformes_species[[i]][fold.list[[i]] == 1,]
  
  bg <- randomPoints(st_env_r, length(loc.test))
  e1 <- evaluate(list_maxent_model[[i]], p=loc.test, a=bg, x=st_env_r)
  
  evaluate.mat[[i]]<-e1
}
evaluate.mat

#### evaluate 한 객체들에서 threshold 값만 빼내기
threshold.mat <- list()
for(i in 1:length(evaluate.mat)){
  t <- threshold(evaluate.mat[[i]])
  threshold.mat[[i]] <- t[["prevalence"]]
}
threshold.mat

#### 각 raster의 픽셀에 나타난 확률값들을 위에서 구한 threshold 기준으로 threshold를 넘으면 1, 넘지 않으면 0으로 하여 출현/비출현 구분.
list_accipitriformes_habitat_filtered <- list_accipitriformes_habitat
for(i in 1:length(list_accipitriformes_habitat_filtered)){
  list_accipitriformes_habitat_filtered[[i]][list_accipitriformes_habitat_filtered[[i]] < threshold.mat[[i]]] <- 0 
  list_accipitriformes_habitat_filtered[[i]][list_accipitriformes_habitat_filtered[[i]] >= threshold.mat[[i]]] <- 1
}

plot(list_accipitriformes_habitat_filtered[[1]]) 


saveRDS(list_accipitriformes_habitat_filtered, file = "Maxent\\Maxent_raster\\list_accipitriformes_habitat_filtered.rds")
list_accipitriformes_habitat_filtered <- readRDS(file = "Maxent\\Maxent_raster\\list_accipitriformes_habitat_filtered.rds") 



#############################################
############    Strigiformes       ############
#############################################


library(readr)
strigiformes <- read_delim("Maxent\\Species_occurence/strigiformes.csv", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE)

#각 species 별로 구분된 spatialpoints 객체를 모아 놓은 list이다. 
strigiformes %>% group_by(species) %>% group_map(~.x) %>%
  lapply(FUN = function(X) {
    SpatialPoints(coords = X[c("decimalLongitude", "decimalLatitude")], proj4string  = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  }
  ) -> sp_strigiformes_species

class(sp_strigiformes_species)
sp_strigiformes_species

#list 안에 총 11 종의 species가 있고, 각 species 별로 몇 개의 point가 찍혀 있는지 확인.
sp_strigiformes_species <- sp_strigiformes_species[unlist(lapply(sp_strigiformes_species, FUN = length)) > 10]
#point 숫자가 최소한 10보다는 큰 종만 골라서 다시 subset -> 10종


strigiformes_species <- as.data.frame(table(strigiformes$species))
strigiformes_species_over10 <- strigiformes_species %>% 
  dplyr::filter(Freq > 10)

write.csv(strigiformes_species_over10, "Maxent\\Species_list\\strigiformes_species_over10.csv")

# load base map
setwd("D:\\Environmental data\\Hendra-Australia\\AUS_basemap\\final")
terrestrial.grid <- readOGR(dsn = "Basemap_clipped_numpoints_final.shp")



#### making representative raster #### maxent 결과 담을 객체
rep_raster <- mask(raster( crs = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"), ext = extent(terrestrial.grid), resolution = res(cr_st_clm), vals = 1 ),terrestrial.grid)
plot(rep_raster)




### parameter preparation
{
  prepPara <- function(userfeatures=NULL, #NULL=autofeature, could be any combination of # c("L", "Q", "H", "H", "P")
                       responsecurves=TRUE,
                       jackknife=TRUE,      
                       outputformat="logistic",
                       outputfiletype="asc", 
                       projectionlayers=NULL,
                       randomseed=FALSE,
                       removeDuplicates=TRUE,
                       betamultiplier=NULL,
                       biasfile=NULL,
                       testsamplesfile=NULL,
                       replicates=1,
                       replicatetype="crossvalidate",
                       writeplotdata=TRUE,
                       extrapolate=TRUE,
                       doclamp=TRUE,
                       beta_threshold=NULL,
                       beta_categorical=NULL,
                       beta_lqp=NULL,
                       beta_hinge=NULL,
                       applythresholdrule=NULL,
                       maximumiterations = 500,
                       convergencethreshold = 1.0E-5,
                       number_core = 1 
  ){
    #20 & 29-33 features, default is autofeature
    if(is.null(userfeatures)){
      args_out <- c("autofeature")
    } else {
      args_out <- c("noautofeature")
      if(grepl("L",userfeatures)) args_out <- c(args_out,"linear") else args_out <- c(args_out,"nolinear")
      if(grepl("Q",userfeatures)) args_out <- c(args_out,"quadratic") else args_out <- c(args_out,"noquadratic")
      if(grepl("H",userfeatures)) args_out <- c(args_out,"hinge") else args_out <- c(args_out,"nohinge")
      if(grepl("P",userfeatures)) args_out <- c(args_out,"product") else args_out <- c(args_out,"noproduct")
      if(grepl("T",userfeatures)) args_out <- c(args_out,"threshold") else args_out <- c(args_out,"nothreshold")
    }
    
    #1 
    if(responsecurves) args_out <- c(args_out,"responsecurves") else args_out <- c(args_out,"noresponsecurves")
    #2
    #if(picture) args_out <- c(args_out,"pictures") else args_out <- c(args_out,"nopictures")
    #3
    if(jackknife) args_out <- c(args_out,"jackknife") else args_out <- c(args_out,"nojackknife")
    #4
    args_out <- c(args_out,paste0("outputformat=",outputformat))
    #5
    args_out <- c(args_out,paste0("outputfiletype=",outputfiletype))
    #7
    if(!is.null(projectionlayers))    args_out <- c(args_out,paste0("projectionlayers=",projectionlayers))
    #10
    if(randomseed) args_out <- c(args_out,"randomseed") else args_out <- c(args_out,"norandomseed")
    #16
    if(removeDuplicates) args_out <- c(args_out,"removeduplicates") else args_out <- c(args_out,"noremoveduplicates")
    #20 & 53-56
    # check if negative
    betas <- c( betamultiplier,beta_threshold,beta_categorical,beta_lqp,beta_hinge)
    if(! is.null(betas) ){
      for(i in 1:length(betas)){
        if(betas[i] <0) stop("betamultiplier has to be positive")
      }
    }
    if (  !is.null(betamultiplier)  ){
      args_out <- c(args_out,paste0("betamultiplier=",betamultiplier))
    } else {
      if(!is.null(beta_threshold)) args_out <- c(args_out,paste0("beta_threshold=",beta_threshold))
      if(!is.null(beta_categorical)) args_out <- c(args_out,paste0("beta_categorical=",beta_categorical))
      if(!is.null(beta_lqp)) args_out <- c(args_out,paste0("beta_lqp=",beta_lqp))
      if(!is.null(beta_hinge)) args_out <- c(args_out,paste0("beta_hinge=",beta_hinge))
    }
    #22
    if(!is.null(biasfile))    args_out <- c(args_out,paste0("biasfile=",biasfile))
    #23
    if(!is.null(testsamplesfile))    args_out <- c(args_out,paste0("testsamplesfile=",testsamplesfile))
    #24&25
    replicates <- as.integer(replicates)
    if(replicates>1 ){
      args_out <- c(args_out,
                    paste0("replicates=",replicates),
                    paste0("replicatetype=",replicatetype) )
    }
    #37
    if(writeplotdata) args_out <- c(args_out,"writeplotdata") else args_out <- c(args_out,"nowriteplotdata")
    #39
    if(extrapolate) args_out <- c(args_out,"extrapolate") else args_out <- c(args_out,"noextrapolate")
    #42
    if(doclamp) args_out <- c(args_out,"doclamp") else args_out <- c(args_out,"nodoclamp")
    #60
    if(!is.null(applythresholdrule))    args_out <- c(args_out,paste0("applythresholdrule=",applythresholdrule))
    
    maximumiterations <- as.integer(maximumiterations)
    args_out <- c(args_out, paste0("maximumiterations=", maximumiterations))
    
    convergencethreshold <- as.integer(convergencethreshold)
    args_out <- c(args_out, paste0("convergencethreshold=", convergencethreshold))
    
    #number_core <- as.integer(number_core)
    #args_out <- c(args_out, paste0("threads=", number_core))
    
    return(args_out)
  }
}



list_maxent_model =list(NA)
list_remain_var = list(NA)
for (i in 1:length(sp_strigiformes_species)) {
  
  sp_point_species = sp_strigiformes_species[[i]]
  
  exclude_var = NA
  var_num = 1:nlayers(st_env_r)
  min_contr_var = 0
  
  model0= maxent(st_env_r[[var_num]], sp_point_species,  silent = TRUE,  a = RandomDismo,
                 args = prepPara(jackknife=TRUE, betamultiplier =1, replicates = 1),
                 removeDuplicates = FALSE,
                 path ="Maxent\\data" )
  min_var = which.min(model0@results[grepl("contribution$",rownames(model0@results))])
  min_contr_var = model0@results[grepl("contribution$",rownames(model0@results))][min_var]
  var_name = rownames(model0@results)[grepl("contribution$",rownames(model0@results))][min_var]
  exclude_var = substr(var_name,1, nchar(var_name)-13)
  var_num = which(!names(st_env_r) %in% exclude_var)
  
  while(min_contr_var <= 10){
    
    model0= maxent(st_env_r[[var_num]], sp_point_species,  silent = TRUE, a = RandomDismo,
                   args = prepPara(jackknife=TRUE, betamultiplier =1, replicates = 1),
                   removeDuplicates = FALSE,
                   path ="Maxent\\data" )
    
    min_var = which.min(model0@results[grepl("contribution$",rownames(model0@results))])
    var_name = rownames(model0@results)[grepl("contribution$",rownames(model0@results))][min_var]
    min_contr_var = model0@results[grepl("contribution$",rownames(model0@results))][min_var]
    exclude_var = c(exclude_var, substr(var_name,1, nchar(var_name)-13))
    var_num = which(!names(st_env_r) %in% exclude_var)
  } 
  list_maxent_model[[i]]= model0
  list_remain_var[[i]] = names(st_env_r[[var_num]])
  
}


saveRDS(list_maxent_model, file = "Maxent\\Maxent_models\\list_maxent_model_strigiformes.rds")
readRDS(file = "D:\\Environmental data\\Hendra-Australia\\Maxent\\Maxent_models\\list_maxent_model_strigiformes.rds") -> list_maxent_model_strigiformes
saveRDS(list_remain_var, file = "D:\\Environmental data\\Hendra-Australia\\Maxent\\Maxent_models\\list_remain_var_strigiformes.rds")
readRDS(file = "D:\\Environmental data\\Hendra-Australia\\Maxent\\Maxent_models\\list_remain_var_strigiformes.rds") -> list_remain_var_strigiformes


list_strigiformes_habitat =list(NA)
for (i in 1:length(list_maxent_model)) { 
  list_strigiformes_habitat[[i]] <- predict(list_maxent_model[[i]], st_env_r)
}

plot(list_strigiformes_habitat[[5]])

saveRDS(list_strigiformes_habitat, file = "Maxent\\Maxent_raster\\list_strigiformes_habitat.rds")
list_strigiformes_habitat <- readRDS(file = "Maxent\\Maxent_raster\\list_strigiformes_habitat.rds") 


######## evaluate/threshold

#### k-fold를 5로 잡아서 train과 test dateset 분리하기.
fold.list<-list()
length(list_maxent_model)
for(i in 1:length(list_maxent_model)){
  fold.list[[i]]<-kfold(sp_strigiformes_species[[i]],k=5)
}

fold.list

#### 각 모델 evaluate 하기
evaluate.mat<-list()
for(i in 1:length(list_maxent_model)){
  
  loc.test<-sp_strigiformes_species[[i]][fold.list[[i]] == 1,]
  
  bg <- randomPoints(st_env_r, length(loc.test))
  e1 <- evaluate(list_maxent_model[[i]], p=loc.test, a=bg, x=st_env_r)
  
  evaluate.mat[[i]]<-e1
}
evaluate.mat

#### evaluate 한 객체들에서 threshold 값만 빼내기
threshold.mat <- list()
for(i in 1:length(evaluate.mat)){
  t <- threshold(evaluate.mat[[i]])
  threshold.mat[[i]] <- t[["prevalence"]]
}
threshold.mat

#### 각 raster의 픽셀에 나타난 확률값들을 위에서 구한 threshold 기준으로 threshold를 넘으면 1, 넘지 않으면 0으로 하여 출현/비출현 구분.
list_strigiformes_habitat_filtered <- list_strigiformes_habitat
for(i in 1:length(list_strigiformes_habitat_filtered)){
  list_strigiformes_habitat_filtered[[i]][list_strigiformes_habitat_filtered[[i]] < threshold.mat[[i]]] <- 0 
  list_strigiformes_habitat_filtered[[i]][list_strigiformes_habitat_filtered[[i]] >= threshold.mat[[i]]] <- 1
}

plot(list_strigiformes_habitat_filtered[[1]]) 


saveRDS(list_strigiformes_habitat_filtered, file = "Maxent\\Maxent_raster\\list_strigiformes_habitat_filtered.rds")
list_strigiformes_habitat_filtered <- readRDS(file = "Maxent_raster\\list_strigiformes_habitat_filtered.rds") 



#############################################
############    Carnivora       ############
#############################################


library(readr)
carnivora <- read_delim("D:\\Environmental data\\Hendra-Australia\\Maxent\\Species_occurence/carnivora.csv", 
                           delim = "\t", escape_double = FALSE, 
                           trim_ws = TRUE)

#각 species 별로 구분된 spatialpoints 객체를 모아 놓은 list이다. 
carnivora %>% group_by(species) %>% group_map(~.x) %>%
  lapply(FUN = function(X) {
    SpatialPoints(coords = X[c("decimalLongitude", "decimalLatitude")], proj4string  = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  }
  ) -> sp_carnivora_species

class(sp_carnivora_species)
sp_carnivora_species

#list 안에 총 8 종의 species가 있고, 각 species 별로 몇 개의 point가 찍혀 있는지 확인.
sp_carnivora_species <- sp_carnivora_species[unlist(lapply(sp_carnivora_species, FUN = length)) > 10]
#point 숫자가 최소한 10보다는 큰 종만 골라서 다시 subset -> 5종


carnivora_species <- as.data.frame(table(carnivora$species))
carnivora_species_over10 <- carnivora_species %>% 
  dplyr::filter(Freq > 10)

write.csv(carnivora_species_over10, "D:\\Environmental data\\Hendra-Australia\\Maxent\\Species_list\\carnivora_species_over10.csv")

# load base map
setwd("D:\\Environmental data\\Hendra-Australia\\AUS_basemap\\final")
terrestrial.grid <- readOGR(dsn = "Basemap_clipped_numpoints_final.shp")




### parameter preparation
{
  prepPara <- function(userfeatures=NULL, #NULL=autofeature, could be any combination of # c("L", "Q", "H", "H", "P")
                       responsecurves=TRUE,
                       jackknife=TRUE,      
                       outputformat="logistic",
                       outputfiletype="asc", 
                       projectionlayers=NULL,
                       randomseed=FALSE,
                       removeDuplicates=TRUE,
                       betamultiplier=NULL,
                       biasfile=NULL,
                       testsamplesfile=NULL,
                       replicates=1,
                       replicatetype="crossvalidate",
                       writeplotdata=TRUE,
                       extrapolate=TRUE,
                       doclamp=TRUE,
                       beta_threshold=NULL,
                       beta_categorical=NULL,
                       beta_lqp=NULL,
                       beta_hinge=NULL,
                       applythresholdrule=NULL,
                       maximumiterations = 500,
                       convergencethreshold = 1.0E-5,
                       number_core = 1 
  ){
    #20 & 29-33 features, default is autofeature
    if(is.null(userfeatures)){
      args_out <- c("autofeature")
    } else {
      args_out <- c("noautofeature")
      if(grepl("L",userfeatures)) args_out <- c(args_out,"linear") else args_out <- c(args_out,"nolinear")
      if(grepl("Q",userfeatures)) args_out <- c(args_out,"quadratic") else args_out <- c(args_out,"noquadratic")
      if(grepl("H",userfeatures)) args_out <- c(args_out,"hinge") else args_out <- c(args_out,"nohinge")
      if(grepl("P",userfeatures)) args_out <- c(args_out,"product") else args_out <- c(args_out,"noproduct")
      if(grepl("T",userfeatures)) args_out <- c(args_out,"threshold") else args_out <- c(args_out,"nothreshold")
    }
    
    #1 
    if(responsecurves) args_out <- c(args_out,"responsecurves") else args_out <- c(args_out,"noresponsecurves")
    #2
    #if(picture) args_out <- c(args_out,"pictures") else args_out <- c(args_out,"nopictures")
    #3
    if(jackknife) args_out <- c(args_out,"jackknife") else args_out <- c(args_out,"nojackknife")
    #4
    args_out <- c(args_out,paste0("outputformat=",outputformat))
    #5
    args_out <- c(args_out,paste0("outputfiletype=",outputfiletype))
    #7
    if(!is.null(projectionlayers))    args_out <- c(args_out,paste0("projectionlayers=",projectionlayers))
    #10
    if(randomseed) args_out <- c(args_out,"randomseed") else args_out <- c(args_out,"norandomseed")
    #16
    if(removeDuplicates) args_out <- c(args_out,"removeduplicates") else args_out <- c(args_out,"noremoveduplicates")
    #20 & 53-56
    # check if negative
    betas <- c( betamultiplier,beta_threshold,beta_categorical,beta_lqp,beta_hinge)
    if(! is.null(betas) ){
      for(i in 1:length(betas)){
        if(betas[i] <0) stop("betamultiplier has to be positive")
      }
    }
    if (  !is.null(betamultiplier)  ){
      args_out <- c(args_out,paste0("betamultiplier=",betamultiplier))
    } else {
      if(!is.null(beta_threshold)) args_out <- c(args_out,paste0("beta_threshold=",beta_threshold))
      if(!is.null(beta_categorical)) args_out <- c(args_out,paste0("beta_categorical=",beta_categorical))
      if(!is.null(beta_lqp)) args_out <- c(args_out,paste0("beta_lqp=",beta_lqp))
      if(!is.null(beta_hinge)) args_out <- c(args_out,paste0("beta_hinge=",beta_hinge))
    }
    #22
    if(!is.null(biasfile))    args_out <- c(args_out,paste0("biasfile=",biasfile))
    #23
    if(!is.null(testsamplesfile))    args_out <- c(args_out,paste0("testsamplesfile=",testsamplesfile))
    #24&25
    replicates <- as.integer(replicates)
    if(replicates>1 ){
      args_out <- c(args_out,
                    paste0("replicates=",replicates),
                    paste0("replicatetype=",replicatetype) )
    }
    #37
    if(writeplotdata) args_out <- c(args_out,"writeplotdata") else args_out <- c(args_out,"nowriteplotdata")
    #39
    if(extrapolate) args_out <- c(args_out,"extrapolate") else args_out <- c(args_out,"noextrapolate")
    #42
    if(doclamp) args_out <- c(args_out,"doclamp") else args_out <- c(args_out,"nodoclamp")
    #60
    if(!is.null(applythresholdrule))    args_out <- c(args_out,paste0("applythresholdrule=",applythresholdrule))
    
    maximumiterations <- as.integer(maximumiterations)
    args_out <- c(args_out, paste0("maximumiterations=", maximumiterations))
    
    convergencethreshold <- as.integer(convergencethreshold)
    args_out <- c(args_out, paste0("convergencethreshold=", convergencethreshold))
    
    #number_core <- as.integer(number_core)
    #args_out <- c(args_out, paste0("threads=", number_core))
    
    return(args_out)
  }
}



list_maxent_model =list(NA)
list_remain_var = list(NA)
for (i in 1:length(sp_carnivora_species)) {
  
  sp_point_species = sp_carnivora_species[[i]]
  
  exclude_var = NA
  var_num = 1:nlayers(st_env_r)
  min_contr_var = 0
  
  model0= maxent(st_env_r[[var_num]], sp_point_species,  silent = TRUE, a = RandomDismo,
                 args = prepPara(jackknife=TRUE, betamultiplier =1, replicates = 1),
                 removeDuplicates = FALSE,
                 path ="Maxent\\data" )
  min_var = which.min(model0@results[grepl("contribution$",rownames(model0@results))])
  min_contr_var = model0@results[grepl("contribution$",rownames(model0@results))][min_var]
  var_name = rownames(model0@results)[grepl("contribution$",rownames(model0@results))][min_var]
  exclude_var = substr(var_name,1, nchar(var_name)-13)
  var_num = which(!names(st_env_r) %in% exclude_var)
  
  while(min_contr_var <= 10){
    
    model0= maxent(st_env_r[[var_num]], sp_point_species,  silent = TRUE, a = RandomDismo,
                   args = prepPara(jackknife=TRUE, betamultiplier =1, replicates = 1),
                   removeDuplicates = FALSE,
                   path ="Maxent\\data" )
    
    min_var = which.min(model0@results[grepl("contribution$",rownames(model0@results))])
    var_name = rownames(model0@results)[grepl("contribution$",rownames(model0@results))][min_var]
    min_contr_var = model0@results[grepl("contribution$",rownames(model0@results))][min_var]
    exclude_var = c(exclude_var, substr(var_name,1, nchar(var_name)-13))
    var_num = which(!names(st_env_r) %in% exclude_var)
  } 
  list_maxent_model[[i]]= model0
  list_remain_var[[i]] = names(st_env_r[[var_num]])
  
}


saveRDS(list_maxent_model, file = "Maxent\\Maxent_models\\list_maxent_model_carnivora.rds")
readRDS(file = "D:\\Environmental data\\Hendra-Australia\\Maxent\\Maxent_models\\list_maxent_model_carnivora.rds") -> list_maxent_model_carnivora
saveRDS(list_remain_var, file = "D:\\Environmental data\\Hendra-Australia\\Maxent\\Maxent_models\\list_remain_var_carnivora.rds")
readRDS(file = "D:\\Environmental data\\Hendra-Australia\\Maxent\\Maxent_models\\list_remain_var_carnivora.rds") -> list_remain_var_carnivora


list_carnivora_habitat =list(NA)
for (i in 1:length(list_maxent_model)) { 
  list_carnivora_habitat[[i]] <- predict(list_maxent_model[[i]], st_env_r)
}

plot(list_carnivora_habitat[[5]])

saveRDS(list_carnivora_habitat, file = "Maxent\\Maxent_raster\\list_carnivora_habitat.rds")
list_carnivora_habitat <- readRDS(file = "Maxent\\Maxent_raster\\list_carnivora_habitat.rds") 


######## evaluate/threshold

#### k-fold를 5로 잡아서 train과 test dateset 분리하기.
fold.list<-list()
length(list_maxent_model)
for(i in 1:length(list_maxent_model)){
  fold.list[[i]]<-kfold(sp_carnivora_species[[i]],k=5)
}

fold.list

#### 각 모델 evaluate 하기
evaluate.mat<-list()
for(i in 1:length(list_maxent_model)){
  
  loc.test<-sp_carnivora_species[[i]][fold.list[[i]] == 1,]
  
  bg <- randomPoints(st_env_r, length(loc.test))
  e1 <- evaluate(list_maxent_model[[i]], p=loc.test, a=bg, x=st_env_r)
  
  evaluate.mat[[i]]<-e1
}
evaluate.mat

#### evaluate 한 객체들에서 threshold 값만 빼내기
threshold.mat <- list()
for(i in 1:length(evaluate.mat)){
  t <- threshold(evaluate.mat[[i]])
  threshold.mat[[i]] <- t[["prevalence"]]
}
threshold.mat

#### 각 raster의 픽셀에 나타난 확률값들을 위에서 구한 threshold 기준으로 threshold를 넘으면 1, 넘지 않으면 0으로 하여 출현/비출현 구분.
list_carnivora_habitat_filtered <- list_carnivora_habitat
for(i in 1:length(list_carnivora_habitat_filtered)){
  list_carnivora_habitat_filtered[[i]][list_carnivora_habitat_filtered[[i]] < threshold.mat[[i]]] <- 0 
  list_carnivora_habitat_filtered[[i]][list_carnivora_habitat_filtered[[i]] >= threshold.mat[[i]]] <- 1
}

plot(list_carnivora_habitat_filtered[[4]]) 


saveRDS(list_carnivora_habitat_filtered, file = "Maxent\\Maxent_raster\\list_carnivora_habitat_filtered.rds")
list_carnivora_habitat_filtered <- readRDS(file = "D:\\Environmental data\\Maxent\\Maxent_raster\\list_carnivora_habitat_filtered.rds") 




#############################################
############    Colubridae       ############
#############################################


library(readr)
colubridae <- read_delim("D:\\Environmental data\\Hendra-Australia\\Maxent\\Species_occurence/colubridae.csv", 
                        delim = "\t", escape_double = FALSE, 
                        trim_ws = TRUE)

#각 species 별로 구분된 spatialpoints 객체를 모아 놓은 list이다. 
colubridae %>% group_by(species) %>% group_map(~.x) %>%
  lapply(FUN = function(X) {
    SpatialPoints(coords = X[c("decimalLongitude", "decimalLatitude")], proj4string  = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  }
  ) -> sp_colubridae_species

class(sp_colubridae_species)
sp_colubridae_species

#list 안에 총 11 종의 species가 있고, 각 species 별로 몇 개의 point가 찍혀 있는지 확인.
sp_colubridae_species <- sp_colubridae_species[unlist(lapply(sp_colubridae_species, FUN = length)) > 10]
#point 숫자가 최소한 10보다는 큰 종만 골라서 다시 subset -> 7종


colubridae_species <- as.data.frame(table(colubridae$species))
colubridae_species_over10 <- colubridae_species %>% 
  dplyr::filter(Freq > 10)

write.csv(colubridae_species_over10, "D:\\Environmental data\\Hendra-Australia\\Maxent\\Species_list\\colubridae_species_over10.csv")

# load base map
setwd("D:\\Environmental data\\Hendra-Australia\\AUS_basemap\\final")
terrestrial.grid <- readOGR(dsn = "Basemap_clipped_numpoints_final.shp")




### parameter preparation
{
  prepPara <- function(userfeatures=NULL, #NULL=autofeature, could be any combination of # c("L", "Q", "H", "H", "P")
                       responsecurves=TRUE,
                       jackknife=TRUE,      
                       outputformat="logistic",
                       outputfiletype="asc", 
                       projectionlayers=NULL,
                       randomseed=FALSE,
                       removeDuplicates=TRUE,
                       betamultiplier=NULL,
                       biasfile=NULL,
                       testsamplesfile=NULL,
                       replicates=1,
                       replicatetype="crossvalidate",
                       writeplotdata=TRUE,
                       extrapolate=TRUE,
                       doclamp=TRUE,
                       beta_threshold=NULL,
                       beta_categorical=NULL,
                       beta_lqp=NULL,
                       beta_hinge=NULL,
                       applythresholdrule=NULL,
                       maximumiterations = 500,
                       convergencethreshold = 1.0E-5,
                       number_core = 1 
  ){
    #20 & 29-33 features, default is autofeature
    if(is.null(userfeatures)){
      args_out <- c("autofeature")
    } else {
      args_out <- c("noautofeature")
      if(grepl("L",userfeatures)) args_out <- c(args_out,"linear") else args_out <- c(args_out,"nolinear")
      if(grepl("Q",userfeatures)) args_out <- c(args_out,"quadratic") else args_out <- c(args_out,"noquadratic")
      if(grepl("H",userfeatures)) args_out <- c(args_out,"hinge") else args_out <- c(args_out,"nohinge")
      if(grepl("P",userfeatures)) args_out <- c(args_out,"product") else args_out <- c(args_out,"noproduct")
      if(grepl("T",userfeatures)) args_out <- c(args_out,"threshold") else args_out <- c(args_out,"nothreshold")
    }
    
    #1 
    if(responsecurves) args_out <- c(args_out,"responsecurves") else args_out <- c(args_out,"noresponsecurves")
    #2
    #if(picture) args_out <- c(args_out,"pictures") else args_out <- c(args_out,"nopictures")
    #3
    if(jackknife) args_out <- c(args_out,"jackknife") else args_out <- c(args_out,"nojackknife")
    #4
    args_out <- c(args_out,paste0("outputformat=",outputformat))
    #5
    args_out <- c(args_out,paste0("outputfiletype=",outputfiletype))
    #7
    if(!is.null(projectionlayers))    args_out <- c(args_out,paste0("projectionlayers=",projectionlayers))
    #10
    if(randomseed) args_out <- c(args_out,"randomseed") else args_out <- c(args_out,"norandomseed")
    #16
    if(removeDuplicates) args_out <- c(args_out,"removeduplicates") else args_out <- c(args_out,"noremoveduplicates")
    #20 & 53-56
    # check if negative
    betas <- c( betamultiplier,beta_threshold,beta_categorical,beta_lqp,beta_hinge)
    if(! is.null(betas) ){
      for(i in 1:length(betas)){
        if(betas[i] <0) stop("betamultiplier has to be positive")
      }
    }
    if (  !is.null(betamultiplier)  ){
      args_out <- c(args_out,paste0("betamultiplier=",betamultiplier))
    } else {
      if(!is.null(beta_threshold)) args_out <- c(args_out,paste0("beta_threshold=",beta_threshold))
      if(!is.null(beta_categorical)) args_out <- c(args_out,paste0("beta_categorical=",beta_categorical))
      if(!is.null(beta_lqp)) args_out <- c(args_out,paste0("beta_lqp=",beta_lqp))
      if(!is.null(beta_hinge)) args_out <- c(args_out,paste0("beta_hinge=",beta_hinge))
    }
    #22
    if(!is.null(biasfile))    args_out <- c(args_out,paste0("biasfile=",biasfile))
    #23
    if(!is.null(testsamplesfile))    args_out <- c(args_out,paste0("testsamplesfile=",testsamplesfile))
    #24&25
    replicates <- as.integer(replicates)
    if(replicates>1 ){
      args_out <- c(args_out,
                    paste0("replicates=",replicates),
                    paste0("replicatetype=",replicatetype) )
    }
    #37
    if(writeplotdata) args_out <- c(args_out,"writeplotdata") else args_out <- c(args_out,"nowriteplotdata")
    #39
    if(extrapolate) args_out <- c(args_out,"extrapolate") else args_out <- c(args_out,"noextrapolate")
    #42
    if(doclamp) args_out <- c(args_out,"doclamp") else args_out <- c(args_out,"nodoclamp")
    #60
    if(!is.null(applythresholdrule))    args_out <- c(args_out,paste0("applythresholdrule=",applythresholdrule))
    
    maximumiterations <- as.integer(maximumiterations)
    args_out <- c(args_out, paste0("maximumiterations=", maximumiterations))
    
    convergencethreshold <- as.integer(convergencethreshold)
    args_out <- c(args_out, paste0("convergencethreshold=", convergencethreshold))
    
    #number_core <- as.integer(number_core)
    #args_out <- c(args_out, paste0("threads=", number_core))
    
    return(args_out)
  }
}



list_maxent_model =list(NA)
list_remain_var = list(NA)
for (i in 1:length(sp_colubridae_species)) {
  
  sp_point_species = sp_colubridae_species[[i]]
  
  exclude_var = NA
  var_num = 1:nlayers(st_env_r)
  min_contr_var = 0
  
  model0= maxent(st_env_r[[var_num]], sp_point_species,  silent = TRUE, a = RandomDismo,
                 args = prepPara(jackknife=TRUE, betamultiplier =1, replicates = 1),
                 removeDuplicates = FALSE,
                 path ="Maxent\\data" )
  min_var = which.min(model0@results[grepl("contribution$",rownames(model0@results))])
  min_contr_var = model0@results[grepl("contribution$",rownames(model0@results))][min_var]
  var_name = rownames(model0@results)[grepl("contribution$",rownames(model0@results))][min_var]
  exclude_var = substr(var_name,1, nchar(var_name)-13)
  var_num = which(!names(st_env_r) %in% exclude_var)
  
  while(min_contr_var <= 10){
    
    model0= maxent(st_env_r[[var_num]], sp_point_species,  silent = TRUE, a = RandomDismo,
                   args = prepPara(jackknife=TRUE, betamultiplier =1, replicates = 1),
                   removeDuplicates = FALSE,
                   path ="Maxent\\data" )
    
    min_var = which.min(model0@results[grepl("contribution$",rownames(model0@results))])
    var_name = rownames(model0@results)[grepl("contribution$",rownames(model0@results))][min_var]
    min_contr_var = model0@results[grepl("contribution$",rownames(model0@results))][min_var]
    exclude_var = c(exclude_var, substr(var_name,1, nchar(var_name)-13))
    var_num = which(!names(st_env_r) %in% exclude_var)
  } 
  list_maxent_model[[i]]= model0
  list_remain_var[[i]] = names(st_env_r[[var_num]])
  
}


saveRDS(list_maxent_model, file = "Maxent\\Maxent_models\\list_maxent_model_colubridae.rds")
readRDS(file = "D:\\Environmental data\\Hendra-Australia\\Maxent\\Maxent_models\\list_maxent_model_colubridae.rds") -> list_maxent_model_colubridae
saveRDS(list_remain_var, file = "D:\\Environmental data\\Hendra-Australia\\Maxent\\Maxent_models\\list_remain_var_colubridae.rds")
readRDS(file = "D:\\Environmental data\\Hendra-Australia\\Maxent\\Maxent_models\\list_remain_var_colubridae.rds") -> list_remain_var_colubridae


list_colubridae_habitat =list(NA)
for (i in 1:length(list_maxent_model)) { 
  list_colubridae_habitat[[i]] <- predict(list_maxent_model[[i]], st_env_r)
}

plot(list_colubridae_habitat[[5]])

saveRDS(list_colubridae_habitat, file = "D:\\Environmental data\\Hendra-Australia\\Maxent\\Maxent_raster\\list_colubridae_habitat.rds")
list_colubridae_habitat <- readRDS(file = "D:\\Environmental data\\Hendra-Australia\\Maxent\\Maxent_raster\\list_colubridae_habitat.rds") 


######## evaluate/threshold

#### k-fold를 5로 잡아서 train과 test dateset 분리하기.
fold.list<-list()
length(list_maxent_model)
for(i in 1:length(list_maxent_model)){
  fold.list[[i]]<-kfold(sp_colubridae_species[[i]],k=5)
}

fold.list

#### 각 모델 evaluate 하기
evaluate.mat<-list()
for(i in 1:length(list_maxent_model)){
  
  loc.test<-sp_colubridae_species[[i]][fold.list[[i]] == 1,]
  
  bg <- randomPoints(st_env_r, length(loc.test))
  e1 <- evaluate(list_maxent_model[[i]], p=loc.test, a=bg, x=st_env_r)
  
  evaluate.mat[[i]]<-e1
}
evaluate.mat

#### evaluate 한 객체들에서 threshold 값만 빼내기
threshold.mat <- list()
for(i in 1:length(evaluate.mat)){
  t <- threshold(evaluate.mat[[i]])
  threshold.mat[[i]] <- t[["prevalence"]]
}
threshold.mat

#### 각 raster의 픽셀에 나타난 확률값들을 위에서 구한 threshold 기준으로 threshold를 넘으면 1, 넘지 않으면 0으로 하여 출현/비출현 구분.
list_colubridae_habitat_filtered <- list_colubridae_habitat
for(i in 1:length(list_colubridae_habitat_filtered)){
  list_colubridae_habitat_filtered[[i]][list_colubridae_habitat_filtered[[i]] < threshold.mat[[i]]] <- 0 
  list_colubridae_habitat_filtered[[i]][list_colubridae_habitat_filtered[[i]] >= threshold.mat[[i]]] <- 1
}

plot(list_colubridae_habitat_filtered[[4]]) 


saveRDS(list_colubridae_habitat_filtered, file = "D:\\Environmental data\\Hendra-Australia\\Maxent\\Maxent_raster\\list_colubridae_habitat_filtered.rds")
list_colubridae_habitat_filtered <- readRDS(file = "D:\\Environmental data\\Maxent\\Maxent_raster\\list_colubridae_habitat_filtered.rds") 





#############################################
############    Pythonidae       ############
#############################################


library(readr)
pythonidae <- read_delim("Maxent\\Species_occurence/pythonidae.csv", 
                         delim = "\t", escape_double = FALSE, 
                         trim_ws = TRUE)

#각 species 별로 구분된 spatialpoints 객체를 모아 놓은 list이다. 
pythonidae %>% group_by(species) %>% group_map(~.x) %>%
  lapply(FUN = function(X) {
    SpatialPoints(coords = X[c("decimalLongitude", "decimalLatitude")], proj4string  = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  }
  ) -> sp_pythonidae_species

class(sp_pythonidae_species)
sp_pythonidae_species

#list 안에 총 11 종의 species가 있고, 각 species 별로 몇 개의 point가 찍혀 있는지 확인.
sp_pythonidae_species <- sp_pythonidae_species[unlist(lapply(sp_pythonidae_species, FUN = length)) > 10]
#point 숫자가 최소한 10보다는 큰 종만 골라서 다시 subset -> 10종


pythonidae_species <- as.data.frame(table(pythonidae$species))
pythonidae_species_over10 <- pythonidae_species %>% 
  dplyr::filter(Freq > 10)

write.csv(pythonidae_species_over10, "Maxent\\Species_list\\pythonidae_species_over10.csv")

# load base map
setwd("D:\\Environmental data\\Hendra-Australia\\AUS_basemap\\final")
terrestrial.grid <- readOGR(dsn = "Basemap_clipped_numpoints_final.shp")




### parameter preparation
{
  prepPara <- function(userfeatures=NULL, #NULL=autofeature, could be any combination of # c("L", "Q", "H", "H", "P")
                       responsecurves=TRUE,
                       jackknife=TRUE,      
                       outputformat="logistic",
                       outputfiletype="asc", 
                       projectionlayers=NULL,
                       randomseed=FALSE,
                       removeDuplicates=TRUE,
                       betamultiplier=NULL,
                       biasfile=NULL,
                       testsamplesfile=NULL,
                       replicates=1,
                       replicatetype="crossvalidate",
                       writeplotdata=TRUE,
                       extrapolate=TRUE,
                       doclamp=TRUE,
                       beta_threshold=NULL,
                       beta_categorical=NULL,
                       beta_lqp=NULL,
                       beta_hinge=NULL,
                       applythresholdrule=NULL,
                       maximumiterations = 500,
                       convergencethreshold = 1.0E-5,
                       number_core = 1 
  ){
    #20 & 29-33 features, default is autofeature
    if(is.null(userfeatures)){
      args_out <- c("autofeature")
    } else {
      args_out <- c("noautofeature")
      if(grepl("L",userfeatures)) args_out <- c(args_out,"linear") else args_out <- c(args_out,"nolinear")
      if(grepl("Q",userfeatures)) args_out <- c(args_out,"quadratic") else args_out <- c(args_out,"noquadratic")
      if(grepl("H",userfeatures)) args_out <- c(args_out,"hinge") else args_out <- c(args_out,"nohinge")
      if(grepl("P",userfeatures)) args_out <- c(args_out,"product") else args_out <- c(args_out,"noproduct")
      if(grepl("T",userfeatures)) args_out <- c(args_out,"threshold") else args_out <- c(args_out,"nothreshold")
    }
    
    #1 
    if(responsecurves) args_out <- c(args_out,"responsecurves") else args_out <- c(args_out,"noresponsecurves")
    #2
    #if(picture) args_out <- c(args_out,"pictures") else args_out <- c(args_out,"nopictures")
    #3
    if(jackknife) args_out <- c(args_out,"jackknife") else args_out <- c(args_out,"nojackknife")
    #4
    args_out <- c(args_out,paste0("outputformat=",outputformat))
    #5
    args_out <- c(args_out,paste0("outputfiletype=",outputfiletype))
    #7
    if(!is.null(projectionlayers))    args_out <- c(args_out,paste0("projectionlayers=",projectionlayers))
    #10
    if(randomseed) args_out <- c(args_out,"randomseed") else args_out <- c(args_out,"norandomseed")
    #16
    if(removeDuplicates) args_out <- c(args_out,"removeduplicates") else args_out <- c(args_out,"noremoveduplicates")
    #20 & 53-56
    # check if negative
    betas <- c( betamultiplier,beta_threshold,beta_categorical,beta_lqp,beta_hinge)
    if(! is.null(betas) ){
      for(i in 1:length(betas)){
        if(betas[i] <0) stop("betamultiplier has to be positive")
      }
    }
    if (  !is.null(betamultiplier)  ){
      args_out <- c(args_out,paste0("betamultiplier=",betamultiplier))
    } else {
      if(!is.null(beta_threshold)) args_out <- c(args_out,paste0("beta_threshold=",beta_threshold))
      if(!is.null(beta_categorical)) args_out <- c(args_out,paste0("beta_categorical=",beta_categorical))
      if(!is.null(beta_lqp)) args_out <- c(args_out,paste0("beta_lqp=",beta_lqp))
      if(!is.null(beta_hinge)) args_out <- c(args_out,paste0("beta_hinge=",beta_hinge))
    }
    #22
    if(!is.null(biasfile))    args_out <- c(args_out,paste0("biasfile=",biasfile))
    #23
    if(!is.null(testsamplesfile))    args_out <- c(args_out,paste0("testsamplesfile=",testsamplesfile))
    #24&25
    replicates <- as.integer(replicates)
    if(replicates>1 ){
      args_out <- c(args_out,
                    paste0("replicates=",replicates),
                    paste0("replicatetype=",replicatetype) )
    }
    #37
    if(writeplotdata) args_out <- c(args_out,"writeplotdata") else args_out <- c(args_out,"nowriteplotdata")
    #39
    if(extrapolate) args_out <- c(args_out,"extrapolate") else args_out <- c(args_out,"noextrapolate")
    #42
    if(doclamp) args_out <- c(args_out,"doclamp") else args_out <- c(args_out,"nodoclamp")
    #60
    if(!is.null(applythresholdrule))    args_out <- c(args_out,paste0("applythresholdrule=",applythresholdrule))
    
    maximumiterations <- as.integer(maximumiterations)
    args_out <- c(args_out, paste0("maximumiterations=", maximumiterations))
    
    convergencethreshold <- as.integer(convergencethreshold)
    args_out <- c(args_out, paste0("convergencethreshold=", convergencethreshold))
    
    #number_core <- as.integer(number_core)
    #args_out <- c(args_out, paste0("threads=", number_core))
    
    return(args_out)
  }
}



list_maxent_model =list(NA)
list_remain_var = list(NA)
for (i in 1:length(sp_pythonidae_species)) {
  
  sp_point_species = sp_pythonidae_species[[i]]
  
  exclude_var = NA
  var_num = 1:nlayers(st_env_r)
  min_contr_var = 0
  
  model0= maxent(st_env_r[[var_num]], sp_point_species,  silent = TRUE, a = RandomDismo,
                 args = prepPara(jackknife=TRUE, betamultiplier =1, replicates = 1),
                 removeDuplicates = FALSE,
                 path ="Maxent\\data" )
  min_var = which.min(model0@results[grepl("contribution$",rownames(model0@results))])
  min_contr_var = model0@results[grepl("contribution$",rownames(model0@results))][min_var]
  var_name = rownames(model0@results)[grepl("contribution$",rownames(model0@results))][min_var]
  exclude_var = substr(var_name,1, nchar(var_name)-13)
  var_num = which(!names(st_env_r) %in% exclude_var)
  
  while(min_contr_var <= 10){
    
    model0= maxent(st_env_r[[var_num]], sp_point_species,  silent = TRUE, a = RandomDismo,
                   args = prepPara(jackknife=TRUE, betamultiplier =1, replicates = 1),
                   removeDuplicates = FALSE,
                   path ="Maxent\\data" )
    
    min_var = which.min(model0@results[grepl("contribution$",rownames(model0@results))])
    var_name = rownames(model0@results)[grepl("contribution$",rownames(model0@results))][min_var]
    min_contr_var = model0@results[grepl("contribution$",rownames(model0@results))][min_var]
    exclude_var = c(exclude_var, substr(var_name,1, nchar(var_name)-13))
    var_num = which(!names(st_env_r) %in% exclude_var)
  } 
  list_maxent_model[[i]]= model0
  list_remain_var[[i]] = names(st_env_r[[var_num]])
  
}


saveRDS(list_maxent_model, file = "Maxent\\Maxent_models\\list_maxent_model_pythonidae.rds")
readRDS(file = "Maxent\\Maxent_models\\list_maxent_model_pythonidae.rds") -> list_maxent_model_pythonidae
saveRDS(list_remain_var, file = "Maxent\\Maxent_models\\list_remain_var_pythonidae.rds")
readRDS(file = "Maxent\\Maxent_models\\list_remain_var_pythonidae.rds") -> list_remain_var_pythonidae


list_pythonidae_habitat =list(NA)
for (i in 1:length(list_maxent_model)) { 
  list_pythonidae_habitat[[i]] <- predict(list_maxent_model[[i]], st_env_r)
}

plot(list_pythonidae_habitat[[5]])

saveRDS(list_pythonidae_habitat, file = "Maxent\\Maxent_raster\\list_pythonidae_habitat.rds")
list_pythonidae_habitat <- readRDS(file = "Maxent\\Maxent_raster\\list_pythonidae_habitat.rds") 


######## evaluate/threshold

#### k-fold를 5로 잡아서 train과 test dateset 분리하기.
fold.list<-list()
length(list_maxent_model)
for(i in 1:length(list_maxent_model)){
  fold.list[[i]]<-kfold(sp_pythonidae_species[[i]],k=5)
}

fold.list

#### 각 모델 evaluate 하기
evaluate.mat<-list()
for(i in 1:length(list_maxent_model)){
  
  loc.test<-sp_pythonidae_species[[i]][fold.list[[i]] == 1,]
  
  bg <- randomPoints(st_env_r, length(loc.test))
  e1 <- evaluate(list_maxent_model[[i]], p=loc.test, a=bg, x=st_env_r)
  
  evaluate.mat[[i]]<-e1
}
evaluate.mat

#### evaluate 한 객체들에서 threshold 값만 빼내기
threshold.mat <- list()
for(i in 1:length(evaluate.mat)){
  t <- threshold(evaluate.mat[[i]])
  threshold.mat[[i]] <- t[["prevalence"]]
}
threshold.mat

#### 각 raster의 픽셀에 나타난 확률값들을 위에서 구한 threshold 기준으로 threshold를 넘으면 1, 넘지 않으면 0으로 하여 출현/비출현 구분.
list_pythonidae_habitat_filtered <- list_pythonidae_habitat
for(i in 1:length(list_pythonidae_habitat_filtered)){
  list_pythonidae_habitat_filtered[[i]][list_pythonidae_habitat_filtered[[i]] < threshold.mat[[i]]] <- 0 
  list_pythonidae_habitat_filtered[[i]][list_pythonidae_habitat_filtered[[i]] >= threshold.mat[[i]]] <- 1
}

plot(list_pythonidae_habitat_filtered[[4]]) 


saveRDS(list_pythonidae_habitat_filtered, file = "Maxent\\Maxent_raster\\list_pythonidae_habitat_filtered.rds")
list_pythonidae_habitat_filtered <- readRDS(file = "D:\\Environmental data\\Maxent\\Maxent_raster\\list_pythonidae_habitat_filtered.rds") 








#############################################
############    Falconiformes       ############
#############################################


library(readr)
falconiformes <- read_delim("Maxent\\Species_occurence/falconiformes.csv", 
                         delim = "\t", escape_double = FALSE, 
                         trim_ws = TRUE)

#각 species 별로 구분된 spatialpoints 객체를 모아 놓은 list이다. 
falconiformes %>% group_by(species) %>% group_map(~.x) %>%
  lapply(FUN = function(X) {
    SpatialPoints(coords = X[c("decimalLongitude", "decimalLatitude")], proj4string  = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  }
  ) -> sp_falconiformes_species

class(sp_falconiformes_species)
sp_falconiformes_species

#list 안에 총 6 종의 species가 있고, 각 species 별로 몇 개의 point가 찍혀 있는지 확인.
sp_falconiformes_species <- sp_falconiformes_species[unlist(lapply(sp_falconiformes_species, FUN = length)) > 10]
#point 숫자가 최소한 10보다는 큰 종만 골라서 다시 subset -> 5종


falconiformes_species <- as.data.frame(table(falconiformes$species))
falconiformes_species_over10 <- falconiformes_species %>% 
  dplyr::filter(Freq > 10)

write.csv(falconiformes_species_over10, "Maxent\\Species_list\\falconiformes_species_over10.csv")

# load base map
setwd("D:\\Environmental data\\Hendra-Australia\\AUS_basemap\\final")
terrestrial.grid <- readOGR(dsn = "Basemap_clipped_numpoints_final.shp")




### parameter preparation
{
  prepPara <- function(userfeatures=NULL, #NULL=autofeature, could be any combination of # c("L", "Q", "H", "H", "P")
                       responsecurves=TRUE,
                       jackknife=TRUE,      
                       outputformat="logistic",
                       outputfiletype="asc", 
                       projectionlayers=NULL,
                       randomseed=FALSE,
                       removeDuplicates=TRUE,
                       betamultiplier=NULL,
                       biasfile=NULL,
                       testsamplesfile=NULL,
                       replicates=1,
                       replicatetype="crossvalidate",
                       writeplotdata=TRUE,
                       extrapolate=TRUE,
                       doclamp=TRUE,
                       beta_threshold=NULL,
                       beta_categorical=NULL,
                       beta_lqp=NULL,
                       beta_hinge=NULL,
                       applythresholdrule=NULL,
                       maximumiterations = 500,
                       convergencethreshold = 1.0E-5,
                       number_core = 1 
  ){
    #20 & 29-33 features, default is autofeature
    if(is.null(userfeatures)){
      args_out <- c("autofeature")
    } else {
      args_out <- c("noautofeature")
      if(grepl("L",userfeatures)) args_out <- c(args_out,"linear") else args_out <- c(args_out,"nolinear")
      if(grepl("Q",userfeatures)) args_out <- c(args_out,"quadratic") else args_out <- c(args_out,"noquadratic")
      if(grepl("H",userfeatures)) args_out <- c(args_out,"hinge") else args_out <- c(args_out,"nohinge")
      if(grepl("P",userfeatures)) args_out <- c(args_out,"product") else args_out <- c(args_out,"noproduct")
      if(grepl("T",userfeatures)) args_out <- c(args_out,"threshold") else args_out <- c(args_out,"nothreshold")
    }
    
    #1 
    if(responsecurves) args_out <- c(args_out,"responsecurves") else args_out <- c(args_out,"noresponsecurves")
    #2
    #if(picture) args_out <- c(args_out,"pictures") else args_out <- c(args_out,"nopictures")
    #3
    if(jackknife) args_out <- c(args_out,"jackknife") else args_out <- c(args_out,"nojackknife")
    #4
    args_out <- c(args_out,paste0("outputformat=",outputformat))
    #5
    args_out <- c(args_out,paste0("outputfiletype=",outputfiletype))
    #7
    if(!is.null(projectionlayers))    args_out <- c(args_out,paste0("projectionlayers=",projectionlayers))
    #10
    if(randomseed) args_out <- c(args_out,"randomseed") else args_out <- c(args_out,"norandomseed")
    #16
    if(removeDuplicates) args_out <- c(args_out,"removeduplicates") else args_out <- c(args_out,"noremoveduplicates")
    #20 & 53-56
    # check if negative
    betas <- c( betamultiplier,beta_threshold,beta_categorical,beta_lqp,beta_hinge)
    if(! is.null(betas) ){
      for(i in 1:length(betas)){
        if(betas[i] <0) stop("betamultiplier has to be positive")
      }
    }
    if (  !is.null(betamultiplier)  ){
      args_out <- c(args_out,paste0("betamultiplier=",betamultiplier))
    } else {
      if(!is.null(beta_threshold)) args_out <- c(args_out,paste0("beta_threshold=",beta_threshold))
      if(!is.null(beta_categorical)) args_out <- c(args_out,paste0("beta_categorical=",beta_categorical))
      if(!is.null(beta_lqp)) args_out <- c(args_out,paste0("beta_lqp=",beta_lqp))
      if(!is.null(beta_hinge)) args_out <- c(args_out,paste0("beta_hinge=",beta_hinge))
    }
    #22
    if(!is.null(biasfile))    args_out <- c(args_out,paste0("biasfile=",biasfile))
    #23
    if(!is.null(testsamplesfile))    args_out <- c(args_out,paste0("testsamplesfile=",testsamplesfile))
    #24&25
    replicates <- as.integer(replicates)
    if(replicates>1 ){
      args_out <- c(args_out,
                    paste0("replicates=",replicates),
                    paste0("replicatetype=",replicatetype) )
    }
    #37
    if(writeplotdata) args_out <- c(args_out,"writeplotdata") else args_out <- c(args_out,"nowriteplotdata")
    #39
    if(extrapolate) args_out <- c(args_out,"extrapolate") else args_out <- c(args_out,"noextrapolate")
    #42
    if(doclamp) args_out <- c(args_out,"doclamp") else args_out <- c(args_out,"nodoclamp")
    #60
    if(!is.null(applythresholdrule))    args_out <- c(args_out,paste0("applythresholdrule=",applythresholdrule))
    
    maximumiterations <- as.integer(maximumiterations)
    args_out <- c(args_out, paste0("maximumiterations=", maximumiterations))
    
    convergencethreshold <- as.integer(convergencethreshold)
    args_out <- c(args_out, paste0("convergencethreshold=", convergencethreshold))
    
    #number_core <- as.integer(number_core)
    #args_out <- c(args_out, paste0("threads=", number_core))
    
    return(args_out)
  }
}



list_maxent_model =list(NA)
list_remain_var = list(NA)
for (i in 1:length(sp_falconiformes_species)) {
  
  sp_point_species = sp_falconiformes_species[[i]]
  
  exclude_var = NA
  var_num = 1:nlayers(st_env_r)
  min_contr_var = 0
  
  model0= maxent(st_env_r[[var_num]], sp_point_species,  silent = TRUE, a = RandomDismo,
                 args = prepPara(jackknife=TRUE, betamultiplier =1, replicates = 1),
                 removeDuplicates = FALSE,
                 path ="Maxent\\data" )
  min_var = which.min(model0@results[grepl("contribution$",rownames(model0@results))])
  min_contr_var = model0@results[grepl("contribution$",rownames(model0@results))][min_var]
  var_name = rownames(model0@results)[grepl("contribution$",rownames(model0@results))][min_var]
  exclude_var = substr(var_name,1, nchar(var_name)-13)
  var_num = which(!names(st_env_r) %in% exclude_var)
  
  while(min_contr_var <= 10){
    
    model0= maxent(st_env_r[[var_num]], sp_point_species,  silent = TRUE, a = RandomDismo,
                   args = prepPara(jackknife=TRUE, betamultiplier =1, replicates = 1),
                   removeDuplicates = FALSE,
                   path ="Maxent\\data" )
    
    min_var = which.min(model0@results[grepl("contribution$",rownames(model0@results))])
    var_name = rownames(model0@results)[grepl("contribution$",rownames(model0@results))][min_var]
    min_contr_var = model0@results[grepl("contribution$",rownames(model0@results))][min_var]
    exclude_var = c(exclude_var, substr(var_name,1, nchar(var_name)-13))
    var_num = which(!names(st_env_r) %in% exclude_var)
  } 
  list_maxent_model[[i]]= model0
  list_remain_var[[i]] = names(st_env_r[[var_num]])
  
}


saveRDS(list_maxent_model, file = "Maxent\\Maxent_models\\list_maxent_model_falconiformes.rds")
readRDS(file = "Maxent\\Maxent_models\\list_maxent_model_falconiformes.rds") -> list_maxent_model_falconiformes
saveRDS(list_remain_var, file = "Maxent\\Maxent_models\\list_remain_var_falconiformes.rds")
readRDS(file = "Maxent\\Maxent_models\\list_remain_var_falconiformes.rds") -> list_remain_var_falconiformes


list_falconiformes_habitat =list(NA)
for (i in 1:length(list_maxent_model)) { 
  list_falconiformes_habitat[[i]] <- predict(list_maxent_model[[i]], st_env_r)
}

plot(list_falconiformes_habitat[[5]])

saveRDS(list_falconiformes_habitat, file = "Maxent\\Maxent_raster\\list_falconiformes_habitat.rds")
list_falconiformes_habitat <- readRDS(file = "Maxent\\Maxent_raster\\list_falconiformes_habitat.rds") 


######## evaluate/threshold

#### k-fold를 5로 잡아서 train과 test dateset 분리하기.
fold.list<-list()
length(list_maxent_model)
for(i in 1:length(list_maxent_model)){
  fold.list[[i]]<-kfold(sp_falconiformes_species[[i]],k=5)
}

fold.list

#### 각 모델 evaluate 하기
evaluate.mat<-list()
for(i in 1:length(list_maxent_model)){
  
  loc.test<-sp_falconiformes_species[[i]][fold.list[[i]] == 1,]
  
  bg <- randomPoints(st_env_r, length(loc.test))
  e1 <- evaluate(list_maxent_model[[i]], p=loc.test, a=bg, x=st_env_r)
  
  evaluate.mat[[i]]<-e1
}
evaluate.mat

#### evaluate 한 객체들에서 threshold 값만 빼내기
threshold.mat <- list()
for(i in 1:length(evaluate.mat)){
  t <- threshold(evaluate.mat[[i]])
  threshold.mat[[i]] <- t[["prevalence"]]
}
threshold.mat

#### 각 raster의 픽셀에 나타난 확률값들을 위에서 구한 threshold 기준으로 threshold를 넘으면 1, 넘지 않으면 0으로 하여 출현/비출현 구분.
list_falconiformes_habitat_filtered <- list_falconiformes_habitat
for(i in 1:length(list_falconiformes_habitat_filtered)){
  list_falconiformes_habitat_filtered[[i]][list_falconiformes_habitat_filtered[[i]] < threshold.mat[[i]]] <- 0 
  list_falconiformes_habitat_filtered[[i]][list_falconiformes_habitat_filtered[[i]] >= threshold.mat[[i]]] <- 1
}

plot(list_falconiformes_habitat_filtered[[4]]) 


saveRDS(list_falconiformes_habitat_filtered, file = "Maxent\\Maxent_raster\\list_falconiformes_habitat_filtered.rds")
list_falconiformes_habitat_filtered <- readRDS(file = "D:\\Environmental data\\Maxent\\Maxent_raster\\list_falconiformes_habitat_filtered.rds") 




