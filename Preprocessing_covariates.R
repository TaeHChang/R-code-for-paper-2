###################################################
### Data extraction from global map ###############
###################################################

### Environmental covariates

## terrestrial grid
library(sf)
library(rgdal)
library(raster)
library(rgeos)
library(dismo)
library(tidyverse)


setwd("D:\\Environmental data\\Hendra-Australia\\AUS_basemap\\final")
terrestrial.grid <- readOGR(dsn = "Basemap_clipped_numpoints_final.shp")
plot(terrestrial.grid)
crs(terrestrial.grid) #4326


ID <- terrestrial.grid@data %>% dplyr::select(id)

############################################################################################################################
## For raster ###############################################
############################################################################################################################


#############################################################################################################
## Global elevation 
############################################################################################################
# process 1: clipping the elevation tiff by world gird, 
# process 2: averaging by grid cell extent 
library(doParallel)
cl<-makeCluster(2)
registerDoParallel(cl)
detectCores()
registerDoParallel(cores=12)
getDoParWorkers()

library(raster)

setwd("D:\\Environmental data\\Hendra-Australia\\Elevation")

raw.elevation <- raster("AUS_elevation.tif")
plot(raw.elevation)

####### R로 할 수 있는데, QGIS가 빠름
## elevation.clipped <- mask(raw.elevation, terrestrial.grid)
elevation.clipped <- raster("elevation_clipped.tif")

terrestrial.grid@data
#######



a<-Sys.time()
global.elevation.mat <- 
  foreach(j = 1:nrow(terrestrial.grid@data),.combine = 'rbind',.packages = c("raster"),.errorhandling = 'pass')%dopar%{
    cropped.grid<-crop(raw.elevation, extent(terrestrial.grid@data$left[j],terrestrial.grid@data$right[j],terrestrial.grid@data$bottom[j],terrestrial.grid@data$top[j]))
    global.elevation<-mean(getValues(cropped.grid),na.rm=T)
    return(global.elevation)
  }
b<-Sys.time()
b-a

global.elevation.mat <- as.data.frame(global.elevation.mat)
global.elevation <- cbind(ID, global.elevation.mat)
global.elevation <- global.elevation %>% 
  rename(ID = id,
         elevation = V1)

write.csv(global.elevation,"global_elevation.csv")

head(global.elevation)
nrow(global.elevation)
sum(is.na(global.elevation))



#############################################################################################################
## Tree cover 
############################################################################################################
setwd("D:\\Environmental data\\Hendra-Australia\\Forest_cover")

library(raster)
treecover <- raster("forestcover.tif")
treeloss <- raster("forestloss.tif")
treegain <- raster("forestgain.tif")


### treecover 계산
treecover_data<-matrix(NA,ncol=1,nrow=nrow(terrestrial.grid@data))

a<-Sys.time() #treecover가 75%이상인 경우만 계산하고, 각 30m X 30m 픽셀에 5m 이상 treee canopy의 면적을 퍼센트로 나타낸 것이므로, 75가 넘는 픽셀만 1로 카운트해서 grid 내의 숫자를 계수한다.
for(i in 1:nrow(treecover_data)){
    treecover_data[i]<-sum(getValues(crop(treecover, extent(terrestrial.grid@data[i,"left"],terrestrial.grid@data[i,"right"],terrestrial.grid@data[i,"bottom"],terrestrial.grid@data[i,"top"])))>75)
}
b<-Sys.time()
b-a #  15 mins

treecover_1 <- cbind(ID, treecover_data)
treecover_1 <- treecover_1 %>% 
  rename(ID = id,
         treecover = treecover_data)
write.csv(treecover_1, "treecover.csv")

### treeloss 계산

treeloss.mat<-matrix(NA,ncol=20,nrow=nrow(terrestrial.grid@data))
dim(treeloss.mat)
#이거는 75퍼센트 기준으로 cover를 0, 1로 나누고,
#거기에다가 lossyear의 year 값을 곱하니까. 그래서 0인 경우, 다시 말해서 원래도 75퍼센트 안되던 곳은 따지지 않고
#75퍼센트 이상이어서 cover에서 계수가 되었던 영역 중에 loss 생긴 지역을 따지게 된다. 
a<-Sys.time()
for(i in 1:nrow(terrestrial.grid@data)){
  treecover.temp <- getValues(crop(treecover, extent(terrestrial.grid@data[i,"left"],terrestrial.grid@data[i,"right"],terrestrial.grid@data[i,"bottom"],terrestrial.grid@data[i,"top"]))) > 75
  treeloss.temp <- getValues(crop(treeloss, extent(terrestrial.grid@data[i,"left"],terrestrial.grid@data[i,"right"],terrestrial.grid@data[i,"bottom"],terrestrial.grid@data[i,"top"])))
  treecover_treeloss.temp<-treecover.temp*treeloss.temp
  
  treeloss.mat[i,1]<-sum(treecover_treeloss.temp==1)
  treeloss.mat[i,2]<-sum(treecover_treeloss.temp==2)
  treeloss.mat[i,3]<-sum(treecover_treeloss.temp==3)
  treeloss.mat[i,4]<-sum(treecover_treeloss.temp==4)
  treeloss.mat[i,5]<-sum(treecover_treeloss.temp==5)
  treeloss.mat[i,6]<-sum(treecover_treeloss.temp==6)
  treeloss.mat[i,7]<-sum(treecover_treeloss.temp==7)
  treeloss.mat[i,8]<-sum(treecover_treeloss.temp==8)
  treeloss.mat[i,9]<-sum(treecover_treeloss.temp==9)
  treeloss.mat[i,10]<-sum(treecover_treeloss.temp==10)
  treeloss.mat[i,11]<-sum(treecover_treeloss.temp==11)
  treeloss.mat[i,12]<-sum(treecover_treeloss.temp==12)
  treeloss.mat[i,13]<-sum(treecover_treeloss.temp==13)
  treeloss.mat[i,14]<-sum(treecover_treeloss.temp==14)
  treeloss.mat[i,15]<-sum(treecover_treeloss.temp==15)
  treeloss.mat[i,16]<-sum(treecover_treeloss.temp==16)
  treeloss.mat[i,17]<-sum(treecover_treeloss.temp==17)
  treeloss.mat[i,18]<-sum(treecover_treeloss.temp==18)
  treeloss.mat[i,19]<-sum(treecover_treeloss.temp==19)
  treeloss.mat[i,20]<-sum(treecover_treeloss.temp==20)
  
  
}
b<-Sys.time()
b-a  # 25 mins

dim(treeloss.mat)
apply(treeloss.mat,2,summary)
sum(is.na(treeloss.mat))


loss_data <- as.data.frame(treeloss.mat)
colnames(loss_data)<-paste0("Y",2001:2020)

loss_data <- cbind(ID, loss_data)


write.csv(loss_data,"treeloss.csv")




## for treegain

gain.mat<-matrix(NA,ncol=1,nrow=nrow(terrestrial.grid@data))

# 75퍼센트이상의 cover 있는지 유무 0, 1 / gain 되었는지 유무 0, 1 그래서 곱하면 0 또는 1임. 
#그러므로 여기서 최종적으로 gain.mat 되는건 75퍼센트 이상의 cover가 있었으면서 동시에 gain 된 경우였다.
### 그러나 좀 코드를 바꿔서, 기존의 treecover에서 기준으로 삼았던 75% 이하의 지역들이 gain이 된 경우를 count하는 것이 맞지 않는가.
a<-Sys.time()
for(i in 1:nrow(terrestrial.grid@data)){
  treecover.temp<-getValues(crop(treecover, extent(terrestrial.grid@data[i,"left"],terrestrial.grid@data[i,"right"],terrestrial.grid@data[i,"bottom"],terrestrial.grid@data[i,"top"]))) < 75
  gain.temp<-getValues(crop(treegain,extent(terrestrial.grid@data[i,"left"],terrestrial.grid@data[i,"right"],terrestrial.grid@data[i,"bottom"],terrestrial.grid@data[i,"top"])))
  treecover_gain.temp<-treecover.temp*gain.temp
  
  gain.mat[i]<-sum(treecover_gain.temp == 1)
}
b<-Sys.time()
b-a # 25 mins


gain_data <- as.data.frame(gain.mat)
colnames(gain_data) <- "treegain"

gain_data <- cbind(ID, gain_data)


write.csv(gain_data,"treegain.csv")




######################################################
## world climate (temperature )
######################################################

library(raster)

temp.list <- list.files(path="D:\\Environmental data\\Temperature_1\\Temperature", pattern = "tif$", full.names = TRUE)
temp.stack <- raster::stack(temp.list)
slot(temp.stack,"z")<-list(as.Date(c("2000-01-01","2000-02-01","2000-03-01","2000-04-01","2000-05-01","2000-06-01",
                                     "2000-07-01","2000-08-01","2000-09-01","2000-10-01","2000-11-01","2000-12-01")))

preci.list <- list.files(path="D:\\Environmental data\\Precipitation_1\\Precipitation", pattern = "tif$", full.names = TRUE)
preci.stack <- raster::stack(preci.list)
slot(preci.stack,"z")<-list(as.Date(c("2000-01-01","2000-02-01","2000-03-01","2000-04-01","2000-05-01","2000-06-01",
                                      "2000-07-01","2000-08-01","2000-09-01","2000-10-01","2000-11-01","2000-12-01")))

dim(temp.stack$wc2.1_30s_tavg_01)
dim(temp.stack$wc2.1_30s_tavg_02)
dim(temp.stack$wc2.1_30s_tavg_03)
dim(temp.stack$wc2.1_30s_tavg_04)
dim(temp.stack$wc2.1_30s_tavg_05)
dim(temp.stack[[1]])




a<-Sys.time()
global.temp.mat<- foreach(i = 1:12,.combine = 'cbind',.multicombine = T,.packages = c("raster"),.errorhandling = 'pass')%:%
  foreach(j = 1:nrow(terrestrial.grid@data),.packages = c("raster"),.errorhandling = 'pass')%dopar%{
    cropped.grid<-crop(temp.stack[[i]], extent(terrestrial.grid@data$left[j],terrestrial.grid@data$right[j],terrestrial.grid@data$bottom[j],terrestrial.grid@data$top[j]))
    global.temp<-mean(getValues(cropped.grid),na.rm=T)
    return(global.temp)
  }
b<-Sys.time()
b-a # 30 sec

global.temp <- as.data.frame(global.temp.mat)
colnames(global.temp) <- c("temp_2000-01-01","temp_2000-02-01","temp_2000-03-01","temp_2000-04-01","temp_2000-05-01","temp_2000-06-01",
                           "temp_2000-07-01","temp_2000-08-01","temp_2000-09-01","temp_2000-10-01","temp_2000-11-01","temp_2000-12-01")
global.temp <- cbind(ID, global.temp)
global.temp
global.temp <- apply(global.temp,2,as.character)

write.csv(global.temp,"D:\\Environmental data\\Hendra-Australia\\Temp_preci\\global_temp.csv")



### precipiation
a<-Sys.time()
global.preci.mat<- foreach(i = 1:12,.combine = 'cbind',.multicombine = T,.packages = c("raster"),.errorhandling = 'pass')%:%
  foreach(j = 1:nrow(terrestrial.grid@data),.packages = c("raster"),.errorhandling = 'pass')%dopar%{
    cropped.grid<-crop(preci.stack[[i]], extent(terrestrial.grid@data$left[j],terrestrial.grid@data$right[j],terrestrial.grid@data$bottom[j],terrestrial.grid@data$top[j]))
    global.preci<-mean(getValues(cropped.grid),na.rm=T)
    return(global.preci)
  }
b<-Sys.time()
b-a # 30 sec

global.preci <- as.data.frame(global.preci.mat)
colnames(global.preci) <- c("preci_2000-01-01","preci_2000-02-01","preci_2000-03-01","preci_2000-04-01","preci_2000-05-01","preci_2000-06-01",
                            "preci_2000-07-01","preci_2000-08-01","preci_2000-09-01","preci_2000-10-01","preci_2000-11-01","preci_2000-12-01")
global.preci <- cbind(ID, global.preci)

global.preci <- apply(global.preci,2,as.character)

write.csv(global.preci,"D:\\Environmental data\\Hendra-Australia\\Temp_preci\\global_preci.csv")


#############################################################################################################
## population  
############################################################################################################

library(raster)

pop.list <- list.files(path="D:\\Environmental data\\Worldpop", pattern = "tif$", full.names = TRUE)


pop.list.raster<-list()
for(i in 1:21){
  pop.list.raster[[i]]<-raster(pop.list[i])
}

crs(terrestrial.grid)
crs(pop.list.raster[[1]])
extent(terrestrial.grid)
extent(pop.list.raster[[5]])
plot(pop.list.raster[[5]])

pop.list.raster[[1]]
pop.list.raster[[2]]
pop.list.raster[[3]]
pop.list.raster[[4]]
pop.list.raster[[5]]





a<-Sys.time()
pop.mat<- foreach(i = 1:21,.combine = 'cbind',.multicombine = T,.packages = c("raster"),.errorhandling = 'pass')%:%
  foreach(j = 1:nrow(terrestrial.grid@data),.packages = c("raster"),.errorhandling = 'pass')%dopar%{
    cropped.grid<-crop(pop.list.raster[[i]], extent(terrestrial.grid@data$left[j],terrestrial.grid@data$right[j],terrestrial.grid@data$bottom[j],terrestrial.grid@data$top[j]))
    pop.temp<-sum(getValues(cropped.grid),na.rm=T)
    return(pop.temp)
  }
b<-Sys.time()
b-a # 1 mins


global_pop_total <- as.data.frame(pop.mat)
colnames(global_pop_total) <- c("pop_2000","pop_2001","pop_2002","pop_2003","pop_2004",
                                "pop_2005","pop_2006","pop_2007","pop_2008","pop_2009","pop_2010",
                                "pop_2011","pop_2012","pop_2013","pop_2014","pop_2015","pop_2016",
                                "pop_2017","pop_2018","pop_2019","pop_2020")
global_pop_total <- cbind(ID, global_pop_total)

global_pop_total<- apply(global_pop_total,2,  as.numeric)
global_pop_total<- round(global_pop_total,2)

write.csv(global_pop_total,"D:\\Environmental data\\Hendra-Australia\\Population\\global_pop_total.csv")







#################################################################################################################

#############################################################################################################
## GDP / HDI  
############################################################################################################

library(raster)

GDP.list <- list.files(path="D:\\Environmental data\\GDP\\GDP_1", pattern = "tif$", full.names = TRUE)
raster(GDP.list)

GDP.list.raster <-list()
for(i in 1:26){
  GDP.list.raster[[i]]<-raster(GDP.list[1],band=i)
}

crs(terrestrial.grid)
crs(GDP.list.raster[[1]])
extent(terrestrial.grid)
extent(GDP.list.raster[[5]])
plot(GDP.list.raster[[20]])

GDP.list.raster[[1]]
GDP.list.raster[[26]]



a<-Sys.time()
GDP.mat<- foreach(i = 11:26,.combine = 'cbind',.multicombine = T,.packages = c("raster"),.errorhandling = 'pass')%:%
  foreach(j = 1:nrow(terrestrial.grid@data),.packages = c("raster"),.errorhandling = 'pass')%dopar%{
    cropped.grid<-crop(GDP.list.raster[[i]], extent(terrestrial.grid@data$left[j],terrestrial.grid@data$right[j],terrestrial.grid@data$bottom[j],terrestrial.grid@data$top[j]))
    GDP.temp<-mean(getValues(cropped.grid),na.rm=T)
    return(GDP.temp)
  }
b<-Sys.time()
b-a # 1 mins


global.GDP <- as.data.frame(GDP.mat)
colnames(global.GDP) <- c("GDP_2000", "GDP_2001", "GDP_2002", "GDP_2003", "GDP_2004",
                          "GDP_2005", "GDP_2006", "GDP_2007", "GDP_2008", "GDP_2009", "GDP_2010", "GDP_2011",
                          "GDP_2012", "GDP_2013", "GDP_2014", "GDP_2015")
global.GDP <- cbind(ID, global.GDP)

global.GDP <- apply(global.GDP,2,as.character)

write.csv(global.GDP,"D:\\Environmental data\\Hendra-Australia\\GDP_HDI\\global_GDP.csv")



### HDI

HDI.list <- list.files(path="D:\\Environmental data\\GDP\\HDI_1", pattern = "tif$", full.names = TRUE)


HDI.list.raster <-list()
for(i in 1:26){
  HDI.list.raster[[i]]<-raster(HDI.list[1],band=i)
}

crs(terrestrial.grid)
crs(HDI.list.raster[[1]])
extent(terrestrial.grid)
extent(HDI.list.raster[[5]])
plot(HDI.list.raster[[5]])

HDI.list.raster[[1]]
HDI.list.raster[[26]]





a<-Sys.time()
HDI.mat<- foreach(i = 11:26,.combine = 'cbind',.multicombine = T,.packages = c("raster"),.errorhandling = 'pass')%:%
  foreach(j = 1:nrow(terrestrial.grid@data),.packages = c("raster"),.errorhandling = 'pass')%dopar%{
    cropped.grid<-crop(HDI.list.raster[[i]], extent(terrestrial.grid@data$left[j],terrestrial.grid@data$right[j],terrestrial.grid@data$bottom[j],terrestrial.grid@data$top[j]))
    HDI.temp<-mean(getValues(cropped.grid),na.rm=T)
    return(HDI.temp)
  }
b<-Sys.time()
b-a # 1 mins


global.HDI <- as.data.frame(HDI.mat)
colnames(global.HDI) <- c("HDI_2000", "HDI_2001", "HDI_2002", "HDI_2003", "HDI_2004",
                          "HDI_2005", "HDI_2006", "HDI_2007", "HDI_2008", "HDI_2009", "HDI_2010", "HDI_2011",
                          "HDI_2012", "HDI_2013", "HDI_2014", "HDI_2015")
global.HDI <- cbind(ID, global.HDI)

global.HDI <- apply(global.HDI,2,as.character)

write.csv(global.HDI,"D:\\Environmental data\\Hendra-Australia\\GDP_HDI\\global_HDI.csv")







#################################################################################################################

#############################################################################################################
## human foot print  
############################################################################################################


library(raster)
hfp.list <- list.files(path="D:\\Environmental data\\Human_footprint_4326", pattern = "tif$", full.names = TRUE)

hfp.list.raster<-list()
for(i in 1:19){
  hfp.list.raster[[i]]<-raster(hfp.list[i])
}


crs(terrestrial.grid)
crs(hfp.list.raster[[1]]) 
extent(terrestrial.grid)
extent(hfp.list.raster[[1]])


hfp.mat<-matrix(NA,ncol=19,nrow=nrow(terrestrial.grid@data))
a<-Sys.time()
for(i in 1:nrow(terrestrial.grid@data) ){for(j in 1:19){
  aa<-0
  tryCatch(crop(hfp.list.raster[[j]],extent(terrestrial.grid@data$left[i],terrestrial.grid@data$right[i],terrestrial.grid@data$bottom[i],terrestrial.grid@data$top[i])),error=function(e){aa<<-1})
  
  if(aa ==0 ){
    cropped.grid<-crop(hfp.list.raster[[j]],extent(terrestrial.grid@data$left[i],terrestrial.grid@data$right[i],terrestrial.grid@data$bottom[i],terrestrial.grid@data$top[i]))
    hfp.mat[i,j]<-mean(getValues(cropped.grid),na.rm=T)
  }else{
    hfp.mat[i,j] <-0  
  }
}}
b<-Sys.time()
b-a #12 mins

hfp.data  <- as.data.frame(hfp.mat)
colnames(hfp.data ) <- c("hfp_2000", "hfp_2001", "hfp_2002", "hfp_2003", "hfp_2004",
                         "hfp_2005", "hfp_2006", "hfp_2007", "hfp_2008", "hfp_2009", "hfp_2010", "hfp_2011",
                         "hfp_2012", "hfp_2013", "hfp_2014", "hfp_2015", "hfp_2016", "hfp_2017",
                         "hfp_2018")
hfp.data <- cbind(ID, hfp.data)


write.csv(hfp.data,"D:\\Environmental data\\Hendra-Australia\\HFP/hfp.csv")



#################################################################################################################

#############################################################################################################
## Global landcover 
############################################################################################################
library(raster)

landcover.list <- list.files(path="D:\\Environmental data\\Hendra-Australia\\Agricultural_landuse\\LC1_Percent", pattern = "tif$", full.names = TRUE)


landcover.list.raster<-list()
for(i in 1:20){
  landcover.list.raster[[i]]<-raster(landcover.list[i])
}


crs(terrestrial.grid)
crs(landcover.list.raster[[1]])
extent(terrestrial.grid)
extent(landcover.list.raster[[1]])
plot(landcover.list.raster[[3]])

landcover_mat_2001 <- matrix(NA,ncol=18,nrow=nrow(terrestrial.grid@data))
landcover_mat_2002 <- matrix(NA,ncol=18,nrow=nrow(terrestrial.grid@data))
landcover_mat_2003 <- matrix(NA,ncol=18,nrow=nrow(terrestrial.grid@data))
landcover_mat_2004 <- matrix(NA,ncol=18,nrow=nrow(terrestrial.grid@data))
landcover_mat_2005 <- matrix(NA,ncol=18,nrow=nrow(terrestrial.grid@data))
landcover_mat_2006 <- matrix(NA,ncol=18,nrow=nrow(terrestrial.grid@data))
landcover_mat_2007 <- matrix(NA,ncol=18,nrow=nrow(terrestrial.grid@data))
landcover_mat_2008 <- matrix(NA,ncol=18,nrow=nrow(terrestrial.grid@data))
landcover_mat_2009 <- matrix(NA,ncol=18,nrow=nrow(terrestrial.grid@data))
landcover_mat_2010 <- matrix(NA,ncol=18,nrow=nrow(terrestrial.grid@data))
landcover_mat_2011 <- matrix(NA,ncol=18,nrow=nrow(terrestrial.grid@data))
landcover_mat_2012 <- matrix(NA,ncol=18,nrow=nrow(terrestrial.grid@data))
landcover_mat_2013 <- matrix(NA,ncol=18,nrow=nrow(terrestrial.grid@data))
landcover_mat_2014 <- matrix(NA,ncol=18,nrow=nrow(terrestrial.grid@data))
landcover_mat_2015 <- matrix(NA,ncol=18,nrow=nrow(terrestrial.grid@data))
landcover_mat_2016 <- matrix(NA,ncol=18,nrow=nrow(terrestrial.grid@data))
landcover_mat_2017 <- matrix(NA,ncol=18,nrow=nrow(terrestrial.grid@data))
landcover_mat_2018 <- matrix(NA,ncol=18,nrow=nrow(terrestrial.grid@data))
landcover_mat_2019 <- matrix(NA,ncol=18,nrow=nrow(terrestrial.grid@data))
landcover_mat_2020 <- matrix(NA,ncol=18,nrow=nrow(terrestrial.grid@data))




a<-Sys.time()
for(i in 1:nrow(terrestrial.grid@data)){
  cropped.grid<-crop(landcover.list.raster[[1]],extent(terrestrial.grid@data$left[i],terrestrial.grid@data$right[i],terrestrial.grid@data$bottom[i],terrestrial.grid@data$top[i]))
  
  
  landcover_mat_2001[i,18]<-sum(getValues(cropped.grid)>=1,na.rm=T)
  landcover_mat_2001[i,1]<-sum(getValues(cropped.grid)==1)
  landcover_mat_2001[i,2]<-sum(getValues(cropped.grid)==2)
  landcover_mat_2001[i,3]<-sum(getValues(cropped.grid)==3)
  landcover_mat_2001[i,4]<-sum(getValues(cropped.grid)==4)
  landcover_mat_2001[i,5]<-sum(getValues(cropped.grid)==5)
  landcover_mat_2001[i,6]<-sum(getValues(cropped.grid)==6)
  landcover_mat_2001[i,7]<-sum(getValues(cropped.grid)==7)
  landcover_mat_2001[i,8]<-sum(getValues(cropped.grid)==8)
  landcover_mat_2001[i,9]<-sum(getValues(cropped.grid)==9)
  landcover_mat_2001[i,10]<-sum(getValues(cropped.grid)==10)
  landcover_mat_2001[i,11]<-sum(getValues(cropped.grid)==11)
  landcover_mat_2001[i,12]<-sum(getValues(cropped.grid)==12)
  landcover_mat_2001[i,13]<-sum(getValues(cropped.grid)==13)
  landcover_mat_2001[i,14]<-sum(getValues(cropped.grid)==14)
  landcover_mat_2001[i,15]<-sum(getValues(cropped.grid)==15)
  landcover_mat_2001[i,16]<-sum(getValues(cropped.grid)==16)
  landcover_mat_2001[i,17]<-sum(getValues(cropped.grid)==17)
  
}

for(i in 1:nrow(terrestrial.grid@data)){
  cropped.grid<-crop(landcover.list.raster[[2]],extent(terrestrial.grid@data$left[i],terrestrial.grid@data$right[i],terrestrial.grid@data$bottom[i],terrestrial.grid@data$top[i]))
  
  
  landcover_mat_2002[i,18]<-sum(getValues(cropped.grid)>=1,na.rm=T)
  landcover_mat_2002[i,1]<-sum(getValues(cropped.grid)==1)
  landcover_mat_2002[i,2]<-sum(getValues(cropped.grid)==2)
  landcover_mat_2002[i,3]<-sum(getValues(cropped.grid)==3)
  landcover_mat_2002[i,4]<-sum(getValues(cropped.grid)==4)
  landcover_mat_2002[i,5]<-sum(getValues(cropped.grid)==5)
  landcover_mat_2002[i,6]<-sum(getValues(cropped.grid)==6)
  landcover_mat_2002[i,7]<-sum(getValues(cropped.grid)==7)
  landcover_mat_2002[i,8]<-sum(getValues(cropped.grid)==8)
  landcover_mat_2002[i,9]<-sum(getValues(cropped.grid)==9)
  landcover_mat_2002[i,10]<-sum(getValues(cropped.grid)==10)
  landcover_mat_2002[i,11]<-sum(getValues(cropped.grid)==11)
  landcover_mat_2002[i,12]<-sum(getValues(cropped.grid)==12)
  landcover_mat_2002[i,13]<-sum(getValues(cropped.grid)==13)
  landcover_mat_2002[i,14]<-sum(getValues(cropped.grid)==14)
  landcover_mat_2002[i,15]<-sum(getValues(cropped.grid)==15)
  landcover_mat_2002[i,16]<-sum(getValues(cropped.grid)==16)
  landcover_mat_2002[i,17]<-sum(getValues(cropped.grid)==17)
  
}

for(i in 1:nrow(terrestrial.grid@data)){
  cropped.grid<-crop(landcover.list.raster[[3]],extent(terrestrial.grid@data$left[i],terrestrial.grid@data$right[i],terrestrial.grid@data$bottom[i],terrestrial.grid@data$top[i]))
  
  
  landcover_mat_2003[i,18]<-sum(getValues(cropped.grid)>=1,na.rm=T)
  landcover_mat_2003[i,1]<-sum(getValues(cropped.grid)==1)
  landcover_mat_2003[i,2]<-sum(getValues(cropped.grid)==2)
  landcover_mat_2003[i,3]<-sum(getValues(cropped.grid)==3)
  landcover_mat_2003[i,4]<-sum(getValues(cropped.grid)==4)
  landcover_mat_2003[i,5]<-sum(getValues(cropped.grid)==5)
  landcover_mat_2003[i,6]<-sum(getValues(cropped.grid)==6)
  landcover_mat_2003[i,7]<-sum(getValues(cropped.grid)==7)
  landcover_mat_2003[i,8]<-sum(getValues(cropped.grid)==8)
  landcover_mat_2003[i,9]<-sum(getValues(cropped.grid)==9)
  landcover_mat_2003[i,10]<-sum(getValues(cropped.grid)==10)
  landcover_mat_2003[i,11]<-sum(getValues(cropped.grid)==11)
  landcover_mat_2003[i,12]<-sum(getValues(cropped.grid)==12)
  landcover_mat_2003[i,13]<-sum(getValues(cropped.grid)==13)
  landcover_mat_2003[i,14]<-sum(getValues(cropped.grid)==14)
  landcover_mat_2003[i,15]<-sum(getValues(cropped.grid)==15)
  landcover_mat_2003[i,16]<-sum(getValues(cropped.grid)==16)
  landcover_mat_2003[i,17]<-sum(getValues(cropped.grid)==17)
  
}


for(i in 1:nrow(terrestrial.grid@data)){
  cropped.grid<-crop(landcover.list.raster[[5]],extent(terrestrial.grid@data$left[i],terrestrial.grid@data$right[i],terrestrial.grid@data$bottom[i],terrestrial.grid@data$top[i]))
  
  
  landcover_mat_2005[i,18]<-sum(getValues(cropped.grid)>=1,na.rm=T)
  landcover_mat_2005[i,1]<-sum(getValues(cropped.grid)==1)
  landcover_mat_2005[i,2]<-sum(getValues(cropped.grid)==2)
  landcover_mat_2005[i,3]<-sum(getValues(cropped.grid)==3)
  landcover_mat_2005[i,4]<-sum(getValues(cropped.grid)==4)
  landcover_mat_2005[i,5]<-sum(getValues(cropped.grid)==5)
  landcover_mat_2005[i,6]<-sum(getValues(cropped.grid)==6)
  landcover_mat_2005[i,7]<-sum(getValues(cropped.grid)==7)
  landcover_mat_2005[i,8]<-sum(getValues(cropped.grid)==8)
  landcover_mat_2005[i,9]<-sum(getValues(cropped.grid)==9)
  landcover_mat_2005[i,10]<-sum(getValues(cropped.grid)==10)
  landcover_mat_2005[i,11]<-sum(getValues(cropped.grid)==11)
  landcover_mat_2005[i,12]<-sum(getValues(cropped.grid)==12)
  landcover_mat_2005[i,13]<-sum(getValues(cropped.grid)==13)
  landcover_mat_2005[i,14]<-sum(getValues(cropped.grid)==14)
  landcover_mat_2005[i,15]<-sum(getValues(cropped.grid)==15)
  landcover_mat_2005[i,16]<-sum(getValues(cropped.grid)==16)
  landcover_mat_2005[i,17]<-sum(getValues(cropped.grid)==17)
  
}


for(i in 1:nrow(terrestrial.grid@data)){
  cropped.grid<-crop(landcover.list.raster[[4]],extent(terrestrial.grid@data$left[i],terrestrial.grid@data$right[i],terrestrial.grid@data$bottom[i],terrestrial.grid@data$top[i]))
  
  
  landcover_mat_2004[i,18]<-sum(getValues(cropped.grid)>=1,na.rm=T)
  landcover_mat_2004[i,1]<-sum(getValues(cropped.grid)==1)
  landcover_mat_2004[i,2]<-sum(getValues(cropped.grid)==2)
  landcover_mat_2004[i,3]<-sum(getValues(cropped.grid)==3)
  landcover_mat_2004[i,4]<-sum(getValues(cropped.grid)==4)
  landcover_mat_2004[i,5]<-sum(getValues(cropped.grid)==5)
  landcover_mat_2004[i,6]<-sum(getValues(cropped.grid)==6)
  landcover_mat_2004[i,7]<-sum(getValues(cropped.grid)==7)
  landcover_mat_2004[i,8]<-sum(getValues(cropped.grid)==8)
  landcover_mat_2004[i,9]<-sum(getValues(cropped.grid)==9)
  landcover_mat_2004[i,10]<-sum(getValues(cropped.grid)==10)
  landcover_mat_2004[i,11]<-sum(getValues(cropped.grid)==11)
  landcover_mat_2004[i,12]<-sum(getValues(cropped.grid)==12)
  landcover_mat_2004[i,13]<-sum(getValues(cropped.grid)==13)
  landcover_mat_2004[i,14]<-sum(getValues(cropped.grid)==14)
  landcover_mat_2004[i,15]<-sum(getValues(cropped.grid)==15)
  landcover_mat_2004[i,16]<-sum(getValues(cropped.grid)==16)
  landcover_mat_2004[i,17]<-sum(getValues(cropped.grid)==17)
  
}


for(i in 1:nrow(terrestrial.grid@data)){
  cropped.grid<-crop(landcover.list.raster[[6]],extent(terrestrial.grid@data$left[i],terrestrial.grid@data$right[i],terrestrial.grid@data$bottom[i],terrestrial.grid@data$top[i]))
  
  
  landcover_mat_2006[i,18]<-sum(getValues(cropped.grid)>=1,na.rm=T)
  landcover_mat_2006[i,1]<-sum(getValues(cropped.grid)==1)
  landcover_mat_2006[i,2]<-sum(getValues(cropped.grid)==2)
  landcover_mat_2006[i,3]<-sum(getValues(cropped.grid)==3)
  landcover_mat_2006[i,4]<-sum(getValues(cropped.grid)==4)
  landcover_mat_2006[i,5]<-sum(getValues(cropped.grid)==5)
  landcover_mat_2006[i,6]<-sum(getValues(cropped.grid)==6)
  landcover_mat_2006[i,7]<-sum(getValues(cropped.grid)==7)
  landcover_mat_2006[i,8]<-sum(getValues(cropped.grid)==8)
  landcover_mat_2006[i,9]<-sum(getValues(cropped.grid)==9)
  landcover_mat_2006[i,10]<-sum(getValues(cropped.grid)==10)
  landcover_mat_2006[i,11]<-sum(getValues(cropped.grid)==11)
  landcover_mat_2006[i,12]<-sum(getValues(cropped.grid)==12)
  landcover_mat_2006[i,13]<-sum(getValues(cropped.grid)==13)
  landcover_mat_2006[i,14]<-sum(getValues(cropped.grid)==14)
  landcover_mat_2006[i,15]<-sum(getValues(cropped.grid)==15)
  landcover_mat_2006[i,16]<-sum(getValues(cropped.grid)==16)
  landcover_mat_2006[i,17]<-sum(getValues(cropped.grid)==17)
  
}


for(i in 1:nrow(terrestrial.grid@data)){
  cropped.grid<-crop(landcover.list.raster[[7]],extent(terrestrial.grid@data$left[i],terrestrial.grid@data$right[i],terrestrial.grid@data$bottom[i],terrestrial.grid@data$top[i]))
  
  
  landcover_mat_2007[i,18]<-sum(getValues(cropped.grid)>=1,na.rm=T)
  landcover_mat_2007[i,1]<-sum(getValues(cropped.grid)==1)
  landcover_mat_2007[i,2]<-sum(getValues(cropped.grid)==2)
  landcover_mat_2007[i,3]<-sum(getValues(cropped.grid)==3)
  landcover_mat_2007[i,4]<-sum(getValues(cropped.grid)==4)
  landcover_mat_2007[i,5]<-sum(getValues(cropped.grid)==5)
  landcover_mat_2007[i,6]<-sum(getValues(cropped.grid)==6)
  landcover_mat_2007[i,7]<-sum(getValues(cropped.grid)==7)
  landcover_mat_2007[i,8]<-sum(getValues(cropped.grid)==8)
  landcover_mat_2007[i,9]<-sum(getValues(cropped.grid)==9)
  landcover_mat_2007[i,10]<-sum(getValues(cropped.grid)==10)
  landcover_mat_2007[i,11]<-sum(getValues(cropped.grid)==11)
  landcover_mat_2007[i,12]<-sum(getValues(cropped.grid)==12)
  landcover_mat_2007[i,13]<-sum(getValues(cropped.grid)==13)
  landcover_mat_2007[i,14]<-sum(getValues(cropped.grid)==14)
  landcover_mat_2007[i,15]<-sum(getValues(cropped.grid)==15)
  landcover_mat_2007[i,16]<-sum(getValues(cropped.grid)==16)
  landcover_mat_2007[i,17]<-sum(getValues(cropped.grid)==17)
  
}


for(i in 1:nrow(terrestrial.grid@data)){
  cropped.grid<-crop(landcover.list.raster[[8]],extent(terrestrial.grid@data$left[i],terrestrial.grid@data$right[i],terrestrial.grid@data$bottom[i],terrestrial.grid@data$top[i]))
  
  
  landcover_mat_2008[i,18]<-sum(getValues(cropped.grid)>=1,na.rm=T)
  landcover_mat_2008[i,1]<-sum(getValues(cropped.grid)==1)
  landcover_mat_2008[i,2]<-sum(getValues(cropped.grid)==2)
  landcover_mat_2008[i,3]<-sum(getValues(cropped.grid)==3)
  landcover_mat_2008[i,4]<-sum(getValues(cropped.grid)==4)
  landcover_mat_2008[i,5]<-sum(getValues(cropped.grid)==5)
  landcover_mat_2008[i,6]<-sum(getValues(cropped.grid)==6)
  landcover_mat_2008[i,7]<-sum(getValues(cropped.grid)==7)
  landcover_mat_2008[i,8]<-sum(getValues(cropped.grid)==8)
  landcover_mat_2008[i,9]<-sum(getValues(cropped.grid)==9)
  landcover_mat_2008[i,10]<-sum(getValues(cropped.grid)==10)
  landcover_mat_2008[i,11]<-sum(getValues(cropped.grid)==11)
  landcover_mat_2008[i,12]<-sum(getValues(cropped.grid)==12)
  landcover_mat_2008[i,13]<-sum(getValues(cropped.grid)==13)
  landcover_mat_2008[i,14]<-sum(getValues(cropped.grid)==14)
  landcover_mat_2008[i,15]<-sum(getValues(cropped.grid)==15)
  landcover_mat_2008[i,16]<-sum(getValues(cropped.grid)==16)
  landcover_mat_2008[i,17]<-sum(getValues(cropped.grid)==17)
  
}


for(i in 1:nrow(terrestrial.grid@data)){
  cropped.grid<-crop(landcover.list.raster[[9]],extent(terrestrial.grid@data$left[i],terrestrial.grid@data$right[i],terrestrial.grid@data$bottom[i],terrestrial.grid@data$top[i]))
  
  
  landcover_mat_2009[i,18]<-sum(getValues(cropped.grid)>=1,na.rm=T)
  landcover_mat_2009[i,1]<-sum(getValues(cropped.grid)==1)
  landcover_mat_2009[i,2]<-sum(getValues(cropped.grid)==2)
  landcover_mat_2009[i,3]<-sum(getValues(cropped.grid)==3)
  landcover_mat_2009[i,4]<-sum(getValues(cropped.grid)==4)
  landcover_mat_2009[i,5]<-sum(getValues(cropped.grid)==5)
  landcover_mat_2009[i,6]<-sum(getValues(cropped.grid)==6)
  landcover_mat_2009[i,7]<-sum(getValues(cropped.grid)==7)
  landcover_mat_2009[i,8]<-sum(getValues(cropped.grid)==8)
  landcover_mat_2009[i,9]<-sum(getValues(cropped.grid)==9)
  landcover_mat_2009[i,10]<-sum(getValues(cropped.grid)==10)
  landcover_mat_2009[i,11]<-sum(getValues(cropped.grid)==11)
  landcover_mat_2009[i,12]<-sum(getValues(cropped.grid)==12)
  landcover_mat_2009[i,13]<-sum(getValues(cropped.grid)==13)
  landcover_mat_2009[i,14]<-sum(getValues(cropped.grid)==14)
  landcover_mat_2009[i,15]<-sum(getValues(cropped.grid)==15)
  landcover_mat_2009[i,16]<-sum(getValues(cropped.grid)==16)
  landcover_mat_2009[i,17]<-sum(getValues(cropped.grid)==17)
  
}


for(i in 1:nrow(terrestrial.grid@data)){
  cropped.grid<-crop(landcover.list.raster[[10]],extent(terrestrial.grid@data$left[i],terrestrial.grid@data$right[i],terrestrial.grid@data$bottom[i],terrestrial.grid@data$top[i]))
  
  
  landcover_mat_2010[i,18]<-sum(getValues(cropped.grid)>=1,na.rm=T)
  landcover_mat_2010[i,1]<-sum(getValues(cropped.grid)==1)
  landcover_mat_2010[i,2]<-sum(getValues(cropped.grid)==2)
  landcover_mat_2010[i,3]<-sum(getValues(cropped.grid)==3)
  landcover_mat_2010[i,4]<-sum(getValues(cropped.grid)==4)
  landcover_mat_2010[i,5]<-sum(getValues(cropped.grid)==5)
  landcover_mat_2010[i,6]<-sum(getValues(cropped.grid)==6)
  landcover_mat_2010[i,7]<-sum(getValues(cropped.grid)==7)
  landcover_mat_2010[i,8]<-sum(getValues(cropped.grid)==8)
  landcover_mat_2010[i,9]<-sum(getValues(cropped.grid)==9)
  landcover_mat_2010[i,10]<-sum(getValues(cropped.grid)==10)
  landcover_mat_2010[i,11]<-sum(getValues(cropped.grid)==11)
  landcover_mat_2010[i,12]<-sum(getValues(cropped.grid)==12)
  landcover_mat_2010[i,13]<-sum(getValues(cropped.grid)==13)
  landcover_mat_2010[i,14]<-sum(getValues(cropped.grid)==14)
  landcover_mat_2010[i,15]<-sum(getValues(cropped.grid)==15)
  landcover_mat_2010[i,16]<-sum(getValues(cropped.grid)==16)
  landcover_mat_2010[i,17]<-sum(getValues(cropped.grid)==17)
  
}

for(i in 1:nrow(terrestrial.grid@data)){
  cropped.grid<-crop(landcover.list.raster[[11]],extent(terrestrial.grid@data$left[i],terrestrial.grid@data$right[i],terrestrial.grid@data$bottom[i],terrestrial.grid@data$top[i]))
  
  
  landcover_mat_2011[i,18]<-sum(getValues(cropped.grid)>=1,na.rm=T)
  landcover_mat_2011[i,1]<-sum(getValues(cropped.grid)==1)
  landcover_mat_2011[i,2]<-sum(getValues(cropped.grid)==2)
  landcover_mat_2011[i,3]<-sum(getValues(cropped.grid)==3)
  landcover_mat_2011[i,4]<-sum(getValues(cropped.grid)==4)
  landcover_mat_2011[i,5]<-sum(getValues(cropped.grid)==5)
  landcover_mat_2011[i,6]<-sum(getValues(cropped.grid)==6)
  landcover_mat_2011[i,7]<-sum(getValues(cropped.grid)==7)
  landcover_mat_2011[i,8]<-sum(getValues(cropped.grid)==8)
  landcover_mat_2011[i,9]<-sum(getValues(cropped.grid)==9)
  landcover_mat_2011[i,10]<-sum(getValues(cropped.grid)==10)
  landcover_mat_2011[i,11]<-sum(getValues(cropped.grid)==11)
  landcover_mat_2011[i,12]<-sum(getValues(cropped.grid)==12)
  landcover_mat_2011[i,13]<-sum(getValues(cropped.grid)==13)
  landcover_mat_2011[i,14]<-sum(getValues(cropped.grid)==14)
  landcover_mat_2011[i,15]<-sum(getValues(cropped.grid)==15)
  landcover_mat_2011[i,16]<-sum(getValues(cropped.grid)==16)
  landcover_mat_2011[i,17]<-sum(getValues(cropped.grid)==17)
  
}


for(i in 1:nrow(terrestrial.grid@data)){
  cropped.grid<-crop(landcover.list.raster[[12]],extent(terrestrial.grid@data$left[i],terrestrial.grid@data$right[i],terrestrial.grid@data$bottom[i],terrestrial.grid@data$top[i]))
  
  
  landcover_mat_2012[i,18]<-sum(getValues(cropped.grid)>=1,na.rm=T)
  landcover_mat_2012[i,1]<-sum(getValues(cropped.grid)==1)
  landcover_mat_2012[i,2]<-sum(getValues(cropped.grid)==2)
  landcover_mat_2012[i,3]<-sum(getValues(cropped.grid)==3)
  landcover_mat_2012[i,4]<-sum(getValues(cropped.grid)==4)
  landcover_mat_2012[i,5]<-sum(getValues(cropped.grid)==5)
  landcover_mat_2012[i,6]<-sum(getValues(cropped.grid)==6)
  landcover_mat_2012[i,7]<-sum(getValues(cropped.grid)==7)
  landcover_mat_2012[i,8]<-sum(getValues(cropped.grid)==8)
  landcover_mat_2012[i,9]<-sum(getValues(cropped.grid)==9)
  landcover_mat_2012[i,10]<-sum(getValues(cropped.grid)==10)
  landcover_mat_2012[i,11]<-sum(getValues(cropped.grid)==11)
  landcover_mat_2012[i,12]<-sum(getValues(cropped.grid)==12)
  landcover_mat_2012[i,13]<-sum(getValues(cropped.grid)==13)
  landcover_mat_2012[i,14]<-sum(getValues(cropped.grid)==14)
  landcover_mat_2012[i,15]<-sum(getValues(cropped.grid)==15)
  landcover_mat_2012[i,16]<-sum(getValues(cropped.grid)==16)
  landcover_mat_2012[i,17]<-sum(getValues(cropped.grid)==17)
  
}


for(i in 1:nrow(terrestrial.grid@data)){
  cropped.grid<-crop(landcover.list.raster[[13]],extent(terrestrial.grid@data$left[i],terrestrial.grid@data$right[i],terrestrial.grid@data$bottom[i],terrestrial.grid@data$top[i]))
  
  
  landcover_mat_2013[i,18]<-sum(getValues(cropped.grid)>=1,na.rm=T)
  landcover_mat_2013[i,1]<-sum(getValues(cropped.grid)==1)
  landcover_mat_2013[i,2]<-sum(getValues(cropped.grid)==2)
  landcover_mat_2013[i,3]<-sum(getValues(cropped.grid)==3)
  landcover_mat_2013[i,4]<-sum(getValues(cropped.grid)==4)
  landcover_mat_2013[i,5]<-sum(getValues(cropped.grid)==5)
  landcover_mat_2013[i,6]<-sum(getValues(cropped.grid)==6)
  landcover_mat_2013[i,7]<-sum(getValues(cropped.grid)==7)
  landcover_mat_2013[i,8]<-sum(getValues(cropped.grid)==8)
  landcover_mat_2013[i,9]<-sum(getValues(cropped.grid)==9)
  landcover_mat_2013[i,10]<-sum(getValues(cropped.grid)==10)
  landcover_mat_2013[i,11]<-sum(getValues(cropped.grid)==11)
  landcover_mat_2013[i,12]<-sum(getValues(cropped.grid)==12)
  landcover_mat_2013[i,13]<-sum(getValues(cropped.grid)==13)
  landcover_mat_2013[i,14]<-sum(getValues(cropped.grid)==14)
  landcover_mat_2013[i,15]<-sum(getValues(cropped.grid)==15)
  landcover_mat_2013[i,16]<-sum(getValues(cropped.grid)==16)
  landcover_mat_2013[i,17]<-sum(getValues(cropped.grid)==17)
  
}

for(i in 1:nrow(terrestrial.grid@data)){
  cropped.grid<-crop(landcover.list.raster[[14]],extent(terrestrial.grid@data$left[i],terrestrial.grid@data$right[i],terrestrial.grid@data$bottom[i],terrestrial.grid@data$top[i]))
  
  
  landcover_mat_2014[i,18]<-sum(getValues(cropped.grid)>=1,na.rm=T)
  landcover_mat_2014[i,1]<-sum(getValues(cropped.grid)==1)
  landcover_mat_2014[i,2]<-sum(getValues(cropped.grid)==2)
  landcover_mat_2014[i,3]<-sum(getValues(cropped.grid)==3)
  landcover_mat_2014[i,4]<-sum(getValues(cropped.grid)==4)
  landcover_mat_2014[i,5]<-sum(getValues(cropped.grid)==5)
  landcover_mat_2014[i,6]<-sum(getValues(cropped.grid)==6)
  landcover_mat_2014[i,7]<-sum(getValues(cropped.grid)==7)
  landcover_mat_2014[i,8]<-sum(getValues(cropped.grid)==8)
  landcover_mat_2014[i,9]<-sum(getValues(cropped.grid)==9)
  landcover_mat_2014[i,10]<-sum(getValues(cropped.grid)==10)
  landcover_mat_2014[i,11]<-sum(getValues(cropped.grid)==11)
  landcover_mat_2014[i,12]<-sum(getValues(cropped.grid)==12)
  landcover_mat_2014[i,13]<-sum(getValues(cropped.grid)==13)
  landcover_mat_2014[i,14]<-sum(getValues(cropped.grid)==14)
  landcover_mat_2014[i,15]<-sum(getValues(cropped.grid)==15)
  landcover_mat_2014[i,16]<-sum(getValues(cropped.grid)==16)
  landcover_mat_2014[i,17]<-sum(getValues(cropped.grid)==17)
  
}

for(i in 1:nrow(terrestrial.grid@data)){
  cropped.grid<-crop(landcover.list.raster[[15]],extent(terrestrial.grid@data$left[i],terrestrial.grid@data$right[i],terrestrial.grid@data$bottom[i],terrestrial.grid@data$top[i]))
  
  
  landcover_mat_2015[i,18]<-sum(getValues(cropped.grid)>=1,na.rm=T)
  landcover_mat_2015[i,1]<-sum(getValues(cropped.grid)==1)
  landcover_mat_2015[i,2]<-sum(getValues(cropped.grid)==2)
  landcover_mat_2015[i,3]<-sum(getValues(cropped.grid)==3)
  landcover_mat_2015[i,4]<-sum(getValues(cropped.grid)==4)
  landcover_mat_2015[i,5]<-sum(getValues(cropped.grid)==5)
  landcover_mat_2015[i,6]<-sum(getValues(cropped.grid)==6)
  landcover_mat_2015[i,7]<-sum(getValues(cropped.grid)==7)
  landcover_mat_2015[i,8]<-sum(getValues(cropped.grid)==8)
  landcover_mat_2015[i,9]<-sum(getValues(cropped.grid)==9)
  landcover_mat_2015[i,10]<-sum(getValues(cropped.grid)==10)
  landcover_mat_2015[i,11]<-sum(getValues(cropped.grid)==11)
  landcover_mat_2015[i,12]<-sum(getValues(cropped.grid)==12)
  landcover_mat_2015[i,13]<-sum(getValues(cropped.grid)==13)
  landcover_mat_2015[i,14]<-sum(getValues(cropped.grid)==14)
  landcover_mat_2015[i,15]<-sum(getValues(cropped.grid)==15)
  landcover_mat_2015[i,16]<-sum(getValues(cropped.grid)==16)
  landcover_mat_2015[i,17]<-sum(getValues(cropped.grid)==17)
  
}

for(i in 1:nrow(terrestrial.grid@data)){
  cropped.grid<-crop(landcover.list.raster[[16]],extent(terrestrial.grid@data$left[i],terrestrial.grid@data$right[i],terrestrial.grid@data$bottom[i],terrestrial.grid@data$top[i]))
  
  
  landcover_mat_2016[i,18]<-sum(getValues(cropped.grid)>=1,na.rm=T)
  landcover_mat_2016[i,1]<-sum(getValues(cropped.grid)==1)
  landcover_mat_2016[i,2]<-sum(getValues(cropped.grid)==2)
  landcover_mat_2016[i,3]<-sum(getValues(cropped.grid)==3)
  landcover_mat_2016[i,4]<-sum(getValues(cropped.grid)==4)
  landcover_mat_2016[i,5]<-sum(getValues(cropped.grid)==5)
  landcover_mat_2016[i,6]<-sum(getValues(cropped.grid)==6)
  landcover_mat_2016[i,7]<-sum(getValues(cropped.grid)==7)
  landcover_mat_2016[i,8]<-sum(getValues(cropped.grid)==8)
  landcover_mat_2016[i,9]<-sum(getValues(cropped.grid)==9)
  landcover_mat_2016[i,10]<-sum(getValues(cropped.grid)==10)
  landcover_mat_2016[i,11]<-sum(getValues(cropped.grid)==11)
  landcover_mat_2016[i,12]<-sum(getValues(cropped.grid)==12)
  landcover_mat_2016[i,13]<-sum(getValues(cropped.grid)==13)
  landcover_mat_2016[i,14]<-sum(getValues(cropped.grid)==14)
  landcover_mat_2016[i,15]<-sum(getValues(cropped.grid)==15)
  landcover_mat_2016[i,16]<-sum(getValues(cropped.grid)==16)
  landcover_mat_2016[i,17]<-sum(getValues(cropped.grid)==17)
  
}


for(i in 1:nrow(terrestrial.grid@data)){
  cropped.grid<-crop(landcover.list.raster[[17]],extent(terrestrial.grid@data$left[i],terrestrial.grid@data$right[i],terrestrial.grid@data$bottom[i],terrestrial.grid@data$top[i]))
  
  
  landcover_mat_2017[i,18]<-sum(getValues(cropped.grid)>=1,na.rm=T)
  landcover_mat_2017[i,1]<-sum(getValues(cropped.grid)==1)
  landcover_mat_2017[i,2]<-sum(getValues(cropped.grid)==2)
  landcover_mat_2017[i,3]<-sum(getValues(cropped.grid)==3)
  landcover_mat_2017[i,4]<-sum(getValues(cropped.grid)==4)
  landcover_mat_2017[i,5]<-sum(getValues(cropped.grid)==5)
  landcover_mat_2017[i,6]<-sum(getValues(cropped.grid)==6)
  landcover_mat_2017[i,7]<-sum(getValues(cropped.grid)==7)
  landcover_mat_2017[i,8]<-sum(getValues(cropped.grid)==8)
  landcover_mat_2017[i,9]<-sum(getValues(cropped.grid)==9)
  landcover_mat_2017[i,10]<-sum(getValues(cropped.grid)==10)
  landcover_mat_2017[i,11]<-sum(getValues(cropped.grid)==11)
  landcover_mat_2017[i,12]<-sum(getValues(cropped.grid)==12)
  landcover_mat_2017[i,13]<-sum(getValues(cropped.grid)==13)
  landcover_mat_2017[i,14]<-sum(getValues(cropped.grid)==14)
  landcover_mat_2017[i,15]<-sum(getValues(cropped.grid)==15)
  landcover_mat_2017[i,16]<-sum(getValues(cropped.grid)==16)
  landcover_mat_2017[i,17]<-sum(getValues(cropped.grid)==17)
  
}

for(i in 1:nrow(terrestrial.grid@data)){
  cropped.grid<-crop(landcover.list.raster[[18]],extent(terrestrial.grid@data$left[i],terrestrial.grid@data$right[i],terrestrial.grid@data$bottom[i],terrestrial.grid@data$top[i]))
  
  
  landcover_mat_2018[i,18]<-sum(getValues(cropped.grid)>=1,na.rm=T)
  landcover_mat_2018[i,1]<-sum(getValues(cropped.grid)==1)
  landcover_mat_2018[i,2]<-sum(getValues(cropped.grid)==2)
  landcover_mat_2018[i,3]<-sum(getValues(cropped.grid)==3)
  landcover_mat_2018[i,4]<-sum(getValues(cropped.grid)==4)
  landcover_mat_2018[i,5]<-sum(getValues(cropped.grid)==5)
  landcover_mat_2018[i,6]<-sum(getValues(cropped.grid)==6)
  landcover_mat_2018[i,7]<-sum(getValues(cropped.grid)==7)
  landcover_mat_2018[i,8]<-sum(getValues(cropped.grid)==8)
  landcover_mat_2018[i,9]<-sum(getValues(cropped.grid)==9)
  landcover_mat_2018[i,10]<-sum(getValues(cropped.grid)==10)
  landcover_mat_2018[i,11]<-sum(getValues(cropped.grid)==11)
  landcover_mat_2018[i,12]<-sum(getValues(cropped.grid)==12)
  landcover_mat_2018[i,13]<-sum(getValues(cropped.grid)==13)
  landcover_mat_2018[i,14]<-sum(getValues(cropped.grid)==14)
  landcover_mat_2018[i,15]<-sum(getValues(cropped.grid)==15)
  landcover_mat_2018[i,16]<-sum(getValues(cropped.grid)==16)
  landcover_mat_2018[i,17]<-sum(getValues(cropped.grid)==17)
  
}

for(i in 1:nrow(terrestrial.grid@data)){
  cropped.grid<-crop(landcover.list.raster[[19]],extent(terrestrial.grid@data$left[i],terrestrial.grid@data$right[i],terrestrial.grid@data$bottom[i],terrestrial.grid@data$top[i]))
  
  
  landcover_mat_2019[i,18]<-sum(getValues(cropped.grid)>=1,na.rm=T)
  landcover_mat_2019[i,1]<-sum(getValues(cropped.grid)==1)
  landcover_mat_2019[i,2]<-sum(getValues(cropped.grid)==2)
  landcover_mat_2019[i,3]<-sum(getValues(cropped.grid)==3)
  landcover_mat_2019[i,4]<-sum(getValues(cropped.grid)==4)
  landcover_mat_2019[i,5]<-sum(getValues(cropped.grid)==5)
  landcover_mat_2019[i,6]<-sum(getValues(cropped.grid)==6)
  landcover_mat_2019[i,7]<-sum(getValues(cropped.grid)==7)
  landcover_mat_2019[i,8]<-sum(getValues(cropped.grid)==8)
  landcover_mat_2019[i,9]<-sum(getValues(cropped.grid)==9)
  landcover_mat_2019[i,10]<-sum(getValues(cropped.grid)==10)
  landcover_mat_2019[i,11]<-sum(getValues(cropped.grid)==11)
  landcover_mat_2019[i,12]<-sum(getValues(cropped.grid)==12)
  landcover_mat_2019[i,13]<-sum(getValues(cropped.grid)==13)
  landcover_mat_2019[i,14]<-sum(getValues(cropped.grid)==14)
  landcover_mat_2019[i,15]<-sum(getValues(cropped.grid)==15)
  landcover_mat_2019[i,16]<-sum(getValues(cropped.grid)==16)
  landcover_mat_2019[i,17]<-sum(getValues(cropped.grid)==17)
  
}

for(i in 1:nrow(terrestrial.grid@data)){
  cropped.grid<-crop(landcover.list.raster[[20]],extent(terrestrial.grid@data$left[i],terrestrial.grid@data$right[i],terrestrial.grid@data$bottom[i],terrestrial.grid@data$top[i]))
  
  
  landcover_mat_2020[i,18]<-sum(getValues(cropped.grid)>=1,na.rm=T)
  landcover_mat_2020[i,1]<-sum(getValues(cropped.grid)==1)
  landcover_mat_2020[i,2]<-sum(getValues(cropped.grid)==2)
  landcover_mat_2020[i,3]<-sum(getValues(cropped.grid)==3)
  landcover_mat_2020[i,4]<-sum(getValues(cropped.grid)==4)
  landcover_mat_2020[i,5]<-sum(getValues(cropped.grid)==5)
  landcover_mat_2020[i,6]<-sum(getValues(cropped.grid)==6)
  landcover_mat_2020[i,7]<-sum(getValues(cropped.grid)==7)
  landcover_mat_2020[i,8]<-sum(getValues(cropped.grid)==8)
  landcover_mat_2020[i,9]<-sum(getValues(cropped.grid)==9)
  landcover_mat_2020[i,10]<-sum(getValues(cropped.grid)==10)
  landcover_mat_2020[i,11]<-sum(getValues(cropped.grid)==11)
  landcover_mat_2020[i,12]<-sum(getValues(cropped.grid)==12)
  landcover_mat_2020[i,13]<-sum(getValues(cropped.grid)==13)
  landcover_mat_2020[i,14]<-sum(getValues(cropped.grid)==14)
  landcover_mat_2020[i,15]<-sum(getValues(cropped.grid)==15)
  landcover_mat_2020[i,16]<-sum(getValues(cropped.grid)==16)
  landcover_mat_2020[i,17]<-sum(getValues(cropped.grid)==17)
  
}

b<-Sys.time()
b-a #


landcover_mat_2001<-cbind(ID, landcover_mat_2001)
landcover_mat_2002<-cbind(ID, landcover_mat_2002)
landcover_mat_2003<-cbind(ID, landcover_mat_2003)
landcover_mat_2004<-cbind(ID, landcover_mat_2004)
landcover_mat_2005<-cbind(ID, landcover_mat_2005)
landcover_mat_2006<-cbind(ID, landcover_mat_2006)
landcover_mat_2007<-cbind(ID, landcover_mat_2007)
landcover_mat_2008<-cbind(ID, landcover_mat_2008)
landcover_mat_2009<-cbind(ID, landcover_mat_2009)
landcover_mat_2010<-cbind(ID, landcover_mat_2010)
landcover_mat_2011<-cbind(ID, landcover_mat_2011)
landcover_mat_2012<-cbind(ID, landcover_mat_2012)
landcover_mat_2013<-cbind(ID, landcover_mat_2013)
landcover_mat_2014<-cbind(ID, landcover_mat_2014)
landcover_mat_2015<-cbind(ID, landcover_mat_2015)
landcover_mat_2016<-cbind(ID, landcover_mat_2016)
landcover_mat_2017<-cbind(ID, landcover_mat_2017)
landcover_mat_2018<-cbind(ID, landcover_mat_2018)
landcover_mat_2019<-cbind(ID, landcover_mat_2019)
landcover_mat_2020<-cbind(ID, landcover_mat_2020)

colnames(landcover_mat_2001)<-c("ID",paste0("landcover_",1:17), "total")
colnames(landcover_mat_2002)<-c("ID",paste0("landcover_",1:17), "total")
colnames(landcover_mat_2003)<-c("ID",paste0("landcover_",1:17), "total")
colnames(landcover_mat_2004)<-c("ID",paste0("landcover_",1:17), "total")
colnames(landcover_mat_2005)<-c("ID",paste0("landcover_",1:17), "total")
colnames(landcover_mat_2006)<-c("ID",paste0("landcover_",1:17), "total")
colnames(landcover_mat_2007)<-c("ID",paste0("landcover_",1:17), "total")
colnames(landcover_mat_2008)<-c("ID",paste0("landcover_",1:17), "total")
colnames(landcover_mat_2009)<-c("ID",paste0("landcover_",1:17), "total")
colnames(landcover_mat_2010)<-c("ID",paste0("landcover_",1:17), "total")
colnames(landcover_mat_2011)<-c("ID",paste0("landcover_",1:17), "total")
colnames(landcover_mat_2012)<-c("ID",paste0("landcover_",1:17), "total")
colnames(landcover_mat_2013)<-c("ID",paste0("landcover_",1:17), "total")
colnames(landcover_mat_2014)<-c("ID",paste0("landcover_",1:17), "total")
colnames(landcover_mat_2015)<-c("ID",paste0("landcover_",1:17), "total")
colnames(landcover_mat_2016)<-c("ID",paste0("landcover_",1:17), "total")
colnames(landcover_mat_2017)<-c("ID",paste0("landcover_",1:17), "total")
colnames(landcover_mat_2018)<-c("ID",paste0("landcover_",1:17), "total")
colnames(landcover_mat_2019)<-c("ID",paste0("landcover_",1:17), "total")
colnames(landcover_mat_2020)<-c("ID",paste0("landcover_",1:17), "total")


setwd("D:\\Environmental data\\Hendra-Australia\\Agricultural_landuse")
write.csv(landcover_mat_2001,"landcover_mat_2001.csv")
write.csv(landcover_mat_2002,"landcover_mat_2002.csv")
write.csv(landcover_mat_2003,"landcover_mat_2003.csv")
write.csv(landcover_mat_2004,"landcover_mat_2004.csv")
write.csv(landcover_mat_2005,"landcover_mat_2005.csv")
write.csv(landcover_mat_2006,"landcover_mat_2006.csv")
write.csv(landcover_mat_2007,"landcover_mat_2007.csv")
write.csv(landcover_mat_2008,"landcover_mat_2008.csv")
write.csv(landcover_mat_2009,"landcover_mat_2009.csv")
write.csv(landcover_mat_2010,"landcover_mat_2010.csv")
write.csv(landcover_mat_2011,"landcover_mat_2011.csv")
write.csv(landcover_mat_2012,"landcover_mat_2012.csv")
write.csv(landcover_mat_2013,"landcover_mat_2013.csv")
write.csv(landcover_mat_2014,"landcover_mat_2014.csv")
write.csv(landcover_mat_2015,"landcover_mat_2015.csv")
write.csv(landcover_mat_2016,"landcover_mat_2016.csv")
write.csv(landcover_mat_2017,"landcover_mat_2017.csv")
write.csv(landcover_mat_2018,"landcover_mat_2018.csv")
write.csv(landcover_mat_2019,"landcover_mat_2019.csv")
write.csv(landcover_mat_2020,"landcover_mat_2020.csv")




#############################################################################################################
## Horse population  
############################################################################################################
setwd("D:\\Environmental data\\Hendra-Australia\\Horese_number")

raw.horse <- raster("6_Ho_2010_Aw.tif")
plot(raw.horse)

####### R로 할 수 있는데, QGIS가 빠름
## horse.clipped <- mask(raw.horse, terrestrial.grid)
horse.clipped <- raster("horse_number_clipped.tif")

terrestrial.grid@data
#######



a<-Sys.time()
global.horse.mat <- 
  foreach(j = 1:nrow(terrestrial.grid@data),.combine = 'rbind',.packages = c("raster"),.errorhandling = 'pass')%dopar%{
    cropped.grid<-crop(horse.clipped, extent(terrestrial.grid@data$left[j],terrestrial.grid@data$right[j],terrestrial.grid@data$bottom[j],terrestrial.grid@data$top[j]))
    global.horse<-sum(getValues(cropped.grid),na.rm=T)
    return(global.horse)
  }
b<-Sys.time()
b-a # 3 secs

global.horse.mat <- as.data.frame(global.horse.mat)
global.horse <- cbind(ID, global.horse.mat)
global.horse <- global.horse %>% 
  rename(ID = id,
         horse = V1)

write.csv(global.horse,"global_horse.csv")

head(global.elevation)
nrow(global.elevation)
sum(is.na(global.elevation))

#################################################################################################################
