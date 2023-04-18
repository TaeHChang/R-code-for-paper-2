#### flying-fox distribution 정리
library(tidyverse)
library(readxl)
final_template <- read_excel("C:/Users/Taehee/OneDrive/바탕 화면/My papers/Hendravirus_작업중/flying-fox_distribution/final_template.xlsx", 
                             na = "NA")

View(final_template)

final_template

##################################
GHFF <- final_template %>% 
  dplyr::filter(Species == "GHFF")
  
LRFF <- final_template %>% 
  dplyr::filter(Species == "LRFF")
  
SFF <-  final_template %>% 
  dplyr::filter(Species == "SFF")

BFF <- final_template %>% 
  dplyr::filter(Species == "BFF")



##################################
GHFF <- GHFF %>% rowwise() %>% 
  mutate(average_GHFF = mean(c(`2012`, `2013`, `2014`, `2015`, `2016`, `2017`,
                               `2018`, `2019`, `2020`, `2021`, `2022`), na.rm = TRUE)) %>% 
  dplyr::select(2, Species, Latitude, Longitude, average_GHFF)

LRFF <- LRFF %>% rowwise() %>% 
  mutate(average_LRFF = mean(c(`2012`, `2013`, `2014`, `2015`, `2016`, `2017`,
                               `2018`, `2019`, `2020`, `2021`, `2022`), na.rm = TRUE)) %>% 
  dplyr::select(2, Species, Latitude, Longitude, average_LRFF)

SFF <- SFF %>% rowwise() %>% 
  mutate(average_SFF = mean(c(`2012`, `2013`, `2014`, `2015`, `2016`, `2017`,
                               `2018`, `2019`, `2020`, `2021`, `2022`), na.rm = TRUE)) %>% 
  dplyr::select(2, Species, Latitude, Longitude, average_SFF)

BFF <- BFF %>% rowwise() %>% 
  mutate(average_BFF = mean(c(`2012`, `2013`, `2014`, `2015`, `2016`, `2017`,
                               `2018`, `2019`, `2020`, `2021`, `2022`), na.rm = TRUE)) %>% 
  dplyr::select(2, Species, Latitude, Longitude, average_BFF)


write.csv(GHFF, "C:\\Users\\Taehee\\OneDrive\\바탕 화면\\My papers\\Hendravirus_작업중\\flying-fox_distribution\\GHFF.csv")

write.csv(LRFF, "C:\\Users\\Taehee\\OneDrive\\바탕 화면\\My papers\\Hendravirus_작업중\\flying-fox_distribution\\LRFF.csv")

write.csv(SFF, "C:\\Users\\Taehee\\OneDrive\\바탕 화면\\My papers\\Hendravirus_작업중\\flying-fox_distribution\\SFF.csv")

write.csv(BFF, "C:\\Users\\Taehee\\OneDrive\\바탕 화면\\My papers\\Hendravirus_작업중\\flying-fox_distribution\\BFF.csv")




################################################
##### Comprehensive map of flying-foxes
################################################

library(sf)
library(raster)
library(dplyr)
library(tmap)
library(ggplot2)

Australia <- st_read("D:\\Environmental data\\Hendra-Australia\\AUS_adm\\AUS_adm1.shp")
GHFF <- st_read("C:\\Users\\Taehee\\OneDrive\\바탕 화면\\My papers\\Hendravirus_작업중\\flying-fox_distribution\\shp\\GHFF.shp")
SFF <- st_read('C:\\Users\\Taehee\\OneDrive\\바탕 화면\\My papers\\Hendravirus_작업중\\flying-fox_distribution\\shp\\SFF.shp')
BFF <- st_read("C:\\Users\\Taehee\\OneDrive\\바탕 화면\\My papers\\Hendravirus_작업중\\flying-fox_distribution\\shp\\BFF.shp")
LRFF <- st_read("C:\\Users\\Taehee\\OneDrive\\바탕 화면\\My papers\\Hendravirus_작업중\\flying-fox_distribution\\shp\\LRFF.shp")

GHFF <- GHFF %>% 
  dplyr::filter(average_GH > 0)
SFF <- SFF %>% 
  dplyr::filter(average_SF > 0)
BFF <- BFF %>% 
  dplyr::filter(average_BF > 0)
LRFF <- LRFF %>% 
  dplyr::filter(average_LR > 0)

map1 <- tm_shape(Australia) + tm_borders(lwd = 0.7, col = "black") + tm_fill("grey", alpha = 0.1)


map2 <- map1 + 
  tm_shape(BFF) + tm_dots(size = "average_BF", col = "purple", alpha = 0.5) +
  tm_scale_bar(size = 1, position = "right") + #스케일 바 집어넣기
  tm_compass(type = "8star", position = c("RIGHT", "top")) 


map3 <- map2 + 
  tm_shape(SFF) + tm_dots(size = "average_SF", col = "#8f784b", alpha = 0.5) 


map4 <- map3 +
  tm_shape(LRFF) + tm_dots(size = "average_LR", col = "#1f78b4", alpha = 0.5)

map5 <- map4 +
  tm_shape(GHFF) + tm_dots(size = "average_GH", col = "orange", alpha = 0.3)

map5










