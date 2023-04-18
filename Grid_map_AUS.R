
### Base map / grid



library(dplyr)
library(tmap)
library(ggplot2)
library(sf)
library(readr)
library(RColorBrewer)

####################################################################
final_data_filtered <- read.csv("D:\\Environmental data\\Hendra-Australia\\Final_dataset\\Updated_20230306\\final_data_filtered.csv")

final_data_filtered <- final_data_filtered %>% 
  dplyr::select(-accipitriformes_Maxent, -strigiformes_Maxent, -carnivora_Maxent,
                -colubridae_Maxent, -chiroptera_Maxent, -hendra_gp, -accipitriformesGP, -strigiformesGP,
                -RaptorsGP, -carnivoraGP, -colubridaeGP, -chiroptera_maxent_existence, -accipitriformes_maxent_existence, 
                -strigiformes_maxent_existence, -carnivora_maxent_existence, -colubridae_maxent_existence)

accipitrformes_prop25 <- read_csv("D:\\Environmental data\\Hendra-Australia\\Maxent_biased\\Species_mat\\accipitriformes_prop25_count.csv", 
                                  col_types = cols(...1 = col_skip()))
accipitrformes_prop50 <- read_csv("D:\\Environmental data\\Hendra-Australia\\Maxent_biased\\Species_mat\\accipitriformes_prop50_count.csv", 
                                  col_types = cols(...1 = col_skip()))
accipitrformes_prop75 <- read_csv("D:\\Environmental data\\Hendra-Australia\\Maxent_biased\\Species_mat\\accipitriformes_prop75_count.csv", 
                                  col_types = cols(...1 = col_skip()))

strigiformes_prop25 <- read_csv("D:\\Environmental data\\Hendra-Australia\\Maxent_biased\\Species_mat\\strigiformes_prop25_count.csv", 
                                col_types = cols(...1 = col_skip()))
strigiformes_prop50 <- read_csv("D:\\Environmental data\\Hendra-Australia\\Maxent_biased\\Species_mat\\strigiformes_prop50_count.csv", 
                                col_types = cols(...1 = col_skip()))
strigiformes_prop75 <- read_csv("D:\\Environmental data\\Hendra-Australia\\Maxent_biased\\Species_mat\\strigiformes_prop75_count.csv", 
                                col_types = cols(...1 = col_skip()))

falconiformes_prop25 <- read_csv("D:\\Environmental data\\Hendra-Australia\\Maxent_biased\\Species_mat\\falconiformes_prop25_count.csv", 
                                 col_types = cols(...1 = col_skip()))
falconiformes_prop50 <- read_csv("D:\\Environmental data\\Hendra-Australia\\Maxent_biased\\Species_mat\\falconiformes_prop50_count.csv", 
                                 col_types = cols(...1 = col_skip()))
falconiformes_prop75 <- read_csv("D:\\Environmental data\\Hendra-Australia\\Maxent_biased\\Species_mat\\falconiformes_prop75_count.csv", 
                                 col_types = cols(...1 = col_skip()))

pythonidae_prop25 <- read_csv("D:\\Environmental data\\Hendra-Australia\\Maxent_biased\\Species_mat\\pythonidae_prop25_count.csv", 
                              col_types = cols(...1 = col_skip()))
pythonidae_prop50 <- read_csv("D:\\Environmental data\\Hendra-Australia\\Maxent_biased\\Species_mat\\pythonidae_prop50_count.csv", 
                              col_types = cols(...1 = col_skip()))
pythonidae_prop75 <- read_csv("D:\\Environmental data\\Hendra-Australia\\Maxent_biased\\Species_mat\\pythonidae_prop75_count.csv", 
                              col_types = cols(...1 = col_skip()))

colubridae_prop25 <- read_csv("D:\\Environmental data\\Hendra-Australia\\Maxent_biased\\Species_mat\\colubridae_prop25_count.csv", 
                              col_types = cols(...1 = col_skip()))
colubridae_prop50 <- read_csv("D:\\Environmental data\\Hendra-Australia\\Maxent_biased\\Species_mat\\colubridae_prop50_count.csv", 
                              col_types = cols(...1 = col_skip()))
colubridae_prop75 <- read_csv("D:\\Environmental data\\Hendra-Australia\\Maxent_biased\\Species_mat\\colubridae_prop75_count.csv", 
                              col_types = cols(...1 = col_skip()))

chiroptera_prop25 <- read_csv("D:\\Environmental data\\Hendra-Australia\\Maxent_biased\\Species_mat\\chiroptera_prop25_count.csv", 
                              col_types = cols(...1 = col_skip()))
chiroptera_prop50 <- read_csv("D:\\Environmental data\\Hendra-Australia\\Maxent_biased\\Species_mat\\chiroptera_prop50_count.csv", 
                              col_types = cols(...1 = col_skip()))
chiroptera_prop75 <- read_csv("D:\\Environmental data\\Hendra-Australia\\Maxent_biased\\Species_mat\\chiroptera_prop75_count.csv", 
                              col_types = cols(...1 = col_skip()))

carnivora_prop25 <- read_csv("D:\\Environmental data\\Hendra-Australia\\Maxent_biased\\Species_mat\\carnivora_prop25_count.csv", 
                             col_types = cols(...1 = col_skip()))
carnivora_prop50 <- read_csv("D:\\Environmental data\\Hendra-Australia\\Maxent_biased\\Species_mat\\carnivora_prop50_count.csv", 
                             col_types = cols(...1 = col_skip()))
carnivora_prop75 <- read_csv("D:\\Environmental data\\Hendra-Australia\\Maxent_biased\\Species_mat\\carnivora_prop75_count.csv", 
                             col_types = cols(...1 = col_skip()))

final_data_filtered <- final_data_filtered %>% 
  left_join(accipitrformes_prop25, by = c("ID" = "id")) %>% 
  left_join(accipitrformes_prop50, by = c("ID" = "id")) %>% 
  left_join(accipitrformes_prop75, by = c("ID" = "id")) %>% 
  left_join(strigiformes_prop25, by = c("ID" = "id")) %>% 
  left_join(strigiformes_prop50, by = c("ID" = "id")) %>% 
  left_join(strigiformes_prop75, by = c("ID" = "id")) %>% 
  left_join(falconiformes_prop25, by = c("ID" = "id")) %>% 
  left_join(falconiformes_prop50, by = c("ID" = "id")) %>% 
  left_join(falconiformes_prop75, by = c("ID" = "id")) %>% 
  left_join(colubridae_prop25, by = c("ID" = "id")) %>% 
  left_join(colubridae_prop50, by = c("ID" = "id")) %>% 
  left_join(colubridae_prop75, by = c("ID" = "id")) %>%   
  left_join(carnivora_prop25, by = c("ID" = "id")) %>% 
  left_join(carnivora_prop50, by = c("ID" = "id")) %>% 
  left_join(carnivora_prop75, by = c("ID" = "id")) %>%    
  left_join(chiroptera_prop25, by = c("ID" = "id")) %>% 
  left_join(chiroptera_prop50, by = c("ID" = "id")) %>% 
  left_join(chiroptera_prop75, by = c("ID" = "id")) %>%    
  left_join(pythonidae_prop25, by = c("ID" = "id")) %>% 
  left_join(pythonidae_prop50, by = c("ID" = "id")) %>% 
  left_join(pythonidae_prop75, by = c("ID" = "id"))   

forest <- read.csv("D:\\Environmental data\\Hendra-Australia\\Forest_cover\\treecover.csv")
forestloss <- read.csv("D:\\Environmental data\\Hendra-Australia\\Forest_cover\\treeloss.csv")
forestgain <- read.csv("D:\\Environmental data\\Hendra-Australia\\Forest_cover\\treegain.csv")

forestloss <- forestloss %>% 
  mutate(forestloss = Y2001 + Y2002 + Y2003 + Y2004 + Y2005 + Y2006 + Y2007 + Y2008 + Y2009 + Y2010) %>% 
  dplyr::select(id, forestloss)
forestcover <- forest %>% 
  left_join(forestloss, by = c("ID" = "id")) %>% 
  mutate(deforestation = (forestloss *100) / treecover)

forestcover[is.na(forestcover)] <- 0

final_data_filtered <- final_data_filtered %>% 
  left_join(forestcover, by ="ID")

final_data_filtered <- final_data_filtered %>% 
  mutate(species_richness = count_accipitriformes_prop50 + count_strigiformes_prop50 + count_falconiformes_prop50 +
           count_colubridae_prop50 + count_carnivora_prop50 + count_pythonidae_prop50)

deforestation <- final_data_filtered %>% 
  dplyr::select(ID, deforestation)

species_richness <- final_data_filtered %>% 
  dplyr::select(ID, species_richness)

################################################################################################
AUS <- st_read("D:\\Environmental data\\Hendra-Australia\\AUS_adm\\AUS_adm1.shp")
hendra_grid <- st_read("D:\\Environmental data\\Hendra-Australia\\AUS_basemap\\final\\Basemap_clipped_numpoints_final.shp")


map1 <- tm_shape(AUS) + tm_borders(lwd = 0.7, col = "black") + tm_fill("grey", alpha = 0.1)

hendra_grid_1 <- hendra_grid %>% 
  mutate(pointGP = ifelse(NUMPOINTS < 1, 1, 2)) %>% 
  dplyr::filter(id %in% final_data_filtered$ID) %>% 
  left_join(deforestation, by = c("id" = "ID")) %>% 
  left_join(species_richness , by = c("id" = "ID"))



map2 <- map1 + tm_shape(hendra_grid_1) + 
  tm_borders(lwd = 1.5, col = "black") + 
  tm_fill(col = "pointGP", alpha = 0.6) +
  tmap_options(check.and.fix = TRUE)
map2 

map3 <- map1 + tm_shape(hendra_grid_1) + 
  tm_borders(lwd = 1.5, col = "black") + 
  tm_fill(col = "deforestation", palette = "OrRd", style = "cont", alpha = 0.8) +
  tmap_options(check.and.fix = TRUE)
map3 

map4 <- map1 + tm_shape(hendra_grid_1) + 
  tm_borders(lwd = 1.5, col = "black") + 
  tm_fill(col = "species_richness", palette = "Blues", style = "cont") +
  tmap_options(check.and.fix = TRUE)
map4 


