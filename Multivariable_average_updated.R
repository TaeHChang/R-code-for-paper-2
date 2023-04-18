

#####################################################################
################ Flying-fox 발견된 곳만 Filtered data / 20230407 이후로 flying-fox occurrence level을 포함하는 모델로 변경하고,
################ Biased Maxent modelling으로 계산한 종다양성 변수 사용.
#####################################################################

library(car)
library(mlogit)
library(boot)
library(ggplot2)
library(tidyverse)
library(readr)




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
  

## Flying-fox distribution covariate로 포함하기 위한 준비과정.
# P. alecto / P. conspicillatus BFF/SFF 포함.
final_data_filtered <- final_data_filtered %>% 
  mutate(BFF_predict = log(BFF_predict),
         SFF_predict = log(SFF_predict),
         GHFF_predict = log(GHFF_predict),
         LRFF_predict = log(LRFF_predict))

######################## 새로 추가한 변수만 univariable table 
final_data_filtered  <- final_data_filtered  %>% 
  mutate(hendra_gp = ifelse(hendra >0, 1, 0))

final_data_filtered$hendra_gp <-factor(final_data_filtered$hendra_gp, levels = c(0, 1))



########################### Deforestation 2000 ~ 2010까지의 기간 #############################
##### forest cover 변화량을 계산.
##### (2000년 - 2010년) / 2000년. forest cover 차이의 절대값이 음수인 경우 0으로 처리
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

# Deforestation
final_data_filtered %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(deforestation),
            SD = sd(deforestation))

ttest <- t.test(deforestation ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(deforestation ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox


##  Chiroptera 
final_data_filtered %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(count_chiroptera_prop50),
            SD = sd(count_chiroptera_prop50))

ttest <- t.test(count_chiroptera_prop50 ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(count_chiroptera_prop50 ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Accipitriformes 
final_data_filtered %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(count_accipitriformes_prop50),
            SD = sd(count_accipitriformes_prop50))

ttest <- t.test(count_accipitriformes_prop50 ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(count_accipitriformes_prop50 ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Strigiformes
final_data_filtered %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(count_strigiformes_prop50),
            SD = sd(count_strigiformes_prop50))

ttest <- t.test(count_strigiformes_prop50 ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(count_strigiformes_prop50 ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Falconiformes
final_data_filtered %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(count_falconiformes_prop50),
            SD = sd(count_falconiformes_prop50))

ttest <- t.test(count_falconiformes_prop50 ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(count_falconiformes_prop50 ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Colubridae
final_data_filtered %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(count_colubridae_prop50),
            SD = sd(count_colubridae_prop50))

ttest <- t.test(count_colubridae_prop50 ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(count_colubridae_prop50 ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Pythonidae
final_data_filtered %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(count_pythonidae_prop50),
            SD = sd(count_pythonidae_prop50))

ttest <- t.test(count_pythonidae_prop50 ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(count_pythonidae_prop50 ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox


##  Carnivora
final_data_filtered %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(count_carnivora_prop50),
            SD = sd(count_carnivora_prop50))

ttest <- t.test(count_carnivora_prop50 ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(count_carnivora_prop50 ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox



library(corrplot)

whole <- final_data_filtered %>% 
  dplyr::select(hfp_average, elevation, precipitation_average, temperature_average, GDP_average, 
                HDI_average, forest_cover_average, pop_dense_average, horse, landcover_2_average,
                landcover_4_average, landcover_5_average, landcover_6_average, landcover_7_average,
                landcover_8_average, landcover_9_average, landcover_10_average, landcover_12_average,
                landcover_13_average, landcover_14_average,BFF_predict, SFF_predict,
                count_accipitriformes_prop50, count_strigiformes_prop50, count_falconiformes_prop50,
                count_chiroptera_prop50, count_colubridae_prop50, count_pythonidae_prop50, 
                count_carnivora_prop50) 

mwhole <- cor(whole, use="pairwise.complete.obs")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mwhole, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
         col = col(10), addCoef.col = "black", cl.pos = "n", order = "AOE", type = {"lower"})


## GDP, HDI, landcover 2, 7, 8, 13, forest cover, precipitation 제외
whole <- final_data_filtered %>% 
  dplyr::select(hfp_average, elevation, temperature_average,
                pop_dense_average, horse, BFF_predict, SFF_predict,
                landcover_4_average, landcover_5_average, landcover_6_average, 
                landcover_9_average, landcover_10_average, landcover_12_average,
                landcover_14_average,
                count_accipitriformes_prop50, count_strigiformes_prop50, count_falconiformes_prop50,
                count_chiroptera_prop50, count_colubridae_prop50, count_pythonidae_prop50, 
                count_carnivora_prop50) 

mwhole <- cor(whole, use="pairwise.complete.obs")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mwhole, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
         col = col(10), addCoef.col = "black", cl.pos = "n", order = "AOE", type = {"lower"})

## temperature, chiroptera, colubridae, pop_dense, landcover 6 추가로 제외 Univariable에서 유의하지 않은 것들.
whole <- final_data_filtered %>% 
  dplyr::select(hfp_average, elevation, horse, BFF_predict, SFF_predict,  landcover_9_average, landcover_10_average,
                count_accipitriformes_prop50, count_strigiformes_prop50, count_falconiformes_prop50,
                count_chiroptera_prop50, count_colubridae_prop50, count_pythonidae_prop50, 
                count_carnivora_prop50)


mwhole <- cor(whole, use="pairwise.complete.obs")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mwhole, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
         col = col(10), addCoef.col = "black", cl.pos = "n", order = "AOE", type = {"lower"})


##### 여기서 지금 문제는, accipitriformes랑 strigiformes가 상관성이 너무 크게 나온다는 건데, 
### 일단은 결과가 유의하였을 때 해석이 좀 더 쉬운 strigiformes로 한 번 해보기.
## 혹은, 미팅 발표에서, 맹금류로 합치거나 하는 방안을 제시해보기. Raptors

## Raptors 만들어서 해보기.
final_data_filtered <- final_data_filtered %>% 
  mutate(Raptors = count_accipitriformes_prop50 + count_strigiformes_prop50 + count_falconiformes_prop50)

## Snakes 만들어서 해보기.
final_data_filtered <- final_data_filtered %>% 
  mutate(Snakes = count_colubridae_prop50 + count_pythonidae_prop50)

####### categorical analysis로 진행해보기.
final_data_filtered %>% summary()

#accipitriformes - 9.00/11.00/13.00
#strigiformes - 4.00/5.00/7.00
#falconiformes - 5랑 6 밖에 없음
#carnivora - 2.00 있고 없고로.
#colubridae - 2.00 있고 없고로.
#pythonidae - 2.00 있고 없고로.

#Raptors - 18.00/21.00/24.00
#Snakes - 3.00/5.00/7.00

final_data_filtered <- final_data_filtered %>% 
  mutate(accipitriformesGP = ifelse(count_accipitriformes_prop50 < 9, 1, 
                                    ifelse(count_accipitriformes_prop50 < 11, 2,
                                           ifelse(count_accipitriformes_prop50 < 13, 3,4))))

final_data_filtered <- final_data_filtered %>% 
  mutate(strigiformesGP = ifelse(count_strigiformes_prop50 < 4, 1, 
                                 ifelse(count_strigiformes_prop50 < 5, 2,
                                        ifelse(count_strigiformes_prop50 < 7, 3,4))))

final_data_filtered <- final_data_filtered %>% 
  mutate(RaptorsGP = ifelse(Raptors < 18, 1, 
                            ifelse(Raptors  < 21, 2,
                                   ifelse(Raptors  < 24, 3,4))))

final_data_filtered <- final_data_filtered %>% 
  mutate(carnivoraGP = ifelse(count_carnivora_prop50 < 2, 1, 2))

final_data_filtered <- final_data_filtered %>% 
  mutate(colubridaeGP = ifelse(count_colubridae_prop50 < 2, 1, 2))

final_data_filtered <- final_data_filtered %>% 
  mutate(pythonidaeGP = ifelse(count_pythonidae_prop50 < 2, 1, 2))

final_data_filtered <- final_data_filtered %>% 
  mutate(SnakesGP = ifelse(Snakes < 3, 1, 
                            ifelse(Snakes  < 5, 2,
                                   ifelse(Snakes  < 7, 3,4))))


final_data_filtered$accipitriformesGP <-factor(final_data_filtered$accipitriformesGP, 
                                               levels = c(1, 2, 3, 4))
final_data_filtered$strigiformesGP <-factor(final_data_filtered$strigiformesGP, 
                                            levels = c(1, 2, 3, 4))
final_data_filtered$RaptorsGP <-factor(final_data_filtered$RaptorsGP, 
                                       levels = c(1, 2, 3, 4))
final_data_filtered$carnivoraGP <-factor(final_data_filtered$carnivoraGP, 
                                         levels = c(1, 2))
final_data_filtered$colubridaeGP <-factor(final_data_filtered$colubridaeGP, 
                                          levels = c(1, 2))
final_data_filtered$pythonidaeGP <-factor(final_data_filtered$pythonidaeGP, 
                                          levels = c(1, 2))
final_data_filtered$SnakesGP <-factor(final_data_filtered$SnakesGP, 
                                      levels = c(1, 2, 3, 4))
summary(final_data_filtered$carnivoraGP)
### 1. 다중공선성만 제외한 포화모형. 
hendra_rev_1 <- glm(hendra_gp ~ strigiformesGP + 
                      pythonidaeGP + colubridaeGP + carnivoraGP + 
                      count_chiroptera_prop50 + hfp_average + elevation + horse + BFF_predict + landcover_9_average + landcover_10_average
                    , 
                    data = final_data_filtered, family = binomial(link = 'logit'), na.action = na.exclude)

# snakes는 다중공선성 때문에 제외
hendra_rev_1 <- glm(hendra_gp ~ RaptorsGP +  carnivoraGP + colubridaeGP +
                      count_chiroptera_prop50 + hfp_average + elevation + horse + BFF_predict + landcover_9_average + landcover_10_average
                    , 
                    data = final_data_filtered, family = binomial(link = 'logit'), na.action = na.exclude)

summary(hendra_rev_1)
round(exp(hendra_rev_1$coefficients), 2) 
round(exp(confint(hendra_rev_1)), 2)


#오차의 독립성 가정을 평가하는 더빈-왓슨 검정.
durbinWatsonTest(hendra_rev_1) #자기상관성이 없음.

#다중공선성 평가를 위한 VIF(분산팽창인자) 통계량. 10보다 작아야 한다.
vif(hendra_rev_1)
#VIF의 역수로 tolerence를 구한다. 0.2보다 커야 한다.
1/vif(hendra_rev_1)
#평균 VIF를 구한다. 1에서 멀면 좋지 않다.
mean(vif(hendra_rev_1))

glmPseudoR2s <- function(LogModel) {
  dev <- LogModel$deviance 
  nullDev <- LogModel$null.deviance 
  modelN <-  length(LogModel$fitted.values)
  R.l <-  1 -  dev / nullDev
  R.cs <- 1- exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2  ", round(R.l, 3), "\n")
  cat("Cox and Snell R^2        ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2           ", round(R.n, 3),    "\n")
}
glmPseudoR2s(hendra_rev_1)








####################################################################################
#### prop 25
#####################################################################################



library(corrplot)

whole <- final_data_filtered %>% 
  dplyr::select(hfp_average, elevation, precipitation_average, temperature_average, GDP_average, 
                HDI_average, forest_cover_average, pop_dense_average, horse, landcover_2_average,
                landcover_4_average, landcover_5_average, landcover_6_average, landcover_7_average,
                landcover_8_average, landcover_9_average, landcover_10_average, landcover_12_average,
                landcover_13_average, landcover_14_average,BFF_predict, SFF_predict,
                count_accipitriformes_prop25, count_strigiformes_prop25, count_falconiformes_prop25,
                count_chiroptera_prop25, count_colubridae_prop25, count_pythonidae_prop25, 
                count_carnivora_prop25) 

mwhole <- cor(whole, use="pairwise.complete.obs")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mwhole, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
         col = col(10), addCoef.col = "black", cl.pos = "n", order = "AOE", type = {"lower"})


## GDP, HDI, landcover 2, 7, 8, 13, forest cover, precipitation 제외
whole <- final_data_filtered %>% 
  dplyr::select(hfp_average, elevation, temperature_average,
                pop_dense_average, horse, BFF_predict, SFF_predict,
                landcover_4_average, landcover_5_average, landcover_6_average, 
                landcover_9_average, landcover_10_average, landcover_12_average,
                landcover_14_average,
                count_accipitriformes_prop25, count_strigiformes_prop25, count_falconiformes_prop25,
                count_chiroptera_prop25, count_colubridae_prop25, count_pythonidae_prop25, 
                count_carnivora_prop25) 

mwhole <- cor(whole, use="pairwise.complete.obs")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mwhole, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
         col = col(10), addCoef.col = "black", cl.pos = "n", order = "AOE", type = {"lower"})

## temperature, chiroptera, colubridae, pop_dense, landcover 6 추가로 제외 Univariable에서 유의하지 않은 것들.
whole <- final_data_filtered %>% 
  dplyr::select(hfp_average, elevation, horse, BFF_predict, SFF_predict,  landcover_9_average, landcover_10_average,
                count_strigiformes_prop25, count_falconiformes_prop25,
                count_colubridae_prop25, count_pythonidae_prop25, 
                count_carnivora_prop25)


mwhole <- cor(whole, use="pairwise.complete.obs")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mwhole, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
         col = col(10), addCoef.col = "black", cl.pos = "n", order = "AOE", type = {"lower"})


##### 여기서 지금 문제는, accipitriformes랑 strigiformes가 상관성이 너무 크게 나온다는 건데, 
### 일단은 결과가 유의하였을 때 해석이 좀 더 쉬운 strigiformes로 한 번 해보기.
## 혹은, 미팅 발표에서, 맹금류로 합치거나 하는 방안을 제시해보기. Raptors

## Raptors 만들어서 해보기.
final_data_filtered <- final_data_filtered %>% 
  mutate(Raptors = count_accipitriformes_prop25 + count_strigiformes_prop25 + count_falconiformes_prop25)

## Snakes 만들어서 해보기.
final_data_filtered <- final_data_filtered %>% 
  mutate(Snakes = count_colubridae_prop25 + count_pythonidae_prop25)

####### categorical analysis로 진행해보기.
final_data_filtered %>% summary()

#accipitriformes - 9.00/11.00/13.00
#strigiformes - 4.00/5.00/7.00
#falconiformes - 5랑 6 밖에 없음
#carnivora - 2.00 있고 없고로.
#colubridae - 2.00 있고 없고로.
#pythonidae - 2.00 있고 없고로.

#Raptors - 18.00/21.00/24.00
#Snakes - 3.00/5.00/7.00

final_data_filtered <- final_data_filtered %>% 
  mutate(accipitriformesGP = ifelse(count_accipitriformes_prop25 < 9, 1, 
                                    ifelse(count_accipitriformes_prop25 < 11, 2,
                                           ifelse(count_accipitriformes_prop25 < 13, 3,4))))

final_data_filtered <- final_data_filtered %>% 
  mutate(strigiformesGP = ifelse(count_strigiformes_prop25 < 4, 1, 
                                 ifelse(count_strigiformes_prop25 < 5, 2,
                                        ifelse(count_strigiformes_prop25 < 7, 3,4))))

final_data_filtered <- final_data_filtered %>% 
  mutate(RaptorsGP = ifelse(Raptors < 20, 1, 
                            ifelse(Raptors  < 23, 2,
                                   ifelse(Raptors  < 25, 3,4))))

final_data_filtered <- final_data_filtered %>% 
  mutate(carnivoraGP = ifelse(count_carnivora_prop25 < 2, 1, 2))

final_data_filtered <- final_data_filtered %>% 
  mutate(colubridaeGP = ifelse(count_colubridae_prop25 < 2, 1, 2))

final_data_filtered <- final_data_filtered %>% 
  mutate(pythonidaeGP = ifelse(count_pythonidae_prop25 < 2, 1, 2))

final_data_filtered <- final_data_filtered %>% 
  mutate(SnakesGP = ifelse(Snakes < 3, 1, 
                           ifelse(Snakes  < 5, 2,
                                  ifelse(Snakes  < 7, 3,4))))


final_data_filtered$accipitriformesGP <-factor(final_data_filtered$accipitriformesGP, 
                                               levels = c(1, 2, 3, 4))
final_data_filtered$strigiformesGP <-factor(final_data_filtered$strigiformesGP, 
                                            levels = c(1, 2, 3, 4))
final_data_filtered$RaptorsGP <-factor(final_data_filtered$RaptorsGP, 
                                       levels = c(1, 2, 3, 4))
final_data_filtered$carnivoraGP <-factor(final_data_filtered$carnivoraGP, 
                                         levels = c(1, 2))
final_data_filtered$colubridaeGP <-factor(final_data_filtered$colubridaeGP, 
                                          levels = c(1, 2))
final_data_filtered$pythonidaeGP <-factor(final_data_filtered$pythonidaeGP, 
                                          levels = c(1, 2))
final_data_filtered$SnakesGP <-factor(final_data_filtered$SnakesGP, 
                                      levels = c(1, 2, 3, 4))
summary(final_data_filtered$carnivoraGP)
### 1. 다중공선성만 제외한 포화모형. 
hendra_rev_1 <- glm(hendra_gp ~ strigiformesGP + 
                      pythonidaeGP + carnivoraGP + 
                      count_chiroptera_prop25 + hfp_average + elevation + horse + BFF_predict + landcover_9_average + landcover_10_average
                    , 
                    data = final_data_filtered, family = binomial(link = 'logit'), na.action = na.exclude)

# snakes는 다중공선성 때문에 제외
hendra_rev_1 <- glm(hendra_gp ~ RaptorsGP +  carnivoraGP + colubridaeGP +
                      count_chiroptera_prop25 + hfp_average + elevation + horse + BFF_predict + landcover_9_average + landcover_10_average
                    , 
                    data = final_data_filtered, family = binomial(link = 'logit'), na.action = na.exclude)

summary(hendra_rev_1)
round(exp(hendra_rev_1$coefficients), 2) 
round(exp(confint(hendra_rev_1)), 2)

#오차의 독립성 가정을 평가하는 더빈-왓슨 검정.
durbinWatsonTest(hendra_rev_1) #자기상관성이 없음.

#다중공선성 평가를 위한 VIF(분산팽창인자) 통계량. 10보다 작아야 한다.
vif(hendra_rev_1)
#VIF의 역수로 tolerence를 구한다. 0.2보다 커야 한다.
1/vif(hendra_rev_1)
#평균 VIF를 구한다. 1에서 멀면 좋지 않다.
mean(vif(hendra_rev_1))

glmPseudoR2s <- function(LogModel) {
  dev <- LogModel$deviance 
  nullDev <- LogModel$null.deviance 
  modelN <-  length(LogModel$fitted.values)
  R.l <-  1 -  dev / nullDev
  R.cs <- 1- exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2  ", round(R.l, 3), "\n")
  cat("Cox and Snell R^2        ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2           ", round(R.n, 3),    "\n")
}
glmPseudoR2s(hendra_rev_1)













####################################################################################
#### prop 75
#####################################################################################



library(corrplot)

whole <- final_data_filtered %>% 
  dplyr::select(hfp_average, elevation, precipitation_average, temperature_average, GDP_average, 
                HDI_average, forest_cover_average, pop_dense_average, horse, landcover_2_average,
                landcover_4_average, landcover_5_average, landcover_6_average, landcover_7_average,
                landcover_8_average, landcover_9_average, landcover_10_average, landcover_12_average,
                landcover_13_average, landcover_14_average,BFF_predict, SFF_predict,
                count_accipitriformes_prop75, count_strigiformes_prop75, count_falconiformes_prop75,
                count_chiroptera_prop75, count_colubridae_prop75, count_pythonidae_prop75, 
                count_carnivora_prop75) 

mwhole <- cor(whole, use="pairwise.complete.obs")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mwhole, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
         col = col(10), addCoef.col = "black", cl.pos = "n", order = "AOE", type = {"lower"})


## GDP, HDI, landcover 2, 7, 8, 13, forest cover, precipitation 제외
whole <- final_data_filtered %>% 
  dplyr::select(hfp_average, elevation, temperature_average,
                pop_dense_average, horse, BFF_predict, SFF_predict,
                landcover_4_average, landcover_5_average, landcover_6_average, 
                landcover_9_average, landcover_10_average, landcover_12_average,
                landcover_14_average,
                count_accipitriformes_prop75, count_strigiformes_prop75, count_falconiformes_prop75,
                count_chiroptera_prop75, count_colubridae_prop75, count_pythonidae_prop75, 
                count_carnivora_prop75) 

mwhole <- cor(whole, use="pairwise.complete.obs")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mwhole, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
         col = col(10), addCoef.col = "black", cl.pos = "n", order = "AOE", type = {"lower"})

## temperature, chiroptera, colubridae, pop_dense, landcover 6 추가로 제외 Univariable에서 유의하지 않은 것들.
whole <- final_data_filtered %>% 
  dplyr::select(hfp_average, elevation, horse, BFF_predict, SFF_predict,  landcover_9_average, landcover_10_average,
                count_strigiformes_prop75, count_falconiformes_prop75,
                count_colubridae_prop75, count_pythonidae_prop75, 
                count_carnivora_prop75)


mwhole <- cor(whole, use="pairwise.complete.obs")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mwhole, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
         col = col(10), addCoef.col = "black", cl.pos = "n", order = "AOE", type = {"lower"})


##### 여기서 지금 문제는, accipitriformes랑 strigiformes가 상관성이 너무 크게 나온다는 건데, 
### 일단은 결과가 유의하였을 때 해석이 좀 더 쉬운 strigiformes로 한 번 해보기.
## 혹은, 미팅 발표에서, 맹금류로 합치거나 하는 방안을 제시해보기. Raptors

## Raptors 만들어서 해보기.
final_data_filtered <- final_data_filtered %>% 
  mutate(Raptors = count_accipitriformes_prop75 + count_strigiformes_prop75 + count_falconiformes_prop75)

## Snakes 만들어서 해보기.
final_data_filtered <- final_data_filtered %>% 
  mutate(Snakes = count_colubridae_prop75 + count_pythonidae_prop75)

####### categorical analysis로 진행해보기.
final_data_filtered %>% summary()

#accipitriformes - 9.00/11.00/13.00
#strigiformes - 4.00/5.00/7.00
#falconiformes - 5랑 6 밖에 없음
#carnivora - 2.00 있고 없고로.
#colubridae - 2.00 있고 없고로.
#pythonidae - 2.00 있고 없고로.

#Raptors - 18.00/21.00/24.00
#Snakes - 3.00/5.00/7.00

final_data_filtered <- final_data_filtered %>% 
  mutate(accipitriformesGP = ifelse(count_accipitriformes_prop75 < 9, 1, 
                                    ifelse(count_accipitriformes_prop75 < 11, 2,
                                           ifelse(count_accipitriformes_prop75 < 13, 3,4))))

final_data_filtered <- final_data_filtered %>% 
  mutate(strigiformesGP = ifelse(count_strigiformes_prop75 < 4, 1, 
                                 ifelse(count_strigiformes_prop75 < 5, 2,
                                        ifelse(count_strigiformes_prop75 < 7, 3,4))))

final_data_filtered <- final_data_filtered %>% 
  mutate(RaptorsGP = ifelse(Raptors < 18, 1, 
                            ifelse(Raptors  < 21, 2,
                                   ifelse(Raptors  < 24, 3,4))))

final_data_filtered <- final_data_filtered %>% 
  mutate(carnivoraGP = ifelse(count_carnivora_prop75 < 2, 1, 2))

final_data_filtered <- final_data_filtered %>% 
  mutate(colubridaeGP = ifelse(count_colubridae_prop75 < 2, 1, 2))

final_data_filtered <- final_data_filtered %>% 
  mutate(pythonidaeGP = ifelse(count_pythonidae_prop75 < 2, 1, 2))

final_data_filtered <- final_data_filtered %>% 
  mutate(SnakesGP = ifelse(Snakes < 3, 1, 
                           ifelse(Snakes  < 5, 2,
                                  ifelse(Snakes  < 7, 3,4))))


final_data_filtered$accipitriformesGP <-factor(final_data_filtered$accipitriformesGP, 
                                               levels = c(1, 2, 3, 4))
final_data_filtered$strigiformesGP <-factor(final_data_filtered$strigiformesGP, 
                                            levels = c(1, 2, 3, 4))
final_data_filtered$RaptorsGP <-factor(final_data_filtered$RaptorsGP, 
                                       levels = c(1, 2, 3, 4))
final_data_filtered$carnivoraGP <-factor(final_data_filtered$carnivoraGP, 
                                         levels = c(1, 2))
final_data_filtered$colubridaeGP <-factor(final_data_filtered$colubridaeGP, 
                                          levels = c(1, 2))
final_data_filtered$pythonidaeGP <-factor(final_data_filtered$pythonidaeGP, 
                                          levels = c(1, 2))
final_data_filtered$SnakesGP <-factor(final_data_filtered$SnakesGP, 
                                      levels = c(1, 2, 3, 4))
summary(final_data_filtered$RaptorsGP)
### 1. 다중공선성만 제외한 포화모형. 
hendra_rev_1 <- glm(hendra_gp ~ strigiformesGP + 
                      pythonidaeGP + carnivoraGP + colubridaeGP + 
                      count_chiroptera_prop75 + hfp_average + elevation + horse + BFF_predict + landcover_9_average + landcover_10_average
                    , 
                    data = final_data_filtered, family = binomial(link = 'logit'), na.action = na.exclude)

# snakes는 다중공선성 때문에 제외
hendra_rev_1 <- glm(hendra_gp ~ RaptorsGP +  carnivoraGP + colubridaeGP +
                      count_chiroptera_prop75 + hfp_average + elevation + horse + BFF_predict + landcover_9_average + landcover_10_average
                    , 
                    data = final_data_filtered, family = binomial(link = 'logit'), na.action = na.exclude)

summary(hendra_rev_1)
round(exp(hendra_rev_1$coefficients), 2) 
round(exp(confint(hendra_rev_1)), 2)
round(exp(1.70), 2)

#오차의 독립성 가정을 평가하는 더빈-왓슨 검정.
durbinWatsonTest(hendra_rev_1) #자기상관성이 없음.

#다중공선성 평가를 위한 VIF(분산팽창인자) 통계량. 10보다 작아야 한다.
vif(hendra_rev_1)
#VIF의 역수로 tolerence를 구한다. 0.2보다 커야 한다.
1/vif(hendra_rev_1)
#평균 VIF를 구한다. 1에서 멀면 좋지 않다.
mean(vif(hendra_rev_1))

glmPseudoR2s <- function(LogModel) {
  dev <- LogModel$deviance 
  nullDev <- LogModel$null.deviance 
  modelN <-  length(LogModel$fitted.values)
  R.l <-  1 -  dev / nullDev
  R.cs <- 1- exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2  ", round(R.l, 3), "\n")
  cat("Cox and Snell R^2        ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2           ", round(R.n, 3),    "\n")
}
glmPseudoR2s(hendra_rev_1)









########################################################################################
###################### Deforestation


library(corrplot)

whole <- final_data_filtered %>% 
  dplyr::select(hfp_average, elevation, precipitation_average, temperature_average, GDP_average, 
                HDI_average, forest_cover_average, pop_dense_average, horse, landcover_2_average,
                landcover_4_average, landcover_5_average, landcover_6_average, landcover_7_average,
                landcover_8_average, landcover_9_average, landcover_10_average, landcover_12_average,
                landcover_13_average, landcover_14_average, deforestation) 

mwhole <- cor(whole, use="pairwise.complete.obs")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mwhole, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
         col = col(10), addCoef.col = "black", cl.pos = "n", order = "AOE", type = {"lower"})


## HDI, landcover 2, 7, 8, 13, 제외
whole <- final_data_filtered %>% 
  dplyr::select(hfp_average, elevation, precipitation_average, temperature_average, GDP_average, 
                forest_cover_average, pop_dense_average, horse, 
                landcover_4_average, landcover_5_average, landcover_6_average, 
                 landcover_9_average, landcover_10_average, landcover_12_average,
                 landcover_14_average, deforestation) 

mwhole <- cor(whole, use="pairwise.complete.obs")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mwhole, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
         col = col(10), addCoef.col = "black", cl.pos = "n", order = "AOE", type = {"lower"})

## GDP, pop_dense, landcover 6 추가로 제외 Univariable에서 유의하지 않은 것들.
whole <- final_data_filtered %>% 
  dplyr::select(hfp_average, elevation, precipitation_average, temperature_average, 
                forest_cover_average,  horse,
                landcover_9_average, landcover_10_average, landcover_12_average,
                 deforestation)


mwhole <- cor(whole, use="pairwise.complete.obs")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mwhole, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
         col = col(10), addCoef.col = "black", cl.pos = "n", order = "AOE", type = {"lower"})


##### Deforestation
summary(final_data_filtered)

final_data_filtered <- final_data_filtered %>% 
  mutate(deforestationGP = ifelse(deforestation < 1, 1, 
                                    ifelse(deforestation < 5, 2,
                                           ifelse(deforestation < 15, 3, 4))))

final_data_filtered$deforestationGP <-factor(final_data_filtered$deforestationGP, 
                                      levels = c(1, 2, 3, 4))

summary(final_data_filtered$deforestationGP)

### 1. 다중공선성만 제외한 포화모형. 
hendra_rev_1 <- glm(hendra_gp ~ deforestationGP + forest_cover_average + hfp_average + elevation + precipitation_average + temperature_average + 
                    horse + landcover_9_average + landcover_10_average + landcover_12_average, 
                    data = final_data_filtered, family = binomial(link = 'logit'), na.action = na.exclude)

summary(hendra_rev_1)
round(exp(hendra_rev_1$coefficients), 2) 
round(exp(confint(hendra_rev_1)), 2)


#오차의 독립성 가정을 평가하는 더빈-왓슨 검정.
durbinWatsonTest(hendra_rev_1) #자기상관성이 없음.

#다중공선성 평가를 위한 VIF(분산팽창인자) 통계량. 10보다 작아야 한다.
vif(hendra_rev_1)
#VIF의 역수로 tolerence를 구한다. 0.2보다 커야 한다.
1/vif(hendra_rev_1)
#평균 VIF를 구한다. 1에서 멀면 좋지 않다.
mean(vif(hendra_rev_1))

glmPseudoR2s <- function(LogModel) {
  dev <- LogModel$deviance 
  nullDev <- LogModel$null.deviance 
  modelN <-  length(LogModel$fitted.values)
  R.l <-  1 -  dev / nullDev
  R.cs <- 1- exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2  ", round(R.l, 3), "\n")
  cat("Cox and Snell R^2        ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2           ", round(R.n, 3),    "\n")
}
glmPseudoR2s(hendra_rev_1)

