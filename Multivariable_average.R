########################### Average data / Multivariable analysis


library(car)
library(mlogit)
library(boot)
library(ggplot2)
library(tidyverse)
library(readr)

final_data <- read_csv("D:/Environmental data/Hendra-Australia/Final_dataset/final_dataset_total.csv", 
                       col_types = cols(...1 = col_skip(), X = col_skip()))

#######################################
### 예비분석.
#######################################

final_data <- final_data %>% 
  mutate(hendra_gp = ifelse(hendra >0, 1, 0))

final_data_filtered$hendra_gp <-factor(final_data_filtered$hendra_gp, levels = c(0, 1))


table(final_data$hendra_gp)


#############################################################################################
####### 먼저, hendra_gp가 1인 지점이 모두 포함되는 선에서 flying-fox의 최대 최소값 구해보고, 
####### 그 범위 안에 들어가는 grid만 골라서 분석.
#############################################################################################
min_max <- final_data %>% 
  dplyr::select(hendra_gp, flying_fox) %>% 
  filter(hendra_gp == 1) 
min <- min(min_max$flying_fox)  
max <- max(min_max$flying_fox)  
min; max ## 최소값은 4.460, 최대값은 9.589

### 원래 672 였는데 641로 감소.
final_data_minmax <- final_data %>% 
  filter(flying_fox >= min & flying_fox <= max)

#############################################################################################
####### 다음으로, hendra_gp가 1인 지점과 비슷한 값의 flying-fox가 있는 경우를 matching으로 잡아서  
####### 거기에 포함되는 control들만 뽑아서 분석.
#############################################################################################
library(MatchIt)
?matchit()

set.seed(102)
match <- matchit(hendra_gp ~ flying_fox, method = "nearest", data = final_data,
                 ratio = 10)
match$match.matrix

m <- c("61", "188", "370", "351", "86",  "368", "369", "372", "51",  "371", "207",
       "63",  "590", "555", "578", "11",  "332", "261", "276", "66",  "43",  "558",
       "64",  "121", "62",  "186", "221", "241", "9",   "148", "240", "657", "354",
       "67",  "112", "643", "203", "97",  "176", "4",   "30",  "196", "99",  "3",
       "72",  "620", "85",  "78",  "94",  "59",  "618", "105", "83",  "84",  "77",
       "244", "65",  "161", "22",  "355", "239", "185", "658", "71",  "232", "107",
       "281", "439", "614" ,"53",  "603", "428", "361", "29" , "440", "571","438",
       "287", "38",  "37",  "390", "666", "283", "564", "317", "507", "382", "363",
       "289", "343", "334", "39" , "610", "420", "13" , "325", "651", "279" ,"672",
       "290", "518", "15" , "383", "381", "362", "577","385" ,"429", "284" ,"14",
       "291", "384", "430", "326", "45",  "27",  "523" ,"667", "552", "446", "410",
       "467", "138", "648", "163", "150", "646" ,"19" , "373", "202", "40",  "189",
       "582", "491" ,"473" ,"280" ,"457", "452", "346" ,"292" ,"418" ,"316", "592",
       "589", "119" ,"357", "556", "24" , "263", "500", "557", "569", "669", "460",
       "591", "469", "396", "392", "365","427", "388", "445", "570", "386", "436",
       "606", "328", "607", "423", "602","504", "319", "550", "434", "594", "404",
       "612", "413", "405", "549", "601", "505", "406", "487", "433", "310" ,"424",
       "613", "443" ,"442", "522", "409", "471" ,"519", "538", "407" ,"593", "422",
       "615", "342", "35",  "271", "458", "604", "539" ,"333", "468", "285", "46" )

final_data_match <- final_data[m,]



#####################################################
####### 위쪽에서 minmax/matching으로 분석해본 것이 잘 안되어서, 
####### 새로 해보려고 하는것이, flying-fox들이 보고된 적이 있는 grid만 분리해서 해보려고.
#####################################################
BFF <- read.csv("C:\\Users\\Taehee\\OneDrive\\바탕 화면\\My papers\\Hendravirus_작업중\\flying-fox_distribution\\교차영역\\BFF_교차영역.csv")
GHFF <- read.csv("C:\\Users\\Taehee\\OneDrive\\바탕 화면\\My papers\\Hendravirus_작업중\\flying-fox_distribution\\교차영역\\GHFF_교차영역.csv")
LRFF <- read.csv("C:\\Users\\Taehee\\OneDrive\\바탕 화면\\My papers\\Hendravirus_작업중\\flying-fox_distribution\\교차영역\\LRFF_교차영역.csv")
SFF <- read.csv("C:\\Users\\Taehee\\OneDrive\\바탕 화면\\My papers\\Hendravirus_작업중\\flying-fox_distribution\\교차영역\\SFF_교차영역.csv")

BFF_filtered <- BFF %>% 
  dplyr::select(Species, average_BF, id) %>% 
  filter(average_BF > 0) %>% 
  select(id)
SFF_filtered <- SFF %>% 
  dplyr::select(Species, average_SF, id) %>% 
  filter(average_SF > 0)%>% 
  select(id)
GHFF_filtered <- GHFF %>% 
  dplyr::select(Species, average_GH, id) %>% 
  filter(average_GH > 0) %>% 
  select(id)
LRFF_filtered <- LRFF %>% 
  dplyr::select(Species, average_LR, id) %>% 
  filter(average_LR > 0) %>% 
  select(id)

positive <- final_data %>% 
  filter(hendra_gp == 1) %>% 
  dplyr::select(ID) %>% 
  rename(id = ID)

total <- rbind(BFF_filtered, SFF_filtered, GHFF_filtered, LRFF_filtered, positive)
total <- unique(total$id)

final_data_filtered <- final_data %>% 
  filter(ID %in% total)

table(final_data_filtered$hendra_gp) # 19/99





#####################################################################
################ MaxMin data
#####################################################################

library(corrplot)

whole <- final_data_minmax %>% 
  dplyr::select(hfp_average, elevation, precipitation_average, temperature_average, GDP_average, 
                HDI_average, forest_cover_average, pop_dense_average, horse, landcover_2_average,
                landcover_4_average, landcover_5_average, landcover_6_average, landcover_7_average,
                landcover_8_average, landcover_9_average, landcover_10_average, landcover_12_average,
                landcover_13_average, landcover_14_average,
                chiroptera_Maxent, accipitriformes_Maxent, strigiformes_Maxent, carnivora_Maxent, 
                colubridae_Maxent) 

mwhole <- cor(whole, use="pairwise.complete.obs")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mwhole, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
         col = col(10), addCoef.col = "black", cl.pos = "n", order = "AOE", type = {"lower"})


## GDP, HDI, carnivora, chiroptera 제외
whole <- final_data_minmax %>% 
  dplyr::select(hfp_average, elevation, precipitation_average, temperature_average,  
                forest_cover_average, pop_dense_average, horse, landcover_2_average,
                landcover_4_average, landcover_5_average, landcover_6_average, landcover_7_average,
                landcover_8_average, landcover_9_average, landcover_10_average, landcover_12_average,
                landcover_13_average, landcover_14_average,
                accipitriformes_Maxent, strigiformes_Maxent,
                colubridae_Maxent) 

mwhole <- cor(whole, use="pairwise.complete.obs")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mwhole, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
         col = col(10), addCoef.col = "black", cl.pos = "n", order = "AOE", type = {"lower"})

## landcover 2, 7, 8, 13 추가로 제외 Univariable에서 유의하지 않은 것들.
whole <- final_data_minmax %>% 
  dplyr::select(hfp_average, elevation, precipitation_average, temperature_average,  
                forest_cover_average, pop_dense_average, horse, 
                landcover_4_average, landcover_5_average, landcover_6_average, 
                landcover_9_average, landcover_10_average, landcover_12_average,
                landcover_14_average,
                accipitriformes_Maxent, strigiformes_Maxent,
                colubridae_Maxent) 

mwhole <- cor(whole, use="pairwise.complete.obs")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mwhole, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
         col = col(10), addCoef.col = "black", cl.pos = "n", order = "AOE", type = {"lower"})


##### 여기서 지금 문제는, accipitriformes랑 strigiformes가 상관성이 너무 크게 나온다는 건데, 
### 일단은 결과가 유의하였을 때 해석이 좀 더 쉬운 strigiformes로 한 번 해보기.
## 혹은, 미팅 발표에서, 맹금류로 합치거나 하는 방안을 제시해보기. Raptors

## Raptors 만들어서 해보기.
### Raptors로 합쳐도 비슷한 결과가 나온다. 
final_data_minmax <- final_data_minmax %>% 
  mutate(Raptors = strigiformes_Maxent + accipitriformes_Maxent)


####### categorical analysis로 진행해보기.
final_data_minmax %>% summary()

#accipitriformes - 5.00/9.00/11.00
#strigiformes - 1.00/3.00/5.00
#carnivora - 1.00 하나라도 있고 없고로.
#colubridae - 2.00
#Raptors - 6.00/12.00/18.00

final_data_minmax <- final_data_minmax %>% 
  mutate(accipitriformesGP = ifelse(accipitriformes_Maxent < 5, 1, 
                                    ifelse(accipitriformes_Maxent < 9, 2,
                                           ifelse(accipitriformes_Maxent < 11, 3,4))))

final_data_minmax <- final_data_minmax %>% 
  mutate(strigiformesGP = ifelse(strigiformes_Maxent < 1, 1, 
                                 ifelse(strigiformes_Maxent < 3, 2,
                                        ifelse(strigiformes_Maxent < 5, 3,4))))
final_data_minmax <- final_data_minmax %>% 
  mutate(RaptorsGP = ifelse(Raptors < 6, 1, 
                                 ifelse(Raptors < 12, 2,
                                        ifelse(Raptors < 18, 3,4))))

final_data_minmax <- final_data_minmax %>% 
  mutate(carnivoraGP = ifelse(carnivora_Maxent < 1, 1, 2))

final_data_minmax <- final_data_minmax %>% 
  mutate(colubridaeGP = ifelse(colubridae_Maxent < 2, 1, 2))

final_data_minmax$accipitriformesGP <-factor(final_data_minmax$accipitriformesGP, 
                                       levels = c(1, 2, 3, 4))
final_data_minmax$strigiformesGP <-factor(final_data_minmax$strigiformesGP, 
                                    levels = c(1, 2, 3, 4))
final_data_minmax$RaptorsGP <-factor(final_data_minmax$RaptorsGP, 
                                          levels = c(1, 2, 3, 4))
final_data_minmax$carnivoraGP <-factor(final_data_minmax$carnivoraGP, 
                                 levels = c(1, 2))
final_data_minmax$colubridaeGP <-factor(final_data_minmax$colubridaeGP, 
                                  levels = c(1, 2))


### 1. 다중공선성만 제외한 포화모형. 
hendra_rev_1 <- glm(hendra_gp ~ strigiformesGP + colubridaeGP + hfp_average + elevation + 
                    forest_cover_average + pop_dense_average +
                    horse + landcover_4_average + landcover_5_average + landcover_6_average +
                    landcover_9_average + landcover_10_average + landcover_12_average +
                    landcover_14_average 
                   , 
                   data = final_data_minmax, family = binomial(link = 'logit'), na.action = na.exclude)
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







#####################################################################
################ Matching data
#####################################################################

library(corrplot)

whole <- final_data_match %>% 
  dplyr::select(hfp_average, elevation, precipitation_average, temperature_average, GDP_average, 
                HDI_average, forest_cover_average, pop_dense_average, horse, landcover_2_average,
                landcover_4_average, landcover_5_average, landcover_6_average, landcover_7_average,
                landcover_8_average, landcover_9_average, landcover_10_average, landcover_12_average,
                landcover_13_average, landcover_14_average,
                chiroptera_Maxent, accipitriformes_Maxent, strigiformes_Maxent, carnivora_Maxent, 
                colubridae_Maxent) 

mwhole <- cor(whole, use="pairwise.complete.obs")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mwhole, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
         col = col(10), addCoef.col = "black", cl.pos = "n", order = "AOE", type = {"lower"})


## GDP, HDI, carnivora, chiroptera 제외
whole <- final_data_match %>% 
  dplyr::select(hfp_average, elevation, precipitation_average, temperature_average,  
                forest_cover_average, pop_dense_average, horse, landcover_2_average,
                landcover_4_average, landcover_5_average, landcover_6_average, landcover_7_average,
                landcover_8_average, landcover_9_average, landcover_10_average, landcover_12_average,
                landcover_13_average, landcover_14_average,
                accipitriformes_Maxent, strigiformes_Maxent,
                colubridae_Maxent) 

mwhole <- cor(whole, use="pairwise.complete.obs")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mwhole, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
         col = col(10), addCoef.col = "black", cl.pos = "n", order = "AOE", type = {"lower"})

## landcover 2, 7, 8, 13 추가로 제외 Univariable에서 유의하지 않은 것들.
whole <- final_data_match %>% 
  dplyr::select(hfp_average, elevation, precipitation_average, temperature_average,  
                forest_cover_average, pop_dense_average, horse, 
                landcover_4_average, landcover_5_average, landcover_6_average, 
                landcover_9_average, landcover_10_average, landcover_12_average,
                landcover_14_average,
                accipitriformes_Maxent, strigiformes_Maxent,
                colubridae_Maxent) 

mwhole <- cor(whole, use="pairwise.complete.obs")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mwhole, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
         col = col(10), addCoef.col = "black", cl.pos = "n", order = "AOE", type = {"lower"})


##### 여기서 지금 문제는, accipitriformes랑 strigiformes가 상관성이 너무 크게 나온다는 건데, 
### 일단은 결과가 유의하였을 때 해석이 좀 더 쉬운 strigiformes로 한 번 해보기.
## 혹은, 미팅 발표에서, 맹금류로 합치거나 하는 방안을 제시해보기. Raptors

## Raptors 만들어서 해보기.
### Raptors로 합쳐도 비슷한 결과가 나온다. 
final_data_match <- final_data_match %>% 
  mutate(Raptors = strigiformes_Maxent + accipitriformes_Maxent)


####### categorical analysis로 진행해보기.
final_data_match %>% summary()

#accipitriformes - 5.00/9.00/12.00
#strigiformes - 1.00/3.00/5.00
#carnivora - 1.00 하나라도 있고 없고로.
#colubridae - 2.00
#Raptors - 6.00/12.00/18.00

final_data_match <- final_data_match %>% 
  mutate(accipitriformesGP = ifelse(accipitriformes_Maxent < 5, 1, 
                                    ifelse(accipitriformes_Maxent < 9, 2,
                                           ifelse(accipitriformes_Maxent < 12, 3,4))))

final_data_match <- final_data_match %>% 
  mutate(strigiformesGP = ifelse(strigiformes_Maxent < 1, 1, 
                                 ifelse(strigiformes_Maxent < 3, 2,
                                        ifelse(strigiformes_Maxent < 5, 3,4))))
final_data_match <- final_data_match %>% 
  mutate(RaptorsGP = ifelse(Raptors < 6, 1, 
                            ifelse(Raptors < 12, 2,
                                   ifelse(Raptors < 18, 3,4))))

final_data_match <- final_data_match %>% 
  mutate(carnivoraGP = ifelse(carnivora_Maxent < 1, 1, 2))

final_data_match <- final_data_match %>% 
  mutate(colubridaeGP = ifelse(colubridae_Maxent < 2, 1, 2))

final_data_match$accipitriformesGP <-factor(final_data_match$accipitriformesGP, 
                                             levels = c(1, 2, 3, 4))
final_data_match$strigiformesGP <-factor(final_data_match$strigiformesGP, 
                                          levels = c(1, 2, 3, 4))
final_data_match$RaptorsGP <-factor(final_data_match$RaptorsGP, 
                                     levels = c(1, 2, 3, 4))
final_data_match$carnivoraGP <-factor(final_data_match$carnivoraGP, 
                                       levels = c(1, 2))
final_data_match$colubridaeGP <-factor(final_data_match$colubridaeGP, 
                                        levels = c(1, 2))


### 1. 다중공선성만 제외한 포화모형. 
hendra_rev_1 <- glm(hendra_gp ~ RaptorsGP + colubridaeGP + hfp_average + elevation + 
                      forest_cover_average + pop_dense_average +
                      horse + landcover_4_average + landcover_6_average +
                      landcover_9_average + landcover_10_average + landcover_12_average +
                      landcover_14_average 
                    , 
                    data = final_data_match, family = binomial(link = 'logit'), na.action = na.exclude)
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







#####################################################################
################ Falying-fox 발견된 곳만 Filtered data / 20230307 이후로 flying-fox occurrence level을 포함하는 모델로 변경경
#####################################################################
final_data_filtered <- read.csv("D:\\Environmental data\\Hendra-Australia\\Final_dataset\\Updated_20230306\\final_data_filtered.csv")
## Flying-fox distribution covariate로 포함하기 위한 준비과정.
# P. alecto / P. conspicillatus BFF/SFF 포함.
final_data_filtered <- final_data_filtered %>% 
  mutate(BFF_predict = log(BFF_predict),
         SFF_predict = log(SFF_predict),
         GHFF_predict = log(GHFF_predict),
         LRFF_predict = log(LRFF_predict))

library(corrplot)

whole <- final_data_filtered %>% 
  dplyr::select(hfp_average, elevation, precipitation_average, temperature_average, GDP_average, 
                HDI_average, forest_cover_average, pop_dense_average, horse, landcover_2_average,
                landcover_4_average, landcover_5_average, landcover_6_average, landcover_7_average,
                landcover_8_average, landcover_9_average, landcover_10_average, landcover_12_average,
                landcover_13_average, landcover_14_average,BFF_predict, SFF_predict,
                chiroptera_Maxent, accipitriformes_Maxent, strigiformes_Maxent, carnivora_Maxent, 
                colubridae_Maxent) 

mwhole <- cor(whole, use="pairwise.complete.obs")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mwhole, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
         col = col(10), addCoef.col = "black", cl.pos = "n", order = "AOE", type = {"lower"})


## GDP, HDI, landcover 2, 7, 8, 13, forest cover 제외
whole <- final_data_filtered %>% 
  dplyr::select(hfp_average, elevation, precipitation_average, temperature_average, chiroptera_Maxent, 
                pop_dense_average, horse, BFF_predict, SFF_predict,
                landcover_4_average, landcover_5_average, landcover_6_average, 
                landcover_9_average, landcover_10_average, landcover_12_average,
                landcover_14_average,
                accipitriformes_Maxent, strigiformes_Maxent,
                colubridae_Maxent) 

mwhole <- cor(whole, use="pairwise.complete.obs")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mwhole, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
         col = col(10), addCoef.col = "black", cl.pos = "n", order = "AOE", type = {"lower"})

## temperature, chiroptera, colubridae, pop_dense, landcover 6 추가로 제외 Univariable에서 유의하지 않은 것들.
whole <- final_data_filtered %>% 
  dplyr::select(hfp_average, elevation, carnivora_Maxent, precipitation_average,
                horse, 
                landcover_4_average, landcover_5_average, 
                landcover_9_average, landcover_10_average, landcover_12_average,
                landcover_14_average, BFF_predict, SFF_predict,
                accipitriformes_Maxent, strigiformes_Maxent)


mwhole <- cor(whole, use="pairwise.complete.obs")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mwhole, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
         col = col(10), addCoef.col = "black", cl.pos = "n", order = "AOE", type = {"lower"})


##### 여기서 지금 문제는, accipitriformes랑 strigiformes가 상관성이 너무 크게 나온다는 건데, 
### 일단은 결과가 유의하였을 때 해석이 좀 더 쉬운 strigiformes로 한 번 해보기.
## 혹은, 미팅 발표에서, 맹금류로 합치거나 하는 방안을 제시해보기. Raptors

## Raptors 만들어서 해보기.
### Raptors로 합쳐도 비슷한 결과가 나온다. 
final_data_filtered <- final_data_filtered %>% 
  mutate(Raptors = strigiformes_Maxent + accipitriformes_Maxent)


####### categorical analysis로 진행해보기.
final_data_filtered %>% summary()

#accipitriformes - 11.00/13.00/15.00
#strigiformes - 3.00/5.00/7.00
#carnivora - 1.00 하나라도 있고 없고로.
#colubridae - 1.00
#Raptors - 14.00/18.00/22.00

final_data_filtered <- final_data_filtered %>% 
  mutate(accipitriformesGP = ifelse(accipitriformes_Maxent < 11, 1, 
                                    ifelse(accipitriformes_Maxent < 13, 2,
                                           ifelse(accipitriformes_Maxent < 15, 3,4))))

final_data_filtered <- final_data_filtered %>% 
  mutate(strigiformesGP = ifelse(strigiformes_Maxent < 3, 1, 
                                 ifelse(strigiformes_Maxent < 5, 2,
                                        ifelse(strigiformes_Maxent < 7, 3,4))))
final_data_filtered <- final_data_filtered %>% 
  mutate(RaptorsGP = ifelse(Raptors < 14, 1, 
                            ifelse(Raptors < 18, 2,
                                   ifelse(Raptors < 22, 3,4))))

final_data_filtered <- final_data_filtered %>% 
  mutate(carnivoraGP = ifelse(carnivora_Maxent < 1, 1, 2))

final_data_filtered <- final_data_filtered %>% 
  mutate(colubridaeGP = ifelse(colubridae_Maxent < 1, 1, 2))

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


### 1. 다중공선성만 제외한 포화모형. 
hendra_rev_1 <- glm(hendra_gp ~ chiroptera_Maxent + carnivoraGP + hfp_average + elevation + 
                       BFF_predict + 
                      horse + landcover_4_average + 
                      landcover_9_average + landcover_10_average + landcover_12_average +
                      landcover_14_average 
                    , 
                    data = final_data_filtered, family = binomial(link = 'logit'), na.action = na.exclude)

hendra_rev_1 <- glm(hendra_gp ~ strigiformesGP + colubridaeGP + hfp_average + elevation + 
                       BFF_predict + SFF_predict +
                      horse + landcover_4_average + 
                      landcover_9_average + landcover_10_average + landcover_12_average +
                      landcover_14_average 
                    , 
                    data = final_data_filtered, family = binomial(link = 'logit'), na.action = na.exclude)

hendra_rev_1 <- glm(hendra_gp ~ RaptorsGP + colubridaeGP + hfp_average + elevation + 
                       BFF_predict + SFF_predict +
                      horse + landcover_4_average + 
                      landcover_9_average + landcover_10_average + landcover_12_average +
                      landcover_14_average 
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
#### IUCN
#####################################################################################

library(corrplot)

whole <- final_data_filtered %>% 
  dplyr::select(hfp_average, elevation, precipitation_average, temperature_average, GDP_average, 
                HDI_average, forest_cover_average, pop_dense_average, horse, landcover_2_average,
                landcover_4_average, landcover_5_average, landcover_6_average, landcover_7_average,
                landcover_8_average, landcover_9_average, landcover_10_average, landcover_12_average,
                landcover_13_average, landcover_14_average,BFF_predict, SFF_predict,
                chiroptera_IUCN, accipitriformes_IUCN, strigiformes_IUCN, 
                colubridae_IUCN) 

mwhole <- cor(whole, use="pairwise.complete.obs")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mwhole, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
         col = col(10), addCoef.col = "black", cl.pos = "n", order = "AOE", type = {"lower"})


## GDP, HDI, landcover 2, 7, 8, 13, forest cover 제외
whole <- final_data_filtered %>% 
  dplyr::select(hfp_average, elevation, precipitation_average, temperature_average, chiroptera_Maxent, 
                pop_dense_average, horse, BFF_predict, SFF_predict,
                landcover_4_average, landcover_5_average, landcover_6_average, 
                landcover_9_average, landcover_10_average, landcover_12_average,
                landcover_14_average,
                chiroptera_IUCN, accipitriformes_IUCN, strigiformes_IUCN, 
                colubridae_IUCN) 

mwhole <- cor(whole, use="pairwise.complete.obs")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mwhole, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
         col = col(10), addCoef.col = "black", cl.pos = "n", order = "AOE", type = {"lower"})

## pop_dense, landcover 6 추가로 제외 Univariable에서 유의하지 않은 것들.
whole <- final_data_filtered %>% 
  dplyr::select(hfp_average, elevation, precipitation_average, temperature_average, chiroptera_Maxent, 
                horse, BFF_predict, SFF_predict,
                landcover_4_average, landcover_5_average, 
                landcover_9_average, landcover_10_average, landcover_12_average,
                landcover_14_average,
                chiroptera_IUCN, accipitriformes_IUCN, strigiformes_IUCN, 
                colubridae_IUCN) 


mwhole <- cor(whole, use="pairwise.complete.obs")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mwhole, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
         col = col(10), addCoef.col = "black", cl.pos = "n", order = "AOE", type = {"lower"})


##### 여기서 지금 문제는, accipitriformes랑 strigiformes가 상관성이 너무 크게 나온다는 건데, 
### 일단은 결과가 유의하였을 때 해석이 좀 더 쉬운 strigiformes로 한 번 해보기.
## 혹은, 미팅 발표에서, 맹금류로 합치거나 하는 방안을 제시해보기. Raptors



####### categorical analysis로 진행해보기.
final_data_filtered %>% summary()

#accipitriformes - 3.00/5.00/7.00
#strigiformes - 3.00/5.00/7.00
#colubridae - 1.00

final_data_filtered <- final_data_filtered %>% 
  mutate(accipitriformesGP = ifelse(accipitriformes_IUCN < 3, 1, 
                                    ifelse(accipitriformes_IUCN < 5, 2,
                                           ifelse(accipitriformes_IUCN < 7, 3,4))))

final_data_filtered <- final_data_filtered %>% 
  mutate(strigiformesGP = ifelse(strigiformes_IUCN < 3, 1, 
                                 ifelse(strigiformes_IUCN < 5, 2,
                                        ifelse(strigiformes_IUCN < 7, 3,4))))

final_data_filtered <- final_data_filtered %>% 
  mutate(colubridaeGP = ifelse(colubridae_IUCN < 1, 1, 2))

final_data_filtered$accipitriformesGP <-factor(final_data_filtered$accipitriformesGP, 
                                               levels = c(1, 2, 3, 4))
final_data_filtered$strigiformesGP <-factor(final_data_filtered$strigiformesGP, 
                                            levels = c(1, 2, 3, 4))
final_data_filtered$colubridaeGP <-factor(final_data_filtered$colubridaeGP, 
                                          levels = c(1, 2))


### 1. 다중공선성만 제외한 포화모형. 
hendra_rev_1 <- glm(hendra_gp ~ chiroptera_IUCN + strigiformesGP + colubridaeGP + 
                      hfp_average + elevation + BFF_predict + SFF_predict + 
                      horse + landcover_4_average + 
                      landcover_9_average + landcover_10_average + landcover_12_average +
                      landcover_14_average 
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
#### Maxent existence
#####################################################################################

library(corrplot)

whole <- final_data_filtered %>% 
  dplyr::select(hfp_average, elevation, precipitation_average, temperature_average, GDP_average, 
                HDI_average, forest_cover_average, pop_dense_average, horse, landcover_2_average,
                landcover_4_average, landcover_5_average, landcover_6_average, landcover_7_average,
                landcover_8_average, landcover_9_average, landcover_10_average, landcover_12_average,
                landcover_13_average, landcover_14_average, BFF_predict, SFF_predict,
                chiroptera_maxent_existence, accipitriformes_maxent_existence, strigiformes_maxent_existence, 
                colubridae_maxent_existence) 

mwhole <- cor(whole, use="pairwise.complete.obs")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mwhole, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
         col = col(10), addCoef.col = "black", cl.pos = "n", order = "AOE", type = {"lower"})


## GDP, HDI, landcover 2, 7, 8, 13, forest cover 제외
whole <- final_data_filtered %>% 
  dplyr::select(hfp_average, elevation, precipitation_average, temperature_average, 
                pop_dense_average, horse, BFF_predict, SFF_predict,
                landcover_4_average, landcover_5_average, landcover_6_average, 
                landcover_9_average, landcover_10_average, landcover_12_average,
                landcover_14_average,
                chiroptera_maxent_existence, accipitriformes_maxent_existence, strigiformes_maxent_existence, 
                colubridae_maxent_existence) 

mwhole <- cor(whole, use="pairwise.complete.obs")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mwhole, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
         col = col(10), addCoef.col = "black", cl.pos = "n", order = "AOE", type = {"lower"})

## pop_dense, landcover 6, chiroptera, colubridae, precipitation 추가로 제외 Univariable에서 유의하지 않은 것들.
whole <- final_data_filtered %>% 
  dplyr::select(hfp_average, elevation, temperature_average, 
                horse, BFF_predict, SFF_predict,
                landcover_4_average, landcover_5_average, 
                landcover_9_average, landcover_10_average, landcover_12_average,
                landcover_14_average,
                accipitriformes_maxent_existence, strigiformes_maxent_existence, colubridae_maxent_existence,
                carnivora_maxent_existence) 


mwhole <- cor(whole, use="pairwise.complete.obs")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mwhole, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
         col = col(10), addCoef.col = "black", cl.pos = "n", order = "AOE", type = {"lower"})


##### 여기서 지금 문제는, accipitriformes랑 strigiformes가 상관성이 너무 크게 나온다는 건데, 
### 일단은 결과가 유의하였을 때 해석이 좀 더 쉬운 strigiformes로 한 번 해보기.
## 혹은, 미팅 발표에서, 맹금류로 합치거나 하는 방안을 제시해보기. Raptors



####### categorical analysis로 진행해보기.
final_data_filtered %>% summary()


## Raptors 만들어서 해보기.
### Raptors로 합쳐도 비슷한 결과가 나온다. 
final_data_filtered <- final_data_filtered %>% 
  mutate(Raptors = strigiformes_maxent_existence + accipitriformes_maxent_existence)


#accipitriformes - 11.00/13.00/15.00
#strigiformes - 3.00/5.00/7.00
#carnivora - 1.00 하나라도 있고 없고로.
#colubridae - 1.00
#Raptors - 16.00/20.00/24.00

final_data_filtered <- final_data_filtered %>% 
  mutate(accipitriformesGP = ifelse(accipitriformes_maxent_existence < 11, 1, 
                                    ifelse(accipitriformes_maxent_existence < 13, 2,
                                           ifelse(accipitriformes_maxent_existence < 15, 3,4))))

final_data_filtered <- final_data_filtered %>% 
  mutate(strigiformesGP = ifelse(strigiformes_maxent_existence < 3, 1, 
                                 ifelse(strigiformes_maxent_existence < 5, 2,
                                        ifelse(strigiformes_maxent_existence < 7, 3,4))))
final_data_filtered <- final_data_filtered %>% 
  mutate(RaptorsGP = ifelse(Raptors < 16, 1, 
                            ifelse(Raptors < 20, 2,
                                   ifelse(Raptors < 24, 3,4))))

final_data_filtered <- final_data_filtered %>% 
  mutate(carnivoraGP = ifelse(carnivora_maxent_existence < 1, 1, 2))

final_data_filtered <- final_data_filtered %>% 
  mutate(colubridaeGP = ifelse(colubridae_maxent_existence < 1, 1, 2))

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


### 1. 다중공선성만 제외한 포화모형. 
hendra_rev_1 <- glm(hendra_gp ~ chiroptera_maxent_existence + carnivoraGP + hfp_average + elevation + 
                      BFF_predict + 
                      horse + landcover_4_average + 
                      landcover_9_average + landcover_10_average + landcover_12_average +
                      landcover_14_average 
                    , 
                    data = final_data_filtered, family = binomial(link = 'logit'), na.action = na.exclude)

hendra_rev_1 <- glm(hendra_gp ~ strigiformesGP + colubridaeGP + hfp_average + elevation + 
                      BFF_predict + SFF_predict +
                      horse + landcover_4_average + 
                      landcover_9_average + landcover_10_average + landcover_12_average +
                      landcover_14_average 
                    , 
                    data = final_data_filtered, family = binomial(link = 'logit'), na.action = na.exclude)

hendra_rev_1 <- glm(hendra_gp ~ RaptorsGP + colubridaeGP + hfp_average + elevation + 
                      BFF_predict +
                      horse + landcover_4_average + 
                      landcover_9_average + landcover_10_average + landcover_12_average +
                      landcover_14_average 
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