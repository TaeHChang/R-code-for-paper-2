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

final_data$hendra_gp <-factor(final_data$hendra_gp, levels = c(0, 1))


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


###################################################################################################





########################## Minmax로 추출한 것 먼저.
final_data_minmax

table(final_data_minmax$hendra_gp)

##  Chiroptera 
final_data_minmax %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(chiroptera_Maxent),
            SD = sd(chiroptera_Maxent))

ttest <- t.test(chiroptera_Maxent ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(chiroptera_Maxent ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Accipoitriformes 
final_data_minmax %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(accipitriformes_Maxent),
            SD = sd(accipitriformes_Maxent))

ttest <- t.test(accipitriformes_Maxent ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(accipitriformes_Maxent ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Strigiformes 
final_data_minmax %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(strigiformes_Maxent),
            SD = sd(strigiformes_Maxent))

ttest <- t.test(strigiformes_Maxent ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(strigiformes_Maxent ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Carnivora 
final_data_minmax %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(carnivora_Maxent),
            SD = sd(carnivora_Maxent))

ttest <- t.test(carnivora_Maxent ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(carnivora_Maxent ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Colubridae 
final_data_minmax %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(colubridae_Maxent),
            SD = sd(colubridae_Maxent))

ttest <- t.test(colubridae_Maxent ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(colubridae_Maxent ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  HFP 
final_data_minmax %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(hfp_average),
            SD = sd(hfp_average))

ttest <- t.test(hfp_average ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(hfp_average ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Elevation 
final_data_minmax %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(elevation),
            SD = sd(elevation))

ttest <- t.test(elevation ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(elevation ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Precipitation 
final_data_minmax %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(precipitation_average),
            SD = sd(precipitation_average))

ttest <- t.test(precipitation_average ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(precipitation_average ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Temperature 
final_data_minmax %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(temperature_average),
            SD = sd(temperature_average))

ttest <- t.test(temperature_average ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(temperature_average ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Temperature 
final_data_minmax %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(pop_dense_average),
            SD = sd(pop_dense_average))

ttest <- t.test(pop_dense_average ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(pop_dense_average ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  GDP 
final_data_minmax %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(GDP_average),
            SD = sd(GDP_average))

ttest <- t.test(GDP_average ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(GDP_average ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  HDI 
final_data_minmax %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(HDI_average),
            SD = sd(HDI_average))

ttest <- t.test(HDI_average ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(HDI_average ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Forest cover 
final_data_minmax %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(forest_cover_average),
            SD = sd(forest_cover_average))

ttest <- t.test(forest_cover_average ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(forest_cover_average ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Horse density 
final_data_minmax %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(horse),
            SD = sd(horse))

ttest <- t.test(horse ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(horse ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Landcover_2
final_data_minmax %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(landcover_2_average),
            SD = sd(landcover_2_average))

ttest <- t.test(landcover_2_average ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(landcover_2_average ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox


##  Landcover_4
final_data_minmax %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(landcover_4_average),
            SD = sd(landcover_4_average))

ttest <- t.test(landcover_4_average ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(landcover_4_average ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Landcover_5
final_data_minmax %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(landcover_5_average),
            SD = sd(landcover_5_average))

ttest <- t.test(landcover_5_average ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(landcover_5_average ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Landcover_6
final_data_minmax %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(landcover_6_average),
            SD = sd(landcover_6_average))

ttest <- t.test(landcover_6_average ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(landcover_6_average ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Landcover_7
final_data_minmax %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(landcover_7_average),
            SD = sd(landcover_7_average))

ttest <- t.test(landcover_7_average ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(landcover_7_average ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Landcover_8
final_data_minmax %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(landcover_8_average),
            SD = sd(landcover_8_average))

ttest <- t.test(landcover_8_average ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(landcover_8_average ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Landcover_9
final_data_minmax %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(landcover_9_average),
            SD = sd(landcover_9_average))

ttest <- t.test(landcover_9_average ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(landcover_9_average ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Landcover_10
final_data_minmax %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(landcover_10_average),
            SD = sd(landcover_10_average))

ttest <- t.test(landcover_10_average ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(landcover_10_average ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Landcover_12
final_data_minmax %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(landcover_12_average),
            SD = sd(landcover_12_average))

ttest <- t.test(landcover_12_average ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(landcover_12_average ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Landcover_13
final_data_minmax %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(landcover_13_average),
            SD = sd(landcover_13_average))

ttest <- t.test(landcover_13_average ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(landcover_13_average ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Landcover_14
final_data_minmax %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(landcover_14_average),
            SD = sd(landcover_14_average))

ttest <- t.test(landcover_14_average ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(landcover_14_average ~ hendra_gp, data = final_data_minmax, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox










########################## Matching으로 추출한 것 
final_data_match

table(final_data_match$hendra_gp)

##  Chiroptera 
final_data_match %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(chiroptera_Maxent),
            SD = sd(chiroptera_Maxent))

ttest <- t.test(chiroptera_Maxent ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(chiroptera_Maxent ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Accipoitriformes 
final_data_match %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(accipitriformes_Maxent),
            SD = sd(accipitriformes_Maxent))

ttest <- t.test(accipitriformes_Maxent ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(accipitriformes_Maxent ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Strigiformes 
final_data_match %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(strigiformes_Maxent),
            SD = sd(strigiformes_Maxent))

ttest <- t.test(strigiformes_Maxent ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(strigiformes_Maxent ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Carnivora 
final_data_match %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(carnivora_Maxent),
            SD = sd(carnivora_Maxent))

ttest <- t.test(carnivora_Maxent ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(carnivora_Maxent ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Colubridae 
final_data_match %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(colubridae_Maxent),
            SD = sd(colubridae_Maxent))

ttest <- t.test(colubridae_Maxent ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(colubridae_Maxent ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  HFP 
final_data_match %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(hfp_average),
            SD = sd(hfp_average))

ttest <- t.test(hfp_average ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(hfp_average ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Elevation 
final_data_match %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(elevation),
            SD = sd(elevation))

ttest <- t.test(elevation ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(elevation ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Precipitation 
final_data_match %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(precipitation_average),
            SD = sd(precipitation_average))

ttest <- t.test(precipitation_average ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(precipitation_average ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Temperature 
final_data_match %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(temperature_average),
            SD = sd(temperature_average))

ttest <- t.test(temperature_average ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(temperature_average ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Population 
final_data_match %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(pop_dense_average),
            SD = sd(pop_dense_average))

ttest <- t.test(pop_dense_average ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(pop_dense_average ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  GDP 
final_data_match %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(GDP_average),
            SD = sd(GDP_average))

ttest <- t.test(GDP_average ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(GDP_average ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  HDI 
final_data_match %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(HDI_average),
            SD = sd(HDI_average))

ttest <- t.test(HDI_average ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(HDI_average ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Forest cover 
final_data_match %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(forest_cover_average),
            SD = sd(forest_cover_average))

ttest <- t.test(forest_cover_average ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(forest_cover_average ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Horse density 
final_data_match %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(horse),
            SD = sd(horse))

ttest <- t.test(horse ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(horse ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Landcover_2
final_data_match %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(landcover_2_average),
            SD = sd(landcover_2_average))

ttest <- t.test(landcover_2_average ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(landcover_2_average ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox


##  Landcover_4
final_data_match %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(landcover_4_average),
            SD = sd(landcover_4_average))

ttest <- t.test(landcover_4_average ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(landcover_4_average ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Landcover_5
final_data_match %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(landcover_5_average),
            SD = sd(landcover_5_average))

ttest <- t.test(landcover_5_average ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(landcover_5_average ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Landcover_6
final_data_match %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(landcover_6_average),
            SD = sd(landcover_6_average))

ttest <- t.test(landcover_6_average ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(landcover_6_average ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Landcover_7
final_data_match %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(landcover_7_average),
            SD = sd(landcover_7_average))

ttest <- t.test(landcover_7_average ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(landcover_7_average ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Landcover_8
final_data_match %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(landcover_8_average),
            SD = sd(landcover_8_average))

ttest <- t.test(landcover_8_average ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(landcover_8_average ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Landcover_9
final_data_match %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(landcover_9_average),
            SD = sd(landcover_9_average))

ttest <- t.test(landcover_9_average ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(landcover_9_average ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Landcover_10
final_data_match %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(landcover_10_average),
            SD = sd(landcover_10_average))

ttest <- t.test(landcover_10_average ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(landcover_10_average ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Landcover_12
final_data_match %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(landcover_12_average),
            SD = sd(landcover_12_average))

ttest <- t.test(landcover_12_average ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(landcover_12_average ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Landcover_13
final_data_match %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(landcover_13_average),
            SD = sd(landcover_13_average))

ttest <- t.test(landcover_13_average ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(landcover_13_average ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Landcover_14
final_data_match %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(landcover_14_average),
            SD = sd(landcover_14_average))

ttest <- t.test(landcover_14_average ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(landcover_14_average ~ hendra_gp, data = final_data_match, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox






final_data_filtered <- read.csv("D:\\Environmental data\\Hendra-Australia\\Final_dataset\\modified_dataset\\final_data_filtered.csv")

## 여기서 넣는건, preprocessing species richness Rcode에서 계산한 existence matrix를 직접 넣는 것.
final_data_filtered <- final_data_filtered %>% 
  left_join(existence_mat, by ="ID")
  
write.csv(final_data_filtered, "D:\\Environmental data\\Hendra-Australia\\Final_dataset\\Updated_20230306\\final_data_filtered.csv")

########################## Flying-fox 발견된 지역만 filtered.
final_data_filtered

table(final_data_filtered$hendra_gp)


## Flying-fox distribution covariate로 포함하기 위한 준비과정.
# P. alecto / P. conspicillatus BFF/SFF 포함.
final_data_filtered <- final_data_filtered %>% 
  mutate(BFF_predict = log(BFF_predict),
         SFF_predict = log(SFF_predict),
         GHFF_predict = log(GHFF_predict),
         LRFF_predict = log(LRFF_predict))

final_data_filtered %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(BFF_predict),
            SD = sd(BFF_predict))

ttest <- t.test(BFF_predict ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(BFF_predict ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox


final_data_filtered %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(SFF_predict),
            SD = sd(SFF_predict))

ttest <- t.test(SFF_predict ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(SFF_predict ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox


##  Chiroptera 
final_data_filtered %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(chiroptera_Maxent),
            SD = sd(chiroptera_Maxent))

ttest <- t.test(chiroptera_Maxent ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(chiroptera_Maxent ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Accipoitriformes 
final_data_filtered %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(accipitriformes_Maxent),
            SD = sd(accipitriformes_Maxent))

ttest <- t.test(accipitriformes_Maxent ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(accipitriformes_Maxent ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Strigiformes 
final_data_filtered %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(strigiformes_Maxent),
            SD = sd(strigiformes_Maxent))

ttest <- t.test(strigiformes_Maxent ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(strigiformes_Maxent ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Carnivora 
final_data_filtered %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(carnivora_Maxent),
            SD = sd(carnivora_Maxent))

ttest <- t.test(carnivora_Maxent ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(carnivora_Maxent ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Colubridae 
final_data_filtered %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(colubridae_Maxent),
            SD = sd(colubridae_Maxent))

ttest <- t.test(colubridae_Maxent ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(colubridae_Maxent ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  HFP 
final_data_filtered %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(hfp_average),
            SD = sd(hfp_average))

ttest <- t.test(hfp_average ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(hfp_average ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Elevation 
final_data_filtered %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(elevation),
            SD = sd(elevation))

ttest <- t.test(elevation ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(elevation ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Precipitation 
final_data_filtered %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(precipitation_average),
            SD = sd(precipitation_average))

ttest <- t.test(precipitation_average ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(precipitation_average ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Temperature 
final_data_filtered %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(temperature_average),
            SD = sd(temperature_average))

ttest <- t.test(temperature_average ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(temperature_average ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Pop dense 
final_data_filtered %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(pop_dense_average),
            SD = sd(pop_dense_average))

ttest <- t.test(pop_dense_average ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(pop_dense_average ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  GDP 
final_data_filtered %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(GDP_average),
            SD = sd(GDP_average))

ttest <- t.test(GDP_average ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(GDP_average ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  HDI 
final_data_filtered %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(HDI_average),
            SD = sd(HDI_average))

ttest <- t.test(HDI_average ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(HDI_average ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Forest cover 
final_data_filtered %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(forest_cover_average),
            SD = sd(forest_cover_average))

ttest <- t.test(forest_cover_average ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(forest_cover_average ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Horse density 
final_data_filtered %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(horse),
            SD = sd(horse))

ttest <- t.test(horse ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(horse ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Landcover_2
final_data_filtered %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(landcover_2_average),
            SD = sd(landcover_2_average))

ttest <- t.test(landcover_2_average ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(landcover_2_average ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox


##  Landcover_4
final_data_filtered %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(landcover_4_average),
            SD = sd(landcover_4_average))

ttest <- t.test(landcover_4_average ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(landcover_4_average ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Landcover_5
final_data_filtered %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(landcover_5_average),
            SD = sd(landcover_5_average))

ttest <- t.test(landcover_5_average ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(landcover_5_average ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Landcover_6
final_data_filtered %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(landcover_6_average),
            SD = sd(landcover_6_average))

ttest <- t.test(landcover_6_average ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(landcover_6_average ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Landcover_7
final_data_filtered %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(landcover_7_average),
            SD = sd(landcover_7_average))

ttest <- t.test(landcover_7_average ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(landcover_7_average ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Landcover_8
final_data_filtered %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(landcover_8_average),
            SD = sd(landcover_8_average))

ttest <- t.test(landcover_8_average ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(landcover_8_average ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Landcover_9
final_data_filtered %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(landcover_9_average),
            SD = sd(landcover_9_average))

ttest <- t.test(landcover_9_average ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(landcover_9_average ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Landcover_10
final_data_filtered %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(landcover_10_average),
            SD = sd(landcover_10_average))

ttest <- t.test(landcover_10_average ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(landcover_10_average ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Landcover_12
final_data_filtered %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(landcover_12_average),
            SD = sd(landcover_12_average))

ttest <- t.test(landcover_12_average ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(landcover_12_average ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Landcover_13
final_data_filtered %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(landcover_13_average),
            SD = sd(landcover_13_average))

ttest <- t.test(landcover_13_average ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(landcover_13_average ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##  Landcover_14
final_data_filtered %>% 
  group_by(hendra_gp) %>% 
  summarise(mean = mean(landcover_14_average),
            SD = sd(landcover_14_average))

ttest <- t.test(landcover_14_average ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(landcover_14_average ~ hendra_gp, data = final_data_filtered, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

