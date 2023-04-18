########### Moran's I 구하고, Autocorrelation의 구조 파악하기.
library(car)
library(mlogit)
library(boot)
library(ggplot2)
library(tidyverse)
### 먼저 Moran's I를 계산하기 위해서는 neighbor에 대한 정보가 필요하므로, 
# 지금까지 다루던 최종데이터셋들을 shp 파일로 변환하여 다시 불러올 필요가 있다. 
# 그러면 최종 데이터셋들을 불러오고, 거기에 base_map의 geometry를 삽입한 다음에 그걸 QGIS에 넣어서 shp로 변환해보자.


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

write.csv(final_data_minmax, "D:\\Environmental data\\Hendra-Australia\\Final_dataset\\modified_dataset\\final_data_minmax.csv")
write.csv(final_data_match, "D:\\Environmental data\\Hendra-Australia\\Final_dataset\\modified_dataset\\final_data_match.csv")
write.csv(final_data_filtered, "D:\\Environmental data\\Hendra-Australia\\Final_dataset\\modified_dataset\\final_data_filtered.csv")



library(rgdal)
## QGIS에서 base_map.shp에다가 csv 합쳐서 만들어낸 것.
# https://wikidocs.net/163960 참고하기.
final_data_minmax_shp <- sf::st_read("D:\\Environmental data\\Hendra-Australia\\Final_dataset\\modified_dataset\\final_data_minmax.shp")
final_data_match_shp <- sf::st_read("D:\\Environmental data\\Hendra-Australia\\Final_dataset\\modified_dataset\\final_data_match.shp")
final_data_filtered_shp <- sf::st_read("D:\\Environmental data\\Hendra-Australia\\Final_dataset\\modified_dataset\\final_data_filtered.shp")


final_data_minmax_shp <- final_data_minmax_shp %>% 
  filter(final_data != is.na(final_data))

final_data_match_shp <- final_data_match_shp %>% 
  filter(final_data != is.na(final_data))

final_data_filtered_shp <- final_data_filtered_shp %>% 
  filter(final_data != is.na(final_data))



### spatial linking structure
#공간적인 연결을 시각화하려고 할 때. 
library(spdep)

### 이 코드 사용해서 잘 안 맞는 도형 고칠 수 있고,
yer_object$geometry <- yer_object$geometry %>%
  s2::s2_rebuild() %>%
  sf::st_as_sfc()

### 혹은 이 코드를 쓰면 그냥 에러가 있더라도 강행하게 할 수 있다.
sf::sf_use_s2(FALSE)

poly.nlist<- spdep::poly2nb(final_data_minmax_shp)

# spatial weight 그리고 위에서 만든 공간가중행렬에 가중치 설정
listw <- spdep::nb2listw(poly.nlist)   
# if there is an island #이웃이 없는 경우를 고려해준다.
listw <- spdep::nb2listw(poly.nlist, zero.policy=TRUE)


### test
# Moran test for global spatial autocorrelation
#공간가중치 행렬을 활용하여 모란 통계량 산출.
moran.test(final_data_minmax_shp$final_da_1, listw, zero.policy=T)
# Monte Carlo Moran Test
bperm <-moran.mc(final_data_minmax_shp$final_da_1, listw, nsim=999 ,zero.policy=T)
bperm

# Moran's I plot 
spdep::moran.plot(final_data_minmax_shp$final_da_1, listw = listw, zero.policy=T)






poly.nlist<- spdep::poly2nb(final_data_match_shp)

# spatial weight 그리고 위에서 만든 공간가중행렬에 가중치 설정
listw <- spdep::nb2listw(poly.nlist)   
# if there is an island #이웃이 없는 경우를 고려해준다.
listw <- spdep::nb2listw(poly.nlist, zero.policy=TRUE)


### test
# Moran test for global spatial autocorrelation
#공간가중치 행렬을 활용하여 모란 통계량 산출.
moran.test(final_data_match_shp$final_da_1, listw, zero.policy=T)
# Monte Carlo Moran Test
bperm <-moran.mc(final_data_match_shp$final_da_1, listw, nsim=999 ,zero.policy=T)
bperm

# Moran's I plot 
spdep::moran.plot(final_data_match_shp$final_da_1, listw = listw, zero.policy=T)




poly.nlist<- spdep::poly2nb(final_data_filtered_shp)

# spatial weight 그리고 위에서 만든 공간가중행렬에 가중치 설정
listw <- spdep::nb2listw(poly.nlist)   
# if there is an island #이웃이 없는 경우를 고려해준다.
listw <- spdep::nb2listw(poly.nlist, zero.policy=TRUE)


### test
# Moran test for global spatial autocorrelation
#공간가중치 행렬을 활용하여 모란 통계량 산출.
moran.test(final_data_filtered_shp$final_da_1, listw, zero.policy=T)
# Monte Carlo Moran Test
bperm <-moran.mc(final_data_filtered_shp$final_da_1, listw, nsim=999 ,zero.policy=T)
bperm

# Moran's I plot 
spdep::moran.plot(final_data_filtered_shp$final_da_1, listw = listw, zero.policy=T)



