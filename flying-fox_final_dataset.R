#####################################################################
#########  Flying-fox distribution 변수를 final dataset에 할당하기
#####################################################################

### 먼저 쯔쯔가무시 환경변수 할당에서 사용한 것처럼, 각 flying-fox shp을 처리해서
### csv 형태로 만들어놓고, 이를 가져와서 처리한다.

library(tidyverse)
library(readr)
library(car)
library(rms)
BFF <- read.csv("C:\\Users\\Taehee\\OneDrive\\바탕 화면\\My papers\\Hendravirus_작업중\\flying-fox_distribution\\교차영역\\BFF_교차영역.csv")
GHFF <- read.csv("C:\\Users\\Taehee\\OneDrive\\바탕 화면\\My papers\\Hendravirus_작업중\\flying-fox_distribution\\교차영역\\GHFF_교차영역.csv")
LRFF <- read.csv("C:\\Users\\Taehee\\OneDrive\\바탕 화면\\My papers\\Hendravirus_작업중\\flying-fox_distribution\\교차영역\\LRFF_교차영역.csv")
SFF <- read.csv("C:\\Users\\Taehee\\OneDrive\\바탕 화면\\My papers\\Hendravirus_작업중\\flying-fox_distribution\\교차영역\\SFF_교차영역.csv")
final_dataset <- read.csv("D:\\Environmental data\\Hendra-Australia\\Final_dataset\\final_dataset_total.csv")

environmental_factor <- final_dataset %>% 
  dplyr::select(ID, precipitation_average, temperature_average, forest_cover_average, hfp_average, elevation)

BFF <- BFF %>% 
  left_join(environmental_factor, by = c('id' = 'ID'))
GHFF <- GHFF %>% 
  left_join(environmental_factor, by = c('id' = 'ID'))
LRFF <- LRFF %>% 
  left_join(environmental_factor, by = c('id' = 'ID'))
SFF <- SFF %>% 
  left_join(environmental_factor, by = c('id' = 'ID'))

BFF$average_BF <- round(as.numeric(BFF$average_BF))

### 그리고, 각 종 별로 환경변수를 설명변수로 하고 flying fox distribution을 결과변수로 하는 glm / poisson 모형을 적합한다.


############### BFF
BFF_model <- glm(average_BF ~ precipitation_average + temperature_average + forest_cover_average + hfp_average + elevation,
                 family = quasipoisson(link = "log"), data = BFF, na.action = na.exclude)
summary(BFF_model)
vif(BFF_model)
lrm(BFF_model)
BFF_model$fitted.values


###모형의 이탈도를 평가해서 기저모형에 비해 유의하게 개선되었는지 확인한다.
modelChi <- BFF_model$null.deviance - BFF_model$deviance #귀무 이탈도 - 모형 이탈도
chidf <- BFF_model$df.null - BFF_model$df.residual #귀무 자유도 - 모형 자유도 = 자유도 변화량
chisq.prob <- 1 - pchisq(modelChi, chidf) #카이제곱 통계량을 계산하고 1에서 뺀다
modelChi; chidf; chisq.prob
#원래의 기저모형과 새로 만든 모형 간에 예측도 차이가 없다는 귀무가설을
#x^2(5) = 233.32, p < .001로 기각한다.


###로지스틱 회귀모형에서 사용 가능한 측도를 계산하는 함수.
#선형회귀에서와 비슷하게 결정계수의 근사치로 활용할 수 있다.
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
glmPseudoR2s(BFF_model)

BFF_predict <- as_tibble(predict(BFF_model, newdata = environmental_factor, type = "response"))

colnames(BFF_predict) <- "BFF_predict"

BFF_predict$BFF_predict <- exp(BFF_predict$BFF_predict) 

final_dataset <- final_dataset %>% cbind(BFF_predict)





##################### SFF
SFF_model <- glm(average_SF ~ precipitation_average + forest_cover_average + hfp_average + elevation,
                 family = quasipoisson(link = "log"), data = SFF, na.action = na.exclude)
summary(SFF_model)
vif(SFF_model)

SFF_model$fitted.values


###모형의 이탈도를 평가해서 기저모형에 비해 유의하게 개선되었는지 확인한다.
modelChi <- SFF_model$null.deviance - SFF_model$deviance #귀무 이탈도 - 모형 이탈도
chidf <- SFF_model$df.null - SFF_model$df.residual #귀무 자유도 - 모형 자유도 = 자유도 변화량
chisq.prob <- 1 - pchisq(modelChi, chidf) #카이제곱 통계량을 계산하고 1에서 뺀다
modelChi; chidf; chisq.prob
#원래의 기저모형과 새로 만든 모형 간에 예측도 차이가 없다는 귀무가설을
#x^2(4) = 274.74, p < .001로 기각한다.


###로지스틱 회귀모형에서 사용 가능한 측도를 계산하는 함수.
#선형회귀에서와 비슷하게 결정계수의 근사치로 활용할 수 있다.
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
glmPseudoR2s(SFF_model)

SFF_predict <- as_tibble(predict(SFF_model, newdata = environmental_factor, type = "response"))

colnames(SFF_predict) <- "SFF_predict"

SFF_predict$SFF_predict <- exp(SFF_predict$SFF_predict) 

final_dataset <- final_dataset %>% cbind(SFF_predict)






##################### GHFF
GHFF_model <- glm(average_GH ~ precipitation_average + forest_cover_average + hfp_average + elevation +
                    temperature_average,
                 family = quasipoisson(link = "log"), data = GHFF, na.action = na.exclude)
summary(GHFF_model)
vif(GHFF_model)

GHFF_model$fitted.values


###모형의 이탈도를 평가해서 기저모형에 비해 유의하게 개선되었는지 확인한다.
modelChi <- GHFF_model$null.deviance - GHFF_model$deviance #귀무 이탈도 - 모형 이탈도
chidf <- GHFF_model$df.null - GHFF_model$df.residual #귀무 자유도 - 모형 자유도 = 자유도 변화량
chisq.prob <- 1 - pchisq(modelChi, chidf) #카이제곱 통계량을 계산하고 1에서 뺀다
modelChi; chidf; chisq.prob
#원래의 기저모형과 새로 만든 모형 간에 예측도 차이가 없다는 귀무가설을
#x^2(4) = 274.74, p < .001로 기각한다.


###로지스틱 회귀모형에서 사용 가능한 측도를 계산하는 함수.
#선형회귀에서와 비슷하게 결정계수의 근사치로 활용할 수 있다.
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
glmPseudoR2s(GHFF_model)

GHFF_predict <- as_tibble(predict(GHFF_model, newdata = environmental_factor, type = "response"))

colnames(GHFF_predict) <- "GHFF_predict"

GHFF_predict$GHFF_predict <- exp(GHFF_predict$GHFF_predict) 

final_dataset <- final_dataset %>% cbind(GHFF_predict)





##################### LRFF
LRFF_model <- glm(average_LR ~ precipitation_average + forest_cover_average + hfp_average + elevation +
                    temperature_average,
                  family = quasipoisson(link = "log"), data = LRFF, na.action = na.exclude)
summary(LRFF_model)
vif(LRFF_model)

LRFF_model$fitted.values


###모형의 이탈도를 평가해서 기저모형에 비해 유의하게 개선되었는지 확인한다.
modelChi <- LRFF_model$null.deviance - LRFF_model$deviance #귀무 이탈도 - 모형 이탈도
chidf <- LRFF_model$df.null - LRFF_model$df.residual #귀무 자유도 - 모형 자유도 = 자유도 변화량
chisq.prob <- 1 - pchisq(modelChi, chidf) #카이제곱 통계량을 계산하고 1에서 뺀다
modelChi; chidf; chisq.prob
#원래의 기저모형과 새로 만든 모형 간에 예측도 차이가 없다는 귀무가설을
#x^2(4) = 274.74, p < .001로 기각한다.


###로지스틱 회귀모형에서 사용 가능한 측도를 계산하는 함수.
#선형회귀에서와 비슷하게 결정계수의 근사치로 활용할 수 있다.
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
glmPseudoR2s(LRFF_model)

LRFF_predict <- as_tibble(predict(LRFF_model, newdata = environmental_factor, type = "response"))

colnames(LRFF_predict) <- "LRFF_predict"

LRFF_predict$LRFF_predict <- exp(LRFF_predict$LRFF_predict) 

final_dataset <- final_dataset %>% cbind(LRFF_predict)

final_dataset <- final_dataset %>% 
  mutate(flying_fox = LRFF_predict + GHFF_predict + BFF_predict + SFF_predict)


















####################################################################################
#### Zero Inflated Poisson models



### 그리고, 각 종 별로 환경변수를 설명변수로 하고 flying fox distribution을 결과변수로 하는 glm / ZIP 모형을 적합한다.
library(pscl)
library(splines)


############### BFF
BFF_model <- zeroinfl(average_BF ~ precipitation_average + temperature_average + 
                        forest_cover_average,
                      dist = "poisson", data = BFF)
summary(BFF_model)
vif(BFF_model)
lrm(BFF_model)
BFF_model$fitted.values


###모형의 이탈도를 평가해서 기저모형에 비해 유의하게 개선되었는지 확인한다.
modelChi <- BFF_model$null.deviance - BFF_model$deviance #귀무 이탈도 - 모형 이탈도
chidf <- BFF_model$df.null - BFF_model$df.residual #귀무 자유도 - 모형 자유도 = 자유도 변화량
chisq.prob <- 1 - pchisq(modelChi, chidf) #카이제곱 통계량을 계산하고 1에서 뺀다
modelChi; chidf; chisq.prob
#원래의 기저모형과 새로 만든 모형 간에 예측도 차이가 없다는 귀무가설을
#x^2(5) = 233.32, p < .001로 기각한다.


###로지스틱 회귀모형에서 사용 가능한 측도를 계산하는 함수.
#선형회귀에서와 비슷하게 결정계수의 근사치로 활용할 수 있다.
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
glmPseudoR2s(BFF_model)

BFF_predict <- as_tibble(predict(BFF_model, newdata = environmental_factor, type = "response"))

colnames(BFF_predict) <- "BFF_predict"

BFF_predict$BFF_predict <- exp(BFF_predict$BFF_predict) 

final_dataset <- final_dataset %>% cbind(BFF_predict)





##################### SFF
SFF_model <- glm(average_SF ~ precipitation_average + forest_cover_average + hfp_average + elevation,
                 family = quasipoisson(link = "log"), data = SFF, na.action = na.exclude)
summary(SFF_model)
vif(SFF_model)

SFF_model$fitted.values


###모형의 이탈도를 평가해서 기저모형에 비해 유의하게 개선되었는지 확인한다.
modelChi <- SFF_model$null.deviance - SFF_model$deviance #귀무 이탈도 - 모형 이탈도
chidf <- SFF_model$df.null - SFF_model$df.residual #귀무 자유도 - 모형 자유도 = 자유도 변화량
chisq.prob <- 1 - pchisq(modelChi, chidf) #카이제곱 통계량을 계산하고 1에서 뺀다
modelChi; chidf; chisq.prob
#원래의 기저모형과 새로 만든 모형 간에 예측도 차이가 없다는 귀무가설을
#x^2(4) = 274.74, p < .001로 기각한다.


###로지스틱 회귀모형에서 사용 가능한 측도를 계산하는 함수.
#선형회귀에서와 비슷하게 결정계수의 근사치로 활용할 수 있다.
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
glmPseudoR2s(SFF_model)

SFF_predict <- as_tibble(predict(SFF_model, newdata = environmental_factor, type = "response"))

colnames(SFF_predict) <- "SFF_predict"

SFF_predict$SFF_predict <- exp(SFF_predict$SFF_predict) 

final_dataset <- final_dataset %>% cbind(SFF_predict)






##################### GHFF
GHFF_model <- glm(average_GH ~ precipitation_average + forest_cover_average + hfp_average + elevation +
                    temperature_average,
                  family = quasipoisson(link = "log"), data = GHFF, na.action = na.exclude)
summary(GHFF_model)
vif(GHFF_model)

GHFF_model$fitted.values


###모형의 이탈도를 평가해서 기저모형에 비해 유의하게 개선되었는지 확인한다.
modelChi <- GHFF_model$null.deviance - GHFF_model$deviance #귀무 이탈도 - 모형 이탈도
chidf <- GHFF_model$df.null - GHFF_model$df.residual #귀무 자유도 - 모형 자유도 = 자유도 변화량
chisq.prob <- 1 - pchisq(modelChi, chidf) #카이제곱 통계량을 계산하고 1에서 뺀다
modelChi; chidf; chisq.prob
#원래의 기저모형과 새로 만든 모형 간에 예측도 차이가 없다는 귀무가설을
#x^2(4) = 274.74, p < .001로 기각한다.


###로지스틱 회귀모형에서 사용 가능한 측도를 계산하는 함수.
#선형회귀에서와 비슷하게 결정계수의 근사치로 활용할 수 있다.
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
glmPseudoR2s(GHFF_model)

GHFF_predict <- as_tibble(predict(GHFF_model, newdata = environmental_factor, type = "response"))

colnames(GHFF_predict) <- "GHFF_predict"

GHFF_predict$GHFF_predict <- exp(GHFF_predict$GHFF_predict) 

final_dataset <- final_dataset %>% cbind(GHFF_predict)





##################### LRFF
LRFF_model <- glm(average_LR ~ precipitation_average + forest_cover_average + hfp_average + elevation +
                    temperature_average,
                  family = quasipoisson(link = "log"), data = LRFF, na.action = na.exclude)
summary(LRFF_model)
vif(LRFF_model)

LRFF_model$fitted.values


###모형의 이탈도를 평가해서 기저모형에 비해 유의하게 개선되었는지 확인한다.
modelChi <- LRFF_model$null.deviance - LRFF_model$deviance #귀무 이탈도 - 모형 이탈도
chidf <- LRFF_model$df.null - LRFF_model$df.residual #귀무 자유도 - 모형 자유도 = 자유도 변화량
chisq.prob <- 1 - pchisq(modelChi, chidf) #카이제곱 통계량을 계산하고 1에서 뺀다
modelChi; chidf; chisq.prob
#원래의 기저모형과 새로 만든 모형 간에 예측도 차이가 없다는 귀무가설을
#x^2(4) = 274.74, p < .001로 기각한다.


###로지스틱 회귀모형에서 사용 가능한 측도를 계산하는 함수.
#선형회귀에서와 비슷하게 결정계수의 근사치로 활용할 수 있다.
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
glmPseudoR2s(LRFF_model)

LRFF_predict <- as_tibble(predict(LRFF_model, newdata = environmental_factor, type = "response"))

colnames(LRFF_predict) <- "LRFF_predict"

LRFF_predict$LRFF_predict <- exp(LRFF_predict$LRFF_predict) 

final_dataset <- final_dataset %>% cbind(LRFF_predict)

final_dataset <- final_dataset %>% 
  mutate(flying_fox = LRFF_predict + GHFF_predict + BFF_predict + SFF_predict)
















##################### 원래 Final_dataset이라는 R-code에서 dataset을 만들고 있는데, 
## 거기에 더해서 여기서 계산한 flying-fox value랑 종 다양성 변수까지 넣어서 아예 통합본 만들기.
accipitriformes_IUCN <- read.csv("D:\\Environmental data\\Hendra-Australia\\IUCN_polygon\\Species_richness_mat\\accipitriformes_count_IUCN.csv")
strigiformes_IUCN <- read.csv("D:\\Environmental data\\Hendra-Australia\\IUCN_polygon\\Species_richness_mat\\strigiformes_count_IUCN.csv")
carnivora_IUCN <- read.csv("D:\\Environmental data\\Hendra-Australia\\IUCN_polygon\\Species_richness_mat\\carnivora_count_IUCN.csv")
colubridae_IUCN <- read.csv("D:\\Environmental data\\Hendra-Australia\\IUCN_polygon\\Species_richness_mat\\colubridae_count_IUCN.csv")
chiroptera_IUCN <- read.csv("D:\\Environmental data\\Hendra-Australia\\IUCN_polygon\\Species_richness_mat\\chiroptera_count_IUCN.csv")

accipitriformes_Maxent <- read.csv("D:\\Environmental data\\Hendra-Australia\\Maxent\\Species_mat\\accipitriformes_prop50_count.csv")
strigiformes_Maxent <- read.csv("D:\\Environmental data\\Hendra-Australia\\Maxent\\Species_mat\\strigiformes_prop50_count.csv")
carnivora_Maxent <- read.csv("D:\\Environmental data\\Hendra-Australia\\Maxent\\Species_mat\\carnivora_prop50_count.csv")
colubridae_Maxent <- read.csv("D:\\Environmental data\\Hendra-Australia\\Maxent\\Species_mat\\colubridae_prop50_count.csv")
chiroptera_Maxent <- read.csv("D:\\Environmental data\\Hendra-Australia\\Maxent\\Species_mat\\chiroptera_prop50_count.csv")

accipitriformes_IUCN <- accipitriformes_IUCN %>% 
  dplyr::select(id, ...1) %>% 
  rename(accipitriformes_IUCN = ...1)
strigiformes_IUCN <- strigiformes_IUCN %>% 
  dplyr::select(id, ...1) %>% 
  rename(strigiformes_IUCN = ...1)
carnivora_IUCN <- carnivora_IUCN %>% 
  dplyr::select(id, ...1) %>% 
  rename(carnivora_IUCN = ...1)
colubridae_IUCN <- colubridae_IUCN %>% 
  dplyr::select(id, ...1) %>% 
  rename(colubridae_IUCN = ...1)
chiroptera_IUCN <- chiroptera_IUCN %>% 
  dplyr::select(id, ...1) %>% 
  rename(chiroptera_IUCN = ...1)


accipitriformes_Maxent <- accipitriformes_Maxent %>% 
  dplyr::select(id, count_accipitriformes_prop50) %>% 
  rename(accipitriformes_Maxent = count_accipitriformes_prop50)
strigiformes_Maxent <- strigiformes_Maxent %>% 
  dplyr::select(id, count_strigiformes_prop50) %>% 
  rename(strigiformes_Maxent = count_strigiformes_prop50)
carnivora_Maxent <- carnivora_Maxent %>% 
  dplyr::select(id, count_carnivora_prop50) %>% 
  rename(carnivora_Maxent = count_carnivora_prop50)
colubridae_Maxent <- colubridae_Maxent %>% 
  dplyr::select(id, count_colubridae_prop50) %>% 
  rename(colubridae_Maxent = count_colubridae_prop50)
chiroptera_Maxent <- chiroptera_Maxent %>% 
  dplyr::select(id, count_chiroptera_prop50) %>% 
  rename(chiroptera_Maxent = count_chiroptera_prop50)

final_dataset <- final_dataset %>% 
  left_join(accipitriformes_IUCN, by = c("ID" = "id")) %>% 
  left_join(strigiformes_IUCN, by = c("ID" = "id")) %>%
  left_join(carnivora_IUCN, by = c("ID" = "id")) %>%
  left_join(colubridae_IUCN, by = c("ID" = "id")) %>%
  left_join(chiroptera_IUCN, by = c("ID" = "id")) %>% 
  left_join(accipitriformes_Maxent, by = c("ID" = "id")) %>% 
  left_join(strigiformes_Maxent, by = c("ID" = "id")) %>%
  left_join(carnivora_Maxent, by = c("ID" = "id")) %>%
  left_join(colubridae_Maxent, by = c("ID" = "id")) %>%
  left_join(chiroptera_Maxent, by = c("ID" = "id")) 

write.csv(final_dataset, "D:\\Environmental data\\Hendra-Australia\\Final_dataset\\final_dataset_total.csv")








#####################################################
####### 위쪽에서 minmax/matching으로 분석해본 것이 잘 안되어서, 
####### 새로 해보려고 하는것이, flying-fox들이 보고된 적이 있는 grid만 분리해서 해보려고.
#####################################################
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

total <- rbind(BFF_filtered, SFF_filtered, GHFF_filtered, LRFF_filtered)
total <- unique(total$id)












