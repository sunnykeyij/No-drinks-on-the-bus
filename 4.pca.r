
rm(list=ls())

## 환경설정--------------------------------------------------------------------------------------------------
require(graphics); require(dplyr)
if(!require(tidyverse)){install.packages("tidyverse")}; library(tidyverse)
if(!require(ggfortify)){install.packages("ggfortify")}; library(ggfortify)

setwd("C:/Users/UOS/Dropbox/DataMiningTeamProject")
load("df_market.RDa")
load("trash_bymarket.RDa")


#############################################################################
# 분석과정 3. 군집 내 상권분석 - PCA
#############################################################################

### step1. 데이터정의(목표군집 내 자치구별 상권의 데이터)

# 데이터생성 A - 상권별(상권이름 mainTrarNm) 쓰레기통이 있을 확률(trash_p)
trash_bymarket2 <- trash_bymarket %>% distinct(자치구명, 설치위치, trarNo, .keep_all = T)
trash_p_data <- trash_bymarket2 %>% group_by(mainTrarNm) %>% dplyr::summarise(trash_p = mean(near_bus)) %>% as.data.frame()

# 데이터생성 B - 목표한 행정자치구의 상권데이터 생성
group2 <- c("강서구", "관악구", "노원구", "마포구", "서초구", "송파구", "영등포구")   # 살펴보고 싶은 군집(clustering result)
goo_pur_data <- df_market %>% filter(goo %in% group2) %>% select(name, goo, area, sales, busnum, buspeople_up)
goo_pur_data$name <- as.character(goo_pur_data$name)

## 최종 데이터 생성 = left_join(B, A)
pca_data <- left_join(goo_pur_data, trash_p_data, by = c("name" = "mainTrarNm"))
pca_data[is.na(pca_data$trash_p),7] <- 0
colnames(pca_data) <- c("상권명", "자치구", "면적크기", "매출액", "버스정류장수", "승차객수", "비율")

### step 2. PCA 분석
pca_model <- prcomp(pca_data[-c(1:3,5)], scale = TRUE); pca_model
summary(pca_model)
screeplot(pca_model, type="l")

# pca_score <- round(predict(pca_model), 2)   # 주성분점수 구하기
# rownames(pca_score) <- paste(pca_data$자치구, pca_data$상권명);pca_score

### step 3. PCA model로 bigraph 그리기
biplot(pca_model)   # 각 obs에 대한 제1주성분, 제2주성분 점수를 그래프로 작성
autoplot(pca_model, data = pca_data, colour = '자치구',
         label = TRUE, label.size = 5, loadings = TRUE, loadings.colour = 'black',
         loadings.label = TRUE, loadings.label.size = 7, loadings.label.colour = "black") + 
  theme(legend.text = element_text(size = 16), legend.title = element_text(size = 20), axis.title = element_text(size = 14))
