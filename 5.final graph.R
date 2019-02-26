rm(list=ls())

##  환경설정 -------------------------------------------------------------------------------
library(ggmap); library(stringr); library(dplyr)
if(!require(viridis)){install.packages("viridis")}; require(viridis)
if(!require(raster)){install.packages("raster")}; require(raster)


#############################################################################
# 분석과정 4. final ggmap visualization
#############################################################################

### step1. 데이터 정제
setwd("C:/Users/UOS/Dropbox/DataMiningTeamProject/")터

# 서울시 카페 데이터(크롤링)
load("caffe data/seoul_caffe.RData")  
seoul_caffe$lat <- as.numeric(seoul_caffe$lat); seoul_caffe$lon <- as.numeric(seoul_caffe$lon)
seoul_caffe$area <- word(seoul_caffe$area, 2)

# 서울시 자치구별 위도, 경도 데이터
seoul_center <- read.csv("서울시구별위경도.csv", encoding = "eur-k")

# 서울시 행정동 코드 데이터
gu_codelist <- read.csv("행정동코드_매핑정보_2018.csv", encoding = "eur-k")

# 서울시 버스위치 및 승하차인구 데이터
load("busloc.RDa")

# 서울시 쓰레기통 위치 및 갯수 데이터
load("trash_df.RDa"); load("trash_bymarket2.RDa")

### step2. 구글 api_key 등록하기-------------------------------------------------------------------------------
api_key <-"your google api key"
register_google(key = api_key); has_google_key()   # check api_key


### step3. 서울시 상권지도 + 버스승차객 + 버스정류장 + 카페 + 쓰레기통 function 'final_map' 정의 ------------------------------------

final_map <- function(location, name_gu){
  com_bus <- buslocation %>% filter(행정구 == name_gu)
  com_caffe <- seoul_caffe %>% filter(area == name_gu)
  com_trash <- trash_bymarket2 %>% filter(자치구명 == name_gu)
  com_trash$x좌표 <- as.numeric(as.character(com_trash$x좌표)); com_trash$y좌표 <- as.numeric(as.character(com_trash$y좌표))
  
  com_map <- get_map(location, source="google", maptype="roadmap", zoom = 16)
  ggmap(com_map) +   # 지도 띄우기
    geom_point(data = com_caffe, aes(x=lon, y=lat), size=8, alpha = 0.5, color = "yellow") +   # 카페 위치
    geom_point(data = com_trash, aes(x=x좌표, y=y좌표), size=8, color = "black", fill = "purple", shape = 21) +   # 쓰레기통 위치
    geom_point(data = com_bus, aes(x=X좌표, y=Y좌표, size = 승차총승객수), alpha = 0.5, color = "forestgreen") +   # 버스정류장 위치
    scale_size_continuous(range=c(10,30)) +
    theme(legend.position = 'none')
}

# 관악구 상권: 서울대입구역_1, 서울대입구역_2 -> location = c(126.953, 37.481)
final_map(location = c(126.953, 37.481), name_gu ="관악구")

# 관악구 상권: 신림역_1 ~ 4 -> location = c(126.9294, 37.4842)
final_map(location = c(126.9294, 37.4842), name_gu ="관악구")

# 서초구 상권: 양재역 -> location = c(127.0321803, 37.4841062)
final_map(location = c(127.0321803, 37.4841062), name_gu ="서초구")

# 서초구 상권: 교대역 -> location = c(127.0144373, 37.4911231)
final_map(location = c(127.0144373, 37.4911231), name_gu ="서초구")
