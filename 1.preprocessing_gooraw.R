rm(list=ls())
setwd("C:\\Users\\UOS\\Dropbox\\DataMiningTeamProject")
source("\function_library.R")
if(!require(xlsx)){ install.packages('xlsx')}; require(xlsx)
if(!require(dplyr)){ install.packages('dplyr')}; require(dplyr)
if(!require(stringr)){ install.packages('stringr')}; require(stringr)
if(!require(rvest)){ install.packages('rvest')}; require(rvest)
if(!require(httr)){ install.packages('httr')}; require(httr)
if(!require(XML)){ install.packages('XML')}; require(XML)

#######################################################################
############################ 데이터 수집 ##############################
#######################################################################

### 행정동별 일자, 시간, 연령별 생활인구 (서울시 정보소통광장) ###
lifepop_raw <- read.csv("201810동별생활인구.csv",header=FALSE,stringsAsFactors=FALSE) 
names(lifepop_raw) <- c("daysid","timeid","dongcode","totalpop",
                        paste0("popmen",c("09","1014","1519","2024","2529","3034","3539","4044","4549","5054","5559","6064","6569","70")),
                        paste0("popwom",c("09","1014","1519","2024","2529","3034","3539","4044","4549","5054","5559","6064","6569","70")))
head(lifepop_raw)
lifepop_raw <- lifepop_raw[-1,]
lifepop_raw <- mutate_all(lifepop_raw, function(x) as.numeric(x))

### 구/동별 행정동코드 매핑정보 (서울시 열린데이터광장) ###
mapping_goo <- read.xlsx("행정동코드_매핑정보_2018.xlsx",1,encoding="UTF-8",header=TRUE,stringsAsFactors=FALSE)
mapping_goo <- mapping_goo[-1,]
mapping_goo$통계청행정동코드 <- as.numeric(mapping_goo$통계청행정동코드)
mapping_goo$행자부행정동코드 <- as.numeric(mapping_goo$행자부행정동코드)
str(lifepop_raw)
str(mapping_goo)
names(mapping_goo) <- c("kostatcode","moicode","localx","localy","localz")

### 구별 면적 정보 (서울시 열린데이터광장) ###
area_raw <- read.xlsx("서울시행정구역(구별)통계.xlsx",1,encoding="UTF-8",header=TRUE,stringsAsFactors=FALSE)

### 구별 커피전문점 수 및 인구 정보 (우리마을가게 상권분석서비스) ###
cafe_raw <- read.xlsx("서울시커피음료상권지표(구별).xlsx",1,encoding="UTF-8",header=TRUE,stringsAsFactors=FALSE)
cafe_raw <- cafe_raw[-1,]

### 구별 쓰레기통 위치 정보 (서울시 정보소통광장) ###
trash_raw <- read.xlsx("201809서울특별시가로쓰레기통.xlsx",1,encoding="UTF-8",header=TRUE,stringsAsFactors=FALSE)
head(trash_raw)

### 구별 인구정보 (서울시 정보소통광장, 서울시열린데이터광장) ###
pop_raw <- read.xlsx("서울시구별주민등록인구.xlsx",1,encoding="UTF-8",header=TRUE,stringsAsFactors=FALSE)
worker_raw <- read.xlsx("서울시구별사업체및종사자수.xlsx",1,encoding="UTF-8",header=TRUE,stringsAsFactors=FALSE)
worker_raw2 <- worker_raw %>% filter(동=="소계")

### 버스정류장 위치 및 승하차인구 정보 (서울시 열린데이터광장, 서울시정보소통광장) ###
busloc_raw <- read.csv("서울버스정류장위치.csv",header=TRUE,stringsAsFactors=FALSE)
buspeople_raw <- read.csv("201810버스승하차인구2.csv",header=TRUE,stringsAsFactors=FALSE)
# 버스정류장이 해당하는 행정구/ 법정동 찾기
busloc_1 <- do.call('rbind', lapply(1:nrow(busloc_raw),function(i) toaddress(long=busloc_raw$X좌표[i], lat=busloc_raw$Y좌표[i])))
busloc_1 <- cbind(busloc_raw,busloc_1)
buspeople_raw$버스정류장ARS번호 <- as.numeric(buspeople_raw$버스정류장ARS번호)
# 버스정류장별 이용객 (한달)평균내기
buspeople <- buspeople_raw %>% select(버스정류장ARS번호,승차총승객수,하차총승객수) %>% group_by(버스정류장ARS번호) %>% summarise_all(mean)
busloc_2 <- inner_join(busloc_1,buspeople,by=c("정류소번호"="버스정류장ARS번호"))
buslocation <- busloc_2 # 정류소번호/정류소명/X좌표/Y좌표/행정구/법정동/승차총승객수/하차총승객수
# save(buslocation,file="busloc.Rda")


### 구별 커피점 매출 정보 (소상공인 상권정보시스템) ###
sales_goo <- read.xlsx("2017주요상권별커피점매출.xlsx",1,encoding="UTF-8",header=TRUE,stringsAsFactors=FALSE)
sales_raw <- sales_goo[,c("행정구","지역","월매출평균")]

### 행정동코드 매핑정보 (서울시 열린데이터광장) ###
mapping_goo2 <- read.xlsx("행정동코드_매핑정보_2018.xlsx",2,encoding="UTF-8",header=TRUE,stringsAsFactors=FALSE)
mapping_goo2 <-mapping_goo %>% filter(RESD_DO_NM=="서울")

### 주요상권 데이터 - 상권명, 상권면적, 상권위치 등 (공공데이터포털 소상공인 상가업소 정보 서비스 API) ###
url0 <- "http://apis.data.go.kr/B553077/api/open/sdsc/storeZoneInAdmi"
key <- "your key"
request_url_vec <- paste0(url0,"?divId=signguCd","&key=",mapping_goo2$RESD_CD,"&ServiceKey=",key) 
marketfunc <- function(request_url){
  raw.data <- xmlTreeParse(request_url, useInternalNodes = TRUE, encoding="UTF-8")
  rootNode <- xmlRoot(raw.data)
  market_area <- xmlToDataFrame(getNodeSet(raw.data,"//item"),stringsAsFactors = FALSE)
  return(market_area)
}
goo_market <- lapply(1:25, function(i) marketfunc(request_url = request_url_vec[i]))
goo_market2 <- do.call('rbind',goo_market)

market_bygoo <- goo_market2

#######################################################################
###################### 변수 생성 및 데이터 병합 #######################
#######################################################################

### 구별 인구합치기 ###
#9시~22시 인구 추출, 시간별로 평균낸다
lifepop_day <- lifepop_raw %>% select(timeid,dongcode,totalpop) %>% filter(timeid %in% c(9:21)) %>% group_by(dongcode) %>% summarise_all(mean)
# 22시~9시 인구 추출, 시간별로 평균낸다
lifepop_night <- lifepop_raw %>% select(timeid,dongcode,totalpop) %>% filter(timeid %in% c(0:8,22,23)) %>% group_by(dongcode) %>% summarise_all(mean)
# 무슨 구인지 매칭시키고 구별로 합을낸다
lifepop_day2 <- inner_join(lifepop_day,mapping_goo,by=c("dongcode"="moicode")) %>% select(-dongcode,-timeid,-kostatcode,-localx,-localz) %>% group_by(localy) %>% summarise_all(sum)
lifepop_night2 <- inner_join(lifepop_night,mapping_goo,by=c("dongcode"="moicode")) %>% select(-dongcode,-timeid,-kostatcode,-localx,-localz) %>% group_by(localy) %>% summarise_all(sum)
lifepop_data <- data.frame(goo=lifepop_day2$localy,daypop=lifepop_day2$totalpop,nightpop=lifepop_night2$totalpop)
### 구별 면적합치기 ###
goo_table1 <- inner_join(lifepop_data,area_raw[,c(2,3)],by=c("goo"="자치구"))
### 구별 카페수합치기 ###
goo_table2 <- inner_join(goo_table1, cafe_raw[,c(1,3,5)], by=c("goo"="지역"))
### 구별 버스정류장수 & 쓰레기통수 합치기 ###
bus_goo_count <- buslocation %>% group_by(행정구) %>% count()
names(bus_goo_count)[2] <- "buscount"
goo_table2 <- inner_join(goo_table2,bus_goo_count,by=c("goo"="행정구"))
trash_goo_count <- trash_raw %>% group_by(자치구명) %>% count()
names(trash_goo_count)[2] <- "trashcount"
goo_table2 <- inner_join(goo_table2,trash_goo_count,by=c("goo"="자치구명"))

goo_table3 <- inner_join(goo_table2,pop_raw,by=c("goo"="자치구"))
goo_table3 <- inner_join(goo_table3,worker_raw2[,c(2,5)],by=c("goo"="자치구"))

### 구별 주요상권 데이터 처리 ###
# 상권 중심 좌표 찾기
market_bygoo$coords <- gsub("POLYGON","", market_bygoo$coords)
market_bygoo$coords <- gsub("\\)","", market_bygoo$coords)
market_bygoo$coords <- gsub("\\(","", market_bygoo$coords)
poly_clean <- function(vec){
  polyarea <- strsplit(vec," ") %>% unlist
  polyarea <- polyarea[-1]
  polymat <- matrix(as.numeric(polyarea),ncol=2,byrow=TRUE)
  min(polymat[,1])
  center <- c(mean(min(polymat[,1]),max(polymat[,1])),mean(min(polymat[,2]),max(polymat[,2])))
  return(center)
}
center <- sapply(1:nrow(market_bygoo),function(i) poly_clean(market_bygoo$coords[i]))
market_bygoo$xcenter = center[1,]
market_bygoo$ycenter = center[2,]
# save(market_bygoo,file="market_bygoo.RDa")

# 상권 별 커피 매출정보
table_1 <- list()
for (i in 1:25){
  sales <- sales_raw %>% filter(행정구==unique(행정구)[i])
  market <- market_bygoo %>% filter(signguNm==unique(sales_raw$행정구)[i])
  table_1[[i]] <- inner_join(market,sales,by=c("mainTrarNm"="지역"))
}
table_market <- do.call('rbind',table_1)
# save(table_market,file="table_market.RDa")

### 구별 주요 상권 수 및 매출정보 합치기 ###
market_goo_count <- table_market %>% group_by(signguNm) %>% count()
names(market_goo_count)[2] <- "num_market"
goo_table4 <- inner_join(goo_table3,market_goo_count,by=c("goo"="signguNm"))
goo_table4 <- inner_join(goo_table4,sales_goo[,c(1,4)],by=c("goo"="지역"))

### 최종 ###
# 행정구명, 주간생활인구, 야간생활인구, 면적, 프랜차이즈 커피점 수, 유도인구, 버스승하차인구, 쓰레기통수, 세대수, 인구수, 종사자수, 주요상권수, 상권내월매출 
goo_raw <- goo_table4
save(goo_raw,file="gooraw.Rda")               
