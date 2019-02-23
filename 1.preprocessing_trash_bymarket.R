rm(list=ls())
require(xlsx)
require(dplyr)
load("busloc.RDa")  # 정류소번호, 정류소명, x좌표, y좌표, 행정구, 법정동, 승차총승객수, 하차총승객수
load("trash_df.RDa") # 자치구명, 쓰레기통설치위치, x좌표, y좌표, nearbus:버스정류장위치여부(0,1)
source("function_library.R")

### 쓰레기통 위치가 해당하는 상점 찾기
mykey <- "your key"
xvec <- as.character(trash_df$x좌표); yvec=as.character(trash_df$y좌표)

find_market_3 <- function(lat,long,key){
  url0 <- "http://apis.data.go.kr/B553077/api/open/sdsc/storeZoneInRadius"
  nrowvector <- c()  
  
  request_Url <- paste0(url0,"?ServiceKey=",key,"&radius=",500,"&cx=",long,"&cy=",lat)
  raw.data <- xmlTreeParse(request_Url, useInternalNodes = TRUE, encoding="UTF-8")
  rootNode <- xmlRoot(raw.data)
  market_area <- xmlToDataFrame(getNodeSet(raw.data,"//item"),stringsAsFactors = FALSE)
  
  return(market_area[,-c(8,9,10)])
}

# for문으로 많이 처리 안되는 문제 해결 필요
trash_bymarket1 <- lapply(1:50,function(i) find_market_3(lat=yvec[i],long=xvec[i],key=mykey))
trash_bymarket2 <- lapply(51:100,function(i) find_market_3(lat=yvec[i],long=xvec[i],key=mykey))
trash_bymarket3 <- lapply(101:150,function(i) find_market_3(lat=yvec[i],long=xvec[i],key=mykey))
trash_bymarket4 <- lapply(151:200,function(i) find_market_3(lat=yvec[i],long=xvec[i],key=mykey))
trash_bymarket5 <- lapply(201:250,function(i) find_market_3(lat=yvec[i],long=xvec[i],key=mykey))
trash_bymarket6 <- lapply(251:300,function(i) find_market_3(lat=yvec[i],long=xvec[i],key=mykey))
trash_bymarket7 <- lapply(301:325,function(i) find_market_3(lat=yvec[i],long=xvec[i],key=mykey))
trash_bymarket8 <- lapply(326:350,function(i) find_market_3(lat=yvec[i],long=xvec[i],key=mykey))
trash_bymarket9 <- lapply(351:400,function(i) find_market_3(lat=yvec[i],long=xvec[i],key=mykey))
trash_bymarket10 <- lapply(401:450,function(i) find_market_3(lat=yvec[i],long=xvec[i],key=mykey))
trash_bymarket11 <- lapply(451:500,function(i) find_market_3(lat=yvec[i],long=xvec[i],key=mykey))
trash_bymarket12 <- lapply(501:550,function(i) find_market_3(lat=yvec[i],long=xvec[i],key=mykey))
trash_bymarket13 <- lapply(551:600,function(i) find_market_3(lat=yvec[i],long=xvec[i],key=mykey))
trash_bymarket14 <- lapply(601:650,function(i) find_market_3(lat=yvec[i],long=xvec[i],key=mykey))
trash_bymarket15 <- lapply(651:690,function(i) find_market_3(lat=yvec[i],long=xvec[i],key=mykey))
trash_bymarket16 <- lapply(691:720,function(i) find_market_3(lat=yvec[i],long=xvec[i],key=mykey))
trash_bymarket17 <- lapply(721:750,function(i) find_market_3(lat=yvec[i],long=xvec[i],key=mykey))
trash_bymarket18 <- lapply(751:800,function(i) find_market_3(lat=yvec[i],long=xvec[i],key=mykey))
trash_bymarket19 <- lapply(801:850,function(i) find_market_3(lat=yvec[i],long=xvec[i],key=mykey))
trash_bymarket20 <- lapply(851:870,function(i) find_market_3(lat=yvec[i],long=xvec[i],key=mykey))
trash_bymarket21 <- lapply(871:910,function(i) find_market_3(lat=yvec[i],long=xvec[i],key=mykey))
trash_bymarket22 <- lapply(911:950,function(i) find_market_3(lat=yvec[i],long=xvec[i],key=mykey))
trash_bymarket23 <- lapply(951:1000,function(i) find_market_3(lat=yvec[i],long=xvec[i],key=mykey))
trash_bymarket24 <- lapply(1001:1030,function(i) find_market_3(lat=yvec[i],long=xvec[i],key=mykey))
trash_bymarket25 <- lapply(1031:1051,function(i) find_market_3(lat=yvec[i],long=xvec[i],key=mykey))


trash_by_market_list <- c(trash_bymarket1,trash_bymarket2,trash_bymarket3,trash_bymarket4,trash_bymarket5,
                          trash_bymarket6,trash_bymarket7,trash_bymarket8,trash_bymarket9,trash_bymarket10,
                          trash_bymarket11,trash_bymarket12,trash_bymarket13,trash_bymarket14,trash_bymarket15,
                          trash_bymarket16,trash_bymarket17,trash_bymarket18,trash_bymarket19,trash_bymarket20,
                          trash_bymarket21,trash_bymarket22,trash_bymarket23,trash_bymarket24,trash_bymarket25)
length(trash_by_market_list)
trash_by_market_list

# save(trash_by_market_list,file="trash_by_market_list.RDa")
# save(list=ls(),file="trash_market_processing.RDa")

head(trash_by_market_list)
df1 <- data.frame( 설치위치 = rep(trash_df$설치위치, lapply(trash_by_market_list, nrow)))
df2 <- do.call("rbind",trash_by_market_list)
trash_bymarket_df <- cbind(df1,df2)

trash_bymarket <- left_join(trash_df,trash_bymarket_df[,c(1,2,3)],by=c("설치위치"="설치위치"))

# save(trash_bymarket,file="trash_bymarket.RDa")