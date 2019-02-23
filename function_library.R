if(!require(XML)){ install.packages('XML')}; require(XML)
if(!require(data.table)){ install.packages('data.table')}; require(data.table)
require(httr)
require(rvest)
library(readr)
library(dplyr)

### 좌표(위도,경도) 넣으면 해당하는 행정구, 법정동 추출하는 함수
### NAVER OPEN API
toaddress <- function(long,lat){
  naver.id = "your id"  # naver API id 
  naver.pw = "your pw"  # naver API pw    
  root2 = "https://naveropenapi.apigw.ntruss.com/map-reversegeocode/v2/gc?request=coordsToaddr&version=1.0&coords="
  url2 = GET(paste0(root2,long,",",lat,"&output=json"),add_headers("X-NCP-APIGW-API-KEY-ID" = naver.id,"X-NCP-APIGW-API-KEY" = naver.pw),
             write_disk('tmp1.json', overwrite = T))
  tt1 = jsonlite::fromJSON('tmp1.json')
  
  goo <- tt1$results[1,]$region$area2$name
  dong <- tt1$results[1,]$region$area3$name
  
  return(data.frame("행정구"=goo,"법정동"=dong))
}

### 경도,위도 해당하는 상권 찾아주는 함수
### 공공데이터포털 소상공인상가업소 정보서비스 API
find_market_1 <- function(lat,long,key){
  url0 <- "http://apis.data.go.kr/B553077/api/open/sdsc/storeZoneInRadius"
  nrowvector <- c()  

  for (radius in seq(100,500,by=100)){      
    request_Url <- paste0(url0,"?ServiceKey=",key,"&radius=",radius,"&cx=",long,"&cy=",lat)
    raw.data <- xmlTreeParse(request_Url, useInternalNodes = TRUE, encoding="UTF-8")
    rootNode <- xmlRoot(raw.data)
    market_area <- xmlToDataFrame(getNodeSet(raw.data,"//item"),stringsAsFactors = FALSE)
    nrowvector[radius/100] <- nrow(market_area)
    if (nrow(market_area)==1){
      break
    }
  }
  
  if (all(nrowvector==0))
    return("NA")
  
  if (nrow(market_area)>1){
    for (radius in (min(which(nrowvector>1))*100+seq(-90,90,by=10))){
      request_Url <- paste0(url0,"?ServiceKey=",key,"&radius=",radius,"&cx=",long,"&cy=",lat)
      raw.data <- xmlTreeParse(request_Url, useInternalNodes = TRUE, encoding="UTF-8")
      rootNode <- xmlRoot(raw.data)
      market_area <- xmlToDataFrame(getNodeSet(raw.data,"//item"),stringsAsFactors = FALSE)
      if (nrow(market_area)==1){
        break
      }
    }
  }
  
  return(market_area[,-c(8,9,10)])
}

find_market_2 = function(request_Url)
{
  url_get = GET(request_Url)
  url_xml_item = read_xml(url_get) %>% xml_nodes('item')
  locat_vec = lapply(url_xml_item, function(x) xml_children(x))[[1]] %>% xml_text()
  return(locat_vec)
}

### 특정 위도,경도특정 반경 내에 있는 버스정류장 찾아주는 함수
### 공공데이터포털 서울특별시 정류소정보조회 서비스 API
find_near_bus <- function(location_x,location_y,radius,key){
  url0 <- "http://ws.bus.go.kr/api/rest/stationinfo/getStationByPos"
  request_Url <- paste(url0,"?serviceKey=",key,sep="","&tmX=",location_x,"&tmY=",location_y,"&radius=",radius)
  raw.data <- xmlTreeParse(request_Url, useInternalNodes = TRUE,encoding="UTF-8")
  rootNode <- xmlRoot(raw.data)
  msgBody <- rootNode[[3]]
  size <- xmlSize(msgBody)
  
  bus_data <- data.frame()
  for (i in 1:size){
    rawbus <-  as.data.frame(t(xmlSApply(msgBody[[i]],xmlValue)))
    bus_data <- rbind(bus_data,rawbus)
  }
  return(bus_data)
}

# 위에꺼 안될때 있어서 만듬
# http://stat-and-news-by-daragon9.tistory.com/118 참고
find_near_bus2 <- function(location_x,location_y,key){
  url0 <- "http://openapi.tago.go.kr/openapi/service/BusSttnInfoInqireService/getCrdntPrxmtSttnList" #국토교통부 500m 근처
  
  # location_y=37.4988392; location_x=126.9300703
  request_Url <- paste(url0,"?serviceKey=",key,sep="","&gpsLati=",location_y,"&gpsLong=",location_x)
  raw.data <- xmlTreeParse(request_Url, useInternalNodes = TRUE,encoding="UTF-8")
  rootNode <- xmlRoot(raw.data)
  items <- rootNode[[2]][['items']]
  size <- xmlSize(items)
  bus_data <- list()
  
  for (i in 1:size){
    xmlrow <- xmlSApply(items[[i]],xmlValue)
    xmlrow <- data.table(citycode=xmlrow[[1]],gpslati=xmlrow[[2]],gpslong=xmlrow[[3]],nodeid=xmlrow[[4]],nodenm=xmlrow[[5]])
    bus_data[[i]] <- xmlrow
  }
  
  bus_data <- rbindlist(bus_data)
  
  return(bus_data)
}

# find_market_1(lat=127.004528,long=37.567538,key=key)
# find_near_bus(location_y=37.4988392, location_x=126.9300703, radius=200, key=key)
# find_near_bus2(location_y=37.4988392, location_x=126.9300703,key=key)

