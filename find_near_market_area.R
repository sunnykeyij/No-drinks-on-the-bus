rm(list=ls())

library("XML")

find_market_area <- function(radius,lat,long){

url <- "http://apis.data.go.kr/B553077/api/open/sdsc/storeZoneInRadius"
key <- "your key"

request_Url <- paste(url,"?ServiceKey=",key,"&radius=",radius,"&cx=",lat,"&cy=",long,sep="")
raw.data <- xmlTreeParse(request_Url, useInternalNodes = TRUE, encoding="UTF-8")
rootNode <- xmlRoot(raw.data)
market_area <- xmlToDataFrame(getNodeSet(raw.data,"//item"),stringsAsFactors = FALSE)
return(market_area)
}

find_market_area(lat=127.004528,long=37.567538,radius=250)



