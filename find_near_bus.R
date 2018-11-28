
library("XML")

find_near_bus <- function(location_x,location_y,radius){
  url <- "http://ws.bus.go.kr/api/rest/stationinfo/getStationByPos"
  key <- "Your_key"

  request_Url <- paste(url,"?serviceKey=",key,sep="","&tmX=",location_x,"&tmY=",location_y,"&radius=",radius)
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

find_near_bus(location_y=37.4988392, location_x=126.9300703, radius=200)

