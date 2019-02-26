### 주요상권별 버스정류장 수 및 인구 찾기 ###
rm(list=ls())

library(dplyr)
library(XML)
key <- "your key of data.go.kr"
load("table_market.Rda")
load("gooraw.Rda")
load("busloc.RDa")

### 상권에 존재하는 버스정류장 찾기
x_vec <- table_market$xcenter 
y_vec <- table_market$ycenter
r_vec <- sqrt(as.numeric(table_market$trarArea)/2)

# 상권에 존재하는 버스정류장 찾기
bus_by_market <- lapply(1:nrow(table_market), function(i) tryCatch(find_near_bus(x_vec[i],y_vec[i],300,key=key),error=function(e){})) 

# 버스정류장에 승차승객수, 하차승객수 붙이기
buslocation$정류소번호[nchar(buslocation$정류소번호)==4] <- paste0(0,buslocation$정류소번호) # 4자리 정류소 번호 앞에 0을 붙여야함 (inner join을 위해)
bus_by_market2 <- lapply(1:length(bus_by_market), function(i) tryCatch(inner_join(bus_by_market[[i]],buslocation[,c(1,7,8)],by=c("arsId"="정류소번호")),error=function(e){}))

bus_bymarket <- bus_by_market2
names(bus_bymarket) <- table_market$mainTrarNm
# save(bus_bymarket,file="bus_bymarket.RDa")      # 상권(LIST) 별 버스정류장, 승객수

# 상권별로 존재하는 버스정류장 수 세기 (busnum)
busnum <- sapply(bus_bymarket,nrow)
busnum <- sapply(1:length(busnum), function(i) if(length(busnum[[i]])==0) busnum[[i]]=0 else busnum[[i]])

# 상권 별 승차승객수 더하기 (buspeople_up)
buspeople_up <- unlist(lapply(1:length(bus_by_market2), function(i) sum(bus_by_market2[[i]]$승차총승객수)))

df_market <- data.frame(marketid=table_market$trarNo,name=table_market$mainTrarNm,goo=table_market$signguNm,
                        area=table_market$trarArea,sales=table_market$월매출평균,busnum,buspeople_up)

df_market$xcenter <- table_market$xcenter
df_market$ycenter <- table_market$ycenter

# 상권번호, 상권명, 행정구, 면적, 매출, busnum, buspeople_up, x중심좌표, y중심좌표
# save(df_market,file="df_market.RDa") 