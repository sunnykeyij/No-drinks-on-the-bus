rm(list = ls())

## 환경설정--------------------------------------------------------------------------------------------------
if(!require(devtools)){install.packages("devtools")}; require(devtools)
devtools::install_github("dkahle/ggmap", ref="tidyup");library(ggmap)   # 2.7ver.
if(!require(ggplot2)){install.packages("ggplot2")}; require(ggplot2)
if(!require(raster)){install.packages("raster")}; require(raster)   # map_update
if(!require(viridis)){install.packages("viridis")};require(viridis)   # map_update
if(!require(dplyr)){install.packages("dplyr")}; require(dplyr) 
if(!require(rgdal)){install.packages("rgdal")}; require(rgdal)   
if(!require(maptools)){install.packages("maptools")}; require(maptools)   # map_data
if(!require(rgeos)){install.packages("rgeos")}; require(rgeos)    # map_data
if(!require(broom)){install.packages("broom")}; require(broom)    # map_data
if(!require(xlsx)){install.packages("xlsx")}; require(xlsx)

# 시군 구분 행정지도 가져오기
korea_new <- shapefile('shp/SIG_201804/TL_SCCO_SIG.shp')
seoul_map <- subset(korea_new, as.integer(korea_new$SIG_CD)%/%1000 == 11) # 한국 행정지도에서 서울시만 추출

# data가져오기
load("gooraw.RDa")

# 변수 별 그래프 그리기
graph_func = function(var,map){
  
  # var = goo_raw$daypop
  dat = data.frame("SIG_KOR_NM"=goo_raw$goo,"count" =var)
  dat$SIG_KOR_NM = as.character(dat$SIG_KOR_NM)
  
  # map =  seoul_map
  seoul_admin <- merge(map, dat, by="SIG_KOR_NM")
  
  x <- seoul_admin$count
  x <- -(x-min(x))/(max(x)-min(x))*100
  x <- cut((x-min(x))/(max(x)-min(x)), seq(-0.1, 1.1, length=109))
  
  plot(map, col=heat.colors(100)[x])
  text(coordinates(map), seoul_admin$SIG_KOR_NM, cex=0.7)
}

#############################################################################
# Daypop, nightpop, 면적, 프랜차이즈, buscount, trashcount, 종사자수, 커피점월평균매출
#############################################################################
daypop_graph = graph_func(var= goo_raw$daypop,map = seoul_map)
title(main = "daypop",cex.main = 1)

nightpop_graph = graph_func(var= goo_raw$nightpop, map = seoul_map)
title(main = "nightpop",cex.main = 1)

areasize_graph = graph_func(var= goo_raw$면적, map = seoul_map)
title(main = "areasize",cex.main = 1)

caffe_graph = graph_func(var= goo_raw$프랜차이즈,map = seoul_map)
title(main = "caffecount",cex.main = 1)

buscount_graph = graph_func(var = goo_raw$buscount,map = seoul_map)
title(main = "buscount",cex.main = 1)

trashcount_graph = graph_func(var=goo_raw$trashcount,map = seoul_map)
title(main = "trashcount",cex.main = 1)

work_graph = graph_func(var= goo_raw$종사자수,map = seoul_map)
title(main = "work people",cex.main = 1)

sales_graph = graph_func(var=goo_raw$월매,map = seoul_map)
title(main = "sales",cex.main = 1)