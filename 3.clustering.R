rm(list=ls())
if(!require(cluster)){install.packages("cluster")}; require(cluster)
if(!require(dplyr)){install.packages("dplyr")} ; require(dplyr)
if(!require(plyr)){install.packages("plyr")};require(plyr)

load("gooraw.Rda")
goo_raw <- goo_raw %>% dplyr::select(goo,daypop,nightpop,면적,프랜차이즈,buscount,trashcount,종사자수,월매출)

### kmeans
set.seed(3)
goo_scale <- as.data.frame(scale(goo_raw[,-1]))
kmeans_fit <- kmeans(goo_scale,3)
kmeanss = kmeans_fit$cluster

par(mfrow=c(1,1), mar=c(3,3,3,3))
if(!require(cluster)){ install.packages('cluster')}; require(cluster)
clusplot(goo_scale, kmeanss, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

### hclust
source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")
par(mfrow=c(1,1), mar=c(4,4,4,4))
clust_out1 <- hclust(dist(goo_scale),method="average")
plot(clust_out1,labels=goo_raw$goo,main= "average cluster")
clust1_cut = cutree(clust_out1,k=3)

# op = par(bg = "#EFEFEF")
# A2Rplot(clust_out1, k = 3, boxes = FALSE, col.up = "gray50", col.down = c("#FF6B6B","#556270","Gold"))
# 
# op = par(bg = "gray15")
# cols = hsv(c(0.2, 0.57, 0.95), 1, 1, 0.8)
# A2Rplot(clust_out1, k = 3, boxes = FALSE, col.up = "gray50", col.down = cols)

clust_out2 <- hclust(dist(goo_scale),method="single")
plot(clust_out2,labels=goo_raw$goo,main= "single cluster")
clust2_cut = cutree(clust_out2,k=3)

clust_out3 <- hclust(dist(goo_scale),method="complete")
plot(clust_out3,labels=goo_raw$goo, main="complete cluster")
clust3_cut = cutree(clust_out3,k=3)


####################### 서울시 그림에 색칠하기 ######################

if(!require(devtools)){install.packages("devtools")}; require(devtools)
devtools::install_github("dkahle/ggmap", ref="tidyup");require(ggmap)   # 2.7ver.
if(!require(googleway)){install.packages("googleway")}; require(googleway)
if(!require(ggplot2)){install.packages("ggplot2")}; require(ggplot2)
if(!require(raster)){install.packages("raster")}; require(raster)   # map_update
if(!require(viridis)){install.packages("viridis")};require(viridis)   # map_update
if(!require(dplyr)){install.packages("dplyr")}; require(dplyr)
if(!require(rgdal)){install.packages("rgdal")}; require(rgdal)

## 2. 서울시 추출 구단위 주요상권 위도경도 업데이트 ---------------------------------------
api_key <-"your google api key"
register_google(key = api_key); has_goog_key()   # check api_key

# 시군 구분 행정지도 가져오기
korea_new <- shapefile('shp/SIG_201804/TL_SCCO_SIG.shp')
seoul_map <- subset(korea_new, as.integer(korea_new$SIG_CD)%/%1000 == 11) # 한국 행정지도에서 서울시만 추출
plot(korea_new)
plot(seoul_map)

### kmeans graph
kmeans.dat= as.table(kmeanss) %>% as.data.frame()
colnames(kmeans.dat) = c("SIG_KOR_NM","count")
kmeans.dat$SIG_KOR_NM =as.character(kmeans.dat$SIG_KOR_NM)

graph_func = function(map,data){

  seoul_admin <- merge(map, data, by="SIG_KOR_NM")
  
  x <- seoul_admin$count
  par(mfrow=c(1,1), mar=c(0.05,0.15,0.05,0.15))
  # par(mfrow=c(1,1), mar=c(0.1,0.2,0.1,0.2))
 
  plot(map, col=c("darkorchid1","chartreuse3","seashell2")[x])
  text(coordinates(map), seoul_admin$SIG_KOR_NM, cex=0.7)
}

kmeans_graph = graph_func(map = seoul_map,data = kmeans.dat)

### h-cluster graph
graph_func2 = function(var,map){
  #변수 정제
  # var = clust1_cut
  dat = as.table(var) %>% as.data.frame()
  colnames(dat) = c("SIG_KOR_NM","count")
  dat$SIG_KOR_NM =as.character(dat$SIG_KOR_NM)

  # map =  seoul_map
  seoul_admin <- merge(map, dat, by="SIG_KOR_NM")
  
  x <- seoul_admin$count
  par(mfrow=c(1,1), mar=c(0.1,0.2,0.1,0.2)) 
  plot(map, col=c("Dark Orange1","Snow","Gold")[x])
  text(coordinates(map), seoul_admin$SIG_KOR_NM, cex=0.7)
}
cluster1 = graph_func2(var=clust1_cut,map =  seoul_map) #average
cluster2 = graph_func2(var=clust2_cut,map =  seoul_map) #single
cluster3 = graph_func2(var=clust3_cut,map =  seoul_map) #complete

