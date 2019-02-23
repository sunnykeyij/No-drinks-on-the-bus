rm(list=ls())
library(dplyr)

load("gooraw.Rda")
goo_raw <- goo_raw %>% dplyr::select(goo,daypop,nightpop,면적,프랜차이즈,buscount,trashcount,종사자수,월매출)

### kmeans
set.seed(3)
goo_scale <- as.data.frame(scale(goo_raw[,-1]))
kmeans_fit <- kmeans(goo_scale,3)

if(!require(cluster)){ install.packages('cluster')}; require(cluster)
clusplot(goo_scale, kmeans_fit$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)
plot(goo_scale,col=(kmeans_fit$cluster))

par(mfrow=c(2,2))
plot(goo_scale$daypop,goo_scale$면적,type="n",main="주간생활인구-면적")
text(goo_scale$daypop,goo_scale$면적,labels=goo_raw$goo,col=(kmeans_fit$cluster))

plot(goo_scale$프랜차이즈,goo_scale$daypop,type="n",main="커피전문점-주간생활인구")
text(goo_scale$프랜차이즈,goo_scale$daypop,labels=goo_raw$goo,col=(kmeans_fit$cluster))

plot(goo_scale$면적,goo_scale$buscount,type="n",main="면적-버스정류장")
text(goo_scale$면적,goo_scale$buscount,labels=goo_raw$goo,col=(kmeans_fit$cluster))

plot(goo_scale$면적,goo_scale$trashcount,type="n",main="면적-쓰레기통")
text(goo_scale$면적,goo_scale$trashcount,labels=goo_raw$goo,col=(kmeans_fit$cluster))

### hclust
clust_out1 <- hclust(dist(goo_scale),method="average")
plot(clust_out1,labels=goo_raw$goo)
clust_out2 <- hclust(dist(goo_scale),method="single")
plot(clust_out2,labels=goo_raw$goo)
clust_out3 <- hclust(dist(goo_scale),method="complete")
plot(clust_out3,labels=goo_raw$goo)

