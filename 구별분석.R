rm(list=ls())
library(dplyr)
library(cluster)

setwd("C:\\Users\\Mycom\\Dropbox\\DataMiningTeamProject")
# setwd("C:\\Users\\UOS\\Dropbox\\DataMiningTeamProject")
load("gooraw.Rda")
goo_raw <- goo_raw[,-10] # 세대,인구수 비슷하니까 세대만 남긴다
rownames(goo_raw) <- goo_raw$goo

#############################################################################
# Daypop, nightpop, 면적, 프랜차이즈, buscount, trashcount, 종사자수, 커피점월평균매출
#############################################################################
goo_raw <- goo_raw %>% select(goo,daypop,nightpop,면적,프랜차이즈,buscount,trashcount,종사자수,월매출)
# #교차분석-GGally
# if(!require(GGally)){ install.packages('GGally')}; require(GGally)
# ggpairs(goo_raw[,-1])
# #상관분석-corrplot
# if(!require(corrplot)){ install.packages('corrplot')}; require(corrplot)
# goo_raw %>% select(-goo) %>%cor() %>% corrplot.mixed(upper="ellipse")
################ 군집분석 #################
# kmeans
set.seed(3)
goo_scale <- as.data.frame(scale(goo_raw[,-1]))
kmeans_fit <- kmeans(goo_scale,3)

if(!require(cluster)){ install.packages('cluster')}; require(cluster)
clusplot(goo_scale, kmeans_fit$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)


plot(goo_scale,col=(kmeans_fit$cluster))

par(mfrow=c(2,2))
plot(goo_scale$daypop,goo_scale$면적,type="n")
text(goo_scale$daypop,goo_scale$면적,labels=goo_raw$goo,col=(kmeans_fit$cluster))

plot(goo_scale$프랜차이즈,goo_scale$daypop,type="n")
text(goo_scale$프랜차이즈,goo_scale$daypop,labels=goo_raw$goo,col=(kmeans_fit$cluster))

plot(goo_scale$면적,goo_scale$buscount,type="n")
text(goo_scale$면적,goo_scale$buscount,labels=goo_raw$goo,col=(kmeans_fit$cluster))

plot(goo_scale$trashcount,goo_scale$면적,type="n")
text(goo_scale$trashcount,goo_scale$면적,labels=goo_raw$goo,col=(kmeans_fit$cluster))

# hclust
clust_out1 <- hclust(dist(goo_scale),method="average")
plot(clust_out1,labels=goo_raw$goo)
clust_out2 <- hclust(dist(goo_scale),method="single")
plot(clust_out2,labels=goo_raw$goo)
clust_out3 <- hclust(dist(goo_scale),method="complete")
plot(clust_out3,labels=goo_raw$goo)

################ 회귀 #################
library(car)
reg1 <- lm(buscount~daypop+nightpop+면적+프랜차이즈+trashcount+종사자수+월매출,data=goo_raw)
vif(reg1)
cor(goo_raw[,-1])

prcomp1 <- prcomp(scale(goo_raw[,c(-1,-6)]))
summary(prcomp1)
print(prcomp1)
plot(prcomp1,type="l")
biplot(prcomp1,cex=c(0.6,1))

library(glmnet)
grid=10^seq(10,-2,length=100)
x=model.matrix(buscount~daypop+nightpop+면적+프랜차이즈+trashcount+종사자수+월매출,data=goo_raw)[,-1]
y=goo_raw$buscount

set.seed(1)
cv.out = cv.glmnet(x,y,alpha=0)
plot(cv.out)
bestlam = cv.out$lambda.min
out = glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)
