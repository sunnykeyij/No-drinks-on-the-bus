rm(list=ls())
getwd() 
# setwd("C:\\Users\\UOS\\Dropbox\\DataMiningTeamProject")
load("gooraw.Rda")
pairs(goo_raw[,-1])
plot(hclust(dist(goo_raw[,-1])),labels=goo_raw$goo)

km_out <- kmeans(goo_raw[,-1],3,nstart=20)
plot(goo_raw[,-1],col=(km_out$cluster+1))
