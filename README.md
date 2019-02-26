# No-drinks-on-the-bus

## EDA
library(GGally)
![EDA](/images/EDA_correlation1.png)
library(corrplot)
![EDA](/images/EDA_correlation2.png)
![EDA](/images/EDA_seoul_daypop.png)
![EDA](/images/EDA_seoul_nightpop.png)
![EDA](/images/EDA_seoul_people.png)
![EDA](/images/EDA_seoul_areasize.png)
![EDA](/images/EDA_seoul_caffecount.png)
![EDA](/images/EDA_seoul_sales.png)
![EDA](/images/EDA_seoul_buscount.png)
![EDA](/images/EDA_seoul_trashcount.png)

## GU Clustering
- k-menas (k=3)
![K-means](/images/kmeans.png)
![K-means](/images/kmeans_graph.png)
- h-clust (average, complte, single methods)
![K-means](/images/cluster_average.png)
![K-means](/images/cluster_complete.png)
![K-means](/images/cluster_single.png)
- final cluster and characteristic  
예산이 자치구 단위로 집행되어 정책이 시행 되기 때문에 구 단위 군집 분석이 적합함  
초록색 군집의 경우 면적, 주간생활인구, 커피전문점수, 버스승차객 수가 많은 축임에 반해 쓰레기통 수가 적음을 알 수 있음  
따라서 초록색 군집에 대해 분석시행
![K-means](/images/final_kmeans.png)
![K-means](/images/cluster_scatterplot.png)

## Correlation Network
- market correlation
![network](/images/market_cor.graph.png)

## PCA
- market pca  
승차객수, 매출액을 대표하는 제 1주성분이 크고 버스정류장근처/전체 쓰레기통 비율을 나타내는 제 2주성분이 작은 관악구, 서초구에 대해 쓰레기통이 필요한 위치를 
![PCA](/images/pca_result.png)

## Final
- 관악구  
서울대입구역, 신림역의 경우 ~~~
![Final](/images/관악구_서울대입구역.png)
![Final](/images/관악구_신림역.png)
- 서초구  
서초구
![Final](/images/서초구_교대역.png)
![Final](/images/서초구_양재역.png)
