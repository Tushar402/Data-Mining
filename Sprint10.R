library(amap)
x = c(1,1.1,5,5.1,1.5,5.2,7.9,1.2,8.1,9)
plot(x)
mycluster = kmeans(x,3)
mycluster$size #size tells how many observation in each cluster
mycluster$cluster# after cluster the which observation belongs which cluster
mycluster$centers#mean value for each cluster
mycluster$withinss#sum of square within the cluster how tight are they(observation) together in a cluster
#first cluster is much smaller then second cluster
plot(x, col=mycluster$cluster)
#e_show cluster centers by points
points(mycluster$centers, col=1:3,pch=8,cex=2)
set.seed(42)
mycluster
set.seed(2345)
mycluster
set.seed(363524)
mycluster
set.seed(4333)
mycluster
#depending on the seed value it would possible to creat different clustering
#_2_a
set.seed(43)
library(amap)
data(USArrests)
hc_avg=hcluster(USArrests,link = "ave")#linkage method average,agglomorative method_Check how similar two cluster are
plot(hc_avg)
#d
hc_sin =hcluster(USArrests,link = "single")
hc_com=hcluster(USArrests, link = "complete")
plot(hc_sin)
plot(hc_com)
#using this three linkage method for hyrarical clustering we arrive different dendogram. but we can not say 
#one of the method is better than other one because we dont know the answer here as cluster analysis is unsupervised learningand we dont know the target variable
#so we cant prove this by confusion table, telling misclasification rate. 
a=data(CO2)
names(CO2)
summary(CO2)
plot(CO2)
myclusters= kmeans(CO2$uptake,2)
myclusters$size
myclusters$cluster
myclusters$withinss
plot(myclusters$cluster)
data("anscombe")
summary(anscombe)
names(anscombe)
dim(anscombe)
View(anscombe)
