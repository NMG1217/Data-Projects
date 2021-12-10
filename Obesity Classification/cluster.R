#clustering
data1=read.table("/Users/zhaojiale/Desktop/study/STAT617/Final/obese6.csv",header=T,sep=",",stringsAsFactors=T)
library(scatterplot3d)
library(NbClust)
library(fpc)
library(cluster)
library('mclust')
str(data1)
par(mfrow=c(1,1))
data = data1[,1:16]
dat = data1
dat.scale = scale(data)
dat.dist = dist(dat.scale)
dat.hclust = hclust(dat.dist)
plot(dat.hclust,labels=dat$NObeyesdad,main="Clustering BMI data")
#decide the number of clusters
nc<-NbClust(data, diss=dat.dist, distance = NULL, method = "complete",
            index = "alllong")
nclust = nlevels(as.factor(nc$Best.partition))
nclust
# draw dendogram with red borders around the 3 clusters
par(mfrow=c(1,1))
plot(dat.hclust,labels=dat$NObeyesdad,main="Clustering BMI data")
rect.hclust(dat.hclust, k=nclust, border="red")
groups = cutree(dat.hclust,nclust)
num = as.matrix(table(groups))
############################## K-means
nc<-NbClust(dat.scale, diss = NULL, distance = "euclidean", method = "kmeans",
            index = "ch")
nclust = nlevels(as.factor(nc$Best.partition))
nclust
fit = kmeans(dat.scale, iter.max=100, 7)
fit

num = fit$size
clusplot(dat.scale, fit$cluster, color=TRUE, shade=TRUE, lines=0)

fit = kmeans(dat.scale, iter.max=100, 3)
plotcluster(dat.scale, fit$cluster)
clusplot(dat.scale, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

###################### mixture model
mfit <- mclustBIC(dat.scale,modelNames = mclust.options("E"))
summary(mfit, dat.scale)$parameters$mean
summary(mfit, dat.scale)

par(mfrow=c(1,1))
plot(mfit)
