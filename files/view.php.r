require(MASS)
set.seed(1)
nofSamples=100
mu=c(0,0)
covMat=matrix(0,2,2)
diag(covMat)=1

covMat[1,2]=0.8
covMat[2,1]=0.8

dat=mvrnorm(nofSamples,mu,covMat)
plot(dat)
colMeans(dat)

pca=princomp(dat)
plot(pca)

str(pca)
summary(pca)

par(mfrow=c(1,2))
plot(dat,ylim=c(-4,4),xlim=c(-4,4))
plot(pca$scores,ylim=c(-4,4),xlim=c(-4,4))

plot(pca)


require(mlbench)

spiraldat=mlbench.spirals(nofSamples, cycles=1, sd=0.2)
pcaspiral=princomp(spiraldat$x)
plot(pcaspiral)


require(data.table)
distanceMat=fread('/home/baydogan/Courses/IE582/InClass_MDS_distance_city.csv')

distanceMat[is.na(distanceMat)]=0

mds=cmdscale(distanceMat)
plot(mds[,1],mds[,2],main='Location',xlab='', ylab='',col=0)
text(mds[,1],mds[,2],names(distanceMat),cex = .75,pos=4)

euclideanMDS=dist(mds)
sum((as.matrix(euclideanMDS)-as.matrix(distanceMat))^2)
