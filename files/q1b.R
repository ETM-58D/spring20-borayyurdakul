# Question 1.b)

#area

library(data.table)
library(scatterplot3d)

rm(list = ls())
cat("\f")

nof_samples=1000
data_points=runif(nof_samples*2,-1,1)
data_points=matrix(data_points,ncol=2)
plot(data_points)
euclidean_distance=sqrt((data_points[ ,1]-0)^2 + (data_points[ ,2]-0)^2)
is_in_circle=euclidean_distance<=1
nof_sphere=sum(is_in_circle)
estimated_pi=(nof_sphere/nof_samples)*6
print(estimated_pi)
print(pi)
points(data_points,col=as.numeric(is_in_circle)+1)

#volume

rm(list = ls())
cat("\f")

nof_samples=1000
data_points=runif(nof_samples*3,-1,1)
data_points=matrix(data_points,ncol=3)
scatterplot3d(data_points)
euclidean_distance=sqrt((data_points[ ,1]-0)^2 + (data_points[ ,2]-0)^2+(data_points[ ,3]-0)^2)
is_in_sphere=euclidean_distance<=1
nof_sphere=sum(is_in_sphere)
estimated_pi=nof_sphere/nof_samples*6
print(estimated_pi)
print(pi)
scatterplot3d(data_points,color=as.numeric(is_in_sphere)+1)