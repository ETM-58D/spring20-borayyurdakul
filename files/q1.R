# Question 1.a)

rm(list = ls())
cat("\f")

#dimension_1
nof_samples=1000
data_points1=runif(nof_samples,-1,1)
data_points1=matrix(data_points1,ncol=1)
euclidean_distance1=sqrt((data_points1[ ,1]-0)^2)
less_than_one1=euclidean_distance1<=1
l1=sum(less_than_one1)

#dimension_2
nof_samples=1000
data_points2=runif(nof_samples*2,-1,1)
data_points2=matrix(data_points2,ncol=2)
euclidean_distance2=sqrt((data_points2[ ,1]-0)^2 + (data_points2[ ,2]-0)^2)
less_than_one2=euclidean_distance2<=1
l2=sum(less_than_one2)

#dimension_3
nof_samples=1000
data_points3=runif(nof_samples*3,-1,1)
data_points3=matrix(data_points3,ncol=3)
euclidean_distance3=sqrt((data_points3[ ,1]-0)^2 + (data_points3[ ,2]-0)^2 + (data_points3[,3]-0)^2)
less_than_one3=euclidean_distance3<=1
l3=sum(less_than_one3)

#dimension_4
nof_samples=1000
data_points4=runif(nof_samples*4,-1,1)
data_points4=matrix(data_points4,ncol=4)
euclidean_distance4=sqrt((data_points4[ ,1]-0)^2 + (data_points4[ ,2]-0)^2 + (data_points4[,3]-0)^2 + (data_points4[,4]-0)^2)
less_than_one4=euclidean_distance4<=1
l4=sum(less_than_one4)

#dimension_5
nof_samples=1000
data_points5=runif(nof_samples*5,-1,1)
data_points5=matrix(data_points5,ncol=5)
euclidean_distance5=sqrt((data_points5[ ,1]-0)^2 + (data_points5[ ,2]-0)^2 + (data_points5[,3]-0)^2 + (data_points5[,4]-0)^2 + (data_points5[,5]-0)^2)
less_than_one5=euclidean_distance5<=1
l5=sum(less_than_one5)

#dimension_6
nof_samples=1000
data_points6=runif(nof_samples*6,-1,1)
data_points6=matrix(data_points6,ncol=6)
euclidean_distance6=sqrt((data_points6[ ,1]-0)^2 + (data_points6[ ,2]-0)^2 + (data_points6[,3]-0)^2 + 
                           (data_points6[,4]-0)^2 + (data_points6[,5]-0)^2 + (data_points6[,6]-0)^2)
less_than_one6=euclidean_distance6<=1
l6=sum(less_than_one6)

#dimension_7
nof_samples=1000
data_points7=runif(nof_samples*7,-1,1)
data_points7=matrix(data_points7,ncol=7)
euclidean_distance7=sqrt((data_points7[ ,1]-0)^2 + (data_points7[ ,2]-0)^2 + (data_points7[,3]-0)^2 + 
                           (data_points7[,4]-0)^2 + (data_points7[,5]-0)^2 + (data_points7[,6]-0)^2 + 
                           (data_points7[,6]-0)^2)
less_than_one7=euclidean_distance7<=1
l7=sum(less_than_one7)

#dimension_8
nof_samples=1000
data_points8=runif(nof_samples*8,-1,1)
data_points8=matrix(data_points8,ncol=8)
euclidean_distance8=sqrt((data_points8[ ,1]-0)^2 + (data_points8[ ,2]-0)^2 + (data_points8[,3]-0)^2 + 
                           (data_points8[,4]-0)^2 + (data_points8[,5]-0)^2 + (data_points8[,6]-0)^2 + 
                           (data_points8[,7]-0)^2 + (data_points8[,8]-0)^2)
less_than_one8=euclidean_distance8<=1
l8=sum(less_than_one8)

#dimension_9
nof_samples=1000
data_points9=runif(nof_samples*9,-1,1)
data_points9=matrix(data_points9,ncol=9)
euclidean_distance9=sqrt((data_points9[ ,1]-0)^2 + (data_points9[ ,2]-0)^2 + (data_points9[,3]-0)^2 + 
                           (data_points9[,4]-0)^2 + (data_points9[,5]-0)^2 + (data_points9[,6]-0)^2 + 
                           (data_points9[,7]-0)^2 + (data_points9[,8]-0)^2 + (data_points9[,9]-0)^2)
less_than_one9=euclidean_distance9<=1
l9=sum(less_than_one9)

#dimension_10
nof_samples=1000
data_points10=runif(nof_samples*10,-1,1)
data_points10=matrix(data_points10,ncol=10)
euclidean_distance10=sqrt((data_points10[ ,1]-0)^2 + (data_points10[ ,2]-0)^2 + (data_points10[,3]-0)^2 + 
                            (data_points10[,4]-0)^2 + (data_points10[,5]-0)^2 + (data_points10[,6]-0)^2 + 
                            (data_points10[,7]-0)^2 + (data_points10[,8]-0)^2 + (data_points10[,9]-0)^2 + 
                            (data_points10[,10]-0)^2)
less_than_one10=euclidean_distance10<=1
l10=sum(less_than_one10)

fraction=c(l1,l2,l3,l4,l5,l6,l7,l8,l9,l10)
plot(fraction)

##############################################

# Question 1.b)

#area

library(data.table)
library(scatterplot3d)

rm(list = ls())
cat("\f")

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

##############################################

# Question 1.c)
install.packages("proxy")
require(proxy)
a=matrix(runif(125),ncol=5) # 25 instances, 5 features
b=matrix(rnorm(10),ncol=5) # 2 instances, 5 features
dist_mat=proxy::dist(a,b) # proxy:: makes sure that you use dist from
print(a)
# dist_mat is 25 by 2 matrix. each entry i,j refers to the distance of instance in row i to the instance in column j