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

#dimension 1
require(proxy)
a=matrix(runif(1000),ncol=1) 
b=matrix(rnorm(100),ncol=1) 
dist_mat=proxy::dist(a,b)
min1_1=min(dist_mat[,1])
min1_1
x1=c(min1_1)
average1=ave(x1)
average1=as.matrix(average1)
x1_1=average1[1,]
x1_1
#dimension 2
a2=matrix(runif(2000),ncol=2) 
b2=matrix(rnorm(200),ncol=2) 
dist_mat2=proxy::dist(a2,b2)
min2_1=min(dist_mat2[,1])
min2_1
min2_2=min(dist_mat2[,2])
min2_2
x2=c(min2_1,min2_2)
average2=ave(x2)
average2=as.matrix(average2)
x2_1=average2[1,]
x2_1
#dimension 3
a3=matrix(runif(3000),ncol=3) 
b3=matrix(rnorm(300),ncol=3) 
dist_mat3=proxy::dist(a3,b3)
min3_1=min(dist_mat3[,1])
min3_1
min3_2=min(dist_mat3[,2])
min3_2
min3_3=min(dist_mat3[,3])
min3_3
x3=c(min3_1,min3_2,min3_3)
average3=ave(x3)
average3=as.matrix(average3)
x3_1=average3[1,]
x3_1
#dimension 4
a4=matrix(runif(4000),ncol=4) 
b4=matrix(rnorm(400),ncol=4) 
dist_mat4=proxy::dist(a4,b4)
min4_1=min(dist_mat4[,1])
min4_1
min4_2=min(dist_mat4[,2])
min4_2
min4_3=min(dist_mat4[,3])
min4_3
min4_4=min(dist_mat4[,4])
min4_4
x4=c(min4_1,min4_2,min4_3,min4_4)
average4=ave(x4)
average4=as.matrix(average4)
x4_1=average4[1,]
x4_1
#dimension 5
a5=matrix(runif(5000),ncol=5) 
b5=matrix(rnorm(500),ncol=5) 
dist_mat5=proxy::dist(a5,b5)
min5_1=min(dist_mat5[,1])
min5_1
min5_2=min(dist_mat5[,2])
min5_2
min5_3=min(dist_mat5[,3])
min5_3
min5_4=min(dist_mat5[,4])
min5_4
min5_5=min(dist_mat5[,5])
min5_5
x5=c(min5_1,min5_2,min5_3,min5_4,min5_5)
average5=ave(x5)
average5=as.matrix(average5)
x5_1=average5[1,]
x5_1
#dimension 6
a6=matrix(runif(6000),ncol=6) 
b6=matrix(rnorm(600),ncol=6) 
dist_mat6=proxy::dist(a6,b6)
min6_1=min(dist_mat6[,1])
min6_1
min6_2=min(dist_mat6[,2])
min6_2
min6_3=min(dist_mat6[,3])
min6_3
min6_4=min(dist_mat6[,4])
min6_4
min6_5=min(dist_mat6[,5])
min6_5
min6_6=min(dist_mat6[,6])
min6_6
x6=c(min6_1,min6_2,min6_3,min6_4,min6_5,min6_6)
average6=ave(x6)
average6=as.matrix(average6)
x6_1=average6[1,]
x6_1
#dimension 7
a7=matrix(runif(7000),ncol=7) 
b7=matrix(rnorm(700),ncol=7) 
dist_mat7=proxy::dist(a7,b7)
min7_1=min(dist_mat7[,1])
min7_1
min7_2=min(dist_mat7[,2])
min7_2
min7_3=min(dist_mat7[,3])
min7_3
min7_4=min(dist_mat7[,4])
min7_4
min7_5=min(dist_mat7[,5])
min7_5
min7_6=min(dist_mat7[,6])
min7_6
min7_7=min(dist_mat7[,7])
min7_7
x7=c(min7_1,min7_2,min7_3,min7_4,min7_5,min7_6,min7_7)
average7=ave(x7)
average7=as.matrix(average7)
x7_1=average7[1,]
x7_1
#dimension 8
a8=matrix(runif(8000),ncol=8) 
b8=matrix(rnorm(800),ncol=8) 
dist_mat8=proxy::dist(a8,b8)
min8_1=min(dist_mat8[,1])
min8_1
min8_2=min(dist_mat8[,2])
min8_2
min8_3=min(dist_mat8[,3])
min8_3
min8_4=min(dist_mat8[,4])
min8_4
min8_5=min(dist_mat8[,5])
min8_5
min8_6=min(dist_mat8[,6])
min8_6
min8_7=min(dist_mat8[,7])
min8_7
min8_8=min(dist_mat8[,8])
min8_8
x8=c(min8_1,min8_2,min8_3,min8_4,min8_5,min8_6,min8_7,min8_8)
average8=ave(x8)
average8=as.matrix(average8)
x8_1=average8[1,]
x8_1
#dimension 9
a9=matrix(runif(9000),ncol=9) 
b9=matrix(rnorm(900),ncol=9) 
dist_mat9=proxy::dist(a9,b9)
min9_1=min(dist_mat9[,1])
min9_1
min9_2=min(dist_mat9[,2])
min9_2
min9_3=min(dist_mat9[,3])
min9_3
min9_4=min(dist_mat9[,4])
min9_4
min9_5=min(dist_mat9[,5])
min9_5
min9_6=min(dist_mat9[,6])
min9_6
min9_7=min(dist_mat9[,7])
min9_7
min9_8=min(dist_mat9[,8])
min9_8
min9_9=min(dist_mat9[,9])
min9_9
x9=c(min9_1,min9_2,min9_3,min9_4,min9_5,min9_6,min9_7,min9_8,min9_9)
average9=ave(x9)
average9=as.matrix(average9)
x9_1=average9[1,]
x9_1
#dimension 10
a10=matrix(runif(10000),ncol=10) 
b10=matrix(rnorm(1000),ncol=10) 
dist_mat10=proxy::dist(a10,b10)
min10_1=min(dist_mat10[,1])
min10_1
min10_2=min(dist_mat10[,2])
min10_2
min10_3=min(dist_mat10[,3])
min10_3
min10_4=min(dist_mat10[,4])
min10_4
min10_5=min(dist_mat10[,5])
min10_5
min10_6=min(dist_mat10[,6])
min10_6
min10_7=min(dist_mat10[,7])
min10_7
min10_8=min(dist_mat10[,8])
min10_8
min10_9=min(dist_mat10[,9])
min10_9
min10_10=min(dist_mat10[,10])
min10_10
x10=c(min10_1,min10_2,min10_3,min10_4,min10_5,min10_6,min10_7,min10_8,min10_9,min10_10)
average10=ave(x10)
average10=as.matrix(average10)
x10_1=average10[1,]
x10_1
average= c(x1_1,x2_1,x3_1,x4_1,x5_1,x6_1,x7_1,x8_1,x9_1,x10_1)
plot(average)
##############################################

# Question 2)

require("data.table")
boston=fread("/Users/Boray/Downloads/boston/boston.txt")
names(boston) = c("CRIM","ZN","INDUS","CHAS","NOX","RM","AGE","DIS","RAD","TAX","PTRATIO","B","LSTAT","MEDV")
boston
plot(boston,col=2,pch=".",cex=7)
plot
summary(boston)
cor(boston)
pca=princomp(boston,cor=T)
summary(pca,loadings=T)
barplot(pca$scores[,6])

##############################################

# Question 3)

library(dplyr)
library(data.table)
library(imputeTS)

rm(list = ls())
cat("\f")

ratings=fread("/Users/Boray/Downloads/ETM58D_Spring20_HW1_q3_data/ETM58D_Spring20_HW1_q3_Netflix_data.txt")


names(ratings)= c("Miss Congeniality",
                  "Independence Day",
                  "The Patriot",
                  "The Day After Tomorrow",
                  "Pirates of the Caribbean: The Curse of the Black Pearl",
                  "Pretty Woman",
                  "Forrest Gump",
                  "The Green Mile",
                  "Con Air",
                  "Twister",
                  "Sweet Home Alabama",
                  "Pearl Harbor",
                  "Armageddon",
                  "The Rock",
                  "What Women Want",
                  "Bruce Almighty",
                  "Ocean's Eleven",
                  "The Bourne Identity",
                  "The Italian Job",
                  "I Robot",
                  "American Beauty",
                  "How to Lose a Guy in 10 Days",
                  "Lethal Weapon 4",
                  "Shrek 2",
                  "Lost in Translation",
                  "Top Gun",
                  "Pulp Fiction",
                  "Gone in 60 Seconds",
                  "The Sixth Sense",
                  "Lord of the Rings: The Two Towers",
                  "Men of Honor",
                  "Gladiator",
                  "Lord of the Rings: The Fellowship of the Ring",
                  " Sister Act",
                  "Double Jeopardy",
                  "Two Weeks Notice",
                  "Troy",
                  "The Royal Tenenbaums",
                  "National Treasure",
                  "50 First Dates",
                  "Indiana Jones and the Last Crusade",
                  "My Big Fat Greek Wedding",
                  "Mystic River",
                  "Titanic",
                  "Dirty Dancing",
                  "Catch Me If You Can",
                  "Finding Nemo (Widescreen)",
                  "The Matrix",
                  "Kill Bill: Vol. 1",
                  "The Wedding Planner",
                  "The Shawshank Redemption Special Edition",
                  "The Last Samurai",
                  "John Q",
                  "Swordfish",
                  "The Bourne Supremacy",
                  "The Terminal",
                  "Men in Black II",
                  "Spider-Man 2",
                  "Braveheart",
                  "Men in Black",
                  "Ghost",
                  " Air Force One",
                  "Lord of the Rings: The Return of the King",
                  "Man on Fire",
                  "The Incredibles",
                  "Mr Deeds",
                  "Collateral",
                  "Spider-Man",
                  "Saving Private Ryan",
                  "Erin Brockovich",
                  "Monsters Inc.",
                  "Shrek (Full-screen)",
                  "The Silence of the Lambs",
                  "Memento",
                  "Tomb Raider",
                  "Ferris Bueller's Day Off",
                  "Maid in Manhattan",
                  "Entrapment",
                  "Meet the Parents",
                  "Dodgeball A True Underdog Story",
                  "Rain Man",
                  "Patch Adams",
                  "Big Fish",
                  "Fight Club",
                  " S.W.A.T.",
                  "Good Will Hunting",
                  "A Few Good Men",
                  "Enemy of the State",
                  "The General's Daughter",
                  "Minority Report",
                  "Something's Gotta Give",
                  "Raiders of the Lost Ark",
                  "Anger Management",
                  "Sideways",
                  "Kill Bill: Vol. 2",
                  "American Pie",
                  "The Fast and the Furious",
                  "The School of Rock",
                  "Napoleon Dynamite",
                  "The Notebook")

summary(ratings)
str(ratings)
ratings[ratings==0] = NA_real_
str(ratings)
ratings = as.data.frame(ratings)
ratings = ratings %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))          
str(ratings)
ratings = t(ratings)
abc=dist(ratings)
mat_ratings=as.matrix(abc)
mds_coord=cmdscale(abc,2)

plot(mds_coord)
text(mds_coord,names(abc))