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