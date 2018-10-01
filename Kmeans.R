#Load the Library

library(NbClust)
library(cluster)

#K-Means

data<- read.csv("D:/Studies/VIII Semester/R/Programs/Ex 10/Candidates.csv", header=TRUE)
head(data)
data_2<-data[-5]
head(data_2)

sapply(data,mean)
sapply(data,sd)
sapply(data_2,mean)
sapply(data_2,sd)

# fitting the clusters
data_kmeans<-kmeans(data_2,centers = 2, nstart = 5)
data_kmeans$centers
data_kmeans$size
data$clstr<-data_kmeans$cluster

# cross-validation with original species available in data

data$clstr<-data_kmeans$cluster

fviz_cluster(data_kmeans, data = data_2)

pam.res <- pam(data_2, 2)
print(pam.res)


# Cluster numbers

head(pam.res$clustering)
fviz_cluster(pam.res, 
             palette = c("#00AFBB", "#FC4E07"), # color palette
             ellipse.type = "t", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_classic())

#K-Medoids

#Cluster using PAMK

library(fpc)
data<- read.csv("D:/Studies/VIII Semester/R/Programs/Ex 10/Candidates.csv", header=TRUE)
data2<-data
data2$Name<-NULL
data2$Category<-NULL
data2$Gender<-NULL
pamk.result<-pamk(data2)
pamk.result$nc

layout(matrix(c(1,2),1,2))
plot(pamk.result$pamobject)

#Cluster using PAM

library(cluster)
data2<-data
data2$Name<-NULL
data2$Category<-NULL
data2$Gender<-NULL
pam.result<-pam(data2,3)


layout(matrix(c(1,2),1,2))
plot(pam.result)


