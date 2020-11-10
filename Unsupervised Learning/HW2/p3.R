library("multtest")
library("fpc")
library("cluster")
library("bootcluster")
library("fossil")

### Reading the tab separated file
dat <- read.delim("C:/Users/kunal/Desktop/Buffalo/Stat Data Mining II/HW2/seeds_dataset.txt")
head(dat)

### Isolating the seed group column
dats <- dat[ ,1:7]
head(dats)

### Calculating Euclidean distances
d <- dist(dats)

### Performing heirarchical clustering with all 3 linkage methods
hc1 <- hclust(d,method="single")
hc2 <- hclust(d,method="complete")
hc3 <- hclust(d,method="average")

### Single linkage plot
x11()
plot(hc1,hang=-1,main="Single Linkage")

### Complete linkage plot
x11()
plot(hc2,hang=-1,main="Complete Linkage")

### Average linkage plot
x11()
plot(hc3,hang=-1,main="Average Linkage")

### Cutting dendrogram since heirarchical clustering does not make clusters, it makes dendrograms
### If it was kmeans, we'd be able to do km$clustering to get what cluster each observation fell in
### However, without cutting the dendrogram, we cannot decipher that since it is only when I cut the
### dendrogram that I get clusters in the first place
### Next I use the seed group values to evaluate how well the clusters match the labels
### Finally, I record the adjusted rand index as a performance measure

ct1 <- cutree(hc1,k=3)
t1 <- table(ct1,dat$Seed.Group)
adj1 <- adj.rand.index(ct1, as.numeric(dat$Seed.Group))
adj1

ct2 <- cutree(hc2,k=3)
t2 <- table(ct2,dat$Seed.Group)
adj2 <- adj.rand.index(ct2, as.numeric(dat$Seed.Group))
adj2

ct3 <- cutree(hc3,k=3)
t3 <- table(ct3,dat$Seed.Group)
adj3 <- adj.rand.index(ct3, as.numeric(dat$Seed.Group))
adj3

##############################################
### Kmeans and Kmedoids
##############################################

### Bootstrapping cluster stability to find best no of clusters
k.select(dats, range = 2:10, B=50, r= 5, scheme_2=TRUE)
k.select(dats, range = 2:10, B=50, r= 5, scheme_2=FALSE)

### Performing kmeans for 3 clusters
km <- kmeans(dats, centers = 3, nstart = 10)

### Evaluating how well clusters fit labels
table(km$cluster, dat$Seed.Group)

### Recording the adjusted rand index of the kmeans clustering
adj4 <- adj.rand.index(km$cluster, as.numeric(dat$Seed.Group))
adj4