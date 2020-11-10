library("multtest")
library("fpc")
library("cluster")
library("bootcluster")
library("fossil")
library("dendextend")

### Importing USArrests data
data("USArrests")

### Calculating Euclidean distance
d <- dist(USArrests)

### Performing heirarchical clustering with complete linkage
hc <- hclust(d,method="complete")
x11()

### Plotting dendrogram
plot(hc, main = "Heirarchical clustering with complete linkage")

### Cutting the tree such that there are 3 clusters
ct <- cutree(hc, k=3)

### Printing what state falls in which cluster
ct

### Silhouette plot to see how good the clusters are
si <- silhouette(ct,dist=d)
x11()
plot(si)

### Scaling the dataset to standard deviation one
sc <- scale(USArrests)

### Euclidean distance
d2 <- dist(sc)

### Heirarchical clustering using complete linkage
hc2 <- hclust(d2,method="complete")
x11()
plot(hc2, main = "Heirarchical clustering of scaled values with complete linkage")

### Cutting dendrogram to get 3 clusters
ct2 <- cutree(hc2, k=3)
ct2

### Silhouette plot
si2 <- silhouette(ct2,dist=d)
x11()
plot(si2)

#######
### Comparing dendrograms, explained in writeup
#######

dend1 <- as.dendrogram(hc)
dend2 <- as.dendrogram(hc2)
dend_list <- dendlist(dend1, dend2)
x11()
dendlist(dend1, dend2) %>%
  untangle(method = "step1side") %>%
    tanglegram()