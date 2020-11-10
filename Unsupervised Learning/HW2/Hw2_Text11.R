library("multtest")
library("fpc")
library("cluster")
library("bootcluster")
library("fossil")

### Reading comma separated dataset with header = F as directed
dats <- read.csv("C:/Users/kunal/Desktop/Buffalo/Stat Data Mining II/HW2/Ch10Ex11.csv", header=F)

### Calculating correlation distance and storing in d in distances form
d <- as.dist(1-cor(dats))

### Heirarchical clustering with all three linkage methods
hc1 <- hclust(d, method = "single")
hc2 <- hclust(d, method = "complete")
hc3 <- hclust(d, method = "average")

### Plotting dendrogram for single linkage method
x11()
plot(hc1)

### Plotting dendrogram for complete linkage method
x11()
plot(hc2)

### Plotting dendrogram for average linkage method
x11()
plot(hc3)

### Principal component analysis
p <- prcomp(t(dats))
ld <- apply(p$rotation, 1, sum)

### Ordering the total loadings in descending order so that
### the most different genes across the two groups are shown
ind <- order(abs(ld), decreasing = T)
head(ind)