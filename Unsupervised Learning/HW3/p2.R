rm(list=ls())
library(ISLR)
library(kohonen)

### Importing USArrests data
data(USArrests)
head(USArrests)

### Scaling USArrests data, centering and setting standard deviation 1
scaled <- scale(USArrests)

### Heirarchically clustering data with complete linkage
d <- dist(scaled)
dim(as.matrix(d))
hc <- hclust(d, method="complete")
x11()
plot(hc)

### Cutting dendrogram such that there are 4 clusters
clust <- cutree(hc, k = 4)

### Setting seed so that results can be replicated
### Defining a 4x4 grid for SOM
set.seed(100)
grid <- somgrid(xdim = 4, ydim = 4, topo = "hexagonal")
usa_som <- som(scaled, grid = grid, rlen = 1000)

### Extracting code vectors
codes <- usa_som$codes[[1]]

### Plotting SOM
### Plotting mean average deviation from code vectors
### Plotting no of points assigned to each cell
### Plotting a map based on the average distance
x11()
plot(usa_som)
x11()
plot(usa_som, type = "changes", main = "USArrests Data")
x11()
plot(usa_som, type = "count")
x11()
plot(usa_som, type = "mapping")

### Color palette as described in lab video
coolBlueHotRed <- function(n, alpha = 1){rainbow(n, end=4/6, alpha = alpha)[n:1]}
x11()
plot(usa_som, type = "dist.neighbours", palette.name = coolBlueHotRed)

### Component plane plots
for (i in 1:4){
  x11()
  plot(usa_som, type = "property", property=codes[,i], main = colnames(codes)[i])
}

### Heirarchically clustering distance between code vectors
d2 <- dist(codes)
hc2 <- hclust(d2)
x11()
plot(hc2)

### Cutting tree into 4 clusters based on dendrogram
clust2 <- cutree(hc2, k = 4)

### Color palette
my_pal <- c("red", "blue", "green", "purple", "yellow")
my_bhcol <- my_pal[clust2]

### Boundaries of distinction of clusters
x11()
plot(usa_som, type = "mapping", col = "black", bgcol = my_bhcol)
add.cluster.boundaries(usa_som, clust2)