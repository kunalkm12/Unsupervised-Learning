rm(list = ls())
### Downloaded ElemStatLearn from archives and installed via Rstudio
library(ElemStatLearn)
library(kohonen)

### Importing NCI data
data(nci)
head(nci)
scaled<-t(scale(nci))

### Setting seed so all the results and figures can be replicated
### Defining a 6x6 grid for SOM
set.seed(100)
grid <- somgrid(xdim = 5, ydim = 5, topo = "hexagonal")
nci_som <- som(scaled, grid = grid, rlen = 1500)

### Extracting code vectors
codes<-nci_som$codes[[1]]

### Plotting SOM
### Plotting mean average deviation from code vectors
### Plotting no of points assigned to each cell
### Plotting a map based on the average distance
x11()
plot(nci_som, main = "NCI microarray data")

x11()
plot(nci_som, type = "changes", main = "NCI microarray Data")

x11()
plot(nci_som, type = "count")

x11()
plot(nci_som, type = "mapping")

### Color palette as described in lab video
coolBlueHotRed <- function(n, alpha = 1){rainbow(n, end=4/6, alpha = alpha)[n:1]}
x11()
plot(nci_som, type = "dist.neighbours", palette.name = coolBlueHotRed)

### Component plane plots
for (i in 1:64){
  x11()
  plot(nci_som, type = "property", property=codes[,i], main = colnames(codes)[i])
}

### Heirarchically clustering distance between code vectors
d <- dist(codes)
hc <- hclust(d)
x11()
plot(hc)

### Cutting tree into 3 clusters based on dendrogram
clust <- cutree(hc, k = 3)

### Color palette
my_pal <- c("red", "blue", "green", "purple", "yellow")
my_bhcol <- my_pal[clust]

### Boundaries of distinction of clusters
x11()
plot(nci_som, type = "mapping", col = "black", bgcol = my_bhcol)
add.cluster.boundaries(nci_som, clust)