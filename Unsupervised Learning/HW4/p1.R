rm(list = ls())
library(igraph)
library(igraphdata)
data(karate)
data(kite)

set.seed(100)
### Plotting initial graph of karate dataset and setting percentage
x11()
plot(karate)
perc <- 0.4

### Calculating number of edges of karate dataset and recording original edges
nedge <- ecount(karate)
orig <- E(karate)

### Calculating no of edges to be deleted
deleted <- round(nedge*perc,0)

### Sampling random edges from the graph
dats <- sample(1:nedge, deleted)

### Removing the edges from the graph, recording new edges and plotting it
g <- karate - edge(dats)
new <- E(g)
deletededges <- karate - g
removed <- E(deletededges)
x11()
plot(g)

### Fitting heirarchical random graph and plotting dendrogram
ghrg <- fit_hrg(g)
x11()
plot_dendrogram(ghrg)

### Predicting missing edges and plotting probabilities
pred <- predict_edges(g)
x11()
plot(pred$prob)

### Adding predicted edges as per no deleted and coloring
E(g)$color <- "black"
lay <- layout_nicely(g)
g2 <- add_edges(g, t(pred$edges[1:deleted, ]), color = "green")
x11()
plot(g2)
newedges <- g2 - g
added <- E(newedges)

### Checking if the new is different from original, and how much
identical(added,removed)
dif <- difference(karate,g2)
dif2 <- difference(g2,karate)
x11()
plot(dif)
x11()
plot(dif2)






set.seed(100)
### Plotting initial graph of kite dataset and setting percentage
x11()
plot(kite)
perc <- 0.4

### Calculating number of edges of karate dataset and recording original edges
nedge <- ecount(kite)
orig <- E(kite)

### Calculating no of edges to be deleted
deleted <- round(nedge*perc,0)

### Sampling random edges from the graph
dats <- sample(1:nedge, deleted)

### Removing the edges from the graph, recording new edges and plotting it
g <- kite - edge(dats)
new <- E(g)
deletededges <- kite - g
removed <- E(deletededges)
x11()
plot(g)

### Fitting heirarchical random graph and plotting dendrogram
ghrg <- fit_hrg(g)
x11()
plot_dendrogram(ghrg)

### Predicting missing edges and plotting probabilities
pred <- predict_edges(g)
x11()
plot(pred$prob)

### Adding predicted edges as per no deleted and coloring
E(g)$color <- "black"
lay <- layout_nicely(g)
g2 <- add_edges(g, t(pred$edges[1:deleted, ]), color = "purple")
x11()
plot(g2)
newedges <- g2 - g
added <- E(newedges)

### Checking if the new is different from original, and how much
identical(added,removed)
dif <- difference(karate,g2)
dif2 <- difference(g2,karate)
x11()
plot(dif)
x11()
plot(dif2)