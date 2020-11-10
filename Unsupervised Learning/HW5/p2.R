library(gRain)
library(Rgraphviz)
library(gRbase)
library(ggm)
library(gRim)
library(bnlearn)
library(igraph)

### Specifying the DAG
g <- list(~Y|W:X, ~Z|Y, ~X, ~W)
chestdag <- dagList(g)

### Checking the set of independencies for validity
# 1
dSep(as(chestdag, "matrix"), "W", "X", c())

# 2
dSep(as(chestdag, "matrix"), "W", "Z", c("X"))

# 3
dSep(as(chestdag, "matrix"), "Z", "W", c("Y"))

# 4
dSep(as(chestdag, "matrix"), "W", "Y", c())

# 5
dSep(as(chestdag, "matrix"), "X", "Y", c())

# 6
dSep(as(chestdag, "matrix"), "W", "X", c("Z"))

# 7
dSep(as(chestdag, "matrix"), "X", "Z", c("W","Y"))
