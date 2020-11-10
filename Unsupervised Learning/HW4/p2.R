rm(list = ls())
graphics.off()

######################################################
## See: http://cran.r-project.org/web/views/gR.html
##################################################
library(gRain)
#library(RHugin)
library(Rgraphviz)
library(gRbase)
library(ggm)

##################################################
## Specify the DAG
##################################################
g <- list(~a, ~b, ~c|a, ~d|a:b, ~e|b, ~f|c:a:e, ~g|d:e, ~h|f:g)
chestdag <- dagList(g)

##################################################
## Inquire about d-separation
##################################################
#dSep(as(chestdag, "matrix"), "tub", "smoke", c("dysp", "xray"))
dSep(as(chestdag, "matrix"), "a", "g", c("d"))