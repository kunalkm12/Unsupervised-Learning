library(gRain)
library(Rgraphviz)
library(gRbase)
library(ggm)
library(gRim)
library(bnlearn)
library(igraph)

### Setting working directory so that final data is stored where I can locate it
setwd("C:/Users/kunal/Desktop/Buffalo/Stat Data Mining II/HW4")

### Accessing cad1 data
data(cad1)

### Specifying the directed acyclic graph as per diagram in question
g <- list(~Sex, ~Smoker|Sex, ~SuffHeartF, ~Inherit|Smoker, ~Hyperchol|Smoker:SuffHeartF, ~CAD|Inherit:Hyperchol)
chestdag<- dagList(g)

### Checking for all possible d-separations
dSep(as(chestdag, "matrix"), "Sex", "Smoker", cond = c())
dSep(as(chestdag, "matrix"), "Smoker", "Hyperchol", cond = c())
dSep(as(chestdag, "matrix"), "Smoker", "SuffHeartF", cond = c())
dSep(as(chestdag, "matrix"), "Smoker", "Inherit", cond = c())
dSep(as(chestdag, "matrix"), "SuffHeartF", "Hyperchol", cond = c())
dSep(as(chestdag, "matrix"), "Hyperchol", "CAD", cond = c())
dSep(as(chestdag, "matrix"), "Inherit", "CAD", cond = c())

dSep(as(chestdag, "matrix"), "Inherit", "Hyperchol", cond = c("CAD"))
dSep(as(chestdag, "matrix"), "Smoker", "CAD", cond = c("Inherit"))
dSep(as(chestdag, "matrix"), "Sex", "Inherit", cond = c("Smoker"))
dSep(as(chestdag, "matrix"), "Sex", "Hyperchol", cond = c("Smoker"))
dSep(as(chestdag, "matrix"), "SuffHeartF", "CAD", cond = c("Hyperchol"))

### Plotting graph to verify if it matches that in the question
x11()
plot(chestdag, type = "graphNEL")

### Specifying the CP tables
sx<-c("M","F")
yesno<-c("y","n")
se<- cptable(~Sex, values=c(50,50), levels=sx)
smokersex<- cptable(~Smoker+Sex, values=c(45,55,30,70), levels=yesno)
suffh<- cptable(~SuffHeartF, values=c(5,5), levels=yesno)
inheritsmoker<- cptable(~Inherit+Smoker, values=c(4,6,15,85), levels=yesno)
hypersmosuff<- cptable(~Hyperchol+Smoker+SuffHeartF, values=c(1,0,1,0,1,0,0,1), levels=yesno)
cadinherithyper<- cptable(~CAD+Inherit+Hyperchol, values=c(8,2,6,4,8,2,4,6), levels=yesno)

### Building the network
plist<-compileCPT(se, smokersex, suffh, inheritsmoker, hypersmosuff, cadinherithyper)
g1<- grain(plist)
summary(g1)
x11()
plot(g1)

### Compiling the network
g2<- compile(g1)
summary(g2)
x11()
plot(g2, type="jt")
summary(g2$dag)

### Propagating the network
g3<- propagate(g2)
summary(g3)
x11()
plot(g3, type="jt")
names(g3)

### Setfinding to absorb information particular to the question
g3rev<- setFinding(g3, nodes=c("Sex","Hyperchol"), states=c("F","y"))
g3rev

### probabilistic queries with conditional distribution
querygrain(g3, nodes=c("SuffHeartF","CAD"), type="conditional")
querygrain(g3rev, nodes=c("SuffHeartF","CAD"), type="conditional")

### Simulating new dataset conditional on info from part B
g4 <- simulate.grain(g3, n=25)
g4 <- rbind(g4, c("F", "n", "n", "n", "y", "n"))

### probabilistic queries with conditional distribution
g5 <- table(g4)
save(g5,file = "tab.csv")
querygrain(g3rev, nodes=c("Smoker","CAD"), type="conditional")

### Simulating new dataset conditional on info from previous code of part C
dat<-simulate.grain(g1, n=500)
dat
save(dat, file="bigdata.RData")