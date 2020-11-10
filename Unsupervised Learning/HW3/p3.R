rm(list=ls())
library(kohonen)
load("C:/Users/kunal/Desktop/Buffalo/Stat Data Mining II/HW3/SwissBankNotes.rdata")

### Separating real and counterfeit notes as informed
real <- data.frame(SwissBankNotes[1:100,])
fake <- data.frame(SwissBankNotes[101:200,])

x11()
boxplot(real)

x11()
boxplot(fake)

x11()
boxplot(SwissBankNotes)


### Performing Principal Component Analysis on the 3 combinations
pca1 <- prcomp(real,center = TRUE, scale= TRUE)
pca2 <- prcomp(fake,center = TRUE, scale= TRUE)
pca3 <- prcomp(SwissBankNotes, center = TRUE, scale = TRUE)

###Scoreplots of the PCA on all 3 combinations
x11()
plot(pca1, main = "Real notes")

x11()
plot(pca2, main = "Counterfeit notes")

x11()
plot(pca3, main = "All notes")

### Summarizing all 3 PCAs
summary(pca1)
summary(pca2)
summary(pca3)

### Plotting the biplots of all the PCAs
x11()
biplot(pca1, title = "Real Notes")

x11()
biplot(pca2, title = "Fake Notes")

x11()
biplot(pca3, title = "All notes")