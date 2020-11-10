rm(list = ls())
graphics.off()
library("arules")
library("MASS")
library("rpart")
data(Boston)
###################
######## Part 1a
###################

Boston[["crim"]] <- ordered(cut(Boston[["crim"]], c(0, 0.2, 0.6, 1.0, 100.0)), labels = c("Low", "Moderate", "High", "Very High"))
Boston[["zn"]] <- NULL
###################
####THE ZN VARIABLE ISSUE
####Boston[["zn"]] <- ordered(cut(Boston[["zn"]], c(-0.5, 12.0, 25.0, 50.0, 100.0), labels = c("Low", "Moderate", "High", "Very High")))
####Boston[["zn"]] <- ordered(cut(Boston[["zn"]], c(4)), labels = c("Low", "Moderate", "High", "Very High")) THIS WORKS SOMEHOW
###################
Boston[["indus"]] <- ordered(cut(Boston[["indus"]], c(0, 10.0, 18.0, 100.0)), labels = c("Low", "Moderate", "High"))
Boston[["chas"]] <- ordered(cut(Boston[["chas"]], c(2)), labels = c("False", "True"))
Boston[["nox"]] <- ordered(cut(Boston[["nox"]], c(3)), labels = c("Low", "Moderate", "High"))
Boston[["rm"]] <- ordered(cut(Boston[["rm"]], c(0.0, 4.0, 5.0, 6.0, 7.0, 9.0)), labels = c("Very Low", "Low", "Moderate", "High", "Very High"))
Boston[["age"]] <- ordered(cut(Boston[["age"]], c(0.0, 30.0, 60.0, 80.0, 100.0)), labels = c("Modern", "Balanced", "Old", "Very Old"))
Boston[["dis"]] <- ordered(cut(Boston[["dis"]], c(0.0, 2.0, 4.0, 6.0, 8.0, 13.0)), labels = c("Close", "Moderately Close", "Moderately Far", "Far", "Very Far"))
Boston[["rad"]] <- ordered(cut(Boston[["rad"]], c(0.0, 2.0, 4.0, 6.0, 8.0, 24.0)), labels = c("Close", "Moderately Close", "Moderately Far", "Far", "Very Far"))
Boston[["tax"]] <- ordered(cut(Boston[["tax"]], c(0.0, 250.0, 350.0, 500.0, 750.0)), labels = c("Very low", "Low", "Moderate", "High"))
Boston[["ptratio"]] <- ordered(cut(Boston[["ptratio"]], c(0.0, 16.0, 20.0, 22.0)), labels = c("Low", "Moderate", "High"))
Boston[["black"]] <- ordered(cut(Boston[["black"]], c(0, 150.0, 300.0, 400.0)), labels = c("Low", "Moderate", "High"))
Boston[["lstat"]] <- ordered(cut(Boston[["lstat"]], c(0, 10.0, 20.0, 40.0)), labels = c("Low", "Moderate", "High"))
Boston[["medv"]] <- ordered(cut(Boston[["medv"]], c(0, 15.0, 25.0, 35.0, 50.0)), labels = c("Low", "Moderate", "High", "Very High"))

###################
######## Part 1b
###################

B1 <- as(Boston, "transactions")
summary(B1)
x11()
itemFrequencyPlot(B1, support=0.2, cex.names=0.8)
rules  <- apriori(B1, parameter = list(support = 0.12, confidence = 0.44))
summary(rules)

######## Part 1c

rulesStudent1 <- subset(rules, subset = rhs %in% c("crim=Low","dis=Close"))
inspect(head(sort(rulesStudent1, by = "confidence")))

######## Part 1d

rulesFamily <- subset(rules, subset = rhs %in% "ptratio=Low")
inspect(head(sort(rulesFamily, by = "confidence")))
inspect(head(sort(rulesFamily, by = "lift")))

######## Part 1e
#data("Boston")
model.controls <- rpart.control(minbucket = 2, minsplit = 4, xval = 10, cp = 0.01)
fit_boston <- rpart(ptratio~., data = Boston, control = model.controls)
min_cp = which.min(fit_boston$cptable[,4])
pruned_fit_boston <- prune(fit_boston, cp = fit_boston$cptable[min_cp, 1])

x11()
plot(fit_boston$cptable[,4], main = "Cp for model selection", ylab = "cv error")

x11()
plot(pruned_fit_boston, branch = .3, compress=T, main = "Pruned Tree")
text(pruned_fit_boston, cex = .5)


