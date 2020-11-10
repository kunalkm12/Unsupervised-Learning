rm(list = ls())
library(recommenderlab)
### Accessing the dataset
data(MovieLense)

### SPECIFICATION of k
k = 30

### Getting a feel of the matrix
getRatingMatrix(MovieLense)[1:10,1:10]

### Verifying if no of rows and columns match values in the problem statement
nrow(MovieLense)
ncol(MovieLense)

### Normalizing Ratings matrix
dats <- normalize(MovieLense)

### Visualizing both Raw and Normalized Ratings. Limiting no for visual clarity
x11()
image(MovieLense[150:250, 150:250], main = "Raw Ratings")
x11()
image(dats[150:250, 150:250], main = "Normalized Ratings")

### Plotting histograms of the data
x11()
hist(getRatings(dats), breaks = 100, main = "Normalized Ratings Histogram")

x11()
hist(rowCounts(dats), breaks = 100, main = "User Ratings Histogram")

x11()
hist(colCounts(dats), breaks = 100, main = "Ratings per Movie Histogram")

### Calculating values that are going to be used everytime
kk <- as(MovieLense, "matrix")
ss <- similarity(MovieLense, method = "cosine")
sim <- as(ss,"matrix")
final <- matrix(0,943,1664)

## Iterating through all users
for(i in 1:943){
    
      ### Checking for which movie the rating is missing
      for(n in 1:1664){
        if(is.na(kk[i,n])==TRUE){
          j <- n
          break
        }
      }
  
      ### Checking for which users have seen the movie
      users = c()
      for(m in 1:943){
        if(is.na(kk[m,j])==FALSE){
          users <- append(users,m)
        }
      }
      
      ### Taking k to be length of users if no of users that have watched j is less than k
      if(length(users)<k){
        k = length(users)
      }
      
      ### Listing similarity of chosen users and ordering it, followed by k subset
      similar <- sim[i, users]
      ord <- order(-similar)
      ord <- ord[1:k]

      ### Designing user-based recommender system
      model <- Recommender(MovieLense[ord], method = "UBCF")

      ### Predicting rating for user i of movie j
      pred <- predict(model, MovieLense[i], type = "ratingMatrix")
      pre <- as(pred, "matrix")
      
      ### Adding to final matrix
      for(n in 1:1664){
        if(is.na(kk[i,n])==TRUE){
          final[i,n] = pre[n]
        }
        else{
          final[i,n] = kk[i,n]
        }
      }
}


