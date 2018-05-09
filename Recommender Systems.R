############## Content Filtering ##############

### Load the data
rat <- read.csv("~/Downloads/ml-latest-small/ratings.csv",stringsAsFactors = F)
movies <- read.csv("~/Downloads/ml-latest-small/movies.csv",stringsAsFactors = F)
dim(movies)
### Load libraries
library(reshape2)
library(recommenderlab)
library(data.table)
library(tidyr)
### Pre processing
genres <- as.data.frame(movies$genres, stringsAsFactors=FALSE)
genres2
genres2 <- as.data.frame(tstrsplit(genres[,1], '[|]', 
                                   type.convert=TRUE), 
                         stringsAsFactors=FALSE)
colnames(genres2) <- c(1:10)
genres2
## Create a genre list
genre_list <- c("Action", "Adventure", "Animation", "Children", "Comedy", 
                "Crime","Documentary", "Drama", "Fantasy","Film-Noir", 
                "Horror", "Musical", "Mystery","Romance","Sci-Fi", 
                "Thriller", "War", "Western")

genre_matrix <- matrix(0,9125,18) #empty matrix
genre_matrix[1,] <- genre_list #set first row to genre list
colnames(genre_matrix) <- genre_list #set column names to genre list
genre_matrix
#iterate through matrix
for (i in (1:nrow(genres2))-1) {
  for (c in 1:ncol(genres2)) {
    genmat_col = which(genre_list == genres2[i,c])
    genre_matrix[i+1,genmat_col] <- 1
  }
}
genre_matrix <- genre_matrix[-1,]
dim(genre_matrix)
### Create user profile rating matrix based on ratings of 4, 5 as 1 and others as -1
binaryratings <- rat
for (i in 1:nrow(binaryratings)){
  if (binaryratings[i,3] > 3){
    binaryratings[i,3] <- 1
  }
  else{
    binaryratings[i,3] <- -1
  }
}
binaryratings<- binaryratings[,-4] # remove the timestamp
bin_rat2<- spread(binaryratings,userId,rating) # we can use the tidyr package to do the same work
bin_rat2<- as.data.frame(bin_rat2) # need to conver the result to a DF
bin_rat <- dcast(binaryratings,movieId~userId,value.var="rating",na.rm=F) # using reshape package's dcast function

for(i in 1:ncol(bin_rat2))
  bin_rat2[which(is.na(bin_rat2[,i]) == TRUE),i] <- 0

#Remove rows that are not rated from movies dataset
(movieIds <- length(unique(movies$movieId))) #9125
(ratingmovieIds <- length(unique(rat$movieId))) #9066
movies2 <- movies[-which((movieIds %in% ratingmovieIds) == FALSE),]
rownames(movies2) <- NULL
#Remove rows that are not rated from genre_matrix2
genre_matrix3 <- genre_matrix[-which((movieIds %in% ratingmovieIds) == FALSE),]
rownames(genre_matrix3) <- NULL
dim(genre_matrix3)
# we are now going to perform the dot product(matrix multiplication)

result=matrix(0,18,706)
for (c in 1:ncol(bin_rat2)){
  for (i in 1:ncol(genre_matrix3)){
    result[i,c] <- sum((genre_matrix3[,i]) * (bin_rat2[,c]))
  }
}

#convert results to binary
for (i in 1:nrow(result)){
  if (result[i] < 0){
    result[i] <- 0
  }
  else {
    result[i] <- 1
  }
}

#### Validating the model
result2 <- result[1,] #First user's profile
sim_mat <- rbind.data.frame(result2, genre_matrix3)
sim_mat <- data.frame(lapply(sim_mat,function(x){as.integer(x)})) #convert data to type integer

#Calculate Jaccard distance between user profile and all movies
library(proxy)
sim_results <- dist(sim_mat, method = "Jaccard")
sim_results <- as.data.frame(as.matrix(sim_results[1:nrow(movies)]))
rows <- which(sim_results == min(sim_results))
#Recommended movies
movies[rows,2]

############### Collaborative filtering #####################

## convert the format into a matrix
ratingmat <- spread(rat,userId,rating)

#Convert rating matrix into a recommenderlab sparse matrix
ratingmat <- as(ratingmat, "realRatingMatrix")

#Normalize the data
ratingmat_norm <- normalize(ratingmat)


#Create Recommender Model. "UBCF" stands for User-Based Collaborative Filtering
recommender_model <- Recommender(ratingmat, method = "UBCF", 
                                 param=list(method="cosine",nn=30))
recom <- predict(recommender_model, ratingmat[1], n=10) #Obtain top 10 recommendations for 1st user in dataset
recom_list <- as(recom, "list") #convert recommenderlab object to readable list

#Obtain recommendations
recom_result <- matrix(0,10)
for (i in c(1:10)){
  recom_result[i] <- movies[as.integer(recom_list[[1]][i]),2]
}

