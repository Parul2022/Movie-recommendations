library(ggplot2)
library(dplyr)
library(plyr)

unzip("./RS/Movie.zip")

movie <- read.delim("./Recommender/movies.dat", sep = ":", colClasses = c(NA,"NULL"), header = F)
names(movie) <- c("movieID","title","genres")

users <- read.delim("./Recommender/users.dat", sep = ":", colClasses = c(NA,"NULL"), header = F)
names(users) <- c("userID", "gender", "age", "occupation","zip-code")

ratings <- read.delim("./Recommender/ratings.dat", sep = ":", colClasses = c(NA,"NULL"), header = F)
names(ratings) <- c("userID","movieID","rating","timestamp")

colSums(is.na(movie))
colSums(is.na(users))
colSums(is.na(ratings))

##Data Exploration

review_count <- ratings %>% group_by(userID) %>% tally()
a <- data.frame(cbind(review_count$userID, review_count$n))
g <- ggplot(h,aes(review_count$userID, review_count$n))
g <- g + geom_line()
g

min(review_count$n)
max(review_count$n)

#All users made some rating, min rating made by ant user is 20 and max is 2314

#Average rating per user

avg_user_rating <- ratings %>% group_by(userID) %>% summarise_each(funs(mean(.)))
#j <- ddply(ratings, .(userID), summarize, avg = mean(rating))

min(avg_user_rating$rating)
max(avg_user_rating$rating)

#Reviews per movie

movie_review <- ratings %>% group_by(movieID) %>% tally()
b <- data.frame(cbind(movieID=movie_review$movieID,Total=movie_review$n))
min(movie_review$n)
max(movie_review$n)

#Min review received by any movie is 1 and maximum is 3428

#Average rating per movie

avg_movie_rating <- ddply(ratings, .(movieID), summarize, avg = mean(rating))
min(avg_movie_rating$avg)
max(avg_movie_rating$avg)

#Min average rating given to any movie is 1 and maximum is 5

#Movie with the highest reviews
m <- merge(b,movie, by="movieID")
m <- m[,-4]
m[which(m$Total==max(movie_review$n)),3]

df <- data.frame(cbind(count= movie_review$n, review= avg_movie_rating$avg))

g <- ggplot(df, aes(x=df$count, y= df$review))
g <- g + geom_point()
g <- g + geom_smooth(method = "lm")
g

##Recommendation System

#Finding common reviewer for every pair of movie

common.reviewers <- function(m.id1,m.id2){
  c1 <- ratings[ratings$movieID==m.id1,1]
  c2 <- ratings[ratings$movieID==m.id2,1]
  cm <- c1[!is.na(match(c1,c2))]
  cm
}

pairs <- expand.grid(movie_review$movieID,movie_review$movieID)
dim(pairs)

#Total movie pairs are way to much to handle
#Hence we will take into consideration movies that have higher that 2500 review to build our recommendation system

m.review.new <- movie_review$n[movie_review$n>2500]
new.pairs <- expand.grid(m.review.new,m.review.new)
dim(new.pairs)

m.id.new <- movie_review$movieID[movie_review$n>2500]

cs <- NULL

for(i in 1:nrow(new.pairs)){
  cs[i] <- length(common.reviewers(new.pairs[i,1],new.pairs[i,2]))
}

min(cs)
max(cs)
mean(cs)

new.pairs$comsupp <- cs

#Calculating similarity for each par of movie

library(Hmisc)

sim <- function(m.id1, m.id2){
  cr <-common.reviewers(m.id1,m.id2)
  n <- length(cr)
  if(n<=4){
    sc <- NA
  }
  else{
    v1 <- ratings[ratings$userID %in% cr & ratings$movieID==m.id1,3]
    v1 <- v1 - mean(v1)
    v2 <- ratings[ratings$userID %in% cr & ratings$movieID==m.id2,3]
    v2 <- v2 - mean(v2)
    sm.matrix <- rcorr(v1,v2)
    sc <- sm.matrix$P[1,2]
  }
  sc
}

similar.score <- NULL
comm.review <- NULL

for(i in 1:nrow(new.pairs)){
  m.id1 <- new.pairs[i,1]
  m.id2 <- new.pairs[i,2]
  comm.review[i] <- length(common.reviewers(m.id1, m.id2))
  similar.score[i] <- sim(m.id1, m.id2)
}

df <- new.pairs
names(df) <- c("movie1", "movie2", "commsupp")
df$similarity <- similar.score

df <- df[order(df$movie1,df$movie2),]

k.nearest <- function(m.id1, k){
  a <- df[df$movie1==m.id1,]
  a[order(-a$similarity),2]
}

##Generating recommendations

#For making recommendations we would only be using movies having higher no. of ratings than a specific value due to limitation of machine

new.movie <- movie
new.movie$movieID <- as.numeric(as.character(new.movie$movieID))
new.movie <- new.movie[!is.na(new.movie$movieID),]
new.movie <- new.movie[new.movie$movieID %in% m.id.new,]
dim(new.movie)

#Combining new.movie and ratings data frame

combi <- right_join(ratings,new.movie)
dim(combi)

combi <- combi[order(combi$userID),]

levels(combi$genres)[ levels(combi$genres)== ""] <- "NA"
combi$genres <- as.factor(as.character(combi$genres))

#Generating a vector of all different genre present in data available

gen <- as.character(unique(combi$genres)) 
gen <- toString(gen) 
gen <- gsub(",", "", a) 
gen <- gsub("-","",t)
gen <- gsub("[[:punct:]]", " ", s)
gen <- unique(strsplit(r, " ")[[1]])

# Function to create a data frame of genre preference of each user

m <- NULL

user.genre <- function(u.id){

  user.data <- combi[combi$userID== u.id, ]
  x <- as.character(user.data$genres)
  
  for(j in 1:length(gen)){
  
    sum <- 0
    n <- 0
    
    for(i in 1:(dim(user.data)[1])){
      
      x <- as.character(user.data$genres)
      x[i] <- gsub("-","",x[i])
      x[i] <- gsub("[[:punct:]]", " ", x[i])
      y <- unlist(strsplit(x[i], " "))
     
      if(gen[j] %in% y){
        sum <- sum + user.data[i,"rating"]
        n <- n + 1
      } 
    }
  m[j] <- sum/n
  
  }
  
  gen.preference <- data.frame(cbind(genre= gen, mean= m))
  gen.preference
}










































































