rm(list=ls())
library(tidyverse)
library(caret)
library(gridExtra)


load(file="data/edx")
load(file="data/validation")
load(file="data/movies")

str(edx)


#Looking for NAs
nas <- function(x) { any(is.na(x)) }
edx %>% summarise_all(nas)
validation %>% summarise_all(nas)

#Overall distribution of the ratings
edx %>% ggplot(aes(x=rating)) + geom_histogram(binwidth = 0.5) + scale_x_continuous(breaks = seq(min(edx$rating), max(edx$rating), by = 0.5))

#The different genres are
dist_genres <- edx %>% distinct(genres)
dg <- as.vector(str_split(dist_genres$genres, "\\|", simplify = T))
unique(dg[dg != ""])

#Distribution of ratings for  different genres
hist_drama <- edx %>% filter(str_detect(genres, "Drama")) %>% ggplot(aes(x=rating)) + geom_histogram(binwidth = 0.5) + scale_x_continuous(breaks = seq(0.5, 5, by = 1)) + labs(title="Drama")

hist_comedy <- edx %>% filter(str_detect(genres, "Comedy"))  %>% ggplot(aes(x=rating)) + geom_histogram(binwidth = 0.5) + scale_x_continuous(breaks = seq(0.5, 5, by = 1)) + labs(title="Comedy")

hist_thriller <- edx %>% filter(str_detect(genres, "Thriller"))  %>% ggplot(aes(x=rating)) + geom_histogram(binwidth = 0.5) + scale_x_continuous(breaks = seq(0.5, 5, by = 1)) + labs(title="Thriller")

hist_romance <- edx %>% filter(str_detect(genres, "Romance"))  %>% ggplot(aes(x=rating)) + geom_histogram(binwidth = 0.5) + scale_x_continuous(breaks = seq(0.5, 5, by = 1)) + labs(title="Romance")

grid.arrange(hist_drama, hist_comedy, hist_thriller,hist_romance, ncol = 2)

#Different users have widely different numbers of ratings
edx %>% count(userId) %>% ggplot(aes(x=n)) + geom_histogram() + scale_x_log10() + xlab("Ratings per user (log)")

#Differet users have different rating means
edx %>% group_by(userId) %>% summarise(user_mean=mean(rating)) %>% ggplot(aes(x=user_mean)) + geom_histogram()  + xlab("Average ratings per user")

#Some movies are rated more often
edx %>% group_by(movieId) %>% summarise(n=n()) %>% ggplot(aes(x=n)) + geom_histogram() + scale_x_log10() + xlab("Ratings per movie (log)")

edx %>% head(3)
#Add release Year
release_year_edx <- as.numeric(str_sub(edx$title, start=-5, end=-2))
edx <- edx %>% mutate(year=release_year_edx)
head(edx)
validation <- validation %>% mutate(year=as.numeric(str_sub(title, start=-5, end=-2)))
head(validation)

edx %>% group_by(year) %>% summarise(avg_rating = mean(rating)) %>% ggplot(aes(x=year, y=avg_rating)) + geom_point() + geom_smooth()

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

mu_rating <- mean(edx$rating)
rmse_mean <- RMSE(validation$rating, mu_rating)
rmse_mean

models <- tibble(model="Mean", rmse=rmse_mean)
models

movie_avgs <- edx %>% group_by(movieId) %>% summarise(b_i = mean(rating - mu_rating))

movie_avgs %>% ggplot(aes(b_i)) + geom_histogram(bins = 10)



head(edx)

edx_movies <- edx %>% left_join(movie_avgs, by="movieId")

head(edx_movies)

validation_movies <- validation %>% left_join(movie_avgs, by="movieId")
head(validation_movies)

predicted_ratings_movies <- mu_rating + validation_movies$b_i


rmse_movies <- RMSE(predicted_ratings_movies, validation$rating)
models <- rbind(models, c("movies", rmse_movies))
models

#user effects

user_avg <- edx_movies %>% group_by(userId) %>% summarise(b_u = mean(rating - mu_rating - b_i)) 
head(user_avg)

edx_movies_users <- edx_movies %>% left_join(user_avg, by="userId")
validation_movies_users <- validation_movies %>% left_join(user_avg, by="userId")


edx_movies_users %>% ggplot(aes(x=b_u)) + geom_histogram()

predicted_ratings_movies_users <- mu_rating + validation_movies_users$b_i + validation_movies_users$b_u
rmse_movies_users <- RMSE(predicted_ratings_movies_users, validation$rating)
rmse_movies_users

models <- rbind(models, c("movies users", rmse_movies_users))
models

#year effect

year_avg <- edx_movies_users %>% group_by(year) %>% summarise(b_y = mean(rating - mu_rating - b_i - b_u))

year_avg %>% ggplot(aes(x=b_y)) + geom_histogram()

edx_movies_users_years <- edx_movies_users %>% left_join(year_avg, by="year")
validation_movies_users_years <- validation_movies_users %>% left_join(year_avg, by="year")

predicted_ratings_movies_users_years <- mu_rating + validation_movies_users_years$b_i + validation_movies_users_years$b_u + validation_movies_users_years$b_y
rmse_movies_users_years <- RMSE(predicted_ratings_movies_users_years, validation$rating)
rmse_movies_users_years

models <- rbind(models, c("movies users years", rmse_movies_users_years))
models

#genre effect

head(edx_movies_users_years)
genre_avg <- edx_movies_users_years %>% group_by(genres) %>% summarise(b_g = mean(rating - mu_rating - b_i - b_u - b_y))
genre_avg %>% ggplot(aes(x=b_g)) + geom_histogram()

edx_movies_users_years_genres <- edx_movies_users_years %>% left_join(genre_avg, by="genres")
validation_movies_users_years_genres <- validation_movies_users_years %>% left_join(genre_avg, by="genres")

predicted_ratings_movies_users_years_genres <- mu_rating + validation_movies_users_years_genres$b_i + validation_movies_users_years_genres$b_u  + validation_movies_users_years_genres$b_y +  + validation_movies_users_years_genres$b_g
rmse_movies_users_years_genres <- RMSE(predicted_ratings_movies_users_years_genres, validation$rating)
rmse_movies_users_years

models <- rbind(models, c("movies users years genres", rmse_movies_users_years_genres))
models

#Regularization

test_lambdas <- function(l) {
  movie_avgs_r <- edx %>% group_by(movieId) %>% summarise(b_i = sum(rating - mu_rating) /(n()+l))
  edx_movies_r <- edx %>% left_join(movie_avgs_r, by="movieId")
  validation_movies_r <- validation %>% left_join(movie_avgs_r, by="movieId")
  
  user_avg_r <- edx_movies_r %>% group_by(userId) %>% summarise(b_u = sum(rating - mu_rating - b_i)/(n()+l))
  edx_movies_users_r <- edx_movies_r %>% left_join(user_avg_r, by="userId")
  validation_movies_users_r <- validation_movies_r %>% left_join(user_avg_r, by="userId")
  
  year_avg_r <- edx_movies_users_r %>% group_by(year) %>% summarise(b_y = sum(rating - mu_rating - b_i - b_u)/(n()+l))
  edx_movies_users_years_r <- edx_movies_users_r %>% left_join(year_avg_r, by="year")
  validation_movies_users_years_r <- validation_movies_users_r %>% left_join(year_avg_r, by="year")
  
  genre_avg_r <- edx_movies_users_years_r %>% group_by(genres) %>% summarise(b_g = sum(rating - mu_rating - b_i - b_u - b_y)/(n()+l))
  edx_movies_users_years_genres_r <- edx_movies_users_years_r %>% left_join(genre_avg_r, by="genres")
  validation_movies_users_years_genres_r <- validation_movies_users_years_r %>% left_join(genre_avg_r, by="genres")
  
  predictions <- mu_rating + validation_movies_users_years_genres_r$b_i + validation_movies_users_years_genres_r$b_u + validation_movies_users_years_genres_r$b_y + validation_movies_users_years_genres_r$b_g
  rmse_movies_users_years <- RMSE(predictions, validation$rating)
  rmse_movies_users_years
}

lambdas <- seq(0,10, 1)
rmses <- sapply(lambdas, test_lambdas)

dfr <- data.frame(lambdas, rmses)
dfr %>% ggplot(aes(x=lambdas, y=rmses)) + geom_point() + labs(x="Lambda", y="RMSE")

#Final Model
#Lambda
l <- lambdas[which.min(rmses)]

movie_avgs_r <- edx %>% group_by(movieId) %>% summarise(b_i = sum(rating - mu_rating) /(n()+l))
edx_movies_r <- edx %>% left_join(movie_avgs_r, by="movieId")
validation_movies_r <- validation %>% left_join(movie_avgs_r, by="movieId")

user_avg_r <- edx_movies_r %>% group_by(userId) %>% summarise(b_u = sum(rating - mu_rating - b_i)/(n()+l))
edx_movies_users_r <- edx_movies_r %>% left_join(user_avg_r, by="userId")
validation_movies_users_r <- validation_movies_r %>% left_join(user_avg_r, by="userId")

year_avg_r <- edx_movies_users_r %>% group_by(year) %>% summarise(b_y = sum(rating - mu_rating - b_i - b_u)/(n()+l))
edx_movies_users_years_r <- edx_movies_users_r %>% left_join(year_avg_r, by="year")
validation_movies_users_years_r <- validation_movies_users_r %>% left_join(year_avg_r, by="year")

genre_avg_r <- edx_movies_users_years_r %>% group_by(genres) %>% summarise(b_g = sum(rating - mu_rating - b_i - b_u - b_y)/(n()+l))
edx_movies_users_years_genres_r <- edx_movies_users_years_r %>% left_join(genre_avg_r, by="genres")
validation_movies_users_years_genres_r <- validation_movies_users_years_r %>% left_join(genre_avg_r, by="genres")

predictions <- mu_rating + validation_movies_users_years_genres_r$b_i + validation_movies_users_years_genres_r$b_u + validation_movies_users_years_genres_r$b_y + validation_movies_users_years_genres_r$b_g
rmse_movies_users_years_genres_reg <- RMSE(predictions, validation$rating)
rmse_movies_users_years_genres_reg

models <- rbind(models, c("movies users years genres regular", rmse_movies_users_years_genres))
models
