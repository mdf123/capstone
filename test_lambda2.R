rm(list=ls())
library(tidyverse)
library(caret)
library(gridExtra)
options(digits=3)

load(file="data/edx")
load(file="data/validation")
load(file="data/movies")

#Add release Year
release_year_edx <- as.numeric(str_sub(edx$title, start=-5, end=-2))
edx <- edx %>% mutate(year=release_year_edx)
validation <- validation %>% mutate(year=as.numeric(str_sub(title, start=-5, end=-2)))

head(edx)

mu_rating <- mean(edx$rating)

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

l <- 1

movie_avgs_r <- edx %>% group_by(movieId) %>% summarise(b_i = sum(rating - mu_rating) /(n()+l))
edx_movies_r <- edx %>% left_join(movie_avgs_r, by="movieId")
validation_movies_r <- validation %>% left_join(movie_avgs_r, by="movieId")
user_avg_r <- edx_movies_r %>% group_by(userId) %>% summarise(b_u = sum(rating - mu_rating - b_i)/(n()+l))
edx_movies_users_r <- edx_movies_r %>% left_join(user_avg_r, by="userId")
validation_movies_users_r <- validation_movies_r %>% left_join(user_avg_r, by="userId")
year_avg_r <- edx_movies_users_r %>% group_by(year) %>% summarise(b_y = sum(rating - mu_rating - b_i - b_u)/(n()+l))
edx_movies_users_years_r <- edx_movies_users_r %>% left_join(year_avg_r, by="year")
validation_movies_users_years_r <- validation_movies_users_r %>% left_join(year_avg_r, by="year")
predictions <- mu_rating + validation_movies_users_years_r$b_i + validation_movies_users_years_r$b_u + validation_movies_users_years_r$b_y
rmse_movies_users_years <- RMSE(predictions, validation$rating)
rmse_movies_users_years

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
options(digits=10)
dfr %>% ggplot(aes(x=lambdas, y=rmses)) + geom_point()
