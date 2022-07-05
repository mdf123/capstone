rm(list=ls())
library(tidyverse)
library(caret)
library(gridExtra)
options(digits=3)

load(file="data/edx")
load(file="data/validation")
load(file="data/movies")

mu_rating <- mean(edx$rating)

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

lambdas <- seq(0,10, 0.25)

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(edx$rating)
  
  edx_movies_reg <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  edx_movies_users_reg <- edx %>% 
    left_join(edx_movies_reg, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, validation$rating))
})
lambda_rmse <- data.frame(l=lambdas, rmse=rmses)
lambda_rmse %>% ggplot(aes(x=l, y=rmse)) + geom_point()
