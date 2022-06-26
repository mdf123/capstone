library(tidyverse)
library(gridExtra)

##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")


# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

str(edx)

head(edx)

#check for columns containing NA values
nas <- function(x) ( any(is.na(x)) )
edx %>% summarise_all(nas)
validation %>% summarise_all(nas)

edx %>% summarize(n_users = n_distinct(userId),
          n_movies = n_distinct(movieId))

edx %>% ggplot(aes(x=rating)) + geom_histogram(binwidth = 0.5) + scale_x_continuous(breaks = seq(min(edx$rating), max(edx$rating), by = 0.5))





hist_drama <- edx %>% filter(str_detect(genres, "Drama")) %>% ggplot(aes(x=rating)) + geom_histogram(binwidth = 0.5) + scale_x_continuous(breaks = seq(0.5, 5, by = 1)) + labs(title="Drama")
#3910127
#Comedy:
hist_comedy <- edx %>% filter(str_detect(genres, "Comedy"))  %>% ggplot(aes(x=rating)) + geom_histogram(binwidth = 0.5) + scale_x_continuous(breaks = seq(0.5, 5, by = 1)) + labs(title="Comedy")
#3540930
#Thriller:
hist_thriller <- edx %>% filter(str_detect(genres, "Thriller"))  %>% ggplot(aes(x=rating)) + geom_histogram(binwidth = 0.5) + scale_x_continuous(breaks = seq(0.5, 5, by = 1)) + labs(title="Thriller")
#2325899
#Romance:
hist_romance <- edx %>% filter(str_detect(genres, "Romance"))  %>% ggplot(aes(x=rating)) + geom_histogram(binwidth = 0.5) + scale_x_continuous(breaks = seq(0.5, 5, by = 1)) + labs(title="Romance")

grid.arrange(hist_drama, hist_comedy, hist_thriller,hist_romance, ncol = 2)


edx %>% count(userId) %>% ggplot(aes(x=n)) + geom_histogram() + scale_x_log10() + xlab("Ratings per user (log)")
edx %>% count(userId)  %>% which.min(n)

edx %>% dplyr::count(movieId) %>% ggplot(aes(x=n)) + geom_histogram() + scale_x_log10() + xlab("Ratings per movie (log)")
edx %>% group_by(movieId) %>% summarise(n=n()) %>% ggplot(aes(x=n)) + geom_histogram() + scale_x_log10() + xlab("Ratings per movie (log)")

head(edx)