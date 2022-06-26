
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
