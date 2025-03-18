##########################################################
# Create edx and final_holdout_test sets 
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


#################
# Create a model 
#################


#installing the recosystem package
if(!require(recosystem)) install.packages("recosystem")
if(!require(parallel)) install.packages("parallel")
library(recosystem)
library(parallel)


#I configure the dataset to only have the variables I need
train_set <- edx %>% select(userId, movieId, rating) %>% data.frame()


#convert the data frame into independent vectors
train_data <- data_memory(train_set[, 1], train_set[, 2], rating = train_set[, 3])


#creating a RecoSys object
r <- Reco()


#hyperparameter tuning
best_tune <- r$tune(train_data, opts = list(dim = c(10L, 20L, 30L),
                                            costp_l1 = c(0, 0.1),
                                            costp_l2 = c(0.01, 0.1),
                                            costq_l1 = c(0, 0.1),
                                            costq_l2 = c(0.01, 0.1),
                                            lrate    = c(0.01, 0.1),
                                            nthread  = detectCores() - 1,
                                            nbin     = detectCores())
)


#training the model with the best parameters that the fit gave
final_model <- r$train(train_data, opts = best_tune$min)


#preparing the evaluation set for prediction just as I did with the training set
test_data <- data_memory(final_holdout_test[, 1], final_holdout_test[, 2])


#predicting ratings for a dataset of users and items
pred2 = r$predict(test_data, out_memory())


#I calculate the RMSE based on the prediction I made and the actual data
RMSE <- sqrt(mean((final_holdout_test[, 3] - pred2)^2))
RMSE
