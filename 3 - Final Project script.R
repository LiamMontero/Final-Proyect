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



###################
# Data Exploratory
###################

if(!require(tibble)) install.packages("tibble")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(lubridate)) install.packages("lubridate")
if(!require(ggthemes)) install.packages("ggthemes")
if(!require(scales)) install.packages("scales")
if(!require(knitr)) install.packages("knitr")
if(!require(kableExtra)) install.packages("kableExtra")
if(!require(dslabs)) install.packages("dslabs")
if(!require(gam)) install.packages("gam")
if(!require(gridExtra)) install.packages("gridExtra")

library(tibble)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(scales)
library(knitr)
library(kableExtra)


# Set a seed for reproducibility
set.seed(1)

# Check the number of rows in the reduced dataset
data.frame(Rows = nrow(edx))


# Show head of dataset edx (first 10 rows)
head(edx, 10)

# Plot distribution of film ratings in edx
ggplot(edx, aes(x = rating)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
  scale_x_continuous(breaks = seq(min(edx$rating)-0.5, max(edx$rating), by = 0.5)) +
  scale_y_continuous(breaks = seq(0, max(length(edx$rating)), by = 500000),labels = function(x) format(x, scientific = FALSE,big.mark=",")) +
  labs(x = "Rating", y = "Number of ratings", caption = "Source: edx data")


library(dplyr)

# Plot Top 10 Films with the most ratings in a table
edx %>%
  group_by(movieId, title) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(desc(count)) %>%
  head(10)


# Convert timestamp to Date format for plotting
edx$date <- as.Date(as.POSIXct(edx$timestamp, origin = "1970-01-01"))

# Create a line plot showing the number of ratings per week
ggplot(edx, aes(x = floor_date(as.Date(as.POSIXct(timestamp, origin = "1970-01-01")), "week"))) +
  geom_line(stat = "count", color = "darkgreen") +
  labs(x = "Year", 
       y = "Number of ratings", 
       caption = "Source: edx data") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(trans = "sqrt", labels = function(x) format(x, scientific = FALSE,big.mark=","))


# user_prep: Preparing data by extracting release year from movie titles
edx$released <- str_extract(edx$title, "\\((\\d{4})\\)") %>% 
  str_remove_all("[()]") %>% 
  as.numeric()


# user_plots: Creating two side-by-side histograms for average rating per user and number of ratings per user
library(gridExtra)

grid.arrange(
  
  # Plot 1: Histogram of average rating per user
  edx %>% group_by(userId) %>%
    summarise(ave_rating = sum(rating)/n()) %>%
    ggplot(aes(ave_rating)) +
    geom_histogram(bins=30, fill = "skyblue", color = "black") +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE,big.mark=",")) +
    labs(x = "Average rating", y = "Number of users", caption = "Source: edx data"),
  
  # Plot 2: Histogram of number of ratings per user
  edx %>% 
    count(userId) %>% 
    ggplot(aes(n)) + 
    geom_histogram(bins=30, fill = "skyblue", color = "black") +
    scale_x_log10(labels = function(x) format(x, scientific = FALSE,big.mark=",")) +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE,big.mark=",")) +
    labs(x = "Users", y = "Number of ratings", caption = "Source: edx data"),
  
  # Arrange plots in 2 columns and set width  
  ncol = 2, 
  widths = c(1, 1)
)


# time_dependent_plots: Plot weekly averaged rating vs. date and averaged rating vs. release year
library(gridExtra)

grid.arrange(
  # Scatter plot of weekly averaged rating over time
  edx %>% 
    ggplot(aes(date, rating)) +
    geom_point(data = edx %>% 
                 mutate(date = round_date(date, unit = "week")) %>%
                 group_by(date) %>%
                 summarize(rating = mean(rating)),
               aes(x = date, y = rating), 
               alpha = 0.5, color = "black", size = 0.7) +
    labs(x = "Date", y = "Rating", caption = "Source: edx data") +
    coord_cartesian(ylim = c(3, 4)),
  
  # Scatter plot of averaged rating by release year
  edx %>%
    group_by(released) %>%
    summarize(rating = mean(rating)) %>%
    ggplot(aes(released, rating)) +
    geom_point(size = 0.7) +
    labs(x = "Released", y = "Rating", caption = "Source: edx data"),
  
  ncol = 2, 
  widths = c(1, 1)
)




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
