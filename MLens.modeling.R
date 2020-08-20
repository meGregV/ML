if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

####################################
# 1. Create edx train and test sets
####################################
  # 1.0 pulling edx and validation sets are a bit computationally expensive.
  # therefore, they are pre-saved to project dir and will be read from project on re-run. 
  # runs the initial block code to get data if no files saved to dir
  if (!file.exists('edx.RDS') || !file.exists('validation.RDS')){
      # MovieLens 10M dataset:
      # https://grouplens.org/datasets/movielens/10m/
      # http://files.grouplens.org/datasets/movielens/ml-10m.zip
      
      dl <- tempfile()
      download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
      
      ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                       col.names = c("userId", "movieId", "rating", "timestamp"))
      
      movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
      colnames(movies) <- c("movieId", "title", "genres")
      
      if (as.numeric(version$year) < 2020 | (version$year=="2020" & as.numeric(version$month) < 3)){
        # if using R 3.6 or earlier
        movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                                   title = as.character(title),
                                                   genres = as.character(genres))
      } else {
        # if using R 4.0 or later
        movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                                   title = as.character(title),
                                                   genres = as.character(genres))}
      
      movielens <- left_join(ratings, movies, by = "movieId")
      
      if (as.numeric(version$year) < 2019 | (version$year=="2019" & as.numeric(version$month) < 4)){
        # if using R 3.5 or earlier
        set.seed(1)
      }else{
        set.seed(1, sample.kind="Rounding")}
      
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
      
      # ADDED code
      # save edx and validation files within the project to cut down Rmd processing time
      saveRDS(edx, file = 'edx.RDS')
      saveRDS (validation, file = 'validation.RDS')
    }

edx <- readRDS("edx.RDS")
validation <- readRDS('validation.RDS')


set.seed(1, sample.kind="Rounding")
  # 1.1  test set will be 10% of edx data
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
  edx_train <- edx[-test_index,]
  temp <- edx[test_index,]
  # 1.2 Make sure userId and movieId in edx_test set are also in edx_train
  edx_test <- temp %>% 
    semi_join(edx_train, by = "movieId") %>%
    semi_join(edx_train, by = "userId")
  # 1.3 Add 17 obs. removed from edx_test back into edx_train set
  removed <- anti_join(temp, edx_test)
  edx_train <- rbind(edx_train, removed)
  # 1.4 cleanup the temp variables
  rm(test_index, temp, removed) 
  
###############################################
# 2. Create baseline model with variations
# Residual Mean Squared Error, RMSE > 1 implies
# an error > 1 star => no good
# RMSE target < .85 
###############################################  
target <- 0.8649

  # 2.1 RMSE function to calc prediction accuracy 
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

  # 2.2 baseline model:  mean is MLE under normal dist assumptions
mu <- mean(edx_train$rating)
results <- tibble(model = "Mean",
                       RMSE = RMSE(edx_test$rating, mu),
                       "Improvement vs Target" = target - RMSE)

  # 2.3 baseline  + movie effects
    movie_avgs <- edx_train %>% 
      group_by(movieId) %>% 
      summarize(b_m = mean(rating - mu))
    # 2.3a visual of movie effects distribution
      qplot(b_m, data = movie_avgs, bins = 30, color = I('black'), fill=('salmon'))
    # 2.3b predict with mean + movie effects, b_i on a test set
    predicted_ratings <- mu + edx_test %>% 
      left_join(movie_avgs, by='movieId') %>%
      pull(b_m)
  results <- add_row(results, model = "Mean + Movie effects",
                  RMSE = RMSE( edx_test$rating, predicted_ratings),
                  "Improvement vs Target" = target - RMSE)

  # 3. baseline  + movie & user effects
  # (approximation of LM model lm(rating ~ 
  #  as.factor(movieId) + as.factor(userId)) due to R objects written to memory)
    user_avgs <- edx_train %>% 
      left_join(movie_avgs, by='movieId') %>%
      group_by(userId) %>% 
      summarise(b_u=mean(rating - mu - b_m)) 
    # 3.1  predict with mean + movie + user effects, b_m and b_u on a test set 
      predicted_ratings <- edx_test %>% 
        left_join(movie_avgs, by='movieId') %>%
        left_join(user_avgs, by='userId') %>%
        mutate(pred = mu + b_m + b_u) %>%
        pull(pred)

      results <- add_row(results, model = "Mean + Movie and User effects",
                         RMSE = RMSE( edx_test$rating, predicted_ratings),
                         "Improvement vs Target" = target - RMSE)

    
    #4.  baseline + regularized movie & user effects
    # 4.1 RMSE function using penalty coeff
    RMSE_reg <- function(l, train, test){
      # lambdas, train and test sets are function arguments
      mu <- mean(train$rating) #mean
      
      b_m <- train %>% 
        group_by(movieId) %>%
        summarize(b_m = sum(rating - mu)/(n()+l))
      
      b_u <- train %>% 
        left_join(b_m, by="movieId") %>%
        group_by(userId) %>%
        summarize(b_u = sum(rating - b_m - mu)/(n()+l))
      
      predicted_ratings <- 
        test %>% 
        left_join(b_m, by = "movieId") %>%
        left_join(b_u, by = "userId") %>%
        mutate(pred = mu + b_m + b_u) %>%
        pull(pred)
      
      return(sqrt(mean((test$rating - predicted_ratings)^2)))
    }
    # 4.2  cross validate lambda that minimize RSME
    lambdas <- seq(0, 10, .5)  
    RMSEs <- sapply(lambdas,FUN=RMSE_reg, train=edx_train, test=edx_test)
    qplot(lambdas, RMSEs, main="RMSE for whole lambdas", color=factor(round(RMSEs,7)))
    initial_l <- lambdas[which.min(RMSEs)]  
    # 4.3  refine on smaller range with more intervals
    lambdas <- seq(initial_l -.5, initial_l+.5, .05)
    RMSEs <- sapply(lambdas,FUN=RMSE_reg, train=edx_train, test=edx_test)
    qplot(lambdas, RMSEs, color=factor(round(RMSEs,7)))
    
    final_l <- lambdas[which.min(RMSEs)]  
    
    results <- add_row(results, model = "Mean + Regularised Movie and User effects",
                       RMSE = min(RMSEs),
                       "Improvement vs Target" = target - RMSE)
    
  ##################################################
  # 3. Matrix Factorization with recosystem package
  ################################################## 
    
    set.seed(1, sample.kind = "Rounding")
    
    # 3.1 Create a model object (a Reference Class object in R) by calling Reco()
    r <- Reco() 
    # 3.2 create train/test sets in recosystem (sparse matrix triplet: user, movie, rating) format
    # data_memory(): Specifies a data set from R objects
    train_set <-  with(edx_train, data_memory(user_index = userId, 
                                               item_index = movieId, 
                                               rating     = rating))
    test_set  <-  with(edx_test,  data_memory(user_index = userId, 
                                               item_index = movieId, 
                                               rating     = rating))
    
    # 3.3 Tune the model parameters
    # computationally expensive; therefore, pre-run and saved to git as 'opts.RDS'
    if (file.exists('opts.RDS')){
        opts <- readRDS('opts.RDS')}else{
        opts <- r$tune(train_set, opts = list(dim = c(70), 
                                           lrate = c(0.1, 0.2),
                                           costp_l2 = c(0.01, 0.1), 
                                           costq_l2 = c(0.01, 0.1),
                                           nthread  = 4, niter = 20, verbose=TRUE))
        }
    # 3.4 train the model
       r$train(train_set, opts = c(opts$min, nthread = 4, niter = 20))
    
    # 3.5 estimate predicted values, with out_memory(): result should be returned as R objects
    y_from_reco <- r$predict(test_set, out_memory())
    # 3.6 calc RMSE and add to results
    results <- add_row(results, model="Matrix Factorization",
                       RMSE = RMSE(edx_test$rating, y_from_reco),
                       "Improvement vs Target" = target - RMSE)
    
    
   
    
    ##################################################
    # 4. Final RMSE Validation
    ################################################## 
      # 4.1. re-train MF model on the entire edx data set
    edx_set <-  with(edx, data_memory(user_index = userId, 
                                              item_index = movieId, 
                                              rating     = rating))
    validation_set  <-  with(validation,  data_memory(user_index = userId, 
                                              item_index = movieId, 
                                              rating     = rating))
    
    # 4.2 assuming the same tuning parameters as before, train the model
    r$train(edx_set, opts = c(opts$min, nthread = 4, niter = 10))
    
    y_from_reco_final <- r$predict(validation_set, out_memory())
    results <- add_row(results, model="Matrix Factorization - Final Validation",
                       RMSE = RMSE(validation$rating, y_from_reco_final),
                       "Improvement vs Target" = target - RMSE)
    
    view(results)