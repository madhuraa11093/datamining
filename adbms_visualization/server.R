library(shiny)
library(ggplot2)
library(readr)
library(dplyr)
library(corrplot)
library(tidyr)
library(rworldmap)
library(classInt)
library(RColorBrewer)
library(randomForest)
library(pROC)
library(mlbench)
library(randomForest)
library(tidyverse)
library(mlbench)
library(caret)
library(gplots)


function(input, output) {
  
  movies <- read_csv("movie_metadata.csv")
  
  output$plot <- renderPlot({
    if(input$options== "Random Forest")
    {
      set.seed(0)
      movies_with_good_variables = movies[, c("imdb_score",
                                              "director_facebook_likes", 
                                              "cast_total_facebook_likes", 
                                              "actor_1_facebook_likes",
                                              "actor_2_facebook_likes",
                                              "actor_3_facebook_likes",
                                              "movie_facebook_likes", 
                                              "facenumber_in_poster",
                                              "gross",
                                              "budget")]
      mvs = na.omit(movies_with_good_variables)
      #finding mean imdb score
      mean_rating <- mean(mvs$imdb_score)
      mvs$rating <- ifelse(mvs$imdb_score < (floor(mean_rating)-1),'poor',ifelse(mvs$imdb_score > ceiling(mean_rating),'good', 'average'))
      #convert to factors 
      mvs$rating <- as.factor(mvs$rating)
      #66.32% of data is recommended to be ideal training size for random forest hence 36.8% for test
      index = sample(1:nrow(mvs), size=0.368*nrow( mvs))
      test = mvs[index,]
      train = mvs[-index,]
      control <- trainControl(method="repeatedcv", number=2, repeats=2, search="grid")
      set.seed(7)
      seed <- 7
      metric <- "Accuracy"
      set.seed(seed)
      #mtry <- sqrt(ncol(train))
      customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
      customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
      customRF$grid <- function(x, y, len = NULL, search = "grid") {}
      customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
        randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
      }
      customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
        predict(modelFit, newdata)
      customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
        predict(modelFit, newdata, type = "prob")
      customRF$sort <- function(x) x[order(x[,1]),]
      customRF$levels <- function(x) x$classes
      tunegrid <- expand.grid(.mtry=c(2:4), .ntree=c(100, 200))
      rf_gridsearch <- train(rating ~. -imdb_score, data=train, method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)
      print(rf_gridsearch)
      pred<- predict(rf_gridsearch, train)
      table(pred, train$rating)
      pred<- predict(rf_gridsearch, test)
      table(pred, test$rating)
      rightPred <- pred == test$rating
      accuracy <- sum(rightPred)/nrow(test)
      p<-plot(rf_gridsearch)
      
    }
    else if(input$options== "Artificial Neural Network")
    {
      movie_data <-movies
      movie_data$title_year = as.factor(movie_data$title_year)
      num_dat = sapply(movie_data, is.numeric)  
      movie_num = movie_data[, num_dat] 
      movie_num_new <- na.omit(movie_num) 
      scaled_data <- data.frame(lapply(movie_num_new, function(x) scale(x, center = FALSE, scale = max(x, na.rm = TRUE))))
      scaled_data$imdb_score = movie_num_new$imdb_score / 10
      scaled_data <- scaled_data[,c("imdb_score","director_facebook_likes","duration","actor_1_facebook_likes","actor_2_facebook_likes","actor_3_facebook_likes","facenumber_in_poster","budget")]
      
      scaled_data = data.frame(scaled_data)
      index <- 1:nrow(scaled_data)
      index_test <- sample(index, trunc(length(index)*0.25))
      test_data <- scaled_data[index_test,]
      train_data <- scaled_data[-index_test,]
      f <- as.formula(paste0('imdb_score ~ ',paste(names(train_data[!names(train_data) %in% 'imdb_score']), collapse = ' + ')))
      ann_model <- neuralnet(f, train_data, hidden = 5)
      p<-plot(ann_model)
    }
    else if(input$options== "Support Vector Machine")
    {
      movies$title_year = as.factor(movies$title_year)
      number_attributes <- sapply(movies,is.numeric)
      movies_num <- movies[,number_attributes] 
      movies_1 <- na.omit(movies_num) 
      scaled_data <- data.frame(lapply(movies_1, function(x) scale(x, center = FALSE, scale = max(x, na.rm = TRUE))))                         #Scale the data
      scaled_data$imdb_score = movies_1$imdb_score / 10
      scaled_data <- scaled_data[,c("imdb_score","director_facebook_likes","duration","actor_1_facebook_likes","actor_2_facebook_likes","actor_3_facebook_likes","facenumber_in_poster","budget")]
      
      index<- 1:nrow(scaled_data)
      index_test <- sample(index, trunc(length(index)*0.25))
      test_data <- scaled_data[index_test,]
      train_data <- scaled_data[-index_test,]
      
      svm_pred <-svm(imdb_score ~., data = train_data, kernel = "radial", gamma = 0.7)
      # Apply the model on test data and get the predicted values
      svm_predictions <- predict(svm_pred, test_data[,-1])
      p<-plot(svm_predictions,test_data$imdb_score,col=c("red","blue"), xlab="Predicted",ylab="Actual", main="Train 75% Test 25%")
      
    }
    else if(input$options== "Linear Regression")
    {
      numeric_attributes<-sapply(movies,is.numeric)
      movies_numeric <- movies[,numeric_attributes]
      
      #removing missing values
      movies_missing_removed <- na.omit(movies_numeric)
      
      #movie_data scaled
      scaled_movie_data <- data.frame(lapply(movies_missing_removed, function(x) scale(x, center = FALSE, scale = max(x, na.rm = TRUE))))  
      
      index_start<- 1:nrow(scaled_movie_data)
      index_test25 <- sample(index_start, trunc(length(index_start)*0.25))
      test_data25 <- scaled_movie_data[index_test25,]
      
      index_train75 <- sample(index_start, trunc(length(index_start)*0.75))
      
      train_data75 <- scaled_movie_data[index_train75,]
      #create model using linear regression
      
      linear_model <-glm(imdb_score ~., data = train_data75)
      
      #applying the model created on test data to obtain predictions
      linear_predictions3 <- predict(linear_model, test_data25)
      
      #plot(linear_predictions,test_data25$imdb_score,col=c("red","green"), xlab="Predicted",ylab="Actual")
      p<- plot(linear_predictions3,test_data25$imdb_score,col=rainbow(2), xlab="Predicted",ylab="Actual", main="Test data 25")
      #root mean square error
      
    }
    print(p)
    
  }, height=700)
  output$info <-renderText(
    if(input$options== "Random Forest")
    {
      set.seed(0)
      movies_with_good_variables = movies[, c("imdb_score",
                                              "director_facebook_likes", 
                                              "cast_total_facebook_likes", 
                                              "actor_1_facebook_likes",
                                              "actor_2_facebook_likes",
                                              "actor_3_facebook_likes",
                                              "movie_facebook_likes", 
                                              "facenumber_in_poster",
                                              "gross",
                                              "budget")]
      mvs = na.omit(movies_with_good_variables)
      #finding mean imdb score
      mean_rating <- mean(mvs$imdb_score)
      mvs$rating <- ifelse(mvs$imdb_score < (floor(mean_rating)-1),'poor',ifelse(mvs$imdb_score > ceiling(mean_rating),'good', 'average'))
      #convert to factors 
      mvs$rating <- as.factor(mvs$rating)
      #66.32% of data is recommended to be ideal training size for random forest hence 36.8% for test
      index = sample(1:nrow(mvs), size=0.368*nrow( mvs))
      test = mvs[index,]
      train = mvs[-index,]
      #control <- trainControl(method="repeatedcv", number=2, repeats=2, search="grid")
      set.seed(7)
      seed <- 7
      metric <- "Accuracy"
      set.seed(seed)
      #mtry <- sqrt(ncol(train))
      customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
      customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
      customRF$grid <- function(x, y, len = NULL, search = "grid") {}
      customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
        randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
      }
      customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
        predict(modelFit, newdata)
      customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
        predict(modelFit, newdata, type = "prob")
      customRF$sort <- function(x) x[order(x[,1]),]
      customRF$levels <- function(x) x$classes
      tunegrid <- expand.grid(.mtry=c(2:4), .ntree=c(100, 200))
      rf_gridsearch <- train(rating ~. -imdb_score, data=train, method=customRF, metric=metric, tuneGrid=tunegrid)
      print(rf_gridsearch)
      pred<- predict(rf_gridsearch, train)
      table(pred, train$rating)
      pred<- predict(rf_gridsearch, test)
      table(pred, test$rating)
      rightPred <- pred == test$rating
      accuracy <- sum(rightPred)/nrow(test)
      p<- "Random forest accuracy :"
      p<- paste(p, accuracy," ")
      print (p)
    }
   
    
     
  )
  
}