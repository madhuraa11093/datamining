
library(RJDBC)
library(randomForest)
library(pROC)

library(mlbench)
library(caret)
server.R<-shinyServer( 
  function(input,output,session){
    observe({
   #   if ( is.null(input$submit) ) return()
      
      if(input$submit==0) return()
      
      isolate({
        current_selection1<-input$txt1
        current_selection2<-input$txt2
        current_selection3<-input$txt3
        current_selection4<-input$txt4
        current_selection5<-input$txt5
        current_selection6<-input$txt6
        current_selection7<-input$txt7
        current_selection8<-input$txt8
        current_selection9<-input$txt9
        
        jdbcDriver =JDBC("oracle.jdbc.OracleDriver",classPath="ojdbc6.jar")
        jdbcConnection =dbConnect(jdbcDriver, "jdbc:oracle:thin:@//localhost:1521/xe", "aditi", "patil")
        x <- "aditi"
        y <- "aditi1"
        print(x)
        
        v1 = dbGetQuery(jdbcConnection, paste0('SELECT max(distinct(ACTOR_1_FACEBOOK_LIKES)) from 
                                               MOVIE_METADATA1 where ACTOR_1_NAME =\'',current_selection2,'\''))
        v2 = dbGetQuery(jdbcConnection, paste0('SELECT max(distinct(ACTOR_2_FACEBOOK_LIKES)) from 
                                               MOVIE_METADATA1 where ACTOR_2_NAME =\'',current_selection3,'\''))
        v3 = dbGetQuery(jdbcConnection, paste0('SELECT max(distinct(ACTOR_3_FACEBOOK_LIKES)) from 
                                               MOVIE_METADATA1 where ACTOR_3_NAME =\'',current_selection4,'\''))
        v4 = dbGetQuery(jdbcConnection, paste0('SELECT max(distinct(DIRECTOR_FACEBOOK_LIKES)) from 
                                               MOVIE_METADATA1 where DIRECTOR_NAME =\'',current_selection5,'\''))
        print(v1)
        print(v2)
        print(v3)
        print(v4)
        v5 = dbGetQuery(jdbcConnection, paste0('select round(((avg(a.IMDB_SCORE)+avg(b.IMDB_SCORE)+avg(c.imdb_score)+avg(d.imdb_score))/4)+1.5) from MOVIE_METADATA1 a ,
MOVIE_METADATA1 b, MOVIE_METADATA1 c,  MOVIE_METADATA1 d
where a.ACTOR_1_NAME=\'',current_selection2,'\' and
b.ACTOR_2_NAME=\'',current_selection3,'\' and c.ACTOR_3_NAME=\'',current_selection4,'\' and d.DIRECTOR_NAME=\'',current_selection5,'\''))
        #v5 = dbGetQuery(jdbcConnection, paste0('SELECT distinct(DIRECTOR_FACEBOOK_LIKES) from 
        #          MOVIE_METADATA1 where DIRECTOR_NAME =\'',current_selection5,'\''))
        print(v5)
        
        # insertstr <-  paste0("insert into x values ('",v1,"','",v2,"','",v3,"','",v4,"','null')")
        
        insertstr <-  paste0("insert into MOVIE_METADATA1 (ROWN, DIRECTOR_NAME, NUM_CRITIC_FOR_REVIEWS,DURATION,DIRECTOR_FACEBOOK_LIKES,ACTOR_3_FACEBOOK_LIKES,ACTOR_2_NAME,ACTOR_1_FACEBOOK_LIKES,
                             GROSS, GENRES, ACTOR_1_NAME, MOVIE_TITLE, NUM_VOTED_USERS, CAST_TOTAL_FACEBOOK_LIKES,
                             ACTOR_3_NAME,FACENUMBER_IN_POSTER,PLOT_KEYWORDS,MOVIE_IMDB_LINK,NUM_USER_FOR_REVIEWS,
                             LANGUAGE,COUNTRY,CONTENT_RATING,BUDGET,TITLE_YEAR,ACTOR_2_FACEBOOK_LIKES,IMDB_SCORE,ASPECT_RATIO,MOVIE_FACEBOOK_LIKES)
                             values ('5045', '",current_selection5,"', '635','",current_selection8,"', '",v4,"', '",v3,"','",current_selection3,"','",v1,"',
                             '458991599', 'anime', '",current_selection2,"', '",current_selection1,"', '462669', '0', 
                             '",current_selection4,"', '",current_selection6,"', 'avatar', 'http://www.imdb.com', '3054',
                             'english','usa','PG-13','",current_selection9,"','",current_selection7,"','",v2,"', '",v5,"','3.2', '100')")   
        print(insertstr)
        
        dbSendUpdate(jdbcConnection, insertstr)
        
        query <- paste0("update MOVIE_METADATA1 set CAST_TOTAL_FACEBOOK_LIKES=ACTOR_1_FACEBOOK_LIKES+ACTOR_2_FACEBOOK_LIKES+ACTOR_3_FACEBOOK_LIKES+100  
                                             where rown=('5045')")
        
        dbSendUpdate(jdbcConnection, query)
        
        movies <- dbGetQuery(jdbcConnection, "SELECT * FROM movie_metadata1")
        
        #movies <- read_csv("movie_metadata.csv")
        set.seed(0)
        movies_with_good_variables = movies[, c("ROWN", "IMDB_SCORE",
                                                "DIRECTOR_FACEBOOK_LIKES", 
                                                "CAST_TOTAL_FACEBOOK_LIKES", 
                                                "ACTOR_1_FACEBOOK_LIKES",
                                                "ACTOR_2_FACEBOOK_LIKES",
                                                "ACTOR_3_FACEBOOK_LIKES",
                                                "FACENUMBER_IN_POSTER",
                                                "BUDGET", "DURATION")]
        
        mvs = na.omit(movies_with_good_variables)
        #finding mean imdb score.
        
        mvs$IMDB_SCORE <- as.numeric(mvs$IMDB_SCORE)
        
        str(mvs)
        
        mean_rating <- mean(mvs$IMDB_SCORE)
        
        #discretization of imdb score using mean
        #if the score is less than "floor of the mean -1" then rating is poor else if score  greater 
        #than ceiling of mean rating is good. else rating is average
        mvs$rating <- ifelse(mvs$IMDB_SCORE < (floor(mean_rating)-1),'poor',ifelse(mvs$IMDB_SCORE > ceiling(mean_rating),'good', 'average'))
        #convert to factors 
        mvs$rating <- as.factor(mvs$rating)
        #66.32% of data is recommended to be ideal training size for random forest hence 36.8% for test
        
        inpt=mvs[mvs$ROWN == "5045",]
        mvs=mvs[-c(5045), ]
        
        index = sample(1:nrow(mvs), size=0.30*nrow( mvs))
        test = mvs[index,]
        train = mvs[-index,]
        ##################
        
        movie.rf <- randomForest(rating ~. -IMDB_SCORE, train, replace=TRUE,na.action=na.omit , mtry = 5, ntree= 100)
        print(movie.rf)
        movie_train_prediction <- predict(movie.rf, train)
        table(train$rating, movie_train_prediction)
        #above table will show if prediction model is correct on training data
        #perform prediction on test data
        movie_test_prediction <- predict(movie.rf, test)
        #show the confusion matrix of test data
        table(test$rating, movie_test_prediction)
        movie_inpt_prediction <- predict(movie.rf, inpt)
        
        
        
        ############################
        
        control <- trainControl(method="repeatedcv", number=2, repeats=2, search="grid")
        set.seed(7)
        seed <- 7
        metric <- "Accuracy"
        set.seed(seed)
        #mtry <- sqrt(ncol(train))
        customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
        print(x)
        customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
        customRF$grid <- function(x, y, len = NULL, search = "grid") {}
        customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
          randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
        }
        
        customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
          predict(modelFit, newdata)
        customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
          predict(modelFit, newdata, type = "prob")
        print(x)
        customRF$sort <- function(x) x[order(x[,1]),]
        customRF$levels <- function(x) x$classes
        tunegrid <- expand.grid(.mtry=c(2:4), .ntree=c(100, 200))
        print(x)
        rf_gridsearch <- train(rating ~. -IMDB_SCORE, data=train, method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)
        
        pred<- predict(rf_gridsearch, train)
        
        train$prerate=pred
        
        pred<- predict(rf_gridsearch, test)
        test$prerate=pred
        pred<- predict(rf_gridsearch,inpt)
        inpt$prerate=pred
        
        rightPred <- pred == test$rating
        accuracy <- sum(rightPred)/nrow(test)
        
        
        ###############
        print(y)
        dbRemoveTable(jdbcConnection, "MYTABLE")
        dbRemoveTable(jdbcConnection, "MYTABLE1")
        dbRemoveTable(jdbcConnection, "MYTABLE2")
        
        dbWriteTable(jdbcConnection, "MyTable",  train , row.names=FALSE, append=TRUE)
        dbWriteTable(jdbcConnection, "MyTable1",  test , row.names=FALSE, append=TRUE)
        dbWriteTable(jdbcConnection, "MyTable2",  inpt , row.names=FALSE, append=TRUE)
        
        ######
        
        
        print(x)
        print(y)
        q <- paste0("delete from MOVIE_METADATA1 where rown=('5045')")
        print(q)
        dbSendUpdate(jdbcConnection, q)
        
        
        oupt = dbGetQuery(jdbcConnection, paste0("select PRERATE from MYTABLE2" ))
        print(oupt)
        dbDisconnect(jdbcConnection)
        
        output$results = renderPrint({print(oupt)})
        
        
      })
    }
    
    )
    
    
    
  }  
  
)


