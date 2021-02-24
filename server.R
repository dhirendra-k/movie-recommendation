## server.R

# load functions
library(reshape2)
library(recommenderlab)
library(Matrix)

##### Credit to pspachtholz Book Recommender github and Piazza post @880 CS 598 FA2020. This server code is built off of post @880

################ 
# IMPORT DATA 
################

remoteDataUrl = "https://liangfgithub.github.io/MovieData/"

# Read in movie data
import_movies = function() {
  movies = readLines(paste0("https://liangfgithub.github.io/MovieData/", 'movies.dat?raw=true'))
  
  print("downloaded movies")
  movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
  movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
  movies = data.frame(movies, stringsAsFactors = FALSE)
  colnames(movies) = c('movie_id', 'title', 'genres')
  movies$movie_id = as.integer(movies$movie_id)
  movies$title = iconv(movies$title, "latin1", "UTF-8")
  
  movies$image_url = sapply(movies$movie_id,function(x) paste0("https://liangfgithub.github.io/MovieImages/", x, '.jpg?raw=true'))
  movies
}

# Read in Rating data
# Source is formatted as (UserID::MovieID::Rating::Timestamp)
import_ratings = function() {
  ratings = readLines(paste0("https://liangfgithub.github.io/MovieData/", 'ratings.dat?raw=true'))
  
  print("downloaded ratings")
  
  ratings = strsplit(ratings, split = "::", fixed = TRUE, useBytes = TRUE)
  
  ratings = matrix(unlist(ratings), ncol = 4, byrow = TRUE)
  ratings = data.frame(ratings, stringsAsFactors = FALSE)
  colnames(ratings) = c('user_id', 'movie_id', 'rating', 'timestamp')
  ratings$movie_id = as.integer(ratings$movie_id)
  ratings$user_id = as.integer(ratings$user_id)
  ratings$rating = as.integer(ratings$rating)
  ratings$timestamp = as.integer(ratings$timestamp)
  ratings
}

movies = import_movies()
ratings = import_ratings()

#############

# Define user rating functions
get_user_ratings <- function(value_list) {
  dat <- data.table(movie_id = sapply(strsplit(names(value_list), "_"), function(x) ifelse(length(x) > 1, x[[2]], NA)),
                    rating = unlist(as.character(value_list)))
  dat <- dat[!is.null(rating) & !is.na(movie_id)]
  dat[rating == " ", rating := 0]
  dat[, ':=' (movie_id = as.numeric(movie_id), rating = as.numeric(rating))]
  dat <- dat[rating > 0]
  
  j = paste0('m', dat$movie_id)
  x = dat$rating
  i = rep('new_user', length(j))
  
  new_user <- data.frame(i, j, x, stringsAsFactors = T)
}

# Define System 1 Function
system1_topN <- function(genre, topN=10) {
  genre_movies <- c()
  for (i in 1:dim(movies)[1]) { #for each row in movies
    if (grepl(genre, movies$genres[i])) {
      genre_movies <- append(genre_movies, movies$movie_id[i])
    }
  }
  
  genre_ratings = ratings[which(ratings$movie_id %in% genre_movies), ]
  #calculate average rating in this genre
  mean_rating = mean(genre_ratings$rating)
  
  #aggregate ratings to averages
  rating_averages = aggregate(rating~movie_id, FUN = mean, data=genre_ratings)
  
  #center by mean rating and then scale by popularity percentile
  rating_counts = plyr::count(genre_ratings, "movie_id")
  rating_averages = merge(rating_averages, rating_counts)
  scales = sapply(rating_averages$freq, function(x) log(ecdf(rating_averages$freq)(x) * 100, base = 2) / log(50, base = 2))
  rating_averages$score = (rating_averages$rating - mean_rating) * scales
  
  rating_averages <- rating_averages[order(-rating_averages$score), ]
  return(rating_averages[1:topN,])
}



#matrix building code referenced from https://liangfgithub.github.io/Rcode_W13_Movie_RS.nb.html
model_UBCF = function() {
  i = paste0('u', ratings$user_id)
  j = paste0('m', ratings$movie_id)
  x = ratings$rating
  tmp = data.frame(i, j, x, stringsAsFactors = T)
  Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
  rownames(Rmat) = levels(tmp$i)
  colnames(Rmat) = levels(tmp$j)
  Rmat = new('realRatingMatrix', data = Rmat)
  
  rec_UBCF = Recommender(Rmat, method = 'UBCF',
                         parameter = list(normalize = 'Z-score', 
                                          method = 'Cosine', 
                                          nn = 35))
}

# returns topN predicted movie ratings given a dataframe of one user (i=userid, j=movieid, x=rating), 
# returns a named list in decreasing order of rating, with names set to the movieid
prediction_UBCF = function(new_user, UBCF_model, topN = 10) {
  i = paste0('u', ratings$user_id)
  j = paste0('m', ratings$movie_id)
  x = ratings$rating
  tmp = data.frame(i, j, x, stringsAsFactors = T)
  new_tmp = rbind(new_user, tmp)
  
  Rmat_new = sparseMatrix(as.integer(new_tmp$i), as.integer(new_tmp$j), x = new_tmp$x)
  rownames(Rmat_new) = levels(new_tmp$i)
  colnames(Rmat_new) = levels(new_tmp$j)
  Rmat_new = new('realRatingMatrix', data = Rmat_new)

  res = predict(UBCF_model, Rmat_new[1], type = "ratings")
  res = as(res, 'matrix')
  res_topN = sort(res[, which(!is.na(res))], decreasing = T)[1:topN]
}


shinyServer(function(input, output, session) {
  
  #============== SYSTEM-2 ==============================================  
  
  # show the books to be rated
  output$system2ratings <- renderUI({
    num_rows <- 20
    num_movies <- 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], style = "max-height:150")),
                 div(style = "text-align:center", strong(movies$title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$movie_id[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })
  
  
  # Calculate recommendations when the sbumbutton is clicked
  df_system2 <- eventReactive(input$btnsystem2, {
    withBusyIndicatorServer("btnsystem2", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list)
      rec_UBCF = model_UBCF()
      pred = prediction_UBCF(user_ratings, rec_UBCF)
      
      predicted_ratings = unname(pred)
      predicted_movie_ids = as.numeric(sub("m", "", names(pred)))
      
      recom_results <- data.table(Rank = 1:10, 
                                  movie_id = predicted_movie_ids, 
                                  title = movies$title[predicted_movie_ids], 
                                  Predicted_rating = predicted_ratings)
      
    }) # still busy
    
  }) # clicked on button
  
  # SYSTEM-2 : display the recommendations
  output$system2results <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df_system2()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = movies$image_url[recom_result$movie_id[(i - 1) * num_movies + j]], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(movies$title[recom_result$movie_id[(i - 1) * num_movies + j]])
            )
        )        
      }))) # columns
    }) # rows
  }) # renderUI function
  
  #============== SYSTEM-1 ==============================================
  
  # Calculate recommendations when the sbumbutton is clicked
  df_system1 <- eventReactive(input$btnsystem1, {
    withBusyIndicatorServer("btnsystem1", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's selected genre
      genre <- reactiveValuesToList(input)$genre
      recommended_movies <- system1_topN(genre, 10)
      print(recommended_movies)
      recom_results <- data.table(Rank = 1:10, 
                                  movie_id = recommended_movies$movie_id, 
                                  title = movies[which(movies$movie_id %in% recommended_movies$movie_id),]$title,
                                  image_url = movies[which(movies$movie_id %in% recommended_movies$movie_id),]$image_url,
                                  Predicted_rating =  recommended_movies$rating) 
      
    }) # still busy
    
  }) # clicked on button
  
  #  # SYSTEM-1 : display the recommendations
  output$system1results <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df_system1()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = recom_result$image_url[(i - 1) * num_movies + j], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(recom_result$title[(i - 1) * num_movies + j])
            )
        )        
      }))) # columns
    }) # rows
  }) # renderUI function
  
}) # server function
