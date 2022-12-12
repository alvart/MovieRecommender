## server.R

# load functions
source("functions/cf_algorithm.R") # collaborative filtering
source("functions/similarity_measures.R") # similarity measures
source("functions/system_1.R")

# define functions
get_user_ratings <- function(value_list) {
  dat <- data.table(book_id = sapply(strsplit(names(value_list), "_"), function(x) ifelse(length(x) > 1, x[[2]], NA)),
                    rating = unlist(as.character(value_list)))
  dat <- dat[!is.null(rating) & !is.na(book_id)]
  dat[rating == " ", rating := 0]
  dat[, ':=' (book_id = as.numeric(book_id), rating = as.numeric(rating))]
  dat <- dat[rating > 0]

  # get the indices of the ratings
  # add the user ratings to the existing rating matrix
  user_ratings <- sparseMatrix(i = dat$book_id,
                               j = rep(1,nrow(dat)),
                               x = dat$rating,
                               dims = c(nrow(ratingmat), 1))
}

# read in data
books <- fread("data/books.csv")
ratings <- fread("data/ratings_cleaned.csv")

# reshape to books x user matrix
ratingmat <- sparseMatrix(ratings$book_id, ratings$user_id, x=ratings$rating) # book x user matrix
ratingmat <- ratingmat[, unique(summary(ratingmat)$j)] # remove users with no ratings
dimnames(ratingmat) <- list(book_id = as.character(1:10000), user_id = as.character(sort(unique(ratings$user_id))))


# Ratings Data
ratings <- read.csv("data/ratings.dat", 
                    sep = ':',
                    colClasses = c('integer', 'NULL'), 
                    header = FALSE)
colnames(ratings) <- c('UserID', 'MovieID', 'Rating', 'Timestamp')


# Movies Data
movies <- readLines('data/movies.dat')
movies <- strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies <- matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies <- data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) <- c('MovieID', 'Title', 'Genres')
movies$MovieID <- as.integer(movies$MovieID)

# convert accented characters
movies$Title <- iconv(movies$Title, "latin1", "UTF-8")

# extract year
movies$Year <- as.numeric(unlist(
  lapply(movies$Title, function(x) substr(x, nchar(x)-4, nchar(x)-1))))


# Create a Genre Matrix
genres <- as.data.frame(movies$Genres, stringsAsFactors=FALSE)
tmp <- as.data.frame(tstrsplit(genres[,1], '[|]',
                               type.convert=TRUE),
                     stringsAsFactors=FALSE)
genre_list <- c("Action", "Adventure", "Animation", 
                "Children's", "Comedy", "Crime",
                "Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", 
                "Mystery", "Romance", "Sci-Fi", 
                "Thriller", "War", "Western")
m <- length(genre_list)
genre_matrix <- matrix(0, nrow(movies), length(genre_list))
for(i in 1:nrow(tmp)){
  genre_matrix[i,genre_list %in% tmp[i,]] <- 1
}
colnames(genre_matrix) <- genre_list
remove("tmp", "genres")

ratings_genre_matrix <- ratings %>% 
  left_join(data.frame(MovieID = movies$MovieID, genre_matrix), 
            by = "MovieID") %>%
  select(-c("UserID", "MovieID", "Rating", "Timestamp"))

# Link for movie images
small_image_url <- "https://liangfgithub.github.io/MovieImages/"

get_highly_rated_movies <- function(genre, n_movies=10) {
  #' Select the top highly rated movies based on their average rating.
  #' The movies must have over 1000 ratings.
  #'
  #' @param genre (string), User's choice of genre for movie selection
  #' @param n_movies (integer, default=10), Number of movies to be returned
  #' @return dataframe of the top highly rated movies
  
  n_ratings <- 1000
  
  highly_rated_movies <- ratings[ratings_genre_matrix[, which(genre_list == genre)] == 1, ] %>%
    group_by(MovieID) %>% 
    summarize(ratings_per_movie = n(), 
              ave_ratings = round(mean(Rating), dig=3)) %>%
    inner_join(movies, by = 'MovieID') %>%
    filter(ratings_per_movie > n_ratings) %>%
    top_n(n_movies, ave_ratings) %>%
    mutate(Image = paste0(small_image_url, MovieID, '.jpg?raw=true"></img>')) %>%
    arrange(desc(ave_ratings))
  
  data.frame(highly_rated_movies)
}

shinyServer(function(input, output, session) {
  
  system1_event <- eventReactive(input$system1_btn, {
    withBusyIndicatorServer("system1_btn", {
      get_highly_rated_movies(input$genre)
    })
  })
  
  output$system1 <- renderUI({
    num_rows <- 2
    num_books <- 5
    recom_result <- system1_event()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_books, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_books + j),
            div(style = "text-align:center",
                a(href = "#",
                  target='blank',
                  img(src = recom_result$Image[(i - 1) * num_books + j], height = 150)
                )
            ),
            div(style="text-align:center; font-size: 100%",
                strong(recom_result$Title[(i - 1) * num_books + j])
            ),
            div(style = "text-align:center; color: #808080; font-size: 80%",
                paste0(round(recom_result$ave_ratings[(i - 1) * num_books + j], 1), "/5 stars")
            )
        )
      }))) # columns
    }) # rows
  }) # renderUI function
  
  # show the books to be rated
  output$ratings <- renderUI({
    num_rows <- 20
    num_books <- 6 # books per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_books, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = books$image_url[(i - 1) * num_books + j], style = "max-height:150")),
                 div(style = "text-align:center; color: #999999; font-size: 80%", books$authors[(i - 1) * num_books + j]),
                 div(style = "text-align:center", strong(books$title[(i - 1) * num_books + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", books$book_id[(i - 1) * num_books + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })
  
  # Calculate recommendations when the sbumbutton is clicked
  df <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", { # showing the busy indicator
        # hide the rating container
        useShinyjs()
        jsCode <- "document.querySelector('[data-widget=collapse]').click();"
        runjs(jsCode)

        # get the user's rating data
        value_list <- reactiveValuesToList(input)
        user_ratings <- get_user_ratings(value_list)

        # add user's ratings as first column to rating matrix
        rmat <- cbind(user_ratings, ratingmat)

        # get the indices of which cells in the matrix should be predicted
        # predict all books the current user has not yet rated
        items_to_predict <- which(rmat[, 1] == 0)
        prediction_indices <- as.matrix(expand.grid(items_to_predict, 1))

        # run the ubcf-alogrithm
        res <- predict_cf(rmat, prediction_indices, "ubcf", TRUE, cal_cos, 1000, FALSE, 2000, 1000)

        # sort, organize, and return the results
        user_results <- sort(res[, 1], decreasing = TRUE)[1:20]
        user_predicted_ids <- as.numeric(names(user_results))
        recom_results <- data.table(Rank = 1:20,
                                    Book_id = user_predicted_ids,
                                    Author = books$authors[user_predicted_ids],
                                    Title = books$title[user_predicted_ids],
                                    Predicted_rating =  user_results)
    }) # still busy
  }) # clicked on button

  # display the recommendations
  output$results <- renderUI({
    num_rows <- 4
    num_books <- 5
    recom_result <- df()

    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_books, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_books + j),

          div(style = "text-align:center",
              a(href = paste0('https://www.goodreads.com/book/show/', books$best_book_id[recom_result$Book_id[(i - 1) * num_books + j]]),
                target='blank',
                img(src = books$image_url[recom_result$Book_id[(i - 1) * num_books + j]], height = 150))
             ),
          div(style = "text-align:center; color: #999999; font-size: 80%",
              books$authors[recom_result$Book_id[(i - 1) * num_books + j]]
             ),
          div(style="text-align:center; font-size: 100%",
              strong(books$title[recom_result$Book_id[(i - 1) * num_books + j]])
             )
        )
      }))) # columns
    }) # rows
  }) # renderUI function
}) # server function
