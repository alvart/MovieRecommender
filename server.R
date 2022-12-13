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

# Prepare the training data as a realRatingMatrix, say Rmat
i = paste0('u', ratings$UserID)
j = paste0('m', ratings$MovieID)
x = ratings$Rating
tmp = data.frame(i, j, x, stringsAsFactors = T)
Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
rownames(Rmat) = levels(tmp$i)
colnames(Rmat) = levels(tmp$j)
Rmat = new('realRatingMatrix', data = Rmat)
Rmat = Rmat[1:500, ] # let's use a subset

# Fit a model using UBCF
rec_UBCF = Recommender(Rmat, method = 'UBCF',
                       parameter = list(normalize = 'Z-score', 
                                        method = 'Cosine', 
                                        nn = 25))

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
    mutate(Image = paste0(small_image_url, MovieID, '.jpg?raw=true')) %>%
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
    num_movies <- 5
    recom_result <- system1_event()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            div(style = "text-align:center",
                a(
                  img(src = recom_result$Image[(i - 1) * num_movies + j], height = 150)
                )
            ),
            div(style="text-align:center; font-size: 100%",
                strong(recom_result$Title[(i - 1) * num_movies + j])
            ),
            div(style = "text-align:center; font-size: 80%",
                recom_result$Genres[(i - 1) * num_movies + j]
            ),
            div(style = "text-align:center; color: #808080; font-size: 80%",
                paste0(round(recom_result$ave_ratings[(i - 1) * num_movies + j], 1), "/5 stars")
            )
        )
      }))) # columns
    }) # rows
  }) # renderUI function
  
  # show the movies to be rated
  output$ratings <- renderUI({
    num_rows <- 5
    num_movies <- 6 # movies per row
    top_movies <- ratings %>%
      group_by(MovieID) %>% 
      summarize(ratings_per_movie = n(), 
                ave_ratings = round(mean(Rating), dig=3)) %>%
      inner_join(movies, by = 'MovieID') %>%
      filter(ratings_per_movie > 1000) %>%
      top_n(num_rows * num_movies, ave_ratings) %>%
      mutate(Image = paste0(small_image_url, MovieID, '.jpg?raw=true')) %>%
      arrange(desc(ave_ratings))
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center",
                     a(
                       img(src = top_movies$Image[(i - 1) * num_movies + j], height = 150)
                     )
                 ),
                 div(style="text-align:center; font-size: 100%",
                     strong(top_movies$Title[(i - 1) * num_movies + j])
                 ),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("m", top_movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
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
        
        value_list <- reactiveValuesToList(input)
        
        # Prepare a new user
        movieIDs = colnames(Rmat)
        n.item = ncol(Rmat)  
        new.ratings = rep(NA, n.item)
        for (k in names(value_list)) {
          if (k %in% colnames(Rmat)) {
            new.ratings[which(movieIDs == k)] = value_list[[k]]
          }
        }
        new.user = matrix(new.ratings, 
                          nrow=1, ncol=n.item,
                          dimnames = list(
                            user=paste('feng'),
                            item=movieIDs
                          ))
        new.Rmat = as(new.user, 'realRatingMatrix')
        
        # Predict the top 10 items
        recom1 = predict(rec_UBCF, new.Rmat, type = 'topN')
        
        movies[movies$MovieID %in% recom1@items[[1]], ] %>% 
          mutate(Image = paste0(small_image_url, MovieID, '.jpg?raw=true'))
    }) # still busy
  }) # clicked on button

  # display the recommendations
  output$results <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df()

    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
          div(style = "text-align:center",
              a(
                img(src = recom_result$Image[(i - 1) * num_movies + j], height = 150)
              )
          ),
          div(style="text-align:center; font-size: 100%",
              strong(recom_result$Title[(i - 1) * num_movies + j])
          ),
          div(style = "text-align:center; font-size: 80%",
              recom_result$Genres[(i - 1) * num_movies + j]
          ),
        )
      }))) # columns
    }) # rows
  }) # renderUI function
}) # server function
