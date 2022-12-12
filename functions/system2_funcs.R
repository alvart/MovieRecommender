library(recommenderlab)
library(magrittr)

myurl = "https://liangfgithub.github.io/MovieData/"

get_rating_data = function() {
    ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'),  sep = ':', colClasses = c('integer', 'NULL'), header = FALSE)
    colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
    return(ratings)
}

get_movies_data = function() {
    movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
    movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
    movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
    movies = data.frame(movies, stringsAsFactors = FALSE)
    colnames(movies) = c('MovieID', 'Title', 'Genres')
    movies$MovieID = as.integer(movies$MovieID)

    # convert accented characters
    movies$Title[73]
    movies$Title = iconv(movies$Title, "latin1", "UTF-8")
    movies$Title[73]

    # extract year
    movies$Year = as.numeric(unlist(
    lapply(movies$Title, function(x) substr(x, nchar(x)-4, nchar(x)-1))))
    return(movies)
}

get_matching_movie_rating_set = function(ratings, movies) {
    movies_not_rated = subset(movies, !(MovieID %in% ratings$MovieID))
    ratings = subset(ratings, !(MovieID %in% movies_not_rated$MovieID))
}

get_sparse_matrix = function(ratings) {
    i = paste0('u', ratings$UserID)
    j = paste0('m', ratings$MovieID)
    x = ratings$Rating
    tmp = data.frame(i, j, x, stringsAsFactors = T)
    Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
    rownames(Rmat) = levels(tmp$i)
    colnames(Rmat) = levels(tmp$j)
    Rmat = new('realRatingMatrix', data = Rmat)
    return(Rmat)
}


ibcf_pred = function(ratings, user_rating, k=30) {
  model = Recommender(ratings, method = "IBCF", parameter = list(normalize = 'center', method = 'Cosine', k = 30))
  pred <- predict(model, user_rating, type="ratings")
  pred <- as.numeric(as(pred, "matrix"))
  return(pred)
}




