library(tm)

imdb_data <- readLines("data/IMDB_dataset.csv")
View(imdb_data)

#Load data as  corpus
#VectorSource() creates character vector

imdb_data <- Corpus(VectorSource(imdb_data))

#covert to lower case

imdb_data <- tm_map(imdb_data, content_transformer(tolower))
