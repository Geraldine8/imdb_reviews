library(tm)
library(SnowballC)
library(Matrix)
library(tidytext)
library(dplyr)
library(lexicon)
library(textdata)
library(textstem)
library(ggplot2)


imdb_data <- readLines("data/IMDB_dataset.csv")
View(imdb_data)

#Load data as  corpus
#VectorSource() creates character vector

imdb_data <- Corpus(VectorSource(imdb_data))

#remove ������ what would be emojis
imdb_data <- tm_map(imdb_data, content_transformer(gsub), pattern="\\W",replace=" ")

# Remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
imdb_data <- tm_map(imdb_data, content_transformer(removeNumPunct))

# Remove stopwords
imdb_data <- tm_map(imdb_data, removeWords, stopwords("english"))

# Apply lemmatization to the corpus
imdb_data <- tm_map(imdb_data, PlainTextDocument)
imdb_data <- tm_map(imdb_data, removePunctuation)
imdb_data <- tm_map(imdb_data, content_transformer(tolower))
imdb_data <- tm_map(imdb_data, removeNumbers)
imdb_data <- tm_map(imdb_data, stripWhitespace)
imdb_data <- tm_map(imdb_data, stemDocument)

#Build a term matrix and reveling word frequencies 
# Create a term matrix and store it as dtm
dtm <- TermDocumentMatrix(imdb_data)


# Identify terms that occur frequently in the corpus
freq_terms <- findFreqTerms(dtm, lowfreq = 10, highfreq = Inf)

# Remove sparse terms
dtm_freq <- removeSparseTerms(dtm, sparse = 0.8)

# Find terms that appear in fewer than 5 documents
freq_terms <- findFreqTerms(dtm_freq, lowfreq = 5)

# Remove infrequent terms
dtm_freq <- dtm_freq[, !colnames(dtm_freq) %in% freq_terms]
View(dtm_freq)


# Get the sentiment scores for each word in the dtm using the AFINN lexicon
afinn <- get_sentiments("afinn")
dtm_mat <- as.matrix(dtm_freq)
sentiment_scores <- sapply(rownames(dtm_mat), function(x) sum(afinn[afinn$word == x, "value"]))

# Get the total sentiment score for each document in the corpus
doc_sentiment <- rowSums(dtm_mat * sentiment_scores)

# Get the top 10 positive and negative documents based on sentiment score
top_pos_docs <- names(sort(doc_sentiment, decreasing = TRUE)[1:20])
top_neg_docs <- names(sort(doc_sentiment, decreasing = FALSE)[1:20])


#Visualize the word frequencies:
# Create a data frame of the document names and sentiment scores
doc_sentiment_df <- data.frame(Document = rownames(dtm_mat), Sentiment = doc_sentiment)

# Create a bar chart of the sentiment scores
doc_sentiment_df_filtered <- doc_sentiment_df %>% 
  filter(abs(Sentiment) > 1) %>% 
  top_n(20, abs(Sentiment)) %>% 
  ggplot(aes(x = Document, y = Sentiment)) +
  geom_bar(stat = "identity", fill = "blue") +
  ggtitle("Sentiment Scores by Document") +
  xlab("Document") +
  ylab("Sentiment Score")+
  theme_bw()
View(doc_sentiment_df_filtered)
