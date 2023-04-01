library(tm)
library(SnowballC)
library(Matrix)
library(tidytext)
library(dplyr)
library(lexicon)
library(textdata)
library(textstem)
library(ggplot2)
library(wordcloud)


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
View(doc_sentiment)

# Get the top 20 positive and negative documents based on sentiment score
top_pos_docs <- names(sort(doc_sentiment, decreasing = TRUE)[1:20])
top_neg_docs <- names(sort(doc_sentiment, decreasing = FALSE)[1:20])


#Visualize the word frequencies:
# Create a data frame of the document names and sentiment scores
doc_sentiment_df <- data.frame(Document = rownames(dtm_mat), Sentiment = doc_sentiment)

# Create a bar chart of the sentiment scores
doc_sentiment_df_filtered <- doc_sentiment_df %>% 
  filter(abs(Sentiment) > 1) %>% 
  top_n(20, abs(Sentiment))

# # Define the colors for positive and negative sentiments
pos_color <- "#f67280"  # light blue
  neg_color <- "#00a7d1"
    
# Create the plot
ggplot(doc_sentiment_df_filtered, aes(x = Sentiment, y = reorder(Document, Sentiment))) +
  geom_bar(stat = "identity", aes(fill = ifelse(Sentiment >= 0, "Positive", "Negative"))) +
  scale_fill_manual(values = c(pos_color, neg_color), name = "Sentiment") +
  coord_flip() +
  labs(x = "Sentiment Score", y = "Words") +
  ggtitle("Sentiment Scores by Words") +
  theme_bw() +
  theme(axis.title = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))



# Convert the corpus to a document-term matrix
dtm <- DocumentTermMatrix(imdb_data)

# Find the most frequent terms in the corpus
freq_terms <- findFreqTerms(dtm, lowfreq = 100)

# Get the frequency of each term in the corpus
term_freq <- colSums(as.matrix(dtm[, freq_terms]))

# Create a data frame with the terms and their frequencies
word_freq_df <- data.frame(word = names(term_freq), freq = term_freq)

# Create the word cloud
wordcloud(words = word_freq_df$word, freq = word_freq_df$freq,
          min.freq = 100, max.words = 100, random.order = FALSE,
          colors = brewer.pal(8, "Dark2"))
