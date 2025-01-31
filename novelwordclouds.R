getwd()

wd <- "/Users/alicethesen/Desktop/R/Practice Data Sets"

setwd(wd)

getwd()

# The Bell Jar

#install.packages("rvest")
#install.packages("tm")
#install.packages("wordcloud")
#install.packages("RColorBrewer")
#install.packages("pdftools")


library(rvest)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(pdftools)

# Scrape the webpage
url <- "https://www.gutenberg.ca/ebooks/plaths-belljar/plaths-belljar-00-h.html"
page <- read_html(url)
text <- html_text(html_nodes(page, "body"))
writeLines(text, "belljar.txt")


# Preprocess the text
corpus <- Corpus(VectorSource(text))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("en"))

# Create a term-document matrix
tdm <- TermDocumentMatrix(corpus)
tdm_matrix <- as.matrix(tdm)
word_freq <- sort(rowSums(tdm_matrix), decreasing = TRUE)
word_freq_df <- data.frame(word = names(word_freq), freq = word_freq)

#word_freq_df <- word_freq_df[-c(1,2,3,4,6,7,9,10,11),]

set.seed(1234)  # For reproducibility
wordcloud(words = word_freq_df$word,
          freq = word_freq_df$freq,
          min.freq = 6,
          max.words = 100,
          random.order = FALSE,
          colors = brewer.pal(8, "Dark2"))


# Catcher in the Rye

# Scrape the webpage
url <- "https://archive.org/stream/1_20191103_20191103_1326/1_djvu.txt"
page <- read_html(url)
text <- html_text(html_nodes(page, "body"))
writeLines(text, "catcher.txt")


# Preprocess the text
corpus <- Corpus(VectorSource(text))

# Convert to lowercase
corpus <- tm_map(corpus, content_transformer(tolower))

# Remove punctuation and numbers
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)

# Remove standard stopwords and add custom ones
custom_stopwords <- c(stopwords("en"),"she", "he", "you", "I", "we", "they", "it", 
                      "said", "did", "don’t", "didn’t", "couldn’t", "would", "wasn’t", 
                      "is", "are", "be", "got", "has", "have", "know", "think", "take", 
                      "get", "like", "just", "old", "right", "sort", "even", 
                      "quite", "always", "around", "what", "that’s", "its")
corpus <- tm_map(corpus, removeWords, custom_stopwords)

# Create a term-document matrix
tdm <- TermDocumentMatrix(corpus)
tdm_matrix <- as.matrix(tdm)
word_freq <- sort(rowSums(tdm_matrix), decreasing = TRUE)
word_freq_df <- data.frame(word = names(word_freq), freq = word_freq)

word_freq_df <- word_freq_df[-c(1,7,10),]

set.seed(201)  # For reproducibility
par(mar = c(0, 0, 0, 0)) 
wordcloud(words = word_freq_df$word,
          freq = word_freq_df$freq,
          min.freq = 4,
          max.words = 110,
          random.order = FALSE,
          rot.per = 0.25,  # Adjust rotation percentage if needed
          scale = c(3, 0.5),  # Adjust word size scaling
          colors = brewer.pal(8, "Dark2"))




# To Kill a Mockingbird

# Scrape the webpage
url <- "https://archive.org/stream/ToKillaMockingbirdFullText/ToKillaMockingbird-FullText_djvu.txt"
page <- read_html(url)
text <- html_text(html_nodes(page, "body"))
writeLines(text, "mockingbird.txt")


# Preprocess the text
corpus <- Corpus(VectorSource(text))

# Convert to lowercase
corpus <- tm_map(corpus, content_transformer(tolower))

# Remove punctuation and numbers
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)

# Remove standard stopwords and add custom ones
custom_stopwords <- c(stopwords("en"),"she", "he", "you", "I", "we", "they", "it", 
                      "said", "did", "don’t", "didn’t", "couldn’t", "would", "wasn’t", 
                      "is", "are", "be", "got", "has", "have", "know", "think", "take", 
                      "get", "like", "just", "old", "right", "sort", "even", 
                      "quite", "always", "around", "what", "that’s", "its")
corpus <- tm_map(corpus, removeWords, custom_stopwords)

# Create a term-document matrix
tdm <- TermDocumentMatrix(corpus)
tdm_matrix <- as.matrix(tdm)
word_freq <- sort(rowSums(tdm_matrix), decreasing = TRUE)
word_freq_df <- data.frame(word = names(word_freq), freq = word_freq)

word_freq_df <- word_freq_df[-c(1,7,10),]

set.seed(20)  # For reproducibility
par(mar = c(0, 0, 0, 0)) 
wordcloud(words = word_freq_df$word,
          freq = word_freq_df$freq,
          min.freq = 4,
          max.words = 110,
          random.order = FALSE,
          rot.per = 0.25,  # Adjust rotation percentage if needed
          scale = c(3, 0.5),  # Adjust word size scaling
          colors = brewer.pal(8, "Dark2"))


# Secret History

# Scrape the webpage
url <- "https://archive.org/stream/the-secret-history-donna-tartt-z-library/The%20Secret%20History%20%28Donna%20Tartt%29%20%28Z-Library%29_djvu.txt"
page <- read_html(url)
text <- html_text(html_nodes(page, "body"))
writeLines(text, "secrethistory.txt")


# Preprocess the text
corpus <- Corpus(VectorSource(text))

# Convert to lowercase
corpus <- tm_map(corpus, content_transformer(tolower))

# Remove punctuation and numbers
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)

# Remove standard stopwords and add custom ones
custom_stopwords <- c(stopwords("en"),"she", "he", "you", "I", "we", "they", "it", 
                      "said", "did", "don’t", "didn’t", "couldn’t", "would", "wasn’t", 
                      "is", "are", "be", "got", "has", "have", "know", "think", "take", 
                      "get", "like", "just", "old", "right", "sort", "even", 
                      "quite", "always", "around", "what", "that’s", "its")
corpus <- tm_map(corpus, removeWords, custom_stopwords)

# Create a term-document matrix
tdm <- TermDocumentMatrix(corpus)
tdm_matrix <- as.matrix(tdm)
word_freq <- sort(rowSums(tdm_matrix), decreasing = TRUE)
word_freq_df <- data.frame(word = names(word_freq), freq = word_freq)

word_freq_df <- word_freq_df[-c(1,2,4),]

set.seed(201)  # For reproducibility
par(mar = c(0, 0, 0, 0)) 
wordcloud(words = word_freq_df$word,
          freq = word_freq_df$freq,
          min.freq = 2,
          max.words = 110,
          random.order = FALSE,
          rot.per = 0.25,  # Adjust rotation percentage if needed
          scale = c(3, 0.5),  # Adjust word size scaling
          colors = brewer.pal(8, "Dark2"))



# Normal People

filepath <- "/Users/alicethesen/Desktop/R/Practice_Data_Sets/normalpeople.pdf"
pdf_text <- pdf_text(filepath)

full_text <- paste(pdf_text, collapse = " ")

# Preprocess the text
corpus <- Corpus(VectorSource(full_text))

# Convert to lowercase
corpus <- tm_map(corpus, content_transformer(tolower))

# Remove punctuation and numbers
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)

# Remove standard stopwords and add custom ones
custom_stopwords <- c(stopwords("en"),"she", "he", "you", "I", "we", "they", "it", 
                      "said", "did", "don’t", "didn’t", "couldn’t", "would", "wasn’t", 
                      "is", "are", "be", "got", "has", "have", "know", "think", "take", 
                      "get", "like", "just", "old", "right", "sort", "even", 
                      "quite", "always", "around", "what", "that’s", "its")
corpus <- tm_map(corpus, removeWords, custom_stopwords)

# Create a term-document matrix
tdm <- TermDocumentMatrix(corpus)
tdm_matrix <- as.matrix(tdm)
word_freq <- sort(rowSums(tdm_matrix), decreasing = TRUE)
word_freq_df <- data.frame(word = names(word_freq), freq = word_freq)

word_freq_df <- word_freq_df[-c(1,2,4),]

set.seed(201)  # For reproducibility
par(mar = c(0, 0, 0, 0)) 
wordcloud(words = word_freq_df$word,
          freq = word_freq_df$freq,
          min.freq = 2,
          max.words = 110,
          random.order = FALSE,
          rot.per = 0.25,  # Adjust rotation percentage if needed
          scale = c(3, 0.5),  # Adjust word size scaling
          colors = brewer.pal(8, "Dark2"))


# Wuthering Heights

# Scrape the webpage
url <- "https://www.gutenberg.org/files/768/768-h/768-h.htm"
page <- read_html(url)
text <- html_text(html_nodes(page, "body"))
writeLines(text, "secrethistory.txt")


# Preprocess the text
corpus <- Corpus(VectorSource(text))

# Convert to lowercase
corpus <- tm_map(corpus, content_transformer(tolower))

# Remove punctuation and numbers
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)

# Remove standard stopwords and add custom ones
custom_stopwords <- c(stopwords("en"),"she", "he", "you", "I", "we", "they", "it", 
                      "said", "did", "don’t", "didn’t", "couldn’t", "would", "wasn’t", 
                      "is", "are", "be", "got", "has", "have", "know", "think", "take", 
                      "get", "like", "just", "old", "right", "sort", "even", 
                      "quite", "always", "around", "what", "that’s", "its")
corpus <- tm_map(corpus, removeWords, custom_stopwords)

# Create a term-document matrix
tdm <- TermDocumentMatrix(corpus)
tdm_matrix <- as.matrix(tdm)
word_freq <- sort(rowSums(tdm_matrix), decreasing = TRUE)
word_freq_df <- data.frame(word = names(word_freq), freq = word_freq)

word_freq_df <- word_freq_df[-c(1,2,6,9,15),]

set.seed(201)  # For reproducibility
par(mar = c(0, 0, 0, 0)) 
wordcloud(words = word_freq_df$word,
          freq = word_freq_df$freq,
          min.freq = 2,
          max.words = 110,
          random.order = FALSE,
          rot.per = 0.25,  # Adjust rotation percentage if needed
          scale = c(3, 0.5),  # Adjust word size scaling
          colors = brewer.pal(8, "Dark2"))

