# Twitter apps information (Keys are removed, fill in your own)
api_key <- ""
api_secret <- ""
acces_token < ""
acces_token_secret <- ""

# Setup twitteR
library(twitteR)
setup_twitter_oauth(api_key, api_secret, acces_token, acces_token_secret)

################################################################################
###################     Getting tweets before release    #######################
################################################################################
tweets <- searchTwitteR('fifa20', n=100000, lang='en')
tweetsdf <- twListToDF(tweets)
write.csv(tweetsdf, file ='fifa20.csv', row.names = F)

# Build Corpus
fifa <- read.csv(file.choose(), header = T )
library(tm)
library(textreg)
corpus <- iconv(fifa$text, "utf-8")
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

# Data cleaning
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords(kind='en'))
removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
corpus <- tm_map(corpus, content_transformer(removeURL))
corpus <- tm_map(corpus, strip_retweets)
corpus <- tm_map(corpus, removeWords, c('fifa20', 'fifa', 'the'))
corpus <- tm_map(corpus, stripWhitespace)

# Term document matrix & Word count
tdm <- TermDocumentMatrix(corpus)
tdm <- as.matrix(tdm)

findAssocs(tdm, "launch", 0.7)

Word_count <- rowSums(tdm)
Word_count <- sort(Word_count, decreasing = TRUE)

Word_count_dataframe <- data.frame(Word_count)
write.csv(Word_count_dataframe, file ='fifadataframe.csv', row.names=TRUE)

################################################################################
###################     Getting tweets after release    #######################
################################################################################
tweets <- searchTwitteR('fifa20', n=100000, lang='en')
tweetsdf <- twListToDF(tweets)
write.csv(tweetsdf, file ='fifa20_2.csv', row.names = F)

# Build Corpus
fifa_2 <- read.csv(file.choose(), header = T )
library(tm)
library(textreg)
corpus_2 <- iconv(fifa_2$text, "utf-8")
corpus_2 <- Corpus(VectorSource(corpus_2))
inspect(corpus_2[1:5])

# Data cleaning
corpus_2 <- tm_map(corpus_2, tolower)
corpus_2 <- tm_map(corpus_2, removePunctuation)
corpus_2 <- tm_map(corpus_2, removeNumbers)
corpus_2 <- tm_map(corpus_2, removeWords, stopwords(kind='en'))
removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
corpus_2 <- tm_map(corpus_2, content_transformer(removeURL))
corpus_2 <- tm_map(corpus_2, strip_retweets)
corpus_2 <- tm_map(corpus_2, removeWords, c('fifa20', 'fifa', 'the'))
corpus_2 <- tm_map(corpus_2, stripWhitespace)

# Term document matrix & Word count
tdm_2 <- TermDocumentMatrix(corpus_2)
tdm_2 <- as.matrix(tdm_2)

findAssocs(tdm_2, "money", 0.7)
findAssocs(tdm_2, "fut", 0.7)
Word_count_2 <- rowSums(tdm_2)
Word_count_2 <- sort(Word_count_2, decreasing = TRUE)

Word_count_dataframe_2 <- data.frame(Word_count_2)
write.csv(Word_count_dataframe_2, file ='fifadataframe_2.csv', row.names=TRUE)

################################################################################
#######################       Sentiment Analysis         #######################
################################################################################
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

# Obtain sentiment scores before release
tweets <- read.csv(file.choose(), header = T )
tweets <- iconv(tweets$text, "utf-8")
par("mar")
par(mar=c(1,1,1,1))
Sentiment_Scores <- get_nrc_sentiment(tweets)
colSums(Sentiment_Scores)

barplot(colSums(s),
        las = 2,
        ylab = 'count',
        main = 'Sentiment scores for fifa 20 tweets before release')

# Obtain sentiment scores After release
tweets <- read.csv(file.choose(), header = T )
tweets <- iconv(tweets$text, "utf-8")
par("mar")
par(mar=c(1,1,1,1))
Sentiment_Scores <- get_nrc_sentiment(tweets)
colSums(Sentiment_Scores)

barplot(colSums(s),
        las = 2,
        ylab = 'count',
        main = 'Sentiment scores for fifa 20 tweets after release')



