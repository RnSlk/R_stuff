
library(tm)
tweets <- read.csv("apple.csv")
tweets <- subset(tweets, select=-sentiment)

######################
#data exploration
######################
summary(tweets)

#NA?
sapply(tweets, function(x) sum(is.na(x)))

#text length
tweets$text_length <- nchar(tweets$text)
#visualizing distribution
text_length_plot <- ggplot(tweets, aes(text_length, fill="blue")) +
  geom_histogram(binwidth=5)+
  labs(title = "Distribution of text length",
       x = "text length",
       y = "total number")

#Data Cleaning
tweets$text <- gsub("http.*","", tweets$text)
tweets$text <- gsub("https.*","", tweets$text)
tweets$text <- gsub("#*","", tweets$text)
tweets$text <- gsub("@*","", tweets$text)
tweets$text <- gsub("^ ", "", tweets$text)
tweets$text <- gsub(" $", "", tweets$text)
tweets$text <- gsub("[[:digit:]]", "", tweets$text)
tweets$text <- gsub("[[:punct:]]", "", tweets$text)
tweets$text <- tolower(tweets$text)
head(tweets, 5)

#########################
#tokenization
#########################
library(tiytext)

tokens_unclean <- tweets %>%
  unnest_tokens(word, text) %>%
  count(word) %>%
  top_n(30, n)

#lets remove stopwords 
word <- c("apple", "aapl")
lexicon <- rep("SMART", times=length(word))
custom_stop_words <- data.frame(word, lexicon)
names(custom_stop_words) <- c("word", "lexicon")

#custom stopwords
stop_words2 <- stop_words %>%
  bind_rows(custom_stop_words)

#remove them 
tokens <- tweets %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words2) %>%
  count(word) %>%
  arrange(desc(n))
head(tokens, 10) #proof


##############
#ngram
##############
ngram_plot <- tweets %>%
  unnest_tokens(word, text, token="ngrams", n=3) %>%
  anti_join(stop_words2) %>%
  count(word) %>%
  top_n(15, n) %>%
  na.omit() %>%
  mutate(word2=fct_reorder(word, n)) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x=word2, y=n, fill="blue")) +
  geom_col() +
 coord_flip() 


##############
#word cloud 
##############
library(wordcloud)
wordcloud <- wordcloud(
  words = tokens$word,
  freq = tokens$n,
  max.words = 50,
  colors = "blue"
)


########################
#sentiment
#######################
library(textdata)
#sentiment for tokens 
tokens_nrc_plot <- tokens %>%
  inner_join(get_sentiments("nrc")) %>%
  count(sentiment) %>%
  mutate(sentiment2 = fct_reorder(sentiment, n)) %>%
  arrange(desc(n)) %>%
  ggplot(aes(sentiment2, n, fill="red")) +
  geom_col() +
  coord_flip()
  
#sentiment for tweets 
tweets_bing <- tweets_text %>%
  inner_join(get_sentiments("nrc"))













