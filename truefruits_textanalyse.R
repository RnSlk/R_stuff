df <- read.csv("truefruits_text.csv")
view(df)

################
# 2. data understanding & exploration
################
library(tidyverse)
summary(df)

#TEXT LENGTH EXPLORATION
df$text_length <- nchar(df$text)
summary(df$text_length)


################
# 2. text preprocessing
################
library(tm)
head(df, n=5)

#text cleaning 
df$text <- gsub("http.*", "", df$text)
df$text <- gsub("https.*", "", df$text)
df$text <- gsub("@*", "", df$text)
df$text <- gsub("#*", "", df$text)
df$text <- gsub("^ ", "", df$text)
df$text <-  gsub(" $", "", df$text)
df$text <- gsub("[[:punct:]]", "", df$text)
df$text <- gsub("[[:digit:]]", "", df$text)
df$text <- tolower(df$text)
head(df$text, 10) #proof

##################
# 4. tokenization (new row for each word)
#################
library(tidytext)

#tokenizaion
tokens <- df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) #wenn functioniert: stop_words2

tokens_n <- tokens %>%
  count(word) %>%
  top_n(15, n) %>%
  arrange(desc(n)) 

######################################
#stemming 
library(SnowballC)
tokens_stemmed <- tokens %>%
  mutate(stem = wordStem(word)) %>%
  count(stem, sort = TRUE)
head(tokens_stemmed, n=10) #proof

#visualization
stemmed_plot <- tokens_stemmed %>%
  top_n(15, n) %>%
  arrange(desc(n)) %>%
  mutate(stem2 = fct_reorder(stem, n)) %>%
  ggplot(aes(x=stem2, y=n, fill = stem)) +
  geom_col() +
  coord_flip() +
  labs(title = "most used stemmed word", 
       y = "total number", 
       x = "top 15 stemmed words")

###########
#N-Grams 
###########

ngrams3_plot <- df %>%
  unnest_tokens(word, text, token="ngrams", n=3) %>%
  count(word) %>%
  top_n(15, n) %>%
  na.omit() %>%
  mutate(word2=fct_reorder(word, n)) %>%
  ggplot(aes(word2, n, fill=word))+
  geom_col(show.legend = FALSE) +
  coord_flip()

######################
#wordcloud for stemmed words
#####################
library(wordcloud)

wordcloud(
  words= tokens_stemmed$stem,
  freq = tokens_stemmed$n,
  max.words = 50,
  colors = "blue"
)


##########################
#sentiment 
#########################
#bing, nrc, afinn
#lexicon exploration to get understanding 
library(textdata)
sentiment_nrc <-  get_sentiments("nrc") %>%
  count(sentiment) %>%
  mutate(sentiment2 = fct_reorder(sentiment, n)) %>%
  arrange(desc(n))
#visualization
facet_sentiment_plot <- tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment) %>%
  group_by(sentiment) %>%
  top_n(10, n) %>%
  ungroup() %>%
  mutate(sentiment2 = fct_reorder(word, n)) %>%
  ggplot(aes(x=word, y=n, fill=sentiment)) +
  geom_col() +
  facet_wrap(~ sentiment, scale="free") +
  coord_flip() 

# sentiment by rating 
rating_sentiment_plot <- tokens %>%
  inner_join(get_sentiments("nrc")) %>%
  count(rating, sentiment) %>%
  ggplot(aes(x=sentiment, y=n )) +
  geom_col() +
  facet_wrap(~rating, scale="free") +
  coord_flip() +
  labs(title = "Attention: different scales on y achsis!")

# sentiment by feedback
feedback_sentiment_plot <- tokens %>%
  inner_join(get_sentiments("nrc")) %>%
  count(feedback, sentiment) %>%
  ggplot(aes(x=sentiment, y=n, fill="blue" )) +
  geom_col() +
  facet_wrap(~feedback, scale="free") +
  coord_flip() +
  labs(title = "Attention: different scales on y achsis!")

