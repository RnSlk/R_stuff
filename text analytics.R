#text analysis

################
# 1. business question understanding:
#what word combinations are most often used
#what is the overall sentiment 
################
library(tidyverse)
amazon <- read_tsv("amazon.tsv")
view(amazon)

################
# 2. data understanding & exploration
################
summary(amazon)
str(amazon, give.attr=FALSE)
#we'll transform date later on

#missing values?
sapply(amazon, function(x) sum(is.na(x)))
amazon <- na.omit(amazon) #remove them

#VARIATION
table(amazon$variation)

#FEEDBACK 
table(amazon$feedback)#
prop.table(table(amazon$feedback))
ggplot(amazon, aes(as.factor(feedback), fill="red")) +
  geom_bar() +
  labs(x = "feedback yes or no", y = "total number", 
       title = "distribution of feedbacks")


#RATINGS
prop.table(table(amazon$rating))
ggplot(amazon, aes(x=rating, fill = as.factor(rating))) +
  geom_bar() +
  labs(x = "ratings  1 - 5", y = "total number", title = "distribution of ratings")

#TEXT LENGTH EXPLORATION
amazon$text_length <- nchar(amazon$verified_reviews)
summary(amazon$text_length)

##################################
#CORRELATIONS EXPLORATION
##################################

cor(as.numeric(amazon$rating), amazon$feedback, method="pearson")
#-yes, high correlation

#Text Length - Rating
ggplot(amazon, aes(x= text_length, fill=as.factor(rating))) +
  geom_histogram(binwidth = 50)
cor(as.numeric(amazon$rating), amazon$text_length, method="pearson")

#Text Length - Feedback
ggplot(amazon2, aes(x= text_length, fill=feedback)) +
  geom_histogram(binwidth = 50)
cor(as.numeric(amazon$feedback), amazon$text_length, method="pearson")

#keeping only columns we need
df <- amazon %>%
  select(rating, verified_reviews, feedback, text_length)
df <- rename(df, text = verified_reviews)
head(df) #proof

################
# 3. text preprocessing
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

#custom stopwords 
word <- c("amazon", "echo", "alexa")
lexicon <-  rep("SMART", times=length(word))
custom_stop_words <- data.frame(word, lexicon)
names(custom_stop_words) <- c("word", "lexicon")

#combine them
stop_words2 <- stop_words %>%
  bind_rows(custom_stop_words)

#tokenizaion
tokens <- df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words2) #wenn functioniert: stop_words2

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

#facet wrap with ratings and feedback 
#ratings
ratings_facet_plot <- tokens %>%
  count(word, rating) %>%
  group_by(rating) %>%
  top_n(10, n) %>%
  ungroup() %>%
  mutate(word2 = fct_reorder(word, n)) %>%
  ggplot(aes(x=word2, y=n, fill=rating)) +
  geom_col() +
  facet_wrap(~ rating, scales="free_y")+
  coord_flip()

#feedback
feedback_facet_plot <- tokens %>%
  count(word, feedback) %>%
  group_by(feedback) %>%
  top_n(10, n) %>%
  ungroup() %>%
  mutate(feedback2 = fct_reorder(word, n)) %>%
  ggplot(aes(feedback2, n, fill=feedback)) +
  geom_col() +
  facet_wrap(~ feedback, scales="free_y") +
  coord_flip()
#the people who gave no feedback (=0), used completly different words than those who gave
  


###########
#N-Grams 
###########

ngrams4_plot <- df %>%
  unnest_tokens(word, text, token="ngrams", n=4) %>%
  count(word) %>%
  top_n(15, n) %>%
  na.omit() %>%
  mutate(word2=fct_reorder(word, n)) %>%
  ggplot(aes(word2, n, fill=word))+
  geom_col() +
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



#########################
#topic modeling with unsupervised ML
#########################





  
























