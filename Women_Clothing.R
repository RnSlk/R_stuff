#womens clothing e-commerce reviews
reviews  <- read.csv("Womens Clothing.csv")

#####################
#1. Business understanding
######################
#interesting correlations?, most used words, and wordcombinations, overall sentiment 
#Topic modeling 

#####################
#Data exploration & understanding 
######################
library(tidyverse)
str(reviews, give.attr=FALSE)
summary(reviews)

#CLOTHING ID
table(reviews$Clothing.ID)

#AGE
summary(reviews$Age)
ggplot(reviews, aes(Age, fill="blue")) +
  geom_histogram(binwidth=4)
#we have some outliers

#RATING
reviews$Rating <- as.numeric(reviews$Rating)
ggplot(reviews, aes(Rating, fill=as.factor(Rating))) +
  geom_bar()

#RECOMMEND
prop.table(table(reviews$Recommended.IND))
ggplot(reviews, aes(as.factor(Recommended.IND), fill=as.factor(Recommended.IND))) +
  geom_bar()

#POSITIVE FEEDBACK COUNT PER CUSTOMER 
prop.table(table(reviews$Positive.Feedback.Count))
summary(reviews$Positive.Feedback.Count)
#some extreme outliers
ggplot(reviews, aes(Positive.Feedback.Count, fill="red")) +
  geom_histogram(binwidth=5)
#filter 
reviews %>%
  filter(Positive.Feedback.Count < 25) %>%
  ggplot(aes(Positive.Feedback.Count, fill="red")) +
  geom_histogram(binwidth=2)
#outlier detection
ggplot(reviews, aes(Positive.Feedback.Count)) +
  geom_boxplot()


table(reviews$Division.Name)
table(reviews$Department.Name)
table(reviews$Class.Name)


#TEXT LENGTH EXPLORATION
reviews$text_length <- nchar(reviews$Review.Text)
summary(reviews$text_length)
ggplot(reviews, aes(text_length)) +
  geom_histogram()
#some extreme outliers again 


#######################
#CORRELATIONS
######################
reviews.num <- reviews %>%
  select(Age, Rating, Recommended.IND, Positive.Feedback.Count, text_length)

library(corrplot)
corrplot(cor(reviews.num), method="number")
#strong between Recommend and Rating


#######################
#Data transformation
######################

#rename columns
reviews <- rename(reviews, text=Review.Text)
reviews <- rename(reviews, recommended=Recommended.IND)
reviews <- rename(reviews, feedback=Positive.Feedback.Count)
str(reviews) #proof

#select only interesting columns for further analysis 
df <- reviews %>%
  select(text, recommended, feedback, text_length, X)


#######################
#Text cleaning
######################

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

#######################
#Tokenization
######################
library(tidytext)
tokens <- df %>%
  unnest_tokens(input=text, output=word) %>%
  anti_join(stop_words)
names(tokens) #proof

#######################
#Stemming
######################
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

#recommended
recommended_facet_plot <- tokens %>%
  count(word, recommended) %>%
  group_by(recommended) %>%
  top_n(10, n) %>%
  ungroup() %>%
  mutate(word2 = fct_reorder(word, n)) %>%
  ggplot(aes(x=word2, y=n, fill=recommended)) +
  geom_col() +
  facet_wrap(~ recommended, scales="free_y")+
  coord_flip()

#####################
#ngrams
##################

ngrams4_plot <- df %>%
  unnest_tokens(input=text, output=word, token="ngrams", n=4) %>%
  count(word) %>%
  top_n(15, n) %>%
  na.omit() %>%
  mutate(word2 = fct_reorder(word, n)) %>%
  ggplot(aes(word2, n, fill=word)) +
  geom_col(show.legend = FALSE) +
  coord_flip()
  

#####################
#wordcloud
##################
library(wordcloud)
wordcloud(
  words =  tokens_stemmed$stem,
  freq = tokens_stemmed$n,
  max.words = 30,
  color="blue",
  random.order=FALSE
)

#####################
#sentiment analysis
##################
library(textdata)
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

# sentiment by recommended 
rating_sentiment_plot <- tokens %>%
  inner_join(get_sentiments("nrc")) %>%
  count(recommended, sentiment) %>%
  ggplot(aes(x=sentiment, y=n, fill=recommended )) +
  geom_col() +
  facet_wrap(~recommended, scale="free") +
  coord_flip() +
  labs(title = "Attention: different scales on y achsis!")


#####################
#topic modeling LDA
##################

#DTM model
dtm_df <- tokens %>%
  count(word, X) %>%
  cast_dtm(X, word, n) %>%
  as.matrix()

#topic modeling
library(topicmodels)
lda_out2 <- LDA(
  dtm_df,
  k=2, #number of k topics
  method="Gibbs",
  control= list(seed=1000))
lda_out2

#terms into topics probabilities
lda_topic <- lda_out2 %>%
  tidy(matrix="beta") %>%
  arrange(desc(beta))

word_probs2 <- lda_topic2 %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  mutate(term2 = fct_reorder(term, beta))

lda_topic_plot2 <- word_probs %>%
  ggplot(aes(term2, beta, fill=as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales="free") +
  coord_flip()
  

#with k=3
lda_out3 <- LDA(
  dtm_df,
  k=3, #number of k topics
  method="Gibbs",
  control= list(seed=42))
lda_topic3 <- lda_out3 %>%
  tidy(matrix="beta") %>%
  arrange(desc(beta))
word_probs3 <- lda_topic3 %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  mutate(term2 = fct_reorder(term, beta))
lda_topic_plot3 <- word_probs3 %>%
  ggplot(aes(term2, beta, fill=as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales="free") +
  coord_flip()

#with k=4
lda_out4 <- LDA(
  dtm_df,
  k=4, #number of k topics
  method="Gibbs",
  control= list(seed=42))
lda_topic4 <- lda_out4 %>%
  tidy(matrix="beta") %>%
  arrange(desc(beta))
word_probs4 <- lda_topic4 %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  mutate(term2 = fct_reorder(term, beta))
lda_topic_plot4 <- word_probs4 %>%
  ggplot(aes(term2, beta, fill=as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales="free") +
  coord_flip()
lda_topic_plot4












########################
lda_mod <- LDA(x=dtm_df, k=2, method="Gibbs", control=list(alpha=1, delta=0.1, seed=100, keep=1))


plot <- tidy(lda_mod, matrix="gamma") %>%
  mutate(topic = as.factor(topic)) %>%
  ggplot(aes(document, gamma, fill=topic)) +
  geom_col()
  

dtm <- df %>%
  unnest_tokens(text, word) %>%
  anti_join(stop_words) %>%
  count(id, word) %>%
  cast_dtm(document=id, term=word, value=n) %>%
  as.matrix()
#######################
#vorgehensweise 
dtm <- df %>%
  unnest_tokens(text, word) %>%
  anti_join(stop_words) %>%
  count(id, word) %>%
  cast_dtm(document=id, term=word, value=n)

mod <- LDA(
  dtm,
  k=4,
  method="Gibbs",
  control=list(alpha=1, seed=1000)
)
#show best 15 words of 4 topüics
terms(mod, k=15)

# Extract matrix gamma and plot it
tidy(mod, "gamma") %>% 
  mutate(document=as.numeric(document)) %>% 
  ggplot(aes(x=document, y=gamma)) + 
  geom_col(aes(fill=factor(topic)))

# Display the words whose probability is above the threshold
terms(mod, threshold=0.0075)













