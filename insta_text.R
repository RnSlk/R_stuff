########################
#import scraped data & merge df
########################
library(tidyverse)
comments <- read.csv("tf_comments.csv")
names <- read.csv("tf_names.csv")

names <- names$name[1:1113]
df <- cbind(names, comments)
df <- subset(df, select=-X)
view(df)
df$names <- df$name 
df <- subset(df, select=-name)
########################
#Data understanding
########################
library(tidyverse)
str(df, give.attr=FALSE)

#missing values?
sapply(df, function(x) sum(is.na(x)))

#convert names to numbers for data security
df$names <- gsub("a", "0", df$names)
df$names <- gsub("e", "1", df$names)
df$names <- gsub("i", "5", df$names)
df$names <- gsub("o", "2", df$names)
df$names <- gsub("u", "4", df$names)
df$names <- gsub("f", "3", df$names)
df$names <- gsub("d", "6", df$names)
head(df, 10) #proof

#are pople commenting often?
df_n <- df %>%
  count(names) %>%
  top_n(15, n) %>%
  arrange(desc(n)) %>%
  mutate(n = n) 
df_test <- prop.table(table(df_n$n))
view(df_test)
ggplot(df_test, aes(x=))

#boxplot
ggplot(df_n, aes(n)) +
  geom_boxplot()

df_n %>%
  filter(n<100)%>% #without outliers
  ggplot(aes(n)) +
  geom_histogram(binwidth=2)
#in 100 posts
#65% 3x; 20% 4x; 3% 6x; 6% 12x; 3% 102x (=content creator)

#create text length column with emojis & @
df$text_length <- nchar(df$text)
summary(df$text_length)
#boxplot 
ggplot(df, aes(text_length)) +
  geom_boxplot()
#histogram
ggplot(df, aes(text_length)) +
  geom_histogram()

########################
#text cleaning 
########################
library(tm)
head(df, n=5)

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

#text_length2 column to remove rows with empty text 
df$text_length2 <- nchar(df$text)
str(df)
df <- df %>%
  filter(text_length2 >0)
str(df)
df <- subset(df, select=-text_length2)
#removed 103 empty rows


########################
#comment frequency
########################
df_n2 <- df %>%
  count(names) %>%
  top_n(15, n) %>%
  arrange(desc(n)) %>%
  mutate(n = n) 
prop.table(table(df_n2$n))

########################
#tokenization
########################
library(tidytext)
tokens <- df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) #error bc english stopwords and text is german
head(tokens) #proof

#word frequency
tokens <- tokens %>%
  count(word) 

###########
#N-Grams 
###########

ngrams3_plot <- df %>%
  unnest_tokens(word, text, token="ngrams", n=3) %>%
  count(word) %>%
  top_n(10, n) %>%
  na.omit() %>%
  mutate(word2=fct_reorder(word, n)) %>%
  ggplot(aes(word2, n, fill=word))+
  geom_col(show.legend = FALSE) +
  coord_flip()
#most of comments are about in which grocery stores you can buy the product

########################
#wordcloud
########################
library(wordcloud)
wordcloud(
  words= tokens$word,
  freq = tokens$n,
  max.words = 20,
  colors = "blue")

########################
#sentiment analysis deutsche wörter!
########################





