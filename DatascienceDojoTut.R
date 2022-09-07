#https://youtu.be/4vuw0AsHeGw
df <- read.csv("spam.csv", stringsAsFactors = FALSE)

#recquired packages
library(caret)
library(tidyverse)
library(e1071)
library(quanteda)
library(randomForest)
library(irlba)


#######
#Data Cleaning
df <- df[,1:2]
#override labels
names(df) <- c("label", "text")
#Missing Values? -No
length(which(!complete.cases(df)))
df$label <- as.factor(df$label)
df$text <- as.vector(df$text)

#######
#Explore Data
str(df)
table(df$label) #anzahl der merkmalsausprägungen
100*prop.table(table(df$label)) #prozentual


#converting Byte into UTF8
df$text <- iconv(enc2utf8(df$text),sub="byte")
#text length of strings
df$text_length <- nchar(df$text)
summary(df$text_length)

#graphic
ggplot(df, aes(x=text_length, fill=label)) +
  geom_histogram(binwidth = 5) +
  labs(title = "Distribution Of Text Length", x="Text Length", y="Text Count")

###############
#Part 2: https://youtu.be/Y7385dGRNLM
###############

#Maschine Learning with caret 
set.seed(100)
#70/30 Stratified Split d.h gleiche proportionen wie df
indexes <- createDataPartition(df$label, p=.7, list=FALSE)
train <- df[indexes, ]
test <- df[-indexes, ]

#verify proportions
prop.table(table(df$label))


##########
#Part 3: Tokenization 
##########
#tokenization
train.tokens <- tokens(train$text, what="word", 
                       remove_punct = TRUE,
                       remove_symbols = TRUE,
                       remove_numbers = TRUE,
                       )
#lowercase 
train.tokens <- tokens_tolower(train.tokens)

#quanteda built in stopwords list
#Note: make sure you are using right stopword list for ur usecase
train.tokens <- tokens_select(train.tokens, pattern = stopwords(), 
                              selection = "remove")

#word stemming (= gleiche wörter zusammenfassen)
train.tokens <- tokens_wordstem(train.tokens, language = "english")

# create Dataframe(?)
train.tokens.dfm <- dfm(train.tokens, tolower = FALSE)

#transform to matrix
train.tokens.matrix <- as.matrix(train.tokens.dfm)

#Investigate The Effects Of Stemming 
colnames(train.tokens.matrix)[1:50]


########
#Part 4: https://youtu.be/IFhDlHKRHno
#Model Building
########

#create dataframe
train.tokens.df <- cbind(label = train$label, convert(train.tokens.dfm, to="data.frame"))

#cleanup column names
names(train.tokens.df) <- make.names(names(train.tokens.df))

#crossvalidation
set.seed(123)
cv.folds <- createMultiFolds(train$label, k=10, times=3)
cv.control <- trainControl(method="repeatedcv", number=10, repeats = 3,
                           index = cv.folds)

library(doSNOW)

#achtung braucht wahnsinnig viel GPU
#cluster to work on 10 logical cores
cl <- makeCluster(3, type="SOCK")
registerDoSNOW(cl)

#single decision tree to begin with (bc its fast)
rpart.cv.1 <- train(label ~ ., data = train.tokens.df.test, method="rpart",
                    trControl = cv.control, tuneLength = 7)
?train()

#stop clusters
stopCluster(cl)
  
#results
rpart.cv.1
#wofür steht cp? wofür kappa?
  
train.tokens.df.test<-sapply(train.tokens.df,function(x) as.numeric(as.character(train.tokens.df)))
  
###############
#Part 5: https://youtu.be/az7yf0IfWPM
###############

###### Term Frequency
#TF-IDF(t,d) = TF(t,d) * IDF(t)

#calculating relative term frequency TF
term.frequency <- function(row) {
  row/sum(row)
}

#calculating inverse document frequency IDF
inverse.doc.freq <- function(col) {
  corpus.size <- length(col)
  doc.count <- length(which(col>0))
  log10(corpus.size / doc.count)
}

#caluculate TF-IDF
tf.idf <- function(tf, idf) {
  tf * idf
}

#1. normalize all documents using TF
train.tokens.df <- apply(train.tokens.matrix, 1, term.frequency)
dim(train.tokens.df)
