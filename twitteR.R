library(twitteR)
library(graphics)
library(purrr)
library(stringr) 
library(tm)
library(syuzhet)
library(base64enc)
library(openssl)
library(httpuv)
#Connect to Twitter API:
api_key<- "iY008afNTpc1OtmVq7FO5FVH7" #consumer key
api_secret <- "VPCmKg9zn2xojwGlgvw7GlXjUkEURdD6ROu7l9CHlDh4Ak8Dlf"
access_token <- "1506194176274280453-CYlZzoD3afhereD4DhU1Zk7SDcGrmj"
access_token_secret <- "PMaubl0K21v8uKxXFuuG8az5CndFVvXuItHg3ECT69MO6"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

#Get tweets:
prat_tweets <- userTimeline("prattprattpratt", n = 250)
oprah_tweets <- userTimeline("Oprah", n = 250)
neil_tweets <- userTimeline("neiltyson", n = 250)
mar_tweets <- userTimeline("billmaher", n = 250)
kutch_tweets <- userTimeline("aplusk", n = 250)

tweets<- tbl_df(map_df(c(prat_tweets,oprah_tweets,neil_tweets,
                         mar_tweets,kutch_tweets),as.data.frame))  

write.csv(tweets, file="tweets.csv", row.names=FALSE)  


#Read in data:
setwd("C:/Users/mateo/Documents/Repo/text-analysis")
tweets<-read.csv("tweets.csv")


#Clean up data:
twitterCorpus <-Corpus(VectorSource(tweets$text))
head(twitterCorpus, 5)
twitterCorpus<- tm_map(twitterCorpus, content_transformer(tolower))
twitterCorpus<- tm_map(twitterCorpus,removeWords,stopwords("en"))
twitterCorpus<- tm_map( twitterCorpus,removeNumbers)
twitterCorpus<- tm_map( twitterCorpus,removePunctuation)

####meine Idee
twitterCorpus <- twitterCorpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords,stopwords("en")) %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  gsub("http.*", "") %>%
  gsub("https.*", "") %>%
  gsub("@.*", "") %>%
  gsub("#.*", "") 

#...
#################################################
#Sentiment analysis:
# find count of 8 emotional sentiments
emotions<-get_nrc_sentiment(twitterCorpus$content)
barplot(colSums(emotions),cex.names = .7,
        col = rainbow(10),
        main = "Sentiment scores for tweets"
)

# sentiment positivity rating
#new column 
sentiment <-get_sentiment(twitterCorpus$content)
#new dataframe
sentimentTweets<- cbind(tweets, sentiment)
# mean of sentiment positivity

meanSentiment <-function(i,n){
  mean(sentimentTweets$sentiment[i:n])
}

(scores<-c(prat=meanSent(1,250),
           oprah=meanSent(251,500),
           neil=meanSent(501,750),
           maher=meanSent(751,849),
           astk=meanSent(850,1002)))


################################
#Cluster analysis:
  

# convert to stem words
twitterCorpus<-tm_map(twitterCorpus,stemDocument)
# build document term matrix
dtm<-DocumentTermMatrix(twitterCorpus)
dtm
mat<-as.matrix(dtm)
# create distance matrix
d<-dist(mat)
# input distance matrix into hclust function using method "ward.D"
groups<-hclust(d,method="ward.D")
plot(groups,hang=-1)
cut<-cutree(groups,k=6)
newMat<-dplyr::bind_cols(tweets,data.frame(cut))
table(newMat$screenName,newMat$cut)

