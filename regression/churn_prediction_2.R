library(tidyverse)
library(plyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(caret)
library(MASS)
library(tidyverse)
url <- "https://storage.googleapis.com/kagglesdsdata/datasets/1710543/2800111/churn2.csv?X-Goog-Algorithm=GOOG4-RSA-SHA256&X-Goog-Credential=gcp-kaggle-com%40kaggle-161607.iam.gserviceaccount.com%2F20220319%2Fauto%2Fstorage%2Fgoog4_request&X-Goog-Date=20220319T064524Z&X-Goog-Expires=259199&X-Goog-SignedHeaders=host&X-Goog-Signature=777887b9131fb12c5c8f96a2b4c9ab3ab980941c01ba84157637c40ab4ce04df6809d0dd12788cb2f91b82412a24d364da42642ed8785f8e03280ba8c26e3f5cda30c0841bfe8ebe023a8db0e2faf244f315a17068c8658ed8cd20e6c29be24da124b7fb6ac0113a652dbd3c87c3488bff73bb4e8cacb9a0efef20cd6f347f9840d6229b6d91c98a166d8c7789dd2da39e6c8d617ed13093214aacfb7a766efd7e93082db394ae154683e14cb0b9adc0ce8637fdc2d6296d1abd3af76be361fc8fee460f602fdebe1cc9b592a88e57fc22d8725436f1386c5803ec0a0c7d7f8cfb5b988e132a996e7ae4e07d916d783bfe24dd283fa7a9ddc7d80efea7c4399a"
df <- read.csv(url)

#data inspection 
names(df)
str(df)
summary(df)
head(df, 10)
view(df)



#remove unneeded columns
df <- subset(df, select=-RowNumber)
df <- subset(df, select=-CustomerId)
df <- subset(df, select=-CreditScore) #nothing to do with churn probability
df <- subset(df, select=-Surname)

#rename "exited" to "churn"
df$Churn <- df$Exited
df <- subset(df, select=-Exited)

#Churn Overview
ggplot(df, aes(x=Churn))+
  geom_bar(fill= c("Blue", "Red"))+
  labs(title = "Churn Overview", x="Churn", y="frequency")
table(df$Churn)

#Missing Values? -No
library(VIM)
aggr(df)

#converting gender and geography to numbers
df$Gender <- as.character(df$Gender)
df$Gender[df$Gender=="Female"] <- 0
df$Gender[df$Gender=="Male"] <- 1
df$Gender <- as.numeric(df$Gender)


######
#Data Analysis
#checking if X Variables are independent
library(corrplot)
plot(df)
df_num <- sapply(df, is.numeric)
correlation_matrix <- cor(df[,df_num])
corrplot(correlation_matrix, method= "number")
#they are :)

#TRaining & Testing
set.seed(100)
intrain <- createDataPartition(df$Churn, p=.7, list=FALSE )
training <- df[intrain,]
testing <- df[-intrain,]
#confirm results
dim(training); dim(testing)


#Model Building 
glm.churn <- glm(as.factor(Churn)~., family="binomial"(link="logit"), data=training)
summary(glm.churn)

anova(glm.churn, test="Chisq")
#Geography, Age and IsActiveMember siginificantly reduce Deviance

fitted.results <- predict(glm.churn, newdata=testing, type="response" )
fitted.results <- ifelse(fitted.results >.5, 1, 0)

#Accuracy 
glm_acc <- mean(fitted.results != testing$Churn)
print(paste("The accuracy is", 1-glm_acc))













