#https://towardsdatascience.com/predict-customer-churn-with-r-9e62357d47b4

library(tidyverse)
library(plyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(caret)
library(MASS)
library(randomForest)
library(party)
#Dataframe https://www.kaggle.com/code/denizbektas/churn-prediction/notebook
df <- read.csv("churn_prediction.csv")

#####
#descriptive statistics 
names(df)
str(df, give.attr=FALSE)
summary(df)
head(df, 10)
view(df)

#Churn Overview
ggplot(df, aes(x= Churn)) +
  geom_bar(fill = c("Blue", "Red")) +
  labs(title = "Customer Churn Overview", x= "Churn", y="Frequency")
table(df$Churn) #5174 No, 1869 Yes



#####
#DATA CLEANING 
#remove columns
df <- subset(df, select=-customerID)

#remove NAs from TotalCharges 
sapply(df, function(x) sum(is.na(x))) #tabular
df <- na.omit(df)


################
#data transformation
###############

#Converting "No phone service" to "No" in 1 Column
df$MultipleLines <- gsub("No phone service", "No", df$MultipleLines)

#Converting "No internet service" to "No" in 6 Columns
df$OnlineSecurity <- gsub("No internet service", "No", df$OnlineSecurity)
df$OnlineBackup <- gsub("No internet service", "No", df$OnlineBackup)
df$DeviceProtection <- gsub("No internet service", "No", df$DeviceProtection)
df$TechSupport <- gsub("No internet service", "No", df$TechSupport)
df$StreamingTV <- gsub("No internet service", "No", df$StreamingTV)
df$StreamingMovies <- gsub("No internet service", "No", df$StreamingMovies)

#Grouping tenure in new column 
min(churn$tenure); max(churn$tenure) #min 0, max 72 months
group_tenure <- function(tenure){
  if (tenure >= 0 & tenure <= 12){
    return('0-12 Month')
  }else if(tenure > 12 & tenure <= 24){
    return('12-24 Month')
  }else if (tenure > 24 & tenure <= 48){
    return('24-48 Month')
  }else if (tenure > 48 & tenure <=60){
    return('48-60 Month')
  }else if (tenure > 60){
    return('> 60 Month')
  }
}

df$tenure_group <- sapply(df$tenure,group_tenure)
df$tenure_group <- as.factor(df$tenure_group)
#removing old column 
df <- subset(df, select=-tenure)

#Changing values from 0 or 1 to “No” or “Yes”.
df$SeniorCitizen <- as.factor(mapvalues(df$SeniorCitizen, from = c("0","1"),
                                        to = c("No", "Yes")))


######
#DATA ANALYSIS

#Correlation between numerical variables 
num_values <- sapply(df, is.numeric)
correlation_matrix <- cor(df[,num_values])
corrplot(correlation_matrix, method= "number")
#MonthlyCharges and TotalCharges have high correlation 0.65, i.e. are dependent 
#so we have to remove one of them 
df <- subset(df, select=-TotalCharges)



#####
# LOGISTIC REGRESSION

#Splitting in Test and Training Model
#make it reproducible
set.seed(100)
intrain<- createDataPartition(df$Churn,p=0.7,list=FALSE)
training<- df[intrain,]
testing<- df[-intrain,]
#confirm the splitting is correct
dim(training); dim(testing)

#Model Building
glm.churn <- glm(as.factor(Churn) ~ ., training, family="binomial"(link="logit"))
print(summary(glm.churn))

#Deviance Table
anova(glm.churn, test="Chisq")
#InternetService, Contract and tenure_group significantly reduce Deviance

#Accuracy of predictablity
testing$Churn <- as.character(testing$Churn)
testing$Churn[testing$Churn=="No"] <- 0
testing$Churn[testing$Churn=="Yes"] <- 1

fitted.results <- predict(glm.churn, newdata = testing, type="response")
fitted.results <- ifelse(fitted.results > 0.5, 1, 0)

#print accuracy
glm.accuracy <- mean(fitted.results != testing$Churn)
print(paste("Accuracy of logistic regression model is", 1-glm.accuracy))

#glm confusion matrix 
print("Confusion matrix for glm"); table(testing$Churn, fitted.results >0.5)

#Odds Ratio
library(MASS)
exp(cbind(OR=coef(glm.churn), confint(glm.churn)))

