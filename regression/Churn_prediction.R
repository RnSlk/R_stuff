churn <- read.csv("churn_prediction.csv")

##### descriptive statistics
names(churn)
summary(churn)
str(churn)
head(churn)
#Churn Overview
ggplot(df, aes(x= Churn)) +
  geom_bar(binwidth = 1, fill = c("Blue", "Red")) +
  labs(title = "Customer Churn Overview", x= "Churn", y="Frequency")



####### Data Cleaning
churn <- subset(churn, select=-customerID)
library(VIM)
aggr(churn)
#NA in TotalCharges
churn <- na.omit(churn)
#Zielvariable Y in factor umwandeln wenn nötig
as.factor(churn$churn)
#churn in 0 und 1 umwandeln
churn$Churn <- as.character(churn$Churn)
churn$Churn[churn$Churn=="Yes"]<-1
churn$Churn[churn$Churn=="No"]<-0
churn$Churn <- as.numeric(churn$Churn)
#X Variablen sind unabhängig voneinenader?
cor(churn, method="spearman") #Problem weil alle strings


##### Model Building
#Test and Training Model
library(caTools)
churn.training.model <- sample.split(churn, SplitRatio = 0.3)
churn.training <- subset(churn, churn.training.model ==TRUE )
churn.testing <- subset(churn, churn.training.model ==FALSE )

#Model Building 
glm.churn <- glm(as.factor(Churn) ~ .,data=churn, family="binomial")

#Model Testing
glm.predict.churn <- predict(glm.churn, data=churn, type='response')
head(glm.predict.churn)

#new column with predictions
churn$predict.churn <- ifelse(glm.predict.churn >= .5, "True", "False")





