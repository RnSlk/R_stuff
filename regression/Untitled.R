##### Model Building #####
#converting factor variables into dummy 
library(dummies)
install.packages(dummies)
gender.dummies <- dummy(df$gender, sep='_')
MultipleLines.dummies <- dummy(df$MultipleLines, sep = "_")
...

#Training & Test Model Split
library(caTools)
df.training <- sample.split(df, SplitRatio = 0.3)
train <- subset(df,df.training==TRUE)
test <- subset(df,df.training==FALSE)

#Model Logistic Regression
churn.glm <- glm(churn ~., data=churn, family="binomial", train)
summary(churn.glm)

#wie mache ich geom_bar mit gendern und ob churn ja oder nein +farben
#siehe screenshot auf desktop



