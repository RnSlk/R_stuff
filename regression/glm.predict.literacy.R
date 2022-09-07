#https://youtu.be/HFO36RrmNc4


#convert Zielvariable in factor 
as.factor(df$literacy) #check if factor already 
df$literacy <- as.factor(df$literacy) #converting

#Training and testmodel
set.seed(100)
library(caTools)
df.training <- sample.split(df, SplitRatio = 0.3)
train <- subset(df, df.training==TRUE)
test <- subset(df, df.training==FALSE)

#logistic regression model
glm.literacy <- glm(literacy ~ education_level + age +
             is_urban, data=train, family="binomial",  )


#predict literacy 
glm.predict.literacy <- predict(glm.literacy, test, type="response")
head(glm.predict.literacy)
#je näher an 0 desto geringer die chance dass literacy,
#je näher an 1 desto höher

#neue spalte createn
df$predict.literacy <- ifelse(glm.predict.literacy >= .5, "True", "False")
head(df$predict.literacy, n=10)

#Determine accuracy of model
accuracy <- mean(head(df$predict.literacy, n=35)) == head(poverty$literacy, n=35)
print(accuracy)