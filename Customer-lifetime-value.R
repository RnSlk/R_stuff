df <- read.csv("CLV.csv")
library(tidyverse)
###################
# Business understanding
##################
#we want to model the customer lifetime value and find out what the most important
#features are.


#################
# data exploration
################

#analysis with IBM Cognos analytics tool to get a first sense of the data 
#analysis shows that there is a strong correlation btw CLV,
#number of policies, vehicle class, monthly premium auto, coverage and total claim amount 

summary(df)
str(df, give.attr=FALSE)


#MISSING VALUES -no
sapply(df, function(x) sum(is.na(x)))

#STATE 
prop.table(table(df$State))
ggplot(df, aes(x=State, fill=as.factor(State))) +
  geom_bar() +
  coord_flip()

#CUSTOMER LIFETIME VALUE 
summary(df$Customer.Lifetime.Value)
ggplot(df, aes(x=Customer.Lifetime.Value)) +
  geom_histogram(binwidth=500)
#rechtsschief und deutlich positiv, sehr viele outlier 
#meisten beibis zu 20 000

#RESPONSE
ggplot(df, aes(x=Response, fill=Response)) +
  geom_bar()
prop.table(table(df$Response))
#we can remove Response, bc Distribution is not broad

#COVERAGE
unique(df$Coverage)
ggplot(df, aes(x=Coverage, fill=Coverage)) +
  geom_bar()

#EDUCATION
unique(df$Education)
ggplot(df, aes(x=Education, fill=Education)) +
  geom_bar()

#EMPLOYMENT.STATUS
unique(df$EmploymentStatus) #5 observations
ggplot(df, aes(x=EmploymentStatus, fill=EmploymentStatus)) +
  geom_bar()
#we can remove later on "disabled", "medical leave" and "retired"

#GENDER
ggplot(df, aes(x=Gender, fill=Gender)) +
  geom_bar()
#we can remove them, bc Distribution is really similiar

#INCOME
summary(df$Income)
#income above 0 (unemployed have 0 income)
df%>%
  filter(Income > 0) %>%
ggplot(aes(x=Income)) +
  geom_histogram(binwidth=500)
#broad range between 1 and 100 000, most of them between 25k and 50k

#LOCATION CODE
unique(df$Location.Code)
ggplot(df, aes(x=Location.Code, fill=Location.Code)) +
  geom_bar()
#we'll change it later to "non-urban" and "urban" to make it binary 

#MARITAL STATUS
ggplot(df, aes(x=Marital.Status, fill=Marital.Status)) +
  geom_bar()
#we'll summarize "single" and "divorced" later

#MONTHLY PREMIUM AUTO 
ggplot(df, aes(Monthly.Premium.Auto, fill="red")) +
  geom_histogram(binwidth=10)

#Months.Since.Last.Claim 
ggplot(df, aes(Months.Since.Last.Claim , fill="red")) +
  geom_histogram(binwidth=1)
#most of them between 0 and 15

#Months.Since.Policy.Inception
ggplot(df, aes(Months.Since.Policy.Inception , fill="red")) +
  geom_histogram(binwidth=5)
#similiar distribution 

#Number.of.Open.Complaints 
ggplot(df, aes(x=Number.of.Open.Complaints, fill=Number.of.Open.Complaints)) +
  geom_bar()

#Policy.Type 
ggplot(df, aes(x=Policy.Type, fill=Policy.Type)) +
  geom_bar()

#Policy
ggplot(df, aes(x=Policy, fill=Policy)) +
  geom_bar()

#Renew.Offer.Type 
ggplot(df, aes(x=Renew.Offer.Type , fill=Renew.Offer.Type )) +
  geom_bar()

#Sales.Channel
ggplot(df, aes(x=Sales.Channel , fill=Sales.Channel )) +
  geom_bar()


#Total.Claim.Amount   
ggplot(df, aes(Total.Claim.Amount , fill="red")) +
  geom_histogram(binwidth=50)
#stark rechtsschief

#Vehicle.Class    
ggplot(df, aes(x=Vehicle.Class , fill=Vehicle.Class )) +
  geom_bar()

#Vehicle.Size
ggplot(df, aes(x=Vehicle.Size , fill=Vehicle.Size )) +
  geom_bar()


#########################
#DATA TRANSFORMATION
#########################

#remove columns
df <- df %>%
  subset(select=-Customer) %>%
  subset(select=-State) %>%
  subset(select=-Response) %>%
  subset(select=-Effective.To.Date) %>%
  subset(select=-Gender)
names(df) #proof 

#EMPLOYMENT STATUS + DUMMIES
df$EmploymentStatus <- gsub("Medical Leave", "other",df$EmploymentStatus)
df$EmploymentStatus <- gsub("Disabled", "other",df$EmploymentStatus)
df$EmploymentStatus <- gsub("Retired", "other",df$EmploymentStatus)
prop.table((table(df$EmploymentStatus)))

df$Unemployed <-  ifelse(df$EmploymentStatus == "Unemployed", 1, 0)
df$Employed <-  ifelse(df$EmploymentStatus == "Employed", 1, 0)

#EDUCATION + DUMMIES
df$Education <- gsub("Doctor", "Other",df$Education)
df$Education <- gsub("Master", "Other",df$Education)

df$Bachelor <-  ifelse(df$Education == "Bachelor", 1, 0)
df$College <-  ifelse(df$Education == "College", 1, 0)
df$HighSchool <-  ifelse(df$Education == "High School or Below", 1, 0)


#COVERAGE DUMMIES
df$CoverageBasic <-  ifelse(df$Coverage == "Basic", 1, 0)
df$CoveragePremium <-  ifelse(df$Coverage == "Premium", 1, 0)

#LOCATION CODE into binary 0 and 1
df$Location.Code <- gsub("Suburban", "Non-urban", df$Location.Code)
df$Location.Code <- gsub("Rural", "Non-urban", df$Location.Code)
df$Location.Code[df$Location.Code=="Non-urban"] <- 0
df$Location.Code[df$Location.Code=="Urban"] <- 1
df$Location.Code <- as.numeric(df$Location.Code)
unique(df$Location.Code) #proof

#MARITAL STATUS
df$Marital.Status <- gsub("Divorced", "Single", df$Marital.Status)
df$Marital.Status[df$Marital.Status=="Single"] <- 0
df$Marital.Status[df$Marital.Status=="Married"] <- 1
df$Marital.Status <- as.numeric(df$Marital.Status)
unique(df$Marital.Status) #proof

#POLICY TYPE
df$Policy.Corporate <- ifelse(df$Policy.Type=="Corporate Auto", 1, 0)
df$Policy.Personal <- ifelse(df$Policy.Type=="Personal Auto", 1, 0)

#Renew.Offer.Type
df$Offer1 <- ifelse(df$Renew.Offer.Type=="Offer1", 1, 0)
df$Offer2 <- ifelse(df$Renew.Offer.Type=="Offer2", 1, 0)
df$Offer3 <- ifelse(df$Renew.Offer.Type=="Offer3", 1, 0)


unique(df$Sales.Channel)
prop.table(table(df$Sales.Channel))


#VEHICLE CLASS
df$Vehicle.2door <- ifelse(df$Vehicle.Class  == "Two-Door Car", 1, 0)
df$Vehicle.4door  <- ifelse(df$Vehicle.Class  == "Four-Door Car", 1, 0)
df$Vehicle.SUV  <- ifelse(df$Vehicle.Class  == "SUV", 1, 0)
df$Vehicle.Lux_SUV <- ifelse(df$Vehicle.Class  == "Luxury SUV", 1, 0)
df$Vehicle.Sport_Car  <- ifelse(df$Vehicle.Class  == "Sports Car", 1, 0)

#VEHICLE SIZE 
df$Vehicle.Size.Small <- ifelse(df$Vehicle.Size == "Small", 1, 0)
df$Vehicle.Size.Large <- ifelse(df$Vehicle.Size == "Large", 1, 0)

#remove old dummy columns Coverage
df <- df %>%
  subset(select=-EmploymentStatus) %>%
  subset(select=-Education) %>%
  subset(select=-Coverage) %>%
  subset(select=-Policy.Type) %>%
  subset(select=-Policy) %>%
  subset(select=-Renew.Offer.Type) %>%
  subset(select=-Sales.Channel) %>%
  subset(select=-Vehicle.Class) %>%
  subset(select=-Vehicle.Size)
  
#rename Customer.Lifetime.Value Column 
df <- rename(df, target = Customer.Lifetime.Value)

########################
#FEATURE SELECTION (CORRELATIONS)
########################

#correlations between features above 0.5 must be removed
library(corrplot)
corrplot(cor(df))

#Income and Employment
df <- subset(df, select=-Unemployed)
df <- subset(df, select=-Employed)

#Monthly Premium auto and Total.Claim.Amount
df <- subset(df, select=-Total.Claim.Amount)


########################
#MODEL BUILDING
########################

#hold out validiation
library(caret)
set.seed(100)
intrain <- createDataPartition(df$target, p=0.7, list=FALSE )
training <- df[intrain,]
testing <- df[-intrain,]
dim(training); dim(testing) #proof

#Feature Selection using Backward elimination 
full.model <- lm(target~ ., data=na.omit(training))
step(full.model, direction = "backward", trace=FALSE)

#now using this to build our model 
lm.clv <- lm(target ~ Income + Monthly.Premium.Auto + Number.of.Open.Complaints + 
               Number.of.Policies + HighSchool + Policy.Corporate + Policy.Personal + 
               Offer1 + Offer3 + Vehicle.2door + Vehicle.4door + Vehicle.SUV + 
               Vehicle.Lux_SUV + Vehicle.Sport_Car + Vehicle.Size.Small, training)
summary(lm.clv)

# top 3 features with lowest p value:
#linear regression w/ Monthly.Premium.Auto, Number.of.Open.Complaints, renew.offer
lm.clv.2 <- lm(target ~ Monthly.Premium.Auto + Number.of.Open.Complaints +
                 Offer1 + Offer2 +Offer3, training )
summary(lm.clv.2)

#lets check the deviance table 
anova(lm.clv, test="Chisq")

#not good results, seems to be the data is not good. ('garbage in, garbage out')
#our data can only predict 16.8% of the variation of the target Variable CLV

#########################
#PREDICTION
#########################

#lets make new columns for the predictions and for the difference between real CLV and prediction
testing.prediction <- testing %>%
  mutate(prediction = predict(lm.clv)[1:2739]) %>% #bc prediction has more rows than testing
  mutate(false.prediction = target - prediction) %>%
  select(target, prediction, false.prediction )
view(testing.prediction)#proof
summary(testing.prediction$false.prediction)

#Root mean squared Error
RMSE <- sqrt(mean(testing.prediction$false.prediction^2))
RMSE

#Error rate 
error.rate <- abs((testing.prediction$false.prediction)/(testing.prediction$target)*100)
mean(error.rate)


###################
#conclusion:
###################

#the data can not determine very well customer lifetime value only 16.2% of the variation of 
#the independent variable (CLV) can be explained through them. The most important features 
#include Monthly.Premium.Auto, Number.of.Open.Complaints and Renew.Offer. So if you want 
#to improve the Customer Lifetime Value you should pay attention to them.
