#time modelling to reorder 

#urvival curve analysis.


#survival object 
install.packages("survival")
library(survival)
surv.df <- df %>%
  select(tenure, churn) %>%
  mutate(surv = Surv(tenure, churn)) 

#kaplan-meier analysis 
#probability for each custumore who is still in contract that he will churn 
fitKM <- survfit(Surv(df$churn, df$tenure) ~1,
                 type="kaplan-meier")
fitKM$surv
print(fitKM)
plot(fitKM)
#interpretation median: time until 50% of C will not churn 

#wird geordent nach binären gender (geht nur binär?)
fitKM.gender <- survfit(Surv(df$churn, df$tenure) ~gender,
                 data=df)

#Cox PH model 
install.packages("rms")
library(rms)
units(df$tenure) <- "months"
dd <- datadist(df)
options(datadist = "dd")

#lets plot, only use some variables to reduce complexity 
fitCPH1 <- cph(Surv(churn, tenure) ~ gender + Partner +
                   CeniorCitizen, data=df,
                 x=TRUE, y=TRUE, time.inc=1)
#interpretation of the coeffizients 
exp(fitCPH1$coefficients) #shows probalbility of churning
#z.B MonthglyCharges : 1,5
#erhöht sich MC um eine einheit erhöht sich die wahrscheinlichkeit zu churnen um faktor x1,5

survplot(fitCPH1, partner)

#####################
#model validation
#####################
#test of PH assumption
testCPH1 <- coxzph(fitCPH1)
print(testCPH1)
#again p must be under 0.05














