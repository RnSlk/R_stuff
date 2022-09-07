#cheatsheet Lineare regression 
Call:
  lm(formula = target ~ . - Monthly.Premium.Auto - Number.of.Open.Complaints, 
     data = training)

Residuals:
  Min     1Q Median     3Q    Max 
-10508  -3229  -1380    847  64365 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)                    1.690e+04  8.991e+02  18.797  < 2e-16 ***
  Response                      -1.794e+02  2.395e+02  -0.749  0.45383    
Income                         7.297e-03  3.139e-03   2.325  0.02013 *  
  Location.Code                 -4.255e+01  2.263e+02  -0.188  0.85085    
Marital.Status                 1.993e+02  1.740e+02   1.145  0.25209    
Months.Since.Last.Claim        3.820e+00  8.095e+00   0.472  0.63702    
Months.Since.Policy.Inception -1.231e+00  2.930e+00  -0.420  0.67432    
Number.of.Policies             6.806e+01  3.436e+01   1.980  0.04770 *  
  Policy.Type                    3.388e+02  1.951e+02   1.737  0.08245 .  
Total.Claim.Amount            -3.194e-01  4.044e-01  -0.790  0.42970    
Vehicle.Size.Small             8.939e+00  2.126e+02   0.042  0.96647    
Vehicle.Size.Large            -3.365e+02  2.694e+02  -1.249  0.21154    
Coverage.Basic                -1.492e+03  1.848e+02  -8.075 8.05e-16 ***
  Coverage.Premium               1.769e+03  3.195e+02   5.536 3.23e-08 ***
  Renew.Offer1                   1.315e+03  2.841e+02   4.630 3.73e-06 ***
  Renew.Offer2                   4.716e+02  2.876e+02   1.640  0.10107    
Renew.Offer3                   8.296e+02  3.209e+02   2.586  0.00974 ** 
  Vehicle.2door                 -1.091e+04  6.850e+02 -15.928  < 2e-16 ***
  Vehicle.4door                 -1.100e+04  6.723e+02 -16.367  < 2e-16 ***
  Vehicle.SUV                   -6.928e+03  6.522e+02 -10.623  < 2e-16 ***
  Vehicle.Lux_SUV               -4.472e+02  8.247e+02  -0.542  0.58766    
Vehicle.Sport_Car             -7.301e+03  7.216e+02 -10.118  < 2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 6338 on 6106 degrees of freedom
(267 Beobachtungen als fehlend gelöscht)
Multiple R-squared:  0.1629,	Adjusted R-squared:   0.16 
F-statistic: 56.59 on 21 and 6106 DF,  p-value: < 2.2e-16



#1. F statistics p value sollte unter 0.05 sein ansosnten kacke, hier: sehr sehr klein
#2. R^2 (schätzgüte): gibt an wieviel der abhängigen Variable erklrt werden kann. is zwischen 0 und 1 und gibt
#prozent an also 0,163 bedeutet 16,3%, seeeehr schlecht 
#3. letzte spalte ansehen, sollte unter 0.05 sein da sonst Nullhypothese fälschlicherweise 
#abgelehnt wird, hier auch sehr kacke 



#coefficient für logistic regression 
coef.CLV <-  coef(lm.value) %>%
  exp() %>%
  round(2)

#Ergebnisse interpretieren. stehen dann unter variablen wert z.B Newsletter: 1,69, d.h die 
#Chance dass jemand zum onlineshop zurückkommt wenn er den NL abonniert hat 
#ist 69% höher als bei jemanden dre nicht den newsletter
#abonniert hat


#nice help for feature selection
install.packages("MASS")
library(MASS)
lm.CLV.new <- stepAIC(lm.value, trace=0)
summary(lm.CLV.new)
