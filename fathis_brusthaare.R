#sie interessieren sich für die einflussfaktoren für die kaufentscheidungen von biologischen Lebensmitteln
#Für ihre untersuchungen steht ihnen folgender datensatz zur verfügung 

#identifikator: id
install.packages("haven")
library(haven)
library(tidyverse)
df <- read_dta("hausarbeit.dta")
view(df)


##################
#erstellen und interpretieren Sie deskriptive Statistiken aller Variablen.
#################

summary(df)
str(df, give.attr=FALSE)
#alle numerisch 

#missing values? Nein
sapply(df, function(x) sum(is.na(x)))

#educ
ggplot(df, aes(x=educ, fill="red")) +
  geom_histogram(binwidth=1)
summary(df$educ)
#linksschief, also minus, meisten werte zwischen 12 und 22 jahren

#regprc, gibt nur drei Preise in Datensatz, Diese sind ziwmlich gleichverteilt
table(df$regprc)
prop.table(table(df$regprc))
summary(df$regprc)
ggplot(df, aes(x=regprc)) +
  geom_bar()

#ecoprc, 9 Preise alle auch ziemlich gleichverteilt 
summary(df$ecoprc)
ggplot(df, aes(ecoprc, fill="red")) +
  geom_histogram()

#Haushaltsgröße, rechtsschief, also positiv, meisten Werte zwischen 1 und 4
summary(df$hhsize)
unique(df$hhsize) #ausprägungen von 1-9
ggplot(df, aes(hhsize)) +
  geom_bar()

#geschlecht 0= female, 1=male 
#für bessere visualisierung 
df2 <- df
df2$male[df2$male == 0] <- "Female"
df2$male[df2$male == 1] <- "Male"

ggplot(df2, aes(male, fill=male)) +
  geom_bar()
prop.table(table(df$male))
#großteil ist female (73%)

#family income 
summary(df$faminc)
ggplot(df, aes(faminc, fill="red")) +
  geom_histogram(binwidth = 10)
#rechtsschief, stark positiv, krasse outlier 
ggplot(df, aes(faminc, fill="red")) +
  geom_boxplot()


#age
summary(df$age)
ggplot(df, aes(age, fill="red")) +
  geom_histogram(binwidth = 5)

#ecolbs
summary(df$ecolbs)
ggplot(df, aes(ecolbs, fill="red")) +
  geom_histogram(binwidth = 0.5)
#rechtsschief, outlier 
ggplot(df, aes(ecolbs, fill="red")) +
  geom_boxplot()

#ln(faminc)
summary(df$log_faminc)
ggplot(df, aes(log_faminc, fill="red")) +
  geom_histogram(binwidth = 0.5)


########################
#2. Überprüfen Sie die Aussage: Frauen kaufen durchschnittlich mehr Bioäpfel als Männer.
######################

ggplot(df2, aes(x=male, y=ecolbs, fill=male)) +
  geom_col()

#neuen Dataframe mit nur frauen gefiltert
df.female <- df %>%
  filter(male==0)
mean(df.female$ecolbs) 
#mean ist 1.477

#vs Dataframe mit nur Männern
df.male <- df %>%
  filter(male==1)
mean(df.male$ecolbs) 
#mean ist 1.466

# Die Aussage ist richtig

########################
#3. lineare regression erstellen mit folgenden werten (siehe angabe)
######################
lm <- lm(ecolbs ~ ecoprc +age +regprc +log_faminc +educ, df.female)
summary(lm)

#Interpretation:
#steigt Preis um 1 Einheit, sinkt gekaufte Menge um -3,1
#steigt Alter "", steigte Menge um 0,0006
#Preis für reguläre Äpfel um 1, steigt menge um 3,48
#steigt Familieneinkommen um 1% (weil log!) steigt menge um 0,02
#Bildung, steigt Menge um 0,07

#Parameter des Modells
#statistisch signifikamnt sind nur Variablen ecoprc und regprc 
#der rest ist über p>0.05 und damit für analyse nicht relevant 
#(irgendwas mit NUllhypothese die abgelehnt wird oder so siehe Öko)

#schätzgüte R^2 ist sehr schwach mit nur 5%
#allgemeienr p value ist okay, weil deutlich kleiner als 0,05


########################
#4. Kausalität?
######################

#kausal ist ja ceteris paribus annahme, d.h der wert bleibt gleich egal ob neue variablen hinzukommen oder 
#entfernt werden 
#ich erstelle eine lineare regression mit nur den kausal untersuchten Wert
lm.ecoprc <- lm(ecolbs ~ ecoprc, df.female)
summary(lm.ecoprc)
#wert ist ohne die anderen variablen wahnsinnig gesunken deswegen ist er nicht kausale Ursache für Y
#annhme 2 mit erwartungswert gleich null oder so (Ökonometrie)


########################
# Aufgabe 5
######################

#single wenn Haushaltsgröße = 1, neue spalte erstellen $single
df.female$single <- ifelse(df.female$hhsize == 1, 1, 0)

#interagieren single mit educ (beduetet multiplizieren)
df.female$singleXeduc <- df.female$single*df.female$educ
head(df$singleXeduc)

#new model 
lm_new <- lm(ecolbs ~ ecoprc +age +regprc +log_faminc +educ +single +singleXeduc,  df.female)
summary(lm_new)

#marginaler effekt von Bildung auf gekaufte Menge Äpfel 
#effekt ist +0,08 ???????stimmt das? (siehe Öko)














