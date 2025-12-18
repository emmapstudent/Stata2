if(!require(tidyverse)){install.packages("tidyverse")}
library(tidyverse) 

if(!require(rcompanion)){install.packages("rcompanion")}
library(rcompanion) # plotNormalHistogram(x), groupWiseMean(x)
?`rcompanion-package`

if(!require(tseries)){install.packages("tseries")}
library(tseries) 

setwd("/Users/emmapanasiuk/Stata II/projekt")
dane <- read.csv(
  "StudentsPerformance.csv",
  header = T,
  sep = ",",
  dec="."
)

#Patrzymy na dane
?dane
View(dane)
dim(dane)

jarque.bera.test(dane$math.score)
jarque.bera.test(dane$reading.score)
jarque.bera.test(dane$writing.score)

#Tworzymy jeden średni wynik z trzech różnych wyników testow
dane <- mutate(dane,
       average.score = (math.score+reading.score+writing.score)/3)

#Boxplot w celu sprawdzenia rozkładu wyników między płciami; porównywalne
boxplot(dane$average.score)
boxplot(average.score~gender,data=dane, main="Przeciętne wyniki w nauce w podziale na płeć",
        xlab="Płeć", ylab="Przeciętny wynik")

#Ile mamy obserwacji dla kazdego poziomu wyksztalcenia rodzicow
ggplot(data = dane) + 
  geom_bar(mapping = aes(x = parental.level.of.education, y = ..prop.., group=1), 
           color="blue", fill="pink")
#Duzo mniej obserwacji dla ukonczonego wyksztalcenia wyzszego niz dla innych grup

#Wykres słupkowy w celu sprawdzenia różnic w wynikach pomiedzy grupami wyksztalcenia rodzicow
dane_edukacja <- dane %>%
  group_by(parental.level.of.education) %>%
  summarise(
    score = mean(average.score, na.rm = TRUE)
  )

ggplot(data = dane_edukacja) + 
  geom_col(mapping = aes(x = parental.level.of.education, y = score), 
           color="blue", fill="pink")
#Przecietne wyniki dosc zblizone, ale wydaja sie byc pozytywnie skorelowane 
#z wyksztalceniem rodzicow; sprawdzimy

dane_ad <- subset(dane, dane$parental.level.of.education == "associate's degree")
dane_bd <- subset(dane, dane$parental.level.of.education == "bachelor's degree")
dane_hs <- subset(dane, dane$parental.level.of.education == "high school")
dane_md <- subset(dane, dane$parental.level.of.education == "master's degree")
dane_sc <- subset(dane, dane$parental.level.of.education == "some college")
dane_shs <- subset(dane, dane$parental.level.of.education == "some high school")

par(mfrow = c(2,3))

hist(dane_ad$average.score)
hist(dane_bd$average.score)
hist(dane_hs$average.score)
hist(dane_md$average.score)
hist(dane_sc$average.score)
hist(dane_shs$average.score)

#plotNormalHistogram(dane_A$average.score)
#plotNormalHistogram(dane_B$average.score)
#plotNormalHistogram(dane_C$average.score)
#plotNormalHistogram(dane_D$average.score)

par(mfrow = c(1,1))

shapiro.test(dane$average.score) #wytlumaczyc, czemu ten test
jarque.bera.test(dane$average.score)

jarque.bera.test(dane_ad$average.score)
jarque.bera.test(dane_bd$average.score)
jarque.bera.test(dane_hs$average.score)
jarque.bera.test(dane_md$average.score)
jarque.bera.test(dane_sc$average.score)
jarque.bera.test(dane_shs$average.score)

#To samo dla grup etnicznych
daneA <- subset(dane, dane$race.ethnicity == "group A")
daneB <- subset(dane, dane$race.ethnicity == "group B")
daneC <- subset(dane, dane$race.ethnicity == "group C")
daneD <- subset(dane, dane$race.ethnicity == "group D")

par(mfrow = c(2,2))

#plotNormalHistogram(dane_A$average.score)
#plotNormalHistogram(dane_B$average.score)
#plotNormalHistogram(dane_C$average.score)
#plotNormalHistogram(dane_D$average.score)

hist(daneA$average.score)
hist(daneB$average.score)
hist(daneC$average.score)
hist(daneD$average.score)

par(mfrow = c(1,1))

jarque.bera.test(dane_A$average.score)
jarque.bera.test(dane_B$average.score)
jarque.bera.test(dane_C$average.score)
jarque.bera.test(dane_D$average.score)

#... i dla lunchu (jako proxy klasy ekonomicznej)
dane_st <- subset(dane, dane$lunch == "standard")
dane_fr <- subset(dane, dane$lunch == "free/reduced")

par(mfrow = c(1,2))

#plotNormalHistogram(dane_A$average.score)
#plotNormalHistogram(dane_B$average.score)
#plotNormalHistogram(dane_C$average.score)
#plotNormalHistogram(dane_D$average.score)

hist(dane_st$average.score)
hist(dane_fr$average.score)

par(mfrow = c(1,1))

jarque.bera.test(dane_st$average.score)
jarque.bera.test(dane_fr$average.score)
