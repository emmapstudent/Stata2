
if(!require(tidyverse)){install.packages("tidyverse")}
library(tidyverse) 

if(!require(ggplot2)){install.packages("ggplot2")}
library(ggplot2)

if(!require(rcompanion)){install.packages("rcompanion")}
library(rcompanion)
?`rcompanion-package`

if(!require(psych)){install.packages("psych")}
library(psych) #describe()

if(!require(exactRankTests)){install.packages("exactRankTests")}
library(exactRankTests) #Ansari-Bradley and Wilcoxon exact test with ties

if(!require(DescTools)){install.packages("DescTools")}
install.packages("DescTools", dependencies = TRUE)

if(!require(nortest)){install.packages("nortest")}
library(nortest) #sf.test

if(!require(lawstat)){install.packages("lawstat")}
library(lawstat) #Leven test

if(!require(dplyr)){install.packages("dplyr")}
library(dplyr)

if(!require(tseries)){install.packages("tseries")}
library(tseries)


setwd("C:/Users/Emma/stata")
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

#Tworzymy jeden średni wynik z trzech różnych wyników testow
dane <- mutate(dane,
               average.score = (math.score+reading.score+writing.score)/3)

#Boxplot w celu sprawdzenia rozkładu wyników między płciami; porównywalne
ggplot(data = dane, mapping = aes(x="", y = average.score)) +
  geom_boxplot(fill = "#82c7a5") +
  labs(
    title = "Przeciętne wyniki uczniów w testach",
    y = "Przeciętny wynik",
    x = ""
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 12)
  )


ggplot(data = dane, mapping = aes(x = gender, y = average.score)) + 
  geom_boxplot(fill="#82c7a5") +
  labs(
    title = "Przeciętny wynik w podziale na płeć",
    x = "Płeć",
    y = "Przeciętny wynik"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )

#Ile mamy obserwacji dla kazdego poziomu wyksztalcenia rodzicow

dane %>%
  count(parental.level.of.education) %>%
  ggplot(aes(
    x = reorder(parental.level.of.education, n),
    y = n
  )) +
  geom_col(color = "blue", fill = "#82c7a5") +
  labs(
    x = "Edukacja rodziców",
    y = "L. wystąpień"
  )
#Duzo mniej obserwacji dla ukonczonego wyksztalcenia wyzszego niz dla innych grup;
#bedziemy razem rozpatrywac wszystkie obserwacje dla wyksztalcenia wyzszego

#Wykres słupkowy w celu sprawdzenia różnic w wynikach pomiedzy grupami wyksztalcenia rodzicow
dane_edukacja <- dane %>%
  group_by(parental.level.of.education) %>%
  summarise(
    score = mean(average.score, na.rm = TRUE)
  )

ggplot(data = dane_edukacja) + 
  geom_col(mapping = aes(x = reorder(parental.level.of.education,score),
                         y = score), 
           color="blue", fill="#82c7a5") +
           labs(
             x = "Edukacja rodziców",
             y = "Przeciętny wynik"
           )
#Przecietne wyniki dosc zblizone, ale wydaja sie byc pozytywnie skorelowane 
#z wyksztalceniem rodzicow; sprawdzimy
jarque.bera.test(dane$math.score)
jarque.bera.test(dane$reading.score)
jarque.bera.test(dane$writing.score)
shapiro.test(dane$average.score)
jarque.bera.test(dane$average.score)
#Dane nie maja rozkladu normalnego ani wziete jako srednia, ani osobno;
#ale przy tak duzej probie nie ma to znaczenia, wiec i tak bedziemy aproksymowac
#rozkladem normalnym

dane_ad <- subset(dane, dane$parental.level.of.education == "associate's degree")
dane_hd <- subset(dane, dane$parental.level.of.education == "bachelor's degree"|
                    dane$parental.level.of.education == "master's degree")
dane_hs <- subset(dane, dane$parental.level.of.education == "high school")
dane_sc <- subset(dane, dane$parental.level.of.education == "some college")
dane_shs <- subset(dane, dane$parental.level.of.education == "some high school")

par(mfrow = c(2,3))

plotNormalHistogram(dane_ad$average.score, col="#82c7a5", linecol = "blue",
                    xlab = "Dwuletni college",
                    ylab = "")
plotNormalHistogram(dane_hd$average.score, col="#82c7a5", linecol = "blue",
                    xlab = "Wykształcenie wyższe",
                    ylab = "")
plotNormalHistogram(dane_hs$average.score, col="#82c7a5", linecol = "blue",
                    xlab = "Ukończona szkoła średnia",
                    ylab = "")
plotNormalHistogram(dane_sc$average.score, col="#82c7a5", linecol = "blue",
                    xlab = "Nieukończone studia",
                    ylab = "")
plotNormalHistogram(dane_shs$average.score, col="#82c7a5", linecol = "blue",
                    xlab = "Nieukończona szkoła średnia",
                    ylab = "")

par(mfrow = c(1,1))

jarque.bera.test(dane_ad$average.score)
jarque.bera.test(dane_hd$average.score)
jarque.bera.test(dane_hs$average.score)
jarque.bera.test(dane_sc$average.score)
jarque.bera.test(dane_shs$average.score)

#To samo dla grup etnicznych
daneA <- subset(dane, dane$race.ethnicity == "group A")
daneB <- subset(dane, dane$race.ethnicity == "group B")
daneC <- subset(dane, dane$race.ethnicity == "group C")
daneD <- subset(dane, dane$race.ethnicity == "group D")

par(mfrow = c(2,2))

plotNormalHistogram(daneA$average.score, col="#82c7a5", linecol = "blue",
                    xlab = "Grupa A",
                    ylab = "")
plotNormalHistogram(daneB$average.score, col="#82c7a5", linecol = "blue",
                    xlab = "Grupa B",
                    ylab = "")
plotNormalHistogram(daneC$average.score, col="#82c7a5", linecol = "blue",
                    xlab = "Grupa C",
                    ylab = "")
plotNormalHistogram(daneD$average.score, col="#82c7a5", linecol = "blue",
                    xlab = "Grupa D",
                    ylab = "")

par(mfrow = c(1,1))

jarque.bera.test(daneA$average.score)
jarque.bera.test(daneB$average.score)
jarque.bera.test(daneC$average.score)
jarque.bera.test(daneD$average.score)

#... i dla lunchu (jako proxy klasy ekonomicznej)
dane_st <- subset(dane, dane$lunch == "standard")
dane_fr <- subset(dane, dane$lunch == "free/reduced")

par(mfrow = c(1,2))

plotNormalHistogram(dane_st$average.score, col="#82c7a5", linecol = "blue",
                    xlab = "Standardowy lunch",
                    ylab = "",
                    xlim = c(0,100),
                    freq=FALSE)
plotNormalHistogram(dane_fr$average.score, col="#82c7a5", linecol = "blue",
                    xlab = "Lunch darmowy lub z dopłatami",
                    ylab = "",
                    freq=FALSE)

par(mfrow = c(1,1))

jarque.bera.test(dane_st$average.score)
jarque.bera.test(dane_fr$average.score)
