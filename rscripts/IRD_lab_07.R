################################################################################
### IRD
### Lab 07
### Reguly asocjacyjne oraz clustering (grupowanie)
################################################################################

# Skrypt wykorzystuje fragmenty kodu z artykulu 
# https://statistical-research.com/index.php/2012/09/26/association-rule-learning-and-the-apriori-algorithm/
# za wiedza i zgoda autora.

################################################################################
# Reguly asocjacyjne
################################################################################

# Biblioteki
#install.packages("arules")
library("arules") # do znajdowania regul
#install.packages("arulesViz")
library("arulesViz") # do wizualizacji regul

################################################################################

# Zrodlo danych: https://wiki.csc.calpoly.edu/datasets/wiki/ExtendedBakery

# Wczytanie danych o nazwach produktow i ich obrobka z uzyciem wyrazen regularnych
# (wyrazenia regularne nie obowiazuja na kolokwium)

goods_names <- readLines("data/EB-build-goods.sql")
goods_names <- gsub("^[a-zA-Z0-9 \\(]*,'", "", goods_names, fixed = FALSE)
goods_names <- gsub("','", " ", goods_names, fixed = FALSE)
goods_names <- gsub("'.*$", "", goods_names, fixed = FALSE)

# Wczytanie i eksploracja danych o transakcjach
df_all <- read.csv("data/75000-out2.csv", header = FALSE,
               row.names = 1)

df <- df_all[1:40000, ] # wybieramy pierwsze 40000 transakcji

# Zamiana ramki danych na macierz i nadanie nazw kolumnom 
mx <- as.matrix(df)
colnames(mx) <- goods_names
ts <- as(mx, "transactions")

# Znajdowanie regul asocjacyjnych algorytmem apriori
rules = apriori(ts, parameter=list(support=0.01, confidence=0.5))

# Podglad regul
rules
inspect(head(sort(rules, by="lift"),3))

# Wizualizacja regul asocjacyjnych - rozne sposoby

plot(rules)
head(quality(rules));
plot(rules, measure=c("support","lift"), shading="confidence")
plot(rules, shading="order", control=list(main ="Two-key plot")) # order - liczba dobr w regule

# Wybor regul asocjacyjnych z najwieksza pewnoscia 

subrules = rules[quality(rules)$confidence > 0.9]
subrules

# Rozne sposoby wizualizacji
plot(subrules, measure=c("support","lift"), shading="confidence")
plot(subrules, shading="order", control=list(main ="Two-key plot"))
plot(subrules, method="matrix", shading="lift")
plot(subrules, method="matrix", shading="confidence")
plot(subrules, method="grouped")

# Wybor regul asocjacyjnych z najwiekszym liftem 
# i kolejne sposoby wizualizacji
subrules2 = head(sort(rules, by="lift"), 3)
plot(subrules2, method="graph")
plot(subrules2, method="paracoord")

oneRule = sample(rules, 1)
inspect(oneRule)

################################################################################
# Dodatkowe materialy dla zainteresowanych:
# https://rdatamining.wordpress.com/2012/07/13/examples-and-resources-on-association-rule-mining-with-r/

################################################################################
# Zadanie 1
################################################################################

# Wybierz ostatnie 30 000 transakcji z pliku 75000-out2.csv.
# Znajdz dla tych transakcji reguly asocjacyjne o minimalnym wsparciu 0.015
# i minimalnej pewnosci 0.6.
# Zwizualizuj:
# - zaleznosci pomiedzy lift, support i confidence dla znalezionych regul
# - macierz lewych i prawych stron regul z pokazana wartoscia lift
# 
# Wybierz sposrod znalezionych 4 reguly o najwyzszej pewnosci.
# Zwizualizuj je w postaci grafow.
# Wypisz w postaci tekstowej regule o najwyzszej pewnosci.

################################################################################
# Clustering - grupowanie
################################################################################

library(datasets)
library(ggplot2)
iris

# Wizualizacja zbioru danych
ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()

set.seed(1)

# Grupowanie metoda k-srednich - z uzyciem 2 kolumn danych
irisCluster <- kmeans(iris[, 3:4], centers = 3, nstart = 10)
irisCluster

# Porownanie odkrytych grup z podzialem na gatunki
table(irisCluster$cluster, iris$Species)

# Wizualizacja wynikow grupowania
iris$Cluster <- as.factor(irisCluster$cluster)
ggplot(iris, aes(Petal.Length, Petal.Width, color = Species, shape = Cluster)) + geom_point()

################################################################################
# Zadanie 2
################################################################################

# Wykonaj clustering roslin ze zbioru danych iris
# uzywajac wymiarow lisci (sepal) zamiast kwiatow (petal).
# Zwizualizuj wyniki. Jak dobrze odkryte grupy pokrywaja sie z naturalnym
# podzialem na gatunki?

# Wykonaj i zwizualizuj analogiczne grupowanie, tym razem uzywajac wszystkich
# dostepnych informacji o wymiarach roslin. Czy poprawilo to wyniki?