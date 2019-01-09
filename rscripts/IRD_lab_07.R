################################################################################
### IRD
### Lab 07
### Analiza skupien (clustering, grupowanie) i powtorzenie przed kolokwium
################################################################################

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

# Grupowanie hierarchiczne - z uzyciem 2 kolumn danych

clusters <- hclust(dist(iris[, 3:4]))
plot(clusters)

clusterCut <- cutree(clusters, 3)
table(clusterCut, iris$Species)

################################################################################
# Zadanie 1
################################################################################

# Wykonaj clustering roslin ze zbioru danych iris
# uzywajac wymiarow lisci (sepal) zamiast kwiatow (petal).
# Zwizualizuj wyniki. Jak dobrze odkryte grupy pokrywaja sie z naturalnym
# podzialem na gatunki?

# Wykonaj i zwizualizuj analogiczne grupowanie, tym razem uzywajac wszystkich
# dostepnych informacji o wymiarach roslin. Czy poprawilo to wyniki?


################################################################################
# Powtorzenie przed kolokwium
################################################################################

# Rozwiaz zadania 1, 3, 4 z przykladowego kolokwium: 
# https://github.com/kaftanowicz/ird/blob/master/exam/kolokwium_przyklad.pdf