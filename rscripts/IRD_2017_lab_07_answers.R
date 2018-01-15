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

library("arules") # do znajdowania regul
library("arulesViz") # do wizualizacji regul

# Wczytanie danych o nazwach produktow i ich obrobka z uzyciem wyrazen regularnych
# (wyrazenia regularne nie obowiazuja na kolokwium)

goods_names <- readLines("data/EB-build-goods.sql")
goods_names <- gsub("^[a-zA-Z0-9 \\(]*,'", "", goods_names, fixed = FALSE)
goods_names <- gsub("','", " ", goods_names, fixed = FALSE)
goods_names <- gsub("'.*$", "", goods_names, fixed = FALSE)

# Wczytanie i eksploracja danych o transakcjach
df_all <- read.csv("data/75000-out2.csv", header = FALSE,
                   row.names = 1)

# Wybierz ostatnie 30 000 transakcji z pliku 75000-out2.csv.
df <- tail(df_all, 30000)

# Zamiana ramki danych na macierz i nadanie nazw kolumnom 
mx <- as.matrix(df)
colnames(mx) <- goods_names
ts <- as(mx, "transactions")

# Znajdz dla tych transakcji reguly asocjacyjne o minimalnym wsparciu 0.015
# i minimalnej pewnosci 0.6.
min_support <- 0.015
min_confidence <- 0.6

# Znajdowanie regul asocjacyjnych algorytmem apriori
rules = apriori(ts, parameter=list(support=min_support, confidence=min_confidence))

# - zaleznosci pomiedzy lift, support i confidence dla znalezionych regul
plot(rules, measure=c("support","lift"), shading="confidence") # mozna tez inaczej rozmiescic te miary na wykresie

# - macierz lewych i prawych stron regul z pokazana wartoscia lift
plot(rules, method="matrix", shading="lift")

# Wybierz sposrod znalezionych 4 reguly o najwyzszej pewnosci.
subrules = head(sort(rules, by="confidence", decreasing = TRUE), 4)

# Zwizualizuj je w postaci grafow.
plot(subrules2, method="graph")

# Wypisz w postaci tekstowej regule o najwyzszej pewnosci.
inspect(subrules[1])

################################################################################
# Zadanie 2
################################################################################

# Wykonaj clustering roslin ze zbioru danych iris
# uzywajac wymiarow lisci (sepal) zamiast kwiatow (petal).
# Zwizualizuj wyniki. Jak dobrze odkryte grupy pokrywaja sie z naturalnym
# podzialem na gatunki?

# Wykonaj i zwizualizuj analogiczne grupowanie, tym razem uzywajac wszystkich
# dostepnych informacji o wymiarach roslin. Czy poprawilo to wyniki?

head(iris)

# uzywajac wymiarow lisci (sepal) zamiast kwiatow (petal).

irisCluster_sepal <- kmeans(iris[, 1:2], centers = 3, nstart = 10)
iris$Cluster_sepal <- as.factor(irisCluster_sepal$cluster)
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species, shape = Cluster_sepal)) + geom_point()

#uzywajac wszystkich dostepnych informacji o wymiarach roslin.

irisCluster_all <- kmeans(iris[, 1:4], centers = 3, nstart = 10)
iris$Cluster_all <- as.factor(irisCluster_all$cluster)
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species, shape = Cluster_all)) + geom_point()
ggplot(iris, aes(Petal.Length, Petal.Width, color = Species, shape = Cluster_all)) + geom_point()

table(iris$Cluster_sepal, iris$Species)
table(iris$Cluster_all,  iris$Species)

# Czy poprawilo to wyniki?
# Wzgledem Cluster_sepal - tak.
