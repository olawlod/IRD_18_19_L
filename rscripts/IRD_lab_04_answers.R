################################################################################
# IRD zajecia nr 4
# Drzewa decyzyjne
################################################################################

# Skrypt z wykladu:
# IRD_2017_lecture_2017-11-07_decision_trees_in_R.R

# Inicjalizacja ziarna do zmiennych pseudolosowych
set.seed(1)

# Dane
data_fpath <- 'github/data/car.data.txt' # Wpisac poprawna sciezke dostepu w zaleznosci od lokalizacji pliku
DATA_SET <- read.csv(data_fpath, header = FALSE)
names(DATA_SET) <- c("buying", "maint", "doors", "persons",
                     "lug_boot", "safety", "class")

# Eksploracja danych
str(DATA_SET)
summary(DATA_SET)

# Laczymy klasy acc, good, vgood w jedna

DATA_SET$class <- factor(ifelse(DATA_SET$class == "unacc", 0, 1))

# Zadanie 1

# W oparciu o przyklad omowiony na wykladzie
# (skrypt IRD_2017_lecture_2017-11-07_decision_trees_in_R.R)
# zbuduj drzewo decyzyjne przewidujace klase samochodu 
# w zaleznosci od jego pozostalych parametrow.
# Uzyj 80% rekordow jako zbioru uczacego i 20% jako zbioru testowego.

# Biblioteki
library(rpart) # do drzewa
library(rpart.plot) # do rysowania drzewa
library(caret) # do oceny wyników

test_prop <- 0.20
test_bound <- floor(nrow(DATA_SET)* test_prop)
tdf <- DATA_SET[sample(nrow(DATA_SET)), ] # mieszamy losowo kolejnosc wierszy
tdf.test <- tdf[1:test_bound, ]
tdf.train <- tdf[(test_bound+1):nrow(tdf), ]

# Budowa drzew decyzyjnych

tree3 <- rpart(class ~ buying + maint + doors + persons + lug_boot + safety,
               data=tdf.train,
               method="class",
               control = list(maxdepth = 3))

rpart.plot(tree3, under=FALSE, tweak=1.3, fallen.leaves = TRUE)

tree5 <- rpart(class ~ buying + maint + doors + persons + lug_boot + safety,
               data=tdf.train,
               method="class",
               control = list(maxdepth = 5))

tree5 <- rpart(class ~ .,
               data=tdf.train,
               method="class",
               control = list(maxdepth = 5))

rpart.plot(tree5, under=FALSE, tweak=1.3, fallen.leaves = TRUE)

# Ewaluacja wynikow

EvaluateClassifier <- function(response_colname, prediction_colname, df,  positive="1")
{
  y <- factor(df[response_colname][[1]]) # factor of positive / negative cases
  predictions <- factor(df[prediction_colname][[1]]) # factor of predictions
  precision <- posPredValue(predictions, y, positive)
  recall <- sensitivity(predictions, y, positive)
  F1 <- (2 * precision * recall) / (precision + recall)
  
  return(list(precision=precision, recall=recall, F1=F1))
}

# Weryfikacja jakości klasyfikacji

## https://en.wikipedia.org/wiki/Precision_and_recall

# tree3

tdf.test$prediction3 <- predict(tree3, tdf.test, type = "class")
EvaluateClassifier('class', 'prediction3', tdf.test)

# tree5

tdf.test$prediction5 <- predict(tree5, tdf.test, type = "class")
EvaluateClassifier('class', 'prediction5', tdf.test)
