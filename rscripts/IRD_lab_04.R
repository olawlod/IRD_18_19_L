################################################################################
### IRD
### Lab 04
### Drzewa decyzyjne
################################################################################

### Przykład 1: Titanic i biblioteka rpart

# Źródła:
# https://www.kaggle.com/c/titanic
# http://trevorstephens.com/kaggle-titanic-tutorial/getting-started-with-r/

# Biblioteki
library(rpart) # do drzewa
library(rpart.plot) # do rysowania drzewa
library(caret) # do oceny wyników

# Wczytanie danych

titanic_fpath <- "github/data/titanic_full.csv"
tdf <- read.csv(titanic_fpath)

# Ułamek liczby rekordów przeznaczony do zbioru testowego
test_prop <- 0.25

test_bound <- floor(nrow(tdf)* test_prop)

tdf <- tdf[sample(nrow(tdf)), ]
tdf.test <- tdf[1:test_bound, ]
tdf.train <- tdf[(test_bound+1):nrow(tdf), ]

# Eksploracja danych

names(tdf)
str(tdf)
table(tdf$survived)
prop.table(table(tdf$survived))

# Budowa drzewa decyzyjnego

tree <- rpart(survived ~ pclass + sex + age + sibsp + parch + fare + embarked,
              data=tdf.train,
              method="class",
              control = list(maxdepth = 3))

# Wizualizacja drzewa

tree

plot(tree)
text(tree, pretty = TRUE)

rpart.plot(tree, under=FALSE, tweak=1.3, fallen.leaves = TRUE)

# Wartości w węzłach drzewa:
# - przewidywana kategoria
# - prawdopodobieństwo przynależności do kategorii 1
# - procentowy udział obserwacji w danym węźle

# Interpretacja wyników

prop.table(table(tdf$sex, tdf$survived), 1)
prop.table(table(tdf$sex, tdf$survived), 2)

# Weryfikacja jakości klasyfikacji

EvaluateClassifier <- function(response_colname, prediction_colname, df,  positive="1")
{
  y <- factor(df[response_colname][[1]]) # factor of positive / negative cases
  predictions <- factor(df[prediction_colname][[1]]) # factor of predictions
  precision <- posPredValue(predictions, y, positive)
  recall <- sensitivity(predictions, y, positive)
  F1 <- (2 * precision * recall) / (precision + recall)
  
  return(list(precision=precision, recall=recall, F1=F1))
}

# Weryfikacja jakości klasyfikacji - zbiór uczący i testowy

## https://en.wikipedia.org/wiki/Precision_and_recall

tdf.train$survival_predicted <- predict(tree, tdf.train, type = "class")
tdf.test$survival_predicted <- predict(tree, tdf.test, type = "class")

EvaluateClassifier('survived', 'survival_predicted', tdf.train)
EvaluateClassifier('survived', 'survival_predicted', tdf.test)

# Alternatywne drzewo decyzyjne - głębsze

tree_deeper <- rpart(survived ~ pclass + sex + age + sibsp + parch + fare + embarked,
                     data=tdf.train,
                     method="class",
                     control = list(maxdepth = 10))

rpart.plot(tree_deeper, under=FALSE, tweak=1.5, fallen.leaves = TRUE)

tdf.train$survival_predicted_deeper <- predict(tree_deeper, tdf.train, type = "class")
tdf.test$survival_predicted_deeper <- predict(tree_deeper, tdf.test, type = "class")

EvaluateClassifier('survived', 'survival_predicted_deeper', tdf.train)
EvaluateClassifier('survived', 'survival_predicted_deeper', tdf.test)

# Las losowy, czyli kombinacja wielu drzew

#install.packages("party")
library(party)

set.seed(123)
forest <- cforest(as.factor(survived) ~ pclass + sex + age + sibsp + parch + fare + embarked,
                  data = tdf.train, 
                  controls=cforest_unbiased(ntree=200, mtry=3))

tdf.train$survival_predicted_forest<- predict(forest, tdf.train, OOB=TRUE, type = "response")
tdf.test$survival_predicted_forest<- predict(forest, tdf.test, OOB=TRUE, type = "response")

EvaluateClassifier('survived', 'survival_predicted_forest', tdf.train)
EvaluateClassifier('survived', 'survival_predicted_forest', tdf.test)

### Przykład 2: Cars i biblioteka rpart

# Inicjalizacja ziarna do zmiennych pseudolosowych
set.seed(1)

# Dane

# Informacje o zbiorze danych: https://rpubs.com/chitrav/118220
#WIN_PATH <- "data\car.data.txt"

data_fpath <- 'data/car.data.txt' # Wpisac poprawna sciezke dostepu w zaleznosci od lokalizacji pliku
DATA_SET <- read.csv(data_fpath, header = FALSE)
names(DATA_SET) <- c("buying", "maint", "doors", "persons",
                     "lug_boot", "safety", "class")

# Eksploracja danych
str(DATA_SET)
summary(DATA_SET)

# Laczymy klasy acc, good, vgood w jedna

DATA_SET$class <- factor(ifelse(DATA_SET$class == "unacc", 0, 1))

################################################################################
# Zadanie 1
################################################################################

# W oparciu o przyklad Titanica
# zbuduj 2 drzewa decyzyjne przewidujace klase samochodu 
# w zaleznosci od jego pozostalych parametrow.

# Niech pierwsze z drzew ma maksymalna glebokosc rowna 3, drugie - rowna 5.

# Zwizualizuj drzewa. Czym sie roznia miedzy soba?
# Policz precyzje i czulosc (precision and recall) klasyfikacji
# z uzyciem obu drzew na zbiorze testowym. Porownaj wyniki. Co jest przyczyna roznicy?

# Uzyj 80% rekordow jako zbioru uczacego i 20% jako zbioru testowego.

################################################################################

### Przykład 3: Cars i biblioteka party (metoda ctree)

# Biblioteki
library(party) # inna biblioteka do drzew decyzyjnych

# Czym sie roznia rpart i ctree?

# https://stats.stackexchange.com/questions/12140/conditional-inference-trees-vs-traditional-decision-trees

# both rpart and ctree recursively perform univariate splits 
# of the dependent variable based on values on a set of covariates.
# rpart and related algorithms usually employ information measures 
# (such as the Gini coefficient) for selecting the current covariate.
# 
# ctree, according to its authors (...) avoids the following variable selection bias 
# of rpart (and related methods): They tend to select variables that have 
# many possible splits or many missing values. 
# Unlike the others, ctree uses a significance test procedure 
# in order to select variables instead of selecting the variable 
# that maximizes an information measure (e.g. Gini coefficient).

# Ponowne wczytanie danych

data_fpath <- 'data/car.data.txt' # Wpisac poprawna sciezke dostepu w zaleznosci od lokalizacji pliku
DATA_SET <- read.csv(data_fpath, header = FALSE)
names(DATA_SET) <- c("buying", "maint", "doors", "persons",
                     "lug_boot", "safety", "class")

# Laczymy klasy acc, good, vgood w jedna

DATA_SET$class <- factor(ifelse(DATA_SET$class == "unacc", 0, 1))

# Alternatywny sposob podzialu zbioru na uczacy i testowy
TRAINING_SET_FRACTION <- 0.7
training.set.index <- (runif(nrow(DATA_SET)) < TRAINING_SET_FRACTION)
train.set <- DATA_SET[training.set.index, ]
test.set <- DATA_SET[!training.set.index, ]

# Budowa modelu
ctree.model <- ctree(factor(class) ~ ., data = train.set,
                     controls = ctree_control(mincriterion = 0.99,
                                              minsplit = 20))
# Wizualizacja modelu
plot(ctree.model, tnex = 2, type = "simple")
plot(ctree.model, tnex = 2, type = "extended")

# Roughly, the algorithm works as follows: 
# 
# 1) Test the global null hypothesis of independence between any of the input variables 
# and the response (which may be multivariate as well). 
# Stop if this hypothesis cannot be rejected. 
# Otherwise select the input variable with strongest association to the response. 
# This association is measured by a p-value corresponding to a test 
# for the partial null hypothesis of a single input variable and the response. 
# 
# 2) Implement a binary split in the selected input variable. 
# 
# 3) Recursively repeat steps 1) and 2).

devAskNewPage(ask = TRUE) # do kontrolowania wydruku wynikow

# Ten sam zbiór danych, ale poprzednia metoda - rpart
rpart.model <- rpart(class ~ ., train.set, cp = 0.01, minsplit = 3)
plotcp(rpart.model)

# Przycinanie drzewa
minimum.error <- which.min(rpart.model$cptable[, "xerror"])
optimal.complexity <- rpart.model$cptable[minimum.error, "CP"]
points(minimum.error, rpart.model$cptable[minimum.error, "xerror"],
       col = "red", pch = 19)
rpart.model.pruned <- prune(rpart.model, cp = optimal.complexity)

# Porownanie drzewa przed i po przycieciu
rpart.plot(rpart.model, under=FALSE, tweak=1.3, fallen.leaves = TRUE)
rpart.plot(rpart.model.pruned, under=FALSE, tweak=1.3, fallen.leaves = TRUE)

# Porownanie wynikow metod
confusion.matrix <- list()
print(confusion.matrix[[1]] <- table(predict(ctree.model, new = test.set),
                                     test.set$class))
print(confusion.matrix[[2]] <- table(predict(rpart.model, type = "class",
                                             newdata = test.set),
                                     test.set$class))
print(confusion.matrix[[3]] <- table(predict(rpart.model.pruned, type = "class",
                                             newdata = test.set),
                                     test.set$class))

CalculateAccuracy <- function(confusion.matrix) {
  return(sum(diag(confusion.matrix)) / sum(confusion.matrix))
}

print(data.frame(model = c("ctree.model", "rpart.model", "rpart.model.pruned"),
                 accuracy = sapply(confusion.matrix, CalculateAccuracy)),
      row.names = FALSE)