# "Drzewa decyzyjne w R"
# Michał Kaftanowicz
# 2017-11-07

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
