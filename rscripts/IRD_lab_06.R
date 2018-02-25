################################################################################
### IRD
### Lab 06
### Lasy losowe
################################################################################

# Biblioteki
library(readr) # do wczytywania danych
library(tidyverse) # do przeksztalcania danych
library(ROCR) # do krzywej ROC
library(rpart) # do drzewa decyzyjnego
library(rpart.plot) # do wizualizacji drzewa decyzyjnego
library(randomForest) # do budowy (zasadzenia?) lasu losowego
library(caret) # do modeli i ich ewaluacji

################################################################################

# Wczytanie i eksploracja danych
data <- read_csv2("data/income.csv", na = c("?", ""))
str(data)
summary(data)

apply(data, 2, unique)

data <- na.omit(data)
data <- select(data, age, workclass,education, income, maritalStatus, occupation, capitalGain,
               capitalLoss, hourPerWeek, nativeCountry, race,sex, relationship)

if (any(data$income == "<=50K")) data$income <- ifelse(data$income == "<=50K", "low", "high")
# Uzycie instrukcji warunkowej nie jest niezbedne,
# ale chroni nas przed zepsuciem danych w sytuacji przypadkowego wywolania tej linijki
# wiecej, niz jeden raz.

table(data$income)

data <- mutate(data, 
               income = factor(income),
               workclass = factor(workclass),
               education = factor(education), 
               maritalStatus = factor(maritalStatus),
               occupation = factor(occupation),
               nativeCountry = factor(nativeCountry), 
               race = factor(race),
               sex = factor(sex), 
               relationship = factor(relationship)
               )

# Podzial zbioru na uczacy i testowy
train_dataset_proportion <- 0.8
train_index <- (runif(nrow(data), 0, 1) <= train_dataset_proportion)

train <- data[train_index, ]
test <- data[!train_index, ]

# Budowa (hodowla?) lasu losowego
rf <- randomForest(income ~., data = train)

# Uzyteczne parametry:
# ntree - liczba drzew
# mtry - liczba zmiennych do losowego próbkowania jako kandydaci przy każdym podziale

varImpPlot(rf)

# Ocena modelu 

# Funkcja ponizej to rozwiazanie zadania 1) z zajec numer 5:
EvaluateModel <- function(classif_mx)
{
  # Sciagawka: https://en.wikipedia.org/wiki/Sensitivity_and_specificity#Confusion_matrix
  true_positive <- classif_mx[1,1]
  true_negative <- classif_mx[2,2]
  condition_positive <- sum(classif_mx[ ,1])
  condition_negative <- sum(classif_mx[ ,2])
  # Uzywanie zmiennych pomocniczych o sensownych nazwach
  # ulatwia zrozumienie, co sie dzieje w funkcji
  accuracy <- (true_positive + true_negative) / sum(classif_mx)
  sensitivity <- true_positive / condition_positive
  specificity <- true_negative / condition_negative
  return(list(accuracy = accuracy, 
              sensitivity = sensitivity,
              specificity = specificity))
  # Notacja "accuracy = accuracy" itd. jest potrzebna,
  # zeby elementy listy mialy nazwy.
}

rf_classif_mx <- table(predict(rf, new = test, type = "class"), test$income)
EvaluateModel(rf_classif_mx)

# Dla porownania: drzewo klasyfikacyjne (powtorzenie z poprzednich zajec)

dtree <- rpart(income ~., data = train,  method = "class")
dtree_classif_mx <- table(predict(dtree, new = test, type = "class"), test$income)
EvaluateModel(dtree_classif_mx)

# Ale jakie AUC dla drzewa?
prognoza_ciagla <- predict(dtree, newdata = test)
prognoza_ciagla <- as.vector(prognoza_ciagla[,2])
performance(prediction(prognoza_ciagla, test[["income"]]),"auc")

# A jakie bedzie dla lasu losowego?

## wykresy diagnostyczne - znow powtorka

forecast <- predict(rf, newdata = test, type = "prob")[,2]
plottingData <- prediction(forecast, test$income)

# krzywa ROC - potrzebuje "ciaglej" prognozy
plot(performance(plottingData,"tpr","fpr"),lwd=2, colorize=T) 

#AUC (Area Under Curve) - pole pod krzywa ROC
performance(plottingData,"auc")@y.values[[1]]
# skladnia obiektowa: @ zamiast $, [[1]] do wyciagniecia elementu z listy

# Sensitivity/specificity plots ~ trade-off
plot(performance(plottingData ,"sens","spec"),lwd=2) 

# Lift chart
plot(performance(plottingData ,"lift","rpp"),lwd=2, col = "darkblue") 


################################################################################
### Pakiet caret - na przykladzie drzewa klasyfikacyjnego 
################################################################################

# http://topepo.github.io/caret/index.html

library(caret)

set.seed(1)

## podzial na zbior testowy i uczacy
inTraining <- createDataPartition(data$income, p = .8, list = FALSE)
training <- data[ inTraining,]
#training  <- na.omit(training) # gdybysmy wczesniej nie usuneli z calego zbioru
testing  <- data[-inTraining,]

## budowa modelu na zbiorze uczacym - na poczatek okreslamy sposob uczenia

## 5-krotna walidacja krzyzowa
fitControl <- trainControl(
  method = "cv",
  number = 5)

# Najpierw proste drzewo z CV
treeCaret_simple <- train(income ~ ., data = training, 
                 method = "rpart", 
                 trControl = fitControl)

plot(treeCaret_simple)
rpart.plot(treeCaret_simple$finalModel)

# Ewaluacja
confusionMatrix(data = predict(treeCaret_simple, testing), reference = testing$income, mode = "everything")

# Teraz recznie zadajemy zbior wartosci parametru zlozonosci do przeszukania
rpartGrid <- expand.grid(cp = seq(0.001, 0.1, by = 0.005))

treeCaret <- train(income ~ ., data = training, 
                    method = "rpart", 
                    trControl = fitControl,
                    tuneGrid = rpartGrid)
treeCaret
# https://en.wikipedia.org/wiki/Cohen%27s_kappa
plot(treeCaret)
rpart.plot(treeCaret$finalModel)

# Ewaluacja
confusionMatrix(data = predict(treeCaret, testing), reference = testing$income, mode = "everything")

################################################################################
# Zadanie 1
################################################################################

# Uzupelnij skrypt z cwiczen 5 (przewidywanie jakosci bialego wina na podstawie jego
# parametrow chemicznych) o model lasu losowego. Porownaj rozne metryki jakosci
# tego modelu do analogicznych metryk jakosci modeli uzywanych na cwiczeniach 5.


################################################################################
# Zadanie 2
################################################################################

# Uzywajac pakietu caret, zbuduj model drzewa klasyfikacyjnego 
# do przewidywania jakosci czerwonego wina. 
# Uzyj 3-krotnej walidacji krzyzowej. Pokaz jego metryki
# dla zbioru uczacego i testowego.
# Zbuduj kolejne drzewo, tym razem uzywajac 10-krotnej walidacji krzyzowej.
# Porownaj jego metryki (dla zbioru uczacego i testowego) z metrykami poprzedniego drzewa.