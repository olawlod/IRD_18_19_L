################################################################################
### IRD
### Lab 05
### Drzewa decyzyjne cd., drzewa regresji, ocena modeli
################################################################################

################################################################################
# Biblioteki
################################################################################

rm(list=ls()) # programowe czyszczenie środowiska

library(rpart) # do drzewa
library(rpart.plot) # do rysowania drzewa
#install.packages('ROCR')
library(ROCR) # do krzywej ROC
library(caret) # do waznosci zmiennych w modelu

# Wczytanie danych - prosze uzupelnic wlasciwa sciezke do pliku
dane <- read.csv2('data/winequality-white.csv',  stringsAsFactors = FALSE, dec = '.')
# read.csv2 zamiast read.csv ze wzgledu na separator kolumn
# dec = '.' - wskazujemy, że separatorem dziesiętnym jest kropka

################################################################################
# Eksploracja danych
################################################################################

head(dane)
str(dane)
summary(dane)

hist(dane$quality)

################################################################################
# Drzewa klasyfikacyjne - powtorzenie, ocena dokladnosci
################################################################################

# zmienna objasniana zamieniamy na zmienna binarna: jezeli quality >= 6, to jakosc wysoka, wpp niska:

dane$quality <- ifelse(dane$quality >= 6, 'high', 'low')

set.seed(1)
train_proportion <- 0.7
train_index <- runif(nrow(dane)) < train_proportion
train <- dane[train_index,]
test <- dane[!train_index,]

# budujemy i porownujemy 2 drzewa klasyfikacyjne
d.klas1 <- rpart(quality~., data = train, method = "class")
d.klas2 <- rpart(quality~., data = train, method = "class", cp = 0.005)
#plot(d.klas, margin = 0.2)
#text(d.klas, pretty = 0)
rpart.plot(d.klas1, under=FALSE, fallen.leaves = FALSE, cex = 0.3)
rpart.plot(d.klas2, under=FALSE, fallen.leaves = FALSE, cex = 0.3)

# 3) Classification matrix + its statistics
CM <- list()
CM[["d.klas1"]] <- table(predict(d.klas1, new = test, type = "class"), test$quality)
CM[["d.klas2"]] <- table(predict(d.klas2, new = test, type = "class"), test$quality)

# Accuracy = odsetek poprawnie sklasyfikowanych odpowiedzi
CalcAcc <- function(macierz) {
  return(sum(diag(macierz))/sum(macierz))
}

printAcc <- function () {
  cat(toupper("Accuracy dla wybranych modeli \n"))
  cat("\n")
  Accuracy <- data.frame(model = names(CM), Accuracy = round(sapply(CM, CalcAcc), 6))
  print(Accuracy, row.names = FALSE)
  return(Accuracy)
}

Accuracy <- printAcc()

# Misclassification rate = 1-Accuracy = odsetek blednie sklasyfikowanych odpowiedzi
MER <- sapply(Accuracy[2], function(x) 1-x)
colnames(MER) <- "Missclassification Error Rate"
rownames(MER) <- Accuracy[[1]]
MER

###############################################################################################
# ROC/LIFT/GAIN Curve
###############################################################################################

## Z dokumentacji pakietu ROCR:

# Here is how to call 'performance' to create some standard evaluation plots:
# ROC curves: measure="tpr", x.measure="fpr".
# Precision/recall graphs: measure="prec", x.measure="rec".
# Sensitivity/specificity plots: measure="sens", x.measure="spec".
# Lift charts: measure="lift", x.measure="rpp".

prognoza_ciagla <- predict(d.klas1, newdata = test)
prognoza_ciagla <- as.vector(prognoza_ciagla[,2])

# krzywa ROC - potrzebuje "ciaglej" prognozy
plot(performance(prediction(prognoza_ciagla,test$quality),"tpr","fpr"),lwd=2, colorize=T) 

# AUC (Area Under Curve) - pole pod krzywa ROC
performance(prediction(prognoza_ciagla, test$quality),"auc")

# Sensitivity/specificity plots ~ trade-off
plot(performance(prediction(prognoza_ciagla,test$quality),"sens","spec"),lwd=2) 

# Lift chart
plot(performance(prediction(prognoza_ciagla,test$quality),"lift","rpp"),lwd=2, col = "darkblue") 
#Lift is a measure of the effectiveness of a predictive model calculated 
#as the ratio between the results obtained with and without the predictive model. 

################################################################################
# Drzewa regresyjne
################################################################################

# Wczytanie danych - ponownie
dane <- read.csv2('data/winequality-white.csv',  stringsAsFactors = FALSE, dec = '.')

# Inicjalizacja ziarna do zmiennych pseudolosowych
set.seed(1)

# dzielimy na zbior treningowy i testowy
train_proportion <- 0.7
train_index <- runif(nrow(dane)) < train_proportion
train <- dane[train_index,]
test <- dane[!train_index,]

# Regresja liniowa - dobry punkt odniesienia
lin_m <- lm(quality ~ ., data = train)

# Drzewo regresji
d.regr <- rpart(quality ~., data = train, cp = 0.01)
#plot(d.regr, margin = 0.2)
#text(d.regr, pretty = 0)
rpart.plot(d.regr, under=FALSE, fallen.leaves = FALSE, cex = 0.9)

# Drzewo regresji - wieksze
d.regr.duze <- rpart(quality ~. , data = train, cp = 0.003)
#plot(d.regr.duze, margin = 0.2)
#text(d.regr.duze, pretty = 0)
rpart.plot(d.regr.duze, under=FALSE, fallen.leaves = FALSE, cex = 0.5)

################################################################################
# Metody oceny drzewa regresji: Variable Importance, RSS, MAE, RMSE, RAE, RRSE, R^2
################################################################################

# variable importance

varImp(lin_m)
d.regr$variable.importance
d.regr.duze$variable.importance

# odchylenia reszt - rozne miary

# funkcja residuals liczy reszty = wartosci rzeczywiste - prognoza:
all(as.vector(residuals(d.regr)) == train$quality - predict(d.regr, train))

modele <- list("d.regr" = d.regr, "d.regr.duze" = d.regr.duze, "lin_m" = lin_m)

OcenaModeli <- function(modele, dane, predicted_col_name) {
  
  print("Suma kwadatow reszt RSS")
  print(sapply(modele, function(x) sum((dane[[predicted_col_name]] - predict(x, dane))^2) ))
  
  print("Średni błąd absolutny MAE")
  print(sapply(modele, function(x) sum(abs((dane[[predicted_col_name]] - predict(x, dane))))/nrow(dane) ))
  
  print("Pierwiastek błędu średniokwadratowego RMSE")
  print(sapply(modele, function(x) sqrt(sum((dane[[predicted_col_name]] - predict(x, dane))^2)/nrow(dane)) ))
  
  print("Względny błąd absolutny RAE")
  print(sapply(modele, function(x) sum(abs((dane[[predicted_col_name]] - predict(x, dane))))/sum(abs(dane[[predicted_col_name]] - mean(dane[[predicted_col_name]]))) ))
  
  print("Pierwiastek Względnego błędu średniokw RRSE")
  print(sapply(modele, function(x) sqrt(sum((dane[[predicted_col_name]] - predict(x, dane))^2)/sum((dane[[predicted_col_name]] - mean(dane[[predicted_col_name]]))^2)) ))
  
}

OcenaModeli(modele, train, 'quality')
OcenaModeli(modele, test, 'quality')

###############################################################################################
# Zadanie 1
###############################################################################################

# napisz funkcje, ktora na podstawie macierzy klasyfikacji oblicza i zwraca
# 3-elementowa nazwana liste zawierajaca informacje o accuracy, sensitivity i specificity modelu.
# Sciagawka: https://en.wikipedia.org/wiki/Sensitivity_and_specificity#Confusion_matrix

###############################################################################################
# Zadanie 2
###############################################################################################

# Wczytaj dane o czerwonych winach (plik "winequality-red.csv"). Zamien wartosc zmiennej quality na
# binarna, przyjmujac, ze wina o jakosci 6 lub wyzszej sa wysokiej jakosci, a pozostale - niskiej jakosci.
# 
# Podziel zbior na uczacy i testowy losowo w proporcji 0.8:0.2.
# 
# Zbuduj drzewo klasyfikacyjne przewidujce jakosc czerwonego wina na podstawie jego parametrow chemicznych.
# Ustaw jego parametr zlozonosci (complexity parameter) na wartosc 0.005.

# Zwizualizuj drzewo. Policz jego macierz klasyfikacji.
# Na postawie macierzy klasyfikacji policz accuracy, sensitivity i specificity.
# Narysuj krzywa ROC i lift oraz policz AUC.
