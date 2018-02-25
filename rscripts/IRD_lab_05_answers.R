###############################################################################################
# Zadanie 1
###############################################################################################

# napisz funkcje, ktora na podstawie macierzy klasyfikacji oblicza i zwraca
# 3-elementowa nazwana liste zawierajaca informacje o accuracy, sensitivity i specificity modelu.

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

###############################################################################################
# Zadanie 2
###############################################################################################

### Po kolei:

# Wczytaj dane o czerwonych winach (plik "winequality-red.csv"). Zamien wartosc zmiennej quality na
# binarna, przyjmujac, ze wina o jakosci 6 lub wyzszej sa wysokiej jakosci, a pozostale - niskiej jakosci.
# 

dane <- read.csv2('data/winequality-white.csv',  stringsAsFactors = FALSE, dec = '.')
if (typeof(dane$quality) == "integer") dane$quality <- ifelse(dane$quality >= 6, 'high', 'low')
## Uzycie warunku na typ zmiennej zabezpiecza nas przed zepsuciem danych
## w przypadku przypadkowego wywolania tej linijki wiecej niz 1 raz.
## Nie jest konieczne, ale czyni kod bezpieczniejszym.

## Inicjalizacja ziarna do zmiennych pseudolosowych
set.seed(1)

# Podziel zbior na uczacy i testowy losowo w proporcji 0.8:0.2.
# 
train_proportion <- 0.8
train_index <- runif(nrow(dane)) < train_proportion
train <- dane[train_index,]
test <- dane[!train_index,]

# Zbuduj drzewo klasfikacyjne przewidujce jakosc czerwonego wina na podstawie jego parametrow chemicznych.
# Przyjmij na poczatek parametr zlozonosci (complexity parameter) rowny 0.005. Zwizualizuj drzewo.

cp_start <- 0.005 ## dobra praktyka jest wypisywanie zalozen wprost, w pomocnicznych zmiennych,
## jeszcze przed wywolaniem funkcji

library(rpart)
library(rpart.plot)
d.klas <- rpart(quality~., data = train, method = "class", cp = cp_start)
rpart.plot(d.klas, under=FALSE, fallen.leaves = FALSE, cex = 0.3)

# Narysuj wykres bledu w zaleznosci od wielkosci drzewa. 
# Czerwoną kropką oznacz na wykresie wielkosc drzewa o minimalnym bledzie.
# 

plotcp(d.klas)
min.error <- which.min(d.klas$cptable[,"xerror"])
opt.cp <- d.klas$cptable[min.error,"CP"]
points(min.error, d.klas$cptable[min.error, "xerror"], pch = 19, col = "red")

# Zbuduj nowe drzewo powstale przez przyciecie poprzedniego drzewa do wartosci optymalnego parametru zlozonosci.
# 

d.klas.przyciete <- prune.rpart(d.klas, cp = opt.cp)
rpart.plot(d.klas.przyciete, under=FALSE, fallen.leaves = FALSE, cex = 0.5)

# Policz macierze klasyfikacji dla obu drzew.

CM <- list()
CM[["d.klas"]] <- table(predict(d.klas, new = test, type = "class"), test$quality)
CM[["d.klas.przyciete"]] <- table(predict(d.klas.przyciete, new = test, type = "class"), test$quality)

# Na postawie macierzy klasyfikacji policz dla obu drzew accuracy, sensitivity i specificity.

## Uzywajac funkcji napisanej w zadaniu 1:

lapply(CM, EvaluateModel) # lapply stosuje funkcje po kolei do kazdego elementu listy i zwraca liste wynikow

# Narysuj krzywa ROC i lift oraz policz AUC dla obu drzew.

## Zamiast pisac dwa razy to samo, wygodniej bedzie napisac funkcje, ktora to robi w sposob ogolny,
## i zastosowac ja do obu drzew.

library(ROCR) # do krzywej ROC

EvaluateTree <- function(tree_model, data_set, response_column_name)
{
  prognoza_ciagla <- predict(tree_model, newdata = data_set)
  prognoza_ciagla <- as.vector(prognoza_ciagla[,2])
  
  # krzywa ROC - potrzebuje "ciaglej" prognozy
  plot(performance(prediction(prognoza_ciagla, data_set[[response_column_name]]),"tpr","fpr"),lwd=2, colorize=T) 
  
  # AUC (Area Under Curve) - pole pod krzywa ROC
  print(performance(prediction(prognoza_ciagla, data_set[[response_column_name]]),"auc"))
  
  # Sensitivity/specificity plots ~ trade-off
  plot(performance(prediction(prognoza_ciagla, data_set[[response_column_name]]),"sens","spec"),lwd=2) 
  
  # Lift chart
  plot(performance(prediction(prognoza_ciagla, data_set[[response_column_name]]),"lift","rpp"),lwd=2, 
       col = "darkblue") 
}

EvaluateTree(tree_model = d.klas, data_set = test, response_column_name = "quality")
EvaluateTree(tree_model = d.klas.przyciete, data_set = test, response_column_name = "quality")

# Za kazdym wywolaniem powstaje kilka wykresow, ale widac tylko ostatni.
# Wczesniejsze mozna obejrzec, uzywajac strzalki wstecz nad wykresem.


