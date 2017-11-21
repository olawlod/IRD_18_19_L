################################################################################
# IRD zajecia nr 4
# Drzewa decyzyjne
################################################################################

# Skrypt z wykladu:
# IRD_2017_lecture_2017-11-07_decision_trees_in_R.R

# Inicjalizacja ziarna do zmiennych pseudolosowych
set.seed(1)

# Dane

# Informacje o zbiorze danych: https://rpubs.com/chitrav/118220

data_fpath <- 'github/data/car.data.txt' # Wpisac poprawna sciezke dostepu w zaleznosci od lokalizacji pliku
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

# W oparciu o przyklad omowiony na wykladzie
# (skrypt IRD_2017_lecture_2017-11-07_decision_trees_in_R.R)
# zbuduj 2 drzewa decyzyjne przewidujace klase samochodu 
# w zaleznosci od jego pozostalych parametrow.

# Niech pierwsze z drzew ma maksymalna glebokosc rowna 3, drugie - rowna 5.

# Zwizualizuj drzewa. Czym sie roznia miedzy soba?
# Policz precyzje i czulosc (precision and recall) klasyfikacji
# z uzyciem obu drzew na zbiorze testowym. Porownaj wyniki. Co jest przyczyna roznicy?

# Uzyj 80% rekordow jako zbioru uczacego i 20% jako zbioru testowego.



################################################################################
################################################################################

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

# Alternatywny sposob podzialu zbioru na uczacy i testowy
TRAINING_SET_FRACTION <- 0.2
training.set.index <- (runif(nrow(DATA_SET)) < TRAINING_SET_FRACTION)
train.set <- DATA_SET[training.set.index, ]
test.set <- DATA_SET[!training.set.index, ]

ctree.model <- ctree(factor(class) ~ ., data = train.set,
                     controls = ctree_control(mincriterion = 0.99,
                                              minsplit = 20))
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

# To samo poprzednia metoda - rpart
rpart.model <- rpart(class ~ ., train.set, cp = 0.01, minsplit = 2)
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

# Bonus: algorytmiczne konstruowanie formul do zadan klasyfikacji

FormulaString <- function(tdf, response_colname)
{
  predictor_colname_vec <- setdiff(names(tdf), response_colname)
  return(paste(response_colname, paste(predictor_colname_vec, collapse = " + "), sep = " ~ "))
}

FormulaString(tdf, "class")
