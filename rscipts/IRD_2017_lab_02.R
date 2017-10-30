### 0) Dygresja: kontrola wersji, git, github

# Tutorial po polsku:
# https://poznajprogramowanie.pl/git-tutorial-jak-zaczac-z-git/

# Tutorial po angielsku - bez pisania kodu
# https://guides.github.com/activities/hello-world/

### 1) Wczytywanie i przetwarzanie danych

# Sciaągawka do zadania - materialy z wykladu z 2017-10-18
# w pliku IRD_2017_lecture_2017-10-17_data_processing_in_R.R

# Cwiczenie 1 - Dokonaj operacji na danych o rybkach zaladowanych do pamieci
install.packages("FSAdata")
library(FSAdata)

# Wczytujemy do pamieci zbior danych zawarty w pakiecie
data(RuffeSLRH92)

# Eksplorujemy strukture zbioru danych
str(RuffeSLRH92)
summary(RuffeSLRH92)

# Podpunkt a)
# Wykonaj ponizsze transformacje, za kazdym razem zapisujac posredni wynik do nowej zmiennej

# ze zbioru danych wybierz kolumny dotyczace miesiecy, plci, dlugosci i wagi rybek, zapisz wynik jako lw
# posortuj dane wedlug miesiecy i plci rybek, zapisz wynik jako ms
# do zbioru LW dodaj nowa kolumne bedaca logarytmem dlugosci rybek, nazwij ja log_l, wynikowa tabele zapisz jako log_lw
# wskaz liczbe rybek dla kazdego miesiaca i plci, zapisz wynik jako sum_mon_sex
# pogrupuj rybki wg plci i wybierz te grupy, ktorych srednia waga jest wieksza niz 15

# Podpunkt b)
# Wykonaj ponizsze transformacje, laczac je za pomoca operatora pipeline %>% z pakietu dplyr

# ze zbioru danych wybierz kolumny dotyczace miesiecy, plci, dlugosci i wagi rybek
# posortuj dane wedlug miesiecy i plci rybek
# dodaj nowa kolumne bedaca logarytmem dlugosci rybek, nazwij ja log_l
# wskaz liczbe rybek dla kazdego miesiaca i plci, zapisz wynik jako sum_mon_sex

### 2) Instrukcje warunkowe, petle, funkcje

## instrukcje warunkowe

# Instrukcja przypisania w nawiasie oznacza: wykonaj przypisanie, ale tez i wydrukuj wynik do konsoli

# jeżeli spełniony warunek to wykonaj komendę
(zmienna <- rnorm(1))
if (zmienna < 0){
  cat("mniejsza ")
}

# jeżeli spełniony warunek to wykonaj komendę, w przeciwnym wypadku zrób co innego
(zmienna <- rnorm(1))
if (zmienna < 0){
  cat("mniejsza \n")
}else cat("wieksza\n")

# warunek musi być pojedynczą wartością
if (c(-1,0,1) > 0){
  cat("wieksze\n")
} #error

# ale można podawać rozbudowane warunki
(zmienna <- rnorm(1))
if (zmienna < 0 || zmienna^2 > 0.5){
  cat("OK\n") 
}else{
  cat("Nie OK\n")
}

# ifelse - wersja wektorowa funkcji IF
(zmienna <- rnorm(5))
ifelse(zmienna < 0, "mniejsza", "wieksza")
d <- ifelse(zmienna < 0, "mniejsza", "wieksza")
d

x <- 1:5
y <- -(1:5)
ifelse(zmienna < 0, x, y)



## petle

for(i in 1:5) {
  cat("aktualna wartosc i to", i, "\n")
}

(macierz <- matrix(1:20, 5, 4))
for(i in 1:nrow(macierz))
{
  print(mean(macierz[i,]))
}


# Petla while
licznik <- 1
while(licznik < 5)
{
  licznik <- licznik + runif(1, min = 0, max = 1)
  cat(licznik, '\n')
}


## funkcje
# składnia:
# NazwaFunkcji <- function(argument1, argument 2) {
#   instrukcje
# (opcjonalnie) return(wynik)
# }

# PRZYKLADY:
MojaFunkcja <- function(a,b) {
  a^2 + b^2
}
#wywołanie:
MojaFunkcja(1,2)
MojaFunkcja(124,445)

MojaFunkcja2 <- function(x, y) {
  a <- sin(x)
  b <- cos(y)
  (a+b)^2
}

MojaFunkcja2(1, pi)

# funkcja domyślnie zwraca ostatnie wyrażenie, ale można zadeklarować co ma zwrócić - instrukcja RETURN
MojaFunkcja3 <- function(x, y) {
  a <- sin(x)
  b <- cos(y)
  wynik <- a*b*100
  print("ala ma kota")
  return(c(a, b*100, wynik))
}

MojaFunkcja3(1, 2)

# po instrukcji RETURN nic nie jest już wykonywane:
MojaFunkcja4 <- function(x, y) {
  a <- sin(x)
  b <- cos(y)
  wynik <- a*b*100
  return(c(a, b*100, wynik))
  print("ala ma kota")
}

MojaFunkcja4(1, 2)

MojaFunkcja5 <- function(x,y)
{
  paste(x,y)
}

MojaFunkcja5("ala", "ma")
MojaFunkcja5(MojaFunkcja5("ala", "ma"), "kota")

# ... - arbitralna liczba argumentow. Uzywane glownie, gdy funkcja bierze jako jeden z argumentow
# inna funkcje i musi jej te argumenty przekazac.

measure_time <- function(f, ...)
{
  start_time <- Sys.time()
  r <- f(...)
  end_time <- Sys.time()
  run_time <- end_time - start_time
  return(list(result = r, run_time = run_time))
}

(mt <- measure_time(MojaFunkcja5, 'ala', 'ma'))

mt$result
mt$run_time

### Stosowanie funkcji na wektorach, listach i macierzach - rodzina funkcji apply
# Tutorial (po angielsku): https://www.datacamp.com/community/tutorials/r-tutorial-apply-family

# apply
n <- 5
m <- 10
mx <- matrix(data = round(100*runif(n*m, 0, 1), 0), nrow = n, ncol = m)

apply(mx, 1, sum)
apply(mx, 2, sum)

# lapply

example_list <- list(c(0, 1, 2, 3), rep('p', 5), seq(2, 20, 3), runif(20))
length(example_list)

lapply(example_list, length)
lapply(example_list, function(element){
  if (length(element) > 5) 'Wiecej niz 5 elementow' else '5 elementow lub mniej'
})
