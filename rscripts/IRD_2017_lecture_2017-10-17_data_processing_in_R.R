##wczytanie i wstepne przetwarzanie danych
#pakiet readr i dplyr

###readr - pakiet sluzacy wygodnemu wczytywaniu danych z formatu csv, tsv lub fwf
#jest nawet do 10 razy szybszy od standardowej funkcji read.csv
#w przeciwienstwie do standardowej funkcji, nie zmienia wektorow o typie character na factor
#tworza obiekty typu tibble czyli proste ramki danych

#instalacja pakietu
#install.packages("tidyverse", dependencies=TRUE)

#zaladowanie pakietu
library("tidyverse")

# Budujemy pomocniczą funkcję do mierzenia czasu wykonywania różnych operacji
measure_time <- function(f, ...)
{
  start_time <- Sys.time()
  r <- f(...)
  end_time <- Sys.time()
  print(end_time - start_time)
  return(r)
}

getwd()

#ustalenie katalogu roboczego (tam, gdzie znajduja sie pliki z danymi do przykladow)
setwd("github/data/")


##wczytywanie roznego rodzaju danych z pliku

#wartosci rozdzielone przecinkami

titanic <- measure_time(read.csv, "Titanic.csv")
titanic <- measure_time(read_csv, "Titanic.csv")
# Dygresja: data.table potrafi być jeszcze szybszy
library(data.table)
titanic <- measure_time(fread, "Titanic.csv")

przecinkowe <- read_csv("Titanic.csv")
head(przecinkowe)

#wartosci rozdzielone srednikami
srednikowe <- read_csv2("cars.csv")
head(srednikowe)

#wartosci rozdzielone stala szerokoscia
#fwf_widths sluzy podaniu szerokosci kolumn i ich nazw
stale <- read_fwf(file="iris.txt", fwf_widths(c(3, 5, 3, 5, 6), c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")))
head(stale)

#wartosci rozdzielone spacjami
spacjowe <- read_table("heights.txt", col_names=c("height", "cubit"))
head(spacjowe)

##wczytanie danych z ciagu liczb - pierwsza linijka zawsze sluzy jako nazwy kolumn
ciag <- read_csv("a,b,c
                 1,2,3
                 A,B,C")
ciag

#pozbywanie sie metadanych za pomoca skip lub "#"
read_csv("Pierwsza linia metadanych
         Druga linia metadanych
         x,y,z
         1,2,3", skip=2)

read_csv("#Komentarz
         x,y,z
         1,2,3", comment="#")

#wczytanie danych bez nazw kolumn
esoph <- read_csv("esoph.csv", col_names=FALSE)
head(esoph)

#nadanie wlasnych nazw dla kolumn
esoph <- read_csv("esoph.csv", col_names=c("jeden", "2", "trzy", "4", "piec"))
head(esoph)

#konwersja pustych lub konkretnych wartosci na NA (w tym przypadku kropki)
#\n sluzy jako przejscie do nastepnej linijki
read_csv("a,b,c\n1,2,.", na=".")


###pakiet dplyr - umozliwia manipulacje danych zarowno zapisanych w formie ramek danych, 
# jak i przechowywanych w bazach danych. 
# Jest w stanie przetlumaczyc kod R na zapytanie SQL (ale to poza zakresem naszych zajęć)

# http://seananderson.ca/2014/09/13/dplyr-intro.html

# dplyr is built around 5 verbs. 
# These verbs make up the majority of the data manipulation you tend to do. 
# You might need to:
  
# Select certain columns of data.
# Filter your data to select specific rows.
# Arrange the rows of your data into an order.
# Mutate your data frame to contain new columns.
# Summarise chunks of you data in some way.

#instalacja pakietu
#install.packages("dplyr")
library("dplyr")

#wczytanie przykladowych danych
#install.packages("nycflights13")
library("nycflights13")
dim(flights)
head(flights)

##operacje na danych

#filtrowanie
filter(flights, month==2, day==22)
#rownoznaczne z: 
flights[flights$month == 2 & flights$day == 22, ]

filter(flights, month == 1 | month == 2)
filter(flights, month %in% c(1,2))
#wycinek danych
slice(flights, 1:5)

#sortowanie danych rosnaco wg kolejnych kolumn
arrange(flights, year, month, day)

#sortowanie danych malejaco
arrange(flights, desc(dep_delay))

#wybor konkretnych kolumn 
select(flights, year, month, day)
select(flights, year:day) #przedzial kolumn
select(flights, -(year:day)) #wszystkie poza przedzialem kolumn

#zmiana nazwy kolumny
rename(flights, tail_num = tailnum)
select(flights, tailnum)

#wybor niepowtarzalnych wartosci
distinct(flights, origin, dest)

#dodawanie nowej kolumny
f2 <- mutate(flights,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60)

View(f2)

#mozna odniesc sie do wlasnie tworzonej kolumny
mutate(flights,
       gain = arr_delay - dep_delay,
       gain_per_hour = gain / (air_time / 60)
)

#zachowanie tylko nowych kolumn
transmute(flights,
          gain = arr_delay - dep_delay,
          gain_per_hour = gain / (air_time / 60)
)

#podsumowanie zbioru
summarise(flights,
          delay = mean(dep_delay, na.rm = TRUE))

#losowe obserwacje
sample_n(flights, 10) #10 wierszy
sample_frac(flights, 0.01) #10% wierszy

##zgrupowane operacje na danych

#funkcja group_by() pozwala na grupowanie obserwacji w zbiorze
#wykorzystane po niej inne funkcje beda wykonywane dla kazdej grupy osobno

#select() dziala identycznie, ale grupujace zmienne sa zachowywane
a1 <- group_by(flights, year, month, day)
select(a1, day)

distinct(select(a1, day))

#arrange() porzadkuje po zgrupowanych wartosciach
arrange(a1)

#mutate() i filter() dzialaja podobnie
filter(a1, day > 30 & dep_time < 10)

#sample_n() i sample_frac() losuja liczbe lub czesc wierszy dla kazdej grupy
sample_n(a1, 5)
sample_frac(a1, 0.01)

#slice() wybiera wiersze w kazdej z grup
slice(a1,1:2)

#summarise() jest latwiejszy do zrozumienia i uzycia
#przyklad - dane pogrupowane wg nr samolotow, podsumowane poprzez liczbe lotow, srednia odleglosc
#i opoznienie lotu kazdego z samolotow
by_tailnum <- group_by(flights, tailnum)
delay <- summarise(by_tailnum,
                   count = n(),  #liczba lotow
                   dist = mean(distance, na.rm = TRUE),  #sredni dystans
                   delay = mean(arr_delay, na.rm = TRUE))  #srednie opoznienie
delay <- filter(delay, count > 20, dist < 2000)
delay


#funkcje agregacyjne
#liczba obserwacji w danej grupie
destinations <- group_by(flights, dest)
summarise(destinations,
          flights = n()
)

#liczba unikatowych wartosci w zbiorze
summarise(destinations,
          planes = n_distinct(tailnum)
)


#kazde kolejne summarise zabiera jedna "warstwe" zgrupowanego zbioru
daily <- group_by(flights, year, month, day)
(per_day   <- summarise(daily, flights = n()))

(per_month <- summarise(per_day, flights = sum(flights)))

(per_year  <- summarise(per_month, flights = sum(flights)))


#lancuchy
#wykonywanie kazdej operacji po kolei z zapisywaniem krokow w kolejnych obiektach jest dosyc uciazliwe
#przyklad:
a1 <- group_by(flights, year, month, day)
a2 <- select(a1, arr_delay, dep_delay)
a3 <- summarise(a2,
                arr = mean(arr_delay, na.rm = TRUE),
                dep = mean(dep_delay, na.rm = TRUE))
a4 <- filter(a3, arr > 30 | dep > 30)
a4


#bez zapisywania kolejnych krokow mozna tworzyc funkcje zagniezdzone
a4_nested <- filter(
  summarise(
    select(
      group_by(flights, year, month, day),
      arr_delay, dep_delay
    ),
    arr = mean(arr_delay, na.rm = TRUE),
    dep = mean(dep_delay, na.rm = TRUE)
  ),
  arr > 30 | dep > 30
)

all(a4 == a4_nested)

# jednak jest to trudne do czytania, dlatego w dplyr istnieje operator %>%,
# ktory wynik poprzedniej operacji przerzuca jako argument do następnej.
# Mozna dzieki temu zapisac wiele operacji, ktore nalezy czytac od lewej do prawej, od gory do dolu
a4_pipeline <- flights %>%
  group_by(year, month, day) %>%
  select(arr_delay, dep_delay) %>%
  summarise(
    arr = mean(arr_delay, na.rm = TRUE),
    dep = mean(dep_delay, na.rm = TRUE)
  ) %>%
  filter(arr > 30 | dep > 30)

all(a4_pipeline == a4)

