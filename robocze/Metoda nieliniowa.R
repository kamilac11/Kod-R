#Zastosowanie metody nieliniowej - dla zbirou malego - zobaczymy czy to cos da
#byc moze na wiekszym bedzie lepiej to wygladalo

#https://pbiecek.gitbooks.io/przewodnik/content/Analiza/beznadzoru/agnes.html

#http://www.biecek.pl/R/naPrzelajPrzezDM.pdf

library(readxl)
zbior_danych <- read_excel("datasets/8_Rozniacych_sie_obiektow.xlsx", 
                           sheet = "Arkusz1", col_types = c("numeric","text", 
                                                            "text", "text", "text", "text", "blank", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric", "numeric", "text", "numeric", 
                                                            "text", "text", "text", "text", "text", 
                                                            "text", "numeric", "text", "text", 
                                                            "text", "numeric", "numeric", "numeric", 
                                                            "numeric", "numeric", "numeric"))

#po przyjrzeniu siê zbiorowi który chcesz poddac porz¹dkowaniu, wybierz podzbior zmiennych iloœciowych, na ktorych chcerz pracowaæ
#w miesce "" wpisz nazwy kolumn
dane_porzadkowanie<-zbior_danych[c("Nr","CENA.BRUTTO_[pln]","MOC_[km]","POJEMNOSC.SKOKOWA_[cm3]","ROK.PRODUKCJI","PRZEBIEG_[km]")]

#dane nalezy poddac stymulacji, normalizacji, potem wyznaczamy macierz odleglosci miedzy obiektami

#stymulacja - przeksztalcenie ilorazowe, normalizacja - unitaryzacja 

stymulacja_przeksztalcenie_ilorazowe<-function(x,y){
  for (i in 1:nrow(x)){
    x[i,which(colnames(x)==y)]=1/x[i,which(colnames(x)==y)]
  }
  return(x)
}
dane_porzadkowanie<-stymulacja_przeksztalcenie_ilorazowe(dane_porzadkowanie,"PRZEBIEG_[km]")

unitaryzacja<-function(x){
  maksi=0
  minim=0
  for (j in 2:ncol(x)){
    maksi[j]=max(x[j])
    minim[j]=min(x[j])
    for (i in 1:nrow(x)){
      x[i,j]=(x[i,j]-minim[j])/(maksi[j]-minim[j])
    }
  }
  return(x)
}

dane_porzadkowanie<-unitaryzacja(dane_porzadkowanie)

odleglosci <- dist(dane_porzadkowanie[,c("CENA.BRUTTO_[pln]","MOC_[km]","POJEMNOSC.SKOKOWA_[cm3]","ROK.PRODUKCJI","PRZEBIEG_[km]")], method = "euclidean")
odleglosci
proba=dist(dane_porzadkowanie)
as.matrix(proba)
as.matrix(odleglosci)#[1:10,1:10]

library(cluster)
grupy1 <- agnes(odleglosci, method = "ward")

library(factoextra)
fviz_dend(grupy1, k = 4, rect = TRUE, main = "Metoda Ward")


library(cluster)
grupy2 <- agnes(odleglosci, method = "single")

library(factoextra)
fviz_dend(grupy2, k = 60, rect = TRUE, main = "Metoda najbli¿szego s¹siedztwa")


library(cluster)
grupy3 <- agnes(odleglosci, method = "complete")

library(factoextra)
fviz_dend(grupy3, k =60, rect = TRUE, main = "Metoda najdalszego s¹siedztwa")


#no fajnie ale co umo¿liwia nam zadecydowanie na ile grup nalezy podzieliæ nasze dane?? 
#zaprezentowac to zastosownaie ( str27 - jest tez miara Sikhouette) lub analiza wykresw 
ggplot(auta, aes(Cena, KM, label=nazwa, color=grupa)) +
  geom_point(size=3) + theme_bw() +
  coord_trans("sqrt", "sqrt")

grupy_eclust <- eclust(auta[,c("Cena_norm", "KM_norm")], "hclust", graph = FALSE)

fviz_gap_stat(grupy_eclust$gap_stat)





