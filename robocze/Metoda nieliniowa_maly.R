#Zastosowanie metody nieliniowej - dla zbirou malego - zobaczymy czy to cos da
#byc moze na wiekszym bedzie lepiej to wygladalo
#cos pokazuje ale dane nie sa jakos super zroznicowane stad taki 2/10

#Procedure laczenia grup obiektow powtarza sie do momentu gdy tworza one jedna grupe(zostalo utworzoen
#pelne drzewko polaczen czyli n-1 razy)
#na wykresie heigh - odleglosc wiazania miedzy obiektami

#https://pbiecek.gitbooks.io/przewodnik/content/Analiza/beznadzoru/agnes.html

#http://www.biecek.pl/R/naPrzelajPrzezDM.pdf


library(readxl)
zbior_danych <- read_excel("~/Praca licencjacka/Moje dane_+zrodlo/8_Rozniacych_sie_obiektow.xlsx", 
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


#proba=dist(dane_porzadkowanie)
#as.matrix(proba)
#as.matrix(odleglosci)#[1:10,1:10]

library(cluster)
grupy1 <- agnes(odleglosci, method = "ward")

library(factoextra)
fviz_dend(grupy1, k = 5, rect = TRUE, main = "Metoda Ward")


library(cluster)
metoda_najblizszego<- agnes(odleglosci, method = "single")
#plot(metoda_najblizszego) #wykresik jak ten podzial wyglada

#Miara silhouetta do wyznaczenia liczby klastrów dla metody k_medoidow- tutaj najlepiej to 4 bo im wiecej tym ta miara jest mniejsza dla tej 1 grupy
library(clv)
kluster<-pam(odleglosci,4)
sil<-silhouette(kluster)
summary(sil)


library(factoextra)
fviz_dend(metoda_najblizszego, k = 3, rect = TRUE, main = "Metoda najbli¿szego s¹siedztwa")


library(cluster)
metoda_najdalszego <- agnes(odleglosci, method = "complete")

library(factoextra)
fviz_dend(metoda_najdalszego, k = 3, rect = TRUE, main = "Metoda najdalszego s¹siedztwa")
sil<-silhouette(metoda_najdalszego)



#dopisanie do ka¿dego obiektu numeru grupy po uporz¹dkowaniu
dane_porzadkowanie$grupy3=factor(cutree(grupy3, k=4))
#~~~~~~~~~~~~~~~~~~~~~~~tO DO~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#no fajnie ale co umo¿liwia nam zadecydowanie na ile grup nalezy podzieliæ nasze dane?? 
#zaprezentowac to zastosownaie ( str27 - jest tez miara Sikhouette) lub analiza wykresw 
ggplot(dane_porzadkowanie, aes(`CENA.BRUTTO_[pln]`, `MOC_[km]`, color=grupy3)) +
  geom_point(size=3) + theme_bw() +
  coord_trans("sqrt", "sqrt")

grupy_eclust <- eclust(dane_porzadkowanie[,c("CENA.BRUTTO_[pln]","MOC_[km]")], "hclust", graph = FALSE)

fviz_gap_stat(grupy_eclust$gap_stat)

dane_porzadkowanie$`MOC_[km]`



