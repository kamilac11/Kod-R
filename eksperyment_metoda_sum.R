library(readxl)

## Wczytanie zbioru danych

col_types = c("numeric","text", 
              "text", "text", "text", "text", "blank","numeric", "numeric", "numeric", 
              "numeric", "numeric", "text",  "numeric", 
              "text", "text", "text", "text","text", 
              "text", "numeric", "text", "text", 
              "text", "numeric", "numeric","numeric", 
              "numeric", "numeric", "numeric")
zbior_danych <- read_excel("datasets/8_Rozniacych_sie_obiektow.xlsx", sheet = "Arkusz1", col_types = col_types)
head(zbior_danych)

## Porządkowanie danych
dane_porzadkowanie<-zbior_danych[c("Nr","CENA.BRUTTO_[pln]","MOC_[km]", "POJEMNOSC.SKOKOWA_[cm3]","ROK.PRODUKCJI","PRZEBIEG_[km]")]

stymulacja_przeksztalcenie_roznicowe<-function(x,y){
  max_wartosc=max(x[which(colnames(x)==y)])
  for (i in 1:nrow(x)){
    x[i,which(colnames(x)==y)]=max_wartosc-x[i,which(colnames(x)==y)]
  }
  return(x)
}

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

ordering_sum_method = function(data)
{
  n = nrow(data)
  m = ncol(data)
  order_vector = seq(1,n,by=1)
  synthetic = order_vector - order_vector
  for (i in 1:n)
  {
      synthetic[i] = synthetic[i] + sum(data[i,])
  }
  # wyznaczenie sredniej
  synthetic = synthetic / m;
  # normalizacja
  synthetic = synthetic - min(synthetic)
  synthetic = synthetic / max(synthetic)
  #ostatnie przeksztalcenie normalizacja zm. syntetycznej
  order_vector = order_vector[order(-synthetic)]
  return( order_vector)
}

funkcja_porzadkowanie_metoda_sum<-function(x){
  x<-unitaryzacja(x) #unitaryzacja stymulant
  
  #sztywne zalozenie___ostania kolumna to zmienna_syntetyczna -za pomoca metody 
  #sredniej arytmetycznej
  x[,"zmienna_syntetyczna"] <-0
  for(i in 1:nrow(x)){
    for(j in 2:(ncol(x)-1)){
      x[i,ncol(x)]=x[i,ncol(x)]+x[i,j]
      j=j+1
    }
    x[i,ncol(x)]=x[i,ncol(x)]/(ncol(x)-2) 
    #-2 bo interesuje nas ilosc zmiennych, poza nr indeksu i kolumna zmienna syntetetyczna
  }
  
  #wyeliminowanie ujemnych wartosci zmiennej syntetycznej
  min_zmienna=min(x$zmienna_syntetyczna)
  for(i in 1:nrow(x)){
    x[i,ncol(x)]=x[i,ncol(x)]-min_zmienna
  }
  #ostatnie przeksztalcenie normalizacja zm. syntetycznej
  max_zmienna=max(x$zmienna_syntetyczna)
  for(i in 1:nrow(x)){
    x[i,ncol(x)]=x[i,ncol(x)]/max_zmienna
  }
  x<-x[order(-x$zmienna_syntetyczna),]
  return(x[1])
}

funkcja_porzadkowanie_metoda_sum(dane_porzadkowanie)

data = unitaryzacja(dane_porzadkowanie[c("CENA.BRUTTO_[pln]","MOC_[km]", "POJEMNOSC.SKOKOWA_[cm3]","ROK.PRODUKCJI","PRZEBIEG_[km]")])
wynik_uporzadkowania = ordering_sum_method(data)
dane_porzadkowanie[wynik_uporzadkowania,c("Nr","CENA.BRUTTO_[pln]","MOC_[km]", "POJEMNOSC.SKOKOWA_[cm3]","ROK.PRODUKCJI","PRZEBIEG_[km]")]
