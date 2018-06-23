#Porownwnay wyniki porzadkowania przy wykorzystaniu funkcji: metoda sum i metoda rang

library(readxl)
zbior_danych <- read_excel("datasets/zbior_danych.xlsx", 
                           sheet = "DANE_INNA_WERSJA")

dane_porzadkowanie<-zbior_danych[c("Nr","CENA.BRUTTO_[pln]","MOC_[km]","POJEMNOSC.SKOKOWA_[cm3]","ROK.PRODUKCJI","PRZEBIEG_[km]")]

#stymulacja - przeksztalcenie ilorazowe-metoda rang, przeksztalcenie roznicowe-metoda sum i normalizacja-unitaryzacje

stymulacja_przeksztalcenie_ilorazowe<-function(x,y){
  for (i in 1:nrow(x)){
    x[i,which(colnames(x)==y)]=1/x[i,which(colnames(x)==y)]
  }
  return(x)
}
dane_rang<-stymulacja_przeksztalcenie_ilorazowe(dane_porzadkowanie,"PRZEBIEG_[km]")


stymulacja_przeksztalcenie_roznicowe<-function(x,y){
  max_wartosc=max(x[which(colnames(x)==y)])
  for (i in 1:nrow(x)){
    x[i,which(colnames(x)==y)]=max_wartosc-x[i,which(colnames(x)==y)]
  }
  return(x)
}
dane_sum<-stymulacja_przeksztalcenie_roznicowe(dane_porzadkowanie,"PRZEBIEG_[km]")

#normalizacja

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
dane_sum<-unitaryzacja(dane_sum)
dane_rang<-unitaryzacja(dane_rang)

#porzadkowanie
##metoda sum
funkcja_porzadkowanie_metoda_sum<-function(x){
  
  #sztywnie zalozenie____ostania kolumna to zmienna_syntetyczna - za pomoca metody sredniej arytmetycznej
  x[,"zmienna_syntetyczna"] <-0
  for(i in 1:nrow(x)){
    for(j in 2:(ncol(x)-1)){
      x[i,ncol(x)]=x[i,ncol(x)]+x[i,j]
    }
    x[i,ncol(x)]=x[i,ncol(x)]/(ncol(x)-2) #-2 bo interesuje nas ilosc zmiennych, poza nr indeksu i kolumna zmienna synte
  }
  #wyeliminowanie ujemnych wartosci
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
  print("Numery indeksów obiektów po uporz¹dkowaniu: ")
  return(x[1])
}


funkcja_porzadkowanie_metoda_rang<-function(x){
  y<-x #dzieki temu nie bede sztywno odwolywac sie do 1kolumny rang
  for (i in 2:ncol(x)){
    x[ncol(x)+1]=rank(-x[i])
  }
  #ostania kolumna to zmienna_syntetyczna - za pomoca metody sredniej arytmetycznej
  x[,"zmienna_syntetyczna"] <-0
  for(i in 1:nrow(x)){
    for(j in (ncol(y)+1):(ncol(x)-1)){
      x[i,ncol(x)]=x[i,ncol(x)]+x[i,j]
      j=j+1
    }
    x[i,ncol(x)]=x[i,ncol(x)]/(ncol(x)-7)
  }
  x<-x[order(x$zmienna_syntetyczna),]
  print("Numery indeksów obiektów po uporz¹dkowaniu: ")
  return(x[1])
}

dane_sum<-funkcja_porzadkowanie_metoda_sum(dane_sum)
dane_rang<-funkcja_porzadkowanie_metoda_rang(dane_rang)
# dziala
#zainicjowane wektora
# inwersja=1:nrow(dane_rang) #wielkosc
# dim(inwersja)=c(61,1) #wymiar #
# a=table(1:61,1)
# 
# k=array(1:nrow(dane_rang),c(nrow(dane_rang),1))
# k=as.data.frame(table(k))
# #zliczenie inwersji
# for(i in 1:nrow(dane_sum)){
#   if(dane_sum[i,1]==dane_rang[i,1]){
#     inwersja[i]=1
#   }
#     else{
#       inwersja[i]=0
#   }
# }
# x = data.frame(1:nrow(dane_rang), array(1:6, c(3, 2)))
# zlicz=as.data.frame(table(inwersja))
#

#count nie dziala tylko: as.data.frame(table(tabela_porownawcza$porownanie))
tabela_porownawcza=data.frame(dane_rang,dane_sum)
#tabela_porownawcza=as.data.frame(tabela_porownawcza)
names(tabela_porownawcza)<-c("dane_rang","dane_sum")
tabela_porownawcza$porownanie=0


#porownanie
for(i in 1:nrow(tabela_porownawcza)){
  if(tabela_porownawcza$dane_sum[i]==tabela_porownawcza$dane_rang[i]){
    tabela_porownawcza$porownanie[i]=1
  }
}
#podglad tabeli
head(tabela_porownawcza,15)

podsumowanie=as.data.frame(table(tabela_porownawcza$porownanie))
names(podsumowanie)<-c("wartoœæ","ilosc wyst¹pieñ")
podsumowanie

