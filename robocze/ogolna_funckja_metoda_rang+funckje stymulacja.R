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



#zamiana na stymulante przebiegu( jest to destymulanta, zmienna mierzona na skali ilorazowej -stad przeksztalcenie ilorazowe)
#UWAGA U¯YTKOWNIK MUSI WIEDZIEC JAKI CHARAKTER MAJA JEGO ZMIENNE, zakladam ze powinny
#byc stymulantami, wiec jesli u niego nie sa to musi to zmienic

#________
#1-wszy sposob
#for (i in 1:nrow(dane_porzadkowanie)){
#  dane_porzadkowanie$`PRZEBIEG_[km]`[i]=1/dane_porzadkowanie$`PRZEBIEG_[km]`[i]
#}
#pytanie czy nie bedzie zwrocalo bledu o liscie
#wersja sztywna ale dzialajaca
#which(colnames(dane_porzadkowanie)=="PRZEBIEG_[km]") #odnajdue kolumne ktorej nazwa to
#2-gi sposob
# miejsce ""wpisz nazwe kolumny ktora wymaga stymulacji
for (i in 1:nrow(dane_porzadkowanie)){
  dane_porzadkowanie[i,which(colnames(dane_porzadkowanie)=="PRZEBIEG_[km]")]=1/dane_porzadkowanie[i,which(colnames(dane_porzadkowanie)=="PRZEBIEG_[km]")]
}

#funckje stymulucjace destymulnaty
#x -podzbior, y-nazwa kolumny do zmiany -nazwa w ""
stymulacja_przeksztalcenie_ilorazowe<-function(x,y){
  for (i in 1:nrow(x)){
    x[i,which(colnames(x)==y)]=1/x[i,which(colnames(x)==y)]
  }
  return(x)
}
#nadpisanie podzbioru
dane_porzadkowanie<-stymulacja_przeksztalcenie(dane_porzadkowanie,"PRZEBIEG_[km]")



stymulacja_przeksztalcenie_roznicowe<-function(x,y){
  for (i in 1:nrow(x)){
    x[i,which(colnames(x)==y)]=1/x[i,which(colnames(x)==y)]
  }
  return(x)
}


#__________________________normalizacja przez unitaryzacje po tym gdy zmienne maja ten sam charakter tj. stymulanty_____________
maksi=0
minim=0
for (j in 2:ncol(dane_porzadkowanie)){
  maksi[j]=max(dane_porzadkowanie[j])
  minim[j]=min(dane_porzadkowanie[j])
  for (i in 1:nrow(dane_porzadkowanie)){
    dane_porzadkowanie[i,j]=(dane_porzadkowanie[i,j]-minim[j])/(maksi[j]-minim[j])
  }
}


#zastapic NA wartosciami =0
#
#for(j in 1:ncol(dane2)){
#  for (i in 1:nrow(dane2))
#    if(is.na(dane2[i,j])){
#      dane2[i,j]=0
#    }
#}



# w miejsce x wpisz podzbior wyjsciowego zbioru, tj tabele zawierajaca indexy, i kolumny ze zmiennymi
#na podstawie ktorych ma zostac przeprowadzone porzadkowanie

funkcja_porzadkowanie_metoda_rang<-function(x){
  y<-x #dzieki temu nie bede sztywno odwolywac sie do 1kolumny rang
  for (i in 2:ncol(x)){
    x[ncol(x)+1]=rank(-x[i])
  }
  #ostania kolumna to zmienna_syntetyczna - za pomoca metody sredniej arytmetycznej
    x[,"zmienna_syntetyczna"] <-0
      for(i in 1:nrow(x)){
        for(j in (ncol(y)+1):(ncol(x)-1)){ # y to podzbior x przed dodaniem rang ncol(x)-1 bo ostatna kolumna to zm. syntetyczna
          x[i,ncol(x)]=x[i,ncol(x)]+x[i,j]
            j=j+1
      }
    x[i,ncol(x)]=x[i,ncol(x)]/(ncol(x)-7)
    }
  x<-x[order(x$zmienna_syntetyczna),]
  print("Numery indeksów obiektów po uporz¹dkowaniu: ")
  return(x[1])
}
funkcja_porzadkowanie_metoda_rang(dane_porzadkowanie)

  


#proba
#dodac numeracje do kolejnych kolumn rangi
#for(i in 2:ncol(dane_porzadkowanie)){
#  dane_porzadkowanie[ncol(dane_porzadkowanie)+1]=rank(-dane_porzadkowanie[i])
#}



#for(i in 1:nrows(dane_porzadkowanie)){
#  for(j in ncol(dane_porzadkowanie)){
#    dane_porzadkowanie[i,]
#  }
#}

#

