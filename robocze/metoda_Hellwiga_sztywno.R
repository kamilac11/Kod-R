#MEOTDA WzORCOWA

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


# miejsce ""wpisz nazwe kolumny ktora wymaga stymulacji za pomoca przeksztalcenia ilorazowego
for (i in 1:nrow(dane_porzadkowanie)){
  dane_porzadkowanie[i,which(colnames(dane_porzadkowanie)=="PRZEBIEG_[km]")]=1/dane_porzadkowanie[i,which(colnames(dane_porzadkowanie)=="PRZEBIEG_[km]")]
}

#standaryzacja - na razie na sztywno brak weny na zbita forme, gdzies sie gubie

suma=0
srednia=0
odchylenie=0
for (j in 2:ncol(dane_porzadkowanie)){
  suma[j]=sum(dane_porzadkowanie[j])
  srednia[j]=suma[j]/nrow(dane_porzadkowanie)

  suma_kwadratow=0
  kwadrat=0
  for(i in 1:nrow(dane_porzadkowanie)){
    kwadrat=(dane_porzadkowanie[i,j]-srednia[j])^2
    suma_kwadratow=suma_kwadratow+kwadrat
  }
  
  odchylenie[j]=sqrt(suma_kwadratow/nrow(dane_porzadkowanie))  
  
  for (i in 1:nrow(dane_porzadkowanie)){
    dane_porzadkowanie[i,j]=(dane_porzadkowanie[i,j]-srednia[j])/odchylenie[j]
  }
}
  
##obiekt wzorcowy, zakladam ze to taki obiekt, ktory ma maksymalne wartosci zmiennych - 
#jako ze zmienne maja charakter stymulant

for (j in 2:ncol(dane_porzadkowanie)){
    obiekt_wz[j]=max(dane_porzadkowanie[j])
}

## odleglosci od kazdego obiektu 
odleg<- dane_porzadkowanie[c("Nr" )]
for (i in 1:nrow(dane_porzadkowanie)){
  SUMKA=0
  #SUMKA_KWADRAT=0
for (j in 2:ncol(dane_porzadkowanie)){
 
    SUMKA=SUMKA+(dane_porzadkowanie[i,j]-obiekt_wz[j])^2
    #SUMKA_KWADRAT=SUMKA_KWADRAT+SUMKA
    
}
  odleg[i,2]=sqrt(SUMKA) #kolumna zawierajaca odleglosci
  
}  

#dziala ale pomyslec o zapisie tablicowym tego

#srednia odlegosc #mean dziala gdy odl jest wektorem wartosci a nie tablica -tu trza petle
#mean(odleg[2])

#odchylenie standardowe
d_0=0
suma=0
srednia=0
odchylenie=0
for (j in 2:ncol(odleg)){
  suma[j]=sum(odleg[j])
  srednia[j]=suma[j]/nrow(odleg)
  
  suma_kwadratow=0
  kwadrat=0
  for(i in 1:nrow(odleg)){
    kwadrat=(odleg[i,j]-srednia[j])^2
    suma_kwadratow=suma_kwadratow+kwadrat
  }
  
  odchylenie[j]=sqrt(suma_kwadratow/nrow(odleg))  
  
  d_0=srednia[j]+2*odchylenie[j]  #d_0 to po prostu wartosc
  
}


#ostatnia kolumna to jak zawsze zmienna syntetyczna
dane_porzadkowanie[,"zmienna_syntetyczna"] <-0
for (i in 1:nrow(dane_porzadkowanie)){
  dane_porzadkowanie[i,ncol(dane_porzadkowanie)]=1-(odleg[i,2]/d_0)
}

dane_porzadkowanie<-dane_porzadkowanie[order(-dane_porzadkowanie$zmienna_syntetyczna),]