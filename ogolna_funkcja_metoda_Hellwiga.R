#MEOTDA WzORCOWA

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
dane_Hellwig<-zbior_danych[c("Nr","CENA.BRUTTO_[pln]","MOC_[km]","POJEMNOSC.SKOKOWA_[cm3]","ROK.PRODUKCJI","PRZEBIEG_[km]")]



#zamiana na stymulante przebiegu( jest to destymulanta, zmienna mierzona na skali ilorazowej -stad przeksztalcenie ilorazowe)
#UWAGA U¯YTKOWNIK MUSI WIEDZIEC JAKI CHARAKTER MAJA JEGO ZMIENNE, zakladam ze powinny
#byc stymulantami, wiec jesli u niego nie sa to musi to zmienic


# miejsce ""wpisz nazwe kolumny ktora wymaga stymulacji
for (i in 1:nrow(dane_Hellwig)){
  dane_Hellwig[i,which(colnames(dane_Hellwig)=="PRZEBIEG_[km]")]=1/dane_Hellwig[i,which(colnames(dane_Hellwig)=="PRZEBIEG_[km]")]
}
#mo¿na od razu skorzystac z funkcji przeksztalcenie ilorazowe: 
# miejsze y, wpisz nazwe kolumny w ""
stymulacja_przeksztalcenie_ilorazowe<-function(x,y){
  for (i in 1:nrow(x)){
    x[i,which(colnames(x)==y)]=1/x[i,which(colnames(x)==y)]
  }
  return(x)
}
dane_Hellwig<-stymulacja_przeksztalcenie_ilorazowe(dane_Hellwig,"PRZEBIEG_[km]")



#standaryzacja - na razie na sztywno brak weny na zbita forme, gdzies sie gubie
#suma=0
#srednia=0
#suma_kwadratow=0
#kwadrat=0
#odchylenie=0
#for (j in 2:ncol(dane_Hellwig)){
 # suma[j]=sum(dane_Hellwig[j])
 # srednia[j]=suma[j]/nrow(dane_Hellwig)
  
  #suma_kwadratow=0
  #kwadrat=0
 # for(i in 1:nrow(dane_Hellwig)){
   # kwadrat=(dane_Hellwig[i,j]-srednia[j])^2
   # suma_kwadratow=suma_kwadratow+kwadrat
  #}
  
  #odchylenie[j]=sqrt(suma_kwadratow/nrow(dane_Hellwig))  
  
 # for (i in 1:nrow(dane_Hellwig)){
  #  dane_Hellwig[i,j]=(dane_Hellwig[i,j]-srednia[j])/odchylenie[j]
  #}
#}

####################
#skorzystac ze zdefiniowanej funkcji standaryzacji zamiast tak krok po kroku
#suma=0
#srednia=0
#odchylenie=0
#for (j in 2:ncol(dane_Hellwig)){
#  suma[j]=sum(dane_Hellwig[j])
#  srednia[j]=suma[j]/nrow(dane_Hellwig)
#  suma_kwadratow=0
#  kwadrat=0
#  for(i in 1:nrow(dane_Hellwig)){
#    kwadrat=(dane_Hellwig[i,j]-srednia[j])^2
#    suma_kwadratow=suma_kwadratow+kwadrat
#  }
#  
#  odchylenie[j]=sqrt(suma_kwadratow/nrow(dane_Hellwig))  
#  for (i in 1:nrow(dane_Hellwig)){
#    dane_Hellwig[i,j]=(dane_Hellwig[i,j]-srednia[j])/odchylenie[j]
#  }
#}



standaryzacja<-function(x){
      suma=0
      srednia=0
      odchylenie=0
      for (j in 2:ncol(x)){
        suma[j]=sum(x[j])
        srednia[j]=suma[j]/nrow(x)
        suma_kwadratow=0
        kwadrat=0
        for(i in 1:nrow(x)){
          kwadrat=(x[i,j]-srednia[j])^2
          suma_kwadratow=suma_kwadratow+kwadrat
        }
        odchylenie[j]=sqrt(suma_kwadratow/nrow(x))  
        for (i in 1:nrow(x)){
          x[i,j]=(x[i,j]-srednia[j])/odchylenie[j]
        } 
      }
      return(x)
    }





##obiekt wzorcowy, zakladam ze to taki obiekt, ktory ma maksymalne wartosci zmiennych - 
#jako ze zmienne maja charakter stymulant

metoda_Hellwiga<-function(x){
  x<-standaryzacja(x) #standaryzacja stymulant
  #wyznaczenie obiektu wzorcowego
obiekt_wz=0
for (j in 2:ncol(x)){  #od 2kolumny bo 1 kolumna to Nr - index
  obiekt_wz[j]=max(x[j])
    }

## odleglosci od kazdego obiektu 
odleg<- x[c("Nr" )]
for (i in 1:nrow(x)){
  SUMKA=0
  for (j in 2:ncol(x)){
    SUMKA=SUMKA+(x[i,j]-obiekt_wz[j])^2
  }
  odleg[i,2]=sqrt(SUMKA) #kolumna zawierajaca odleglosci
}  

#odchylenie standardowe dla odleglosci - moze tez tu mozna skorzystac z funkcji z tym ze dla innego argumentu
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
    x[,"zmienna_syntetyczna"] <-0
        for (i in 1:nrow(x)){
      x[i,ncol(x)]=1-(odleg[i,2]/d_0)
        }
x<-x[order(-x$zmienna_syntetyczna),]
return(x[1])
}

metoda_Hellwiga(dane_Hellwig)

