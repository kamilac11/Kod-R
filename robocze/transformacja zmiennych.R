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
library(dplyr)
test<-select(zbior_danych, c("Nr","CENA.BRUTTO_[pln]","MOC_[km]","POJEMNOSC.SKOKOWA_[cm3]",
                          "ROK.PRODUKCJI", "PRZEBIEG_[km]" ))

#Stymmulacja zmiennych - funkcje
##w miejsce x wpisz nazwe na zbiorze na ktorym pracujesz, a w miejsce y - nazwie kolumny
###UWAGA: dane stymuluj po kolei, po kazdej stymulacji nadpisuj zbior, aby zmiany zostaly zachowane

#dla destymulant

stymulacja_przeksztalcenie_ilorazowe<-function(x,y){
  for (i in 1:nrow(x)){
    x[i,which(colnames(x)=="y")]=1/x[i,which(colnames(x)=="y")]
  }
  return(x)
}



stymulacja_przeksztalcenie_roznicowe<-function(x,y){
  max_wartosc=max(x$'y')
  for (i in 1:nrow(x)){
    x[i,which(colnames(x)=="y")]=max_wartosc-x[i,which(colnames(x)=="y")]
  }
  return(x)
}


#Po dokonaniu stymulacji, nadpisz zbior na ktorym pracujesz, na zbior po stymulacji
#nadpisanie podzbioru
dane_porzadkowanie<-stymulacja_przeksztalcenie_roznicowe(dane_porzadkowanie,"PRZEBIEG_[km]")
#lub
dane_porzadkowanie<-stymulacja_przeksztalcenie_ilorazowe(dane_porzadkowanie,"PRZEBIEG_[km]")


#normalizacja wystumulowanego podzbioru przez: (w miejsce x wpisz wystmylowany nadpisany zbior do porzadkowania)
##unitaryzacje: 

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


#mean(as.vector(dane2[7])) #bo dane2[4] traktuje jako lista a mean dziala na wektorach, to max dziala na listach- nie jest glupie bo to sortowanie plus minut

##przekszta³cenie ilorazowe, przy odniesieniu do wartoœci oczekiwanej (gdzie jest to wartoœæ œrednia dla zbiorów skoñczonych)
przeksztalcenie_ilorazowe<-function(x){
    suma=0
    srednia=0
    for (j in 2:ncol(x)){
      suma[j]=sum(x[j])
      srednia[j]=suma[j]/nrow(x)
      
    for(i in 1:nrow(x)){
      x[i,j]=x[i,j]/srednia[j]
      }
    } 
  return(x)
}

b=przeksztalcenie_ilorazowe(test)


##spr czy srednia dziala srednia: 
#a<-function(x){
 # suma=0
  #srednia=0
  #for (j in 2:ncol(x)){
  #suma[j]=sum(x[j])
  #srednia[j]=suma[j]/nrow(x)
  #}
  #return(srednia)
  #}

##standaryzacja - potrzeba dla zmiennej sredniej i odchylenia
standaryzacja<-function(x){
  suma=0
  srednia=0
<<<<<<< HEAD
=======
  suma_kwadratow=0
  kwadrat=0
>>>>>>> 369aba3ae20b5c9f0a3d45251d0026b11024e064
  odchylenie=0
  for (j in 2:ncol(x)){
    suma[j]=sum(x[j])
    srednia[j]=suma[j]/nrow(x)
    
<<<<<<< HEAD
    suma_kwadratow=0
    kwadrat=0
    for(i in 1:nrow(x)){
      kwadrat=(x[i,j]-srednia[j])^2
      suma_kwadratow=suma_kwadratow+kwadrat
    }
    
    odchylenie[j]=sqrt(suma_kwadratow/nrow(x))  
    
    for (i in 1:nrow(x)){
      x[i,j]=(x[i,j]-srednia[j])/odchylenie[j]
=======
    for(i in 1:nrow(x)){
      kwadrat[i]=(x[i,j]-srednia[j])^2
      suma_kwadratow[j]=suma_kwadratow[j]+kwadrat[i]

>>>>>>> 369aba3ae20b5c9f0a3d45251d0026b11024e064
    }
    odchylenie[j]=sqrt(suma_kwadratow[j]/nrow(x))
  for(k in 1:nrow(x)){
    x[i,j]=(x[i,j]-srednia[j])/odchylenie[j]
  }
<<<<<<< HEAD
=======
    } 
>>>>>>> 369aba3ae20b5c9f0a3d45251d0026b11024e064
  return(x)
}
#cos nie dziala przemyslec
uu<-standaryzacja(test)




