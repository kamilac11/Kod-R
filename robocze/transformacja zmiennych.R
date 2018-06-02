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
}

mean(as.vector(dane2[7])) #bo dane2[4] traktuje jako lista a mean dziala na wektorach, to max dziala na listach- nie jest glupie bo to sortowanie plus minut
max(dane2[7])
#klopot z mean why??? 
##przekszta³cenie ilorazowe, przy odniesieniu do wartoœci oczekiwanej (gdzie jest to wartoœæ œrednia dla zbiorów skoñczonych)
przeksztalcenie_roznicowe<-function(x){
  srednia=0
  for (j in 2:ncol(x)){
    srednia[j]=mean(x[j])
   
    for (i in 1:nrow(x)){
      x[i,j]=(x[i,j]-minim[j])/(maksi[j]-minim[j])
    }
  }
}




