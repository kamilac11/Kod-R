#zaimportowac dane
library(readxl)
zbior_danych <- read_excel("~/Praca licencjacka/Moje dane_+zrodlo/zbior_danych.xlsx", 
                           sheet = "DANE_INNA_WERSJA", col_types = c("numeric","text", 
                                                                     "text", "text", "text", "text", "blank", 
                                                                     "numeric", "numeric", "numeric", 
                                                                     "numeric", "numeric", "text", "numeric", 
                                                                     "text", "text", "text", "text", "text", 
                                                                     "text", "numeric", "text", "text", 
                                                                     "text", "numeric", "numeric", "numeric", 
                                                                     "numeric", "numeric", "numeric"))

#rm(dane_sum)
dane_sum=zbior_danych

#zamiana przebieg-destymulanty w stymulanty - za pomoca przeksztalcenia roznicowego x_ij=max x_ij - x_ij
#max(dane_sum[10]) maksymalna wartosc w kolumnie przebieg
#maxim=max(dane_sum[11])

for (i in 1:61){
  dane_sum[i,11]=maxim-dane_sum[i,11]       #wczesniej by³ blad bo bralo w petli juz wystumulowana zmienna max(dane_sum[11])-dane_sum[i,11]
}





#unitaryzacja do metody sum! 
#cena, moc, pojemnosc, rok, przebieg


maksi=0
minim=0
for ( j in 7:11){
  maksi[j]=max(dane_sum[j])
  minim[j]=min(dane_sum[j])
  for (i in 1:nrow(dane_sum)){
    dane_sum[i,j]=(dane_sum[i,j]-minim[j])/(maksi[j]-minim[j])
  }
}



#zmienna syntetyczna
dane_sum$zmienna_syntetyczna=NULL
for (i in 1:61){
  dane_sum$zmienna_syntetyczna[i]=(dane_sum$`CENA.BRUTTO_[pln]`[i]+dane_sum$`MOC_[km]`[i]+dane_sum$`POJEMNOSC.SKOKOWA_[cm3]`[i]+
                              dane_sum$ROK.PRODUKCJI[i]+dane_sum$`PRZEBIEG_[km]`[i])/5
}

#eliminacja ujemnych wartosci zmiennej syntetycznej - przy wykorzystaniu przeksztalcenia roznicowego
min_zmienna=min(dane_sum$zmienna_syntetyczna)
for(i in 1:61){
  dane_sum$zmienna_syntetyczna[i]=dane_sum$zmienna_syntetyczna[i]-min_zmienna
}

#i ostateczna weersja zmiennej syntetycznje - stosujac normalizacje
max_zmienna=max(dane_sum$zmienna_syntetyczna)
for(i in 1:61){
  dane_sum$zmienna_syntetyczna[i]=dane_sum$zmienna_syntetyczna[i]/max_zmienna
}
## ORDER BY descenging
dane_sum<-dane_sum[order(-dane_sum$zmienna_syntetyczna),]

#pozycja
dane_sum$pozycja=NULL
for (i in 1:61){
  if(dane_sum$zmienna_syntetyczna[i]==max(dane_sum$zmienna_syntetyczna))
    dane_sum$pozycja[i]=1
  else
    dane_sum$pozycja[i]=i-1 #tu powinno byc length min
}


library(dplyr)
tabela_pomocnicza1<-select(dane_sum, c("Nr","MARKA", "MODEL",
                                    "zmienna_syntetyczna"))

tabela_pomocnicza2<-select(zbior_danych,c("Nr","CENA.BRUTTO_[pln]", "MOC_[km]", "POJEMNOSC.SKOKOWA_[cm3]","ROK.PRODUKCJI", 
                                          "PRZEBIEG_[km]"))

koncowa_tab<-merge(tabela_pomocnicza1,tabela_pomocnicza2, by="Nr", sort=FALSE) #porzadek zgodnie z tab_1
