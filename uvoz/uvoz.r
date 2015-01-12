# 2. faza: Uvoz podatkov

#uvoz tabele csv iz te strani: http://data.worldbank.org/indicator/IT.NET.USER.P2
uvoz_tabele1 <- function() {
  t1<-read.csv("uporabniki_interneta_po_drzavah.csv", skip=2, na.strings="NA", sep=",", dec=".")
  tabela_1<-t1[, c(1, 3,34:58)]
  tab1 <- data.frame(Country = tabela_1$Country.Name,
                     Indicator.Name = tabela_1$Indicator.Name,
                     apply(tabela_1[3:26], 2, function(x) { if (is.numeric(x)){round(x, 2)}}))
  stolpci <- gsub("[X]", "", colnames(tab1))
  colnames(tab1)<-stolpci
  return(tab1)
  
  # podatki o  uporabnikih interneta po posameznih drzavah v letih 1990-2013
}




#uvoz podatkov iz spleta
library(XML)
uvoz_tabele2<-function(){
  u<-"http://www.internetlivestats.com/internet-users-by-country/" #Tabela prikazuje podatke o letu 2014
  tables <- readHTMLTable(u)
  n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
  
  tabela2 <- tables[[which.max(n.rows)]]
  stolpci <- gsub("\\s+", " ", colnames(r))
  
  tabela2 <- data.frame(Country = tabela2$Country, tabela2["Rank"],
                  apply(tabela2[3:10], 2,
                        function(x) as.numeric(gsub("[,%]", "", x))))
  colnames(tabela2) <- stolpci[-2]
  return(tabela2)
}

#Tabela prikazuje uporabnike interneta svetovnega prebivalstva med leti 1993-2014
uvoz_tabele3<-function(){
  u<-"http://www.internetlivestats.com/internet-users/#trend" 
  tables <- readHTMLTable(u)
  tabela <- tables[2] # tabela, ki jo zelim je drugi element na tej spletni strani
  stolpci <- c('Year', 'Internet users', 'Users growth', 'World population', 
               'Population growth', 'Penetration(% of population with Internet)')
  
  
}

cat("Uvazam podatke o uporabnikih interneta...\n")

