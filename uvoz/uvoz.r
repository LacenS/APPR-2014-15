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




#uvoz podatkov iz spleta iz te strani: http://www.internetlivestats.com/internet-users-by-country/
#tabela prikazuje podatke o stevilu uporabnikov interneta po posameznih drzavah v letu 2014, delez populacije itd...
library(XML)
uvoz_tabele2<-function(){
  u<-"http://www.internetlivestats.com/internet-users-by-country/"
  tables <- readHTMLTable(u)
  n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
  
  r <- tables[[which.max(n.rows)]]
  stolpci <- gsub("\\s+", " ", colnames(r))
  stolpci <- c(stolpci[2], stolpci[1], stolpci[3:length(stolpci)])
  rownames(r)<-r[,1]
  
  r <- data.frame(r["Country"], r["Rank"],
                  apply(r[3:10], 2,
                        function(x) as.numeric(gsub("[,%]", "", x))))
  colnames(r) <- stolpci
  return(r)
}


#Tabela prikazuje uporabnike interneta svetovnega prebivalstva med leti 1993-2014
uvoz_tabele3<-function(){
  u<-"http://www.internetlivestats.com/internet-users/#trend" 
  tables <- readHTMLTable(u)
  tabela3 <- tables[2] # tabela, ki jo zelim je drugi element na tej spletni strani
  stolpci <- c('Year', 'Internet users', 'Users growth %', 'World population', 
               'Population growth %', 'Penetration (% of population with Internet)')
  tabela3 <- data.frame(tabela3)
  colnames(tabela3)<-stolpci
  tabela3 <- data.frame(tabela3["Year"],
                        apply(tabela3[2:6], 2,
                              function(x) as.numeric(gsub("[,%]", "", x))))
  return(tabela3[1:22, ])
  
  
}

#Uvoz tabele iz te strani: http://www.internetworldstats.com/stats.htm
#Tabela prikazuje uporabnike interneta po geografskih regijah
uvoz_tabele4<-function(){
  u<-"http://www.internetworldstats.com/stats.htm" 
  tables <- readHTMLTable(u)
  t4 <- data.frame(tables[6])
  stolpci <- c('World Regions', 'Population(2014 Est.)', 
               'Internet Users\nDec. 31, 2000', 'Internet Users\nLatest Data', 
               'Penetration(% Population)', 'Growth\n2000-2014', 'Users %\nof Table')
  colnames(t4) <- stolpci
  t4 <- t4[2:9,]
  rownames(t4) <- t4[,1]
  t4 <- t4[, 2:7]
  vrstice <- rownames(t4)
  stolpci <- colnames(t4)
  t4 <- data.frame(Country=vrstice, apply(tab[1:6], 2 ,function(x) as.numeric(gsub("[,%]", "",x))))
  
  colnames(t4) <- c("Country", stolpci)
  return(t4)
  
  
  
}

# Uvoz 5 tabele, ki prikazuje drzave glede na "Income group""(za primerjavo med delezem uporabnikov interneta)
# v obliki .csv iz te strani: http://databank.worldbank.org/data/views/reports/tableview.aspx?isshared=true
uvoz_tabele5 <- function() {
  t5 <- read.csv("gdp.pc.csv")
  t5 <- t5[,c(1,4)]
  t5 <- data.frame( Country=t5$X...Country.Name, apply(t5[2], 2 ,function(x) gsub(": nonOECD", "", x)))
  t5 <- data.frame( Country=t5["Country"], apply(t5[2], 2 ,function(x) gsub(": OECD", "", x)))
  leveli <- c("Low income", "Lower middle income", "Upper middle income",  "High income")
  t5$IncomeGroup<-factor(t5$IncomeGroup, levels=leveli, ordered=TRUE)
  return(t5)
  
}

# Uvoz 6 tabele, ki prikazuje gdp pc
uvoz_tabele6 <- function() {
  t6 <- read.csv("gdp.pc.st.csv", skip=1)
  t6 <- t6[, c(1, 45 :58)] # da imam podatke samo od 200-2013 (2014 so tako samo "NA-ji")
  return(t6) #ne cisto "urejena tabela" 
  
  
}

  


cat("Uvazam podatke o uporabnikih interneta...\n")



