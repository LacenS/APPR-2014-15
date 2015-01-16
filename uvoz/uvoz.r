# 2. faza: Uvoz podatkov

#uvoz tabele csv iz te strani: http://data.worldbank.org/indicator/IT.NET.USER.P2
uvoz_tabele1 <- function() {
  t1<-read.csv("podatki/uporabniki_interneta_po_drzavah.csv", skip=2, na.strings="NA", sep=",", dec=".", fileEncoding="Windows-1252")
  tabela_1<-t1[, c(1, 3,44:58)]
  tab1 <- data.frame(row.names = tabela_1$Country.Name,
                     Indicator.Name = tabela_1$Indicator.Name,
                     apply(tabela_1[3:16], 2, function(x) { if (is.numeric(x)){round(x, 2)}}))
  stolpci <- gsub("[X]", "", colnames(tab1))
  colnames(tab1)<-stolpci
  rownames(tab1)[247]<-"Venezuela"
  rownames(tab1)[48]<-"Republic of Congo"
  rownames(tab1)[71]<-'Egypt'
  rownames(tab1)[110]<-"Iran"
  rownames(tab1)[193]<-"Republic of Korea"
  rownames(tab1)[157]<-"Macedonia"
  rownames(tab1)[254]<-"Yemen"
  rownames(tab1)[248]<-"United States Virgin Islands"
  rownames(tab1)[226]<-"Syria"
  rownames(tab1)[220]<-"Slovakia"
  rownames(tab1)[99]<-"Hong Kong"
  
    
   
    
  
  return(tab1)
   # za izlocitev vrstic z manjkajocimi podatki bom kasneje uporabila funkcijo complete.cases()
  
  # podatki o  uporabnikih interneta po posameznih drzavah v letih 1990-2013
}

cat("Uvazam podatke o uporabnikih interneta po posameznih drzavah v letih 1990-2013\n")


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
  vrstice<-r[,1]
  
  r <- data.frame(row.names = r$Country, r["Rank"],
                  apply(r[3:10], 2,
                        function(x) as.numeric(gsub("[,%]", "", x))))
  colnames(r) <- stolpci[-1]
  
  
  return(r)
}
cat("Uvazam podatke o stevilu uporabnikov interneta po posameznih drzavah v letu 2014 v primerjavi z delezem svetovne populacije.\n")


#Tabela prikazuje uporabnike interneta svetovnega prebivalstva med leti 1993-2014
uvoz_tabele3<-function(){
  u<-"http://www.internetlivestats.com/internet-users/#trend" 
  tables <- readHTMLTable(u)
  tabela3 <- tables[2] # tabela, ki jo zelim je drugi element na tej spletni strani
  stolpci <- c('Year', 'Internet users', 'Users growth %', 'World population', 
               'Population growth %', 'Penetration (% of population with Internet)')
  tabela3 <- data.frame(tabela3)
  vrstice <- tabela3$internet.users.table.Year.July.1.
  
  tabela3 <- data.frame(row.names=vrstice, apply(tabela3[2:6], 2,
                              function(x) as.numeric(gsub("[,%]", "", x))))
  colnames(tabela3) <- stolpci[-1]
  
  return(tabela3[1:22, ])
  
  
}
cat('Uvazam podatke o delezu uporabnikov interneta glede na celotno populacijo med leti 1993-2014.\n')

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
  t4 <- data.frame(row.names=vrstice, apply(t4[2:6], 2 ,function(x) as.numeric(gsub("[,%]", "",x))))
  
  colnames(t4) <- stolpci[-1]
  
  return(t4)
  
  
  
}
cat("Uvazam podatke o uporabnikih interneta po geografskih regijah-kontinentih.\n")

# Uvoz 5 tabele, ki prikazuje drzave glede na "Income group""(za primerjavo med delezem uporabnikov interneta)
# v obliki .csv iz te strani: http://databank.worldbank.org/data/views/reports/tableview.aspx?isshared=true

uvoz_tabele5 <- function() {
  t5 <- read.csv("podatki/gdp.pc.csv")
  t5 <- t5[,c(1,4)]
  t5 <- data.frame(Country=t5[,1], apply(t5[2], 2 ,function(x) gsub(": nonOECD", "", x)))
  t5 <- data.frame( row.names=t5$Country, apply(t5[2], 2 ,function(x) gsub(": OECD", "", x)))
  leveli <- c("Low income", "Lower middle income", "Upper middle income",  "High income")
  t5$IncomeGroup<-factor(t5$IncomeGroup, levels=leveli, ordered=TRUE)
  return(t5)
  # ali return(na.omit(t5))...da se znebim vrstic z manjkajocimi podatki
  
}
cat('Uvazam tabelo, ki prikazuje drzave glede na "Income group".\n')

# Uvoz 6 tabele, ki prikazuje gdp pc
uvoz_tabele6 <- function() {
  t6 <- read.csv("podatki/gdp.pc.st.csv", skip=1, fileEncoding="Windows-1252")
  t6 <- t6[, c(1, 45 :58)] # da imam podatke samo od 200-2013 (2014 so tako samo "NA-ji")
  t6 <- data.frame(Country = t6$Country.Name,
                     apply(t6[2:length(colnames(t6))], 2, function(x) { if (is.numeric(x)){round(x, 2)}}))
  stolpci <- gsub("[X]", "", colnames(t6))
  colnames(t6)<-stolpci
  return(t6) 
  
  
}
cat("Uvazam podatke o GDP per capita po posameznih drzavah.\n")

t1 <- uvoz_tabele1()
t2 <- uvoz_tabele2()
t3 <- uvoz_tabele3()
t4 <- uvoz_tabele4()
t5 <- uvoz_tabele5()
t6 <- uvoz_tabele6()

#tabela sestavljena iz t1 in t2, ki jo bom uporabila pri kasnejsi analizi
tabela_7 <- function(){
  
  kategorije <- c("0-10 %", "10-20 %", "20-30 %", "30-40 %", "40-50 %", 
                  "50-60 %", "60-70 %", "70-80 %", "80-90 %", "90-100 %")
  #vrstice, ki ustrezajo pogoju
  k1_2000 <- which(t1["2000"] <= 10)
  k2_2000 <- which(t1["2000"] <= 20 & t1["2000"]  > 10 )
  k3_2000 <- which(t1["2000"] <= 30 & t1["2000"]  > 20 )
  k4_2000 <- which(t1["2000"] <= 40 & t1["2000"]  > 30 )
  k5_2000 <- which(t1["2000"] <= 50 & t1["2000"]  > 40 )
  k6_2000 <- which(t1["2000"] <= 60 & t1["2000"]  > 50 )
  k7_2000 <- which(t1["2000"] <= 70 & t1["2000"]  > 60 )
  k8_2000 <- which(t1["2000"] <= 80 & t1["2000"]  > 70 )
  k9_2000 <- which(t1["2000"] <= 90 & t1["2000"]  > 80 )
  k10_2000 <- which(t1["2000"] > 90 )
  k1_2003 <- which(t1["2003"] <= 10)
  k2_2003 <- which(t1["2003"] <= 20 & t1["2003"]  > 10 )
  k3_2003 <- which(t1["2003"] <= 30 & t1["2003"]  > 20 )
  k4_2003 <- which(t1["2003"] <= 40 & t1["2003"]  > 30 )
  k5_2003 <- which(t1["2003"] <= 50 & t1["2003"]  > 40 )
  k6_2003 <- which(t1["2003"] <= 60 & t1["2003"]  > 50 )
  k7_2003 <- which(t1["2003"] <= 70 & t1["2003"]  > 60 )
  k8_2003 <- which(t1["2003"] <= 80 & t1["2003"]  > 70 )
  k9_2003 <- which(t1["2003"] <= 90 & t1["2003"]  > 80 )
  k10_2003 <- which(t1["2003"] > 90)
  k1_2006 <- which(t1["2006"] <= 10)
  k2_2006 <- which(t1["2006"] <= 20 & t1["2006"]  > 10 )
  k3_2006 <- which(t1["2006"] <= 30 & t1["2006"]  > 20 )
  k4_2006 <- which(t1["2006"] <= 40 & t1["2006"]  > 30 )
  k5_2006 <- which(t1["2006"] <= 50 & t1["2006"]  > 40 )
  k6_2006 <- which(t1["2006"] <= 60 & t1["2006"]  > 50 )
  k7_2006 <- which(t1["2006"] <= 70 & t1["2006"]  > 60 )
  k8_2006 <- which(t1["2006"] <= 80 & t1["2006"]  > 70 )
  k9_2006 <- which(t1["2006"] <= 90 & t1["2006"]  > 80 )
  k10_2006 <- which(t1["2006"] > 90 )
  k1_2009 <- which(t1["2009"] <= 10)
  k2_2009 <- which(t1["2009"]<= 20 & t1["2009"]  > 10 )
  k3_2009 <- which(t1["2009"] <= 30 & t1["2009"] > 20 )
  k4_2009 <- which(t1["2009"] <= 40 & t1["2009"]  > 30 )
  k5_2009 <- which(t1["2009"] <= 50 & t1["2009"]  > 40 )
  k6_2009 <- which(t1["2009"] <= 60 & t1["2009"]  > 50 )
  k7_2009 <- which(t1["2009"] <= 70 & t1["2009"]  > 60 )
  k8_2009 <- which(t1["2009"] <= 80 & t1["2009"]  > 70 )
  k9_2009 <- which(t1["2009"] <= 90 & t1["2009"]  > 80 )
  k10_2009 <- which(t1["2009"] > 90)
  k1_2012 <- which(t1["2012"] <= 10)
  k2_2012 <- which(t1["2012"] <= 20 & t1["2012"]  > 10 )
  k3_2012 <- which(t1["2012"] <= 30 & t1["2012"]  > 20 )
  k4_2012 <- which(t1["2012"] <= 40 & t1["2012"]  > 30 )
  k5_2012 <- which(t1["2012"] <= 50 & t1["2012"]  > 40 )
  k6_2012 <- which(t1["2012"] <= 60 & t1["2012"]  > 50 )
  k7_2012 <- which(t1["2012"] <= 70 & t1["2012"]  > 60 )
  k8_2012 <- which(t1["2012"] <= 80 & t1["2012"]  > 70 )
  k9_2012 <- which(t1["2012"] <= 90 & t1["2012"]  > 80 )
  k10_2012 <- which(t1["2012"] > 90  )
  k1_2014 <- which(t2[, 7] <= 10)
  k2_2014 <- which(t2[,7] <= 20 & t2[,7] > 10 )
  k3_2014 <- which(t2[,7] <= 30 & t2[,7] > 20 )
  k4_2014 <- which(t2[,7] <= 40 & t2[,7] > 30 )
  k5_2014 <- which(t2[,7] <= 50 & t2[,7] > 40 )
  k6_2014 <- which(t2[,7] <= 60 & t2[,7] > 50 )
  k7_2014 <- which(t2[,7] <= 70 & t2[,7] > 60 )
  k8_2014 <- which(t2[,7] <= 80 & t2[,7] > 70 )
  k9_2014 <- which(t2[,7] <= 90 & t2[,7] > 80 )
  k10_2014 <- which(t2[,7] > 90 )
  
  l2000 <- c(length(k1_2000),length(k2_2000),length(k3_2000),length(k4_2000),
             length(k5_2000),length(k6_2000),length(k7_2000),length(k8_2000),
             length(k9_2000),length(k10_2000))
  l2003 <- c(length(k1_2003),length(k2_2003),length(k3_2003),length(k4_2003),
             length(k5_2003),length(k6_2003),length(k7_2003),length(k8_2003),
             length(k9_2003),length(k10_2003))
  l2006 <- c(length(k1_2006),length(k2_2006),length(k3_2006),length(k4_2006),
             length(k5_2006),length(k6_2006),length(k7_2006),length(k8_2006),
             length(k9_2006),length(k10_2006))
  l2009 <- c(length(k1_2009),length(k2_2009),length(k3_2009),length(k4_2009),
             length(k5_2009),length(k6_2009),length(k7_2009),length(k8_2009),
             length(k9_2009),length(k10_2009))
  l2012 <- c(length(k1_2012),length(k2_2012),length(k3_2012),length(k4_2012),
             length(k5_2012),length(k6_2012),length(k7_2012),length(k8_2012),
             length(k9_2012),length(k10_2012))
  l2014 <- c(length(k1_2014),length(k2_2014),length(k3_2014),length(k4_2014),
             length(k5_2014),length(k6_2014),length(k7_2014),length(k8_2014),
             length(k9_2014),length(k10_2014))
  
  
  zagraf<-data.frame(
    k1=c(length(k1_2000),length(k1_2003),length(k1_2006),length(k1_2009),length(k1_2012),length(k1_2014)),
    k2=c(length(k2_2000),length(k2_2003),length(k2_2006),length(k2_2009),length(k2_2012),length(k2_2014)),
    k3=c(length(k3_2000),length(k3_2003),length(k3_2006),length(k3_2009),length(k3_2012),length(k3_2014)),
    k4=c(length(k4_2000),length(k4_2003),length(k4_2006),length(k4_2009),length(k4_2012),length(k4_2014)),
    k5=c(length(k5_2000),length(k5_2003),length(k5_2006),length(k5_2009),length(k5_2012),length(k5_2014)),
    k6=c(length(k6_2000),length(k6_2003),length(k6_2006),length(k6_2009),length(k6_2012),length(k6_2014)),
    k7=c(length(k7_2000),length(k7_2003),length(k7_2006),length(k7_2009),length(k7_2012),length(k7_2014)),
    k8=c(length(k8_2000),length(k8_2003),length(k8_2006),length(k8_2009),length(k8_2012),length(k8_2014)),
    k9=c(length(k9_2000),length(k9_2003),length(k9_2006),length(k9_2009),length(k9_2012),length(k9_2014)),
    k10=c(length(k10_2000),length(k10_2003),length(k10_2006),length(k10_2009),length(k10_2012),length(k10_2014)))
  
  rownames(zagraf)<-c("2000","2003","2006","2009","2012","2014")
  colnames(zagraf)<-c("0-10 %","10-20 %","20-30 %" ,"30-40 %" ,"40-50 %" ,"50-60 %" ,"60-70 %" ,"70-80 %" ,"80-90 %" ,"90-100 %")
  nov<-data.frame(t(zagraf))
  
  rownames(nov)<-kategorije
  
  return(nov)
  
  
  
  
  
}

t7 <- tabela_7()



# uvoz tabele, ki prikazuje pricakovano zivljenjsko dobo po posameznih drzavah
#(bom kasneje uredila, ce jo bom potrebovala)
# u <- "http://www.nationmaster.com/country-info/stats/Health/Life-expectancy-at-birth%2C-total/Years#map"
# tables <- readHTMLTable(u)
# tabela7 <- tables[1]


