# 2. faza: Uvoz podatkov

# Funkcija, ki uvozi podatke iz datoteke druzine.csv
uvoziDruzine <- function() {
  return(read.table("podatki/druzine.csv", sep = ";", as.is = TRUE,
                      row.names = 1,
                      col.names = c("obcina", "en", "dva", "tri", "stiri"),
                      fileEncoding = "Windows-1250"))
}

# Zapišimo podatke v razpredelnico druzine.
cat("Uvažam podatke o družinah...\n")
druzine <- uvoziDruzine()

# Če bi imeli več funkcij za uvoz in nekaterih npr. še ne bi
# potrebovali v 3. fazi, bi bilo smiselno funkcije dati v svojo
# datoteko, tukaj pa bi klicali tiste, ki jih potrebujemo v
# 2. fazi. Seveda bi morali ustrezno datoteko uvoziti v prihodnjih
# fazah.


#uvoz podatkov iz spleta
library(XML) 
uvoz_tabele2<-function(){
  u<-"http://www.internetlivestats.com/internet-users-by-country/"
  tables <- readHTMLTable(u)
  n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
  
  tables[[which.max(n.rows)]]
  
  
}