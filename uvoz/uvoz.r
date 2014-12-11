# 2. faza: Uvoz podatkov

#uvoz podatkov iz spleta
uvoz_tabele2<-function(){
  u<-"http://www.internetlivestats.com/internet-users-by-country/"
  tables <- readHTMLTable(u)
  n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
  
  r <- tables[[which.max(n.rows)]]
  stolpci <- gsub("\\s+", " ", colnames(r))
  
  r <- data.frame(row.names = r$Country, r["Rank"],
                  apply(r[3:10], 2,
                        function(x) as.numeric(gsub("[,%]", "", x))))
  colnames(r) <- stolpci[-2]
  return(r)
}

cat("Uvažam podatke o uporabnikih interneta...\n")
uporabniki.interneta <- uvoz_tabele2()
