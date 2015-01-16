# 3. faza: Izdelava zemljevida

# Uvozimo funkcijo za pobiranje in uvoz zemljevida.
source("lib/uvozi.zemljevid.r")

# Uvozimo zemljevid.
cat("Uvažam zemljevid...\n")

svet <- uvozi.zemljevid("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip",
                        "svet", "ne_50m_admin_0_countries.shp", mapa = "zemljevid",
                        encoding = "Windows-1252")



preuredi <- function(podatki, zemljevid) {
  nove.drzave <- svet$name_long[!svet$name_long %in% row.names(podatki)]
  manjkajo <- ! nove.drzave %in% rownames(podatki)
  M <- as.data.frame(matrix(nrow=sum(manjkajo), ncol=length(podatki)))
  names(M) <- names(podatki)
  row.names(M) <- nove.drzave[manjkajo]
  podatki <- rbind(podatki, M)
  
  out <- data.frame(podatki[order(rownames(podatki)), ])[rank(levels(zemljevid$name_long)[rank(zemljevid$name_long)]), ]
  if (ncol(podatki) == 1) {
    out <- data.frame(out)
    names(out) <- names(podatki)
    rownames(out) <- rownames(podatki)
  }
  return(out)
}

tab1 <- preuredi(t1[-c(37,38,39,34,46,53,66,67,68,69,77,98,101,108,127,128,133,135,136,139,140,153,156,161,177,181,182,184,196,204,214,216,217,242,252),], svet)
svet$X2000 <-tab1$X2000
svet$X2007 <-tab1$X2007
tab2 <- preuredi(t2, svet)
svet$X2014 <- tab2$Penetration....of.Pop..with.Internet.


# en zemljevid bo glede na % uporabnikov v letu 2000
# drugi glede na % uporabnikov v letu 2014
# tretnji glede na visino gdp
# in cetrti glede na pricakovano zivljenjsko dobo

# # Preuredimo podatke, da jih bomo lahko izrisali na zemljevid.
# druzine <- preuredi(druzine, obcine)
# 
# # Izračunamo povprečno velikost družine.
# druzine$povprecje <- apply(druzine[1:4], 1, function(x) sum(x*(1:4))/sum(x))
# min.povprecje <- min(druzine$povprecje, na.rm=TRUE)
# max.povprecje <- max(druzine$povprecje, na.rm=TRUE)
# 
# # Narišimo zemljevid v PDF.
# cat("Rišem zemljevid...\n")
# pdf("slike/povprecna_druzina.pdf", width=6, height=4)
# 
# n = 100
# barve = topo.colors(n)[1+(n-1)*(druzine$povprecje-min.povprecje)/(max.povprecje-min.povprecje)]
# plot(obcine, col = barve)
# 
# dev.off()