# 3. faza: Izdelava zemljevida

# Uvozimo funkcijo za pobiranje in uvoz zemljevida.
source("lib/uvozi.zemljevid.r")
library(maptools)
library(RColorBrewer)
library(classInt)

# Uvozimo zemljevid.
cat("Uvažam zemljevid...\n")
# d <- d[!is.na(d)]

svet <- uvozi.zemljevid("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip",
                        "svet", "ne_50m_admin_0_countries.shp", mapa = "zemljevid",
                        encoding = "Windows-1252")



# preuredi <- function(podatki, zemljevid) {
#   nove.drzave <- svet$name_long[!svet$name_long %in% row.names(podatki)]
#   manjkajo <- ! nove.drzave %in% rownames(podatki)
#   M <- as.data.frame(matrix(nrow=sum(manjkajo), ncol=length(podatki)))
#   names(M) <- names(podatki)
#   row.names(M) <- nove.drzave[manjkajo]
#   podatki <- rbind(podatki, M)
#   
#   out <- data.frame(podatki[order(rownames(podatki)), ])[rank(levels(zemljevid$name_long)[rank(zemljevid$name_long)]), ]
#   if (ncol(podatki) == 1) {
#     out <- data.frame(out)
#     names(out) <- names(podatki)
#     rownames(out) <- rownames(podatki)
#   }
#   return(out)
# }

imena.svet <- gsub("Saint", "St.",
                   iconv(svet$name_long, from = "UTF-8", to = "ASCII//TRANSLIT"))
names(imena.svet) <- imena.svet
imena.svet["Bahamas"] <- "Bahamas, The"
imena.svet["Cape Verde"] <- "Cabo Verde"
imena.svet["Dem. Rep. Korea"] <- "Congo, Dem. Rep."
imena.svet["Democratic Republic of the Congo"] <- "Korea, Rep."
imena.svet["Federated States of Micronesia"] <- "Micronesia, Fed. Sts."
imena.svet["Guernsey"] <- "Channel Islands"
imena.svet["Jersey"] <- "Channel Islands"
imena.svet["Kyrgyzstan"] <- "Kyrgyz Republic"
imena.svet["Macao"] <- "Macao SAR, China"
imena.svet["St.-Martin"] <- "St. Martin (French part)"
imena.svet["Sint Maarten"] <- "Sint Maarten (Dutch part)"
imena.svet["The Gambia"] <- "Gambia, The"

imena.t2 <- rownames(t2)
names(imena.t2) <- imena.t2
imena.t2["Bosnia Herzegovina"] <- "Bosnia and Herzegovina"
imena.t2["Brunei"] <- "Brunei Darussalam"
imena.t2["Congo"] <- "Republic of Congo" # ni jasno, katera država je mišljena
imena.t2["Gambia"] <- "The Gambia"
imena.t2["Hong Kong SAR"] <- "Hong Kong"
imena.t2["Micronesia"] <- "Federated States of Micronesia"
imena.t2["Russia"] <- "Russian Federation"
imena.t2["Sao Tome and Principe"] <- grep("Principe", svet$name_long, value=TRUE)
imena.t2["South Korea"] <- "Republic of Korea"
imena.t2["Viet Nam"] <- "Vietnam"

m1 <- match(imena.svet, rownames(t1))
m2 <- match(as.character(svet$name_long), imena.t2)
tab1 <- data.frame(t1[m1,])
tab2 <- data.frame(t2[m2,])
svet$X2000 <-tab1$X2000
svet$X2007 <-tab1$X2007

svet$X2014 <- tab2$Penetration....of.Pop..with.Internet.


# vektor barv
#ramp <- colorRamp(c("blue", "white"))
#vektor <- rgb( ramp(seq(0, 1, length = 10)), max = 255)
vektor <- c(brewer.pal(9, "Blues"),"black")
barve <- ifelse(is.na(svet$X2000), "white", "black")
barve <- vektor[floor(svet$X2000/10) + 1]

 
#1. zemljevid leto 2000
cat("Rišem zemljevid deleza uporabnikov interneta po svetu v letu 2000. \n")
pdf("slike/zemljevid1.pdf", width=6, height=4)
print(plot(svet, col=barve)) 
# dodala bom se imenske oznake za nekatere drzave
dev.off()

#2 zemljevid
barve2 <- ifelse(is.na(svet$X2007), "white", "black")
barve2 <- vektor[floor(svet$X2007/10) + 1]
cat("Rišem zemljevid deleza uporabnikov interneta po svetu v letu 2007. \n")
pdf("slike/zemljevid2.pdf", width=6, height=4)


print(plot(svet, col=barve2))
dev.off()

#3 zemljevid bo glede na leto 2014 pobarvan se glede na gdp kategorije-high income...
barve3 <- ifelse(is.na(svet$X2014), "white", "black")
barve3 <- vektor[floor(svet$X2014/10) + 1]
cat("Rišem zemljevid deleza uporabnikov interneta po svetu v letu 2014. \n")
pdf("slike/zemljevid3.pdf", width=6, height=4)


print(plot(svet, col=barve3))
dev.off()



#4 zemljevid bo glede na leto 2014 + kategorije zivljenjske dobe(ce ne bo zgledalo ok pa bom naredila graf)

# da bom dobila drzave  po skupinah :high income, ...


# tab5 <- preuredi(t5, svet)
# drzave1 <- which(tab5$IncomeGroupe=="High income")
# drzave2 <- which(tab5$IncomeGroupe=="Upper middle income")
# drzave3 <- which(tab5$IncomeGroupe=="Lower middle income")
# drzave4 <- which(tab5$IncomeGroupe=="Low income")








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