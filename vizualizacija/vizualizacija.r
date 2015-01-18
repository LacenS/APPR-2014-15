# 3. faza: Izdelava zemljevida

# Uvozimo funkcijo za pobiranje in uvoz zemljevida.
source("lib/uvozi.zemljevid.r")
library(maptools)
library(RColorBrewer)
library(classInt)

# Uvozimo zemljevid.
cat("Uvažam zemljevid...\n")

svet <- uvozi.zemljevid("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip",
                        "svet", "ne_50m_admin_0_countries.shp", mapa = "zemljevid",
                        encoding = "Windows-1252")

#preurejanje tabel, ker funkcija preuredi ni delovala
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

# razen manjkajocih se imena ujemajo
imena.t8 <- rownames(t8)
names(imena.t8) <- imena.t8


m1 <- match(imena.svet, rownames(t1))
m2 <- match(as.character(svet$name_long), imena.t2)
m3 <- match(as.character(svet$name_long), imena.t8)
tab1 <- data.frame(t1[m1,])
tab2 <- data.frame(t2[m2,])
tab3 <- data.frame(t8[m3,])

svet$X2000 <-tab1$X2000
svet$X2007 <-tab1$X2007
svet$X2011 <-tab1$X2011
svet$X2014 <- tab2$Penetration....of.Pop..with.Internet.
svet$leta <- as.numeric(tab3$t8.m3...)


# vektor barv
#ramp <- colorRamp(c("blue", "white"))
#vektor <- rgb( ramp(seq(0, 1, length = 10)), max = 255)
vektor <- c(brewer.pal(9, "Blues"),"black")
vektor2 <- c(brewer.pal(9, "BuPu"),"#330033")
barve <- ifelse(is.na(svet$X2000), "white", "black")
barve <- vektor[floor(svet$X2000/10) + 1]
barve2 <- ifelse(is.na(svet$X2007), "white", "black")
barve2 <- vektor[floor(svet$X2007/10) + 1]
barve3 <- ifelse(is.na(svet$X2014), "white", "black")
barve3 <- vektor[floor(svet$X2014/10) + 1]
barve11 <- ifelse(is.na(svet$X2011), "white", "black")
barve11 <- vektor[floor(svet$X2011/10) + 1]
barve_leta <- ifelse(is.na(svet$leta), "white", "black")
barve_leta <- vektor2[floor(svet$leta/10) + 1]
kategorije <- c("0-10 %", "10-20 %", "20-30 %", "30-40 %", "40-50 %", 
                "50-60 %", "60-70 %", "70-80 %", "80-90 %", "90-100 %")

 
#1. zemljevid leto 2000
cat("Rišem zemljevid deleza uporabnikov interneta po svetu v letu 2000, 2007, 2014. \n")
pdf("slike/zemljevid1.pdf", width=6, height=4)
par(mar = rep(2, 4))
print(plot(svet, col=barve)) 
legend("bottom",kategorije, fill = vektor,
       border = "black", cex=.42, xjust=0.5, horiz=TRUE)
title("Uporabniki interneta v letu 2000", 
      cex.main = 2,   font.main= 3, col.main= "black")
print(plot(svet, col=barve2))
title("Uporabniki interneta v letu 2007", 
      cex.main = 2,   font.main= 3, col.main= "black")
legend("bottom",kategorije, fill = vektor,
       border = "black", cex=.42, xjust=0.5, horiz=TRUE)
print(plot(svet, col=barve3))
legend("bottom",kategorije, fill = vektor,
       border = "black", cex=.42, xjust=0.5, horiz=TRUE)
title("Uporabniki interneta v letu 2014", cex.main = 2,   font.main= 3, col.main= "black")
 
dev.off()


# na zemljevidu za 2014 bom oznacila drzave, ki spadajo v high income group
cat("Rišem zemljevid deleza uporabnikov interneta po svetu v letu 2014 z oznacenimi drzavami, ki spadajo v \"high income group\". \n")
pdf("slike/zemljevid2.pdf", width=6, height=4)
par(mar = rep(2, 4))
print(plot(svet, col=barve3))
title("Uporabniki interneta v letu 2014 \n z oznacenimi drzavami, ki spadajo v \"High income group\"", cex.main = 1,   font.main= 2, col.main= "black")
drzave1 <- which(svet$income_grp=="1. High income: OECD" | svet$income_grp=="2. High income: nonOECD"  )
legend("bottom",kategorije, fill = vektor,
       border = "black", cex=.392, xjust=0.5)
points(coordinates(svet[drzave1,]), pch = 20, col="White", cex=.3) #vidimo da se ujema z drzavami z najvecjim delezem..(oznake bom se kasneje spremenila..)
print(plot(svet, col=barve3))
title("Uporabniki interneta v letu 2014 \n z oznacenimi drzavami, ki spadajo v \"Low income group\"", cex.main = 1,   font.main= 2, col.main= "black")
drzave2 <- which(svet$income_grp=="5. Low income" )
legend("bottom",kategorije, fill = vektor,
       border = "black", cex=.392, xjust=0.5)
points(coordinates(svet[drzave2,]), pch = 20, col="Red", cex=.3) #vidimo da se ujema z drzavami z najvecjim delezem..(oznake bom se kasneje spremenila..)

dev.off()

# 3 zemljevid primerjava pricakovane zivljenjske dobe in deleza uporabnikov v letu 2011
cat("Rišem zemljevid deleza uporabnikov interneta po svetu v letu 2011 v primerjavi s pricakovano zivljenjsko dobo. \n")
pdf("slike/zemljevid3.pdf", width=6, height=4)
par(mar = rep(2, 4))
print(plot(svet, col=barve11)) 
legend("bottom",kategorije, fill = vektor,
       border = "black", cex=.42, xjust=0.5, horiz=TRUE)
title("Uporabniki interneta v letu 2011", 
      cex.main = 2,   font.main= 3, col.main= "black")
print(plot(svet, col=barve_leta))
title("Pricakovana zivljenjska doba v letu 2011", 
      cex.main = 2,   font.main= 3, col.main= "black")
legend("bottom",kategorije, fill = vektor2,
       border = "black", cex=.42, xjust=0.5, horiz=TRUE)

dev.off()




# spplot(svet, c("X2011", "SID79"), names.attr = c("1974","1979"),
#        colorkey=list(space="bottom"), scales = list(draw = TRUE),
#        main = "SIDS (sudden infant death syndrome) in North Carolina",
#        sp.layout = list(arrow), as.table = TRUE)




#se zemljevid glede na visino gdp v letu 2013
# rrt <- nc$SID74/nc$BIR74
# brks <- quantile(rrt, seq(0,1,1/10)) #bom razdelila v 10 kategorij
# cols <- grey((length(brks):2)/length(brks))
# dens <- (2:length(brks))*3






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