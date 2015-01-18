# 3. faza: Izdelava zemljevida

# Uvozimo funkcijo za pobiranje in uvoz zemljevida.
source("lib/uvozi.zemljevid.r")

#Nalo?imo pakete, ki jih bomo potrebovali
library(maptools)
library(RColorBrewer)
library(classInt)

# Uvozimo zemljevid s pomo??jo funkcije uvozi zemljevid
cat("Uvažam zemljevid sveta...\n")

svet <- uvozi.zemljevid("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip",
                        "svet", "ne_50m_admin_0_countries.shp", mapa = "zemljevid",
                        encoding = "Windows-1252")

#Preurejanje tabel, da bodo v skladu z zemljevidom, ker funkcija preuredi zaradi prevelikih razlik ni delovala
#Preurejanje imen iz zemljevida
imena.svet <- gsub("Saint", "St.",
                   iconv(svet$name_long, from = "UTF-8", to = "ASCII//TRANSLIT")) 
names(imena.svet) <- imena.svet
imena.svet["Bahamas"] <- "Bahamas, The"
imena.svet["Cape Verde"] <- "Cabo Verde"
imena.svet["Dem. Rep. Korea"] <- "Korea, Rep."
imena.svet["Democratic Republic of the Congo"] <- "Congo, Dem. Rep."
imena.svet["Federated States of Micronesia"] <- "Micronesia, Fed. Sts."
imena.svet["Guernsey"] <- "Channel Islands"
imena.svet["Jersey"] <- "Channel Islands"
imena.svet["Kyrgyzstan"] <- "Kyrgyz Republic"
imena.svet["Macao"] <- "Macao SAR, China"
imena.svet["St.-Martin"] <- "St. Martin (French part)"
imena.svet["Sint Maarten"] <- "Sint Maarten (Dutch part)"
imena.svet["The Gambia"] <- "Gambia, The"
imena.svet["Russian Federation"] <- "Russia"

#Urejanje imen iz tabele t2
imena.t2 <- rownames(t2)
names(imena.t2) <- imena.t2
imena.t2["Bosnia Herzegovina"] <- "Bosnia and Herzegovina"
imena.t2["Brunei"] <- "Brunei Darussalam"
imena.t2["Congo"] <- "Republic of Congo" # ni jasno, katera dr?ava je mi?ljena
imena.t2["Gambia"] <- "The Gambia"
imena.t2["Hong Kong SAR"] <- "Hong Kong"
imena.t2["Micronesia"] <- "Federated States of Micronesia"
imena.t2["Sao Tome and Principe"] <- grep("Principe", svet$name_long, value=TRUE)
imena.t2["South Korea"] <- "Republic of Korea"
imena.t2["Viet Nam"] <- "Vietnam"

# Pri tabeli t8 se razen manjkajocih imena ujemajo
imena.t8 <- rownames(t8)
names(imena.t8) <- imena.t8

# Iz tabel t1, t2 in t8 naredimo nove tabele v katerih se vrstni red podatkov ujema s podatki iz zemljevida
m1 <- match(imena.svet, rownames(t1))
m2 <- match(as.character(svet$name_long), imena.t2)
m3 <- match(imena.svet, rownames(t8))
tab1 <- data.frame(t1[m1,])
tab2 <- data.frame(t2[m2,])
tab3 <- data.frame(t8[m3,])

# Dodamo stolpce s podatki v zemljevid
svet$X2000 <- tab1$X2000
svet$X2007 <- tab1$X2007
svet$X2011 <- tab1$X2011
svet$X2014 <- tab2$Penetration....of.Pop..with.Internet.
svet$leta <- as.numeric(levels(tab3$t8.m3...)[tab3$t8.m3...])


# Vektorji barv, da lahko ustrezno pobarvam zemljevide
vektor <- c(brewer.pal(9, "Blues"), "black")
vektor2 <- brewer.pal(8, "BuPu")
barve <- ifelse(is.na(svet$X2000), "white", "black")
barve <- vektor[floor(svet$X2000/10) + 1]
barve2 <- ifelse(is.na(svet$X2007), "white", "black")
barve2 <- vektor[floor(svet$X2007/10) + 1]
barve3 <- ifelse(is.na(svet$X2014), "white", "black")
barve3 <- vektor[floor(svet$X2014/10) + 1]
barve11 <- ifelse(is.na(svet$X2011), "white", "black")
barve11 <- vektor[floor(svet$X2011/10) + 1]
barve_leta <- ifelse(is.na(svet$leta), "white", "black")
barve_leta[which(svet$leta >= 45 & svet$leta < 50)] <- vektor2[1]
barve_leta[which(svet$leta >= 50 & svet$leta < 55)] <- vektor2[2]
barve_leta[which(svet$leta >= 55 & svet$leta < 60)] <- vektor2[3]
barve_leta[which(svet$leta >= 60 & svet$leta < 65)] <- vektor2[4]
barve_leta[which(svet$leta >= 65 & svet$leta < 70)] <- vektor2[5]
barve_leta[which(svet$leta >= 70 & svet$leta < 75)] <- vektor2[6]
barve_leta[which(svet$leta >= 75 & svet$leta < 80)] <- vektor2[7]
barve_leta[which(svet$leta >= 80)] <- vektor2[8]
kategorije <- c("0-10 %", "10-20 %", "20-30 %", "30-40 %", "40-50 %", 
                "50-60 %", "60-70 %", "70-80 %", "80-90 %", "90-100 %")

 #1. zemljevid glede na leto 2000, 2007, 2014
cat("Rišem zemljevid deleža uporabnikov interneta po svetu v letu 2000, 2007, 2014. \n")
cairo_pdf("slike/zemljevid1.pdf", width = 6, height = 4, family = "Arial", onefile = TRUE)
par(mar = rep(2, 4))
plot(svet, col = barve)
legend("bottom", kategorije, fill = vektor,
       border = "black", cex = .42, xjust = 0.5, horiz = TRUE)
title("Delež uporabnikov interneta v letu 2000", 
      cex.main = 1.5,   font.main = 2.5, col.main = "black")
plot(svet, col = barve2)
title("Delež uporabnikov interneta v letu 2007", 
      cex.main = 1.5, font.main = 2.5, col.main = "black")
legend("bottom", kategorije, fill = vektor,
       border = "black", cex = .42, xjust = 0.5, horiz = TRUE)
plot(svet, col = barve3)
legend("bottom", kategorije, fill = vektor,
       border = "black", cex = .42, xjust = 0.5, horiz = TRUE)
title("Delež uporabnikov interneta v letu 2014", cex.main = 1.5,   font.main = 2.5, col.main=  "black")
 
dev.off()


# Na zemljevidu za 2014 bom oznacila drzave, ki spadajo v high income group ter low income group.
cat("Rišem zemljevid deleža uporabnikov interneta po svetu v letu 2014,\n z označenimi državami, ki spadajo v \"high ali low income group\". \n")
cairo_pdf("slike/zemljevid2.pdf", width = 6, height = 4, family = "Arial")
plot(svet, col = barve3)
title("Uporabniki interneta v letu 2014 \n z označenimi dr?avami, ki spadajo v \"High income group\" in
      \"Low income group\"", cex.main = .5, font.main = 2, col.main = "black")
drzave1 <- which(svet$income_grp == "1. High income: OECD" | svet$income_grp == "2. High income: nonOECD")
drzave2 <- which(svet$income_grp == "5. Low income")
legend("bottom", kategorije, fill = vektor,
       border = "black", cex = .392, xjust = 0.5, horiz = TRUE)
legend(x = "topleft", legend = c("High income","Low income"),
        col = c("gold","deeppink"), lwd = 1, lty = c(0,0), 
        pch = c(19, 19), cex = .45)
points(coordinates(svet[drzave1, ]), pch = 20, col = "gold", cex = .3)
points(coordinates(svet[drzave2, ]), pch = 20, col = "deeppink", cex = .3) 

# 3 zemljevid: primerjava pri??akovane ?ivljenjske dobe in dele?a uporabnikov v letu 2011
cat("Rišem zemljevid dele?a uporabnikov interneta po svetu v letu 2011 \n v primerjavi s pričakovano življenjsko dobo. \n")
cairo_pdf("slike/zemljevid3.pdf", width = 6, height = 4, family = "Arial", onefile = TRUE)
par(mar = rep(2, 4))
plot(svet, col = barve11)
legend("bottom", kategorije, fill = vektor,
       border = "black", cex = .42, xjust = 0.5, horiz = TRUE)
title("Uporabniki interneta v letu 2011", 
      cex.main = 1.5, font.main = 2.5, col.main = "black")
plot(svet, col = barve_leta)
title("Pričakovana življenjska doba v letu 2011", 
      cex.main = 1.5, font.main = 2.5, col.main = "black")
legend("bottom", c("45-50", "50-55", "55-60", "60-65","65-70", "75-80", "80-85"), fill = vektor2,
       border = "black", cex = .42, xjust = 0.5, horiz = TRUE)

dev.off()

#se zemljevid glede na visino gdp v letu 2013
#ni se dokoncan!!!..ni prav obarvano, legenda se ni izdelana...!!!!
cat("Rišem zemljevid gdp pc v letu 2013. \n")
cairo_pdf("slike/zemljevid4.pdf", width = 6, height = 4, family = "Arial", onefile = TRUE)
par(mar = rep(2, 4))
t6<-data.frame(t6)
imf.norm <- scale(t6["X2013"][!is.na(t6["X2013"])])
k <- kmeans(imf.norm, 10, nstart = 1000)
t6 <- data.frame(t6)
drzave <- t6$Country
m <- match(svet$name_long, drzave)
bar <- rev(c(brewer.pal(9,"Greens"), "darkgreen"))
plot(svet, col = ifelse(is.na(m), "white", bar[k$cluster[t6$Country[m]]]))
#tukaj ne prikaze podatkov o USA
title("GDPpc v letu 2013", 
      cex.main = 2,   font.main= 3, col.main= "black")
legend("bottom", kategorije, fill = bar,
       border = "black", cex=.42, xjust=0.5, horiz=TRUE)
plot(svet, col=barve3)
legend("bottom",kategorije, fill = vektor,
       border = "black", cex=.42, xjust=0.5, horiz=TRUE)
title("Delež uporabnikov interneta v letu 2014", cex.main = 1.5,   font.main= 2, col.main= "black")

dev.off()






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
# # Izra??unamo povpre??no velikost dru?ine.
# druzine$povprecje <- apply(druzine[1:4], 1, function(x) sum(x*(1:4))/sum(x))
# min.povprecje <- min(druzine$povprecje, na.rm=TRUE)
# max.povprecje <- max(druzine$povprecje, na.rm=TRUE)
# 
# # Nari?imo zemljevid v PDF.
# cat("Ri?em zemljevid...\n")
# pdf("slike/povprecna_druzina.pdf", width=6, height=4)
# 
# n = 100
# barve = topo.colors(n)[1+(n-1)*(druzine$povprecje-min.povprecje)/(max.povprecje-min.povprecje)]
# plot(obcine, col = barve)
# 
# dev.off()