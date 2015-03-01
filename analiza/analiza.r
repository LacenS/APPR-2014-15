# 4. faza: Analiza podatkov

library("animation")
library("psych")
library("gridExtra")

# 2.) tabela z vsemi uporabnimi podatki zbranimi
zdruzeno <- data.frame(svet[,c(19,c(64:93))])
#podatki za slovenijo
slovenija <- zdruzeno[205, ]
slo<-data.frame(t(slovenija[,c(2:15)]), t(slovenija[,c(18:31)]))
colnames(slo)<-c("internet", "gdp_pc")

# Pri vseh drzavah bi dobili podobno, npr. za Slovenijo:
attach(slo)
cairo_pdf("slike/slo1.pdf", width = 6, height = 4, family = "Arial", onefile = TRUE)
plot(gdp_pc, internet, xlab="gdp pc v US$", ylab="Delež uporabnikov interneta")
lin<-lm(internet~gdp_pc)
abline(lin, col="blue")
title("Primerjava naraščanja gdp pc ter uporabnikov interneta\n v Sloveniji med leti 2000-2013", 
      cex.main = .9,  font.main = 2.5, col.main = "black")
detach(slo)
dev.off()

#vrstice z vsemi podatki
nova<-zdruzeno[complete.cases(zdruzeno),]
#sortirane po delezu uporabnikov od najmanjsega do najvecjega, glede na leto 2014
order.2014 <- order(nova$X2014)
nova2<-nova[order.2014, ]

# 5 drzav z najmanjsim delezem uporabnikov, 5 s srednjim ter 5 z najvecjim
povzetek <- nova2[c(1, 2, 3, 4, 5, 68, 69, 70, 71, 72, 137, 138, 139, 140, 141), ]

# Graf za primerjavo med gdp v letu 2013, delezem uporabnikov ter pricakovano zivljenjsko dobo
cairo_pdf("slike/pari1.pdf", width = 6, height = 4, family = "Arial", onefile = TRUE)
attach(povzetek)
colnames(povzetek)[c(15,17,31)] <- c("Uporabniki interneta v letu 2013", "Pričakovana življenjska doba", "Gdp pc v letu 2013")
skalar1 <- scale(povzetek[, c(15,17,31)])
k <- kmeans(skalar1, 6, nstart=1000)
kat <- k$cluster
barve <- c("chocolate1", "green", "deepskyblue", "darkmagenta", "gold", "deeppink")
pairs(skalar1, col = barve[kat])

dev.off()
detach(povzetek)

# Povezava med gdp ter delezem uporabnikov interneta
attach(nova2)
cairo_pdf("slike/gdpinternet.pdf", width = 6, height = 4, family = "Arial", onefile = TRUE)
print(qplot(X2013, gdp2013, data = nova2, geom = c("point", "smooth"),
            main = "Razmerje med gdp pc v US$\n ter deležem uporabnikov interneta",
            xlab = "Uporabniki interneta", ylab = "gdp pc v US$", cex = .8))
dev.off()

# ter med pricakovano zivljenjsko dobo ter delezem uporabnikov interneta
cairo_pdf("slike/letainternet.pdf", width = 6, height = 4, family = "Arial", onefile = TRUE)
print(qplot(X2011, leta, data = nova2, geom = c("point", "smooth"), 
            main = "Razmerje med pričakovano življenjsko dobo\n ter deležem uporabnikov interneta", cex = .8,
            xlab = "Uporabniki interneta", ylab = "Pričakovana življenjska doba"))

dev.off()
detach(nova2)




