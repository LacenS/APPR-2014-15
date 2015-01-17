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


# vektor barv
#ramp <- colorRamp(c("blue", "white"))
#vektor <- rgb( ramp(seq(0, 1, length = 10)), max = 255)
vektor <- rev(palette(gray(seq(0,.9,len = 10))))
barve <- ifelse(is.na(svet$X2000), "white", "black")
barve[which(svet$X2000>=0 & svet$X2000<10)] <- vektor[1]
barve[which(svet$X2000>10 & svet$X2000<20)] <- vektor[2]
barve[which(svet$X2000>20 & svet$X2000<30)] <- vektor[3]
barve[which(svet$X2000>30 & svet$X2000<40)] <- vektor[4]
barve[which(svet$X2000>40 & svet$X2000<50)] <- vektor[5]
barve[which(svet$X2000>50 & svet$X2000<60)] <- vektor[6]
barve[which(svet$X2000>60 & svet$X2000<70)] <- vektor[7]
barve[which(svet$X2000>70 & svet$X2000<80)] <- vektor[8]
barve[which(svet$X2000>80 & svet$X2000<90)] <- vektor[9]
barve[which(svet$X2000>90)] <- vektor[10]
 
#1. zemljevid leto 2000
cat("Rišem zemljevid deleza uporabnikov interneta po svetu v letu 2000. \n")
pdf("slike/zemljevid1.pdf", width=6, height=4)
print(plot(svet, col=barve)) #To zdaj zgleda ok, problem je, da so napacni podatki v tabelah ??
# dodala bom se imenske oznake za nekatere drzave
dev.off()

#2 zemljevid
barve2 <- ifelse(is.na(svet$X2007), "white", "black")
barve2[which(svet$X2007>=0 & svet$X2007<10)] <- vektor[1]
barve2[which(svet$X2007>10 & svet$X2007<20)] <- vektor[2]
barve2[which(svet$X2007>20 & svet$X2007<30)] <- vektor[3]
barve2[which(svet$X2007>30 & svet$X2007<40)] <- vektor[4]
barve2[which(svet$X2007>40 & svet$X2007<50)] <- vektor[5]
barve2[which(svet$X2007>50 & svet$X2007<60)] <- vektor[6]
barve2[which(svet$X2007>60 & svet$X2007<70)] <- vektor[7]
barve2[which(svet$X2007>70 & svet$X2007<80)] <- vektor[8]
barve2[which(svet$X2007>80 & svet$X2007<90)] <- vektor[9]
barve2[which(svet$X2007>90)] <- vektor[10]
cat("Rišem zemljevid deleza uporabnikov interneta po svetu v letu 2007. \n")
pdf("slike/zemljevid2.pdf", width=6, height=4)


print(plot(svet, col=barve2))
dev.off()

#3 zemljevid bo glede na leto 2014 pobarvan se glede na gdp kategorije-high income...
#4 zemljevid bo glede na leto 2014 + kategorije zivljenjske dobe(ce ne bo zgledalo ok pa bom naredila graf)


# #Narišimo zemljevid v PDF.
# cat("Rišem zemljevid deleza uporabnikov interneta po svetu v letu 2000. \n")
# pdf("slike/zemljevid1.pdf", width=6, height=4)
# 
# spplot(svet, c("X2000","X2014"),col.regions=rainbow(10), scales=list(draw = TRUE))

# # creates a own color palette from red to green
 
# library(lattice) # required for trellis.par.set():
# trellis.par.set(sp.theme()) # sets color ramp to bpy.colors()
# 
# # (optional) defines the color breaks manually for a "skewed" color transition
# col_breaks = c(seq(-1,0,length=100),  # for red
#                seq(0,0.8,length=100),              # for yellow
#                seq(0.8,1,length=100))              # for green
# 
# 
# colors <- brewer.pal(9, "YlOrRd")
# 
# print(spplot(svet, "X2000", col.regions = rainbows(10, start=3/6, end=4/6),
#              main = "Delez uporabnikov interneta po svetu v letu 2000",
#              sp.layout = list(list("sp.polygons", svet[United States,], fill = "red"),
#                               list("sp.text", coordinates(svet[United States,]),
#                                    svet$stevilo.trgovin[USA], cex = 0.5))))

#dev.off()


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