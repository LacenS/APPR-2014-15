# 4. faza: Analiza podatkov

library("animation")

# 1. animacija spreminjanja deleža uporabnikov interneta od leta 2000-2014
cat("Rišem animacijo spreminjanja deleža uporabnikov interneta med leti 2000-2014")
# ## record plots and replay immediately
oopts = ani.options(interval = 2)
ani.record(reset = TRUE) 
saveHTML(autoplay=FALSE,{
  dev.control("enable")  # enable recording
  #   n = 14
  #   x = sort(rnorm(n))
  #   y = rnorm(n)
  par(bg = "white")  # ensure the background color is white
  plot(svet, col = barve00)
  legend("bottom", kategorije, fill = vektor,
         border = "black", cex = .42, xjust = 0.5, horiz = TRUE)
  title("Delež uporabnikov interneta v letu 2000", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  ani.record()  
  plot(svet, col = barve01)
  legend("bottom", kategorije, fill = vektor,
         border = "black", cex = .42, xjust = 0.5, horiz = TRUE)
  title("Delež uporabnikov interneta v letu 2001", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  ani.record()  
  plot(svet, col = barve02)
  legend("bottom", kategorije, fill = vektor,
         border = "black", cex = .42, xjust = 0.5, horiz = TRUE)
  title("Delež uporabnikov interneta v letu 2002", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  ani.record()  
  plot(svet, col = barve03)
  legend("bottom", kategorije, fill = vektor,
         border = "black", cex = .42, xjust = 0.5, horiz = TRUE)
  title("Delež uporabnikov interneta v letu 2003", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  ani.record()  
  plot(svet, col = barve04)
  legend("bottom", kategorije, fill = vektor,
         border = "black", cex = .42, xjust = 0.5, horiz = TRUE)
  title("Delež uporabnikov interneta v letu 2004", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  ani.record()  
  plot(svet, col = barve05)
  legend("bottom", kategorije, fill = vektor,
         border = "black", cex = .42, xjust = 0.5, horiz = TRUE)
  title("Delež uporabnikov interneta v letu 2005", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  ani.record()  
  plot(svet, col = barve06)
  legend("bottom", kategorije, fill = vektor,
         border = "black", cex = .42, xjust = 0.5, horiz = TRUE)
  title("Delež uporabnikov interneta v letu 2006", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  ani.record()  
  plot(svet, col = barve07)
  legend("bottom", kategorije, fill = vektor,
         border = "black", cex = .42, xjust = 0.5, horiz = TRUE)
  title("Delež uporabnikov interneta v letu 2007", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  ani.record()  
  plot(svet, col = barve08)
  legend("bottom", kategorije, fill = vektor,
         border = "black", cex = .42, xjust = 0.5, horiz = TRUE)
  title("Delež uporabnikov interneta v letu 2008", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  ani.record()  
  plot(svet, col = barve09)
  legend("bottom", kategorije, fill = vektor,
         border = "black", cex = .42, xjust = 0.5, horiz = TRUE)
  title("Delež uporabnikov interneta v letu 2009", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  ani.record()  
  plot(svet, col = barve10)
  legend("bottom", kategorije, fill = vektor,
         border = "black", cex = .42, xjust = 0.5, horiz = TRUE)
  title("Delež uporabnikov interneta v letu 2010", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  ani.record()  
  plot(svet, col = barve11)
  legend("bottom", kategorije, fill = vektor,
         border = "black", cex = .42, xjust = 0.5, horiz = TRUE)
  title("Delež uporabnikov interneta v letu 2011", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  ani.record()  
  plot(svet, col = barve12)
  legend("bottom", kategorije, fill = vektor,
         border = "black", cex = .42, xjust = 0.5, horiz = TRUE)
  title("Delež uporabnikov interneta v letu 2012", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  ani.record()  
  plot(svet, col = barve13)
  legend("bottom", kategorije, fill = vektor,
         border = "black", cex = .42, xjust = 0.5, horiz = TRUE)
  title("Delež uporabnikov interneta v letu 2013", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  ani.record()  
  plot(svet, col = barve14)
  legend("bottom", kategorije, fill = vektor,
         border = "black", cex = .42, xjust = 0.5, horiz = TRUE)
  title("Delež uporabnikov interneta v letu 2014", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  ani.record()  
},img.name = "spreminjanje.uporabnikov.interneta", htmlfile = "spreminjanje.uporabnikov.interneta.html",
ani.height = 680, ani.width = 900, interval = 1,
title = "Spreminjanje deleza uporabnikov interneta med leti 2000-2014.", 
description = c("Animacija spreminjanja deleza uporabnikov interneta med leti 2000-2014")
)

ani.options(oopts)









# 1. poskusala bom naredit graf za 5 drzav primerjava narascanja gdp cez leta ter narascanja up. interneta
# 2. poskusala bom napovedat kako bo narascalo st uporabnikov, verjetno samo za dolocene drzave??..
# recimo za slovenijo rabim tab1 in t6 pri t6 bi bilo dobro preuredit podatke...
# plot(tab1[205,], t6[211,])
#funkcija decompose
##ramp <- colorRamp(c("blue", "white"))
#vektor <- rgb( ramp(seq(0, 1, length = 10)), max = 255) ...za barve
# za izlocitev vrstic z manjkajocimi podatki funkcija complete.cases()




# # Uvozimo funkcijo za uvoz spletne strani.
# source("lib/xml.r")
# 
# # Preberemo spletno stran v razpredelnico.
# cat("Uvažam spletno stran...\n")
# tabela <- preuredi(uvozi.obcine(), obcine)
# 
# # Narišemo graf v datoteko PDF.
# cat("Rišem graf...\n")
# pdf("slike/naselja.pdf", width=6, height=4)
# plot(tabela[[1]], tabela[[4]],
#      main = "Število naselij glede na površino občine",
#      xlab = "Površina (km^2)",
#      ylab = "Št. naselij")
# dev.off()