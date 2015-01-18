# 4. faza: Analiza podatkov

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