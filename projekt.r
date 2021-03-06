# # Pobrišemo PDF-je in počistimo delovno okolje
silent <- TRUE
source("clearpdf.r")

# 2. faza: Obdelava, uvoz in čiščenje podatkov
source("uvoz/uvoz.r",  encoding = "UTF-8")
source("slike/slike.R", encoding = "UTF-8")

# # 3. faza: Analiza in vizualizacija podatkov
source("vizualizacija/vizualizacija.r")

# # 4. faza: Napredna analiza podatkov
source("analiza/analiza.r")
source("analiza/animacija.R")
cat("Končano.\n")