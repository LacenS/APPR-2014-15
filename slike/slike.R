# program, ki izrise grafe
library(zoo)
library(spacetime)
library(ggplot2)
library(plotrix)
library(extrafont)
source("fontconfig.r")
library("RColorBrewer")


# 1 graf prikazuje prvih 10 drzav iz 2. tabele

prvi_graf <- function() {
  prvih10 <- data.frame(t2[1:10, ])
  attach(prvih10)
  barplot(apply(as.matrix(prvih10[ ,c(7,9)]), 1, c), beside = TRUE, xlab = "Countries", width = 1, 
          names.arg = row.names(prvih10), ylim = c(0,100), ylab = "%", cex.names = 0.6, las = 2,
          main = "Delež populacije z internetom v primerjavi z \ndeležem države v svetovnih uporabnikih interneta.",
          col = c("grey", "grey1"), cex = .9)
  legend("top", legend = c("Delež prebivalcev z internetom (%)", 
                                "Delež države v svetovnih uporabnikih interneta (%)"),
         cex = .5, pt.cex = .5, bty = "n", fill = c("grey", "grey1"), lty = c(0,0))
  detach(prvih10)
}

# Drugi graf iz tabele "t7"
drugi_graf <- function() {
  barplot(as.matrix(t7), beside = TRUE, ylim = c(0, 200), col = c(brewer.pal(9, "Blues"),"black"), 
          main = "Število držav razvrščenih v skupine glede na delež uporabnikov interneta\n po posameznih letih", 
          names.arg = substr(names(t7), 2, 5), cex.main = .9)
  legend("topright", 
         legend = c("0-10 %", "10-20 %", "20-30 %", "30-40 %", "40-50 %", "50-60 %", "60-70 %", "70-80 %", "80-90%", "90-100%"), 
         fill = c(brewer.pal(9, "Blues"), "black"), cex = .5)
  
}


# Graf iz 3 tabele-nara???anje ?tevila uporabnikov...
tretji_graf <- function() {
  dr <- data.frame(t3)
  print(qplot(rownames(dr), dr$Penetration....of.population.with.Internet.,
              data = dr, ylab = NULL, xlab = NULL, 
              main = "Delež uporabnikov interneta med 2000-2014")
        + theme(axis.text.x = element_text(size = 5)))
}

# Pita iz tabele t4
cetrti_graf <- function() {
  pie3D(t4[1:7, 5], labels = rownames(t4)[1:7], radius = 0.9, explode = 0.2, 
        main = "Delež uporabnikov glede na geografsko regijo",
        col = brewer.pal(7, "Blues"), labelcex = 0.8, start = pi/2)
}

# Zapi?imo grafe v pdf v mapi slike
cat("Ri?em stolpični graf:\nDele? populacije z internetom v primerjavi z deležem države v svetovnih uporabnikih interneta. \n")
cairo_pdf("slike/graf1.pdf", family = "Arial")
prvi_graf()
dev.off()

cat("Ri?em stolpični graf: \n število dr?av razvrščenih v skupine glede na dele? uporabnikov interneta po posameznih letih.")
cairo_pdf("slike/graf2.pdf", family = "Arial")
drugi_graf()
dev.off()

cat("Rišem graf naraščanja števila uporabnikov med leti 2000-2014. \n")
cairo_pdf("slike/graf3.pdf", family = "Arial")
tretji_graf()
dev.off()  

cat("Rišem \"pito\" : delež uporabnikov glede na geografsko regijo. \n")
cairo_pdf("slike/graf4.pdf", family = "Arial")
cetrti_graf()
dev.off()   


                   

