library("animation")
library("psych", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
library("gridExtra", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")

# 1. animacija spreminjanja deleža uporabnikov interneta od leta 2000-2014
cat("Izdelujem animacijo spreminjanja deleža uporabnikov interneta med leti 2000-2014")
#html verzija
oopts = ani.options(interval = 2, autobrowse = FALSE)
ani.record(reset = TRUE) 
saveHTML(autoplay=FALSE,{
  dev.control("enable")  #Omogoči "snemanje"
  par(bg = "white")  #Zagotovi, da je ozadje belo
  plot(svet, col = barve00)
  legend("bottom", kategorije, fill = vektor,
         border = "black", cex = .9, xjust = 0.5, horiz = TRUE)
  title("Delež uporabnikov interneta v letu 2000", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  ani.record()  
  plot(svet, col = barve01)
  legend("bottom", kategorije, fill = vektor,
         border = "black", cex = .9, xjust = 0.5, horiz = TRUE)
  title("Delež uporabnikov interneta v letu 2001", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  ani.record()  
  plot(svet, col = barve02)
  legend("bottom", kategorije, fill = vektor,
         border = "black", cex = .9, xjust = 0.5, horiz = TRUE)
  title("Delež uporabnikov interneta v letu 2002", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  ani.record()  
  plot(svet, col = barve03)
  legend("bottom", kategorije, fill = vektor,
         border = "black", cex = .9, xjust = 0.5, horiz = TRUE)
  title("Delež uporabnikov interneta v letu 2003", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  ani.record()  
  plot(svet, col = barve04)
  legend("bottom", kategorije, fill = vektor,
         border = "black", cex = .9, xjust = 0.5, horiz = TRUE)
  title("Delež uporabnikov interneta v letu 2004", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  ani.record()  
  plot(svet, col = barve05)
  legend("bottom", kategorije, fill = vektor,
         border = "black", cex = .9, xjust = 0.5, horiz = TRUE)
  title("Delež uporabnikov interneta v letu 2005", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  ani.record()  
  plot(svet, col = barve06)
  legend("bottom", kategorije, fill = vektor,
         border = "black", cex = .9, xjust = 0.5, horiz = TRUE)
  title("Delež uporabnikov interneta v letu 2006", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  ani.record()  
  plot(svet, col = barve07)
  legend("bottom", kategorije, fill = vektor,
         border = "black", cex = .9, xjust = 0.5, horiz = TRUE)
  title("Delež uporabnikov interneta v letu 2007", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  ani.record()  
  plot(svet, col = barve08)
  legend("bottom", kategorije, fill = vektor,
         border = "black", cex = .9, xjust = 0.5, horiz = TRUE)
  title("Delež uporabnikov interneta v letu 2008", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  ani.record()  
  plot(svet, col = barve09)
  legend("bottom", kategorije, fill = vektor,
         border = "black", cex = .9, xjust = 0.5, horiz = TRUE)
  title("Delež uporabnikov interneta v letu 2009", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  ani.record()  
  plot(svet, col = barve10)
  legend("bottom", kategorije, fill = vektor,
         border = "black", cex = .9, xjust = 0.5, horiz = TRUE)
  title("Delež uporabnikov interneta v letu 2010", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  ani.record()  
  plot(svet, col = barve11)
  legend("bottom", kategorije, fill = vektor,
         border = "black", cex = .9, xjust = 0.5, horiz = TRUE)
  title("Delež uporabnikov interneta v letu 2011", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  ani.record()  
  plot(svet, col = barve12)
  legend("bottom", kategorije, fill = vektor,
         border = "black", cex = .9, xjust = 0.5, horiz = TRUE)
  title("Delež uporabnikov interneta v letu 2012", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  ani.record()  
  plot(svet, col = barve13)
  legend("bottom", kategorije, fill = vektor,
         border = "black", cex = .9, xjust = 0.5, horiz = TRUE)
  title("Delež uporabnikov interneta v letu 2013", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  ani.record()  
  plot(svet, col = barve14)
  legend("bottom", kategorije, fill = vektor,
         border = "black", cex = .9, xjust = 0.5, horiz = TRUE)
  title("Delež uporabnikov interneta v letu 2014", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  ani.record()  
},img.name = "spreminjanje.uporabnikov.interneta", htmlfile = "spreminjanje.uporabnikov.interneta.html",
ani.height = 680, ani.width = 900, interval = 1,
title = "Spreminjanje deleza uporabnikov interneta med leti 2000-2014.", 
description = c("Animacija spreminjanja deleza uporabnikov interneta med leti 2000-2014")
)
ani.options(oopts, autobrowse = FALSE)

#pdf verzija, ki jo bom vkljucila v porocilo, vendar pri meni ne dela(os x op. sistem)
cairo_pdf("slike/animacija.pdf", width = 6, height = 4, family = "Arial", onefile = TRUE)
plot(svet, col = barve00)
legend("bottom", kategorije, fill = vektor,
       border = "black", cex = .45, xjust = 0.5, horiz = TRUE)
title("Delež uporabnikov interneta v letu 2000", 
      cex.main = 1.5,   font.main = 2.5, col.main = "black")

plot(svet, col = barve01)
legend("bottom", kategorije, fill = vektor,
       border = "black", cex = .45, xjust = 0.5, horiz = TRUE)
title("Delež uporabnikov interneta v letu 2001", 
      cex.main = 1.5,   font.main = 2.5, col.main = "black")

plot(svet, col = barve02)
legend("bottom", kategorije, fill = vektor,
       border = "black", cex = .45, xjust = 0.5, horiz = TRUE)
title("Delež uporabnikov interneta v letu 2002", 
      cex.main = 1.5,   font.main = 2.5, col.main = "black")

plot(svet, col = barve03)
legend("bottom", kategorije, fill = vektor,
       border = "black", cex = .45, xjust = 0.5, horiz = TRUE)
title("Delež uporabnikov interneta v letu 2003", 
      cex.main = 1.5,   font.main = 2.5, col.main = "black")

plot(svet, col = barve04)
legend("bottom", kategorije, fill = vektor,
       border = "black", cex = .45, xjust = 0.5, horiz = TRUE)
title("Delež uporabnikov interneta v letu 2004", 
      cex.main = 1.5,   font.main = 2.5, col.main = "black")

plot(svet, col = barve05)
legend("bottom", kategorije, fill = vektor,
       border = "black", cex = .45, xjust = 0.5, horiz = TRUE)
title("Delež uporabnikov interneta v letu 2005", 
      cex.main = 1.5,   font.main = 2.5, col.main = "black")

plot(svet, col = barve06)
legend("bottom", kategorije, fill = vektor,
       border = "black", cex = .45, xjust = 0.5, horiz = TRUE)
title("Delež uporabnikov interneta v letu 2006", 
      cex.main = 1.5,   font.main = 2.5, col.main = "black")

plot(svet, col = barve07)
legend("bottom", kategorije, fill = vektor,
       border = "black", cex = .45, xjust = 0.5, horiz = TRUE)
title("Delež uporabnikov interneta v letu 2007", 
      cex.main = 1.5,   font.main = 2.5, col.main = "black")

plot(svet, col = barve08)
legend("bottom", kategorije, fill = vektor,
       border = "black", cex = .45, xjust = 0.5, horiz = TRUE)
title("Delež uporabnikov interneta v letu 2008", 
      cex.main = 1.5,   font.main = 2.5, col.main = "black")

plot(svet, col = barve09)
legend("bottom", kategorije, fill = vektor,
       border = "black", cex = .45, xjust = 0.5, horiz = TRUE)
title("Delež uporabnikov interneta v letu 2009", 
      cex.main = 1.5,   font.main = 2.5, col.main = "black")

plot(svet, col = barve10)
legend("bottom", kategorije, fill = vektor,
       border = "black", cex = .45, xjust = 0.5, horiz = TRUE)
title("Delež uporabnikov interneta v letu 2010", 
      cex.main = 1.5,   font.main = 2.5, col.main = "black")

plot(svet, col = barve11)
legend("bottom", kategorije, fill = vektor,
       border = "black", cex = .45, xjust = 0.5, horiz = TRUE)
title("Delež uporabnikov interneta v letu 2011", 
      cex.main = 1.5,   font.main = 2.5, col.main = "black")

plot(svet, col = barve12)
legend("bottom", kategorije, fill = vektor,
       border = "black", cex = .45, xjust = 0.5, horiz = TRUE)
title("Delež uporabnikov interneta v letu 2012", 
      cex.main = 1.5,   font.main = 2.5, col.main = "black")

plot(svet, col = barve13)
legend("bottom", kategorije, fill = vektor,
       border = "black", cex = .45, xjust = 0.5, horiz = TRUE)
title("Delež uporabnikov interneta v letu 2013", 
      cex.main = 1.5,   font.main = 2.5, col.main = "black")

plot(svet, col = barve14)
legend("bottom", kategorije, fill = vektor,
       border = "black", cex = .45, xjust = 0.5, horiz = TRUE)
title("Delež uporabnikov interneta v letu 2014", 
      cex.main = 1.5,   font.main = 2.5, col.main = "black")
dev.off()




# 3.) primerjava spreminjanja gdp ter uporabnikov interneta na zemljevidu
cat("Izdelujem animacijo spreminjanja deleža uporabnikov interneta med leti 2000-2013 \nv primerjavi s spreminjanjem gdp pc. ")
ani.record(reset = TRUE) 
oopts2 = ani.options(interval = 4, autobrowse = FALSE)
saveHTML(autoplay=FALSE,{
  dev.control("enable")  # enable recording
  par(bg = "white")  # ensure the background color is white
  par(mfrow=c(1,2))
  plot(svet, col = barve00)
  legend("bottom", kategorije, fill = vektor,
         border = "black", cex = .75 , ncol = 4, xjust = 1)
  title("Delež uporabnikov interneta v letu 2000", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  plot(svet, col = barvegdp00)
  legend("bottom", kateg2000, fill = bar,
         border = "black", cex=.7, ncol = 4, title = "Gdp pc v US$", xjust=1)
  title("Višina gdp pc v letu 2000", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  ani.record()  
  par(mfrow=c(1,2))
  plot(svet, col = barve01)
  legend("bottom", kategorije, fill = vektor,
         border = "black", cex = .75 , ncol = 4, xjust = 1)
  title("Delež uporabnikov interneta v letu 2001", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  plot(svet, col = barvegdp01)
  legend("bottom", kateg2001, fill = bar,
         border = "black", cex=.7, ncol = 4, title = "Gdp pc v US$", xjust=1)
  title("Višina gdp pc v letu 2001", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  ani.record() 
  par(mfrow=c(1,2))
  plot(svet, col = barve02)
  legend("bottom", kategorije, fill = vektor,
         border = "black", cex = .75 , ncol = 4, xjust = 1)
  title("Delež uporabnikov interneta v letu 2002", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  plot(svet, col = barvegdp02)
  legend("bottom", kateg2002, fill = bar,
         border = "black", cex=.7, ncol = 4, title = "Gdp pc v US$", xjust=1)
  title("Višina gdp pc v letu 2002", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  ani.record() 
  par(mfrow=c(1,2))
  plot(svet, col = barve03)
  legend("bottom", kategorije, fill = vektor,
         border = "black", cex = .75 , ncol = 4, xjust = 1)
  title("Delež uporabnikov interneta v letu 2003", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  plot(svet, col = barvegdp03)
  legend("bottom", kateg2003, fill = bar,
         border = "black", cex=.7, ncol = 4, title = "Gdp pc v US$", xjust=1)
  title("Višina gdp pc v letu 2003", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  ani.record() 
  par(mfrow=c(1,2))
  plot(svet, col = barve04)
  legend("bottom", kategorije, fill = vektor,
         border = "black", cex = .75 , ncol = 4, xjust = 1)
  title("Delež uporabnikov interneta v letu 2004", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  plot(svet, col = barvegdp04)
  legend("bottom", kateg2004, fill = bar,
         border = "black", cex=.7, ncol = 4, title = "Gdp pc v US$", xjust=1)
  title("Višina gdp pc v letu 2004", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  ani.record() 
  par(mfrow=c(1,2))
  plot(svet, col = barve05)
  legend("bottom", kategorije, fill = vektor,
         border = "black", cex = .75 , ncol = 4, xjust = 1)
  title("Delež uporabnikov interneta v letu 2005", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  plot(svet, col = barvegdp05)
  legend("bottom", kateg2005, fill = bar,
         border = "black", cex=.7, ncol = 4, title = "Gdp pc v US$", xjust=1)
  title("Višina gdp pc v letu 2005", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  ani.record() 
  par(mfrow=c(1,2))
  plot(svet, col = barve06)
  legend("bottom", kategorije, fill = vektor,
         border = "black", cex = .75 , ncol = 4, xjust = 1)
  title("Delež uporabnikov interneta v letu 2006", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  plot(svet, col = barvegdp06)
  legend("bottom", kateg2006, fill = bar,
         border = "black", cex=.7, ncol = 4, title = "Gdp pc v US$", xjust=1)
  title("Višina gdp pc v letu 2006", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  ani.record() 
  par(mfrow=c(1,2))
  plot(svet, col = barve07)
  legend("bottom", kategorije, fill = vektor,
         border = "black", cex = .75 , ncol = 4, xjust = 1)
  title("Delež uporabnikov interneta v letu 2007", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  plot(svet, col = barvegdp07)
  legend("bottom", kateg2007, fill = bar,
         border = "black", cex=.7, ncol = 4, title = "Gdp pc v US$", xjust=1)
  title("Višina gdp pc v letu 2007", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  ani.record() 
  par(mfrow=c(1,2))
  plot(svet, col = barve08)
  legend("bottom", kategorije, fill = vektor,
         border = "black", cex = .75 , ncol = 4, xjust = 1)
  title("Delež uporabnikov interneta v letu 2008", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  plot(svet, col = barvegdp08)
  legend("bottom", kateg2008, fill = bar,
         border = "black", cex=.7, ncol = 4, title = "Gdp pc v US$", xjust=1)
  title("Višina gdp pc v letu 2008", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  ani.record() 
  par(mfrow=c(1,2))
  plot(svet, col = barve09)
  legend("bottom", kategorije, fill = vektor,
         border = "black", cex = .75 , ncol = 4, xjust = 1)
  title("Delež uporabnikov interneta v letu 2009", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  plot(svet, col = barvegdp09)
  legend("bottom", kateg2009, fill = bar,
         border = "black", cex=.7, ncol = 4, title = "Gdp pc v US$", xjust=1)
  title("Višina gdp pc v letu 2009", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  ani.record() 
  par(mfrow=c(1,2))
  plot(svet, col = barve10)
  legend("bottom", kategorije, fill = vektor,
         border = "black", cex = .75 , ncol = 4, xjust = 1)
  title("Delež uporabnikov interneta v letu 2010", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  plot(svet, col = barvegdp10)
  legend("bottom", kateg2010, fill = bar,
         border = "black", cex=.7, ncol = 4, title = "Gdp pc v US$", xjust=1)
  title("Višina gdp pc v letu 2010", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  ani.record() 
  par(mfrow=c(1,2))
  plot(svet, col = barve11)
  legend("bottom", kategorije, fill = vektor,
         border = "black", cex = .75 , ncol = 4, xjust = 1)
  title("Delež uporabnikov interneta v letu 2011", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  plot(svet, col = barvegdp11)
  legend("bottom", kateg2011, fill = bar,
         border = "black", cex=.7, ncol = 4, title = "Gdp pc v US$", xjust=1)
  title("Višina gdp pc v letu 2011", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  ani.record() 
  par(mfrow=c(1,2))
  plot(svet, col = barve12)
  legend("bottom", kategorije, fill = vektor,
         border = "black", cex = .75 , ncol = 4, xjust = 1)
  title("Delež uporabnikov interneta v letu 2012", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  plot(svet, col = barvegdp12)
  legend("bottom", kateg2012, fill = bar,
         border = "black", cex=.7, ncol = 4, title = "Gdp pc v US$", xjust=1)
  title("Višina gdp pc v letu 2012", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  ani.record() 
  par(mfrow=c(1,2))
  plot(svet, col = barve13)
  legend("bottom", kategorije, fill = vektor,
         border = "black", cex = .75 , ncol = 4, xjust = 1)
  title("Delež uporabnikov interneta v letu 2013", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  plot(svet, col = barvegdp13)
  legend("bottom", kateg2013, fill = bar,
         border = "black", cex=.7, ncol = 4, title = "Gdp pc v US$", xjust=1)
  title("Višina gdp pc v letu 2013", 
        cex.main = 1.5,   font.main = 2.5, col.main = "black")
  ani.record() 
  
},img.name = "spreminjanje.uporabnikov.interneta.in.gdp", htmlfile = "spreminjanje.uporabnikov.interneta.in.gdp.html",
ani.height = 680, ani.width = 900, interval = 1,
title = "Spreminjanje deleza uporabnikov interneta v primerjavi s spreminjanjem gdp pc med leti 2000-2013.", 
description = c("Animacija spreminjanja deleza uporabnikov interneta v primerjavi s spreminjanjem gdp pc med leti 2000-2013.")
)
ani.options(oopts2)

#pdf verzija2, ki jo bom vkljucila v porocilo, vendar pri meni ne dela
cairo_pdf("slike/animacija2.pdf", width = 7, height = 4, family = "Arial", onefile = TRUE)
par(mfrow=c(1,2))
plot(svet, col = barve00)
legend("bottom", kategorije, fill = vektor,
       border = "black", cex = .42 , ncol = 4, xjust = 1)
title("Delež uporabnikov interneta v letu 2000", 
      cex.main = 0.6,   font.main = 2.5, col.main = "black")
plot(svet, col = barvegdp00)
legend("bottom", kateg2000, fill = bar,
       border = "black", cex=.32, ncol = 4, title = "Gdp pc v US$", xjust=1)
title("Višina gdp pc v letu 2000", 
      cex.main = 0.6,   font.main = 2.5, col.main = "black")
 
par(mfrow=c(1,2))
plot(svet, col = barve01)
legend("bottom", kategorije, fill = vektor,
       border = "black", cex = .42 , ncol = 4, xjust = 1)
title("Delež uporabnikov interneta v letu 2001", 
      cex.main = .6,   font.main = 2.5, col.main = "black")
plot(svet, col = barvegdp01)
legend("bottom", kateg2001, fill = bar,
       border = "black", cex=.32, ncol = 4, title = "Gdp pc v US$", xjust=1)
title("Višina gdp pc v letu 2001", 
      cex.main = .6,   font.main = 2.5, col.main = "black")

par(mfrow=c(1,2))
plot(svet, col = barve02)
legend("bottom", kategorije, fill = vektor,
       border = "black", cex = .42 , ncol = 4, xjust = 1)
title("Delež uporabnikov interneta v letu 2002", 
      cex.main = .6,   font.main = 2.5, col.main = "black")
plot(svet, col = barvegdp02)
legend("bottom", kateg2002, fill = bar,
       border = "black", cex=.32, ncol = 4, title = "Gdp pc v US$", xjust=1)
title("Višina gdp pc v letu 2002", 
      cex.main = .6,   font.main = 2.5, col.main = "black")

par(mfrow=c(1,2))
plot(svet, col = barve03)
legend("bottom", kategorije, fill = vektor,
       border = "black", cex = .42 , ncol = 4, xjust = 1)
title("Delež uporabnikov interneta v letu 2003", 
      cex.main = .6,   font.main = 2.5, col.main = "black")
plot(svet, col = barvegdp03)
legend("bottom", kateg2003, fill = bar,
       border = "black", cex=.32, ncol = 4, title = "Gdp pc v US$", xjust=1)
title("Višina gdp pc v letu 2003", 
      cex.main = .6,   font.main = 2.5, col.main = "black")

par(mfrow=c(1,2))
plot(svet, col = barve04)
legend("bottom", kategorije, fill = vektor,
       border = "black", cex = .42 , ncol = 4, xjust = 1)
title("Delež uporabnikov interneta v letu 2004", 
      cex.main = .6,   font.main = 2.5, col.main = "black")
plot(svet, col = barvegdp04)
legend("bottom", kateg2004, fill = bar,
       border = "black", cex=.32, ncol = 4, title = "Gdp pc v US$", xjust=1)
title("Višina gdp pc v letu 2004", 
      cex.main = .6,   font.main = 2.5, col.main = "black")

par(mfrow=c(1,2))
plot(svet, col = barve05)
legend("bottom", kategorije, fill = vektor,
       border = "black", cex = .42 , ncol = 4, xjust = 1)
title("Delež uporabnikov interneta v letu 2005", 
      cex.main = .6,   font.main = 2.5, col.main = "black")
plot(svet, col = barvegdp05)
legend("bottom", kateg2005, fill = bar,
       border = "black", cex=.32, ncol = 4, title = "Gdp pc v US$", xjust=1)
title("Višina gdp pc v letu 2005", 
      cex.main = .6,   font.main = 2.5, col.main = "black")

par(mfrow=c(1,2))
plot(svet, col = barve06)
legend("bottom", kategorije, fill = vektor,
       border = "black", cex = .42 , ncol = 4, xjust = 1)
title("Delež uporabnikov interneta v letu 2006", 
      cex.main = .6,   font.main = 2.5, col.main = "black")
plot(svet, col = barvegdp06)
legend("bottom", kateg2006, fill = bar,
       border = "black", cex=.32, ncol = 4, title = "Gdp pc v US$", xjust=1)
title("Višina gdp pc v letu 2006", 
      cex.main = .6,   font.main = 2.5, col.main = "black")

par(mfrow=c(1,2))
plot(svet, col = barve07)
legend("bottom", kategorije, fill = vektor,
       border = "black", cex = .42 , ncol = 4, xjust = 1)
title("Delež uporabnikov interneta v letu 2007", 
      cex.main = .6,   font.main = 2.5, col.main = "black")
plot(svet, col = barvegdp07)
legend("bottom", kateg2007, fill = bar,
       border = "black", cex=.32, ncol = 4, title = "Gdp pc v US$", xjust=1)
title("Višina gdp pc v letu 2007", 
      cex.main = .6,   font.main = 2.5, col.main = "black")

par(mfrow=c(1,2))
plot(svet, col = barve08)
legend("bottom", kategorije, fill = vektor,
       border = "black", cex = .42 , ncol = 4, xjust = 1)
title("Delež uporabnikov interneta v letu 2008", 
      cex.main = .6,   font.main = 2.5, col.main = "black")
plot(svet, col = barvegdp08)
legend("bottom", kateg2008, fill = bar,
       border = "black", cex=.32, ncol = 4, title = "Gdp pc v US$", xjust=1)
title("Višina gdp pc v letu 2008", 
      cex.main = .6,   font.main = 2.5, col.main = "black")

par(mfrow=c(1,2))
plot(svet, col = barve09)
legend("bottom", kategorije, fill = vektor,
       border = "black", cex = .42 , ncol = 4, xjust = 1)
title("Delež uporabnikov interneta v letu 2009", 
      cex.main = .6,   font.main = 2.5, col.main = "black")
plot(svet, col = barvegdp09)
legend("bottom", kateg2009, fill = bar,
       border = "black", cex=.32, ncol = 4, title = "Gdp pc v US$", xjust=1)
title("Višina gdp pc v letu 2009", 
      cex.main = .6,   font.main = 2.5, col.main = "black")

par(mfrow=c(1,2))
plot(svet, col = barve10)
legend("bottom", kategorije, fill = vektor,
       border = "black", cex = .42 , ncol = 4, xjust = 1)
title("Delež uporabnikov interneta v letu 2010", 
      cex.main = .6,   font.main = 2.5, col.main = "black")
plot(svet, col = barvegdp10)
legend("bottom", kateg2010, fill = bar,
       border = "black", cex=.32, ncol = 4, title = "Gdp pc v US$", xjust=1)
title("Višina gdp pc v letu 2010", 
      cex.main = .6,   font.main = 2.5, col.main = "black")

par(mfrow=c(1,2))
plot(svet, col = barve11)
legend("bottom", kategorije, fill = vektor,
       border = "black", cex = .42 , ncol = 4, xjust = 1)
title("Delež uporabnikov interneta v letu 2011", 
      cex.main = .6,   font.main = 2.5, col.main = "black")
plot(svet, col = barvegdp11)
legend("bottom", kateg2011, fill = bar,
       border = "black", cex=.32, ncol = 4, title = "Gdp pc v US$", xjust=1)
title("Višina gdp pc v letu 2011", 
      cex.main = .6,   font.main = 2.5, col.main = "black")

par(mfrow=c(1,2))
plot(svet, col = barve12)
legend("bottom", kategorije, fill = vektor,
       border = "black", cex = .42 , ncol = 4, xjust = 1)
title("Delež uporabnikov interneta v letu 2012", 
      cex.main = .6,   font.main = 2.5, col.main = "black")
plot(svet, col = barvegdp12)
legend("bottom", kateg2012, fill = bar,
       border = "black", cex=.32, ncol = 4, title = "Gdp pc v US$", xjust=1)
title("Višina gdp pc v letu 2012", 
      cex.main = .6,   font.main = 2.5, col.main = "black")

par(mfrow=c(1,2))
plot(svet, col = barve13)
legend("bottom", kategorije, fill = vektor,
       border = "black", cex = .42 , ncol = 4, xjust = 1)
title("Delež uporabnikov interneta v letu 2013", 
      cex.main = .6,   font.main = 2.5, col.main = "black")
plot(svet, col = barvegdp13)
legend("bottom", kateg2013, fill = bar,
       border = "black", cex=.32, ncol = 4, title = "Gdp pc v US$", xjust=1)
title("Višina gdp pc v letu 2013", 
      cex.main = .6,   font.main = 2.5, col.main = "black")

dev.off()