# program, ki izrise grafe
source("uvoz/uvoz.r")
library(ggplot2)

# iz t7 bom naredila graf po posameznih letih v koliko drzavah je med 0-5%, 5-10%...95-100% uporabnikov

# 1 graf prikazuje prvih 10 drzav iz 2. tabele...zanimivo: Kitajci predstavljajo vec kot 20 % vseh uporabnikov interneta,
# medtem, ko jih ima manj kot 50% dostop do interneta

prvih10 <- data.frame(t2[1:10,])
attach(prvih10)
barplot(Penetration....of.Pop..with.Internet., xlab="Drzave")
axis(1, at=1:10, lab= row.names(prvih10))
barplot(Country.s.share.of.World.Internet.Users, xlab="Drzave")
axis(1, at=1:10, lab= row.names(prvih10))

pie(table(t7$X2014))

# graf iz 3 tabele
dr<-data.frame(t3)
qplot(rownames(dr),dr$Penetration....of.population.with.Internet. , data=dr, geom=c('point', 'smooth'))


                   
                   

