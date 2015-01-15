# program, ki izrise grafe
source("uvoz/uvoz.r")

# iz t7 bom naredila graf po posameznih letih v koliko drzavah je med 0-5%, 5-10%...95-100% uporabnikov

#graf iy četrte tabele-kontinenti
d<-data.frame(t4)
attach(d)
pie(d[1:7,5])

detach(d)

                   
                   

