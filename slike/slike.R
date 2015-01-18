# program, ki izrise grafe
library(zoo)
library(spacetime)
library(ggplot2)
library(plotrix)

# 

# 1 graf prikazuje prvih 10 drzav iz 2. tabele...zanimivo: Kitajci predstavljajo vec kot 20 % vseh uporabnikov interneta,
# medtem, ko jih ima manj kot 50% dostop do interneta

prvi_graf <- function() {
  prvih10 <- data.frame(t2[1:10,])
  attach(prvih10)
  barplot(apply(as.matrix(prvih10[,c(7,9)]),1,c), beside=TRUE,xlab="Countries", width=1, 
          names.arg=row.names(prvih10), ylim=c(0,100), ylab="%", cex.names=0.6, las=2,
          main="Penetration of Population with Internet vs.\n Country's share of World Internet Users", col=c("grey","grey1"))

  detach(prvih10)
  
}

drugi_graf <- function(){
  barplot(as.matrix(t7), beside = TRUE, ylim=c(0,200), col=c(brewer.pal(9, "Blues"),"black"), 
          main="Stevilo drzav razvrscenih v skupine po stevilu uporabnikov interneta\n po posameznih letih",names.arg = substr(names(t7), 2, 5))
  legend("topright", 
         legend = c("0-10 %", "10-20 %", "20-30 %", "30-40 %", "40-50 %", "50-60 %", "60-70 %", "70-80 %", "80-90%", "90-100%"), 
         fill = rainbow(10), cex=0.5)
  
}




# graf iz 3 tabele-narascanje stevila uporabnikov...
tretji_graf <- function(){
  dr<-data.frame(t3)
  print(qplot(rownames(dr),dr$Penetration....of.population.with.Internet.,
              data=dr, ylab=NULL, xlab=NULL, 
              main="Penetration (% of population with Internet)")
        + theme(axis.text.x = element_text(size = 5)))
  
}

# pita iz t4
cetrti_graf <- function(){
  pie3D(t4[1:7,5], labels=rownames(t4)[1:7], radius=0.9, explode=0.2, main="Delez uporabnikov po geografskih regijah",
        col=brewer.pal(7, "Blues"), labelcex=0.8, start=pi/2)
}


pdf("slike/graf1.pdf")
prvi_graf()
dev.off()

pdf("slike/graf2.pdf")
drugi_graf()
dev.off()

pdf("slike/graf3.pdf")
tretji_graf()
dev.off()   

pdf("slike/graf4.pdf")
cetrti_graf()
dev.off()   

#decompose...se bom uporabila na kakem grafu...
                   

