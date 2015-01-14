# program, ki izrise grafe
source("uvoz/uvoz.r")

# iz t1 in t2 bom naredila graf po posameznih letih v koliko drzavah je med 0-5%, 5-10%...95-100% uporabnikov

graf_1 <- function(){
  attach(t1)
  attach(t2)
  kategorije <- c("0-10 %", "10-20 %", "20-30 %", "30-40 %", "40-50 %", 
                  "50-60 %", "60-70 %", "70-80 %", "80-90 %", "90-100 %")
  #vrstice, ki ustrezajo pogoju
  k1_2000 <- which(as.numeric(t1["2000"]) <= 10)
  k2_2000 <- which(as.numeric(t1["2000"]) <= 20 & as.numeric(t1["2000"])  > 10 )
  k3_2000 <- which(as.numeric(t1["2000"]) <= 30 & as.numeric(t1["2000"])  > 20 )
  k4_2000 <- which(as.numeric(t1["2000"] )<= 40 & as.numeric(t1["2000"] ) > 30 )
  k5_2000 <- which(as.numeric(t1["2000"] )<= 50 & as.numeric(t1["2000"]  )> 40 )
  k6_2000 <- which(as.numeric(t1["2000"] )<= 60 & as.numeric(t1["2000"] ) > 50 )
  k7_2000 <- which(as.numeric(t1["2000"]) <= 70 & as.numeric(t1["2000"] ) > 60 )
  k8_2000 <- which(as.numeric(t1["2000"]) <= 80 & as.numeric(t1["2000"]  )> 70 )
  k9_2000 <- which(as.numeric(t1["2000"]) <= 90 & as.numeric(t1["2000"] ) > 80 )
  k10_2000 <- which(as.numeric(t1["2000"]) > 90 )
  k1_2003 <- which(as.numeric(t1["2003"]) <= 10)
  k2_2003 <- which(as.numeric(t1["2003"] )<= 20 & as.numeric(t1["2003"] ) > 10 )
  k3_2003 <- which(as.numeric(t1["2003"] )<= 30 & as.numeric(t1["2003"] ) > 20 )
  k4_2003 <- which(as.numeric(t1["2003"] )<= 40 & as.numeric(t1["2003"] ) > 30 )
  k5_2003 <- which(as.numeric(t1["2003"] )<= 50 & as.numeric(t1["2003"] ) > 40 )
  k6_2003 <- which(as.numeric(t1["2003"] )<= 60 & as.numeric(t1["2003"] ) > 50 )
  k7_2003 <- which(as.numeric(t1["2003"] )<= 70 & as.numeric(t1["2003"] ) > 60 )
  k8_2003 <- which(as.numeric(t1["2003"] )<= 80 & as.numeric(t1["2003"] ) > 70 )
  k9_2003 <- which(as.numeric(t1["2003"] )<= 90 & as.numeric(t1["2003"] ) > 80 )
  k10_2003 <- which(as.numeric(t1["2003"]) > 90)
  k1_2006 <- which(as.numeric(t1["2006"] )<= 10)
  k2_2006 <- which(as.numeric(t1["2006"] )<= 20 & as.numeric(t1["2006"] ) > 10 )
  k3_2006 <- which(as.numeric(t1["2006"]) <= 30 & as.numeric(t1["2006"] ) > 20 )
  k4_2006 <- which(as.numeric(t1["2006"]) <= 40 & as.numeric(t1["2006"] ) > 30 )
  k5_2006 <- which(as.numeric(t1["2006"]) <= 50 & as.numeric(t1["2006"] ) > 40 )
  k6_2006 <- which(as.numeric(t1["2006"]) <= 60 & as.numeric(t1["2006"] ) > 50 )
  k7_2006 <- which(as.numeric(t1["2006"]) <= 70 & as.numeric(t1["2006"] ) > 60 )
  k8_2006 <- which(as.numeric(t1["2006"]) <= 80 & as.numeric(t1["2006"] ) > 70 )
  k9_2006 <- which(as.numeric(t1["2006"]) <= 90 & as.numeric(t1["2006"] ) > 80 )
  k10_2006 <- which(as.numeric(t1["2006"]) > 90 )
  k1_2009 <- which(as.numeric(t1["2009"] )<= 10)
  k2_2009 <- which(as.numeric(t1["2009"] )<= 20 & as.numeric(t1["2009"] ) > 10 )
  k3_2009 <- which(as.numeric(t1["2009"] )<= 30 & as.numeric(t1["2009"] ) > 20 )
  k4_2009 <- which(as.numeric(t1["2009"] )<= 40 & as.numeric(t1["2009"] ) > 30 )
  k5_2009 <- which(as.numeric(t1["2009"] )<= 50 & as.numeric(t1["2009"] ) > 40 )
  k6_2009 <- which(as.numeric(t1["2009"] )<= 60 & as.numeric(t1["2009"] ) > 50 )
  k7_2009 <- which(as.numeric(t1["2009"] )<= 70 & as.numeric(t1["2009"] ) > 60 )
  k8_2009 <- which(as.numeric(t1["2009"] )<= 80 & as.numeric(t1["2009"] ) > 70 )
  k9_2009 <- which(as.numeric(t1["2009"] )<= 90 & as.numeric(t1["2009"] ) > 80 )
  k10_2009 <- which(as.numeric(t1["2009"]) > 90)
  k1_2012 <- which(as.numeric(t1["2012"] )<= 10)
  k2_2012 <- which(as.numeric(t1["2012"] )<= 20 & as.numeric(t1["2012"] ) > 10 )
  k3_2012 <- which(as.numeric(t1["2012"] )<= 30 & as.numeric(t1["2012"] ) > 20 )
  k4_2012 <- which(as.numeric(t1["2012"] )<= 40 & as.numeric(t1["2012"] ) > 30 )
  k5_2012 <- which(as.numeric(t1["2012"] )<= 50 & as.numeric(t1["2012"] ) > 40 )
  k6_2012 <- which(as.numeric(t1["2012"] )<= 60 & as.numeric(t1["2012"] ) > 50 )
  k7_2012 <- which(as.numeric(t1["2012"] )<= 70 & as.numeric(t1["2012"] ) > 60 )
  k8_2012 <- which(as.numeric(t1["2012"] )<= 80 & as.numeric(t1["2012"] ) > 70 )
  k9_2012 <- which(as.numeric(t1["2012"] )<= 90 & as.numeric(t1["2012"] ) > 80 )
  k10_2012 <- which(as.numeric(t1["2012"]) > 90  )
  k1_2014 <- which(as.numeric(t2[, 7]) <= 10)
  k2_2014 <- which(as.numeric(t2[,7] )<= 20 & as.numeric(t2[,7] ) > 10 )
  k3_2014 <- which(as.numeric(t2[,7] )<= 30 & as.numeric(t2[,7] )> 20 )
  k4_2014 <- which(as.numeric(t2[,7] )<= 40 & as.numeric(t2[,7] )> 30 )
  k5_2014 <- which(as.numeric(t2[,7] )<= 50 & as.numeric(t2[,7] ) > 40 )
  k6_2014 <- which(as.numeric(t2[,7] )<= 60 & as.numeric(t2[,7] ) > 50 )
  k7_2014 <- which(as.numeric(t2[,7] )<= 70 & as.numeric(t2[,7] ) > 60 )
  k8_2014 <- which(as.numeric(t2[,7] )<= 80 & as.numeric(t2[,7] ) > 70 )
  k9_2014 <- which(as.numeric(t2[,7] )<= 90 & as.numeric(t2[,7] )> 80 )
  k10_2014 <- which(as.numeric(t2[,7]) > 90 )
  
  l2000 <- c(length(k1_2000),length(k2_2000),length(k3_2000),length(k4_2000),
             length(k5_2000),length(k6_2000),length(k7_2000),length(k8_2000),
             length(k9_2000),length(k10_2000))
  l2003 <- c(length(k1_2003),length(k2_2003),length(k3_2003),length(k4_2003),
             length(k5_2003),length(k6_2003),length(k7_2003),length(k8_2003),
             length(k9_2003),length(k10_2003))
  l2006 <- c(length(k1_2006),length(k2_2006),length(k3_2006),length(k4_2006),
             length(k5_2006),length(k6_2006),length(k7_2006),length(k8_2006),
             length(k9_2006),length(k10_2006))
  l2009 <- c(length(k1_2009),length(k2_2009),length(k3_2009),length(k4_2009),
             length(k5_2009),length(k6_2009),length(k7_2009),length(k8_2009),
             length(k9_2009),length(k10_2009))
  l2012 <- c(length(k1_2012),length(k2_2012),length(k3_2012),length(k4_2012),
             length(k5_2012),length(k6_2012),length(k7_2012),length(k8_2012),
             length(k9_2012),length(k10_2012))
  l2014 <- c(length(k1_2014),length(k2_2014),length(k3_2014),length(k4_2014),
             length(k5_2014),length(k6_2014),length(k7_2014),length(k8_2014),
             length(k9_2014),length(k10_2014))
  
  
  
  
  detach(t1)
  detach(t2)
}

zagraf<-data.frame(
                   k1=c(length(k1_2000),length(k1_2003),length(k1_2006),length(k1_2009),length(k1_2012),length(k1_2014)),
                   k2=c(length(k2_2000),length(k2_2003),length(k2_2006),length(k2_2009),length(k2_2012),length(k2_2014)),
                   k3=c(length(k3_2000),length(k3_2003),length(k3_2006),length(k3_2009),length(k3_2012),length(k3_2014)),
                   k4=c(length(k4_2000),length(k4_2003),length(k4_2006),length(k4_2009),length(k4_2012),length(k4_2014)),
                   k5=c(length(k5_2000),length(k5_2003),length(k5_2006),length(k5_2009),length(k5_2012),length(k5_2014)),
                   k6=c(length(k6_2000),length(k6_2003),length(k6_2006),length(k6_2009),length(k6_2012),length(k6_2014)),
                   k7=c(length(k7_2000),length(k7_2003),length(k7_2006),length(k7_2009),length(k7_2012),length(k7_2014)),
                   k8=c(length(k8_2000),length(k8_2003),length(k8_2006),length(k8_2009),length(k8_2012),length(k8_2014)),
                   k9=c(length(k9_2000),length(k9_2003),length(k9_2006),length(k9_2009),length(k9_2012),length(k9_2014)),
                   k10=c(length(k10_2000),length(k10_2003),length(k10_2006),length(k10_2009),length(k10_2012),length(k10_2014)))

rownames(zagraf)<-c("2000","2003","2006","2009","2012","2014")
colnames(zagraf)<-c("0-10 %","10-20 %","20-30 %" ,"30-40 %" ,"40-50 %" ,"50-60 %" ,"60-70 %" ,"70-80 %" ,"80-90 %" ,"90-100 %")

                   
                   

