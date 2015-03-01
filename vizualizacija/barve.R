# Vektorji barv, da lahko ustrezno pobarvam zemljevide
# 1.) Barve glede na uporabnike interneta
vektor <- c(brewer.pal(9, "Blues"), "black")
vektor2 <- brewer.pal(8, "BuPu")
barve00 <- ifelse(is.na(svet$X2000), "white", "black")
barve00 <- vektor[floor(svet$X2000/10) + 1]
barve01 <- ifelse(is.na(svet$X2001), "white", "black")
barve01 <- vektor[floor(svet$X2001/10) + 1]
barve02 <- ifelse(is.na(svet$X2002), "white", "black")
barve02 <- vektor[floor(svet$X2002/10) + 1]
barve03 <- ifelse(is.na(svet$X2003), "white", "black")
barve03 <- vektor[floor(svet$X2003/10) + 1]
barve04 <- ifelse(is.na(svet$X2004), "white", "black")
barve04 <- vektor[floor(svet$X2004/10) + 1]
barve05 <- ifelse(is.na(svet$X2005), "white", "black")
barve05 <- vektor[floor(svet$X2005/10) + 1]
barve06 <- ifelse(is.na(svet$X2006), "white", "black")
barve06 <- vektor[floor(svet$X2006/10) + 1]
barve07 <- ifelse(is.na(svet$X2007), "white", "black")
barve07 <- vektor[floor(svet$X2007/10) + 1]
barve08 <- ifelse(is.na(svet$X2008), "white", "black")
barve08 <- vektor[floor(svet$X2008/10) + 1]
barve09 <- ifelse(is.na(svet$X2009), "white", "black")
barve09 <- vektor[floor(svet$X2009/10) + 1]
barve10 <- ifelse(is.na(svet$X2010), "white", "black")
barve10 <- vektor[floor(svet$X2010/10) + 1]
barve11 <- ifelse(is.na(svet$X2011), "white", "black")
barve11 <- vektor[floor(svet$X2011/10) + 1]
barve12 <- ifelse(is.na(svet$X2012), "white", "black")
barve12 <- vektor[floor(svet$X2012/10) + 1]
barve13 <- ifelse(is.na(svet$X2013), "white", "black")
barve13 <- vektor[floor(svet$X2013/10) + 1]
barve14 <- ifelse(is.na(svet$X2014), "white", "black")
barve14 <- vektor[floor(svet$X2014/10) + 1]

# 2.) Barve glede na pricakovano zivljenjsko dobo v letu 2011
barve_leta <- ifelse(is.na(svet$leta), "white", "black")
barve_leta[which(svet$leta >= 45 & svet$leta < 50)] <- vektor2[1]
barve_leta[which(svet$leta >= 50 & svet$leta < 55)] <- vektor2[2]
barve_leta[which(svet$leta >= 55 & svet$leta < 60)] <- vektor2[3]
barve_leta[which(svet$leta >= 60 & svet$leta < 65)] <- vektor2[4]
barve_leta[which(svet$leta >= 65 & svet$leta < 70)] <- vektor2[5]
barve_leta[which(svet$leta >= 70 & svet$leta < 75)] <- vektor2[6]
barve_leta[which(svet$leta >= 75 & svet$leta < 80)] <- vektor2[7]
barve_leta[which(svet$leta >= 80)] <- vektor2[8]
kategorije <- c("0-10 %", "10-20 %", "20-30 %", "30-40 %", "40-50 %", 
                "50-60 %", "60-70 %", "70-80 %", "80-90 %", "90-100 %")

# 3.) Barve glede na gdp pc med leti 2000-2011
kateg00 <- (max(svet$gdp2000, na.rm=TRUE)-min(svet$gdp2000, na.rm=TRUE))/10
bar <- c(brewer.pal(9,"Greens"), "darkgreen")
barvegdp00 <- ifelse(is.na(svet$gdp2000), "white", "black")
barvegdp00[which(svet$gdp2000 < kateg00/4)] <- bar[1]
barvegdp00[which(svet$gdp2000 >= kateg00/4 & svet$gdp2000 < kateg00)] <- bar[2]
barvegdp00[which(svet$gdp2000 >= kateg00 & svet$gdp2000 < 2*kateg00)] <- bar[3]
barvegdp00[which(svet$gdp2000 >= 2*kateg00 & svet$gdp2000 < 3*kateg00)] <- bar[4]
barvegdp00[which(svet$gdp2000 >= 3*kateg00 & svet$gdp2000 < 4*kateg00)] <- bar[5]
barvegdp00[which(svet$gdp2000 >= 4*kateg00 & svet$gdp2000 < 5*kateg00)] <- bar[6]
barvegdp00[which(svet$gdp2000 >= 5*kateg00 & svet$gdp2000 < 6*kateg00)] <- bar[7]
barvegdp00[which(svet$gdp2000 >= 6*kateg00 & svet$gdp2000 < 7*kateg00)] <- bar[8]
barvegdp00[which(svet$gdp2000 >= 7*kateg00 & svet$gdp2000 < 8*kateg00)] <- bar[9]
barvegdp00[which(svet$gdp2000 >= 8*kateg00)] <- bar[10]
kateg01 <- (max(svet$gdp2001, na.rm=TRUE)-min(svet$gdp2001, na.rm=TRUE))/10
barvegdp01 <- ifelse(is.na(svet$gdp2001), "white", "black")
barvegdp01[which(svet$gdp2001 < kateg01/4)] <- bar[1]
barvegdp01[which(svet$gdp2001 >= kateg01/4 & svet$gdp2001 < kateg01)] <- bar[2]
barvegdp01[which(svet$gdp2001 >= kateg01 & svet$gdp2001 < 2*kateg01)] <- bar[3]
barvegdp01[which(svet$gdp2001 >= 2*kateg01 & svet$gdp2001 < 3*kateg01)] <- bar[4]
barvegdp01[which(svet$gdp2001 >= 3*kateg01 & svet$gdp2001 < 4*kateg01)] <- bar[5]
barvegdp01[which(svet$gdp2001 >= 4*kateg01 & svet$gdp2001 < 5*kateg01)] <- bar[6]
barvegdp01[which(svet$gdp2001 >= 5*kateg01 & svet$gdp2001 < 6*kateg01)] <- bar[7]
barvegdp01[which(svet$gdp2001 >= 6*kateg01 & svet$gdp2001 < 7*kateg01)] <- bar[8]
barvegdp01[which(svet$gdp2001 >= 7*kateg01 & svet$gdp2001 < 8*kateg01)] <- bar[9]
barvegdp01[which(svet$gdp2001 >= 8*kateg01)] <- bar[10]
kateg02 <- (max(svet$gdp2002, na.rm=TRUE)-min(svet$gdp2002, na.rm=TRUE))/10
barvegdp02 <- ifelse(is.na(svet$gdp2002), "white", "black")
barvegdp02[which(svet$gdp2002 < kateg02/4)] <- bar[1]
barvegdp02[which(svet$gdp2002 >= kateg02/4 & svet$gdp2002 < kateg02)] <- bar[2]
barvegdp02[which(svet$gdp2002 >= kateg02 & svet$gdp2002 < 2*kateg02)] <- bar[3]
barvegdp02[which(svet$gdp2002 >= 2*kateg02 & svet$gdp2002 < 3*kateg02)] <- bar[4]
barvegdp02[which(svet$gdp2002 >= 3*kateg02 & svet$gdp2002 < 4*kateg02)] <- bar[5]
barvegdp02[which(svet$gdp2002 >= 4*kateg02 & svet$gdp2002 < 5*kateg02)] <- bar[6]
barvegdp02[which(svet$gdp2002 >= 5*kateg02 & svet$gdp2002 < 6*kateg02)] <- bar[7]
barvegdp02[which(svet$gdp2002 >= 6*kateg02 & svet$gdp2002 < 7*kateg02)] <- bar[8]
barvegdp02[which(svet$gdp2002 >= 7*kateg02 & svet$gdp2002 < 8*kateg02)] <- bar[9]
barvegdp02[which(svet$gdp2002 >= 8*kateg02)] <- bar[10]
kateg03 <- (max(svet$gdp2003, na.rm=TRUE)-min(svet$gdp2003, na.rm=TRUE))/10
barvegdp03 <- ifelse(is.na(svet$gdp2003), "white", "black")
barvegdp03[which(svet$gdp2003 < kateg03/4)] <- bar[1]
barvegdp03[which(svet$gdp2003 >= kateg03/4 & svet$gdp2003 < kateg03)] <- bar[2]
barvegdp03[which(svet$gdp2003 >= kateg03 & svet$gdp2003 < 2*kateg03)] <- bar[3]
barvegdp03[which(svet$gdp2003 >= 2*kateg03 & svet$gdp2003 < 3*kateg03)] <- bar[4]
barvegdp03[which(svet$gdp2003 >= 3*kateg03 & svet$gdp2003 < 4*kateg03)] <- bar[5]
barvegdp03[which(svet$gdp2003 >= 4*kateg03 & svet$gdp2003 < 5*kateg03)] <- bar[6]
barvegdp03[which(svet$gdp2003 >= 5*kateg03 & svet$gdp2003 < 6*kateg03)] <- bar[7]
barvegdp03[which(svet$gdp2003 >= 6*kateg03 & svet$gdp2003 < 7*kateg03)] <- bar[8]
barvegdp03[which(svet$gdp2003 >= 7*kateg03 & svet$gdp2003 < 8*kateg03)] <- bar[9]
barvegdp03[which(svet$gdp2003 >= 8*kateg03)] <- bar[10]
kateg04 <- (max(svet$gdp2004, na.rm=TRUE)-min(svet$gdp2004, na.rm=TRUE))/10
barvegdp04 <- ifelse(is.na(svet$gdp2004), "white", "black")
barvegdp04[which(svet$gdp2004 < kateg04/4)] <- bar[1]
barvegdp04[which(svet$gdp2004 >= kateg04/4 & svet$gdp2004 < kateg04)] <- bar[2]
barvegdp04[which(svet$gdp2004 >= kateg04 & svet$gdp2004 < 2*kateg04)] <- bar[3]
barvegdp04[which(svet$gdp2004 >= 2*kateg04 & svet$gdp2004 < 3*kateg04)] <- bar[4]
barvegdp04[which(svet$gdp2004 >= 3*kateg04 & svet$gdp2004 < 4*kateg04)] <- bar[5]
barvegdp04[which(svet$gdp2004 >= 4*kateg04 & svet$gdp2004 < 5*kateg04)] <- bar[6]
barvegdp04[which(svet$gdp2004 >= 5*kateg04 & svet$gdp2004 < 6*kateg04)] <- bar[7]
barvegdp04[which(svet$gdp2004 >= 6*kateg04 & svet$gdp2004 < 7*kateg04)] <- bar[8]
barvegdp04[which(svet$gdp2004 >= 7*kateg04 & svet$gdp2004 < 8*kateg04)] <- bar[9]
barvegdp04[which(svet$gdp2004 >= 8*kateg04)] <- bar[10]
kateg05 <- (max(svet$gdp2005, na.rm=TRUE)-min(svet$gdp2005, na.rm=TRUE))/10
barvegdp05 <- ifelse(is.na(svet$gdp2005), "white", "black")
barvegdp05[which(svet$gdp2005 < kateg05/4)] <- bar[1]
barvegdp05[which(svet$gdp2005 >= kateg05/4 & svet$gdp2005 < kateg05)] <- bar[2]
barvegdp05[which(svet$gdp2005 >= kateg05 & svet$gdp2005 < 2*kateg05)] <- bar[3]
barvegdp05[which(svet$gdp2005 >= 2*kateg05 & svet$gdp2005 < 3*kateg05)] <- bar[4]
barvegdp05[which(svet$gdp2005 >= 3*kateg05 & svet$gdp2005 < 4*kateg05)] <- bar[5]
barvegdp05[which(svet$gdp2005 >= 4*kateg05 & svet$gdp2005 < 5*kateg05)] <- bar[6]
barvegdp05[which(svet$gdp2005 >= 5*kateg05 & svet$gdp2005 < 6*kateg05)] <- bar[7]
barvegdp05[which(svet$gdp2005 >= 6*kateg05 & svet$gdp2005 < 7*kateg05)] <- bar[8]
barvegdp05[which(svet$gdp2005 >= 7*kateg05 & svet$gdp2005 < 8*kateg05)] <- bar[9]
barvegdp05[which(svet$gdp2005 >= 8*kateg05)] <- bar[10]
kateg06 <- (max(svet$gdp2006, na.rm=TRUE)-min(svet$gdp2006, na.rm=TRUE))/10
barvegdp06 <- ifelse(is.na(svet$gdp2006), "white", "black")
barvegdp06[which(svet$gdp2006 < kateg06/4)] <- bar[1]
barvegdp06[which(svet$gdp2006 >= kateg06/4 & svet$gdp2006 < kateg06)] <- bar[2]
barvegdp06[which(svet$gdp2006 >= kateg06 & svet$gdp2006 < 2*kateg06)] <- bar[3]
barvegdp06[which(svet$gdp2006 >= 2*kateg06 & svet$gdp2006 < 3*kateg06)] <- bar[4]
barvegdp06[which(svet$gdp2006 >= 3*kateg06 & svet$gdp2006 < 4*kateg06)] <- bar[5]
barvegdp06[which(svet$gdp2006 >= 4*kateg06 & svet$gdp2006 < 5*kateg06)] <- bar[6]
barvegdp06[which(svet$gdp2006 >= 5*kateg06 & svet$gdp2006 < 6*kateg06)] <- bar[7]
barvegdp06[which(svet$gdp2006 >= 6*kateg06 & svet$gdp2006 < 7*kateg06)] <- bar[8]
barvegdp06[which(svet$gdp2006 >= 7*kateg06 & svet$gdp2006 < 8*kateg06)] <- bar[9]
barvegdp06[which(svet$gdp2006 >= 8*kateg06)] <- bar[10]
kateg07 <- (max(svet$gdp2007, na.rm=TRUE)-min(svet$gdp2007, na.rm=TRUE))/10
barvegdp07 <- ifelse(is.na(svet$gdp2007), "white", "black")
barvegdp07[which(svet$gdp2007 < kateg07/4)] <- bar[1]
barvegdp07[which(svet$gdp2007 >= kateg07/4 & svet$gdp2007 < kateg07)] <- bar[2]
barvegdp07[which(svet$gdp2007 >= kateg07 & svet$gdp2007 < 2*kateg07)] <- bar[3]
barvegdp07[which(svet$gdp2007 >= 2*kateg07 & svet$gdp2007 < 3*kateg07)] <- bar[4]
barvegdp07[which(svet$gdp2007 >= 3*kateg07 & svet$gdp2007 < 4*kateg07)] <- bar[5]
barvegdp07[which(svet$gdp2007 >= 4*kateg07 & svet$gdp2007 < 5*kateg07)] <- bar[6]
barvegdp07[which(svet$gdp2007 >= 5*kateg07 & svet$gdp2007 < 6*kateg07)] <- bar[7]
barvegdp07[which(svet$gdp2007 >= 6*kateg07 & svet$gdp2007 < 7*kateg07)] <- bar[8]
barvegdp07[which(svet$gdp2007 >= 7*kateg07 & svet$gdp2007 < 8*kateg07)] <- bar[9]
barvegdp07[which(svet$gdp2007 >= 8*kateg07)] <- bar[10]
kateg08 <- (max(svet$gdp2008, na.rm=TRUE)-min(svet$gdp2008, na.rm=TRUE))/10
barvegdp08 <- ifelse(is.na(svet$gdp2008), "white", "black")
barvegdp08[which(svet$gdp2008 < kateg08/4)] <- bar[1]
barvegdp08[which(svet$gdp2008 >= kateg08/4 & svet$gdp2008 < kateg08)] <- bar[2]
barvegdp08[which(svet$gdp2008 >= kateg08 & svet$gdp2008 < 2*kateg08)] <- bar[3]
barvegdp08[which(svet$gdp2008 >= 2*kateg08 & svet$gdp2008 < 3*kateg08)] <- bar[4]
barvegdp08[which(svet$gdp2008 >= 3*kateg08 & svet$gdp2008 < 4*kateg08)] <- bar[5]
barvegdp08[which(svet$gdp2008 >= 4*kateg08 & svet$gdp2008 < 5*kateg08)] <- bar[6]
barvegdp08[which(svet$gdp2008 >= 5*kateg08 & svet$gdp2008 < 6*kateg08)] <- bar[7]
barvegdp08[which(svet$gdp2008 >= 6*kateg08& svet$gdp2008< 7*kateg08)] <- bar[8]
barvegdp08[which(svet$gdp2008 >= 7*kateg08 & svet$gdp2008 < 8*kateg08)] <- bar[9]
barvegdp08[which(svet$gdp2008 >= 8*kateg08)] <- bar[10]
kateg09 <- (max(svet$gdp2009, na.rm=TRUE)-min(svet$gdp2009, na.rm=TRUE))/10
barvegdp09 <- ifelse(is.na(svet$gdp2009), "white", "black")
barvegdp09[which(svet$gdp2009 < kateg09/4)] <- bar[1]
barvegdp09[which(svet$gdp2009 >= kateg09/4 & svet$gdp2009 < kateg09)] <- bar[2]
barvegdp09[which(svet$gdp2009 >= kateg09 & svet$gdp2009 < 2*kateg09)] <- bar[3]
barvegdp09[which(svet$gdp2009 >= 2*kateg09 & svet$gdp2009 < 3*kateg09)] <- bar[4]
barvegdp09[which(svet$gdp2009 >= 3*kateg09 & svet$gdp2009 < 4*kateg09)] <- bar[5]
barvegdp09[which(svet$gdp2009 >= 4*kateg09 & svet$gdp2009 < 5*kateg09)] <- bar[6]
barvegdp09[which(svet$gdp2009 >= 5*kateg09 & svet$gdp2009 < 6*kateg09)] <- bar[7]
barvegdp09[which(svet$gdp2009 >= 6*kateg09 & svet$gdp2009 < 7*kateg09)] <- bar[8]
barvegdp09[which(svet$gdp2009 >= 7*kateg09 & svet$gdp2009 < 8*kateg09)] <- bar[9]
barvegdp09[which(svet$gdp2009 >= 8*kateg09)] <- bar[10]
kateg10 <- (max(svet$gdp2010, na.rm=TRUE)-min(svet$gdp2010, na.rm=TRUE))/10
barvegdp10 <- ifelse(is.na(svet$gdp2010), "white", "black")
barvegdp10[which(svet$gdp2010 < kateg10/4)] <- bar[1]
barvegdp10[which(svet$gdp2010 >= kateg10/4 & svet$gdp2010 < kateg10)] <- bar[2]
barvegdp10[which(svet$gdp2010 >= kateg10 & svet$gdp2010 < 2*kateg10)] <- bar[3]
barvegdp10[which(svet$gdp2010 >= 2*kateg10 & svet$gdp2010 < 3*kateg10)] <- bar[4]
barvegdp10[which(svet$gdp2010 >= 3*kateg10 & svet$gdp2010 < 4*kateg10)] <- bar[5]
barvegdp10[which(svet$gdp2010 >= 4*kateg10 & svet$gdp2010 < 5*kateg10)] <- bar[6]
barvegdp10[which(svet$gdp2010 >= 5*kateg10 & svet$gdp2010 < 6*kateg10)] <- bar[7]
barvegdp10[which(svet$gdp2010 >= 6*kateg10 & svet$gdp2010 < 7*kateg10)] <- bar[8]
barvegdp10[which(svet$gdp2010 >= 7*kateg10 & svet$gdp2010 < 8*kateg10)] <- bar[9]
barvegdp10[which(svet$gdp2010 >= 8*kateg10)] <- bar[10]
kateg11 <- (max(svet$gdp2011, na.rm=TRUE)-min(svet$gdp2011, na.rm=TRUE))/10
barvegdp11 <- ifelse(is.na(svet$gdp2011), "white", "black")
barvegdp11[which(svet$gdp2011 < kateg11/4)] <- bar[1]
barvegdp11[which(svet$gdp2011 >= kateg11/4 & svet$gdp2011 < kateg11)] <- bar[2]
barvegdp11[which(svet$gdp2011 >= kateg11 & svet$gdp2011 < 2*kateg11)] <- bar[3]
barvegdp11[which(svet$gdp2011 >= 2*kateg11 & svet$gdp2011 < 3*kateg11)] <- bar[4]
barvegdp11[which(svet$gdp2011 >= 3*kateg11 & svet$gdp2011 < 4*kateg11)] <- bar[5]
barvegdp11[which(svet$gdp2011 >= 4*kateg11 & svet$gdp2011 < 5*kateg11)] <- bar[6]
barvegdp11[which(svet$gdp2011 >= 5*kateg11 & svet$gdp2011 < 6*kateg11)] <- bar[7]
barvegdp11[which(svet$gdp2011 >= 6*kateg11 & svet$gdp2011 < 7*kateg11)] <- bar[8]
barvegdp11[which(svet$gdp2011 >= 7*kateg11 & svet$gdp2011 < 8*kateg11)] <- bar[9]
barvegdp11[which(svet$gdp2011 >= 8*kateg11)] <- bar[10]
kateg12 <- (max(svet$gdp2012, na.rm=TRUE)-min(svet$gdp2012, na.rm=TRUE))/10
barvegdp12 <- ifelse(is.na(svet$gdp2012), "white", "black")
barvegdp12[which(svet$gdp2012 < kateg12/4)] <- bar[1]
barvegdp12[which(svet$gdp2012 >= kateg12/4 & svet$gdp2012 < kateg12)] <- bar[2]
barvegdp12[which(svet$gdp2012 >= kateg12 & svet$gdp2012 < 2*kateg12)] <- bar[3]
barvegdp12[which(svet$gdp2012 >= 2*kateg12 & svet$gdp2012 < 3*kateg12)] <- bar[4]
barvegdp12[which(svet$gdp2012 >= 3*kateg12 & svet$gdp2012 < 4*kateg12)] <- bar[5]
barvegdp12[which(svet$gdp2012 >= 4*kateg12 & svet$gdp2012 < 5*kateg12)] <- bar[6]
barvegdp12[which(svet$gdp2012 >= 5*kateg12 & svet$gdp2012 < 6*kateg12)] <- bar[7]
barvegdp12[which(svet$gdp2012 >= 6*kateg12 & svet$gdp2012 < 7*kateg12)] <- bar[8]
barvegdp12[which(svet$gdp2012 >= 7*kateg12 & svet$gdp2012 < 8*kateg12)] <- bar[9]
barvegdp12[which(svet$gdp2012 >= 8*kateg12)] <- bar[10]
kateg13 <- (max(svet$gdp2013, na.rm=TRUE)-min(svet$gdp2013, na.rm=TRUE))/10
barvegdp13 <- ifelse(is.na(svet$gdp2013), "white", "black")
barvegdp13[which(svet$gdp2013 < kateg13/4)] <- bar[1]
barvegdp13[which(svet$gdp2013 >= kateg13/4 & svet$gdp2013 < kateg13)] <- bar[2]
barvegdp13[which(svet$gdp2013 >= kateg13 & svet$gdp2013 < 2*kateg13)] <- bar[3]
barvegdp13[which(svet$gdp2013 >= 2*kateg13 & svet$gdp2013 < 3*kateg13)] <- bar[4]
barvegdp13[which(svet$gdp2013 >= 3*kateg13 & svet$gdp2013 < 4*kateg13)] <- bar[5]
barvegdp13[which(svet$gdp2013 >= 4*kateg13 & svet$gdp2013 < 5*kateg13)] <- bar[6]
barvegdp13[which(svet$gdp2013 >= 5*kateg13 & svet$gdp2013 < 6*kateg13)] <- bar[7]
barvegdp13[which(svet$gdp2013 >= 6*kateg13 & svet$gdp2013 < 7*kateg13)] <- bar[8]
barvegdp13[which(svet$gdp2013 >= 7*kateg13 & svet$gdp2013 < 8*kateg13)] <- bar[9]
barvegdp13[which(svet$gdp2013 >= 8*kateg13)] <- bar[10]

# za legende pri gdp
kateg2000 <- c(paste0(0,"-",round(kateg00/4,1)), paste0(round(kateg00/4,1),"-", round(kateg00,1)),
               paste0(round(kateg00,1),"-", round(2*kateg00,1)),
               paste0(round(2*kateg00,1),"-", round(3*kateg00,1)),
               paste0(round(3*kateg00,1),"-", round(4*kateg00,1)),
               paste0(round(4*kateg00,1),"-", round(5*kateg00,1)),
               paste0(round(5*kateg00,1),"-", round(6*kateg00,1)),
               paste0(round(6*kateg00,1),"-", round(7*kateg00,1)),
               paste0(round(7*kateg00,1),"-", round(8*kateg00,1)),
               paste0(round(8*kateg00,1),"-", round(max(svet$gdp2000, na.rm=TRUE),1)))
kateg2001 <- c(paste0(0,"-",round(kateg01/4,1)), paste0(round(kateg01/4,1),"-", round(kateg01,1)),
               paste0(round(kateg01,1),"-", round(2*kateg01,1)),
               paste0(round(2*kateg01,1),"-", round(3*kateg01,1)),
               paste0(round(3*kateg01,1),"-", round(4*kateg01,1)),
               paste0(round(4*kateg01,1),"-", round(5*kateg01,1)),
               paste0(round(5*kateg01,1),"-", round(6*kateg01,1)),
               paste0(round(6*kateg01,1),"-", round(7*kateg01,1)),
               paste0(round(7*kateg01,1),"-", round(8*kateg01,1)),
               paste0(round(8*kateg01,1),"-", round(max(svet$gdp2001, na.rm=TRUE),1)))
kateg2002 <- c(paste0(0,"-",round(kateg02/4,1)), paste0(round(kateg02/4,1),"-", round(kateg02,1)),
               paste0(round(kateg02,1),"-", round(2*kateg02,1)),
               paste0(round(2*kateg02,1),"-", round(3*kateg02,1)),
               paste0(round(3*kateg02,1),"-", round(4*kateg02,1)),
               paste0(round(4*kateg02,1),"-", round(5*kateg02,1)),
               paste0(round(5*kateg02,1),"-", round(6*kateg02,1)),
               paste0(round(6*kateg02,1),"-", round(7*kateg02,1)),
               paste0(round(7*kateg02,1),"-", round(8*kateg02,1)),
               paste0(round(8*kateg02,1),"-", round(max(svet$gdp2002, na.rm=TRUE),1)))
kateg2003 <- c(paste0(0,"-",round(kateg03/4,1)), paste0(round(kateg03/4,1),"-", round(kateg03,1)),
               paste0(round(kateg03,1),"-", round(2*kateg03,1)),
               paste0(round(2*kateg03,1),"-", round(3*kateg03,1)),
               paste0(round(3*kateg03,1),"-", round(4*kateg03,1)),
               paste0(round(4*kateg03,1),"-", round(5*kateg03,1)),
               paste0(round(5*kateg03,1),"-", round(6*kateg03,1)),
               paste0(round(6*kateg03,1),"-", round(7*kateg03,1)),
               paste0(round(7*kateg03,1),"-", round(8*kateg03,1)),
               paste0(round(8*kateg03,1),"-", round(max(svet$gdp2003, na.rm=TRUE),1)))
kateg2004 <- c(paste0(0,"-",round(kateg04/4,1)), paste0(round(kateg04/4,1), "-", round(kateg04,1)),
               paste0(round(kateg04,1),"-", round(2*kateg04,1)),
               paste0(round(2*kateg04,1),"-", round(3*kateg04,1)),
               paste0(round(3*kateg04,1),"-", round(4*kateg04,1)),
               paste0(round(4*kateg04,1),"-", round(5*kateg04,1)),
               paste0(round(5*kateg04,1),"-", round(6*kateg04,1)),
               paste0(round(6*kateg04,1),"-", round(7*kateg04,1)),
               paste0(round(7*kateg04,1),"-", round(8*kateg04,1)),
               paste0(round(8*kateg04,1),"-", round(max(svet$gdp2004, na.rm=TRUE),1)))
kateg2005 <- c(paste0(0,"-",round(kateg05/4,1)), paste0(round(kateg05/4,1), "-", round(kateg05,1)),
               paste0(round(kateg05,1),"-", round(2*kateg05,1)),
               paste0(round(2*kateg05,1),"-", round(3*kateg05,1)),
               paste0(round(3*kateg05,1),"-", round(4*kateg05,1)),
               paste0(round(4*kateg05,1),"-", round(5*kateg05,1)),
               paste0(round(5*kateg05,1),"-", round(6*kateg05,1)),
               paste0(round(6*kateg05,1),"-", round(7*kateg05,1)),
               paste0(round(7*kateg05,1),"-", round(8*kateg05,1)),
               paste0(round(8*kateg05,1),"-", round(max(svet$gdp2005, na.rm=TRUE),1)))
kateg2006 <- c(paste0(0,"-",round(kateg06/4,1)), paste0(round(kateg06/4,1), "-", round(kateg06,1)),
               paste0(round(kateg06,1),"-", round(2*kateg06,1)),
               paste0(round(2*kateg06,1),"-", round(3*kateg06,1)),
               paste0(round(3*kateg06,1),"-", round(4*kateg06,1)),
               paste0(round(4*kateg06,1),"-", round(5*kateg06,1)),
               paste0(round(5*kateg06,1),"-", round(6*kateg06,1)),
               paste0(round(6*kateg06,1),"-", round(7*kateg06,1)),
               paste0(round(7*kateg06,1),"-", round(8*kateg06,1)),
               paste0(round(8*kateg06,1),"-", round(max(svet$gdp2006, na.rm=TRUE),1)))
kateg2007 <- c(paste0(0,"-",round(kateg07/4,1)), paste0(round(kateg07/4,1), "-", round(kateg07,1)),
               paste0(round(kateg07,1),"-", round(2*kateg07,1)),
               paste0(round(2*kateg07,1),"-", round(3*kateg07,1)),
               paste0(round(3*kateg07,1),"-", round(4*kateg07,1)),
               paste0(round(4*kateg07,1),"-", round(5*kateg07,1)),
               paste0(round(5*kateg07,1),"-", round(6*kateg07,1)),
               paste0(round(6*kateg07,1),"-", round(7*kateg07,1)),
               paste0(round(7*kateg07,1),"-", round(8*kateg07,1)),
               paste0(round(8*kateg07,1),"-", round(max(svet$gdp2007, na.rm=TRUE),1)))
kateg2008 <- c(paste0(0,"-",round(kateg08/4,1)), paste0(round(kateg08/4,1), "-", round(kateg08,1)),
               paste0(round(kateg08,1),"-", round(2*kateg08,1)),
               paste0(round(2*kateg08,1),"-", round(3*kateg08,1)),
               paste0(round(3*kateg08,1),"-", round(4*kateg08,1)),
               paste0(round(4*kateg08,1),"-", round(5*kateg08,1)),
               paste0(round(5*kateg08,1),"-", round(6*kateg08,1)),
               paste0(round(6*kateg08,1),"-", round(7*kateg08,1)),
               paste0(round(7*kateg08,1),"-", round(8*kateg08,1)),
               paste0(round(8*kateg08,1),"-", round(max(svet$gdp2008, na.rm=TRUE),1)))
kateg2009 <- c(paste0(0,"-",round(kateg09/4,1)), paste0(round(kateg09/4,1), "-", round(kateg09,1)),
               paste0(round(kateg09,1),"-", round(2*kateg09,1)),
               paste0(round(2*kateg09,1),"-", round(3*kateg09,1)),
               paste0(round(3*kateg09,1),"-", round(4*kateg09,1)),
               paste0(round(4*kateg09,1),"-", round(5*kateg09,1)),
               paste0(round(5*kateg09,1),"-", round(6*kateg09,1)),
               paste0(round(6*kateg09,1),"-", round(7*kateg09,1)),
               paste0(round(7*kateg09,1),"-", round(8*kateg09,1)),
               paste0(round(8*kateg09,1),"-", round(max(svet$gdp2009, na.rm=TRUE),1)))
kateg2010 <- c(paste0(0,"-",round(kateg10/4,1)), paste0(round(kateg10/4,1), "-", round(kateg10,1)),
               paste0(round(kateg10,1),"-", round(2*kateg10,1)),
               paste0(round(2*kateg10,1),"-", round(3*kateg10,1)),
               paste0(round(3*kateg10,1),"-", round(4*kateg10,1)),
               paste0(round(4*kateg10,1),"-", round(5*kateg10,1)),
               paste0(round(5*kateg10,1),"-", round(6*kateg10,1)),
               paste0(round(6*kateg10,1),"-", round(7*kateg10,1)),
               paste0(round(7*kateg10,1),"-", round(8*kateg10,1)),
               paste0(round(8*kateg10,1),"-", round(max(svet$gdp2010, na.rm=TRUE),1)))
kateg2011 <- c(paste0(0,"-",round(kateg11/4,1)), paste0(round(kateg11/4,1), "-", round(kateg11,1)),
               paste0(round(kateg11,1),"-", round(2*kateg11,1)),
               paste0(round(2*kateg11,1),"-", round(3*kateg11,1)),
               paste0(round(3*kateg11,1),"-", round(4*kateg11,1)),
               paste0(round(4*kateg11,1),"-", round(5*kateg11,1)),
               paste0(round(5*kateg11,1),"-", round(6*kateg11,1)),
               paste0(round(6*kateg11,1),"-", round(7*kateg11,1)),
               paste0(round(7*kateg11,1),"-", round(8*kateg11,1)),
               paste0(round(8*kateg11,1),"-", round(max(svet$gdp2011, na.rm=TRUE),1)))
kateg2012 <- c(paste0(0,"-",round(kateg12/4,1)), paste0(round(kateg12/4,1), "-", round(kateg12,1)),
               paste0(round(kateg12,1),"-", round(2*kateg12,1)),
               paste0(round(2*kateg12,1),"-", round(3*kateg12,1)),
               paste0(round(3*kateg12,1),"-", round(4*kateg12,1)),
               paste0(round(4*kateg12,1),"-", round(5*kateg12,1)),
               paste0(round(5*kateg12,1),"-", round(6*kateg12,1)),
               paste0(round(6*kateg12,1),"-", round(7*kateg12,1)),
               paste0(round(7*kateg12,1),"-", round(8*kateg12,1)),
               paste0(round(8*kateg12,1),"-", round(max(svet$gdp2012, na.rm=TRUE),1)))
kateg2013 <- c(paste0(0,"-",round(kateg13/4,1)), paste0(round(kateg13/4,1), "-", round(kateg13,1)),
               paste0(round(kateg13,1),"-", round(2*kateg13,1)),
               paste0(round(2*kateg13,1),"-", round(3*kateg13,1)),
               paste0(round(3*kateg13,1),"-", round(4*kateg13,1)),
               paste0(round(4*kateg13,1),"-", round(5*kateg13,1)),
               paste0(round(5*kateg13,1),"-", round(6*kateg13,1)),
               paste0(round(6*kateg13,1),"-", round(7*kateg13,1)),
               paste0(round(7*kateg13,1),"-", round(8*kateg13,1)),
               paste0(round(8*kateg13,1),"-", round(max(svet$gdp2013, na.rm=TRUE),1)))



  