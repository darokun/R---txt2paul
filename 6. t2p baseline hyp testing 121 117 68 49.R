#' ---
#' title: "txt2paul Baseline Analyses on subsets - n = 121 and 117 - Hypothesis testing"
#' author: "Daloha Rodriguez-Molina"
#' date: "April 07th, 2015"
#' ---

#-------------------#
# Main data used: txt2PAUL_Gesamt_vers1_130115.csv
# Name given to this main dataset: data
#
# Subset dataset mostly used on this script: data117, data121, data68 and data49
# which contains data1 = soziodemographie
#
# Variables in the data1 dataset:
# [1] "ID"             
# [2] "intervention"   
# [3] "gruppe"         
# [4] "panel"          
# [5] "studienort"     
# [6] "befdatum"       
# [7] "gebdatum"       
# [8] "gebjahr"        
# [9] "alter"          
# [10] "alterkat"       
# [11] "sex"            
# [12] "schulabschluss" 
# [13] "rauchen"        
# [14] "rauchen_aktuell"
# 
#-------------------#

#-------------------#
# Preliminary stuff #
#-------------------#

# When working from home
setwd("~/Dropbox/R - txt2paul")

# When working at KR5
# setwd("/usr281/ben/msc1426/Desktop/dataproject_datasets-1")

# install packages
require("psych")

# Read the data
# d <- "/Users/daro/Desktop/txt2PAUL/z_Dokumente fuer Daloha/txt2PAUL_Gesamt_vers1_130115.csv"
# data <- read.csv(d, header=T)

# explore data and subset demographics
# head(data[,1:4])
# data1 <- data[,c(1:6,14:21)] # data1 = soziodemographie
# names(data1)

#---

#--------------------------#
# Comparisons: 121 vs. 117 #
#--------------------------#

# Sex # report as male?
rbind(sex.demog,cbind(table(sex.factor121),round(prop.table(table(sex.factor121)),4), table(sex.factor117),round(prop.table(table(sex.factor117)),4))) 
chisq.test(rbind(table(sex.factor121),table(sex.factor117)))$p.value #[1] 0.2191967 # NS

# Age
rbind(age.demog.long,cbind(round(mean(data121$alter, na.rm=T),2),round(sd(data121$alter, na.rm = T),2), round(mean(data117$alter, na.rm=T),2),round(sd(data117$alter, na.rm = T),2)))
t.test(data121$alter, data117$alter)$p.value #[1] 0.8879944 # NS

rbind(age.demog.cat,cbind(table(alterkat.factor121),round(prop.table(table(alterkat.factor121)),4), table(alterkat.factor117),round(prop.table(table(alterkat.factor117)),4))) 
chisq.test(rbind(table(alterkat.factor121),table(alterkat.factor117)))$p.value #[1] 1 # NS

# Studienort
rbind(studienort.demog,cbind(table(studienort.factor121),round(prop.table(table(studienort.factor121)),4), table(studienort.factor117),round(prop.table(table(studienort.factor117)),4))) 
chisq.test(rbind(table(studienort.factor121),table(studienort.factor117)))$p.value #[1] 2.511517e-12

# Schulabschluss
rbind(schulabschluss.demog,cbind(table(schulabschluss.factor121),round(prop.table(table(schulabschluss.factor121)),4), table(schulabschluss.factor117),round(prop.table(table(schulabschluss.factor117)),4))) 
fisher.test(rbind(table(schulabschluss.factor121),table(schulabschluss.factor117)))$p.value #[1] 0.7407913 # NS

# Smoker
smoker.demog
rbind(smoker.demog,cbind(table(smoking.status.factor121),round(prop.table(table(smoking.status.factor121)),4), table(smoking.status.factor117),round(prop.table(table(smoking.status.factor117)),4))) 
chisq.test(rbind(table(smoking.status.factor121),table(smoking.status.factor117)))$p.value #[1] 0.2426499 #NS

# asthma3
rbind(sex.demog,cbind(table(asthma3.factor121),round(prop.table(table(asthma3.factor121)),4), table(asthma3.factor117),round(prop.table(table(asthma3.factor117)),4))) 
chisq.test(rbind(table(asthma3.factor121),table(asthma3.factor117)))$p.value #[1] 0.1404635 # NS
sum(is.na(asthma3.factor121))
sum(is.na(asthma3.factor117))

# asthma.dx
rbind(sex.demog,cbind(table(asthma.dx.factor121),round(prop.table(table(asthma.dx.factor121)),4), table(asthma.dx.factor117),round(prop.table(table(asthma.dx.factor117)),4))) 
fisher.test(rbind(table(asthma.dx.factor121),table(asthma.dx.factor117)))$p.value #[1] 0.3711115 # NS
sum(is.na(asthma.dx.factor121))
sum(is.na(asthma.dx.factor117))


# allergies
rbind(sex.demog,cbind(table(allergies.factor121),round(prop.table(table(allergies.factor121)),4), table(allergies.factor117),round(prop.table(table(allergies.factor117)),4))) 
chisq.test(rbind(table(allergies.factor121),table(allergies.factor117)))$p.value #[1] 0.8194083 # NS
sum(is.na(allergies.factor121))
sum(is.na(allergies.factor117))

# par.asthma
rbind(sex.demog,cbind(table(par.asthma.factor121),round(prop.table(table(par.asthma.factor121)),4), table(par.asthma.factor117),round(prop.table(table(par.asthma.factor117)),4))) 
chisq.test(rbind(table(par.asthma.factor121),table(par.asthma.factor117)))$p.value #[1] 0.3997413 # NS
sum(is.na(par.asthma.factor121))
sum(is.na(par.asthma.factor117))


# WISSEN
# haare
rbind(sex.demog,cbind(table(haare.factor121),round(prop.table(table(haare.factor121)),4), table(haare.factor117),round(prop.table(table(haare.factor117)),4))) 
chisq.test(rbind(table(haare.factor121),table(haare.factor117)))$p.value #[1] 0.2936699 # NS
sum(is.na(haare.factor121))
sum(is.na(haare.factor117))

# arbeitsschuhe
rbind(sex.demog,cbind(table(arbeitsschuhe.factor121),round(prop.table(table(arbeitsschuhe.factor121)),4), table(arbeitsschuhe.factor117),round(prop.table(table(arbeitsschuhe.factor117)),4))) 
chisq.test(rbind(table(arbeitsschuhe.factor121),table(arbeitsschuhe.factor117)))$p.value #[1] 0.3928259 # NS
sum(is.na(arbeitsschuhe.factor121))
sum(is.na(arbeitsschuhe.factor117))

# kleidung_aufb
rbind(sex.demog,cbind(table(kleidung_aufb.factor121),round(prop.table(table(kleidung_aufb.factor121)),4), table(kleidung_aufb.factor117),round(prop.table(table(kleidung_aufb.factor117)),4))) 
chisq.test(rbind(table(kleidung_aufb.factor121),table(kleidung_aufb.factor117)))$p.value #[1] 0.953806 # NS
sum(is.na(kleidung_aufb.factor121))
sum(is.na(kleidung_aufb.factor117))

# desinfizieren
rbind(sex.demog,cbind(table(desinfizieren.factor121),round(prop.table(table(desinfizieren.factor121)),4), table(desinfizieren.factor117),round(prop.table(table(desinfizieren.factor117)),4))) 
chisq.test(rbind(table(desinfizieren.factor121),table(desinfizieren.factor117)))$p.value # [1] 0.5482001 # NS
sum(is.na(desinfizieren.factor121))
sum(is.na(desinfizieren.factor117))

# schutzbrille
rbind(sex.demog,cbind(table(schutzbrille.factor121),round(prop.table(table(schutzbrille.factor121)),4), table(schutzbrille.factor117),round(prop.table(table(schutzbrille.factor117)),4))) 
chisq.test(rbind(table(schutzbrille.factor121),table(schutzbrille.factor117)))$p.value #[1] 0.4678177 # NS
sum(is.na(schutzbrille.factor121))
sum(is.na(schutzbrille.factor117))

# kleidung_wohnraum
rbind(sex.demog,cbind(table(kleidung_wohnraum.factor121),round(prop.table(table(kleidung_wohnraum.factor121)),4), table(kleidung_wohnraum.factor117),round(prop.table(table(kleidung_wohnraum.factor117)),4))) 
chisq.test(rbind(table(kleidung_wohnraum.factor121),table(kleidung_wohnraum.factor117)))$p.value #[1] 1 # NS
sum(is.na(kleidung_wohnraum.factor121))
sum(is.na(kleidung_wohnraum.factor117))

# BEHAVIOR
# letzteWoche
# arbeitskleidung3_letzteWoche
rbind(sex.demog,cbind(table(arbeitskleidung3_letzteWoche.factor121),round(prop.table(table(arbeitskleidung3_letzteWoche.factor121)),4), table(arbeitskleidung3_letzteWoche.factor117),round(prop.table(table(arbeitskleidung3_letzteWoche.factor117)),4))) 
chisq.test(rbind(table(arbeitskleidung3_letzteWoche.factor121),table(arbeitskleidung3_letzteWoche.factor117)))$p.value #[1] 0.2298049 # NS

# haare3_letzteWoche
rbind(sex.demog,cbind(table(haare3_letzteWoche.factor121),round(prop.table(table(haare3_letzteWoche.factor121)),4), table(haare3_letzteWoche.factor117),round(prop.table(table(haare3_letzteWoche.factor117)),4))) 
fisher.test(rbind(table(haare3_letzteWoche.factor121),table(haare3_letzteWoche.factor117)))$p.value #[1] 0.6447883 # NS

# aufbewahrung3_letzteWoche
rbind(sex.demog,cbind(table(aufbewahrung3_letzteWoche.factor121),round(prop.table(table(aufbewahrung3_letzteWoche.factor121)),4), table(aufbewahrung3_letzteWoche.factor117),round(prop.table(table(aufbewahrung3_letzteWoche.factor117)),4))) 
chisq.test(rbind(table(aufbewahrung3_letzteWoche.factor121),table(aufbewahrung3_letzteWoche.factor117)))$p.value #[1] 0.2864857 # NS


# intention
# arbeitskleidung3_intention
rbind(sex.demog,cbind(table(arbeitskleidung3_intention.factor121),round(prop.table(table(arbeitskleidung3_intention.factor121)),4), table(arbeitskleidung3_intention.factor117),round(prop.table(table(arbeitskleidung3_intention.factor117)),4))) 
chisq.test(rbind(table(arbeitskleidung3_intention.factor121),table(arbeitskleidung3_intention.factor117)))$p.value #[1] 0.6660813 # NS

# haare3_intention
rbind(sex.demog,cbind(table(haare3_intention.factor121),round(prop.table(table(haare3_intention.factor121)),4), table(haare3_intention.factor117),round(prop.table(table(haare3_intention.factor117)),4))) 
fisher.test(rbind(table(haare3_intention.factor121),table(haare3_intention.factor117)))$p.value #[1] 0.9388558 # NS

# aufbewahrung3_intention
rbind(sex.demog,cbind(table(aufbewahrung3_intention.factor121),round(prop.table(table(aufbewahrung3_intention.factor121)),4), table(aufbewahrung3_intention.factor117),round(prop.table(table(aufbewahrung3_intention.factor117)),4))) 
chisq.test(rbind(table(aufbewahrung3_intention.factor121),table(aufbewahrung3_intention.factor117)))$p.value #[1] 0.2608431 # NS

#einstellung
# arbeitskleidung3_einstellung
rbind(sex.demog,cbind(table(arbeitskleidung3_einstellung.factor121),round(prop.table(table(arbeitskleidung3_einstellung.factor121)),4), table(arbeitskleidung3_einstellung.factor117),round(prop.table(table(arbeitskleidung3_einstellung.factor117)),4))) 
chisq.test(rbind(table(arbeitskleidung3_einstellung.factor121),table(arbeitskleidung3_einstellung.factor117)))$p.value #[1] 0.4583552 # NS

# haare3_einstellung
rbind(sex.demog,cbind(table(haare3_einstellung.factor121),round(prop.table(table(haare3_einstellung.factor121)),4), table(haare3_einstellung.factor117),round(prop.table(table(haare3_einstellung.factor117)),4))) 
fisher.test(rbind(table(haare3_einstellung.factor121),table(haare3_einstellung.factor117)))$p.value #[1] 0.6745873 # NS

# aufbewahrung3_einstellung
rbind(sex.demog,cbind(table(aufbewahrung3_einstellung.factor121),round(prop.table(table(aufbewahrung3_einstellung.factor121)),4), table(aufbewahrung3_einstellung.factor117),round(prop.table(table(aufbewahrung3_einstellung.factor117)),4))) 
fisher.test(rbind(table(aufbewahrung3_einstellung.factor121),table(aufbewahrung3_einstellung.factor117)))$p.value #[1] 0.758559 # NS

#norm
# arbeitskleidung3_norm
rbind(sex.demog,cbind(table(arbeitskleidung3_norm.factor121),round(prop.table(table(arbeitskleidung3_norm.factor121)),4), table(arbeitskleidung3_norm.factor117),round(prop.table(table(arbeitskleidung3_norm.factor117)),4))) 
chisq.test(rbind(table(arbeitskleidung3_norm.factor121),table(arbeitskleidung3_norm.factor117)))$p.value #[1] 0.02884524 ******

# haare3_norm
rbind(sex.demog,cbind(table(haare3_norm.factor121),round(prop.table(table(haare3_norm.factor121)),4), table(haare3_norm.factor117),round(prop.table(table(haare3_norm.factor117)),4))) 
fisher.test(rbind(table(haare3_norm.factor121),table(haare3_norm.factor117)))$p.value #[1] 0.1916306 # NS

# aufbewahrung3_norm
rbind(sex.demog,cbind(table(aufbewahrung3_norm.factor121),round(prop.table(table(aufbewahrung3_norm.factor121)),4), table(aufbewahrung3_norm.factor117),round(prop.table(table(aufbewahrung3_norm.factor117)),4))) 
chisq.test(rbind(table(aufbewahrung3_norm.factor121),table(aufbewahrung3_norm.factor117)))$p.value #[1] 0.8118318 # NS

#kontrolle
# arbeitskleidung3_kontrolle
rbind(sex.demog,cbind(table(arbeitskleidung3_kontrolle.factor121),round(prop.table(table(arbeitskleidung3_kontrolle.factor121)),4), table(arbeitskleidung3_kontrolle.factor117),round(prop.table(table(arbeitskleidung3_kontrolle.factor117)),4))) 
chisq.test(rbind(table(arbeitskleidung3_kontrolle.factor121),table(arbeitskleidung3_kontrolle.factor117)))$p.value #[1] 0.9704561 # NS

# haare3_kontrolle
rbind(sex.demog,cbind(table(haare3_kontrolle.factor121),round(prop.table(table(haare3_kontrolle.factor121)),4), table(haare3_kontrolle.factor117),round(prop.table(table(haare3_kontrolle.factor117)),4))) 
fisher.test(rbind(table(haare3_kontrolle.factor121),table(haare3_kontrolle.factor117)))$p.value #[1] 0.6364363 # NS

# aufbewahrung3_kontrolle
rbind(sex.demog,cbind(table(aufbewahrung3_kontrolle.factor121),round(prop.table(table(aufbewahrung3_kontrolle.factor121)),4), table(aufbewahrung3_kontrolle.factor117),round(prop.table(table(aufbewahrung3_kontrolle.factor117)),4))) 
chisq.test(rbind(table(aufbewahrung3_kontrolle.factor121),table(aufbewahrung3_kontrolle.factor117)))$p.value #[1] 0.5842781 # NS

# allergie_bekommen
rbind(sex.demog,cbind(table(allergie3_bekommen.factor121),round(prop.table(table(allergie3_bekommen.factor121)),4), table(allergie3_bekommen.factor117),round(prop.table(table(allergie3_bekommen.factor117)),4))) 
chisq.test(rbind(table(allergie3_bekommen.factor121),table(allergie3_bekommen.factor117)))$p.value #[1] 0.7138531 # NS

# allergie_schlimm
rbind(sex.demog,cbind(table(allergie3_schlimm.factor121),round(prop.table(table(allergie3_schlimm.factor121)),4), table(allergie3_schlimm.factor117),round(prop.table(table(allergie3_schlimm.factor117)),4))) 
chisq.test(rbind(table(allergie3_schlimm.factor121),table(allergie3_schlimm.factor117)))$p.value #[1] 0.8086622 # NS



#--- 

#---------------#
# END OF SCRIPT #
#---------------#


