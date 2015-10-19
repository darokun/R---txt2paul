#' ---
#' title: "txt2paul Baseline Analyses on subsets - n = 68 and 49 - Hypothesis testing"
#' author: "Daloha Rodriguez-Molina"
#' date: "April 07th, 2015"
#' ---

#-------------------#
# Main data used: txt2PAUL_Gesamt_vers1_130115.csv
# Name given to this main dataset: data
#
# Subset dataset mostly used on this script: data68 and data49
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
# Comparisons: 68 vs. 49 #
#--------------------------#

# Sex # report as male?
rbind(sex.demog,cbind(table(sex.factor68),round(prop.table(table(sex.factor68)),4), table(sex.factor49),round(prop.table(table(sex.factor49)),4))) 
fisher.test(rbind(table(sex.factor68),table(sex.factor49)))$p.value #[1] 0.1416416 # NS
sum(is.na(sex.factor68))
sum(is.na(sex.factor49))

# Age
rbind(age.demog.long,cbind(round(mean(data68$alter, na.rm=T),2),round(sd(data68$alter, na.rm = T),2), round(mean(data49$alter, na.rm=T),2),round(sd(data49$alter, na.rm = T),2)))
t.test(data68$alter, data49$alter)$p.value #[1] 0.01333069 ******
sum(is.na(data68$alter))
sum(is.na(data49$alter))

rbind(age.demog.cat,cbind(table(alterkat.factor68),round(prop.table(table(alterkat.factor68)),4), table(alterkat.factor49),round(prop.table(table(alterkat.factor49)),4))) 
fisher.test(rbind(table(alterkat.factor68),table(alterkat.factor49)))$p.value #[1] 0.01588993 ******

# Studienort
rbind(studienort.demog,cbind(table(studienort.factor68),round(prop.table(table(studienort.factor68)),4), table(studienort.factor49),round(prop.table(table(studienort.factor49)),4))) 
chisq.test(rbind(table(studienort.factor68),table(studienort.factor49)))$p.value #[1] 0.0001909999 ******
# chisq might be inapropriate but fisher returns an error 'cause there is not enough sample size
sum(is.na(studienort.factor68))
sum(is.na(studienort.factor49))

# Schulabschluss
rbind(schulabschluss.demog,cbind(table(data68$schulabschluss.factor),round(prop.table(table(data68$schulabschluss.factor)),4), table(data49$schulabschluss.factor),round(prop.table(table(data49$schulabschluss.factor)),4))) 
fisher.test(rbind(table(data68$schulabschluss.factor),table(data49$schulabschluss.factor)))$p.value #[1] 0.04118667
sum(is.na(schulabschluss.factor68))
sum(is.na(schulabschluss.factor49))


# Smoker
rbind(smoker.demog,cbind(table(smoking.status.factor68),round(prop.table(table(smoking.status.factor68)),4), table(smoking.status.factor49),round(prop.table(table(smoking.status.factor49)),4))) 
chisq.test(rbind(table(smoking.status.factor68),table(smoking.status.factor49)))$p.value #[1] 0.4341069 # NS
sum(is.na(smoking.status.factor68))
sum(is.na(smoking.status.factor49))


# asthma3
rbind(sex.demog,cbind(table(asthma3.factor68),round(prop.table(table(asthma3.factor68)),4), table(asthma3.factor49),round(prop.table(table(asthma3.factor49)),4))) 
fisher.test(rbind(table(asthma3.factor68),table(asthma3.factor49)))$p.value #[1] 1 # NS
sum(is.na(asthma3.factor68))
sum(is.na(asthma3.factor49))

# asthma.dx
rbind(sex.demog,cbind(table(asthma.dx.factor68),round(prop.table(table(asthma.dx.factor68)),4), table(asthma.dx.factor49),round(prop.table(table(asthma.dx.factor49)),4))) 
fisher.test(rbind(table(asthma.dx.factor68),table(asthma.dx.factor49)))$p.value #[1] 0.4535625 # NS
sum(is.na(asthma.dx.factor68))
sum(is.na(asthma.dx.factor49))


# allergies
rbind(sex.demog,cbind(table(allergies.factor68),round(prop.table(table(allergies.factor68)),4), table(allergies.factor49),round(prop.table(table(allergies.factor49)),4))) 
chisq.test(rbind(table(allergies.factor68),table(allergies.factor49)))$p.value #[1] 0.7254711 # NS
sum(is.na(allergies.factor68))
sum(is.na(allergies.factor49))


# par.asthma
rbind(sex.demog,cbind(table(par.asthma.factor68),round(prop.table(table(par.asthma.factor68)),4), table(par.asthma.factor49),round(prop.table(table(par.asthma.factor49)),4))) 
chisq.test(rbind(table(par.asthma.factor68),table(par.asthma.factor49)))$p.value #[1] 0.01747058 ******
sum(is.na(par.asthma.factor68))
sum(is.na(par.asthma.factor49))



# WISSEN
# haare
rbind(sex.demog,cbind(table(haare.factor68),round(prop.table(table(haare.factor68)),4), table(haare.factor49),round(prop.table(table(haare.factor49)),4))) 
chisq.test(rbind(table(haare.factor68),table(haare.factor49)))$p.value #[1] 0.8375334 # NS

# arbeitsschuhe
rbind(sex.demog,cbind(table(arbeitsschuhe.factor68),round(prop.table(table(arbeitsschuhe.factor68)),4), table(arbeitsschuhe.factor49),round(prop.table(table(arbeitsschuhe.factor49)),4))) 
chisq.test(rbind(table(arbeitsschuhe.factor68),table(arbeitsschuhe.factor49)))$p.value #[1] 0.7794335 # NS

# kleidung_aufb
rbind(sex.demog,cbind(table(kleidung_aufb.factor68),round(prop.table(table(kleidung_aufb.factor68)),4), table(kleidung_aufb.factor49),round(prop.table(table(kleidung_aufb.factor49)),4))) 
chisq.test(rbind(table(kleidung_aufb.factor68),table(kleidung_aufb.factor49)))$p.value #[1] 0.6627807 # NS

# desinfizieren
rbind(sex.demog,cbind(table(desinfizieren.factor68),round(prop.table(table(desinfizieren.factor68)),4), table(desinfizieren.factor49),round(prop.table(table(desinfizieren.factor49)),4))) 
chisq.test(rbind(table(desinfizieren.factor68),table(desinfizieren.factor49)))$p.value # [1] 0.8636169 # NS

# schutzbrille
rbind(sex.demog,cbind(table(schutzbrille.factor68),round(prop.table(table(schutzbrille.factor68)),4), table(schutzbrille.factor49),round(prop.table(table(schutzbrille.factor49)),4))) 
chisq.test(rbind(table(schutzbrille.factor68),table(schutzbrille.factor49)))$p.value #[1] 0.9890888 # NS

# kleidung_wohnraum
rbind(sex.demog,cbind(table(kleidung_wohnraum.factor68),round(prop.table(table(kleidung_wohnraum.factor68)),4), table(kleidung_wohnraum.factor49),round(prop.table(table(kleidung_wohnraum.factor49)),4))) 
chisq.test(rbind(table(kleidung_wohnraum.factor68),table(kleidung_wohnraum.factor49)))$p.value #[1] 0.3264782

# BEHAVIOR
# letzteWoche
# arbeitskleidung3_letzteWoche
rbind(sex.demog,cbind(table(arbeitskleidung3_letzteWoche.factor68),round(prop.table(table(arbeitskleidung3_letzteWoche.factor68)),4), table(arbeitskleidung3_letzteWoche.factor49),round(prop.table(table(arbeitskleidung3_letzteWoche.factor49)),4))) 
chisq.test(rbind(table(arbeitskleidung3_letzteWoche.factor68),table(arbeitskleidung3_letzteWoche.factor49)))$p.value #[1] 0.9331174 # NS

# haare3_letzteWoche
rbind(sex.demog,cbind(table(haare3_letzteWoche.factor68),round(prop.table(table(haare3_letzteWoche.factor68)),4), table(haare3_letzteWoche.factor49),round(prop.table(table(haare3_letzteWoche.factor49)),4))) 
fisher.test(rbind(table(haare3_letzteWoche.factor68),table(haare3_letzteWoche.factor49)))$p.value #[1] 0.4158978 # NS

# aufbewahrung3_letzteWoche
rbind(sex.demog,cbind(table(aufbewahrung3_letzteWoche.factor68),round(prop.table(table(aufbewahrung3_letzteWoche.factor68)),4), table(aufbewahrung3_letzteWoche.factor49),round(prop.table(table(aufbewahrung3_letzteWoche.factor49)),4))) 
fisher.test(rbind(table(aufbewahrung3_letzteWoche.factor68),table(aufbewahrung3_letzteWoche.factor49)))$p.value #[1] 0.7045584 # NS


# intention
# arbeitskleidung3_intention
rbind(sex.demog,cbind(table(arbeitskleidung3_intention.factor68),round(prop.table(table(arbeitskleidung3_intention.factor68)),4), table(arbeitskleidung3_intention.factor49),round(prop.table(table(arbeitskleidung3_intention.factor49)),4))) 
chisq.test(rbind(table(arbeitskleidung3_intention.factor68),table(arbeitskleidung3_intention.factor49)))$p.value #[1] 0.9042003 # NS

# haare3_intention
rbind(sex.demog,cbind(table(haare3_intention.factor68),round(prop.table(table(haare3_intention.factor68)),4), table(haare3_intention.factor49),round(prop.table(table(haare3_intention.factor49)),4))) 
fisher.test(rbind(table(haare3_intention.factor68),table(haare3_intention.factor49)))$p.value #[1] 0.2257527 # NS

# aufbewahrung3_intention
rbind(sex.demog,cbind(table(aufbewahrung3_intention.factor68),round(prop.table(table(aufbewahrung3_intention.factor68)),4), table(aufbewahrung3_intention.factor49),round(prop.table(table(aufbewahrung3_intention.factor49)),4))) 
fisher.test(rbind(table(aufbewahrung3_intention.factor68),table(aufbewahrung3_intention.factor49)))$p.value #[1] 0.8517473 # NS

#einstellung
# arbeitskleidung3_einstellung
rbind(sex.demog,cbind(table(arbeitskleidung3_einstellung.factor68),round(prop.table(table(arbeitskleidung3_einstellung.factor68)),4), table(arbeitskleidung3_einstellung.factor49),round(prop.table(table(arbeitskleidung3_einstellung.factor49)),4))) 
fisher.test(rbind(table(arbeitskleidung3_einstellung.factor68),table(arbeitskleidung3_einstellung.factor49)))$p.value #[1] 1 # NS

# haare3_einstellung
rbind(sex.demog,cbind(table(haare3_einstellung.factor68),round(prop.table(table(haare3_einstellung.factor68)),4), table(haare3_einstellung.factor49),round(prop.table(table(haare3_einstellung.factor49)),4))) 
fisher.test(rbind(table(haare3_einstellung.factor68),table(haare3_einstellung.factor49)))$p.value #[1] 0.8221566 

# aufbewahrung3_einstellung
rbind(sex.demog,cbind(table(aufbewahrung3_einstellung.factor68),round(prop.table(table(aufbewahrung3_einstellung.factor68)),4), table(aufbewahrung3_einstellung.factor49),round(prop.table(table(aufbewahrung3_einstellung.factor49)),4))) 
fisher.test(rbind(table(aufbewahrung3_einstellung.factor68),table(aufbewahrung3_einstellung.factor49)))$p.value #[1] 1 # NS

#norm
# arbeitskleidung3_norm
rbind(sex.demog,cbind(table(arbeitskleidung3_norm.factor68),round(prop.table(table(arbeitskleidung3_norm.factor68)),4), table(arbeitskleidung3_norm.factor49),round(prop.table(table(arbeitskleidung3_norm.factor49)),4))) 
chisq.test(rbind(table(arbeitskleidung3_norm.factor68),table(arbeitskleidung3_norm.factor49)))$p.value #[1] 0.4949239

# haare3_norm
rbind(sex.demog,cbind(table(haare3_norm.factor68),round(prop.table(table(haare3_norm.factor68)),4), table(haare3_norm.factor49),round(prop.table(table(haare3_norm.factor49)),4))) 
fisher.test(rbind(table(haare3_norm.factor68),table(haare3_norm.factor49)))$p.value #[1] 0.1702824

# aufbewahrung3_norm
rbind(sex.demog,cbind(table(aufbewahrung3_norm.factor68),round(prop.table(table(aufbewahrung3_norm.factor68)),4), table(aufbewahrung3_norm.factor49),round(prop.table(table(aufbewahrung3_norm.factor49)),4))) 
fisher.test(rbind(table(aufbewahrung3_norm.factor68),table(aufbewahrung3_norm.factor49)))$p.value #[1] 0.6737608 # NS

#kontrolle
# arbeitskleidung3_kontrolle
rbind(sex.demog,cbind(table(arbeitskleidung3_kontrolle.factor68),round(prop.table(table(arbeitskleidung3_kontrolle.factor68)),4), table(arbeitskleidung3_kontrolle.factor49),round(prop.table(table(arbeitskleidung3_kontrolle.factor49)),4))) 
chisq.test(rbind(table(arbeitskleidung3_kontrolle.factor68),table(arbeitskleidung3_kontrolle.factor49)))$p.value #[1] 0.796811 # NS

# haare3_kontrolle
rbind(sex.demog,cbind(table(haare3_kontrolle.factor68),round(prop.table(table(haare3_kontrolle.factor68)),4), table(haare3_kontrolle.factor49),round(prop.table(table(haare3_kontrolle.factor49)),4))) 
fisher.test(rbind(table(haare3_kontrolle.factor68),table(haare3_kontrolle.factor49)))$p.value #[1] 0.3756526 # NS

# aufbewahrung3_kontrolle
rbind(sex.demog,cbind(table(aufbewahrung3_kontrolle.factor68),round(prop.table(table(aufbewahrung3_kontrolle.factor68)),4), table(aufbewahrung3_kontrolle.factor49),round(prop.table(table(aufbewahrung3_kontrolle.factor49)),4))) 
fisher.test(rbind(table(aufbewahrung3_kontrolle.factor68),table(aufbewahrung3_kontrolle.factor49)))$p.value #[1] 0.6101986 # NS

# allergie_bekommen
rbind(sex.demog,cbind(table(allergie3_bekommen.factor68),round(prop.table(table(allergie3_bekommen.factor68)),4), table(allergie3_bekommen.factor49),round(prop.table(table(allergie3_bekommen.factor49)),4))) 
fisher.test(rbind(table(allergie3_bekommen.factor68),table(allergie3_bekommen.factor49)))$p.value #[1] 0.6148393 # NS

# allergie_schlimm
rbind(sex.demog,cbind(table(allergie3_schlimm.factor68),round(prop.table(table(allergie3_schlimm.factor68)),4), table(allergie3_schlimm.factor49),round(prop.table(table(allergie3_schlimm.factor49)),4))) 
fisher.test(rbind(table(allergie3_schlimm.factor68),table(allergie3_schlimm.factor49)))$p.value #[1] 0.1581523 # NS

#-------
#-------

# how many out of the 49 are in which intervention group
sum(data49$gruppe==1) # 18
sum(data49$gruppe==0) # 31

#-------
#-------



