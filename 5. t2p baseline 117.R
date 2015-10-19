#' ---
#' title: "txt2paul Baseline Analyses on subsets - n = 121, 117, 68 and 49"
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

# convert to factors nachher behavior
# behavior nachher general
data$arbeitskleidung3_letzteWoche_nachher.factor <- factor(arbeitskleidung3_letzteWoche_nachher)
data$haare3_letzteWoche_nachher.factor <- factor(haare3_letzteWoche_nachher)
data$aufbewahrung3_letzteWoche_nachher.factor <- factor(aufbewahrung3_letzteWoche_nachher, levels=c(0,1))
data$arbeitskleidung3_intention_nachher.factor <- factor(arbeitskleidung3_intention_nachher)
data$haare3_intention_nachher.factor <- factor(haare3_intention_nachher)
data$aufbewahrung3_intention_nachher.factor <- factor(aufbewahrung3_intention_nachher)
data$arbeitskleidung3_einstellung_nachher.factor <- factor(arbeitskleidung3_einstellung_nachher)
data$haare3_einstellung_nachher.factor <- factor(haare3_einstellung_nachher)
data$aufbewahrung3_einstellung_nachher.factor <- factor(aufbewahrung3_einstellung_nachher)
data$arbeitskleidung3_norm_nachher.factor <- factor(arbeitskleidung3_norm_nachher)
data$haare3_norm_nachher.factor <- factor(haare3_norm_nachher)
data$aufbewahrung3_norm_nachher.factor <- factor(aufbewahrung3_norm_nachher)
data$arbeitskleidung3_kontrolle_nachher.factor <- factor(arbeitskleidung3_kontrolle_nachher)
data$haare3_kontrolle_nachher.factor <- factor(haare3_kontrolle_nachher)
data$aufbewahrung3_kontrolle_nachher.factor <- factor(aufbewahrung3_kontrolle_nachher)

# from file 1:
# Zusammensetzung der Studienpopulation: (Abbildung 2 in Abschlussbericht document)
# nrow(data) # total n = 238
# sum(data$Ausgeschlossen==1) # 119
# sum(data$Ausgeschlossen==0) # 119
# sum(data$Absage==1) # 2, these belong to Ausgeschlossen==1, so the total number of Ausgeschlossen==1 is 121, while the total n of ==0 is 117 
# sum(data$Ausgeschlossen==0) - sum(data$Absage==1) # 117 (eingeschlossen)

# new dataset with n=117 from those with follow-up info
data117 <- data[data$intervention==1,]
data117 <- data117[data117$Absage==0,]
# how many out of the 117 are in which intervention group
sum(data117$gruppe==1) #70
sum(data117$gruppe==0) # 47


data117$FB_bekommen==1[data117$gruppe==1]
table(data117$gruppe, data117$FB_bekommen)
sum(data117$FB_bekommen, na.rm=T)

#new dataset with n=121 from the dropouts
data121 <- data[c(data$Ausgeschlossen==1, data$Absage==1),]

# new datasets with n=68 and n=49 from those 
# who have info "only before" and "before + after", respectively.
data68 <- data117[is.na(data117$haare_nachher),] # 68
data49 <- data117[!is.na(data117$haare_nachher),] # 49
# how many out of the 49 are in which intervention group
sum(data49$gruppe==1) # 18
sum(data49$gruppe==0) # 31

#---------------------#
# Convert to factors: #
#---------------------#



# data121
sex.factor121 <- factor(data121$sex, labels = sex.labels)
studienort.factor121 <- factor(data121$studienort, labels = studienort.labels)
alterkat.factor121 <- factor(data121$alterkat,levels=c("1","2","3"),labels=alterkat.labels)
schulabschluss.factor121 <- factor(data121$schulabschluss.factor, labels=schulabschluss.labels)
smoking.status.factor121 <- factor(data121$smoking.status, labels = smoking.status.labels)
pfeifen.factor121 <- factor(data121$pfeifen, labels = yes.no.labels, levels = c(1,0))
pfeifen_ohne_erk.factor121 <- factor(data121$pfeifen_ohne_erk, labels = yes.no.kein.labels, levels = c(1,0,2))
asthma.factor121 <- factor(data121$asthma, labels = yes.no.labels, levels = c(1,0))
asthma_arzt.factor121 <- factor(data121$asthma_arzt, labels = yes.no.kein.labels, levels = c(1,0,2))
medikamente.factor121 <- factor(data121$medikamente, labels = yes.no.kein.labels, levels = c(1,0,2))
nasenprobleme.factor121 <- factor(data121$nasenprobleme, labels = yes.no.labels, levels = c(1,0))
nasenprobleme_augen.factor121 <- factor(data121$nasenprobleme_augen, labels = yes.no.kein.labels, levels = c(1,0,2))
allergie_mutter.factor121 <- factor(data121$allergie_mutter, labels = yes.no.weissnicht.labels, levels = c(1,0,2))
allergie_vater.factor121 <- factor(data121$allergie_vater, labels = yes.no.weissnicht.labels, levels = c(1,0,2))
asthma3.factor121 <- factor(data121$asthma3, labels = yes.no.labels, levels = c(1,0))
asthma.dx.factor121 <- factor(data121$asthma.dx, labels = yes.no.labels, levels = c(1,0))
allergies.factor121 <- factor(data121$allergies, labels = yes.no.labels, levels = c(1,0))
par.asthma.factor121 <- factor(data121$par.asthma, labels = yes.no.labels, levels = c(1,0))
haare.factor121 <- factor(data121$haare, labels = yes.no.labels, levels = c(1,0))
arbeitsschuhe.factor121 <- factor(data121$arbeitsschuhe, labels = yes.no.labels, levels = c(1,0))
kleidung_aufb.factor121 <- factor(data121$kleidung_aufb, labels = yes.no.labels, levels = c(1,0))
desinfizieren.factor121 <- factor(data121$desinfizieren, labels = yes.no.labels, levels = c(1,0))
schutzbrille.factor121 <- factor(data121$schutzbrille, labels = yes.no.labels, levels = c(1,0))
kleidung_wohnraum.factor121 <- factor(data121$kleidung_wohnraum, labels = yes.no.labels, levels = c(1,0))
arbeitskleidung3_letzteWoche.factor121 <- factor(data121$arbeitskleidung3_letzteWoche)
haare3_letzteWoche.factor121 <- factor(data121$haare3_letzteWoche)
aufbewahrung3_letzteWoche.factor121 <- factor(data121$aufbewahrung3_letzteWoche)
arbeitskleidung3_intention.factor121 <- factor(data121$arbeitskleidung3_intention)
haare3_intention.factor121 <- factor(data121$haare3_intention)
aufbewahrung3_intention.factor121 <- factor(data121$aufbewahrung3_intention)
arbeitskleidung3_einstellung.factor121 <- factor(data121$arbeitskleidung3_einstellung)
haare3_einstellung.factor121 <- factor(data121$haare3_einstellung)
aufbewahrung3_einstellung.factor121 <- factor(data121$aufbewahrung3_einstellung)
arbeitskleidung3_norm.factor121 <- factor(data121$arbeitskleidung3_norm)
haare3_norm.factor121 <- factor(data121$haare3_norm)
aufbewahrung3_norm.factor121 <- factor(data121$aufbewahrung3_norm)
arbeitskleidung3_kontrolle.factor121 <- factor(data121$arbeitskleidung3_kontrolle)
haare3_kontrolle.factor121 <- factor(data121$haare3_kontrolle)
aufbewahrung3_kontrolle.factor121 <- factor(data121$aufbewahrung3_kontrolle)
allergie3_bekommen.factor121 <- factor(data121$allergie3_bekommen)
allergie3_schlimm.factor121 <- factor(data121$allergie3_schlimm)

# data117
sex.factor117 <- factor(data117$sex, labels = sex.labels)
studienort.factor117 <- factor(data117$studienort, labels = studienort.labels)
alterkat.factor117 <- factor(data117$alterkat,levels=c("1","2","3"),labels=alterkat.labels)
schulabschluss.factor117 <- factor(data117$schulabschluss.factor, labels=schulabschluss.labels)
smoking.status.factor117 <- factor(data117$smoking.status, labels = smoking.status.labels)
pfeifen.factor117 <- factor(data117$pfeifen, labels = yes.no.labels, levels = c(1,0))
pfeifen_ohne_erk.factor117 <- factor(data117$pfeifen_ohne_erk, labels = yes.no.kein.labels, levels = c(1,0,2))
asthma.factor117 <- factor(data117$asthma, labels = yes.no.labels, levels = c(1,0))
asthma_arzt.factor117 <- factor(data117$asthma_arzt, labels = yes.no.kein.labels, levels = c(1,0,2))
medikamente.factor117 <- factor(data117$medikamente, labels = yes.no.kein.labels, levels = c(1,0,2))
nasenprobleme.factor117 <- factor(data117$nasenprobleme, labels = yes.no.labels, levels = c(1,0))
nasenprobleme_augen.factor117 <- factor(data117$nasenprobleme_augen, labels = yes.no.kein.labels, levels = c(1,0,2))
allergie_mutter.factor117 <- factor(data117$allergie_mutter, labels = yes.no.weissnicht.labels, levels = c(1,0,2))
allergie_vater.factor117 <- factor(data117$allergie_vater, labels = yes.no.weissnicht.labels, levels = c(1,0,2))
asthma3.factor117 <- factor(data117$asthma3, labels = yes.no.labels, levels = c(1,0))
asthma.dx.factor117 <- factor(data117$asthma.dx, labels = yes.no.labels, levels = c(1,0))
allergies.factor117 <- factor(data117$allergies, labels = yes.no.labels, levels = c(1,0))
par.asthma.factor117 <- factor(data117$par.asthma, labels = yes.no.labels, levels = c(1,0))
haare.factor117 <- factor(data117$haare, labels = yes.no.labels, levels = c(1,0))
arbeitsschuhe.factor117 <- factor(data117$arbeitsschuhe, labels = yes.no.labels, levels = c(1,0))
kleidung_aufb.factor117 <- factor(data117$kleidung_aufb, labels = yes.no.labels, levels = c(1,0))
desinfizieren.factor117 <- factor(data117$desinfizieren, labels = yes.no.labels, levels = c(1,0))
schutzbrille.factor117 <- factor(data117$schutzbrille, labels = yes.no.labels, levels = c(1,0))
kleidung_wohnraum.factor117 <- factor(data117$kleidung_wohnraum, labels = yes.no.labels, levels = c(1,0))
arbeitskleidung3_letzteWoche.factor117 <- factor(data117$arbeitskleidung3_letzteWoche)
haare3_letzteWoche.factor117 <- factor(data117$haare3_letzteWoche)
aufbewahrung3_letzteWoche.factor117 <- factor(data117$aufbewahrung3_letzteWoche)
arbeitskleidung3_intention.factor117 <- factor(data117$arbeitskleidung3_intention)
haare3_intention.factor117 <- factor(data117$haare3_intention)
aufbewahrung3_intention.factor117 <- factor(data117$aufbewahrung3_intention)
arbeitskleidung3_einstellung.factor117 <- factor(data117$arbeitskleidung3_einstellung)
haare3_einstellung.factor117 <- factor(data117$haare3_einstellung)
aufbewahrung3_einstellung.factor117 <- factor(data117$aufbewahrung3_einstellung)
arbeitskleidung3_norm.factor117 <- factor(data117$arbeitskleidung3_norm)
haare3_norm.factor117 <- factor(data117$haare3_norm)
aufbewahrung3_norm.factor117 <- factor(data117$aufbewahrung3_norm)
arbeitskleidung3_kontrolle.factor117 <- factor(data117$arbeitskleidung3_kontrolle)
haare3_kontrolle.factor117 <- factor(data117$haare3_kontrolle)
aufbewahrung3_kontrolle.factor117 <- factor(data117$aufbewahrung3_kontrolle)
allergie3_bekommen.factor117 <- factor(data117$allergie3_bekommen)
allergie3_schlimm.factor117 <- factor(data117$allergie3_schlimm)

# data68
sex.factor68 <- factor(data68$sex, labels = sex.labels)
studienort.factor68 <- factor(data68$studienort, labels = studienort.labels)
alterkat.factor68 <- factor(data68$alterkat,levels=c("1","2","3"),labels=alterkat.labels)
schulabschluss.factor68 <- factor(data68$schulabschluss.factor)
schulabschluss.factor68
smoking.status.factor68 <- factor(data68$smoking.status, labels = smoking.status.labels)
pfeifen.factor68 <- factor(data68$pfeifen, labels = yes.no.labels, levels = c(1,0))
pfeifen_ohne_erk.factor68 <- factor(data68$pfeifen_ohne_erk, labels = yes.no.kein.labels, levels = c(1,0,2))
asthma.factor68 <- factor(data68$asthma, labels = yes.no.labels, levels = c(1,0))
asthma_arzt.factor68 <- factor(data68$asthma_arzt, labels = yes.no.kein.labels, levels = c(1,0,2))
medikamente.factor68 <- factor(data68$medikamente, labels = yes.no.kein.labels, levels = c(1,0,2))
nasenprobleme.factor68 <- factor(data68$nasenprobleme, labels = yes.no.labels, levels = c(1,0))
nasenprobleme_augen.factor68 <- factor(data68$nasenprobleme_augen, labels = yes.no.kein.labels, levels = c(1,0,2))
allergie_mutter.factor68 <- factor(data68$allergie_mutter, labels = yes.no.weissnicht.labels, levels = c(1,0,2))
allergie_vater.factor68 <- factor(data68$allergie_vater, labels = yes.no.weissnicht.labels, levels = c(1,0,2))
asthma3.factor68 <- factor(data68$asthma3, labels = yes.no.labels, levels = c(1,0))
asthma.dx.factor68 <- factor(data68$asthma.dx, labels = yes.no.labels, levels = c(1,0))
allergies.factor68 <- factor(data68$allergies, labels = yes.no.labels, levels = c(1,0))
par.asthma.factor68 <- factor(data68$par.asthma, labels = yes.no.labels, levels = c(1,0))
haare.factor68 <- factor(data68$haare, labels = yes.no.labels, levels = c(1,0))
arbeitsschuhe.factor68 <- factor(data68$arbeitsschuhe, labels = yes.no.labels, levels = c(1,0))
kleidung_aufb.factor68 <- factor(data68$kleidung_aufb, labels = yes.no.labels, levels = c(1,0))
desinfizieren.factor68 <- factor(data68$desinfizieren, labels = yes.no.labels, levels = c(1,0))
schutzbrille.factor68 <- factor(data68$schutzbrille, labels = yes.no.labels, levels = c(1,0))
kleidung_wohnraum.factor68 <- factor(data68$kleidung_wohnraum, labels = yes.no.labels, levels = c(1,0))
arbeitskleidung3_letzteWoche.factor68 <- factor(data68$arbeitskleidung3_letzteWoche)
haare3_letzteWoche.factor68 <- factor(data68$haare3_letzteWoche)
aufbewahrung3_letzteWoche.factor68 <- factor(data68$aufbewahrung3_letzteWoche)
arbeitskleidung3_intention.factor68 <- factor(data68$arbeitskleidung3_intention)
haare3_intention.factor68 <- factor(data68$haare3_intention)
aufbewahrung3_intention.factor68 <- factor(data68$aufbewahrung3_intention)
arbeitskleidung3_einstellung.factor68 <- factor(data68$arbeitskleidung3_einstellung)
haare3_einstellung.factor68 <- factor(data68$haare3_einstellung)
aufbewahrung3_einstellung.factor68 <- factor(data68$aufbewahrung3_einstellung)
arbeitskleidung3_norm.factor68 <- factor(data68$arbeitskleidung3_norm)
haare3_norm.factor68 <- factor(data68$haare3_norm)
haare3_norm.factor68 <- factor(haare3_norm.factor68, levels = c(1,2,3))
aufbewahrung3_norm.factor68 <- factor(data68$aufbewahrung3_norm)
arbeitskleidung3_kontrolle.factor68 <- factor(data68$arbeitskleidung3_kontrolle)
haare3_kontrolle.factor68 <- factor(data68$haare3_kontrolle)
aufbewahrung3_kontrolle.factor68 <- factor(data68$aufbewahrung3_kontrolle)
allergie3_bekommen.factor68 <- factor(data68$allergie3_bekommen)
allergie3_schlimm.factor68 <- factor(data68$allergie3_schlimm)

# data49
sex.factor49 <- factor(data49$sex, labels = sex.labels)
studienort.factor49 <- factor(data49$studienort, labels = studienort.labels)
alterkat.factor49 <- factor(data49$alterkat,levels=c("1","2","3"),labels=alterkat.labels)
schulabschluss.factor49 <- factor(data49$schulabschluss.factor, labels=schulabschluss.labels)
smoking.status.factor49 <- factor(data49$smoking.status, labels = smoking.status.labels)
pfeifen.factor49 <- factor(data49$pfeifen, labels = yes.no.labels, levels = c(1,0))
pfeifen_ohne_erk.factor49 <- factor(data49$pfeifen_ohne_erk, labels = yes.no.kein.labels, levels = c(1,0,2))
asthma.factor49 <- factor(data49$asthma, labels = yes.no.labels, levels = c(1,0))
asthma_arzt.factor49 <- factor(data49$asthma_arzt, labels = yes.no.kein.labels, levels = c(1,0,2))
medikamente.factor49 <- factor(data49$medikamente, labels = yes.no.kein.labels, levels = c(1,0,2))
nasenprobleme.factor49 <- factor(data49$nasenprobleme, labels = yes.no.labels, levels = c(1,0))
nasenprobleme_augen.factor49 <- factor(data49$nasenprobleme_augen, labels = yes.no.kein.labels, levels = c(1,0,2))
allergie_mutter.factor49 <- factor(data49$allergie_mutter, labels = yes.no.weissnicht.labels, levels = c(1,0,2))
allergie_vater.factor49 <- factor(data49$allergie_vater, labels = yes.no.weissnicht.labels, levels = c(1,0,2))
asthma3.factor49 <- factor(data49$asthma3, labels = yes.no.labels, levels = c(1,0))
asthma.dx.factor49 <- factor(data49$asthma.dx, labels = yes.no.labels, levels = c(1,0))
allergies.factor49 <- factor(data49$allergies, labels = yes.no.labels, levels = c(1,0))
par.asthma.factor49 <- factor(data49$par.asthma, labels = yes.no.labels, levels = c(1,0))
haare.factor49 <- factor(data49$haare, labels = yes.no.labels, levels = c(1,0))
arbeitsschuhe.factor49 <- factor(data49$arbeitsschuhe, labels = yes.no.labels, levels = c(1,0))
kleidung_aufb.factor49 <- factor(data49$kleidung_aufb, labels = yes.no.labels, levels = c(1,0))
desinfizieren.factor49 <- factor(data49$desinfizieren, labels = yes.no.labels, levels = c(1,0))
schutzbrille.factor49 <- factor(data49$schutzbrille, labels = yes.no.labels, levels = c(1,0))
kleidung_wohnraum.factor49 <- factor(data49$kleidung_wohnraum, labels = yes.no.labels, levels = c(1,0))
arbeitskleidung3_letzteWoche.factor49 <- factor(data49$arbeitskleidung3_letzteWoche)
haare3_letzteWoche.factor49 <- factor(data49$haare3_letzteWoche)
aufbewahrung3_letzteWoche.factor49 <- factor(data49$aufbewahrung3_letzteWoche)
arbeitskleidung3_intention.factor49 <- factor(data49$arbeitskleidung3_intention)
haare3_intention.factor49 <- factor(data49$haare3_intention)
aufbewahrung3_intention.factor49 <- factor(data49$aufbewahrung3_intention)
arbeitskleidung3_einstellung.factor49 <- factor(data49$arbeitskleidung3_einstellung)
haare3_einstellung.factor49 <- factor(data49$haare3_einstellung)
haare3_einstellung.factor49 <- factor(haare3_einstellung.factor49, levels = c(1,2,3))
aufbewahrung3_einstellung.factor49 <- factor(data49$aufbewahrung3_einstellung)
arbeitskleidung3_norm.factor49 <- factor(data49$arbeitskleidung3_norm)
haare3_norm.factor49 <- factor(data49$haare3_norm)
aufbewahrung3_norm.factor49 <- factor(data49$aufbewahrung3_norm)
arbeitskleidung3_kontrolle.factor49 <- factor(data49$arbeitskleidung3_kontrolle)
haare3_kontrolle.factor49 <- factor(data49$haare3_kontrolle)
aufbewahrung3_kontrolle.factor49 <- factor(data49$aufbewahrung3_kontrolle)
allergie3_bekommen.factor49 <- factor(data49$allergie3_bekommen)
allergie3_schlimm.factor49 <- factor(data49$allergie3_schlimm)

#----------------------------#
# Sociodemographic analysis: #
#----------------------------#

# Note: all code related to labels was deleted. If needed, go to file 2.
# This next part is again in file 6.

#---
# data121
nrow(data121) # n

# Sex
rbind(sex.demog,cbind(table(sex.factor121),round(prop.table(table(sex.factor121)),4))) # sex
sum(is.na(sex.factor121))

# Age
rbind(age.demog.long,cbind(round(mean(data121$alter, na.rm=T),2),round(sd(data121$alter, na.rm=T),2))) # age longitudinal
# sum(is.na(alterkat.factor121))

rbind(age.demog.cat,cbind(table(alterkat.factor121),round(prop.table(table(alterkat.factor121)),4))) # age categorical
# sum(is.na(alterkat.factor121))

# Plots to look at age distribution: (optional) 
# hist(data121$alter, breaks = 37)
# boxplot(data121$alter)
# plot(data121$alter)
# abline(h=21)
# abline(h=25)
# plot(alterkat.factor121)

# Studienort
rbind(studienort.demog,cbind(table(studienort.factor121),round(prop.table(table(studienort.factor121)),4))) # studienort
# sum(is.na(studienort.factor121))

# Schulabschluss
rbind(schulabschluss.demog,cbind(table(schulabschluss.factor121),round(prop.table(table(schulabschluss.factor121)),4))) # schulabschluss
# sum(is.na(schulabschluss.factor121))

#---#
# Smoker
rbind(smoker.demog,cbind(table(smoking.status.factor121),round(prop.table(table(smoking.status.factor121)),4))) # smoker
# sum(is.na(smoking.status.factor121))

#---

#---
# data117
nrow(data117) # n

# Sex
rbind(sex.demog,cbind(table(sex.factor117),round(prop.table(table(sex.factor117)),4))) # sex
sum(is.na(sex.factor117))

# Age
rbind(age.demog.long,cbind(round(mean(data117$alter, na.rm=T),2),round(sd(data117$alter, na.rm=T),2))) # age longitudinal
# sum(is.na(alterkat.factor117))

rbind(age.demog.cat,cbind(table(alterkat.factor117),round(prop.table(table(alterkat.factor117)),4))) # age categorical
# sum(is.na(alterkat.factor117))

# Plots to look at age distribution: (optional) 
# hist(data117$alter, breaks = 37)
# boxplot(data117$alter)
# plot(data117$alter)
# abline(h=21)
# abline(h=25)
# plot(alterkat.factor117)

# Studienort
rbind(studienort.demog,cbind(table(studienort.factor117),round(prop.table(table(studienort.factor117)),4))) # studienort
# sum(is.na(studienort.factor117))

# Schulabschluss
rbind(schulabschluss.demog,cbind(table(schulabschluss.factor117),round(prop.table(table(schulabschluss.factor117)),4))) # schulabschluss
# sum(is.na(schulabschluss.factor117))

# Smoker
rbind(smoker.demog,cbind(table(smoking.status.factor117),round(prop.table(table(smoking.status.factor117)),4))) # smoker
# sum(is.na(smoking.status.factor117))

#---

#---
# data68
nrow(data68) # n

# Sex
rbind(sex.demog,cbind(table(sex.factor68),round(prop.table(table(sex.factor68)),4))) # sex
sum(is.na(sex.factor68))

# Age
rbind(age.demog.long,cbind(round(mean(data68$alter, na.rm=T),2),round(sd(data68$alter, na.rm=T),2))) # age longitudinal
# sum(is.na(alterkat.factor68))

rbind(age.demog.cat,cbind(table(alterkat.factor68),round(prop.table(table(alterkat.factor68)),4))) # age categorical
# sum(is.na(alterkat.factor68))

# Plots to look at age distribution: (optional) 
# hist(data68$alter, breaks = 37)
# boxplot(data68$alter)
# plot(data68$alter)
# abline(h=21)
# abline(h=25)
# plot(alterkat.factor68)

# Studienort
rbind(studienort.demog,cbind(table(studienort.factor68),round(prop.table(table(studienort.factor68)),4))) # studienort
# sum(is.na(studienort.factor68))

# Schulabschluss
rbind(schulabschluss.demog,cbind(table(schulabschluss.factor68),round(prop.table(table(schulabschluss.factor68)),4))) # schulabschluss
# sum(is.na(schulabschluss.factor68))

# Smoker
rbind(smoker.demog,cbind(table(smoking.status.factor68),round(prop.table(table(smoking.status.factor68)),4))) # smoker
# sum(is.na(smoking.status.factor68))

#---

#---
# data49
nrow(data49) # n

# Sex
rbind(sex.demog,cbind(table(sex.factor49),round(prop.table(table(sex.factor49)),4))) # sex
sum(is.na(sex.factor49))

# Age
rbind(age.demog.long,cbind(round(mean(data49$alter, na.rm=T),2),round(sd(data49$alter, na.rm=T),2))) # age longitudinal
# sum(is.na(alterkat.factor49))

rbind(age.demog.cat,cbind(table(alterkat.factor49),round(prop.table(table(alterkat.factor49)),4))) # age categorical
# sum(is.na(alterkat.factor49))

# Plots to look at age distribution: (optional) 
# hist(data49$alter, breaks = 37)
# boxplot(data49$alter)
# plot(data49$alter)
# abline(h=21)
# abline(h=25)
# plot(alterkat.factor49)

# Studienort
rbind(studienort.demog,cbind(table(studienort.factor49),round(prop.table(table(studienort.factor49)),4))) # studienort
# sum(is.na(studienort.factor49))

# Schulabschluss
rbind(schulabschluss.demog,cbind(table(schulabschluss.factor49),round(prop.table(table(schulabschluss.factor49)),4))) # schulabschluss
# sum(is.na(schulabschluss.factor49))

# Smoker
rbind(smoker.demog,cbind(table(smoking.status.factor49),round(prop.table(table(smoking.status.factor49)),4))) # smoker
# sum(is.na(smoking.status.factor49))

#---


#---------------#
# END OF SCRIPT #
#---------------#