# R code for implementation
# Author: Daloha Rodriguez-Molina
# This code is available electronically at https://github.com/darokun/R---txt2paul/blob/master/23.%20R%20code%20for%20implementation.R

#---
# Loading required packages
# library(psych)
# library(modeltools)
# library(mvtnorm)
# library(coin)
library(SuperLearner)
library(tmle)
library(devtools)
library(httr)
install_github("lendle/tmlecte")
library(tmlecte)

# Setting up the working directory
setwd("~/Dropbox/R - txt2paul")

# Reading in the data
d <- "/Users/daro/Desktop/txt2PAUL/z_Dokumente fuer Daloha/txt2PAUL_Gesamt_vers1_130115.csv"
data <- read.csv(d, header=T)
#---

#---
# SOCIODEMOGRAPHIC AND MEDICAL VARIABLES

# create labels
sex.labels <- c("male","female")
studienort.labels <- c("Miesbach","Rosenheim", "Muehldorf", "Traunstein", "Schoengeising", "Andechs", "Erding", "Bad Heilbrunn", "Ebersberg", "Ingolstadt")
alterkat.labels <- c("18-20", "21-24", "25-44")
no.yes.labels <- c("No", "Yes")
no.yes.kein.labels <- c("No", "Yes", "Kein")
no.yes.weissnicht.labels <- c("No", "Yes","Don't know")
no.yes.weissnicht.labels <- c("No", "Yes","Don't know")
yes.no.labels <- c("Yes", "No")
yes.no.kein.labels <- c("Yes", "No", "Kein")
yes.no.weissnicht.labels <- c("Yes", "No", "Don't know")

# Convert to factors
data$sex.factor <- factor(data$sex, labels = sex.labels)
data$studienort.factor <- factor(data$studienort, labels = studienort.labels)
alterkat.factor <- factor(data$alterkat,levels=c("1","2","3"),labels=alterkat.labels)

# age categorical
labels.alterkat2 <- c("younger", "older")
data$alterkat2 <- cut(data$alter, breaks = c(17,24,44), labels = labels.alterkat2)

# Schulabschluss
schulabschluss.factor <- NULL
schulabschluss.factor[data$schulabschluss==0] <- 0
schulabschluss.factor[data$schulabschluss==1] <- 1
schulabschluss.factor[data$schulabschluss==2 | data$schulabschluss==3] <- 2
schulabschluss.factor[data$schulabschluss==4] <- 3
schulabschluss.labels <- c("Hauptschulabschluss", "Realschulabschluss", "Fachhochschulreife + Abitur", "anderer Abschluss")
data$schulabschluss.factor <- factor(schulabschluss.factor, labels=schulabschluss.labels, levels = c(0,1,2,3))

# Modified Schulabschluss
levels(data$schulabschluss.factor)
data$schulabschluss.factor.twolevels <- NULL
data$schulabschluss.factor.twolevels[
  data$schulabschluss.factor=="Hauptschulabschluss"] <-1
data$schulabschluss.factor.twolevels[
  data$schulabschluss.factor=="Realschulabschluss"] <- 2
data$schulabschluss.factor.twolevels[
  data$schulabschluss.factor=="Fachhochschulreife + Abitur"] <- 2 
length(data$schulabschluss.factor.twolevels)

labels.schulabschluss.twolevels <- c("Hauptschulabschluss","Realschulab. + Fachhochsch. + Abitur")
data$schulabschluss.factor.twolevels <- factor(data$schulabschluss.factor.twolevels,
                                               labels = labels.schulabschluss.twolevels)

# smoking.status
smoking.status <- NULL 
smoking.status[data$rauchen==0 | data$rauchen_aktuell==0] <- 0
smoking.status[data$rauchen==1 | data$rauchen_aktuell==1] <- 1
smoking.status.labels <- c("Non-smoker", "Smoker")
smoking.status.factor <- factor(smoking.status, labels = smoking.status.labels, levels=c(0,1))

# Risk Perception
# join allergie_bekommen and allergie_schlimm into just one variable: risk_perception
# 1. create data$allergie_bekommen_reverse, reversing the score into the correct direction
# The correct direction should be: 1 (likely) = high risk perception level, 5 (unlikely) = low risk perception level

data$allergie_bekommen_reverse <- NULL
for(i in 1:length(data$allergie_bekommen)) {
  if(data$allergie_bekommen[i]==1) {
    data$allergie_bekommen_reverse[i] <- 5
  }
  if(data$allergie_bekommen[i]==2) {
    data$allergie_bekommen_reverse[i] <- 4
  }
  if(data$allergie_bekommen[i]==3) {
    data$allergie_bekommen_reverse[i] <- 3
  }
  if(data$allergie_bekommen[i]==4) {
    data$allergie_bekommen_reverse[i] <- 2
  }
  if(data$allergie_bekommen[i]==5) {
    data$allergie_bekommen_reverse[i] <- 1
  }
}

# 2. sum up data$allergie_bekommen_reverse + data$allergie_schlimm into a new vector (rp)
rp <- NULL
rp <- data$allergie_bekommen_reverse + data$allergie_schlimm

# 3. Determine the 75th percentile for rp
quantile(rp, 0.75) # 7

# 4. Define those at a high level of risk perception (>=7), and add to new variable data$risk_perception, where 0=ref level=high risk perception level and 1=low risk perception level.
# The ideal is that people have a higher risk perception level.
data$risk_perception <- NULL

for(i in 1:length(rp)) {
  if(rp[i]<7) {
    data$risk_perception[i] <- 0
  }
  else {
    data$risk_perception[i] <- 1
  }
}

# 5. convert data$risk_perception into factor, and add labels
data$risk_perception.factor <- factor(data$risk_perception, labels=c("high risk perception level", "low risk perception level"))

# asthma into 3 levels
asthma3 <- NULL
asthma3[data$pfeifen_ohne_erk==0 | data$medikamente==0 | data$pfeifen_ohne_erk==2 | data$medikamente==2] <- 0
asthma3[data$pfeifen_ohne_erk==1 | data$medikamente==1] <- 1
data$asthma3 <- asthma3
data$asthma3.factor <- factor(asthma3, labels = no.yes.labels)

# asthma diagnosis
asthma.dx <- NULL
asthma.dx[data$asthma==0 | data$asthma_arzt==0 | data$asthma_arzt==2] <- 0
asthma.dx[data$asthma==1 | data$asthma_arzt==1] <- 1
data$asthma.dx <- asthma.dx
asthma.dx.factor <- factor(asthma.dx, labels = no.yes.labels)
sum(data$asthma==0, na.rm=T)
sum(data$asthma_arzt==0, na.rm=T)
sum(data$asthma_arzt==2, na.rm=T)
sum(data$asthma.dx==0, na.rm=T)
which(data$asthma_arzt==0) #45
sum(data$asthma_arzt==0, na.rm=T)[45]

# allergies
allergies <- NULL
allergies[data$nasenprobleme==0 | data$nasenprobleme==0 | data$nasenprobleme_augen==2] <- 0
allergies[data$nasenprobleme==1 | data$nasenprobleme_augen==1] <- 1
data$allergies <- allergies
data$allergies.factor <- factor(allergies, labels = no.yes.labels)

# asthma symptoms or rhinoconjunctivitis:
data$asthma.or.rhinoconj.factor <- NULL

for(i in 1:length(data$asthma3.factor)) {
  if (data$asthma3.factor[i]=="Yes" |
      data$allergies.factor[i]=="Yes") {
    data$asthma.or.rhinoconj.factor[i] <- 1
  } else {
    data$asthma.or.rhinoconj.factor[i] <- 0
  }
}
labels.asthma.rhin <- c("No", "Yes")
data$asthma.or.rhinoconj.factor <- factor(
  data$asthma.or.rhinoconj.factor, labels = labels.asthma.rhin)

# parental asthma
par.asthma <- NULL
par.asthma[data$allergie_mutter==0 | data$allergie_vater==0 | data$allergie_mutter==2 | data$allergie_vater==2] <- 0
par.asthma[data$allergie_mutter==1 | data$allergie_vater==1] <- 1
data$par.asthma <- par.asthma

# convert to factors:
pfeifen.factor <- factor(data$pfeifen, labels = no.yes.labels)
pfeifen_ohne_erk.factor <- factor(data$pfeifen_ohne_erk, labels = no.yes.kein.labels)
asthma.factor <- factor(data$asthma, labels = no.yes.labels)
asthma_arzt.factor <- factor(data$asthma_arzt, labels = no.yes.kein.labels)
medikamente.factor <- factor(data$medikamente, labels = no.yes.kein.labels)
nasenprobleme.factor <- factor(data$nasenprobleme, labels = no.yes.labels)
nasenprobleme_augen.factor <- factor(data$nasenprobleme_augen, labels = no.yes.kein.labels)
allergie_mutter.factor <- factor(data$allergie_mutter, labels = no.yes.weissnicht.labels)
allergie_vater.factor <- factor(data$allergie_vater, labels = no.yes.weissnicht.labels)
asthma.dx.factor <- factor(asthma.dx, labels = no.yes.labels)
allergies.factor <- factor(allergies, labels = no.yes.labels)
par.asthma.factor <- factor(par.asthma, labels = no.yes.labels)

# convert to factors, while applying yes/no inversion as needed:
pfeifen.factor <- factor(data$pfeifen, labels = yes.no.labels, levels = c(1,0))
pfeifen_ohne_erk.factor <- factor(data$pfeifen_ohne_erk, labels = yes.no.kein.labels, levels = c(1,0,2))
asthma.factor <- factor(data$asthma, labels = yes.no.labels, levels = c(1,0))
asthma_arzt.factor <- factor(data$asthma_arzt, labels = yes.no.kein.labels, levels = c(1,0,2))
medikamente.factor <- factor(data$medikamente, labels = yes.no.kein.labels, levels = c(1,0,2))
nasenprobleme.factor <- factor(data$nasenprobleme, labels = yes.no.labels, levels = c(1,0))
nasenprobleme_augen.factor <- factor(data$nasenprobleme_augen, labels = yes.no.kein.labels, levels = c(1,0,2))
allergie_mutter.factor <- factor(data$allergie_mutter, labels = yes.no.weissnicht.labels, levels = c(1,0,2))
allergie_vater.factor <- factor(data$allergie_vater, labels = yes.no.weissnicht.labels, levels = c(1,0,2))
asthma3.factor <- factor(asthma3, labels = yes.no.labels, levels = c(1,0))
asthma.dx.factor <- factor(asthma.dx, labels = yes.no.labels, levels = c(1,0))
allergies.factor <- factor(allergies, labels = yes.no.labels, levels = c(1,0))
par.asthma.factor <- factor(par.asthma, labels = yes.no.labels, levels = c(1,0))
data$smoking.status.factor <- smoking.status.factor
data$smoking.status <- smoking.status
data$asthma3.factor <- asthma3.factor
data$asthma.dx.factor <- asthma.dx
data$allergies.factor <- allergies.factor
data$par.asthma.factor <- par.asthma.factor

# Relevel variables:
data$sex.factor <- relevel(data$sex.factor, ref = "female")
# data$asthma3.factor <- relevel(data$asthma3.factor, ref = "No")
# data$allergies.factor <- relevel(data$allergies.factor, ref = "No")
data$par.asthma.factor <- relevel(data$par.asthma.factor, ref = "No")
data$schulabschluss.factor.twolevels <- relevel(data$schulabschluss.factor.twolevels, ref = "Hauptschulabschluss")

# knowledge at baseline (one of the covariates)
# all 6 measures right
# create new variables to store all correct answers as 1.
data$haare.correct <- data$haare
data$arbeitsschuhe.correct <- NULL
data$kleidung_aufb.correct <- data$kleidung_aufb
data$desinfizieren.correct <- NULL
data$schutzbrille.correct <- NULL
data$kleidung_wohnraum.correct <- data$kleidung_wohnraum

# arbeitsschuhe
for (i in 1:length(data$arbeitsschuhe)) {
  if(data$arbeitsschuhe[i]==0) {
    data$arbeitsschuhe.correct[i] <- 1
  }  else {
    data$arbeitsschuhe.correct[i] <- 0
  }
}

# desinfizieren
for (i in 1:length(data$desinfizieren)) {
  if(data$desinfizieren[i]==0) {
    data$desinfizieren.correct[i] <- 1
  }  else {
    data$desinfizieren.correct[i] <- 0
  }
}

# schutzbrille
for (i in 1:length(data$schutzbrille)) {
  if(data$schutzbrille[i]==0) {
    data$schutzbrille.correct[i] <- 1
  }  else {
    data$schutzbrille.correct[i] <- 0
  }
}

# construct vector of 6 measures
data$all_wissen <- NULL
data$all_wissen[data$haare.correct==1 & 
                  data$arbeitsschuhe.correct==1 &
                  data$kleidung_aufb.correct==1 &
                  data$desinfizieren.correct==1 &
                  data$schutzbrille.correct==1 &
                  data$kleidung_wohnraum.correct==1] <- 1
data$all_wissen[is.na(data$all_wissen)] <- 0
all_wissen.labels <- c("incorrect", "correct")
data$all_wissen.factor <- factor(data$all_wissen, labels = all_wissen.labels)
# data$all_wissen==1: all positive measures
# data$all_wissen==0: not all positive measures

# knowledge 0-5 correct
data$all_wissen5 <- NULL
data$all_wissen4 <- NULL
data$all_wissen3 <- NULL
data$all_wissen2 <- NULL
data$all_wissen1 <- NULL
data$all_wissen0 <- NULL

data$check <- apply(data[,c("haare.correct", "kleidung_aufb.correct",
                            "kleidung_wohnraum.correct", "arbeitsschuhe.correct",
                            "desinfizieren.correct", "schutzbrille.correct")],1,sum)
count.only6 <- sum(data$check==6) # all 6 correct
count.only5 <- sum(data$check==5) # just 5 correct
count.only4 <- sum(data$check==4) # just 4 correct
count.only3 <- sum(data$check==3) # just 3 correct
count.only2 <- sum(data$check==2) # just 2 correct
count.only1 <- sum(data$check==1) # just 1 correct
count.zero <- sum(data$check==0) # none correct

# 5 measures
for (i in 1:length(data$check)) {
  if(data$check[i]>=5) {
    data$all_wissen5[i] <- 1
  } else {
    data$all_wissen5[i] <- 0
  }
}

data$all_wissen5.factor <- factor(data$all_wissen5, labels = all_wissen.labels)
# data$all_wissen==1: at least 5 positive measures
# data$all_wissen==0: not at least 5 positive measures


# 4 measures
for (i in 1:length(data$check)) {
  if(data$check[i]>=4) {
    data$all_wissen4[i] <- 1
  } else {
    data$all_wissen4[i] <- 0
  }
}
data$all_wissen4.factor <- factor(data$all_wissen4, labels = all_wissen.labels)
# data$all_wissen==1: at least 4 positive measures
# data$all_wissen==0: not at least 4 positive measures
#---

#---
# PREPARE DATASET
# create new dataset without NAs 
which(is.na(data$all_wissen.factor)) #0
which(is.na(data$sex.factor)) # 162
which(is.na(data$alterkat2)) #43, 45
which(is.na(data$smoking.status.factor)) #0
which(is.na(data$schulabschluss.factor.twolevels)) # 45, 71, 83, 134, 195
which(is.na(data$asthma.or.rhinoconj.factor)) #0
which(is.na(data$par.asthma.factor)) #2
# delete: 2, 43, 45, 71, 83, 134, 162, 195. 

data.wo.nas <- data[-c(2, 43, 45, 71, 83, 134, 162, 195),]
#---

#---
# COMPLETED INTERVENTION
# I create a data.wo.nas$completed variable, which has answers 0=no and 1=yes, on whether or not they completed the intervention phase (either in the control or in the experimental group)

nachher.table <- cbind(data.wo.nas$haare_nachher, data.wo.nas$arbeitsschuhe_nachher, data.wo.nas$kleidung_aufb_nachher, data.wo.nas$desinfizieren_nachher, data.wo.nas$schutzbrille_nachher, data.wo.nas$kleidung_wohnraum_nachher)

data.wo.nas$completed <- rowSums(nachher.table)
for(i in 1:length(data.wo.nas$completed)) {
  if(is.na(rowSums(nachher.table)[i])==T) {
    data.wo.nas$completed[i] <- 0
  }
  else {
    data.wo.nas$completed[i] <- 1
  }
}
data.wo.nas$completed[1:114] <- NA

# setting up the four groups:
# all from the intervention==1
# group 1: control, dropouts
# group 2: control, completed
# group 3: experimental, dropouts
# group 4: experimental, completed

#Four groups
data.wo.nas$four.groups <- NULL
data.wo.nas$four.groups[
  data.wo.nas$intervention==1 & data.wo.nas$gruppe==0 & data.wo.nas$completed==0] <- 1

data.wo.nas$four.groups[
  data.wo.nas$intervention==1 & data.wo.nas$gruppe==0 & data.wo.nas$completed==1] <- 2

data.wo.nas$four.groups[
  data.wo.nas$intervention==1 & data.wo.nas$gruppe==1 & data.wo.nas$completed==0] <- 3 

data.wo.nas$four.groups[
  data.wo.nas$intervention==1 & data.wo.nas$gruppe==1 & data.wo.nas$completed==1] <- 4

data.wo.nas$four.groups
table(data.wo.nas$four.groups)

group1 <- data.wo.nas$four.groups==1
group2 <- data.wo.nas$four.groups==2
group3 <- data.wo.nas$four.groups==3
group4 <- data.wo.nas$four.groups==4

# Two groups by level (dropouts and experimental)
# controls
data.wo.nas$two.groups.control <- NULL
data.wo.nas$two.groups.control[
  data.wo.nas$intervention==1 & data.wo.nas$gruppe==0 & data.wo.nas$completed==1] <- 0
data.wo.nas$two.groups.control[
  data.wo.nas$intervention==1 & data.wo.nas$gruppe==0 & data.wo.nas$completed==0] <- 1
table(data.wo.nas$two.groups.control)

# intervention (experimental)
data.wo.nas$two.groups.exp <- NULL
data.wo.nas$two.groups.exp[
  data.wo.nas$intervention==1 & data.wo.nas$gruppe==1 & data.wo.nas$completed==1] <- 0
data.wo.nas$two.groups.exp[
  data.wo.nas$intervention==1 & data.wo.nas$gruppe==1 & data.wo.nas$completed==0] <- 1
table(data.wo.nas$two.groups.exp)
#---


#---
# OUTCOME
# Knowledge at follow-up

# data with 116 subjects (post-intervention; follow-up)
int.data.wo.nas <- data.wo.nas[115:230,]

##########################
### all_wissen_nachher ###
##########################

# WISSEN NACHHER
# all wissen 6 right

# create new variables to store all correct answers as 1.
int.data.wo.nas$haare_nachher.correct <- int.data.wo.nas$haare_nachher
int.data.wo.nas$arbeitsschuhe_nachher.correct <- NULL
int.data.wo.nas$kleidung_aufb_nachher.correct <- int.data.wo.nas$kleidung_aufb_nachher
int.data.wo.nas$desinfizieren_nachher.correct <- NULL
int.data.wo.nas$schutzbrille_nachher.correct <- NULL
int.data.wo.nas$kleidung_wohnraum_nachher.correct <- int.data.wo.nas$kleidung_wohnraum_nachher

# arbeitsschuhe
int.data.wo.nas$arbeitsschuhe_nachher.correct <- NULL
for (i in 1:length(int.data.wo.nas$arbeitsschuhe_nachher)) {
  if(is.na(int.data.wo.nas$arbeitsschuhe_nachher[i])) {
    int.data.wo.nas$arbeitsschuhe_nachher.correct[i] <- NA
  }
  else if(int.data.wo.nas$arbeitsschuhe_nachher[i]==0) {
    int.data.wo.nas$arbeitsschuhe_nachher.correct[i] <- 1
  }  
  else {
    int.data.wo.nas$arbeitsschuhe_nachher.correct[i] <- 0
  }
}

# desinfizieren
int.data.wo.nas$desinfizieren_nachher.correct <- NULL
for (i in 1:length(int.data.wo.nas$desinfizieren_nachher)) {
  if(is.na(int.data.wo.nas$desinfizieren_nachher[i])) {
    int.data.wo.nas$desinfizieren_nachher.correct[i] <- NA
  }
  else if(int.data.wo.nas$desinfizieren_nachher[i]==0) {
    int.data.wo.nas$desinfizieren_nachher.correct[i] <- 1
  }  
  else {
    int.data.wo.nas$desinfizieren_nachher.correct[i] <- 0
  }
}

# schutzbrille
int.data.wo.nas$schutzbrille_nachher.correct <- NULL
for (i in 1:length(int.data.wo.nas$schutzbrille_nachher)) {
  if(is.na(int.data.wo.nas$schutzbrille_nachher[i])) {
    int.data.wo.nas$schutzbrille_nachher.correct[i] <- NA
  }
  else if(int.data.wo.nas$schutzbrille_nachher[i]==0) {
    int.data.wo.nas$schutzbrille_nachher.correct[i] <- 1
  }  
  else {
    int.data.wo.nas$schutzbrille_nachher.correct[i] <- 0
  }
}

# table all wissen
# cbind(
#   int.data.wo.nas$haare_nachher.correct, 
#   int.data.wo.nas$arbeitsschuhe_nachher.correct, 
#   int.data.wo.nas$kleidung_aufb_nachher.correct, 
#   int.data.wo.nas$desinfizieren_nachher.correct, 
#   int.data.wo.nas$schutzbrille_nachher.correct, 
#   int.data.wo.nas$kleidung_wohnraum_nachher.correct,
#   int.data.wo.nas$gruppe
# )

# construct all wissen
int.data.wo.nas$all_wissen_nachher <- NULL
for(i in 1:length(int.data.wo.nas$haare_nachher.correct)) {
  if( is.na(int.data.wo.nas$haare_nachher.correct)[i] &
      is.na(int.data.wo.nas$arbeitsschuhe_nachher.correct)[i] &
      is.na(int.data.wo.nas$kleidung_aufb_nachher.correct)[i] &
      is.na(int.data.wo.nas$desinfizieren_nachher.correct)[i] &
      is.na(int.data.wo.nas$schutzbrille_nachher.correct)[i] &
      is.na(int.data.wo.nas$kleidung_wohnraum_nachher.correct)[i]
  ) {
    int.data.wo.nas$all_wissen_nachher[i] <- NA
  }
  
  else if(  int.data.wo.nas$haare_nachher.correct[i]==1 & 
            int.data.wo.nas$arbeitsschuhe_nachher.correct[i]==1 &
            int.data.wo.nas$kleidung_aufb_nachher.correct[i]==1 &
            int.data.wo.nas$desinfizieren_nachher.correct[i]==1 &
            int.data.wo.nas$schutzbrille_nachher.correct[i]==1 &
            int.data.wo.nas$kleidung_wohnraum_nachher.correct[i]==1) {
    int.data.wo.nas$all_wissen_nachher[i] <- 1
  }
  
  else {
    int.data.wo.nas$all_wissen_nachher[i] <- 0
  }
}

int.data.wo.nas$all_wissen_nachher[is.na(int.data.wo.nas$all_wissen_nachher)] <- NA
all_wissen.labels <- c("incorrect", "correct")
int.data.wo.nas$all_wissen_nachher.factor <- factor(int.data.wo.nas$all_wissen_nachher, labels = all_wissen.labels)
# int.data.wo.nas$all_wissen_nachher==1: all positive measures
# int.data.wo.nas$all_wissen_nachher==0: not all positive measures

# all wissen 0-5 correct
int.data.wo.nas$all_wissen_nachher5 <- NULL
int.data.wo.nas$all_wissen_nachher4 <- NULL
int.data.wo.nas$all_wissen_nachher3 <- NULL
# int.data.wo.nas$all_wissen_nachher2 <- NULL
# int.data.wo.nas$all_wissen_nachher1 <- NULL
# int.data.wo.nas$all_wissen_nachher0 <- NULL

x <- cbind(
  int.data.wo.nas$haare_nachher.correct, 
  int.data.wo.nas$arbeitsschuhe_nachher.correct, 
  int.data.wo.nas$kleidung_aufb_nachher.correct, 
  int.data.wo.nas$desinfizieren_nachher.correct, 
  int.data.wo.nas$schutzbrille_nachher.correct, 
  int.data.wo.nas$kleidung_wohnraum_nachher.correct
)
check <- rowSums(x, na.rm=TRUE)
check[check==0] <- NA
count.only6 <- sum(check==6, na.rm=T) # all 6 correct ## 6
count.only5 <- sum(check==5, na.rm=T) # just 5 correct ## 16
count.only4 <- sum(check==4, na.rm=T) # just 4 correct ## 14
count.only3 <- sum(check==3, na.rm=T) # just 3 correct ## 8 
count.only2 <- sum(check==2, na.rm=T) # just 2 correct ## 3
count.only1 <- sum(check==1, na.rm=T) # just 1 correct ## 0
count.zero <- sum(check==0, na.rm=T) # none correct ## 0

# 5
int.data.wo.nas$all_wissen_nachher5 <- NULL
for (i in 1:length(check)) {
  if(is.na(check)[i]) {
    int.data.wo.nas$all_wissen_nachher5[i] <- NA
  }
  else if(check[i]>=5) {
    int.data.wo.nas$all_wissen_nachher5[i] <- 1
  } 
  else {
    int.data.wo.nas$all_wissen_nachher5[i] <- 0
  }
}

int.data.wo.nas$all_wissen_nachher5.factor <- factor(int.data.wo.nas$all_wissen_nachher5, labels = all_wissen.labels)
# int.data.wo.nas$all_wissen_nachher==1: at least 5 positive measures
# int.data.wo.nas$all_wissen_nachher==0: not at least 5 positive measures

# 4
int.data.wo.nas$all_wissen_nachher4 <- NULL
for (i in 1:length(check)) {
  if(is.na(check)[i]) {
    int.data.wo.nas$all_wissen_nachher4[i] <- NA
  }
  else if(check[i]>=4) {
    int.data.wo.nas$all_wissen_nachher4[i] <- 1
  } 
  else {
    int.data.wo.nas$all_wissen_nachher4[i] <- 0
  }
}
int.data.wo.nas$all_wissen_nachher4.factor <- factor(int.data.wo.nas$all_wissen_nachher4, labels = all_wissen.labels)
# int.data.wo.nas$all_wissen_nachher==1: at least 4 positive measures
# int.data.wo.nas$all_wissen_nachher==0: not at least 4 positive measures
cbind(table(int.data.wo.nas$all_wissen_nachher4.factor), round(prop.table(table(int.data.wo.nas$all_wissen_nachher4.factor)),4))
#---

#---
# HOUSEKEEPING
# age
int.data.wo.nas$alterkat2.binary <- NULL
int.data.wo.nas$alterkat2.binary <- as.numeric(int.data.wo.nas$alterkat2)
int.data.wo.nas$alterkat2.binary[int.data.wo.nas$alterkat2.binary==1] <- 0
int.data.wo.nas$alterkat2.binary[int.data.wo.nas$alterkat2.binary==2] <- 1

# asthma or rhinoconjunctivitis
int.data.wo.nas$asthma.or.rhinoconj <- as.numeric(int.data.wo.nas$asthma.or.rhinoconj.factor)
int.data.wo.nas$asthma.or.rhinoconj[int.data.wo.nas$asthma.or.rhinoconj==1] <- 0
int.data.wo.nas$asthma.or.rhinoconj[int.data.wo.nas$asthma.or.rhinoconj==2] <- 1

# Schulabschluss
int.data.wo.nas$schulabschluss.binary <- as.numeric(int.data.wo.nas$schulabschluss.factor.twolevels)
int.data.wo.nas$schulabschluss.binary[int.data.wo.nas$schulabschluss.binary==1] <- 0
int.data.wo.nas$schulabschluss.binary[int.data.wo.nas$schulabschluss.binary==2] <- 1

# knowledge at baseline
int.data.wo.nas$all_wissen.binary <- as.numeric(int.data.wo.nas$all_wissen.factor)
int.data.wo.nas$all_wissen.binary[int.data.wo.nas$all_wissen.binary==1] <- 0
int.data.wo.nas$all_wissen.binary[int.data.wo.nas$all_wissen.binary==2] <- 1

# vector of missings: 1 == observed (not missing), 0 == not observed (missing).
table(int.data.wo.nas$all_wissen_nachher, useNA="always")
69/116 #59.48% of missings

na.vector.all_wissen_nachher <- NULL
for(i in 1:length(int.data.wo.nas$all_wissen_nachher)) {
  if(is.na(int.data.wo.nas$all_wissen_nachher)[i]) {
    na.vector.all_wissen_nachher[i] <- 0
  }
  else {na.vector.all_wissen_nachher[i] <- 1}
}
#---

#---
# TABLE 1
# Total
# age
cbind(table(int.data.wo.nas$alterkat2.binary,
            useNA="always"), 
      prop.table(table(int.data.wo.nas$alterkat2.binary, 
                       useNA="always")))

# sex
cbind(table(int.data.wo.nas$sex.factor,
            useNA="always"), 
      prop.table(table(int.data.wo.nas$sex.factor, 
                       useNA="always")))

# smoking.status
cbind(table(int.data.wo.nas$smoking.status,
            useNA="always"), 
      prop.table(table(int.data.wo.nas$smoking.status, 
                       useNA="always")))

# schulabschluss.binary
cbind(table(int.data.wo.nas$schulabschluss.binary,
            useNA="always"), 
      prop.table(table(int.data.wo.nas$schulabschluss.binary, 
                       useNA="always")))

# risk_perception
cbind(table(int.data.wo.nas$risk_perception,
            useNA="always"), 
      prop.table(table(int.data.wo.nas$risk_perception, 
                       useNA="always")))

# asthma.or.rhinoconj
cbind(table(int.data.wo.nas$asthma.or.rhinoconj,
            useNA="always"), 
      prop.table(table(int.data.wo.nas$asthma.or.rhinoconj, 
                       useNA="always")))

# par.asthma
cbind(table(int.data.wo.nas$par.asthma,
            useNA="always"), 
      prop.table(table(int.data.wo.nas$par.asthma, 
                       useNA="always")))

# all_wissen
cbind(table(int.data.wo.nas$all_wissen,
            useNA="always"), 
      prop.table(table(int.data.wo.nas$all_wissen, 
                       useNA="always")))
# gruppe
cbind(table(int.data.wo.nas$gruppe,
            useNA="always"), 
      prop.table(table(int.data.wo.nas$gruppe, 
                       useNA="always")))

#---
# Follow-up info
# age
# follow-up numbers:
cbind(table(int.data.wo.nas$alterkat2.binary
            [!is.na(int.data.wo.nas$all_wissen_nachher)],
            useNA="always"), 
      prop.table(table(int.data.wo.nas$alterkat2.binary
                       [!is.na(int.data.wo.nas$all_wissen_nachher)],
                       useNA="always")))

# NA numbers:
tempTab.NA <- cbind(table(int.data.wo.nas$alterkat2.binary, 
                          int.data.wo.nas$all_wissen_nachher, 
                          useNA="always"), 
                    prop.table(table(int.data.wo.nas$alterkat2.binary, 
                                     int.data.wo.nas$all_wissen_nachher, 
                                     useNA="always")))

rbind(cbind(tempTab.NA[7], round(tempTab.NA[7]/(tempTab.NA[7]+tempTab.NA[8]),4)),
      cbind(tempTab.NA[8], round(tempTab.NA[8]/(tempTab.NA[7]+tempTab.NA[8]),4)))

# cbind( tempTab[1] + tempTab[4], 
#        round(tempTab[10] + tempTab[13],4),
#        tempTab[7], round(tempTab[16],4))
# cbind( tempTab[2] + tempTab[5], 
#        round(tempTab[11] + tempTab[14],4),
#        tempTab[8], round(tempTab[17],4))

int.data.wo.nas$sex.binary
# sex
# follow-up numbers:
cbind(table(int.data.wo.nas$sex.factor
            [!is.na(int.data.wo.nas$all_wissen_nachher)],
            useNA="always"), 
      prop.table(table(int.data.wo.nas$sex.factor
                       [!is.na(int.data.wo.nas$all_wissen_nachher)],
                       useNA="always")))

# NA numbers:
tempTab.NA <- cbind(table(int.data.wo.nas$sex.factor, 
                          int.data.wo.nas$all_wissen_nachher, 
                          useNA="always"), 
                    prop.table(table(int.data.wo.nas$sex.factor, 
                                     int.data.wo.nas$all_wissen_nachher, 
                                     useNA="always")))

rbind(cbind(tempTab.NA[7], round(tempTab.NA[7]/(tempTab.NA[7]+tempTab.NA[8]),4)),
      cbind(tempTab.NA[8], round(tempTab.NA[8]/(tempTab.NA[7]+tempTab.NA[8]),4)))

# smoking status
# follow-up numbers:
cbind(table(int.data.wo.nas$smoking.status
            [!is.na(int.data.wo.nas$all_wissen_nachher)],
            useNA="always"), 
      prop.table(table(int.data.wo.nas$smoking.status
                       [!is.na(int.data.wo.nas$all_wissen_nachher)],
                       useNA="always")))

# NA numbers:
tempTab.NA <- cbind(table(int.data.wo.nas$smoking.status, 
                          int.data.wo.nas$all_wissen_nachher, 
                          useNA="always"), 
                    prop.table(table(int.data.wo.nas$smoking.status, 
                                     int.data.wo.nas$all_wissen_nachher, 
                                     useNA="always")))

rbind(cbind(tempTab.NA[7], round(tempTab.NA[7]/(tempTab.NA[7]+tempTab.NA[8]),4)),
      cbind(tempTab.NA[8], round(tempTab.NA[8]/(tempTab.NA[7]+tempTab.NA[8]),4)))

# educational level
# follow-up numbers:
cbind(table(int.data.wo.nas$schulabschluss.binary
            [!is.na(int.data.wo.nas$all_wissen_nachher)],
            useNA="always"), 
      prop.table(table(int.data.wo.nas$schulabschluss.binary
                       [!is.na(int.data.wo.nas$all_wissen_nachher)],
                       useNA="always")))

# NA numbers:
tempTab.NA <- cbind(table(int.data.wo.nas$schulabschluss.binary, 
                          int.data.wo.nas$all_wissen_nachher, 
                          useNA="always"), 
                    prop.table(table(int.data.wo.nas$schulabschluss.binary, 
                                     int.data.wo.nas$all_wissen_nachher, 
                                     useNA="always")))

rbind(cbind(tempTab.NA[7], round(tempTab.NA[7]/(tempTab.NA[7]+tempTab.NA[8]),4)),
      cbind(tempTab.NA[8], round(tempTab.NA[8]/(tempTab.NA[7]+tempTab.NA[8]),4)))

# risk perception
# follow-up numbers:
cbind(table(int.data.wo.nas$risk_perception
            [!is.na(int.data.wo.nas$all_wissen_nachher)],
            useNA="always"), 
      prop.table(table(int.data.wo.nas$risk_perception
                       [!is.na(int.data.wo.nas$all_wissen_nachher)],
                       useNA="always")))

# NA numbers:
tempTab.NA <- cbind(table(int.data.wo.nas$risk_perception, 
                          int.data.wo.nas$all_wissen_nachher, 
                          useNA="always"), 
                    prop.table(table(int.data.wo.nas$risk_perception, 
                                     int.data.wo.nas$all_wissen_nachher, 
                                     useNA="always")))

rbind(cbind(tempTab.NA[7], round(tempTab.NA[7]/(tempTab.NA[7]+tempTab.NA[8]),4)),
      cbind(tempTab.NA[8], round(tempTab.NA[8]/(tempTab.NA[7]+tempTab.NA[8]),4)))

# asthma or rhinoconjunctivitis
# follow-up numbers:
cbind(table(int.data.wo.nas$asthma.or.rhinoconj
            [!is.na(int.data.wo.nas$all_wissen_nachher)],
            useNA="always"), 
      prop.table(table(int.data.wo.nas$asthma.or.rhinoconj
                       [!is.na(int.data.wo.nas$all_wissen_nachher)],
                       useNA="always")))

# NA numbers:
tempTab.NA <- cbind(table(int.data.wo.nas$asthma.or.rhinoconj, 
                          int.data.wo.nas$all_wissen_nachher, 
                          useNA="always"), 
                    prop.table(table(int.data.wo.nas$asthma.or.rhinoconj, 
                                     int.data.wo.nas$all_wissen_nachher, 
                                     useNA="always")))

rbind(cbind(tempTab.NA[7], round(tempTab.NA[7]/(tempTab.NA[7]+tempTab.NA[8]),4)),
      cbind(tempTab.NA[8], round(tempTab.NA[8]/(tempTab.NA[7]+tempTab.NA[8]),4)))

# parental asthma
# follow-up numbers:
cbind(table(int.data.wo.nas$par.asthma
            [!is.na(int.data.wo.nas$all_wissen_nachher)],
            useNA="always"), 
      prop.table(table(int.data.wo.nas$par.asthma
                       [!is.na(int.data.wo.nas$all_wissen_nachher)],
                       useNA="always")))

# NA numbers:
tempTab.NA <- cbind(table(int.data.wo.nas$par.asthma, 
                          int.data.wo.nas$all_wissen_nachher, 
                          useNA="always"), 
                    prop.table(table(int.data.wo.nas$par.asthma, 
                                     int.data.wo.nas$all_wissen_nachher, 
                                     useNA="always")))

rbind(cbind(tempTab.NA[7], round(tempTab.NA[7]/(tempTab.NA[7]+tempTab.NA[8]),4)),
      cbind(tempTab.NA[8], round(tempTab.NA[8]/(tempTab.NA[7]+tempTab.NA[8]),4)))

# knowledge at baseline
# follow-up numbers:
cbind(table(int.data.wo.nas$all_wissen
            [!is.na(int.data.wo.nas$all_wissen_nachher)],
            useNA="always"), 
      prop.table(table(int.data.wo.nas$all_wissen
                       [!is.na(int.data.wo.nas$all_wissen_nachher)],
                       useNA="always")))

# NA numbers:
tempTab.NA <- cbind(table(int.data.wo.nas$all_wissen, 
                          int.data.wo.nas$all_wissen_nachher, 
                          useNA="always"), 
                    prop.table(table(int.data.wo.nas$all_wissen, 
                                     int.data.wo.nas$all_wissen_nachher, 
                                     useNA="always")))

rbind(cbind(tempTab.NA[7], round(tempTab.NA[7]/(tempTab.NA[7]+tempTab.NA[8]),4)),
      cbind(tempTab.NA[8], round(tempTab.NA[8]/(tempTab.NA[7]+tempTab.NA[8]),4)))

# treatment
# follow-up numbers:
cbind(table(int.data.wo.nas$gruppe
            [!is.na(int.data.wo.nas$all_wissen_nachher)],
            useNA="always"), 
      prop.table(table(int.data.wo.nas$gruppe
                       [!is.na(int.data.wo.nas$all_wissen_nachher)],
                       useNA="always")))

# NA numbers:
tempTab.NA <- cbind(table(int.data.wo.nas$gruppe, 
                          int.data.wo.nas$all_wissen_nachher, 
                          useNA="always"), 
                    prop.table(table(int.data.wo.nas$gruppe, 
                                     int.data.wo.nas$all_wissen_nachher, 
                                     useNA="always")))

rbind(cbind(tempTab.NA[7], round(tempTab.NA[7]/(tempTab.NA[7]+tempTab.NA[8]),4)),
      cbind(tempTab.NA[8], round(tempTab.NA[8]/(tempTab.NA[7]+tempTab.NA[8]),4)))

#---
# All 6:
# age
cbind(table(int.data.wo.nas$alterkat2.binary, 
            int.data.wo.nas$all_wissen_nachher, 
            useNA="no"), 
      prop.table(table(int.data.wo.nas$alterkat2.binary, 
                       int.data.wo.nas$all_wissen_nachher, 
                       useNA="no"),2))

# sex
cbind(table(int.data.wo.nas$sex.factor, 
            int.data.wo.nas$all_wissen_nachher, 
            useNA="no"), 
      prop.table(table(int.data.wo.nas$sex.factor, 
                       int.data.wo.nas$all_wissen_nachher, 
                       useNA="no"),2))


# smoking status
cbind(table(int.data.wo.nas$smoking.status, 
            int.data.wo.nas$all_wissen_nachher, 
            useNA="no"), 
      prop.table(table(int.data.wo.nas$smoking.status, 
                       int.data.wo.nas$all_wissen_nachher, 
                       useNA="no"),2))


# educational level
cbind(table(int.data.wo.nas$schulabschluss.binary, 
            int.data.wo.nas$all_wissen_nachher, 
            useNA="no"), 
      prop.table(table(int.data.wo.nas$schulabschluss.binary, 
                       int.data.wo.nas$all_wissen_nachher, 
                       useNA="no"),2))


# risk perception
cbind(table(int.data.wo.nas$risk_perception, 
            int.data.wo.nas$all_wissen_nachher, 
            useNA="no"), 
      prop.table(table(int.data.wo.nas$risk_perception, 
                       int.data.wo.nas$all_wissen_nachher, 
                       useNA="no"),2))

# asthma or rhinoconjunctivitis
cbind(table(int.data.wo.nas$asthma.or.rhinoconj, 
            int.data.wo.nas$all_wissen_nachher, 
            useNA="no"), 
      prop.table(table(int.data.wo.nas$asthma.or.rhinoconj, 
                       int.data.wo.nas$all_wissen_nachher, 
                       useNA="no"),2))

# parental asthma
cbind(table(int.data.wo.nas$par.asthma, 
            int.data.wo.nas$all_wissen_nachher, 
            useNA="no"), 
      prop.table(table(int.data.wo.nas$par.asthma, 
                       int.data.wo.nas$all_wissen_nachher, 
                       useNA="no"),2))


# knowledge at baseline
cbind(table(int.data.wo.nas$all_wissen, 
            int.data.wo.nas$all_wissen_nachher, 
            useNA="no"), 
      prop.table(table(int.data.wo.nas$all_wissen, 
                       int.data.wo.nas$all_wissen_nachher, 
                       useNA="no"),2))


# treatment
cbind(table(int.data.wo.nas$gruppe, 
            int.data.wo.nas$all_wissen_nachher, 
            useNA="no"), 
      prop.table(table(int.data.wo.nas$gruppe, 
                       int.data.wo.nas$all_wissen_nachher, 
                       useNA="no"),2))


#---
# At least 5:
# age
cbind(table(int.data.wo.nas$alterkat2.binary, 
            int.data.wo.nas$all_wissen_nachher5), 
      prop.table(table(int.data.wo.nas$alterkat2.binary, 
                       int.data.wo.nas$all_wissen_nachher5),2))

# sex
cbind(table(int.data.wo.nas$sex.factor, int.data.wo.nas$all_wissen_nachher5), 
      prop.table(table(int.data.wo.nas$sex.factor, int.data.wo.nas$all_wissen_nachher5),2))

# smoking status
cbind(table(int.data.wo.nas$smoking.status, int.data.wo.nas$all_wissen_nachher5), 
      prop.table(table(int.data.wo.nas$smoking.status, int.data.wo.nas$all_wissen_nachher5),2))

# educational level
cbind(table(int.data.wo.nas$schulabschluss.binary, int.data.wo.nas$all_wissen_nachher5), 
      prop.table(table(int.data.wo.nas$schulabschluss.binary, int.data.wo.nas$all_wissen_nachher5),2))

# risk perception
cbind(table(int.data.wo.nas$risk_perception, int.data.wo.nas$all_wissen_nachher5), 
      prop.table(table(int.data.wo.nas$risk_perception, int.data.wo.nas$all_wissen_nachher5),2))

# asthma or rhinoconjunctivitis
cbind(table(int.data.wo.nas$asthma.or.rhinoconj, int.data.wo.nas$all_wissen_nachher5), 
      prop.table(table(int.data.wo.nas$asthma.or.rhinoconj, int.data.wo.nas$all_wissen_nachher5),2))

# parental asthma
cbind(table(int.data.wo.nas$par.asthma, int.data.wo.nas$all_wissen_nachher5), 
      prop.table(table(int.data.wo.nas$par.asthma, int.data.wo.nas$all_wissen_nachher5),2))

# knowledge at baseline
cbind(table(int.data.wo.nas$all_wissen5, int.data.wo.nas$all_wissen_nachher5), 
      prop.table(table(int.data.wo.nas$all_wissen5, int.data.wo.nas$all_wissen_nachher5),2))

# treatment
cbind(table(int.data.wo.nas$gruppe, int.data.wo.nas$all_wissen_nachher5), 
      prop.table(table(int.data.wo.nas$gruppe, int.data.wo.nas$all_wissen_nachher5),2))

#---
# At least (and less than) 4:
# age
cbind(table(int.data.wo.nas$alterkat2.binary, int.data.wo.nas$all_wissen_nachher4), 
      prop.table(table(int.data.wo.nas$alterkat2.binary, int.data.wo.nas$all_wissen_nachher4),2))

# sex
cbind(table(int.data.wo.nas$sex.factor, int.data.wo.nas$all_wissen_nachher4), 
      prop.table(table(int.data.wo.nas$sex.factor, int.data.wo.nas$all_wissen_nachher4),2))

# smoking status
cbind(table(int.data.wo.nas$smoking.status, int.data.wo.nas$all_wissen_nachher4), 
      prop.table(table(int.data.wo.nas$smoking.status, int.data.wo.nas$all_wissen_nachher4),2))

# educational level
cbind(table(int.data.wo.nas$schulabschluss.binary, int.data.wo.nas$all_wissen_nachher4), 
      prop.table(table(int.data.wo.nas$schulabschluss.binary, int.data.wo.nas$all_wissen_nachher4),2))

# risk perception
cbind(table(int.data.wo.nas$risk_perception, int.data.wo.nas$all_wissen_nachher4), 
      prop.table(table(int.data.wo.nas$risk_perception, int.data.wo.nas$all_wissen_nachher4),2))

# asthma or rhinoconjunctivitis
cbind(table(int.data.wo.nas$asthma.or.rhinoconj, int.data.wo.nas$all_wissen_nachher4), 
      prop.table(table(int.data.wo.nas$asthma.or.rhinoconj, int.data.wo.nas$all_wissen_nachher4),2))

# parental asthma
cbind(table(int.data.wo.nas$par.asthma, int.data.wo.nas$all_wissen_nachher4), 
      prop.table(table(int.data.wo.nas$par.asthma, int.data.wo.nas$all_wissen_nachher4),2))

# knowledge at baseline
cbind(table(int.data.wo.nas$all_wissen4, int.data.wo.nas$all_wissen_nachher4), 
      prop.table(table(int.data.wo.nas$all_wissen4, int.data.wo.nas$all_wissen_nachher4),2))

# treatment
cbind(table(int.data.wo.nas$gruppe, int.data.wo.nas$all_wissen_nachher4), 
      prop.table(table(int.data.wo.nas$gruppe, int.data.wo.nas$all_wissen_nachher4),2))
#---

#---
# TABLE 2
# completed and control variable:
# completed.table2 variable contains info on those who completed the intervention phase, and were either in the control (value=0) or the intervention (value=1) group.

data.wo.nas$completed.table2 <- data.wo.nas$four.groups
data.wo.nas$completed.table2[data.wo.nas$four.groups==1 | data.wo.nas$four.groups==3] <- NA
data.wo.nas$completed.table2[data.wo.nas$four.groups==2] <- 0
data.wo.nas$completed.table2[data.wo.nas$four.groups==4] <- 1
int.data.wo.nas$completed.table2 <- NULL
int.data.wo.nas$completed.table2 <- data.wo.nas$completed.table2[115:230]

# age
cbind(table(int.data.wo.nas$alterkat2.binary, int.data.wo.nas$completed.table2), 
      prop.table(table(int.data.wo.nas$alterkat2.binary, int.data.wo.nas$completed.table2),2))

fisher.test(int.data.wo.nas$alterkat2.binary, 
            int.data.wo.nas$completed.table2) #p-value = 0.3363

# sex
cbind(table(int.data.wo.nas$sex.factor, int.data.wo.nas$completed.table2), 
      prop.table(table(int.data.wo.nas$sex.factor, int.data.wo.nas$completed.table2),2))
fisher.test(int.data.wo.nas$sex.factor, 
            int.data.wo.nas$completed.table2) #p-value = 0.6918

# smoking status
cbind(table(int.data.wo.nas$smoking.status, int.data.wo.nas$completed.table2), 
      prop.table(table(int.data.wo.nas$smoking.status, int.data.wo.nas$completed.table2),2))
chisq.test(int.data.wo.nas$smoking.status, 
           int.data.wo.nas$completed.table2) #p-value = 1

# educational level
cbind(table(int.data.wo.nas$schulabschluss.binary, int.data.wo.nas$completed.table2), 
      prop.table(table(int.data.wo.nas$schulabschluss.binary, int.data.wo.nas$completed.table2),2))
chisq.test(int.data.wo.nas$schulabschluss.binary, 
           int.data.wo.nas$completed.table2) #p-value = 0.6818

# risk perception
cbind(table(int.data.wo.nas$risk_perception, int.data.wo.nas$completed.table2), 
      prop.table(table(int.data.wo.nas$risk_perception, int.data.wo.nas$completed.table2),2))
fisher.test(int.data.wo.nas$risk_perception, 
            int.data.wo.nas$completed.table2) #p-value = 0.06937

# asthma or rhinoconjunctivitis
cbind(table(int.data.wo.nas$asthma.or.rhinoconj, int.data.wo.nas$completed.table2), 
      prop.table(table(int.data.wo.nas$asthma.or.rhinoconj, int.data.wo.nas$completed.table2),2))
chisq.test(int.data.wo.nas$asthma.or.rhinoconj, 
           int.data.wo.nas$completed.table2) #p-value = 0.8245

# parental asthma
cbind(table(int.data.wo.nas$par.asthma, int.data.wo.nas$completed.table2), 
      prop.table(table(int.data.wo.nas$par.asthma, int.data.wo.nas$completed.table2),2))
chisq.test(int.data.wo.nas$par.asthma, 
           int.data.wo.nas$completed.table2) #p-value = 0.3933

# knowledge at baseline
# knowledge at baseline: wissen6
cbind(table(int.data.wo.nas$all_wissen, int.data.wo.nas$completed.table2), 
      prop.table(table(int.data.wo.nas$all_wissen, int.data.wo.nas$completed.table2),2))
fisher.test(int.data.wo.nas$all_wissen, 
            int.data.wo.nas$completed.table2) #p-value = 0.3214

# knowledge at baseline: wissen5
cbind(table(int.data.wo.nas$all_wissen5, int.data.wo.nas$completed.table2), 
      prop.table(table(int.data.wo.nas$all_wissen5, int.data.wo.nas$completed.table2),2))
chisq.test(int.data.wo.nas$all_wissen5, 
           int.data.wo.nas$completed.table2) #p-value = 0.6978

# knowledge at baseline: wissen4
cbind(table(int.data.wo.nas$all_wissen4, int.data.wo.nas$completed.table2), 
      prop.table(table(int.data.wo.nas$all_wissen4, int.data.wo.nas$completed.table2),2))
fisher.test(int.data.wo.nas$all_wissen4, 
            int.data.wo.nas$completed.table2) #p-value = 0.3958

# knowledge at follow-up
# knowledge at follow-up: wissen6
cbind(table(int.data.wo.nas$all_wissen_nachher, int.data.wo.nas$completed.table2), 
      prop.table(table(int.data.wo.nas$all_wissen_nachher, int.data.wo.nas$completed.table2),2))


# knowledge at follow-up: wissen5
cbind(table(int.data.wo.nas$all_wissen_nachher5, int.data.wo.nas$completed.table2), 
      prop.table(table(int.data.wo.nas$all_wissen_nachher5, int.data.wo.nas$completed.table2),2))


# knowledge at follow-up: wissen4
cbind(table(int.data.wo.nas$all_wissen_nachher4, int.data.wo.nas$completed.table2), 
      prop.table(table(int.data.wo.nas$all_wissen_nachher4, int.data.wo.nas$completed.table2),2))
#---

#--- 
# LOGISTIC REGRESSION MODELS (APPENDIX)

# Crude all 6
int.all_wissen_nachher.cat.glm.crude <- glm(all_wissen_nachher.factor ~ 
                                              gruppe, 
                                            data = int.data.wo.nas, 
                                            family = binomial, 
                                            na.action = na.omit)
# summary(int.all_wissen_nachher.cat.glm.crude)
exp(coef(int.all_wissen_nachher.cat.glm.crude))
exp(confint.default(int.all_wissen_nachher.cat.glm.crude))

# Adjusted all 6
int.all_wissen_nachher.cat.glm <- glm(all_wissen_nachher.factor ~ 
                                        gruppe +
                                        all_wissen.factor +
                                        alterkat2 +
                                        sex.factor + 
                                        smoking.status.factor + 
                                        schulabschluss.factor.twolevels + 
                                        risk_perception.factor +
                                        asthma.or.rhinoconj.factor + 
                                        par.asthma.factor, 
                                      data = int.data.wo.nas, 
                                      family = binomial, 
                                      na.action = na.omit)
# summary(int.all_wissen_nachher.cat.glm)
exp(coef(int.all_wissen_nachher.cat.glm))
exp(confint.default(int.all_wissen_nachher.cat.glm))

# Crude at least 5
int.all_wissen_nachher5.cat.glm.crude <- glm(all_wissen_nachher5.factor ~ 
                                               gruppe, 
                                             data = int.data.wo.nas, 
                                             family = binomial, 
                                             na.action = na.omit)
# summary(int.all_wissen_nachher5.cat.glm.crude)
exp(coef(int.all_wissen_nachher5.cat.glm.crude))
exp(confint.default(int.all_wissen_nachher5.cat.glm.crude))

# Adjusted at least 5
int.all_wissen_nachher5.cat.glm <- glm(all_wissen_nachher5.factor ~ 
                                         gruppe +
                                         all_wissen5.factor +
                                         alterkat2 +
                                         sex.factor + 
                                         smoking.status.factor + 
                                         schulabschluss.factor.twolevels + 
                                         risk_perception.factor +
                                         asthma.or.rhinoconj.factor + 
                                         par.asthma.factor, 
                                       data = int.data.wo.nas, 
                                       family = binomial, 
                                       na.action = na.omit)
# summary(int.all_wissen_nachher5.cat.glm)
exp(coef(int.all_wissen_nachher5.cat.glm))
exp(confint.default(int.all_wissen_nachher5.cat.glm))

# Crude at least 4
int.all_wissen_nachher4.cat.glm.crude <- glm(all_wissen_nachher4.factor ~ 
                                               gruppe, 
                                             data = int.data.wo.nas, 
                                             family = binomial, 
                                             na.action = na.omit)
# summary(int.all_wissen_nachher4.cat.glm.crude)
exp(coef(int.all_wissen_nachher4.cat.glm.crude))
exp(confint.default(int.all_wissen_nachher4.cat.glm.crude))

# Adjusted at least 4
int.all_wissen_nachher4.cat.glm <- glm(all_wissen_nachher4.factor ~ 
                                         gruppe +
                                         all_wissen4.factor +
                                         alterkat2 +
                                         sex.factor + 
                                         smoking.status.factor + 
                                         schulabschluss.factor.twolevels + 
                                         risk_perception.factor +
                                         asthma.or.rhinoconj.factor + 
                                         par.asthma.factor, 
                                       data = int.data.wo.nas, 
                                       family = binomial, 
                                       na.action = na.omit)
# summary(int.all_wissen_nachher4.cat.glm)
exp(coef(int.all_wissen_nachher4.cat.glm))
exp(confint.default(int.all_wissen_nachher4.cat.glm))

# Logistic regression is not able to provide adequate estimates (due to small sample size)
# Proposed solution: use TMLE
#---

#--- 
# TMLE

# variables:

# treatment
A <- as.numeric(int.data.wo.nas$gruppe)

# missings
Delta <- na.vector.all_wissen_nachher 

# matrix of covariates
ff <- model.matrix( ~ int.data.wo.nas$sex + int.data.wo.nas$alterkat2.binary + int.data.wo.nas$schulabschluss.binary + int.data.wo.nas$smoking.status + int.data.wo.nas$asthma.or.rhinoconj + int.data.wo.nas$risk_perception + int.data.wo.nas$par.asthma + int.data.wo.nas$all_wissen)
W <- ff[,-1]
colnames(W) <- paste("W",1:8, sep="")

# SuperLearner libraries
create.SL.knn <- function(k = c(20, 30)) {
  for(mm in seq(length(k))){
    eval(parse(text = paste('SL.knn.', k[mm], '<- function(..., k = ', k[mm],
                            ') SL.knn(..., k = k)', sep = '')), envir = .GlobalEnv)
  }
  invisible(TRUE)
}
create.SL.knn(c(20, 30, 40, 50, 60, 70))
SL.library <- c("SL.glmnet", "SL.knn.20", "SL.knn.40", "SL.knn.40", "SL.knn.60", 
                "SL.mean", "SL.bayesglm", "SL.glm",
                "SL.step.interaction", "SL.randomForest")

# Main outcome: all 6 correct measures
Y <- int.data.wo.nas$all_wissen_nachher
set.seed(1985)
result6 <- tmle(Y=Y,A=A,W=W, Delta=Delta, family= "binomial", cvQinit = FALSE,
                g.SL.library = SL.library, 
                Q.SL.library = SL.library, 
                gbound = c(0.025, 0.995))
summary(result6)

# Sensitivity analyses
# at least 5 correct measures
Y <- int.data.wo.nas$all_wissen_nachher5
set.seed(1985)
result5 <- tmle(Y=Y,A=A,W=W, Delta=Delta, family= "binomial", cvQinit = FALSE,
                g.SL.library = SL.library, 
                Q.SL.library = SL.library, 
                gbound = c(0.025, 0.995))
summary(result5)

# at least 4 correct measures
Y <- int.data.wo.nas$all_wissen_nachher4
set.seed(1985)
result4 <- tmle(Y=Y,A=A,W=W, Delta=Delta, family= "binomial", cvQinit = FALSE,
                g.SL.library = SL.library, 
                Q.SL.library = SL.library, 
                gbound = c(0.025, 0.995))
summary(result4)
#---

#---
# ATT and ATC
# Main outcome: all 6 correct measures
# outcome variable
Y <- int.data.wo.nas$all_wissen_nachher

# att
set.seed(1985)
a <- 1
att6 <- tmle.cte(Y=Y,A=A,B=W, Delta=Delta, a=a, family= "binomial",
                 Q.method = "SL", g.method = "SL", gDelta.method = "SL",
                 g.SL.library = SL.library,
                 gDelta.SL.library = SL.library,
                 Q.SL.library = SL.library)

# atc
set.seed(1985)
a <- 0
atc6 <- tmle.cte(Y=Y,A=A,B=W, Delta=Delta, a=a, family= "binomial",
                 Q.method = "SL", g.method = "SL", gDelta.method = "SL",
                 g.SL.library = SL.library,
                 gDelta.SL.library = SL.library,
                 Q.SL.library = SL.library)
att6
atc6

#---
# Sensitivity analyses
# at least 5 correct measures
# outcome variable
Y <- int.data.wo.nas$all_wissen_nachher5

# att
set.seed(1985)
a <- 1
att5 <- tmle.cte(Y=Y,A=A,B=W, Delta=Delta, a=a, family= "binomial",
                 Q.method = "SL", g.method = "SL", gDelta.method = "SL",
                 g.SL.library = SL.library,
                 gDelta.SL.library = SL.library,
                 Q.SL.library = SL.library)

# atc
set.seed(1985)
a <- 0
atc5 <- tmle.cte(Y=Y,A=A,B=W, Delta=Delta, a=a, family= "binomial",
                 Q.method = "SL", g.method = "SL", gDelta.method = "SL",
                 g.SL.library = SL.library,
                 gDelta.SL.library = SL.library,
                 Q.SL.library = SL.library)
att5
atc5

#---
# at least 4 correct measures
# outcome variable
Y <- int.data.wo.nas$all_wissen_nachher4

# att
set.seed(1985)
a <- 1
att4 <- tmle.cte(Y=Y,A=A,B=W, Delta=Delta, a=a, family= "binomial",
                 Q.method = "SL", g.method = "SL", gDelta.method = "SL",
                 g.SL.library = SL.library,
                 gDelta.SL.library = SL.library,
                 Q.SL.library = SL.library)

# atc
set.seed(1985)
a <- 0
atc4 <- tmle.cte(Y=Y,A=A,B=W, Delta=Delta, a=a, family= "binomial",
                 Q.method = "SL", g.method = "SL", gDelta.method = "SL",
                 g.SL.library = SL.library,
                 gDelta.SL.library = SL.library,
                 Q.SL.library = SL.library)
att4
atc4

#---------------#
# END OF SCRIPT #
#---------------#