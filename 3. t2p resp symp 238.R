#' ---
#' title: "txt2paul Descriptive Statistics on Respiratory symptoms of 238 subjects"
#' author: "Daloha Rodriguez-Molina"
#' date: "March 25th, 2015"
#' ---

#-------------------#
# Main data used: txt2PAUL_Gesamt_vers1_130115.csv
# Name given to this main dataset: data
#
# Subset dataset mostly used on this script: data3
# which contains data3 = ATEMWEGSBESCHWERDEN (Basisbefragung)
#
# Variables in the data3 dataset:
# [1] "pfeifen"            
# [2] "pfeifen_ohne_erk"   
# [3] "asthma"             
# [4] "asthma_arzt"        
# [5] "medikamente"        
# [6] "nasenprobleme"      
# [7] "nasenprobleme_augen"
# [8] "allergie_mutter"    
# [9] "allergie_vater"
# 
# Newly created variables: (All have .factor versions as well exc. asthma2)
# asthma2: asthma symptoms according to both qq. 1 and 2 from FB
# asthma3: asthma symptoms according to qq.1, 2 and 5 from FB
# asthma.dx: asthma diagnosis according to qq.3 and 4 from FB
# allergies: allergies according to qq. 6 and 7 from FB
# par.asthma: parental asthma according to qq. 8 and 9 from FB
#
# Note: run conversion to factors on "t2p baseline 238.R" first if needed
#
# Contents:
# I.    Preliminary stuff: setwd, packages, read data, subset data, briefly explore data
# II.   Recoding variables
# III.  Convert to factors
# IV.   Convert to factors (yes/no inversion) (optional)
# V.    Respiratory Symptoms analysis
# VI.   By Sex
# VII.  By Smoking status
# VIII. By Current smoker
# IX.   By Age category
#-------------------#

#----------------------#
# I. Preliminary stuff #
#----------------------#

# When working from home
setwd("~/Dropbox/R - txt2paul")

# When working at KR5
# setwd("/usr281/ben/msc1426/Desktop/dataproject_datasets-1")

# install packages
# pckgs <- c("psych", "modeltools", "mvtnorm")
# install.packages(pckgs)
# install.packages("/Users/daro/Documents/R/coin_1.0-24.tgz", repos = NULL, type="source")
library(psych)
library(modeltools)
library(mvtnorm)
library(coin)


# Read the data
# d <- "/Users/daro/Desktop/txt2PAUL/z_Dokumente fuer Daloha/txt2PAUL_Gesamt_vers1_130115.csv"
# data <- read.csv(d, header=T)
# attach(data)

# explore data and subset demographics
# head(data[,1:4])
# View(data)
# names(data)
data3 <- data[,22:30] # data3 = ATEMWEGSBESCHWERDEN (Basisbefragung)
# names(data3)

#----------------------------#
# II. Recoding of variables: #
#----------------------------#

# asthma3
asthma3 <- NULL
asthma3[data$pfeifen_ohne_erk==0 | data$medikamente==0 | data$pfeifen_ohne_erk==2 | data$medikamente==2] <- 0
asthma3[data$pfeifen_ohne_erk==1 | data$medikamente==1] <- 1
data$asthma3 <- asthma3

# asthma.dx
asthma.dx <- NULL
asthma.dx[data$asthma==0 | data$asthma_arzt==0 | data$asthma_arzt==2] <- 0
asthma.dx[data$asthma==1 | data$asthma_arzt==1] <- 1
data$asthma.dx <- asthma.dx
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

# par.asthma
par.asthma <- NULL
par.asthma[data$allergie_mutter==0 | data$allergie_vater==0 | data$allergie_mutter==2 | data$allergie_vater==2] <- 0
par.asthma[data$allergie_mutter==1 | data$allergie_vater==1] <- 1
data$par.asthma <- par.asthma

#--------------------------#
# III. Convert to factors: #
#--------------------------#
no.yes.labels <- c("No", "Yes")
pfeifen.factor <- factor(data$pfeifen, labels = no.yes.labels)
no.yes.kein.labels <- c("No", "Yes", "Kein")
pfeifen_ohne_erk.factor <- factor(data$pfeifen_ohne_erk, labels = no.yes.kein.labels)
asthma.factor <- factor(data$asthma, labels = no.yes.labels)
asthma_arzt.factor <- factor(data$asthma_arzt, labels = no.yes.kein.labels)
medikamente.factor <- factor(data$medikamente, labels = no.yes.kein.labels)
nasenprobleme.factor <- factor(data$nasenprobleme, labels = no.yes.labels)
nasenprobleme_augen.factor <- factor(data$nasenprobleme_augen, labels = no.yes.kein.labels)
no.yes.weissnicht.labels <- c("No", "Yes","Don't know")
allergie_mutter.factor <- factor(data$allergie_mutter, labels = no.yes.weissnicht.labels)
allergie_vater.factor <- factor(data$allergie_vater, labels = no.yes.weissnicht.labels)
asthma3.factor <- factor(asthma3, labels = no.yes.labels)
asthma.dx.factor <- factor(asthma.dx, labels = no.yes.labels)
allergies.factor <- factor(allergies, labels = no.yes.labels)
par.asthma.factor <- factor(par.asthma, labels = no.yes.labels)
no.yes.weissnicht.labels <- c("No", "Yes","Don't know")



#-------------------------#
# IV. Convert to factors: #
#   Yes / No inversion.   #
#-------------------------#
yes.no.labels <- c("Yes", "No")
pfeifen.factor <- factor(data$pfeifen, labels = yes.no.labels, levels = c(1,0))
yes.no.kein.labels <- c("Yes", "No", "Kein")
pfeifen_ohne_erk.factor <- factor(data$pfeifen_ohne_erk, labels = yes.no.kein.labels, levels = c(1,0,2))
asthma.factor <- factor(data$asthma, labels = yes.no.labels, levels = c(1,0))
asthma_arzt.factor <- factor(data$asthma_arzt, labels = yes.no.kein.labels, levels = c(1,0,2))
medikamente.factor <- factor(data$medikamente, labels = yes.no.kein.labels, levels = c(1,0,2))
nasenprobleme.factor <- factor(data$nasenprobleme, labels = yes.no.labels, levels = c(1,0))
nasenprobleme_augen.factor <- factor(data$nasenprobleme_augen, labels = yes.no.kein.labels, levels = c(1,0,2))
yes.no.weissnicht.labels <- c("Yes", "No", "Don't know")
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

#-----------------------------------#
# V. Respiratory Symptoms analysis: #
#-----------------------------------#
nrow(data) #n

# Wheezing or buzz
# cbind(table(pfeifen.factor),round(prop.table(table(pfeifen.factor)),4))
# sum(is.na(pfeifen.factor))

# Wheezing or buzz without a cold
# cbind(table(pfeifen_ohne_erk.factor),round(prop.table(table(pfeifen_ohne_erk.factor)),4)) # Kein = Kein Pfeifen / Brummen
# sum(is.na(pfeifen_ohne_erk.factor))

# Asthma Self-reported
# cbind(table(asthma.factor),round(prop.table(table(asthma.factor)),4))
# sum(is.na(asthma.factor))

# Asthma Dx
# cbind(table(asthma_arzt.factor),round(prop.table(table(asthma_arzt.factor)),4))
# sum(is.na(asthma_arzt.factor))

# Medikamente
# cbind(table(medikamente.factor),round(prop.table(table(medikamente.factor)),4))
# sum(is.na(medikamente.factor))

# Nasenprobleme
# cbind(table(nasenprobleme.factor),round(prop.table(table(nasenprobleme.factor)),4))
# sum(is.na(nasenprobleme.factor))

# Nasenprobleme Augen
# cbind(table(nasenprobleme_augen.factor),round(prop.table(table(nasenprobleme_augen.factor)),4))
# sum(is.na(nasenprobleme_augen.factor))

# Allergy Mother
# cbind(table(allergie_mutter.factor),round(prop.table(table(allergie_mutter.factor)),4))
# sum(is.na(allergie_mutter.factor))

# Allergy Father
# cbind(table(allergie_vater.factor),round(prop.table(table(allergie_vater.factor)),4))
# sum(is.na(allergie_vater.factor))

# Asthma symptoms
cbind(table(asthma3.factor),round(prop.table(table(asthma3.factor)),4))
sum(is.na(asthma3.factor))

# Asthma diagnosis
# cbind(table(asthma.dx.factor),round(prop.table(table(asthma.dx.factor)),4))
# sum(is.na(asthma.dx.factor))

# Allergies
cbind(table(allergies.factor),round(prop.table(table(allergies.factor)),4))
sum(is.na(allergies.factor))

# Parental asthma
cbind(table(par.asthma.factor),round(prop.table(table(par.asthma.factor)),4))
sum(is.na(par.asthma.factor))

#-------------#
# VI. By Sex: #         # Not included in the final analyses.
#-------------#
# cbind(table(sex.factor), round(prop.table(table(sex.factor)),4)) # n

# Wheezing or buzz
# cbind(table(pfeifen.factor, sex.factor), round(prop.table(table(pfeifen.factor, sex.factor),1),4)) # if error, run first conversion to factors in file "t2p baseline 238.R"
# fisher.test(pfeifen.factor, sex.factor)$conf.int # NS
# fisher.test(pfeifen.factor, sex.factor)$p.value # NS

# Wheezing or buzz without a cold
# cbind(table(pfeifen_ohne_erk.factor, sex.factor), round(prop.table(table(pfeifen_ohne_erk.factor, sex.factor),1),4))
# chisq_test(table(sex.factor, pfeifen_ohne_erk.factor)) # NS

# Asthma Self-reported
# cbind(table(asthma.factor, sex.factor), round(prop.table(table(asthma.factor, sex.factor),1),4))
# fisher.test(asthma.factor, sex.factor)$conf.int # NS
# fisher.test(asthma.factor, sex.factor)$p.value # NS

# Asthma Dx
# cbind(table(asthma_arzt.factor, sex.factor), round(prop.table(table(asthma_arzt.factor, sex.factor),1),4))
# chisq_test(table(sex.factor, asthma_arzt.factor)) # NS

# Medikamente
# cbind(table(medikamente.factor, sex.factor), round(prop.table(table(medikamente.factor, sex.factor),1),4))
# chisq_test(table(sex.factor, medikamente.factor)) # NS

# Nasenprobleme
# cbind(table(nasenprobleme.factor, sex.factor), round(prop.table(table(nasenprobleme.factor, sex.factor),1),4))
# chisq.test(nasenprobleme.factor, sex.factor) # NS

# Nasenprobleme Augen
# cbind(table(nasenprobleme_augen.factor, sex.factor), round(prop.table(table(nasenprobleme_augen.factor, sex.factor),1),4))
# fisher.test(nasenprobleme_augen.factor, sex.factor) # NS

# Allergy Mother
# cbind(table(allergie_mutter.factor, sex.factor), round(prop.table(table(allergie_mutter.factor, sex.factor),1),4))
# chisq_test(table(sex.factor, allergie_mutter.factor)) # NS

# Allergy Father
# cbind(table(allergie_vater.factor, sex.factor), round(prop.table(table(allergie_vater.factor, sex.factor),1),4))
# chisq_test(table(sex.factor, allergie_vater.factor)) # NS

# Asthma symptoms
# cbind(table(asthma3.factor, sex.factor), round(prop.table(table(asthma3.factor, sex.factor),1),4))
# fisher.test(sex.factor, asthma3.factor) # NS

# Asthma diagnosis
# cbind(table(asthma.dx.factor, sex.factor), round(prop.table(table(asthma.dx.factor, sex.factor),1),4))
# fisher.test(sex.factor, asthma.dx.factor) # NS

# Allergies
# cbind(table(allergies.factor, sex.factor), round(prop.table(table(allergies.factor, sex.factor),1),4))
# chisq.test(sex.factor, allergies.factor) # NS

# Parental asthma
# cbind(table(par.asthma.factor, sex.factor), round(prop.table(table(par.asthma.factor, sex.factor),1),4))
# fisher.test(sex, par.asthma.factor) # NS

#-------------------------#
# VII. By Smoking status: #
#-------------------------#
cbind(table(data$smoking.status.factor), round(prop.table(table(data$smoking.status.factor)),4)) # n

# Wheezing or buzz
# cbind(table(pfeifen.factor, smoking.status.factor), round(prop.table(table(pfeifen.factor, smoking.status.factor),1),4))
# fisher.test(table(pfeifen.factor, smoking.status.factor)) # NS

# Wheezing or buzz without a cold
# cbind(table(pfeifen_ohne_erk.factor, smoking.status.factor), round(prop.table(table(pfeifen_ohne_erk.factor, smoking.status.factor),1),4))
# fisher.test(table(smoking.status.factor, pfeifen_ohne_erk.factor)) # NS

# Asthma Self-reported
# cbind(table(asthma.factor, smoking.status.factor), round(prop.table(table(asthma.factor, smoking.status.factor),1),4))
# fisher.test(asthma.factor, smoking.status.factor)$conf.int # NS
# fisher.test(asthma.factor, smoking.status.factor)$p.value # NS

# Asthma Dx
# cbind(table(asthma_arzt.factor, smoking.status.factor), round(prop.table(table(asthma_arzt.factor, smoking.status.factor),1),4))
# fisher.test(table(smoking.status.factor, asthma_arzt.factor)) # NS

# Medikamente
# cbind(table(medikamente.factor, smoking.status.factor), round(prop.table(table(medikamente.factor, smoking.status.factor),1),4))
# fisher.test(table(smoking.status.factor, medikamente.factor)) # NS

# Nasenprobleme
# cbind(table(nasenprobleme.factor, smoking.status.factor), round(prop.table(table(nasenprobleme.factor, smoking.status.factor),1),4))
# fisher.test(nasenprobleme.factor, smoking.status.factor) # NS

# Nasenprobleme Augen
# cbind(table(nasenprobleme_augen.factor, smoking.status.factor), round(prop.table(table(nasenprobleme_augen.factor, smoking.status.factor),1),4))
# fisher.test(nasenprobleme_augen.factor, smoking.status.factor) # NS

# Allergy Mother
# cbind(table(allergie_mutter.factor, smoking.status.factor), round(prop.table(table(allergie_mutter.factor, smoking.status.factor),1),4))
# fisher.test(table(smoking.status.factor, allergie_mutter.factor)) # NS

# Allergy Father
# cbind(table(allergie_vater.factor, smoking.status.factor), round(prop.table(table(allergie_vater.factor, smoking.status.factor),1),4))
# fisher.test(table(smoking.status.factor, allergie_vater.factor)) # NS

# Asthma symptoms
cbind(table(asthma3.factor, smoking.status.factor), round(prop.table(table(asthma3.factor, smoking.status.factor),1),4))
chisq.test(smoking.status.factor, asthma3.factor)$p.value # NS

# Asthma diagnosis
# cbind(table(asthma.dx.factor, smoking.status.factor), round(prop.table(table(asthma.dx.factor, smoking.status.factor),1),4))
# fisher.test(smoking.status.factor, asthma.dx.factor) # NS

# Allergies
cbind(table(allergies.factor, smoking.status.factor), round(prop.table(table(allergies.factor, smoking.status.factor),1),4))
chisq.test(smoking.status.factor, allergies.factor)$p.value # NS

# Parental asthma
cbind(table(par.asthma.factor, smoking.status.factor), round(prop.table(table(par.asthma.factor, smoking.status.factor),1),4))
chisq.test(smoking.status.factor, par.asthma.factor)$p.value 


#----------------------#
# IX. By Age category: #      # Not included in the final analyses
#----------------------#
# cbind(table(alterkat.factor), round(prop.table(table(alterkat.factor)),4)) # n

# Wheezing or buzz
# cbind(table(pfeifen.factor, alterkat.factor), round(prop.table(table(pfeifen.factor, alterkat.factor),1),4)) # if error, run first conversion to factors in file "t2p baseline 238.R"
# fisher.test(table(pfeifen.factor, alterkat.factor))$p.value # NS

# Wheezing or buzz without a cold
# cbind(table(pfeifen_ohne_erk.factor, alterkat.factor), round(prop.table(table(pfeifen_ohne_erk.factor, alterkat.factor),1),4))
# fisher.test(table(alterkat.factor, pfeifen_ohne_erk.factor))$p.value # NS

# Asthma Self-reported
# cbind(table(asthma.factor, alterkat.factor), round(prop.table(table(asthma.factor, alterkat.factor),1),4))
# fisher.test(asthma.factor, alterkat.factor)$p.value # NS

# Asthma Dx
# cbind(table(asthma_arzt.factor, alterkat.factor), round(prop.table(table(asthma_arzt.factor, alterkat.factor),1),4))
# fisher.test(table(alterkat.factor, asthma_arzt.factor))$p.value # NS

# Medikamente
# cbind(table(medikamente.factor, alterkat.factor), round(prop.table(table(medikamente.factor, alterkat.factor),1),4))
# fisher.test(table(alterkat.factor, medikamente.factor))$p.value # NS

# Nasenprobleme
# cbind(table(nasenprobleme.factor, alterkat.factor), round(prop.table(table(nasenprobleme.factor, alterkat.factor),1),4))
# fisher.test(nasenprobleme.factor, alterkat.factor)$p.value # NS

# Nasenprobleme Augen
# cbind(table(nasenprobleme_augen.factor, alterkat.factor), round(prop.table(table(nasenprobleme_augen.factor, alterkat.factor),1),4))
# fisher.test(nasenprobleme_augen.factor, alterkat.factor)$p.value # NS

# Allergy Mother
# cbind(table(allergie_mutter.factor, alterkat.factor), round(prop.table(table(allergie_mutter.factor, alterkat.factor),1),4))
# fisher.test(table(alterkat.factor, allergie_mutter.factor))$p.value # NS

# Allergy Father
# cbind(table(allergie_vater.factor, alterkat.factor), round(prop.table(table(allergie_vater.factor, alterkat.factor),1),4))
# fisher.test(table(alterkat.factor, allergie_vater.factor))$p.value # NS

# Asthma symptoms
# cbind(table(asthma3.factor, alterkat.factor), round(prop.table(table(asthma3.factor, alterkat.factor),1),4))
# fisher.test(alterkat.factor, asthma3.factor)$p.value # NS

# Asthma diagnosis
# cbind(table(asthma.dx.factor, alterkat.factor), round(prop.table(table(asthma.dx.factor, alterkat.factor),1),4))
# fisher.test(alterkat.factor, asthma.dx.factor)$p.value # NS

# Allergies
# cbind(table(allergies.factor, alterkat.factor), round(prop.table(table(allergies.factor, alterkat.factor),1),4))
# fisher.test(alterkat.factor, allergies.factor)$p.value # NS

# Parental asthma
# cbind(table(par.asthma.factor, alterkat.factor), round(prop.table(table(par.asthma.factor, alterkat.factor),1),4))
# fisher.test(alterkat.factor, par.asthma.factor)$p.value # NS

#---------------#
# END OF SCRIPT #
#---------------#