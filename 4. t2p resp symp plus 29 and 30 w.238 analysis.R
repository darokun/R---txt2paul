#' ---
#' title: "txt2paul Respiratory symptoms and Risk assessment on 238 subjects"
#' author: "Daloha Rodriguez-Molina"
#' date: "March 27th, 2015"
#' ---

# Bookmark: #---#

#-------------------#
# Main data used: txt2PAUL_Gesamt_vers1_130115.csv
# Name given to this main dataset: data
#
# Subset(s) dataset mostly used on this script: data3, data4, and part of data7 = data16
# which contains 
# data3 = ATEMWEGSBESCHWERDEN (Basisbefragung)
# data4 = WISSEN (Basisbefragung)
# data7 = ARBEITSALLTAG (Basisbefragung) 
#
# Variables in the data16 dataset: (the same numbering applies to data16.factor)
# UPDATE!!! There are 32 variables!!!
#  [1] "pfeifen"                     "pfeifen_ohne_erk"            "asthma"                     
#  [4] "asthma_arzt"                 "medikamente"                 "nasenprobleme"              
#  [7] "nasenprobleme_augen"         "allergie_mutter"             "allergie_vater"             
# [10] "haare"                       "arbeitsschuhe"               "kleidung_aufb"              
# [13] "desinfizieren"               "schutzbrille"                "kleidung_wohnraum"          
# [16] "arbeitskleidung_letzteWoche" "haare_letzteWoche"           "aufbewahrung_letzteWoche"   
# [19] "arbeitskleidung_intention"   "haare_intention"             "aufbewahrung_intention"     
# [22] "arbeitskleidung_einstellung" "haare_einstellung"           "aufbewahrung_einstellung"   
# [25] "arbeitskleidung_norm"        "haare_norm"                  "aufbewahrung_norm"          
# [28] "arbeitskleidung_kontrolle"   "haare_kontrolle"             "aufbewahrung_kontrolle"     
# [31] "allergie_bekommen"           "allergie_schlimm" 
#
# Contents:
# I.    Preliminary stuff: setwd, packages, read data, subset data, briefly explore data
# II.   Convert to factors
# III.  Convert to factors (yes/no inversion) (optional)
# IV.   Collapse levels and covert to factors
# V.    Univariate analysis
#   Va. Using two-level variables
# VI.   Two-variable analysis
#-------------------#

#----------------------#
# I. Preliminary stuff #
#----------------------#

# RUN SCRIPTS "t2p baseline 238.R" and "t2p resp symp 238.R" FIRST!!!

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
data16 <- data[,c(22:30,31:36,143:157,167:168)] # data16 = New subset. See info at beginning of script.
names(data16)

#--------------------------#
# II. Convert to factors: #
#--------------------------#
names(data16)
haare.factor <- factor(data$haare, labels = no.yes.labels)
arbeitsschuhe.factor <- factor(data$arbeitsschuhe, labels = no.yes.labels)
kleidung_aufb.factor <- factor(data$kleidung_aufb, labels = no.yes.labels)
desinfizieren.factor <- factor(data$desinfizieren, labels = no.yes.labels)
schutzbrille.factor <- factor(data$schutzbrille, labels = no.yes.labels)
kleidung_wohnraum.factor <- factor(data$kleidung_wohnraum, labels = no.yes.labels)
arbeitskleidung_letzteWoche.factor <- factor(data$arbeitskleidung_letzteWoche)
haare_letzteWoche.factor <- factor(data$haare_letzteWoche)
aufbewahrung_letzteWoche.factor <- factor(data$aufbewahrung_letzteWoche)
arbeitskleidung_intention.factor <- factor(data$arbeitskleidung_intention)
haare_intention.factor <- factor(data$haare_intention)
aufbewahrung_intention.factor <- factor(data$aufbewahrung_intention)
arbeitskleidung_einstellung.factor <- factor(data$arbeitskleidung_einstellung)
haare_einstellung.factor <- factor(data$haare_einstellung)
aufbewahrung_einstellung.factor <- factor(data$aufbewahrung_einstellung)
arbeitskleidung_norm.factor <- factor(data$arbeitskleidung_norm)
haare_norm.factor <- factor(data$haare_norm)
aufbewahrung_norm.factor <- factor(data$aufbewahrung_norm)
arbeitskleidung_kontrolle.factor <- factor(data$arbeitskleidung_kontrolle)
haare_kontrolle.factor <- factor(data$haare_kontrolle)
aufbewahrung_kontrolle.factor <- factor(data$aufbewahrung_kontrolle)
allergie_bekommen.factor <- factor(data$allergie_bekommen)
allergie_schlimm.factor <- factor(data$allergie_schlimm)

# Create dataset of factors to call the variables easily from now on:
# data16.factor <- data.frame(pfeifen.factor,pfeifen_ohne_erk.factor, asthma.factor,asthma_arzt.factor, medikamente.factor, nasenprobleme.factor, nasenprobleme_augen.factor, allergie_mutter.factor, allergie_vater.factor, haare.factor, arbeitsschuhe.factor, kleidung_aufb.factor, desinfizieren.factor, schutzbrille.factor, kleidung_wohnraum.factor, arbeitskleidung_letzteWoche.factor, haare_letzteWoche.factor, aufbewahrung_letzteWoche.factor, arbeitskleidung_intention.factor, haare_intention.factor, aufbewahrung_intention.factor, arbeitskleidung_einstellung.factor, haare_einstellung.factor, aufbewahrung_einstellung.factor, arbeitskleidung_norm.factor, haare_norm.factor, aufbewahrung_norm.factor, arbeitskleidung_kontrolle.factor, haare_kontrolle.factor, aufbewahrung_kontrolle.factor, allergie_bekommen.factor, allergie_schlimm.factor)

#-------------------------#
# III. Convert to factors: #
#   Yes / No inversion.   #
#-------------------------#

haare.factor <- factor(data$haare, labels = yes.no.labels, levels = c(1,0))
arbeitsschuhe.factor <- factor(data$arbeitsschuhe, labels = yes.no.labels, levels = c(1,0))
kleidung_aufb.factor <- factor(data$kleidung_aufb, labels = yes.no.labels, levels = c(1,0))
desinfizieren.factor <- factor(data$desinfizieren, labels = yes.no.labels, levels = c(1,0))
schutzbrille.factor <- factor(data$schutzbrille, labels = yes.no.labels, levels = c(1,0))
kleidung_wohnraum.factor <- factor(data$kleidung_wohnraum, labels = yes.no.labels, levels = c(1,0))

#-------------------------#
# IV. Collapse levels and #
#    convert to factors:  #
#-------------------------#

# Note: these variables have 5 levels (1:5). 
# They are being collapsed into 3 leves: 1+2 ,3+4+5.
# Some recoding was done also to change the direction in arbeitskleidung

# arbeitskleidung_letzteWoche
arbeitskleidung3_letzteWoche <- NULL
arbeitskleidung3_letzteWoche[arbeitskleidung_letzteWoche.factor==5] <- 1
arbeitskleidung3_letzteWoche[arbeitskleidung_letzteWoche.factor==2 |arbeitskleidung_letzteWoche.factor==3 |arbeitskleidung_letzteWoche.factor==4 | arbeitskleidung_letzteWoche.factor==1 ] <- 0
data$arbeitskleidung3_letzteWoche.factor <- as.factor(arbeitskleidung3_letzteWoche)

# haare_letzteWoche
haare3_letzteWoche <- NULL
haare3_letzteWoche[haare_letzteWoche.factor==1] <- 1
haare3_letzteWoche[haare_letzteWoche.factor==2 |haare_letzteWoche.factor==3 | haare_letzteWoche.factor==4 | haare_letzteWoche.factor==5 ] <- 0
data$haare3_letzteWoche.factor <- as.factor(haare3_letzteWoche)

# aufbewahrung3_letzteWoche
aufbewahrung3_letzteWoche <- NULL
aufbewahrung3_letzteWoche[aufbewahrung3_letzteWoche.factor==1] <- 1
aufbewahrung3_letzteWoche[aufbewahrung3_letzteWoche.factor==2 |aufbewahrung3_letzteWoche.factor==3 | aufbewahrung3_letzteWoche.factor==4 | aufbewahrung3_letzteWoche.factor==5 ] <- 0
data$aufbewahrung3_letzteWoche.factor <- as.factor(aufbewahrung3_letzteWoche)
data$aufbewahrung3_letzteWoche.factor <- factor(data$aufbewahrung3_letzteWoche.factor, levels=c(0,1))

# arbeitskleidung_intention
arbeitskleidung3_intention <- NULL
arbeitskleidung3_intention[arbeitskleidung_intention.factor==5] <- 1
arbeitskleidung3_intention[arbeitskleidung_intention.factor==2 |arbeitskleidung_intention.factor==3 |arbeitskleidung_intention.factor==4 | arbeitskleidung_intention.factor==5 ] <- 0
data$arbeitskleidung3_intention.factor <- factor(arbeitskleidung3_intention, levels=c(0,1))

# haare_intention
haare3_intention <- NULL
haare3_intention[haare_intention.factor==1] <- 1
haare3_intention[haare_intention.factor==2 |haare_intention.factor==3 | haare_intention.factor==4 | haare_intention.factor==5 ] <- 0
data$haare3_intention.factor <- as.factor(haare3_intention)

# aufbewahrung3_intention
aufbewahrung3_intention <- NULL
aufbewahrung3_intention[aufbewahrung3_intention.factor==1] <- 1
aufbewahrung3_intention[aufbewahrung3_intention.factor==2 |aufbewahrung3_intention.factor==3 | aufbewahrung3_intention.factor==4 | aufbewahrung3_intention.factor==5 ] <- 0
data$aufbewahrung3_intention.factor <- as.factor(aufbewahrung3_intention)

# arbeitskleidung_einstellung
arbeitskleidung3_einstellung <- NULL
arbeitskleidung3_einstellung[arbeitskleidung_einstellung.factor==5] <- 1
arbeitskleidung3_einstellung[arbeitskleidung_einstellung.factor==2 |arbeitskleidung_einstellung.factor==3 |arbeitskleidung_einstellung.factor==4 | arbeitskleidung_einstellung.factor==1 ] <- 0
data$arbeitskleidung3_einstellung.factor <- as.factor(arbeitskleidung3_einstellung)

# haare_einstellung
haare3_einstellung <- NULL
haare3_einstellung[haare_einstellung.factor==1] <- 1
haare3_einstellung[haare_einstellung.factor==2 |haare_einstellung.factor==3 | haare_einstellung.factor==4 | haare_einstellung.factor==5 ] <- 0
data$haare3_einstellung.factor <- as.factor(haare3_einstellung)

# aufbewahrung3_einstellung
aufbewahrung3_einstellung <- NULL
aufbewahrung3_einstellung[aufbewahrung3_einstellung.factor==1] <- 1
aufbewahrung3_einstellung[aufbewahrung3_einstellung.factor==2 |aufbewahrung3_einstellung.factor==3 | aufbewahrung3_einstellung.factor==4 | aufbewahrung3_einstellung.factor==5 ] <- 0
data$aufbewahrung3_einstellung.factor <- as.factor(aufbewahrung3_einstellung)

# arbeitskleidung_norm
arbeitskleidung3_norm <- NULL
arbeitskleidung3_norm[arbeitskleidung_norm.factor==5] <- 1
arbeitskleidung3_norm[arbeitskleidung_norm.factor==2 |arbeitskleidung_norm.factor==3 |arbeitskleidung_norm.factor==4 | arbeitskleidung_norm.factor==1 ] <- 0
data$arbeitskleidung3_norm.factor <- as.factor(arbeitskleidung3_norm)

# haare_norm
haare3_norm <- NULL
haare3_norm[haare_norm.factor==1] <- 1
haare3_norm[haare_norm.factor==2 |haare_norm.factor==3 | haare_norm.factor==4 | haare_norm.factor==5 ] <- 0
data$haare3_norm.factor <- as.factor(haare3_norm)

# aufbewahrung3_norm
aufbewahrung3_norm <- NULL
aufbewahrung3_norm[aufbewahrung3_norm.factor==1] <- 1
aufbewahrung3_norm[aufbewahrung3_norm.factor==2 |aufbewahrung3_norm.factor==3 | aufbewahrung3_norm.factor==4 | aufbewahrung3_norm.factor==5 ] <- 0
data$aufbewahrung3_norm.factor <- as.factor(aufbewahrung3_norm)

# arbeitskleidung_kontrolle
arbeitskleidung3_kontrolle <- NULL
arbeitskleidung3_kontrolle[arbeitskleidung_kontrolle.factor==5] <- 1
arbeitskleidung3_kontrolle[arbeitskleidung_kontrolle.factor==3 | arbeitskleidung_kontrolle.factor==4 | arbeitskleidung_kontrolle.factor==1 | arbeitskleidung_kontrolle.factor==2 ] <- 0
data$arbeitskleidung3_kontrolle.factor <- as.factor(arbeitskleidung3_kontrolle)

# haare_kontrolle
haare3_kontrolle <- NULL
haare3_kontrolle[haare_kontrolle.factor==1] <- 1
haare3_kontrolle[haare_kontrolle.factor==2 |haare_kontrolle.factor==3 | haare_kontrolle.factor==4 | haare_kontrolle.factor==5 ] <- 0
data$haare3_kontrolle.factor <- as.factor(haare3_kontrolle)

# aufbewahrung3_kontrolle
aufbewahrung3_kontrolle <- NULL
aufbewahrung3_kontrolle[aufbewahrung3_kontrolle.factor==1] <- 1
aufbewahrung3_kontrolle[aufbewahrung3_kontrolle.factor==2 |aufbewahrung3_kontrolle.factor==3 | aufbewahrung3_kontrolle.factor==4 | aufbewahrung3_kontrolle.factor==5 ] <- 0
data$aufbewahrung3_kontrolle.factor <- as.factor(aufbewahrung3_kontrolle)

# allergie_bekommen
allergie3_bekommen <- NULL
allergie3_bekommen[allergie_bekommen.factor==1 | allergie_bekommen.factor==2 ] <- 1
allergie3_bekommen[allergie_bekommen.factor==3 | allergie_bekommen.factor==4 | allergie_bekommen.factor==5 ] <- 2
labels.allergie3_bekommen <- c("very likely", "very unlikely")
allergie3_bekommen.factor <- factor(allergie3_bekommen, labels = labels.allergie3_bekommen)
# 1: very likely to get allergies in the next 5 years
# 2: very unlikely to get allergies in the next 5 years


# allergie_schlimm
allergie3_schlimm <- NULL
allergie3_schlimm[allergie_schlimm.factor==1 | allergie_schlimm.factor==2 ] <- 1
allergie3_schlimm[allergie_schlimm.factor==3 | allergie_schlimm.factor==4 | allergie_schlimm.factor==5 ] <- 2
labels.allergie3_schlimm <- c("not so bad", "very bad")
allergie3_schlimm.factor <- factor(allergie3_schlimm, labels = labels.allergie3_schlimm)
# 1: not bad to get allergies
# 2: very bad to get allergies


# nachher
# arbeitskleidung_letzteWoche_nach
arbeitskleidung3_letzteWoche_nachher <- NULL
arbeitskleidung3_letzteWoche_nachher[data$arbeitskleidung_letzteWoche_nach==5] <- 1
arbeitskleidung3_letzteWoche_nachher[data$arbeitskleidung_letzteWoche_nach==2 |data$arbeitskleidung_letzteWoche_nach==3 | data$arbeitskleidung_letzteWoche_nach==4 | data$arbeitskleidung_letzteWoche_nach==1 ] <- 0

# haare_letzteWoche_nachher
haare3_letzteWoche_nachher <- NULL
haare3_letzteWoche_nachher[data$haare_letzteWoche_nachher==1] <- 0
haare3_letzteWoche_nachher[data$haare_letzteWoche_nachher==2 | data$haare_letzteWoche_nachher==3 | data$haare_letzteWoche_nachher==4 | data$haare_letzteWoche_nachher==5 ] <- 1

# aufbewahrung3_letzteWoche_nachher
aufbewahrung3_letzteWoche_nachher <- NULL
aufbewahrung3_letzteWoche_nachher[data$aufbewahrung_letzteWoche_nachher==1] <- 0
aufbewahrung3_letzteWoche_nachher[data$aufbewahrung_letzteWoche_nachher==2 |data$aufbewahrung3_letzteWoche_nachher==3 | data$aufbewahrung_letzteWoche_nachher==4 | data$aufbewahrung3_letzteWoche_nachher==5 ] <- 1
data$aufbewahrung3_letzteWoche_nachher.factor <- as.factor(aufbewahrung3_letzteWoche_nachher)

# arbeitskleidung_intention
arbeitskleidung3_intention_nachher <- NULL
arbeitskleidung3_intention_nachher[data$arbeitskleidung_intention_nachhe==5] <- 1
arbeitskleidung3_intention_nachher[data$arbeitskleidung_intention_nachhe==2 |data$arbeitskleidung_intention_nachhe==3 |data$arbeitskleidung_intention_nachhe==4 | data$arbeitskleidung_intention_nachhe==1 ] <- 0

# haare_intention
haare3_intention_nachher <- NULL
haare3_intention_nachher[data$haare_intention_nachher==1] <- 0
haare3_intention_nachher[data$haare_intention_nachher==2 |data$haare_intention_nachher==3 | data$haare_intention_nachher==4 | data$haare_intention_nachher==5 ] <- 1

# aufbewahrung3_intention_nachher
aufbewahrung3_intention_nachher <- NULL
aufbewahrung3_intention_nachher[data$aufbewahrung_intention_nachher==1] <- 0
aufbewahrung3_intention_nachher[data$aufbewahrung_intention_nachher==2 |data$aufbewahrung_intention_nachher==3 | data$aufbewahrung_intention_nachher==4 | data$aufbewahrung_intention_nachher==5 ] <- 1

# arbeitskleidung_einstellung_nach
arbeitskleidung3_einstellung_nachher <- NULL
arbeitskleidung3_einstellung_nachher[data$arbeitskleidung_einstellung_nach==5] <- 1
arbeitskleidung3_einstellung_nachher[data$arbeitskleidung_einstellung_nach==2 |data$arbeitskleidung_einstellung_nach==3 |data$arbeitskleidung_einstellung_nach==4 | data$arbeitskleidung_einstellung_nach==1 ] <- 0

# haare_einstellung_nachher
haare3_einstellung_nachher <- NULL
haare3_einstellung_nachher[data$haare_einstellung_nachher==1] <- 0
haare3_einstellung_nachher[data$haare_einstellung_nachher==2 |data$haare_einstellung_nachher==3 | data$haare_einstellung_nachher==4 | data$haare_einstellung_nachher==5 ] <- 1

# aufbewahrung3_einstellung_nachher
aufbewahrung3_einstellung_nachher <- NULL
aufbewahrung3_einstellung_nachher[data$aufbewahrung_einstellung_nachher==1] <- 0
aufbewahrung3_einstellung_nachher[data$aufbewahrung_einstellung_nachher==2 |data$aufbewahrung_einstellung_nachher==3 | data$aufbewahrung_einstellung_nachher==4 | data$aufbewahrung_einstellung_nachher==5 ] <- 1

# arbeitskleidung_norm_nachher
arbeitskleidung3_norm_nachher <- NULL
arbeitskleidung3_norm_nachher[data$arbeitskleidung_norm_nachher==5] <- 1
arbeitskleidung3_norm_nachher[data$arbeitskleidung_norm_nachher==2 |data$arbeitskleidung_norm_nachher==3 |data$arbeitskleidung_norm_nachher==4 | data$arbeitskleidung_norm_nachher==1 ] <- 0

# haare_norm_nachher
haare3_norm_nachher <- NULL
haare3_norm_nachher[data$haare_norm_nachher==1] <- 0
haare3_norm_nachher[data$haare_norm_nachher==2 |data$haare_norm_nachher==3 | data$haare_norm_nachher==4 | data$haare_norm_nachher==5 ] <- 1

# aufbewahrung3_norm_nachher
aufbewahrung3_norm_nachher <- NULL
aufbewahrung3_norm_nachher[data$aufbewahrung_norm_nachher==1] <- 0
aufbewahrung3_norm_nachher[data$aufbewahrung_norm_nachher==2 |data$aufbewahrung_norm_nachher==3 | data$aufbewahrung_norm_nachher==4 | data$aufbewahrung_norm_nachher==5 ] <- 1

# arbeitskleidung_kontrolle_nachher
arbeitskleidung3_kontrolle_nachher <- NULL
arbeitskleidung3_kontrolle_nachher[data$arbeitskleidung_kontrolle_nachhe==5] <- 1
arbeitskleidung3_kontrolle_nachher[data$arbeitskleidung_kontrolle_nachhe==3 | data$arbeitskleidung_kontrolle_nachhe==4 | data$arbeitskleidung_kontrolle_nachhe==1 | data$arbeitskleidung_kontrolle_nachhe==2] <- 0

# haare_kontrolle_nachher
haare3_kontrolle_nachher <- NULL
haare3_kontrolle_nachher[data$haare_kontrolle_nachher==1] <- 0
haare3_kontrolle_nachher[data$haare_kontrolle_nachher==2 | data$haare_kontrolle_nachher==3 | data$haare_kontrolle_nachher==4 | data$haare_kontrolle_nachher==5 ] <- 1

# aufbewahrung3_kontrolle_nachher
aufbewahrung3_kontrolle_nachher <- NULL
aufbewahrung3_kontrolle_nachher[data$aufbewahrung_kontrolle_nachher==1] <- 0
aufbewahrung3_kontrolle_nachher[data$aufbewahrung_kontrolle_nachher==2 |data$aufbewahrung_kontrolle_nachher==3 | data$aufbewahrung_kontrolle_nachher==4 | data$aufbewahrung_kontrolle_nachher==5 ] <- 1

# Add missing variables to the whole dataset
data$asthma3 <- asthma3 #add asthma3 as a variable to the whole dataset
data$asthma.dx <- asthma.dx
data$allergies <- allergies
data$par.asthma <- par.asthma
data$arbeitskleidung3_letzteWoche <- arbeitskleidung3_letzteWoche
data$haare3_letzteWoche <- haare3_letzteWoche
data$aufbewahrung3_letzteWoche <- aufbewahrung3_letzteWoche
data$arbeitskleidung3_intention <- arbeitskleidung3_intention
data$haare3_intention <- haare3_intention
data$aufbewahrung3_intention <- aufbewahrung3_intention
data$arbeitskleidung3_einstellung <- arbeitskleidung3_einstellung
data$haare3_einstellung <- haare3_einstellung
data$aufbewahrung3_einstellung <- aufbewahrung3_einstellung
data$arbeitskleidung3_norm <- arbeitskleidung3_norm
data$haare3_norm <- haare3_norm
data$aufbewahrung3_norm <- aufbewahrung3_norm
data$arbeitskleidung3_kontrolle <- arbeitskleidung3_kontrolle
data$haare3_kontrolle <- haare3_kontrolle
data$aufbewahrung3_kontrolle <- aufbewahrung3_kontrolle
data$allergie3_bekommen <- allergie3_bekommen
data$allergie3_schlimm <- allergie3_schlimm
# nachher
data$arbeitskleidung3_letzteWoche_nachher <- arbeitskleidung3_letzteWoche_nachher
data$haare3_letzteWoche_nachher <- haare3_letzteWoche_nachher
data$aufbewahrung3_letzteWoche_nachher <- aufbewahrung3_letzteWoche_nachher
data$arbeitskleidung3_intention_nachher <- arbeitskleidung3_intention_nachher
data$haare3_intention_nachher <- haare3_intention_nachher
data$aufbewahrung3_intention_nachher <- aufbewahrung3_intention_nachher
data$arbeitskleidung3_einstellung_nachher <- arbeitskleidung3_einstellung_nachher
data$haare3_einstellun_nachherg <- haare3_einstellung_nachher
data$aufbewahrung3_einstellung_nachher <- aufbewahrung3_einstellung_nachher
data$arbeitskleidung3_norm_nachher <- arbeitskleidung3_norm_nachher
data$haare3_norm_nachher <- haare3_norm_nachher
data$aufbewahrung3_norm_nachher <- aufbewahrung3_norm_nachher
data$arbeitskleidung3_kontrolle_nachher <- arbeitskleidung3_kontrolle_nachher
data$haare3_kontrolle_nachher <- haare3_kontrolle_nachher
data$aufbewahrung3_kontrolle_nachher <- aufbewahrung3_kontrolle_nachher

#-------------------------#
# V. Univariate analysis: #   # five-level variables, not included in final.
#-------------------------#
# nrow(data) #n

# behavior:
# arbeitskleidung_letzteWoche
# cbind(table(arbeitskleidung_letzteWoche.factor), round(prop.table(table(arbeitskleidung_letzteWoche.factor)),4))
# sum(is.na(table(arbeitskleidung_letzteWoche.factor)))

# haare_letzteWoche
# cbind(table(haare_letzteWoche.factor), round(prop.table(table(haare_letzteWoche.factor)),4))
# sum(is.na(table(haare_letzteWoche.factor)))

# aufbewahrung_letzteWoche
# cbind(table(aufbewahrung_letzteWoche.factor), round(prop.table(table(aufbewahrung_letzteWoche.factor)),4))
# sum(is.na(table(aufbewahrung_letzteWoche.factor)))

# arbeitskleidung_intention
# cbind(table(arbeitskleidung_intention.factor), round(prop.table(table(arbeitskleidung_intention.factor)),4))
# sum(is.na(table(arbeitskleidung_intention.factor)))

# haare_intention
# cbind(table(haare_intention.factor), round(prop.table(table(haare_intention.factor)),4))
# sum(is.na(table(haare_intention.factor)))

# aufbewahrung_intention
# cbind(table(aufbewahrung_intention.factor), round(prop.table(table(aufbewahrung_intention.factor)),4))
# sum(is.na(table(aufbewahrung_intention.factor)))

# arbeitskleidung_einstellung
# cbind(table(arbeitskleidung_einstellung.factor), round(prop.table(table(arbeitskleidung_einstellung.factor)),4))
# sum(is.na(table(arbeitskleidung_einstellung.factor)))

# haare_einstellung
# cbind(table(haare_einstellung.factor), round(prop.table(table(haare_einstellung.factor)),4))
# sum(is.na(table(haare_einstellung.factor)))

# aufbewahrung_einstellung
# cbind(table(aufbewahrung_einstellung.factor), round(prop.table(table(aufbewahrung_einstellung.factor)),4))
# sum(is.na(table(aufbewahrung_einstellung.factor)))

# arbeitskleidung_norm
# cbind(table(arbeitskleidung_norm.factor), round(prop.table(table(arbeitskleidung_norm.factor)),4))
# sum(is.na(table(arbeitskleidung_norm.factor)))

# haare_norm
# cbind(table(haare_norm.factor), round(prop.table(table(haare_norm.factor)),4))
# sum(is.na(table(haare_norm.factor)))

# aufbewahrung_norm
# cbind(table(aufbewahrung_norm.factor), round(prop.table(table(aufbewahrung_norm.factor)),4))
# sum(is.na(table(aufbewahrung_norm.factor)))

# arbeitskleidung_kontrolle
# cbind(table(arbeitskleidung_kontrolle.factor), round(prop.table(table(arbeitskleidung_kontrolle.factor)),4))
# sum(is.na(table(arbeitskleidung_kontrolle.factor)))

# haare_kontrolle
# cbind(table(haare_kontrolle.factor), round(prop.table(table(haare_kontrolle.factor)),4))
# sum(is.na(table(haare_kontrolle.factor)))

# aufbewahrung_kontrolle
# cbind(table(aufbewahrung_kontrolle.factor), round(prop.table(table(aufbewahrung_kontrolle.factor)),4))
# sum(is.na(table(aufbewahrung_kontrolle.factor)))

# allergie_bekommen
# cbind(table(allergie_bekommen.factor), round(prop.table(table(allergie_bekommen.factor)),4))
# sum(is.na(table(allergie_bekommen.factor)))

# allergie_schlimm
# cbind(table(allergie_schlimm.factor), round(prop.table(table(allergie_schlimm.factor)),4))
# sum(is.na(table(allergie_schlimm.factor)))

#--------------------------------------#
# Va. Variable analysis (one variable) #
# (again, using two-level variables):  #
#--------------------------------------#

# knowledge (wissen)
# Haare
cbind(table(haare.factor), round(prop.table(table(haare.factor)),4))
sum(is.na(table(haare.factor)))

# Arbeitsschuhe
cbind(table(arbeitsschuhe.factor), round(prop.table(table(arbeitsschuhe.factor)),4))
sum(is.na(table(arbeitsschuhe.factor)))

# kleidung_aufb
cbind(table(kleidung_aufb.factor), round(prop.table(table(kleidung_aufb.factor)),4))
sum(is.na(table(kleidung_aufb.factor)))

# desinfizieren
cbind(table(desinfizieren.factor), round(prop.table(table(desinfizieren.factor)),4))
sum(is.na(table(desinfizieren.factor)))

# schutzbrille
cbind(table(schutzbrille.factor), round(prop.table(table(schutzbrille.factor)),4))
sum(is.na(table(schutzbrille.factor)))

# kleidung_wohnraum
cbind(table(kleidung_wohnraum.factor), round(prop.table(table(kleidung_wohnraum.factor)),4))
sum(is.na(table(kleidung_wohnraum.factor)))

# behavior
# arbeitskleidung_letzteWoche
cbind(table(arbeitskleidung3_letzteWoche.factor), round(prop.table(table(arbeitskleidung3_letzteWoche.factor)),4))
# sum(is.na(table(arbeitskleidung3_letzteWoche.factor)))
# 1: never wearing work clothes at home
# 2: always wearing work clothes at home


# haare_letzteWoche
cbind(table(haare3_letzteWoche.factor), round(prop.table(table(haare3_letzteWoche.factor)),4))
# sum(is.na(table(haare3_letzteWoche.factor)))
# 1: always washing hair after work
# 2: never washing hair after work

# aufbewahrung_letzteWoche
cbind(table(aufbewahrung3_letzteWoche.factor), round(prop.table(table(aufbewahrung3_letzteWoche.factor)),4))
# sum(is.na(table(aufbewahrung3_letzteWoche.factor)))
# 1: always keeping work clothes away
# 2: never keeping work clothes away

# all letzteWoche
data$all_letzteWoche <- NULL
data$all_letzteWoche[arbeitskleidung3_letzteWoche==1 & 
                       haare3_letzteWoche==1 &
                       aufbewahrung3_letzteWoche==1] <- 1
data$all_letzteWoche[arbeitskleidung3_letzteWoche==2 | 
                       haare3_letzteWoche==2 |
                       aufbewahrung3_letzteWoche==2] <- 2
data$all_letzteWoche.factor <- factor(data$all_letzteWoche)
# data$all_letzteWoche==1: all positive measures
# data$all_letzteWoche==2: not all positive measures
cbind(table(data$all_letzteWoche.factor), round(prop.table(table(data$all_letzteWoche.factor)),4))


# arbeitskleidung_intention
cbind(table(arbeitskleidung3_intention.factor), round(prop.table(table(arbeitskleidung3_intention.factor)),4))
# sum(is.na(table(arbeitskleidung3_intention.factor)))
# 1: not at all intending to wear work clothes at home
# 2: totally intending to wear work clothes at home

# haare_intention
cbind(table(haare3_intention.factor), round(prop.table(table(haare3_intention.factor)),4))
# sum(is.na(table(haare3_intention.factor)))
# 1: totally intending to wash hair after work
# 2: not at all intending to wash hair after work

# aufbewahrung_intention
cbind(table(aufbewahrung3_intention.factor), round(prop.table(table(aufbewahrung3_intention.factor)),4))
# sum(is.na(table(aufbewahrung3_intention.factor)))
# 1: totally intending to keep work clothes away
# 2: not at all intending to keep work clothes away

# all intention
data$all_intention <- NULL
data$all_intention[arbeitskleidung3_intention==1 & 
                       haare3_intention==1 &
                       aufbewahrung3_intention==1] <- 1
data$all_intention[arbeitskleidung3_intention==2 | 
                       haare3_intention==2 |
                       aufbewahrung3_intention==2] <- 2
data$all_intention.factor <- factor(data$all_intention)
# data$all_intention==1: all positive measures
# data$all_intention==2: not all positive measures
cbind(table(data$all_intention.factor), round(prop.table(table(data$all_intention.factor)),4))


# arbeitskleidung_einstellung
cbind(table(arbeitskleidung3_einstellung.factor), round(prop.table(table(arbeitskleidung3_einstellung.factor)),4))
# sum(is.na(table(arbeitskleidung3_einstellung.factor)))
# 1: negative attitude towards NOT wearing work clothes at home
# 2: positive attitude towards NOT wearing work clothes at home

# haare_einstellung
cbind(table(haare3_einstellung.factor), round(prop.table(table(haare3_einstellung.factor)),4))
# sum(is.na(table(haare3_einstellung.factor)))
# 1: positive attitude towards hair washing after work
# 2: negative attitude towards hair washing after work

# aufbewahrung_einstellung
cbind(table(aufbewahrung3_einstellung.factor), round(prop.table(table(aufbewahrung3_einstellung.factor)),4))
# sum(is.na(table(aufbewahrung3_einstellung.factor)))
# 1: positive attitude towards keeping work clothes away
# 2: negative attitude towards keeping work clothes away

# all einstellung
data$all_einstellung <- NULL
data$all_einstellung[arbeitskleidung3_einstellung==1 & 
                       haare3_einstellung==1 &
                       aufbewahrung3_einstellung==1] <- 1
data$all_einstellung[arbeitskleidung3_einstellung==2 | 
                       haare3_einstellung==2 |
                       aufbewahrung3_einstellung==2] <- 2
data$all_einstellung.factor <- factor(data$all_einstellung)
# data$all_einstellung==1: all positive measures
# data$all_einstellung==2: not all positive measures
cbind(table(data$all_einstellung.factor), round(prop.table(table(data$all_einstellung.factor)),4))


# arbeitskleidung_norm
cbind(table(arbeitskleidung3_norm.factor), round(prop.table(table(arbeitskleidung3_norm.factor)),4))
# sum(is.na(table(arbeitskleidung3_norm.factor)))
# 1: not at all seeing other peers NOT wearing work clothes at home
# 2: totally seeing other peers NOT wearing work clothes at home

# haare_norm
cbind(table(haare3_norm.factor), round(prop.table(table(haare3_norm.factor)),4))
# sum(is.na(table(haare3_norm.factor)))
# 1: totally seeing other peers washing hair after work
# 2: not at all seeing other peers washing hair after work

# aufbewahrung_norm
cbind(table(aufbewahrung3_norm.factor), round(prop.table(table(aufbewahrung3_norm.factor)),4))
# sum(is.na(table(aufbewahrung3_norm.factor)))
# 1: totally seeing other peers keeping work clothes away
# 2: not at all seeing other peers keeping work clothes away

# all norm
data$all_norm <- NULL
data$all_norm[arbeitskleidung3_norm==1 & 
                       haare3_norm==1 &
                       aufbewahrung3_norm==1] <- 1
data$all_norm[arbeitskleidung3_norm==2 | 
                       haare3_norm==2 |
                       aufbewahrung3_norm==2] <- 2
data$all_norm.factor <- factor(data$all_norm)
# data$all_norm==1: all positive measures
# data$all_norm==2: not all positive measures
cbind(table(data$all_norm.factor), round(prop.table(table(data$all_norm.factor)),4))


# arbeitskleidung_kontrolle
cbind(table(arbeitskleidung3_kontrolle.factor), round(prop.table(table(arbeitskleidung3_kontrolle.factor)),4))
# sum(is.na(table(arbeitskleidung3_kontrolle.factor)))
# 1: totally in control over NOT wearing work clothes at home
# 2: not at all in control over NOT wearing work clothes at home

# haare_kontrolle
cbind(table(haare3_kontrolle.factor), round(prop.table(table(haare3_kontrolle.factor)),4))
# sum(is.na(table(haare3_kontrolle.factor)))
# 1: totally in control of washing hair after work
# 2: not at all in control of washing hair after work

# aufbewahrung_kontrolle
cbind(table(aufbewahrung3_kontrolle.factor), round(prop.table(table(aufbewahrung3_kontrolle.factor)),4))
# sum(is.na(table(aufbewahrung3_kontrolle.factor)))
# 1: totally in control of keeping work clothes away
# 2: not at all in control of keeping work clothes away

# all kontrolle
data$all_kontrolle <- NULL
data$all_kontrolle[arbeitskleidung3_kontrolle==1 & 
                       haare3_kontrolle==1 &
                       aufbewahrung3_kontrolle==1] <- 1
data$all_kontrolle[arbeitskleidung3_kontrolle==2 | 
                       haare3_kontrolle==2 |
                       aufbewahrung3_kontrolle==2] <- 2
data$all_kontrolle.factor <- factor(data$all_kontrolle)
# data$all_kontrolle==1: all positive measures
# data$all_kontrolle==2: not all positive measures
cbind(table(data$all_kontrolle.factor), round(prop.table(table(data$all_kontrolle.factor)),4))


# allergie3_bekommen
cbind(table(allergie3_bekommen.factor), round(prop.table(table(allergie3_bekommen.factor)),4))
# sum(is.na(table(allergie3_bekommen.factor)))
# 1: very likely to get allergies in the next 5 years
# 2: very unlikely to get allergies in the next 5 years

# allergie3_schlimm
cbind(table(allergie3_schlimm.factor), round(prop.table(table(allergie3_schlimm.factor)),4))
# sum(is.na(table(allergie3_schlimm.factor)))
# 1: not bad to get allergies
# 2: very bad to get allergies

#----------------------------------------#
# VI. Variable analysis (two variables): #
#----------------------------------------#

# I. Respiratory Symptoms vs. Wissen
# 1. ASTHMA VS.
# Haare
cbind(table(asthma3.factor, haare.factor), round(prop.table(table(asthma3.factor, haare.factor),1),4))
fisher.test(table(asthma3.factor, haare.factor))$p.value #NS

# Arbeitsschuhe
cbind(table(asthma3.factor, arbeitsschuhe.factor), round(prop.table(table(asthma3.factor, arbeitsschuhe.factor),1),4))
fisher.test(table(asthma3.factor, arbeitsschuhe.factor))$p.value #NS

# Arbeitskleidung aufbewahren
cbind(table(asthma3.factor, kleidung_aufb.factor), round(prop.table(table(asthma3.factor, kleidung_aufb.factor),1),4))
fisher.test(table(asthma3.factor, kleidung_aufb.factor))$p.value #NS

# Stall desinfizieren
cbind(table(asthma3.factor, desinfizieren.factor), round(prop.table(table(asthma3.factor, desinfizieren.factor),1),4))
fisher.test(table(asthma3.factor, desinfizieren.factor))$p.value #NS

# Schutzbrille tragen
cbind(table(asthma3.factor, schutzbrille.factor), round(prop.table(table(asthma3.factor, schutzbrille.factor),1),4))
fisher.test(table(asthma3.factor, schutzbrille.factor))$p.value #NS

# Kleidung nicht im Wohnraum tragen
cbind(table(asthma3.factor, kleidung_wohnraum.factor), round(prop.table(table(asthma3.factor, kleidung_wohnraum.factor),1),4))
fisher.test(table(asthma3.factor, kleidung_wohnraum.factor))$p.value #NS

# 2. ASTHMA DX VS.        # Not included in final analysis
# Haare
# cbind(table(asthma.dx.factor, haare.factor), round(prop.table(table(asthma.dx.factor, haare.factor),1),4))
# fisher.test(table(asthma.dx.factor, haare.factor))$p.value #NS

# Arbeitsschuhe
# cbind(table(asthma.dx.factor, arbeitsschuhe.factor), round(prop.table(table(asthma.dx.factor, arbeitsschuhe.factor),1),4))
# fisher.test(table(asthma.dx.factor, arbeitsschuhe.factor))$p.value #NS

# Arbeitskleidung aufbewahren
# cbind(table(asthma.dx.factor, kleidung_aufb.factor), round(prop.table(table(asthma.dx.factor, kleidung_aufb.factor),1),4))
# fisher.test(table(asthma.dx.factor, kleidung_aufb.factor))$p.value #NS

# Stall desinfizieren
# cbind(table(asthma.dx.factor, desinfizieren.factor), round(prop.table(table(asthma.dx.factor, desinfizieren.factor),1),4))
# fisher.test(table(asthma.dx.factor, desinfizieren.factor))$p.value #NS

# Schutzbrille tragen
# cbind(table(asthma.dx.factor, schutzbrille.factor), round(prop.table(table(asthma.dx.factor, schutzbrille.factor),1),4))
# fisher.test(table(asthma.dx.factor, schutzbrille.factor))$p.value #NS

# Kleidung nicht im Wohnraum tragen
# cbind(table(asthma.dx.factor, kleidung_wohnraum.factor), round(prop.table(table(asthma.dx.factor, kleidung_wohnraum.factor),1),4))
# fisher.test(table(asthma.dx.factor, kleidung_wohnraum.factor))$p.value #NS

# 3. ALLERGIES VS.
# Haare
cbind(table(allergies.factor, haare.factor), round(prop.table(table(allergies.factor, haare.factor),1),4))
chisq.test(table(allergies.factor, haare.factor))$p.value #NS

# Arbeitsschuhe
cbind(table(allergies.factor, arbeitsschuhe.factor), round(prop.table(table(allergies.factor, arbeitsschuhe.factor),1),4))
chisq.test(table(allergies.factor, arbeitsschuhe.factor))$p.value #NS 

# Arbeitskleidung aufbewahren
cbind(table(allergies.factor, kleidung_aufb.factor), round(prop.table(table(allergies.factor, kleidung_aufb.factor),1),4))
chisq.test(table(allergies.factor, kleidung_aufb.factor))$p.value #NS

# Stall desinfizieren
cbind(table(allergies.factor, desinfizieren.factor), round(prop.table(table(allergies.factor, desinfizieren.factor),1),4))
chisq.test(table(allergies.factor, desinfizieren.factor))$p.value #NS

# Schutzbrille tragen
cbind(table(allergies.factor, schutzbrille.factor), round(prop.table(table(allergies.factor, schutzbrille.factor),1),4))
chisq.test(table(allergies.factor, schutzbrille.factor))$p.value #NS

# Kleidung nicht im Wohnraum tragen
cbind(table(allergies.factor, kleidung_wohnraum.factor), round(prop.table(table(allergies.factor, kleidung_wohnraum.factor),1),4))
chisq.test(table(allergies.factor, kleidung_wohnraum.factor))$p.value #NS

# 4. PARENTAL ASTHMA VS.
# Haare
cbind(table(par.asthma.factor, haare.factor), round(prop.table(table(par.asthma.factor, haare.factor),1),4))
chisq.test(table(par.asthma.factor, haare.factor))$p.value #NS

# Arbeitsschuhe
cbind(table(par.asthma.factor, arbeitsschuhe.factor), round(prop.table(table(par.asthma.factor, arbeitsschuhe.factor),1),4))
chisq.test(table(par.asthma.factor, arbeitsschuhe.factor))$p.value #NS 

# Arbeitskleidung aufbewahren
cbind(table(par.asthma.factor, kleidung_aufb.factor), round(prop.table(table(par.asthma.factor, kleidung_aufb.factor),1),4))
chisq.test(table(par.asthma.factor, kleidung_aufb.factor))$p.value #NS

# Stall desinfizieren
cbind(table(par.asthma.factor, desinfizieren.factor), round(prop.table(table(par.asthma.factor, desinfizieren.factor),1),4))
chisq.test(table(par.asthma.factor, desinfizieren.factor))$p.value #NS

# Schutzbrille tragen
cbind(table(par.asthma.factor, schutzbrille.factor), round(prop.table(table(par.asthma.factor, schutzbrille.factor),1),4))
chisq.test(table(par.asthma.factor, schutzbrille.factor))$p.value #NS

# Kleidung nicht im Wohnraum tragen
cbind(table(par.asthma.factor, kleidung_wohnraum.factor), round(prop.table(table(par.asthma.factor, kleidung_wohnraum.factor),1),4))
chisq.test(table(par.asthma.factor, kleidung_wohnraum.factor))$p.value #NS

#---

# II. Respiratory Symptoms vs. Behavior

# 1. ASTHMA VS.
# 1a. BEHAVIOR LAST WEEK
# arbeitskleidung_letzteWoche
cbind(table(asthma3.factor, arbeitskleidung3_letzteWoche.factor), round(prop.table(table(asthma3.factor, arbeitskleidung3_letzteWoche.factor),1),4))
fisher.test(asthma3, arbeitskleidung3_letzteWoche)$p.value # [1] 0.5782152529

# haare_letzteWoche
cbind(table(asthma3.factor, haare3_letzteWoche.factor), round(prop.table(table(asthma3.factor, haare3_letzteWoche.factor),1),4))
fisher.test(asthma3, data$haare3_letzteWoche)$p.value #[1] 0.7023601

# aufbewahrung_letzteWoche
cbind(table(asthma3.factor, aufbewahrung3_letzteWoche.factor), round(prop.table(table(asthma3.factor, aufbewahrung3_letzteWoche.factor),1),4))
fisher.test(asthma3, data$aufbewahrung3_letzteWoche)$p.value #[1] 0.08243049

# all letzteWoche
cbind(table(asthma3.factor, data$all_letzteWoche.factor), round(prop.table(table(asthma3.factor, data$all_letzteWoche.factor),1),4))
fisher.test(asthma3, data$all_letzteWoche.factor)$p.value #[1] 0.3278393


# 1b. BEHAVIOR INTENTION IN THE NEXT SIX MONTHS
# arbeitskleidung_intention
cbind(table(asthma3.factor, arbeitskleidung3_intention.factor), round(prop.table(table(asthma3.factor, arbeitskleidung3_intention.factor),1),4))
fisher.test(asthma3, data$arbeitskleidung3_intention)$p.value # [1] 0.4957648

# haare_intention
cbind(table(asthma3.factor, haare3_intention.factor), round(prop.table(table(asthma3.factor, haare3_intention.factor),1),4))
fisher.test(asthma3, data$haare3_intention)$p.value # [1] 0.2290292

# aufbewahrung_intention
cbind(table(asthma3.factor, aufbewahrung3_intention.factor), round(prop.table(table(asthma3.factor, aufbewahrung3_intention.factor),1),4))
fisher.test(asthma3, data$aufbewahrung3_intention)$p.value # [1] 1

# all intention
cbind(table(asthma3.factor, data$all_intention.factor), round(prop.table(table(asthma3.factor, data$all_intention.factor),1),4))
fisher.test(asthma3, data$all_intention.factor)$p.value #[1] 0.6149514

# 1c. ATTITUDE TOWARDS
# arbeitskleidung_einstellung
cbind(table(asthma3.factor, arbeitskleidung3_einstellung.factor), round(prop.table(table(asthma3.factor, arbeitskleidung3_einstellung.factor),1),4))
fisher.test(asthma3, data$arbeitskleidung3_einstellung)$p.value # [1] 0.7878873

# haare_einstellung
cbind(table(asthma3.factor, haare3_einstellung.factor), round(prop.table(table(asthma3.factor, haare3_einstellung.factor),1),4))
fisher.test(asthma3, data$haare3_einstellung)$p.value # [1] 1

# aufbewahrung_einstellung
cbind(table(asthma3.factor, aufbewahrung3_einstellung.factor), round(prop.table(table(asthma3.factor, aufbewahrung3_einstellung.factor),1),4))
fisher.test(asthma3, data$aufbewahrung3_einstellung)$p.value # [1] 0.4659936
names(data)
# all letzteWoche
cbind(table(asthma3.factor, data$all_einstellung.factor), round(prop.table(table(asthma3.factor, data$all_einstellung.factor),1),4))
fisher.test(asthma3, data$all_einstellung.factor)$p.value #[1] 1


# 1d. PEER BEHAVIOR
# arbeitskleidung_norm
cbind(table(asthma3.factor, arbeitskleidung3_norm.factor), round(prop.table(table(asthma3.factor, arbeitskleidung3_norm.factor),1),4))
fisher.test(asthma3, data$arbeitskleidung3_norm)$p.value # [1] 0.7811357

# haare_norm
cbind(table(asthma3.factor, haare3_norm.factor), round(prop.table(table(asthma3.factor, haare3_norm.factor),1),4))
fisher.test(asthma3, data$haare3_norm)$p.value # [1] 0.7450235

# aufbewahrung_norm
cbind(table(asthma3.factor, aufbewahrung3_norm.factor), round(prop.table(table(asthma3.factor, aufbewahrung3_norm.factor),1),4))
fisher.test(asthma3, data$aufbewahrung3_norm)$p.value # [1] 0.3771825

# all norm
cbind(table(asthma3.factor, data$all_norm.factor), round(prop.table(table(asthma3.factor, data$all_norm.factor),1),4))
chisq.test(asthma3, data$all_norm)$p.value #[1] 0.7515873


# 1e. PERCEIVED BEHAVIORAL CONTROL
# arbeitskleidung_kontrolle
cbind(table(asthma3.factor, arbeitskleidung3_kontrolle.factor), round(prop.table(table(asthma3.factor, arbeitskleidung3_kontrolle.factor),1),4))
chisq.test(asthma3, data$arbeitskleidung3_kontrolle)$p.value # [1] 0.9864635

# haare_kontrolle
cbind(table(asthma3.factor, haare3_kontrolle.factor), round(prop.table(table(asthma3.factor, haare3_kontrolle.factor),1),4))
fisher.test(asthma3, data$haare3_kontrolle)$p.value # [1] 1

# aufbewahrung_kontrolle
cbind(table(asthma3.factor, aufbewahrung3_kontrolle.factor), round(prop.table(table(asthma3.factor, aufbewahrung3_kontrolle.factor),1),4))
fisher.test(asthma3, data$aufbewahrung3_kontrolle)$p.value # [1] 1

# all kontrolle
cbind(table(asthma3.factor, data$all_kontrolle.factor), round(prop.table(table(asthma3.factor, data$all_kontrolle.factor),1),4))
chisq.test(asthma3, data$all_kontrolle)$p.value # [1] 1

#---

# 2. ASTHMA DX VS.                # Not included in final analyses
# 2a. BEHAVIOR LAST WEEK
# arbeitskleidung_letzteWoche
# cbind(table(asthma.dx.factor, arbeitskleidung3_letzteWoche.factor), round(prop.table(table(asthma.dx.factor, arbeitskleidung3_letzteWoche.factor),1),4))
# fisher.test(asthma.dx, arbeitskleidung3_letzteWoche)$p.value # [1] 0.4636303

# haare_letzteWoche
# cbind(table(asthma.dx.factor, haare3_letzteWoche.factor), round(prop.table(table(asthma.dx.factor, haare3_letzteWoche.factor),1),4))
# fisher.test(asthma.dx, data$haare3_letzteWoche)$p.value # [1] 1

# aufbewahrung_letzteWoche
# cbind(table(asthma.dx.factor, aufbewahrung3_letzteWoche.factor), round(prop.table(table(asthma.dx.factor, aufbewahrung3_letzteWoche.factor),1),4))
# fisher.test(asthma.dx, data$aufbewahrung3_letzteWoche)$p.value #[1] 1


# 2b. BEHAVIOR INTENTION IN THE NEXT SIX MONTHS
# arbeitskleidung_intention
# cbind(table(asthma.dx.factor, arbeitskleidung3_intention.factor), round(prop.table(table(asthma.dx.factor, arbeitskleidung3_intention.factor),1),4))
# fisher.test(asthma.dx, data$arbeitskleidung3_intention)$p.value # [1] 1

# haare_intention
# cbind(table(asthma.dx.factor, haare3_intention.factor), round(prop.table(table(asthma.dx.factor, haare3_intention.factor),1),4))
# fisher.test(asthma.dx, data$haare3_intention)$p.value # [1] 0.6098022

# aufbewahrung_intention
# cbind(table(asthma.dx.factor, aufbewahrung3_intention.factor), round(prop.table(table(asthma.dx.factor, aufbewahrung3_intention.factor),1),4))
# fisher.test(asthma.dx, data$aufbewahrung3_intention)$p.value # [1] 0.4465285


# 2c. ATTITUDE TOWARDS
# arbeitskleidung_einstellung
# cbind(table(asthma.dx.factor, arbeitskleidung3_einstellung.factor), round(prop.table(table(asthma.dx.factor, arbeitskleidung3_einstellung.factor),1),4))
# fisher.test(asthma.dx, data$arbeitskleidung3_einstellung)$p.value # [1] 0.7338273

# haare_einstellung
# cbind(table(asthma.dx.factor, haare3_einstellung.factor), round(prop.table(table(asthma.dx.factor, haare3_einstellung.factor),1),4))
# fisher.test(asthma.dx, data$haare3_einstellung)$p.value # [1] 0.5302732

# aufbewahrung_einstellung
# cbind(table(asthma.dx.factor, aufbewahrung3_einstellung.factor), round(prop.table(table(asthma.dx.factor, aufbewahrung3_einstellung.factor),1),4))
# fisher.test(asthma.dx, data$aufbewahrung3_einstellung)$p.value # [1] 0.3151169


# 2d. PEER BEHAVIOR
# arbeitskleidung_norm
# cbind(table(asthma.dx.factor, arbeitskleidung3_norm.factor), round(prop.table(table(asthma.dx.factor, arbeitskleidung3_norm.factor),1),4))
# fisher.test(asthma.dx, data$arbeitskleidung3_norm)$p.value # [1] 1

# haare_norm
# cbind(table(asthma.dx.factor, haare3_norm.factor), round(prop.table(table(asthma.dx.factor, haare3_norm.factor),1),4))
# fisher.test(asthma.dx, data$haare3_norm)$p.value # [1] 0.2186437

# aufbewahrung_norm
# cbind(table(asthma.dx.factor, aufbewahrung3_norm.factor), round(prop.table(table(asthma.dx.factor, aufbewahrung3_norm.factor),1),4))
# fisher.test(asthma.dx, data$aufbewahrung3_norm)$p.value # [1] 1


# 2e. PERCEIVED BEHAVIORAL CONTROL
# arbeitskleidung_kontrolle
# cbind(table(asthma.dx.factor, arbeitskleidung3_kontrolle.factor), round(prop.table(table(asthma.dx.factor, arbeitskleidung3_kontrolle.factor),1),4))
# fisher.test(asthma.dx, data$arbeitskleidung3_kontrolle)$p.value # [1] 0.7531308

# haare_kontrolle
# cbind(table(asthma.dx.factor, haare3_kontrolle.factor), round(prop.table(table(asthma.dx.factor, haare3_kontrolle.factor),1),4))
# fisher.test(asthma.dx, data$haare3_kontrolle)$p.value # [1] 0.6073634

# aufbewahrung_kontrolle
# cbind(table(asthma.dx.factor, aufbewahrung3_kontrolle.factor), round(prop.table(table(asthma.dx.factor, aufbewahrung3_kontrolle.factor),1),4))
# fisher.test(asthma.dx, data$aufbewahrung3_kontrolle)$p.value # [1] 1

#---

# 3. ALLERGIES VS.
# 3a. BEHAVIOR LAST WEEK
# arbeitskleidung_letzteWoche
cbind(table(allergies.factor, arbeitskleidung3_letzteWoche.factor), round(prop.table(table(allergies.factor, arbeitskleidung3_letzteWoche.factor),1),4))
chisq.test(allergies, data$arbeitskleidung3_letzteWoche)$p.value # [1] 0.2114723

# haare_letzteWoche
cbind(table(allergies.factor, haare3_letzteWoche.factor), round(prop.table(table(allergies.factor, haare3_letzteWoche.factor),1),4))
chisq.test(allergies, data$haare3_letzteWoche)$p.value # [1] 0.9864971

# aufbewahrung_letzteWoche
cbind(table(allergies.factor, aufbewahrung3_letzteWoche.factor), round(prop.table(table(allergies.factor, aufbewahrung3_letzteWoche.factor),1),4))
chisq.test(allergies, data$aufbewahrung3_letzteWoche)$p.value # [1] 0.7447381

# all letzteWoche
cbind(table(allergies.factor, data$all_letzteWoche.factor), round(prop.table(table(allergies.factor, data$all_letzteWoche.factor),1),4))
chisq.test(allergies, data$all_letzteWoche)$p.value # [1] 0.5165954

# 3b. BEHAVIOR INTENTION IN THE NEXT SIX MONTHS
# arbeitskleidung_intention
cbind(table(allergies.factor, arbeitskleidung3_intention.factor), round(prop.table(table(allergies.factor, arbeitskleidung3_intention.factor),1),4))
chisq.test(allergies, data$arbeitskleidung_intention)$p.value # [1] 0.8075797

# haare_intention
cbind(table(allergies.factor, haare3_intention.factor), round(prop.table(table(allergies.factor, haare3_intention.factor),1),4))
chisq.test(allergies, data$haare3_intention)$p.value # [1] 0.9668296

# aufbewahrung_intention
cbind(table(allergies.factor, aufbewahrung3_intention.factor), round(prop.table(table(allergies.factor, aufbewahrung3_intention.factor),1),4))
chisq.test(allergies, data$aufbewahrung3_intention)$p.value # [1] 0.55106931

# all intention
cbind(table(allergies.factor, data$all_intention.factor), round(prop.table(table(allergies.factor, data$all_intention.factor),1),4))
chisq.test(allergies, data$all_intention)$p.value # [1] 0.6886002

# 3c. ATTITUDE TOWARDS
# arbeitskleidung_einstellung
cbind(table(allergies.factor, arbeitskleidung3_einstellung.factor), round(prop.table(table(allergies.factor, arbeitskleidung3_einstellung.factor),1),4))
chisq.test(allergies, data$arbeitskleidung3_einstellung)$p.value # [1] 0.409696

# haare_einstellung
cbind(table(allergies.factor, haare3_einstellung.factor), round(prop.table(table(allergies.factor, haare3_einstellung.factor),1),4))
fisher.test(allergies, data$haare3_einstellung)$p.value # [1] 0.3985471

# aufbewahrung_einstellung
cbind(table(allergies.factor, aufbewahrung3_einstellung.factor), round(prop.table(table(allergies.factor, aufbewahrung3_einstellung.factor),1),4))
fisher.test(allergies, data$aufbewahrung3_einstellung)$p.value # [1] 0.4302092

# all einstellung
cbind(table(allergies.factor, data$all_einstellung.factor), round(prop.table(table(allergies.factor, data$all_einstellung.factor),1),4))
chisq.test(allergies, data$all_einstellung)$p.value # [1] 1

# 3d. PEER BEHAVIOR
# arbeitskleidung_norm
cbind(table(allergies.factor, arbeitskleidung3_norm.factor), round(prop.table(table(allergies.factor, arbeitskleidung3_norm.factor),1),4))
chisq.test(allergies, data$arbeitskleidung3_norm)$p.value # [1] 0.3743319

# haare_norm
cbind(table(allergies.factor, haare3_norm.factor), round(prop.table(table(allergies.factor, haare3_norm.factor),1),4))
chisq.test(allergies, data$haare3_norm)$p.value # [1] 0.9796291

# aufbewahrung_norm
cbind(table(allergies.factor, aufbewahrung3_norm.factor), round(prop.table(table(allergies.factor, aufbewahrung3_norm.factor),1),4))
chisq.test(allergies, data$aufbewahrung3_norm)$p.value # [1] 0.7460719

# all norm
cbind(table(allergies.factor, data$all_norm.factor), round(prop.table(table(allergies.factor, data$all_norm.factor),1),4))
chisq.test(allergies, data$all_norm)$p.value # [1] 1

# 3e. PERCEIVED BEHAVIORAL CONTROL
# arbeitskleidung_kontrolle
cbind(table(allergies.factor, arbeitskleidung3_kontrolle.factor), round(prop.table(table(allergies.factor, arbeitskleidung3_kontrolle.factor),1),4))
chisq.test(allergies, data$arbeitskleidung3_kontrolle)$p.value # [1] 0.7627291

# haare_kontrolle
cbind(table(allergies.factor, haare3_kontrolle.factor), round(prop.table(table(allergies.factor, haare3_kontrolle.factor),1),4))
chisq.test(allergies, data$haare3_kontrolle)$p.value # [1] 1

# aufbewahrung_kontrolle
cbind(table(allergies.factor, aufbewahrung3_kontrolle.factor), round(prop.table(table(allergies.factor, aufbewahrung3_kontrolle.factor),1),4))
chisq.test(allergies, data$aufbewahrung3_kontrolle)$p.value # [1] 0.06684256

# all kontrolle
cbind(table(allergies.factor, data$all_kontrolle.factor), round(prop.table(table(allergies.factor, data$all_kontrolle.factor),1),4))
chisq.test(allergies, data$all_kontrolle)$p.value # [1] 0.6722591

#---

# 4. PARENTAL ASTHMA VS.
# 4a. BEHAVIOR LAST WEEK
# arbeitskleidung_letzteWoche
cbind(table(par.asthma.factor, arbeitskleidung3_letzteWoche.factor), round(prop.table(table(par.asthma.factor, arbeitskleidung3_letzteWoche.factor),1),4))
chisq.test(par.asthma, data$arbeitskleidung3_letzteWoche)$p.value # [1] 0.7853494

# haare_letzteWoche
cbind(table(par.asthma.factor, haare3_letzteWoche.factor), round(prop.table(table(par.asthma.factor, haare3_letzteWoche.factor),1),4))
chisq.test(par.asthma, data$haare3_letzteWoche)$p.value # [1] 0.1564053

# aufbewahrung_letzteWoche
cbind(table(par.asthma.factor, aufbewahrung3_letzteWoche.factor), round(prop.table(table(par.asthma.factor, aufbewahrung3_letzteWoche.factor),1),4))
chisq.test(par.asthma, data$aufbewahrung3_letzteWoche)$p.value # [1] 0.8715445

# all letzteWoche
cbind(table(par.asthma.factor, data$all_letzteWoche.factor), round(prop.table(table(par.asthma.factor, data$all_letzteWoche.factor),1),4))
chisq.test(par.asthma, data$all_letzteWoche)$p.value # [1] 0.862464

# 4b. BEHAVIOR INTENTION IN THE NEXT SIX MONTHS
# arbeitskleidung_intention
cbind(table(par.asthma.factor, arbeitskleidung3_intention.factor), round(prop.table(table(par.asthma.factor, arbeitskleidung3_intention.factor),1),4))
chisq.test(par.asthma, data$arbeitskleidung3_intention)$p.value # [1] 0.7982438

# haare_intention
cbind(table(par.asthma.factor, haare3_intention.factor), round(prop.table(table(par.asthma.factor, haare3_intention.factor),1),4))
fisher.test(par.asthma, data$haare3_intention)$p.value # [1] 0.7921558

# aufbewahrung_intention
cbind(table(par.asthma.factor, aufbewahrung3_intention.factor), round(prop.table(table(par.asthma.factor, aufbewahrung3_intention.factor),1),4))
chisq.test(par.asthma, data$aufbewahrung3_intention)$p.value # [1] 0.4330578

# all intention
cbind(table(par.asthma.factor, data$all_intention.factor), round(prop.table(table(par.asthma.factor, data$all_intention.factor),1),4))
chisq.test(par.asthma, data$all_intention)$p.value # [1] 0.3111213

# 4c. ATTITUDE TOWARDS
# arbeitskleidung_einstellung
cbind(table(par.asthma.factor, arbeitskleidung3_einstellung.factor), round(prop.table(table(par.asthma.factor, arbeitskleidung3_einstellung.factor),1),4))
chisq.test(par.asthma, data$arbeitskleidung3_einstellung)$p.value # [1] 0.9415881

# haare_einstellung
cbind(table(par.asthma.factor, haare3_einstellung.factor), round(prop.table(table(par.asthma.factor, haare3_einstellung.factor),1),4))
fisher.test(par.asthma, data$haare3_einstellung)$p.value # [1] 0.7435647

# aufbewahrung_einstellung
cbind(table(par.asthma.factor, aufbewahrung3_einstellung.factor), round(prop.table(table(par.asthma.factor, aufbewahrung3_einstellung.factor),1),4))
fisher.test(par.asthma, data$aufbewahrung3_einstellung)$p.value # [1] 1

# all einstellung
cbind(table(par.asthma.factor, data$all_einstellung.factor), round(prop.table(table(par.asthma.factor, data$all_einstellung.factor),1),4))
chisq.test(par.asthma, data$all_einstellung)$p.value # [1] 0.1271545

# 4d. PEER BEHAVIOR
# arbeitskleidung_norm
cbind(table(par.asthma.factor, arbeitskleidung3_norm.factor), round(prop.table(table(par.asthma.factor, arbeitskleidung3_norm.factor),1),4))
chisq.test(par.asthma, data$arbeitskleidung3_norm)$p.value # [1] 0.1262352

# haare_norm
cbind(table(par.asthma.factor, haare3_norm.factor), round(prop.table(table(par.asthma.factor, haare3_norm.factor),1),4)) 
chisq.test(par.asthma, data$haare3_norm)$p.value # [1] 1

# aufbewahrung_norm
cbind(table(par.asthma.factor, aufbewahrung3_norm.factor), round(prop.table(table(par.asthma.factor, aufbewahrung3_norm.factor),1),4))
chisq.test(par.asthma, data$aufbewahrung3_norm)$p.value # [1] 0.4661797

# all norm
cbind(table(par.asthma.factor, data$all_norm.factor), round(prop.table(table(par.asthma.factor, data$all_norm.factor),1),4))
chisq.test(par.asthma, data$all_norm)$p.value # [1] 1

# 4e. PERCEIVED BEHAVIORAL CONTROL
# arbeitskleidung_kontrolle
cbind(table(par.asthma.factor, arbeitskleidung3_kontrolle.factor), round(prop.table(table(par.asthma.factor, arbeitskleidung3_kontrolle.factor),1),4))
chisq.test(par.asthma, data$arbeitskleidung3_kontrolle)$p.value # [1] 0.7989541

# haare_kontrolle
cbind(table(par.asthma.factor, haare3_kontrolle.factor), round(prop.table(table(par.asthma.factor, haare3_kontrolle.factor),1),4))
fisher.test(par.asthma, data$haare3_kontrolle)$p.value # [1] 1

# aufbewahrung_kontrolle
cbind(table(par.asthma.factor, aufbewahrung3_kontrolle.factor), round(prop.table(table(par.asthma.factor, aufbewahrung3_kontrolle.factor),1),4))
chisq.test(par.asthma, data$aufbewahrung3_kontrolle)$p.value # [1] 1

# all kontrolle
cbind(table(par.asthma.factor, data$all_kontrolle.factor), round(prop.table(table(par.asthma.factor, data$all_kontrolle.factor),1),4))
chisq.test(par.asthma, data$all_kontrolle)$p.value # [1] 0.9383751

#---

# III. Risk perception vs. Wissen 

# 1. ALLERGIE_BEKOMMEN VS.
# Haare
cbind(table(allergie3_bekommen.factor, haare.factor), round(prop.table(table(allergie3_bekommen.factor, haare.factor),1),4))
chisq.test(table(allergie3_bekommen.factor, haare.factor))$p.value #NS

# Arbeitsschuhe
cbind(table(allergie3_bekommen.factor, arbeitsschuhe.factor), round(prop.table(table(allergie3_bekommen.factor, arbeitsschuhe.factor),1),4))
fisher.test(table(allergie3_bekommen.factor, arbeitsschuhe.factor))$p.value #NS

# Arbeitskleidung aufbewahren
cbind(table(allergie3_bekommen.factor, kleidung_aufb.factor), round(prop.table(table(allergie3_bekommen.factor, kleidung_aufb.factor),1),4))
fisher.test(table(allergie3_bekommen.factor, kleidung_aufb.factor))$p.value #NS

# Stall desinfizieren
cbind(table(allergie3_bekommen.factor, desinfizieren.factor), round(prop.table(table(allergie3_bekommen.factor, desinfizieren.factor),1),4))
fisher.test(table(allergie3_bekommen.factor, desinfizieren.factor))$p.value #NS

# Schutzbrille tragen
cbind(table(allergie3_bekommen.factor, schutzbrille.factor), round(prop.table(table(allergie3_bekommen.factor, schutzbrille.factor),1),4))
fisher.test(table(allergie3_bekommen.factor, schutzbrille.factor))$p.value #NS 

# Kleidung nicht im Wohnraum tragen
cbind(table(allergie3_bekommen.factor, kleidung_wohnraum.factor), round(prop.table(table(allergie3_bekommen.factor, kleidung_wohnraum.factor),1),4))
fisher.test(table(allergie3_bekommen.factor, kleidung_wohnraum.factor))$p.value #NS

# 2. ALLERGIE_SCHLIMM VS.
# Haare
cbind(table(allergie3_schlimm.factor, haare.factor), round(prop.table(table(allergie3_schlimm.factor, haare.factor),1),4))
chisq.test(table(allergie3_schlimm.factor, haare.factor))$p.value #NS

# Arbeitsschuhe
cbind(table(allergie3_schlimm.factor, arbeitsschuhe.factor), round(prop.table(table(allergie3_schlimm.factor, arbeitsschuhe.factor),1),4))
chisq.test(table(allergie3_schlimm.factor, arbeitsschuhe.factor))$p.value #NS

# Arbeitskleidung aufbewahren
cbind(table(allergie3_schlimm.factor, kleidung_aufb.factor), round(prop.table(table(allergie3_schlimm.factor, kleidung_aufb.factor),1),4))
chisq.test(table(allergie3_schlimm.factor, kleidung_aufb.factor))$p.value #NS

# Stall desinfizieren
cbind(table(allergie3_schlimm.factor, desinfizieren.factor), round(prop.table(table(allergie3_schlimm.factor, desinfizieren.factor),1),4))
chisq.test(table(allergie3_schlimm.factor, desinfizieren.factor))$p.value #NS

# Schutzbrille tragen
cbind(table(allergie3_schlimm.factor, schutzbrille.factor), round(prop.table(table(allergie3_schlimm.factor, schutzbrille.factor),1),4))
fisher.test(table(allergie3_schlimm.factor, schutzbrille.factor))$p.value #NS

# Kleidung nicht im Wohnraum tragen
cbind(table(allergie3_schlimm.factor, kleidung_wohnraum.factor), round(prop.table(table(allergie3_schlimm.factor, kleidung_wohnraum.factor),1),4))
fisher.test(table(allergie3_schlimm.factor, kleidung_wohnraum.factor))$p.value #[1] 0.02358631

#---
#---#
# IV. Risk perception vs. Behavior

# 1. ALLERGIE_BEKOMMEN VS.
# 1a. BEHAVIOR LAST WEEK
# arbeitskleidung_letzteWoche
cbind(table(allergie3_bekommen.factor, arbeitskleidung3_letzteWoche.factor), round(prop.table(table(allergie3_bekommen.factor, arbeitskleidung3_letzteWoche.factor),1),4))
chisq.test(allergie3_bekommen, arbeitskleidung3_letzteWoche)$p.value # NS

# haare_letzteWoche
cbind(table(allergie3_bekommen.factor, haare3_letzteWoche.factor), round(prop.table(table(allergie3_bekommen.factor, haare3_letzteWoche.factor),1),4))
fisher.test(allergie3_bekommen, data$haare3_letzteWoche)$p.value # [1] NS

# aufbewahrung_letzteWoche
cbind(table(allergie3_bekommen.factor, aufbewahrung3_letzteWoche.factor), round(prop.table(table(allergie3_bekommen.factor, aufbewahrung3_letzteWoche.factor),1),4))
fisher.test(allergie3_bekommen, data$aufbewahrung3_letzteWoche)$p.value # [1] NS

# all letzteWoche
cbind(table(allergie3_bekommen, data$all_letzteWoche.factor), round(prop.table(table(allergie3_bekommen, data$all_letzteWoche.factor),1),4))
chisq.test(allergie3_bekommen, data$all_letzteWoche)$p.value # [1] NS


# 1b. BEHAVIOR INTENTION IN THE NEXT SIX MONTHS
# arbeitskleidung_intention
cbind(table(allergie3_bekommen.factor, arbeitskleidung3_intention.factor), round(prop.table(table(allergie3_bekommen.factor, arbeitskleidung3_intention.factor),1),4))
fisher.test(allergie3_bekommen, data$arbeitskleidung3_intention)$p.value #[1] NS

# haare_intention
cbind(table(allergie3_bekommen.factor, haare3_intention.factor), round(prop.table(table(allergie3_bekommen.factor, haare3_intention.factor),1),4))
fisher.test(allergie3_bekommen, data$haare3_intention)$p.value #[1] NS

# aufbewahrung_intention
cbind(table(allergie3_bekommen.factor, aufbewahrung3_intention.factor), round(prop.table(table(allergie3_bekommen.factor, aufbewahrung3_intention.factor),1),4))
fisher.test(allergie3_bekommen, data$aufbewahrung3_intention)$p.value #[1] NS

# all intention
cbind(table(allergie3_bekommen, data$all_intention.factor), round(prop.table(table(allergie3_bekommen, data$all_intention.factor),1),4))
chisq.test(allergie3_bekommen, data$all_intention)$p.value # [1] NS


# 1c. ATTITUDE TOWARDS
# arbeitskleidung_einstellung
cbind(table(allergie3_bekommen.factor, arbeitskleidung3_einstellung.factor), round(prop.table(table(allergie3_bekommen.factor, arbeitskleidung3_einstellung.factor),1),4))
fisher.test(allergie3_bekommen, data$arbeitskleidung3_einstellung)$p.value #[1] 0.04768099 ******

# haare_einstellung
cbind(table(allergie3_bekommen.factor, haare3_einstellung.factor), round(prop.table(table(allergie3_bekommen.factor, haare3_einstellung.factor),1),4))
fisher.test(allergie3_bekommen, data$haare3_einstellung)$p.value # [1] 1

# aufbewahrung_einstellung
cbind(table(allergie3_bekommen.factor, aufbewahrung3_einstellung.factor), round(prop.table(table(allergie3_bekommen.factor, aufbewahrung3_einstellung.factor),1),4))
fisher.test(allergie3_bekommen, data$aufbewahrung3_einstellung)$p.value #[1] 0.5391753

# all einstellung
cbind(table(allergie3_bekommen, data$all_einstellung.factor), round(prop.table(table(allergie3_bekommen, data$all_einstellung.factor),1),4))
fisher.test(allergie3_bekommen, data$all_einstellung)$p.value # [1] NS


# 1d. PEER BEHAVIOR
# arbeitskleidung_norm
cbind(table(allergie3_bekommen.factor, arbeitskleidung3_norm.factor), round(prop.table(table(allergie3_bekommen.factor, arbeitskleidung3_norm.factor),1),4))
chisq.test(allergie3_bekommen, data$arbeitskleidung3_norm)$p.value #[1] NS

# haare_norm
cbind(table(allergie3_bekommen.factor, haare3_norm.factor), round(prop.table(table(allergie3_bekommen.factor, haare3_norm.factor),1),4))
fisher.test(allergie3_bekommen, data$haare3_norm)$p.value #[1] NS

# aufbewahrung_norm
cbind(table(allergie3_bekommen.factor, aufbewahrung3_norm.factor), round(prop.table(table(allergie3_bekommen.factor, aufbewahrung3_norm.factor),1),4))
fisher.test(allergie3_bekommen, data$aufbewahrung3_norm)$p.value #[1] NS

# all norm
cbind(table(allergie3_bekommen, data$all_norm.factor), round(prop.table(table(allergie3_bekommen, data$all_norm.factor),1),4))
chisq.test(allergie3_bekommen, data$all_norm)$p.value # [1] NS


# 1e. PERCEIVED BEHAVIORAL CONTROL
# arbeitskleidung_kontrolle
cbind(table(allergie3_bekommen.factor, arbeitskleidung3_kontrolle.factor), round(prop.table(table(allergie3_bekommen.factor, arbeitskleidung3_kontrolle.factor),1),4))
chisq.test(allergie3_bekommen, data$arbeitskleidung3_kontrolle)$p.value #[1] NS

# haare_kontrolle
cbind(table(allergie3_bekommen.factor, haare3_kontrolle.factor), round(prop.table(table(allergie3_bekommen.factor, haare3_kontrolle.factor),1),4))
fisher.test(allergie3_bekommen, data$haare3_kontrolle)$p.value #[1] NS

# aufbewahrung_kontrolle
cbind(table(allergie3_bekommen.factor, aufbewahrung3_kontrolle.factor), round(prop.table(table(allergie3_bekommen.factor, aufbewahrung3_kontrolle.factor),1),4))
fisher.test(allergie3_bekommen, data$aufbewahrung3_kontrolle)$p.value #[1] NS

# all kontrolle
cbind(table(allergie3_bekommen, data$all_kontrolle.factor), round(prop.table(table(allergie3_bekommen, data$all_kontrolle.factor),1),4))
chisq.test(allergie3_bekommen, data$all_kontrolle)$p.value # [1] NS

#---

# 2. ALLERGIE_SCHLIMM VS.
# 2a. BEHAVIOR LAST WEEK
# arbeitskleidung_letzteWoche
cbind(table(allergie3_schlimm.factor, arbeitskleidung3_letzteWoche.factor), round(prop.table(table(allergie3_schlimm.factor, arbeitskleidung3_letzteWoche.factor),1),4))
chisq.test(allergie3_schlimm, arbeitskleidung3_letzteWoche)$p.value # [1] NS

# haare_letzteWoche
cbind(table(allergie3_schlimm.factor, haare3_letzteWoche.factor), round(prop.table(table(allergie3_schlimm.factor, haare3_letzteWoche.factor),1),4))
fisher.test(allergie3_schlimm, data$haare3_letzteWoche)$p.value # [1] NS

# aufbewahrung_letzteWoche
cbind(table(allergie3_schlimm.factor, aufbewahrung3_letzteWoche.factor), round(prop.table(table(allergie3_schlimm.factor, aufbewahrung3_letzteWoche.factor),1),4))
chisq.test(allergie3_schlimm, data$aufbewahrung3_letzteWoche)$p.value #[1] NS

# all letzteWoche
cbind(table(allergie3_schlimm, data$all_letzteWoche.factor), round(prop.table(table(allergie3_schlimm, data$all_letzteWoche.factor),1),4))
chisq.test(allergie3_schlimm, data$all_letzteWoche)$p.value # [1] NS


# 2b. BEHAVIOR INTENTION IN THE NEXT SIX MONTHS
# arbeitskleidung_intention
cbind(table(allergie3_schlimm.factor, arbeitskleidung3_intention.factor), round(prop.table(table(allergie3_schlimm.factor, arbeitskleidung3_intention.factor),1),4))
fisher.test(allergie3_schlimm, data$arbeitskleidung3_intention)$p.value #[1] NS

# haare_intention
cbind(table(allergie3_schlimm.factor, haare3_intention.factor), round(prop.table(table(allergie3_schlimm.factor, haare3_intention.factor),1),4))
fisher.test(allergie3_schlimm, data$haare3_intention)$p.value #[1] NS

# aufbewahrung_intention
cbind(table(allergie3_schlimm.factor, aufbewahrung3_intention.factor), round(prop.table(table(allergie3_schlimm.factor, aufbewahrung3_intention.factor),1),4))
chisq.test(allergie3_schlimm, data$aufbewahrung3_intention)$p.value #[1] NS

# all intention
cbind(table(allergie3_schlimm, data$all_intention.factor), round(prop.table(table(allergie3_schlimm, data$all_intention.factor),1),4))
chisq.test(allergie3_schlimm, data$all_intention)$p.value # [1] NS


# 2c. ATTITUDE TOWARDS
# arbeitskleidung_einstellung
cbind(table(allergie3_schlimm.factor, arbeitskleidung3_einstellung.factor), round(prop.table(table(allergie3_schlimm.factor, arbeitskleidung3_einstellung.factor),1),4))
chisq.test(allergie3_schlimm, data$arbeitskleidung3_einstellung)$p.value #[1] NS

# haare_einstellung
cbind(table(allergie3_schlimm.factor, haare3_einstellung.factor), round(prop.table(table(allergie3_schlimm.factor, haare3_einstellung.factor),1),4))
fisher.test(allergie3_schlimm, data$haare3_einstellung)$p.value #[1] NS

# aufbewahrung_einstellung
cbind(table(allergie3_schlimm.factor, aufbewahrung3_einstellung.factor), round(prop.table(table(allergie3_schlimm.factor, aufbewahrung3_einstellung.factor),1),4))
fisher.test(allergie3_schlimm, data$aufbewahrung3_einstellung)$p.value #[1] NS

# all einstellung
cbind(table(allergie3_schlimm, data$all_einstellung.factor), round(prop.table(table(allergie3_schlimm, data$all_einstellung.factor),1),4))
fisher.test(allergie3_schlimm, data$all_einstellung)$p.value # [1] NS

# 2d. PEER BEHAVIOR
# arbeitskleidung_norm
cbind(table(allergie3_schlimm.factor, arbeitskleidung3_norm.factor), round(prop.table(table(allergie3_schlimm.factor, arbeitskleidung3_norm.factor),1),4))
chisq.test(allergie3_schlimm, data$arbeitskleidung3_norm)$p.value #[1] NS

# haare_norm
cbind(table(allergie3_schlimm.factor, haare3_norm.factor), round(prop.table(table(allergie3_schlimm.factor, haare3_norm.factor),1),4))
chisq.test(allergie3_schlimm, data$haare3_norm)$p.value #[1] NS

# aufbewahrung_norm
cbind(table(allergie3_schlimm.factor, aufbewahrung3_norm.factor), round(prop.table(table(allergie3_schlimm.factor, aufbewahrung3_norm.factor),1),4))
chisq.test(allergie3_schlimm, data$aufbewahrung3_norm)$p.value #[1] NS

# all norm
cbind(table(allergie3_schlimm, data$all_norm.factor), round(prop.table(table(allergie3_schlimm, data$all_norm.factor),1),4))
chisq.test(allergie3_schlimm, data$all_norm)$p.value # [1] NS

# 2e. PERCEIVED BEHAVIORAL CONTROL
# arbeitskleidung_kontrolle
cbind(table(allergie3_schlimm.factor, arbeitskleidung3_kontrolle.factor), round(prop.table(table(allergie3_schlimm.factor, arbeitskleidung3_kontrolle.factor),1),4))
chisq.test(allergie3_schlimm, data$arbeitskleidung3_kontrolle)$p.value #[1] NS

# haare_kontrolle
cbind(table(allergie3_schlimm.factor, haare3_kontrolle.factor), round(prop.table(table(allergie3_schlimm.factor, haare3_kontrolle.factor),1),4))
fisher.test(allergie3_schlimm, data$haare_kontrolle)$p.value #[1] NS

# aufbewahrung_kontrolle
cbind(table(allergie3_schlimm.factor, aufbewahrung3_kontrolle.factor), round(prop.table(table(allergie3_schlimm.factor, aufbewahrung3_kontrolle.factor),1),4))
chisq.test(allergie3_schlimm, data$aufbewahrung3_kontrolle)$p.value #[1] NS

# all kontrolle
cbind(table(allergie3_schlimm, data$all_kontrolle.factor), round(prop.table(table(allergie3_schlimm, data$all_kontrolle.factor),1),4))
chisq.test(allergie3_schlimm, data$all_kontrolle)$p.value # [1] NS

#---

#---------------#
# END OF SCRIPT #
#---------------#