#' ---
#' title: "txt2paul Baseline Analyses on 238 subjects"
#' author: "Daloha Rodriguez-Molina"
#' date: "March 25th, 2015"
#' ---

#-------------------#
# Main data used: txt2PAUL_Gesamt_vers1_130115.csv
# Name given to this main dataset: data
#
# Subset dataset mostly used on this script: data1
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
d <- "/Users/daro/Desktop/txt2PAUL/z_Dokumente fuer Daloha/txt2PAUL_Gesamt_vers1_130115.csv"
data <- read.csv(d, header=T)

# explore data and subset demographics
# head(data[,1:4])
data1 <- data[,c(1:6,14:21)] # data1 = soziodemographie
# names(data1)

#---------------------#
# Convert to factors: #
#---------------------#
sex.labels <- c("male","female")
data$sex.factor <- factor(data$sex, labels = sex.labels)
studienort.labels <- c("Miesbach","Rosenheim", "Muehldorf", "Traunstein", "Schoengeising", "Andechs", "Erding", "Bad Heilbrunn", "Ebersberg", "Ingolstadt")
data$studienort.factor <- factor(data$studienort, labels = studienort.labels)
alterkat.labels <- c("18-20", "21-24", "25-44")
alterkat.factor <- factor(data$alterkat,levels=c("1","2","3"),labels=alterkat.labels)
no.yes.labels <- c("No", "Yes")

# Schulabschluss
schulabschluss.factor <- NULL
schulabschluss.factor[data$schulabschluss==0] <- 0
schulabschluss.factor[data$schulabschluss==1] <- 1
schulabschluss.factor[data$schulabschluss==2 | data$schulabschluss==3] <- 2
schulabschluss.factor[data$schulabschluss==4] <- 3
schulabschluss.labels <- c("Hauptschulabschluss", "Realschulabschluss", "Fachhochschulreife + Abitur", "anderer Abschluss")
data$schulabschluss.factor <- factor(schulabschluss.factor, labels=schulabschluss.labels, levels = c(0,1,2,3))

# smoking.status
smoking.status <- NULL 
smoking.status[data$rauchen==0 | data$rauchen_aktuell==0] <- 0
smoking.status[data$rauchen==1 | data$rauchen_aktuell==1] <- 1
smoking.status.labels <- c("Non-smoker", "Smoker")
smoking.status.factor <- factor(smoking.status, labels = smoking.status.labels, levels=c(0,1))


#----------------------------#
# Sociodemographic analysis: #
#----------------------------#
nrow(data) # n

# Sex
sex.demog <- c("absolute", "percentage")
rbind(sex.demog,cbind(table(sex.factor),round(prop.table(table(sex.factor)),4))) # sex
sum(is.na(sex.factor))

# Age
age.demog.long <- c("mean", "sd")
rbind(age.demog.long,cbind(round(mean(data$alter, na.rm=T),2),round(sd(data$alter, na.rm=T),2))) # age longitudinal
sum(is.na(alterkat.factor))

age.demog.cat <- c("absolute", "percentage")
rbind(age.demog.cat,cbind(table(alterkat.factor),round(prop.table(table(alterkat.factor)),4))) # age categorical
sum(is.na(alterkat.factor))

# Plots to look at age distribution: (optional) 
# hist(alter, breaks = 37)
# boxplot(alter)
# plot(alter)
# abline(h=21)
# abline(h=25)
# plot(alterkat.factor)

# Studienort
studienort.demog <- c("absolute", "percentage")
rbind(studienort.demog,cbind(table(studienort.factor),round(prop.table(table(studienort.factor)),4))) # studienort
sum(is.na(studienort.factor))

# Schulabschluss
schulabschluss.demog <- c("absolute", "percentage")
rbind(schulabschluss.demog,cbind(table(data$schulabschluss.factor),round(prop.table(table(data$schulabschluss.factor)),4))) # schulabschluss
# sum(is.na(data$schulabschluss.factor))

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

# Smoking status
smoker.demog <- c("absolute", "percentage")
rbind(smoker.demog,cbind(table(smoking.status.factor),round(prop.table(table(smoking.status.factor)),4))) # smoker
sum(is.na(smoking.status.factor))

# WISSEN
# all wissen 6 right

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

# construct all wissen
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
cbind(table(data$all_wissen.factor), round(prop.table(table(data$all_wissen.factor))*100,2))
# note from the ?glm
# For binomial and quasibinomial families the response can also be specified as a factor (when the first level denotes failure and all others success)

# all wissen 0-5 correct
data$all_wissen5 <- NULL
data$all_wissen4 <- NULL
data$all_wissen3 <- NULL
data$all_wissen2 <- NULL
data$all_wissen1 <- NULL
data$all_wissen0 <- NULL

data$check <- apply(data[,325:330],1,sum)
count.only6 <- sum(data$check==6) # all 6 correct
count.only5 <- sum(data$check==5) # just 5 correct
count.only4 <- sum(data$check==4) # just 4 correct
count.only3 <- sum(data$check==3) # just 3 correct
count.only2 <- sum(data$check==2) # just 2 correct
count.only1 <- sum(data$check==1) # just 1 correct
count.zero <- sum(data$check==0) # none correct

# 5
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
cbind(table(data$all_wissen5.factor), round(prop.table(table(data$all_wissen5.factor)),4))

# 4
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
cbind(table(data$all_wissen4.factor), round(prop.table(table(data$all_wissen4.factor)),4))

# 3
for (i in 1:length(data$check)) {
  if(data$check[i]>=3) {
    data$all_wissen3[i] <- 1
  } else {
    data$all_wissen3[i] <- 0
  }
}
data$all_wissen3.factor <- factor(data$all_wissen3, labels = all_wissen.labels)
# data$all_wissen==1: at least 3 positive measures
# data$all_wissen==0: not at least 3 positive measures
cbind(table(data$all_wissen3.factor), round(prop.table(table(data$all_wissen3.factor)),4))

# 2
for (i in 1:length(data$check)) {
  if(data$check[i]>=2) {
    data$all_wissen2[i] <- 1
  } else {
    data$all_wissen2[i] <- 0
  }
}
data$all_wissen2.factor <- factor(data$all_wissen2, labels = all_wissen.labels)
# data$all_wissen==1: at least 2 positive measures
# data$all_wissen==0: not at least 2 positive measures
cbind(table(data$all_wissen2.factor), round(prop.table(table(data$all_wissen2.factor)),4))

# 1
for (i in 1:length(data$check)) {
  if(data$check[i]>=1) {
    data$all_wissen1[i] <- 1
  } else {
    data$all_wissen1[i] <- 0
  }
}
data$all_wissen1.factor <- factor(data$all_wissen1, labels = "correct")
# data$all_wissen==1: at least 1 positive measures
# data$all_wissen==0: not at least 1 positive measures
cbind(table(data$all_wissen1.factor), round(prop.table(table(data$all_wissen1.factor)),4))

# 0
for (i in 1:length(data$check)) {
  if(data$check[i]>=0) {
    data$all_wissen0[i] <- 1
  } else {
    data$all_wissen0[i] <- 0
  }
}
data$all_wissen0.factor <- factor(data$all_wissen0, labels = "correct")
# data$all_wissen==1: at least 0 positive measures
# data$all_wissen==0: not at least 0 positive measures
cbind(table(data$all_wissen0.factor), round(prop.table(table(data$all_wissen0.factor)),4))


# PLOTS
# spdf("myplot.pdf")
par(mfrow=c(1,1))
barplot(c(count.zero, count.only1, count.only2, count.only3, count.only4, count.only5, count.only6),
        col = "#444444", 
        xlab = "Number of correct preventive measures",
        main = "Frequency of correct preventive measures (n = 238)",
        ylim = range(-10,105))
text(0.7, -5, "0")
text(1.85, -5, "1")
text(3, -5, "2")
text(4.3, -5, "3")
text(5.5, -5, "4")
text(6.7, -5, "5")
text(7.9, -5, "6")

text(0.7, 5, sum(data$check==0)) 
text(1.85, 6, sum(data$check==1)) 
text(3, 13, sum(data$check==2)) 
text(4.3, 28, sum(data$check==3)) 
text(5.5, 65, sum(data$check==4))
text(6.7, 98, sum(data$check==5)) 
text(7.9, 58, sum(data$check==6))


count.atleast6 <- sum(data$check==6)
count.atleast5 <- sum(data$check==6) + sum(data$check==5)
count.atleast4 <- sum(data$check==6) + sum(data$check==5) + sum(data$check==4)
count.atleast3 <- sum(data$check==6) + sum(data$check==5) + 
                  sum(data$check==4) + sum(data$check==3)
count.atleast2 <- sum(data$check==6) + sum(data$check==5) + 
                  sum(data$check==4) + sum(data$check==3) + sum(data$check==2)
count.atleast1 <- sum(data$check==6) + sum(data$check==5) + 
                  sum(data$check==4) + sum(data$check==3) + 
                  sum(data$check==2) + sum(data$check==1)
count.atleast0 <- sum(data$check==6) + sum(data$check==5) + 
                  sum(data$check==4) + sum(data$check==3) + 
                  sum(data$check==2) + sum(data$check==1) + sum(data$check==0)

barplot(c(count.atleast0, count.atleast1, count.atleast2, count.atleast3, 
          count.atleast4, count.atleast5, count.atleast6),
        col = "#444444",
        ylim = range(-20,260),
        xlab = "Number of correct preventive measures",
        ylab = "Frequency",
        main = "Frequency of correct preventive measures (n = 238)")
text(0.7, -10, "???0")
text(1.85, -10, "???1")
text(3.1, -10, "???2")
text(4.3, -10, "???3")
text(5.5, -10, "???4")
text(6.7, -10, "???5")
text(7.9, -10, "6")

text(0.7, 248, count.atleast0) 
text(1.85, 248, count.atleast1) 
text(3.1, 247, count.atleast2) 
text(4.3, 239, count.atleast3) 
text(5.5, 216, count.atleast4)
text(6.7, 156, count.atleast5) 
text(7.9, 63, count.atleast6)
# dev.off()

# RISK PERCEPTION
# join allergie_bekommen and allergie_schlimm into just one variable: risk_perception,
# according to Katja's email from Oct 20th 2015
???
# intructions: "You could sum up the two scores (taking the direction into account) and than use the 75th percentile as cut-off in order to define those with a high level of risk perception"

# 1. create data$allergie_bekommen_reverse, reversing the score into the correct direction
# The correct direction should be: 1 (likely) = high risk perception level, 5 (unlikely) = low risk perception level

data$allergie_bekommen_reverse <- NULL
table(data$allergie_bekommen)
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
table(data$allergie_bekommen_reverse)

# 2. sum up data$allergie_bekommen_reverse + data$allergie_schlimm into a new vector (rp)
rp <- NULL
rp <- data$allergie_bekommen_reverse + data$allergie_schlimm

# 3. Determine the 75th percentile for rp
quantile(rp, 0.75) # 7

# 4. Define those at a high level of risk perception (>=7), and add to new variable data$risk_perception, where 0=ref level=high risk perception level and 1=low risk perception level.
# The ideal is that people have a higher risk perception level.
data$risk_perception <- NULL

for(i in 1:length(rp)) {
  if(rp[i]>=7) {
    data$risk_perception[i] <- 0
  }
  else {
    data$risk_perception[i] <- 1
  }
}

# 5. convert data$risk_perception into factor, and add labels
data$risk_perception.factor <- factor(data$risk_perception, labels=c("high risk perception level", "low risk perception level"))


#---------------#
# END OF SCRIPT #
#---------------#

