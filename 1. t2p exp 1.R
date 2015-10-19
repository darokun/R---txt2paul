#' ---
#' title: "txt2paul Preliminary exploration"
#' author: "Daloha Rodriguez-Molina"
#' date: "March 23rd, 2015"
#' ---


#########################
### Preliminary stuff ###
#########################

# When working from home
setwd("~/Dropbox/R - txt2paul")

# When working at KR5
# setwd("/usr281/ben/msc1426/Desktop/dataproject_datasets-1")

# Read the data
d <- "/Users/daro/Desktop/txt2PAUL/z_Dokumente fuer Daloha/txt2PAUL_Gesamt_vers1_130115.csv"
data <- read.csv(d, header=T)


### Stuff
table(data$alterkat)
table(data$sex)
table(data$schulabschluss)
table(data$rauchen)
table(data$rauchen,data$sex)
table(data$sex,data$schulabschluss)
table(data$rauchen,data$rauchen_aktuell)
table(data$sex,data$pfeifen)
sum(is.na(data$pfeifen))
table(data$sex,data$asthma)
sum(is.na(data$asthma))
table(data$studienort) # would it be worth it to run random effects models?


# different datasets:
# data1 = soziodemographie
# data2 = ruecklauf
# data3 = ATEMWEGSBESCHWERDEN (Basisbefragung)
# data4 = WISSEN (Basisbefragung)
# data5 = KONTAKT MIT TIEREN (Basisbefragung)
# data6 = KONTAKT MIT PFLANZEN (Basisbefragung)
# data7 = ARBEITSALLTAG (Basisbefragung)
# data8 = WISSEN (NACHHERBEFRAGUNG)
# data9 = ARBEITSALLTAG (Nachherbefragung)
# data10 = MEDIENNUTZUNG (Nachherbefragung)
# data11 = SUBJEKITVE BEDEUTUNG VON GESUNDHEIT (Nachherbefragung)
# data12 = INTERVENTION (Nachherbefragung)
# data13 = MONATLICHER FRAGEBOGEN 1
# data14 = MONATLICHER FRAGEBOGEN 2
# data15 = MONATLICHER FRAGEBOGEN 3

names(data)
data1 <- data[,c(1:6,14:21)]
data2 <- data[,7:13]
data3 <- data[,22:30]
data4 <- data[,31:36]
data5 <- data[,37:66]
data6 <- data[,67:142]
data7 <- data[,143:168]
data8 <- data[,169:174]
data9 <- data[,175:198] # allergie_bekommen_nachher + allergie_schlimm_nachher sind 167 + 168 im Codebuch
data10 <- data[,201:213]
data11 <- data[,214:226]
data12 <- data[,227:244]
data13 <- data[,245:269]
data14 <- data[,270:294]
data15 <- data[,295:320]

# Zusammensetzung der Studienpopulation: (Abbildung 2 in Abschlussbericht document)
nrow(data) # total n = 238
sum(data$Ausgeschlossen==1) # 119
sum(data$Ausgeschlossen==0) # 119
sum(data$Absage==1) # 2
sum(data$Ausgeschlossen==0) - sum(data$Absage==1) # 117 (eingeschlossen)

#---------------#
# END OF SCRIPT #
#---------------#