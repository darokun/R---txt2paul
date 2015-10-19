#' ---
#' title: "txt2paul Multiple Imputation"
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
d <- "/Users/daro/Desktop/txt2PAUL/z_Dokumente f??r Daloha/txt2PAUL_Gesamt_vers1_130115.csv"
data <- read.csv(d, header=T)
attach(data)

# packages
pckgs <- c("Amelia","Zelig","tcltk")
require(pckgs)
# install.packages("Amelia")
# install.packages("Zelig")
# install.packages("tcltk")
# library(Amelia)
# library(Zelig)
# library(tcltk)

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

# Missingness maps:
# missmap(data) # nothing really readable
missmap(data1) 
  # 1-119 of gruppe
missmap(data2) 
  # ALL Ausfallgrund
missmap(data3)
missmap(data4)
missmap(data5)
  # - SEVERAL in andere_welche, all *_betrieb_zahl except kuh_betrieb_zahl,   		andere_betrieb_welche.
  # - MODERATE andere, ziegen, schaf, pferde, schwein, hase, kuh_betrieb_zahl,     huhn, hund
  # - LESS rest
missmap(data6) 
  # SEVERAL Ackergras
missmap(data7)
missmap(data8) 
  # ALL control (duh), several non-responders from intervention (NAs in all   	variables).
missmap(data9) 
  # same as with data8
missmap(data10) 
  # same as w data8 and 9
missmap(data11) 
  # same as w data8,9,10 except sonstiges (complete).
missmap(data12) 
  # Almost everything missing
missmap(data13) 
  # Almost everything missing except befdatum_MF1**
missmap(data14) 
  # Almost everything missing except befdatum_MF2**
missmap(data15) 
  # Almost everything missing except befdatum_MF3**

# ** confirm that data13 14 and 15 will not be used because only basis und nachherbefragung will be taken into account.

#---------------#
# END OF SCRIPT #
#---------------#