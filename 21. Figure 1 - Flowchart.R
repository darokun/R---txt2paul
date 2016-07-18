# Flowchart:

# by hand
314 # eligible
  74 # did not participate
    37/314 # refused
    37/314 # underaged
  240 # enrolled
    124/240 # excluded
      29/240 # no email address
      72/240 # no mobile phone number
      19/240 # no mobile nor email
      3/240 # empty questionnaires
      1/240 # NA covariates
  116/240 # included in the intervention phase
    46/116 # control group
      16/116 # control group drop-outs
      30/116 # final control group
    70/116 # intervention group
      53/116 # intervention group drop-outs
      17/116 # final intervention group

47/116 # final study population


  
  
240-29-72-19-3


# no me dan los n??meros:
12.18+30.25+7.98+1.26
29+72+19+3


# Read the data
d <- "/Users/daro/Desktop/txt2PAUL/z_Dokumente fuer Daloha/txt2PAUL_Gesamt_vers1_130115.csv"
data <- read.csv(d, header=T)

names(data)
table(data$alter)
nrow(data)

# interesting variables:
# data$alter, data$Zusage, data$Absage, data$Ausfall, data$Ausgeschlossen, data$Ausfallgrund, data$Ausschlussgrund, data$Absagegrund

cbind(data$Zusage, data$Absage, data$Absagegrund, data$Ausfall, data$Ausfallgrund, data$Ausgeschlossen)

table(data$Zusage)
which(data$Zusage==0) # 195, 223

table(data$Absage)
which(data$Absage==1) # 195, 223

table(data$Absagegrund)
which(data$Absagegrund=="am 30.05. per Mail abgesagt, m\xf6chte SMS nicht mehr erhalten, diese sind wenig hilfreich und nur l\xe4stig.") # 195
which(data$Absagegrund=="hat sich in SoSciSurvey von dem Emailverteiler abgemeldet w\xe4hrend der Feldphase") # 223

table(data$Ausfall) # none

table(data$Ausfallgrund) # none?: table of extent 0

table(data$Ausgeschlossen) # 0==119, 1==119

table(data$email)
head(data$email)
data$email

table(data$handy)
data$handy

cbind(data$email, data$handy, data$gruppe)
29+72+19
123+119

117+37+51+125+37+12+2+29+72+19+3

3+19+72+29

314-240 == 37+37
29+72+19+3
table(data$gruppe)
47+72
47+70
