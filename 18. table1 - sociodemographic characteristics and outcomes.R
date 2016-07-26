#-------
# Table 1 - Sociodemographic characteristics and outcomes
#-------

# outcomes:
# all 6:      int.data.wo.nas$all_wissen_nachher
# at least 5: int.data.wo.nas$all_wissen_nachher5
# at least 4: int.data.wo.nas$all_wissen_nachher4


# W: (covariates)
# age:                            int.data.wo.nas$alterkat2.binary        0 = younger
# sex:                            int.data.wo.nas$sex.factor              0 = male
# smoking status:                 int.data.wo.nas$smoking.status          0 = non-smoker
# educational level:              int.data.wo.nas$schulabschluss.binary   0 = Hauptschulabschluss
# risk perception:                int.data.wo.nas$risk_perception         0 = low risk perception
# asthma or rhinoconjunctivitis:  int.data.wo.nas$asthma.or.rhinoconj     0 = no
# parental asthma:                int.data.wo.nas$par.asthma              0 = no
# knowledge at baseline:          int.data.wo.nas$all_wissen              0 = no

# A:
# treatment                       int.data.wo.nas$gruppe                  0 = control

table(int.data.wo.nas$all_wissen_nachher)
table(int.data.wo.nas$alterkat2.binary)
table(int.data.wo.nas$completed.table2)

cbind(int.data.wo.nas$all_wissen_nachher,data.wo.nas$completed[115:230])

# TOTAL
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
# Less than 4: (data$all_wissen3)
# int.data.wo.nas$all_wissen_nachher3

# age
cbind(table(int.data.wo.nas$alterkat2.binary, int.data.wo.nas$all_wissen_nachher3), 
      prop.table(table(int.data.wo.nas$alterkat2.binary, int.data.wo.nas$all_wissen_nachher3),1))

# sex
cbind(table(int.data.wo.nas$sex.factor, int.data.wo.nas$all_wissen_nachher3), 
      prop.table(table(int.data.wo.nas$sex.factor, int.data.wo.nas$all_wissen_nachher3),1))

# smoking status
cbind(table(int.data.wo.nas$smoking.status, int.data.wo.nas$all_wissen_nachher3), 
      prop.table(table(int.data.wo.nas$smoking.status, int.data.wo.nas$all_wissen_nachher3),1))

# educational level
cbind(table(int.data.wo.nas$schulabschluss.binary, int.data.wo.nas$all_wissen_nachher3), 
      prop.table(table(int.data.wo.nas$schulabschluss.binary, int.data.wo.nas$all_wissen_nachher3),1))

# risk perception
cbind(table(int.data.wo.nas$risk_perception, int.data.wo.nas$all_wissen_nachher3), 
      prop.table(table(int.data.wo.nas$risk_perception, int.data.wo.nas$all_wissen_nachher3),1))

# asthma or rhinoconjunctivitis
cbind(table(int.data.wo.nas$asthma.or.rhinoconj, int.data.wo.nas$all_wissen_nachher3), 
      prop.table(table(int.data.wo.nas$asthma.or.rhinoconj, int.data.wo.nas$all_wissen_nachher3),1))

# parental asthma
cbind(table(int.data.wo.nas$par.asthma, int.data.wo.nas$all_wissen_nachher3), 
      prop.table(table(int.data.wo.nas$par.asthma, int.data.wo.nas$all_wissen_nachher3),1))

# knowledge at baseline
cbind(table(int.data.wo.nas$all_wissen3, int.data.wo.nas$all_wissen_nachher3), 
      prop.table(table(int.data.wo.nas$all_wissen3, int.data.wo.nas$all_wissen_nachher3),1))

# treatment
cbind(table(int.data.wo.nas$gruppe, int.data.wo.nas$all_wissen_nachher3), 
      prop.table(table(int.data.wo.nas$gruppe, int.data.wo.nas$all_wissen_nachher3),1))

#---------------#
# END OF SCRIPT #
#---------------#