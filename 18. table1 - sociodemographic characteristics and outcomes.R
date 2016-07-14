#-------
# Table 1 - Sociodemographic characteristics and outcomes
#-------

# outcomes:
# all 6:      int.data.wo.nas$all_wissen_nachher
# at least 5: int.data.wo.nas$all_wissen_nachher5
# at least 4: int.data.wo.nas$all_wissen_nachher4


# W: (covariates)
# age:                            int.data.wo.nas$alterkat2.binary        0 = younger
# sex:                            int.data.wo.nas$sex                     0 = male
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




# ALL 6:
# age
cbind(table(int.data.wo.nas$alterkat2.binary, int.data.wo.nas$all_wissen_nachher), 
      prop.table(table(int.data.wo.nas$alterkat2.binary, int.data.wo.nas$all_wissen_nachher),1))

# sex
cbind(table(int.data.wo.nas$sex.factor, int.data.wo.nas$all_wissen_nachher), 
      prop.table(table(int.data.wo.nas$sex.factor, int.data.wo.nas$all_wissen_nachher),1))

# smoking status
cbind(table(int.data.wo.nas$smoking.status, int.data.wo.nas$all_wissen_nachher), 
      prop.table(table(int.data.wo.nas$smoking.status, int.data.wo.nas$all_wissen_nachher),1))

# educational level
cbind(table(int.data.wo.nas$schulabschluss.binary, int.data.wo.nas$all_wissen_nachher), 
      prop.table(table(int.data.wo.nas$schulabschluss.binary, int.data.wo.nas$all_wissen_nachher),1))

# risk perception
cbind(table(int.data.wo.nas$risk_perception, int.data.wo.nas$all_wissen_nachher), 
      prop.table(table(int.data.wo.nas$risk_perception, int.data.wo.nas$all_wissen_nachher),1))

# asthma or rhinoconjunctivitis
cbind(table(int.data.wo.nas$asthma.or.rhinoconj, int.data.wo.nas$all_wissen_nachher), 
      prop.table(table(int.data.wo.nas$asthma.or.rhinoconj, int.data.wo.nas$all_wissen_nachher),1))

# parental asthma
cbind(table(int.data.wo.nas$par.asthma, int.data.wo.nas$all_wissen_nachher), 
      prop.table(table(int.data.wo.nas$par.asthma, int.data.wo.nas$all_wissen_nachher),1))

# knowledge at baseline
cbind(table(int.data.wo.nas$all_wissen, int.data.wo.nas$all_wissen_nachher), 
      prop.table(table(int.data.wo.nas$all_wissen, int.data.wo.nas$all_wissen_nachher),1))

# treatment
cbind(table(int.data.wo.nas$gruppe, int.data.wo.nas$all_wissen_nachher), 
      prop.table(table(int.data.wo.nas$gruppe, int.data.wo.nas$all_wissen_nachher),1))

#---
# At least 5:
# age
cbind(table(int.data.wo.nas$alterkat2.binary, int.data.wo.nas$all_wissen_nachher5), 
      prop.table(table(int.data.wo.nas$alterkat2.binary, int.data.wo.nas$all_wissen_nachher5),1))

# sex
cbind(table(int.data.wo.nas$sex.factor, int.data.wo.nas$all_wissen_nachher5), 
      prop.table(table(int.data.wo.nas$sex.factor, int.data.wo.nas$all_wissen_nachher5),1))

# smoking status
cbind(table(int.data.wo.nas$smoking.status, int.data.wo.nas$all_wissen_nachher5), 
      prop.table(table(int.data.wo.nas$smoking.status, int.data.wo.nas$all_wissen_nachher5),1))

# educational level
cbind(table(int.data.wo.nas$schulabschluss.binary, int.data.wo.nas$all_wissen_nachher5), 
      prop.table(table(int.data.wo.nas$schulabschluss.binary, int.data.wo.nas$all_wissen_nachher5),1))

# risk perception
cbind(table(int.data.wo.nas$risk_perception, int.data.wo.nas$all_wissen_nachher5), 
      prop.table(table(int.data.wo.nas$risk_perception, int.data.wo.nas$all_wissen_nachher5),1))

# asthma or rhinoconjunctivitis
cbind(table(int.data.wo.nas$asthma.or.rhinoconj, int.data.wo.nas$all_wissen_nachher5), 
      prop.table(table(int.data.wo.nas$asthma.or.rhinoconj, int.data.wo.nas$all_wissen_nachher5),1))

# parental asthma
cbind(table(int.data.wo.nas$par.asthma, int.data.wo.nas$all_wissen_nachher5), 
      prop.table(table(int.data.wo.nas$par.asthma, int.data.wo.nas$all_wissen_nachher5),1))

# knowledge at baseline
cbind(table(int.data.wo.nas$all_wissen5, int.data.wo.nas$all_wissen_nachher5), 
      prop.table(table(int.data.wo.nas$all_wissen5, int.data.wo.nas$all_wissen_nachher5),1))

# treatment
cbind(table(int.data.wo.nas$gruppe, int.data.wo.nas$all_wissen_nachher5), 
      prop.table(table(int.data.wo.nas$gruppe, int.data.wo.nas$all_wissen_nachher5),1))

#---
# At least 4:
# age
cbind(table(int.data.wo.nas$alterkat2.binary, int.data.wo.nas$all_wissen_nachher4), 
      prop.table(table(int.data.wo.nas$alterkat2.binary, int.data.wo.nas$all_wissen_nachher4),1))

# sex
cbind(table(int.data.wo.nas$sex.factor, int.data.wo.nas$all_wissen_nachher4), 
      prop.table(table(int.data.wo.nas$sex.factor, int.data.wo.nas$all_wissen_nachher4),1))

# smoking status
cbind(table(int.data.wo.nas$smoking.status, int.data.wo.nas$all_wissen_nachher4), 
      prop.table(table(int.data.wo.nas$smoking.status, int.data.wo.nas$all_wissen_nachher4),1))

# educational level
cbind(table(int.data.wo.nas$schulabschluss.binary, int.data.wo.nas$all_wissen_nachher4), 
      prop.table(table(int.data.wo.nas$schulabschluss.binary, int.data.wo.nas$all_wissen_nachher4),1))

# risk perception
cbind(table(int.data.wo.nas$risk_perception, int.data.wo.nas$all_wissen_nachher4), 
      prop.table(table(int.data.wo.nas$risk_perception, int.data.wo.nas$all_wissen_nachher4),1))

# asthma or rhinoconjunctivitis
cbind(table(int.data.wo.nas$asthma.or.rhinoconj, int.data.wo.nas$all_wissen_nachher4), 
      prop.table(table(int.data.wo.nas$asthma.or.rhinoconj, int.data.wo.nas$all_wissen_nachher4),1))

# parental asthma
cbind(table(int.data.wo.nas$par.asthma, int.data.wo.nas$all_wissen_nachher4), 
      prop.table(table(int.data.wo.nas$par.asthma, int.data.wo.nas$all_wissen_nachher4),1))

# knowledge at baseline
cbind(table(int.data.wo.nas$all_wissen4, int.data.wo.nas$all_wissen_nachher4), 
      prop.table(table(int.data.wo.nas$all_wissen4, int.data.wo.nas$all_wissen_nachher4),1))

# treatment
cbind(table(int.data.wo.nas$gruppe, int.data.wo.nas$all_wissen_nachher4), 
      prop.table(table(int.data.wo.nas$gruppe, int.data.wo.nas$all_wissen_nachher4),1))

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

#---
# NAs

# age
cbind(table(int.data.wo.nas$alterkat2.binary, useNA="always"), 
      prop.table(table(int.data.wo.nas$alterkat2.binary, useNA="always"),1))

# sex
cbind(table(int.data.wo.nas$sex.factor, useNA="always"), 
      prop.table(table(int.data.wo.nas$sex.factor, useNA="always"),1))


# smoking status
cbind(table(int.data.wo.nas$smoking.status, useNA="always"), 
      prop.table(table(int.data.wo.nas$smoking.status, useNA="always"),1))

# educational level
cbind(table(int.data.wo.nas$schulabschluss.binary, useNA="always"), 
      prop.table(table(int.data.wo.nas$schulabschluss.binary, useNA="always"),1))

# risk perception
cbind(table(int.data.wo.nas$risk_perception, useNA="always"), 
      prop.table(table(int.data.wo.nas$risk_perception, useNA="always"),1))

# asthma or rhinoconjunctivitis
cbind(table(int.data.wo.nas$asthma.or.rhinoconj, useNA="always"), 
      prop.table(table(int.data.wo.nas$asthma.or.rhinoconj, useNA="always"),1))

# parental asthma
cbind(table(int.data.wo.nas$par.asthma, useNA="always"), 
      prop.table(table(int.data.wo.nas$par.asthma, useNA="always"),1))

# knowledge at baseline
cbind(table(int.data.wo.nas$all_wissen3, useNA="always"), 
      prop.table(table(int.data.wo.nas$all_wissen3, useNA="always"),1))

# treatment
cbind(table(int.data.wo.nas$gruppe, useNA="always"), 
      prop.table(table(int.data.wo.nas$gruppe, useNA="always"),1))

