#-------
# Table 2 - Sociodemographic characteristics and intervention group
#-------

# all from the intervention==1
# variable: data.wo.nas$four.groups
# group 1: control, dropouts
# group 2: control, completed 
# group 3: experimental, dropouts
# group 4: experimental, completed


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

# completed and control variable:
data.wo.nas$completed.table2 <- data.wo.nas$four.groups[115:230]
cbind(nachher.table[115:230], data.wo.nas$four.groups[115:230])
data.wo.nas$completed.table2[data.wo.nas$four.groups==1 | data.wo.nas$four.groups==3] <- NA
data.wo.nas$completed.table2[data.wo.nas$four.groups==2] <- 0
data.wo.nas$completed.table2[data.wo.nas$four.groups==4] <- 1
int.data.wo.nas$completed.table2 <- NULL
int.data.wo.nas$completed.table2 <- data.wo.nas$completed.table2[115:230]

# age
cbind(table(int.data.wo.nas$alterkat2.binary, int.data.wo.nas$completed.table2), 
      prop.table(table(int.data.wo.nas$alterkat2.binary, int.data.wo.nas$completed.table2),2))

# sex
cbind(table(int.data.wo.nas$sex.factor, int.data.wo.nas$completed.table2), 
      prop.table(table(int.data.wo.nas$sex.factor, int.data.wo.nas$completed.table2),2))

# smoking status
cbind(table(int.data.wo.nas$smoking.status, int.data.wo.nas$completed.table2), 
      prop.table(table(int.data.wo.nas$smoking.status, int.data.wo.nas$completed.table2),2))

# educational level
cbind(table(int.data.wo.nas$schulabschluss.binary, int.data.wo.nas$completed.table2), 
      prop.table(table(int.data.wo.nas$schulabschluss.binary, int.data.wo.nas$completed.table2),2))

# risk perception
cbind(table(int.data.wo.nas$risk_perception, int.data.wo.nas$completed.table2), 
      prop.table(table(int.data.wo.nas$risk_perception, int.data.wo.nas$completed.table2),2))

# asthma or rhinoconjunctivitis
cbind(table(int.data.wo.nas$asthma.or.rhinoconj, int.data.wo.nas$completed.table2), 
      prop.table(table(int.data.wo.nas$asthma.or.rhinoconj, int.data.wo.nas$completed.table2),2))

# parental asthma
cbind(table(int.data.wo.nas$par.asthma, int.data.wo.nas$completed.table2), 
      prop.table(table(int.data.wo.nas$par.asthma, int.data.wo.nas$completed.table2),2))

# knowledge at baseline: wissen6
cbind(table(int.data.wo.nas$all_wissen, int.data.wo.nas$completed.table2), 
      prop.table(table(int.data.wo.nas$all_wissen, int.data.wo.nas$completed.table2),2))

# knowledge at baseline: wissen5
cbind(table(int.data.wo.nas$all_wissen5, int.data.wo.nas$completed.table2), 
      prop.table(table(int.data.wo.nas$all_wissen5, int.data.wo.nas$completed.table2),2))

# knowledge at baseline: wissen6
cbind(table(int.data.wo.nas$all_wissen4, int.data.wo.nas$completed.table2), 
      prop.table(table(int.data.wo.nas$all_wissen4, int.data.wo.nas$completed.table2),2))

#---------------#
# END OF SCRIPT #
#---------------#
