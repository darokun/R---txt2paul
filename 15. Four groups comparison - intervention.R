# NOTE: Use data.wo.nas.wo.nas

# names(data.wo.nas)
# table(data.wo.nas$intervention)
# 
# table(data.wo.nas$gruppe)
# 
# 
# table(data.wo.nas$haare_nachher)
# 
# cbind(data.wo.nas$haare, data.wo.nas$haare_nachher)[120:238,]
# sum(is.na(data.wo.nas$haare_nachher)[120:238]) #70
# sum(!is.na(data.wo.nas$haare_nachher)[120:238])

completed1 <- NULL
completed2 <- NULL
data.wo.nas$completed <- NULL

nachher.table <- cbind(data.wo.nas$haare_nachher, data.wo.nas$arbeitsschuhe_nachher, data.wo.nas$kleidung_aufb_nachher, data.wo.nas$desinfizieren_nachher, data.wo.nas$schutzbrille_nachher, data.wo.nas$kleidung_wohnraum_nachher)

# I create a data.wo.nas$completed variable, which has answers 0=no and 1=yes, on whether or not they completed the intervention phase (either in the control or in the experimental group)

cbind(nachher.table,data.wo.nas$completed)
data.wo.nas$completed <- rowSums(nachher.table)
length(data.wo.nas$completed)

for(i in 1:length(data.wo.nas$completed)) {
  if(is.na(rowSums(nachher.table)[i])==T) {
    data.wo.nas$completed[i] <- 0
  }
  else {
    data.wo.nas$completed[i] <- 1
  }
}


data.wo.nas$completed[1:114] <- NA
table(data.wo.nas$completed)
table(data$completed)
cbind(data.wo.nas$gruppe, data.wo.nas$completed)
cbind(data$gruppe, data$completed)

# setting up the four groups:

# all from the intervention==1
# group 1: control, dropouts
# group 2: control, completed
# group 3: experimental, dropouts
# group 4: experimental, completed

length(data.wo.nas$intervention)

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

sum(group4, na.rm=T)
group1 <- data.wo.nas$four.groups==1
group2 <- data.wo.nas$four.groups==2
group3 <- data.wo.nas$four.groups==3
group4 <- data.wo.nas$four.groups==4

table(data.wo.nas$four.groups)


# Two groups by level (dropouts and experimental)
# controls
data.wo.nas$two.groups.control <- NULL
data.wo.nas$two.groups.control[
  data.wo.nas$intervention==1 & data.wo.nas$gruppe==0 & data.wo.nas$completed==1] <- 0
data.wo.nas$two.groups.control[
  data.wo.nas$intervention==1 & data.wo.nas$gruppe==0 & data.wo.nas$completed==0] <- 1
table(data.wo.nas$two.groups.control)


# experimental
data.wo.nas$two.groups.exp <- NULL
data.wo.nas$two.groups.exp[
  data.wo.nas$intervention==1 & data.wo.nas$gruppe==1 & data.wo.nas$completed==1] <- 0
data.wo.nas$two.groups.exp[
  data.wo.nas$intervention==1 & data.wo.nas$gruppe==1 & data.wo.nas$completed==0] <- 1
table(data.wo.nas$two.groups.exp)


# demographics of these 4 groups:
table(data.wo.nas$sex.factor,data.wo.nas$four.groups, useNA = "ifany")

table(data.wo.nas$sex.factor,data.wo.nas$two.groups.control)
# prop.table(table(data.wo.nas$sex.factor,data.wo.nas$two.groups.control, useNA="no"))*100

table(data.wo.nas$sex.factor,data.wo.nas$two.groups.exp)

fisher.test(table(data.wo.nas$two.groups.control,data.wo.nas$sex.factor))
fisher.test(table(data.wo.nas$two.groups.exp,data.wo.nas$sex.factor))

#---

median(data.wo.nas$alter[data.wo.nas$four.groups==1], na.rm=T)
median(data.wo.nas$alter[data.wo.nas$four.groups==2], na.rm=T)
median(data.wo.nas$alter[data.wo.nas$four.groups==3], na.rm=T)
median(data.wo.nas$alter[data.wo.nas$four.groups==4], na.rm=T)

#---

table(data.wo.nas$smoking.status.factor,data.wo.nas$two.groups.control)
# prop.table(table(data.wo.nas$sex.factor,data.wo.nas$two.groups.control, useNA="no"))*100
table(data.wo.nas$smoking.status.factor,data.wo.nas$two.groups.exp)

fisher.test(table(data.wo.nas$two.groups.control,data.wo.nas$smoking.status.factor))
chisq.test(table(data.wo.nas$two.groups.exp,data.wo.nas$smoking.status.factor))

#---

# table(data.wo.nas$studienort.factor,data.wo.nas$four.groups, useNA = "ifany")
table(data.wo.nas$studienort.factor,data.wo.nas$two.groups.control)
table(data.wo.nas$studienort.factor,data.wo.nas$two.groups.exp)

#---

# table(data.wo.nas$schulabschluss.factor.twolevels,data.wo.nas$four.groups, useNA = "ifany")
table(data.wo.nas$schulabschluss.factor.twolevels,data.wo.nas$two.groups.control)
fisher.test(table(data.wo.nas$two.groups.control,data.wo.nas$schulabschluss.factor.twolevels))

table(data.wo.nas$schulabschluss.factor.twolevels,data.wo.nas$two.groups.exp)
fisher.test(table(data.wo.nas$two.groups.exp,data.wo.nas$schulabschluss.factor.twolevels))

#---

# # table(data.wo.nas$asthma.dx,data.wo.nas$four.groups, useNA = "ifany")
# table(data.wo.nas$asthma.dx,data.wo.nas$two.groups.control)
# fisher.test(table(data.wo.nas$two.groups.control,data.wo.nas$asthma.dx))
# 
# table(data.wo.nas$asthma.dx,data.wo.nas$two.groups.exp)
# fisher.test(table(data.wo.nas$two.groups.exp,data.wo.nas$asthma.dx))

#---

# # table(data.wo.nas$allergies.factor,data.wo.nas$four.groups, useNA = "ifany")
# 
# table(data.wo.nas$allergies.factor,data.wo.nas$two.groups.control)
# fisher.test(table(data.wo.nas$two.groups.control,data.wo.nas$allergies.factor))
# 
# table(data.wo.nas$allergies.factor,data.wo.nas$two.groups.exp)
# fisher.test(table(data.wo.nas$two.groups.exp,data.wo.nas$allergies.factor))

#---

# risk perception
# table(data.wo.nas$allergie3_bekommen.factor,data.wo.nas$four.groups, useNA = "ifany")
table(data.wo.nas$allergie3_bekommen.factor,data.wo.nas$two.groups.control)
fisher.test(table(data.wo.nas$two.groups.control,data.wo.nas$allergie3_bekommen.factor))

table(data.wo.nas$allergie3_bekommen.factor,data.wo.nas$two.groups.exp)
fisher.test(table(data.wo.nas$two.groups.exp,data.wo.nas$allergie3_bekommen.factor))


# table(data.wo.nas$allergie3_schlimm.factor,data.wo.nas$four.groups, useNA = "ifany")
table(data.wo.nas$allergie3_schlimm.factor,data.wo.nas$two.groups.control)
fisher.test(table(data.wo.nas$two.groups.control,data.wo.nas$allergie3_schlimm.factor))

table(data.wo.nas$allergie3_schlimm.factor,data.wo.nas$two.groups.exp)
fisher.test(table(data.wo.nas$two.groups.exp,data.wo.nas$allergie3_schlimm.factor))

#---

#asthma or rhinoc.
# table(data.wo.nas$asthma.or.rhinoconj.factor,data.wo.nas$four.groups, useNA = "ifany")
table(data.wo.nas$asthma.or.rhinoconj.factor,data.wo.nas$two.groups.control)
fisher.test(table(data.wo.nas$two.groups.control,data.wo.nas$asthma.or.rhinoconj.factor))

table(data.wo.nas$asthma.or.rhinoconj.factor,data.wo.nas$two.groups.exp)
chisq.test(table(data.wo.nas$two.groups.exp,data.wo.nas$asthma.or.rhinoconj.factor))

#---

#par.asthma
# table(data.wo.nas$par.asthma.factor,data.wo.nas$four.groups, useNA = "ifany")
table(data.wo.nas$par.asthma.factor,data.wo.nas$two.groups.control)
fisher.test(table(data.wo.nas$two.groups.control,data.wo.nas$par.asthma.factor))

table(data.wo.nas$par.asthma.factor,data.wo.nas$two.groups.exp)
fisher.test(table(data.wo.nas$two.groups.exp,data.wo.nas$par.asthma.factor))

#---

#####################
### END OF SCRIPT ###
#####################

# par.asthma da significativo


