# HOUSEKEEPING
#------------

# add to database
data$allergie3_bekommen.factor <- allergie3_bekommen.factor
data$allergie3_schlimm.factor <- allergie3_schlimm.factor
data$alterkat <- factor(data$alterkat, levels=c(1,2,3), labels=c("18-20", "21-24", "25-44"))

# Relevel variables:
data$sex.factor <- relevel(data$sex.factor, ref = "female")
data$asthma3.factor <- relevel(data$asthma3.factor, ref = "No")
data$allergies.factor <- relevel(data$allergies.factor, ref = "No")
data$par.asthma.factor <- relevel(data$par.asthma.factor, ref = "No")
data$allergie3_schlimm.factor <- relevel(data$allergie3_schlimm.factor, ref = "very bad")
data$schulabschluss.factor.twolevels <- relevel(data$schulabschluss.factor.twolevels, ref = "Hauptschulabschluss")
levels(data$smoking.status.factor)
table(data$allergie3_schlimm.factor, data$all_wissen.factor)
table(data$allergie3_bekommen.factor, data$all_wissen.factor)

table(data$allergie_schlimm, data$all_wissen.factor)
table(data$allergie_bekommen, data$all_wissen.factor)



# modify asthma symptoms and rhinoconjunctivitis:
data$asthma.or.rhinoconj.factor <- NULL

for(i in 1:length(data$asthma3.factor)) {
  if (data$asthma3.factor[i]=="Yes" |
        data$allergies.factor[i]=="Yes") {
    data$asthma.or.rhinoconj.factor[i] <- 1
  } else {
    data$asthma.or.rhinoconj.factor[i] <- 0
  }
}
labels.asthma.rhin <- c("No", "Yes")
data$asthma.or.rhinoconj.factor <- factor(
  data$asthma.or.rhinoconj.factor, labels = labels.asthma.rhin)
table(data$asthma.or.rhinoconj.factor)

# object-oriented: (same as above)
# data$asthma.or.rhinoconj.factor[
  # data$asthma3.factor=="Yes"] <- 1
# data$asthma.or.rhinoconj.factor[
  # data$allergies.factor=="Yes"] <- 1
# data$asthma.or.rhinoconj.factor[
  # data$asthma3.factor=="No" &
    # data$allergies.factor=="No"] <- 0

# age
# age categorical
labels.alterkat2 <- c("younger", "older")
data$alterkat2 <- cut(data$alter, breaks = c(17,24,44), labels = labels.alterkat2)
table(data$all_wissen,data$alterkat2)
prop.table(table(data$all_wissen,data$alterkat2),1)*100

# create new dataset without NAs 
which(is.na(data$all_wissen.factor)) #0
which(is.na(data$sex.factor)) # 162
which(is.na(data$alterkat2)) #43, 45
which(is.na(data$smoking.status.factor)) #0
which(is.na(data$schulabschluss.factor.twolevels)) # 45, 71, 83, 134, 195
which(is.na(data$allergie3_bekommen.factor)) #0
which(is.na(data$allergie3_schlimm.factor)) #0
which(is.na(data$asthma.or.rhinoconj.factor)) #0
which(is.na(data$par.asthma.factor)) #2
# delete: 2, 43, 45, 71, 83, 134, 162, 195. 

data.wo.nas <- data[-c(2, 43, 45, 71, 83, 134, 162, 195),]

#-----------
# MODELS WITH all_wissen (all 6 measures)
#-----------
# all_wissen
# crude models:

# age categorical
age.cat_wissen.glm <- glm(all_wissen.factor ~ alterkat2,
                          data = data.wo.nas,
                          family = binomial, 
                          na.action = na.omit)
summary(age.cat_wissen.glm)
exp(coef(age.cat_wissen.glm))
exp(confint.default(age.cat_wissen.glm))

# sex
sex_wissen.glm <- glm(all_wissen.factor ~ sex.factor,
                      data = data.wo.nas,
                      family = binomial, 
                      na.action = na.exclude)
summary(sex_wissen.glm)
exp(coef(sex_wissen.glm))
exp(confint.default(sex_wissen.glm))

# smoking.status
smoking.status_wissen.glm <- glm(all_wissen.factor ~ smoking.status.factor,
                      data = data.wo.nas,
                      family = binomial, 
                      na.action = na.exclude)
summary(smoking.status_wissen.glm)
exp(coef(smoking.status_wissen.glm))
exp(confint.default(smoking.status_wissen.glm))

# schulabschluss
schulabschluss_wissen.glm <- glm(all_wissen.factor ~ schulabschluss.factor.twolevels,
                      data = data.wo.nas,
                      family = binomial, 
                      na.action = na.exclude)
summary(schulabschluss_wissen.glm)
exp(coef(schulabschluss_wissen.glm))
exp(confint.default(schulabschluss_wissen.glm))


# allergie3_bekommen
# allergie3_bekommen_wissen.glm <- glm(all_wissen.factor ~ allergie3_bekommen.factor,
#                       data = data.wo.nas,
#                       family = binomial, 
#                       na.action = na.exclude)
# summary(allergie3_bekommen_wissen.glm)
# exp(coef(allergie3_bekommen_wissen.glm))
# exp(confint.default(allergie3_bekommen_wissen.glm))
# 
# # allergie3_schlimm
# allergie3_schlimm_wissen.glm <- glm(all_wissen.factor ~ allergie3_schlimm.factor,
#                       data = data.wo.nas,
#                       family = binomial, 
#                       na.action = na.exclude)
# summary(allergie3_schlimm_wissen.glm)
# exp(coef(allergie3_schlimm_wissen.glm))
# exp(confint.default(allergie3_schlimm_wissen.glm))

# risk perception
risk_perception_wissen.glm <- glm(all_wissen.factor ~ risk_perception.factor,
                                  data = data.wo.nas,
                                  family = binomial,
                                  na.action = na.exclude)
summary(risk_perception_wissen.glm)
exp(coef(risk_perception_wissen.glm))
exp(confint.default(risk_perception_wissen.glm))
table(data$risk_perception.factor, data$all_wissen.factor)
round(prop.table(table(data$all_wissen.factor, data$risk_perception.factor),2)*100,2)

# asthma3
# asthma3_wissen.glm <- glm(all_wissen.factor ~ asthma3.factor,
#                       data = data.wo.nas,
#                       family = binomial, 
#                       na.action = na.exclude)
# summary(asthma3_wissen.glm)
# exp(coef(asthma3_wissen.glm))
# exp(confint.default(asthma3_wissen.glm))

# allergies
# allergies_wissen.glm <- glm(all_wissen.factor ~ allergies.factor,
#                       data = data.wo.nas,
#                       family = binomial, 
#                       na.action = na.exclude)
# summary(allergies_wissen.glm)
# exp(coef(allergies_wissen.glm))
# exp(confint.default(allergies_wissen.glm))

# asthma or allergies
asthma.or.allergies_wissen.glm <- glm(all_wissen.factor ~ asthma.or.rhinoconj.factor, data = data.wo.nas, family = binomial, 
                                      na.action = na.exclude)
summary(asthma.or.allergies_wissen.glm)
exp(coef(asthma.or.allergies_wissen.glm))
exp(confint.default(asthma.or.allergies_wissen.glm))

# par.asthma
par.asthma_wissen.glm <- glm(all_wissen.factor ~ par.asthma.factor,
                      data = data.wo.nas,
                      family = binomial, 
                      na.action = na.exclude)
summary(par.asthma_wissen.glm)
exp(coef(par.asthma_wissen.glm))
exp(confint.default(par.asthma_wissen.glm))


# adjusted model w/age.cat ALL WISSEN6
# all_wissen.cat.glm <- glm(all_wissen.factor ~ 
#                         alterkat2 +
#                         sex.factor + 
#                         smoking.status.factor + 
#                         schulabschluss.factor.twolevels + 
#                         allergie3_bekommen.factor + 
#                         allergie3_schlimm.factor + 
#                         asthma.or.rhinoconj.factor + 
#                         par.asthma.factor, 
#                       data = data.wo.nas, 
#                       family = binomial, 
#                       na.action = na.omit)
# summary(all_wissen.cat.glm)
# exp(coef(all_wissen.cat.glm))
# exp(confint.default(all_wissen.cat.glm))

all_wissen.cat.glm <- glm(all_wissen.factor ~ 
                            alterkat2 +
                            sex.factor + 
                            smoking.status.factor + 
                            schulabschluss.factor.twolevels + 
                            risk_perception.factor +
                            asthma.or.rhinoconj.factor + 
                            par.asthma.factor, 
                          data = data.wo.nas, 
                          family = binomial, 
                          na.action = na.omit)
summary(all_wissen.cat.glm)
exp(coef(all_wissen.cat.glm))
exp(confint.default(all_wissen.cat.glm))

#table
#rows1.wissen6 <- c("age", "", "", "sex", "", "", "smok.status", "", "",
#                   "ed.level", "", "", "risk.perc", "", "", "", "", "",
#                   "asthma.rhin.conj", "", "","parent.asthm", "", "")
#
#rows2.wissen6 <- c("younger", "older", "", "female", "male", "", "non-smoker", "smoker", "",
#                   "haupt.", "real + fachhoch + abitur", "", "allerg.bekomm", "", "",
#                   "allerg.schlimm", "", "", "no", "yes", "", "no", "yes","")
#rows3.wissen6 <- c("", "", "", "", "", "", "", "", "", "", "", "",
#                   "likely", "unlikely", "", "very bad", "not so bad", "",
#                   "", "", "", "", "", "")
#rows4.wissen6 <- c(table(data$all_wissen.factor,data$alterkat2)[2],
#                   table(data$all_wissen.factor,data$alterkat2)[4],
#                   "",
#                   
#                   table(data$all_wissen.factor,data$sex.factor)[2],
#                   table(data$all_wissen.factor,data$sex.factor)[4],
#                   "",
#                   
#                   table(data$all_wissen.factor,data$smoking.status.factor)[2],
#                   table(data$all_wissen.factor,data$smoking.status.factor)[4],
#                   "",
#                   
#                   table(data$all_wissen.factor,data$schulabschluss.factor.twolevels)[2],
#                   table(data$all_wissen.factor,data$schulabschluss.factor.twolevels)[4],
#                   "",
#                  
#                   table(data$all_wissen.factor,data$allergie3_bekommen.factor)[2],
#                  table(data$all_wissen.factor,data$allergie3_bekommen.factor)[4],
#                   "",
#                   
#                   table(data$all_wissen.factor,data$allergie3_schlimm.factor)[2],
#                   table(data$all_wissen.factor,data$allergie3_schlimm.factor)[4],
#                   "",
#                   
#                   table(data$all_wissen.factor,data$asthma.or.rhinoconj.factor)[2],
#                   table(data$all_wissen.factor,data$asthma.or.rhinoconj.factor)[4],
#                   "",
#                   
#                   table(data$all_wissen.factor,data$par.asthma.factor)[2],
#                   table(data$all_wissen.factor,data$par.asthma.factor)[4],
#                   ""
#)
#
#rows5.wissen6 <- c(round(prop.table(table(data$all_wissen.factor,data$alterkat2),2)[2]*100,2),
#                   round(prop.table(table(data$all_wissen.factor,data$alterkat2),2)[4]*100,2),
#                   "",
#                   
#                   round(prop.table(table(data$all_wissen.factor,data$sex.factor),2)[2]*100,2),
#                   round(prop.table(table(data$all_wissen.factor,data$sex.factor),2)[4]*100,2),
#                   "",
#                   
#                   round(prop.table(table(data$all_wissen.factor,data$smoking.status.factor),2)[2]*100,2),
#                   round(prop.table(table(data$all_wissen.factor,data$smoking.status.factor),2)[4]*100,2),
#                   "",
#                   
#                   round(prop.table(table(data$all_wissen.factor,data$schulabschluss.factor.twolevels),2)[2]*100,2),
#                 round(prop.table(table(data$all_wissen.factor,data$schulabschluss.factor.twolevels),2)[4]*100,2),
#                  "",
#                   
#                   round(prop.table(table(data$all_wissen.factor,data$allergie3_bekommen.factor),2)[2]*100,2),
#                   round(prop.table(table(data$all_wissen.factor,data$allergie3_bekommen.factor),2)[4]*100,2),
#                   "",
#                   
#                   round(prop.table(table(data$all_wissen.factor,data$allergie3_schlimm.factor),2)[2]*100,2#),
#                   round(prop.table(table(data$all_wissen.factor,data$allergie3_schlimm.factor),2)[4]*100,2),
#                   "",
#                   
#                   round(prop.table(table(data$all_wissen.factor,data$asthma.or.rhinoconj.factor),2)[2]*100,2),
#                   round(prop.table(table(data$all_wissen.factor,data$asthma.or.rhinoconj.factor),2)[4]*100,2),
#                   "",
#                   
#                   round(prop.table(table(data$all_wissen.factor,data$par.asthma.factor),2)[2]*100,2),
#                   round(prop.table(table(data$all_wissen.factor,data$par.asthma.factor),2)[4]*100,2),
#                   "")
#
#rows6.wissen6 <- c("1",
#                   round(exp(coef(age.cat_wissen.glm))[2],2),
#                   "",
#                   "1",
#                   round(exp(coef(sex_wissen.glm))[2],2),
#                   "",
#                   "1",
#                   round(exp(coef(smoking.status_wissen.glm))[2],2),
#                   "",
#                   "1",
#                   round(exp(coef(schulabschluss_wissen.glm))[2],2),
#                   "",
#                   "1",
#                   round(exp(coef(allergie3_bekommen_wissen.glm))[2],2),
#                   "",
#                   "1",
#                   round(exp(coef(allergie3_schlimm_wissen.glm))[2],2),
#                   "",
#                   "1",
#                   round(exp(coef(asthma.or.allergies_wissen.glm))[2],2),
#                   "",
#                   "1",
#                   round(exp(coef(par.asthma_wissen.glm))[2],2),
#                   "")
#
#rows7.wissen6 <- c("1",
#                   
#                   round(exp(confint.default(age.cat_wissen.glm))[2],2), 
#                   round(exp(confint.default(age.cat_wissen.glm))[4],2),
#                   
#                   "1",
#                   
#                   round(exp(confint.default(sex_wissen.glm))[2],2), 
#                   round(exp(confint.default(sex_wissen.glm))[4],2),
#                   
#                   "1",
#                   
#                   round(exp(confint.default(smoking.status_wissen.glm))[2],2),  
#                   round(exp(confint.default(smoking.status_wissen.glm))[4],2),
#                   
#                   "1",
#                   
#                   round(exp(confint.default(schulabschluss_wissen.glm))[2],2), 
#                   round(exp(confint.default(schulabschluss_wissen.glm))[4],2),
#                   
#                   "1",
#                   
#                   round(exp(confint.default(allergie3_bekommen_wissen.glm))[2],2), 
#                   round(exp(confint.default(allergie3_bekommen_wissen.glm))[4],2),
#                   
#                   "1",
#                   
#                   round(exp(confint.default(allergie3_schlimm_wissen.glm))[2],2),
#                   round(exp(confint.default(allergie3_schlimm_wissen.glm))[4],2),
#                   
#                   "1",
#                   
#                   round(exp(confint.default(asthma.or.allergies_wissen.glm))[2],2),
#                   round(exp(confint.default(asthma.or.allergies_wissen.glm))[4],2),
#                   
#                   "1",
#                   
#                   round(exp(confint.default(par.asthma_wissen.glm))[2],2), 
#                   round(exp(confint.default(par.asthma_wissen.glm))[4],2)
#)
#
#rows8.wissen6 <- c("1",
#                   round(exp(coef(all_wissen.cat.glm))[2],2),
#                   "",
#                   "1",
#                   round(exp(coef(all_wissen.cat.glm))[3],2),
#                   "",
#                   "1",
#                   round(exp(coef(all_wissen.cat.glm))[4],2),
#                   "",
#                   "1",
#                   round(exp(coef(all_wissen.cat.glm))[5],2),
#                   "",
#                   "1",
#                   round(exp(coef(all_wissen.cat.glm))[6],2),
#                   "",
#                   "1",
#                   round(exp(coef(all_wissen.cat.glm))[7],2),
#                   "",
#                   "1",
#                   round(exp(coef(all_wissen.cat.glm))[8],2),
#                   "",
#                   "1",
#                   round(exp(coef(all_wissen.cat.glm))[9],2),
#                   "")
#
#rows9.wissen6 <- c(          
#  "1",
#  
#  round(exp(confint.default(all_wissen.cat.glm))[2],2), 
#  round(exp(confint.default(all_wissen.cat.glm))[11],2),
#  
#  "1",
#  
#  round(exp(confint.default(all_wissen.cat.glm))[3],2),  
#  round(exp(confint.default(all_wissen.cat.glm))[12],2),
#  
#  "1",
#  
#   round(exp(confint.default(all_wissen.cat.glm))[4],2), 
#   round(exp(confint.default(all_wissen.cat.glm))[13],2),
#   
#   "1",
#   
#   round(exp(confint.default(all_wissen.cat.glm))[5],2), 
#   round(exp(confint.default(all_wissen.cat.glm))[14],2),
#   
#   "1",
#   
#   round(exp(confint.default(all_wissen.cat.glm))[6],2),
#   round(exp(confint.default(all_wissen.cat.glm))[15],2),
#   
#   "1",
#   
#   round(exp(confint.default(all_wissen.cat.glm))[7],2),
#   round(exp(confint.default(all_wissen.cat.glm))[16],2),
#   
#   "1",
#   
#   round(exp(confint.default(all_wissen.cat.glm))[8],2), 
#   round(exp(confint.default(all_wissen.cat.glm))[17],2),
#   
#   "1",
#   
#   round(exp(confint.default(all_wissen.cat.glm))[9],2), 
#   round(exp(confint.default(all_wissen.cat.glm))[18],2)
# )
# 
# table_all_wissen6.1 <- cbind(rows4.wissen6, rows5.wissen6) #frequencies
# table_all_wissen6.2 <- cbind(rows6.wissen6, rows7.wissen6) # crude ORs and CIs
# table_all_wissen6.3 <- cbind(rows8.wissen6, rows9.wissen6) # adjusted ORs and CIs
# table_all_wissen6 <- cbind(rows4.wissen6, rows5.wissen6, rows6.wissen6, rows7.wissen6, rows8.wissen6, rows9.wissen6)

#-----------
# MODELS WITH all_wissen4 (all 4 measures)
#-----------
# all_wissen4
# crude models:

# age categorical
age.cat_wissen4.glm <- glm(all_wissen4.factor ~ alterkat2,
                           data = data.wo.nas,
                           family = binomial)
summary(age.cat_wissen4.glm)
exp(coef(age.cat_wissen4.glm))
exp(confint.default(age.cat_wissen4.glm))

# sex
sex_wissen4.glm <- glm(all_wissen4.factor ~ sex.factor,
                      data = data.wo.nas,
                      family = binomial)
summary(sex_wissen4.glm)
exp(coef(sex_wissen4.glm))
exp(confint.default(sex_wissen4.glm))


# smoking.status
smoking.status_wissen4.glm <- glm(all_wissen4.factor ~ smoking.status.factor,
                                 data = data.wo.nas,
                                 family = binomial)
summary(smoking.status_wissen4.glm)
exp(coef(smoking.status_wissen4.glm))
exp(confint.default(smoking.status_wissen4.glm))

# schulabschluss
schulabschluss_wissen4.glm <- glm(all_wissen4.factor ~ schulabschluss.factor.twolevels,
                                 data = data.wo.nas,
                                 family = binomial)
summary(schulabschluss_wissen4.glm)
exp(coef(schulabschluss_wissen4.glm))
exp(confint.default(schulabschluss_wissen4.glm))

# # allergie3_bekommen
# allergie3_bekommen_wissen4.glm <- glm(all_wissen4.factor ~ allergie3_bekommen.factor,
#                                      data = data.wo.nas,
#                                      family = binomial)
# summary(allergie3_bekommen_wissen4.glm)
# exp(coef(allergie3_bekommen_wissen4.glm))
# exp(confint.default(allergie3_bekommen_wissen4.glm))
# 
# # allergie3_schlimm
# allergie3_schlimm_wissen4.glm <- glm(all_wissen4.factor ~ allergie3_schlimm.factor,
#                                     data = data.wo.nas,
#                                     family = binomial)
# summary(allergie3_schlimm_wissen4.glm)
# exp(coef(allergie3_schlimm_wissen4.glm))
# exp(confint.default(allergie3_schlimm_wissen4.glm))

# risk perception
risk_perception_wissen4.glm <- glm(all_wissen4.factor ~ risk_perception.factor,
                                   data = data.wo.nas,
                                   family = binomial,
                                   na.action = na.exclude)
summary(risk_perception_wissen4.glm)
exp(coef(risk_perception_wissen4.glm))
exp(confint.default(risk_perception_wissen4.glm))

# asthma3
# asthma3_wissen4.glm <- glm(all_wissen4.factor ~ asthma3.factor,
#                           data = data.wo.nas,
#                           family = binomial)
# summary(asthma3_wissen4.glm)
# exp(coef(asthma3_wissen4.glm))
# exp(confint.default(asthma3_wissen4.glm))

# allergies
# allergies_wissen4.glm <- glm(all_wissen4.factor ~ allergies.factor,
#                             data = data.wo.nas,
#                             family = binomial)
# summary(allergies_wissen4.glm)
# exp(coef(allergies_wissen4.glm))
# exp(confint.default(allergies_wissen4.glm))

# asthma or allergies
asthma.or.allergies_wissen4.glm <- glm(all_wissen4.factor ~ 
                                         asthma.or.rhinoconj.factor, 
                                       data = data.wo.nas, family = binomial)
summary(asthma.or.allergies_wissen4.glm)
exp(coef(asthma.or.allergies_wissen4.glm))
exp(confint.default(asthma.or.allergies_wissen4.glm))

# par.asthma
par.asthma_wissen4.glm <- glm(all_wissen4.factor ~ par.asthma.factor,
                             data = data.wo.nas,
                             family = binomial)
summary(par.asthma_wissen4.glm)
exp(coef(par.asthma_wissen4.glm))
exp(confint.default(par.asthma_wissen4.glm))


# adjusted model w/age.cat WISSEN4
# all_wissen4.cat.glm <- glm(all_wissen4.factor ~ 
#                              sex.factor + 
#                              smoking.status.factor + 
#                              schulabschluss.factor.twolevels + 
#                              risk_perception.factor + 
#                              asthma.or.rhinoconj.factor + 
#                              par.asthma.factor, 
#                            data = data.wo.nas, 
#                            family = binomial)
# summary(all_wissen4.cat.glm)
# exp(coef(all_wissen4.cat.glm))
# exp(confint.default(all_wissen4.cat.glm))

all_wissen4.cat.glm <- glm(all_wissen4.factor ~ 
                             sex.factor + 
                             smoking.status.factor + 
                             schulabschluss.factor.twolevels + 
                             risk_perception.factor + 
                             asthma.or.rhinoconj.factor + 
                             par.asthma.factor, 
                           data = data.wo.nas, 
                           family = binomial)
summary(all_wissen4.cat.glm)
exp(coef(all_wissen4.cat.glm))
exp(confint.default(all_wissen4.cat.glm))

#table
# rows1.wissen4 <- c("age", "", "", "sex", "", "", "smok.status", "", "",
#                    "ed.level", "", "", "risk.perc", "", "", "", "", "",
#                    "asthma.rhin.conj", "", "","parent.asthm", "", "")
# 
# rows2.wissen4 <- c("younger", "older", "", "female", "male", "", "non-smoker", "smoker", "",
#                    "haupt.", "real + fachhoch + abitur", "", "allerg.bekomm", "", "",
#                    "allerg.schlimm", "", "", "no", "yes", "", "no", "yes","")
# rows3.wissen4 <- c("", "", "", "", "", "", "", "", "", "", "", "",
#                    "likely", "unlikely", "", "very bad", "not so bad", "",
#                    "", "", "", "", "", "")
# rows4.wissen4 <- c(table(data$all_wissen4.factor,data$alterkat2)[2],
#                    table(data$all_wissen4.factor,data$alterkat2)[4],
#                    "",
#                    
#                    table(data$all_wissen4.factor,data$sex.factor)[2],
#                    table(data$all_wissen4.factor,data$sex.factor)[4],
#                    "",
#                    
#                    table(data$all_wissen4.factor,data$smoking.status.factor)[2],
#                    table(data$all_wissen4.factor,data$smoking.status.factor)[4],
#                    "",
#                    
#                    table(data$all_wissen4.factor,data$schulabschluss.factor.twolevels)[2],
#                    table(data$all_wissen4.factor,data$schulabschluss.factor.twolevels)[4],
#                    "",
#                    
#                    table(data$all_wissen4.factor,data$allergie3_bekommen.factor)[2],
#                    table(data$all_wissen4.factor,data$allergie3_bekommen.factor)[4],
#                    "",
#                    
#                    table(data$all_wissen4.factor,data$allergie3_schlimm.factor)[2],
#                    table(data$all_wissen4.factor,data$allergie3_schlimm.factor)[4],
#                    "",
#                    
#                    table(data$all_wissen4.factor,data$asthma.or.rhinoconj.factor)[2],
#                    table(data$all_wissen4.factor,data$asthma.or.rhinoconj.factor)[4],
#                    "",
#                    
#                    table(data$all_wissen4.factor,data$par.asthma.factor)[2],
#                    table(data$all_wissen4.factor,data$par.asthma.factor)[4],
#                    ""
# )
# 
# rows5.wissen4 <- c(round(prop.table(table(data$all_wissen4.factor,data$alterkat2),2)[2]*100,2),
#                    round(prop.table(table(data$all_wissen4.factor,data$alterkat2),2)[4]*100,2),
#                    "",
#                    
#                    round(prop.table(table(data$all_wissen4.factor,data$sex.factor),2)[2]*100,2),
#                    round(prop.table(table(data$all_wissen4.factor,data$sex.factor),2)[4]*100,2),
#                    "",
#                    
#                    round(prop.table(table(data$all_wissen4.factor,data$smoking.status.factor),2)[2]*100,2),
#                    round(prop.table(table(data$all_wissen4.factor,data$smoking.status.factor),2)[4]*100,2),
#                    "",
#                    
#                    round(prop.table(table(data$all_wissen4.factor,data$schulabschluss.factor.twolevels),2)[2]*100,2),
#                    round(prop.table(table(data$all_wissen4.factor,data$schulabschluss.factor.twolevels),2)[4]*100,2),
#                    "",
#                    
#                    round(prop.table(table(data$all_wissen4.factor,data$allergie3_bekommen.factor),2)[2]*100,2),
#                    round(prop.table(table(data$all_wissen4.factor,data$allergie3_bekommen.factor),2)[4]*100,2),
#                    "",
#                    
#                    round(prop.table(table(data$all_wissen4.factor,data$allergie3_schlimm.factor),2)[2]*100,2),
#                    round(prop.table(table(data$all_wissen4.factor,data$allergie3_schlimm.factor),2)[4]*100,2),
#                    "",
#                    
#                    round(prop.table(table(data$all_wissen4.factor,data$asthma.or.rhinoconj.factor),2)[2]*100,2),
#                    round(prop.table(table(data$all_wissen4.factor,data$asthma.or.rhinoconj.factor),2)[4]*100,2),
#                    "",
#                    
#                    round(prop.table(table(data$all_wissen4.factor,data$par.asthma.factor),2)[2]*100,2),
#                    round(prop.table(table(data$all_wissen4.factor,data$par.asthma.factor),2)[4]*100,2),
#                    "")
# 
# rows6.wissen4 <- c("1",
#                    round(exp(coef(age.cat_wissen4.glm))[2],2),
#                    "",
#                    "1",
#                    round(exp(coef(sex_wissen4.glm))[2],2),
#                    "",
#                    "1",
#                    round(exp(coef(smoking.status_wissen4.glm))[2],2),
#                    "",
#                    "1",
#                    round(exp(coef(schulabschluss_wissen4.glm))[2],2),
#                    "",
#                    "1",
#                    round(exp(coef(allergie3_bekommen_wissen4.glm))[2],2),
#                    "",
#                    "1",
#                    round(exp(coef(allergie3_schlimm_wissen4.glm))[2],2),
#                    "",
#                    "1",
#                    round(exp(coef(asthma.or.allergies_wissen4.glm))[2],2),
#                    "",
#                    "1",
#                    round(exp(coef(par.asthma_wissen4.glm))[2],2),
#                    "")
# 
# rows7.wissen4 <- c("1",
#                    
#                    round(exp(confint.default(age.cat_wissen4.glm))[2],2), 
#                    round(exp(confint.default(age.cat_wissen4.glm))[4],2),
#                    
#                    "1",
#                    
#                    round(exp(confint.default(sex_wissen4.glm))[2],2), 
#                    round(exp(confint.default(sex_wissen4.glm))[4],2),
#                    
#                    "1",
#                    
#                    round(exp(confint.default(smoking.status_wissen4.glm))[2],2),  
#                    round(exp(confint.default(smoking.status_wissen4.glm))[4],2),
#                    
#                    "1",
#                    
#                    round(exp(confint.default(schulabschluss_wissen4.glm))[2],2), 
#                    round(exp(confint.default(schulabschluss_wissen4.glm))[4],2),
#                    
#                    "1",
#                    
#                    round(exp(confint.default(allergie3_bekommen_wissen4.glm))[2],2), 
#                    round(exp(confint.default(allergie3_bekommen_wissen4.glm))[4],2),
#                    
#                    "1",
#                    
#                    round(exp(confint.default(allergie3_schlimm_wissen4.glm))[2],2),
#                    round(exp(confint.default(allergie3_schlimm_wissen4.glm))[4],2),
#                    
#                    "1",
#                    
#                    round(exp(confint.default(asthma.or.allergies_wissen4.glm))[2],2),
#                    round(exp(confint.default(asthma.or.allergies_wissen4.glm))[4],2),
#                    
#                    "1",
#                    
#                    round(exp(confint.default(par.asthma_wissen4.glm))[2],2), 
#                    round(exp(confint.default(par.asthma_wissen4.glm))[4],2)
# )
# 
# rows8.wissen4 <- c("1",
#                    round(exp(coef(all_wissen4.cat.glm))[3],2),
#                    "",
#                    "1",
#                    round(exp(coef(all_wissen4.cat.glm))[2],2),
#                    "",
#                    "1",
#                    round(exp(coef(all_wissen4.cat.glm))[4],2),
#                    "",
#                    "1",
#                    round(exp(coef(all_wissen4.cat.glm))[5],2),
#                    "",
#                    "1",
#                    round(exp(coef(all_wissen4.cat.glm))[6],2),
#                    "",
#                    "1",
#                    round(exp(coef(all_wissen4.cat.glm))[7],2),
#                    "",
#                    "1",
#                    round(exp(coef(all_wissen4.cat.glm))[8],2),
#                    "",
#                    "1",
#                    round(exp(coef(all_wissen4.cat.glm))[9],2),
#                    "")
# 
# rows9.wissen4 <- c(          
#   "1",
#   
#   round(exp(confint.default(all_wissen4.cat.glm))[3],2), 
#   round(exp(confint.default(all_wissen4.cat.glm))[12],2),
#   
#   "1",
#   
#   round(exp(confint.default(all_wissen4.cat.glm))[2],2),  
#   round(exp(confint.default(all_wissen4.cat.glm))[11],2),
#   
#   "1",
#   
#   round(exp(confint.default(all_wissen4.cat.glm))[4],2), 
#   round(exp(confint.default(all_wissen4.cat.glm))[13],2),
#   
#   "1",
#   
#   round(exp(confint.default(all_wissen4.cat.glm))[5],2), 
#   round(exp(confint.default(all_wissen4.cat.glm))[14],2),
#   
#   "1",
#   
#   round(exp(confint.default(all_wissen4.cat.glm))[6],2),
#   round(exp(confint.default(all_wissen4.cat.glm))[15],2),
#   
#   "1",
#   
#   round(exp(confint.default(all_wissen4.cat.glm))[7],2),
#   round(exp(confint.default(all_wissen4.cat.glm))[16],2),
#   
#   "1",
#   
#   round(exp(confint.default(all_wissen4.cat.glm))[8],2), 
#   round(exp(confint.default(all_wissen4.cat.glm))[17],2),
#   
#   "1",
#   
#   round(exp(confint.default(all_wissen4.cat.glm))[9],2), 
#   round(exp(confint.default(all_wissen4.cat.glm))[18],2)
# )
# 
# table_all_wissen4.1 <- cbind(rows4.wissen4, rows5.wissen4) #frequencies
# table_all_wissen4.2 <- cbind(rows6.wissen4, rows7.wissen4) # crude ORs and CIs
# table_all_wissen4.3 <- cbind(rows8.wissen4, rows9.wissen4) # adjusted ORs and CIs
# table_all_wissen4 <- cbind(rows4.wissen4, rows5.wissen4, rows6.wissen4, rows7.wissen4, rows8.wissen4, rows9.wissen4)

#-----------
# MODELS WITH all_wissen5 (all 5 measures)
#-----------
table(data$all_wissen5.factor)
# all_wissen5
# crude models:
# age categorical
age.cat_wissen5.glm <- glm(all_wissen5.factor ~ alterkat2,
                           data = data.wo.nas,
                           family = binomial)
summary(age.cat_wissen5.glm)
exp(coef(age.cat_wissen5.glm))
exp(confint.default(age.cat_wissen5.glm))

# sex
sex_wissen5.glm <- glm(all_wissen5.factor ~ sex.factor,
                      data = data.wo.nas,
                      family = binomial)
summary(sex_wissen5.glm)
exp(coef(sex_wissen5.glm))
exp(confint.default(sex_wissen5.glm))

# smoking.status
smoking.status_wissen5.glm <- glm(all_wissen5.factor ~ smoking.status.factor,
                                 data = data.wo.nas,
                                 family = binomial)
summary(smoking.status_wissen5.glm)
exp(coef(smoking.status_wissen5.glm))
exp(confint.default(smoking.status_wissen5.glm))

# schulabschluss
schulabschluss_wissen5.glm <- glm(all_wissen5.factor ~ schulabschluss.factor.twolevels,
                                 data = data.wo.nas,
                                 family = binomial)
summary(schulabschluss_wissen5.glm)
exp(coef(schulabschluss_wissen5.glm))
exp(confint.default(schulabschluss_wissen5.glm))

# # allergie3_bekommen
# allergie3_bekommen_wissen5.glm <- glm(all_wissen5.factor ~ allergie3_bekommen.factor,
#                                      data = data.wo.nas,
#                                      family = binomial)
# summary(allergie3_bekommen_wissen5.glm)
# exp(coef(allergie3_bekommen_wissen5.glm))
# exp(confint.default(allergie3_bekommen_wissen5.glm))
# 
# # allergie3_schlimm
# allergie3_schlimm_wissen5.glm <- glm(all_wissen5.factor ~ allergie3_schlimm.factor,
#                                     data = data.wo.nas,
#                                     family = binomial)
# summary(allergie3_schlimm_wissen5.glm)
# exp(coef(allergie3_schlimm_wissen5.glm))
# exp(confint.default(allergie3_schlimm_wissen5.glm))

# risk perception
risk_perception_wissen5.glm <- glm(all_wissen5.factor ~ risk_perception.factor,
                                  data = data.wo.nas,
                                  family = binomial,
                                  na.action = na.exclude)
summary(risk_perception_wissen5.glm)
exp(coef(risk_perception_wissen5.glm))
exp(confint.default(risk_perception_wissen5.glm))


# asthma3
# asthma3_wissen5.glm <- glm(all_wissen5.factor ~ asthma3.factor,
#                           data = data.wo.nas,
#                           family = binomial)
# summary(asthma3_wissen5.glm)
# exp(coef(asthma3_wissen5.glm))
# exp(confint.default(asthma3_wissen5.glm))

# allergies
# allergies_wissen5.glm <- glm(all_wissen5.factor ~ allergies.factor,
#                             data = data.wo.nas,
#                             family = binomial)
# summary(allergies_wissen5.glm)
# exp(coef(allergies_wissen5.glm))
# exp(confint.default(allergies_wissen5.glm))

# asthma or allergies
asthma.or.allergies_wissen5.glm <- glm(all_wissen5.factor ~ asthma.or.rhinoconj.factor, data = data.wo.nas, family = binomial)
summary(asthma.or.allergies_wissen5.glm)
exp(coef(asthma.or.allergies_wissen5.glm))
exp(confint.default(asthma.or.allergies_wissen5.glm))

# par.asthma
par.asthma_wissen5.glm <- glm(all_wissen5.factor ~ par.asthma.factor,
                             data = data.wo.nas,
                             family = binomial)
summary(par.asthma_wissen5.glm)
exp(coef(par.asthma_wissen5.glm))
exp(confint.default(par.asthma_wissen5.glm))

# adjusted model w/age.cat wissen5
# all_wissen5.cat.glm <- glm(all_wissen5.factor ~ 
#                              alterkat2 +
#                              sex.factor +
#                              smoking.status.factor + 
#                              schulabschluss.factor.twolevels + 
#                              allergie3_bekommen.factor + 
#                              allergie3_schlimm.factor + 
#                              asthma.or.rhinoconj.factor + 
#                              par.asthma.factor, 
#                            data = data.wo.nas, 
#                            family = binomial)
# summary(all_wissen5.cat.glm)
# exp(coef(all_wissen5.cat.glm))
# exp(confint.default(all_wissen5.cat.glm))

all_wissen5.cat.glm <- glm(all_wissen5.factor ~ 
                             alterkat2 +
                             sex.factor + 
                             smoking.status.factor + 
                             schulabschluss.factor.twolevels + 
                             risk_perception.factor + 
                             asthma.or.rhinoconj.factor + 
                             par.asthma.factor, 
                           data = data.wo.nas, 
                           family = binomial)
summary(all_wissen5.cat.glm)
exp(coef(all_wissen5.cat.glm))
exp(confint.default(all_wissen5.cat.glm))

#table
# rows1.wissen5 <- c("age", "", "", "sex", "", "", "smok.status", "", "",
#                    "ed.level", "", "", "risk.perc", "", "", "", "", "",
#                    "asthma.rhin.conj", "", "","parent.asthm", "", "")
# 
# rows2.wissen5 <- c("younger", "older", "", "female", "male", "", "non-smoker", "smoker", "",
#                    "haupt.", "real + fachhoch + abitur", "", "allerg.bekomm", "", "",
#                    "allerg.schlimm", "", "", "no", "yes", "", "no", "yes","")
# rows3.wissen5 <- c("", "", "", "", "", "", "", "", "", "", "", "",
#                    "likely", "unlikely", "", "very bad", "not so bad", "",
#                    "", "", "", "", "", "")
# rows4.wissen5 <- c(table(data$all_wissen5.factor,data$alterkat2)[2],
#                    table(data$all_wissen5.factor,data$alterkat2)[4],
#                    "",
#                    
#                    table(data$all_wissen5.factor,data$sex.factor)[2],
#                    table(data$all_wissen5.factor,data$sex.factor)[4],
#                    "",
#                    
#                    table(data$all_wissen5.factor,data$smoking.status.factor)[2],
#                    table(data$all_wissen5.factor,data$smoking.status.factor)[4],
#                    "",
#                    
#                    table(data$all_wissen5.factor,data$schulabschluss.factor.twolevels)[2],
#                    table(data$all_wissen5.factor,data$schulabschluss.factor.twolevels)[4],
#                    "",
#                    
#                    table(data$all_wissen5.factor,data$allergie3_bekommen.factor)[2],
#                    table(data$all_wissen5.factor,data$allergie3_bekommen.factor)[4],
#                    "",
#                    
#                    table(data$all_wissen5.factor,data$allergie3_schlimm.factor)[2],
#                    table(data$all_wissen5.factor,data$allergie3_schlimm.factor)[4],
#                    "",
#                    
#                    table(data$all_wissen5.factor,data$asthma.or.rhinoconj.factor)[2],
#                    table(data$all_wissen5.factor,data$asthma.or.rhinoconj.factor)[4],
#                    "",
#                    
#                    table(data$all_wissen5.factor,data$par.asthma.factor)[2],
#                    table(data$all_wissen5.factor,data$par.asthma.factor)[4],
#                    ""
# )
# 
# rows5.wissen5 <- c(round(prop.table(table(data$all_wissen5.factor,data$alterkat2),2)[2]*100,2),
#                    round(prop.table(table(data$all_wissen5.factor,data$alterkat2),2)[4]*100,2),
#                    "",
#                    
#                    round(prop.table(table(data$all_wissen5.factor,data$sex.factor),2)[2]*100,2),
#                    round(prop.table(table(data$all_wissen5.factor,data$sex.factor),2)[4]*100,2),
#                    "",
#                    
#                    round(prop.table(table(data$all_wissen5.factor,data$smoking.status.factor),2)[2]*100,2),
#                    round(prop.table(table(data$all_wissen5.factor,data$smoking.status.factor),2)[4]*100,2),
#                    "",
#                    
#                    round(prop.table(table(data$all_wissen5.factor,data$schulabschluss.factor.twolevels),2)[2]*100,2),
#                    round(prop.table(table(data$all_wissen5.factor,data$schulabschluss.factor.twolevels),2)[4]*100,2),
#                    "",
#                    
#                    round(prop.table(table(data$all_wissen5.factor,data$allergie3_bekommen.factor),2)[2]*100,2),
#                    round(prop.table(table(data$all_wissen5.factor,data$allergie3_bekommen.factor),2)[4]*100,2),
#                    "",
#                    
#                    round(prop.table(table(data$all_wissen5.factor,data$allergie3_schlimm.factor),2)[2]*100,2),
#                    round(prop.table(table(data$all_wissen5.factor,data$allergie3_schlimm.factor),2)[4]*100,2),
#                    "",
#                    
#                    round(prop.table(table(data$all_wissen5.factor,data$asthma.or.rhinoconj.factor),2)[2]*100,2),
#                    round(prop.table(table(data$all_wissen5.factor,data$asthma.or.rhinoconj.factor),2)[4]*100,2),
#                    "",
#                    
#                    round(prop.table(table(data$all_wissen5.factor,data$par.asthma.factor),2)[2]*100,2),
#                    round(prop.table(table(data$all_wissen5.factor,data$par.asthma.factor),2)[4]*100,2),
#                    "")
# 
# rows6.wissen5 <- c("1",
#                    round(exp(coef(age.cat_wissen5.glm))[2],2),
#                    "",
#                    "1",
#                    round(exp(coef(sex_wissen5.glm))[2],2),
#                    "",
#                    "1",
#                    round(exp(coef(smoking.status_wissen5.glm))[2],2),
#                    "",
#                    "1",
#                    round(exp(coef(schulabschluss_wissen5.glm))[2],2),
#                    "",
#                    "1",
#                    round(exp(coef(allergie3_bekommen_wissen5.glm))[2],2),
#                    "",
#                    "1",
#                    round(exp(coef(allergie3_schlimm_wissen5.glm))[2],2),
#                    "",
#                    "1",
#                    round(exp(coef(asthma.or.allergies_wissen5.glm))[2],2),
#                    "",
#                    "1",
#                    round(exp(coef(par.asthma_wissen5.glm))[2],2),
#                    "")
# 
# rows7.wissen5 <- c("1",
#                    
#                    round(exp(confint.default(age.cat_wissen5.glm))[2],2), 
#                    round(exp(confint.default(age.cat_wissen5.glm))[4],2),
#                    
#                    "1",
#                    
#                    round(exp(confint.default(sex_wissen5.glm))[2],2), 
#                    round(exp(confint.default(sex_wissen5.glm))[4],2),
#                    
#                    "1",
#                    
#                    round(exp(confint.default(smoking.status_wissen5.glm))[2],2),  
#                    round(exp(confint.default(smoking.status_wissen5.glm))[4],2),
#                    
#                    "1",
#                    
#                    round(exp(confint.default(schulabschluss_wissen5.glm))[2],2), 
#                    round(exp(confint.default(schulabschluss_wissen5.glm))[4],2),
#                    
#                    "1",
#                    
#                    round(exp(confint.default(allergie3_bekommen_wissen5.glm))[2],2), 
#                    round(exp(confint.default(allergie3_bekommen_wissen5.glm))[4],2),
#                    
#                    "1",
#                    
#                    round(exp(confint.default(allergie3_schlimm_wissen5.glm))[2],2),
#                    round(exp(confint.default(allergie3_schlimm_wissen5.glm))[4],2),
#                    
#                    "1",
#                    
#                    round(exp(confint.default(asthma.or.allergies_wissen5.glm))[2],2),
#                    round(exp(confint.default(asthma.or.allergies_wissen5.glm))[4],2),
#                    
#                    "1",
#                    
#                    round(exp(confint.default(par.asthma_wissen5.glm))[2],2), 
#                    round(exp(confint.default(par.asthma_wissen5.glm))[4],2)
# )
# 
# rows8.wissen5 <- c("1",
#                    round(exp(coef(all_wissen5.cat.glm))[3],2),
#                    "",
#                    "1",
#                    round(exp(coef(all_wissen5.cat.glm))[2],2),
#                    "",
#                    "1",
#                    round(exp(coef(all_wissen5.cat.glm))[4],2),
#                    "",
#                    "1",
#                    round(exp(coef(all_wissen5.cat.glm))[5],2),
#                    "",
#                    "1",
#                    round(exp(coef(all_wissen5.cat.glm))[6],2),
#                    "",
#                    "1",
#                    round(exp(coef(all_wissen5.cat.glm))[7],2),
#                    "",
#                    "1",
#                    round(exp(coef(all_wissen5.cat.glm))[8],2),
#                    "",
#                    "1",
#                    round(exp(coef(all_wissen5.cat.glm))[9],2),
#                    "")
# 
# rows9.wissen5 <- c(          
#   "1",
#   
#   round(exp(confint.default(all_wissen5.cat.glm))[3],2), 
#   round(exp(confint.default(all_wissen5.cat.glm))[12],2),
#   
#   "1",
#   
#   round(exp(confint.default(all_wissen5.cat.glm))[2],2),  
#   round(exp(confint.default(all_wissen5.cat.glm))[11],2),
#   
#   "1",
#   
#   round(exp(confint.default(all_wissen5.cat.glm))[4],2), 
#   round(exp(confint.default(all_wissen5.cat.glm))[13],2),
#   
#   "1",
#   
#   round(exp(confint.default(all_wissen5.cat.glm))[5],2), 
#   round(exp(confint.default(all_wissen5.cat.glm))[14],2),
#   
#   "1",
#   
#   round(exp(confint.default(all_wissen5.cat.glm))[6],2),
#   round(exp(confint.default(all_wissen5.cat.glm))[15],2),
#   
#   "1",
#   
#   round(exp(confint.default(all_wissen5.cat.glm))[7],2),
#   round(exp(confint.default(all_wissen5.cat.glm))[16],2),
#   
#   "1",
#   
#   round(exp(confint.default(all_wissen5.cat.glm))[8],2), 
#   round(exp(confint.default(all_wissen5.cat.glm))[17],2),
#   
#   "1",
#   
#   round(exp(confint.default(all_wissen5.cat.glm))[9],2), 
#   round(exp(confint.default(all_wissen5.cat.glm))[18],2)
# )
# 
# table_all_wissen5.1 <- cbind(rows4.wissen5, rows5.wissen5) #frequencies
# table_all_wissen5.2 <- cbind(rows6.wissen5, rows7.wissen5) # crude ORs and CIs
# table_all_wissen5.3 <- cbind(rows8.wissen5, rows9.wissen5) # adjusted ORs and CIs
# table_all_wissen5 <- cbind(rows4.wissen5, rows5.wissen5, rows6.wissen5, rows7.wissen5, rows8.wissen5, rows9.wissen5)


#####################
### END OF SCRIPT ###
#####################
