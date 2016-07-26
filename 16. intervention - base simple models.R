int.data.wo.nas <- data.wo.nas[115:230,]


length(int.data.wo.nas$haare_nachher)

##########################
### all_wissen_nachher ###
##########################

# WISSEN NACHHER
# all wissen 6 right

# create new variables to store all correct answers as 1.

int.data.wo.nas$haare_nachher.correct <- int.data.wo.nas$haare_nachher
int.data.wo.nas$arbeitsschuhe_nachher.correct <- NULL
int.data.wo.nas$kleidung_aufb_nachher.correct <- int.data.wo.nas$kleidung_aufb_nachher
int.data.wo.nas$desinfizieren_nachher.correct <- NULL
int.data.wo.nas$schutzbrille_nachher.correct <- NULL
int.data.wo.nas$kleidung_wohnraum_nachher.correct <- int.data.wo.nas$kleidung_wohnraum_nachher

# arbeitsschuhe
int.data.wo.nas$arbeitsschuhe_nachher.correct <- NULL
for (i in 1:length(int.data.wo.nas$arbeitsschuhe_nachher)) {
  if(is.na(int.data.wo.nas$arbeitsschuhe_nachher[i])) {
    int.data.wo.nas$arbeitsschuhe_nachher.correct[i] <- NA
  }
  else if(int.data.wo.nas$arbeitsschuhe_nachher[i]==0) {
    int.data.wo.nas$arbeitsschuhe_nachher.correct[i] <- 1
  }  
  else {
    int.data.wo.nas$arbeitsschuhe_nachher.correct[i] <- 0
  }
}

# desinfizieren
int.data.wo.nas$desinfizieren_nachher.correct <- NULL
for (i in 1:length(int.data.wo.nas$desinfizieren_nachher)) {
  if(is.na(int.data.wo.nas$desinfizieren_nachher[i])) {
    int.data.wo.nas$desinfizieren_nachher.correct[i] <- NA
  }
  else if(int.data.wo.nas$desinfizieren_nachher[i]==0) {
    int.data.wo.nas$desinfizieren_nachher.correct[i] <- 1
  }  
  else {
    int.data.wo.nas$desinfizieren_nachher.correct[i] <- 0
  }
}

# schutzbrille
int.data.wo.nas$schutzbrille_nachher.correct <- NULL
for (i in 1:length(int.data.wo.nas$schutzbrille_nachher)) {
  if(is.na(int.data.wo.nas$schutzbrille_nachher[i])) {
    int.data.wo.nas$schutzbrille_nachher.correct[i] <- NA
  }
  else if(int.data.wo.nas$schutzbrille_nachher[i]==0) {
    int.data.wo.nas$schutzbrille_nachher.correct[i] <- 1
  }  
  else {
    int.data.wo.nas$schutzbrille_nachher.correct[i] <- 0
  }
}

# table all wissen
cbind(
  int.data.wo.nas$haare_nachher.correct, 
    int.data.wo.nas$arbeitsschuhe_nachher.correct, 
    int.data.wo.nas$kleidung_aufb_nachher.correct, 
    int.data.wo.nas$desinfizieren_nachher.correct, 
    int.data.wo.nas$schutzbrille_nachher.correct, 
    int.data.wo.nas$kleidung_wohnraum_nachher.correct,
    int.data.wo.nas$gruppe
)

# construct all wissen
int.data.wo.nas$all_wissen_nachher <- NULL
for(i in 1:length(int.data.wo.nas$haare_nachher.correct)) {
  if( is.na(int.data.wo.nas$haare_nachher.correct)[i] &
      is.na(int.data.wo.nas$arbeitsschuhe_nachher.correct)[i] &
      is.na(int.data.wo.nas$kleidung_aufb_nachher.correct)[i] &
      is.na(int.data.wo.nas$desinfizieren_nachher.correct)[i] &
      is.na(int.data.wo.nas$schutzbrille_nachher.correct)[i] &
      is.na(int.data.wo.nas$kleidung_wohnraum_nachher.correct)[i]
      ) {
    int.data.wo.nas$all_wissen_nachher[i] <- NA
  }
  
  else if(  int.data.wo.nas$haare_nachher.correct[i]==1 & 
            int.data.wo.nas$arbeitsschuhe_nachher.correct[i]==1 &
            int.data.wo.nas$kleidung_aufb_nachher.correct[i]==1 &
            int.data.wo.nas$desinfizieren_nachher.correct[i]==1 &
            int.data.wo.nas$schutzbrille_nachher.correct[i]==1 &
            int.data.wo.nas$kleidung_wohnraum_nachher.correct[i]==1) {
    int.data.wo.nas$all_wissen_nachher[i] <- 1
  }
  
  else {
    int.data.wo.nas$all_wissen_nachher[i] <- 0
  }
}

int.data.wo.nas$all_wissen_nachher[is.na(int.data.wo.nas$all_wissen_nachher)] <- NA
all_wissen.labels <- c("incorrect", "correct")
int.data.wo.nas$all_wissen_nachher.factor <- factor(int.data.wo.nas$all_wissen_nachher, labels = all_wissen.labels)
# int.data.wo.nas$all_wissen_nachher==1: all positive measures
# int.data.wo.nas$all_wissen_nachher==0: not all positive measures
cbind(table(int.data.wo.nas$all_wissen_nachher.factor), round(prop.table(table(int.data.wo.nas$all_wissen_nachher.factor))*100,2))
# note from the ?glm
# For binomial and quasibinomial families the response can also be specified as a factor (when the first level denotes failure and all others success)


# all wissen 0-5 correct
int.data.wo.nas$all_wissen_nachher5 <- NULL
int.data.wo.nas$all_wissen_nachher4 <- NULL
int.data.wo.nas$all_wissen_nachher3 <- NULL
# int.data.wo.nas$all_wissen_nachher2 <- NULL
# int.data.wo.nas$all_wissen_nachher1 <- NULL
# int.data.wo.nas$all_wissen_nachher0 <- NULL



x <- cbind(
  int.data.wo.nas$haare_nachher.correct, 
  int.data.wo.nas$arbeitsschuhe_nachher.correct, 
  int.data.wo.nas$kleidung_aufb_nachher.correct, 
  int.data.wo.nas$desinfizieren_nachher.correct, 
  int.data.wo.nas$schutzbrille_nachher.correct, 
  int.data.wo.nas$kleidung_wohnraum_nachher.correct
)
check <- rowSums(x, na.rm=TRUE)
check[check==0] <- NA
count.only6 <- sum(check==6, na.rm=T) # all 6 correct ## 6
count.only5 <- sum(check==5, na.rm=T) # just 5 correct ## 16
count.only4 <- sum(check==4, na.rm=T) # just 4 correct ## 14
count.only3 <- sum(check==3, na.rm=T) # just 3 correct ## 8 
count.only2 <- sum(check==2, na.rm=T) # just 2 correct ## 3
count.only1 <- sum(check==1, na.rm=T) # just 1 correct ## 0
count.zero <- sum(check==0, na.rm=T) # none correct ## 0

# 5
int.data.wo.nas$all_wissen_nachher5 <- NULL
for (i in 1:length(check)) {
  if(is.na(check)[i]) {
    int.data.wo.nas$all_wissen_nachher5[i] <- NA
  }
  else if(check[i]>=5) {
    int.data.wo.nas$all_wissen_nachher5[i] <- 1
  } 
  else {
    int.data.wo.nas$all_wissen_nachher5[i] <- 0
  }
}

int.data.wo.nas$all_wissen_nachher5.factor <- factor(int.data.wo.nas$all_wissen_nachher5, labels = all_wissen.labels)
# int.data.wo.nas$all_wissen_nachher==1: at least 5 positive measures
# int.data.wo.nas$all_wissen_nachher==0: not at least 5 positive measures
cbind(table(int.data.wo.nas$all_wissen_nachher5.factor), round(prop.table(table(int.data.wo.nas$all_wissen_nachher5.factor)),4))

# 4
int.data.wo.nas$all_wissen_nachher4 <- NULL
for (i in 1:length(check)) {
  if(is.na(check)[i]) {
    int.data.wo.nas$all_wissen_nachher4[i] <- NA
  }
  else if(check[i]>=4) {
    int.data.wo.nas$all_wissen_nachher4[i] <- 1
  } 
  else {
    int.data.wo.nas$all_wissen_nachher4[i] <- 0
  }
}
int.data.wo.nas$all_wissen_nachher4.factor <- factor(int.data.wo.nas$all_wissen_nachher4, labels = all_wissen.labels)
# int.data.wo.nas$all_wissen_nachher==1: at least 4 positive measures
# int.data.wo.nas$all_wissen_nachher==0: not at least 4 positive measures
cbind(table(int.data.wo.nas$all_wissen_nachher4.factor), round(prop.table(table(int.data.wo.nas$all_wissen_nachher4.factor)),4))

# 3
int.data.wo.nas$all_wissen_nachher3 <- NULL
for (i in 1:length(check)) {
  if(is.na(check)[i]) {
    int.data.wo.nas$all_wissen_nachher3[i] <- NA
  }
  else if(check[i]>=3) {
    int.data.wo.nas$all_wissen_nachher3[i] <- 1
  } 
  else {
    int.data.wo.nas$all_wissen_nachher3[i] <- 0
  }
}
int.data.wo.nas$all_wissen_nachher3.factor <- factor(int.data.wo.nas$all_wissen_nachher3, labels = all_wissen.labels)
# int.data.wo.nas$all_wissen_nachher==1: at least 3 positive measures
# int.data.wo.nas$all_wissen_nachher==0: not at least 3 positive measures
cbind(table(int.data.wo.nas$all_wissen_nachher3.factor), round(prop.table(table(int.data.wo.nas$all_wissen_nachher3.factor)),4))


# cbind(table(int.data.wo.nas$all_wissen_nachher5.factor, int.data.wo.nas$gruppe), round(prop.table(table(int.data.wo.nas$all_wissen_nachher5.factor,int.data.wo.nas$gruppe)),4))
cbind(table(int.data.wo.nas$all_wissen_nachher.factor, int.data.wo.nas$gruppe, useNA="ifany"))
cbind(table(int.data.wo.nas$all_wissen_nachher5.factor, int.data.wo.nas$gruppe, useNA="ifany"))
cbind(table(int.data.wo.nas$all_wissen_nachher4.factor, int.data.wo.nas$gruppe, useNA="ifany"))


#-----------
# MODELS WITH all_wissen (all 6 measures)
#-----------
# all_wissen
# crude models: (plus baseline knowledge)

# age categorical
int.age.cat_wissen.glm <- glm(all_wissen_nachher.factor ~ 
                                gruppe + 
                                all_wissen.factor + 
                                alterkat2,
                              data = int.data.wo.nas,
                              family = binomial, 
                              na.action = na.omit)
summary(int.age.cat_wissen.glm)
exp(coef(int.age.cat_wissen.glm))
exp(confint.default(int.age.cat_wissen.glm))

table(int.data.wo.nas$alterkat2, 
      int.data.wo.nas$all_wissen_nachher.factor,
      int.data.wo.nas$gruppe)
# sex
int.sex_wissen.glm <- glm(all_wissen_nachher.factor ~ 
                            gruppe + 
                            all_wissen.factor + 
                            sex.factor,
                          data = int.data.wo.nas,
                          family = binomial, 
                          na.action = na.exclude)
summary(int.sex_wissen.glm)
exp(coef(int.sex_wissen.glm))
exp(confint.default(int.sex_wissen.glm))

table(int.data.wo.nas$sex.factor, 
      int.data.wo.nas$all_wissen_nachher.factor,
      int.data.wo.nas$gruppe)

# smoking.status
int.smoking.status_wissen.glm <- glm(all_wissen_nachher.factor ~
                                       gruppe +
                                       all_wissen.factor + 
                                       smoking.status.factor,
                                     data = int.data.wo.nas,
                                     family = binomial, 
                                     na.action = na.exclude)
summary(int.smoking.status_wissen.glm)
exp(coef(int.smoking.status_wissen.glm))
exp(confint.default(int.smoking.status_wissen.glm))
table(int.data.wo.nas$smoking.status.factor, 
      int.data.wo.nas$all_wissen_nachher.factor,
      int.data.wo.nas$gruppe)

# schulabschluss
int.schulabschluss_wissen.glm <- glm(all_wissen_nachher.factor ~ 
                                       gruppe +
                                       all_wissen.factor + 
                                       schulabschluss.factor.twolevels,
                                     data = int.data.wo.nas,
                                     family = binomial, 
                                     na.action = na.exclude)
summary(int.schulabschluss_wissen.glm)
exp(coef(int.schulabschluss_wissen.glm))
exp(confint.default(int.schulabschluss_wissen.glm))
table(int.data.wo.nas$schulabschluss.factor.twolevels, 
      int.data.wo.nas$all_wissen_nachher.factor,
      int.data.wo.nas$gruppe)


# allergie3_bekommen
# int.allergie3_bekommen_wissen.glm <- glm(all_wissen_nachher.factor ~ all_wissen.factor + allergie3_bekommen.factor,
#                       data = int.data.wo.nas,
#                       family = binomial, 
#                       na.action = na.exclude)
# summary(int.allergie3_bekommen_wissen.glm)
# exp(coef(int.allergie3_bekommen_wissen.glm))
# exp(confint.default(int.allergie3_bekommen_wissen.glm))
# 
# # allergie3_schlimm
# int.allergie3_schlimm_wissen.glm <- glm(all_wissen_nachher.factor ~ all_wissen.factor + allergie3_schlimm.factor,
#                       data = int.data.wo.nas,
#                       family = binomial, 
#                       na.action = na.exclude)
# summary(int.allergie3_schlimm_wissen.glm)
# exp(coef(int.allergie3_schlimm_wissen.glm))
# exp(confint.default(int.allergie3_schlimm_wissen.glm))

# risk perception
int.risk_perception_wissen.glm <- glm(all_wissen_nachher.factor ~
                                        gruppe +
                                        all_wissen.factor + 
                                        risk_perception.factor,
                                      data = int.data.wo.nas,
                                      family = binomial,
                                      na.action = na.exclude)
summary(int.risk_perception_wissen.glm)
exp(coef(int.risk_perception_wissen.glm))
exp(confint.default(int.risk_perception_wissen.glm))
table(int.data.wo.nas$risk_perception.factor, 
      int.data.wo.nas$all_wissen_nachher.factor,
      int.data.wo.nas$gruppe)

# asthma3
# int.asthma3_wissen.glm <- glm(all_wissen_nachher.factor ~ all_wissen.factor + asthma3.factor,
#                       data = int.data.wo.nas,
#                       family = binomial, 
#                       na.action = na.exclude)
# summary(int.asthma3_wissen.glm)
# exp(coef(int.asthma3_wissen.glm))
# exp(confint.default(int.asthma3_wissen.glm))

# allergies
# int.allergies_wissen.glm <- glm(all_wissen_nachher.factor ~ all_wissen.factor + allergies.factor,
#                       data = int.data.wo.nas,
#                       family = binomial, 
#                       na.action = na.exclude)
# summary(int.allergies_wissen.glm)
# exp(coef(int.allergies_wissen.glm))
# exp(confint.default(int.allergies_wissen.glm))

# asthma or allergies
int.asthma.or.allergies_wissen.glm <- glm(all_wissen_nachher.factor ~
                                            gruppe +
                                            all_wissen.factor + 
                                            asthma.or.rhinoconj.factor,
                                            data = int.data.wo.nas,
                                            family = binomial, 
                                            na.action = na.exclude)
summary(int.asthma.or.allergies_wissen.glm)
exp(coef(int.asthma.or.allergies_wissen.glm))
exp(confint.default(int.asthma.or.allergies_wissen.glm))
table(int.data.wo.nas$asthma.or.rhinoconj.factor, 
      int.data.wo.nas$all_wissen_nachher.factor,
      int.data.wo.nas$gruppe)

# par.asthma
int.par.asthma_wissen.glm <- glm(all_wissen_nachher.factor ~ 
                                   gruppe +
                                   all_wissen.factor + 
                                   par.asthma.factor,
                                 data = int.data.wo.nas,
                                 family = binomial, 
                                 na.action = na.exclude)
summary(int.par.asthma_wissen.glm)
exp(coef(int.par.asthma_wissen.glm))
exp(confint.default(int.par.asthma_wissen.glm))
table(int.data.wo.nas$par.asthma.factor, 
      int.data.wo.nas$all_wissen_nachher.factor,
      int.data.wo.nas$gruppe)

# knowledge baseline
int.par.asthma_wissen.glm <- glm(all_wissen_nachher.factor ~ 
                                   gruppe +
                                   all_wissen.factor + 
                                   par.asthma.factor,
                                 data = int.data.wo.nas,
                                 family = binomial, 
                                 na.action = na.exclude)
summary(int.par.asthma_wissen.glm)
exp(coef(int.par.asthma_wissen.glm))
exp(confint.default(int.par.asthma_wissen.glm))
table(int.data.wo.nas$par.asthma.factor, 
      int.data.wo.nas$all_wissen_nachher.factor,
      int.data.wo.nas$gruppe)


# adjusted model w/age.cat ALL WISSEN6
# int.all_wissen.cat.glm <- glm(all_wissen_nachher.factor ~ 
#                         all_wissen.factor +
#                         alterkat2 +
#                         sex.factor + 
#                         smoking.status.factor + 
#                         schulabschluss.factor.twolevels + 
#                         allergie3_bekommen.factor + 
#                         allergie3_schlimm.factor + 
#                         asthma.or.rhinoconj.factor + 
#                         par.asthma.factor, 
#                       data = int.data.wo.nas, 
#                       family = binomial, 
#                       na.action = na.omit)
# summary(int.all_wissen.cat.glm)
# exp(coef(int.all_wissen.cat.glm))
# exp(confint.default(int.all_wissen.cat.glm))



int.all_wissen.cat.glm <- glm(all_wissen_nachher.factor ~ 
                                gruppe +
                                all_wissen.factor +
                                alterkat2 +
                                sex.factor + 
                                smoking.status.factor + 
                                schulabschluss.factor.twolevels + 
                                risk_perception.factor +
                                asthma.or.rhinoconj.factor + 
                                par.asthma.factor, 
                              data = int.data.wo.nas, 
                              family = binomial, 
                              na.action = na.omit)
summary(int.all_wissen.cat.glm)
exp(coef(int.all_wissen.cat.glm))
exp(confint.default(int.all_wissen.cat.glm))


#-----------
# MODELS WITH all_wissen (all 6 measures)
# WITHOUT BASELINE KNOWLEDGE
#-----------
# all_wissen
# crude models: (plus baseline knowledge)

# age categorical
int.age.cat_wissen.glm <- glm(all_wissen_nachher.factor ~ 
                                gruppe + 
                                alterkat2,
                              data = int.data.wo.nas,
                              family = binomial, 
                              na.action = na.omit)
summary(int.age.cat_wissen.glm)
exp(coef(int.age.cat_wissen.glm))
exp(confint.default(int.age.cat_wissen.glm))
28+14
3/42
table(int.data.wo.nas$alterkat2, 
      int.data.wo.nas$all_wissen_nachher.factor,
      int.data.wo.nas$gruppe)
prop.table(table(int.data.wo.nas$alterkat2, 
                 int.data.wo.nas$all_wissen_nachher.factor,
                 int.data.wo.nas$gruppe),2)*100

# sex
int.sex_wissen.glm <- glm(all_wissen_nachher.factor ~ 
                            gruppe + 
                            sex.factor,
                          data = int.data.wo.nas,
                          family = binomial, 
                          na.action = na.exclude)
summary(int.sex_wissen.glm)
exp(coef(int.sex_wissen.glm))
exp(confint.default(int.sex_wissen.glm))

table(int.data.wo.nas$sex.factor, 
      int.data.wo.nas$all_wissen_nachher.factor,
      int.data.wo.nas$gruppe)

# smoking.status
int.smoking.status_wissen.glm <- glm(all_wissen_nachher.factor ~
                                       gruppe +
                                       smoking.status.factor,
                                     data = int.data.wo.nas,
                                     family = binomial, 
                                     na.action = na.exclude)
summary(int.smoking.status_wissen.glm)
exp(coef(int.smoking.status_wissen.glm))
exp(confint.default(int.smoking.status_wissen.glm))
table(int.data.wo.nas$smoking.status.factor, 
      int.data.wo.nas$all_wissen_nachher.factor,
      int.data.wo.nas$gruppe)

# schulabschluss
int.schulabschluss_wissen.glm <- glm(all_wissen_nachher.factor ~ 
                                       gruppe +
                                       schulabschluss.factor.twolevels,
                                     data = int.data.wo.nas,
                                     family = binomial, 
                                     na.action = na.exclude)
summary(int.schulabschluss_wissen.glm)
exp(coef(int.schulabschluss_wissen.glm))
exp(confint.default(int.schulabschluss_wissen.glm))
table(int.data.wo.nas$schulabschluss.factor.twolevels, 
      int.data.wo.nas$all_wissen_nachher.factor,
      int.data.wo.nas$gruppe)


# allergie3_bekommen
# int.allergie3_bekommen_wissen.glm <- glm(all_wissen_nachher.factor ~ all_wissen.factor + allergie3_bekommen.factor,
#                       data = int.data.wo.nas,
#                       family = binomial, 
#                       na.action = na.exclude)
# summary(int.allergie3_bekommen_wissen.glm)
# exp(coef(int.allergie3_bekommen_wissen.glm))
# exp(confint.default(int.allergie3_bekommen_wissen.glm))
# 
# # allergie3_schlimm
# int.allergie3_schlimm_wissen.glm <- glm(all_wissen_nachher.factor ~ all_wissen.factor + allergie3_schlimm.factor,
#                       data = int.data.wo.nas,
#                       family = binomial, 
#                       na.action = na.exclude)
# summary(int.allergie3_schlimm_wissen.glm)
# exp(coef(int.allergie3_schlimm_wissen.glm))
# exp(confint.default(int.allergie3_schlimm_wissen.glm))

# risk perception
int.risk_perception_wissen.glm <- glm(all_wissen_nachher.factor ~
                                        gruppe +
                                        risk_perception.factor,
                                      data = int.data.wo.nas,
                                      family = binomial,
                                      na.action = na.exclude)
summary(int.risk_perception_wissen.glm)
exp(coef(int.risk_perception_wissen.glm))
exp(confint.default(int.risk_perception_wissen.glm))
table(int.data.wo.nas$risk_perception.factor, 
      int.data.wo.nas$all_wissen_nachher.factor,
      int.data.wo.nas$gruppe)

# asthma3
# int.asthma3_wissen.glm <- glm(all_wissen_nachher.factor ~ all_wissen.factor + asthma3.factor,
#                       data = int.data.wo.nas,
#                       family = binomial, 
#                       na.action = na.exclude)
# summary(int.asthma3_wissen.glm)
# exp(coef(int.asthma3_wissen.glm))
# exp(confint.default(int.asthma3_wissen.glm))

# allergies
# int.allergies_wissen.glm <- glm(all_wissen_nachher.factor ~ all_wissen.factor + allergies.factor,
#                       data = int.data.wo.nas,
#                       family = binomial, 
#                       na.action = na.exclude)
# summary(int.allergies_wissen.glm)
# exp(coef(int.allergies_wissen.glm))
# exp(confint.default(int.allergies_wissen.glm))

# asthma or allergies
int.asthma.or.allergies_wissen.glm <- glm(all_wissen_nachher.factor ~
                                            gruppe +
                                            asthma.or.rhinoconj.factor,
                                          data = int.data.wo.nas,
                                          family = binomial, 
                                          na.action = na.exclude)
summary(int.asthma.or.allergies_wissen.glm)
exp(coef(int.asthma.or.allergies_wissen.glm))
exp(confint.default(int.asthma.or.allergies_wissen.glm))
table(int.data.wo.nas$asthma.or.rhinoconj.factor, 
      int.data.wo.nas$all_wissen_nachher.factor,
      int.data.wo.nas$gruppe)

# par.asthma
int.par.asthma_wissen.glm <- glm(all_wissen_nachher.factor ~ 
                                   gruppe +
                                   par.asthma.factor,
                                 data = int.data.wo.nas,
                                 family = binomial, 
                                 na.action = na.exclude)
summary(int.par.asthma_wissen.glm)
exp(coef(int.par.asthma_wissen.glm))
exp(confint.default(int.par.asthma_wissen.glm))
table(int.data.wo.nas$par.asthma.factor, 
      int.data.wo.nas$all_wissen_nachher.factor,
      int.data.wo.nas$gruppe)

# knowledge baseline
int.all_wissen.factor_wissen.glm <- glm(all_wissen_nachher.factor ~ 
                                   gruppe +
                                   all_wissen.factor,
                                 data = int.data.wo.nas,
                                 family = binomial, 
                                 na.action = na.exclude)
summary(int.all_wissen.factor_wissen.glm)
exp(coef(int.all_wissen.factor_wissen.glm))
exp(confint.default(int.all_wissen.factor_wissen.glm))
table(int.data.wo.nas$all_wissen.factor, 
      int.data.wo.nas$all_wissen_nachher.factor,
      int.data.wo.nas$gruppe)


# adjusted model w/age.cat ALL WISSEN6
# int.all_wissen.cat.glm <- glm(all_wissen_nachher.factor ~ 
#                         all_wissen.factor +
#                         alterkat2 +
#                         sex.factor + 
#                         smoking.status.factor + 
#                         schulabschluss.factor.twolevels + 
#                         allergie3_bekommen.factor + 
#                         allergie3_schlimm.factor + 
#                         asthma.or.rhinoconj.factor + 
#                         par.asthma.factor, 
#                       data = int.data.wo.nas, 
#                       family = binomial, 
#                       na.action = na.omit)
# summary(int.all_wissen.cat.glm)
# exp(coef(int.all_wissen.cat.glm))
# exp(confint.default(int.all_wissen.cat.glm))

# Crude all 6
int.all_wissen_nachher.cat.glm.crude <- glm(all_wissen_nachher.factor ~ 
                                              gruppe, 
                                            data = int.data.wo.nas, 
                                            family = binomial, 
                                            na.action = na.omit)
summary(int.all_wissen_nachher.cat.glm.crude)
exp(coef(int.all_wissen_nachher.cat.glm.crude))
exp(confint.default(int.all_wissen_nachher.cat.glm.crude))

# Adjusted all 6
int.all_wissen_nachher.cat.glm <- glm(all_wissen_nachher.factor ~ 
                                gruppe +
                                all_wissen.factor +
                                alterkat2 +
                                sex.factor + 
                                smoking.status.factor + 
                                schulabschluss.factor.twolevels + 
                                risk_perception.factor +
                                asthma.or.rhinoconj.factor + 
                                par.asthma.factor, 
                              data = int.data.wo.nas, 
                              family = binomial, 
                              na.action = na.omit)
summary(int.all_wissen_nachher.cat.glm)
exp(coef(int.all_wissen_nachher.cat.glm))
exp(confint.default(int.all_wissen_nachher.cat.glm))

# Crude at least 5
int.all_wissen_nachher5.cat.glm.crude <- glm(all_wissen_nachher5.factor ~ 
                                              gruppe, 
                                            data = int.data.wo.nas, 
                                            family = binomial, 
                                            na.action = na.omit)
summary(int.all_wissen_nachher5.cat.glm.crude)
exp(coef(int.all_wissen_nachher5.cat.glm.crude))
exp(confint.default(int.all_wissen_nachher5.cat.glm.crude))

# Adjusted at least 5
int.all_wissen_nachher5.cat.glm <- glm(all_wissen_nachher5.factor ~ 
                                        gruppe +
                                        all_wissen5.factor +
                                        alterkat2 +
                                        sex.factor + 
                                        smoking.status.factor + 
                                        schulabschluss.factor.twolevels + 
                                        risk_perception.factor +
                                        asthma.or.rhinoconj.factor + 
                                        par.asthma.factor, 
                                      data = int.data.wo.nas, 
                                      family = binomial, 
                                      na.action = na.omit)
summary(int.all_wissen_nachher5.cat.glm)
exp(coef(int.all_wissen_nachher5.cat.glm))
exp(confint.default(int.all_wissen_nachher5.cat.glm))

# Crude at least 4
int.all_wissen_nachher4.cat.glm.crude <- glm(all_wissen_nachher4.factor ~ 
                                               gruppe, 
                                             data = int.data.wo.nas, 
                                             family = binomial, 
                                             na.action = na.omit)
summary(int.all_wissen_nachher4.cat.glm.crude)
exp(coef(int.all_wissen_nachher4.cat.glm.crude))
exp(confint.default(int.all_wissen_nachher4.cat.glm.crude))

# Adjusted at least 4
int.all_wissen_nachher4.cat.glm <- glm(all_wissen_nachher4.factor ~ 
                                         gruppe +
                                         all_wissen4.factor +
                                         alterkat2 +
                                         sex.factor + 
                                         smoking.status.factor + 
                                         schulabschluss.factor.twolevels + 
                                         risk_perception.factor +
                                         asthma.or.rhinoconj.factor + 
                                         par.asthma.factor, 
                                       data = int.data.wo.nas, 
                                       family = binomial, 
                                       na.action = na.omit)
summary(int.all_wissen_nachher4.cat.glm)
exp(coef(int.all_wissen_nachher4.cat.glm))
exp(confint.default(int.all_wissen_nachher4.cat.glm))



#---------------#
# END OF SCRIPT #
#---------------#