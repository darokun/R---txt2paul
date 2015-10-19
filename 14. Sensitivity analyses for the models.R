#----
# models
#----

# WISSEN6
# age categorical: age.cat_wissen.glm
# sex: sex_wissen.glm
# smoking.status: smoking.status_wissen.glm
# schulabschluss: schulabschluss_wissen.glm
# allergie3_bekommen: allergie3_bekommen_wissen.glm
# allergie3_schlimm: allergie3_schlimm_wissen.glm
# asthma or allergies: asthma.or.allergies_wissen.glm
# par.asthma: par.asthma_wissen.glm
# adjusted model w/age.cat ALL WISSEN6: all_wissen.cat.glm

# WISSEN4
# age categorical: age.cat_wissen4.glm
# sex: sex_wissen4.glm
# smoking.status: smoking.status_wissen4.glm
# schulabschluss: schulabschluss_wissen4.glm
# allergie3_bekommen: allergie3_bekommen_wissen4.glm
# allergie3_schlimm: allergie3_schlimm_wissen4.glm
# asthma or allergies: asthma.or.allergies_wissen4.glm
# par.asthma: par.asthma_wissen4.glm
# adjusted model w/age.cat ALL WISSEN6: all_wissen4.cat.glm

# WISSEN5
# age categorical: age.cat_wissen5.glm
# sex: sex_wissen5.glm
# smoking.status: smoking.status_wissen5.glm
# schulabschluss: schulabschluss_wissen5.glm
# allergie3_bekommen: allergie3_bekommen_wissen5.glm
# allergie3_schlimm: allergie3_schlimm_wissen5.glm
# asthma or allergies: asthma.or.allergies_wissen5.glm
# par.asthma: par.asthma_wissen5.glm
# adjusted model w/age.cat ALL WISSEN6: all_wissen5.cat.glm

#----
# Anova for WISSEN6
#----

anova(age.cat_wissen.glm, all_wissen.cat.glm, test = "Chisq")
anova(sex_wissen.glm, all_wissen.cat.glm, test = "Chisq")
anova(smoking.status_wissen.glm, all_wissen.cat.glm, test = "Chisq")
anova(schulabschluss_wissen.glm, all_wissen.cat.glm, test = "Chisq")
anova(allergie3_bekommen_wissen.glm, all_wissen.cat.glm, test = "Chisq")
anova(allergie3_schlimm_wissen.glm, all_wissen.cat.glm, test = "Chisq")
anova(asthma.or.allergies_wissen.glm, all_wissen.cat.glm, test = "Chisq")
anova(par.asthma_wissen.glm, all_wissen.cat.glm, test = "Chisq")


#----
# Anova for WISSEN4
#----

anova(age.cat_wissen4.glm, all_wissen4.cat.glm, test = "Chisq")
anova(sex_wissen4.glm, all_wissen4.cat.glm, test = "Chisq")
anova(smoking.status_wissen4.glm, all_wissen4.cat.glm, test = "Chisq")
anova(schulabschluss_wissen4.glm, all_wissen4.cat.glm, test = "Chisq")
anova(allergie3_bekommen_wissen4.glm, all_wissen4.cat.glm, test = "Chisq")
anova(allergie3_schlimm_wissen4.glm, all_wissen4.cat.glm, test = "Chisq")
anova(asthma.or.allergies_wissen4.glm, all_wissen4.cat.glm, test = "Chisq")
anova(par.asthma_wissen4.glm, all_wissen4.cat.glm, test = "Chisq")

#----
# Anova for WISSEN5
#----

anova(age.cat_wissen5.glm, all_wissen5.cat.glm, test = "Chisq")
anova(sex_wissen5.glm, all_wissen5.cat.glm, test = "Chisq")
anova(smoking.status_wissen5.glm, all_wissen5.cat.glm, test = "Chisq")
anova(schulabschluss_wissen5.glm, all_wissen5.cat.glm, test = "Chisq")
anova(allergie3_bekommen_wissen5.glm, all_wissen5.cat.glm, test = "Chisq")
anova(allergie3_schlimm_wissen5.glm, all_wissen5.cat.glm, test = "Chisq")
anova(asthma.or.allergies_wissen5.glm, all_wissen5.cat.glm, test = "Chisq")
anova(par.asthma_wissen5.glm, all_wissen5.cat.glm, test = "Chisq")

AIC(all_wissen5.cat.glm)
AIC(age.cat_wissen5.glm)
AIC(sex_wissen5.glm)
AIC(smoking.status_wissen5.glm)
AIC(schulabschluss_wissen5.glm)
AIC(allergie3_bekommen_wissen5.glm)
AIC(allergie3_schlimm_wissen5.glm)
AIC(asthma.or.allergies_wissen5.glm)
AIC(par.asthma_wissen5.glm)

BIC(all_wissen5.cat.glm)
BIC(age.cat_wissen5.glm)
BIC(sex_wissen5.glm)
BIC(smoking.status_wissen5.glm)
BIC(schulabschluss_wissen5.glm)
BIC(allergie3_bekommen_wissen5.glm)
BIC(allergie3_schlimm_wissen5.glm)
BIC(asthma.or.allergies_wissen5.glm)
BIC(par.asthma_wissen5.glm)

