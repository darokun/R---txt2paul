
# age categorical
labels.alterkat2 <- c("younger", "older")
data$alterkat2 <- cut(data$alter, breaks = c(17,24,44), labels = labels.alterkat2)
# with wissen
table(data$all_wissen.factor,data$alterkat2)
prop.table(table(data$all_wissen.factor,data$alterkat2),1)*100
# with wissen4
table(data$all_wissen4.factor,data$alterkat2)
prop.table(table(data$all_wissen4.factor,data$alterkat2),1)*100
# with wissen5
prop.table(table(data$all_wissen5.factor,data$alterkat2),1)*100


# sex
# with wissen
table(data$all_wissen.factor,data$sex.factor)
prop.table(table(data$all_wissen.factor,data$sex.factor),1)*100
# with wissen4
table(data$all_wissen4.factor,data$sex.factor)
prop.table(table(data$all_wissen4.factor,data$sex.factor),1)*100
# with wissen5
table(data$all_wissen5.factor,data$sex.factor)
prop.table(table(data$all_wissen5.factor,data$sex.factor),1)*100

# smoking
# with wissen
table(data$all_wissen.factor,data$smoking.status.factor)
prop.table(table(data$all_wissen.factor,data$smoking.status.factor),1)*100
# with wissen4
table(data$all_wissen4.factor,data$smoking.status.factor)
prop.table(table(data$all_wissen4.factor,data$smoking.status.factor),1)*100
# with wissen5
table(data$all_wissen5.factor,data$smoking.status.factor)
prop.table(table(data$all_wissen5.factor,data$smoking.status.factor),1)*100

# educational level
# with wissen
table(data$all_wissen.factor,data$schulabschluss.factor.twolevels)
prop.table(table(data$all_wissen.factor,data$schulabschluss.factor.twolevels),1)*100
# with wissen4
table(data$all_wissen4.factor,data$schulabschluss.factor.twolevels)
prop.table(table(data$all_wissen4.factor,data$schulabschluss.factor.twolevels),1)*100
# with wissen5
table(data$all_wissen5.factor,data$schulabschluss.factor.twolevels)
prop.table(table(data$all_wissen5.factor,data$schulabschluss.factor.twolevels),1)*100

# allergie bekommen
# with wissen
table(data$all_wissen.factor,data$allergie3_bekommen.factor)
prop.table(table(data$all_wissen.factor,data$allergie3_bekommen.factor),1)*100
# with wissen4
table(data$all_wissen4.factor,data$allergie3_bekommen.factor)
prop.table(table(data$all_wissen4.factor,data$allergie3_bekommen.factor),1)*100
  # with wissen5
  table(data$all_wissen5.factor,data$allergie3_bekommen.factor)
  prop.table(table(data$all_wissen5.factor,data$allergie3_bekommen.factor),1)*100

# alergie_schlimm
# with wissen
table(data$all_wissen.factor,data$allergie3_schlimm.factor)
prop.table(table(data$all_wissen.factor,data$allergie3_schlimm.factor),1)*100
# with wissen4
table(data$all_wissen4.factor,data$allergie3_schlimm.factor)
prop.table(table(data$all_wissen4.factor,data$allergie3_schlimm.factor),1)*100
# with wissen5
table(data$all_wissen5.factor,data$allergie3_schlimm.factor)
prop.table(table(data$all_wissen5.factor,data$allergie3_schlimm.factor),1)*100

# asthma or rhinoconjunctivitis
# with wissen
table(data$all_wissen.factor,data$asthma.or.rhinoconj.factor)
prop.table(table(data$all_wissen.factor,data$asthma.or.rhinoconj.factor),1)*100
# with wissen4
table(data$all_wissen4.factor,data$asthma.or.rhinoconj.factor)
prop.table(table(data$all_wissen4.factor,data$asthma.or.rhinoconj.factor),1)*100
# with wissen5
table(data$all_wissen5.factor,data$asthma.or.rhinoconj.factor)
prop.table(table(data$all_wissen5.factor,data$asthma.or.rhinoconj.factor),1)*100

# parental ashtma
# with wissen
table(data$all_wissen.factor,data$par.asthma.factor)
prop.table(table(data$all_wissen.factor,data$par.asthma.factor),1)*100
# with wissen4
table(data$all_wissen4.factor,data$par.asthma.factor)
prop.table(table(data$all_wissen4.factor,data$par.asthma.factor),1)*100
# with wissen5
table(data$all_wissen5.factor,data$par.asthma.factor)
prop.table(table(data$all_wissen5.factor,data$par.asthma.factor),1)*100
 
#------------------------

# 2x2 table risk perception
#table(data$`, data$allergie3_bekommen.factor)
round(prop.table(table(data$allergie3_schlimm.factor, data$allergie3_bekommen.factor),1)*100,2)

cor.test(data$allergie3_schlimm, data$allergie3_bekommen)


table(data$allergie_schlimm, data$allergie_bekommen)



#------------------------
# count how many answered correctly

table(data$all_wissen.factor) # all 6
table(data$all_wissen5.factor) # at least 5
table(data$all_wissen4.factor) # at least 4
table(data$all_wissen3.factor) # at least 3
table(data$all_wissen2.factor) # at least 2
table(data$all_wissen1.factor) # at least 1
table(data$all_wissen0.factor) # none

#---------------------------
# main tables (again, now right) # deleted rows3....
# WISSEN 6
rows1.wissen6 <- c("age", "", "", "sex", "", "", "smok.status", "", "",
                   "ed.level", "", "", "risk.perc", "", "", "asthma.rhin.conj", 
                   "", "","parent.asthm", "", "")
length(rows9.wissen6)
rows2.wissen6 <- c("younger", "older", "", "female", "male", "", "non-smoker", "smoker", "",
                   "haupt.", "real + fachhoch + abitur", "", "high.perc", "low.perc", "",
                   "no", "yes", "", "no", "yes","")

rows4.wissen6 <- c(table(data$all_wissen.factor,data$alterkat2)[2],
                   table(data$all_wissen.factor,data$alterkat2)[4],
                   "",
                   
                   table(data$all_wissen.factor,data$sex.factor)[2],
                   table(data$all_wissen.factor,data$sex.factor)[4],
                   "",
                   
                   table(data$all_wissen.factor,data$smoking.status.factor)[2],
                   table(data$all_wissen.factor,data$smoking.status.factor)[4],
                   "",
                   
                   table(data$all_wissen.factor,data$schulabschluss.factor.twolevels)[2],
                   table(data$all_wissen.factor,data$schulabschluss.factor.twolevels)[4],
                   "",
                   
                   table(data$all_wissen.factor,data$risk_perception.factor)[2],
                   table(data$all_wissen.factor,data$risk_perception.factor)[4],
                   "",
                   
                   table(data$all_wissen.factor,data$asthma.or.rhinoconj.factor)[2],
                   table(data$all_wissen.factor,data$asthma.or.rhinoconj.factor)[4],
                   "",
                   
                   table(data$all_wissen.factor,data$par.asthma.factor)[2],
                   table(data$all_wissen.factor,data$par.asthma.factor)[4],
                   ""
)

rows5.wissen6 <- c(round(prop.table(table(data$all_wissen.factor,data$alterkat2),2)[2]*100,2),
                   round(prop.table(table(data$all_wissen.factor,data$alterkat2),2)[4]*100,2),
                   "",
                   
                   round(prop.table(table(data$all_wissen.factor,data$sex.factor),2)[2]*100,2),
                   round(prop.table(table(data$all_wissen.factor,data$sex.factor),2)[4]*100,2),
                   "",
                   
                   round(prop.table(table(data$all_wissen.factor,data$smoking.status.factor),2)[2]*100,2),
                   round(prop.table(table(data$all_wissen.factor,data$smoking.status.factor),2)[4]*100,2),
                   "",
                   
                   round(prop.table(table(data$all_wissen.factor,data$schulabschluss.factor.twolevels),2)[2]*100,2),
                   round(prop.table(table(data$all_wissen.factor,data$schulabschluss.factor.twolevels),2)[4]*100,2),
                   "",
                   
                   round(prop.table(table(data$all_wissen.factor,data$risk_perception.factor),2)[2]*100,2),
                   round(prop.table(table(data$all_wissen.factor,data$risk_perception.factor),2)[4]*100,2),
                   "",
                   
                   round(prop.table(table(data$all_wissen.factor,data$asthma.or.rhinoconj.factor),2)[2]*100,2),
                   round(prop.table(table(data$all_wissen.factor,data$asthma.or.rhinoconj.factor),2)[4]*100,2),
                   "",
                   
                   round(prop.table(table(data$all_wissen.factor,data$par.asthma.factor),2)[2]*100,2),
                   round(prop.table(table(data$all_wissen.factor,data$par.asthma.factor),2)[4]*100,2),
                   "")

rows6.wissen6 <- c("1",
                   round(exp(coef(age.cat_wissen.glm))[2],2),
                   "",
                   "1",
                   round(exp(coef(sex_wissen.glm))[2],2),
                   "",
                   "1",
                   round(exp(coef(smoking.status_wissen.glm))[2],2),
                   "",
                   "1",
                   round(exp(coef(schulabschluss_wissen.glm))[2],2),
                   "",
                   "1",
                   round(exp(coef(risk_perception_wissen.glm))[2],2),
                   "",
                   "1",
                   round(exp(coef(asthma.or.allergies_wissen.glm))[2],2),
                   "",
                   "1",
                   round(exp(coef(par.asthma_wissen.glm))[2],2),
                   "")

rows7.wissen6 <- c("1",
                   
                   round(exp(confint.default(age.cat_wissen.glm))[2],2), 
                   round(exp(confint.default(age.cat_wissen.glm))[4],2),
                   
                   "1",
                   
                   round(exp(confint.default(sex_wissen.glm))[2],2), 
                   round(exp(confint.default(sex_wissen.glm))[4],2),
                   
                   "1",
                   
                   round(exp(confint.default(smoking.status_wissen.glm))[2],2),  
                   round(exp(confint.default(smoking.status_wissen.glm))[4],2),
                   
                   "1",
                   
                   round(exp(confint.default(schulabschluss_wissen.glm))[2],2), 
                   round(exp(confint.default(schulabschluss_wissen.glm))[4],2),
                   
                   "1",
                   
                   round(exp(confint.default(risk_perception_wissen.glm))[2],2),
                   round(exp(confint.default(risk_perception_wissen.glm))[4],2),
                   
                   "1",
                   
                   round(exp(confint.default(asthma.or.allergies_wissen.glm))[2],2),
                   round(exp(confint.default(asthma.or.allergies_wissen.glm))[4],2),
                   
                   "1",
                   
                   round(exp(confint.default(par.asthma_wissen.glm))[2],2), 
                   round(exp(confint.default(par.asthma_wissen.glm))[4],2)
)

rows8.wissen6 <- c("1",
                   round(exp(coef(all_wissen.cat.glm))[2],2),
                   "",
                   "1",
                   round(exp(coef(all_wissen.cat.glm))[3],2),
                   "",
                   "1",
                   round(exp(coef(all_wissen.cat.glm))[4],2),
                   "",
                   "1",
                   round(exp(coef(all_wissen.cat.glm))[5],2),
                   "",
                   "1",
                   round(exp(coef(all_wissen.cat.glm))[6],2),
                   "",
                   "1",
                   round(exp(coef(all_wissen.cat.glm))[7],2),
                   "",
                   "1",
                   round(exp(coef(all_wissen.cat.glm))[8],2),
                   "")

rows9.wissen6 <- c(          
  "1",
  
  round(exp(confint.default(all_wissen.cat.glm))[2],2), 
  round(exp(confint.default(all_wissen.cat.glm))[10],2),
  
  "1",
  
  round(exp(confint.default(all_wissen.cat.glm))[3],2),  
  round(exp(confint.default(all_wissen.cat.glm))[11],2),
  
  "1",
  
  round(exp(confint.default(all_wissen.cat.glm))[4],2), 
  round(exp(confint.default(all_wissen.cat.glm))[12],2),
  
  "1",
  
  round(exp(confint.default(all_wissen.cat.glm))[5],2), 
  round(exp(confint.default(all_wissen.cat.glm))[13],2),
  
  "1",
  
  round(exp(confint.default(all_wissen.cat.glm))[6],2),
  round(exp(confint.default(all_wissen.cat.glm))[14],2),
  
  "1",
  
  round(exp(confint.default(all_wissen.cat.glm))[7],2),
  round(exp(confint.default(all_wissen.cat.glm))[15],2),
  
  "1",
  
  round(exp(confint.default(all_wissen.cat.glm))[8],2), 
  round(exp(confint.default(all_wissen.cat.glm))[16],2)
)

table_all_wissen6.1 <- cbind(rows4.wissen6, rows5.wissen6) #frequencies
table_all_wissen6.2 <- cbind(rows6.wissen6, rows7.wissen6) # crude ORs and CIs
table_all_wissen6.3 <- cbind(rows8.wissen6, rows9.wissen6) # adjusted ORs and CIs
table_all_wissen6 <- cbind(rows4.wissen6, rows5.wissen6, rows6.wissen6, rows7.wissen6, rows8.wissen6, rows9.wissen6)

#----
# WISSEN 5
rows1.wissen5 <- c("age", "", "", "sex", "", "", "smok.status", "", "",
                   "ed.level", "", "", "risk.perc", "", "", "asthma.rhin.conj", 
                   "", "","parent.asthm", "", "")

rows2.wissen5 <- c("younger", "older", "", "female", "male", "", "non-smoker", "smoker", "",
                   "haupt.", "real + fachhoch + abitur", "", "high.perc", "low.perc", "",
                   "no", "yes", "", "no", "yes","")

rows4.wissen5 <- c(table(data$all_wissen5.factor,data$alterkat2)[2],
                   table(data$all_wissen5.factor,data$alterkat2)[4],
                   "",
                   
                   table(data$all_wissen5.factor,data$sex.factor)[2],
                   table(data$all_wissen5.factor,data$sex.factor)[4],
                   "",
                   
                   table(data$all_wissen5.factor,data$smoking.status.factor)[2],
                   table(data$all_wissen5.factor,data$smoking.status.factor)[4],
                   "",
                   
                   table(data$all_wissen5.factor,data$schulabschluss.factor.twolevels)[2],
                   table(data$all_wissen5.factor,data$schulabschluss.factor.twolevels)[4],
                   "",
                   
                   table(data$all_wissen5.factor,data$risk_perception.factor)[2],
                   table(data$all_wissen5.factor,data$risk_perception.factor)[4],
                   "",
                   
                   table(data$all_wissen5.factor,data$asthma.or.rhinoconj.factor)[2],
                   table(data$all_wissen5.factor,data$asthma.or.rhinoconj.factor)[4],
                   "",
                   
                   table(data$all_wissen5.factor,data$par.asthma.factor)[2],
                   table(data$all_wissen5.factor,data$par.asthma.factor)[4],
                   ""
)

rows5.wissen5 <- c(round(prop.table(table(data$all_wissen5.factor,data$alterkat2),2)[2]*100,2),
                   round(prop.table(table(data$all_wissen5.factor,data$alterkat2),2)[4]*100,2),
                   "",
                   
                   round(prop.table(table(data$all_wissen5.factor,data$sex.factor),2)[2]*100,2),
                   round(prop.table(table(data$all_wissen5.factor,data$sex.factor),2)[4]*100,2),
                   "",
                   
                   round(prop.table(table(data$all_wissen5.factor,data$smoking.status.factor),2)[2]*100,2),
                   round(prop.table(table(data$all_wissen5.factor,data$smoking.status.factor),2)[4]*100,2),
                   "",
                   
                   round(prop.table(table(data$all_wissen5.factor,data$schulabschluss.factor.twolevels),2)[2]*100,2),
                   round(prop.table(table(data$all_wissen5.factor,data$schulabschluss.factor.twolevels),2)[4]*100,2),
                   "",
                   
                   round(prop.table(table(data$all_wissen5.factor,data$risk_perception.factor),2)[2]*100,2),
                   round(prop.table(table(data$all_wissen5.factor,data$risk_perception.factor),2)[4]*100,2),
                   "",
                   
                   round(prop.table(table(data$all_wissen5.factor,data$asthma.or.rhinoconj.factor),2)[2]*100,2),
                   round(prop.table(table(data$all_wissen5.factor,data$asthma.or.rhinoconj.factor),2)[4]*100,2),
                   "",
                   
                   round(prop.table(table(data$all_wissen5.factor,data$par.asthma.factor),2)[2]*100,2),
                   round(prop.table(table(data$all_wissen5.factor,data$par.asthma.factor),2)[4]*100,2),
                   "")

rows6.wissen5 <- c("1",
                   round(exp(coef(age.cat_wissen5.glm))[2],2),
                   "",
                   "1",
                   round(exp(coef(sex_wissen5.glm))[2],2),
                   "",
                   "1",
                   round(exp(coef(smoking.status_wissen5.glm))[2],2),
                   "",
                   "1",
                   round(exp(coef(schulabschluss_wissen5.glm))[2],2),
                   "",
                   "1",
                   round(exp(coef(risk_perception_wissen5.glm))[2],2),
                   "",
                   "1",
                   round(exp(coef(asthma.or.allergies_wissen5.glm))[2],2),
                   "",
                   "1",
                   round(exp(coef(par.asthma_wissen5.glm))[2],2),
                   "")

rows7.wissen5 <- c("1",
                   
                   round(exp(confint.default(age.cat_wissen5.glm))[2],2), 
                   round(exp(confint.default(age.cat_wissen5.glm))[4],2),
                   
                   "1",
                   
                   round(exp(confint.default(sex_wissen5.glm))[2],2), 
                   round(exp(confint.default(sex_wissen5.glm))[4],2),
                   
                   "1",
                   
                   round(exp(confint.default(smoking.status_wissen5.glm))[2],2),  
                   round(exp(confint.default(smoking.status_wissen5.glm))[4],2),
                   
                   "1",
                   
                   round(exp(confint.default(schulabschluss_wissen5.glm))[2],2), 
                   round(exp(confint.default(schulabschluss_wissen5.glm))[4],2),
                   
                   "1",
                   
                   round(exp(confint.default(risk_perception_wissen5.glm))[2],2),
                   round(exp(confint.default(risk_perception_wissen5.glm))[4],2),
                   
                   "1",
                   
                   round(exp(confint.default(asthma.or.allergies_wissen5.glm))[2],2),
                   round(exp(confint.default(asthma.or.allergies_wissen5.glm))[4],2),
                   
                   "1",
                   
                   round(exp(confint.default(par.asthma_wissen5.glm))[2],2), 
                   round(exp(confint.default(par.asthma_wissen5.glm))[4],2)
)

rows8.wissen5 <- c("1",
                   round(exp(coef(all_wissen5.cat.glm))[2],2),
                   "",
                   "1",
                   round(exp(coef(all_wissen5.cat.glm))[3],2),
                   "",
                   "1",
                   round(exp(coef(all_wissen5.cat.glm))[4],2),
                   "",
                   "1",
                   round(exp(coef(all_wissen5.cat.glm))[5],2),
                   "",
                   "1",
                   round(exp(coef(all_wissen5.cat.glm))[6],2),
                   "",
                   "1",
                   round(exp(coef(all_wissen5.cat.glm))[7],2),
                   "",
                   "1",
                   round(exp(coef(all_wissen5.cat.glm))[8],2),
                   "")

rows9.wissen5 <- c(          
  "1",
  
  round(exp(confint.default(all_wissen5.cat.glm))[2],2), 
  round(exp(confint.default(all_wissen5.cat.glm))[10],2),
  
  "1",
  
  round(exp(confint.default(all_wissen5.cat.glm))[3],2),  
  round(exp(confint.default(all_wissen5.cat.glm))[11],2),
  
  "1",
  
  round(exp(confint.default(all_wissen5.cat.glm))[4],2), 
  round(exp(confint.default(all_wissen5.cat.glm))[12],2),
  
  "1",
  
  round(exp(confint.default(all_wissen5.cat.glm))[5],2), 
  round(exp(confint.default(all_wissen5.cat.glm))[13],2),
  
  "1",
  
  round(exp(confint.default(all_wissen5.cat.glm))[6],2),
  round(exp(confint.default(all_wissen5.cat.glm))[14],2),
  
  "1",
  
  round(exp(confint.default(all_wissen5.cat.glm))[7],2),
  round(exp(confint.default(all_wissen5.cat.glm))[15],2),
  
  "1",
  
  round(exp(confint.default(all_wissen5.cat.glm))[8],2), 
  round(exp(confint.default(all_wissen5.cat.glm))[16],2)
)

table_all_wissen5.1 <- cbind(rows4.wissen5, rows5.wissen5) #frequencies
table_all_wissen5.2 <- cbind(rows6.wissen5, rows7.wissen5) # crude ORs and CIs
table_all_wissen5.3 <- cbind(rows8.wissen5, rows9.wissen5) # adjusted ORs and CIs
table_all_wissen5 <- cbind(rows4.wissen5, rows5.wissen5, rows6.wissen5, rows7.wissen5, rows8.wissen5, rows9.wissen5)

#----
# WISSEN 4
rows1.wissen4 <- c("sex", "", "", "smok.status", "", "",
                   "ed.level", "", "", "risk.perc", "", "", "asthma.rhin.conj", 
                   "", "","parent.asthm", "", "")

rows2.wissen4 <- c("female", "male", "", "non-smoker", "smoker", "",
                   "haupt.", "real + fachhoch + abitur", "", "high.perc", "low.perc", "",
                   "no", "yes", "", "no", "yes","")

rows4.wissen4 <- c(table(data$all_wissen4.factor,data$sex.factor)[2],
                   table(data$all_wissen4.factor,data$sex.factor)[4],
                   "",
                   
                   table(data$all_wissen4.factor,data$smoking.status.factor)[2],
                   table(data$all_wissen4.factor,data$smoking.status.factor)[4],
                   "",
                   
                   table(data$all_wissen4.factor,data$schulabschluss.factor.twolevels)[2],
                   table(data$all_wissen4.factor,data$schulabschluss.factor.twolevels)[4],
                   "",
                   
                   table(data$all_wissen4.factor,data$risk_perception.factor)[2],
                   table(data$all_wissen4.factor,data$risk_perception.factor)[4],
                   "",
                   
                   table(data$all_wissen4.factor,data$asthma.or.rhinoconj.factor)[2],
                   table(data$all_wissen4.factor,data$asthma.or.rhinoconj.factor)[4],
                   "",
                   
                   table(data$all_wissen4.factor,data$par.asthma.factor)[2],
                   table(data$all_wissen4.factor,data$par.asthma.factor)[4],
                   ""
)

rows5.wissen4 <- c(round(prop.table(table(data$all_wissen4.factor,data$sex.factor),2)[2]*100,2),
                   round(prop.table(table(data$all_wissen4.factor,data$sex.factor),2)[4]*100,2),
                   "",
                   
                   round(prop.table(table(data$all_wissen4.factor,data$smoking.status.factor),2)[2]*100,2),
                   round(prop.table(table(data$all_wissen4.factor,data$smoking.status.factor),2)[4]*100,2),
                   "",
                   
                   round(prop.table(table(data$all_wissen4.factor,data$schulabschluss.factor.twolevels),2)[2]*100,2),
                   round(prop.table(table(data$all_wissen4.factor,data$schulabschluss.factor.twolevels),2)[4]*100,2),
                   "",
                   
                   round(prop.table(table(data$all_wissen4.factor,data$risk_perception.factor),2)[2]*100,2),
                   round(prop.table(table(data$all_wissen4.factor,data$risk_perception.factor),2)[4]*100,2),
                   "",
                   
                   round(prop.table(table(data$all_wissen4.factor,data$asthma.or.rhinoconj.factor),2)[2]*100,2),
                   round(prop.table(table(data$all_wissen4.factor,data$asthma.or.rhinoconj.factor),2)[4]*100,2),
                   "",
                   
                   round(prop.table(table(data$all_wissen4.factor,data$par.asthma.factor),2)[2]*100,2),
                   round(prop.table(table(data$all_wissen4.factor,data$par.asthma.factor),2)[4]*100,2),
                   "")

rows6.wissen4 <- c("1",
                   round(exp(coef(sex_wissen4.glm))[2],2),
                   "",
                   "1",
                   round(exp(coef(smoking.status_wissen4.glm))[2],2),
                   "",
                   "1",
                   round(exp(coef(schulabschluss_wissen4.glm))[2],2),
                   "",
                   "1",
                   round(exp(coef(risk_perception_wissen4.glm))[2],2),
                   "",
                   "1",
                   round(exp(coef(asthma.or.allergies_wissen4.glm))[2],2),
                   "",
                   "1",
                   round(exp(coef(par.asthma_wissen4.glm))[2],2),
                   "")

rows7.wissen4 <- c("1",
                   
                   round(exp(confint.default(sex_wissen4.glm))[2],2), 
                   round(exp(confint.default(sex_wissen4.glm))[4],2),
                   
                   "1",
                   
                   round(exp(confint.default(smoking.status_wissen4.glm))[2],2),  
                   round(exp(confint.default(smoking.status_wissen4.glm))[4],2),
                   
                   "1",
                   
                   round(exp(confint.default(schulabschluss_wissen4.glm))[2],2), 
                   round(exp(confint.default(schulabschluss_wissen4.glm))[4],2),
                   
                   "1",
                   
                   round(exp(confint.default(risk_perception_wissen4.glm))[2],2),
                   round(exp(confint.default(risk_perception_wissen4.glm))[4],2),
                   
                   "1",
                   
                   round(exp(confint.default(asthma.or.allergies_wissen4.glm))[2],2),
                   round(exp(confint.default(asthma.or.allergies_wissen4.glm))[4],2),
                   
                   "1",
                   
                   round(exp(confint.default(par.asthma_wissen4.glm))[2],2), 
                   round(exp(confint.default(par.asthma_wissen4.glm))[4],2)
)

rows8.wissen4 <- c("1",
                   round(exp(coef(all_wissen4.cat.glm))[2],2),
                   "",
                   "1",
                   round(exp(coef(all_wissen4.cat.glm))[3],2),
                   "",
                   "1",
                   round(exp(coef(all_wissen4.cat.glm))[4],2),
                   "",
                   "1",
                   round(exp(coef(all_wissen4.cat.glm))[5],2),
                   "",
                   "1",
                   round(exp(coef(all_wissen4.cat.glm))[6],2),
                   "",
                   "1",
                   round(exp(coef(all_wissen4.cat.glm))[7],2),
                   "")

rows9.wissen4 <- c("1",
  
  round(exp(confint.default(all_wissen4.cat.glm))[2],2),  
  round(exp(confint.default(all_wissen4.cat.glm))[8],2),
  
  "1",
  
  round(exp(confint.default(all_wissen4.cat.glm))[3],2), 
  round(exp(confint.default(all_wissen4.cat.glm))[9],2),
  
  "1",
  
  round(exp(confint.default(all_wissen4.cat.glm))[4],2), 
  round(exp(confint.default(all_wissen4.cat.glm))[10],2),
  
  "1",
  
  round(exp(confint.default(all_wissen4.cat.glm))[5],2),
  round(exp(confint.default(all_wissen4.cat.glm))[11],2),
  
  "1",
  
  round(exp(confint.default(all_wissen4.cat.glm))[6],2),
  round(exp(confint.default(all_wissen4.cat.glm))[12],2),
  
  "1",
  
  round(exp(confint.default(all_wissen4.cat.glm))[7],2), 
  round(exp(confint.default(all_wissen4.cat.glm))[13],2)
)

table_all_wissen4.1 <- cbind(rows4.wissen4, rows5.wissen4) #frequencies
table_all_wissen4.2 <- cbind(rows6.wissen4, rows7.wissen4) # crude ORs and CIs
table_all_wissen4.3 <- cbind(rows8.wissen4, rows9.wissen4) # adjusted ORs and CIs
table_all_wissen4 <- cbind(rows4.wissen4, rows5.wissen4, rows6.wissen4, rows7.wissen4, rows8.wissen4, rows9.wissen4)
