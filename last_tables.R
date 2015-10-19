
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
