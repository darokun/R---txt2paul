# 1 #
# 1a #
arbeitskleidung3_letzteWoche.glm1 <- glm(arbeitskleidung3_letzteWoche_nachher.factor ~ gruppe.factor, data = data49, family = binomial)

summary(arbeitskleidung3_letzteWoche.glm1)
round(exp(coef(arbeitskleidung3_letzteWoche.glm1)),2)
round(exp(confint.default(arbeitskleidung3_letzteWoche.glm1)),2)

# 1b #
arbeitskleidung3_letzteWoche.glm2 <- glm(arbeitskleidung3_letzteWoche_nachher.factor ~ gruppe.factor + arbeitskleidung3_letzteWoche.factor, data = data49, family = binomial)

summary(arbeitskleidung3_letzteWoche.glm2)
round(exp(coef(arbeitskleidung3_letzteWoche.glm2)),2)
round(exp(confint.default(arbeitskleidung3_letzteWoche.glm2)),2)

# 1c #
arbeitskleidung3_letzteWoche.glm3 <- glm(arbeitskleidung3_letzteWoche_nachher.factor ~ arbeitskleidung3_letzteWoche.factor, data = data49, family = binomial)

summary(arbeitskleidung3_letzteWoche.glm3)
round(exp(coef(arbeitskleidung3_letzteWoche.glm3)),2)
round(exp(confint.default(arbeitskleidung3_letzteWoche.glm3)),2)

# 2 #
# 2a #
haare3_letzteWoche.glm1 <- glm(haare3_letzteWoche_nachher.factor ~ gruppe.factor, data = data49, family = binomial)

summary(haare3_letzteWoche.glm)
round(exp(coef(haare3_letzteWoche.glm)),2)
round(exp(confint.default(haare3_letzteWoche.glm)),2)

# 2b #
haare3_letzteWoche.glm2 <- glm(haare3_letzteWoche_nachher.factor ~ gruppe.factor + haare3_letzteWoche.factor, data = data49, family = binomial)
data49$haare3_letzteWoche.factor

summary(haare3_letzteWoche.glm2)
round(exp(coef(haare3_letzteWoche.glm2)),2)
round(exp(confint.default(haare3_letzteWoche.glm2)),2)

# 2c #
haare3_letzteWoche.glm3 <- glm(haare3_letzteWoche_nachher.factor ~ haare3_letzteWoche.factor, data = data49, family = binomial)

summary(haare3_letzteWoche.glm3)
round(exp(coef(haare3_letzteWoche.glm3)),2)
round(exp(confint.default(haare3_letzteWoche.glm3)),2)

# 3 #
# 3a #
aufbewahrung3_letzteWoche.glm1 <- glm(aufbewahrung3_letzteWoche_nachher.factor ~ gruppe.factor, data = data49, family = binomial)

summary(aufbewahrung3_letzteWoche.glm1)
exp(coef(aufbewahrung3_letzteWoche.glm1))
exp(confint.default(aufbewahrung3_letzteWoche.glm1))

# 3b #
aufbewahrung3_letzteWoche.glm2 <- glm(aufbewahrung3_letzteWoche_nachher.factor ~ gruppe.factor + aufbewahrung3_letzteWoche.factor, data = data49, family = binomial)

summary(aufbewahrung3_letzteWoche.glm2)
round(exp(coef(aufbewahrung3_letzteWoche.glm2)),2)
round(exp(confint.default(aufbewahrung3_letzteWoche.glm2)),2)

# 3c #
aufbewahrung3_letzteWoche.glm3 <- glm(aufbewahrung3_letzteWoche_nachher.factor ~ aufbewahrung3_letzteWoche.factor, data = data49, family = binomial)

summary(aufbewahrung3_letzteWoche.glm3)
round(exp(coef(aufbewahrung3_letzteWoche.glm3)),2)
round(exp(confint.default(aufbewahrung3_letzteWoche.glm3)),2)

# 4 #
# 4a #
arbeitskleidung3_intention.glm1 <- glm(arbeitskleidung3_intention_nachher.factor ~ gruppe.factor, data = data49, family = binomial)

summary(arbeitskleidung3_intention.glm1)
round(exp(coef(arbeitskleidung3_intention.glm1)),2)
round(exp(confint.default(arbeitskleidung3_intention.glm1)),2)

# 4b #
arbeitskleidung3_intention.glm2 <- glm(arbeitskleidung3_intention_nachher.factor ~ gruppe.factor + arbeitskleidung3_intention.factor, data = data49, family = binomial)
data49$aufbewahrung3_letzteWoche.factor

summary(arbeitskleidung3_intention.glm2)
round(exp(coef(arbeitskleidung3_intention.glm2)),2)
round(exp(confint.default(arbeitskleidung3_intention.glm2)),2)

# 4c #
arbeitskleidung3_intention.glm3 <- glm(arbeitskleidung3_intention_nachher.factor ~ arbeitskleidung3_intention.factor, data = data49, family = binomial)

summary(arbeitskleidung3_intention.glm3)
round(exp(coef(arbeitskleidung3_intention.glm3)),2)
round(exp(confint.default(arbeitskleidung3_intention.glm3)),2)

# 5 #
# 5a #
haare3_intention.glm1 <- glm(haare3_intention_nachher.factor ~ gruppe.factor, data = data49, family = binomial)

summary(haare3_intention.glm1)
round(exp(coef(haare3_intention.glm1)),2)
round(exp(confint.default(haare3_intention.glm1)),2)

# 5 #
# 5b #
haare3_intention.glm2 <- glm(haare3_intention_nachher.factor ~ gruppe.factor + haare3_intention.factor, data = data49, family = binomial)

summary(haare3_intention.glm2)
round(exp(coef(haare3_intention.glm2)),2)
round(exp(confint.default(haare3_intention.glm2)),2)

# 5 #
# 5c #
haare3_intention.glm3 <- glm(haare3_intention_nachher.factor ~ haare3_intention.factor, data = data49, family = binomial)

summary(haare3_intention.glm3)
round(exp(coef(haare3_intention.glm3)),2)
round(exp(confint.default(haare3_intention.glm3)),2)

# 6 #
# 6a #
aufbewahrung3_intention.glm1 <- glm(aufbewahrung3_intention_nachher.factor ~ gruppe.factor, data = data49, family = binomial)

summary(aufbewahrung3_intention.glm1)
round(exp(coef(aufbewahrung3_intention.glm1)),2)
round(exp(confint.default(aufbewahrung3_intention.glm1)),2)

# 6 #
# 6b #
aufbewahrung3_intention.glm2 <- glm(aufbewahrung3_intention_nachher.factor ~ gruppe.factor + aufbewahrung3_intention.factor, data = data49, family = binomial)

summary(aufbewahrung3_intention.glm2)
round(exp(coef(aufbewahrung3_intention.glm2)),2)
round(exp(confint.default(aufbewahrung3_intention.glm2)),2)

# 6 #
# 6c #
aufbewahrung3_intention.glm3 <- glm(aufbewahrung3_intention_nachher.factor ~ aufbewahrung3_intention.factor, data = data49, family = binomial)

summary(aufbewahrung3_intention.glm3)
round(exp(coef(aufbewahrung3_intention.glm3)),2)
round(exp(confint.default(aufbewahrung3_intention.glm3)),2)

# 7 #
# 7a #
arbeitskleidung3_einstellung.glm1 <- glm(arbeitskleidung3_einstellung_nachher.factor ~ gruppe.factor, data = data49, family = binomial)

summary(arbeitskleidung3_einstellung.glm1)
round(exp(coef(arbeitskleidung3_einstellung.glm1)),2)
round(exp(confint.default(arbeitskleidung3_einstellung.glm1)),2)

# 7b #
arbeitskleidung3_einstellung.glm2 <- glm(arbeitskleidung3_einstellung_nachher.factor ~ gruppe.factor + arbeitskleidung3_einstellung.factor, data = data49, family = binomial)

summary(arbeitskleidung3_einstellung.glm2)
round(exp(coef(arbeitskleidung3_einstellung.glm2)),2)
round(exp(confint.default(arbeitskleidung3_einstellung.glm2)),2)

# 7c #
arbeitskleidung3_einstellung.glm3 <- glm(arbeitskleidung3_einstellung_nachher.factor ~ arbeitskleidung3_einstellung.factor, data = data49, family = binomial)

summary(arbeitskleidung3_einstellung.glm3)
round(exp(coef(arbeitskleidung3_einstellung.glm3)),2)
round(exp(confint.default(arbeitskleidung3_einstellung.glm3)),2)

# 8 #
# 8a #
haare3_einstellung.glm1 <- glm(haare3_einstellung_nachher.factor ~ gruppe.factor, data = data49, family = binomial)

summary(haare3_einstellung.glm1)
round(exp(coef(haare3_einstellung.glm1)),2)
round(exp(confint.default(haare3_einstellung.glm1)),2)

# 8b #
haare3_einstellung.glm2 <- glm(haare3_einstellung_nachher.factor ~ gruppe.factor + haare3_einstellung.factor, data = data49, family = binomial)

summary(haare3_einstellung.glm2)
round(exp(coef(haare3_einstellung.glm2)),2)
round(exp(confint.default(haare3_einstellung.glm2)),2)

# 8c #
haare3_einstellung.glm3 <- glm(haare3_einstellung_nachher.factor ~ haare3_einstellung.factor, data = data49, family = binomial)

summary(haare3_einstellung.glm3)
round(exp(coef(haare3_einstellung.glm3)),2)
round(exp(confint.default(haare3_einstellung.glm3)),2)

# 9 #
# 9a #
aufbewahrung3_einstellung.glm1 <- glm(aufbewahrung3_einstellung_nachher.factor ~ gruppe.factor, data = data49, family = binomial)

summary(aufbewahrung3_einstellung.glm1)
round(exp(coef(aufbewahrung3_einstellung.glm1)),2)
round(exp(confint.default(aufbewahrung3_einstellung.glm1)),2)

# 9b #
aufbewahrung3_einstellung.glm2 <- glm(aufbewahrung3_einstellung_nachher.factor ~ gruppe.factor + aufbewahrung3_einstellung.factor, data = data49, family = binomial)

summary(aufbewahrung3_einstellung.glm2)
round(exp(coef(aufbewahrung3_einstellung.glm2)),2)
round(exp(confint.default(aufbewahrung3_einstellung.glm2)),2)

# 9c #
aufbewahrung3_einstellung.glm3 <- glm(aufbewahrung3_einstellung_nachher.factor ~ aufbewahrung3_einstellung.factor, data = data49, family = binomial)

summary(aufbewahrung3_einstellung.glm3)
round(exp(coef(aufbewahrung3_einstellung.glm3)),2)
round(exp(confint.default(aufbewahrung3_einstellung.glm3)),2)

# 10 #
# 10a #
arbeitskleidung3_norm.glm1 <- glm(arbeitskleidung3_norm_nachher.factor ~ gruppe.factor, data = data49, family = binomial)

summary(arbeitskleidung3_norm.glm1)
round(exp(coef(arbeitskleidung3_norm.glm1)),2)
round(exp(confint.default(arbeitskleidung3_norm.glm1)),2)

# 10b #
arbeitskleidung3_norm.glm2 <- glm(arbeitskleidung3_norm_nachher.factor ~ gruppe.factor + arbeitskleidung3_norm.factor, data = data49, family = binomial)

summary(arbeitskleidung3_norm.glm2)
round(exp(coef(arbeitskleidung3_norm.glm2)),2)
round(exp(confint.default(arbeitskleidung3_norm.glm2)),2)

# 10c #
arbeitskleidung3_norm.glm3 <- glm(arbeitskleidung3_norm_nachher.factor ~ arbeitskleidung3_norm.factor, data = data49, family = binomial)

summary(arbeitskleidung3_norm.glm3)
round(exp(coef(arbeitskleidung3_norm.glm3)),2)
round(exp(confint.default(arbeitskleidung3_norm.glm3)),2)

# 11 #
# 11a #
haare3_norm.glm1 <- glm(haare3_norm_nachher.factor ~ gruppe.factor, data = data49, family = binomial)

summary(haare3_norm.glm1)
round(exp(coef(haare3_norm.glm1)),2)
round(exp(confint.default(haare3_norm.glm1)),2)

# 11b #
haare3_norm.glm2 <- glm(haare3_norm_nachher.factor ~ gruppe.factor + haare3_norm.factor, data = data49, family = binomial)

summary(haare3_norm.glm2)
round(exp(coef(haare3_norm.glm2)),2)
round(exp(confint.default(haare3_norm.glm2)),2)

# 11c #
haare3_norm.glm3 <- glm(haare3_norm_nachher.factor ~ haare3_norm.factor, data = data49, family = binomial)

summary(haare3_norm.glm3)
round(exp(coef(haare3_norm.glm3)),2)
round(exp(confint.default(haare3_norm.glm3)),2)

# 12 #
# 12a #
aufbewahrung3_norm.glm1 <- glm(aufbewahrung3_norm_nachher.factor ~ gruppe.factor, data = data49, family = binomial)

summary(aufbewahrung3_norm.glm1)
round(exp(coef(aufbewahrung3_norm.glm1)),2)
round(exp(confint.default(aufbewahrung3_norm.glm1)),2)

# 12b #
aufbewahrung3_norm.glm2 <- glm(aufbewahrung3_norm_nachher.factor ~ gruppe.factor + aufbewahrung3_norm.factor, data = data49, family = binomial)

summary(aufbewahrung3_norm.glm2)
round(exp(coef(aufbewahrung3_norm.glm2)),2)
round(exp(confint.default(aufbewahrung3_norm.glm2)),2)

# 12c #
aufbewahrung3_norm.glm3 <- glm(aufbewahrung3_norm_nachher.factor ~ aufbewahrung3_norm.factor, data = data49, family = binomial)

summary(aufbewahrung3_norm.glm3)
round(exp(coef(aufbewahrung3_norm.glm3)),2)
round(exp(confint.default(aufbewahrung3_norm.glm3)),2)

# 13 #
# 13a #
arbeitskleidung3_kontrolle.glm1 <- glm(arbeitskleidung3_kontrolle_nachher.factor ~ gruppe.factor, data = data49, family = binomial)

summary(arbeitskleidung3_kontrolle.glm1)
round(exp(coef(arbeitskleidung3_kontrolle.glm1)),2)
round(exp(confint.default(arbeitskleidung3_kontrolle.glm1)),2)

# 13b #
arbeitskleidung3_kontrolle.glm2 <- glm(arbeitskleidung3_kontrolle_nachher.factor ~ gruppe.factor + arbeitskleidung3_kontrolle.factor, data = data49, family = binomial)

summary(arbeitskleidung3_kontrolle.glm2)
round(exp(coef(arbeitskleidung3_kontrolle.glm2)),2)
round(exp(confint.default(arbeitskleidung3_kontrolle.glm2)),2)

# 13c #
arbeitskleidung3_kontrolle.glm3 <- glm(arbeitskleidung3_kontrolle_nachher.factor ~ arbeitskleidung3_kontrolle.factor, data = data49, family = binomial)

summary(arbeitskleidung3_kontrolle.glm3)
round(exp(coef(arbeitskleidung3_kontrolle.glm3)),2)
round(exp(confint.default(arbeitskleidung3_kontrolle.glm3)),2)

# 14 #
# 14a #
haare3_kontrolle.glm1 <- glm(haare3_kontrolle_nachher.factor ~ gruppe.factor, data = data49, family = binomial)

summary(haare3_kontrolle.glm1)
round(exp(coef(haare3_kontrolle.glm1)),2)
round(exp(confint.default(haare3_kontrolle.glm1)),2)

# 14b #
haare3_kontrolle.glm2 <- glm(haare3_kontrolle_nachher.factor ~ gruppe.factor + haare3_kontrolle.factor, data = data49, family = binomial)

summary(haare3_kontrolle.glm2)
round(exp(coef(haare3_kontrolle.glm2)),2)
round(exp(confint.default(haare3_kontrolle.glm2)),2)

# 14c #
haare3_kontrolle.glm3 <- glm(haare3_kontrolle_nachher.factor ~ haare3_kontrolle.factor, data = data49, family = binomial)

summary(haare3_kontrolle.glm3)
round(exp(coef(haare3_kontrolle.glm3)),2)
round(exp(confint.default(haare3_kontrolle.glm3)),2)

# 15 #
# 15a #
aufbewahrung3_kontrolle.glm1 <- glm(aufbewahrung3_kontrolle_nachher.factor ~ gruppe.factor, data = data49, family = binomial)

summary(aufbewahrung3_kontrolle.glm1)
round(exp(coef(aufbewahrung3_kontrolle.glm1)),2)
round(exp(confint.default(aufbewahrung3_kontrolle.glm1)),2)

# 15b #
aufbewahrung3_kontrolle.glm2 <- glm(aufbewahrung3_kontrolle_nachher.factor ~ gruppe.factor + aufbewahrung3_kontrolle.factor, data = data49, family = binomial)

summary(aufbewahrung3_kontrolle.glm2)
round(exp(coef(aufbewahrung3_kontrolle.glm2)),2)
round(exp(confint.default(aufbewahrung3_kontrolle.glm2)),2)

# 15c #
aufbewahrung3_kontrolle.glm3 <- glm(aufbewahrung3_kontrolle_nachher.factor ~ aufbewahrung3_kontrolle.factor, data = data49, family = binomial)

summary(aufbewahrung3_kontrolle.glm3)
round(exp(coef(aufbewahrung3_kontrolle.glm3)),2)
round(exp(confint.default(aufbewahrung3_kontrolle.glm3)),2)

