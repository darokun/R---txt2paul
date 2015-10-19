# haare adj
haare.glm1 <- glm(haare_nachher.factor ~ gruppe.factor + haare.factor, data = data49, family = binomial)

summary(haare.glm1)
round(exp(coef(haare.glm1)),2)
round(exp(confint.default(haare.glm1)),2)

# haare not adj
haare.glm2 <- glm(haare_nachher.factor ~ gruppe.factor, data = data49, family = binomial)

summary(haare.glm2)
round(exp(coef(haare.glm2)),2)
round(exp(confint.default(haare.glm2)),2)

# arbeitsschuhe adj
arbeitsschuhe.glm1 <- glm(arbeitsschuhe_nachher.factor ~ gruppe.factor + arbeitsschuhe.factor, data = data49, family = binomial)

summary(arbeitsschuhe.glm1)
round(exp(coef(arbeitsschuhe.glm1)),2)
round(exp(confint.default(arbeitsschuhe.glm1)),2)

# arbeitsschuhe not adj
arbeitsschuhe.glm2 <- glm(arbeitsschuhe_nachher.factor ~ gruppe.factor, data = data49, family = binomial)

summary(arbeitsschuhe.glm2)
round(exp(coef(arbeitsschuhe.glm2)),2)
round(exp(confint.default(arbeitsschuhe.glm2)),2)

# kleidung_aufb adj
kleidung_aufb.glm1 <- glm(kleidung_aufb_nachher.factor ~ gruppe.factor + kleidung_aufb.factor, data = data49, family = binomial)

summary(kleidung_aufb.glm1)
round(exp(coef(kleidung_aufb.glm1)),2)
round(exp(confint.default(kleidung_aufb.glm1)),2)

# kleidung_aufb not adj
kleidung_aufb.glm2 <- glm(kleidung_aufb_nachher.factor ~ gruppe.factor, data = data49, family = binomial)

summary(kleidung_aufb.glm2)
round(exp(coef(kleidung_aufb.glm2)),2)
round(exp(confint.default(kleidung_aufb.glm2)),2)

# desinfizieren adj
desinfizieren.glm1 <- glm(desinfizieren_nachher.factor ~ gruppe.factor + desinfizieren.factor, data = data49, family = binomial)

summary(desinfizieren.glm1)
round(exp(coef(desinfizieren.glm1)),2)
round(exp(confint.default(desinfizieren.glm1)),2)

# desinfizieren not adj
desinfizieren.glm2 <- glm(desinfizieren_nachher.factor ~ gruppe.factor, data = data49, family = binomial)

summary(desinfizieren.glm2)
round(exp(coef(desinfizieren.glm2)),2)
round(exp(confint.default(desinfizieren.glm2)),2)

# kleidung_wohnraum adj
kleidung_wohnraum.glm1 <- glm(kleidung_wohnraum_nachher.factor ~ gruppe.factor + kleidung_wohnraum.factor, data = data49, family = binomial)

summary(kleidung_wohnraum.glm1)
round(exp(coef(kleidung_wohnraum.glm1)),2)
round(exp(confint.default(kleidung_wohnraum.glm1)),2)

# kleidung_wohnraum not adj
kleidung_wohnraum.glm2 <- glm(kleidung_wohnraum_nachher.factor ~ gruppe.factor, data = data49, family = binomial)

summary(kleidung_wohnraum.glm2)
round(exp(coef(kleidung_wohnraum.glm2)),2)
round(exp(confint.default(kleidung_wohnraum.glm2)),2)

# schutzbrille adj
schutzbrille.glm1 <- glm(schutzbrille_nachher.factor ~ gruppe.factor + schutzbrille.factor, data = data49, family = binomial)

summary(schutzbrille.glm1)
round(exp(coef(schutzbrille.glm1)),2)
round(exp(confint.default(schutzbrille.glm1)),2)

# schutzbrille not adj
schutzbrille.glm2 <- glm(schutzbrille_nachher.factor ~ gruppe.factor, data = data49, family = binomial)

summary(schutzbrille.glm2)
round(exp(coef(schutzbrille.glm2)),2)
round(exp(confint.default(schutzbrille.glm2)),2)
  
