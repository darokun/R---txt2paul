#' ---
#' title: "txt2paul logistic models on knowledge variables"
#' author: "Daloha Rodriguez-Molina"
#' date: "April 24th, 2015"
#' ---

#-------------------#
# Contents:
# I.   Preliminary stuff (installing packages, creating data, etc.) 
#      Lines 15 - 32
# II.  Logistic models using variables gruppe, haare_nachher and haare
#      Lines 34 - 64
# #-------------------#

#----------------------#
# I. Preliminary stuff #
#----------------------#
#----
# Install packages
# install.packages("arm")
library(arm)
detach("package:psych", unload=T)

# Create data: (exact replication of what's in the big dataset)
# gruppe <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 0, 0)

# haare <- c(0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1)

# haare_nachher <- c(1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

# data.ex <- data.frame(gruppe,haare,haare_nachher)

data49$gruppe.factor <- factor(data49$gruppe)
data49$haare.factor <- factor(data49$haare)
data49$arbeitsschuhe.factor <- factor(data49$arbeitsschuhe)
data49$kleidung_aufb.factor <- factor(data49$kleidung_aufb)
data49$desinfizieren.factor <- factor(data49$desinfizieren)
data49$schutzbrille.factor <- factor(data49$schutzbrille)
data49$kleidung_wohnraum.factor <- factor(data49$kleidung_wohnraum)
data49$haare_nachher.factor <- factor(data49$haare_nachher)
data49$arbeitsschuhe_nachher.factor <- factor(data49$arbeitsschuhe_nachher)
data49$kleidung_aufb_nachher.factor <- factor(data49$kleidung_aufb_nachher)
data49$desinfizieren_nachher.factor <- factor(data49$desinfizieren_nachher)
data49$schutzbrille_nachher.factor <- factor(data49$schutzbrille_nachher)
data49$kleidung_wohnraum_nachher.factor <- factor(data49$kleidung_wohnraum_nachher)

#----

#----------------#
# II. Log models #
#----------------#
#----

# intended model:
# knowing the right preventive measure at follow up (haare_nachher: no==0, yes==1) =
# whether or not the subject is in the intervention group (gruppe: control==0, intervention==1) +
# knowing the right preventive measure at baseline (haare: no==0, yes==1)

# with glm
haare.glm <- glm(kleidung_wohnraum_nachher.factor ~ gruppe.factor + kleidung_wohnraum.factor, data = data49, family = binomial)
# Warning message:
# glm.fit: fitted probabilities numerically 0 or 1 occurred 

# then, this doesn't make sense:
summary(haare.glm)
exp(coef(haare.glm))
exp(confint.default(haare.glm))

# I looked into this webpage http://stats.stackexchange.com/questions/11109/how-to-deal-with-perfect-separation-in-logistic-regression
# where someone suggests to use a Bayesian approach with non-informative prior assumptions:
haare.glm.bayes.yo <- bayesglm(haare_nachher ~ gruppe + haare, data = data.ex, family = binomial)
summary(haare.glm.bayes.yo)
exp(coef(haare.glm.bayes.yo))
exp(confint.default(haare.glm.bayes.yo))

# The Bayesian model seems to make a little bit more sense, but I am not entirely sure if this approach should be the best one in this case. I chose it because it seemed to be simpler.
# what do you suggest?

# Ronald's suggestion:
haare.glm.bayes.ron <- bayesglm(haare_nachher ~ gruppe + haare, data = data.ex, family = binomial, 
                            prior.scale=2.5, prior.df=3)

summary(haare.glm.bayes.ron)
cosa    <- sim(haare.glm.bayes.ron, n.sims=100)
cosaExp <- exp(cosa@"coef")
apply(cosaExp, 2, function(x) quantile(x, prob= c(0.025, 0.975))) # intervalos de credibilidad

# con haare en offset
haare.glm.bayes2 <- bayesglm(haare_nachher ~ gruppe + offset(haare), data = data.ex, family = binomial, 
                             prior.scale=2.5, prior.df=3)
summary(haare.glm.bayes2)
cosa2   <- sim(haare.glm.bayes2, n.sims=100)
cosaExp2 <- exp(cosa2@"coef")
apply(cosaExp2, 2, function(x) quantile(x, prob= c(0.025, 0.975))) # intervalos de credibilidad

#----
# Actual models:
set.seed(123456)
#haare:
haare.glm.bayes <- bayesglm(haare_nachher ~ gruppe + offset(haare), data = data49, family = binomial, prior.scale=2.5, prior.df=3)

summary(haare.glm.bayes)
simul.haare <- sim(haare.glm.bayes, n.sims=100)
simulExp.haare <- exp(simul.haare@"coef")
credint.haare <- apply(simulExp.haare, 2, function(x) quantile(x, prob= c(0.025, 0.975))) # intervalos de credibilidad

exp(haare.glm.bayes$"coef")
round(exp(credint.haare),2)

#arbeitschuhe:
arbeitschuhe.glm.bayes <- bayesglm(arbeitsschuhe_nachher ~ gruppe + offset(arbeitsschuhe), data = data49, family = binomial, 
                            prior.scale=2.5, prior.df=3)

summary(arbeitschuhe.glm.bayes)
simul.arbeitschuhe <- sim(arbeitschuhe.glm.bayes, n.sims=100)
simulExp.arbeitschuhe <- exp(simul.arbeitschuhe@"coef")
credint.arbeitschuhe <- apply(simulExp.arbeitschuhe, 2, function(x) quantile(x, prob= c(0.025, 0.975))) # intervalos de credibilidad

exp(arbeitschuhe.glm.bayes$"coef")
exp(credint.arbeitschuhe)

#kleidung_aufb:
kleidung_aufb.glm.bayes <- bayesglm(kleidung_aufb_nachher ~ gruppe + offset(kleidung_aufb), data = data49, family = binomial, 
                            prior.scale=2.5, prior.df=3)

summary(kleidung_aufb.glm.bayes)
simul.kleidung_aufb <- sim(kleidung_aufb.glm.bayes, n.sims=100)
simulExp.kleidung_aufb <- exp(simul.kleidung_aufb@"coef")
credint.kleidung_aufb <- apply(simulExp.kleidung_aufb, 2, function(x) quantile(x, prob= c(0.025, 0.975))) # intervalos de credibilidad

exp(kleidung_aufb.glm.bayes$"coef")
exp(credint.kleidung_aufb)

#desinfizieren:
desinfizieren.glm.bayes <- bayesglm(desinfizieren_nachher ~ gruppe + offset(desinfizieren), data = data49, family = binomial, 
                            prior.scale=2.5, prior.df=3)

summary(desinfizieren.glm.bayes)
simul.desinfizieren <- sim(desinfizieren.glm.bayes, n.sims=100)
simulExp.desinfizieren <- exp(simul.desinfizieren@"coef")
credint.desinfizieren <- apply(simulExp.desinfizieren, 2, function(x) quantile(x, prob= c(0.025, 0.975))) # intervalos de credibilidad

exp(desinfizieren.glm.bayes$"coef")
exp(credint.desinfizieren)

#schutzbrille:
schutzbrille.glm.bayes <- bayesglm(schutzbrille_nachher ~ gruppe + offset(schutzbrille), data = data49, family = binomial, 
                            prior.scale=2.5, prior.df=3)

summary(schutzbrille.glm.bayes)
simul.schutzbrille <- sim(schutzbrille.glm.bayes, n.sims=100)
simulExp.schutzbrille <- exp(simul.schutzbrille@"coef")
credint.schutzbrille <- apply(simulExp.schutzbrille, 2, function(x) quantile(x, prob= c(0.025, 0.975))) # intervalos de credibilidad

exp(schutzbrille.glm.bayes$"coef")
exp(credint.schutzbrille)

#kleidung_wohnraum:
kleidung_wohnraum.glm.bayes <- bayesglm(kleidung_wohnraum_nachher ~ gruppe + offset(kleidung_wohnraum), data = data49, family = binomial, 
                            prior.scale=2.5, prior.df=3)

summary(kleidung_wohnraum.glm.bayes)
simul.kleidung_wohnraum <- sim(kleidung_wohnraum.glm.bayes, n.sims=100)
simulExp.kleidung_wohnraum <- exp(simul.kleidung_wohnraum@"coef")
credint.kleidung_wohnraum <- apply(simulExp.kleidung_wohnraum, 2, function(x) quantile(x, prob= c(0.025, 0.975))) # intervalos de credibilidad

exp(kleidung_wohnraum.glm.bayes$"coef")
exp(credint.kleidung_wohnraum)


#----
# Actual models: NOT ON OFFSET
set.seed(123456)
#haare:
haare.glm.bayes <- bayesglm(haare_nachher ~ gruppe + haare, data = data49, family = binomial, prior.scale=2.5, prior.df=3)

summary(haare.glm.bayes)
simul.haare <- sim(haare.glm.bayes, n.sims=100)
simulExp.haare <- exp(simul.haare@"coef")
credint.haare <- apply(simulExp.haare, 2, function(x) quantile(x, prob= c(0.025, 0.975))) # intervalos de credibilidad

exp(haare.glm.bayes$"coef")
round(exp(credint.haare),2)

#arbeitschuhe:
arbeitschuhe.glm.bayes <- bayesglm(arbeitsschuhe_nachher ~ gruppe + arbeitsschuhe, data = data49, family = binomial, 
                                   prior.scale=2.5, prior.df=3)

summary(arbeitschuhe.glm.bayes)
simul.arbeitschuhe <- sim(arbeitschuhe.glm.bayes, n.sims=100)
simulExp.arbeitschuhe <- exp(simul.arbeitschuhe@"coef")
credint.arbeitschuhe <- apply(simulExp.arbeitschuhe, 2, function(x) quantile(x, prob= c(0.025, 0.975))) # intervalos de credibilidad

exp(arbeitschuhe.glm.bayes$"coef")
exp(credint.arbeitschuhe)

#kleidung_aufb:
kleidung_aufb.glm.bayes <- bayesglm(kleidung_aufb_nachher ~ gruppe + kleidung_aufb, data = data49, family = binomial, 
                                    prior.scale=2.5, prior.df=3)

summary(kleidung_aufb.glm.bayes)
simul.kleidung_aufb <- sim(kleidung_aufb.glm.bayes, n.sims=100)
simulExp.kleidung_aufb <- exp(simul.kleidung_aufb@"coef")
credint.kleidung_aufb <- apply(simulExp.kleidung_aufb, 2, function(x) quantile(x, prob= c(0.025, 0.975))) # intervalos de credibilidad

exp(kleidung_aufb.glm.bayes$"coef")
exp(credint.kleidung_aufb)

#desinfizieren:
desinfizieren.glm.bayes <- bayesglm(desinfizieren_nachher ~ gruppe + desinfizieren, data = data49, family = binomial, 
                                    prior.scale=2.5, prior.df=3)

summary(desinfizieren.glm.bayes)
simul.desinfizieren <- sim(desinfizieren.glm.bayes, n.sims=100)
simulExp.desinfizieren <- exp(simul.desinfizieren@"coef")
credint.desinfizieren <- apply(simulExp.desinfizieren, 2, function(x) quantile(x, prob= c(0.025, 0.975))) # intervalos de credibilidad

exp(desinfizieren.glm.bayes$"coef")
exp(credint.desinfizieren)

#schutzbrille:
schutzbrille.glm.bayes <- bayesglm(schutzbrille_nachher ~ gruppe + schutzbrille, data = data49, family = binomial, 
                                   prior.scale=2.5, prior.df=3)

summary(schutzbrille.glm.bayes)
simul.schutzbrille <- sim(schutzbrille.glm.bayes, n.sims=100)
simulExp.schutzbrille <- exp(simul.schutzbrille@"coef")
credint.schutzbrille <- apply(simulExp.schutzbrille, 2, function(x) quantile(x, prob= c(0.025, 0.975))) # intervalos de credibilidad

exp(schutzbrille.glm.bayes$"coef")
exp(credint.schutzbrille)

#kleidung_wohnraum:
kleidung_wohnraum.glm.bayes <- bayesglm(kleidung_wohnraum_nachher ~ gruppe + kleidung_wohnraum, data = data49, family = binomial, 
                                        prior.scale=2.5, prior.df=3)

summary(kleidung_wohnraum.glm.bayes)
simul.kleidung_wohnraum <- sim(kleidung_wohnraum.glm.bayes, n.sims=100)
simulExp.kleidung_wohnraum <- exp(simul.kleidung_wohnraum@"coef")
credint.kleidung_wohnraum <- apply(simulExp.kleidung_wohnraum, 2, function(x) quantile(x, prob= c(0.025, 0.975))) # intervalos de credibilidad
14*14
exp(kleidung_wohnraum.glm.bayes$"coef")
exp(credint.kleidung_wohnraum)
#---------------#
# END OF SCRIPT #
#---------------#



