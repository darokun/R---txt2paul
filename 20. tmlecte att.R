# install.packages("devtools") #if you don't have it already
# install.packages("httr")
# install.packages("SuperLearner")
library(devtools)
library(httr)
install_github("lendle/tmlecte")
library(tmlecte)
library(SuperLearner)
# ?tmle.cte

# treatment
A <- int.data.wo.nas$gruppe 

# missings
Delta <- na.vector.all_wissen_nachher 

# covariates
B <- matrix(c(int.data.wo.nas$sex,
              int.data.wo.nas$alterkat2.binary,
              int.data.wo.nas$schulabschluss.binary,
              int.data.wo.nas$smoking.status,
              int.data.wo.nas$asthma.or.rhinoconj, 
              int.data.wo.nas$risk_perception,
              int.data.wo.nas$par.asthma,
              int.data.wo.nas$all_wissen),
            ncol=8)
colnames(B) <- paste("B",1:8, sep="") 
head(B)



# wissen6
# outcome
Y <- int.data.wo.nas$all_wissen_nachher

# att (non-treated)
a <- 1
att6 <- tmle.cte(Y=Y,A=A,B=B, Delta=Delta, a=a, family= "binomial",
                 Q.method = "SL", g.method = "SL", gDelta.method = "SL",
                 g.SL.library = c("SL.glm", "SL.step", "SL.glm.interaction"),
                 gDelta.SL.library = c("SL.glm", "SL.step", "SL.glm.interaction"),
                 Q.SL.library = c("SL.glm", "SL.step", "SL.glm.interaction"))
att6

att6 <- tmle.cte(Y=Y,A=A,B=B, Delta=Delta, a=a, family= "binomial",
                 Q.method = "SL", g.method = "SL", gDelta.method = "SL",
                 g.SL.library = c("SL.glm", "SL.step", "SL.glm.interaction"),
                 gDelta.SL.library = c("SL.glm", "SL.step", "SL.glm.interaction"),
                 Q.SL.library = c("SL.glm", "SL.step", "SL.glm.interaction"))



# att6 <- tmle.cte(Y=Y,A=A,B=B, Delta=Delta, a=a, family= "binomial",
#                  Q.method = "SL", g.method = "SL", gDelta.method = "SL",
#                  g.SL.library = c("SL.glm", "SL.step", "SL.glm.interaction"),
#                  gDelta.SL.library = c("SL.glm", "SL.step", "SL.glm.interaction"),
#                  Q.SL.library = c("SL.glm", "SL.step", "SL.glm.interaction"))



warnings()

