# install.packages("devtools") #if you don't have it already
# install.packages("httr")
library(devtools)
library(httr)
install_github("lendle/tmlecte")
library(tmlecte)

# wissen6
# outcome
Y <- int.data.wo.nas$all_wissen_nachher

# att (non-treated)
set.seed(1985)
a <- 0
att6 <- tmle.cte(Y=Y,A=A,B=W, Delta=Delta, a=a, family= "binomial",
                 Q.method = "SL", g.method = "SL", gDelta.method = "SL",
                 g.SL.library = SL.library,
                 gDelta.SL.library = SL.library,
                 Q.SL.library = SL.library)

set.seed(1985)
a <- 1
atc6 <- tmle.cte(Y=Y,A=A,B=W, Delta=Delta, a=a, family= "binomial",
                 Q.method = "SL", g.method = "SL", gDelta.method = "SL",
                 g.SL.library = SL.library,
                 gDelta.SL.library = SL.library,
                 Q.SL.library = SL.library)
att6
atc6

#---
# wissen5
# outcome
Y <- int.data.wo.nas$all_wissen_nachher5

# att (non-treated)
set.seed(1986)
a <- 0
att5 <- tmle.cte(Y=Y,A=A,B=W, Delta=Delta, a=a, family= "binomial",
                   Q.method = "SL", g.method = "SL", gDelta.method = "SL",
                   g.SL.library = SL.library,
                   gDelta.SL.library = SL.library,
                   Q.SL.library = SL.library)

set.seed(1986)
a <- 1
atc5 <- tmle.cte(Y=Y,A=A,B=W, Delta=Delta, a=a, family= "binomial",
                   Q.method = "SL", g.method = "SL", gDelta.method = "SL",
                   g.SL.library = SL.library,
                   gDelta.SL.library = SL.library,
                   Q.SL.library = SL.library)
att5
atc5

#---
# wissen4
# outcome
Y <- int.data.wo.nas$all_wissen_nachher4

# att (non-treated)
set.seed(1986)
a <- 0
att4 <- tmle.cte(Y=Y,A=A,B=W, Delta=Delta, a=a, family= "binomial",
                   Q.method = "SL", g.method = "SL", gDelta.method = "SL",
                   g.SL.library = SL.library,
                   gDelta.SL.library = SL.library,
                   Q.SL.library = SL.library)

set.seed(1986)
a <- 1
atc4 <- tmle.cte(Y=Y,A=A,B=W, Delta=Delta, a=a, family= "binomial",
                   Q.method = "SL", g.method = "SL", gDelta.method = "SL",
                   g.SL.library = SL.library,
                   gDelta.SL.library = SL.library,
                   Q.SL.library = SL.library)
att4
atc4

#---
# testing without covariates (refer to script 17.)
# Y <- int.data.wo.nas$all_wissen_nachher
# a <- 0
# att6.0 <- tmle.cte(Y=Y,A=A,B=B, Delta=Delta, a=a, family= "binomial",
#                  Q.method = "SL", g.method = "SL", gDelta.method = "SL",
#                  g.SL.library = SL.library,
#                  gDelta.SL.library = SL.library,
#                  Q.SL.library = SL.library))
# 
# a <- 1
# att6 <- tmle.cte(Y=Y,A=A,B=B, Delta=Delta, a=a, family= "binomial",
#                  Q.method = "SL", g.method = "SL", gDelta.method = "SL",
#                  g.SL.library = SL.library,
#                  gDelta.SL.library = SL.library,
#                  Q.SL.library = SL.library))
# att6.0
# att6.1



#---------------#
# END OF SCRIPT #
#---------------#