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
set.seed(1986)
a <- 0
att6.0 <- tmle.cte(Y=Y,A=A,B=W, Delta=Delta, a=a, family= "binomial",
                 Q.method = "SL", g.method = "SL", gDelta.method = "SL",
                 g.SL.library = SL.library,
                 gDelta.SL.library = SL.library,
                 Q.SL.library = SL.library)

set.seed(1986)
a <- 1
att6.1 <- tmle.cte(Y=Y,A=A,B=W, Delta=Delta, a=a, family= "binomial",
                 Q.method = "SL", g.method = "SL", gDelta.method = "SL",
                 g.SL.library = SL.library,
                 gDelta.SL.library = SL.library,
                 Q.SL.library = SL.library)
att6.0
att6.1

#---
# wissen5
# outcome
Y <- int.data.wo.nas$all_wissen_nachher5

# att (non-treated)
set.seed(1986)
a <- 0
att5.0 <- tmle.cte(Y=Y,A=A,B=W, Delta=Delta, a=a, family= "binomial",
                   Q.method = "SL", g.method = "SL", gDelta.method = "SL",
                   g.SL.library = SL.library,
                   gDelta.SL.library = SL.library,
                   Q.SL.library = SL.library)

set.seed(1986)
a <- 1
att5.1 <- tmle.cte(Y=Y,A=A,B=W, Delta=Delta, a=a, family= "binomial",
                   Q.method = "SL", g.method = "SL", gDelta.method = "SL",
                   g.SL.library = SL.library,
                   gDelta.SL.library = SL.library,
                   Q.SL.library = SL.library)
att5.0
att5.1

#---
# wissen4
# outcome
Y <- int.data.wo.nas$all_wissen_nachher5

# att (non-treated)
set.seed(1986)
a <- 0
att4.0 <- tmle.cte(Y=Y,A=A,B=W, Delta=Delta, a=a, family= "binomial",
                   Q.method = "SL", g.method = "SL", gDelta.method = "SL",
                   g.SL.library = SL.library,
                   gDelta.SL.library = SL.library,
                   Q.SL.library = SL.library)

set.seed(1986)
a <- 1
att4.1 <- tmle.cte(Y=Y,A=A,B=W, Delta=Delta, a=a, family= "binomial",
                   Q.method = "SL", g.method = "SL", gDelta.method = "SL",
                   g.SL.library = SL.library,
                   gDelta.SL.library = SL.library,
                   Q.SL.library = SL.library)
att4.0
att4.1

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