# 1. Test with my data: 18.05.2016
# housekeeping

# age
int.data.wo.nas$alterkat2.binary <- NULL
int.data.wo.nas$alterkat2.binary <- as.numeric(int.data.wo.nas$alterkat2)
int.data.wo.nas$alterkat2.binary[int.data.wo.nas$alterkat2.binary==1] <- 0
int.data.wo.nas$alterkat2.binary[int.data.wo.nas$alterkat2.binary==2] <- 1

# asthma or rhinoconjunctivitis

int.data.wo.nas$asthma.or.rhinoconj <- as.numeric(int.data.wo.nas$asthma.or.rhinoconj.factor)
int.data.wo.nas$asthma.or.rhinoconj[int.data.wo.nas$asthma.or.rhinoconj==1] <- 0
int.data.wo.nas$asthma.or.rhinoconj[int.data.wo.nas$asthma.or.rhinoconj==2] <- 1

# Schulabschluss
int.data.wo.nas$schulabschluss.binary <- as.numeric(int.data.wo.nas$schulabschluss.factor.twolevels)
int.data.wo.nas$schulabschluss.binary[int.data.wo.nas$schulabschluss.binary==1] <- 0
int.data.wo.nas$schulabschluss.binary[int.data.wo.nas$schulabschluss.binary==2] <- 1

# knowledge at baseline
int.data.wo.nas$all_wissen.binary <- as.numeric(int.data.wo.nas$all_wissen.factor)
int.data.wo.nas$all_wissen.binary[int.data.wo.nas$all_wissen.binary==1] <- 0
int.data.wo.nas$all_wissen.binary[int.data.wo.nas$all_wissen.binary==2] <- 1

# vector of missings: 1 == observed (not missing), 0 == not observed (missing).
table(int.data.wo.nas$all_wissen_nachher, useNA="always")
69/116 #59.48% de missings


na.vector.all_wissen_nachher <- NULL
for(i in 1:length(int.data.wo.nas$all_wissen_nachher)) {
  if(is.na(int.data.wo.nas$all_wissen_nachher)[i]) {
    na.vector.all_wissen_nachher[i] <- 0
  }
  else {na.vector.all_wissen_nachher[i] <- 1}
}
# cbind(int.data.wo.nas$all_wissen_nachher, na.vector.all_wissen_nachher)



# Installing and loading packages 
# install.packages("SuperLearner")
# install.packages("tmle")
library(SuperLearner)
library(tmle)

# my variables
A <- as.numeric(int.data.wo.nas$gruppe) # treatment
Delta <- na.vector.all_wissen_nachher # missings
ff <- model.matrix( ~ int.data.wo.nas$sex + int.data.wo.nas$alterkat2.binary + int.data.wo.nas$schulabschluss.binary + int.data.wo.nas$smoking.status + int.data.wo.nas$asthma.or.rhinoconj + int.data.wo.nas$risk_perception + int.data.wo.nas$par.asthma + int.data.wo.nas$all_wissen)
W <- ff[,-1]
colnames(W) <- paste("W",1:8, sep="") # covariates
head(W)

# WISSEN6
Y <- int.data.wo.nas$all_wissen_nachher

create.SL.knn <- function(k = c(20, 30)) {
  for(mm in seq(length(k))){
    eval(parse(text = paste('SL.knn.', k[mm], '<- function(..., k = ', k[mm],
                            ') SL.knn(..., k = k)', sep = '')), envir = .GlobalEnv)
  }
  invisible(TRUE)
}
create.SL.knn(c(20, 30, 40, 50, 60, 70))
SL.library <- c("SL.glmnet", "SL.knn.20", "SL.knn.40", "SL.knn.40", "SL.knn.60", 
                "SL.mean", "SL.bayesglm", "SL.glm",
                "SL.step.interaction", "SL.randomForest")

set.seed(1985)
result6 <- tmle(Y=Y,A=A,W=W, Delta=Delta, family= "binomial", cvQinit = FALSE,
                g.SL.library = SL.library, 
                Q.SL.library = SL.library, 
                gbound = c(0.025, 0.995))
summary(result6)

# testing without covariates (from script 20.)
# detach("package:SuperLearner", unload=TRUE)
# A <- int.data.wo.nas$gruppe # treatment
# Delta <- na.vector.all_wissen_nachher # missings
# W <- matrix(data = 0, nrow = 116, ncol = 8, byrow = FALSE,
#             dimnames = NULL)
# colnames(W) <- paste("W",1:8, sep="")
# head(W)
# Y <- int.data.wo.nas$all_wissen_nachher
# result6 <- tmle(Y=Y,A=A,W=W, Delta=Delta, family= "binomial", 
#                 g.SL.library = c("SL.glm", "SL.step", "SL.glm.interaction"), 
#                 Q.SL.library = c("SL.glm", "SL.step", "SL.glm.interaction")) 
# summary(result6)


# WISSEN5
Y <- int.data.wo.nas$all_wissen_nachher5
set.seed(1985)
result5 <- tmle(Y=Y,A=A,W=W, Delta=Delta, family= "binomial", cvQinit = FALSE,
                g.SL.library = SL.library, 
                Q.SL.library = SL.library, 
                gbound = c(0.025, 0.995))
summary(result5)

# WISSEN4
Y <- int.data.wo.nas$all_wissen_nachher4
set.seed(1985)
result4 <- tmle(Y=Y,A=A,W=W, Delta=Delta, family= "binomial", cvQinit = FALSE,
                g.SL.library = SL.library, 
                Q.SL.library = SL.library, 
                gbound = c(0.025, 0.995))
summary(result4)

#---------------#
# END OF SCRIPT #
#---------------#