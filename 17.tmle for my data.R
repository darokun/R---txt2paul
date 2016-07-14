# #------------------------------------------------------------------------------------
# # I. Running the code
# #------------------------------------------------------------------------------------
# # Targeted Maximum Likelihood Estimation
# # for binary point treatment, non-parametric estimation
# # parameter of interest = E_W[E(Y|A=1,W) - E(Y|A=0,W)]
# # taking into account treatment (g_A) and missingness (g_M) mechanisms
# # models or estimates for Q, g_A, g_M can be user-supplied or estimated internally using DSA
# # as implemented, arguments to DSA are the same for all estimation procedures
# # these can be user-supplied or set to default values
# # maxorderint = 2, maxsumofpow=2, maxsize = 15
# # Dmove=TRUE, Smove=TRUE, formula = Y~A, A forced into model.
# #
# # August 16, 2009
# # Susan Gruber, sgruber@berkeley.edu
# #
# # for information see
# # M.J. van der Laan and D. Rubin. Targeted maximum likelihood learning.
# # The International Journal of Biostatistics, 2(1), 2006.
# # http://www.bepress.com/ijb/vol2/iss1/11/
# #-------------verify_args------------------
# verify_args <- function(Y,A,W,Delta){
#   ok1 <- length(Y) == length(A) & length(A) == nrow(W)
#   ok2 <- all(A[!is.na(A)] %in% 0:1)
#   if (!ok1) {warning("Y, A, W must contain the same number of observations")}
#   if (!ok2) {warning("A must be binary (0,1)")}
#   return(ok1&ok2)
# }
# #-----------set_DSAargs----------------
# set_DSAargs <- function(DSAargs, wts){
#   if(is.null(DSAargs$maxsumofpow)){DSAargs$maxsumofpow <- 2}
#   if(is.null(DSAargs$maxorderint)){DSAargs$maxorderint <- 2}
#   if(is.null(DSAargs$maxsize)) {DSAargs$maxsize <- 15}
#   if(is.null(DSAargs$Dmove)) {DSAargs$Dmove <- TRUE}
#   if(is.null(DSAargs$Smove)) {DSAargs$Smove <- TRUE}
#   if(is.null(DSAargs$vfold)) {DSAargs$vfold <- 5}
#   if(is.null(DSAargs$formula)){DSAargs$formula <- Y~A}
#   if(is.null(DSAargs$family)){DSAargs$family <- "gaussian"}
#   if(is.null(DSAargs$silent)) {DSAargs$silent <- TRUE}
#   if(is.null(DSAargs$wts)) {DSAargs$wts <- matrix(data=rep(wts, DSAargs$vfold+1),
#                                                   byrow=TRUE, nrow=DSAargs$vfold+1)}
#   if(is.null(DSAargs$nsplits)) {DSAargs$nsplits <- 1}
#   if(is.null(DSAargs$silent)) {DSAargs$silent <- -1}
#   return(DSAargs)
# }
# #-----------function logit---------
# # convert probability to logit
# # truncate probability passed in
# #-------------------------------
# logit <- function(x){
#   x[x>1] <-1
#   x[x<0] <-0
#   return(-log(1/x - 1))
# }
# 
# #-----------estimate_Q----------------
# # figure out if Q is one of three things:
# # 1. a matrix of values, QAW, Q1W, Q0W
# # 2. a model to use glm on
# # 3. null - estimate with DSA if available, otherwise main terms with glm
# # returns matrix of linear predictors for Q(A,W), Q(1,W), Q(0,W)
# #----------------------------------------
# estimate_Q <- function (Q, DSAargs, Y,A,W, Delta, family, wts, id) {
#   if(is.matrix(Q)){
#     if (family == "binomial") {Q <- logit(Q)}
#     coef <- NA
#   } else {
#     if (is.null(Q)){
#       if(require(DSA)){
#         DSAargs <- set_DSAargs(DSAargs, wts)
#         m <- DSA(formula=DSAargs$formula, data=data.frame(Y,A,W)[Delta==1,],
#                  weights=DSAargs$wts[,Delta==1], id=id[Delta==1],
#                  maxsumofpow=DSAargs$maxsumofpow, maxorderint=DSAargs$maxorderint,
#                  maxsize=DSAargs$maxsize, Dmoves=DSAargs$Dmove, Smove=DSAargs$Smove,
#                  family=family, candidate.rank=DSAargs$candidate.rank,
#                  rank.cutoffs = DSAargs$rank.cutoffs, usersplits=DSAargs$usersplits,
#                  userseed=DSAargs$userseed, vfold=DSAargs$vfold, nsplits=DSAargs$nsplits,
#                  silent=DSAargs$silent )
#       } else {
#         warning("DSA not found, running main terms regression for Q using glm")
#         form <- paste("Y??A", paste(colnames(W), collapse = "+"), sep="+")
#         m <- glm(form, family=family, data=data.frame(Y,A,W, wts, Delta), weights=wts,
#                  na.action=na.exclude, subset=Delta==1)
#       }
#     } else {
#       form <- try(as.formula(Q))
#       if(class(form)== "formula") {
#         m <- glm(form, family=family, data=data.frame(Y,A,W, wts, Delta), weights=wts,
#                  na.action=na.exclude, subset=Delta==1)
#       } else {
#         warning("Invalid formula supplied, running main terms regression for Q using glm")
#         form <- paste("Y??A", paste(colnames(W), collapse = "+"), sep="+")
#         m <- glm(form, family=family, data=data.frame(Y,A,W, wts, Delta), weights=wts,
#                  na.action=na.exclude, subset=Delta==1)
#       }
#     }
#     QAW <- predict(m, newdata=data.frame(Y,A,W))
#     Q1W <- predict(m, newdata=data.frame(Y,A=1,W))
#     Q0W <- predict(m, newdata=data.frame(Y,A=0,W))
#     Q <- cbind(QAW, Q1W, Q0W)
#     coef <- coef(m)
#   }
#   return(list(Q=Q, coef=coef))
# }
# #-----------estimate_g----------------
# # Estimate any factor of g
# #----------------------------------------
# estimate_g <- function (g, DSAargs,A,W, Delta, wts, id) {
#   if (!is.numeric(g)){
#     if (all(A==A[1])) {
#       g1W <- 1
#       coef<- NA
#     } else {
#       if (is.null(g)){
#         if(require(DSA)){
#           DSAargs <- set_DSAargs(DSAargs, wts)
#           m <- DSA(formula=DSAargs$formula, data=data.frame(A,W)[Delta==1,],
#                    weights=DSAargs$wts[,Delta==1], id=id[Delta==1],
#                    maxsumofpow=DSAargs$maxsumofpow, maxorderint=DSAargs$maxorderint,
#                    maxsize=DSAargs$maxsize, Dmoves=DSAargs$Dmove, Smove=DSAargs$Smove,
#                    family="binomial", candidate.rank=DSAargs$candidate.rank,
#                    rank.cutoffs = DSAargs$rank.cutoffs, usersplits=DSAargs$usersplits,
#                    userseed=DSAargs$userseed, vfold=DSAargs$vfold, nsplits=DSAargs$nsplits,
#                    silent=DSAargs$silent )
#         } else {
#           warning("DSA not found, running main terms regression for g using glm")
#           form <- paste("A??1", paste(colnames(W), collapse = "+"), sep="+")
#           m <- glm(form, family="binomial", data=data.frame(A,W, wts, Delta), weights=wts,
#                    na.action=na.exclude, subset=Delta==1)
#         }
#       } else {
#         form <- try(as.formula(g))
#         if(class(form)== "formula") {
#           m <- try(glm(form, family="binomial", data=data.frame(A,W, wts, Delta), weights=wts,
#                        na.action=na.exclude, subset=Delta==1))
#           if (class(m)[1]=="try-error"){
#             warning("Invalid formula supplied, running main terms regression for g using glm")
#             form <- paste("A??1", paste(colnames(W), collapse = "+"), sep="+")
#             m <- glm(form, family="binomial", data=data.frame(A,W, wts, Delta),
#                      weights=wts,na.action=na.exclude, subset=Delta==1)
#           }
#         } else {
#           form <- paste("A??1", paste(colnames(W), collapse = "+"), sep="+")
#           m <- glm(form, family="binomial", data=data.frame(A,W, wts, Delta), weights=wts,
#                    na.action=na.exclude, subset=Delta==1)
#         }
#       }
#       g1W <- predict(m, newdata=data.frame(A,W,wts), type="response")
#       coef <- m$coef
#     }
#   } else {
#     g1W <- g
#     coef <- NA
#   }
#   return(list(g1W=g1W, coef=coef))
# }

#-------------------------------tmle----------------------------------------
# estimate marginal treatment effect for binary point treatment
# accounting for missing outcomes.
# arguments:
# Y - outcome
# A - binary treatment indicator, 1-treatment, 0 - control
# W - vector, matrix or dataframe containing baseline covariates
# Delta - indicator of missing outcome or treatment assignment. 1 - observed, 0 - missing
# id - id identifying repeated measures
# Q - E(Y|A,W), specified in one of three ways:
# 1. NULL - defaults to DSA estimation of E(Y|A=a, W), with A forced into the model
# 2. matrix of values containing three columns. 1: E(Y|A=a,W), 2: E(Y|A=1,W), 3: E(Y|A=0,W)
# 3. formula for estimation of E(Y|A, W), suitable for call to glm
# g_A - binary treatment mechanism, specified in one of three ways:
# 1. NULL - defaults to DSA estimation of P(A=1|W)
# 2. vector of values P(A=1|W)


# 3. formula for estimation of P(A=1,W), suitable for call to glm
# g_M - missingness mechanism, specified in one of three ways:
# 1. NULL - defaults to DSA estimation of P(Delta=1|W)
# 2. vector of values P(Delta=1|W)
# 3. formula for estimation of P(Delta=1,W), suitable for call to glm
# wts - optional weights on observations
# DSAargs - optional settings for DSA estimation
# defaults: maxsumofpow = 2, maxorderint = 2, maxsize=min(2*ncol(W),15) (model size capped at 15),
# vfold = 5, nsplits=1, Dmove=TRUE, Smove=TRUE
# family - family specification for regression models, defaults to gaussian
# DETAILED - flag indicating basic or detailed return value.
# TRUE - psi, treatment effect estimate,
# var - estimated variance of parameter estimate,
# epsilon - coefficient used in targeting step
# coefficients and predicted values for Q_n??0(A,W), g_A(1,W), g_M(1,A,W)
# FALSE - psi, treatment effect estimate,
# var - estimated variance of parameter estimate
#-------------------------------------------------------------------------------


#------------------------------------------------------------------------------------
# II. Data, sample calls and examples
#------------------------------------------------------------------------------------
# tmle examples
# use with function tmle in file tmle.R
# Susan Gruber
# sgruber@berkeley.edu
# August 16, 2009
# Important: Generate data before running the examples!
# psi_0 = 1
#------------generate data --------------
# set.seed(10)
# n <- 500
# W <- matrix(rnorm(n*3), ncol=3)
# A <- rbinom(n,1, 1/(1+exp(-(.1*W[,1] - .1*W[,2] + .5*W[,3]))))
# Y <- A + 2*W[,1] + W[,3] + W[,2]^2 + rnorm(n)
# colnames(W) <- paste("W",1:3, sep="")

# rm(n, W, A, Y)

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




#--------------------------------------------------------
# Example 1, default function invocation
# invokes DSA to estimate Q, g_A, g_M,
# because Delta argument is not supplied, assumes (Y,A) observed for all obs
# result1 <- tmle(Y,A,W) 
# summary(result1)

#--------------------------------------------------------
# This example doesn't apply to me, I think
# Example 2: Binary outcome, DSA estimates Q
# known g_A = 0.5 is user-supplied,
#
# A.ex2 <- rbinom(n,1,.5)
# Y.ex2 <- A.ex2 + 2*W[,1] + W[,3] + W[,2]^2 + rnorm(n)
# result2 <- tmle(Y=Y.ex2,A=A.ex2,W, g_A =rep(.5, length(Y)))
# summary(result2)
#--------------------------------------------------------
# USE THIS ONE!!!
# Example 3: Supplying an indicator for observations missing the outcome
# set Delta to 1 for obs where Y is observed, 0 when Y is missing
# In this example, Delta is set to indicate 20% missing values, MCAR
# DSA to estimate Q, g_A, g_M,
# set DETAILED=TRUE to see model selected by DSA and predicted values
# for Q_n^0, g_A, g_M for each observation, and epsilon.
# Delta <- rbinom(n,1,.8)

library(tmle)
# my variables
A <- int.data.wo.nas$gruppe # treatment
Delta <- na.vector.all_wissen_nachher # missings
W <- matrix(c(int.data.wo.nas$sex,
              int.data.wo.nas$alterkat2.binary,
              int.data.wo.nas$schulabschluss.binary,
              int.data.wo.nas$smoking.status,
              int.data.wo.nas$asthma.or.rhinoconj, 
              int.data.wo.nas$risk_perception,
              int.data.wo.nas$par.asthma,
              int.data.wo.nas$all_wissen),
            ncol=8)
colnames(W) <- paste("W",1:8, sep="") # covariates
head(W)
Y <- int.data.wo.nas$all_wissen_nachher
result6 <- tmle(Y=Y,A=A,W=W, Delta=Delta, family= "binomial", 
                g.SL.library = c("SL.glm", "SL.step", "SL.glm.interaction"), 
                Q.SL.library = c("SL.glm", "SL.step", "SL.glm.interaction")) 
# result6 <- tmle(Y=Y,A=NULL,W=W, Delta=Delta, family= "binomial", 
#                 g.SL.library = c("SL.glm", "SL.step", "SL.glm.interaction"), 
#                 Q.SL.library = c("SL.glm", "SL.step", "SL.glm.interaction")) 
summary(result6)


W <- matrix(c(int.data.wo.nas$sex,
              int.data.wo.nas$alterkat2.binary,
              int.data.wo.nas$schulabschluss.binary,
              int.data.wo.nas$smoking.status,
              int.data.wo.nas$asthma.or.rhinoconj, 
              int.data.wo.nas$risk_perception,
              int.data.wo.nas$par.asthma,
              int.data.wo.nas$all_wissen5),
            ncol=8)
Y <- int.data.wo.nas$all_wissen_nachher5
table(Y)
result5 <- tmle(Y=Y,A=A,W=W, Delta=Delta, family= "binomial", 
                g.SL.library = c("SL.glm", "SL.step", "SL.glm.interaction"), 
                Q.SL.library = c("SL.glm", "SL.step", "SL.glm.interaction")) 
summary(result5)

W <- matrix(c(int.data.wo.nas$sex,
              int.data.wo.nas$alterkat2.binary,
              int.data.wo.nas$schulabschluss.binary,
              int.data.wo.nas$smoking.status,
              int.data.wo.nas$asthma.or.rhinoconj, 
              int.data.wo.nas$risk_perception,
              int.data.wo.nas$par.asthma,
              int.data.wo.nas$all_wissen4),
            ncol=8)
Y <- int.data.wo.nas$all_wissen_nachher4
table(Y)
result4 <- tmle(Y=Y,A=A,W=W, Delta=Delta, family= "binomial", 
                g.SL.library = c("SL.glm", "SL.step", "SL.glm.interaction"), 
                Q.SL.library = c("SL.glm", "SL.step", "SL.glm.interaction")) 
summary(result4)



#--------------------------------------------------------
# Example 4: User-supplied (misspecified) model for Q, DSA estimates for g_A, g_M
# approx. 20% missing, MAR
# Delta <- rbinom(n, 1, 1/(1+exp(-(1.7-1*W[,1]))))
# result4 <- tmle(Y,A,W, Delta=Delta, Q=Y~A+W1+W2+W3, DETAILED=TRUE)

#--------------------------------------------------------
# Example 5: User-supplied models for g_A and missingness mechanism g_M,
# DSA estimates Q.
# 100 unique IDs supplied
# Usage note: use "A" for dependent variable name in the formula for g_M
# Delta <- rbinom(n, 1, 1/(1+exp(-(1.6-1*W[,1]))))
# result5 <- tmle(Y,A,W, Delta=Delta, g_A=A~W1+W2+W3, g_M=A~W1, DETAILED=FALSE)
#--------------------------------------------------------
# results_summary <- cbind(c(result1$psi, result3$psi, result4$psi),
#                          c(result1$var, result3$var, result4$var))
# colnames(results_summary) <- c("estimate", "variance")
# print(results_summary,digits=3)

