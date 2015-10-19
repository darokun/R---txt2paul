#----------------------------#
# II. Recoding of variables: #
#----------------------------#

#----------------------------------------------------------------------
# Logic behind this process:
# asthma == 1 is:
# (q1 == 1 AND q2 == 1) OR q5 == 1.
#
# using interaction()
# Step 1. between q1 and q2, the possibilities are:
# q1 == 0 AND q2 == 0 -> 1
# q1 == 1 AND q2 == 0 -> 2
# q1 == 0 AND q2 == 1 -> 3
# q1 == 1 AND q2 == 1 -> 4
# q1 == 0 AND q2 == 2 -> 5
# q1 == 1 AND q2 == 2 -> 6
#
# Step 2. if interaction() result is 4 -> assign 1 to a new variable asthma2
# This means that the person has asthma symptoms, as defined by qq. 1+2.
# if interaction() result is 1:3+5:6 -> assign 0 to asthma2
# This means that the person does NOT have asthma symptoms, as defined by qq. 1+2.
#

# Step 3. use interaction() between asthma2 and q.5
# between q1 and q2, the possibilities are:
# asthma2 == 0 AND medikamente == 0 -> 1
# asthma2 == 1 AND medikamente == 0 -> 2
# asthma2 == 0 AND medikamente == 1 -> 3
# asthma2 == 1 AND medikamente == 1 -> 4
# asthma2 == 0 AND medikamente == 2 -> 5
# asthma2 == 1 AND medikamente == 2 -> 6

# Step 4. obtain asthma3, which is the final variable that defines presence of asthma.
# in this case, it is asthma2 == 1 OR medikamente == 1 -> asthma3 == 1
# therefore, if interaction() result is 2,3 or 4 -> assign 1 to a new variable asthma3
# This means that the person has asthma symptoms, as defined by qq. 1,2+5.
# if interaction() result is 1, 5 or 6 -> assign 0 to asthma3
# This means that the person does NOT have asthma symptoms, as defined by qq. 1,2+5.
#
# Step 5. Repeat the same process for qq. 3+4, 6+7, 8+9.
#----------------------------------------------------------------------

# This method apparently does not work. Not using it.
# ------------------ Recoding q.1, q.2 and a.5. (Asthma Symptoms)
# Step 1. Comprise pfeifen and pfeifen_ohne_erk
# q.1 <- data3[,1]
# q.2 <- data3[,2]
# inter.asthma <- as.numeric(interaction(q.1, q.2))
# inter.asthma # check

# Step 2. Create new variable asthma2 and store results from inter.asthma
# asthma2 <- NULL # create a new NULL variable to store positives from interaction
# (a.k.a. inter.asthma == 4)
# asthma2[inter.asthma==4] <- 1
# asthma2[inter.asthma==1 | inter.asthma==2 | inter.asthma==3 | inter.asthma==5 | inter.asthma==6] <- 0
# asthma2 # check

# Step 3. use interaction() between asthma2 and q.5
# q.5 <- data3[,5]
# inter.asthma2 <- as.numeric(interaction(asthma2, q.5))
# inter.asthma2 # check

# Step 4. Create new variable asthma3 and store results from inter.asthma2
# asthma3 <- NULL # create a new NULL variable to store positives from interaction
# asthma3[inter.asthma2==2 | inter.asthma2==3 | inter.asthma2==4] <- 1
# asthma3[inter.asthma2==1 | inter.asthma2==5 | inter.asthma2==6] <- 0
# asthma3 # check

# asthma3[43] # check. It is == 1
# data3[43,c(1,2,5)] # check
# this subject has not had wheezing symptoms (pfeifen == 0 and pfeifen_ohne_erk == 0) in the last 12 months, but has taken medication for asthma in the same period. Therefore, he or she is considered to have asthma symptoms. 

#--- RE-DO - Apparently this doesn't work either
# definition: asthma3 == wheeze without cold==1 OR medication==1
# q.2 <- data3[,2]
# q.5 <- data3[,5]
# inter.asthma3 <- as.numeric(interaction(q.2, q.5))
# inter.asthma3
# asthma3[inter.asthma3==2 | inter.asthma3==4 | inter.asthma3==5 | inter.asthma3==6 | inter.asthma3==8] <- 1
# asthma3[inter.asthma3==1 | inter.asthma3==3 | inter.asthma3==7 | inter.asthma3==9] <- 0

#--- keep this - This is the one that seems to do the job
asthma3 <- NULL
asthma3[data$pfeifen_ohne_erk==0 | data$medikamente==0 | data$pfeifen_ohne_erk==2 | data$medikamente==2] <- 0
asthma3[data$pfeifen_ohne_erk==1 | data$medikamente==1] <- 1
#--- keep this
table(asthma3, useNA="ifany")



# ------------------ Recoding q.3 and q.4 (Asthma Dx)
# Step 1. Comprise asthma and asthma_arzt
q.3 <- data3[,3]
q.4 <- data3[,4]
inter.asthma3 <- as.numeric(interaction(q.3, q.4))
# inter.asthma3 # check

# Step 2. Create new variable asthma.dx and store results from inter.asthma3
asthma.dx <- NULL # create a new NULL variable to store positives from interaction
asthma.dx[inter.asthma3==2 | inter.asthma3==3 |inter.asthma3==4 | inter.asthma3==6] <- 1
asthma.dx[inter.asthma3==1 |  inter.asthma3==5 ] <- 0
# asthma.dx # check
# I am getting 11 positives because observation 45 had said 'no' to q.4 (asthma_arzt[45] == 0), but he or she did not answer q.3, hence there is a NA there.
table(asthma.dx, useNA="ifany")

#--- keep this - This is the one that seems to do the job
asthma.dx <- NULL
asthma.dx[data$asthma==0 | data$asthma_arzt==0 | data$asthma_arzt==2] <- 0
asthma.dx[data$asthma==1 | data$asthma_arzt==1] <- 1
#--- keep this
table(asthma3, useNA="ifany")


# ------------------ Recoding q.6 and q.7 (Allergies)
# Step 1. Comprise nasenprobleme and nasenprobleme_augen
q.6 <- data3[,6]
q.7 <- data3[,7]
inter.allergies <- as.numeric(interaction(q.6, q.7))
# inter.allergies # check

# Step 2. Create new variable allergies and store results from inter.allergies
allergies <- NULL # create a new NULL variable to store positives from interaction
allergies[inter.allergies==2 | inter.allergies==3 |inter.allergies==4 | inter.allergies==6] <- 1
allergies[inter.allergies==1 |  inter.allergies==5 ] <- 0
# allergies # check
# it seems like there's is no point in doing this, as in e.g. q. 6 already contains q.7's information. Therefore, the variable allergies has the same information as q.6. It might still come in handy for qq. 8 and 9 on parental asthma.

#------------------ Recoding q.8 and q.9 (Parental asthma)
# Step 1. Comprise allergie_mutter and allergie_vater
q.8 <- data3[,8]
q.9 <- data3[,9]
inter.par.asthma <- as.numeric(interaction(q.8, q.9))
# inter.par.asthma # check

# Step 2. Create new variable par.asthma and store results from inter.par.asthma
par.asthma <- NULL # create a new NULL variable to store positives from interaction
par.asthma[inter.par.asthma==2 | inter.par.asthma==4 | inter.par.asthma==5 | inter.par.asthma==6 | inter.par.asthma==8] <- 1
par.asthma[inter.par.asthma==1 |  inter.par.asthma==3 |  inter.par.asthma==7 |  inter.par.asthma==9] <- 0
# par.asthma # check

#------------------ Recoding smoking and current smoking
# Step 1. Comprise rauchen and rauchen_aktuell
q.ex.smoker <- data[,20]
q.cur.smoker <- data[,21]
inter.smoker <- as.numeric(interaction(q.ex.smoker, q.cur.smoker))
inter.smoker # check
unique(inter.smoker)

# Step 2. Create new variable smoking.status and store results from inter.smoker
smoking.status <- NULL # create a new NULL variable to store positives from interaction
smoking.status[inter.smoker==1] <- 0
smoking.status[inter.smoker==2] <- 2
smoking.status[inter.smoker==3 | inter.smoker==4] <- 1
# smoking.status # check