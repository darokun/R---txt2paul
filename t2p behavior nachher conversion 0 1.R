# arbeitskleidung_letzteWoche_nach
arbeitskleidung3_letzteWoche_nach <- NULL
arbeitskleidung3_letzteWoche_nach[data$arbeitskleidung_letzteWoche_nach==5] <- 1
arbeitskleidung3_letzteWoche_nach[data$arbeitskleidung_letzteWoche_nach==2 |data$arbeitskleidung_letzteWoche_nach==3 | data$arbeitskleidung_letzteWoche_nach==4 | data$arbeitskleidung_letzteWoche_nach==1 ] <- 0
data$arbeitskleidung3_letzteWoche_nachher.factor <- as.factor(arbeitskleidung3_letzteWoche_nach)


# haare_letzteWoche_nachher
haare3_letzteWoche <- NULL
haare3_letzteWoche[data$haare_letzteWoche_nachher==1] <- 0
haare3_letzteWoche[data$haare_letzteWoche_nachher==2 | data$haare_letzteWoche_nachher==3 | data$haare_letzteWoche_nachher==4 | data$haare_letzteWoche_nachher==5 ] <- 1
data$haare3_letzteWoche_nachher.factor <- as.factor(haare3_letzteWoche)

# aufbewahrung3_letzteWoche_nachher
aufbewahrung3_letzteWoche_nachher <- NULL
aufbewahrung3_letzteWoche_nachher[data$aufbewahrung_letzteWoche_nachher==1] <- 0
aufbewahrung3_letzteWoche_nachher[data$aufbewahrung_letzteWoche_nachher==2 |data$aufbewahrung3_letzteWoche_nachher==3 | data$aufbewahrung_letzteWoche_nachher==4 | data$aufbewahrung3_letzteWoche_nachher==5 ] <- 1
data$aufbewahrung3_letzteWoche_nachher.factor <- as.factor(aufbewahrung3_letzteWoche_nachher)
data49$aufbewahrung3_letzteWoche_nachher.factor <- factor(data49$aufbewahrung3_letzteWoche_nachher.factor, levels=c(0,1))

# arbeitskleidung_intention
arbeitskleidung3_intention_nachher <- NULL
arbeitskleidung3_intention_nachher[data$arbeitskleidung_intention==5] <- 1
arbeitskleidung3_intention_nachher[data$arbeitskleidung_intention==2 |data$arbeitskleidung_intention==3 |data$arbeitskleidung_intention==4 | data$arbeitskleidung_intention==1 ] <- 0
data$arbeitskleidung3_intention_nachher.factor <- as.factor(arbeitskleidung3_intention_nachher)

# haare_intention
haare3_intention_nachher <- NULL
haare3_intention_nachher[data$haare_intention==1] <- 0
haare3_intention_nachher[data$haare_intention==2 |data$haare_intention==3 | data$haare_intention==4 | data$haare_intention==5 ] <- 1
data$haare3_intention_nachher.factor <- as.factor(haare3_intention_nachher)

# aufbewahrung3_intention_nachher
aufbewahrung3_intention_nachher <- NULL
aufbewahrung3_intention_nachher[data$aufbewahrung_intention_nachher==1] <- 0
aufbewahrung3_intention_nachher[data$aufbewahrung_intention_nachher==2 |data$aufbewahrung_intention_nachher==3 | data$aufbewahrung_intention_nachher==4 | data$aufbewahrung_intention_nachher==5 ] <- 1
data$aufbewahrung3_intention_nachher.factor <- as.factor(aufbewahrung3_intention_nachher)

# arbeitskleidung_einstellung_nach
arbeitskleidung3_einstellung_nach <- NULL
arbeitskleidung3_einstellung_nach[data$arbeitskleidung_einstellung_nach==5] <- 1
arbeitskleidung3_einstellung_nach[data$arbeitskleidung_einstellung_nach==2 |data$arbeitskleidung_einstellung_nach==3 |data$arbeitskleidung_einstellung_nach==4 | data$arbeitskleidung_einstellung_nach==1 ] <- 0
data$arbeitskleidung3_einstellung_nachher.factor <- as.factor(arbeitskleidung3_einstellung_nach)

# haare_einstellung_nachher
haare3_einstellung_nachher <- NULL
haare3_einstellung_nachher[data$haare_einstellung_nachher==1] <- 0
haare3_einstellung_nachher[data$haare_einstellung_nachher==2 |data$haare_einstellung_nachher==3 | data$haare_einstellung_nachher==4 | data$haare_einstellung_nachher==5 ] <- 1
data$haare3_einstellung_nachher.factor <- as.factor(haare3_einstellung_nachher)

# aufbewahrung3_einstellung_nachher
aufbewahrung3_einstellung_nachher <- NULL
aufbewahrung3_einstellung_nachher[data$aufbewahrung_einstellung_nachher==1] <- 0
aufbewahrung3_einstellung_nachher[data$aufbewahrung_einstellung_nachher==2 |data$aufbewahrung_einstellung_nachher==3 | data$aufbewahrung_einstellung_nachher==4 | data$aufbewahrung_einstellung_nachher==5 ] <- 1
data$aufbewahrung3_einstellung_nachher.factor <- as.factor(aufbewahrung3_einstellung_nachher)

# arbeitskleidung_norm_nachher
arbeitskleidung3_norm_nachher <- NULL
arbeitskleidung3_norm_nachher[data$arbeitskleidung_norm_nachher==5] <- 1
arbeitskleidung3_norm_nachher[data$arbeitskleidung_norm_nachher==2 |data$arbeitskleidung_norm_nachher==3 |data$arbeitskleidung_norm_nachher==4 | data$arbeitskleidung_norm_nachher==1 ] <- 0
data$arbeitskleidung3_norm_nachher.factor <- as.factor(arbeitskleidung3_norm_nachher)

# haare_norm_nachher
haare3_norm_nachher <- NULL
haare3_norm_nachher[data$haare_norm_nachher==1] <- 0
haare3_norm_nachher[data$haare_norm_nachher==2 |data$haare_norm_nachher==3 | data$haare_norm_nachher==4 | data$haare_norm_nachher==5 ] <- 1
data$haare3_norm_nachher.factor <- as.factor(haare3_norm_nachher)

# aufbewahrung3_norm_nachher
aufbewahrung33_norm <- NULL
aufbewahrung33_norm[data$aufbewahrung_norm_nachher==1] <- 0
aufbewahrung33_norm[data$aufbewahrung_norm_nachher==2 |data$aufbewahrung_norm_nachher==3 | data$aufbewahrung_norm_nachher==4 | data$aufbewahrung_norm_nachher==5 ] <- 1
data$aufbewahrung3_norm_nachher.factor <- as.factor(aufbewahrung33_norm)

# arbeitskleidung_kontrolle_nachher
arbeitskleidung3_kontrolle_nachher <- NULL
arbeitskleidung3_kontrolle_nachher[data$arbeitskleidung_kontrolle_nachhe==5] <- 1
arbeitskleidung3_kontrolle_nachher[data$arbeitskleidung_kontrolle_nachhe==3 | data$arbeitskleidung_kontrolle_nachhe==4 | data$arbeitskleidung_kontrolle_nachhe==1 | data$arbeitskleidung_kontrolle_nachhe==2] <- 0
data$arbeitskleidung3_kontrolle_nachher.factor <- as.factor(arbeitskleidung3_kontrolle_nachher)


# haare_kontrolle_nachher
haare3_kontrolle_nachher <- NULL
haare3_kontrolle_nachher[data$haare_kontrolle_nachher==1] <- 0
haare3_kontrolle_nachher[data$haare_kontrolle_nachher==2 | data$haare_kontrolle_nachher==3 | data$haare_kontrolle_nachher==4 | data$haare_kontrolle_nachher==5 ] <- 1
data$haare3_kontrolle_nachher.factor <- as.factor(haare3_kontrolle_nachher)

# aufbewahrung3_kontrolle_nachher
aufbewahrung3_kontrolle_nachher <- NULL
aufbewahrung3_kontrolle_nachher[data$aufbewahrung_kontrolle_nachher==1] <- 0
aufbewahrung3_kontrolle_nachher[data$aufbewahrung_kontrolle_nachher==2 |data$aufbewahrung_kontrolle_nachher==3 | data$aufbewahrung_kontrolle_nachher==4 | data$aufbewahrung_kontrolle_nachher==5 ] <- 1
data$aufbewahrung3_kontrolle_nachher.factor <- as.factor(aufbewahrung3_kontrolle_nachher)


