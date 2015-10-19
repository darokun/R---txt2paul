#-------------------------#
# IV. Collapse levels and #
#    convert to factors:  #
#-------------------------#

# Note: these variables have 5 levels (1:5). 
# They are being collapsed into 3 leves: 1+2 ,3+4+5.
# Some recoding was done also to change the direction in arbeitskleidung

# arbeitskleidung_letzteWoche
arbeitskleidung3_letzteWoche <- NULL
arbeitskleidung3_letzteWoche[arbeitskleidung_letzteWoche.factor==3] <- 2
arbeitskleidung3_letzteWoche[arbeitskleidung_letzteWoche.factor==1 | arbeitskleidung_letzteWoche.factor==2 ] <- 2
arbeitskleidung3_letzteWoche[arbeitskleidung_letzteWoche.factor==3 |arbeitskleidung_letzteWoche.factor==4 | arbeitskleidung_letzteWoche.factor==5 ] <- 1
arbeitskleidung3_letzteWoche.factor <- as.factor(arbeitskleidung3_letzteWoche)

# haare_letzteWoche
haare3_letzteWoche <- NULL
haare3_letzteWoche[haare_letzteWoche.factor==3] <- 2
haare3_letzteWoche[haare_letzteWoche.factor==1 | haare_letzteWoche.factor==2 ] <- 1
haare3_letzteWoche[haare_letzteWoche.factor==4 | haare_letzteWoche.factor==5 ] <- 3
haare3_letzteWoche.factor <- as.factor(haare3_letzteWoche)

# aufbewahrung_letzteWoche
aufbewahrung3_letzteWoche <- NULL
aufbewahrung3_letzteWoche[aufbewahrung_letzteWoche.factor==3] <- 2
aufbewahrung3_letzteWoche[aufbewahrung_letzteWoche.factor==1 | aufbewahrung_letzteWoche.factor==2 ] <- 1
aufbewahrung3_letzteWoche[aufbewahrung_letzteWoche.factor==4 | aufbewahrung_letzteWoche.factor==5 ] <- 3
aufbewahrung3_letzteWoche.factor <- as.factor(aufbewahrung3_letzteWoche)

# arbeitskleidung_intention
arbeitskleidung3_intention <- NULL
arbeitskleidung3_intention[arbeitskleidung_intention.factor==3] <- 2
arbeitskleidung3_intention[arbeitskleidung_intention.factor==1 | arbeitskleidung_intention.factor==2 ] <- 1
arbeitskleidung3_intention[arbeitskleidung_intention.factor==4 | arbeitskleidung_intention.factor==5 ] <- 3
arbeitskleidung3_intention.factor <- as.factor(arbeitskleidung3_intention)

# haare_intention
haare3_intention <- NULL
haare3_intention[haare_intention.factor==3] <- 2
haare3_intention[haare_intention.factor==1 | haare_intention.factor==2 ] <- 1
haare3_intention[haare_intention.factor==4 | haare_intention.factor==5 ] <- 3
haare3_intention.factor <- as.factor(haare3_intention)

# aufbewahrung_intention
aufbewahrung3_intention <- NULL
aufbewahrung3_intention[aufbewahrung_intention.factor==3] <- 2
aufbewahrung3_intention[aufbewahrung_intention.factor==1 | aufbewahrung_intention.factor==2 ] <- 1
aufbewahrung3_intention[aufbewahrung_intention.factor==4 | aufbewahrung_intention.factor==5 ] <- 3
aufbewahrung3_intention.factor <- as.factor(aufbewahrung3_intention)

# arbeitskleidung_einstellung
arbeitskleidung3_einstellung <- NULL
arbeitskleidung3_einstellung[arbeitskleidung_einstellung.factor==3] <- 2
arbeitskleidung3_einstellung[arbeitskleidung_einstellung.factor==1 | arbeitskleidung_einstellung.factor==2 ] <- 1
arbeitskleidung3_einstellung[arbeitskleidung_einstellung.factor==4 | arbeitskleidung_einstellung.factor==5 ] <- 3
arbeitskleidung3_einstellung.factor <- as.factor(arbeitskleidung3_einstellung)

# haare_einstellung
haare3_einstellung <- NULL
haare3_einstellung[haare_einstellung.factor==3] <- 2
haare3_einstellung[haare_einstellung.factor==1 | haare_einstellung.factor==2 ] <- 1
haare3_einstellung[haare_einstellung.factor==4 | haare_einstellung.factor==5 ] <- 3
haare3_einstellung.factor <- as.factor(haare3_einstellung)

# aufbewahrung_einstellung
aufbewahrung3_einstellung <- NULL
aufbewahrung3_einstellung[aufbewahrung_einstellung.factor==3] <- 2
aufbewahrung3_einstellung[aufbewahrung_einstellung.factor==1 | aufbewahrung_einstellung.factor==2 ] <- 1
aufbewahrung3_einstellung[aufbewahrung_einstellung.factor==4 | aufbewahrung_einstellung.factor==5 ] <- 3
aufbewahrung3_einstellung.factor <- as.factor(aufbewahrung3_einstellung)

# arbeitskleidung_norm
arbeitskleidung3_norm <- NULL
arbeitskleidung3_norm[arbeitskleidung_norm.factor==3] <- 2
arbeitskleidung3_norm[arbeitskleidung_norm.factor==1 | arbeitskleidung_norm.factor==2 ] <- 1
arbeitskleidung3_norm[arbeitskleidung_norm.factor==4 | arbeitskleidung_norm.factor==5 ] <- 3
arbeitskleidung3_norm.factor <- as.factor(arbeitskleidung3_norm)

# haare_norm
haare3_norm <- NULL
haare3_norm[haare_norm.factor==3] <- 2
haare3_norm[haare_norm.factor==1 | haare_norm.factor==2 ] <- 1
haare3_norm[haare_norm.factor==4 | haare_norm.factor==5 ] <- 3
haare3_norm.factor <- as.factor(haare3_norm)

# aufbewahrung_norm
aufbewahrung3_norm <- NULL
aufbewahrung3_norm[aufbewahrung_norm.factor==3] <- 2
aufbewahrung3_norm[aufbewahrung_norm.factor==1 | aufbewahrung_norm.factor==2 ] <- 1
aufbewahrung3_norm[aufbewahrung_norm.factor==4 | aufbewahrung_norm.factor==5 ] <- 3
aufbewahrung3_norm.factor <- as.factor(aufbewahrung3_norm)

# arbeitskleidung_kontrolle
arbeitskleidung3_kontrolle <- NULL
arbeitskleidung3_kontrolle[arbeitskleidung_kontrolle.factor==3] <- 2
arbeitskleidung3_kontrolle[arbeitskleidung_kontrolle.factor==1 | arbeitskleidung_kontrolle.factor==2 ] <- 1
arbeitskleidung3_kontrolle[arbeitskleidung_kontrolle.factor==4 | arbeitskleidung_kontrolle.factor==5 ] <- 3
arbeitskleidung3_kontrolle.factor <- as.factor(arbeitskleidung3_kontrolle)

# haare_kontrolle
haare3_kontrolle <- NULL
haare3_kontrolle[haare_kontrolle.factor==3] <- 2
haare3_kontrolle[haare_kontrolle.factor==1 | haare_kontrolle.factor==2 ] <- 1
haare3_kontrolle[haare_kontrolle.factor==4 | haare_kontrolle.factor==5 ] <- 3
haare3_kontrolle.factor <- as.factor(haare3_kontrolle)

# aufbewahrung_kontrolle
aufbewahrung3_kontrolle <- NULL
aufbewahrung3_kontrolle[aufbewahrung_kontrolle.factor==3] <- 2
aufbewahrung3_kontrolle[aufbewahrung_kontrolle.factor==1 | aufbewahrung_kontrolle.factor==2 ] <- 1
aufbewahrung3_kontrolle[aufbewahrung_kontrolle.factor==4 | aufbewahrung_kontrolle.factor==5 ] <- 3
aufbewahrung3_kontrolle.factor <- as.factor(aufbewahrung3_kontrolle)

# allergie_bekommen
allergie3_bekommen <- NULL
allergie3_bekommen[allergie_bekommen.factor==3] <- 2
allergie3_bekommen[allergie_bekommen.factor==1 | allergie_bekommen.factor==2 ] <- 1
allergie3_bekommen[allergie_bekommen.factor==4 | allergie_bekommen.factor==5 ] <- 3
allergie3_bekommen.factor <- as.factor(allergie3_bekommen)

# allergie_schlimm
allergie3_schlimm <- NULL
allergie3_schlimm[allergie_schlimm.factor==3] <- 2
allergie3_schlimm[allergie_schlimm.factor==1 | allergie_schlimm.factor==2 ] <- 1
allergie3_schlimm[allergie_schlimm.factor==4 | allergie_schlimm.factor==5 ] <- 3
allergie3_schlimm.factor <- as.factor(allergie3_schlimm)