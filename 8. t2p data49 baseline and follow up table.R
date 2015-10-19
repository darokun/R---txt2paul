#' ---
#' title: "txt2paul Patient characteristics at baseline and follow-up in the intervention n = 18 and control groups n = 31"
#' author: "Daloha Rodriguez-Molina"
#' date: "April 15th, 2015"
#' ---
#' 

#-------------------#
# Comments (add later)
#-------------------#

#-------------------#
# Preliminary stuff #
#-------------------#

# When working from home
setwd("~/Dropbox/R - txt2paul")

# When working at KR5
# setwd("/usr281/ben/msc1426/Desktop/dataproject_datasets-1")

# install packages
require("psych")

# Read the data
# d <- "/Users/daro/Desktop/txt2PAUL/z_Dokumente fuer Daloha/txt2PAUL_Gesamt_vers1_130115.csv"
# data <- read.csv(d, header=T)

# explore data and subset demographics
# head(data[,1:4])
# data1 <- data[,c(1:6,14:21)] # data1 = soziodemographie
# names(data1)


# Convert to factors
#haare
haare.factor.nachher49 <- factor(data49$haare_nachher, labels = yes.no.labels, levels = c(1,0))

#arbeitsschuhe
arbeitsschuhe.factor.nachher49 <- factor(data49$arbeitsschuhe_nachher, labels = yes.no.labels, levels = c(1,0))

#kleidung_aufb
kleidung_aufb.factor.nachher49 <- factor(data49$kleidung_aufb_nachher, labels = yes.no.labels, levels = c(1,0))

#desinfizieren
desinfizieren.factor.nachher49 <- factor(data49$desinfizieren_nachher, labels = yes.no.labels, levels = c(1,0))

#schutzbrille
schutzbrille.factor.nachher49 <- factor(data49$schutzbrille_nachher, labels = yes.no.labels, levels = c(1,0))

#kleidung_wohnraum
kleidung_wohnraum.factor.nachher49 <- factor(data49$kleidung_wohnraum_nachher, labels = yes.no.labels, levels = c(1,0))



#-------------------------#
# IV. Collapse levels and #
#    convert to factors:  #
#-------------------------#

# Note: these variables have 5 levels (1:5). 
# They are being collapsed into 3 leves: 1+2 ,3+4+5.
# Some recoding was done also to change the direction in arbeitskleidung

# arbeitskleidung_letzteWoche
arbeitskleidung3_letzteWoche.nachher49 <- NULL
arbeitskleidung3_letzteWoche.nachher49[data49$arbeitskleidung_letzteWoche_nach==1 | data49$arbeitskleidung_letzteWoche_nach==2 ] <- 2
arbeitskleidung3_letzteWoche.nachher49[data49$arbeitskleidung_letzteWoche_nach==3 | data49$arbeitskleidung_letzteWoche_nach==4 | data49$arbeitskleidung_letzteWoche_nach==5 ] <- 1
arbeitskleidung3_letzteWoche.factor.nachher49 <- as.factor(arbeitskleidung3_letzteWoche.nachher49)

# haare_letzteWoche
haare3_letzteWoche.nachher49 <- NULL
haare3_letzteWoche.nachher49[data49$haare_letzteWoche_nachher==1 | data49$haare_letzteWoche_nachher==2 ] <- 1
haare3_letzteWoche.nachher49[data49$haare_letzteWoche_nachher==3 | data49$haare_letzteWoche_nachher==4 | data49$haare_letzteWoche_nachher==5 ] <- 2
haare3_letzteWoche.factor.nachher49 <- as.factor(haare3_letzteWoche.nachher49)

# aufbewahrung_letzteWoche
aufbewahrung3_letzteWoche.nachher49 <- NULL
aufbewahrung3_letzteWoche.nachher49[data49$aufbewahrung_letzteWoche_nachher==1 | data49$aufbewahrung_letzteWoche_nachher==2 ] <- 1
aufbewahrung3_letzteWoche.nachher49[data49$aufbewahrung_letzteWoche_nachher==3 | data49$aufbewahrung_letzteWoche_nachher==4 | data49$aufbewahrung_letzteWoche_nachher==5 ] <- 2
aufbewahrung3_letzteWoche.factor.nachher49 <- as.factor(aufbewahrung3_letzteWoche.nachher49)

# arbeitskleidung_intention
arbeitskleidung3_intention.nachher49 <- NULL
arbeitskleidung3_intention.nachher49[data49$arbeitskleidung_intention_nachhe==3 | data49$arbeitskleidung_intention_nachhe==4 | data49$arbeitskleidung_intention_nachhe==5 ] <- 1
arbeitskleidung3_intention.nachher49[data49$arbeitskleidung_intention_nachhe==1 | data49$arbeitskleidung_intention_nachhe==2 ] <- 2
arbeitskleidung3_intention.factor.nachher49 <- as.factor(arbeitskleidung3_intention.nachher49)

# haare_intention
haare3_intention.nachher49 <- NULL
haare3_intention.nachher49[data49$haare_intention_nachher==1 | data49$haare_intention_nachher==2 ] <- 1
haare3_intention.nachher49[data49$haare_intention_nachher==3 | data49$haare_intention_nachher==4 | data49$haare_intention_nachher==5 ] <- 2
haare3_intention.factor.nachher49 <- as.factor(haare3_intention.nachher49)

# aufbewahrung_intention
aufbewahrung3_intention.nachher49 <- NULL
aufbewahrung3_intention.nachher49[data49$aufbewahrung_intention_nachher==1 | data49$aufbewahrung_intention_nachher==2 ] <- 1
aufbewahrung3_intention.nachher49[data49$aufbewahrung_intention_nachher==3 | data49$aufbewahrung_intention_nachher==4 | data49$aufbewahrung_intention_nachher==5 ] <- 2
aufbewahrung3_intention.factor.nachher49 <- as.factor(aufbewahrung3_intention.nachher49)

# arbeitskleidung_einstellung
arbeitskleidung3_einstellung.nachher49 <- NULL
arbeitskleidung3_einstellung.nachher49[data49$arbeitskleidung_einstellung_nach==3 | data49$arbeitskleidung_einstellung_nach==4 | data49$arbeitskleidung_einstellung_nach==5 ] <- 1
arbeitskleidung3_einstellung.nachher49[data49$arbeitskleidung_einstellung_nach==1 | data49$arbeitskleidung_einstellung_nach==2 ] <- 2
arbeitskleidung3_einstellung.factor.nachher49 <- as.factor(arbeitskleidung3_einstellung.nachher49)

# haare_einstellung
haare3_einstellung.nachher49 <- NULL
haare3_einstellung.nachher49[data49$haare_einstellung_nachher==1 | data49$haare_einstellung_nachher==2 ] <- 1
haare3_einstellung.nachher49[data49$haare_einstellung_nachher==3 | data49$haare_einstellung_nachher==4 | data49$haare_einstellung_nachher==5 ] <- 2
haare3_einstellung.factor.nachher49 <- as.factor(haare3_einstellung.nachher49)

table(haare3_einstellung.nachher49)

# aufbewahrung_einstellung
aufbewahrung3_einstellung.nachher49 <- NULL
aufbewahrung3_einstellung.nachher49[data49$aufbewahrung_einstellung_nachher==1 | data49$aufbewahrung_einstellung_nachher==2 ] <- 1
aufbewahrung3_einstellung.nachher49[data49$aufbewahrung_einstellung_nachher==4 | data49$aufbewahrung_einstellung_nachher==4 | data49$aufbewahrung_einstellung_nachher==5 ] <- 2
aufbewahrung3_einstellung.factor.nachher49 <- as.factor(aufbewahrung3_einstellung.nachher49)

# arbeitskleidung_norm
arbeitskleidung3_norm.nachher49 <- NULL
arbeitskleidung3_norm.nachher49[data49$arbeitskleidung_norm_nachher==3 | data49$arbeitskleidung_norm_nachher==4 | data49$arbeitskleidung_norm_nachher==5 ] <- 1
arbeitskleidung3_norm.nachher49[data49$arbeitskleidung_norm_nachher==1 | data49$arbeitskleidung_norm_nachher==2 ] <- 2
arbeitskleidung3_norm.factor.nachher49 <- as.factor(arbeitskleidung3_norm.nachher49)

# haare_norm
haare3_norm.nachher49 <- NULL
haare3_norm.nachher49[data49$haare_norm_nachher==1 | data49$haare_norm_nachher==2 ] <- 1
haare3_norm.nachher49[data49$haare_norm_nachher==3 | data49$haare_norm_nachher==4 | data49$haare_norm_nachher==5 ] <- 2
haare3_norm.factor.nachher49 <- as.factor(haare3_norm.nachher49)

# aufbewahrung_norm
aufbewahrung3_norm.nachher49 <- NULL
aufbewahrung3_norm.nachher49[data49$aufbewahrung_norm_nachher==1 | data49$aufbewahrung_norm_nachher==2 ] <- 1
aufbewahrung3_norm.nachher49[data49$aufbewahrung_norm_nachher==3 | data49$aufbewahrung_norm_nachher==4 | data49$aufbewahrung_norm_nachher==5 ] <- 2
aufbewahrung3_norm.factor.nachher49 <- as.factor(aufbewahrung3_norm.nachher49)

# arbeitskleidung_kontrolle
arbeitskleidung3_kontrolle.nachher49 <- NULL
arbeitskleidung3_kontrolle.nachher49[data49$arbeitskleidung_kontrolle_nachhe==1 | data49$arbeitskleidung_kontrolle_nachhe==2 ] <- 1
arbeitskleidung3_kontrolle.nachher49[data49$arbeitskleidung_kontrolle_nachhe==3 | data49$arbeitskleidung_kontrolle_nachhe==4 | data49$arbeitskleidung_kontrolle_nachhe==5 ] <- 2
arbeitskleidung3_kontrolle.factor.nachher49 <- as.factor(arbeitskleidung3_kontrolle.nachher49)

# haare_kontrolle
haare3_kontrolle.nachher49 <- NULL
haare3_kontrolle.nachher49[data49$haare_kontrolle_nachher==1 | data49$haare_kontrolle_nachher==2 ] <- 1
haare3_kontrolle.nachher49[data49$haare_kontrolle_nachher==3 | data49$haare_kontrolle_nachher==4 | data49$haare_kontrolle_nachher==5 ] <- 2
haare3_kontrolle.factor.nachher49 <- as.factor(haare3_kontrolle.nachher49)

# aufbewahrung_kontrolle
aufbewahrung3_kontrolle.nachher49 <- NULL
aufbewahrung3_kontrolle.nachher49[data49$aufbewahrung_kontrolle_nachher==1 | data49$aufbewahrung_kontrolle_nachher==2 ] <- 1
aufbewahrung3_kontrolle.nachher49[data49$aufbewahrung_kontrolle_nachher==3 | data49$aufbewahrung_kontrolle_nachher==4 | data49$aufbewahrung_kontrolle_nachher==5 ] <- 2
aufbewahrung3_kontrolle.factor.nachher49 <- as.factor(aufbewahrung3_kontrolle.nachher49)


#---

# Order: (read from left to right)
# Knowledge         #gruppe==1 (baseline) #gruppe==1 _nachher | #gruppe==0 (baseline) #gruppe==0 _nachher
# Behavior          #gruppe==1 (baseline) #gruppe==1 _nachher | #gruppe==0 (baseline) #gruppe==0 _nachher

#-----------#
# Knowledge #
#-----------#

# Labels
f.8.table.labels <- c("int.base.abs", "int.base.perc", "int.fol.abs", "int.fol.perc", "cont.base.abs", "cont.base.perc", "cont.fol.abs", "cont.fol.perc")

# WISSEN
# haare
rbind(f.8.table.labels, 
      cbind(
        table(haare.factor49[data49$gruppe==1]), 
        round(prop.table(table(haare.factor49[data49$gruppe==1])),4), 
        
        table(haare.factor.nachher49[data49$gruppe==1]), 
        round(prop.table(table(haare.factor.nachher49[data49$gruppe==1])),4),
        
        table(haare.factor49[data49$gruppe==0]), 
        round(prop.table(table(haare.factor49[data49$gruppe==0])),4), 
        
        table(haare.factor.nachher49[data49$gruppe==0]), 
        round(prop.table(table(haare.factor.nachher49[data49$gruppe==0])),4)
      )
)

# arbeitsschue
rbind(f.8.table.labels, 
      cbind(
        table(arbeitsschuhe.factor49[data49$gruppe==1]), 
        round(prop.table(table(arbeitsschuhe.factor49[data49$gruppe==1])),4), 
        
        table(arbeitsschuhe.factor.nachher49[data49$gruppe==1]), 
        round(prop.table(table(arbeitsschuhe.factor.nachher49[data49$gruppe==1])),4),
        
        table(arbeitsschuhe.factor49[data49$gruppe==0]), 
        round(prop.table(table(arbeitsschuhe.factor49[data49$gruppe==0])),4), 
        
        table(arbeitsschuhe.factor.nachher49[data49$gruppe==0]), 
        round(prop.table(table(arbeitsschuhe.factor.nachher49[data49$gruppe==0])),4)
      )
)

# kleidung_aufb
rbind(f.8.table.labels, 
      cbind(
        table(kleidung_aufb.factor49[data49$gruppe==1]), 
        round(prop.table(table(kleidung_aufb.factor49[data49$gruppe==1])),4), 
        
        table(kleidung_aufb.factor.nachher49[data49$gruppe==1]), 
        round(prop.table(table(kleidung_aufb.factor.nachher49[data49$gruppe==1])),4),
        
        table(kleidung_aufb.factor49[data49$gruppe==0]), 
        round(prop.table(table(kleidung_aufb.factor49[data49$gruppe==0])),4), 
        
        table(kleidung_aufb.factor.nachher49[data49$gruppe==0]), 
        round(prop.table(table(kleidung_aufb.factor.nachher49[data49$gruppe==0])),4)
      )
)

# desinfizieren
rbind(f.8.table.labels, 
      cbind(
        table(desinfizieren.factor49[data49$gruppe==1]), 
        round(prop.table(table(desinfizieren.factor49[data49$gruppe==1])),4), 
        
        table(desinfizieren.factor.nachher49[data49$gruppe==1]), 
        round(prop.table(table(desinfizieren.factor.nachher49[data49$gruppe==1])),4),
        
        table(desinfizieren.factor49[data49$gruppe==0]), 
        round(prop.table(table(desinfizieren.factor49[data49$gruppe==0])),4), 
        
        table(desinfizieren.factor.nachher49[data49$gruppe==0]), 
        round(prop.table(table(desinfizieren.factor.nachher49[data49$gruppe==0])),4)
      )
)

# schutzbrille
rbind(f.8.table.labels, 
      cbind(
        table(schutzbrille.factor49[data49$gruppe==1]), 
        round(prop.table(table(schutzbrille.factor49[data49$gruppe==1])),4), 
        
        table(schutzbrille.factor.nachher49[data49$gruppe==1]), 
        round(prop.table(table(schutzbrille.factor.nachher49[data49$gruppe==1])),4),
        
        table(schutzbrille.factor49[data49$gruppe==0]), 
        round(prop.table(table(schutzbrille.factor49[data49$gruppe==0])),4), 
        
        table(schutzbrille.factor.nachher49[data49$gruppe==0]), 
        round(prop.table(table(schutzbrille.factor.nachher49[data49$gruppe==0])),4)
      )
)

# kleidung_wohnraum
rbind(f.8.table.labels, 
      cbind(
        table(kleidung_wohnraum.factor49[data49$gruppe==1]), 
        round(prop.table(table(kleidung_wohnraum.factor49[data49$gruppe==1])),4), 
        
        table(kleidung_wohnraum.factor.nachher49[data49$gruppe==1]), 
        round(prop.table(table(kleidung_wohnraum.factor.nachher49[data49$gruppe==1])),4),
        
        table(kleidung_wohnraum.factor49[data49$gruppe==0]), 
        round(prop.table(table(kleidung_wohnraum.factor49[data49$gruppe==0])),4), 
        
        table(kleidung_wohnraum.factor.nachher49[data49$gruppe==0]), 
        round(prop.table(table(kleidung_wohnraum.factor.nachher49[data49$gruppe==0])),4)
      )
)




# BEHAVIOR
# arbeitskleidung3_letzteWoche
rbind(f.8.table.labels, 
      cbind(
        table(arbeitskleidung3_letzteWoche.factor49[data49$gruppe==1]), 
        round(prop.table(table(arbeitskleidung3_letzteWoche.factor49[data49$gruppe==1])),4), 
        
        table(arbeitskleidung3_letzteWoche.factor.nachher49[data49$gruppe==1]), 
        round(prop.table(table(arbeitskleidung3_letzteWoche.factor.nachher49[data49$gruppe==1])),4),
        
        table(arbeitskleidung3_letzteWoche.factor49[data49$gruppe==0]), 
        round(prop.table(table(arbeitskleidung3_letzteWoche.factor49[data49$gruppe==0])),4), 
        
        table(arbeitskleidung3_letzteWoche.factor.nachher49[data49$gruppe==0]), 
        round(prop.table(table(arbeitskleidung3_letzteWoche.factor.nachher49[data49$gruppe==0])),4)
      )
)

# haare3_letzteWoche
rbind(f.8.table.labels, 
      cbind(
        table(haare3_letzteWoche.factor49[data49$gruppe==1]), 
        round(prop.table(table(haare3_letzteWoche.factor49[data49$gruppe==1])),4), 
        
        table(haare3_letzteWoche.factor.nachher49[data49$gruppe==1]), 
        round(prop.table(table(haare3_letzteWoche.factor.nachher49[data49$gruppe==1])),4),
        
        table(haare3_letzteWoche.factor49[data49$gruppe==0]), 
        round(prop.table(table(haare3_letzteWoche.factor49[data49$gruppe==0])),4), 
        
        table(haare3_letzteWoche.factor.nachher49[data49$gruppe==0]), 
        round(prop.table(table(haare3_letzteWoche.factor.nachher49[data49$gruppe==0])),4)
      )
)

# aufbewahrung3_letzteWoche
rbind(f.8.table.labels, 
      cbind(
        table(aufbewahrung3_letzteWoche.factor49[data49$gruppe==1]), 
        round(prop.table(table(aufbewahrung3_letzteWoche.factor49[data49$gruppe==1])),4), 
        
        table(aufbewahrung3_letzteWoche.factor.nachher49[data49$gruppe==1]), 
        round(prop.table(table(aufbewahrung3_letzteWoche.factor.nachher49[data49$gruppe==1])),4),
        
        table(aufbewahrung3_letzteWoche.factor49[data49$gruppe==0]), 
        round(prop.table(table(aufbewahrung3_letzteWoche.factor49[data49$gruppe==0])),4), 
        
        table(aufbewahrung3_letzteWoche.factor.nachher49[data49$gruppe==0]), 
        round(prop.table(table(aufbewahrung3_letzteWoche.factor.nachher49[data49$gruppe==0])),4)
      )
)

# arbeitskleidung3_intention
rbind(f.8.table.labels, 
      cbind(
        table(arbeitskleidung3_intention.factor49[data49$gruppe==1]), 
        round(prop.table(table(arbeitskleidung3_intention.factor49[data49$gruppe==1])),4), 
        
        table(arbeitskleidung3_intention.factor.nachher49[data49$gruppe==1]), 
        round(prop.table(table(arbeitskleidung3_intention.factor.nachher49[data49$gruppe==1])),4),
        
        table(arbeitskleidung3_intention.factor49[data49$gruppe==0]), 
        round(prop.table(table(arbeitskleidung3_intention.factor49[data49$gruppe==0])),4), 
        
        table(arbeitskleidung3_intention.factor.nachher49[data49$gruppe==0]), 
        round(prop.table(table(arbeitskleidung3_intention.factor.nachher49[data49$gruppe==0])),4)
      )
)

# haare3_intention
rbind(f.8.table.labels, 
      cbind(
        table(haare3_intention.factor49[data49$gruppe==1]), 
        round(prop.table(table(haare3_intention.factor49[data49$gruppe==1])),4), 
        
        table(haare3_intention.factor.nachher49[data49$gruppe==1]), 
        round(prop.table(table(haare3_intention.factor.nachher49[data49$gruppe==1])),4),
        
        table(haare3_intention.factor49[data49$gruppe==0]), 
        round(prop.table(table(haare3_intention.factor49[data49$gruppe==0])),4), 
        
        table(haare3_intention.factor.nachher49[data49$gruppe==0]), 
        round(prop.table(table(haare3_intention.factor.nachher49[data49$gruppe==0])),4)
      )
)

# aufbewahrung3_intention
rbind(f.8.table.labels, 
      cbind(
        table(aufbewahrung3_intention.factor49[data49$gruppe==1]), 
        round(prop.table(table(aufbewahrung3_intention.factor49[data49$gruppe==1])),4), 
        
        table(aufbewahrung3_intention.factor.nachher49[data49$gruppe==1]), 
        round(prop.table(table(aufbewahrung3_intention.factor.nachher49[data49$gruppe==1])),4),
        
        table(aufbewahrung3_intention.factor49[data49$gruppe==0]), 
        round(prop.table(table(aufbewahrung3_intention.factor49[data49$gruppe==0])),4), 
        
        table(aufbewahrung3_intention.factor.nachher49[data49$gruppe==0]), 
        round(prop.table(table(aufbewahrung3_intention.factor.nachher49[data49$gruppe==0])),4)
      )
)

# arbeitskleidung3_einstellung
rbind(f.8.table.labels, 
      cbind(
        table(arbeitskleidung3_einstellung.factor49[data49$gruppe==1]), 
        round(prop.table(table(arbeitskleidung3_einstellung.factor49[data49$gruppe==1])),4), 
        
        table(arbeitskleidung3_einstellung.factor.nachher49[data49$gruppe==1]), 
        round(prop.table(table(arbeitskleidung3_einstellung.factor.nachher49[data49$gruppe==1])),4),
        
        table(arbeitskleidung3_einstellung.factor49[data49$gruppe==0]), 
        round(prop.table(table(arbeitskleidung3_einstellung.factor49[data49$gruppe==0])),4), 
        
        table(arbeitskleidung3_einstellung.factor.nachher49[data49$gruppe==0]), 
        round(prop.table(table(arbeitskleidung3_einstellung.factor.nachher49[data49$gruppe==0])),4)
      )
)

# haare3_einstellung
rbind(f.8.table.labels, 
      cbind(
        table(haare3_einstellung.factor49[data49$gruppe==1]), 
        round(prop.table(table(haare3_einstellung.factor49[data49$gruppe==1])),4), 
        
        table(haare3_einstellung.factor.nachher49[data49$gruppe==1]), 
        round(prop.table(table(haare3_einstellung.factor.nachher49[data49$gruppe==1])),4),
        
        table(haare3_einstellung.factor49[data49$gruppe==0]), 
        round(prop.table(table(haare3_einstellung.factor49[data49$gruppe==0])),4), 
        
        table(haare3_einstellung.factor.nachher49[data49$gruppe==0]), 
        round(prop.table(table(haare3_einstellung.factor.nachher49[data49$gruppe==0])),4)
      )
)

# aufbewahrung3_einstellung
rbind(f.8.table.labels, 
      cbind(
        table(aufbewahrung3_einstellung.factor49[data49$gruppe==1]), 
        round(prop.table(table(aufbewahrung3_einstellung.factor49[data49$gruppe==1])),4), 
        
        table(aufbewahrung3_einstellung.factor.nachher49[data49$gruppe==1]), 
        round(prop.table(table(aufbewahrung3_einstellung.factor.nachher49[data49$gruppe==1])),4),
        
        table(aufbewahrung3_einstellung.factor49[data49$gruppe==0]), 
        round(prop.table(table(aufbewahrung3_einstellung.factor49[data49$gruppe==0])),4), 
        
        table(aufbewahrung3_einstellung.factor.nachher49[data49$gruppe==0]), 
        round(prop.table(table(aufbewahrung3_einstellung.factor.nachher49[data49$gruppe==0])),4)
      )
)

# arbeitskleidung3_norm
rbind(f.8.table.labels, 
      cbind(
        table(arbeitskleidung3_norm.factor49[data49$gruppe==1]), 
        round(prop.table(table(arbeitskleidung3_norm.factor49[data49$gruppe==1])),4), 
        
        table(arbeitskleidung3_norm.factor.nachher49[data49$gruppe==1]), 
        round(prop.table(table(arbeitskleidung3_norm.factor.nachher49[data49$gruppe==1])),4),
        
        table(arbeitskleidung3_norm.factor49[data49$gruppe==0]), 
        round(prop.table(table(arbeitskleidung3_norm.factor49[data49$gruppe==0])),4), 
        
        table(arbeitskleidung3_norm.factor.nachher49[data49$gruppe==0]), 
        round(prop.table(table(arbeitskleidung3_norm.factor.nachher49[data49$gruppe==0])),4)
      )
)

# haare3_norm
rbind(f.8.table.labels, 
      cbind(
        table(haare3_norm.factor49[data49$gruppe==1]), 
        round(prop.table(table(haare3_norm.factor49[data49$gruppe==1])),4), 
        
        table(haare3_norm.factor.nachher49[data49$gruppe==1]), 
        round(prop.table(table(haare3_norm.factor.nachher49[data49$gruppe==1])),4),
        
        table(haare3_norm.factor49[data49$gruppe==0]), 
        round(prop.table(table(haare3_norm.factor49[data49$gruppe==0])),4), 
        
        table(haare3_norm.factor.nachher49[data49$gruppe==0]), 
        round(prop.table(table(haare3_norm.factor.nachher49[data49$gruppe==0])),4)
      )
)

# aufbewahrung3_norm
rbind(f.8.table.labels, 
      cbind(
        table(aufbewahrung3_norm.factor49[data49$gruppe==1]), 
        round(prop.table(table(aufbewahrung3_norm.factor49[data49$gruppe==1])),4), 
        
        table(aufbewahrung3_norm.factor.nachher49[data49$gruppe==1]), 
        round(prop.table(table(aufbewahrung3_norm.factor.nachher49[data49$gruppe==1])),4),
        
        table(aufbewahrung3_norm.factor49[data49$gruppe==0]), 
        round(prop.table(table(aufbewahrung3_norm.factor49[data49$gruppe==0])),4), 
        
        table(aufbewahrung3_norm.factor.nachher49[data49$gruppe==0]), 
        round(prop.table(table(aufbewahrung3_norm.factor.nachher49[data49$gruppe==0])),4)
      )
)

# arbeitskleidung3_kontrolle
rbind(f.8.table.labels, 
      cbind(
        table(arbeitskleidung3_kontrolle.factor49[data49$gruppe==1]), 
        round(prop.table(table(arbeitskleidung3_kontrolle.factor49[data49$gruppe==1])),4), 
        
        table(arbeitskleidung3_kontrolle.factor.nachher49[data49$gruppe==1]), 
        round(prop.table(table(arbeitskleidung3_kontrolle.factor.nachher49[data49$gruppe==1])),4),
        
        table(arbeitskleidung3_kontrolle.factor49[data49$gruppe==0]), 
        round(prop.table(table(arbeitskleidung3_kontrolle.factor49[data49$gruppe==0])),4), 
        
        table(arbeitskleidung3_kontrolle.factor.nachher49[data49$gruppe==0]), 
        round(prop.table(table(arbeitskleidung3_kontrolle.factor.nachher49[data49$gruppe==0])),4)
      )
)

# haare3_kontrolle
rbind(f.8.table.labels, 
      cbind(
        table(haare3_kontrolle.factor49[data49$gruppe==1]), 
        round(prop.table(table(haare3_kontrolle.factor49[data49$gruppe==1])),4), 
        
        table(haare3_kontrolle.factor.nachher49[data49$gruppe==1]), 
        round(prop.table(table(haare3_kontrolle.factor.nachher49[data49$gruppe==1])),4),
        
        table(haare3_kontrolle.factor49[data49$gruppe==0]), 
        round(prop.table(table(haare3_kontrolle.factor49[data49$gruppe==0])),4), 
        
        table(haare3_kontrolle.factor.nachher49[data49$gruppe==0]), 
        round(prop.table(table(haare3_kontrolle.factor.nachher49[data49$gruppe==0])),4)
      )
)

# aufbewahrung3_kontrolle
rbind(f.8.table.labels, 
      cbind(
        table(aufbewahrung3_kontrolle.factor49[data49$gruppe==1]), 
        round(prop.table(table(aufbewahrung3_kontrolle.factor49[data49$gruppe==1])),4), 
        
        table(aufbewahrung3_kontrolle.factor.nachher49[data49$gruppe==1]), 
        round(prop.table(table(aufbewahrung3_kontrolle.factor.nachher49[data49$gruppe==1])),4),
        
        table(aufbewahrung3_kontrolle.factor49[data49$gruppe==0]), 
        round(prop.table(table(aufbewahrung3_kontrolle.factor49[data49$gruppe==0])),4), 
        
        table(aufbewahrung3_kontrolle.factor.nachher49[data49$gruppe==0]), 
        round(prop.table(table(aufbewahrung3_kontrolle.factor.nachher49[data49$gruppe==0])),4)
      )
)

#--------------------------------------#
# Comparisons: gruppe==1 vs. gruppe==0 #
#--------------------------------------#

# Sex # report as male?
rbind(sex.demog,cbind(table(sex.factor49[data49$gruppe==1]),round(prop.table(table(sex.factor49[data49$gruppe==1])),4), table(sex.factor49[data49$gruppe==0]),round(prop.table(table(sex.factor49[data49$gruppe==0])),4))) 
fisher.test(rbind(table(sex.factor49[data49$gruppe==1]),table(sex.factor49[data49$gruppe==0])))$p.value #[1] 0.6938127 # NS
sum(is.na(sex.factor49[data49$gruppe==1]))
sum(is.na(sex.factor49[data49$gruppe==0]))

# Age
rbind(age.demog.long,cbind(round(mean(data49$alter[data49$gruppe==1], na.rm=T),2),round(sd(data49$alter[data49$gruppe==1], na.rm = T),2), round(mean(data49$alter[data49$gruppe==0], na.rm=T),2),round(sd(data49$alter[data49$gruppe==0], na.rm = T),2)))
t.test(data49$alter[data49$gruppe==1], data49$alter[data49$gruppe==0])$p.value #[1] 0.8749513
sum(is.na(data49$alter[data49$gruppe==1]))
sum(is.na(data49$alter[data49$gruppe==0]))

rbind(age.demog.cat,cbind(table(alterkat.factor49[data49$gruppe==1]),round(prop.table(table(alterkat.factor49[data49$gruppe==1])),4), table(alterkat.factor49[data49$gruppe==0]),round(prop.table(table(alterkat.factor49[data49$gruppe==0])),4))) 
fisher.test(rbind(table(alterkat.factor49[data49$gruppe==1]),table(alterkat.factor49[data49$gruppe==0])))$p.value #[1] 0.5177544

# Studienort
rbind(studienort.demog,cbind(table(studienort.factor49[data49$gruppe==1]),round(prop.table(table(studienort.factor49[data49$gruppe==1])),4), table(studienort.factor49[data49$gruppe==0]),round(prop.table(table(studienort.factor49[data49$gruppe==0])),4))) 
fisher.test(rbind(table(studienort.factor49[data49$gruppe==1]),table(studienort.factor49[data49$gruppe==0])))$p.value #[1] 2.769542e-12 ******
sum(is.na(studienort.factor49[data49$gruppe==1]))
sum(is.na(studienort.factor49[data49$gruppe==0]))

# Schulabschluss
rbind(schulabschluss.demog,cbind(table(schulabschluss.factor49[data49$gruppe==1]),round(prop.table(table(schulabschluss.factor49[data49$gruppe==1])),4), table(schulabschluss.factor49[data49$gruppe==0]),round(prop.table(table(schulabschluss.factor49[data49$gruppe==0])),4))) 
fisher.test(rbind(table(schulabschluss.factor49[data49$gruppe==1]),table(schulabschluss.factor49[data49$gruppe==0])))$p.value #[1] 0.8307077
sum(is.na(schulabschluss.factor49[data49$gruppe==1]))
sum(is.na(schulabschluss.factor49[data49$gruppe==0]))

# Smoker
rbind(smoker.demog,cbind(table(smoking.status.factor49[data49$gruppe==1]),round(prop.table(table(smoking.status.factor49[data49$gruppe==1])),4), table(smoking.status.factor49[data49$gruppe==0]),round(prop.table(table(smoking.status.factor49[data49$gruppe==0])),4))) 
chisq.test(rbind(table(smoking.status.factor49[data49$gruppe==1]),table(smoking.status.factor49[data49$gruppe==0])))$p.value #[1] 1.00 # NS
sum(is.na(smoking.status.factor49[data49$gruppe==1]))
sum(is.na(smoking.status.factor49[data49$gruppe==0]))

# asthma3
rbind(sex.demog,cbind(table(asthma3.factor49[data49$gruppe==1]),round(prop.table(table(asthma3.factor49[data49$gruppe==1])),4), table(asthma3.factor49[data49$gruppe==0]),round(prop.table(table(asthma3.factor49[data49$gruppe==0])),4))) 
fisher.test(rbind(table(asthma3.factor49[data49$gruppe==1]),table(asthma3.factor49[data49$gruppe==0])))$p.value #[1] 0.3423276 # NS
sum(is.na(asthma3.factor49[data49$gruppe==1]))
sum(is.na(asthma3.factor49[data49$gruppe==0]))

# asthma.dx
rbind(sex.demog,cbind(table(asthma.dx.factor49[data49$gruppe==1]),round(prop.table(table(asthma.dx.factor49[data49$gruppe==1])),4), table(asthma.dx.factor49[data49$gruppe==0]),round(prop.table(table(asthma.dx.factor49[data49$gruppe==0])),4))) 
fisher.test(rbind(table(asthma.dx.factor49[data49$gruppe==1]),table(asthma.dx.factor49[data49$gruppe==0])))$p.value #[1] 0.01444241 # NS

# allergies
rbind(sex.demog,cbind(table(allergies.factor49[data49$gruppe==1]),round(prop.table(table(allergies.factor49[data49$gruppe==1])),4), table(allergies.factor49[data49$gruppe==0]),round(prop.table(table(allergies.factor49[data49$gruppe==0])),4))) 
chisq.test(rbind(table(allergies.factor49[data49$gruppe==1]),table(allergies.factor49[data49$gruppe==0])))$p.value #[1] 0.7254711 # NS
sum(is.na(allergies.factor49[data49$gruppe==1]))
sum(is.na(allergies.factor49[data49$gruppe==0]))

# par.asthma
rbind(sex.demog,cbind(table(par.asthma.factor49[data49$gruppe==1]),round(prop.table(table(par.asthma.factor49[data49$gruppe==1])),4), table(par.asthma.factor49[data49$gruppe==0]),round(prop.table(table(par.asthma.factor49[data49$gruppe==0])),4))) 
chisq.test(rbind(table(par.asthma.factor49[data49$gruppe==1]),table(par.asthma.factor49[data49$gruppe==0])))$p.value #[1] 0.43
sum(is.na(par.asthma.factor49[data49$gruppe==1]))
sum(is.na(par.asthma.factor49[data49$gruppe==0]))

