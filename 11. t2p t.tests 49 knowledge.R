#----

### KNOWLEDGE (WISSEN)
#----
### 1. Knowledge: haare
# mean dif control: [1] -0.23
round(mean(data49$haare[data49$gruppe==0], na.rm=T) -
        mean(data49$haare_nachher[data49$gruppe==0], na.rm=T),2)

# mean dif intervention: [1] -0.22
round(mean(data49$haare[data49$gruppe==1], na.rm=T) -
        mean(data49$haare_nachher[data49$gruppe==1], na.rm=T),2)

# median dif control: 
round(median(data49$haare[data49$gruppe==0], na.rm=T) -
        median(data49$haare_nachher[data49$gruppe==0], na.rm=T),2)

# median dif intervention: 
round(median(data49$haare[data49$gruppe==1], na.rm=T) -
        median(data49$haare_nachher[data49$gruppe==1], na.rm=T),2)

M.haare49 <- as.table(rbind(
  c(sum(data49$haare[data49$gruppe==1]), sum(data49$haare_nachher[data49$gruppe==1])),
  c(sum(data49$haare[data49$gruppe==0]), sum(data49$haare_nachher[data49$gruppe==0]))))
chisq.test(M.haare49)$p.value


### 2. Knowledge: arbeitsschuhe
# mean dif control: [1] -0.35
round(mean(data49$arbeitsschuhe[data49$gruppe==0], na.rm=T) -
        mean(data49$arbeitsschuhe_nachher[data49$gruppe==0], na.rm=T),2)

# mean dif intervention: [1] 0
round(mean(data49$arbeitsschuhe[data49$gruppe==1], na.rm=T) -
        mean(data49$arbeitsschuhe_nachher[data49$gruppe==1], na.rm=T),2)

# median dif control: [1] -0.35
round(median(data49$arbeitsschuhe[data49$gruppe==0], na.rm=T) -
        median(data49$arbeitsschuhe_nachher[data49$gruppe==0], na.rm=T),2)

# median dif intervention: [1] 0
round(median(data49$arbeitsschuhe[data49$gruppe==1], na.rm=T) -
        median(data49$arbeitsschuhe_nachher[data49$gruppe==1], na.rm=T),2)

M.arbeitsschuhe49 <- as.table(rbind(
  c(sum(data49$arbeitsschuhe[data49$gruppe==1]), sum(data49$arbeitsschuhe_nachher[data49$gruppe==1])),
  c(sum(data49$arbeitsschuhe[data49$gruppe==0]), sum(data49$arbeitsschuhe_nachher[data49$gruppe==0]))))
fisher.test(M.arbeitsschuhe49)$p.value


### 3. Knowledge: kleidung_aufb
# mean dif control: [1] -0.1
round(mean(data49$kleidung_aufb[data49$gruppe==0], na.rm=T) -
        mean(data49$kleidung_aufb_nachher[data49$gruppe==0], na.rm=T),2)

# mean dif intervention: [1] -0.06
round(mean(data49$kleidung_aufb[data49$gruppe==1], na.rm=T) -
        mean(data49$kleidung_aufb_nachher[data49$gruppe==1], na.rm=T),2)

# median dif control: [1] -0.1
round(median(data49$kleidung_aufb[data49$gruppe==0], na.rm=T) -
        median(data49$kleidung_aufb_nachher[data49$gruppe==0], na.rm=T),2)

# median dif intervention: [1] -0.06
round(median(data49$kleidung_aufb[data49$gruppe==1], na.rm=T) -
        median(data49$kleidung_aufb_nachher[data49$gruppe==1], na.rm=T),2)

M.kleidung_aufb49 <- as.table(rbind(
  c(sum(data49$kleidung_aufb[data49$gruppe==1]), sum(data49$kleidung_aufb_nachher[data49$gruppe==1])),
  c(sum(data49$kleidung_aufb[data49$gruppe==0]), sum(data49$kleidung_aufb_nachher[data49$gruppe==0]))))
chisq.test(M.kleidung_aufb49)$p.value


### Knowledge: desinfizieren
# mean dif control: [1] -0.45
round(mean(data49$desinfizieren[data49$gruppe==0], na.rm=T) -
        mean(data49$desinfizieren_nachher[data49$gruppe==0], na.rm=T),2)

# mean dif intervention: [1] -0.33
round(mean(data49$desinfizieren[data49$gruppe==1], na.rm=T) -
        mean(data49$desinfizieren_nachher[data49$gruppe==1], na.rm=T),2)

# median dif control: [1] -0.45
round(median(data49$desinfizieren[data49$gruppe==0], na.rm=T) -
        median(data49$desinfizieren_nachher[data49$gruppe==0], na.rm=T),2)

# median dif intervention: [1] -0.33
round(median(data49$desinfizieren[data49$gruppe==1], na.rm=T) -
        median(data49$desinfizieren_nachher[data49$gruppe==1], na.rm=T),2)

M.desinfizieren49 <- as.table(rbind(
  c(sum(data49$desinfizieren[data49$gruppe==1]), sum(data49$desinfizieren_nachher[data49$gruppe==1])),
  c(sum(data49$desinfizieren[data49$gruppe==0]), sum(data49$desinfizieren_nachher[data49$gruppe==0]))))
chisq.test(M.desinfizieren49)$p.value 


### Knowledge: schutzbrille
# mean dif control: [1] -0.19
round(mean(data49$schutzbrille[data49$gruppe==0], na.rm=T) -
        mean(data49$schutzbrille_nachher[data49$gruppe==0], na.rm=T),2)

# mean dif intervention: [1] 0
round(mean(data49$schutzbrille[data49$gruppe==1], na.rm=T) -
        mean(data49$schutzbrille_nachher[data49$gruppe==1], na.rm=T),2)

# median dif control: [1] -0.19
round(median(data49$schutzbrille[data49$gruppe==0], na.rm=T) -
        median(data49$schutzbrille_nachher[data49$gruppe==0], na.rm=T),2)

# median dif intervention: [1] 0
round(median(data49$schutzbrille[data49$gruppe==1], na.rm=T) -
        median(data49$schutzbrille_nachher[data49$gruppe==1], na.rm=T),2)

M.schutzbrille49 <- as.table(rbind(
  c(sum(data49$schutzbrille[data49$gruppe==1]), sum(data49$schutzbrille_nachher[data49$gruppe==1])),
  c(sum(data49$schutzbrille[data49$gruppe==0]), sum(data49$schutzbrille_nachher[data49$gruppe==0]))))
fisher.test(M.schutzbrille49)$p.value


### Knowledge: kleidung_wohnraum
# mean dif control: [1] -0.03
round(mean(data49$kleidung_wohnraum[data49$gruppe==0], na.rm=T) -
        mean(data49$kleidung_wohnraum_nachher[data49$gruppe==0], na.rm=T),2)

# mean dif intervention: [1] -0.22
round(mean(data49$kleidung_wohnraum[data49$gruppe==1], na.rm=T) -
        mean(data49$kleidung_wohnraum_nachher[data49$gruppe==1], na.rm=T),2)

# median dif control: [1] -0.03
round(median(data49$kleidung_wohnraum[data49$gruppe==0], na.rm=T) -
        median(data49$kleidung_wohnraum_nachher[data49$gruppe==0], na.rm=T),2)

# median dif intervention: [1] -0.22
round(median(data49$kleidung_wohnraum[data49$gruppe==1], na.rm=T) -
        median(data49$kleidung_wohnraum_nachher[data49$gruppe==1], na.rm=T),2)

M.kleidung_wohnraum49 <- as.table(rbind(
  c(sum(data49$kleidung_wohnraum[data49$gruppe==1]), sum(data49$kleidung_wohnraum_nachher[data49$gruppe==1])),
  c(sum(data49$kleidung_wohnraum[data49$gruppe==0]), sum(data49$kleidung_wohnraum_nachher[data49$gruppe==0]))))
chisq.test(M.kleidung_wohnraum49)$p.value

#---------------#
# END OF SCRIPT #
#---------------#


