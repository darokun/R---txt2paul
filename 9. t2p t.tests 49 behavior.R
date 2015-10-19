#' ---
#' title: "txt2paul t-test on behavior between control and intervention groups"
#' author: "Daloha Rodriguez-Molina"
#' date: "April 20th, 2015"
#' ---

#-------------------#
# Main data used: txt2PAUL_Gesamt_vers1_130115.csv
# Name given to this main dataset: data
#
# Subset dataset used on this script: data49
#
# Contents:
# I.   Preliminary stuff (reading data, creating subsets, etc.) 
#      Lines 22 - 54
# II.  Individual and detailed calculations by variable (Only look if details are needed)
#      Lines 59 - 305
# III. As a nice table (Recommended)
#      Lines 307 - 880
#-------------------#

#----------------------#
# I. Preliminary stuff #
#----------------------#
#----
# Read the data
# d <- "/Users/daro/Desktop/txt2PAUL/z_Dokumente fuer Daloha/txt2PAUL_Gesamt_vers1_130115.csv"
# data <- read.csv(d, header=T)
#
# Subsets:
# Zusammensetzung der Studienpopulation: (Abbildung 2 in Abschlussbericht document)
nrow(data) # total n = 238
sum(data$Ausgeschlossen==1) # 119
sum(data$Ausgeschlossen==0) # 119
sum(data$Absage==1) # 2
sum(data$Ausgeschlossen==0) - sum(data$Absage==1) # 117 (eingeschlossen)

# new dataset with n=117 from those with follow-up info
data117 <- data[data$intervention==1,]
data117 <- data117[data117$Absage==0,]
nrow(data117) # 117

# new dataset with n=49 from those who have info "before + after"
data49 <- data117[!is.na(data117$haare_nachher),] 
nrow(data49) # 49
# how many out of the 49 are in which intervention group
sum(data49$gruppe==1) # 18 in the intervention group
sum(data49$gruppe==0) # 31 in the control group
#
#-------------------#


#----
#-----------------------------------------------------------#
# replication intent of table 1 from the HealthCom abstract #
#-----------------------------------------------------------#

#------------------------------------------------------#
# II. Individual and detailed calculations by variable #
#------------------------------------------------------#
#----
### BEHAVIOR

### 1. dif. last week's behavior: workwear. 
# mean dif control: 
round(mean(data49$arbeitskleidung_letzteWoche[data49$gruppe==0], na.rm=T) -
        mean(data49$arbeitskleidung_letzteWoche_nach[data49$gruppe==0], na.rm=T),2)

# mean dif intervention:
round(mean(data49$arbeitskleidung_letzteWoche[data49$gruppe==1], na.rm=T) -
        mean(data49$arbeitskleidung_letzteWoche_nach[data49$gruppe==1], na.rm=T),2)

t.test(
  data49$arbeitskleidung_letzteWoche[data49$gruppe==0] -
    data49$arbeitskleidung_letzteWoche_nach[data49$gruppe==0],
  data49$arbeitskleidung_letzteWoche[data49$gruppe==1] -
    data49$arbeitskleidung_letzteWoche_nach[data49$gruppe==1])



### 2. dif. last week's behavior: hair. 
# mean dif control: 
round(mean(data49$haare_letzteWoche[data49$gruppe==0], na.rm=T) -
        mean(data49$haare_letzteWoche_nach[data49$gruppe==0], na.rm=T),2)

# mean dif intervention: 
round(mean(data49$haare_letzteWoche[data49$gruppe==1], na.rm=T) -
        mean(data49$haare_letzteWoche_nach[data49$gruppe==1], na.rm=T),2)

t.test(
  data49$haare_letzteWoche[data49$gruppe==0] -
    data49$haare_letzteWoche_nach[data49$gruppe==0],
  data49$haare_letzteWoche[data49$gruppe==1] -
    data49$haare_letzteWoche_nach[data49$gruppe==1])



### 3. dif. last week's behavior: storage. 
# mean dif control: 
round(mean(data49$aufbewahrung_letzteWoche[data49$gruppe==0], na.rm=T) -
        mean(data49$aufbewahrung_letzteWoche_nach[data49$gruppe==0], na.rm=T),2)

# mean dif intervention:
round(mean(data49$aufbewahrung_letzteWoche[data49$gruppe==1], na.rm=T) -
        mean(data49$aufbewahrung_letzteWoche_nach[data49$gruppe==1], na.rm=T),2)

t.test(
  data49$aufbewahrung_letzteWoche[data49$gruppe==0] -
    data49$aufbewahrung_letzteWoche_nach[data49$gruppe==0],
  data49$aufbewahrung_letzteWoche[data49$gruppe==1] -
    data49$aufbewahrung_letzteWoche_nach[data49$gruppe==1])


### 4. dif. intention behavior: workwear. 
# mean dif control: 
round(mean(data49$arbeitskleidung_intention[data49$gruppe==0], na.rm=T) -
        mean(data49$arbeitskleidung_intention_nach[data49$gruppe==0], na.rm=T),2)

# mean dif intervention: 
round(mean(data49$arbeitskleidung_intention[data49$gruppe==1], na.rm=T) -
        mean(data49$arbeitskleidung_intention_nach[data49$gruppe==1], na.rm=T),2)

t.test(
  data49$arbeitskleidung_intention[data49$gruppe==0] -
    data49$arbeitskleidung_intention_nach[data49$gruppe==0],
  data49$arbeitskleidung_intention[data49$gruppe==1] -
    data49$arbeitskleidung_intention_nach[data49$gruppe==1])


### 5. dif. intention behavior: hair. 
# mean dif control: 
round(mean(data49$haare_intention[data49$gruppe==0], na.rm=T) -
        mean(data49$haare_intention_nach[data49$gruppe==0], na.rm=T),2)

# mean dif intervention: 
round(mean(data49$haare_intention[data49$gruppe==1], na.rm=T) -
        mean(data49$haare_intention_nach[data49$gruppe==1], na.rm=T),2)

t.test(
  data49$haare_intention[data49$gruppe==0] -
    data49$haare_intention_nach[data49$gruppe==0],
  data49$haare_intention[data49$gruppe==1] -
    data49$haare_intention_nach[data49$gruppe==1])


### 6. dif. intention behavior: storage. 
# mean dif control: 
round(mean(data49$aufbewahrung_intention[data49$gruppe==0], na.rm=T) -
        mean(data49$aufbewahrung_intention_nach[data49$gruppe==0], na.rm=T),2)

# mean dif intervention: 
round(mean(data49$aufbewahrung_intention[data49$gruppe==1], na.rm=T) -
        mean(data49$aufbewahrung_intention_nach[data49$gruppe==1], na.rm=T),2)

t.test(
  data49$aufbewahrung_intention[data49$gruppe==0] -
    data49$aufbewahrung_intention_nach[data49$gruppe==0],
  data49$aufbewahrung_intention[data49$gruppe==1] -
    data49$aufbewahrung_intention_nach[data49$gruppe==1])


### 7. dif. attitude behavior: workwear. 
# mean dif control: 
round(mean(data49$arbeitskleidung_einstellung[data49$gruppe==0], na.rm=T) -
        mean(data49$arbeitskleidung_einstellung_nach[data49$gruppe==0], na.rm=T),2)

# mean dif intervention: 
round(mean(data49$arbeitskleidung_einstellung[data49$gruppe==1], na.rm=T) -
        mean(data49$arbeitskleidung_einstellung_nach[data49$gruppe==1], na.rm=T),2)

t.test(
  data49$arbeitskleidung_einstellung[data49$gruppe==0] -
    data49$arbeitskleidung_einstellung_nach[data49$gruppe==0],
  data49$arbeitskleidung_einstellung[data49$gruppe==1] -
    data49$arbeitskleidung_einstellung_nach[data49$gruppe==1])


### 8. dif. attitude behavior: hair. 
# mean dif control: 
round(mean(data49$haare_einstellung[data49$gruppe==0], na.rm=T) -
        mean(data49$haare_einstellung_nach[data49$gruppe==0], na.rm=T),2)

# mean dif intervention: 
round(mean(data49$haare_einstellung[data49$gruppe==1], na.rm=T) -
        mean(data49$haare_einstellung_nach[data49$gruppe==1], na.rm=T),2)

t.test(
  data49$haare_einstellung[data49$gruppe==0] -
    data49$haare_einstellung_nach[data49$gruppe==0],
  data49$haare_einstellung[data49$gruppe==1] -
    data49$haare_einstellung_nach[data49$gruppe==1])


### 9. dif. attitude behavior: storage. 
# mean dif control: 
round(mean(data49$aufbewahrung_einstellung[data49$gruppe==0], na.rm=T) -
        mean(data49$aufbewahrung_einstellung_nach[data49$gruppe==0], na.rm=T),2)

# mean dif intervention:
round(mean(data49$aufbewahrung_einstellung[data49$gruppe==1], na.rm=T) -
        mean(data49$aufbewahrung_einstellung_nach[data49$gruppe==1], na.rm=T),2)

t.test(
  data49$aufbewahrung_einstellung[data49$gruppe==0] -
    data49$aufbewahrung_einstellung_nach[data49$gruppe==0],
  data49$aufbewahrung_einstellung[data49$gruppe==1] -
    data49$aufbewahrung_einstellung_nach[data49$gruppe==1])


### 10. dif. peer behavior: workwear.
# mean dif control: 
round(mean(data49$arbeitskleidung_norm[data49$gruppe==0], na.rm=T) -
        mean(data49$arbeitskleidung_norm_nach[data49$gruppe==0], na.rm=T),2)

# mean dif intervention: 
round(mean(data49$arbeitskleidung_norm[data49$gruppe==1], na.rm=T) -
        mean(data49$arbeitskleidung_norm_nach[data49$gruppe==1], na.rm=T),2)

t.test(
  data49$arbeitskleidung_norm[data49$gruppe==0] -
    data49$arbeitskleidung_norm_nach[data49$gruppe==0],
  data49$arbeitskleidung_norm[data49$gruppe==1] -
    data49$arbeitskleidung_norm_nach[data49$gruppe==1])


### 11. dif. peer behavior: hair. 
# mean dif control:
round(mean(data49$haare_norm[data49$gruppe==0], na.rm=T) -
        mean(data49$haare_norm_nach[data49$gruppe==0], na.rm=T),2)

# mean dif intervention: 
round(mean(data49$haare_norm[data49$gruppe==1], na.rm=T) -
        mean(data49$haare_norm_nach[data49$gruppe==1], na.rm=T),2)

t.test(
  data49$haare_norm[data49$gruppe==0] -
    data49$haare_norm_nach[data49$gruppe==0],
  data49$haare_norm[data49$gruppe==1] -
    data49$haare_norm_nach[data49$gruppe==1])


### 12. dif. peer behavior: storage. 
# mean dif control: 
round(mean(data49$aufbewahrung_norm[data49$gruppe==0], na.rm=T) -
        mean(data49$aufbewahrung_norm_nach[data49$gruppe==0], na.rm=T),2)

# mean dif intervention:
round(mean(data49$aufbewahrung_norm[data49$gruppe==1], na.rm=T) -
        mean(data49$aufbewahrung_norm_nach[data49$gruppe==1], na.rm=T),2)

t.test(
  data49$aufbewahrung_norm[data49$gruppe==0] -
    data49$aufbewahrung_norm_nach[data49$gruppe==0],
  data49$aufbewahrung_norm[data49$gruppe==1] -
    data49$aufbewahrung_norm_nach[data49$gruppe==1])


### 13. dif. control behavior: workwear. 
# mean dif control: 
round(mean(data49$arbeitskleidung_kontrolle[data49$gruppe==0], na.rm=T) -
        mean(data49$arbeitskleidung_kontrolle_nach[data49$gruppe==0], na.rm=T),2)

# mean dif intervention:
round(mean(data49$arbeitskleidung_kontrolle[data49$gruppe==1], na.rm=T) -
        mean(data49$arbeitskleidung_kontrolle_nach[data49$gruppe==1], na.rm=T),2)

t.test(
  data49$arbeitskleidung_kontrolle[data49$gruppe==0] -
    data49$arbeitskleidung_kontrolle_nach[data49$gruppe==0],
  data49$arbeitskleidung_kontrolle[data49$gruppe==1] -
    data49$arbeitskleidung_kontrolle_nach[data49$gruppe==1])


### 14. dif. control behavior: hair. 
# mean dif control: 
round(mean(data49$haare_kontrolle[data49$gruppe==0], na.rm=T) -
        mean(data49$haare_kontrolle_nach[data49$gruppe==0], na.rm=T),2)

# mean dif intervention: 
round(mean(data49$haare_kontrolle[data49$gruppe==1], na.rm=T) -
        mean(data49$haare_kontrolle_nach[data49$gruppe==1], na.rm=T),2)

t.test(
  data49$haare_kontrolle[data49$gruppe==0] -
    data49$haare_kontrolle_nach[data49$gruppe==0],
  data49$haare_kontrolle[data49$gruppe==1] -
    data49$haare_kontrolle_nach[data49$gruppe==1])


### 15. dif. control behavior: storage. 
# mean dif control: 
round(mean(data49$aufbewahrung_kontrolle[data49$gruppe==0], na.rm=T) -
        mean(data49$aufbewahrung_kontrolle_nach[data49$gruppe==0], na.rm=T),2)

# mean dif intervention: 
round(mean(data49$aufbewahrung_kontrolle[data49$gruppe==1], na.rm=T) -
        mean(data49$aufbewahrung_kontrolle_nach[data49$gruppe==1], na.rm=T),2)

t.test(
  data49$aufbewahrung_kontrolle[data49$gruppe==0] -
    data49$aufbewahrung_kontrolle_nach[data49$gruppe==0],
  data49$aufbewahrung_kontrolle[data49$gruppe==1] -
    data49$aufbewahrung_kontrolle_nach[data49$gruppe==1])
#----

#----------------------#
# III. As a nice table #
#----------------------#
#----
name.labels <- c("Variable", "dif. Behavior: workwear (R)", "dif. Behavior: hair", "dif. Behavior storage", "dif. Behavioral intention: workwear (R)", "dif. Behavioral intention: hair", "dif. Behavioral intention: storage", "dif. Attitude: workwear (R)", "dif. Attitude: hair", "dif. Attitude: storage", "dif. Norm: workwear (R)", "dif. Norm: hair", "dif. Norm: storage", "dif. Perceived behavioral control: workwear (R)", "dif. Perceived behavioral control: hair", "dif. Perceived behavioral control: storage")

control.means <- c(
  round(mean(data49$arbeitskleidung_letzteWoche[data49$gruppe==0], na.rm=T) -
          mean(data49$arbeitskleidung_letzteWoche_nach[data49$gruppe==0], na.rm=T),2),
  
  round(mean(data49$haare_letzteWoche[data49$gruppe==0], na.rm=T) -
          mean(data49$haare_letzteWoche_nach[data49$gruppe==0], na.rm=T),2),
  
  round(mean(data49$aufbewahrung_letzteWoche[data49$gruppe==0], na.rm=T) -
          mean(data49$aufbewahrung_letzteWoche_nach[data49$gruppe==0], na.rm=T),2),
  
  round(mean(data49$arbeitskleidung_intention[data49$gruppe==0], na.rm=T) -
          mean(data49$arbeitskleidung_intention_nach[data49$gruppe==0], na.rm=T),2),
  
  round(mean(data49$haare_intention[data49$gruppe==0], na.rm=T) -
          mean(data49$haare_intention_nach[data49$gruppe==0], na.rm=T),2),
  
  round(mean(data49$aufbewahrung_intention[data49$gruppe==0], na.rm=T) -
          mean(data49$aufbewahrung_intention_nach[data49$gruppe==0], na.rm=T),2),
  
  round(mean(data49$arbeitskleidung_einstellung[data49$gruppe==0], na.rm=T) -
          mean(data49$arbeitskleidung_einstellung_nach[data49$gruppe==0], na.rm=T),2),
  
  round(mean(data49$haare_einstellung[data49$gruppe==0], na.rm=T) -
          mean(data49$haare_einstellung_nach[data49$gruppe==0], na.rm=T),2),
  
  round(mean(data49$aufbewahrung_einstellung[data49$gruppe==0], na.rm=T) -
          mean(data49$aufbewahrung_einstellung_nach[data49$gruppe==0], na.rm=T),2),
  
  round(mean(data49$arbeitskleidung_norm[data49$gruppe==0], na.rm=T) -
          mean(data49$arbeitskleidung_norm_nach[data49$gruppe==0], na.rm=T),2),
  
  round(mean(data49$haare_norm[data49$gruppe==0], na.rm=T) -
          mean(data49$haare_norm_nach[data49$gruppe==0], na.rm=T),2),
  
  round(mean(data49$aufbewahrung_norm[data49$gruppe==0], na.rm=T) -
          mean(data49$aufbewahrung_norm_nach[data49$gruppe==0], na.rm=T),2),
  
  round(mean(data49$arbeitskleidung_kontrolle[data49$gruppe==0], na.rm=T) -
          mean(data49$arbeitskleidung_kontrolle_nach[data49$gruppe==0], na.rm=T),2),
  
  round(mean(data49$haare_kontrolle[data49$gruppe==0], na.rm=T) -
          mean(data49$haare_kontrolle_nach[data49$gruppe==0], na.rm=T),2),
  
  round(mean(data49$aufbewahrung_kontrolle[data49$gruppe==0], na.rm=T) -
          mean(data49$aufbewahrung_kontrolle_nach[data49$gruppe==0], na.rm=T),2)
)

intervention.means <- c(
  round(mean(data49$arbeitskleidung_letzteWoche[data49$gruppe==1], na.rm=T) -
          mean(data49$arbeitskleidung_letzteWoche_nach[data49$gruppe==1], na.rm=T),2),
  
  round(mean(data49$haare_letzteWoche[data49$gruppe==1], na.rm=T) -
          mean(data49$haare_letzteWoche_nach[data49$gruppe==1], na.rm=T),2),
  
  round(mean(data49$aufbewahrung_letzteWoche[data49$gruppe==1], na.rm=T) -
          mean(data49$aufbewahrung_letzteWoche_nach[data49$gruppe==1], na.rm=T),2),
  
  round(mean(data49$arbeitskleidung_intention[data49$gruppe==1], na.rm=T) -
          mean(data49$arbeitskleidung_intention_nach[data49$gruppe==1], na.rm=T),2),
  
  round(mean(data49$haare_intention[data49$gruppe==1], na.rm=T) -
          mean(data49$haare_intention_nach[data49$gruppe==1], na.rm=T),2),
  
  round(mean(data49$aufbewahrung_intention[data49$gruppe==1], na.rm=T) -
          mean(data49$aufbewahrung_intention_nach[data49$gruppe==1], na.rm=T),2),
  
  round(mean(data49$arbeitskleidung_einstellung[data49$gruppe==1], na.rm=T) -
          mean(data49$arbeitskleidung_einstellung_nach[data49$gruppe==1], na.rm=T),2),
  
  round(mean(data49$haare_einstellung[data49$gruppe==1], na.rm=T) -
          mean(data49$haare_einstellung_nach[data49$gruppe==1], na.rm=T),2),
  
  round(mean(data49$aufbewahrung_einstellung[data49$gruppe==1], na.rm=T) -
          mean(data49$aufbewahrung_einstellung_nach[data49$gruppe==1], na.rm=T),2),
  
  round(mean(data49$arbeitskleidung_norm[data49$gruppe==1], na.rm=T) -
          mean(data49$arbeitskleidung_norm_nach[data49$gruppe==1], na.rm=T),2),
  
  round(mean(data49$haare_norm[data49$gruppe==1], na.rm=T) -
          mean(data49$haare_norm_nach[data49$gruppe==1], na.rm=T),2),
  
  round(mean(data49$aufbewahrung_norm[data49$gruppe==1], na.rm=T) -
          mean(data49$aufbewahrung_norm_nach[data49$gruppe==1], na.rm=T),2),
  
  round(mean(data49$arbeitskleidung_kontrolle[data49$gruppe==1], na.rm=T) -
          mean(data49$arbeitskleidung_kontrolle_nach[data49$gruppe==1], na.rm=T),2),
  
  round(mean(data49$haare_kontrolle[data49$gruppe==1], na.rm=T) -
          mean(data49$haare_kontrolle_nach[data49$gruppe==1], na.rm=T),2),
  
  round(mean(data49$aufbewahrung_kontrolle[data49$gruppe==1], na.rm=T) -
          mean(data49$aufbewahrung_kontrolle_nach[data49$gruppe==1], na.rm=T),2)
)

t.values <- c(
  round(t.test(
    data49$arbeitskleidung_letzteWoche[data49$gruppe==0] -
      data49$arbeitskleidung_letzteWoche_nach[data49$gruppe==0],
    data49$arbeitskleidung_letzteWoche[data49$gruppe==1] -
      data49$arbeitskleidung_letzteWoche_nach[data49$gruppe==1])$statistic,2),
  
  round(t.test(
    data49$haare_letzteWoche[data49$gruppe==0] -
      data49$haare_letzteWoche_nach[data49$gruppe==0],
    data49$haare_letzteWoche[data49$gruppe==1] -
      data49$haare_letzteWoche_nach[data49$gruppe==1])$statistic,2),
  
  round(t.test(
    data49$aufbewahrung_letzteWoche[data49$gruppe==0] -
      data49$aufbewahrung_letzteWoche_nach[data49$gruppe==0],
    data49$aufbewahrung_letzteWoche[data49$gruppe==1] -
      data49$aufbewahrung_letzteWoche_nach[data49$gruppe==1])$statistic,2),
  
  round(t.test(
    data49$arbeitskleidung_intention[data49$gruppe==0] -
      data49$arbeitskleidung_intention_nach[data49$gruppe==0],
    data49$arbeitskleidung_intention[data49$gruppe==1] -
      data49$arbeitskleidung_intention_nach[data49$gruppe==1])$statistic,2),
  
  round(t.test(
    data49$haare_intention[data49$gruppe==0] -
      data49$haare_intention_nach[data49$gruppe==0],
    data49$haare_intention[data49$gruppe==1] -
      data49$haare_intention_nach[data49$gruppe==1])$statistic,2),
  
  round(t.test(
    data49$aufbewahrung_intention[data49$gruppe==0] -
      data49$aufbewahrung_intention_nach[data49$gruppe==0],
    data49$aufbewahrung_intention[data49$gruppe==1] -
      data49$aufbewahrung_intention_nach[data49$gruppe==1])$statistic,2),
  
  round(t.test(
    data49$arbeitskleidung_einstellung[data49$gruppe==0] -
      data49$arbeitskleidung_einstellung_nach[data49$gruppe==0],
    data49$arbeitskleidung_einstellung[data49$gruppe==1] -
      data49$arbeitskleidung_einstellung_nach[data49$gruppe==1])$statistic,2),
  
  round(t.test(
    data49$haare_einstellung[data49$gruppe==0] -
      data49$haare_einstellung_nach[data49$gruppe==0],
    data49$haare_einstellung[data49$gruppe==1] -
      data49$haare_einstellung_nach[data49$gruppe==1])$statistic,2),
  
  round(t.test(
    data49$aufbewahrung_einstellung[data49$gruppe==0] -
      data49$aufbewahrung_einstellung_nach[data49$gruppe==0],
    data49$aufbewahrung_einstellung[data49$gruppe==1] -
      data49$aufbewahrung_einstellung_nach[data49$gruppe==1])$statistic,2),
  
  round(t.test(
    data49$arbeitskleidung_norm[data49$gruppe==0] -
      data49$arbeitskleidung_norm_nach[data49$gruppe==0],
    data49$arbeitskleidung_norm[data49$gruppe==1] -
      data49$arbeitskleidung_norm_nach[data49$gruppe==1])$statistic,2),
  
  round(t.test(
    data49$haare_norm[data49$gruppe==0] -
      data49$haare_norm_nach[data49$gruppe==0],
    data49$haare_norm[data49$gruppe==1] -
      data49$haare_norm_nach[data49$gruppe==1])$statistic,2),
  
  round(t.test(
    data49$aufbewahrung_norm[data49$gruppe==0] -
      data49$aufbewahrung_norm_nach[data49$gruppe==0],
    data49$aufbewahrung_norm[data49$gruppe==1] -
      data49$aufbewahrung_norm_nach[data49$gruppe==1])$statistic,2),
  
  round(t.test(
    data49$arbeitskleidung_kontrolle[data49$gruppe==0] -
      data49$arbeitskleidung_kontrolle_nach[data49$gruppe==0],
    data49$arbeitskleidung_kontrolle[data49$gruppe==1] -
      data49$arbeitskleidung_kontrolle_nach[data49$gruppe==1])$statistic,2),
  
  round(t.test(
    data49$haare_kontrolle[data49$gruppe==0] -
      data49$haare_kontrolle_nach[data49$gruppe==0],
    data49$haare_kontrolle[data49$gruppe==1] -
      data49$haare_kontrolle_nach[data49$gruppe==1])$statistic,2),
  
  round(t.test(
    data49$aufbewahrung_kontrolle[data49$gruppe==0] -
      data49$aufbewahrung_kontrolle_nach[data49$gruppe==0],
    data49$aufbewahrung_kontrolle[data49$gruppe==1] -
      data49$aufbewahrung_kontrolle_nach[data49$gruppe==1])$statistic,2)
)

df.values <- c(
  round(t.test(
    data49$arbeitskleidung_letzteWoche[data49$gruppe==0] -
      data49$arbeitskleidung_letzteWoche_nach[data49$gruppe==0],
    data49$arbeitskleidung_letzteWoche[data49$gruppe==1] -
      data49$arbeitskleidung_letzteWoche_nach[data49$gruppe==1])$parameter,3),
  
  round(t.test(
    data49$haare_letzteWoche[data49$gruppe==0] -
      data49$haare_letzteWoche_nach[data49$gruppe==0],
    data49$haare_letzteWoche[data49$gruppe==1] -
      data49$haare_letzteWoche_nach[data49$gruppe==1])$parameter,3),
  
  round(t.test(
    data49$aufbewahrung_letzteWoche[data49$gruppe==0] -
      data49$aufbewahrung_letzteWoche_nach[data49$gruppe==0],
    data49$aufbewahrung_letzteWoche[data49$gruppe==1] -
      data49$aufbewahrung_letzteWoche_nach[data49$gruppe==1])$parameter,3),
  
  round(t.test(
    data49$arbeitskleidung_intention[data49$gruppe==0] -
      data49$arbeitskleidung_intention_nach[data49$gruppe==0],
    data49$arbeitskleidung_intention[data49$gruppe==1] -
      data49$arbeitskleidung_intention_nach[data49$gruppe==1])$parameter,3),
  
  round(t.test(
    data49$haare_intention[data49$gruppe==0] -
      data49$haare_intention_nach[data49$gruppe==0],
    data49$haare_intention[data49$gruppe==1] -
      data49$haare_intention_nach[data49$gruppe==1])$parameter,3),
  
  round(t.test(
    data49$aufbewahrung_intention[data49$gruppe==0] -
      data49$aufbewahrung_intention_nach[data49$gruppe==0],
    data49$aufbewahrung_intention[data49$gruppe==1] -
      data49$aufbewahrung_intention_nach[data49$gruppe==1])$parameter,3),
  
  round(t.test(
    data49$arbeitskleidung_einstellung[data49$gruppe==0] -
      data49$arbeitskleidung_einstellung_nach[data49$gruppe==0],
    data49$arbeitskleidung_einstellung[data49$gruppe==1] -
      data49$arbeitskleidung_einstellung_nach[data49$gruppe==1])$parameter,3),
  
  round(t.test(
    data49$haare_einstellung[data49$gruppe==0] -
      data49$haare_einstellung_nach[data49$gruppe==0],
    data49$haare_einstellung[data49$gruppe==1] -
      data49$haare_einstellung_nach[data49$gruppe==1])$parameter,3),
  
  round(t.test(
    data49$aufbewahrung_einstellung[data49$gruppe==0] -
      data49$aufbewahrung_einstellung_nach[data49$gruppe==0],
    data49$aufbewahrung_einstellung[data49$gruppe==1] -
      data49$aufbewahrung_einstellung_nach[data49$gruppe==1])$parameter,3),
  
  round(t.test(
    data49$arbeitskleidung_norm[data49$gruppe==0] -
      data49$arbeitskleidung_norm_nach[data49$gruppe==0],
    data49$arbeitskleidung_norm[data49$gruppe==1] -
      data49$arbeitskleidung_norm_nach[data49$gruppe==1])$parameter,3),
  
  round(t.test(
    data49$haare_norm[data49$gruppe==0] -
      data49$haare_norm_nach[data49$gruppe==0],
    data49$haare_norm[data49$gruppe==1] -
      data49$haare_norm_nach[data49$gruppe==1])$parameter,3),
  
  round(t.test(
    data49$aufbewahrung_norm[data49$gruppe==0] -
      data49$aufbewahrung_norm_nach[data49$gruppe==0],
    data49$aufbewahrung_norm[data49$gruppe==1] -
      data49$aufbewahrung_norm_nach[data49$gruppe==1])$parameter,3),
  
  round(t.test(
    data49$arbeitskleidung_kontrolle[data49$gruppe==0] -
      data49$arbeitskleidung_kontrolle_nach[data49$gruppe==0],
    data49$arbeitskleidung_kontrolle[data49$gruppe==1] -
      data49$arbeitskleidung_kontrolle_nach[data49$gruppe==1])$parameter,3),
  
  round(t.test(
    data49$haare_kontrolle[data49$gruppe==0] -
      data49$haare_kontrolle_nach[data49$gruppe==0],
    data49$haare_kontrolle[data49$gruppe==1] -
      data49$haare_kontrolle_nach[data49$gruppe==1])$parameter,3),
  
  round(t.test(
    data49$aufbewahrung_kontrolle[data49$gruppe==0] -
      data49$aufbewahrung_kontrolle_nach[data49$gruppe==0],
    data49$aufbewahrung_kontrolle[data49$gruppe==1] -
      data49$aufbewahrung_kontrolle_nach[data49$gruppe==1])$parameter,3) 
)

CI.95perc.low <- c(
  round(t.test(
    data49$arbeitskleidung_letzteWoche[data49$gruppe==0] -
      data49$arbeitskleidung_letzteWoche_nach[data49$gruppe==0],
    data49$arbeitskleidung_letzteWoche[data49$gruppe==1] -
      data49$arbeitskleidung_letzteWoche_nach[data49$gruppe==1])$conf.int[1],2),
  
  round(t.test(
    data49$haare_letzteWoche[data49$gruppe==0] -
      data49$haare_letzteWoche_nach[data49$gruppe==0],
    data49$haare_letzteWoche[data49$gruppe==1] -
      data49$haare_letzteWoche_nach[data49$gruppe==1])$conf.int[1],2),
  
  round(t.test(
    data49$aufbewahrung_letzteWoche[data49$gruppe==0] -
      data49$aufbewahrung_letzteWoche_nach[data49$gruppe==0],
    data49$aufbewahrung_letzteWoche[data49$gruppe==1] -
      data49$aufbewahrung_letzteWoche_nach[data49$gruppe==1])$conf.int[1],2),
  
  round(t.test(
    data49$arbeitskleidung_intention[data49$gruppe==0] -
      data49$arbeitskleidung_intention_nach[data49$gruppe==0],
    data49$arbeitskleidung_intention[data49$gruppe==1] -
      data49$arbeitskleidung_intention_nach[data49$gruppe==1])$conf.int[1],2),
  
  round(t.test(
    data49$haare_intention[data49$gruppe==0] -
      data49$haare_intention_nach[data49$gruppe==0],
    data49$haare_intention[data49$gruppe==1] -
      data49$haare_intention_nach[data49$gruppe==1])$conf.int[1],2),
  
  round(t.test(
    data49$aufbewahrung_intention[data49$gruppe==0] -
      data49$aufbewahrung_intention_nach[data49$gruppe==0],
    data49$aufbewahrung_intention[data49$gruppe==1] -
      data49$aufbewahrung_intention_nach[data49$gruppe==1])$conf.int[1],2),
  
  round(t.test(
    data49$arbeitskleidung_einstellung[data49$gruppe==0] -
      data49$arbeitskleidung_einstellung_nach[data49$gruppe==0],
    data49$arbeitskleidung_einstellung[data49$gruppe==1] -
      data49$arbeitskleidung_einstellung_nach[data49$gruppe==1])$conf.int[1],2),
  
  round(t.test(
    data49$haare_einstellung[data49$gruppe==0] -
      data49$haare_einstellung_nach[data49$gruppe==0],
    data49$haare_einstellung[data49$gruppe==1] -
      data49$haare_einstellung_nach[data49$gruppe==1])$conf.int[1],2),
  
  round(t.test(
    data49$aufbewahrung_einstellung[data49$gruppe==0] -
      data49$aufbewahrung_einstellung_nach[data49$gruppe==0],
    data49$aufbewahrung_einstellung[data49$gruppe==1] -
      data49$aufbewahrung_einstellung_nach[data49$gruppe==1])$conf.int[1],2),
  
  round(t.test(
    data49$arbeitskleidung_norm[data49$gruppe==0] -
      data49$arbeitskleidung_norm_nach[data49$gruppe==0],
    data49$arbeitskleidung_norm[data49$gruppe==1] -
      data49$arbeitskleidung_norm_nach[data49$gruppe==1])$conf.int[1],2),
  
  round(t.test(
    data49$haare_norm[data49$gruppe==0] -
      data49$haare_norm_nach[data49$gruppe==0],
    data49$haare_norm[data49$gruppe==1] -
      data49$haare_norm_nach[data49$gruppe==1])$conf.int[1],2),
  
  round(t.test(
    data49$aufbewahrung_norm[data49$gruppe==0] -
      data49$aufbewahrung_norm_nach[data49$gruppe==0],
    data49$aufbewahrung_norm[data49$gruppe==1] -
      data49$aufbewahrung_norm_nach[data49$gruppe==1])$conf.int[1],2),
  
  round(t.test(
    data49$arbeitskleidung_kontrolle[data49$gruppe==0] -
      data49$arbeitskleidung_kontrolle_nach[data49$gruppe==0],
    data49$arbeitskleidung_kontrolle[data49$gruppe==1] -
      data49$arbeitskleidung_kontrolle_nach[data49$gruppe==1])$conf.int[1],2),
  
  round(t.test(
    data49$haare_kontrolle[data49$gruppe==0] -
      data49$haare_kontrolle_nach[data49$gruppe==0],
    data49$haare_kontrolle[data49$gruppe==1] -
      data49$haare_kontrolle_nach[data49$gruppe==1])$conf.int[1],2),
  
  round(t.test(
    data49$aufbewahrung_kontrolle[data49$gruppe==0] -
      data49$aufbewahrung_kontrolle_nach[data49$gruppe==0],
    data49$aufbewahrung_kontrolle[data49$gruppe==1] -
      data49$aufbewahrung_kontrolle_nach[data49$gruppe==1])$conf.int[1],2)
)

CI.95perc.high <- c(
  round(t.test(
    data49$arbeitskleidung_letzteWoche[data49$gruppe==0] -
      data49$arbeitskleidung_letzteWoche_nach[data49$gruppe==0],
    data49$arbeitskleidung_letzteWoche[data49$gruppe==1] -
      data49$arbeitskleidung_letzteWoche_nach[data49$gruppe==1])$conf.int[2],2),
  
  round(t.test(
    data49$haare_letzteWoche[data49$gruppe==0] -
      data49$haare_letzteWoche_nach[data49$gruppe==0],
    data49$haare_letzteWoche[data49$gruppe==1] -
      data49$haare_letzteWoche_nach[data49$gruppe==1])$conf.int[2],2),
  
  round(t.test(
    data49$aufbewahrung_letzteWoche[data49$gruppe==0] -
      data49$aufbewahrung_letzteWoche_nach[data49$gruppe==0],
    data49$aufbewahrung_letzteWoche[data49$gruppe==1] -
      data49$aufbewahrung_letzteWoche_nach[data49$gruppe==1])$conf.int[2],2),
  
  round(t.test(
    data49$arbeitskleidung_intention[data49$gruppe==0] -
      data49$arbeitskleidung_intention_nach[data49$gruppe==0],
    data49$arbeitskleidung_intention[data49$gruppe==1] -
      data49$arbeitskleidung_intention_nach[data49$gruppe==1])$conf.int[2],2),
  
  round(t.test(
    data49$haare_intention[data49$gruppe==0] -
      data49$haare_intention_nach[data49$gruppe==0],
    data49$haare_intention[data49$gruppe==1] -
      data49$haare_intention_nach[data49$gruppe==1])$conf.int[2],2),
  
  round(t.test(
    data49$aufbewahrung_intention[data49$gruppe==0] -
      data49$aufbewahrung_intention_nach[data49$gruppe==0],
    data49$aufbewahrung_intention[data49$gruppe==1] -
      data49$aufbewahrung_intention_nach[data49$gruppe==1])$conf.int[2],2),
  
  round(t.test(
    data49$arbeitskleidung_einstellung[data49$gruppe==0] -
      data49$arbeitskleidung_einstellung_nach[data49$gruppe==0],
    data49$arbeitskleidung_einstellung[data49$gruppe==1] -
      data49$arbeitskleidung_einstellung_nach[data49$gruppe==1])$conf.int[2],2),
  
  round(t.test(
    data49$haare_einstellung[data49$gruppe==0] -
      data49$haare_einstellung_nach[data49$gruppe==0],
    data49$haare_einstellung[data49$gruppe==1] -
      data49$haare_einstellung_nach[data49$gruppe==1])$conf.int[2],2),
  
  round(t.test(
    data49$aufbewahrung_einstellung[data49$gruppe==0] -
      data49$aufbewahrung_einstellung_nach[data49$gruppe==0],
    data49$aufbewahrung_einstellung[data49$gruppe==1] -
      data49$aufbewahrung_einstellung_nach[data49$gruppe==1])$conf.int[2],2),
  
  round(t.test(
    data49$arbeitskleidung_norm[data49$gruppe==0] -
      data49$arbeitskleidung_norm_nach[data49$gruppe==0],
    data49$arbeitskleidung_norm[data49$gruppe==1] -
      data49$arbeitskleidung_norm_nach[data49$gruppe==1])$conf.int[2],2),
  
  round(t.test(
    data49$haare_norm[data49$gruppe==0] -
      data49$haare_norm_nach[data49$gruppe==0],
    data49$haare_norm[data49$gruppe==1] -
      data49$haare_norm_nach[data49$gruppe==1])$conf.int[2],2),
  
  round(t.test(
    data49$aufbewahrung_norm[data49$gruppe==0] -
      data49$aufbewahrung_norm_nach[data49$gruppe==0],
    data49$aufbewahrung_norm[data49$gruppe==1] -
      data49$aufbewahrung_norm_nach[data49$gruppe==1])$conf.int[2],2),
  
  round(t.test(
    data49$arbeitskleidung_kontrolle[data49$gruppe==0] -
      data49$arbeitskleidung_kontrolle_nach[data49$gruppe==0],
    data49$arbeitskleidung_kontrolle[data49$gruppe==1] -
      data49$arbeitskleidung_kontrolle_nach[data49$gruppe==1])$conf.int[2],2),
  
  round(t.test(
    data49$haare_kontrolle[data49$gruppe==0] -
      data49$haare_kontrolle_nach[data49$gruppe==0],
    data49$haare_kontrolle[data49$gruppe==1] -
      data49$haare_kontrolle_nach[data49$gruppe==1])$conf.int[2],2),
  
  round(t.test(
    data49$aufbewahrung_kontrolle[data49$gruppe==0] -
      data49$aufbewahrung_kontrolle_nach[data49$gruppe==0],
    data49$aufbewahrung_kontrolle[data49$gruppe==1] -
      data49$aufbewahrung_kontrolle_nach[data49$gruppe==1])$conf.int[2],2)
)

p.Values <- c(
  round(t.test(
    data49$arbeitskleidung_letzteWoche[data49$gruppe==0] -
      data49$arbeitskleidung_letzteWoche_nach[data49$gruppe==0],
    data49$arbeitskleidung_letzteWoche[data49$gruppe==1] -
      data49$arbeitskleidung_letzteWoche_nach[data49$gruppe==1])$p.value,2),
  
  round(t.test(
    data49$haare_letzteWoche[data49$gruppe==0] -
      data49$haare_letzteWoche_nach[data49$gruppe==0],
    data49$haare_letzteWoche[data49$gruppe==1] -
      data49$haare_letzteWoche_nach[data49$gruppe==1])$p.value,2),
  
  round(t.test(
    data49$aufbewahrung_letzteWoche[data49$gruppe==0] -
      data49$aufbewahrung_letzteWoche_nach[data49$gruppe==0],
    data49$aufbewahrung_letzteWoche[data49$gruppe==1] -
      data49$aufbewahrung_letzteWoche_nach[data49$gruppe==1])$p.value,2),
  
  round(t.test(
    data49$arbeitskleidung_intention[data49$gruppe==0] -
      data49$arbeitskleidung_intention_nach[data49$gruppe==0],
    data49$arbeitskleidung_intention[data49$gruppe==1] -
      data49$arbeitskleidung_intention_nach[data49$gruppe==1])$p.value,2),
  
  round(t.test(
    data49$haare_intention[data49$gruppe==0] -
      data49$haare_intention_nach[data49$gruppe==0],
    data49$haare_intention[data49$gruppe==1] -
      data49$haare_intention_nach[data49$gruppe==1])$p.value,2),
  
  round(t.test(
    data49$aufbewahrung_intention[data49$gruppe==0] -
      data49$aufbewahrung_intention_nach[data49$gruppe==0],
    data49$aufbewahrung_intention[data49$gruppe==1] -
      data49$aufbewahrung_intention_nach[data49$gruppe==1])$p.value,2),
  
  round(t.test(
    data49$arbeitskleidung_einstellung[data49$gruppe==0] -
      data49$arbeitskleidung_einstellung_nach[data49$gruppe==0],
    data49$arbeitskleidung_einstellung[data49$gruppe==1] -
      data49$arbeitskleidung_einstellung_nach[data49$gruppe==1])$p.value,2),
  
  round(t.test(
    data49$haare_einstellung[data49$gruppe==0] -
      data49$haare_einstellung_nach[data49$gruppe==0],
    data49$haare_einstellung[data49$gruppe==1] -
      data49$haare_einstellung_nach[data49$gruppe==1])$p.value,2),
  
  round(t.test(
    data49$aufbewahrung_einstellung[data49$gruppe==0] -
      data49$aufbewahrung_einstellung_nach[data49$gruppe==0],
    data49$aufbewahrung_einstellung[data49$gruppe==1] -
      data49$aufbewahrung_einstellung_nach[data49$gruppe==1])$p.value,2),
  
  round(t.test(
    data49$arbeitskleidung_norm[data49$gruppe==0] -
      data49$arbeitskleidung_norm_nach[data49$gruppe==0],
    data49$arbeitskleidung_norm[data49$gruppe==1] -
      data49$arbeitskleidung_norm_nach[data49$gruppe==1])$p.value,2),
  
  round(t.test(
    data49$haare_norm[data49$gruppe==0] -
      data49$haare_norm_nach[data49$gruppe==0],
    data49$haare_norm[data49$gruppe==1] -
      data49$haare_norm_nach[data49$gruppe==1])$p.value,2),
  
  round(t.test(
    data49$aufbewahrung_norm[data49$gruppe==0] -
      data49$aufbewahrung_norm_nach[data49$gruppe==0],
    data49$aufbewahrung_norm[data49$gruppe==1] -
      data49$aufbewahrung_norm_nach[data49$gruppe==1])$p.value,2),
  
  round(t.test(
    data49$arbeitskleidung_kontrolle[data49$gruppe==0] -
      data49$arbeitskleidung_kontrolle_nach[data49$gruppe==0],
    data49$arbeitskleidung_kontrolle[data49$gruppe==1] -
      data49$arbeitskleidung_kontrolle_nach[data49$gruppe==1])$p.value,2),
  
  round(t.test(
    data49$haare_kontrolle[data49$gruppe==0] -
      data49$haare_kontrolle_nach[data49$gruppe==0],
    data49$haare_kontrolle[data49$gruppe==1] -
      data49$haare_kontrolle_nach[data49$gruppe==1])$p.value,2),
  
  round(t.test(
    data49$aufbewahrung_kontrolle[data49$gruppe==0] -
      data49$aufbewahrung_kontrolle_nach[data49$gruppe==0],
    data49$aufbewahrung_kontrolle[data49$gruppe==1] -
      data49$aufbewahrung_kontrolle_nach[data49$gruppe==1])$p.value,2)  
  
)


# table:
cbind(name.labels,
      c(print("Control group"), control.means),
      c(print("Intervention group"), intervention.means),
      c(print("t-value"), t.values),
      c(print("df"), df.values),
      c(print("95% CI low"), CI.95perc.low),
      c(print("95% CI high"), CI.95perc.high),
      c(print("p-value"), p.Values)
)

#----
#---------------#
# END OF SCRIPT #
#---------------#