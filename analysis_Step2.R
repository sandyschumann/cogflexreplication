###Goals: Calculate reliability of 'willingness to fight' scale, descriptive statistics of all variables, correlations between all variables

###Requirements
install.packages("psych")
install.packages("Hmisc")
library(psych) 
library(Hmisc)

###Chronbach's alpha for 'willingness to fight'
fight_scale<- data.frame(fulldataset[ ,1:5])
alpha(fight_scale)

###Descriptive statistics: means and standard deviations to reproduce Table 1
##Willingness to die
mean(fulldataset$die)
sd(fulldataset$die)

##Willingness to fight
mean(fulldataset$fight)
sd(fulldataset$fight)

##Certainty trolley dilemma
mean(fulldataset$certainty_trolley)
sd(fulldataset$certainty_trolley)

##RAT accuracy rate
mean(fulldataset$RATaccuracy)
sd(fulldataset$RATaccuracy)

##WCST accuracy rate
mean(fulldataset$WCSTaccuracy)
sd(fulldataset$WCSTaccuracy)

###Bi-variate correlations to reproduce Table 1
subset <- select(fulldataset, die, fight, certainty_trolley, RATaccuracy, WCSTaccuracy)
cormat <- corr.test(subset, use = "complete", method = "pearson")
print(cormat, short = FALSE)

###Descriptive statistics: frequency choice in the trolley dilemma
choice_summary<-factor(fulldataset$choice, levels= c(0:1), labels = c("safe self", "self-sacrifice"))
table(choice_summary)


