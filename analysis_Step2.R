###Goals: Calculate reliability of 'willingness to fight' scale, descriptive statistics of all variables, tests of normal distribution, and correlations between all variables (test Hypothesis 1)

###Requirements
install.packages("psych")
install.packages("Hmisc")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("pastecs")
library(psych) 
library(Hmisc)
library(dplyr)
library(ggplot2)
library(pastecs)

###Cronbach's alpha for 'willingness to fight'
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

###Descriptive statistics: frequency choice in the trolley dilemma
choice_summary<-factor(fulldataset$choice, levels= c(0:1), labels = c("save self", "self-sacrifice"))
table(choice_summary)

###Test normal distribution of all variables (Supplementary Material)
options(scipen=999) #disable the scientific notation of values, optional

##Skewness, kurtosis, Shapiro-Wilk test
stat.desc(cbind(fulldataset$fight, fulldataset$die, fulldataset$certainty_trolley, fulldataset$RATaccuracy, fulldataset$WCSTaccuracy), basic = FALSE, norm = TRUE)

##Q-Q plots
qqplot.fight<-qplot(sample = fulldataset$fight, main = "Willingness to fight")
qqplot.fight

qqplot.die<-qplot(sample = fulldataset$die, main = "Willingness to die")
qqplot.die

qqplot.certainty_trolley<-qplot(sample = fulldataset$certainty_trolley, main = "Certainty trolley dilemma")
qqplot.certainty_trolley

qqplot.RATaccuracy<-qplot(sample = fulldataset$RATaccuracy, main = "RAT")
qqplot.RATaccuracy

qqplot.WCSTaccuracy<-qplot(sample = fulldataset$WCSTaccuracy, main = "WCST")
qqplot.WCSTaccuracy


###Parametric: Bi-variate correlations to reproduce Table S2
subset_fulldataset <- select(fulldataset, die, fight, certainty_trolley, RATaccuracy, WCSTaccuracy)
cormat_par <- corr.test(subset_fulldataset, use = "complete", method = "pearson")
print(cormat_par, short = FALSE)

###Non-parametric: Spearman correlations to reproduce Table 1
cormat_nonpar <- corr.test(subset_fulldataset, use = "complete", method = "spearman")
print(cormat_nonpar, short = FALSE)




