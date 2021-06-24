###Goals: Test Hypothesis 3

###Requirements
install.packages("mvnormtest")
install.packages("psych")
install.packages("WRS2")
install.packages("reshape2")
library(mvnormtest)
library(psych) 
library(WRS2)
library(reshape2)

###MANOVA
##test homogeneity of variances
by(fulldataset[,143:144], fulldataset$choice, cov)

##test multivariate normality
saveself <- subset(fulldataset, choice==0) #select those who chose to save themselves
sacrifice <- subset(fulldataset, choice==1) #select those who chose to self-sacrifice

saveselftest<-saveself[, 143:144]
sacrificeselftest<-sacrifice[, 143:144]

saveselftest<-t(saveselftest)
sacrificeselftest<-t(sacrificeselftest)

mshapiro.test(saveselftest)
mshapiro.test(sacrificeselftest)

##Parametric: MANOVA model test
outcome<-cbind(fulldataset$RATaccuracy, fulldataset$WCSTaccuracy) ##create outcome measure(s)
model_outcome<-manova(outcome ~ choice, data = fulldataset)
summary(model_outcome, intercept = TRUE)
summary.aov(model_outcome)

##Robust MANOAVA
#this test was conducted in SPSS - Mann-Whitney-U-test for two independent samples

###Parametric: Sub-group analysis of correlations between certainty in choice in trolley dilemma and cognitive flexiblity scores
saveself <- subset(fulldataset, choice==0) #select those who chose to save themselves
cor.test(saveself$certainty_trolley, saveself$RATaccuracy, use = "complete", method = "pearson")
cor.test(saveself$certainty_trolley, saveself$WCSTaccuracy, use = "complete", method = "pearson")

sacrifice <- subset(fulldataset, choice==1) #select those who chose to self-sacrifice
cor.test(sacrifice$certainty_trolley, sacrifice$RATaccuracy, use = "complete", method = "pearson")
cor.test(sacrifice$certainty_trolley, sacrifice$WCSTaccuracy, use = "complete", method = "pearson")

###Non-parametric: Sub-group analysis of correlations between certainty in choice in trolley dilemma and cognitive flexiblity scores
saveself <- subset(fulldataset, choice==0) #select those who chose to save themselves
corsafeRAT<-corr.test(saveself$certainty_trolley, saveself$RATaccuracy, use = "complete", method = "spearman")
print(corsafeRAT, short = FALSE)
corsafeWCST<-corr.test(saveself$certainty_trolley, saveself$WCSTaccuracy, use = "complete.", method = "spearman")
print(corsafeWCST, short = FALSE)

sacrifice <- subset(fulldataset, choice==1) #select those who chose to self-sacrifice
corsacrificeRAT<-corr.test(sacrifice$certainty_trolley, sacrifice$RATaccuracy, use = "complete", method = "spearman")
print(corsacrificeRAT, short = FALSE)
corsacrificeWCST<-corr.test(sacrifice$certainty_trolley, sacrifice$WCSTaccuracy, use = "complete", method = "spearman")
print(corsacrificeWCST, short = FALSE)
