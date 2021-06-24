###Goals: Complete additional analyses for Hypotheses 1, 2, and 3

###Requirements
install.packages("lavaan")
install.packages("psych")
install.packages("Hmisc")
install.packages("dplyr")
install.packages("mvnormtest")
library(lavaan)
library(psych) 
library(Hmisc)
library(dplyr)
library(mvnormtest)

###Parametric: Additional analyses Hypothesis 1

cor.test(fulldataset$Alternatives, fulldataset$fight, use = "complete.obs", method = "pearson")
cor.test(fulldataset$Control, fulldataset$fight, use = "complete.obs", method = "pearson")
cor.test(fulldataset$Alternatives, fulldataset$die, use = "complete.obs", method = "pearson")
cor.test(fulldataset$Control, fulldataset$die, use = "complete.obs", method = "pearson")
cor.test(fulldataset$Alternatives, fulldataset$choice, use = "complete.obs", method = "pearson")
cor.test(fulldataset$Control, fulldataset$choice, use = "complete.obs", method = "pearson")

###Non-parametric: Additional analyses Hypothesis 1

cor.Altfight<-corr.test(fulldataset$Alternatives, fulldataset$fight, use = "complete.obs", method = "spearman")
print(cor.Altfight, short = FALSE)
cor.Controlfight<-corr.test(fulldataset$Control, fulldataset$fight, use = "complete.obs", method = "spearman")
print(cor.Controlfight, short = FALSE)
cor.Altdie<-corr.test(fulldataset$Alternatives, fulldataset$die, use = "complete.obs", method = "spearman")
print(cor.Altdie, short = FALSE)
cor.Controldie<-corr.test(fulldataset$Control, fulldataset$die, use = "complete.obs", method = "spearman")
print(cor.Controldie, short = FALSE)

###Additional analyses Hypothesis 2

##Model 1b: direct relations between additional cognitive flexibility measures and willingness to die/trolley choice plus controls
#Outcome: willingness to die
fullmodel11b<-'##regressions
            die~fight + RATaccuracy + WCSTaccuracy+Alternatives+Control+age+gender_recoded+education+ overlap
            ##covariances
            RATaccuracy ~~ WCSTaccuracy
            RATaccuracy ~~ Alternatives
            RATaccuracy ~~ Control
            WCSTaccuracy ~~ Alternatives
            WCSTaccuracy ~~ Control
            Alternatives ~~ Control
            age~~education
            age~~gender_recoded
            education~~gender_recoded
            overlap ~~ fight
            ##intercepts            
            die~1'
fitfull11b <- sem(fullmodel11b, data = fulldataset, estimator = "MLR", missing = "FIML")
summary(fitfull11b, fit.measures = TRUE, standardized = TRUE)

#Outcome: trolley choice
fulldataset$choice = as.ordered(fulldataset$choice)
fullmodel12b<-'##regressions
            choice~fight + RATaccuracy + WCSTaccuracy+Alternatives+Control+age+gender_recoded+education+ overlap
            ##covariances
            RATaccuracy ~~ WCSTaccuracy
            RATaccuracy ~~ Alternatives
            RATaccuracy ~~ Control
            WCSTaccuracy ~~ Alternatives
            WCSTaccuracy ~~ Control
            Alternatives ~~ Control
            age~~education
            age~~gender_recoded
            education~~gender_recoded
            overlap ~~ fight
            ##intercepts            
            choice~1'
fitfull12b <- sem(fullmodel12b, data = fulldataset, estimator = "WLSMV")
summary(fitfull12b, fit.measures = TRUE, standardized = TRUE)


##Model 2: mediated relations between additonal cognitive flexibility measures and willingness to die/trolley choice plus controls
#Outcome: willingness to die
mediatedmodel11b <-'##regressions
            die ~ fight+age+gender_recoded+education+ overlap
            fight ~ RATaccuracy + WCSTaccuracy+Alternatives+Control
            ##covariances
            RATaccuracy ~~ WCSTaccuracy
            RATaccuracy ~~ Alternatives
            RATaccuracy ~~ Control
            WCSTaccuracy ~~ Alternatives
            WCSTaccuracy ~~ Control
            Alternatives ~~ Control
            age~~education
            age~~gender_recoded
            education~~gender_recoded
            overlap ~~ fight
            ##intercepts
            die~1
            fight~1'
fitmediated11b <- sem(mediatedmodel11b, data = fulldataset, estimator = "MLR", missing = "FIML")
summary(fitmediated11b, fit.measures = TRUE, standardized = TRUE)

#Outcome: trolley choice
fulldataset$choice = as.ordered(fulldataset$choice)
mediatedmodel12b <-'##regressions
            choice ~ fight+age+gender_recoded+education+ overlap
            fight ~ RATaccuracy + WCSTaccuracy+Alternatives+Control
            ##covariances
            RATaccuracy ~~ WCSTaccuracy
            RATaccuracy ~~ Alternatives
            RATaccuracy ~~ Control
            WCSTaccuracy ~~ Alternatives
            WCSTaccuracy ~~ Control
            Alternatives ~~ Control
            age~~education
            age~~gender_recoded
            education~~gender_recoded
            overlap ~~ fight
            ##intercepts
            choice~1
            fight~1'
fitmediated12b <- sem(mediatedmodel12b, data = fulldataset, estimator = "WLSMV")
summary(fitmediated12b, fit.measures = TRUE, standardized = TRUE)

##Model 1 and 2 of the original study with normative pro-group behaviour as outcome variable
#Model 1
fullmodel13<-'##regressions
            ARIS~fight + RATaccuracy + WCSTaccuracy+age+gender_recoded+education
            ##covariances
            RATaccuracy ~~ WCSTaccuracy
            age~~education
            age~~gender_recoded
            education~~gender_recoded
            ##intercepts            
            ARIS~1'
fitfull13 <- sem(fullmodel13, data = fulldataset, estimator = "MLR", missing = "FIML")
summary(fitfull13, fit.measures = TRUE, standardized = TRUE)

#Model 2
mediatedmodel13<-'##regressions
              ARIS~fight +age+gender_recoded+education
              fight~RATaccuracy + WCSTaccuracy
              ##covariance
              RATaccuracy ~~ WCSTaccuracy
              age~~education
              age~~gender_recoded
              education~~gender_recoded
              ##intercepts
              ARIS~1
              fight~1'
fitmediated13 <- sem(mediatedmodel13, data = fulldataset, estimator = "MLR", missing = "FIML")
summary(fitmediated13, fit.measures = TRUE, standardized = TRUE)

##Model 1 and 2 of the additional analysis with normative pro-group behaviour as outcome variable
#Model 1
fullmodel13b<-'##regressions
            ARIS~fight + RATaccuracy + WCSTaccuracy+Alternatives+Control+age+gender_recoded+education+ overlap
            ##covariances
            RATaccuracy ~~ WCSTaccuracy
            RATaccuracy ~~ Alternatives
            RATaccuracy ~~ Control
            WCSTaccuracy ~~ Alternatives
            WCSTaccuracy ~~ Control
            Alternatives ~~ Control
            age~~education
            age~~gender_recoded
            education~~gender_recoded
            overlap ~~ fight
            ##intercepts            
            ARIS~1'
fitfull13b <- sem(fullmodel13b, data = fulldataset, estimator = "MLR", missing = "FIML")
summary(fitfull13b, fit.measures = TRUE, standardized = TRUE)

#Model 2
mediatedmodel13b <-'##regressions
            ARIS ~ fight+age+gender_recoded+education+ overlap
            fight ~ RATaccuracy + WCSTaccuracy+Alternatives+Control
            ##covariances
            RATaccuracy ~~ WCSTaccuracy
            RATaccuracy ~~ Alternatives
            RATaccuracy ~~ Control
            WCSTaccuracy ~~ Alternatives
            WCSTaccuracy ~~ Control
            Alternatives ~~ Control
            age~~education
            age~~gender_recoded
            education~~gender_recoded
            overlap ~~ fight
            ##intercepts            
            ##intercepts
            ARIS~1
            fight~1'
fitmediated13b <- sem(mediatedmodel13b, data = fulldataset, estimator = "MLR", missing = "FIML")
summary(fitmediated13b, fit.measures = TRUE, standardized = TRUE)

#Parametric: Correlation cognitive flexibility tests and normative pro-group behaviour
cor.test(fulldataset$ARIS, fulldataset$RATaccuracy, use = "complete.obs", method = "pearson")
cor.test(fulldataset$ARIS, fulldataset$WCSTaccuracy, use = "complete.obs", method = "pearson")

#Non-parametric: Correlation cognitive flexibility tests and normative pro-group behaviour
cor.ARISRAT<-corr.test(fulldataset$ARIS, fulldataset$RATaccuracy, use = "complete.obs", method = "spearman")
print(cor.ARISRAT, short = FALSE)
cor.ARISWCST<-corr.test(fulldataset$ARIS, fulldataset$WCSTaccuracy, use = "complete.obs", method = "spearman")
print(cor.ARISWCST, short = FALSE)

###Additional analyses Hypothesis 3
##test homogeneity of variances
by(fulldataset[,147:148], fulldataset$choice, cov)

##test multivariate normality
saveself <- subset(fulldataset, choice==0) #select those who chose to save themselves
sacrifice <- subset(fulldataset, choice==1) #select those who chose to self-sacrifice
saveselftest.add<-saveself[, 147:148]
sacrificeselftest.add<-sacrifice[, 147:148]

saveselftest.add<-t(saveselftest.add)
sacrificeselftest.add<-t(sacrificeselftest.add)

mshapiro.test(saveselftest.add)
mshapiro.test(sacrificeselftest.add)

##Parametric: MANOVA with self-report cognitive flexibility scores
outcome_addition<-cbind(fulldataset$Alternatives, fulldataset$Control)
model_addition<-manova(outcome_addition ~ choice, data = fulldataset)
summary(model_addition, intercept = TRUE)
summary.aov(model_addition)

##Robust MANOVA
#This test was conducted in SPSS - Mann-WHitney-U test for independent samples

##Parametric: Correlations whole sample
cor.test(fulldataset$Alternatives, fulldataset$certainty_trolley, use = "complete.obs", method = "pearson")
cor.test(fulldataset$Control, fulldataset$certainty_trolley, use = "complete.obs", method = "pearson")

##Non-parametric: Correlations whole sample
cor.Alttrolley<-corr.test(fulldataset$Alternatives, fulldataset$certainty_trolley, use = "complete.obs", method = "spearman")
print(cor.Alttrolley, short = FALSE)
cor.Controltrolley<-corr.test(fulldataset$Control, fulldataset$certainty_trolley, use = "complete.obs", method = "spearman")
print(cor.Controltrolley, short = FALSE)

##Parametric: Sub-group analysis of correlations
saveself <- subset(fulldataset, choice==0) #select those who chose to save themselves
cor.test(saveself$certainty_trolley, saveself$Alternatives, use = "complete.obs", method = "pearson")
cor.test(saveself$certainty_trolley, saveself$Control, use = "complete.obs", method = "pearson")

sacrifice <- subset(fulldataset, choice==1) #select those who chose to self-sacrifice
cor.test(sacrifice$certainty_trolley, sacrifice$Alternatives, use = "complete.obs", method = "pearson")
cor.test(sacrifice$certainty_trolley, sacrifice$Control, use = "complete.obs", method = "pearson")

##Non-parametric: Sub-group analysis of correlations
saveself <- subset(fulldataset, choice==0) #select those who chose to save themselves
cor.Altcertain<-corr.test(saveself$certainty_trolley, saveself$Alternatives, use = "complete.obs", method = "spearman")
print(cor.Altcertain, short = FALSE)
cor.Controlcertain<-corr.test(saveself$certainty_trolley, saveself$Control, use = "complete.obs", method = "spearman")
print(cor.Controlcertain, short = FALSE)

sacrifice <- subset(fulldataset, choice==1) #select those who chose to self-sacrifice
cor.Altcertain_s<-corr.test(sacrifice$certainty_trolley, sacrifice$Alternatives, use = "complete.obs", method = "spearman")
print(cor.Altcertain_s, short = FALSE)
cor.Controlcertain_s<-corr.test(sacrifice$certainty_trolley, sacrifice$Control, use = "complete.obs", method = "spearman")
print(cor.Controlcertain_s, short = FALSE)
