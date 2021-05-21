###Goals: Complete additional analyses for Hypotheses 1, 2, and 3

###Requirements
install.packages("lavaan")
library(lavaan)

###Additional analyses Hypothesis 1

cor.test(fulldataset$Alternatives, fulldataset$fight, use = "complete.obs", method = "pearson")
cor.test(fulldataset$Control, fulldataset$fight, use = "complete.obs", method = "pearson")
cor.test(fulldataset$Alternatives, fulldataset$die, use = "complete.obs", method = "pearson")
cor.test(fulldataset$Control, fulldataset$die, use = "complete.obs", method = "pearson")

cor.test(fulldataset$Alternatives, fulldataset$choice, use = "complete.obs", method = "pearson")
cor.test(fulldataset$Control, fulldataset$choice, use = "complete.obs", method = "pearson")

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
fitfull11b <- sem(fullmodel11b, data = fulldataset)
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
fitmediated11b <- sem(mediatedmodel11b, data = fulldataset)
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
fitmediated12b <- sem(mediatedmodel12b, data = fulldataset, ordered = c("choice"), estimator = "WLSMV")
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
fitfull13 <- sem(fullmodel13, data = fulldataset)
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
fitmediated13 <- sem(mediatedmodel13, data = fulldataset)
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
fitfull13b <- sem(fullmodel13b, data = fulldataset)
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
fitmediated13b <- sem(mediatedmodel13b, data = fulldataset)
summary(fitmediated13b, fit.measures = TRUE, standardized = TRUE)

#Correlation cognitive flexibility tests and normative pro-group behaviour
cor.test(fulldataset$ARIS, fulldataset$RATaccuracy, use = "complete.obs", method = "pearson")
cor.test(fulldataset$ARIS, fulldataset$WCSTaccuracy, use = "complete.obs", method = "pearson")


###Additional analyses Hypothesis 3
##MANOVA with self-report cognitive flexibility scores
outcome_addition<-cbind(fulldataset$Alternatives, fulldataset$Control)
model_addition<-manova(outcome_addition ~ choice, data = fulldataset)
summary(model_addition, intercept = TRUE)
summary.aov(model_addition)

##Correlations whole sample
cor.test(fulldataset$Alternatives, fulldataset$certainty_trolley, use = "complete.obs", method = "pearson")
cor.test(fulldataset$Control, fulldataset$certainty_trolley, use = "complete.obs", method = "pearson")

##Sub-group analysis of correlations
saveself <- subset(fulldataset, choice==0) #select those who chose to save themselves
cor.test(saveself$certainty_trolley, safe$Alternatives, use = "complete.obs", method = "pearson")
cor.test(saveself$certainty_trolley, safe$Control, use = "complete.obs", method = "pearson")

sacrifice <- subset(fulldataset, choice==1) #select those who chose to self-sacrifice
cor.test(sacrifice$certainty_trolley, sacrifice$Alternatives, use = "complete.obs", method = "pearson")
cor.test(sacrifice$certainty_trolley, sacrifice$Control, use = "complete.obs", method = "pearson")

