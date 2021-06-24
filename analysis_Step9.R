###Goals: Conduct all pre-registered analyses with sample where those who failed the attention check are excluded

###Requirements
install.packages("psych")
install.packages("Hmisc")
install.packages("dplyr")
install.packages("lavaan", dependencies = TRUE)
library(psych) 
library(Hmisc)
library(dplyr)
library(lavaan)

####Step 1: Calculate descriptive statistics of all variables, correlations between all variables

###Descriptive statistics: means and standard deviations to reproduce Table 1
##Willingness to die
mean(fulldataset_attention$die)
sd(fulldataset_attention$die)

##Willingness to fight
mean(fulldataset_attention$fight)
sd(fulldataset_attention$fight)

##Certainty trolley dilemma
mean(fulldataset_attention$certainty_trolley)
sd(fulldataset_attention$certainty_trolley)

##RAT accuracy rate
mean(fulldataset_attention$RATaccuracy)
sd(fulldataset_attention$RATaccuracy)

##WCST accuracy rate
mean(fulldataset_attention$WCSTaccuracy)
sd(fulldataset_attention$WCSTaccuracy)

###Bi-variate correlations to reproduce Table 1
subset_fulldataset_attention <- select(fulldataset_attention, die, fight, certainty_trolley, RATaccuracy, WCSTaccuracy)
cormat <- corr.test(subset_fulldataset_attention, use = "complete", method = "pearson")
print(cormat, short = FALSE)

###Descriptive statistics: frequency choice in the trolley dilemma
choice_summary<-factor(fulldataset_attention$choice, levels= c(0:1), labels = c("save self", "self-sacrifice"))
table(choice_summary)

####Step 2: Assess Hypothesis 2

###Model 1: direct relations between cognitive flexibility and willingness to die/trolley choice
##Outcome: Willingness to die
fullmodel11<-'##regressions
            die~fight + RATaccuracy + WCSTaccuracy+age+gender_recoded+education
            ##covariances
            RATaccuracy ~~ WCSTaccuracy
            age~~education
            age~~gender_recoded
            education~~gender_recoded
            ##intercepts            
            die~1'
fitfull11 <- sem(fullmodel11, data = fulldataset_attention)
summary(fitfull11, fit.measures = TRUE, standardized = TRUE)


##Outcome: trolley choice
fullmodel12<-'##regressions
            choice~fight + RATaccuracy + WCSTaccuracy+age+gender_recoded+education
            ##covariances
            RATaccuracy ~~ WCSTaccuracy
            age~~education
            age~~gender_recoded
            education~~gender_recoded
            ##intercepts            
            choice~1'
fitfull12 <- sem(fullmodel12, data = fulldataset_attention, ordered = c("choice"), estimator = "WLSMV")
summary(fitfull12, fit.measures = TRUE, standardized = TRUE)

###Model 2: mediated relations between cognitive flexibility and willingness to die/trolley choice
##Outcome: willingness to die
mediatedmodel11<-'##regressions
              die~fight +age+gender_recoded+education
              fight~RATaccuracy + WCSTaccuracy
              ##covariance
              RATaccuracy ~~ WCSTaccuracy
              age~~education
              age~~gender_recoded
              education~~gender_recoded
              ##intercepts
              die~1
              fight~1'
fitmediated11 <- sem(mediatedmodel11, data = fulldataset_attention)
summary(fitmediated11, fit.measures = TRUE, standardized = TRUE)


##Outcome: trolley choice
mediatedmodel12<-'##regressions
              choice~fight+age+gender_recoded+education
              fight~RATaccuracy + WCSTaccuracy
              ##covariance
              RATaccuracy ~~ WCSTaccuracy
              age~~education
              age~~gender_recoded
              education~~gender_recoded
              ##intercepts
              choice~1
              fight~1'
fitmediated12 <- sem(mediatedmodel12, data = fulldataset_attention, ordered = c("choice"), estimator = "WLSMV")
summary(fitmediated12, fit.measures = TRUE, standardized = TRUE)

####Step 3: Assess Hypothesis 3

##MANOVA model test
outcome<-cbind(fulldataset_attention$RATaccuracy, fulldataset_attention$WCSTaccuracy) ##create outcome measure(s)
model_outcome<-manova(outcome ~ choice, data = fulldataset_attention)
summary(model_outcome, intercept = TRUE)
summary.aov(model_outcome)

###Sub-group analysis of correlations between certainty in choice in trolley dilemma and cognitive flexiblity scores
saveself <- subset(fulldataset_attention, choice==0) #select those who chose to save themselves
cor.test(saveself$certainty_trolley, saveself$RATaccuracy, use = "complete.obs", method = "pearson")
cor.test(saveself$certainty_trolley, saveself$WCSTaccuracy, use = "complete.obs", method = "pearson")

sacrifice <- subset(fulldataset_attention, choice==1) #select those who chose to self-sacrifice
cor.test(sacrifice$certainty_trolley, sacrifice$RATaccuracy, use = "complete.obs", method = "pearson")
cor.test(sacrifice$certainty_trolley, sacrifice$WCSTaccuracy, use = "complete.obs", method = "pearson")

####Step 4: Additional hypotheses tests

###Additional analyses Hypothesis 1

cor.test(fulldataset_attention$Alternatives, fulldataset_attention$fight, use = "complete.obs", method = "pearson")
cor.test(fulldataset_attention$Control, fulldataset_attention$fight, use = "complete.obs", method = "pearson")
cor.test(fulldataset_attention$Alternatives, fulldataset_attention$die, use = "complete.obs", method = "pearson")
cor.test(fulldataset_attention$Control, fulldataset_attention$die, use = "complete.obs", method = "pearson")
cor.test(fulldataset_attention$Alternatives, fulldataset_attention$choice, use = "complete.obs", method = "pearson")
cor.test(fulldataset_attention$Control, fulldataset_attention$choice, use = "complete.obs", method = "pearson")

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
fitfull11b <- sem(fullmodel11b, data = fulldataset_attention)
summary(fitfull11b, fit.measures = TRUE, standardized = TRUE)

#Outcome: trolley choice
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
fitfull12b <- sem(fullmodel12b, data = fulldataset_attention, ordered = c("choice"), estimator = "WLSMV")
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
fitmediated11b <- sem(mediatedmodel11b, data = fulldataset_attention)
summary(fitmediated11b, fit.measures = TRUE, standardized = TRUE)

#Outcome: trolley choice
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
fitmediated12b <- sem(mediatedmodel12b, data = fulldataset_attention, ordered = c("choice"), estimator = "WLSMV")
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
fitfull13 <- sem(fullmodel13, data = fulldataset_attention)
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
fitmediated13 <- sem(mediatedmodel13, data = fulldataset_attention)
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
fitfull13b <- sem(fullmodel13b, data = fulldataset_attention)
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
fitmediated13b <- sem(mediatedmodel13b, data = fulldataset_attention)
summary(fitmediated13b, fit.measures = TRUE, standardized = TRUE)

#Correlation cognitive flexibility tests and normative pro-group behaviour
cor.test(fulldataset_attention$ARIS, fulldataset_attention$RATaccuracy, use = "complete.obs", method = "pearson")
cor.test(fulldataset_attention$ARIS, fulldataset_attention$WCSTaccuracy, use = "complete.obs", method = "pearson")


###Additional analyses Hypothesis 3
##MANOVA with self-report cognitive flexibility scores
outcome_addition<-cbind(fulldataset_attention$Alternatives, fulldataset_attention$Control)
model_addition<-manova(outcome_addition ~ choice, data = fulldataset_attention)
summary(model_addition, intercept = TRUE)
summary.aov(model_addition)

##Correlations whole sample
cor.test(fulldataset_attention$Alternatives, fulldataset_attention$certainty_trolley, use = "complete.obs", method = "pearson")
cor.test(fulldataset_attention$Control, fulldataset_attention$certainty_trolley, use = "complete.obs", method = "pearson")

##Sub-group analysis of correlations
saveself <- subset(fulldataset_attention, choice==0) #select those who chose to save themselves
cor.test(saveself$certainty_trolley, saveself$Alternatives, use = "complete.obs", method = "pearson")
cor.test(saveself$certainty_trolley, saveself$Control, use = "complete.obs", method = "pearson")

sacrifice <- subset(fulldataset_attention, choice==1) #select those who chose to self-sacrifice
cor.test(sacrifice$certainty_trolley, sacrifice$Alternatives, use = "complete.obs", method = "pearson")
cor.test(sacrifice$certainty_trolley, sacrifice$Control, use = "complete.obs", method = "pearson")

###Confirmatory factor analysis Model 1: one latent factor
CFA_model1<- 'progroup =~ killandfight_1+killandfight_2+killandfight_3+killandfight_4+killandfight_5+die'
CFA_model1fit <- cfa(CFA_model1, data=fulldataset_attention)
summary(CFA_model1fit, fit.measures=TRUE)

###Confirmatory factor analysis Model 2: two correlated factors
CFA_model2<- '##two latent factors willingness to fight and willingness to die
              progroup2 =~ killandfight_1+killandfight_2+killandfight_3+killandfight_4+killandfight_5
              die_l=~NA*die'
CFA_model2fit <- cfa(CFA_model2, data=fulldataset_attention)
summary(CFA_model2fit, fit.measures=TRUE)
