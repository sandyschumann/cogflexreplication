###Goals: Test Hypothesis 2

###Requirements
install.packages("lavaan", dependencies = TRUE)
library(lavaan)

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
fitfull11 <- sem(fullmodel11, data = fulldataset)
summary(fitfull11, fit.measures = TRUE, standardized = TRUE)


##Outcome: trolley choice
fulldataset$choice = as.ordered(fulldataset$choice)
fullmodel12<-'##regressions
            choice~fight + RATaccuracy + WCSTaccuracy+age+gender_recoded+education
            ##covariances
            RATaccuracy ~~ WCSTaccuracy
            age~~education
            age~~gender_recoded
            education~~gender_recoded
            ##intercepts            
            choice~1'
fitfull12 <- sem(fullmodel12, data = fulldataset, ordered = c("choice"), estimator = "WLSMV")
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
fitmediated11 <- sem(mediatedmodel11, data = fulldataset)
summary(fitmediated11, fit.measures = TRUE, standardized = TRUE)


##Outcome: trolley choice
fulldataset$choice = as.ordered(fulldataset$choice)
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
fitmediated12 <- sem(mediatedmodel12, data = fulldataset, ordered = c("choice"), estimator = "WLSMV")
summary(fitmediated12, fit.measures = TRUE, standardized = TRUE)

###Compare model fit of Model 1 and Model2 'willingness to die'
lavTestLRT(fitfull11, fitmediated11)
