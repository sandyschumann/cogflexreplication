###Goals: Conduct further exploratory analyses

###Requirements
install.packages("psych")
install.packages("lavaan")
install.packages("tidySEM")
install.packages("ggplo2")
install.packages("dplyr")
library(lavaan)
library(psych)
library(tidySEM)
library(ggplot2)
library(dplyr)

###Re-examine Hypothesis 2 by considering modification indices
##Model 1 original model testing modification indices
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
modindices(fitfull11, sort = TRUE, maximum.number = 10)

#Model 1 including additional covariances
fullmodel11mod<-'##regressions
            die~fight + RATaccuracy + WCSTaccuracy+age+gender_recoded+education
            ##covariances
            RATaccuracy ~~ WCSTaccuracy
            age~~education
            age~~gender_recoded
            education~~gender_recoded
            RATaccuracy ~~education
            age ~~ RATaccuracy
            ##intercepts            
            die~1'
fitfull11mod <- sem(fullmodel11mod, data = fulldataset)
summary(fitfull11mod, fit.measures = TRUE, standardized = TRUE)

#Model 2 including additional covariances
mediatedmodel11mod<-'##regressions
              die~fight +age+gender_recoded+education
              fight~RATaccuracy + WCSTaccuracy
              ##covariance
              RATaccuracy ~~ WCSTaccuracy
              age~~education
              age~~gender_recoded
              education~~gender_recoded
              RATaccuracy ~~education
              age ~~ RATaccuracy
              ##intercepts
              die~1
              fight~1'
fitmediated11mod <- sem(mediatedmodel11mod, data = fulldataset)
summary(fitmediated11mod, fit.measures = TRUE, standardized = TRUE)

#Compare model fit of modified Model 1 and Model2
lavTestLRT(fitfull11mod, fitmediated11mod)

###Re-examine confirmatory factor analysis of willingness to fight/die scale using modification indices
CFA_model1<- 'progroup =~ killandfight_1+killandfight_2+killandfight_3+killandfight_4+killandfight_5+die'
CFA_model1fit <- cfa(CFA_model1, data=fulldataset)
summary(CFA_model1fit, fit.measures=TRUE)
modindices(CFA_model1fit, sort = TRUE, maximum.number = 5)

##Confirmatory factor analysis Model 1: one latent factor, including covariances based on modification indices
CFA_model1b<- 'progroup1b =~ killandfight_1+killandfight_2+killandfight_3+killandfight_4+killandfight_5+die
              killandfight_2 ~~ killandfight_3
              killandfight_5 ~~ die
              killandfight_2 ~~ killandfight_4
              killandfight_3 ~~ die
              killandfight_1 ~~ die'
CFA_model1bfit <- cfa(CFA_model1b, data=fulldataset)
summary(CFA_model1bfit, fit.measures=TRUE)


##Confirmatory factor analysis model 2: two correlated factors, including covariances based on modification indices
CFA_model2b<- '##latent factors willingness to fight and die
              progroup3 =~ killandfight_1+killandfight_2+killandfight_3+killandfight_4+killandfight_5
              die_l3 =~ die
              ##correlation
              killandfight_2 ~~ killandfight_3
              killandfight_2 ~~ killandfight_4'
CFA_model2bfit <- cfa(CFA_model2b, data=fulldataset)
summary(CFA_model2bfit, fit.measures=TRUE)

#Compare model fit of modified Model 1 and Model2
lavTestLRT(CFA_model1bfit, CFA_model2bfit)

#Plot Model 1 (reproduce Figure 1)
get_layout(CFA_model1bfit)
lay1<- get_layout("","","","","","progroup1b","", "","","", "",
                  "","","","","","","","","","","",
                  "killandfight_1","","killandfight_2","","killandfight_3","","killandfight_4","","killandfight_5","","die", rows = 3)
graph_sem(model = CFA_model1bfit, layout = lay1)


###Re-examine Hypothesis 2 with outcome 'willingness to fight/die'
##Chronbach's alpha for 'willingness to fight/die'
fightdie_scale<- data.frame(fulldataset[ ,1:6])
alpha(fightdie_scale)

##Correlations 'willingness to fight/die' and RAT and WCST
cor.test(fulldataset$fightdie, fulldataset$RATaccuracy, use = "complete.obs", method = "pearson")
cor.test(fulldataset$fightdie, fulldataset$WCSTaccuracy, use = "complete.obs", method = "pearson")

##Model 1: direct relations between cognitive flexibility and willingness to fight/die, inspect modification indices
fullmodel14<-'##regressions
            fightdie~RATaccuracy + WCSTaccuracy+age+gender_recoded+education
            ##covariances
            RATaccuracy ~~ WCSTaccuracy
            age~~education
            age~~gender_recoded
            education~~gender_recoded
            ##intercepts            
            fightdie~1'
fitfull14 <- sem(fullmodel14, data = fulldataset)
summary(fitfull14, fit.measures = TRUE, standardized = TRUE)
modindices(fitfull14, sort = TRUE, maximum.number = 5)

#Model 1: direct relations between cognitive flexibility and willingness to fight/die as well as additional covariances based on modification indices
fullmodel14mod<-'##regressions
            fightdie~RATaccuracy + WCSTaccuracy+age+gender_recoded+education
            ##covariances
            RATaccuracy ~~ WCSTaccuracy
            age~~education
            age~~gender_recoded
            education~~gender_recoded
            RATaccuracy ~~ education
            RATaccuracy ~~ age
            ##intercepts            
            fightdie~1'
fitfull14mod <- sem(fullmodel14mod, data = fulldataset)
summary(fitfull14mod, fit.measures = TRUE, standardized = TRUE)
inspect(fitfull14mod, 'r2') #inspect explained variance

#Compare original Model 1 and Model 1 including 'willingness to fight/die' as well as additional covariances
lavTestLRT(fitfull11mod, fitfull14mod)

#Plot Model 1 including 'willingness to fight/die' as well as additional covariances (reproduce Figure 2)
graph_sem(model = fitfull14mod)

###Re-examine association between cognitive flexibility and normative pro-group behaviour as outcome variable
##Inspect modication indices
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
modindices(fitfull13, sort = TRUE, maximum.number = 10)

##Assess model applying covariances based on modification indices
fullmodel13mod<-'##regressions
            ARIS~fight + RATaccuracy + WCSTaccuracy+age+gender_recoded+education
            fight  ~ gender_recoded
            ##covariances
            RATaccuracy ~~ WCSTaccuracy
            age~~education
            age~~gender_recoded
            education~~gender_recoded
            RATaccuracy ~~education
            RATaccuracy ~~age
            ##intercepts            
            ARIS~1'
fitfull13mod <- sem(fullmodel13mod, data = fulldataset)
summary(fitfull13mod, fit.measures = TRUE, standardized = TRUE)

#Plot model (reproduce Figure 3)
graph_sem(model = fitfull13mod)
get_layout(fitfull13mod)

###Correlations CFI subscales and RAT and WCST
cor.test(fulldataset$Alternatives, fulldataset$RATaccuracy, use = "complete.obs", method = "pearson")
cor.test(fulldataset$Control, fulldataset$RATaccuracy, use = "complete.obs", method = "pearson")
cor.test(fulldataset$Alternatives, fulldataset$WCSTaccuracy, use = "complete.obs", method = "pearson")
cor.test(fulldataset$Control, fulldataset$WCSTaccuracy, use = "complete.obs", method = "pearson")
cor.test(fulldataset$Control, fulldataset$Alternatives, use = "complete.obs", method = "pearson")

