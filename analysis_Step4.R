###Goals: Test Hypothesis 3

###Requirements
##none

###MANOVA
##homogeneity of variances test
by(fulldataset[,151:152], fulldataset$choice, cov)
##MANOVA model test
outcome<-cbind(fulldataset$RATaccuracy, fulldataset$WCSTaccuracy) ##create outcome measure(s)
model_outcome<-manova(outcome ~ choice, data = fulldataset)
summary(model_outcome, intercept = TRUE)
summary.aov(model_outcome)

###Sub-group analysis of correlations between certainty in choice in trolley dilemma and cognitive flexiblity scores
safeself <- subset(fulldataset, choice==0) #select those who chose to safe themselves
cor.test(safeself$certainty_trolley, safeself$RATaccuracy, use = "complete.obs", method = "pearson")
cor.test(safeself$certainty_trolley, safeself$WCSTaccuracy, use = "complete.obs", method = "pearson")

sacrifice <- subset(fulldataset, choice==1) #select those who chose to self-sacrifice
cor.test(sacrifice$certainty_trolley, sacrifice$RATaccuracy, use = "complete.obs", method = "pearson")
cor.test(sacrifice$certainty_trolley, sacrifice$WCSTaccuracy, use = "complete.obs", method = "pearson")
