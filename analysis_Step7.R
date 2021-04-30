###Goals: Complete additional analyses to examine scale properties of willingness to fight and die

###Requirements
install.packages("lavaan")
library(lavaan)

###Confirmatory factor analysis Model 1: one latent factor
CFA_model1<- 'progroup =~ killandfight_1+killandfight_2+killandfight_3+killandfight_4+killandfight_5+die'
CFA_model1fit <- cfa(CFA_model1, data=fulldataset)
summary(CFA_model1fit, fit.measures=TRUE)

###Confirmatory factor analysis Model 2: two correlated factors
CFA_model2<- '##two latent factors willingness to fight and willingness to die
              progroup2 =~ killandfight_1+killandfight_2+killandfight_3+killandfight_4+killandfight_5
              die_l=~ die'
CFA_model2fit <- cfa(CFA_model2, data=fulldataset)
summary(CFA_model2fit, fit.measures=TRUE)


