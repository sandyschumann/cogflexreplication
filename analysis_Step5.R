###Goals: Identify scale properties of CFI and ARIS scales, including test for normal distribution

###Requirements
install.packages("psych")
install.packages("lavaan")
install.packages("pastecs")
install.packages("ggplot2")
library(psych)
library(lavaan)
library(pastecs)
library(ggplot2)

###Scale properties additional measures
##CFI inventory
#CFA in line with prescribed factors
CFA_CFI<- 'Alternatives_l =~ CFI_1+CFI_3+CFI_5+CFI_6+CFI_8+CFI_12+CFI_13+CFI_14+CFI_16+CFI_18+CFI_19+CFI_20
          Control_l =~ CFI_2+CFI_4+CFI_7+CFI_10+CFI_11+CFI_15+CFI_17'
CFA_CFIfit <- cfa(CFA_CFI, data=fulldataset)
summary(CFA_CFIfit, fit.measures=TRUE)
modindices(CFA_CFIfit, sort = TRUE, maximum.number = 5)

#CFA inlcuding changes proposed by modification indices
CFA_CFImod<- 'Alternatives_l =~ CFI_3+CFI_5+CFI_6+CFI_8+CFI_12+CFI_13+CFI_14+CFI_16+CFI_18+CFI_19+CFI_20
          Control_l =~ CFI_2+CFI_4+CFI_7+CFI_10+CFI_11+CFI_15+CFI_17
          CFI_8 ~~ CFI_14'
CFA_CFIfitmod <- cfa(CFA_CFImod, data=fulldataset)
summary(CFA_CFIfitmod, fit.measures=TRUE)

#Reliability
CFI_Alternatives<-data.frame(fulldataset[,121:131])
alpha(CFI_Alternatives)
CFI_Control<-data.frame(fulldataset[,114:120])
alpha(CFI_Control, check.keys=TRUE)

#Mean and SD
mean(fulldataset$Alternatives)
sd(fulldataset$Alternatives)

mean(fulldataset$Control)
sd(fulldataset$Control)

##ARIS scale
#CFA
#estimators MLM to address that scale is not distributed normally
CFA_ARIS<- 'ARIS_l =~ ARIS_1+ARIS_2+ARIS_3+ARIS_4'
CFA_ARISfit <- cfa(CFA_ARIS, data=fulldataset, estimator = "MLM")
summary(CFA_ARISfit, fit.measures=TRUE)

#Reliability
ARIS_scale<-data.frame(fulldataset[,135:138])
alpha(ARIS_scale)

#Mean and SD
mean(fulldataset$ARIS)
sd(fulldataset$ARIS)

###Test normal distribution of both variables
options(scipen=999) #disable the scientific notation of values, optional

##Skewness, kurtosis, Shapiro-Wilk test
stat.desc(cbind(fulldataset$Alternatives, fulldataset$Control, fulldataset$ARIS), basic = FALSE, norm = TRUE)

##Q-Q plots
qqplot.Alternatives<-qplot(sample = fulldataset$Alternatives, main = "CFI - ALternatives")
qqplot.Alternatives

qqplot.Control<-qplot(sample = fulldataset$Control, main = "CFI - Control")
qqplot.Control

qqplot.ARIS<-qplot(sample = fulldataset$ARIS, main = "Normative pro-group behavior")
qqplot.ARIS

