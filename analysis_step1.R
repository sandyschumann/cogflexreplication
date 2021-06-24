###Goals: Describe sample and identify how many participants failed the attention check

###Requirements
##none

###Sample description
##Age
mean(fulldataset$age)
sd(fulldataset$age)

##Gender
gender<-factor(fulldataset$gender, levels= c(0:3), labels = c("female", "male", "non-binary", "prefer not to answer"))
tabgender<-table(gender)
prop.table(tabgender)

##Ethnicity
ethnicity<-factor(fulldataset$ethnicity, levels= c(1:7), labels = c("White", "Hispanic", "Black", "Asian", "Arab", "Mixed ethnicity", "Other"))
tabethnicity<-table(ethnicity)
prop.table(tabethnicity)

##Highest level of education
education<-factor(fulldataset$education, levels= c(1:4), labels = c("University", "A levels", "Compulsory schooling", "Not completed compulsory schooling"))
tabeducation<-table(education)
prop.table(tabeducation)

##Failed attention check
table(fulldataset$attention_check)
