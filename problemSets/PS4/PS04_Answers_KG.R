# We’re interested in modeling the historical causes of infant mortality. We have data from 5641 first-born 
# in seven Swedish parishes 1820-1895. Using the ”infants” dataset in the eha library, fit a Cox Proportional 
# Hazard model using mother’s age and infant’s gender as covariates. Present and interpret the output.


data(infants)
?infants
# event // death = 1, censoring = 0 
infants_surv <- with (infants, Surv(enter, exit, event))


# Cox proportional hazard model 
cox <- coxph(infants_surv ~ age + sex, data = infants)
summary(cox)

# INTERPRATION 

# Age = coef = -0.04, exp(coef) = 0.96 
# Older mothers leads to a 4% increase in infant and maternal mortality, holding sex constant.  

# Sexboy = coef = -0.49, exp(coef) = 0.6
# Boys are 40% less likely to survive than girls are, holding the age of mothers constant. 
