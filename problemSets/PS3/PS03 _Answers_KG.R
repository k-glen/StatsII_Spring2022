# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
lapply(c("nnet"), pkgTest)
library(nnet)
library("MASS")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Question 1
#####################
# We are interested in how governments’ management of public resources impacts economic prosperity. Our data come from Alvarez, Cheibub, Limongi, and Przeworski (1996) and is labelled gdpChange.csv on GitHub. The dataset covers 135 countries observed between 1950 or the year of independence or the first year forwhich data on economic growth are available (”entry year”), and 1990 or the last year for which data on economic growth are available (”exit year”). The unit of analysis is a particular country during a particular year, for a total > 3,500 observations.
# • Response variable:
#   – GDPWdiff: Difference in GDP between year t and t−1. Possible categories include:
#   ”positive”, ”negative”, or ”no change” • Explanatory variables:
#   – REG: 1=Democracy; 0=Non-Democracy
# – OIL: 1=if the average ratio of fuel exports to total exports in 1984-86 exceeded 50%; 0= otherwise

# Please answer the following questions:

data <- read.csv("/Users/Kate/Desktop/Hacker/Stats - HT/Problem Sets - HT/PS3/gdpChange.csv")

str(data)
View(data)

#  1. Construct and interpret an unordered multinomial logit with GDPWdiff as the output and ”no change” as the reference category, including the estimated cutoff points and coefficients.

# GDPWdiff a count variable, not categories. Needs to be changed. 
attach(data)
cat = c();
for (i in 1: length(GDPWdiff)){
  if (GDPWdiff [i] == 0){cat[i]=0} # no change = 0 
  if(GDPWdiff[i]>0){cat[i]=1} # positive = 1
  if(GDPWdiff[i]<0){cat[i]=2} # negative = 2
} 

data$cat = as.factor(cat)
as.factor(cat) #changed to factor 3 categories
data$OIL = as.factor(data$OIL)
data$REG = as.factor(data$REG)
# did same to OIL and REG because even though they are binary 
# they might not be as factors, better safe than sorry

# need to determine reference category for cat when I run logit
# is 0 "no change"
multinom_model1 <- multinom(cat ~ OIL + REG, data = data)
summary(multinom_model1)

# need to exponentiate those results to interpret 
exp(coef(multinom_model1)[,c(1:3)])

# Interpret
# In a given country, there is an increase in the baseline
# odds that the GDP difference would increase by 4.58 times when 
# the average ratio of fuel exports to total exports in 1984-86 exceeded 50%
# In a given country, there is an increase in the baseline.

# odds that the GDP difference would decrease by 4.78 times when 
# the average ratio of fuel exports to total exports in 1984-86 exceeded 50%.

# In a given country, there is an increase in the baseline
# odds that the GDP difference would increase by 1.77 times when 
# the country is a democracy. 

# In a given country, there is an increase in the baseline
# odds that the GDP difference would decrease by 1.38 times when 
# the country is a democracy. 


#  2. Construct and interpret an ordered multinomial logit with GDPWdiff as the outcome variable, including the estimated cutoff points and coefficients.

m <- MASS::polr(cat ~ OIL + REG, data = data)
summary(m)

#exponentiate

exp(cbind(OR=coef(m), confint(m)))

# For countries whose average ratio of fuel exports to total exports in 1984-86 
# exceeded 50%, the odds of their GDP changing year on year is 1.26X higher
# than countries whose average ratio of fuel exports to total exports in 1984-86
# was less than 50%, holding constant all other variables.

# Countries which are democracies have odds of their GDP changing which are .7X 
# higher than countries that are not democracies, holding constant all other variables.
#####################
# Question 2
#####################

# Consider the data set MexicoMuniData.csv, which includes municipal-level information from Mexico. The outcome of interest is the number of times the winning PAN presidential can- didate in 2006 (PAN.visits.06) visited a district leading up to the 2009 federal elections, which is a count. Our main predictor of interest is whether the district was highly contested, or whether it was not (the PAN or their opponents have electoral security) in the previ- ous federal elections during 2000 (competitive.district), which is binary (1=close/swing district, 0=”safe seat”). We also include marginality.06 (a measure of poverty) and PAN.governor.06 (a dummy for whether the state has a PAN-affiliated governor) as ad- ditional control variables.
# (a) Run a Poisson regression because the outcome is a count variable. Is there evidence that PAN presidential candidates visit swing districts more? Provide a test statistic and p-value.

mexico <- read.csv("/Users/Kate/Desktop/Hacker/Stats - HT/Problem Sets - HT/PS3/MexicoMuniData.csv")
mexico$competitive.district = as.factor(mexico$competitive.district)
mexico$PAN.governor.06 = as.factor(mexico$PAN.governor.06)

poisson <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06, family = poisson (link = "log"), data=mexico)
summary(poisson)

exp(poisson$coefficients)

# competetive.district: Test Statistic = -0.477, P-Value = 0.6336 
# marginality.06: Test Statistic = -17.728, P-Value <2e-16
# PAN.governor.061: Test Statistic = -1.869, P-Value= 0.0617 

# Interpret
# Yes, there is evidence that PAN presidential candidates visit swing districts more. 
# There is an increase in the baseline odds that PAN presidential candidates will visit 
# if the district is a swing district by 0.92 times, holding other variables constant.

# (b) Interpret the marginality.06 and PAN.governor.06 coefficients.
# marginality.06 
# There is an increase in the baseline odds that PAN presidential candidates will visit a district,
# for every increase of the poverty measure, by 0.12 times, holding all other variables constant. 

# PAN.governor.06
# There is an increase in the baseline odds that the PAN presidential candidates will visit a 
# district, if the state has a PAN-affiliated governor, by 0.73 times, holding all other variables constant.

# (c) Provide the estimated mean number of visits from the winning PAN presidential candidate for a hypothetical district that was competitive (competitive.district=1), had an average poverty level (marginality.06 = 0), and a PAN governor (PAN.governor.06=1).
(poisson$coefficients)

competitive.district*1 + marginality.06*0 + PAN.governor.06*1
-0.08135 + (-2.08014*0) + -0.31158 # -0.39293

exp(-0.39293) # = 0.675076

# Estimated mean number of 0.68 visits from the winning PAN presidential candidate to district when 
# competitive = 1, poverty = 0, PAN.governor = 1
