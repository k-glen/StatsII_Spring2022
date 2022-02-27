#####################
# load libraries
# set wd
# clear global .envir
#####################

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
# lapply(c("stringr"),  pkgTest)

lapply(c("rstudioapi",
         "mgcv",
         "gam",
         "ggplot2"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2022/blob/main/datasets/climateSupport.RData?raw=true"))
ClimSup <- climateSupport

View(ClimSup)

# 1. Remember, we are interested in predicting the likelihood of an individual supporting 
# a policy based on the number of countries participating and the possible sanctions for 
# non-compliance.

# Run some analysis of the data to better understand the variables:

unique(ClimSup$choice)
# 1 = support, 0 = not support 

unique(ClimSup$countries)
# Countries = 3 levels : Levels: 20 of 192 < 80 of 192 < 160 of 192

unique(ClimSup$sanctions)
# Sanctions =  3 Levels: None < 5% < 15% < 20%


# Fit an additive model. Provide the summary output, the global null hypothesis, and p-value. 
# Please describe the results and provide a conclusion.

# Do GLM, binomial family 

glmChoice <- glm(ClimSup$choice ~ ClimSup$countries + ClimSup$sanctions, family=binomial(logit))
summary(glmChoice)

# Output including .L, .Q and .C at the end - indicating Linear, Quadratic and Cubic 
# I do not understand how to interpret these in this format 
glmChoice <- glm(ClimSup$choice ~ ClimSup$countries + ClimSup$sanctions, family=binomial(logit))


# Stackexchange says the problem is that they are ordered, I need to unorder the variables by hand
# So I can see the labels on the factors

# my.variable <- factor(my.variable, ordered=FALSE) # need to try unorder them 
ClimSup$countries <-  factor(ClimSup$countries, ordered = FALSE)
ClimSup$sanctions <- factor(ClimSup$sanctions, ordered = FALSE)

# Run glmChoice again 
glmChoice <- glm(ClimSup$choice ~ ClimSup$countries + ClimSup$sanctions, family=binomial(logit))
summary(glmChoice)
# And they are now labelled so I can understand what it is trying to say. 

# Global Null Hypothesis
anova(glm.reduced, glm.full, test = "Chisq")
# I tried your code, but you didn't show where you got glm.reduced and glm.full from? 

# So instead found this online 
# Significance for the overall model:
# Null deviance: 11783  on 8499  degrees of freedom
# Residual deviance: 11568  on 8494  degrees of freedom
1-pchisq(11783-11568, 8499-8494)
# But I only get this: 0 
# Testing null hypothesis this model is no better at predicting likelihood than one fit with the 
# intercept only. 

# Attempting to create a glm null model by only having Choice (intercept)
glmnull <- glm(ClimSup$choice ~ 1, family=binomial(logit))

# Running anova()
anova(glmnull, glmChoice, test = "Chisq")
# INTERPRETATION: 
# P-value is below 0.05. At least one predictor is reliable in the the model.

# P-Value
# I don't know if you mean something specific about providing "the" p-value 
# But all I can think of is including p-value in my discussion of the coefficients
summary(glmChoice)


# Interpretation of the coefficients
# 1 means support
exp(-0.27266) 
#In the lectures you didn't indicate how to interpret the negative factors as percentages, so I 
# initially referred to e-0.27266 = 0.7613516 = 76%. However Dunteman & Ho indicates that you 
# should take the difference from 0 as the percentage, making 0.7613516 = 100-76 = 24%

#  20 of 192 countries participating is associated with a decrease in likelihood of an individual 
# supporting a policy by e-0.27266 = 0.7613516 = 24% 

exp(0.33636) 
exp(0.64835) 
# 80 of 192 and 160 of 192 countries participating is associated with an increase in likelihood of an 
# individual supporting a policy by e0.33636( = 1.399843 = 40%) and e0.64835 (= 1.912383 = 91%) respectively. 

exp(0.19186) 
# 5% sanctions is associated with an increase in likelihood of an individual supporting a policy 
# by e0.19186 = 1.211501 = 21% increase.
exp(-0.13325)
exp(-0.30356)
# 15% and 20% sanctions is associated with a decrease in likelihood of an individual supporting a 
# policy by e-0.13325 (= 0.8752463 = 12%) and e-0.30356 (= 0.7381856 = 26%) respectively. 

# All of them are significant as their p-value is above .05.

# Conclusion: 
# Small numbers of participating countries in a policy means individuals will not support while 
# higher numbers of participating countries leads to the likelihood of more widespread support. 
# In contrast, individuals will be more likely to support a policy if the sanctions are lower,
# while higher sanctions is associated with a lower likelihood of individual support. 

glmChoice$coefficients
# (a) For the policy in which nearly all countries participate [160 of 192], how does 
# increasing sanctions from 5% to 15% change the odds that an individual will support the 
# policy? (Interpretation of a coefficient)

# Need to do an interaction to see how country participation and sanctions interact 

glmint <- glm(ClimSup$choice ~ ClimSup$countries * ClimSup$sanctions, family=binomial(logit))
summary(glmint)

# ClimSup$countries160 of 192:ClimSup$sanctions5%   0.13009
# ClimSup$countries160 of 192:ClimSup$sanctions15% -0.05165
exp(0.13009) # 1.138931 = 13% increase
exp(-0.05165) # 0.9496612 = 6% decrease 

# Increasing sanctions from 5% to 15% for the policy in which nearly all countries participated
# is associated with a shift from the  likelihood of an individual supporting a policy increasing by 13%
# at 5% to the likelihood of an individual supporting the policy decreasing by 6% at 15%.  


# (b) For the policy in which very few countries participate [20 of 192], how does in- creasing
# sanctions from 5% to 15% change the odds that an individual will support the policy? 
# (Interpretation of a coefficient)

# ClimSup$sanctions5%                               0.12179       
# ClimSup$sanctions15%                             -0.09687

exp(0.12179) # 1.129517 # 13%
exp(-0.09687) # 0.907674 # 9%

# Increasing sanctions from 5% to 15% for the policy in which very few countries participated
# is associated with a shift from the  likelihood of an individual supporting a policy increasing by 13%
# at 5% to the likelihood of an individual supporting the policy decreasing by 9% at 15%.  


# (c) What is the estimated probability that an individual will support a policy if there are
# 80 of 192 countries participating with no sanctions?
exp(0.37562)
# 1.455894
# Probability - increases by 46% 

# (d) Would the answers to 2a and 2b potentially change if we included the interaction term in 
# this model? Why?
summary(glmChoice)
summary(glmint)

# I don't understand this question. I don't know how I would have answered the first two 
# questions if I hadn't included the interaction term. It is also confusing because it refers to 
# 2a and 2b while these questions are only 1a and 1b. Maybe it is from a previous draft of the problem
# set?

#   • Perform a test to see if including an interaction is appropriate.
# Additive and multipilicative
# Compare the additive and multiplicative models using anova to judge whether the deviance is large 
# enough to judge whether using an interaction would give us different information and provide a better
# fit to the data. 

anova(glmChoice, glmint, test="Chisq")

# The deviance is 6.2928, which is large enough a difference between the slopes of the two models 
# that I think using an interaction is appropriate. 

