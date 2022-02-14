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

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# set seed so we all get same answers 
set.seed(1234)
# create design matrix
# only 2 predictors, remember 1st column is 1s
X <- rnorm(100)
# define "real"/true relationship
real_beta <- 3

# create output variables
# as linear functions of covariates
# (1) binom
y_binom <- rbinom(100, 1, exp(X*real_beta)/(1+exp(X*real_beta)))
# (2) normal
y_norm <- X*real_beta + rnorm(100, 0, 0.5)

# derive our log-likelihood function for binomial distribution
binom_likelihood <- function(outcome, input, parameter) {
  # calculate probability of success on each trial
  p <- exp(parameter[1] + parameter[2]*input)/(1+exp(parameter[1] + parameter[2]*input))
  # access probability density function (pdf) for binomial distribution
  # specifically, calculate log.likelihood function 
  # using sum and negative since its' log, not normal likelihood function
  -sum(dbinom(outcome, 1, p, log=TRUE))
}

# optimise our log-likelihood function
# need to put in par, which are initial values for parameters to be optimized over
# we'll start with zero and 1 for intercept and beta
# using BFGS because it's a quasi-Newton method
# so similar to what we did in class, and what you'll get from glm()
results_binom <- optim(fn=binom_likelihood, outcome=y_binom, input=X, par=0:1, hessian=T, method="BFGS")
# print our estimated coefficients (intercept and beta_1)
results_binom$par
# confirm that we get the same thing in with glm()
coef(glm(y_binom~X, family=binomial))

# now do the same process to derive our log-likelihood function for normal distribution
norm_likelihood <- function(outcome, input, parameter) {
  n      <- nrow(input)
  k      <- ncol(input)
  beta   <- parameter[1:k]
  sigma2 <- parameter[k+1]^2
  e      <- outcome - input%*%beta
  logl   <- -.5*n*log(2*pi)-.5*n*log(sigma2) - ( (t(e) %*% e)/ (2*sigma2) )
  return(-logl)
}
# show you two different ways to set up same likelihood function
norm_likelihood2 <- function(outcome, input, parameter) {
  n <- ncol(input)
  beta <- parameter[1:n]
  sigma <- sqrt(parameter[1+n])
  -sum(dnorm(outcome, input %*% beta, sigma, log=TRUE))
}
# print our estimated coefficients (intercept and beta_1)
results_norm <- optim(fn=norm_likelihood, outcome=y_norm, input=cbind(1, X), par=c(1,1,1), hessian=T, method="BFGS")
results_norm2 <- optim(fn=norm_likelihood, outcome=y_norm, input=cbind(1, X), par=c(1,1,1), hessian=T, method="BFGS")
# print our estimated coefficients (intercept and beta_1)
# get same results regardless of which log-likelihood function we use
results_norm$par; results_norm2$par
# confirm that we get the same thing in with glm()
coef(lm(y_norm~X))


