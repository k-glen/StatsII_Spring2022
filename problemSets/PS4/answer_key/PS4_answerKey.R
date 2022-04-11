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

lapply(c("eha", "survival", "ggfortify"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data on infant mortality by mother's background
data("infants")

# estimate duration Cox PH model that includes both predictors (child, mother)
infantMorality <- coxph(Surv(enter, exit, event) ~ age + sex, data = infants)

# create survival model differenced by extent of cancer growth
infantSurvFit <- survfit(Surv(enter, exit, event) ~ age + sex, data = infants)

infantSurvFit$risk <- 1 - infantSurvFit$n.event/infacn
survivor <- cumprod(risk)

colonSurvFit$extent <- as.factor(matrix(c(rep("Muscle", 16), rep("Serosa", 32),
                                          rep("Submucosa", 34), rep("Contiguous Structures", 21))))
# differentiate time values by extent of growth group
xvals <- tapply(colonSurvFit$time, colonSurvFit$extent, function(x) return(x)) # differentiate survival values by extent of growth group
yvals <- tapply(colonSurvFit$surv, colonSurvFit$extent, function(x) return(x)) # open plot
plot(1:max(unlist(xvals)), ylim=(c(0,max(unlist(yvals)))),
     xlim=(c(0,max(unlist(xvals))+10)),
     xlab="100 Times Days",ylab="Survival Probability", main="Figure 2: Chemotherapy for Stage 2-3 Colon Cancer Difference by Extent of Growth",
     type="n",axes=F)
# add axes
axis(1); axis(2)
# plot lines for each group
mapply(lines, xvals, yvals, col=1:4, pch=19)
# add legend
legend('topright', legend=levels(colonSurvFit$extent), col=1:4, pch=19, bty="n")
