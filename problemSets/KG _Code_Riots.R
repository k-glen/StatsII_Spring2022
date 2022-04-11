if(!require(devtools)) install.packages("devtools") # devtools to download packages from github
library(devtools)
# Load packages 
# (note: you may need to install certain packages first using install.packages(""))
packages <- c("apsrtable",
              "car",
              "dplyr",
              "ggplot2",
              "ebal",
              "ei",
              "gmm", # I added
              "eiPack", # I added 
              "foreign",
              "gdata",
              "geosphere",
              "Hmisc",
              "Matching",
              "mosaic",
              "questionr", # I had to add this myself - not in replication code
              "optmatch",
              "reshape2",
              "RItools",
              "stargazer",
              "tidyverse",
              "weights",
              "xtable",
              "gtsummary",
              "imager")
lapply(packages, require, character.only = TRUE)


# To skip all the data processing, I have included these CSVs to streamline the process: 

#merged_final <- read.csv("./merged_final.csv")
#out <- read.csv("./finalout.csv")
#dat <- read.csv("./dat.csv")


# I focus on precinct data - specifically EdDiff 

# Using code as given in the replication, including data sets
options(stringsAsFactors = FALSE)


## Set up ballot initiative data, run EI

# Start with election data
edu90 = read.csv("/Users/Kate/Desktop/Hacker/Stats - HT/Replication/Data/election_data_121_123.csv")[,-c(1,8)] # remove rowname var
edu92 = read.csv("/Users/Kate/Desktop/Hacker/Stats - HT/Replication/Data/election_data_152_153.csv")[,-c(1,8)]
merged_final = merge(edu90, edu92, by="precinct")
merged_final$precinct = as.character(merged_final$precinct)
colnames(merged_final) = c("precinct", "ballots_cast_90", "P121Y", "P121N", "P123Y", "P123N", 
                           "ballots_cast_92", "P152Y", "P152N", "P153Y", "P153N")

merged_final$pct_pubschool_90 = merged_final$P123Y/merged_final$ballots_cast_90
merged_final$pct_pubschool_92 = merged_final$P152Y/merged_final$ballots_cast_92
merged_final$pct_highered_90 = merged_final$P121Y/merged_final$ballots_cast_90
merged_final$pct_highered_92 = merged_final$P153Y/merged_final$ballots_cast_92

merged_final$pubschool_dif = merged_final$pct_pubschool_92 - merged_final$pct_pubschool_90
merged_final$highered_dif = merged_final$pct_highered_92 - merged_final$pct_highered_90
merged_final$edu_dif = merged_final$pubschool_dif - merged_final$highered_dif

merged_final$pct_pubschool_92_N = 1-merged_final$pct_pubschool_92
merged_final$pct_pubschool_90_N = 1-merged_final$pct_pubschool_90
merged_final$pct_highered_92_N = 1-merged_final$pct_highered_92
merged_final$pct_highered_90_N = 1-merged_final$pct_highered_90


## In text report on shift in voter turnout, section on "Mobilization"
b = read.csv("/Users/Kate/Desktop/Hacker/Stats - HT/Replication/Data/precinct_centroids.csv")[,c("precinct", "X", "Y")]
out = merge(merged_final, b, by="precinct")
ballots_pub_90 = sum(out$P123Y + out$P123N)
ballots_high_90 = sum(out$P121Y + out$P121N)

ballots_pub_92 = sum(out$P152Y + out$P152N)
ballots_high_92 = sum(out$P153Y + out$P153N)


dif92 = ballots_pub_92 - ballots_high_92
dif90 = ballots_pub_90 - ballots_high_90


cat("In text report on shift in voter turnout, section on 'Mobilization'\n")
cat(paste0("Public Schools 1990: ", ballots_pub_90,'\n')) # 376593
cat(paste0("Higher Ed 1990: ", ballots_high_90,'\n')) # 387303
cat(paste0("Public Schools 1992: ", ballots_pub_92,'\n')) # 520970
cat(paste0("Higher Ed 1992: ", ballots_high_92,'\n')) # 503669
cat(paste0("Pub - High 1990: ", dif90,'\n')) # -10710
cat(paste0("Pub - High 1992: ", dif92,'\n'))# 17301


# Load in race data
load("/Users/Kate/Desktop/Hacker/Stats - HT/Replication/Data/RacePercents92.RData") 
merged_final = merge(data92, merged_final, by="precinct") #3982/5917, 67%

merged_final$other = merged_final$pop18 - (merged_final$latino + merged_final$white+merged_final$black + merged_final$asian)
merged_final$remainder = 1 - (merged_final$latinopct + merged_final$whitepct + merged_final$blackpct + merged_final$asianpct)


# CALCULATE EIs

## Set up the EI estimates
## Clean up
# remove rows with 0 race data 
merged_final = merged_final[merged_final$pop18 > 0,] # minus 3 observations
merged_final = merged_final[merged_final$remainder >= 0,] # minus 5 observations

# write.csv(merged_final, "/Users/Kate/Desktop/Hacker/Stats - HT/Replication/Data/merged_final.csv")
merged_final <- read.csv("/Users/Kate/Desktop/Hacker/Stats - HT/Replication/Data/merged_final.csv")
# Next, do the EI estimates
??ei

#install.packages("eiPack")

library(eiPack)

#install.packages("gmm")
library(gmm)
library(ei)


form = cbind(pct_highered_90, pct_highered_90_N) ~ cbind(whitepct,blackpct,latinopct,asianpct, remainder)
eiout_high90 = ei(form, id="precinct", total = "pop18", data=merged_final, sample=20000)
temp = colMeans(eiout_high90$draws$Beta)
df = data.frame(temp[grepl("whitepct.pct_highered_90\\.", names(temp))])
colnames(df) = "white_highered_90"
df$black_highered_90 = temp[grepl("blackpct.pct_highered_90\\.", names(temp))]
df$latino_highered_90 = temp[grepl("latinopct.pct_highered_90\\.", names(temp))]
df$asian_highered_90 = temp[grepl("asianpct.pct_highered_90\\.", names(temp))]
df$other_highered_90 = temp[grepl("remainder.pct_highered_90\\.", names(temp))]

## These EI outputs are LARGE, so best to do this in pieces.
rm(eiout_high90)

# Kept getting errors saying I had exhausted vector memory (limit reached?) 
# Needed to process in smaller pieces, saving df as I went along 

#write.csv(df, "/Users/Kate/Desktop/Hacker/Stats - HT/Replication/Data/halfdf.csv")

# df <- read.csv("/Users/Kate/Desktop/Hacker/Stats - HT/Replication/Data/halfdf.csv")


form = cbind(pct_highered_92, pct_highered_92_N) ~ cbind(whitepct,blackpct,latinopct,asianpct, remainder)
eiout_high92 = ei(form, id="precinct", total = "pop18", data=merged_final, sample=20000)
temp = colMeans(eiout_high92$draws$Beta)
df$white_highered_92 = temp[grepl("whitepct.pct_highered_92\\.", names(temp))]
df$black_highered_92 = temp[grepl("blackpct.pct_highered_92\\.", names(temp))]
df$latino_highered_92 = temp[grepl("latinopct.pct_highered_92\\.", names(temp))]
df$asian_highered_92 = temp[grepl("asianpct.pct_highered_92\\.", names(temp))]
df$other_highered_92 = temp[grepl("remainder.pct_highered_92\\.", names(temp))]
rm(eiout_high92)

# write.csv(df, "/Users/Kate/Desktop/Hacker/Stats - HT/Replication/Data/thirddf.csv")

# df <- read.csv("/Users/Kate/Desktop/Hacker/Stats - HT/Replication/Data/thirddf.csv")

form = cbind(pct_pubschool_90, pct_pubschool_90_N) ~ cbind(whitepct,blackpct,latinopct,asianpct, remainder)
eiout_pub90 = ei(form, id="precinct", total = "pop18", data=merged_final, sample=20000)
temp = colMeans(eiout_pub90$draws$Beta)
df$white_pubschool_90 = temp[grepl("whitepct.pct_pubschool_90\\.", names(temp))]
df$black_pubschool_90 = temp[grepl("blackpct.pct_pubschool_90\\.", names(temp))]
df$latino_pubschool_90 = temp[grepl("latinopct.pct_pubschool_90\\.", names(temp))]
df$asian_pubschool_90 = temp[grepl("asianpct.pct_pubschool_90\\.", names(temp))]
df$other_pubschool_90 = temp[grepl("remainder.pct_pubschool_90\\.", names(temp))]
rm(eiout_pub90)

# write.csv(df, "/Users/Kate/Desktop/Hacker/Stats - HT/Replication/Data/fourthdf.csv")



## Pub92
form = cbind(pct_pubschool_92, pct_pubschool_92_N) ~ cbind(whitepct,blackpct,latinopct,asianpct, remainder)
eiout_pub92 = ei(form, id="precinct", total = "pop18", data=merged_final, sample=20000)
temp = colMeans(eiout_pub92$draws$Beta)
df$white_pubschool_92 = temp[grepl("whitepct.pct_pubschool_92\\.", names(temp))]
df$black_pubschool_92 = temp[grepl("blackpct.pct_pubschool_92\\.", names(temp))]
df$latino_pubschool_92 = temp[grepl("latinopct.pct_pubschool_92\\.", names(temp))]
df$asian_pubschool_92 = temp[grepl("asianpct.pct_pubschool_92\\.", names(temp))]
df$other_pubschool_92 = temp[grepl("remainder.pct_pubschool_92\\.", names(temp))]
rm(eiout_pub92)
rm(form)
rm(temp)

# write.csv(df, "/Users/Kate/Desktop/Hacker/Stats - HT/Replication/Data/finaldf.csv")


## Generate the dif-in-dif
out = cbind(df, merged_final)


# save post - ei's because i am sick of rerunning them when it take my computer so long

out$white_pubdiff = out$white_pubschool_92 - out$white_pubschool_90
out$black_pubdiff = out$black_pubschool_92 - out$black_pubschool_90
out$latino_pubdiff = out$latino_pubschool_92 - out$latino_pubschool_90
out$other_pubdiff = out$other_pubschool_92 - out$other_pubschool_90

out$white_highdiff = out$white_highered_92 - out$white_highered_90
out$black_highdiff = out$black_highered_92 - out$black_highered_90
out$latino_highdiff = out$latino_highered_92 - out$latino_highered_90
out$other_highdiff = out$other_highered_92 - out$other_highered_90

out$white_edudiff = out$white_pubdiff - out$white_highdiff
out$black_edudiff = out$black_pubdiff - out$black_highdiff
out$latino_edudiff = out$latino_pubdiff - out$latino_highdiff
out$other_edudiff = out$other_pubdiff - out$other_highdiff

out$asian_pubdiff = out$asian_pubschool_92 - out$asian_pubschool_90
out$asian_highdiff = out$asian_highered_92 - out$asian_highered_90
out$asian_edudiff = out$asian_pubdiff - out$asian_highdiff

# write.csv(out, "/Users/Kate/Desktop/Hacker/Stats - HT/Replication/Data/out1.csv")
# save completed out dataframe - out1 


### Generate the variance on the dif-in-dif
## Take posterior draws for all four components, then add all 6 covariances together
form = cbind(pct_highered_90, pct_highered_90_N) ~ cbind(whitepct,blackpct,latinopct,asianpct, remainder)
eiout_high90 = ei(form, id="precinct", total = "pop18", data=out, sample=5000)
temp1 = eiout_high90$draws$Beta
rm(eiout_high90)

temp1b = temp1[,grepl("blackpct.pct_highered_90\\.", colnames(temp1))]
temp1w = temp1[,grepl("whitepct.pct_highered_90\\.", colnames(temp1))]
rm(temp1)
gc()

form = cbind(pct_highered_92, pct_highered_92_N) ~ cbind(whitepct,blackpct,latinopct,asianpct, remainder)
eiout_high92 = ei(form, id="precinct", total = "pop18", data=out, sample=5000)
temp2 = eiout_high92$draws$Beta
rm(eiout_high92)

temp2b = temp2[,grepl("blackpct.pct_highered_92\\.", colnames(temp2))]
temp2w = temp2[,grepl("whitepct.pct_highered_92\\.", colnames(temp2))]
rm(temp2)


form = cbind(pct_pubschool_92, pct_pubschool_92_N) ~ cbind(whitepct,blackpct,latinopct,asianpct, remainder)
eiout_pub92 = ei(form, id="precinct", total = "pop18", data=out, sample=5000)
temp4 =eiout_pub92$draws$Beta
temp4b = temp4[,grepl("blackpct.pct_pubschool_92\\.", colnames(temp4))]
temp4w = temp4[,grepl("whitepct.pct_pubschool_92\\.", colnames(temp4))]

rm(eiout_pub92)
rm(temp4)

form = cbind(pct_pubschool_90, pct_pubschool_90_N) ~ cbind(whitepct,blackpct,latinopct,asianpct, remainder)
eiout_pub90 = ei(form, id="precinct", total = "pop18", data=out, sample=5000)
temp3 =eiout_pub90$draws$Beta
temp3b = temp3[,grepl("blackpct.pct_pubschool_90\\.", colnames(temp3))]
temp3w = temp3[,grepl("whitepct.pct_pubschool_90\\.", colnames(temp3))]

rm(eiout_pub90)
rm(temp3)
rm(form)





covs_w = sapply(1:ncol(temp1b), FUN=function(i) 2*cov(temp1w[,i], temp2w[,i]) +
                  2*cov(temp1w[,i], temp3w[,i]) +
                  2*cov(temp1w[,i], temp4w[,i]) + 
                  2*cov(temp2w[,i], temp3w[,i]) + 
                  2*cov(temp2w[,i], temp4w[,i]) + 
                  2*cov(temp3w[,i], temp4w[,i]) + 
                  var(temp1w[,i]) + var(temp2w[,i]) + var(temp3w[,i]) + var(temp4w[,i]))
covs_b = sapply(1:ncol(temp1b), FUN=function(i) cov(temp1b[,i], temp2b[,1]) +
                  2*cov(temp1b[,i], temp3b[,i]) +
                  2*cov(temp1b[,i], temp4b[,i]) + 
                  2*cov(temp2b[,i], temp3b[,i]) + 
                  2*cov(temp2b[,i], temp4b[,i]) + 
                  2*cov(temp3b[,i], temp4b[,i]) + 
                  var(temp1b[,i]) + var(temp2b[,i]) + var(temp3b[,i]) + var(temp4b[,i]))
sds_w = sqrt(covs_w)
sds_b = sqrt(covs_b)


## Add it to 'out'
## Take the inverse
out$black_edudiff_se = 1/sds_b
out$white_edudiff_se = 1/sds_w

write.csv(out, "/Users/Kate/Desktop/Hacker/Stats - HT/Replication/Data/out2.csv", row.names=FALSE, quote=FALSE) 

out1 <- read.csv("/Users/Kate/Desktop/Hacker/Stats - HT/Replication/Data/out2.csv")


## In text report means of EdDiff, section on 'Results: Changes in Policy Support
b = read.csv("/Users/Kate/Desktop/Hacker/Stats - HT/Replication/Data/precinct_centroids.csv")[,c("precinct", "X", "Y")]
out = out[out$precinct %in% b$precinct,]

write.csv(out, "/Users/Kate/Desktop/Hacker/Stats - HT/Replication/Data/finalout.csv")

# edu_dif


library(mosaic)
library(questionr)
# I had to bring in questionr as that is not a mosaic function (maybe was and no longer)

boot_one = function(){
  samp = mosaic::sample(out, replace=T)
  x = wtd.mean(x=samp$edu_dif, samp$pop18)
  return(x)
}
reps = replicate(1000, boot_one())
quantile(reps, c(0.975, 0.025)) 

## Results: Changes in Policy Support: weighted mean of EdDiff 
cat("In text report means of EdDiff, section on 'Results: Changes in Policy Support'\n")
print(wtd.mean(out$edu_dif, out$pop18)) # 0.04905395

all_wtd.mean <- wtd.mean(out$edu_dif, out$pop18)
print(all_wtd.mean)

print(wtd.mean(out$edu_dif, out$pop18) + 1.97*sqrt(Hmisc::wtd.var(out$edu_dif, out$pop18)/length(out))) #upper bound 0.06289554
print(wtd.mean(out$edu_dif, out$pop18) - 1.97*sqrt(Hmisc::wtd.var(out$edu_dif, out$pop18)/length(out))) #lower bound 0.03521237
# I added Hmisc:: for weighted variance 

## Results: Changes in Policy Support: weighted mean of EdDiff 
cat("In text report means of EdDiff by race, section on 'Results by Race'\n")
# white edu_diff
cat("whites \n")
boot_one = function(){
  samp = mosaic::sample(out, replace=T)
  x = wtd.mean(x=samp$white_edudiff, samp$white_edudiff_se)
  return(x)
}
reps = replicate(1000, boot_one())
quantile(reps, c(0.975, 0.025)) # 97.5%       2.5%  0.03240275 0.02693024 
white_wtd.mean <- (wtd.mean(out$white_edudiff, out$white_edudiff_se)) # 0.02972858
print(white_wtd.mean)

print(wtd.mean(out$white_edudiff, out$white_edudiff_se) + 1.97*sqrt(Hmisc::wtd.var(out$white_edudiff, out$white_edudiff_se)/length(out))) # 0.03966345
print(wtd.mean(out$white_edudiff, out$white_edudiff_se) - 1.97*sqrt(Hmisc::wtd.var(out$white_edudiff, out$white_edudiff_se)/length(out))) # 0.01979371


# black edu_diff
cat("Blacks \n")
boot_one = function(){
  samp = mosaic::sample(out, replace=T)
  x = wtd.mean(x=samp$black_edudiff, samp$black_edudiff_se)
  return(x)
}
reps = replicate(1000, boot_one())
quantile(reps, c(0.975, 0.025)) #  97.5%       2.5% 0.07360299 0.06958104 
print(wtd.mean(out$black_edudiff, out$black_edudiff_se)) 
# 0.07162608

black_wtd.mean <- wtd.mean(out$black_edudiff, out$black_edudiff_se)

print(wtd.mean(out$black_edudiff, out$black_edudiff_se) + 1.97*sqrt(Hmisc::wtd.var(out$black_edudiff, out$black_edudiff_se)/length(out))) # 0.07902265
print(wtd.mean(out$black_edudiff, out$black_edudiff_se) - 1.97*sqrt(Hmisc::wtd.var(out$black_edudiff, out$black_edudiff_se)/length(out))) # 0.06422951



# weighted t.test
cat("white/Black t-test \n")

print(weights::wtd.t.test(x=out$white_edudiff, y = out$black_edudiff, weight = out$white_edudiff_se, weighty = out$black_edudiff_se))
# added weights::

#t$test
#[1] "Two Sample Weighted T-Test (Welch)"

#$coefficients
#t.value         df    p.value 

#-31.49325 3095.44823    0.00000 
#$additional
#Difference       Mean.x       Mean.y     Std. Err 
#-0.041897503  0.029728578  0.071626080  0.001330364 

# Very statistically significant 
# Can reject the null hypothesis that there is no difference between 
# Sufficient evidence to think the mean levels in support for education is different between white and black populations in Los Angeles

# DISTANCE
#
#
#
#

##  Distance Regressions
fn = c(-118.3000171, 33.974556)

dists = sapply(1:nrow(b), FUN=function(x) distCosine(c(fn[1], fn[2]), c(b$X[x], b$Y[x])))
b$dist = dists

## Load in the precinct data
dat = merge(b, out, by="precinct")

write.csv(dat, "/Users/Kate/Desktop/Hacker/Stats - HT/Replication/Data/dat.csv")

# Convert to 10km
dat$dist = dat$dist/10000

## distance regressions

white_m0 = lm(white_edudiff ~ dist , data=dat, weight=dat$white_edudiff_se)
black_m0 = lm(black_edudiff ~ dist , data=dat, weight=dat$black_edudiff_se)
white_m1 = lm(white_edudiff ~ dist + I(dist^2), data=dat, weight=dat$white_edudiff_se) # further
black_m1 = lm(black_edudiff ~ dist + I(dist^2), data=dat, weight=dat$black_edudiff_se)
white_m2 = lm(white_edudiff ~ dist + I(dist^2) + I(dist^3), data=dat, weight=dat$white_edudiff_se) # furtherer
black_m2 = lm(black_edudiff ~ dist + I(dist^2) + I(dist^3), data=dat, weight=dat$black_edudiff_se)

all_m0 = lm(edu_dif ~ dist, data=dat, weight=dat$pop18) # in area
all_m1 = lm(edu_dif ~ dist + I(dist^2), data=dat, weight=dat$pop18) # further from area
all_m2 = lm(edu_dif ~ dist + I(dist^2) + I(dist^3), data=dat, weight=dat$pop18) # further from area



#### Figure 2 and Appendix Figure C.4

final2 = dat[,c("precinct", "dist", "black_edudiff", "black", "white_edudiff", "white",
                "edu_dif", "pop18", "white_edudiff_se", "black_edudiff_se")]
final2$dist = final2$dist * 10

final3 = melt(final2, id = c("precinct", "dist", "black", "white"))
final3$variable = as.character(final3$variable)
final3$variable[final3$variable == "black_edudiff"] = "African Americans"
final3$variable[final3$variable == "white_edudiff"] = "Whites"
final3$variable[final3$variable == "edu_dif"] = "All Voters"


final3_v1 = final3[1:(nrow(final3)/2),]
final3_v2 = final3[((nrow(final3)/2)+1):nrow(final3),]
colnames(final3_v2)[6] = "edudiff_se"
final3_v2$variable[final3_v2$variable == "black_edudiff_se"] = "African Americans"
final3_v2$variable[final3_v2$variable == "white_edudiff_se"] = "Whites"
final3_v2$variable[final3_v2$variable == "pop18"] = "All Voters"
final3 = merge(final3_v1, final3_v2)

final3$size = final3$black
final3$size[final3$variable=="Whites"] = final3$white[final3$variable=="Whites"]
final3$size[final3$variable=="All Voters"] = final3$edudiff_se[final3$variable=="All Voters"]



jpeg("output/Figure_2.jpg", height=1200, width = 1800, quality=300)
ggplot(final3[final3$variable == "All Voters",], aes(x=dist, y=value, size=edudiff_se, weight=edudiff_se)) + geom_point() + stat_smooth(method="loess", span=0.5, aes(weight=edudiff_se), level=0.995) + #facet_wrap(~variable, ncol=1) +
  xlab("Distance from Florence/Normandie (kilometers)") + ylab("Difference-in-Differences Value") +scale_size("Population", range=c(0.1, 8)) + 
  guides(size = guide_legend(override.aes = list(linetype=0))) +  theme_bw() +
  theme(text = element_text(size=32),  legend.key = element_rect(size = 5), legend.key.size = unit(1.5, 'lines')) 
dev.off()

jpeg("output/Figure_C4.jpg", height=1500, width = 1500, quality=300)
ggplot(final3[final3$variable != "All Voters",], aes(x=dist, y=value, size=edudiff_se, weight=edudiff_se)) + geom_point() + stat_smooth(method="loess", span=0.5, aes(weight=edudiff_se), level=0.995) + #facet_wrap(~variable, ncol=1) +
  xlab("Distance from Florence/Normandie (kilometers)") + ylab("D-in-D Value") +scale_size("Inverse SE", range=c(0.1, 8)) + 
  guides(size = guide_legend(override.aes = list(linetype=0))) + facet_wrap(~variable, ncol=1)+  theme_bw() + theme(text = element_text(size=28))
dev.off()


## Figure 1: Dif-in-Dif
pdf("./output/yetANOTHERKATEZFigure_1.pdf")
par(mfrow=c(3,1), mar=c(3,5,1,2))
hist(out$edu_dif, xlim = c(-0.1, 0.2), las=1, cex.main=1.5,ylab = "Frequency",
     xlab = "", main="Difference in Differences: All Voters",
     breaks = seq(-.7, .3, by=0.002))
lines(x=c(0,0), y=c(0,100), lty="dotted", lwd=2)
lines(x=rep(weighted.mean(out$edu_dif, w=out$pop18, na.rm=T),2), y=c(0,100), lwd=3, lty="dashed")
hist(out$white_edudiff, xlim = c(-0.1, 0.2),las=1, cex.main=1.5,ylab = "Frequency",
     xlab = "", main="White Voters", breaks=seq(-.8, .4, by=0.002))
lines(x=c(0,0), y=c(0,220), lty="dotted", lwd=2)
lines(x=rep(weighted.mean(out$white_edudiff, w=out$white_edudiff_se, na.rm=T),2), y=c(0,220),  lwd=3, lty="dashed")
hist(out$black_edudiff, xlim = c(-0.1, 0.2),las=1, cex.main=1.5,ylab = "Frequency",
     xlab = "", main="African American Voters", breaks=seq(-.7, .3, by=0.002))
lines(x=rep(weighted.mean(out$black_edudiff,w=out$black_edudiff_se, na.rm=T),2), y=c(0,350),  lwd=3, lty="dashed")
lines(x=c(0,0), y=c(0,350), lty="dotted", lwd=2)
dev.off()


# My own exploration
lm(y~x)
# x = causal (independent), y = effected (dependent)

whiteed <- lm(out$edu_dif~out$whitepct) # (Intercept)  out$whitepct   0.0633       -0.0434  
blacked <- lm(out$edu_dif~out$blackpct) # Intercept)  out$blackpct  0.03398       0.05044 

# plot comparing white and black % 
plot(out$whitepct, out$edu_dif, col="red", main = "Comparing Black and White Education Support Difference", xlab = "Race %", ylab = "Education Difference")
points(out$blackpct, out$edu_dif,col="blue")
legend("bottomright", c("White %","Black %"),cex=.8,col=c("red","blue"),pch=c(1,2))

tbl_regression(whiteed)
# for every pct increase in white voters in a precinct, support for education decreased by -0.04

tbl_regression(blacked)
# for every pct increase in black voters in a precinct, support for education increased by 0.05

tbl_regression(all_m0)
# for every 10km away from riots support for oublic policy change decreases by 0.04

# plot effect of distance 
par(mfrow=c(3,1))
plot(edu_dif ~ dist, data=dat, weight=dat$pop18) # in area
plot(edu_dif ~ dist + I(dist^2), data=dat, weight=dat$pop18) # further from area
plot(edu_dif ~ dist + I(dist^2) + I(dist^3), data=dat, weight=dat$pop18) # further from area

#  regression of effect of distance and % black pop in precinct on edu_dif

summary(lm(dat$edu_dif ~ dat$dist + dat$blackpct))
# dat$dist     -0.020913 pvalue 4.95e-08 ***
# dat$blackpct  0.029303   pvaleu  9.19e-08 ***


# interaction of effect of distance and % black pop in precinct on edu_dif
lm(dat$edu_dif ~ dat$dist * dat$blackpct)

# interaction - p value = 0.5977
summary(lm(dat$edu_dif ~ dat$dist * dat$blackpct))

# While being in a precinct with a high black % and being close to the riots does have a positive effect on 
# education support, they do not have an effect in combination. I suppose this might be because most precincts 
# with high % of black people were close to the riots.

plot(dat$dist, dat$blackpct, main = "Black % and Distance from Riots")

# There is a negative trend of black population percentage and distance from riots. Slightly obscured by again fact 
# that most people don't live in area with a high percentage of black people. 


