setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# This neat snippet sets your working directory to wherever the script is located!

###################################################
## This script proceeds in four parts. 
## 1) Individual-level analysis
## 2) Precinct-level analysis
## 3) Voter file analysis
## 4) Survey data analysis
##################################################

##required libraries

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
              "foreign",
              "gdata",
              "geosphere",
              "Hmisc",
              "Matching",
              "mosaic",
              "questionr" # I had to add this myself - not in replication code
              "optmatch",
              "reshape2",
              "RItools",
              "stargazer",
              "tidyverse",
              "weights",
              "xtable")
lapply(packages, require, character.only = TRUE)



###### 1) Individual Analysis
#### Outside of R we impute race and geocode the voters.
#### The gecoding and subsetting of voters was done with ArcGIS
#### To maintain the anonymity of voters, the entire voterfile is not made available with this replication 


##load voterfile and impuated race data
load("/Users/Kate/Desktop/Hacker/Stats - HT/Replication/Data/Voterfile1992.RData")
load("/Users/Kate/Desktop/Hacker/Stats - HT/Replication/Data/race_imputation.RData") 

## Q: What is the proportion of people we impute as white and black, pre and post-riot?
## Tables 2, 3, H.1, and H.2 and Figure H.1
out = vf %>%
  dplyr::select(imputed_race_name, age, treat, DISTANCE) %>%
  dplyr::group_by(treat) %>%
  dplyr::summarise(pct_black = mean(imputed_race_name=="black", na.rm=T),
                   pct_white = mean(imputed_race_name=="white", na.rm=T),
                   avg_age = mean(age),
                   dist = mean(DISTANCE),
                   count = n())

## For Appendix figures with confidence intervials, remove people we imputed using only geography
vf = vf[!is.na(vf$pos_white),]
vf = vf

## Appendix Figure H.1: race imputation distributions
blacks_reg_pre = vf[vf$imputed_race_name == "black" & vf$weekfrom >= 0,]
whites_reg_pre = vf[vf$imputed_race_name == "white"& vf$weekfrom >= 0,]
blacks_reg_post = vf[vf$imputed_race_name == "black"& vf$weekfrom < 0,]
whites_reg_post = vf[vf$imputed_race_name == "white"& vf$weekfrom < 0,]

t.test(length(blacks_reg_pre), length(blacks_reg_post))

length(blacks_reg_pre)
length(blacks_reg_post)

pdf('/Users/Kate/Desktop/Hacker/Stats - HT/Replication/output/KATESFigure_H1.pdf')
par(mfrow=c(3,2))
hist(blacks$pos_black, breaks=200, xlab="P(Black)", main="All Registrants", freq=F, ylim=c(0,80))
hist(whites$pos_white, breaks=200, xlab="P(White)", main="", freq=F, ylim=c(0,80))
hist(blacks_reg_pre$pos_black, breaks=200, xlab="P(Black)", main="Pre-Riot", freq=F, ylim=c(0,50))
hist(whites_reg_pre$pos_white, breaks=200, xlab="P(White)", main="", freq=F, ylim=c(0,50))
hist(blacks_reg_post$pos_black, breaks=200, xlab="P(Black)", main="Post-Riot", freq=F, ylim=c(0,70))
hist(whites_reg_post$pos_white, breaks=200, xlab="P(White)", main="", freq=F, ylim=c(0,70))
dev.off()


#### Appendix Supplement to Tables 2 and 3 (see Appendix H)
## Draw from the race posteriors and get the distribution of pct black and pct white pre- and post-riot

## Select only the columns that are race imputations
temp1 = vf[,59:64]
colnames(temp1) = c("white", "black", "asian", "hispanic", "native", "other")
# Draw the bootstrap races
set.seed(02138)
reps = Hmisc::rMultinom(temp1, m=1000) #an N x M matrix: N registrants, 1000 imputed races based on the imputation posteriors
temp1$treat = vf$treat

# Appendix Table H.1: Supplement to Table 2 
## Generate 1000 bootstrap tables
# Using the same dplyr call as before, but replacing imputed_race with newrace, the bootstrapped race
tab2_reps = lapply(1:1000, FUN=function(x) {
  test = temp1
  test$newrace = reps[,x] #for each draw, extract race distribution for each individual
  out1 = test %>% dplyr::group_by(treat) %>%
    dplyr::summarise(pct_black = mean(newrace=="black", na.rm=T), #extract prob that each indiv is black or white, based on that distribution; summarize, for each of 1000 draws, the prob white and prob black in that set of draws 
                     pct_white = mean(newrace=="white", na.rm=T))
      })
black_control = quantile(sapply(tab2_reps, FUN=function(x) c(x)$pct_black[1]), c(0.05, 0.5, 0.95))
white_control = quantile(sapply(tab2_reps, FUN=function(x) c(x)$pct_white[1]), c(0.05, 0.5, 0.95))
black_treat = quantile(sapply(tab2_reps, FUN=function(x) c(x)$pct_black[2]), c(0.05, 0.5, 0.95))
white_treat = quantile(sapply(tab2_reps, FUN=function(x) c(x)$pct_white[2]), c(0.05, 0.5, 0.95))

tab2_sup = matrix(c(black_control, black_treat,white_control, white_treat), nrow=2, byrow=T)
colnames(tab2_sup) = c("Pre-Riot (lower)", "Pre-Riot (median)", "Pre-Riot (upper)",
                       "Post-Riot (lower)", "Post-Riot (median)", "Post-Riot (upper)")
rownames(tab2_sup) = c("African American", "White")

print(xtable(tab2_sup, digits=3),"/Users/Kate/Desktop/Hacker/Stats - HT/Replication/output/KATETable_H1.tex", type = "latex")
View(xtable(tab2_sup, digits=3))

stargazer(tab2_sup, "/Users/Kate/Desktop/Hacker/Stats - HT/Replication/output/ITRY.tex", type = "tex")

## Table 2:
out[,2] = tab2_sup[1,c(2,5)]
out[,3] = tab2_sup[2,c(2,5)]

print(xtable(t(out),digits = 3), "/Users/Kate/Desktop/Hacker/Stats - HT/Replication/output/KATETable_2.tex", type = 'latex')
View(xtable(t(out),digits = 3))

print(xtable(t(out),digits = 3), "/Users/Kate/Desktop/Hacker/Stats - HT/Replication/output/KATETable_2.tex", type = 'latex')

## Table 3 and Appendix Table H.2: Supplement to Table 3 
tab3_reps = lapply(1:1000, FUN=function(x){
  test = vf
  test$Demreg = ifelse(test$party_abbrev == "D", 1, ifelse(test$party_abbrev == "R", 0, NA))
  test$Repreg = ifelse(test$party_abbrev == "R", 1, ifelse(test$party_abbrev == "D", 0, NA))
  
  test$newrace = reps[,x]
  
  dw<-subset(test, test$newrace=="white") 
  testWR<-t.test(dw$Repreg[dw$treat==0],dw$Repreg[dw$treat==1])
  testWD<-t.test(dw$Demreg[dw$treat==0],dw$Demreg[dw$treat==1])
  
  db<-subset(test, test$newrace=="black") 
  testBR<-t.test(db$Repreg[db$treat==0],db$Repreg[db$treat==1])
  testBD<-t.test(db$Demreg[db$treat==0],db$Demreg[db$treat==1])
  
  out = c(testWR$estimate[1], testWR$estimate[2],
          testWD$estimate[1], testWD$estimate[2],
          testBR$estimate[1], testBR$estimate[2],
          testBD$estimate[1], testBD$estimate[2])
  
    })
WR_pre = quantile(sapply(tab3_reps, FUN=function(x) x[1]), c(0.05, 0.5, 0.95))
WR_post = quantile(sapply(tab3_reps, FUN=function(x) x[2]), c(0.05, 0.5, 0.95))
WD_pre = quantile(sapply(tab3_reps, FUN=function(x) x[3]), c(0.05, 0.5, 0.95))
WD_post = quantile(sapply(tab3_reps, FUN=function(x) x[4]), c(0.05, 0.5, 0.95))
BR_pre = quantile(sapply(tab3_reps, FUN=function(x) x[5]), c(0.05, 0.5, 0.95))
BR_post = quantile(sapply(tab3_reps, FUN=function(x) x[6]), c(0.05, 0.5, 0.95))
BD_pre = quantile(sapply(tab3_reps, FUN=function(x) x[7]), c(0.05, 0.5, 0.95))
BD_post = quantile(sapply(tab3_reps, FUN=function(x) x[8]), c(0.05, 0.5, 0.95))

tab3_sup = matrix(c(WR_pre, WR_post, WD_pre, WD_post,
                    BR_pre, BR_post, BD_pre, BD_post), nrow=4, byrow=T)
colnames(tab3_sup) = c("Pre-Riot (lower)", "Pre-Riot (median)", "Pre-Riot (upper)",
                       "Post-Riot (lower)", "Post-Riot (median)", "Post-Riot (upper)")
rownames(tab3_sup)<-c("White Pr(Reg Republican)","White Pr(Reg Democratic)",
                      "Black Pr(Reg Republican)", "Black Pr(Reg Democratic)")

## Calculate differences
tab3_v2 = as.data.frame(tab3_sup)
tab3_v2$Difference = tab3_v2[,2] - tab3_v2[,5] # difference is pre minus post riot 

View(tab3_v2)

View(tab2_sup[,2])
pre.riot <- tab2_sup[,2]
post.riot <- tab2_sup[,5]

whitez = post.riot[2]
l8rwhitez = pre.riot[2]

t.test(whitez, l8rwhitez, alternative = "less")

View(pre.riot)

post.riot, mu =pre.riot[2]


t.test(post.riot[2], mu =pre.riot[2])
       


wr_p = t.test(sapply(tab3_reps, FUN=function(x) x[1]), sapply(tab3_reps, FUN=function(x) x[2]))$p.value
wd_p = t.test(sapply(tab3_reps, FUN=function(x) x[3]), sapply(tab3_reps, FUN=function(x) x[4]))$p.value
br_p = t.test(sapply(tab3_reps, FUN=function(x) x[5]), sapply(tab3_reps, FUN=function(x) x[6]))$p.value
bd_p = t.test(sapply(tab3_reps, FUN=function(x) x[7]), sapply(tab3_reps, FUN=function(x) x[8]))$p.value

tab3_v2$`p value` = c(wr_p, wd_p, br_p, bd_p)

white_n = c("",nrow(vf[vf$imputed_race_name == "white" & vf$treat == 0 & vf$party_abbrev %in% c("D", "R"),]), "",
            "",nrow(vf[vf$imputed_race_name == "white" & vf$treat == 1 & vf$party_abbrev %in% c("D", "R"),]), "", "", "")
black_n = c("",nrow(vf[vf$imputed_race_name == "black" & vf$treat == 0 & vf$party_abbrev %in% c("D", "R"),]), "",
            "",nrow(vf[vf$imputed_race_name == "black" & vf$treat == 1 & vf$party_abbrev %in% c("D", "R"),]), "", "", "")

tab3_v3 = rbind(tab3_v2[c(1,2),], as.numeric(white_n), tab3_v2[c(3,4),], as.numeric(black_n))

## Table H.2 
print(xtable(tab3_v3, digits=3), "output/Table_H2.tex", type = 'latex')
## Table 3
print(xtable(tab3_v3[,-c(1,3,4,6)], digits=3), "output/Table_3.tex", type = 'latex')


## Tables E.1 and E.2
## Robustness check: analysis comparing 4/28 to 5/4, and 4/27-4/28 to 5/4-5/5 (see appendix) ##
# White voters only
dw<-subset(vf, vf$imputed_race_name=="white") 
dw1<- subset(dw, dw$registration_date=="1992-04-28" | dw$registration_date=="1992-05-04" )
dw2<-subset(dw, dw$registration_date=="1992-04-28" | dw$registration_date=="1992-04-27"|dw$registration_date=="1992-05-04"|dw$registration_date=="1992-05-05" )

ttestWR1<-t.test(dw1$Repreg[dw1$treat==0],dw1$Repreg[dw1$treat==1])
ttestWD1<-t.test(dw1$Demreg[dw1$treat==0],dw1$Demreg[dw1$treat==1])
ttestWR2<-t.test(dw2$Repreg[dw2$treat==0],dw2$Repreg[dw2$treat==1])
ttestWD2<-t.test(dw2$Demreg[dw2$treat==0],dw2$Demreg[dw2$treat==1])

# Black voters only
db<-subset(vf, vf$imputed_race_name=="black") 
db1<- subset(db, db$registration_date=="1992-04-28" | db$registration_date=="1992-05-04" )
db2<-subset(db, db$registration_date=="1992-04-28" | db$registration_date=="1992-04-27"|db$registration_date=="1992-05-04"|db$registration_date=="1992-05-05" )

ttestBR1<-t.test(db1$Repreg[db1$treat==0],db1$Repreg[db1$treat==1])
ttestBD1<-t.test(db1$Demreg[db1$treat==0],db1$Demreg[db1$treat==1])
ttestBR2<-t.test(db2$Repreg[db2$treat==0],db2$Repreg[db2$treat==1])
ttestBD2<-t.test(db2$Demreg[db2$treat==0],db2$Demreg[db2$treat==1])

# Appendix Table E.1 (4/28 to 5/4 comparison):
tests <- list()
tests[[1]]<- ttestWR1
tests[[2]]<-ttestWD1
tests[[3]]<- ttestBR1
tests[[4]]<- ttestBD1
# extract  values using `sapply`
tab1<-sapply(tests, function(x) {
  c(x$estimate[1],
    x$estimate[2],
    x$estimate[2]-x$estimate[1],
    p.value = x$p.value)
  })

ttable<-t(tab1)
ns<-c(sum(dw1$Repreg==1, na.rm=T), 
      sum(dw1$Demreg==1, na.rm=T),
      sum(db1$Repreg==1, na.rm=T),
      sum(db1$Demreg==1, na.rm=T))
ttable<-cbind(ttable, round(ns) )

row.names(ttable)<-c("White Pr(Reg Republican)","White Pr(Reg Democratic)", "Black Pr(Reg Republican)", "Black Pr(Reg Democratic)")
colnames(ttable)<-c("Before riot", "After riot", "Difference",  "p value", "N")

white_n = c(sum(dw1$treat==0& dw1$party_abbrev %in% c("D", "R")), sum(dw1$treat==1& dw1$party_abbrev %in% c("D", "R")), NA,NA)
black_n = c(sum(db1$treat==0& db1$party_abbrev %in% c("D", "R")), sum(db1$treat==1& db1$party_abbrev %in% c("D", "R")), NA,NA)

ttable2 = rbind(ttable[1:2,-5], white_n, ttable[3:4,-5], black_n)
row.names(ttable2)<-c("White Pr(Reg Republican)","White Pr(Reg Democratic)", "White N",
                      "Black Pr(Reg Republican)", "Black Pr(Reg Democratic)", "Black N")
colnames(ttable2)<-c("Before riot", "After riot", "Difference",  "p value")
print(xtable(ttable2, digits=3), "output/Table_E1.tex", type = 'latex')

# Appendix Table E.2 ( 4/27-4/28 to 5/4-5/5 comparison):
tests <- list()
tests[[1]]<- ttestWR2
tests[[2]]<-ttestWD2
tests[[3]]<- ttestBR2
tests[[4]]<- ttestBD2
# extract  values using `sapply`
tab1<-sapply(tests, function(x) {
  c(x$estimate[1],
    x$estimate[2],
    x$estimate[2]-x$estimate[1],
    p.value = x$p.value)
  })

ttable<-t(tab1)
ns<-c(sum(dw2$Repreg==1, na.rm=T), 
      sum(dw2$Demreg==1, na.rm=T),
      sum(db2$Repreg==1, na.rm=T),
      sum(db2$Demreg==1, na.rm=T))
ttable<-cbind(ttable, round(ns) )

row.names(ttable)<-c("White Pr(Reg Republican)","White Pr(Reg Democratic)", "Black Pr(Reg Republican)", "Black Pr(Reg Democratic)")
colnames(ttable)<-c("Before riot", "After riot", "Difference",  "p value", "N")

white_n = c(sum(dw2$treat==0 & dw2$party_abbrev %in% c("D", "R")), sum(dw2$treat==1 & dw2$party_abbrev %in% c("D", "R")), NA,NA)
black_n = c(sum(db2$treat==0 & db2$party_abbrev %in% c("D", "R")), sum(db2$treat==1 & db2$party_abbrev %in% c("D", "R")), NA,NA)

ttable2 = rbind(ttable[1:2,-5], white_n, ttable[3:4,-5], black_n)
row.names(ttable2)<-c("White Pr(Reg Republican)","White Pr(Reg Democratic)", "White N",
                      "Black Pr(Reg Republican)", "Black Pr(Reg Democratic)", "Black N")
colnames(ttable2)<-c("Before riot", "After riot", "Difference",  "p value")
print(xtable(ttable2, digits=3), "output/Table_E2.tex", type = 'latex')


## Table 4: Long-term partisan stability for those registering pre and post riot.
# 4 2x2s: race, pre/post: each 2x2 is D/R 1992/2005
load("Voterfile1992_2004.RData") #merged anonymized 1992 and 2004 data. Merge used full name and birthday.
load("precincts.RData")  #precincts in the sample
merged3 = merged3[merged3$precinct.x %in% prec,]
merged3 = merged3[merged3$DISTANCE <= 100000,]
merged3 = merged3[!is.na(merged3$imputed_race_name),]

##create a white table and black table, pre and post
white.pre.mat = matrix(ncol = 6, nrow = 2)
white.post.mat = matrix(ncol = 6, nrow = 2)
black.pre.mat = matrix(ncol = 6, nrow = 2)
black.post.mat = matrix(ncol = 6, nrow = 2)

#### WHITE PRE: means
white.pre.mat[1,1] = mean(merged3$party[merged3$party_abbrev == "D" & merged3$imputed_race_name == "white" & merged3$treat == 0] == "DEM", na.rm=T)
white.pre.mat[1,3] = mean(merged3$party[merged3$party_abbrev == "D" & merged3$imputed_race_name == "white" & merged3$treat == 0] == "REP", na.rm=T)
white.pre.mat[1,5] = mean(merged3$party[merged3$party_abbrev == "D" & merged3$imputed_race_name == "white" & merged3$treat == 0] %in% c("REP", "DEM"), na.rm=T)
white.pre.mat[2,1] = mean(merged3$party[merged3$party_abbrev == "R" & merged3$imputed_race_name == "white" & merged3$treat == 0] == "DEM", na.rm=T)
white.pre.mat[2,3] = mean(merged3$party[merged3$party_abbrev == "R" & merged3$imputed_race_name == "white" & merged3$treat == 0] == "REP", na.rm=T)
white.pre.mat[2,5] = mean(merged3$party[merged3$party_abbrev == "R" & merged3$imputed_race_name == "white" & merged3$treat == 0] %in% c("REP", "DEM"), na.rm=T)

#### WHITE PRE: Counts
white.pre.mat[1,2] = sum(merged3$party[merged3$party_abbrev == "D" & merged3$imputed_race_name == "white" & merged3$treat == 0] == "DEM", na.rm=T)
white.pre.mat[1,4] = sum(merged3$party[merged3$party_abbrev == "D" & merged3$imputed_race_name == "white" & merged3$treat == 0] == "REP", na.rm=T)
white.pre.mat[1,6] = sum(merged3$party[merged3$party_abbrev == "D" & merged3$imputed_race_name == "white" & merged3$treat == 0] %in% c("REP", "DEM"), na.rm=T)
white.pre.mat[2,2] = sum(merged3$party[merged3$party_abbrev == "R" & merged3$imputed_race_name == "white" & merged3$treat == 0] == "DEM", na.rm=T)
white.pre.mat[2,4] = sum(merged3$party[merged3$party_abbrev == "R" & merged3$imputed_race_name == "white" & merged3$treat == 0] == "REP", na.rm=T)
white.pre.mat[2,6] = sum(merged3$party[merged3$party_abbrev == "R" & merged3$imputed_race_name == "white" & merged3$treat == 0] %in% c("REP", "DEM"), na.rm=T)

#### WHITE POST: means
white.post.mat[1,1] = mean(merged3$party[merged3$party_abbrev == "D" & merged3$imputed_race_name == "white" & merged3$treat == 1] == "DEM", na.rm=T)
white.post.mat[1,3] = mean(merged3$party[merged3$party_abbrev == "D" & merged3$imputed_race_name == "white" & merged3$treat == 1] == "REP", na.rm=T)
white.post.mat[1,5] = mean(merged3$party[merged3$party_abbrev == "D" & merged3$imputed_race_name == "white" & merged3$treat == 1] %in% c("REP", "DEM"), na.rm=T)
white.post.mat[2,1] = mean(merged3$party[merged3$party_abbrev == "R" & merged3$imputed_race_name == "white" & merged3$treat == 1] == "DEM", na.rm=T)
white.post.mat[2,3] = mean(merged3$party[merged3$party_abbrev == "R" & merged3$imputed_race_name == "white" & merged3$treat == 1] == "REP", na.rm=T)
white.post.mat[2,5] = mean(merged3$party[merged3$party_abbrev == "R" & merged3$imputed_race_name == "white" & merged3$treat == 1] %in% c("REP", "DEM"), na.rm=T)

#### WHITE POST: Counts
white.post.mat[1,2] = sum(merged3$party[merged3$party_abbrev == "D" & merged3$imputed_race_name == "white" & merged3$treat == 1] == "DEM", na.rm=T)
white.post.mat[1,4] = sum(merged3$party[merged3$party_abbrev == "D" & merged3$imputed_race_name == "white" & merged3$treat == 1] == "REP", na.rm=T)
white.post.mat[1,6] = sum(merged3$party[merged3$party_abbrev == "D" & merged3$imputed_race_name == "white" & merged3$treat == 1] %in% c("REP", "DEM"), na.rm=T)
white.post.mat[2,2] = sum(merged3$party[merged3$party_abbrev == "R" & merged3$imputed_race_name == "white" & merged3$treat == 1] == "DEM", na.rm=T)
white.post.mat[2,4] = sum(merged3$party[merged3$party_abbrev == "R" & merged3$imputed_race_name == "white" & merged3$treat == 1] == "REP", na.rm=T)
white.post.mat[2,6] = sum(merged3$party[merged3$party_abbrev == "R" & merged3$imputed_race_name == "white" & merged3$treat == 1] %in% c("REP", "DEM"), na.rm=T)

#### BLACK PRE: means
black.pre.mat[1,1] = mean(merged3$party[merged3$party_abbrev == "D" & merged3$imputed_race_name == "black" & merged3$treat == 0] == "DEM", na.rm=T)
black.pre.mat[1,3] = mean(merged3$party[merged3$party_abbrev == "D" & merged3$imputed_race_name == "black" & merged3$treat == 0] == "REP", na.rm=T)
black.pre.mat[1,5] = mean(merged3$party[merged3$party_abbrev == "D" & merged3$imputed_race_name == "black" & merged3$treat == 0] %in% c("REP", "DEM"), na.rm=T)
black.pre.mat[2,1] = mean(merged3$party[merged3$party_abbrev == "R" & merged3$imputed_race_name == "black" & merged3$treat == 0] == "DEM", na.rm=T)
black.pre.mat[2,3] = mean(merged3$party[merged3$party_abbrev == "R" & merged3$imputed_race_name == "black" & merged3$treat == 0] == "REP", na.rm=T)
black.pre.mat[2,5] = mean(merged3$party[merged3$party_abbrev == "R" & merged3$imputed_race_name == "black" & merged3$treat == 0] %in% c("REP", "DEM"), na.rm=T)

#### BLACK PRE: Counts
black.pre.mat[1,2] = sum(merged3$party[merged3$party_abbrev == "D" & merged3$imputed_race_name == "black" & merged3$treat == 0] == "DEM", na.rm=T)
black.pre.mat[1,4] = sum(merged3$party[merged3$party_abbrev == "D" & merged3$imputed_race_name == "black" & merged3$treat == 0] == "REP", na.rm=T)
black.pre.mat[1,6] = sum(merged3$party[merged3$party_abbrev == "D" & merged3$imputed_race_name == "black" & merged3$treat == 0] %in% c("REP", "DEM"), na.rm=T)
black.pre.mat[2,2] = sum(merged3$party[merged3$party_abbrev == "R" & merged3$imputed_race_name == "black" & merged3$treat == 0] == "DEM", na.rm=T)
black.pre.mat[2,4] = sum(merged3$party[merged3$party_abbrev == "R" & merged3$imputed_race_name == "black" & merged3$treat == 0] == "REP", na.rm=T)
black.pre.mat[2,6] = sum(merged3$party[merged3$party_abbrev == "R" & merged3$imputed_race_name == "black" & merged3$treat == 0] %in% c("REP", "DEM"), na.rm=T)

#### BLACK POST: means
black.post.mat[1,1] = mean(merged3$party[merged3$party_abbrev == "D" & merged3$imputed_race_name == "black" & merged3$treat == 1] == "DEM", na.rm=T)
black.post.mat[1,3] = mean(merged3$party[merged3$party_abbrev == "D" & merged3$imputed_race_name == "black" & merged3$treat == 1] == "REP", na.rm=T)
black.post.mat[1,5] = mean(merged3$party[merged3$party_abbrev == "D" & merged3$imputed_race_name == "black" & merged3$treat == 1] %in% c("REP", "DEM"), na.rm=T)
black.post.mat[2,1] = mean(merged3$party[merged3$party_abbrev == "R" & merged3$imputed_race_name == "black" & merged3$treat == 1] == "DEM", na.rm=T)
black.post.mat[2,3] = mean(merged3$party[merged3$party_abbrev == "R" & merged3$imputed_race_name == "black" & merged3$treat == 1] == "REP", na.rm=T)
black.post.mat[2,5] = mean(merged3$party[merged3$party_abbrev == "R" & merged3$imputed_race_name == "black" & merged3$treat == 1] %in% c("REP", "DEM"), na.rm=T)

#### BLACK POST: Counts
black.post.mat[1,2] = sum(merged3$party[merged3$party_abbrev == "D" & merged3$imputed_race_name == "black" & merged3$treat == 1] == "DEM", na.rm=T)
black.post.mat[1,4] = sum(merged3$party[merged3$party_abbrev == "D" & merged3$imputed_race_name == "black" & merged3$treat == 1] == "REP", na.rm=T)
black.post.mat[1,6] = sum(merged3$party[merged3$party_abbrev == "D" & merged3$imputed_race_name == "black" & merged3$treat == 1] %in% c("REP", "DEM"), na.rm=T)
black.post.mat[2,2] = sum(merged3$party[merged3$party_abbrev == "R" & merged3$imputed_race_name == "black" & merged3$treat == 1] == "DEM", na.rm=T)
black.post.mat[2,4] = sum(merged3$party[merged3$party_abbrev == "R" & merged3$imputed_race_name == "black" & merged3$treat == 1] == "REP", na.rm=T)
black.post.mat[2,6] = sum(merged3$party[merged3$party_abbrev == "R" & merged3$imputed_race_name == "black" & merged3$treat == 1] %in% c("REP", "DEM"), na.rm=T)

rnames = c('Dem','Rep')
cnames = c('Dem','Dem.N','Rep','Rep.N','Total','Total.N')
row.names(white.pre.mat) =rnames; colnames(white.pre.mat) =cnames
row.names(white.post.mat) =rnames; colnames(white.post.mat) =cnames
row.names(black.pre.mat) =rnames; colnames(black.pre.mat) =cnames
row.names(black.post.mat) =rnames; colnames(black.post.mat) =cnames
white.pre.mat = round(white.pre.mat,2)
white.post.mat = round(white.post.mat,2)
black.pre.mat = round(black.pre.mat,2)
black.post.mat = round(black.post.mat,2)

print(xtable(white.pre.mat, digits=2), "output/Table4_white_pre.tex", type = 'latex')
print(xtable(white.post.mat, digits=2), "output/Table4_white_post.tex", type = 'latex')
print(xtable(black.pre.mat, digits=2), "output/Table4_black_pre.tex", type = 'latex')
print(xtable(black.post.mat, digits=2), "output/Table4_black_post.tex", type = 'latex')

#### Appendix Tables G.1 and G.2 
vote_elig = function(x){
  z = sum(x < as.Date("1992-11-3"),
          x < as.Date("1994-11-8"),
          x < as.Date("1996-11-5"),
          x < as.Date("1998-11-3"),
          x < as.Date("2000-11-7"),
          x < as.Date("2002-11-5"),
          x < as.Date("2004-11-2"))
  return(z*2)
}

merged3$times_voted = rowSums(merged3[,141:158]== "TRUE")
merged3$times_eligible = sapply(merged3$registrationdate, vote_elig)
merged3$times_voted_norm = merged3$times_voted/merged3$times_eligible
merged3$times_voted_norm[is.na(merged3$times_voted_norm )] = 0
merged3$times_voted_norm[merged3$times_voted_norm > 1] = 1

## Voted in 2004 analysis
merged3$PG4 = as.logical(merged3$PG4)
merged3$PP4 = as.logical(merged3$PP4)
merged3$party_id = "ThirdParty"
merged3$party_id[merged3$party_abbrev == "DS"] = "DeclineToState"
merged3$party_id[merged3$party_abbrev == "R"] = "Rep"
merged3$party_id[merged3$party_abbrev == "D"] = "Dem"

m1 = (lm(PP4 ~ treat, data=merged3[merged3$imputed_race=="white",]))
m2 = (lm(PP4 ~ treat, data=merged3[merged3$imputed_race=="black",]))
m3 = (lm(PG4 ~ treat, data=merged3[merged3$imputed_race=="white",]))
m4 = (lm(PG4 ~ treat, data=merged3[merged3$imputed_race=="black",]))
m5 = (lm(PP4 ~ treat + age + gender + party_id, data=merged3[merged3$imputed_race=="white",]))
m6 = (lm(PP4 ~ treat+ age + gender + party_id, data=merged3[merged3$imputed_race=="black",]))
m7 = (lm(PG4 ~ treat+ age + gender + party_id, data=merged3[merged3$imputed_race=="white",]))
m8 = (lm(PG4 ~ treat+ age + gender + party_id, data=merged3[merged3$imputed_race=="black",]))

tabl = apsrtable(m1, m2,  m5, m6, 
                 model.names = c("Primary 04: White", "Primary 04: Black", "Primary 04: White",
                                 "Primary 04: Black"))
writeLines(tabl, file("output/Table_G1.tex"))

tabl2 = apsrtable(m3, m4,  m7, m8,
                  model.names = c("General 04: White", "General 04: Black", "General 04: White",
                                  "General 04: Black"))
writeLines(tabl2, con="output/Table_G2.tex")


########### Appendix Table C.3, generating EI estimates

set.seed(12345) ##set seed to make output constant

# election data 1986
load("ElectionData1986.RData")
dat56 = read.csv("election_data_56.csv")
data_1986 = merge(data_1986, dat56, by="precinct")
data_1986 = data_1986[,c(1,3,4,5,10,11)]

load("RacePercents92.RData") #  race data
merged_final = merge(data92, data_1986, by="precinct")

load("ElectionData92.RData") # election data 1992
data92 = data92[,c("precinct", "pr146y_90", "pr146n_90", "pr143y_90", "pr143n_90")]
merged_final = merge(merged_final, data92, by="precinct")

merged_final$pct_school_53 = merged_final$prop53Y/(merged_final$prop53Y + merged_final$prop53N)
merged_final$pct_school_53_N = 1-merged_final$pct_school_53
merged_final$pct_school_146 = merged_final$pr146y_90/(merged_final$pr146y_90 + merged_final$pr146n_90)
merged_final$pct_school_146_N = 1-merged_final$pct_school_146
merged_final$pct_school_56 = merged_final$prop56Y/(merged_final$prop56Y + merged_final$prop56N)
merged_final$pct_school_56_N = 1-merged_final$pct_school_56
merged_final$pct_school_143 = merged_final$pr143y_90/(merged_final$pr143y_90 + merged_final$pr143n_90)
merged_final$pct_school_143_N = 1-merged_final$pct_school_143

merged_final$pubschool_dif = merged_final$pct_school_146 - merged_final$pct_school_53
merged_final$highered_dif = merged_final$pct_school_143 - merged_final$pct_school_56
merged_final$edu_dif = merged_final$pubschool_dif - merged_final$highered_dif

b = read.csv("precinct_centroids.csv")[,c("precinct", "X", "Y")]
merged_final = merge(b, merged_final, by="precinct")


form = cbind(pct_school_56, pct_school_56_N) ~ cbind(whitepct,blackpct,latinopct, asianpct,remainder)
eiout_high86 = ei(form, id="precinct", total = "pop18", data=merged_final, sample=5000)
temp = colMeans(eiout_high86$draws$Beta)
df = data.frame(temp[grepl("whitepct.pct_school_56\\.", names(temp))])
colnames(df) = "white_highered_86"
df$black_highered_86 = temp[grepl("blackpct.pct_school_56\\.", names(temp))]
df$latino_highered_86 = temp[grepl("latinopct.pct_school_56\\.", names(temp))]
df$other_highered_86= temp[grepl("remainder.pct_school_56\\.", names(temp))]
rm(eiout_high86)

form = cbind(pct_school_143, pct_school_143_N) ~ cbind(whitepct,blackpct,latinopct,asianpct, remainder)
eiout_high90 = ei(form, id="precinct", total = "pop18", data=merged_final, sample=5000)
temp = colMeans(eiout_high90$draws$Beta)
df$white_highered_90 = temp[grepl("whitepct.pct_school_143\\.", names(temp))]
df$latino_highered_90 = temp[grepl("latinopct.pct_school_143\\.", names(temp))]
df$black_highered_90 = temp[grepl("blackpct.pct_school_143\\.", names(temp))]
df$other_highered_90 = temp[grepl("remainder.pct_school_143\\.", names(temp))]
rm(eiout_high90)

form = cbind(pct_school_53, pct_school_53_N) ~ cbind(whitepct,blackpct,latinopct, asianpct, remainder)
eiout_school86 = ei(form, id="precinct", total = "pop18", data=merged_final, sample=5000)
temp = colMeans(eiout_school86$draws$Beta)
df$white_pubschool_86 = temp[grepl("whitepct.pct_school_53\\.", names(temp))]
df$latino_pubschool_86 = temp[grepl("latinopct.pct_school_53\\.", names(temp))]
df$black_pubschool_86 = temp[grepl("blackpct.pct_school_53\\.", names(temp))]
df$other_pubschool_86 = temp[grepl("remainder.pct_school_53\\.", names(temp))]
rm(eiout_school86)

form = cbind(pct_school_146, pct_school_146_N) ~ cbind(whitepct,blackpct,latinopct,asianpct, remainder)
eiout_school90 = ei(form, id="precinct", total = "pop18", data=merged_final, sample=5000)
temp = colMeans(eiout_school90$draws$Beta)
df$white_pubschool_90 = temp[grepl("whitepct.pct_school_146\\.", names(temp))]
df$latino_pubschool_90 = temp[grepl("latinopct.pct_school_146\\.", names(temp))]
df$black_pubschool_90 = temp[grepl("blackpct.pct_school_146\\.", names(temp))]
df$other_pubschool_90 = temp[grepl("remainder.pct_school_146\\.", names(temp))]
rm(eiout_school90)

df$white_pubdiff = df$white_pubschool_90 - df$white_pubschool_86
df$black_pubdiff = df$black_pubschool_90 - df$black_pubschool_86
df$latino_pubdiff = df$latino_pubschool_90 - df$latino_pubschool_86
df$other_pubdiff = df$other_pubschool_90 - df$other_pubschool_86

df$white_highdiff = df$white_highered_90 - df$white_highered_86
df$black_highdiff = df$black_highered_90 - df$black_highered_86
df$latino_highdiff = df$latino_highered_90 - df$latino_highered_86
df$other_highdiff = df$other_highered_90 - df$other_highered_86

df$white_edudiff = df$white_pubdiff - df$white_highdiff
df$black_edudiff = df$black_pubdiff - df$black_highdiff
df$latino_edudiff = df$latino_pubdiff - df$latino_highdiff
df$other_edudiff = df$other_pubdiff - df$other_highdiff

## Now add the inverse SE estimation
form = cbind(pct_school_56, pct_school_56_N) ~ cbind(whitepct,blackpct,latinopct, asianpct,remainder)
eiout_high86 = ei(form, id="precinct", total = "pop18", data=merged_final, sample=5000)
temp1 = eiout_high86$draws$Beta
rm(eiout_high86)

temp1b = temp1[,grepl("blackpct.pct_school_56\\.", colnames(temp1))] 
temp1w = temp1[,grepl("whitepct.pct_school_56\\.", colnames(temp1))]
rm(temp1)
gc()

form = cbind(pct_school_143, pct_school_143_N) ~ cbind(whitepct,blackpct,latinopct,asianpct, remainder)
eiout_high90 = ei(form, id="precinct", total = "pop18", data=merged_final, sample=5000)
temp2 = eiout_high90$draws$Beta
rm(eiout_high90)

temp2b = temp2[,grepl("blackpct.pct_school_143\\.", colnames(temp2))]
temp2w = temp2[,grepl("whitepct.pct_school_143\\.", colnames(temp2))]
rm(temp2)


form = cbind(pct_school_53, pct_school_53_N) ~ cbind(whitepct,blackpct,latinopct, asianpct, remainder)
eiout_school86 = ei(form, id="precinct", total = "pop18", data=merged_final, sample=5000)
temp4 =eiout_school86$draws$Beta
temp4b = temp4[,grepl("blackpct.pct_school_53\\.", colnames(temp4))]
temp4w = temp4[,grepl("whitepct.pct_school_53\\.", colnames(temp4))]

rm(eiout_school86)
rm(temp4)

form = cbind(pct_school_146, pct_school_146_N) ~ cbind(whitepct,blackpct,latinopct,asianpct, remainder)
eiout_school90 = ei(form, id="precinct", total = "pop18", data=merged_final, sample=5000)
temp3 =eiout_school90$draws$Beta
temp3b = temp3[,grepl("blackpct.pct_school_146\\.", colnames(temp3))]
temp3w = temp3[,grepl("whitepct.pct_school_146\\.", colnames(temp3))]

rm(eiout_school90)
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
sds_w = 1/sqrt(covs_w)
sds_b = 1/sqrt(covs_b)

merged_final$white_edudiff_se = sds_w
merged_final$black_edudiff_se = sds_b

merged_final = cbind(merged_final, df)

fn = c(-118.3000171, 33.974556)
merged_final$dist = sapply(1:nrow(merged_final), FUN=function(x) distCosine(c(fn[1], fn[2]), c(merged_final$X[x], merged_final$Y[x])))
merged_final$dist = merged_final$dist/10000

dat = merged_final

white_m0 = lm(white_edudiff ~ dist , data=dat, weight=dat$white_edudiff_se)
black_m0 = lm(black_edudiff ~ dist , data=dat, weight=dat$black_edudiff_se)
all_m0 = lm(edu_dif ~ dist , data=dat, weight=dat$pop18)
white_m1 = lm(white_edudiff ~ dist + I(dist^2), data=dat, weight=dat$white_edudiff_se)
black_m1 = lm(black_edudiff ~ dist + I(dist^2), data=dat, weight=dat$black_edudiff_se)
all_m1 = lm(edu_dif ~ dist  + I(dist^2), data=dat, weight=dat$pop18)

writeLines(apsrtable(all_m0, all_m1, white_m0, white_m1, black_m0, black_m1), file("output/Table_C3.tex"))

#######################################
##### 2) Precinct-level analysis
#######################################

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




### yo yo yo 
#
#
#
#
#
#
#
#
#
##
#
#
#
#
#
#
#
#
##
#
#


## In text report on shift in voter turnout, section on "Mobilization"
b = read.csv("/Users/Kate/Desktop/Hacker/Stats - HT/Replication/Data/precinct_centroids.csv")[,c("precinct", "X", "Y")]
out = merge(merged_final, b, by="precinct")
ballots_pub_90 = sum(out$P123Y + out$P123N)
ballots_high_90 = sum(out$P121Y + out$P121N)

yes90 = (out$P121Y + out$P123Y)

yes92 = (out$P152Y + out$P153Y)

t.test(yes90, yes92)

ballots_pub_92 = sum(out$P152Y + out$P152N)
ballots_high_92 = sum(out$P153Y + out$P153N)


dif92 = ballots_pub_92 - ballots_high_92
dif90 = ballots_pub_90 - ballots_high_90


cat("In text report on shift in voter turnout, section on 'Mobilization'\n")
cat(paste0("Public Schools 1990: ", ballots_pub_90,'\n'))
cat(paste0("Higher Ed 1990: ", ballots_high_90,'\n'))
cat(paste0("Public Schools 1992: ", ballots_pub_92,'\n'))
cat(paste0("Higher Ed 1992: ", ballots_high_92,'\n'))
cat(paste0("Pub - High 1990: ", dif90,'\n'))
cat(paste0("Pub - High 1992: ", dif92,'\n'))


# Load in race data
load("/Users/Kate/Desktop/Hacker/Stats - HT/Replication/Data/RacePercents92.RData") 
merged_final = merge(data92, merged_final, by="precinct") #3982/5917, 67%

merged_final$other = merged_final$pop18 - (merged_final$latino + merged_final$white+merged_final$black + merged_final$asian)
merged_final$remainder = 1 - (merged_final$latinopct + merged_final$whitepct + merged_final$blackpct + merged_final$asianpct)


## Set up the EI estimates
## Clean up
# remove rows with 0 race data 
merged_final = merged_final[merged_final$pop18 > 0,] # minus 3 observations
merged_final = merged_final[merged_final$remainder >= 0,] # minus 5 observations

# Next, do the EI estimates
??ei

#nstall.packages("eiPack")

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

form = cbind(pct_highered_92, pct_highered_92_N) ~ cbind(whitepct,blackpct,latinopct,asianpct, remainder)
eiout_high92 = ei(form, id="precinct", total = "pop18", data=merged_final, sample=20000)
temp = colMeans(eiout_high92$draws$Beta)
df$white_highered_92 = temp[grepl("whitepct.pct_highered_92\\.", names(temp))]
df$black_highered_92 = temp[grepl("blackpct.pct_highered_92\\.", names(temp))]
df$latino_highered_92 = temp[grepl("latinopct.pct_highered_92\\.", names(temp))]
df$asian_highered_92 = temp[grepl("asianpct.pct_highered_92\\.", names(temp))]
df$other_highered_92 = temp[grepl("remainder.pct_highered_92\\.", names(temp))]
rm(eiout_high92)


form = cbind(pct_pubschool_90, pct_pubschool_90_N) ~ cbind(whitepct,blackpct,latinopct,asianpct, remainder)
eiout_pub90 = ei(form, id="precinct", total = "pop18", data=merged_final, sample=20000)
temp = colMeans(eiout_pub90$draws$Beta)
df$white_pubschool_90 = temp[grepl("whitepct.pct_pubschool_90\\.", names(temp))]
df$black_pubschool_90 = temp[grepl("blackpct.pct_pubschool_90\\.", names(temp))]
df$latino_pubschool_90 = temp[grepl("latinopct.pct_pubschool_90\\.", names(temp))]
df$asian_pubschool_90 = temp[grepl("asianpct.pct_pubschool_90\\.", names(temp))]
df$other_pubschool_90 = temp[grepl("remainder.pct_pubschool_90\\.", names(temp))]
rm(eiout_pub90)


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


## Generate the dif-in-dif
out = cbind(df, merged_final)

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


lm(out$blackpct ~ out$edu_dif) # (Intercept)  out$edu_dif  0.1704       1.8022  
lm(out$whitepct ~ out$edu_dif) # (Intercept)  out$edu_dif  0.4748      -2.0338  
lm(out$pct_highered_90 ~ out$whitepct) #  (Intercept)  out$whitepct  0.62485      -0.06455
lm(out$pct_highered_90 ~ out$blackpct) # (Intercept)  out$blackpct  0.5731        0.1070
lm(out$blackpct~out$pct_highered_90) # (Intercept)  out$pct_highered_90  -0.5503               1.3409 
plot(lm(out$pct_highered_90 ~ out$blackpct))



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

## In text report means of EdDiff, section on 'Results: Changes in Policy Support
b = read.csv("/Users/Kate/Desktop/Hacker/Stats - HT/Replication/Data/precinct_centroids.csv")[,c("precinct", "X", "Y")]
out = out[out$precinct %in% b$precinct,]

View(samp)
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
quantile(reps, c(0.975, 0.025)) # 97.5%     2.5%  #15.14057 14.12652 
print(wtd.mean(out$white_edudiff, out$white_edudiff_se)) # 14.62108

print(wtd.mean(out$white_edudiff, out$white_edudiff_se) + 1.97*sqrt(Hmisc::wtd.var(out$white_edudiff, out$white_edudiff_se)/length(out))) # 16.70979
print(wtd.mean(out$white_edudiff, out$white_edudiff_se) - 1.97*sqrt(Hmisc::wtd.var(out$white_edudiff, out$white_edudiff_se)/length(out))) # 12.53237


# black edu_diff
cat("Blacks \n")
boot_one = function(){
  samp = mosaic::sample(out, replace=T)
  x = wtd.mean(x=samp$black_edudiff, samp$black_edudiff_se)
  return(x)
}
reps = replicate(1000, boot_one())
quantile(reps, c(0.975, 0.025)) # 97.5%     2.5%  #11.51809 10.57794 
print(wtd.mean(out$black_edudiff, out$black_edudiff_se)) 
# 11.04314

print(wtd.mean(out$black_edudiff, out$black_edudiff_se) + 1.97*sqrt(Hmisc::wtd.var(out$black_edudiff, out$black_edudiff_se)/length(out))) # 12.80186
print(wtd.mean(out$black_edudiff, out$black_edudiff_se) - 1.97*sqrt(Hmisc::wtd.var(out$black_edudiff, out$black_edudiff_se)/length(out))) # 9.284422


# weighted t.test
cat("white/Black t-test \n")

print(weights::wtd.t.test(x=out$white_edudiff, y = out$black_edudiff, weight = out$white_edudiff_se, weighty = out$black_edudiff_se))
# added weights::

# [1] "Two Sample Weighted T-Test (Welch)"

#$coefficients
#t.value         df    p.value 
#13.99383 3255.59199    0.00000 

#$additional
#Difference     Mean.x     Mean.y   Std. Err 
#3.5779385 14.6210778 11.0431393  0.2556797 



## Figure 1: Dif-in-Dif
pdf("./output/Figure_1.pdf")
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



## Appendix Figure C.1: referendum change by race

jpeg("output/Figure_C1.jpg", height=1800, width=1400, quality=400)
par(mfrow=c(3,2), oma = c(5,4,0,0) + 0.1, mar = c(5,5,2,2) + 0.1)
scatter.smooth(out$edu_dif~out$whitepct, lpars=list(lwd=3, col="red"), ylab="Diff-in-Diff ", xlab="", pch=46, cex=8, cex.lab=3.5, cex.main=6, cex.axis=2.5, ylim=c(-0.3,0.3), xlim=c(0,1), ann=F)
mtext(side = 2, text = "Diff-in-Diff", line = 4, cex=3.5)
scatter.smooth(out$edu_dif~out$blackpct, lpars=list(lwd=3, col="red"),  ylab="", xlab="", pch=46, cex=8, cex.main=6, ylim=c(-0.3,0.3), xlim=c(0,1), cex.axis=2.5)
scatter.smooth(out$highered_dif~out$whitepct, lpars=list(lwd=3, col="red"), ann=F, xlab="", ylab="Higher Ed", pch=46, cex=8, cex.lab=3.5, ylim=c(-0.3,0.3), xlim=c(0,1), cex.axis=2.5)
mtext(side = 2, text = "Higher Ed", line = 4, cex=3.5)
scatter.smooth(out$highered_dif~out$blackpct, lpars=list(lwd=3, col="red"),  xlab="", ylab="", pch=46, cex=8, ylim=c(-0.3,0.3), xlim=c(0,1), cex.axis=2.5)
scatter.smooth(out$pubschool_dif~out$whitepct, lpars=list(lwd=3, col="red"), ann=F, xlab="Precinct Proportion White", ylab="Public School", pch=46, cex=8, cex.lab=3.5, ylim=c(-0.3,0.3), xlim=c(0,1), cex.axis=2.5)
mtext(side = 2, text = "Public School", line = 4, cex=3.5)
mtext(side = 1, text = "Precinct Proportion White", line = 5.5, cex=3.5)
scatter.smooth(out$pubschool_dif~out$blackpct, lpars=list(lwd=3, col="red"), ann=F,  xlab="Precinct Proportion African American", ylab="", pch=46, cex=8, cex.lab=3.5, ylim=c(-0.3,0.3), xlim=c(0,1), cex.axis=2.5)
mtext(side = 1, text = "Precinct Proportion African American", line = 5.5, cex=3.5)
dev.off()




## Appendix Table C.1: Distance Regressions
fn = c(-118.3000171, 33.974556)

dists = sapply(1:nrow(b), FUN=function(x) distCosine(c(fn[1], fn[2]), c(b$X[x], b$Y[x])))
b$dist = dists

## Load in the precinct data
dat = merge(b, out, by="precinct")

# Convert to 10km
dat$dist = dat$dist/10000

## distance regressions
white_m0 = lm(white_edudiff ~ dist , data=dat, weight=dat$white_edudiff_se)
black_m0 = lm(black_edudiff ~ dist , data=dat, weight=dat$black_edudiff_se)
white_m1 = lm(white_edudiff ~ dist + I(dist^2), data=dat, weight=dat$white_edudiff_se)
black_m1 = lm(black_edudiff ~ dist + I(dist^2), data=dat, weight=dat$black_edudiff_se)
white_m2 = lm(white_edudiff ~ dist + I(dist^2) + I(dist^3), data=dat, weight=dat$white_edudiff_se)
black_m2 = lm(black_edudiff ~ dist + I(dist^2) + I(dist^3), data=dat, weight=dat$black_edudiff_se)

all_m0 = lm(edu_dif ~ dist, data=dat, weight=dat$pop18)
all_m1 = lm(edu_dif ~ dist + I(dist^2), data=dat, weight=dat$pop18)

writeLines(apsrtable(all_m0, all_m1, white_m0, white_m1, black_m0, black_m1, digits=3), file("output/Table_C1.tex"))


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
  xlab("Distance from Florence/Normandie (kilometers)") + ylab("Difference-in-Differences Value") +scale_size("Inverse SE", range=c(0.1, 8)) + 
  guides(size = guide_legend(override.aes = list(linetype=0))) + facet_wrap(~variable, ncol=1)+  theme_bw() + theme(text = element_text(size=28))
dev.off()



### Now, using the code above, we generate all n*n precinct distances
deaths = read.csv("TopBlackPrecincts.csv")

idx = expand.grid(1:nrow(dat), 1:nrow(deaths))
alldists = sapply(1:nrow(idx), FUN=function(x) distCosine(c(dat$X[idx$Var1[x]], dat$Y[idx$Var1[x]]), c(deaths$X[idx$Var2[x]], deaths$Y[idx$Var2[x]])))
idx$dists = alldists
idx$dists = idx$dists/10000
idx$prec1 = dat$precinct[idx$Var1]
idx$prec2 = deaths$precinct[idx$Var2]
idx$x1 = dat$X[idx$Var1]
idx$x2 = deaths$X[idx$Var2]
idx$y1 = dat$Y[idx$Var1]
idx$y2 = deaths$Y[idx$Var2]

idx2 =reshape2::dcast(idx, prec1~prec2, value.var="dists")
idx2[is.na(idx2)] = 0

## Then we merge this in with the data set
dat1 = merge(dat, idx2, by.y="prec1", by.x="precinct")

########### The next 100 lines produce the CSV file to generate Figure 3 in the main text, and appendix figures C.2 and C.3
########### There is an ArcGIS step to visualize, noted below.

## Run the distance regression using each alternate distance as an IV and store the coefficient
coefs_lin_white = c()
sigs_lin_white = c()
coefs_linquad_white = c()
coefs_quad_white = c()
sigs_linquad_white = c()
sigs_quad_white = c()
fscore_white = c()
fscore_sig_white = c()

coefs_lin_black = c()
sigs_lin_black = c()
coefs_linquad_black = c()
coefs_quad_black = c()
sigs_linquad_black = c()
sigs_quad_black = c()
fscore_black = c()
fscore_sig_black = c()

coefs_all_lin = c()
coefs_all_linquad = c()
sig_all_lin = c()
sig_all_linquad = c()

for(i in 1:nrow(deaths)){
  precname = deaths$precinct[i]
  temp = dat1
  temp$iv = temp[,as.character(precname)]
  temp$iv2 = temp$iv^2
  
  # Quadratic regression for whites
  temp_m = lm(white_edudiff ~ iv + iv2, dat=temp, weights = white_edudiff_se)
  coefs_linquad_white[i] = coef(temp_m)[2]
  coefs_quad_white[i] = coef(temp_m)[3]
  sigs_linquad_white[i] = summary(temp_m)[5]$coefficients[2,4]
  sigs_quad_white[i] = summary(temp_m)[5]$coefficients[3,4]
  f = summary(temp_m)$fstatistic
  p = pf(f[1],f[2],f[3],lower.tail=F)
  fscore_white[i] = f[1]
  fscore_sig_white[i] = p
  # Just linear for whites
  temp_m = lm(white_edudiff ~ iv, dat=temp, weights = white_edudiff_se)
  coefs_lin_white[i] = coef(temp_m)[2]
  sigs_lin_white[i] = summary(temp_m)[5]$coefficients[2,4]
  
  # Quadratic regression for blacks
  temp_m = lm(black_edudiff ~ iv + iv2, dat=temp, weights = black_edudiff_se)
  coefs_linquad_black[i] = coef(temp_m)[2]
  coefs_quad_black[i] = coef(temp_m)[3]
  sigs_linquad_black[i] = summary(temp_m)[5]$coefficients[2,4]
  sigs_quad_black[i] = summary(temp_m)[5]$coefficients[3,4]
  f = summary(temp_m)$fstatistic
  p = pf(f[1],f[2],f[3],lower.tail=F)
  fscore_black[i] = f[1]
  fscore_sig_black[i] = p
  # Just linear for blacks
  temp_m = lm(black_edudiff ~ iv, dat=temp, weights = black_edudiff_se)
  coefs_lin_black[i] = coef(temp_m)[2]
  sigs_lin_black[i] = summary(temp_m)[5]$coefficients[2,4]
  
  # All edu_dif, not just white
  temp_m2 = lm(edu_dif ~ iv + iv2, dat=temp, weights=pop18)
  coefs_all_linquad[i] = coef(temp_m2)[2]
  sig_all_linquad[i] = summary(temp_m2)[5]$coefficients[2,2]
  temp_m2 = lm(edu_dif ~ iv, dat=temp, weights=pop18)
  coefs_all_lin[i] = coef(temp_m2)[2]
  sig_all_lin[i] = summary(temp_m2)[5]$coefficients[2,2]
  
}

## Output a data set of coefficient size by precinct, with some associated precinct-level metadata

dat2 = deaths[,c("precinct", "X", "Y", "black", "blackpct", "white", "whitepct", "black_edudiff", "black_edudiff_se",
                 "white_edudiff", "white_edudiff_se", "edu_dif", "pop18")]
dat2$coef_linquad_white = coefs_linquad_white
dat2$coef_quad_white = coefs_quad_white
dat2$coef_lin_white = coefs_lin_white
dat2$sig_linquad_white = sigs_linquad_white
dat2$sig_quad_white = sigs_quad_white
dat2$sig_lin_white = sigs_lin_white
dat2$fscore_white = fscore_white
dat2$fscore_sig_white = fscore_sig_white

dat2$coef_linquad_black = coefs_linquad_black
dat2$coef_quad_black = coefs_quad_black
dat2$coef_lin_black = coefs_lin_black
dat2$sig_linquad_black = sigs_linquad_black
dat2$sig_quad_black = sigs_quad_black
dat2$sig_lin_black = sigs_lin_black
dat2$fscore_black = fscore_black
dat2$fscore_sig_black = fscore_sig_black

dat2$coef_all_lin = coefs_all_lin
dat2$coef_all_linquad = coefs_all_linquad
dat2$sig_all_lin = sig_all_lin
dat2$sig_all_linquad = sig_all_linquad

write.csv(dat2, file="Table2_data.csv")


##### Then there is an ArcGIS step to geocode these results
#### This code generates Appendix Fig C.5

dat = data.table::fread('Table2_data_geocoded.csv')

dat$abs_lin = abs(dat$coef_all_lin)
summary(dat$abs_lin)
summary(dat$abs_lin[dat$in_elipse == 1])
summary(dat$abs_lin[dat$in_elipse == 0])
t.test(dat$abs_lin[dat$in_elipse == 1],dat$abs_lin[dat$in_elipse == 0])


summary(dat$coef_all_lin[dat$in_elipse == 1])
summary(dat$coef_all_lin[dat$in_elipse == 0])
t.test(dat$coef_all_lin[dat$in_elipse == 1],dat$coef_all_lin[dat$in_elipse == 0])

dat$NearRiot = ifelse(dat$in_elipse == 1,'Yes','No')
colors = c('gray60', 'gray23')

density.plot = ggplot(dat,aes(x=coef_all_lin, fill= NearRiot))  +
  geom_density(position="identity", alpha=0.6) +
  scale_fill_manual( values = colors) +
  guides(fill=guide_legend(title="Riot Area")) +
  xlab(expression(beta)) +
  ylab("Density")

ggsave('output/Figure_C5.pdf',
       density.plot)

dat$round_abs_lin = as.character(round(abs(dat$coef_all_lin),2))



####### Remove precincts with competitive races
#read data
comp = read.csv('competitive_districts.csv')
## Merge this with above data set
dat2 = merge(out, comp, by.x="precinct", by.y="Group_1")
dat_comp = dat2[dat2$compdist_any == 1,]
dat_noncomp = dat2[dat2$compdist_any == 0,]


## Appendix Figure A.1
final=dat_comp
pdf("output/Figure_A1.pdf")
par(mfrow=c(3,1), mar=c(3,5,1,2))
hist(final$edu_dif, xlim = c(-0.1, 0.2), las=1, cex.main=1.5,ylab = "Frequency",
     xlab = "", main="Difference in Differences: All Voters",
     breaks = seq(-.8, .4, by=0.002))
lines(x=c(0,0), y=c(0,100), lty="dotted", lwd=2)
lines(x=rep(weighted.mean(final$edu_dif, w=final$pop18, na.rm=T),2), y=c(0,100), lwd=3, lty="dashed")
hist(final$white_edudiff, xlim = c(-0.1, 0.2),las=1, cex.main=1.5,ylab = "Frequency",
     xlab = "", main="White Voters", breaks = seq(-.8, .4, by=0.002))
lines(x=c(0,0), y=c(0,220), lty="dotted", lwd=2)
lines(x=rep(weighted.mean(final$white_edudiff, w=final$white_edudiff_se, na.rm=T),2), y=c(0,220),  lwd=3, lty="dashed")
hist(final$black_edudiff, xlim = c(-0.1, 0.2),las=1, cex.main=1.5,ylab = "Frequency",
     xlab = "", main="African American Voters", breaks = seq(-.8, .4, by=0.002))
lines(x=rep(weighted.mean(final$black_edudiff,w=final$black_edudiff_se, na.rm=T),2), y=c(0,350),  lwd=3, lty="dashed")
lines(x=c(0,0), y=c(0,350), lty="dotted", lwd=2)
dev.off()

## Appendix Figure A.2

final=dat_noncomp
pdf("output/Figure_A2.pdf")
par(mfrow=c(3,1), mar=c(3,5,1,2))
hist(final$edu_dif, xlim = c(-0.1, 0.2), las=1, cex.main=1.5,ylab = "Frequency",
     xlab = "", main="Difference in Differences: All Voters",
     breaks = seq(-.8, .4, by=0.002))
lines(x=c(0,0), y=c(0,100), lty="dotted", lwd=2)
lines(x=rep(weighted.mean(final$edu_dif, w=final$pop18, na.rm=T),2), y=c(0,100), lwd=3, lty="dashed")
hist(final$white_edudiff, xlim = c(-0.1, 0.2),las=1, cex.main=1.5,ylab = "Frequency",
     xlab = "", main="White Voters",breaks = seq(-.8, .4, by=0.002))
lines(x=c(0,0), y=c(0,220), lty="dotted", lwd=2)
lines(x=rep(weighted.mean(final$white_edudiff, w=final$white_edudiff_se, na.rm=T),2), y=c(0,220),  lwd=3, lty="dashed")
hist(final$black_edudiff, xlim = c(-0.1, 0.2),las=1, cex.main=1.5,ylab = "Frequency",
     xlab = "", main="African American Voters",breaks = seq(-.8, .4, by=0.002))
lines(x=rep(weighted.mean(final$black_edudiff,w=final$black_edudiff_se, na.rm=T),2), y=c(0,350),  lwd=3, lty="dashed")
lines(x=c(0,0), y=c(0,350), lty="dotted", lwd=2)
dev.off()





########### 3) Voter file results

## Aggregate them to precincts
vf6 = vf %>%
  filter(treat==1) %>%
  group_by(precinct) %>%
  summarise(pctdem = mean(party_abbrev == "D"), Registrants = n(), num_postriot = sum(treat))

## Merge
dat = merge(out, vf6, by="precinct")


## Appendix Figure I.1

png("output/Figure_I1.png", height=720, width=960)
ggplot(dat, aes(x=pctdem, y=edu_dif, size=Registrants, weight=Registrants)) +  geom_point() + geom_smooth(method='lm',formula=y~x) +
  ylab("EdDiff") + xlab("Democratic Proportion of Post-Riot Registrants") + scale_size(range=c(0.01, 5)) + theme(text = element_text(size=20)) +
  guides(size = guide_legend(override.aes = list(linetype=0)))
dev.off()

## Appendix Figure I.2

png("output/Figure_I2.png", height=720, width=960)
ggplot(dat, aes(x=num_postriot, y=edu_dif, size=Registrants, weight=Registrants)) +  geom_point() + geom_smooth(method='lm',formula=y~x) +
  ylab("EdDiff") + xlab("Number of Post-Riot Registrants")  + scale_size(range=c(0.01, 3)) + theme(text = element_text(size=20)) +
  guides(size = guide_legend(override.aes = list(linetype=0)))
dev.off()

## Appendix Figure E.2 
##### load 2005 voter file 
dat05<-read.csv("voter_file05_forappendix.csv", stringsAsFactors = F)
dat05$registrationdate = as.Date(dat05$registrationdate)
dat05$week = cut(dat05$registrationdate, "week")

dat3 = dat05 %>%
  group_by(week) %>%
  dplyr::summarize(dempct = mean(party == "DEM"),
                   numreg = n())
dat3 = dat3[as.Date(dat3$week) > as.Date("2003-1-1"),] 
dat3$week = as.Date(as.character(dat3$week))  

dat3 = dat3[as.Date(dat3$week) < as.Date("2005-12-31"),] 

out.plot = ggplot(dat3, aes(x=as.POSIXct(week), y=numreg)) + geom_point() + stat_smooth(method="loess", span=0.1) +
  ylab("Number of Weekly Registrants") + xlab("Date") + 
  guides(size = guide_legend(override.aes = list(linetype=0))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_vline(xintercept=as.numeric(as.POSIXlt("2004-2-16")) , col="blue", linetype="dashed") +
  geom_vline(xintercept=as.numeric(as.POSIXlt("2004-10-9")) , col="grey", linetype="dashed")

ggsave('output/Figure_E2.pdf',out.plot)

## Appendix Figure E.1 
##### load enitre 1992 voter file 
dat92<-read.csv("voter_file92_forappendix.csv", stringsAsFactors = F)
dat92$registrationdate = as.Date(as.character(dat92$registration_date), "%Y-%m-%d") 
dat92$week = cut(dat92$registrationdate, "week")

dat4 = dat92 %>%
  group_by(week) %>%
  dplyr::summarize(dempct = mean(party_abbrev == "D "),
                   numreg = n()) 

dat4 = dat4[as.Date(dat4$week) > as.Date("1990-01-01"),] 
dat4$week = as.Date(as.character(dat4$week))  

out.plot = ggplot(dat4, aes(x=as.POSIXct(week), y=numreg)) + geom_point() + stat_smooth(method="loess", span=0.1) +
  ylab("Number of Weekly Registrants") + xlab("Date") + 
  guides(size = guide_legend(override.aes = list(linetype=0))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_vline(xintercept=as.numeric(as.POSIXlt("1992-5-8")) , col="red", linetype="dashed") + 
  geom_vline(xintercept=as.numeric(as.POSIXlt("1990-5-7")) , col="blue", linetype="dashed") +
  geom_vline(xintercept=as.numeric(as.POSIXlt("1990-10-10")), col="grey", linetype="dashed") +
  geom_vline(xintercept=as.numeric(as.POSIXlt("1992-10-5")), col="grey", linetype="dashed") +
  geom_vline(xintercept=as.numeric(as.POSIXlt("1994-10-3")), col="grey", linetype="dashed") 

ggsave('output/Figure_E1.pdf',out.plot)

## Appendix Figure E.3 
out.plot = ggplot(dat4, aes(x=as.POSIXct(week), y=dempct, size=numreg, weight = numreg)) + geom_point() + stat_smooth(method="loess", span=0.1) +
  scale_size("Number of Registrants", range=c(0.5,3)) +
  ylab("Weekly Registrants: Proportion Democrat") + xlab("Date") + 
  ylim(0,1)+
  guides(size = guide_legend(override.aes = list(linetype=0))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_vline(xintercept=as.numeric(as.POSIXlt("1992-5-8")) , col="red", linetype="dashed") + 
  geom_vline(xintercept=as.numeric(as.POSIXlt("1990-5-7")) , col="blue", linetype="dashed") +
  geom_vline(xintercept=as.numeric(as.POSIXlt("1990-10-10")), col="grey", linetype="dashed") +
  geom_vline(xintercept=as.numeric(as.POSIXlt("1992-10-5")), col="grey", linetype="dashed") 

ggsave('output/Figure_E3.pdf',out.plot)


## Appendix Figure E.4 
dat3x<-dat3[as.Date(dat3$week) < as.Date("2005-10-01"),] 
out.plot = ggplot(dat3, aes(x=as.POSIXct(week), y=dempct, size=numreg, weight = numreg)) + geom_point() + stat_smooth(method="loess", span=0.1) +
  scale_size("Number of Registrants", range=c(0.5,3)) +
  ylab("Weekly Registrants: Proportion Democrat") + xlab("Date") + 
  ylim(0,1)+
  guides(size = guide_legend(override.aes = list(linetype=0))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_vline(xintercept=as.numeric(as.POSIXlt("2004-2-16")) , col="blue", linetype="dashed") +
  geom_vline(xintercept=as.numeric(as.POSIXlt("2004-10-9")) , col="grey", linetype="dashed") 

ggsave('output/Figure_E4.pdf',out.plot)


####################################
###4. Survey data analysis (LACSS) ###
####################################



# Load LACSS data
d<-read.spss("LACSS92.sav", to.data.frame=TRUE, use.value.labels = FALSE) 

# Subset to survey respondents in LA basin zip codes
load("zip_fn_distances.RData")
dat<-merge(d, data, by.x="dm14", by.y="zip.code", all.x=T, all.y=F)
d<-subset(dat, dat$distance<=15000)

# Recode variables 
d$treat<-ifelse(d$b4rk==0, 1,0)

# recode race & ethnicity
d$eth1[d$eth1==99]<- NA 
d$white<-ifelse(d$eth1==1, 1,0)
d$hisp<-ifelse(d$eth1==2, 1,0)
d$black<-ifelse(d$eth1==3, 1,0)
d$asian<-ifelse(d$eth1==4, 1,0)

#recode pbm4- 1= spending too much, 2- spending too little, 3=right amount (originally categorical)
#recode refused and don't know
d$pbm4[d$pbm4==8 |d$pbm4==9 ]<- NA #Spending - improving nation's educ sys
d$pbm4_recode<- ifelse(d$pbm4==2,3, NA) ##spending too little = 3
d$pbm4_recode<- ifelse(d$pbm4==3,2, d$pbm4_recode) #spending right amount is 2
d$pbm4_recode<- ifelse(d$pbm4==1,1, d$pbm4_recode) #spending too much=1

#recode ideology and party ID 
d$Conservative<-ifelse(d$pa05==2,1,0)
d$Dem<-ifelse(d$pa11==1, 1,0)  
d$Rep<-ifelse(d$pa11==2, 1,0)
d$Indep<-ifelse(d$pa11==3, 1,0) 

#Respondent's demographics - controls
d$dm06[d$dm06==9 ]<- NA 
d$hsgrad<-ifelse(d$dm06==1,1,0)
d$college<-ifelse(d$dm07==1,1,0)
d$rage[d$rage==99 ]<- NA #age
d$inc1[d$inc1==8 |d$inc1==9 ]<- NA ##hh income
d$incbelow30k<-ifelse(d$inc1==5,1,0)
d$female<-ifelse(d$rsex==5, 1, 0)
d$ownhome<-ifelse(d$nbh2==1,1,0)
d$married<-ifelse(d$mrs1==1,1,0)
d$unemployed<-ifelse(d$wrk1==4,1,0)

## Appendix Table D.3

## subset to  black only 
d1<-subset(d, d$black==1) #185 whites; 426 black

##SPENDING variables (pbm), 1= spending too much -> 3= spending too little
reg1<-lm(pbm4_recode ~ treat  , data=d1)
reg2<-lm(pbm4_recode ~ treat +rage +ownhome +distance + college+ married+female+ incbelow30k +Rep , data=d1) #Spending - improving nation's educ sys

## ideology (black)
d1$pa05[d1$pa05==8 | d1$pa05==9 | d1$pa05==4 ]<- NA 
d1$pa05_recode<-ifelse(d1$pa05==2,3,ifelse(d1$pa05==3,2,d1$pa05)) #1= lib; 2=moderate, 3= conservative

reg3<-lm(pa05_recode ~ treat , data=d1)
reg4<-lm(pa05_recode ~ treat +rage +ownhome +distance +married+female + college + incbelow30k + Rep, data=d1)


out.table = stargazer(reg1, reg2, reg3,reg4, 
          covariate.labels = c("After verdict"),
          add.lines = list(c("Controls?", "No", "Yes", "No", "Yes")),
          keep = c("treat", "Constant"),
          column.labels = c("Spending too little on education", "More conservative"),
          column.separate = c(2, 2),
          star.cutoffs = c(0.05),
          dep.var.labels.include = FALSE)

writeLines(capture.output(out.table), file("output/Table_D3.tex"))


## Appendix Table D.1

db<-d[d$black==1 & d$distance<=15000,]
attach(db)

mb<-MatchBalance(treat ~ hsgrad + college + incbelow30k+ female + ownhome + married 
                 +unemployed + rage +Conservative + Dem + Rep + Indep + distance )

varnames<-c ( "hsgrad",  "college" ,"incbelow30k" , "female" ,"ownhome" , "married",
              "unemployed", "age", "Conservative", "Democrat", "Republican" , "Independent", "distance" )

btest<-baltest.collect(mb, varnames, after=F) 
btest1<-data.frame(btest)
btest1$Difference<-btest1$mean.Tr-btest1$mean.Co
btest1<-btest1[c( "mean.Co", "mean.Tr", "Difference", "T.pval")]
btest1<-round(btest1,2)
names(btest1)<- c("Before riot", "After riot", "Difference",  "p value")


xbtest<-xtable(btest1)
print(xbtest, "output/Table_D1.tex", type = 'latex')



detach(db)

## Appendix Table D.2

dw<-d[d$white==1 & d$distance<=15000,]
attach(dw)

mb<-MatchBalance(treat ~ hsgrad + college + incbelow30k+ female + ownhome + married 
                 +unemployed + rage +Conservative + Dem + Rep + Indep + distance )

varnames<-c ( "hsgrad",  "college" ,"incbelow30k" , "female" ,"ownhome" , "married",
              "unemployed", "age", "Conservative", "Democrat", "Republican" , "Independent", "distance" )

btest<-baltest.collect(mb, varnames, after=F) 
btest1<-data.frame(btest)
btest1$Difference<-btest1$mean.Tr-btest1$mean.Co
btest1<-btest1[c( "mean.Co", "mean.Tr", "Difference", "T.pval")]
btest1<-round(btest1,2)
names(btest1)<- c("Before riot", "After riot", "Difference",  "p value")


xbtest<-xtable(btest1)

print(xbtest, "output/Table_D2.tex", type = 'latex')


detach(dw)


###### omnibus balance test


### omnibus test - African Americans (Table D.1 caption)
dbx<-subset(db, db$treat==1 | db$treat==0) ## drop obs w/ missing treatment variable

cat('Table D.1 Caption: balance test\n')
print(xBalance(treat ~ hsgrad + college + incbelow30k+ female + ownhome + married  +unemployed + rage +Conservative + Dem + Rep + Indep + distance , 
         data = dbx, 
         report = c("chisquare.test")))


###  omnibus test - white (Table D.2 caption)
dwx<-subset(dw, dw$treat==1 | dw$treat==0) ## drop obs w/ missing treatment variable

cat('Table D.2 Caption: balance test\n')
print(xBalance(treat ~ hsgrad + college + incbelow30k+ female + ownhome + married  +unemployed + rage +Conservative + Dem + Rep + Indep + distance , 
         data = dwx, 
         report = c("chisquare.test")))




