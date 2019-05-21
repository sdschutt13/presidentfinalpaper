## Hypothesis 1 Testing on Correlation ##
## Sam Schutt 2019 ##



### OPENING SET-UP ###
#####
packages_required <- c('foreign',
                       'car',
                       'SDMTools',
                       'texreg',
                       'plyr',
                       'Zelig',
                       'weights',
                       'robust',
                       'pander',
                       'dplyr',
                       'mice',
                       'tidyr',
                       'broom',
                       'ggplot2')

for (package in packages_required) {
  if (!(package %in% installed.packages())) {
    install.packages(package)
  }
}

library(foreign)
library(car)
library(SDMTools)
library(texreg)
library(plyr)
library(Zelig)
library(weights)
library(robust)
library(pander)
library(dplyr)
library(mice)
library(tidyr)
library(broom)
library(ggplot2)


# LOAD IN DATA FROM GIT REPOSITORY #

load(url("https://github.com/sdschutt13/presidentfinalpaper/raw/master/Replication%20data/RR-AJPS-taps-processed%20(3).RData"))
d<-tapsData ##Faster to type every time, keeps original data safe







### CODING TO SET UP CORRELATION ###
#####
## Attitude Binary Pot ##
d$pot.att<-d$pot.attitudes
d$pot.att<-ifelse(d$pot.att > 0, 1, 0)



## Attitude Binary Tax ##
d$tax.att<-d$tax.attitudes
d$tax.att<-ifelse(d$tax.att < 0, 1, 0)


## Attitude Binary Defense ##
d$def.att<-d$defense.attitudes
d$def.att<-ifelse(d$def.att > 0, 1, 0)

#####
## PID Binary ##
## Binary PID DEM ##
d$pid.dem<-d$pid7
d$pid.dem<-ifelse(d$pid.dem > 1, 1, 0)

## Binary PID Rep ##
d$pid.rep<-d$pid7
d$pid.rep<-ifelse(d$pid.rep < 1, 1, 0)

## Binary PID Ind ##
d$pid.ind<-d$pid7
d$pid.ind<-ifelse(d$pid.ind == 0, 1, 0)





### CORRELATION ###
#####
cor.test(d$pot.att, d$pid.dem, use="pairwise")
cor.test(d$pot.att, d$pid.rep, use="pairwise")
cor.test(d$pot.att, d$pid.ind, use="pairwise")

cor.test(d$pot.candidate.binary, d$pid.dem)
cor.test(d$pot.candidate.binary, d$pid.rep)
cor.test(d$pot.candidate.binary, d$pid.ind)

