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





### CORRELATION for Hypothesis 1 ###
#####
## Chi Sqs on Hypothesis 1a ##
chi.h1a<-chisq.test(d$pot.att, d$pid.dem)
cor.h1a<-cor.test(d$pot.att, d$pid.dem, use="pairwise")

chi.dem.h1a<-chisq.test(d$pot.candidate.binary, d$pid.dem)
cor.dem.h1a<-cor.test(d$pot.candidate.binary, d$pid.dem)


## Chi Sqs on Hypothesis 1b ##
chi.h1b<-chisq.test(d$tax.att, d$pid.dem)
cor.h1b<-cor.test(d$tax.att, d$pid.dem, use="pairwise")

chi.h1b.extra<-chisq.test(d$tax.att, d$pid.rep)
cor.h1b.extra<-cor.test(d$tax.att, d$pid.rep, use="pairwise")

chi.rep.h1b<-chisq.test(d$tax.candidate.binary, d$pid.rep)
cor.rep.h1b<-cor.test(d$tax.candidate.binary, d$pid.rep)

## Chi Sqs on Hypothesis 1c ##
chi.h1c<-chisq.test(d$def.att, d$pid.dem)
cor.h1c<-cor.test(d$def.att, d$pid.dem)

chi.h1c.extra<-chisq.test(d$def.att, d$pid.rep)
cor.h1c.extra<-cor.test(d$def.att, d$pid.rep)

chi.dem.h1c<-chisq.test(d$defense.candidate.binary, d$pid.dem)
cor.dem.h1c<-cor.test(d$defense.candidate.binary, d$pid.dem)


### Logit for Hypothesis 2 ###















