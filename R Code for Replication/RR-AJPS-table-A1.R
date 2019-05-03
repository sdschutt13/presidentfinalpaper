## Replication files for The Cost of Unilateral Action
## Andrew Reeves and Jon C. Rogowski
## May 9, 2017
## Creates Table A1


## Load necessary packages

packages_required <- c('foreign',
                       'car',
                       'SDMTools',
                       'texreg',
                       'plyr',
                       'Zelig',
                       'MASS',
                       'weights',
                       'xtable')
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
library(MASS)
library(weights)
library(xtable)

## Set your working directory
#setwd("~/Dropbox/Articles/legitimacy/outcomes-paper/replication/")

## Load TAPS data
load("RR-AJPS-taps-processed.RData")

attach(tapsData)

##############################################################################
## Table A.1: Public Response to Presidential Policymaking
##############################################################################


sumTable <-
  data.frame(
    Type = c("Control", "SE", "N", "Legislative", "SE", "N", "Unilateral", "SE", "N", "F", "p", "Control", "SE", "N", "Legislative", "SE", "N", "Unilateral", "SE", "N", "F", "p"),
    LegalizeMarijuana = NA,
    LowerTaxes = NA,
    DeployTroops = NA
  )

## Legalize Marijuana Proportion Supporting Candidate
sumTable[c(1,4,7),2] <- summary(lm(
  pot.candidate.binary ~ treatment - 1,
  data = tapsData,
  weights = oct2015wt1
))$coefficients[, 1][c(3,2,1)]
sumTable[c(2,5,8),2] <- summary(lm(
  pot.candidate.binary ~ treatment - 1,
  data = tapsData,
  weights = oct2015wt1
))$coefficients[, 2][c(3,2,1)]
theFStats <-
  summary(lm(
    pot.candidate.binary ~ treatment,
    data = tapsData,
    weights = oct2015wt1
  ))$fstatistic
sumTable[10,2] <- theFStats[1]
## p value
sumTable[11,2] <- 1 - pf(theFStats[1], theFStats[2], theFStats[3])
sumTable[c(3,6,9),2] <- apply(table(tapsData$pot.candidate.binary, tapsData$treatment), 2 ,sum)[c(3,2,1)]


## Legalize Marijuana Proportion Approving of Candidate's Handling of Issue
sumTable[c(12,15,18),2] <- summary(lm(
  pot.handling.binary ~ treatment - 1,
  data = tapsData,
  weights = oct2015wt1
))$coefficients[, 1][c(3,2,1)]
sumTable[c(13,16,19),2] <- summary(lm(
  pot.handling.binary ~ treatment - 1,
  data = tapsData,
  weights = oct2015wt1
))$coefficients[, 2][c(3,2,1)]
theFStats <-
  summary(lm(
    pot.handling.binary ~ treatment,
    data = tapsData,
    weights = oct2015wt1
  ))$fstatistic
## F Statistics
sumTable[21, 2] <- theFStats[1]
## p value
sumTable[22, 2] <- 1 - pf(theFStats[1], theFStats[2], theFStats[3])
## Ns
sumTable[c(14,17,20),2] <- apply(table(tapsData$pot.handling.binary, tapsData$treatment), 2 ,sum)[c(3,2,1)]


## Lower Corporate Taxes Proportion Supporting Candidate
sumTable[c(1,4,7),3] <- summary(lm(
  tax.candidate.binary ~ treatment - 1,
  data = tapsData,
  weights = oct2015wt1
))$coefficients[, 1][c(3,2,1)]
sumTable[c(2,5,8),3] <- summary(lm(
  tax.candidate.binary ~ treatment - 1,
  data = tapsData,
  weights = oct2015wt1
))$coefficients[, 2][c(3,2,1)]
theFStats <-
  summary(lm(
    tax.candidate.binary ~ treatment,
    data = tapsData,
    weights = oct2015wt1
  ))$fstatistic
sumTable[10,3] <- theFStats[1]
## p value
sumTable[11,3] <- 1 - pf(theFStats[1], theFStats[2], theFStats[3])
## Ns
sumTable[c(3,6,9),3] <- apply(table(tapsData$tax.candidate.binary, tapsData$treatment), 2 ,sum)[c(3,2,1)]


## Lower Corporate Taxes Proportion Approving of Candidate's Handling of Issue
sumTable[c(12,15,18),3] <- summary(lm(
  tax.handling.binary ~ treatment - 1,
  data = tapsData,
  weights = oct2015wt1
))$coefficients[, 1][c(3,2,1)]
sumTable[c(13,16,19),3] <- summary(lm(
  tax.handling.binary ~ treatment - 1,
  data = tapsData,
  weights = oct2015wt1
))$coefficients[, 2][c(3,2,1)]
theFStats <-
  summary(lm(
    tax.handling.binary ~ treatment,
    data = tapsData,
    weights = oct2015wt1
  ))$fstatistic
## F Statistics
sumTable[21, 3] <- theFStats[1]
## p value
sumTable[22, 3] <- 1 - pf(theFStats[1], theFStats[2], theFStats[3])
## N
sumTable[c(14,17,20),3] <- apply(table(tapsData$tax.handling.binary, tapsData$treatment), 2 ,sum)[c(3,2,1)]


## Deploy Troops Proportion Supporting Candidate
sumTable[c(1,4,7),4] <- summary(lm(
  defense.candidate.binary ~ treatment - 1,
  data = tapsData,
  weights = oct2015wt1
))$coefficients[, 1][c(3,2,1)]
sumTable[c(2,5,8),4] <- summary(lm(
  defense.candidate.binary ~ treatment - 1,
  data = tapsData,
  weights = oct2015wt1
))$coefficients[, 2][c(3,2,1)]
theFStats <-
  summary(lm(
    defense.candidate.binary ~ treatment,
    data = tapsData,
    weights = oct2015wt1
  ))$fstatistic
sumTable[10,4] <- theFStats[1]
## p value
sumTable[11,4] <- 1 - pf(theFStats[1], theFStats[2], theFStats[3])
## Ns
sumTable[c(3,6,9),4] <- apply(table(tapsData$defense.candidate.binary, tapsData$treatment), 2 ,sum)[c(3,2,1)]


## Deploy Troops Proportion Approving of Candidate's Handling of Issue
sumTable[c(12,15,18),4] <- summary(lm(
  defense.handling.binary ~ treatment - 1,
  data = tapsData,
  weights = oct2015wt1
))$coefficients[, 1][c(3,2,1)]
sumTable[c(13,16,19),4] <- summary(lm(
  defense.handling.binary ~ treatment - 1,
  data = tapsData,
  weights = oct2015wt1
))$coefficients[, 2][c(3,2,1)]
theFStats <-
  summary(lm(
    defense.handling.binary ~ treatment,
    data = tapsData,
    weights = oct2015wt1
  ))$fstatistic
## F Statistics
sumTable[21, 4] <- theFStats[1]
## p value
sumTable[22, 4] <- 1 - pf(theFStats[1], theFStats[2], theFStats[3])
## N
sumTable[c(14,17,20),4] <- apply(table(tapsData$defense.handling.binary, tapsData$treatment), 2 ,sum)[c(3,2,1)]

sumTable <- xtable(sumTable, digits = c(3,3,3,3,3))
print(sumTable, row.names= F, file = "tableA1.html", type = "html")


