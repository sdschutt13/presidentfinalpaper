## Replication files for The Cost of Unilateral Action
## Andrew Reeves and Jon C. Rogowski
## May 9, 2017
## Creates Table A2


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


########################################################################################
## Table A.2: Policy Preference Measures
########################################################################################
sumStatTable <-
  data.frame(
    Issue = c(
      "Medical marijuana",
      "",
      "Corporate taxes",
      "",
      "Troop deployment",
      ""
    ),
    QuestionWording = c(
      "Marijuana use should be legal in all states.",
      "",
      "Large business corporations pay less than their fair share in taxes.",
      "",
      "Large business corpora- tions pay less than their fair share in taxes.",
      ""
    ),
    Date = c("March 2014", "", "October 2015", "", "November 2015", ""),
    SA = NA,
    A = NA,
    Neither = NA,
    D = NA,
    SD = NA
  )



# Weighted proportions
sumStatTable[1, 4:8] <-
  round(wpct(tapsData$pot.attitudes * -1, tapsData$oct2015wt1) * 100, 1)
sumStatTable[3, 4:8] <-
  round(wpct(tapsData$tax.attitudes, tapsData$oct2015wt1) * 100, 1)
sumStatTable[5, 4:8] <-
  round(wpct(tapsData$defense.attitudes * -1, tapsData$oct2015wt1) * 100,
        1)

# Unweighted N
sumStatTable[2, 4:8] <- table(tapsData$pot.attitudes * -1)
sumStatTable[4, 4:8] <- table(tapsData$tax.attitudes)
sumStatTable[6, 4:8] <- table(tapsData$defense.attitudes * -1)

sumStatTable <- xtable(sumStatTable)
print(sumStatTable, type = "html", file = "tableA2.html", row.names=F)
