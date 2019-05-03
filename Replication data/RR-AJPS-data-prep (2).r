## Replication files for The Cost of Unilateral Action
## Andrew Reeves and Jon C. Rogowski
## May 9, 2017

## Load necessary packages

packages_required <- c('foreign',
                       'car',
                       'SDMTools',
                       'texreg',
                       'plyr',
                       'Zelig',
                       'MASS',
                       'weights')
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

## Set your working directory
#setwd("~/Dropbox/Articles/legitimacy/outcomes-paper/replication/")

## Load TAPS data
load("RR-AJPS-taps.RData")


## Create a number of recoding functions
recodeAgreement <- function(x) {
  tmp <-
    as.numeric(as.character(
      recode(
        x,
        "'Refused'=NA; 'Strongly agree'=5; 'Agree'=4; 'Neither Agree nor Disagree'=3; 'Disagree'=2;'Strongly Disagree'=1"
      )
    ))
  return(tmp)
}
recodeAgreementBinary <- function(x) {
  tmp <-
    as.numeric(as.character(
      recode(
        x,
        "'Refused'=NA; 'Strongly agree'=1; 'Agree'=1; 'Neither Agree nor Disagree'=NA; 'Disagree'=0;'Strongly Disagree'=0"
      )
    ))
  return(tmp)
}
recodeApprovalBinary <- function(x) {
  tmp <-
    as.numeric(as.character(
      recode(
        x,
        "'Refused'=NA; 'strongly approve'=1; 'approve'=1; 'neither approve nor disapprove'=NA; 'disapprove'=0;'strongly disapprove'=0"
      )
    ))
  return(tmp)
}
recodeApproval <- function(x) {
  tmp <-
    as.numeric(as.character(
      recode(
        x,
        "'Refused'=NA; 'strongly approve'=5; 'approve'=4; 'neither approve nor disapprove'=3; 'disapprove'=2;'strongly disapprove'=1"
      )
    ))
  return(tmp)
}
recodeLikelyBinary <- function(x) {
  tmp <-
    as.numeric(as.character(
      recode(
        x,
        "'Refused'=NA; 'very likely'=1; 'somewhat likely'=1; 'somewhat unlikely'=0; 'very unlikely'=0"
      )
    ))
  return(tmp)
}
recodeLikely <- function(x) {
  tmp <-
    as.numeric(as.character(
      recode(
        x,
        "'Refused'=NA; 'very likely'=4; 'somewhat likely'=3; 'somewhat unlikely'=2; 'very unlikely'=1"
      )
    ))
  return(tmp)
}
recodeWellBinary <-  function(x) {
  tmp <-
    as.numeric(as.character(
      recode(
        x,
        "'Refused'=NA; 'extremely well'=1; 'quite well'=1; 'not too well'=0; 'not well at all'=0"
      )
    ))
  return(tmp)
}
recodeWell <-  function(x) {
  tmp <-
    as.numeric(as.character(
      recode(
        x,
        "'Refused'=NA; 'extremely well'=4; 'quite well'=3; 'not too well'=2; 'not well at all'=1"
      )
    ))
  return(tmp)
}
recodeAgreement2 <- function(x) {
  tmp <-
    as.numeric(as.character(
      recode(
        x,
        "'Refused'=NA; 'strongly agree'=5; 'agree'=4; 'neither agree nor disagree'=3; 'disagree'=2;'strongly disagree'=1"
      )
    ))
  return(tmp)
}
recodeAgreement3 <- function(x) {
  tmp <-
    as.numeric(as.character(
      recode(
        x,
        "'Refused'=NA; 'Strongly agree'=1; 'Agree'=2; 'Neither Agree nor Disagree'=3; 'Disagree'=4;'Strongly Disagree'=5"
      )
    ))
  return(tmp)
}




## Create and Recode variables
## Treatment groups
tapsData$treatment <-
  recode(
    tapsData$xgrouprr,
    "1='control';2='control';3='unilateral';4='unilateral';5='bilateral';6='bilateral'"
  )
tapsData$treatment <-
  relevel(tapsData$treatment, ref = "unilateral")
## Indicator for Unilateral / bilateral only
tapsData$treatment2 <- rep(NA, nrow(tapsData))
tapsData$treatment2[tapsData$treatment == "unilateral"] <- 1
tapsData$treatment2[tapsData$treatment == "bilateral"] <- 0

## Party ID
tapsData$pid3 <- recode(
  tapsData$PARTYID1S46,
  "'Democrat'='dem';
  'Republican'='gop'; 'Independent'='ind';else=NA"
)

### Views of candidate
## Handling of issue (binary)
tapsData$pot.handling.binary <-
  recodeApprovalBinary(tapsData$RERO9S47)
tapsData$tax.handling.binary <-
  recodeApprovalBinary(tapsData$RERO15S47)
tapsData$defense.handling.binary <-
  recodeApprovalBinary(tapsData$RERO21S47)
## Handling of issue (5pt scale)
tapsData$pot.handling <- recodeApproval(tapsData$RERO9S47)
tapsData$tax.handling <- recodeApproval(tapsData$RERO15S47)
tapsData$defense.handling <- recodeApproval(tapsData$RERO21S47)
## Approval of candidate (binary)
tapsData$pot.candidate.binary <-
  recodeLikelyBinary(tapsData$RERO10S47)
tapsData$tax.candidate.binary <-
  recodeLikelyBinary(tapsData$RERO16S47)
tapsData$defense.candidate.binary <-
  recodeLikelyBinary(tapsData$RERO22S47)
## Approval of candidate (5pt scale)
tapsData$pot.candidate <- recodeLikely(tapsData$RERO10S47)
tapsData$tax.candidate <- recodeLikely(tapsData$RERO16S47)
tapsData$defense.candidate <- recodeLikely(tapsData$RERO22S47)

### Candidate traits
## Rule of Law (4 pt scale)
tapsData$pot.rol <- recodeWell(tapsData$RERO13S47)
tapsData$tax.rol <- recodeWell(tapsData$RERO19S47)
tapsData$defense.rol <- recodeWell(tapsData$RERO25S47)
## Rule of Law (binary)
tapsData$pot.rol.binary <- recodeWellBinary(tapsData$RERO13S47)
tapsData$tax.rol.binary <- recodeWellBinary(tapsData$RERO19S47)
tapsData$defense.rol.binary <- recodeWellBinary(tapsData$RERO25S47)
## Leadership (4 pt scale)
tapsData$pot.leadership <- recodeWell(tapsData$RERO12S47)
tapsData$tax.leadership <- recodeWell(tapsData$RERO18S47)
tapsData$defense.leadership <- recodeWell(tapsData$RERO24S47)
## Leadership (binary)
tapsData$pot.leadership.binary <-
  recodeWellBinary(tapsData$RERO12S47)
tapsData$tax.leadership.binary <-
  recodeWellBinary(tapsData$RERO18S47)
tapsData$defense.leadership.binary <-
  recodeWellBinary(tapsData$RERO24S47)
## Able to Get things Done (4 pt scale)
tapsData$pot.gtd <- recodeWell(tapsData$RERO14S47)
tapsData$tax.gtd <- recodeWell(tapsData$RERO20S47)
tapsData$defense.gtd <- recodeWell(tapsData$RERO26S47)
## Able to Get things Done (binary)
tapsData$pot.gtd.binary <- recodeWellBinary(tapsData$RERO14S47)
tapsData$tax.gtd.binary <- recodeWellBinary(tapsData$RERO20S47)
tapsData$defense.gtd.binary <- recodeWellBinary(tapsData$RERO26S47)

### Some demographic covariates
## Female
tapsData$female <- rep(0, nrow(tapsData))
tapsData$female[tapsData$ppgender == 2] <- 1
### Race
## other
tapsData$other <- rep(0, nrow(tapsData))
tapsData$other[tapsData$ppethm == "Other, Non-Hispanic"] <- 1
tapsData$other[tapsData$ppethm == "2+ Race, Non-Hispanic"] <- 1
## Black
tapsData$black <- rep(0, nrow(tapsData))
tapsData$black[tapsData$ppethm == "Black, Non-Hispanic"] <- 1
## Latino
tapsData$hispanic <- rep(0, nrow(tapsData))
tapsData$hispanic[tapsData$ppethm == "Hispanic"] <- 1
## College
tapsData$college <- rep(0, nrow(tapsData))
tapsData$college[tapsData$ppeducat == "Bachelor's degree or higher"] <-
  1
## Age
tapsData$age <- tapsData$ppagect4
## PID
tapsData$pid7 <- rep(NA, nrow(tapsData))
tapsData$pid7[tapsData$PARTYID1S46 == "Republican" &
                tapsData$PARTYID2S46 == "Strong"] <- -3
tapsData$pid7[tapsData$PARTYID1S46 == "Republican" &
                tapsData$PARTYID2S46 == "Not Very Strong"] <- -2
tapsData$pid7[tapsData$PARTYID1S46 == "Independent" &
                tapsData$PARTYID3S46 == "Republican Party"] <- -1
tapsData$pid7[tapsData$PARTYID1S46 == "Independent" &
                tapsData$PARTYID3S46 != "Democratic Party" &
                tapsData$PARTYID3S46 != "Republican Party"] <- 0
tapsData$pid7[tapsData$PARTYID1S46 == "Independent" &
                tapsData$PARTYID3S46 == "Democratic Party"] <- 1
tapsData$pid7[tapsData$PARTYID1S46 == "Democrat" &
                tapsData$PARTYID2S46 == "Strong"] <- 3
tapsData$pid7[tapsData$PARTYID1S46 == "Democrat" &
                tapsData$PARTYID2S46 == "Not Very Strong"] <- 2
## Flip the PID Scale
tapsData$pid7.rev <- tapsData$pid7 * -1

## Ideology
tapsData$ideology7 <- rep(NA, nrow(tapsData))
tapsData$ideology7[tapsData$IDEOL1S46 == "Very conservative"] <- -3
tapsData$ideology7[tapsData$IDEOL1S46 == "Conservative"] <- -2
tapsData$ideology7[tapsData$IDEOL1S46 == "Slightly conservative"] <-
  -1
tapsData$ideology7[tapsData$IDEOL1S46 == "Moderate"] <- 0
tapsData$ideology7[tapsData$IDEOL1S46 == "Slightly liberal"] <- 1
tapsData$ideology7[tapsData$IDEOL1S46 == "Liberal"] <- 2
tapsData$ideology7[tapsData$IDEOL1S46 == "Very liberal"] <- 3
## Flip Ideology Scale
tapsData$ideology7.rev <- tapsData$ideology7 * -1

### Political Knowledge; code correct answers and aggregate score
tapsData$correct1 <- rep(0, nrow(tapsData))
tapsData$correct1[tapsData$POLKNOW1S30 == "Republicans"] <- 1
tapsData$correct1[is.na(tapsData$POLKNOW1S30)] <- NA
tapsData$correct2 <- rep(0, nrow(tapsData))
tapsData$correct2[tapsData$POLKNOW2S30 == "a two-thirds majority of both houses of Congress"] <-
  1
tapsData$correct2[is.na(tapsData$POLKNOW2S30)] <- NA
tapsData$correct3 <- rep(0, nrow(tapsData))
tapsData$correct3[tapsData$POLKNOW3S30 == "six years"] <- 1
tapsData$correct3[is.na(tapsData$POLKNOW3S30)] <- NA
tapsData$correct5 <- rep(0, nrow(tapsData))
tapsData$correct5[tapsData$POLKNOW5S30 == "a filibuster"] <- 1
tapsData$correct5[is.na(tapsData$POLKNOW5S30)] <- NA
tapsData$correct6 <- rep(0, nrow(tapsData))
tapsData$correct6[tapsData$POLKNOW6S30 == "Joseph Biden"] <- 1
tapsData$correct6[is.na(tapsData$POLKNOW6S30)] <- NA
tapsData$correct7 <- rep(0, nrow(tapsData))
tapsData$correct7[tapsData$POLKNOW7S30 == "two terms"] <- 1
tapsData$correct7[is.na(tapsData$POLKNOW7S30)] <- NA
tapsData$correct8 <- rep(0, nrow(tapsData))
tapsData$correct8[tapsData$POLKNOW12S30 == "life terms"] <- 1
tapsData$correct8[is.na(tapsData$POLKNOW12S30)] <- NA
tapsData$correct9 <- rep(0, nrow(tapsData))
tapsData$correct9[tapsData$POLKNOW13S30 == "John Roberts"] <- 1
tapsData$correct9[is.na(tapsData$POLKNOW13S30)] <- NA
tapsData$correct10 <- rep(0, nrow(tapsData))
tapsData$correct10[tapsData$POLKNOW17S30 == "the benefit program for senior citizens"] <-
  1
tapsData$correct10[is.na(tapsData$POLKNOW17S30)] <- NA
tapsData$correct11 <- rep(0, nrow(tapsData))
tapsData$correct11[tapsData$POLKNOW18S30 == "Medicare"] <- 1
tapsData$correct11[is.na(tapsData$POLKNOW18S30)] <- NA
tapsData$correct12 <- rep(0, nrow(tapsData))
tapsData$correct12[tapsData$POLKNOW21S30 == "a federation"] <- 1
tapsData$correct12[is.na(tapsData$POLKNOW21S30)] <- NA
tapsData$correct13 <- rep(0, nrow(tapsData))
tapsData$correct13[tapsData$POLKNOW22S30 == "conservatives"] <- 1
tapsData$correct13[is.na(tapsData$POLKNOW22S30)] <- NA
tapsData$correct14 <- rep(0, nrow(tapsData))
tapsData$correct14[tapsData$POLKNOW23S30 == "President"] <- 1
tapsData$correct14[is.na(tapsData$POLKNOW23S30)] <- NA
tapsData$correct15 <- rep(0, nrow(tapsData))
tapsData$correct15[tapsData$POLKNOW24S30 == "Democrat"] <- 1
tapsData$correct15[is.na(tapsData$POLKNOW24S30)] <- NA
tapsData$correct16 <- rep(0, nrow(tapsData))
tapsData$correct16[tapsData$POLKNOW25S30 == "was originally called the Bureau of the Budget"] <-
  1
tapsData$correct16[is.na(tapsData$POLKNOW25S30)] <- NA
tapsData$sum.correct <-
  apply(tapsData[, c(
    "correct1",
    "correct2",
    "correct3",
    "correct5",
    "correct6",
    "correct7",
    "correct8",
    "correct9",
    "correct10",
    "correct11",
    "correct12",
    "correct13",
    "correct14",
    "correct15",
    "correct16"
  )], 1, sum)
tapsData$sum.correct <-
  tapsData$sum.correct - mean(tapsData$sum.correct, na.rm = T)
### Preferences toward issues
## Pot
tapsData$pot.attitudes <- recodeAgreement2(tapsData$ATTS7S27)
tapsData$pot.attitudes <- tapsData$pot.attitudes - 3
## Defense
tapsData$defense.attitudes <-
  as.numeric(as.character(
    recode(
      tapsData$IDEFS48,
      "'Refused'=NA; 'Strongly Agree'=5; 'Agree'=4; 'Neither Agree nor Disagree'=3; 'Disagree'=2;'Strongly Disagree'=1"
    )
  ))
tapsData$defense.attitudes <- tapsData$defense.attitudes - 3
## Taxes
tapsData$tax.attitudes <- recodeAgreement3(tapsData$RERO7S47)
tapsData$tax.attitudes <- tapsData$tax.attitudes - 3
## Dump empty factor
tapsData$age <- factor(tapsData$age)

save(tapsData, file = "RR-AJPS-taps-processed.RData")
