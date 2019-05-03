## Replication files for The Cost of Unilateral Action
## Andrew Reeves and Jon C. Rogowski
## May 9, 2017
## Creates Table A15: Age and the Effect of Unilateral Action


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
load("RR-AJPS-taps-processed.RData")

attach(tapsData)


######################################################
## Table A.15: Age and the Effect of Unilateral Action
######################################################

pot.candidate.age.glm <-
  glm(
    pot.candidate.binary ~ treatment * age,
    data = tapsData,
    weights = oct2015wt1,
    family = binomial(link = "logit")
  )
tax.candidate.age.glm <-
  glm(
    tax.candidate.binary ~ treatment * age,
    data = tapsData,
    weights = oct2015wt1,
    family = binomial(link = "logit")
  )
defense.candidate.age.glm <-
  glm(
    defense.candidate.binary ~ treatment * age,
    data = tapsData,
    weights = oct2015wt1,
    family = binomial(link = "logit")
  )
pot.handling.age.glm <-
  glm(
    pot.handling.binary ~ treatment * age,
    data = tapsData,
    weights = oct2015wt1,
    family = binomial(link = "logit")
  )
tax.handling.age.glm <-
  glm(
    tax.handling.binary ~ treatment * age,
    data = tapsData,
    weights = oct2015wt1,
    family = binomial(link = "logit")
  )
defense.handling.age.glm <-
  glm(
    defense.handling.binary ~ treatment * age,
    data = tapsData,
    weights = oct2015wt1,
    family = binomial(link = "logit")
  )
ageReg <-
  htmlreg(
    l = list(
      pot.candidate.age.glm,
      tax.candidate.age.glm,
      defense.candidate.age.glm,
      pot.handling.age.glm,
      tax.handling.age.glm,
      defense.handling.age.glm
    ),
    custom.coef.names = c(
      "(Intercept)",
      "Legislative Condition",
      "Control Condition",
      "Age: 30-44",
      "Age: 45-59",
      "Age: 60 and older",
      "Legislative condition $\\times$ Age: 30-44",
      "Legislative condition $\\times$ Age: 45-59",
      "Legislative condition $\\times$ Age: 60 and older",
      "Control condition $\\times$ Age: 30-44",
      "Control condition $\\times$ Age: 45-59",
      "Control condition $\\times$ Age: 60 and older"
    ),
    caption = c(
      "\\textbf{Age and the Effect of Unilateral Action.} Entries are logistic regression coefficients with standard errors in parentheses. The dependent variable is listed at the top of each column. Respondents who received the unilateral treatment condition are the omitted category. Data are weighted to national population parameters."
    ),
    custom.model.names = c("Marijuana", "Taxes", "Troops", "Marijuana", "Taxes", "Troops"),
    include.aic = FALSE,
    include.bic = FALSE,
    include.adjrs = FALSE,
    use.package = FALSE,
    include.deviance = FALSE,
    include.loglik = FALSE,
    dcolumn = TRUE,
    reorder.coef = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 1),
    fontsize = "footnotesize",
    stars = .05,
    custom.columns = list(" " = rep("", 12)),
    custom.col.pos = 5,
    file = "tableA15.html"
  )
