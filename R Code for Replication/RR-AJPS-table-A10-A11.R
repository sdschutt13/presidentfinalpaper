## Replication files for The Cost of Unilateral Action
## Andrew Reeves and Jon C. Rogowski
## May 9, 2017
## Creates Table A3


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


########################################################################################
## Table A.10: Partisanship and the Effect of Unilateral Action (without Covariates)
########################################################################################

pot.candidate.pid.glm <-
  glm(
    pot.candidate.binary ~ treatment2 * pid7.rev,
    family = binomial(link = "logit"),
    weights = oct2015wt1,
    data = tapsData
  )
tax.candidate.pid.glm <-
  glm(
    tax.candidate.binary ~ treatment2 * pid7.rev,
    family = binomial(link = "logit"),
    weights = oct2015wt1,
    data = tapsData
  )
defense.candidate.pid.glm <-
  glm(
    defense.candidate.binary ~ treatment2 * pid7.rev,
    family = binomial(link = "logit"),
    weights = oct2015wt1,
    data = tapsData
  )
pot.handling.pid.glm <-
  glm(
    pot.handling.binary ~ treatment2 * pid7.rev,
    family = binomial(link = "logit"),
    weights = oct2015wt1,
    data = tapsData
  )
tax.handling.pid.glm <-
  glm(
    tax.handling.binary ~ treatment2 * pid7.rev,
    family = binomial(link = "logit"),
    weights = oct2015wt1,
    data = tapsData
  )
defense.handling.pid.glm <-
  glm(
    defense.handling.binary ~ treatment2 * pid7.rev,
    family = binomial(link = "logit"),
    weights = oct2015wt1,
    data = tapsData
  )

partyReg <-
  htmlreg(
    l = list(
      pot.candidate.pid.glm,
      tax.candidate.pid.glm,
      defense.candidate.pid.glm,
      pot.handling.pid.glm,
      tax.handling.pid.glm,
      defense.handling.pid.glm
    ),
    custom.coef.names = c(
      "Intercept",
      "Unilateral Condition",
      "Partisanship",
      "Unilateral Condition $\\times$ Partisanship"
    ),
    caption = c(
      "\\textbf{Partisanship and the Effect of Unilateral Action.} Entries are logistic regression coefficients with standard errors in parentheses. The dependent variable is listed at the top of each column. \\emph{Partisanship} is measured on a seven-point scale, centered at zero. The direction of the variable is coded such that larger values indicate respondents more likely to approve of the policy outcome in question. Thus, positive values of this variable index Democrats in the marijuana experiment, and Republicans in the taxes and troops experiments. Demographic controls for the models presented in the bottom panel include gender, race, and education, and income. Respondents who received the legislative treatment condition are the omitted category. Data are weighted to national population parameters."
    ),
    custom.model.names = c("Marijuana", "Taxes", "Troops", "Marijuana", "Taxes", "Troops"),
    include.aic = FALSE,
    include.bic = FALSE,
    include.adjrs = FALSE,
    use.package = FALSE,
    include.deviance = FALSE,
    include.loglik = FALSE,
    dcolumn = TRUE,
    reorder.coef = c(2, 3, 4, 1),
    stars = .05,
    custom.columns = list(" " = rep("", 8)),
    custom.col.pos = 5,
    file = "tableA10.html"
  )


########################################################################################
## Table A.11: Partisanship and the Effect of Unilateral Action (with Covariates)
########################################################################################
pot.candidate.pid.glm <-
  glm(
    pot.candidate.binary ~ treatment2 * pid7.rev + female + other + black +
      hispanic + college + age + income6,
    family = binomial(link = "logit"),
    weights = oct2015wt1,
    data = tapsData
  )
tax.candidate.pid.glm <-
  glm(
    tax.candidate.binary ~ treatment2 * pid7.rev + female + other + black +
      hispanic + college + age + income6,
    family = binomial(link = "logit"),
    weights = oct2015wt1,
    data = tapsData
  )
defense.candidate.pid.glm <-
  glm(
    defense.candidate.binary ~ treatment2 * pid7.rev + female + other + black +
      hispanic + college + age + income6,
    family = binomial(link = "logit"),
    weights = oct2015wt1,
    data = tapsData
  )
pot.handling.pid.glm <-
  glm(
    pot.handling.binary ~ treatment2 * pid7.rev + female + other + black + hispanic +
      college + age + income6,
    family = binomial(link = "logit"),
    weights = oct2015wt1,
    data = tapsData
  )
tax.handling.pid.glm <-
  glm(
    tax.handling.binary ~ treatment2 * pid7.rev + female + other + black + hispanic +
      college + age + income6,
    family = binomial(link = "logit"),
    weights = oct2015wt1,
    data = tapsData
  )
defense.handling.pid.glm <-
  glm(
    defense.handling.binary ~ treatment2 * pid7.rev + female + other + black +
      hispanic + college + age + income6,
    family = binomial(link = "logit"),
    weights = oct2015wt1,
    data = tapsData
  )

partyRegCov <-
  htmlreg(
    l = list(
      pot.candidate.pid.glm,
      tax.candidate.pid.glm,
      defense.candidate.pid.glm,
      pot.handling.pid.glm,
      tax.handling.pid.glm,
      defense.handling.pid.glm
    ),
    caption = c(
      "\\textbf{Partisanship and the Effect of Unilateral Action (with covariates).} Entries are logistic regression coefficients with standard errors in parentheses. The dependent variable is listed at the top of each column. \\emph{Partisanship} is measured on a seven-point scale, centered at zero. The direction of the variable is coded such that larger values indicate respondents more likely to approve of the policy outcome in question. Thus, positive values of this variable index Democrats in the marijuana experiment, and Republicans in the taxes and troops experiments. Demographic controls for the models presented in the bottom panel include gender, race, and education, and income. Respondents who received the legislative treatment condition are the omitted category. Data are weighted to national population parameters."
    ),
    custom.model.names = c("Marijuana", "Taxes", "Troops", "Marijuana", "Taxes", "Troops"),
    include.aic = FALSE,
    include.bic = FALSE,
    include.adjrs = FALSE,
    use.package = FALSE,
    include.deviance = FALSE,
    include.loglik = FALSE,
    dcolumn = TRUE,
    reorder.coef = c(2, 3, 4, 1),
    omit.coef = "(female)|(other)|(black)|(hispanic)|(college)|(age)|(income)",
    custom.coef.names = c(
      "Intercept",
      "Unilateral Condition",
      "Partisanship",
      "Unilateral Condition $\\times$ Partisanship"
    ),
    stars = .05,
    custom.columns = list(" " = rep("", 8)),
    custom.col.pos = 5,
    file = "tableA11.html"
  )