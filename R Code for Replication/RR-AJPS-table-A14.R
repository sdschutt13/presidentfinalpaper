## Replication files for The Cost of Unilateral Action
## Andrew Reeves and Jon C. Rogowski
## May 9, 2017
## Creates Table A12 and A13: Ideology and the Effect of Unilateral Action


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


##############################################################
### Table A.14: Education and the Effect of Unilateral Action
##############################################################
pot.candidate.educ.glm <-
  glm(
    pot.candidate.binary ~ treatment * college,
    data = tapsData,
    weights = oct2015wt1,
    family = binomial(link = "logit")
  )
tax.candidate.educ.glm <-
  glm(
    tax.candidate.binary ~ treatment * college,
    data = tapsData,
    weights = oct2015wt1,
    family = binomial(link = "logit")
  )
defense.candidate.educ.glm <-
  glm(
    defense.candidate.binary ~ treatment * college,
    data = tapsData,
    weights = oct2015wt1,
    family = binomial(link = "logit")
  )
pot.handling.educ.glm <-
  glm(
    pot.handling.binary ~ treatment * college,
    data = tapsData,
    weights = oct2015wt1,
    family = binomial(link = "logit")
  )
tax.handling.educ.glm <-
  glm(
    tax.handling.binary ~ treatment * college,
    data = tapsData,
    weights = oct2015wt1,
    family = binomial(link = "logit")
  )
defense.handling.educ.glm <-
  glm(
    defense.handling.binary ~ treatment * college,
    data = tapsData,
    weights = oct2015wt1,
    family = binomial(link = "logit")
  )

edReg <-
  htmlreg(
    l = list(
      pot.candidate.educ.glm,
      tax.candidate.educ.glm,
      defense.candidate.educ.glm,
      pot.handling.educ.glm,
      tax.handling.educ.glm,
      defense.handling.educ.glm
    ),
    custom.coef.names = c(
      "(Intercept)",
      "Legislative Condition",
      "Control Condition",
      "College",
      "Legislative condition $\\times$ College",
      "Control condition $\\times$ College"
    ),
    caption = c(
      "\\textbf{Education and the Effect of Unilateral Action.} Entries are logistic regression coefficients with standard errors in parentheses. The dependent variable is listed at the top of each column. Respondents who received the unilateral treatment condition are the omitted category. Data are weighted to national population parameters."
    ),
    custom.model.names = c("Marijuana", "Taxes", "Troops", "Marijuana", "Taxes", "Troops"),
    include.aic = FALSE,
    include.bic = FALSE,
    include.adjrs = FALSE,
    use.package = FALSE,
    include.deviance = FALSE,
    include.loglik = FALSE,
    dcolumn = TRUE,
    reorder.coef = c(2, 3, 4, 5, 6, 1),
    fontsize = "footnotesize",
    stars = .05,
    custom.columns = list(" " = rep("", 8)),
    custom.col.pos = 5,
    file = "tableA14.html"
  )