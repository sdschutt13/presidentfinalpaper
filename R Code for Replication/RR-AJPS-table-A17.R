## Replication files for The Cost of Unilateral Action
## Andrew Reeves and Jon C. Rogowski
## May 9, 2017
## Creates Table A17: Political Knowledge and the Effect of Unilateral Action


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


######################################################################
## Table A.17: Political Knowledge and the Effect of Unilateral Action
######################################################################
pot.candidate.pk.glm <-
  glm(
    pot.candidate.binary ~ treatment * sum.correct,
    data = tapsData,
    weights = oct2015wt1,
    family = binomial(link = "logit")
  )
tax.candidate.pk.glm <-
  glm(
    tax.candidate.binary ~ treatment * sum.correct,
    data = tapsData,
    weights = oct2015wt1,
    family = binomial(link = "logit")
  )
defense.candidate.pk.glm <-
  glm(
    defense.candidate.binary ~ treatment * sum.correct,
    data = tapsData,
    weights = oct2015wt1,
    family = binomial(link = "logit")
  )
pot.handling.pk.glm <-
  glm(
    pot.handling.binary ~ treatment * sum.correct,
    data = tapsData,
    weights = oct2015wt1,
    family = binomial(link = "logit")
  )
tax.handling.pk.glm <-
  glm(
    tax.handling.binary ~ treatment * sum.correct,
    data = tapsData,
    weights = oct2015wt1,
    family = binomial(link = "logit")
  )
defense.handling.pk.glm <-
  glm(
    defense.handling.binary ~ treatment * sum.correct,
    data = tapsData,
    weights = oct2015wt1,
    family = binomial(link = "logit")
  )

pkReg <-
  htmlreg(
    l = list(
      pot.candidate.pk.glm,
      tax.candidate.pk.glm,
      defense.candidate.pk.glm,
      pot.handling.pk.glm,
      tax.handling.pk.glm,
      defense.handling.pk.glm
    ),
    custom.coef.names = c(
      "(Intercept)",
      "Legislative Condition",
      "Control Condition",
      "Political Knowledge",
      "Legislative condition $\\times$ Political knowledge",
      "Control condition $\\times$ Political Knowledge"
    ),
    caption = c(
      "\\textbf{Political Knowledge and the Effect of Unilateral Action.} Entries are logistic regression coefficients with standard errors in parentheses. The dependent variable is listed at the top of each column. \\emph{Political knowledge} is measured on a 16-point scale, centered at zero. Respondents who received the unilateral treatment condition are the omitted category. Data are weighted to national population parameters."
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
    omit.coef = "(female)|(other)|(black)|(hispanic)|(college)|(age)|(income)",
    stars = .05,
    custom.columns = list(" " = rep("", 8)),
    custom.col.pos = 5,
    file = "tableA17.html"
  )
