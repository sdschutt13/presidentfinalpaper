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
################################################################################
## Table A.12: Ideology and the Effect of Unilateral Action (without covariates)
################################################################################
pot.candidate.ideol.glm <-
  glm(
    pot.candidate.binary ~ treatment2 * ideology7.rev,
    family = binomial(link = "logit"),
    weights = oct2015wt1,
    data = tapsData
  )
tax.candidate.ideol.glm <-
  glm(
    tax.candidate.binary ~ treatment2 * ideology7.rev,
    family = binomial(link = "logit"),
    weights = oct2015wt1,
    data = tapsData
  )
defense.candidate.ideol.glm <-
  glm(
    defense.candidate.binary ~ treatment2 * ideology7.rev,
    family = binomial(link = "logit"),
    weights = oct2015wt1,
    data = tapsData
  )
pot.handling.ideol.glm <-
  glm(
    pot.handling.binary ~ treatment2 * ideology7.rev,
    family = binomial(link = "logit"),
    weights = oct2015wt1,
    data = tapsData
  )
tax.handling.ideol.glm <-
  glm(
    tax.handling.binary ~ treatment2 * ideology7.rev,
    family = binomial(link = "logit"),
    weights = oct2015wt1,
    data = tapsData
  )
defense.handling.ideol.glm <-
  glm(
    defense.handling.binary ~ treatment2 * ideology7.rev,
    family = binomial(link = "logit"),
    weights = oct2015wt1,
    data = tapsData
  )

ideolReg <-
  htmlreg(
    l = list(
      pot.candidate.ideol.glm,
      tax.candidate.ideol.glm,
      defense.candidate.ideol.glm,
      pot.handling.ideol.glm,
      tax.handling.ideol.glm,
      defense.handling.ideol.glm
    ),
    omit.coef = "(female)|(other)|(black)|(hispanic)|(college)|(age)|(income)",
    custom.coef.names = c(
      "Intercept",
      "Unilateral Condition",
      "Ideology",
      "Unilateral Condition $\\times$ Ideology"
    ),
    caption = c(
      "\\textbf{Ideology and the Effect of Unilateral Action (without covariates).} Entries are logistic regression coefficients with standard errors in parentheses. The dependent variable is listed at the top of each column. \\emph{Ideology} is measured on a seven-point scale, centered at zero. The direction of the variable is coded such that larger values indicate respondents more likely to approve of the policy outcome in question. Thus, positive values of this variable index liberals in the marijuana experiment, and conservatives in the taxes and troops experiments. Demographic controls for the models presented in the bottom panel include gender, race, and education, and income. Respondents who received the legislative treatment condition are the omitted category. Data are weighted to national population parameters."
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
    file = "tableA12.html"
  )

########################################################################################
## Table A.13: Ideology and the Effect of Unilateral Action (with covariates)
########################################################################################
pot.candidate.ideol.glm <-
  glm(
    pot.candidate.binary ~ treatment2 * ideology7.rev + female + other + black +
      hispanic + college + age + income6,
    family = binomial(link = "logit"),
    weights = oct2015wt1,
    data = tapsData
  )
tax.candidate.ideol.glm <-
  glm(
    tax.candidate.binary ~ treatment2 * ideology7.rev + female + other + black +
      hispanic + college + age + income6,
    family = binomial(link = "logit"),
    weights = oct2015wt1,
    data = tapsData
  )
defense.candidate.ideol.glm <-
  glm(
    defense.candidate.binary ~ treatment2 * ideology7.rev + female + other +
      black + hispanic + college + age + income6,
    family = binomial(link = "logit"),
    weights = oct2015wt1,
    data = tapsData
  )
pot.handling.ideol.glm <-
  glm(
    pot.handling.binary ~ treatment2 * ideology7.rev + female + other + black +
      hispanic + college + age + income6,
    family = binomial(link = "logit"),
    weights = oct2015wt1,
    data = tapsData
  )
tax.handling.ideol.glm <-
  glm(
    tax.handling.binary ~ treatment2 * ideology7.rev + female + other + black +
      hispanic + college + age + income6,
    family = binomial(link = "logit"),
    weights = oct2015wt1,
    data = tapsData
  )
defense.handling.ideol.glm <-
  glm(
    defense.handling.binary ~ treatment2 * ideology7.rev + female + other +
      black + hispanic + college + age + income6,
    family = binomial(link = "logit"),
    weights = oct2015wt1,
    data = tapsData
  )

ideolRegCovs <-
  htmlreg(
    l = list(
      pot.candidate.ideol.glm,
      tax.candidate.ideol.glm,
      defense.candidate.ideol.glm,
      pot.handling.ideol.glm,
      tax.handling.ideol.glm,
      defense.handling.ideol.glm
    ),
    custom.coef.names = c(
      "Intercept",
      "Unilateral Condition",
      "Ideology",
      "Unilateral Condition $\\times$ Ideology"
    ),
    caption = c(
      "\\textbf{Ideology and the Effect of Unilateral Action (with covariates)}. Entries are logistic regression coefficients with standard errors in parentheses. The dependent variable is listed at the top of each column. \\emph{Ideology} is measured on a seven-point scale, centered at zero. The direction of the variable is coded such that larger values indicate respondents more likely to approve of the policy outcome in question. Thus, positive values of this variable index liberals in the marijuana experiment, and conservatives in the taxes and troops experiments. Demographic controls for the models presented in the bottom panel include gender, race, and education, and income. Respondents who received the legislative treatment condition are the omitted category. Data are weighted to national population parameters."
    ),
    custom.model.names = c("Marijuana", "Taxes", "Troops", "Marijuana", "Taxes", "Troops"),
    include.aic = FALSE,
    include.bic = FALSE,
    include.adjrs = FALSE,
    use.package = FALSE,
    include.deviance = FALSE,
    include.loglik = FALSE,
    dcolumn = TRUE,
    omit.coef = "(female)|(other)|(black)|(hispanic)|(college)|(age)|(income)",
    reorder.coef = c(2, 3, 4, 1),
    stars = .05,
    custom.columns = list(" " = rep("", 8)),
    custom.col.pos = 5,
    file = "tableA13.html"
  )
