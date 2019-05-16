## Replication files for The Cost of Unilateral Action
## Andrew Reeves and Jon C. Rogowski
## May 9, 2017
## Creates Table 2


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
library(robust)
library(pander)
## Set your working directory
#setwd("~/Dropbox/Articles/legitimacy/outcomes-paper/replication/")

## Load TAPS data
load("RR-AJPS-taps-processed.RData")

attach(tapsData)

###################################################
## POT ORIGINAL MODELS AND ALTERNATIVES WITH PID ##
###################################################
pot.candidate.pref.glm <-
  glmRob(
    pot.candidate.binary ~ treatment2 * pot.attitudes,
    data = tapsData,
    weights = oct2015wt1,
    family = binomial(link = "logit")
  )

alt1pot.candidate.pref.glm <-
    glmRob(
      pot.candidate.binary ~ treatment2 + pot.attitudes + pid7,
      data = tapsData,
      weights=oct2015wt1,
      family = binomial(link = "logit"))

alt2pot.candidate.pref.glm <-
  glmRob(
    pot.candidate.binary ~ treatment2 * pot.attitudes + pid7,
    data = tapsData,
    weights=oct2015wt1,
    family = binomial(link = "logit"))

alt3pot.candidate.pref.glm <-
  glmRob(
    pot.candidate.binary ~ treatment2 * pot.attitudes * pid7,
    data = tapsData,
    weights=oct2015wt1,
    family = binomial(link = "logit"))

probPot <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

probPot(coef(alt3pot.candidate.pref.glm))

origin.model.pot<- pander(probPot(coef(pot.candidate.pref.glm)))
alt1.model.pot<-pander(probPot(coef(alt1pot.candidate.pref.glm)))


###################################################
## TAX ORIGINAL MODELS AND ALTERNATIVES WITH PID ##
###################################################

tax.candidate.pref.glm <-
  glm(
    tax.candidate.binary ~ treatment2 * tax.attitudes,
    data = tapsData,
    weights = oct2015wt1,
    family = binomial(link = "logit")
  )
defense.candidate.pref.glm <-
  glm(
    defense.candidate.binary ~ treatment2 * defense.attitudes,
    data = tapsData,
    weights = oct2015wt1,
    family = binomial(link = "logit")
  )
pot.handling.pref.glm <-
  glm(
    pot.handling.binary ~ treatment2 * pot.attitudes,
    data = tapsData,
    weights = oct2015wt1,
    family = binomial(link = "logit")
  )
tax.handling.pref.glm <-
  glm(
    tax.handling.binary ~ treatment2 * tax.attitudes,
    data = tapsData,
    weights = oct2015wt1,
    family = binomial(link = "logit")
  )
defense.handling.pref.glm <-
  glm(
    defense.handling.binary ~ treatment2 * defense.attitudes,
    data = tapsData,
    weights = oct2015wt1,
    family = binomial(link = "logit")
  )

table2 <-
  htmlreg(
    l = list(
      pot.candidate.pref.glm,
      tax.candidate.pref.glm,
      defense.candidate.pref.glm,
      pot.handling.pref.glm,
      tax.handling.pref.glm,
      defense.handling.pref.glm
    ),
    custom.model.names = c(
      "Marijuana",
      "Taxes",
      "Troops",
      "Marijuana",
      "Taxes",
      "Troops"
    ),
    custom.coef.names =  c(
      "Intercept",
      "Unilateral Condition",
      "Marijuana Support",
      "Unilateral condition$\\times$Marijuana Support",
      "Tax Support",
      "Unilateral Condition$\\times$Tax Support",
      "Defense Support",
      "Unilateral Condition$\\times$Defense Support"
    ),
    caption = c(
      "\\textbf{Policy Preferences and the Effect of Unilateral Action.} Entries are logistic regression coefficients with standard errors in parentheses. The dependent variable is listed at the top of each column. Demographic controls for the models presented in the bottom panel include gender, race, and education, and income. Respondents who received the legislative treatment condition are the omitted category. Data are weighted to national population parameters."
    ),
    include.aic = FALSE,
    include.bic = FALSE,
    include.adjrs = FALSE,
    use.package = FALSE,
    include.deviance = FALSE,
    include.loglik = FALSE,
    dcolumn = TRUE,
    reorder.coef = c(2, 3, 4, 5, 6, 7, 8, 1),
    omit.coef = "(female)|(other)|(black)|(hispanic)|(college)|(age)|(income)",
    fontsize = "footnotesize",
    stars = .05,
    custom.columns = list(" " = rep("", 8)),
    custom.col.pos = 5,
    file = "table2.html"
  )