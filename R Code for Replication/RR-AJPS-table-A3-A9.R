## Replication files for The Cost of Unilateral Action
## Andrew Reeves and Jon C. Rogowski
## May 9, 2017
## Creates Table A3 through A9


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
## Table A.3: Policy Preferences and the Effect of Unilateral Action (with Covariates)
########################################################################################

pot.candidate.pref.glm <-
  glm(
    pot.candidate.binary ~ treatment2 * pot.attitudes + female + other + black +
      hispanic + college + age + income6,
    data = tapsData,
    weights = oct2015wt1,
    family = binomial(link = "logit")
  )
tax.candidate.pref.glm <-
  glm(
    tax.candidate.binary ~ treatment2 * tax.attitudes + female + other + black +
      hispanic + college + age + income6,
    data = tapsData,
    weights = oct2015wt1,
    family = binomial(link = "logit")
  )
defense.candidate.pref.glm <-
  glm(
    defense.candidate.binary ~ treatment2 * defense.attitudes + female + other +
      black + hispanic + college + age + income6,
    data = tapsData,
    weights = oct2015wt1,
    family = binomial(link = "logit")
  )
pot.handling.pref.glm <-
  glm(
    pot.handling.binary ~ treatment2 * pot.attitudes + female + other + black +
      hispanic + college + age + income6,
    data = tapsData,
    weights = oct2015wt1,
    family = binomial(link = "logit")
  )
tax.handling.pref.glm <-
  glm(
    tax.handling.binary ~ treatment2 * tax.attitudes + female + other + black +
      hispanic + college + age + income6,
    data = tapsData,
    weights = oct2015wt1,
    family = binomial(link = "logit")
  )
defense.handling.pref.glm <-
  glm(
    defense.handling.binary ~ treatment2 * defense.attitudes + female + other +
      black + hispanic + college + age + income6,
    data = tapsData,
    weights = oct2015wt1,
    family = binomial(link = "logit")
  )

prefReg <-
  htmlreg(
    l = list(
      pot.candidate.pref.glm,
      tax.candidate.pref.glm,
      defense.candidate.pref.glm,
      pot.handling.pref.glm,
      tax.handling.pref.glm,
      defense.handling.pref.glm
    ),
    custom.coef.names = c(
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
    custom.model.names = c("Marijuana", "Taxes", "Troops", "Marijuana", "Taxes", "Troops"),
    include.aic = FALSE,
    include.bic = FALSE,
    include.adjrs = FALSE,
    use.package = FALSE,
    include.deviance = FALSE,
    include.loglik = FALSE,
    dcolumn = TRUE,
    reorder.coef = c(2, 3, 4, 5, 6, 7, 8, 1),
    fontsize = "footnotesize",
    omit.coef = "(female)|(other)|(black)|(hispanic)|(college)|(age)|(income)",
    stars = .05,
    custom.columns = list(" " = rep("", 8)),
    custom.col.pos = 5,
    file = "tableA3.html"
  )

###################################################################################################
## Table A.4: Policy Preferences and the Effect of Unilateral Action (OLS estimates, No covariates)
###################################################################################################
pot.candidate.pref.lm <-
  lm(pot.candidate ~ treatment2 * pot.attitudes,
     data = tapsData,
     weights = oct2015wt1)
tax.candidate.pref.lm <-
  lm(tax.candidate ~ treatment2 * tax.attitudes,
     data = tapsData,
     weights = oct2015wt1)
defense.candidate.pref.lm <-
  lm(
    defense.candidate ~ treatment2 * defense.attitudes,
    data = tapsData,
    weights = oct2015wt1
  )
pot.handling.pref.lm <-
  lm(pot.handling ~ treatment2 * pot.attitudes,
     data = tapsData,
     weights = oct2015wt1)
tax.handling.pref.lm <-
  lm(tax.handling ~ treatment2 * tax.attitudes,
     data = tapsData,
     weights = oct2015wt1)
defense.handling.pref.lm <-
  lm(
    defense.handling ~ treatment2 * defense.attitudes,
    data = tapsData,
    weights = oct2015wt1
  )

prefRegLS <-
  htmlreg(
    l = list(
      pot.candidate.pref.lm,
      tax.candidate.pref.lm,
      defense.candidate.pref.lm,
      pot.handling.pref.lm,
      tax.handling.pref.lm,
      defense.handling.pref.lm
    ),
    model.names = c("Marijuana", "Taxes", "Troops", "Marijuana", "Taxes", "Troops"),
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
      "\\textbf{Policy Preferences and the Effect of Unilateral Action (OLS estimates).} Entries are linear regression coefficients with standard errors in parentheses. The dependent variable is listed at the top of each column. Demographic controls for the models presented in the bottom panel include gender, race, and education, and income. Respondents who received the legislative treatment condition are the omitted category. Data are weighted to national population parameters."
    ),
    custom.model.names = c("Marijuana", "Taxes", "Troops", "Marijuana", "Taxes", "Troops"),
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
    file = "tableA4.html"
  )

###################################################################################################
## Table A.5: Policy Preferences and the Effect of Unilateral Action (OLS estimates, covariates)
###################################################################################################
pot.candidate.pref.lm <-
  lm(
    pot.candidate ~ treatment2 * pot.attitudes + female + other + black + hispanic +
      college + age + income6,
    data = tapsData,
    weights = oct2015wt1
  )
tax.candidate.pref.lm <-
  lm(
    tax.candidate ~ treatment2 * tax.attitudes + female + other + black + hispanic +
      college + age + income6,
    data = tapsData,
    weights = oct2015wt1
  )
defense.candidate.pref.lm <-
  lm(
    defense.candidate ~ treatment2 * defense.attitudes + female + other + black +
      hispanic + college + age + income6,
    data = tapsData,
    weights = oct2015wt1
  )
pot.handling.pref.lm <-
  lm(
    pot.handling ~ treatment2 * pot.attitudes + female + other + black + hispanic +
      college + age + income6,
    data = tapsData,
    weights = oct2015wt1
  )
tax.handling.pref.lm <-
  lm(
    tax.handling ~ treatment2 * tax.attitudes + female + other + black + hispanic +
      college + age + income6,
    data = tapsData,
    weights = oct2015wt1
  )
defense.handling.pref.lm <-
  lm(
    defense.handling ~ treatment2 * defense.attitudes + female + other + black +
      hispanic + college + age + income6,
    data = tapsData,
    weights = oct2015wt1
  )

prefRegLSCovs <-
  htmlreg(
    l = list(
      pot.candidate.pref.lm,
      tax.candidate.pref.lm,
      defense.candidate.pref.lm,
      pot.handling.pref.lm,
      tax.handling.pref.lm,
      defense.handling.pref.lm
    ),
    model.names = c("Marijuana", "Taxes", "Troops", "Marijuana", "Taxes", "Troops"),
    custom.coef.names = c(
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
      "\\textbf{Policy Preferences and the Effect of Unilateral Action (OLS estimates, with covariates).} Entries are linear regression coefficients with standard errors in parentheses. The dependent variable is listed at the top of each column. Demographic controls for the models presented in the bottom panel include gender, race, and education, and income. Respondents who received the legislative treatment condition are the omitted category. Data are weighted to national population parameters."
    ),
    custom.model.names = c("Marijuana", "Taxes", "Troops", "Marijuana", "Taxes", "Troops"),
    include.aic = FALSE,
    include.bic = FALSE,
    include.adjrs = FALSE,
    use.package = FALSE,
    include.deviance = FALSE,
    include.loglik = FALSE,
    dcolumn = TRUE,
    reorder.coef = c(2, 3, 4, 5, 6, 7, 8, 1),
    fontsize = "footnotesize",
    omit.coef = "(female)|(other)|(black)|(hispanic)|(college)|(age)|(income)",
    stars = .05,
    custom.columns = list(" " = rep("", 8)),
    custom.col.pos = 5,
    file = "tableA5.html"
  )



##############################################################################################################
## Table A.6: Policy Preferences and the Effect of Unilateral Action (Ordered logit estimates, no covariates)
##############################################################################################################

pot.candidate.pref.ol <-
  polr(
    as.factor(pot.candidate) ~ treatment2 * pot.attitudes,
    data = tapsData,
    weights = oct2015wt1
  )
tax.candidate.pref.ol <-
  polr(
    as.factor(tax.candidate) ~ treatment2 * tax.attitudes,
    data = tapsData,
    weights = oct2015wt1
  )
defense.candidate.pref.ol <-
  polr(
    as.factor(defense.candidate) ~ treatment2 * defense.attitudes,
    data = tapsData,
    weights = oct2015wt1
  )
pot.handling.pref.ol <-
  polr(
    as.factor(pot.handling) ~ treatment2 * pot.attitudes,
    data = tapsData,
    weights = oct2015wt1
  )
tax.handling.pref.ol <-
  polr(
    as.factor(tax.handling) ~ treatment2 * tax.attitudes,
    data = tapsData,
    weights = oct2015wt1
  )
defense.handling.pref.ol <-
  polr(
    as.factor(defense.handling) ~ treatment2 * defense.attitudes,
    data = tapsData,
    weights = oct2015wt1
  )

prefRegOL <-
  htmlreg(
    l = list(
      pot.candidate.pref.ol,
      tax.candidate.pref.ol,
      defense.candidate.pref.ol,
      pot.handling.pref.ol,
      tax.handling.pref.ol,
      defense.handling.pref.ol
    ),
    model.names = c("Marijuana", "Taxes", "Troops", "Marijuana", "Taxes", "Troops"),
    custom.coef.names = c(
      "Unilateral Condition",
      "Marijuana Support",
      "Unilateral condition$\\times$Marijuana Support",
      "Tax Support",
      "Unilateral Condition$\\times$Tax Support",
      "Defense Support",
      "Unilateral Condition$\\times$Defense Support"
    ),
    caption = c(
      "\\textbf{Policy Preferences and the Effect of Unilateral Action (Ordered logit estimates):} Entries are ordered logistic regression coefficients with standard errors in parentheses. The dependent variable is listed at the top of each column. Demographic controls for the models presented in the bottom panel include gender, race, and education, and income. Respondents who received the legislative treatment condition are the omitted category. Data are weighted to national population parameters."
    ),
    custom.model.names = c("Marijuana", "Taxes", "Troops", "Marijuana", "Taxes", "Troops"),
    include.aic = FALSE,
    include.bic = FALSE,
    include.adjrs = FALSE,
    use.package = FALSE,
    include.deviance = FALSE,
    include.loglik = FALSE,
    dcolumn = TRUE,
    #reorder.coef=c(2,3,4,5,6,7,8,1),
    fontsize = "footnotesize",
    omit.coef = "(female)|(other)|(black)|(hispanic)|(college)|(age)|(income)",
    stars = .05,
    custom.columns = list(" " = rep("", 8)),
    custom.col.pos = 5,
    file = "tableA6.html"
  )

##############################################################################################################
## Table A.7: Policy Preferences and the Effect of Unilateral Action (Ordered logit estimates, covariates)
##############################################################################################################
pot.candidate.pref.ol <-
  polr(
    as.factor(pot.candidate) ~ treatment2 * pot.attitudes + female + other +
      black + hispanic + college + age + income6,
    data = tapsData,
    weights = oct2015wt1
  )
tax.candidate.pref.ol <-
  polr(
    as.factor(tax.candidate) ~ treatment2 * tax.attitudes + female + other +
      black + hispanic + college + age + income6,
    data = tapsData,
    weights = oct2015wt1
  )
defense.candidate.pref.ol <-
  polr(
    as.factor(defense.candidate) ~ treatment2 * defense.attitudes + female +
      other + black + hispanic + college + age + income6,
    data = tapsData,
    weights = oct2015wt1
  )
pot.handling.pref.ol <-
  polr(
    as.factor(pot.handling) ~ treatment2 * pot.attitudes + female + other +
      black + hispanic + college + age + income6,
    data = tapsData,
    weights = oct2015wt1
  )
tax.handling.pref.ol <-
  polr(
    as.factor(tax.handling) ~ treatment2 * tax.attitudes + female + other +
      black + hispanic + college + age + income6,
    data = tapsData,
    weights = oct2015wt1
  )
defense.handling.pref.ol <-
  polr(
    as.factor(defense.handling) ~ treatment2 * defense.attitudes + female +
      other + black + hispanic + college + age + income6,
    data = tapsData,
    weights = oct2015wt1
  )

prefRegOLCovs <-
  htmlreg(
    l = list(
      pot.candidate.pref.ol,
      tax.candidate.pref.ol,
      defense.candidate.pref.ol,
      pot.handling.pref.ol,
      tax.handling.pref.ol,
      defense.handling.pref.ol
    ),
    custom.coef.names = c(
      "Unilateral Condition",
      "Marijuana Support",
      "Unilateral condition$\\times$Marijuana Support",
      "Tax Support",
      "Unilateral Condition$\\times$Tax Support",
      "Defense Support",
      "Unilateral Condition$\\times$Defense Support"
    ),
    caption = c(
      "\\textbf{Policy Preferences and the Effect of Unilateral Action (Ordered logit estimates, with covariates):} Entries are ordered logistic regression coefficients with standard errors in parentheses. The dependent variable is listed at the top of each column. Demographic controls for the models presented in the bottom panel include gender, race, and education, and income. Respondents who received the legislative treatment condition are the omitted category. Data are weighted to national population parameters."
    ),
    custom.model.names = c("Marijuana", "Taxes", "Troops", "Marijuana", "Taxes", "Troops"),
    include.aic = FALSE,
    include.bic = FALSE,
    include.adjrs = FALSE,
    use.package = FALSE,
    include.deviance = FALSE,
    include.loglik = FALSE,
    dcolumn = TRUE,
    #reorder.coef=c(2,3,4,5,6,7,8,1),
    fontsize = "footnotesize",
    omit.coef = "(female)|(other)|(black)|(hispanic)|(college)|(age)|(income)",
    stars = .05,
    custom.columns = list(" " = rep("", 8)),
    custom.col.pos = 5,
    file = "tableA7.html"
  )




#####################################################################################################
## Table A.8: Policy Preferences and the Effect of Unilateral Action (all conditions, no covariates)
#####################################################################################################
pot.candidate.pref.glm <-
  glm(
    pot.candidate.binary ~ treatment * pot.attitudes,
    data = tapsData,
    weights = oct2015wt1,
    family = binomial(link = "logit")
  )
tax.candidate.pref.glm <-
  glm(
    tax.candidate.binary ~ treatment * tax.attitudes,
    data = tapsData,
    weights = oct2015wt1,
    family = binomial(link = "logit")
  )
defense.candidate.pref.glm <-
  glm(
    defense.candidate.binary ~ treatment * defense.attitudes,
    data = tapsData,
    weights = oct2015wt1,
    family = binomial(link = "logit")
  )
pot.handling.pref.glm <-
  glm(
    pot.handling.binary ~ treatment * pot.attitudes,
    data = tapsData,
    weights = oct2015wt1,
    family = binomial(link = "logit")
  )
tax.handling.pref.glm <-
  glm(
    tax.handling.binary ~ treatment * tax.attitudes,
    data = tapsData,
    weights = oct2015wt1,
    family = binomial(link = "logit")
  )
defense.handling.pref.glm <-
  glm(
    defense.handling.binary ~ treatment * defense.attitudes,
    data = tapsData,
    weights = oct2015wt1,
    family = binomial(link = "logit")
  )

prefRegAll <-
  htmlreg(
    l = list(
      pot.candidate.pref.glm,
      tax.candidate.pref.glm,
      defense.candidate.pref.glm,
      pot.handling.pref.glm,
      tax.handling.pref.glm,
      defense.handling.pref.glm
    ),
    custom.coef.names = c(
      "Intercept",
      "Legislative Condition",
      "Control",
      "Marijuana Support",
      "Legislative condition$\\times$Marijuana Support",
      "Control Condition$\\times$Marijuana Support",
      "Tax Support",
      "Legislative Condition$\\times$Tax Support",
      "Control Condition$\\times$Tax Support",
      "Defense Support",
      "Legislative Condition$\\times$Defense Support",
      "Control Condition$\\times$Defense Support"
    ),
    caption = c(
      "\\textbf{Policy Preferences and the Effect of Unilateral Action (All Conditions).} Entries are logistic regression coefficients with standard errors in parentheses. The dependent variable is listed at the top of each column. Respondents who received the unilateral treatment condition are the omitted category. Data are weighted to national population parameters."
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
    omit.coef = "(female)|(other)|(black)|(hispanic)|(college)|(age)|(income)",
    stars = .05,
    custom.columns = list(" " = rep("", 12)),
    custom.col.pos = 5,
    file = "tableA8.html"
  )

#####################################################################################################
## Table A.9: Policy Preferences and the Effect of Unilateral Action (all conditions, covariates)
#####################################################################################################
pot.candidate.pref.glm <-
  glm(
    pot.candidate.binary ~ treatment * pot.attitudes + female + other + black +
      hispanic + college + age + income6,
    data = tapsData,
    weights = oct2015wt1,
    family = binomial(link = "logit")
  )
tax.candidate.pref.glm <-
  glm(
    tax.candidate.binary ~ treatment * tax.attitudes + female + other + black +
      hispanic + college + age + income6,
    data = tapsData,
    weights = oct2015wt1,
    family = binomial(link = "logit")
  )
defense.candidate.pref.glm <-
  glm(
    defense.candidate.binary ~ treatment * defense.attitudes + female + other +
      black + hispanic + college + age + income6,
    data = tapsData,
    weights = oct2015wt1,
    family = binomial(link = "logit")
  )
pot.handling.pref.glm <-
  glm(
    pot.handling.binary ~ treatment * pot.attitudes + female + other + black +
      hispanic + college + age + income6,
    data = tapsData,
    weights = oct2015wt1,
    family = binomial(link = "logit")
  )
tax.handling.pref.glm <-
  glm(
    tax.handling.binary ~ treatment * tax.attitudes + female + other + black +
      hispanic + college + age + income6,
    data = tapsData,
    weights = oct2015wt1,
    family = binomial(link = "logit")
  )
defense.handling.pref.glm <-
  glm(
    defense.handling.binary ~ treatment * defense.attitudes + female + other +
      black + hispanic + college + age + income6,
    data = tapsData,
    weights = oct2015wt1,
    family = binomial(link = "logit")
  )

prefRegAllCovs <-
  htmlreg(
    l = list(
      pot.candidate.pref.glm,
      tax.candidate.pref.glm,
      defense.candidate.pref.glm,
      pot.handling.pref.glm,
      tax.handling.pref.glm,
      defense.handling.pref.glm
    ),
    custom.coef.names = c(
      "Intercept",
      "Legislative Condition",
      "Control",
      "Marijuana Support",
      "Legislative condition$\\times$Marijuana Support",
      "Control Condition$\\times$Marijuana Support",
      "Tax Support",
      "Legislative Condition$\\times$Tax Support",
      "Control Condition$\\times$Tax Support",
      "Defense Support",
      "Legislative Condition$\\times$Defense Support",
      "Control Condition$\\times$Defense Support"
    ),
    caption = c(
      "\\textbf{Policy Preferences and the Effect of Unilateral Action (All Conditions, with covariates).} Entries are logistic regression coefficients with standard errors in parentheses. The dependent variable is listed at the top of each column. Respondents who received the unilateral treatment condition are the omitted category. Data are weighted to national population parameters."
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
    omit.coef = "(female)|(other)|(black)|(hispanic)|(college)|(age)|(income)",
    stars = .05,
    custom.columns = list(" " = rep("", 12)),
    custom.col.pos = 5,
    file = "tableA9.html"
  )
