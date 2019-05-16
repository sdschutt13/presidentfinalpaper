## Replication File for Critique Reeves and Rogowski 2019
## Sam Schutt Critique Reeves Rogowski
## May 15th, 2019
## Creates Table 2


## Load necessary packages

packages_required <- c('foreign',
                       'car',
                       'SDMTools',
                       'texreg',
                       'plyr',
                       'Zelig',
                       'weights',
                       'robust',
                       'Mass',
                       'pander',
                       'dplyr',
                       'mice',
                       'tidyr',
                       'broom')

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
library(weights)
library(robust)
library(pander)
library(dplyr)
library(mice)
library(tidyr)
library(broom)


# LOAD IN DATA FROM GIT REPOSITORY #

load(url("https://github.com/sdschutt13/presidentfinalpaper/raw/master/Replication%20data/RR-AJPS-taps-processed%20(3).RData"))
d<-tapsData ##Faster to type every time, keeps original data safe
d1<-d ##extra copy just in case

########################
## POT ORIGINAL MODELS##
########################

# ORIGINAL CANDIDATE MODEL WITH NO ROBUST #
pot.candidate.pref.glm <-
  glm(
    pot.candidate.binary ~ treatment2 * pot.attitudes,
    data = d,
    weights = oct2015wt1,
    family = binomial(link = "logit")
  )

# ORIGINAL HANDLING MODEL WITH NO ROBUST #
pot.handling.pref.glm <-
  glm(
    pot.handling.binary ~ treatment2 * pot.attitudes,
    data = d,
    weights = oct2015wt1,
    family = binomial(link = "logit")
  )

#######################################################
## POT CANDIDATE MODEL AND ALTERNATIVES ROBUST W/PID ##
#######################################################

# ORIGINAL MODEL WITH ROBUST # 
pot.candidate.orig.rob <-
  glmRob(
    pot.candidate.binary ~ treatment2 * pot.attitudes,
    data = d,
    weights = oct2015wt1,
    family = binomial(link = "logit")
  )

#ALTERNATIVE MODEL 1 #
alt1pot.candidate.rob <-
  glmRob(
    pot.candidate.binary ~ treatment2 * pot.attitudes + pid7,
    data = d,
    weights=oct2015wt1,
    family = binomial(link = "logit"))

#ALTERNATIVE MODEL 2 #
alt2pot.candidate.rob<-
    glmRob(
      pot.candidate.binary ~ treatment2 + pot.attitudes + pid7,
      data = d,
      weights=oct2015wt1,
      family = binomial(link = "logit"))

# ALTERNATIVE MODEL 3 #
alt3pot.candidate.rob <-
  glmRob(
    pot.candidate.binary ~ pot.attitudes + pid7,
    data = d,
    weights=oct2015wt1,
    family = binomial(link = "logit"))

# ALTERNATIVE MODEL 4 #
alt4pot.candidate.rob <-
  glmRob(
    pot.candidate.binary ~ treatment2 * pot.attitudes * pid7,
    data = d,
    weights=oct2015wt1,
    family = binomial(link = "logit"))

######################################################
## POT HANDLING MODEL AND ALTERNATIVES ROBUST W/PID ##
######################################################

# ORIGINAL MODEL WITH ROBUST # 
pot.handle.orig.rob <-
  glm(
    pot.handling.binary ~ treatment2 * pot.attitudes,
    data = d,
    weights = oct2015wt1,
    family = binomial(link = "logit")
  )

#ALTERNATIVE MODEL 1 #
alt1pot.handle.rob <-
  glmRob(
    pot.handling.binary ~ treatment2 * pot.attitudes + pid7,
    data = d,
    weights=oct2015wt1,
    family = binomial(link = "logit"))

#ALTERNATIVE MODEL 2 #
alt2pot.handle.rob<-
  glmRob(
    pot.handling.binary ~ treatment2 + pot.attitudes + pid7,
    data = d,
    weights=oct2015wt1,
    family = binomial(link = "logit"))

# ALTERNATIVE MODEL 3 #
alt3pot.handle.rob <-
  glmRob(
    pot.handling.binary ~ pot.attitudes + pid7,
    data = d,
    weights=oct2015wt1,
    family = binomial(link = "logit"))

# ALTERNATIVE MODEL 4 #
alt4pot.handle.rob <-
  glmRob(
    pot.handling.binary ~ treatment2 * pot.attitudes * pid7,
    data = d,
    weights=oct2015wt1,
    family = binomial(link = "logit"))




probPot <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

val.orig<- d %>%
  expand(treatment2=1,pot.attitudes)

predict.original<-augment(pot.candidate.pref.glm,
                         type.predict = "response",
                         newdata = val.orig
                         )


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

table3 <-
  htmlreg(
    l = list(
      pot.candidate.orig.rob,
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
    file = "table3.html"
  )