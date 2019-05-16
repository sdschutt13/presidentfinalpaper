
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

## Load T

attach(tapsData)

###################################################################
## Table 2: Policy Preferences and the Effect of Unilateral Action
##################################################################
pot.candidate.pref.glm <-
  glmRob(
    pot.candidate.binary ~ treatment2 * pot.attitudes,
    data = tapsData,
    weights = oct2015wt1,
    family = binomial(),
    method = "misclass"
  )
summary(pot.candidate.pref.glm)
pot.candidate.pref.glm$fitted.values

cor(treatment2, pot.attitudes)

predict.glmRob(pot.candidate.pref.glm, newdata = tapsData)

dat0 <- data.frame(pot.candidate.binary=tapsData$pot.candidate.binary, pred0=pot.candidate.pref.glm$fitted.values)
summary(dat0)

roc0 <- roc(dat0$pot.candidate.binary, dat0$pred0, ci=TRUE)
roc0
plot.roc(roc0)

pos_or_neg <- ifelse(dat0$pred0 > 0.5, 1, 0)
p_class <- factor(pos_or_neg)
t_class <- factor(dat0$pot.candidate.binary)
confusionMatrix(t_class, p_class, positive = "1")

plot(resid(pot.candidate.pref.glm))

vcov(pot.candidate.pref.glm)
cor(pot.attitudes, treatment2, use = "complete.obs")
cor(ideology7, treatment2, use = "complete.obs")
cor(pot.attitudes, ideology7, use = "complete.obs")
cor(pot.attitudes, pid7, use = "complete.obs")
cor(tax.attitudes, ideology7, use = "complete.obs")
cor(tax.attitudes, pid7, use = "complete.obs")
cor(defense.attitudes, ideology7, use = "complete.obs")
cor(defense.attitudes, pid7, use = "complete.obs")


