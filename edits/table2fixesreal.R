## Replication File for Critique Reeves and Rogowski 2019
## Sam Schutt Critique Reeves Rogowski
## May 15th, 2019
## Creates Table 2


## Load necessary packages

################
## LOAD PACKS ##
################

#####
packages_required <- c('foreign',
                       'car',
                       'SDMTools',
                       'texreg',
                       'plyr',
                       'Zelig',
                       'weights',
                       'robust',
                       'pander',
                       'dplyr',
                       'mice',
                       'tidyr',
                       'broom',
                       'ggplot2')

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
library(ggplot2)


# LOAD IN DATA FROM GIT REPOSITORY #

load(url("https://github.com/sdschutt13/presidentfinalpaper/raw/master/Replication%20data/RR-AJPS-taps-processed%20(3).RData"))
d<-tapsData ##Faster to type every time, keeps original data safe
d1<-d ##extra copy just in case

#####

#######################
## CODING NEW MODELS ##
#######################

#########
## POT ##
#########


################################################
## POT CANDIDATE MODEL AND ALTERNATIVES W/PID ##
################################################


#####
# ORIGINAL MODEL CANDIDATE POT # 
pot.cand.orig <-
  glm(
    pot.candidate.binary ~ treatment2 * pot.attitudes,
    data = d,
    weights=oct2015wt1,
    family = binomial(link = "logit"))

# ORIGINAL CANDIDATE MODEL PREDICT #
val.pot<- d %>%
  expand(treatment2=1,pot.attitudes)

pred.pot.cand.orig<-augment(pot.cand.orig,
                                    type.predict = "response",
                                    newdata = val.pot)

#####
# ALTERNATIVE MODEL 1 CANDIDATE POT #
alt1pot.cand <-
  glm(
    pot.candidate.binary ~ treatment2 * pot.attitudes + pid3,
    data = d,
    weights=oct2015wt1,
    family = binomial(link = "logit"))

# CANDIDATE MODEL ALT 1 PREDICT #
val.pot.alt<- d %>%
  expand(treatment2=1,pot.attitudes, pid3)

pred.alt1pot.cand<-augment(alt1pot.cand,
                              type.predict = "response",
                              newdata = val.pot.alt)

#####
# ALTERNATIVE MODEL 2 CANDIDATE POT #
alt2pot.cand<-
    glm(
      pot.candidate.binary ~ treatment2 + pot.attitudes + pid3,
      data = d,
      weights=oct2015wt1,
      family = binomial(link = "logit"))

# ALTERNATIVE MODEL 2 CANDIDATE PREDICT POT #
pred.alt2pot.cand<-augment(alt2pot.cand,
                              type.predict = "response",
                              newdata = val.pot.alt)

#####
# ALTERNATIVE MODEL 3 CANDIDATE POT #
alt3pot.cand <-
  glm(
    pot.candidate.binary ~ pot.attitudes + pid3,
    data = d,
    weights=oct2015wt1,
    family = binomial(link = "logit"))

# ALTERNATIVE MODEL 3 CANDIDATE PREDICT POT #
pred.alt3pot.cand<-augment(alt3pot.cand,
                           type.predict = "response",
                           newdata = val.pot.alt)

#####

###############################################
## POT HANDLING MODEL AND ALTERNATIVES W/PID ##
###############################################

#####
# ORIGINAL MODEL HANDLING POT # 
pot.hand.orig <-
  glm(
    pot.handling.binary ~ treatment2 * pot.attitudes,
    data = d,
    weights=oct2015wt1,
    family = binomial(link = "logit"))

# ORIGINAL HANDLING MODEL PREDICT POT #
val.pot.hand<- d %>%
  expand(treatment2=1,pot.attitudes)

pred.pot.hand.orig<-augment(pot.hand.orig,
                            type.predict = "response",
                            newdata = val.pot.hand)

#####
# ALTERNATIVE MODEL 1 HANDLING POT #
alt1pot.hand <-
  glm(
    pot.handling.binary ~ treatment2 * pot.attitudes + pid3,
    data = d,
    weights=oct2015wt1,
    family = binomial(link = "logit"))

# HANDLING MODEL ALT 1 PREDICT POT #
val.pot.alt.hand<- d %>%
  expand(treatment2=1,pot.attitudes, pid3)

pred.alt1pot.hand<-augment(alt1pot.hand,
                           type.predict = "response",
                           newdata = val.pot.alt.hand)

#####
# ALTERNATIVE MODEL 2 HANDLING POT #
alt2pot.hand<-
  glm(
    pot.handling.binary ~ treatment2 + pot.attitudes + pid3,
    data = d,
    weights=oct2015wt1,
    family = binomial(link = "logit"))

# ALTERNATIVE MODEL 2 HANDLING PREDICT POT #
pred.alt2pot.hand<-augment(alt2pot.hand,
                           type.predict = "response",
                           newdata = val.pot.alt.hand)

#####
# ALTERNATIVE MODEL 3 HANDLING POT #
alt3pot.hand <-
  glm(
    pot.handling.binary ~ pot.attitudes + pid3,
    data = d,
    weights=oct2015wt1,
    family = binomial(link = "logit"))

# ALTERNATIVE MODEL 3 HANDLING PREDICT POT #
pred.alt3pot.hand<-augment(alt3pot.hand,
                           type.predict = "response",
                           newdata = val.pot.alt.hand)

#####



#########
## TAX ##
#########
################################################
## TAX CANDIDATE MODEL AND ALTERNATIVES W/PID ##
################################################


#####
#  TAX ORIGINAL MODEL CANDIDATE # 
tax.cand.orig <-
  glm(
    tax.candidate.binary ~ treatment2 * tax.attitudes,
    data = d,
    weights=oct2015wt1,
    family = binomial(link = "logit"))

# TAX ORIGINAL CANDIDATE MODEL PREDICT #
val.tax<- d %>%
  expand(treatment2=1,tax.attitudes)

pred.tax.cand.orig<-augment(tax.cand.orig,
                            type.predict = "response",
                            newdata = val.tax)

#####
# ALTERNATIVE MODEL 1 CANDIDATE TAX #
alt1tax.cand <-
  glm(
    tax.candidate.binary ~ treatment2 * tax.attitudes + pid3,
    data = d,
    weights=oct2015wt1,
    family = binomial(link = "logit"))

# CANDIDATE MODEL ALT 1 PREDICT TAX #
val.tax.alt<- d %>%
  expand(treatment2=1,tax.attitudes, pid3)

pred.alt1tax.cand<-augment(alt1tax.cand,
                           type.predict = "response",
                           newdata = val.tax.alt)

#####
# ALTERNATIVE MODEL 2 CANDIDATE TAX #
alt2tax.cand<-
  glm(
    tax.candidate.binary ~ treatment2 + tax.attitudes + pid3,
    data = d,
    weights=oct2015wt1,
    family = binomial(link = "logit"))

# ALTERNATIVE MODEL 2 CANDIDATE PREDICT TAX #
pred.alt2tax.cand<-augment(alt2tax.cand,
                           type.predict = "response",
                           newdata = val.tax.alt)

#####
# ALTERNATIVE MODEL 3 CANDIDATE TAX #
alt3tax.cand <-
  glm(
    tax.candidate.binary ~ tax.attitudes + pid3,
    data = d,
    weights=oct2015wt1,
    family = binomial(link = "logit"))

# ALTERNATIVE MODEL 3 CANDIDATE PREDICT TAX #
pred.alt3tax.cand<-augment(alt3tax.cand,
                           type.predict = "response",
                           newdata = val.tax.alt)

#####


###############################################
## TAX HANDLING MODEL AND ALTERNATIVES W/PID ##
###############################################

#####
# ORIGINAL MODEL HANDLING TAX # 
tax.hand.orig <-
  glm(
    tax.handling.binary ~ treatment2 * tax.attitudes,
    data = d,
    weights=oct2015wt1,
    family = binomial(link = "logit"))

# ORIGINAL HANDLING MODEL PREDICT TAX #
val.tax.hand<- d %>%
  expand(treatment2=1,tax.attitudes)

pred.tax.hand.orig<-augment(tax.hand.orig,
                            type.predict = "response",
                            newdata = val.tax.hand)

#####
# ALTERNATIVE MODEL 1 HANDLING TAX #
alt1tax.hand <-
  glm(
    tax.handling.binary ~ treatment2 * tax.attitudes + pid3,
    data = d,
    weights=oct2015wt1,
    family = binomial(link = "logit"))

# HANDLING MODEL ALT 1 PREDICT TAX #
val.tax.alt.hand<- d %>%
  expand(treatment2=1,tax.attitudes, pid3)

pred.alt1tax.hand<-augment(alt1tax.hand,
                           type.predict = "response",
                           newdata = val.tax.alt.hand)

#####
# ALTERNATIVE MODEL 2 HANDLING TAX #
alt2tax.hand<-
  glm(
    tax.handling.binary ~ treatment2 + tax.attitudes + pid3,
    data = d,
    weights=oct2015wt1,
    family = binomial(link = "logit"))

# ALTERNATIVE MODEL 2 HANDLING PREDICT TAX #
pred.alt2tax.hand<-augment(alt2tax.hand,
                           type.predict = "response",
                           newdata = val.tax.alt.hand)

#####
# ALTERNATIVE MODEL 3 HANDLING TAX #
alt3tax.hand <-
  glm(
    tax.handling.binary ~ tax.attitudes + pid3,
    data = d,
    weights=oct2015wt1,
    family = binomial(link = "logit"))

# ALTERNATIVE MODEL 3 HANDLING PREDICT TAX #
pred.alt3tax.hand<-augment(alt3tax.hand,
                           type.predict = "response",
                           newdata = val.tax.alt.hand)

#####




#############
## DEFENSE ##
#############
####################################################
## DEFENSE CANDIDATE MODEL AND ALTERNATIVES W/PID ##
####################################################

#####
#  DEFENSE ORIGINAL MODEL CANDIDATE # 
def.cand.orig <-
  glm(
    defense.candidate.binary ~ treatment2 * defense.attitudes,
    data = d,
    weights=oct2015wt1,
    family = binomial(link = "logit"))

# DEFENSE ORIGINAL CANDIDATE MODEL PREDICT #
val.def<- d %>%
  expand(treatment2=1,defense.attitudes)

pred.def.cand.orig<-augment(def.cand.orig,
                            type.predict = "response",
                            newdata = val.def)

#####
# ALTERNATIVE MODEL 1 CANDIDATE DEFENSE #
alt1def.cand <-
  glm(
    defense.candidate.binary ~ treatment2 * defense.attitudes + pid3,
    data = d,
    weights=oct2015wt1,
    family = binomial(link = "logit"))

# CANDIDATE MODEL ALT 1 PREDICT TAX #
val.def.alt<- d %>%
  expand(treatment2=1,defense.attitudes, pid3)

pred.alt1def.cand<-augment(alt1def.cand,
                           type.predict = "response",
                           newdata = val.def.alt)

#####
# ALTERNATIVE MODEL 2 CANDIDATE DEFENSE #
alt2def.cand<-
  glm(
    defense.candidate.binary ~ treatment2 + defense.attitudes + pid3,
    data = d,
    weights=oct2015wt1,
    family = binomial(link = "logit"))

# ALTERNATIVE MODEL 2 CANDIDATE PREDICT DEFENSE #
pred.alt2def.cand<-augment(alt2def.cand,
                           type.predict = "response",
                           newdata = val.def.alt)

#####
# ALTERNATIVE MODEL 3 CANDIDATE DEFENSE #
alt3def.cand <-
  glm(
    defense.candidate.binary ~ defense.attitudes + pid3,
    data = d,
    weights=oct2015wt1,
    family = binomial(link = "logit"))

# ALTERNATIVE MODEL 3 CANDIDATE PREDICT DEFENSE #
pred.alt3def.cand<-augment(alt3def.cand,
                           type.predict = "response",
                           newdata = val.def.alt)

#####

###################################################
## DEFENSE HANDLING MODEL AND ALTERNATIVES W/PID ##
###################################################

#####
# ORIGINAL MODEL HANDLING DEFENSE # 
def.hand.orig <-
  glm(
    defense.handling.binary ~ treatment2 * defense.attitudes,
    data = d,
    weights=oct2015wt1,
    family = binomial(link = "logit"))

# ORIGINAL HANDLING MODEL PREDICT TAX #
val.def.hand<- d %>%
  expand(treatment2=1,defense.attitudes)

pred.def.hand.orig<-augment(def.hand.orig,
                            type.predict = "response",
                            newdata = val.def.hand)

#####
# ALTERNATIVE MODEL 1 HANDLING DEFENSE #
alt1def.hand <-
  glm(
    defense.handling.binary ~ treatment2 * defense.attitudes + pid3,
    data = d,
    weights=oct2015wt1,
    family = binomial(link = "logit"))

# HANDLING MODEL ALT 1 PREDICT DEFENSE #
val.def.alt.hand<- d %>%
  expand(treatment2=1,defense.attitudes, pid3)

pred.alt1def.hand<-augment(alt1def.hand,
                           type.predict = "response",
                           newdata = val.def.alt.hand)

#####
# ALTERNATIVE MODEL 2 HANDLING DEFENSE #
alt2def.hand<-
  glm(
    defense.handling.binary ~ treatment2 + defense.attitudes + pid3,
    data = d,
    weights=oct2015wt1,
    family = binomial(link = "logit"))

# ALTERNATIVE MODEL 2 HANDLING PREDICT DEFENSE #
pred.alt2def.hand<-augment(alt2def.hand,
                           type.predict = "response",
                           newdata = val.def.alt.hand)

#####
# ALTERNATIVE MODEL 3 HANDLING DEFENSE #
alt3def.hand <-
  glm(
    defense.handling.binary ~ defense.attitudes + pid3,
    data = d,
    weights=oct2015wt1,
    family = binomial(link = "logit"))

# ALTERNATIVE MODEL 3 HANDLING PREDICT DEFENSE #
pred.alt3def.hand<-augment(alt3def.hand,
                           type.predict = "response",
                           newdata = val.def.alt.hand)

#####

################
## END CODING ##
################

##############
## GRAPHICS ##
##############

###########
## SETUP ##
###########
#####
labelpot <- c("-2" = "Strongly Disagree", "-1" = "Disagree", "0" = "Neither", "1" = "Agree", "2" = "Strongly Agree")

#####




#########
## POT ##
#########
#####
## ALTERNATIVE 1 PREDICTED POT CANDIDATE ##

pred.alt1pot.cand %>% drop_na() %>%
  ggplot() + 
    aes(x = pid3, y = .fitted, color = pid3) + 
     geom_pointrange(aes(ymin = .fitted - 1.96*.se.fit,
                      ymax = .fitted + 1.96*.se.fit) )  + 
      coord_flip() +
      facet_wrap("pot.attitudes", ncol = 1, labeller = labeller("pot.attitudes" = labelpot)) +
      labs(y = "Probability of Support for Candidate", x = "Support for Marijuana Legalization") + 
      labs(color ="Party ID")+
      labs(title = "Unilateral Action Use for Pot\n Support of Candidate", 
           subtitle = "Alternative Model 1 Pot - Candidate")+
      scale_color_manual(limits= c("dem", "ind", "gop"), 
                         values = c("dodgerblue3", "purple3", "red4"), 
                         labels = c("Democrats", "Independents", "Republicans")) +
      scale_x_discrete(limits= c("gop", "ind", "dem"),
                       breaks = c("0", ".1", ".2", ".3", ".4", ".5", ".6", ".7", ".8", ".9", "1")) +
      theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = 15, face="bold", hjust = 0.5),
        plot.subtitle = element_text(size = 8))

#####
## ALTERNATIVE 1 PREDICTED POT HANDLING ##

pred.alt1pot.hand %>% drop_na() %>%
  ggplot() + 
  aes(x = pid3, y = .fitted, color = pid3) + 
  geom_pointrange(aes(ymin = .fitted - 1.96*.se.fit,
                      ymax = .fitted + 1.96*.se.fit) )  + 
  coord_flip() +
  facet_wrap("pot.attitudes", ncol = 1, labeller = labeller("pot.attitudes" = labelpot)) +
  labs(y = "Probability of Support for Handling", x = "Support for Marijuana Legalization") + 
  labs(color ="Party ID")+
  labs(title = "Unilateral Action Use for Pot\n Support of Candidate's Handling", 
       subtitle = "Alternative Model 1 Pot - Handling")+
  scale_color_manual(limits= c("dem", "ind", "gop"), 
                     values = c("dodgerblue3", "purple3", "red4"), 
                     labels = c("Democrats", "Independents", "Republicans")) +
  scale_x_discrete(limits= c("gop", "ind", "dem"),
                   breaks = c("0", ".1", ".2", ".3", ".4", ".5", ".6", ".7", ".8", ".9", "1")) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = 15, face="bold", hjust = 0.5),
        plot.subtitle = element_text(size = 8))

#####
## ALTERNATIVE 2 PREDICTED POT CANDIDATE ##

pred.alt2pot.cand %>% drop_na() %>%
  ggplot() + 
  aes(x = pid3, y = .fitted, color = pid3) + 
  geom_pointrange(aes(ymin = .fitted - 1.96*.se.fit,
                      ymax = .fitted + 1.96*.se.fit) )  + 
  coord_flip() +
  facet_wrap("pot.attitudes", ncol = 1, labeller = labeller("pot.attitudes" = labelpot)) +
  labs(y = "Probability of Support for Candidate", x = "Support for Marijuana Legalization") + 
  labs(color ="Party ID")+
  labs(title = "Unilateral Action Use for Pot\n Support of Candidate", 
       subtitle = "Alternative Model 2 Pot - Candidate")+
  scale_color_manual(limits= c("dem", "ind", "gop"), 
                     values = c("dodgerblue3", "purple3", "red4"), 
                     labels = c("Democrats", "Independents", "Republicans")) +
  scale_x_discrete(limits= c("gop", "ind", "dem"),
                   breaks = c("0", ".1", ".2", ".3", ".4", ".5", ".6", ".7", ".8", ".9", "1")) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = 15, face="bold", hjust = 0.5),
        plot.subtitle = element_text(size = 8))

#####
## ALTERNATIVE 2 PREDICTED POT HANDLING ##

pred.alt2pot.hand %>% drop_na() %>%
  ggplot() + 
  aes(x = pid3, y = .fitted, color = pid3) + 
  geom_pointrange(aes(ymin = .fitted - 1.96*.se.fit,
                      ymax = .fitted + 1.96*.se.fit) )  + 
  coord_flip() +
  facet_wrap("pot.attitudes", ncol = 1, labeller = labeller("pot.attitudes" = labelpot)) +
  labs(y = "Probability of Support for Handling", x = "Support for Marijuana Legalization") + 
  labs(color ="Party ID")+
  labs(title = "Unilateral Action Use for Pot\n Support of Candidate's Handling", 
       subtitle = "Alternative Model 2 Pot - Handling")+
  scale_color_manual(limits= c("dem", "ind", "gop"), 
                     values = c("dodgerblue3", "purple3", "red4"), 
                     labels = c("Democrats", "Independents", "Republicans")) +
  scale_x_discrete(limits= c("gop", "ind", "dem"),
                   breaks = c("0", ".1", ".2", ".3", ".4", ".5", ".6", ".7", ".8", ".9", "1")) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = 15, face="bold", hjust = 0.5),
        plot.subtitle = element_text(size = 8))

#####
## ALTERNATIVE 3 PREDICTED POT CANDIDATE ##

pred.alt3pot.cand %>% drop_na() %>%
  ggplot() + 
  aes(x = pid3, y = .fitted, color = pid3) + 
  geom_pointrange(aes(ymin = .fitted - 1.96*.se.fit,
                      ymax = .fitted + 1.96*.se.fit) )  + 
  coord_flip() +
  facet_wrap("pot.attitudes", ncol = 1, labeller = labeller("pot.attitudes" = labelpot)) +
  labs(y = "Probability of Support for Candidate", x = "Support for Marijuana Legalization") + 
  labs(color ="Party ID")+
  labs(title = "Unilateral Action Use for Pot\n Support of Candidate", 
       subtitle = "Alternative Model 3 Pot - Candidate")+
  scale_color_manual(limits= c("dem", "ind", "gop"), 
                     values = c("dodgerblue3", "purple3", "red4"), 
                     labels = c("Democrats", "Independents", "Republicans")) +
  scale_x_discrete(limits= c("gop", "ind", "dem"),
                   breaks = c("0", ".1", ".2", ".3", ".4", ".5", ".6", ".7", ".8", ".9", "1")) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = 15, face="bold", hjust = 0.5),
        plot.subtitle = element_text(size = 8))

#####
## ALTERNATIVE 3 PREDICTED POT HANDLING ##

pred.alt3pot.hand %>% drop_na() %>%
  ggplot() + 
  aes(x = pid3, y = .fitted, color = pid3) + 
  geom_pointrange(aes(ymin = .fitted - 1.96*.se.fit,
                      ymax = .fitted + 1.96*.se.fit) )  + 
  coord_flip() +
  facet_wrap("pot.attitudes", ncol = 1, labeller = labeller("pot.attitudes" = labelpot)) +
  labs(y = "Probability of Support for Handling", x = "Support for Marijuana Legalization") + 
  labs(color ="Party ID")+
  labs(title = "Unilateral Action Use for Pot\n Support of Candidate's Handling", 
       subtitle = "Alternative Model 3 Pot - Handling")+
  scale_color_manual(limits= c("dem", "ind", "gop"), 
                     values = c("dodgerblue3", "purple3", "red4"), 
                     labels = c("Democrats", "Independents", "Republicans")) +
  scale_x_discrete(limits= c("gop", "ind", "dem"),
                   breaks = c("0", ".1", ".2", ".3", ".4", ".5", ".6", ".7", ".8", ".9", "1")) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = 15, face="bold", hjust = 0.5),
        plot.subtitle = element_text(size = 8))


#####



#########
## TAX ##
#########
#####
## ALTERNATIVE 1 PREDICTED TAX CANDIDATE ##

pred.alt1tax.cand %>% drop_na() %>%
  ggplot() + 
  aes(x = pid3, y = .fitted, color = pid3) + 
  geom_pointrange(aes(ymin = .fitted - 1.96*.se.fit,
                      ymax = .fitted + 1.96*.se.fit) )  + 
  coord_flip() +
  facet_wrap("tax.attitudes", ncol = 1, labeller = labeller("tax.attitudes" = labelpot)) +
  labs(y = "Probability of Support for Candidate", x = "Support for Taxing Corporations") + 
  labs(color ="Party ID")+
  labs(title = "Unilateral Action Use for Tax\n Support of Candidate", 
       subtitle = "Alternative Model 1 Taxes - Candidate")+
  scale_color_manual(limits= c("dem", "ind", "gop"), 
                     values = c("dodgerblue3", "purple3", "red4"), 
                     labels = c("Democrats", "Independents", "Republicans")) +
  scale_x_discrete(limits= c("gop", "ind", "dem"),
                   breaks = c("0", ".1", ".2", ".3", ".4", ".5", ".6", ".7", ".8", ".9", "1")) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = 15, face="bold", hjust = 0.5),
        plot.subtitle = element_text(size = 8))

#####
## ALTERNATIVE 1 PREDICTED TAX HANDLING ##

pred.alt1tax.hand %>% drop_na() %>%
  ggplot() + 
  aes(x = pid3, y = .fitted, color = pid3) + 
  geom_pointrange(aes(ymin = .fitted - 1.96*.se.fit,
                      ymax = .fitted + 1.96*.se.fit) )  + 
  coord_flip() +
  facet_wrap("tax.attitudes", ncol = 1, labeller = labeller("tax.attitudes" = labelpot)) +
  labs(y = "Probability of Support for Handling", x = "Support for Taxing Corporations") + 
  labs(color ="Party ID")+
  labs(title = "Unilateral Action Use for Tax\n Support of Candidate's Handling", 
       subtitle = "Alternative Model 1 Taxes - Handling")+
  scale_color_manual(limits= c("dem", "ind", "gop"),
                     values = c("dodgerblue3", "purple3", "red4"), 
                     labels = c("Democrats", "Independents", "Republicans")) +
  scale_x_discrete(limits= c("gop", "ind", "dem"),
                   breaks = c("0", ".1", ".2", ".3", ".4", ".5", ".6", ".7", ".8", ".9", "1")) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = 15, face="bold", hjust = 0.5),
        plot.subtitle = element_text(size = 8))

#####
## ALTERNATIVE 2 PREDICTED TAX CANDIDATE ##

pred.alt2tax.cand %>% drop_na() %>%
  ggplot() + 
  aes(x = pid3, y = .fitted, color = pid3) + 
  geom_pointrange(aes(ymin = .fitted - 1.96*.se.fit,
                      ymax = .fitted + 1.96*.se.fit) )  + 
  coord_flip() +
  facet_wrap("tax.attitudes", ncol = 1, labeller = labeller("tax.attitudes" = labelpot)) +
  labs(y = "Probability of Support for Candidate", x = "Support for Taxing Corporations") + 
  labs(color ="Party ID")+
  labs(title = "Unilateral Action Use for Tax\n Support of Candidate", 
       subtitle = "Alternative Model 2 Taxes - Candidate")+
  scale_color_manual(limits= c("dem", "ind", "gop"), 
                     values = c("dodgerblue3", "purple3", "red4"), 
                     labels = c("Democrats", "Independents", "Republicans")) +
  scale_x_discrete(limits= c("gop", "ind", "dem"),
                   breaks = c("0", ".1", ".2", ".3", ".4", ".5", ".6", ".7", ".8", ".9", "1")) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = 15, face="bold", hjust = 0.5),
        plot.subtitle = element_text(size = 8))

#####
## ALTERNATIVE 2 PREDICTED TAX HANDLING ##

pred.alt2tax.hand %>% drop_na() %>%
  ggplot() + 
  aes(x = pid3, y = .fitted, color = pid3) + 
  geom_pointrange(aes(ymin = .fitted - 1.96*.se.fit,
                      ymax = .fitted + 1.96*.se.fit) )  + 
  coord_flip() +
  facet_wrap("tax.attitudes", ncol = 1, labeller = labeller("tax.attitudes" = labelpot)) +
  labs(y = "Probability of Support for Handling", x = "Support for Taxing Corporations") + 
  labs(color ="Party ID")+
  labs(title = "Unilateral Action Use for Tax\n Support of Candidate's Handling", 
       subtitle = "Alternative Model 2 Taxes - Handling")+
  scale_color_manual(limits= c("dem", "ind", "gop"), 
                     values = c("dodgerblue3", "purple3", "red4"), 
                     labels = c("Democrats", "Independents", "Republicans")) +
  scale_x_discrete(limits= c("gop", "ind", "dem"),
                   breaks = c("0", ".1", ".2", ".3", ".4", ".5", ".6", ".7", ".8", ".9", "1")) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = 15, face="bold", hjust = 0.5),
        plot.subtitle = element_text(size = 8))

#####
## ALTERNATIVE 3 PREDICTED TAX CANDIDATE ##

pred.alt3tax.cand %>% drop_na() %>%
  ggplot() + 
  aes(x = pid3, y = .fitted, color = pid3) + 
  geom_pointrange(aes(ymin = .fitted - 1.96*.se.fit,
                      ymax = .fitted + 1.96*.se.fit) )  + 
  coord_flip() +
  facet_wrap("tax.attitudes", ncol = 1, labeller = labeller("tax.attitudes" = labelpot)) +
  labs(y = "Probability of Support for Candidate", x = "Support for Taxing Corporations") + 
  labs(color ="Party ID")+
  labs(title = "Unilateral Action Use for Tax\n Support of Candidate", 
       subtitle = "Alternative Model 3 Taxes - Candidate")+
  scale_color_manual(limits= c("dem", "ind", "gop"), 
                     values = c("dodgerblue3", "purple3", "red4"), 
                     labels = c("Democrats", "Independents", "Republicans")) +
  scale_x_discrete(limits= c("gop", "ind", "dem"),
                   breaks = c("0", ".1", ".2", ".3", ".4", ".5", ".6", ".7", ".8", ".9", "1")) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = 15, face="bold", hjust = 0.5),
        plot.subtitle = element_text(size = 8))

#####
## ALTERNATIVE 3 PREDICTED TAX HANDLING ##

pred.alt3tax.hand %>% drop_na() %>%
  ggplot() + 
  aes(x = pid3, y = .fitted, color = pid3) + 
  geom_pointrange(aes(ymin = .fitted - 1.96*.se.fit,
                      ymax = .fitted + 1.96*.se.fit) )  + 
  coord_flip() +
  facet_wrap("tax.attitudes", ncol = 1, labeller = labeller("tax.attitudes" = labelpot)) +
  labs(y = "Probability of Support for Handling", x = "Support for Taxing Corporations") + 
  labs(color ="Party ID")+
  labs(title = "Unilateral Action Use for Tax\n Support of Candidate's Handling", 
       subtitle = "Alternative Model 3 Taxes - Handling")+
  scale_color_manual(limits= c("dem", "ind", "gop"), 
                     values = c("dodgerblue3", "purple3", "red4"), 
                     labels = c("Democrats", "Independents", "Republicans")) +
  scale_x_discrete(limits= c("gop", "ind", "dem"),
                   breaks = c("0", ".1", ".2", ".3", ".4", ".5", ".6", ".7", ".8", ".9", "1")) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = 15, face="bold", hjust = 0.5),
        plot.subtitle = element_text(size = 8))














#####



#############
## DEFENSE ##
#############
#####
## ALTERNATIVE 1 PREDICTED DEFENSE CANDIDATE ##

pred.alt1def.cand %>% drop_na() %>%
  ggplot() + 
  aes(x = pid3, y = .fitted, color = pid3) + 
  geom_pointrange(aes(ymin = .fitted - 1.96*.se.fit,
                      ymax = .fitted + 1.96*.se.fit) )  + 
  coord_flip() +
  facet_wrap("defense.attitudes", ncol = 1, labeller = labeller("defense.attitudes" = labelpot)) +
  labs(y = "Probability of Support for Candidate", x = "Support for Defense Spending") + 
  labs(color ="Party ID")+
  labs(title = "Unilateral Action Use for Defense\n Support of Candidate", 
       subtitle = "Alternative Model 1 Defense - Candidate")+
  scale_color_manual(limits= c("dem", "ind", "gop"), 
                     values = c("dodgerblue3", "purple3", "red4"), 
                     labels = c("Democrats", "Independents", "Republicans")) +
  scale_x_discrete(limits= c("gop", "ind", "dem"),
                   breaks = c("0", ".1", ".2", ".3", ".4", ".5", ".6", ".7", ".8", ".9", "1")) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = 15, face="bold", hjust = 0.5),
        plot.subtitle = element_text(size = 8))

#####
## ALTERNATIVE 1 PREDICTED DEFENSE HANDLING ##

pred.alt1def.hand %>% drop_na() %>%
  ggplot() + 
  aes(x = pid3, y = .fitted, color = pid3) + 
  geom_pointrange(aes(ymin = .fitted - 1.96*.se.fit,
                      ymax = .fitted + 1.96*.se.fit) )  + 
  coord_flip() +
  facet_wrap("defense.attitudes", ncol = 1, labeller = labeller("defense.attitudes" = labelpot)) +
  labs(y = "Probability of Support for Candidate", x = "Support for Defense Spending") + 
  labs(color ="Party ID")+
  labs(title = "Unilateral Action Use for Defense\n Support of Candidate", 
       subtitle = "Alternative Model 1 Defense - Handling")+
  scale_color_manual(limits= c("dem", "ind", "gop"), 
                     values = c("dodgerblue3", "purple3", "red4"), 
                     labels = c("Democrats", "Independents", "Republicans")) +
  scale_x_discrete(limits= c("gop", "ind", "dem"),
                   breaks = c("0", ".1", ".2", ".3", ".4", ".5", ".6", ".7", ".8", ".9", "1")) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = 15, face="bold", hjust = 0.5),
        plot.subtitle = element_text(size = 8))

#####
## ALTERNATIVE 2 PREDICTED DEFENSE CANDIDATE ##

pred.alt2def.cand %>% drop_na() %>%
  ggplot() + 
  aes(x = pid3, y = .fitted, color = pid3) + 
  geom_pointrange(aes(ymin = .fitted - 1.96*.se.fit,
                      ymax = .fitted + 1.96*.se.fit) )  + 
  coord_flip() +
  facet_wrap("defense.attitudes", ncol = 1, labeller = labeller("defense.attitudes" = labelpot)) +
  labs(y = "Probability of Support for Candidate", x = "Support for Defense Spending") + 
  labs(color ="Party ID")+
  labs(title = "Unilateral Action Use for Defense\n Support of Candidate", 
       subtitle = "Alternative Model 2 Defense - Candidate")+
  scale_color_manual(limits= c("dem", "ind", "gop"), 
                     values = c("dodgerblue3", "purple3", "red4"), 
                     labels = c("Democrats", "Independents", "Republicans")) +
  scale_x_discrete(limits= c("gop", "ind", "dem"),
                   breaks = c("0", ".1", ".2", ".3", ".4", ".5", ".6", ".7", ".8", ".9", "1")) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = 15, face="bold", hjust = 0.5),
        plot.subtitle = element_text(size = 8))

#####
## ALTERNATIVE 2 PREDICTED DEFENSE HANDLING ##

pred.alt2def.hand %>% drop_na() %>%
  ggplot() + 
  aes(x = pid3, y = .fitted, color = pid3) + 
  geom_pointrange(aes(ymin = .fitted - 1.96*.se.fit,
                      ymax = .fitted + 1.96*.se.fit) )  + 
  coord_flip() +
  facet_wrap("defense.attitudes", ncol = 1, labeller = labeller("defense.attitudes" = labelpot)) +
  labs(y = "Probability of Support for Candidate", x = "Support for Defense Spending") + 
  labs(color ="Party ID")+
  labs(title = "Unilateral Action Use for Defense\n Support of Candidate", 
       subtitle = "Alternative Model 2 Defense - Handling")+
  scale_color_manual(limits= c("dem", "ind", "gop"), 
                     values = c("dodgerblue3", "purple3", "red4"), 
                     labels = c("Democrats", "Independents", "Republicans")) +
  scale_x_discrete(limits= c("gop", "ind", "dem"),
                   breaks = c("0", ".1", ".2", ".3", ".4", ".5", ".6", ".7", ".8", ".9", "1")) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = 15, face="bold", hjust = 0.5),
        plot.subtitle = element_text(size = 8))

#####
## ALTERNATIVE 3 PREDICTED DEFENSE CANDIDATE ##

pred.alt3def.cand %>% drop_na() %>%
  ggplot() + 
  aes(x = pid3, y = .fitted, color = pid3) + 
  geom_pointrange(aes(ymin = .fitted - 1.96*.se.fit,
                      ymax = .fitted + 1.96*.se.fit) )  + 
  coord_flip() +
  facet_wrap("defense.attitudes", ncol = 1, labeller = labeller("defense.attitudes" = labelpot)) +
  labs(y = "Probability of Support for Candidate", x = "Support for Defense Spending") + 
  labs(color ="Party ID")+
  labs(title = "Unilateral Action Use for Defense\n Support of Candidate", 
       subtitle = "Alternative Model 3 Defense - Candidate")+
  scale_color_manual(limits= c("dem", "ind", "gop"), 
                     values = c("dodgerblue3", "purple3", "red4"), 
                     labels = c("Democrats", "Independents", "Republicans")) +
  scale_x_discrete(limits= c("gop", "ind", "dem"),
                   breaks = c("0", ".1", ".2", ".3", ".4", ".5", ".6", ".7", ".8", ".9", "1")) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = 15, face="bold", hjust = 0.5),
        plot.subtitle = element_text(size = 8))


#####
## ALTERNATIVE 3 PREDICTED TAX HANDLING ##

pred.alt3def.hand %>% drop_na() %>%
  ggplot() + 
  aes(x = pid3, y = .fitted, color = pid3) + 
  geom_pointrange(aes(ymin = .fitted - 1.96*.se.fit,
                      ymax = .fitted + 1.96*.se.fit) )  + 
  coord_flip() +
  facet_wrap("defense.attitudes", ncol = 1, labeller = labeller("defense.attitudes" = labelpot)) +
  labs(y = "Probability of Support for Candidate", x = "Support for Defense Spending") + 
  labs(color ="Party ID")+
  labs(title = "Unilateral Action Use for Defense\n Support of Candidate", 
       subtitle = "Alternative Model 3 Defense - Handling")+
  scale_color_manual(limits= c("dem", "ind", "gop"), 
                     values = c("dodgerblue3", "purple3", "red4"), 
                     labels = c("Democrats", "Independents", "Republicans")) +
  scale_x_discrete(limits= c("gop", "ind", "dem"),
                   breaks = c("0", ".1", ".2", ".3", ".4", ".5", ".6", ".7", ".8", ".9", "1")) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = 15, face="bold", hjust = 0.5),
        plot.subtitle = element_text(size = 8))


#####


























Logit_bounds <- function(model, Take, Report, Night, Convict){
  predictions <- rmvnorm(n = 1000, 
                         mean = model$coefficients, 
                         sigma = vcov(model)) %>%
    as_tibble() %>% 
    # Add a prefix to be clear that these are our betas
    rename_all(~str_c("beta_", .)) %>% 
    # z = log odds (the linear combination of predictors)
    mutate(z = beta_(Intercept) + beta_Take*Take + beta_Report*Report + beta_Night*Night + beta_Convict*Convict) %>% 
    # p = probabilty. Apply the logistic (inverse logit) function to the log odds
    mutate(p = 1/(1+exp(-z)) ) %>%
    # Add values to the data frame
    mutate(Take = Take,
           Report = Report,
           Night = Night,
           Convict = Convict)
  return(predictions)
}





























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






