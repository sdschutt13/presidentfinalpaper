## Replication files for The Cost of Unilateral Action
## Andrew Reeves and Jon C. Rogowski
## May 9, 2017
## Creates Figures 1


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
## Figure 1: Unilateral Action and Presidential Candidate Evaluations
######################################################################
## Top Panel
## Support Candidate
## Legalize marijuana Unilateral-Control
pot.cand.control <-
  wtd.t.test(pot.candidate.binary[treatment == "unilateral"],
             pot.candidate.binary[treatment == "control"],
             oct2015wt1[treatment == "unilateral"],
             oct2015wt1[treatment == "control"],
             samedata = F)$additional[c("Difference", "Std. Err")]
pot.cand.control
## Legalize marijuana Unilateral-Legislative
pot.cand.legis <-
  wtd.t.test(pot.candidate.binary[treatment == "unilateral"],
             pot.candidate.binary[treatment == "bilateral"],
             oct2015wt1[treatment == "unilateral"],
             oct2015wt1[treatment == "bilateral"],
             samedata = F)$additional[c("Difference", "Std. Err")]
pot.cand.legis
## Lower corporate taxes Unilateral-Control
tax.cand.control <-
  wtd.t.test(tax.candidate.binary[treatment == "unilateral"],
             tax.candidate.binary[treatment == "control"],
             oct2015wt1[treatment == "unilateral"],
             oct2015wt1[treatment == "control"],
             samedata = F)$additional[c("Difference", "Std. Err")]
tax.cand.control
## Lower corporate taxes Unilateral-Legislative
tax.cand.legis <-
  wtd.t.test(tax.candidate.binary[treatment == "unilateral"],
             tax.candidate.binary[treatment == "bilateral"],
             oct2015wt1[treatment == "unilateral"],
             oct2015wt1[treatment == "bilateral"],
             samedata = F)$additional[c("Difference", "Std. Err")]
tax.cand.legis

## Deploy troops Unilateral-Control
defense.cand.control <- wtd.t.test(
  defense.candidate.binary[treatment == "unilateral"],
  defense.candidate.binary[treatment == "control"],
  oct2015wt1[treatment == "unilateral"],
  oct2015wt1[treatment == "control"],
  samedata = F
)$additional[c("Difference", "Std. Err")]
defense.cand.control

## Deploy troops Unilateral-Legislative
defense.cand.legis <- wtd.t.test(
  defense.candidate.binary[treatment == "unilateral"],
  defense.candidate.binary[treatment == "bilateral"],
  oct2015wt1[treatment == "unilateral"],
  oct2015wt1[treatment == "bilateral"],
  samedata = F
)$additional[c("Difference", "Std. Err")]
defense.cand.legis

## Bottom Panel
## Approval of Candidate's Handling of Iss
## Legalize marijuana Unilateral-Control
pot.handling.control <-
  wtd.t.test(pot.handling.binary[treatment == "unilateral"],
             pot.handling.binary[treatment == "control"],
             oct2015wt1[treatment == "unilateral"],
             oct2015wt1[treatment == "control"],
             samedata = F)$additional[c("Difference", "Std. Err")]
pot.handling.control
## Legalize marijuana Unilateral-Legislative
pot.handling.legis <-
  wtd.t.test(pot.handling.binary[treatment == "unilateral"],
             pot.handling.binary[treatment == "bilateral"],
             oct2015wt1[treatment == "unilateral"],
             oct2015wt1[treatment == "bilateral"],
             samedata = F)$additional[c("Difference", "Std. Err")]
pot.handling.legis
## Lower corporate taxes Unilateral-Control
tax.handling.control <-
  wtd.t.test(tax.handling.binary[treatment == "unilateral"],
             tax.handling.binary[treatment == "control"],
             oct2015wt1[treatment == "unilateral"],
             oct2015wt1[treatment == "control"],
             samedata = F)$additional[c("Difference", "Std. Err")]
tax.handling.control
## Lower corporate taxes Unilateral-Legislative
tax.handling.legis <-
  wtd.t.test(tax.handling.binary[treatment == "unilateral"],
             tax.handling.binary[treatment == "bilateral"],
             oct2015wt1[treatment == "unilateral"],
             oct2015wt1[treatment == "bilateral"],
             samedata = F)$additional[c("Difference", "Std. Err")]
tax.handling.legis
## Deploy troops Unilateral-Control
defense.handling.control <- wtd.t.test(
  defense.handling.binary[treatment == "unilateral"],
  defense.handling.binary[treatment == "control"],
  oct2015wt1[treatment == "unilateral"],
  oct2015wt1[treatment == "control"],
  samedata = F
)$additional[c("Difference", "Std. Err")]
defense.handling.control
## Deploy troops Unilateral-Legislative
defense.handling.legis <- wtd.t.test(
  defense.handling.binary[treatment == "unilateral"],
  defense.handling.binary[treatment == "bilateral"],
  oct2015wt1[treatment == "unilateral"],
  oct2015wt1[treatment == "bilateral"],
  samedata = F
)$additional[c("Difference", "Std. Err")]
defense.handling.legis

effect <-
  c(
    pot.cand.control[1],
    pot.cand.legis[1],
    tax.cand.control[1],
    tax.cand.legis[1],
    defense.cand.control[1],
    defense.cand.legis[1]
  )
se <-
  c(
    pot.cand.control[2],
    pot.cand.legis[2],
    tax.cand.control[2],
    tax.cand.legis[2],
    defense.cand.control[2],
    defense.cand.legis[2]
  )
group <- c(6.15, 6,  4.15, 4, 2.15, 2)
pdf(file = "figure1-top.pdf")
plot(
  effect,
  group,
  xlim = c(-.3, .15),
  xaxt = "n",
  ylim = c(1, 6.75),
  pch = c(17, 19),
  tck = -.02,
  cex.axis = 0.9,
  cex = 1,
  ylab = "",
  yaxt = "n",
  xlab = "",
  axes = FALSE
)
abline(v = 0,
       lwd = .5,
       col = "gray",
       lty = 2)
segments(effect - se * 1.96, group, effect + 1.96 * se, group, lty = 1)
axis(1, at = c(-.3,-.2,-.1, 0, .1))#, labels = c("-0.60", "-0.40", "-0.20","0\n(No Difference\nfrom the control)", "0.20"), padj=1)#,tck=-.02)
par(mgp = c(2, .1, 0))
axis(
  2,
  at = c(6, 4, 2),
  las = 2,
  labels = c(
    "Legalize \nMarijuana",
    "Lower    \nCorporate \nTaxes    ",
    "Deploy   \n Troops   "
  ) ,
  tck = 0,
  lwd = 0,
  line = 0,
  cex.axis = .9
)
title(main = "Support Candidate",
      cex.main = 1,
      line = 1)
text(pot.cand.control[1], group[1] + .35, round(pot.cand.control[1], 2))
text(pot.cand.legis[1], group[2] - .35, round(pot.cand.legis[1], 2))
text(tax.cand.control[1], group[3] + .35, round(tax.cand.control[1], 2))
text(tax.cand.legis[1], group[4] - .35, round(tax.cand.legis[1], 2))
text(defense.cand.control[1],
     group[5] + .35,
     round(defense.cand.control[1], 2))
text(defense.cand.legis[1],
     group[6] - .35,
     round(defense.cand.legis[1], 2))
mtext("Effect Size", 1, line = 2.5)
legend(
  .02,
  7,
  c("Unilateral -\n Control\n", "Unilateral -\n Legislative"),
  pch = c(17, 19),
  lty = 1,
  bty = "n",
  cex = .9,
  pt.cex = 1
)
box()
dev.off()

pdf(file = "figure1-bottom.pdf")
effect <-
  c(
    pot.handling.control[1],
    pot.handling.legis[1],
    tax.handling.control[1],
    tax.handling.legis[1],
    defense.handling.control[1],
    defense.handling.legis[1]
  )
se <-
  c(
    pot.handling.control[2],
    pot.handling.legis[2],
    tax.handling.control[2],
    tax.handling.legis[2],
    defense.handling.control[2],
    defense.handling.legis[2]
  )
group <- c(6.15, 6,  4.15, 4, 2.15, 2)
plot(
  effect,
  group,
  xlim = c(-.3, .15),
  xaxt = "n",
  ylim = c(1, 6.75),
  pch = c(17, 19),
  tck = -.02,
  cex.axis = 0.9,
  cex = 1,
  ylab = "",
  yaxt = "n",
  xlab = "",
  axes = FALSE
)
abline(v = 0,
       lwd = .5,
       col = "gray",
       lty = 2)
segments(effect - se * 1.96, group, effect + 1.96 * se, group, lty = 1)
axis(1, at = c(-.3,-.2,-.1, 0, .1))#, labels = c("-0.60", "-0.40", "-0.20","0\n(No Difference\nfrom the control)", "0.20"), padj=1)#,tck=-.02)
par(mgp = c(2, .1, 0))
axis(
  2,
  at = c(6, 4, 2),
  las = 2,
  labels = c(
    "Legalize \nMarijuana",
    "Lower    \nCorporate \nTaxes    ",
    "Deploy   \n Troops   "
  ) ,
  tck = 0,
  lwd = 0,
  line = 0,
  cex.axis = .9
)
title(main = "Approval of Candidate's Handling of Issue",
      cex.main = 1,
      line = 1)
text(pot.handling.control[1],
     group[1] + .35,
     round(pot.handling.control[1], 2))
text(pot.handling.legis[1],
     group[2] - .35,
     round(pot.handling.legis[1], 2))
text(tax.handling.control[1],
     group[3] + .35,
     round(tax.handling.control[1], 2))
text(tax.handling.legis[1],
     group[4] - .35,
     round(tax.handling.legis[1], 2))
text(defense.handling.control[1],
     group[5] + .35,
     round(defense.handling.control[1], 2))
text(defense.handling.legis[1],
     group[6] - .35,
     round(defense.handling.legis[1], 2))
mtext("Effect Size", 1, line = 2.5)
legend(
  .02,
  7,
  c("Unilateral -\n Control\n", "Unilateral -\n Legislative"),
  pch = c(17, 19),
  lty = 1,
  bty = "n",
  cex = .9,
  pt.cex = 1
)
box()
dev.off()