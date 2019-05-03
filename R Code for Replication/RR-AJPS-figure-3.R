## Replication files for The Cost of Unilateral Action
## Andrew Reeves and Jon C. Rogowski
## September 6, 2017
## Creates Figure 3


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

###############################################################################################
## Figure 3: Unilateral Action and Perception of the Candidate's Commitment to the Rule of Law
###############################################################################################
## Legalize marijuana Unilateral-Control
pot.rol.control <-
  wtd.t.test(pot.rol.binary[treatment == "unilateral"],
             pot.rol.binary[treatment == "control"],
             oct2015wt1[treatment == "unilateral"],
             oct2015wt1[treatment == "control"],
             samedata = F)$additional[c("Difference", "Std. Err")]
pot.rol.control
## Legalize marijuana Unilateral-Legislative
pot.rol.legis <-
  wtd.t.test(pot.rol.binary[treatment == "unilateral"],
             pot.rol.binary[treatment == "bilateral"],
             oct2015wt1[treatment == "unilateral"],
             oct2015wt1[treatment == "bilateral"],
             samedata = F)$additional[c("Difference", "Std. Err")]
pot.rol.legis
## Lower corporate taxes Unilateral-Control
tax.rol.control <-
  wtd.t.test(tax.rol.binary[treatment == "unilateral"],
             tax.rol.binary[treatment == "control"],
             oct2015wt1[treatment == "unilateral"],
             oct2015wt1[treatment == "control"],
             samedata = F)$additional[c("Difference", "Std. Err")]
tax.rol.control
## Lower corporate taxes Unilateral-Legislative
tax.rol.legis <-
  wtd.t.test(tax.rol.binary[treatment == "unilateral"],
             tax.rol.binary[treatment == "bilateral"],
             oct2015wt1[treatment == "unilateral"],
             oct2015wt1[treatment == "bilateral"],
             samedata = F)$additional[c("Difference", "Std. Err")]
tax.rol.legis
## Deploy troops Unilateral-Control
defense.rol.control <-
  wtd.t.test(defense.rol.binary[treatment == "unilateral"],
             defense.rol.binary[treatment == "control"],
             oct2015wt1[treatment == "unilateral"],
             oct2015wt1[treatment == "control"],
             samedata = F)$additional[c("Difference", "Std. Err")]
defense.rol.control
## Deploy troops Unilateral-Legislative
defense.rol.legis <-
  wtd.t.test(defense.rol.binary[treatment == "unilateral"],
             defense.rol.binary[treatment == "bilateral"],
             oct2015wt1[treatment == "unilateral"],
             oct2015wt1[treatment == "bilateral"],
             samedata = F)$additional[c("Difference", "Std. Err")]
defense.rol.legis

effect <-
  c(
    pot.rol.control[1],
    pot.rol.legis[1],
    tax.rol.control[1],
    tax.rol.legis[1],
    defense.rol.control[1],
    defense.rol.legis[1]
  )
se <-
  c(
    pot.rol.control[2],
    pot.rol.legis[2],
    tax.rol.control[2],
    tax.rol.legis[2],
    defense.rol.control[2],
    defense.rol.legis[2]
  )
group <- c(6.15, 6,  4.15, 4, 2.15, 2)
par(mgp = c(2, .5, 0))
pdf(file = "figure3.pdf")
plot(
  effect,
  group,
  xlim = c(-.4, .15),
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
axis(1, at = c(-.4,-.3,-.2,-.1, 0, .1))#, labels = c("-0.60", "-0.40", "-0.20","0\n(No Difference\nfrom the control)", "0.20"), padj=1)#,tck=-.02)
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

text(pot.rol.control[1], group[1] + .35, round(pot.rol.control[1], 2))
text(pot.rol.legis[1], group[2] - .35, round(pot.rol.legis[1], 2))
text(tax.rol.control[1], group[3] + .35, round(tax.rol.control[1], 2))
text(tax.rol.legis[1], group[4] - .35, round(tax.rol.legis[1], 2))
text(defense.rol.control[1],
     group[5] + .35,
     round(defense.rol.control[1], 2))
text(defense.rol.legis[1], group[6] - .35, round(defense.rol.legis[1], 2))
mtext("Effect Size", 1, line = 2.5)
legend(
  -.005,
  6.5,
  c("Unilateral -\n Control\n", "Unilateral -\n Legislative"),
  pch = c(17, 19),
  lty = 1,
  bty = "n",
  cex = .9,
  pt.cex = 1
)
box()
dev.off()
