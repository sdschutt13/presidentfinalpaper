## Replication files for The Cost of Unilateral Action
## Andrew Reeves and Jon C. Rogowski
## September 6, 2017
## Creates Figure 2


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

#####################################################################
## Figure 2: Unilateral Action and Presidential Candidate Evaluations.
#####################################################################
## Top Panel
## Support is Strong Leader
## Legalize marijuana Unilateral-Control
pot.leadership.control <- wtd.t.test(
  pot.leadership.binary[treatment == "unilateral"],
  pot.leadership.binary[treatment == "control"],
  oct2015wt1[treatment == "unilateral"],
  oct2015wt1[treatment == "control"],
  samedata = F
)$additional[c("Difference", "Std. Err")]
pot.leadership.control
## Legalize marijuana Unilateral-Legislative
pot.leadership.legis <- wtd.t.test(
  pot.leadership.binary[treatment == "unilateral"],
  pot.leadership.binary[treatment == "bilateral"],
  oct2015wt1[treatment == "unilateral"],
  oct2015wt1[treatment == "bilateral"],
  samedata = F
)$additional[c("Difference", "Std. Err")]
pot.leadership.legis
## Lower corporate taxes Unilateral-Control
tax.leadership.control <- wtd.t.test(
  tax.leadership.binary[treatment == "unilateral"],
  tax.leadership.binary[treatment == "control"],
  oct2015wt1[treatment == "unilateral"],
  oct2015wt1[treatment == "control"],
  samedata = F
)$additional[c("Difference", "Std. Err")]
tax.leadership.control
## Lower corporate taxes Unilateral-Legislative
tax.leadership.legis <- wtd.t.test(
  tax.leadership.binary[treatment == "unilateral"],
  tax.leadership.binary[treatment == "bilateral"],
  oct2015wt1[treatment == "unilateral"],
  oct2015wt1[treatment == "bilateral"],
  samedata = F
)$additional[c("Difference", "Std. Err")]
tax.leadership.legis
## Deploy troops Unilateral-Control
defense.leadership.control <- wtd.t.test(
  defense.leadership.binary[treatment == "unilateral"],
  defense.leadership.binary[treatment == "control"],
  oct2015wt1[treatment == "unilateral"],
  oct2015wt1[treatment == "control"],
  samedata = F
)$additional[c("Difference", "Std. Err")]
defense.leadership.control
## Deploy troops Unilateral-Legislative
defense.leadership.legis <- wtd.t.test(
  defense.leadership.binary[treatment == "unilateral"],
  defense.leadership.binary[treatment == "bilateral"],
  oct2015wt1[treatment == "unilateral"],
  oct2015wt1[treatment == "bilateral"],
  samedata = F
)$additional[c("Difference", "Std. Err")]
defense.leadership.legis
## Bottom Panel
## Candidate Gets Things Done
## Legalize marijuana Unilateral-Control
pot.gtd.control <-
  wtd.t.test(pot.gtd.binary[treatment == "unilateral"],
             pot.gtd.binary[treatment == "control"],
             oct2015wt1[treatment == "unilateral"],
             oct2015wt1[treatment == "control"],
             samedata = F)$additional[c("Difference", "Std. Err")]
pot.gtd.control
## Legalize marijuana Unilateral-Legislative
pot.gtd.legis <-
  wtd.t.test(pot.gtd.binary[treatment == "unilateral"],
             pot.gtd.binary[treatment == "bilateral"],
             oct2015wt1[treatment == "unilateral"],
             oct2015wt1[treatment == "bilateral"],
             samedata = F)$additional[c("Difference", "Std. Err")]
pot.gtd.legis
## Lower corporate taxes Unilateral-Control
tax.gtd.control <-
  wtd.t.test(tax.gtd.binary[treatment == "unilateral"],
             tax.gtd.binary[treatment == "control"],
             oct2015wt1[treatment == "unilateral"],
             oct2015wt1[treatment == "control"],
             samedata = F)$additional[c("Difference", "Std. Err")]
tax.gtd.control
## Lower corporate taxes Unilateral-Legislative
tax.gtd.legis <-
  wtd.t.test(tax.gtd.binary[treatment == "unilateral"],
             tax.gtd.binary[treatment == "bilateral"],
             oct2015wt1[treatment == "unilateral"],
             oct2015wt1[treatment == "bilateral"],
             samedata = F)$additional[c("Difference", "Std. Err")]
tax.gtd.legis
## Deploy troops Unilateral-Control
defense.gtd.control <-
  wtd.t.test(defense.gtd.binary[treatment == "unilateral"],
             defense.gtd.binary[treatment == "control"],
             oct2015wt1[treatment == "unilateral"],
             oct2015wt1[treatment == "control"],
             samedata = F)$additional[c("Difference", "Std. Err")]
defense.gtd.control
## Deploy troops Unilateral-Legislative
defense.gtd.legis <-
  wtd.t.test(defense.gtd.binary[treatment == "unilateral"],
             defense.gtd.binary[treatment == "bilateral"],
             oct2015wt1[treatment == "unilateral"],
             oct2015wt1[treatment == "bilateral"],
             samedata = F)$additional[c("Difference", "Std. Err")]
defense.gtd.legis






effect <-
  c(
    pot.leadership.control[1],
    pot.leadership.legis[1],
    tax.leadership.control[1],
    tax.leadership.legis[1],
    defense.leadership.control[1],
    defense.leadership.legis[1]
  )
se <-
  c(
    pot.leadership.control[2],
    pot.leadership.legis[2],
    tax.leadership.control[2],
    tax.leadership.legis[2],
    defense.leadership.control[2],
    defense.leadership.legis[2]
  )
group <- c(6.15, 6,  4.15, 4, 2.15, 2)
pdf(file = "figure2-top.pdf")
par(mgp = c(2, .5, 0))
plot(
  effect,
  group,
  xlim = c(-.2, .15),
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
axis(1, at = c(-.2,-.1, 0, .1))#, labels = c("-0.60", "-0.40", "-0.20","0\n(No Difference\nfrom the control)", "0.20"), padj=1)#,tck=-.02)
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
title(main = "Candidate is Strong Leader",
      cex.main = 1,
      line = 1)
text(pot.leadership.control[1],
     group[1] + .35,
     round(pot.leadership.control[1], 2))
text(pot.leadership.legis[1],
     group[2] - .35,
     round(pot.leadership.legis[1], 2))
text(tax.leadership.control[1],
     group[3] + .35,
     round(tax.leadership.control[1], 2))
text(tax.leadership.legis[1],
     group[4] - .35,
     round(tax.leadership.legis[1], 2))
text(defense.leadership.control[1],
     group[5] + .35,
     round(defense.leadership.control[1], 2))
text(defense.leadership.legis[1],
     group[6] - .35,
     round(defense.leadership.legis[1], 2))
mtext("Effect Size", 1, line = 2.5)
legend(
  .05,
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

pdf(file = "figure2-bottom.pdf")
effect <-
  c(
    pot.gtd.control[1],
    pot.gtd.legis[1],
    tax.gtd.control[1],
    tax.gtd.legis[1],
    defense.gtd.control[1],
    defense.gtd.legis[1]
  )
se <-
  c(
    pot.gtd.control[2],
    pot.gtd.legis[2],
    tax.gtd.control[2],
    tax.gtd.legis[2],
    defense.gtd.control[2],
    defense.gtd.legis[2]
  )
group <- c(6.15, 6,  4.15, 4, 2.15, 2)
par(mgp = c(2, .5, 0))
plot(
  effect,
  group,
  xlim = c(-.2, .15),
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
title(main = "Candidate Gets Things Done",
      cex.main = 1,
      line = 1)
text(pot.gtd.control[1], group[1] + .35, round(pot.gtd.control[1], 2))
text(pot.gtd.legis[1], group[2] - .35, round(pot.gtd.legis[1], 2))
text(tax.gtd.control[1], group[3] + .35, round(tax.gtd.control[1], 2))
text(tax.gtd.legis[1], group[4] - .35, round(tax.gtd.legis[1], 2))
text(defense.gtd.control[1],
     group[5] + .35,
     round(defense.gtd.control[1], 2))
text(defense.gtd.legis[1], group[6] - .35, round(defense.gtd.legis[1], 2))
mtext("Effect Size", 1, line = 2.5)
legend(
  .05,
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