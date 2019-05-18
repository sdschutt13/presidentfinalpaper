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

