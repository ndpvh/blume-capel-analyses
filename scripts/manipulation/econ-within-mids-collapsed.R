################################################################################
# PURPOSE: Perform analyses targeted at understanding whether the 
# manipulations worked. Economic items only. Full sample.
################################################################################


############

# This script provides the basis of all subsequent analyses that examine whether
# the effects of the study manipulations produced changes in midpoint endorsement
# rates. This is done both by between-factor analyses (i.e., testing binary
# variables that denote changes between T1 and T2) and within-factor analyses.
#
# This script focuses on the within-factor analysis across the full sample for 
# econ attitude items.

############

# Assuming you start from ~/blume-capel-analyses/

# ignore the NAs introduction warning from the preprocessing script
# it is a lie, I checked!

library("haven")
library("tidyverse")
library(lme4)
library(clipr)

source("./scripts/preprocessing.R")
source("./scripts/lazy-functions-v2.R")



############



# Interaction time and condition. Midpoint = binary coding of whether
# a midpoint was chosen or not.


econ_mid_change <- glmer(midpoint ~ time*Condition + (1|pseudo),
                        data = econ_changes_long,
                        family = binomial)

econ_changes <- summary(econ_mid_change)


econ_mid_change2 <- glmer(midpoint ~ time*Condition + (1|pseudo) + (1|item),
                         data = econ_changes_long,
                         family = binomial,
                         control = glmerControl(optimizer = "bobyqa"))

econ_changes2 <- summary(econ_mid_change2)

############

#Interaction time, Condition, Importance, and Knowledge

############

econ_mid_change3 <- glmer(midpoint ~ time*Condition*scale(Importance)*scale(Knowledge) + (1|pseudo),
                         data = econ_changes_long,
                         family = binomial)

econ_changes3 <- summary(econ_mid_change3)

############

#Interaction time, Condition, Importance

############

econ_mid_change4 <- glmer(midpoint ~ time*Condition*scale(Importance) + (1|pseudo),
                         data = econ_changes_long,
                         family = binomial)

econ_changes4 <- summary(econ_mid_change4)


############

# Interaction time, Condition, Knowledge

############

econ_mid_change5 <- glmer(midpoint ~ time*Condition*scale(Knowledge) + (1|pseudo),
                         data = econ_changes_long,
                         family = binomial)

econ_changes5 <- summary(econ_mid_change5)

############

# Interaction time, Importance

############

econ_mid_change6 <- glmer(midpoint ~ time*scale(Importance) + (1|pseudo),
             data = econ_changes_long,
             family = binomial)

econ_changes6 <- summary(econ_mid_change6)

############

# Interaction time, Knowledge

############

econ_mid_change7 <- glmer(midpoint ~ time*scale(Knowledge) + (1|pseudo),
             data = econ_changes_long,
             family = binomial)

econ_changes7 <- summary(econ_mid_change7)

AIC(
  econ_mid_change,
  econ_mid_change2,
  econ_mid_change3,
  econ_mid_change4,
  econ_mid_change5,
  econ_mid_change6,
  econ_mid_change7
)

econ_chg_aov <-car::Anova(econ_mid_change)
econ_chg_aov2 <-car::Anova(econ_mid_change2)
econ_chg_aov3 <-car::Anova(econ_mid_change3)
econ_chg_aov4 <-car::Anova(econ_mid_change4)
econ_chg_aov5 <-car::Anova(econ_mid_change5)
econ_chg_aov6 <-car::Anova(econ_mid_change6)
econ_chg_aov7 <-car::Anova(econ_mid_change7)

econ_changes_sig_coefs <- sig_coefs(
  econ_changes,
  econ_chg_aov,
  econ_changes2,
  econ_chg_aov2,
  econ_changes3,
  econ_chg_aov3,
  econ_changes4,
  econ_chg_aov4,
  econ_changes5,
  econ_chg_aov5,
  econ_changes6,
  econ_chg_aov6,
  econ_changes7,
  econ_chg_aov7,
  p = 0.15
)

write_clip(econ_changes_sig_coefs)


############

# As a difference model

############


econ_mid_diffs1 <- glmer(post_mid ~ Condition + pre_mid + (1|pseudo),
                         data = econ_changes,
                         family = binomial)
econ_diffs1 <- summary(econ_mid_diffs1)

econ_mid_diffs2 <- glmer(post_mid ~ Condition + pre_mid + (1|pseudo) + (1|var),
                       data = econ_changes,
                       family = binomial)
econ_diffs2 <- summary(econ_mid_diffs2)

# Random effects of var essentially 0, dropping

############
#Interaction Condition, Importance, and Knowledge
############
econ_mid_diffs3 <- glmer(post_mid ~ Condition*scale(Importance)*scale(Knowledge) + pre_mid + (1|pseudo),
                       data = econ_changes,
                       family = binomial,
                       control = glmerControl(optimizer = "bobyqa"))
econ_diffs3 <- summary(econ_mid_diffs3)

############
#Interaction Condition, Importance
############
econ_mid_diffs4 <- glmer(post_mid ~ Condition*scale(Importance) + pre_mid + (1|pseudo),
                       data = econ_changes,
                       family = binomial)
econ_diffs4 <- summary(econ_mid_diffs4)

############
# Interaction Condition, Knowledge
############
econ_mid_diffs5 <- glmer(post_mid ~ Condition*scale(Knowledge) + pre_mid + (1|pseudo),
                       data = econ_changes,
                       family = binomial)
econ_diffs5 <- summary(econ_mid_diffs5)

############
# Interaction Importance
############
econ_mid_diffs6 <- glmer(post_mid ~ scale(Importance) + pre_mid + (1|pseudo),
                       data = econ_changes,
                       family = binomial)
econ_diffs6 <- summary(econ_mid_diffs6)

############
# Interaction Knowledge
############
econ_mid_diffs7 <- glmer(post_mid ~ scale(Knowledge) + pre_mid + (1|pseudo),
                       data = econ_changes,
                       family = binomial)
econ_diffs7 <- summary(econ_mid_diffs7)


AIC(econ_mid_diffs1, econ_mid_diffs2, econ_mid_diffs3, econ_mid_diffs4, econ_mid_diffs5, econ_mid_diffs6, econ_mid_diffs7)

econ_diff_aov1 <-car::Anova(econ_mid_diffs1)
econ_diff_aov2 <-car::Anova(econ_mid_diffs2)
econ_diff_aov3 <-car::Anova(econ_mid_diffs3)
econ_diff_aov4 <-car::Anova(econ_mid_diffs4)
econ_diff_aov5 <-car::Anova(econ_mid_diffs5)
econ_diff_aov6 <-car::Anova(econ_mid_diffs6)
econ_diff_aov7 <-car::Anova(econ_mid_diffs7)

econ_diffs_sig_coefs <- sig_coefs(
  econ_diffs1,
  econ_diff_aov1,
  econ_diffs2,
  econ_diff_aov2,
  econ_diffs3,
  econ_diff_aov3,
  econ_diffs4,
  econ_diff_aov4,
  econ_diffs5,
  econ_diff_aov5,
  econ_diffs6,
  econ_diff_aov6,
  econ_diffs7,
  econ_diff_aov7,
  p = 0.15
)

write_clip(econ_diffs_sig_coefs)
