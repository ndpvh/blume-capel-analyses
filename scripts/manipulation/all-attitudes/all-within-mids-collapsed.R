################################################################################
# PURPOSE: Perform analyses targeted at understanding whether the 
# manipulations worked. All attitude items for both topics. Full sample.
################################################################################


############

# This script provides the basis of all subsequent analyses that examine whether
# the effects of the study manipulations produced changes in midpoint endorsement
# rates. This is done both by between-factor analyses (i.e., testing binary
# variables that denote changes between T1 and T2) and within-factor analyses.
#
# This script focuses on the within-factor analysis across the full sample for all
# attitude items, across both topics.

############

# Assuming you start from ~/blume-capel-analyses/

# ignore the NAs introduction warning from the preprocessing script
# it is a lie, I checked!

library("haven")
library("tidyverse")
library(lme4)
library(clipr)

source("./scripts/preprocessing.R")
source("./scripts/lazy functions v2.R")

############

# Interaction time and condition. Midpoint = binary coding of whether
# a midpoint was chosen or not.


all_mid_change <- glmer(midpoint ~ time*Condition + (1|pseudo),
                      data = all_changes_long,
                      family = binomial,
                      control = glmerControl(optimizer = "bobyqa")
                      )

##Due to convergence warnings (and something clearly being funky), should to look 
## into item-level variance. Further analyzed in respective Econ and Meat analyses.

all_mid_change2 <- glmer(midpoint ~ time*Condition + (1|pseudo) + (1|item),
                        data = all_changes_long,
                        family = binomial)

global_changes2 <- summary(all_mid_change2)

############

#Interaction time, Condition, Importance, and Knowledge

############

all_mid_change3 <- glmer(midpoint ~ time*Condition*scale(Importance)*scale(Knowledge) + (1|pseudo) + (1|item),
                          data = all_changes_long,
                          family = binomial,
                          control = glmerControl(optimizer = "bobyqa"))

global_changes3 <- summary(all_mid_change3)

############

#Interaction time, Condition, Importance

############

all_mid_change4 <- glmer(midpoint ~ time*Condition*scale(Importance) + (1|pseudo) + (1|item),
                          data = all_changes_long,
                          family = binomial,
                          control = glmerControl(optimizer = "bobyqa"))

global_changes4 <- summary(all_mid_change4)


############

#Interaction time, Condition, Knowledge

############

all_mid_change5 <- glmer(midpoint ~ time*Condition*scale(Knowledge) + (1|pseudo) + (1|item),
                          data = all_changes_long,
                          family = binomial,
                          control = glmerControl(optimizer = "bobyqa"))

global_changes5 <- summary(all_mid_change5)

gc6 <- glmer(midpoint ~ time*scale(Importance) + (1|pseudo) + (1|item),
             data = all_changes_long,
             family = binomial)

global_changes6 <- summary(gc6)

gc7 <- glmer(midpoint ~ time*scale(Knowledge) + (1|pseudo) + (1|item),
      data = all_changes_long,
      family = binomial)

global_changes7 <- summary(gc7)

AIC(all_mid_change2, all_mid_change3, all_mid_change4, all_mid_change5, gc6, gc7)

glbl_chg_aov2 <-car::Anova(all_mid_change2)
glbl_chg_aov3 <-car::Anova(all_mid_change3)
glbl_chg_aov4 <-car::Anova(all_mid_change4)
glbl_chg_aov5 <-car::Anova(all_mid_change5)
glbl_chg_aov6 <-car::Anova(gc6)
glbl_chg_aov7 <-car::Anova(gc7)

all_changes_sig_coefs <- sig_coefs(
  global_changes2,
  glbl_chg_aov2,
  global_changes3,
  glbl_chg_aov3,
  global_changes4,
  glbl_chg_aov4,
  global_changes5,
  glbl_chg_aov5,
  global_changes6,
  glbl_chg_aov6,
  global_changes7,
  glbl_chg_aov7,
  p = 0.10
)

write_clip(all_changes_sig_coefs)
