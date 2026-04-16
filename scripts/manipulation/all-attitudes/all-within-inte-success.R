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
source("./scripts/lazy-functions-v2.R")



############


############






all_inte_suc_diffs1 <- glmer(inte_success ~ Condition + (1|pseudo),
                            data = all_changes,
                            family = binomial,
                            control = glmerControl(optimizer = "bobyqa"))
glbl_inte_suc1 <- summary(all_inte_suc_diffs1)

all_inte_suc_diffs2 <- glmer(inte_success ~ Condition + (1|pseudo) + (1|var),
                            data = all_changes,
                            family = binomial,
                            control = glmerControl(optimizer = "bobyqa"))
glbl_inte_suc2 <- summary(all_inte_suc_diffs2)

# Random effects of var essentially 0, dropping

############
#Interaction Condition, Importance, and Knowledge
############
all_inte_suc_diffs3 <- glmer(inte_success ~ Condition*scale(Importance)*scale(Knowledge) + (1|pseudo),
                            data = all_changes,
                            family = binomial,
                            control = glmerControl(optimizer = "bobyqa"))
glbl_inte_suc3 <- summary(all_inte_suc_diffs3)

############
#Interaction Condition, Importance
############
all_inte_suc_diffs4 <- glmer(inte_success ~ Condition*scale(Importance) + (1|pseudo),
                            data = all_changes,
                            family = binomial,
                            control = glmerControl(optimizer = "bobyqa"))
glbl_inte_suc4 <- summary(all_inte_suc_diffs4)

############
# Interaction Condition, Knowledge
############
all_inte_suc_diffs5 <- glmer(inte_success ~ Condition*scale(Knowledge) + (1|pseudo),
                            data = all_changes,
                            family = binomial,
                            control = glmerControl(optimizer = "bobyqa"))
glbl_inte_suc5 <- summary(all_inte_suc_diffs5)

############
# Interaction Importance

# Note: random effects removed due to convergence issues
############
all_inte_suc_diffs6 <- glmer(inte_success ~ scale(Importance) + (1|pseudo),
                          data = all_changes,
                          family = binomial)
glbl_inte_suc6 <- summary(all_inte_suc_diffs6)

############
# Interaction Knowledge
############
all_inte_suc_diffs7 <- glmer(inte_success ~ scale(Knowledge) + (1|pseudo),
                          data = all_changes,
                          family = binomial)
glbl_inte_suc7 <- summary(all_inte_suc_diffs7)


AIC(all_inte_suc_diffs1, all_inte_suc_diffs2, all_inte_suc_diffs3, all_inte_suc_diffs4, all_inte_suc_diffs5, all_inte_suc_diffs6, all_inte_suc_diffs7)

glb_inte_suc_aov1 <-car::Anova(all_inte_suc_diffs1)
glb_inte_suc_aov2 <-car::Anova(all_inte_suc_diffs2)
glb_inte_suc_aov3 <-car::Anova(all_inte_suc_diffs3)
glb_inte_suc_aov4 <-car::Anova(all_inte_suc_diffs4)
glb_inte_suc_aov5 <-car::Anova(all_inte_suc_diffs5)
glb_inte_suc_aov6 <-car::Anova(all_inte_suc_diffs6)
glb_inte_suc_aov7 <-car::Anova(all_inte_suc_diffs7)

all_suc_inte_sig_coefs <- sig_coefs(
  glbl_inte_suc1,
  glb_inte_suc_aov1,
  glbl_inte_suc2,
  glb_inte_suc_aov2,
  glbl_inte_suc3,
  glb_inte_suc_aov3,
  glbl_inte_suc4,
  glb_inte_suc_aov4,
  glbl_inte_suc5,
  glb_inte_suc_aov5,
  glbl_inte_suc6,
  glb_inte_suc_aov6,
  glbl_inte_suc7,
  glb_inte_suc_aov7,
  p = 0.10
)

write_clip(all_suc_inte_sig_coefs)



########################

# Section 3 

# Did people who changed (using the midpoint as the reference) between T1 and T2 
# do so at different rates based on condition? 
# (subsample: only those who changed between T1 and T2)

########################

##########

# As a between effect:

##########

inte_direction1 <- glmer(inte_success ~ Condition + (1|pseudo),
                          family = binomial,
                          data = all_changers)

inte_change1 <- summary(inte_direction1)

# random effects virtually 0. dropping

inte_direction2 <- glmer(inte_success ~ Condition + (1|pseudo) + (1|var),
                         family = binomial,
                         data = all_changers)

inte_change2 <- summary(inte_direction2)

inte_direction3 <- glmer(inte_success ~ Condition*scale(Knowledge)*scale(Importance)+ (1|pseudo),
                        family = binomial,
                        data = all_changers,
                        control = glmerControl(optimizer = "bobyqa"))

inte_change3 <- summary(inte_direction3)

inte_direction4 <- glmer(inte_success ~ Condition*scale(Importance) + (1|pseudo),
                        family = binomial,
                        data = all_changers)

inte_change4 <- summary(inte_direction4)

inte_direction5 <- glm(inte_success ~ Condition*scale(Knowledge),
                        family = binomial,
                        data = all_changers)

inte_change5 <- summary(inte_direction5)

inte_direction6 <- glm(inte_success ~ scale(Importance),
                       family = binomial,
                       data = all_changers)

inte_change6 <- summary(inte_direction6)

inte_direction7 <- glm(inte_success ~ scale(Knowledge),
                       family = binomial,
                       data = all_changers)

inte_change7 <- summary(inte_direction7)

AIC(
  inte_direction1,
  inte_direction2,
  inte_direction3,
  inte_direction4,
  inte_direction5,
  inte_direction6,
  inte_direction7
)

aov_inteChange1 <- car::Anova(inte_direction1)
aov_inteChange2 <- car::Anova(inte_direction2)
aov_inteChange3 <- car::Anova(inte_direction3)
aov_inteChange4 <- car::Anova(inte_direction4)
aov_inteChange5 <- car::Anova(inte_direction5)
aov_inteChange6 <- car::Anova(inte_direction6)
aov_inteChange7 <- car::Anova(inte_direction7)

inte_change_coefs <- sig_coefs(
  inte_change1,
  aov_inteChange1,
  inte_change2,
  aov_inteChange2,
  inte_change3,
  aov_inteChange3,
  inte_change4,
  aov_inteChange4,
  inte_change5,
  aov_inteChange5,
  inte_direction6,
  aov_inteChange6,
  inte_direction7,
  aov_inteChange7,
  p = 0.10
)

write_clip(con_change_coefs)
