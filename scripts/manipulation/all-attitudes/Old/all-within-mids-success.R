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



# Interaction time and condition. Midpoint = binary coding of whether
# a midpoint was chosen or not.


all_success_change <- glmer(midpoint ~ time*Condition + (1|pseudo),
                        data = all_changes_long,
                        family = binomial,
                        control = glmerControl(optimizer = "bobyqa")
)

##Due to convergence warnings (and something clearly being funky), should to look 
## into item-level variance. Further analyzed in respective Econ and Meat analyses.

all_success_change2 <- glmer(midpoint ~ time*Condition + (1|pseudo) + (1|item),
                         data = all_changes_long,
                         family = binomial)

global_changes2 <- summary(all_success_change2)

############

#Interaction time, Condition, Importance, and Knowledge

############

all_success_change3 <- glmer(midpoint ~ time*Condition*scale(Importance)*scale(Knowledge) + (1|pseudo) + (1|item),
                         data = all_changes_long,
                         family = binomial,
                         control = glmerControl(optimizer = "bobyqa"))

global_changes3 <- summary(all_success_change3)

############

#Interaction time, Condition, Importance

############

all_success_change4 <- glmer(midpoint ~ time*Condition*scale(Importance) + (1|pseudo) + (1|item),
                         data = all_changes_long,
                         family = binomial,
                         control = glmerControl(optimizer = "bobyqa"))

global_changes4 <- summary(all_success_change4)


############

# Interaction time, Condition, Knowledge

############

all_success_change5 <- glmer(midpoint ~ time*Condition*scale(Knowledge) + (1|pseudo) + (1|item),
                         data = all_changes_long,
                         family = binomial,
                         control = glmerControl(optimizer = "bobyqa"))

global_changes5 <- summary(all_success_change5)

############

# Interaction time, Importance

############

all_success_change6 <- glmer(midpoint ~ time*scale(Importance) + (1|pseudo) + (1|item),
             data = all_changes_long,
             family = binomial)

global_changes6 <- summary(all_success_change6)

############

# Interaction time, Knowledge

############

all_success_change7 <- glmer(midpoint ~ time*scale(Knowledge) + (1|pseudo) + (1|item),
             data = all_changes_long,
             family = binomial)

global_changes7 <- summary(all_success_change7)

AIC(all_success_change2, all_success_change3, all_success_change4, all_success_change5, all_success_change6, all_success_change7)

glbl_chg_aov2 <-car::Anova(all_success_change2)
glbl_chg_aov3 <-car::Anova(all_success_change3)
glbl_chg_aov4 <-car::Anova(all_success_change4)
glbl_chg_aov5 <-car::Anova(all_success_change5)
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


############






all_success_diffs1 <- glmer(mid_success ~ Condition + pre_mid + (1|pseudo),
                        data = all_changes,
                        family = binomial,
                        control = glmerControl(optimizer = "bobyqa"))
global_success1 <- summary(all_success_diffs1)

all_success_diffs2 <- glmer(mid_success ~ Condition + pre_mid + (1|pseudo) + (1|var),
                        data = all_changes,
                        family = binomial,
                        control = glmerControl(optimizer = "bobyqa"))
global_success2 <- summary(all_success_diffs2)

# Random effects of var essentially 0, dropping

############
#Interaction Condition, Importance, and Knowledge
############
all_success_diffs3 <- glmer(mid_success ~ Condition*scale(Importance)*scale(Knowledge) + pre_mid + (1|pseudo),
                        data = all_changes,
                        family = binomial,
                        control = glmerControl(optimizer = "bobyqa"))
global_success3 <- summary(all_success_diffs3)

############
#Interaction Condition, Importance
############
all_success_diffs4 <- glmer(mid_success ~ Condition*scale(Importance) + pre_mid + (1|pseudo),
                        data = all_changes,
                        family = binomial,
                        control = glmerControl(optimizer = "bobyqa"))
global_success4 <- summary(all_success_diffs4)

############
# Interaction Condition, Knowledge
############
all_success_diffs5 <- glmer(mid_success ~ Condition*scale(Knowledge) + pre_mid + (1|pseudo),
                        data = all_changes,
                        family = binomial,
                        control = glmerControl(optimizer = "bobyqa"))
global_success5 <- summary(all_success_diffs5)

############
# Interaction Importance

# Note: random effects removed due to convergence issues
############
all_success_diffs6 <- glm(mid_success ~ scale(Importance) + pre_mid,
                        data = all_changes,
                        family = binomial)
global_success6 <- summary(all_success_diffs6)

############
# Interaction Knowledge
############
all_success_diffs7 <- glm(mid_success ~ scale(Knowledge) + pre_mid,
                        data = all_changes,
                        family = binomial)
global_success7 <- summary(all_success_diffs7)


AIC(all_success_diffs1, all_success_diffs2, all_success_diffs3, all_success_diffs4, all_success_diffs5, all_success_diffs6, all_success_diffs7)

glbl_succ_aov1 <-car::Anova(all_success_diffs1)
glbl_succ_aov2 <-car::Anova(all_success_diffs2)
glbl_succ_aov3 <-car::Anova(all_success_diffs3)
glbl_succ_aov4 <-car::Anova(all_success_diffs4)
glbl_succ_aov5 <-car::Anova(all_success_diffs5)
glbl_succ_aov6 <-car::Anova(all_success_diffs6)
glbl_succ_aov7 <-car::Anova(all_success_diffs7)

all_succ_sig_coefs <- sig_coefs(
  global_success1,
  glbl_succ_aov1,
  global_success2,
  glbl_succ_aov2,
  global_success3,
  glbl_succ_aov3,
  global_success4,
  glbl_succ_aov4,
  global_success5,
  glbl_succ_aov5,
  global_success6,
  glbl_succ_aov6,
  global_success7,
  glbl_succ_aov7,
  p = 0.10
)

write_clip(all_succ_sig_coefs)







all_success_ord1 <- clmm(as.factor(post_mid - pre_mid) ~ Condition + (1|pseudo), data = all_changes)
glb_ord_diffs1 <- summary(all_success_ord1)

# Random effects of var essentially 0, dropping
all_success_ord2 <- clmm(as.factor(post_mid - pre_mid) ~ Condition + (1|pseudo) +(1|var), data = all_changes)
glb_ord_diffs2 <- summary(all_success_ord2)

############
#Interaction Condition, Importance, and Knowledge
############

all_success_ord3 <- clmm(as.factor(post_mid - pre_mid) ~ Condition*scale(Importance)*scale(Knowledge) + (1|pseudo), data = all_changes)
glb_ord_diffs3 <- summary(all_success_ord3)

############
#Interaction Condition, Importance
############

all_success_ord4 <- clmm(as.factor(post_mid - pre_mid) ~ Condition*scale(Importance) + (1|pseudo), data = all_changes)
glb_ord_diffs4 <- summary(all_success_ord4)

############
# Interaction Condition, Knowledge
############
all_success_ord5 <- clmm(as.factor(post_mid - pre_mid) ~ Condition*scale(Knowledge) + (1|pseudo), data = all_changes)
glb_ord_diffs5 <- summary(all_success_ord5)

############
# Interaction Importance
############
all_success_ord6 <- clmm(as.factor(post_mid - pre_mid) ~ scale(Importance) + (1|pseudo), data = all_changes)
glb_ord_diffs6 <- summary(all_success_ord6)

############
# Interaction Knowledge
############
all_success_ord7 <- clmm(as.factor(post_mid - pre_mid) ~ scale(Knowledge) + (1|pseudo), data = all_changes)
glb_ord_diffs7 <- summary(all_success_ord7)

AIC(all_success_ord1, all_success_ord2, all_success_ord3, all_success_ord4, all_success_ord5, all_success_ord6, all_success_ord7)

glbl_ord_aov1 <- car::Anova(all_success_ord1)
glbl_ord_aov2 <- car::Anova(all_success_ord2)
glbl_ord_aov3 <- car::Anova(all_success_ord3)
glbl_ord_aov4 <- car::Anova(all_success_ord4)
glbl_ord_aov5 <- car::Anova(all_success_ord5)
glbl_ord_aov6 <- car::Anova(all_success_ord6)
glbl_ord_aov7 <- car::Anova(all_success_ord7)

all_ords_sig_coefs <- sig_coefs(
  glb_ord_diffs1, glbl_ord_aov1,
  glb_ord_diffs2, glbl_ord_aov2,
  glb_ord_diffs3, glbl_ord_aov3,
  glb_ord_diffs4, glbl_ord_aov4,
  glb_ord_diffs5, glbl_ord_aov5,
  glb_ord_diffs6, glbl_ord_aov6,
  glb_ord_diffs7, glbl_ord_aov7,
  p = 0.10
)
write_clip(all_ords_sig_coefs)




drop_ord1 <- drop1(all_success_ord1, test = "Chi")
drop_ord2 <- drop1(all_success_ord2, test = "Chi")
drop_ord3 <- drop1(all_success_ord3, test = "Chi")
drop_ord4 <- drop1(all_success_ord4, test = "Chi")
drop_ord5 <- drop1(all_success_ord5, test = "Chi")
drop_ord6 <- drop1(all_success_ord6, test = "Chi")
drop_ord7 <- drop1(all_success_ord7, test = "Chi")
