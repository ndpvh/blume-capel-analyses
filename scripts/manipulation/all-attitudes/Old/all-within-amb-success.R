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



# ambraction time and condition. Midpoint = binary coding of whether
# a midpoint was chosen or not.


all_amb_suc_change <- glmer(midpoint ~ time*Condition + (1|pseudo),
                             data = all_changes_long,
                             family = binomial,
                             control = glmerControl(optimizer = "bobyqa")
)

##Due to convergence warnings (and something clearly being funky), should to look 
## into item-level variance. Further analyzed in respective Econ and Meat analyses.

all_amb_suc_change2 <- glmer(midpoint ~ time*Condition + (1|pseudo) + (1|item),
                              data = all_changes_long,
                              family = binomial)

global_changes2 <- summary(all_amb_suc_change2)

############

#ambraction time, Condition, Importance, and Knowledge

############

all_amb_suc_change3 <- glmer(midpoint ~ time*Condition*scale(Importance)*scale(Knowledge) + (1|pseudo) + (1|item),
                              data = all_changes_long,
                              family = binomial,
                              control = glmerControl(optimizer = "bobyqa"))

global_changes3 <- summary(all_amb_suc_change3)

############

#ambraction time, Condition, Importance

############

all_amb_suc_change4 <- glmer(midpoint ~ time*Condition*scale(Importance) + (1|pseudo) + (1|item),
                              data = all_changes_long,
                              family = binomial,
                              control = glmerControl(optimizer = "bobyqa"))

global_changes4 <- summary(all_amb_suc_change4)


############

# ambraction time, Condition, Knowledge

############

all_amb_suc_change5 <- glmer(midpoint ~ time*Condition*scale(Knowledge) + (1|pseudo) + (1|item),
                              data = all_changes_long,
                              family = binomial,
                              control = glmerControl(optimizer = "bobyqa"))

global_changes5 <- summary(all_amb_suc_change5)

############

# ambraction time, Importance

############

gc6 <- glmer(midpoint ~ time*scale(Importance) + (1|pseudo) + (1|item),
             data = all_changes_long,
             family = binomial)

global_changes6 <- summary(gc6)

############

# ambraction time, Knowledge

############

gc7 <- glmer(midpoint ~ time*scale(Knowledge) + (1|pseudo) + (1|item),
             data = all_changes_long,
             family = binomial)

global_changes7 <- summary(gc7)

AIC(all_amb_suc_change2, all_amb_suc_change3, all_amb_suc_change4, all_amb_suc_change5, gc6, gc7)

glbl_chg_aov2 <-car::Anova(all_amb_suc_change2)
glbl_chg_aov3 <-car::Anova(all_amb_suc_change3)
glbl_chg_aov4 <-car::Anova(all_amb_suc_change4)
glbl_chg_aov5 <-car::Anova(all_amb_suc_change5)
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






all_amb_suc_diffs1 <- glmer(amb_success ~ Condition + pre_amb + (1|pseudo),
                             data = all_changes,
                             family = binomial,
                             control = glmerControl(optimizer = "bobyqa"))
glbl_amb_succ1 <- summary(all_amb_suc_diffs1)

all_amb_suc_diffs2 <- glmer(amb_success ~ Condition + pre_amb + (1|pseudo) + (1|var),
                             data = all_changes,
                             family = binomial,
                             control = glmerControl(optimizer = "bobyqa"))
glbl_amb_succ2 <- summary(all_amb_suc_diffs2)

# Random effects of var essentially 0, but it allows convergence?

############
#ambraction Condition, Importance, and Knowledge
############
all_amb_suc_diffs3 <- glmer(amb_success ~ Condition*scale(Importance)*scale(Knowledge) + pre_amb + (1|pseudo)+(1|var),
                             data = all_changes,
                             family = binomial,
                             control = glmerControl(optimizer = "bobyqa"))
glbl_amb_succ3 <- summary(all_amb_suc_diffs3)

############
#ambraction Condition, Importance
############
all_amb_suc_diffs4 <- glmer(amb_success ~ Condition*scale(Importance) + pre_amb + (1|pseudo),
                             data = all_changes,
                             family = binomial,
                             control = glmerControl(optimizer = "bobyqa"))
glbl_amb_succ4 <- summary(all_amb_suc_diffs4)

############
# ambraction Condition, Knowledge

#
############
all_amb_suc_diffs5 <- glmer(amb_success ~ Condition*scale(Knowledge) + pre_amb + (1|pseudo),
                             data = all_changes,
                             family = binomial,
                             control = glmerControl(optimizer = "bobyqa"))
glbl_amb_succ5 <- summary(all_amb_suc_diffs5)

############
# ambraction Importance

# Note: random effects removed due to convergence issues
############
all_amb_suc_diffs6 <- glm(amb_success ~ scale(Importance) + pre_amb,
                           data = all_changes,
                           family = binomial)
glbl_amb_succ6 <- summary(all_amb_suc_diffs6)

############
# ambraction Knowledge
############
all_amb_suc_diffs7 <- glm(amb_success ~ scale(Knowledge) + pre_amb,
                           data = all_changes,
                           family = binomial)
glbl_amb_succ7 <- summary(all_amb_suc_diffs7)


AIC(all_amb_suc_diffs1, all_amb_suc_diffs2, all_amb_suc_diffs3, all_amb_suc_diffs4, all_amb_suc_diffs5, all_amb_suc_diffs6, all_amb_suc_diffs7)

glb_amb_suc_aov1 <-car::Anova(all_amb_suc_diffs1)
glb_amb_suc_aov2 <-car::Anova(all_amb_suc_diffs2)
glb_amb_suc_aov3 <-car::Anova(all_amb_suc_diffs3)
glb_amb_suc_aov4 <-car::Anova(all_amb_suc_diffs4)
glb_amb_suc_aov5 <-car::Anova(all_amb_suc_diffs5)
glb_amb_suc_aov6 <-car::Anova(all_amb_suc_diffs6)
glb_amb_suc_aov7 <-car::Anova(all_amb_suc_diffs7)

all_suc_amb_sig_coefs <- sig_coefs(
  glbl_amb_succ1,
  glb_amb_suc_aov1,
  glbl_amb_succ2,
  glb_amb_suc_aov2,
  glbl_amb_succ3,
  glb_amb_suc_aov3,
  glbl_amb_succ4,
  glb_amb_suc_aov4,
  glbl_amb_succ5,
  glb_amb_suc_aov5,
  glbl_amb_succ6,
  glb_amb_suc_aov6,
  glbl_amb_succ7,
  glb_amb_suc_aov7,
  p = 0.10
)

write_clip(all_suc_amb_sig_coefs)







all_amb_suc_ord1 <- clmm(as.factor(post_mid - pre_amb) ~ Condition + (1|pseudo), data = all_changes)
glb_ord_diffs1 <- summary(all_amb_suc_ord1)

# Random effects of var essentially 0, dropping
all_amb_suc_ord2 <- clmm(as.factor(post_mid - pre_amb) ~ Condition + (1|pseudo) +(1|var), data = all_changes)
glb_ord_diffs2 <- summary(all_amb_suc_ord2)

############
#ambraction Condition, Importance, and Knowledge
############

all_amb_suc_ord3 <- clmm(as.factor(post_mid - pre_amb) ~ Condition*scale(Importance)*scale(Knowledge) + (1|pseudo), data = all_changes)
glb_ord_diffs3 <- summary(all_amb_suc_ord3)

############
#ambraction Condition, Importance
############

all_amb_suc_ord4 <- clmm(as.factor(post_mid - pre_amb) ~ Condition*scale(Importance) + (1|pseudo), data = all_changes)
glb_ord_diffs4 <- summary(all_amb_suc_ord4)

############
# ambraction Condition, Knowledge
############
all_amb_suc_ord5 <- clmm(as.factor(post_mid - pre_amb) ~ Condition*scale(Knowledge) + (1|pseudo), data = all_changes)
glb_ord_diffs5 <- summary(all_amb_suc_ord5)

############
# ambraction Importance
############
all_amb_suc_ord6 <- clmm(as.factor(post_mid - pre_amb) ~ scale(Importance) + (1|pseudo), data = all_changes)
glb_ord_diffs6 <- summary(all_amb_suc_ord6)

############
# ambraction Knowledge
############
all_amb_suc_ord7 <- clmm(as.factor(post_mid - pre_amb) ~ scale(Knowledge) + (1|pseudo), data = all_changes)
glb_ord_diffs7 <- summary(all_amb_suc_ord7)

AIC(all_amb_suc_ord1, all_amb_suc_ord2, all_amb_suc_ord3, all_amb_suc_ord4, all_amb_suc_ord5, all_amb_suc_ord6, all_amb_suc_ord7)

glbl_ord_aov1 <- car::Anova(all_amb_suc_ord1)
glbl_ord_aov2 <- car::Anova(all_amb_suc_ord2)
glbl_ord_aov3 <- car::Anova(all_amb_suc_ord3)
glbl_ord_aov4 <- car::Anova(all_amb_suc_ord4)
glbl_ord_aov5 <- car::Anova(all_amb_suc_ord5)
glbl_ord_aov6 <- car::Anova(all_amb_suc_ord6)
glbl_ord_aov7 <- car::Anova(all_amb_suc_ord7)

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




drop_ord1 <- drop1(all_amb_suc_ord1, test = "Chi")
drop_ord2 <- drop1(all_amb_suc_ord2, test = "Chi")
drop_ord3 <- drop1(all_amb_suc_ord3, test = "Chi")
drop_ord4 <- drop1(all_amb_suc_ord4, test = "Chi")
drop_ord5 <- drop1(all_amb_suc_ord5, test = "Chi")
drop_ord6 <- drop1(all_amb_suc_ord6, test = "Chi")
drop_ord7 <- drop1(all_amb_suc_ord7, test = "Chi")
