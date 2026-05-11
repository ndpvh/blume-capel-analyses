################################################################################
# PURPOSE: Perform analyses targeted at understanding whether the 
# manipulations worked. Economic items only. Full sample.
################################################################################


############

# This script provides the basis of all subsequent analyses that examine whether
# the effects of the study manipulations produced changes in inte_bin endorsement
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



# Interaction time and condition. inte_bin = binary coding of whether
# a inte_bin was chosen or not.


all_inte_change1 <- glmer(inte_bin ~ time*Condition + (1|pseudo),
                        data = all_changes_long,
                        family = binomial
)

global_inte_changes1 <- summary(all_inte_change1)


##Due to convergence warnings (and something clearly being funky), should to look 
## into item-level variance. Further analyzed in respective Econ and Meat analyses.

all_inte_change2 <- glmer(inte_bin ~ time*Condition + (1|pseudo) + (1|item),
                         data = all_changes_long,
                         family = binomial)

global_inte_changes2 <- summary(all_inte_change2)

############

#Interaction time, Condition, Importance, and Knowledge

############

all_inte_change3 <- glmer(inte_bin ~ time*Condition*scale(Importance)*scale(Knowledge) + (1|pseudo) + (1|item),
                         data = all_changes_long,
                         family = binomial,
                         control = glmerControl(optimizer = "bobyqa"))

global_inte_changes3 <- summary(all_inte_change3)

all_inte_change3b <- glmer(inte_bin ~ time*Condition*scale(Importance)*scale(Knowledge) + (1|pseudo),
                          data = all_changes_long,
                          family = binomial)

global_inte_changes3b <- summary(all_inte_change3b)

############

#Interaction time, Condition, Importance

############

all_inte_change4 <- glmer(inte_bin ~ time*Condition*scale(Importance) + (1|pseudo) + (1|item),
                         data = all_changes_long,
                         family = binomial,
                         control = glmerControl(optimizer = "bobyqa"))

global_inte_changes4 <- summary(all_inte_change4)


############

# Interaction time, Condition, Knowledge

############

all_inte_change5 <- glmer(inte_bin ~ time*Condition*scale(Knowledge) + (1|pseudo) + (1|item),
                         data = all_changes_long,
                         family = binomial,
                         control = glmerControl(optimizer = "bobyqa"))

global_inte_changes5 <- summary(all_inte_change5)

############

# Interaction time, Importance

############

all_inte_change6 <- glmer(inte_bin ~ time*scale(Importance) + (1|pseudo) + (1|item),
             data = all_changes_long,
             family = binomial)

global_inte_changes6 <- summary(all_inte_change6)

############

# Interaction time, Knowledge

############

all_inte_change7 <- glmer(inte_bin ~ time*scale(Knowledge) + (1|pseudo) + (1|item),
             data = all_changes_long,
             family = binomial)

global_inte_changes7 <- summary(all_inte_change7)

AIC(all_inte_change1, all_inte_change2, all_inte_change3, all_inte_change4, all_inte_change5, gc6, gc7)

glbl_chg_aov2 <-car::Anova(all_inte_change2)
glbl_chg_aov3 <-car::Anova(all_inte_change3)
glbl_chg_aov4 <-car::Anova(all_inte_change4)
glbl_chg_aov5 <-car::Anova(all_inte_change5)
glbl_chg_aov6 <-car::Anova(gc6)
glbl_chg_aov7 <-car::Anova(gc7)

all_changes_sig_coefs <- sig_coefs(
  global_inte_changes2,
  glbl_chg_aov2,
  global_inte_changes3,
  glbl_chg_aov3,
  global_inte_changes4,
  glbl_chg_aov4,
  global_inte_changes5,
  glbl_chg_aov5,
  global_inte_changes6,
  glbl_chg_aov6,
  global_inte_changes7,
  glbl_chg_aov7,
  p = 0.10
)

write_clip(all_changes_sig_coefs)


############






all_inte_diffs1 <- glmer(post_inte ~ Condition + pre_inte + (1|pseudo),
                       data = all_changes,
                       family = binomial)
global_inte_diffs1 <- summary(all_inte_diffs1)

all_inte_diffs2 <- glmer(post_inte ~ Condition + pre_inte + (1|pseudo) + (1|var),
                        data = all_changes,
                        family = binomial)
global_inte_diffs2 <- summary(all_inte_diffs2)

# Random effects of var essentially 0, dropping

############
#Interaction Condition, Importance, and Knowledge
############
all_inte_diffs3 <- glmer(post_inte ~ Condition*scale(Importance)*scale(Knowledge) + pre_inte + (1|pseudo),
                        data = all_changes,
                        family = binomial,
                        control = glmerControl(optimizer = "bobyqa"))
global_inte_diffs3 <- summary(all_inte_diffs3)

############
#Interaction Condition, Importance
############
all_inte_diffs4 <- glmer(post_inte ~ Condition*scale(Importance) + pre_inte + (1|pseudo),
                        data = all_changes,
                        family = binomial)
global_inte_diffs4 <- summary(all_inte_diffs4)

############
# Interaction Condition, Knowledge
############
all_inte_diffs5 <- glmer(post_inte ~ Condition*scale(Knowledge) + pre_inte + (1|pseudo),
                        data = all_changes,
                        family = binomial)
global_inte_diffs5 <- summary(all_inte_diffs5)

############
# Interaction Importance
############
all_inte_diffs6 <- glmer(post_inte ~ scale(Importance) + pre_inte + (1|pseudo),
                        data = all_changes,
                        family = binomial)
global_inte_diffs6 <- summary(all_inte_diffs6)

############
# Interaction Knowledge
############
all_inte_diffs7 <- glmer(post_inte ~ scale(Knowledge) + pre_inte + (1|pseudo),
                        data = all_changes,
                        family = binomial)
global_inte_diffs7 <- summary(all_inte_diffs7)


AIC(all_inte_diffs1, all_inte_diffs2, all_inte_diffs3, all_inte_diffs4, all_inte_diffs5, all_inte_diffs6, all_inte_diffs7)

glbl_diff_aov1 <-car::Anova(all_inte_diffs1)
glbl_diff_aov2 <-car::Anova(all_inte_diffs2)
glbl_diff_aov3 <-car::Anova(all_inte_diffs3)
glbl_diff_aov4 <-car::Anova(all_inte_diffs4)
glbl_diff_aov5 <-car::Anova(all_inte_diffs5)
glbl_diff_aov6 <-car::Anova(all_inte_diffs6)
glbl_diff_aov7 <-car::Anova(all_inte_diffs7)

all_diffs_sig_coefs <- sig_coefs(
  global_inte_diffs1,
  glbl_diff_aov1,
  global_inte_diffs2,
  glbl_diff_aov2,
  global_inte_diffs3,
  glbl_diff_aov3,
  global_inte_diffs4,
  glbl_diff_aov4,
  global_inte_diffs5,
  glbl_diff_aov5,
  global_inte_diffs6,
  glbl_diff_aov6,
  global_inte_diffs7,
  glbl_diff_aov7,
  p = 0.10
)

write_clip(all_diffs_sig_coefs)







all_inte_ord1 <- clmm(as.factor(post_inte - pre_inte) ~ Condition + (1|pseudo), data = all_changes)
glb_ord_diffs1 <- summary(all_inte_ord1)

# Random effects of var essentially 0, dropping
all_inte_ord2 <- clmm(as.factor(post_inte - pre_inte) ~ Condition + (1|pseudo) +(1|var), data = all_changes)
glb_ord_diffs2 <- summary(all_inte_ord2)

############
#Interaction Condition, Importance, and Knowledge
############

all_inte_ord3 <- clmm(as.factor(post_inte - pre_inte) ~ Condition*scale(Importance)*scale(Knowledge) + (1|pseudo), data = all_changes)
glb_ord_diffs3 <- summary(all_inte_ord3)

############
#Interaction Condition, Importance
############

all_inte_ord4 <- clmm(as.factor(post_inte - pre_inte) ~ Condition*scale(Importance) + (1|pseudo), data = all_changes)
glb_ord_diffs4 <- summary(all_inte_ord4)

############
# Interaction Condition, Knowledge
############
all_inte_ord5 <- clmm(as.factor(post_inte - pre_inte) ~ Condition*scale(Knowledge) + (1|pseudo), data = all_changes)
glb_ord_diffs5 <- summary(all_inte_ord5)

############
# Interaction Importance
############
all_inte_ord6 <- clmm(as.factor(post_inte - pre_inte) ~ scale(Importance) + (1|pseudo), data = all_changes)
glb_ord_diffs6 <- summary(all_inte_ord6)

############
# Interaction Knowledge
############
all_inte_ord7 <- clmm(as.factor(post_inte - pre_inte) ~ scale(Knowledge) + (1|pseudo), data = all_changes)
glb_ord_diffs7 <- summary(all_inte_ord7)

AIC(all_inte_ord1, all_inte_ord2, all_inte_ord3, all_inte_ord4, all_inte_ord5, all_inte_ord6, all_inte_ord7)

glbl_ord_aov1 <- car::Anova(all_inte_ord1)
glbl_ord_aov2 <- car::Anova(all_inte_ord2)
glbl_ord_aov3 <- car::Anova(all_inte_ord3)
glbl_ord_aov4 <- car::Anova(all_inte_ord4)
glbl_ord_aov5 <- car::Anova(all_inte_ord5)
glbl_ord_aov6 <- car::Anova(all_inte_ord6)
glbl_ord_aov7 <- car::Anova(all_inte_ord7)

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




drop_ord1 <- drop1(all_inte_ord1, test = "Chi")
drop_ord2 <- drop1(all_inte_ord2, test = "Chi")
drop_ord3 <- drop1(all_inte_ord3, test = "Chi")
drop_ord4 <- drop1(all_inte_ord4, test = "Chi")
drop_ord5 <- drop1(all_inte_ord5, test = "Chi")
drop_ord6 <- drop1(all_inte_ord6, test = "Chi")
drop_ord7 <- drop1(all_inte_ord7, test = "Chi")
