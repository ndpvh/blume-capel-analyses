################################################################################
# PURPOSE: Perform analyses targeted at understanding whether the 
# manipulations worked. All attitude items for both topics. Each analysis
# in this script subsamples based on different types of changing behavior.
################################################################################


# This script looks at different subsets of the sample, specifically based on
# changes in answers between T1 and T2. This is important to look at, because
# we proposed that changes toward neutrality would be most likely accomplished
# in participants whose beta values are not too high (as this would firmly root
# them in one of the three discrete states) but also not too low (indifference).

# The linear model structures follow the structures of the base full sample 
# analysis found in "All Within - Mids Collapsed".

# For each sample tested:
# Linear models ending with 1 and 2 are “baseline” models. 1 tests with random 
# effects, and 2 drops the random effects that are 0.
# *** For between models, the outcome variable is tested on Condition
# *** For within models, the outcome variable is tested on Condition*Time
# Models ending with 3 add * Knowledge * Importance to the baseline model
# Models ending with 4 is the baseline model with * Importance added
# Models ending with 5 is the baseline model with * Knowledge added


# Assuming you start from ~/blume-capel-analyses/

library("haven")
library("tidyverse")
library(lme4)
library(clipr)

# ignore the NAs introduction warning from the preprocessing script
# it is a lie, I checked!

source("./scripts/preprocessing.R")
source("./scripts/lazy functions v2.R")


########################

# Section 1

# Not midpoint at T1, did they change to midpoint at T2
# Note: This is a between-factor version of the within analysis performed in the 
# "All Within - Mids Collapsed" script

########################


all_mid_switchers <- all_changes %>%
  filter(pre_mid == 0)

all_model_switch1 <- glmer(post_mid ~ Condition + (1|pseudo) + (1|var),
                           data = all_mid_switchers,
                           family = binomial)
to_midpoint1 <- summary(all_model_switch1)

# random effect of item is 0. discarding

all_model_switch2 <- glmer(post_mid ~ Condition + (1|pseudo),
                          data = all_mid_switchers,
                          family = binomial)
to_midpoint2 <- summary(all_model_switch2)

all_model_switch3 <- glmer(post_mid ~ Condition*scale(Knowledge)*scale(Importance) + (1|pseudo),
                          data = all_mid_switchers,
                          family = binomial,
                          control = glmerControl(optimizer = "bobyqa"))

to_midpoint3 <- summary(all_model_switch3)

all_model_switch4 <- glmer(post_mid ~ Condition*scale(Importance) + (1|pseudo),
                          data = all_mid_switchers,
                          family = binomial,
                          control = glmerControl(optimizer = "bobyqa"))

to_midpoint4 <- summary(all_model_switch4)

all_model_switch5 <- glmer(post_mid ~ Condition*scale(Knowledge) + (1|pseudo),
                           data = all_mid_switchers,
                           family = binomial,
                           control = glmerControl(optimizer = "bobyqa"))

to_midpoint5 <- summary(all_model_switch5)

AIC(all_model_switch2, all_model_switch3, all_model_switch4, all_model_switch5)

aov_to_mid <- car::Anova(all_model_switch1, type = 3)
aov_to_mid2 <- car::Anova(all_model_switch2, type = 3)
aov_to_mid3 <- car::Anova(all_model_switch3, type = 3)
aov_to_mid4 <- car::Anova(all_model_switch4, type = 3)
aov_to_mid5 <- car::Anova(all_model_switch5, type = 3)

to_midpoint_coefs <- sig_coefs(
  to_midpoint1,
  aov_to_mid,
  to_midpoint2,
  aov_to_mid2,
  to_midpoint3,
  aov_to_mid3,
  to_midpoint4,
  aov_to_mid4,
  to_midpoint5,
  aov_to_mid5,
  p = 0.10
)

write_clip(to_midpoint_coefs)

########################

# Section 2 

# Changed to midpoint vs everyone else 
# A full sample analysis

########################


all_change_model1 <- glmer(moved_to_mid ~ Condition + (1|pseudo),
                           data = all_changes_rewide,
                           family = binomial)

to_mid_vs_not1 <- summary(all_change_model1)

# no random effect for item

all_change_model2 <- glmer(mid_success ~ Condition + (1|pseudo),
                          data = all_changes,
                          family = binomial)

to_mid_vs_not2 <- summary(all_change_model2)

all_change_model3 <- glmer(moved_to_mid ~ Condition*scale(Knowledge)*scale(Importance) + (1|pseudo),
                          data = all_changes_rewide,
                          family = binomial,
                          control = glmerControl(optimizer = "bobyqa"))

to_mid_vs_not3 <- summary(all_change_model3)

all_change_model4 <- glmer(moved_to_mid ~ Condition*scale(Importance) + (1|pseudo),
                          data = all_changes_rewide,
                          family = binomial,
                          control = glmerControl(optimizer = "bobyqa"))

to_mid_vs_not4 <- summary(all_change_model4)

all_change_model5 <- glmer(moved_to_mid ~ Condition*scale(Knowledge) + (1|pseudo),
                           data = all_changes_rewide,
                           family = binomial,
                           control = glmerControl(optimizer = "bobyqa"))

to_mid_vs_not5 <- summary(all_change_model5)

AIC(all_change_model1, all_change_model2, all_change_model3, all_change_model4, all_change_model5)

aov_MidChgNot <- car::Anova(all_change_model1)
aov_MidChgNot2 <- car::Anova(all_change_model2)
aov_MidChgNot3 <- car::Anova(all_change_model3)
aov_MidChgNot4 <- car::Anova(all_change_model4)
aov_MidChgNot5 <- car::Anova(all_change_model5)

to_mid_vs_not_coefs <- sig_coefs(
  to_mid_vs_not1,
  aov_MidChgNot,
  to_mid_vs_not2,
  aov_MidChgNot2,
  to_mid_vs_not3,
  aov_MidChgNot3,
  to_mid_vs_not4,
  aov_MidChgNot4,
  to_mid_vs_not5,
  aov_MidChgNot5,
  p = 0.10
)

write_clip(to_mid_vs_not_coefs)


########################

# Section 3 

# Did people who changed (using the midpoint as the reference) between T1 and T2 
# do so at different rates based on condition? 
# (subsample: only those who changed between T1 and T2)

########################

##########

# As a between effect:

##########

model_direction1 <- glmer(toward_mid ~ Condition + (1|pseudo),
                         family = binomial,
                         data = all_changers)

condition_change1 <- summary(model_direction1)

# random effects virtually 0. dropping

model_direction2 <- glm(toward_mid ~ Condition,
                        family = binomial,
                        data = all_changers)

condition_change2 <- summary(model_direction2)

model_direction3 <- glm(toward_mid ~ Condition*scale(Knowledge)*scale(Importance),
                        family = binomial,
                        data = all_changers)

condition_change3 <- summary(model_direction3)

model_direction4 <- glm(toward_mid ~ Condition*scale(Importance),
                        family = binomial,
                        data = all_changers)

condition_change4 <- summary(model_direction4)

model_direction5 <- glm(toward_mid ~ Condition*scale(Knowledge),
                        family = binomial,
                        data = all_changers)

condition_change5 <- summary(model_direction5)

AIC(model_direction2, model_direction3, model_direction4, model_direction5)

aov_conChange1 <- car::Anova(model_direction1)
aov_conChange2 <- car::Anova(model_direction2)
aov_conChange3 <- car::Anova(model_direction3)
aov_conChange4 <- car::Anova(model_direction4)
aov_conChange5 <- car::Anova(model_direction5)

con_change_coefs <- sig_coefs(
  condition_change1,
  aov_conChange1,
  condition_change2,
  aov_conChange2,
  condition_change3,
  aov_conChange3,
  condition_change4,
  aov_conChange4,
  condition_change5,
  aov_conChange5,
  p = 0.10
)

write_clip(con_change_coefs)

