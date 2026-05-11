################################################################################
# PURPOSE: Perform analyses targeted at understanding whether the 
# manipulations worked. Economic items only. Full sample.
################################################################################


############

# This script simplifies previous analyses and provides effect size estimates.

############

# Assuming you start from ~/blume-capel-analyses/

# ignore the NAs introduction warning from the preprocessing script
# it is a lie, I checked!

library("haven")
library("tidyverse")
library(lme4)
library(clipr)
library(lsr)

source("./scripts/preprocessing.R")
source("./scripts/lazy-functions-v2.R")

############

######

# First test all non-attitude variables as predictors of whether someone moved 
# from non-midpoint to midpoint (1) or not (0). I.e., post_mid - pre_mid, with 
# -1 recoded to 0.

######

econ_main_effects <- glmer(mid_success ~ Condition + imp_scaled + knw_scaled + pol_scaled + (1|pseudo),
                          data = econ_changes,
                          family = binomial)

summary(econ_main_effects)

###

# Only Condition 3 is a significant predictor. No interactions will be tested.

###


aov_mains_econ1 <- aov(mid_success ~ Condition + imp_scaled + knw_scaled + pol_scaled, data = econ_changes)
summary(aov_mains_econ1)
etaSquared(aov_mains_econ1)

# Calculating cohen's f
# sqrt((2.547354e-03/(1-2.547354e-03)))
# Which can also be done via:
effectsize::cohens_f(aov_mains_econ1)

############

# If you want to test the post_mid with pre_mid as an explicit predictor.
# However, this arguable obfuscates the role of the condition in increasing
# alpha, as it now includes those who were midpoint at both T1 and T2 as "success".

econ_main_effects2 <- glmer(post_mid ~ Condition + imp_scaled + knw_scaled + pol_scaled + pre_mid + (1|pseudo),
                           data = econ_changes,
                           family = binomial)

summary(econ_main_effects2)

############

# Did people who changed (using the midpoint as the reference) between T1 and T2 
# do so at different rates based on condition? 
# (subsample: only those who changed between T1 and T2)

econ_mid_switchers <- econ_changes %>%
  filter(pre_mid == 0)

econ_model_switch1 <- glmer(mid_success ~ Condition + imp_scaled + knw_scaled + pol_scaled + (1|pseudo),
                           data = econ_mid_switchers,
                           family = binomial)
summary(econ_model_switch1)

aov_switch_econ1 <- aov(mid_success ~ Condition + imp_scaled + knw_scaled + pol_scaled, data = econ_mid_switchers)
summary(aov_switch_econ1)
#etaSquared(aov_switch1)
#sqrt((2.835682e-03/(1-2.835682e-03)))
effectsize::cohens_f(aov_switch_econ1)

#######

# With interaction effects of (near) significant variables

switch_econ_interx <- glmer(mid_success ~ Condition + imp_scaled + Condition*imp_scaled + (1|pseudo),
                             data = econ_mid_switchers,
                             family = binomial)

summary(switch_econ_interx)

aov_switch_econ_itx <-aov(mid_success ~ Condition + imp_scaled + Condition*imp_scaled, data = econ_mid_switchers)
summary(aov_switch_econ_itx)
effectsize::cohens_f(aov_switch_econ_itx)







