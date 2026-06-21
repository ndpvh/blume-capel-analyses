################################################################################
# PURPOSE: Checking the demographics of the participants.
################################################################################

library(dplyr)
library(haven)

pilot_demo <- read.csv("./data/pilot_demographics.csv")

# Mean, SD, range of Age
mean(pilot_demo$Age)
sd(pilot_demo$Age)
range(pilot_demo$Age)

# Table of sex
table(pilot_demo$Sex)

# Percentange male/female

print(100/201)

# Table of ethnicities
table(pilot_demo$Ethnicity.simplified)

# Percent white participants
print(142/201)

# Table of primary languages
table(pilot_demo$Language)

# Table of birth countries
table(pilot_demo$Country.of.birth)
