################################################################################
# PURPOSE: Preprocessing the data to get them in useful shapes.
################################################################################

# Assuming you start from ~/blume-capel-analyses/

library("tidyverse")
library("haven")
source("./scripts/lazy-functions-v2.R")


############

# Data prep

############

pseudo_data <- read.csv2("./data/pseudo_data.csv")

raw_recoded1 <- recode_items(pseudo_data)

trim_raw_recoded1 <- raw_recoded1[-c(1,2), -1] %>% trim_items()


# Not counting those who did not agree to the IC, only one person failed one
# of the attention checks. Prolific requires 2 failures for rejection



# Reversed items except 60 are literally reverse coded in the survey. 60 is 
# reversed for analysis because it is worded in a way that is "anti" meat reduction.

reverse_cols <- c(20, 23, 26, 35, 38, 42, 45, 48, 49, 57, 59, 60)

trim_raw_recoded1[, reverse_cols] <-
  lapply(trim_raw_recoded1[, reverse_cols], function(x) {
    x <- as.numeric(as.character(x))
    if_else(x %in% c(3, 6, 7, 8, 9), x, 6 - x)
  })


# Drop all columns that are completely unnecessary
rare_data1 <- trim_raw_recoded1[,-c(1:5, 7:17, 62:66)]

# Drop rows without IC agreement
medium_rare_data1 <- rare_data1 %>% filter(IC.Agree == 1)


# Yeah I misspelled "tariff" in the var names
# But not in the survey itself =]
medium_rare_data1 <- medium_rare_data1 %>%
  rename_with(~ str_replace_all(., regex("Tarr?iff?s?", ignore_case = TRUE), "Tariff"))


# For this one, ignore the warning about NAs. It's a lie, I checked!
medium_data1 <- medium_rare_data1 %>%
  left_join(
    raw_recoded1 %>%
      select(pseudo, 20:27) %>%
      mutate(across(2:9, as.numeric)),
    by = "pseudo"
  ) %>%
  rename_with(~str_remove(., "_1$"), ends_with("_1")) %>%
  mutate(
    Meat_Imp = rowMeans(across(c(Imp.Meat, Direct.Meat, Future.Meat))),
    Econ_Imp = rowMeans(across(c(Imp.Econ, Direct.Econ, Future.Econ)))
  )


all_pre_data <- medium_data1 %>% select(1, 3:11,, 28, 29, 31:33, 45:56)

all_post_data <- medium_data1 %>% select(1, 18:23, 25:27, 40:56)

all_changes <- medium_data1 %>%
  select(1, 3:11, 18:23, 25:29, 31:33, 40:56) %>%
  pivot_longer(
    cols = matches("\\.(Pre|Post)\\."),
    names_to = c("var", "time", "num"),
    names_pattern = "(.*)\\.(Pre|Post)\\.(.*)",
    values_to = "value",
    values_transform = as.numeric
  ) %>%
  pivot_wider(
    names_from = time,
    values_from = value
  ) %>%
  mutate(
    Importance = case_when(
      var %in% c("HC", "Tariff", "Costs") ~ Econ_Imp,
      var == "Meat" ~ Meat_Imp
    ),
    Knowledge = case_when(
      var %in% c("HC", "Tariff", "Costs") ~ Know.Econ,
      var == "Meat" ~ Know.Meat
    ),
    pre_simple = case_when(
      Pre %in% c(3, 6, 7, 8, 9) ~ 3,
      TRUE ~ Pre
    ),
    post_simple = case_when(
      Post %in% c(3, 6, 7, 8, 9) ~ 3,
      TRUE ~ Post,
    ),
    pre_mid = case_when(
      Pre %in% c(3, 6, 7, 8, 9) ~ 1,
      TRUE ~ 0
    ),
    post_mid = case_when(
      Post %in%  c(3, 6, 7, 8, 9) ~ 1,
      TRUE ~ 0
    ),
    mid_change = case_when(
      !(Pre %in% c(3, 6, 7, 8, 9)) & Post %in% c(3, 6, 7, 8, 9) ~ 1,
      TRUE ~ 0
    ),
    pre_inte = case_when(
      Pre == 7 ~ 1,
      TRUE ~ 0
    ),
    post_inte = case_when(
      Post == 7 ~ 1,
      TRUE ~ 0
    ),
    inte_change = case_when(
      Pre != 7 & Post == 7 ~ 1,
      Pre == 7 & Post != 7 ~ -1,
      TRUE ~ 0
    ),
    inte_bin = case_when(
      Pre != 7 & Post == 7 ~ 1,
      TRUE ~ 0),
    pre_neut = case_when(
      Pre %in% c(6, 7, 9) ~ 1,
      TRUE ~ 0
    ),
    post_neut = case_when(
      Post %in% c(6, 7, 9) ~ 1,
      TRUE ~ 0
    ),
    neut_change = case_when(
      Pre %in% !c(6, 7, 9) & Post %in% c(6, 7, 9) ~ 1,
      Pre %in% c(6, 7, 9) & Post %in% !c(6, 7, 9) ~ -1,
      TRUE ~ 0
    ),
    neut_bin = case_when(
      Pre %in% !c(6, 7, 9) & Post %in% c(6, 7, 9) ~ 1,
      TRUE ~ 0),
    pre_amb = case_when(
      Pre == 8 ~ 1,
      TRUE ~ 0
    ),
    post_amb = case_when(
      Post == 8 ~ 1,
      TRUE ~ 0
    ),
    amb_change = case_when(
      Pre != 8 & Post == 8 ~ 1,
      Pre == 8 & Post != 8 ~ -1,
      TRUE ~ 0
    ),
    amb_bin = case_when(
      Pre != 8 & Post == 8 ~ 1,
      TRUE ~ 0
    ),
    pre_indif = case_when(
      Pre == 3 ~ 1,
      TRUE ~ 0
    ),
    post_indif = case_when(
      Post == 3 ~ 1,
      TRUE ~ 0
    ),
    indif_change = case_when(
      Pre != 3 & Post == 3 ~ 1,
      Pre == 3 & Post != 3 ~ -1,
      TRUE ~ 0
    ),
    indif_bin = case_when(
      Pre != 3 & Post == 3 ~ 1,
      TRUE ~ 0
    ))


all_changes_long <- all_changes %>%
  pivot_longer(
    cols = c(pre_mid, post_mid),
    names_to = "time",
    values_to = "midpoint"
  ) %>%
  mutate(
    time = factor(time, levels = c("pre_mid", "post_mid")),
    Condition = factor(Condition),
    item = paste(var, num, sep = "_")
  )



all_changes_rewide <- all_changes_long %>%
  pivot_wider(names_from = time, values_from = midpoint) %>%
  mutate(moved_to_mid = as.integer(pre_mid == 0 & post_mid == 1))


all_changers <- all_changes_rewide %>%
  filter(pre_mid != post_mid) %>%
  mutate(toward_mid = as.integer(post_mid == 1))

all_changers_long <- all_changes_rewide %>%
  filter(pre_mid != post_mid) %>%
  pivot_longer(cols = c(pre_mid, post_mid), 
               names_to = "time", 
               values_to = "midpoint") %>%
  mutate(time = factor(time, levels = c("pre_mid", "post_mid")))

###########

# All Items: Expanded Midpoints Analyses

###########

all_mids_long <- all_changes %>%
  pivot_longer(
    cols = c(Pre, Post),
    names_to = "time",
    values_to = "response"
  ) %>%
  mutate(
    time = factor(time, levels = c("Pre", "Post")),
    Condition = factor(Condition),
    item = paste(var, num, sep = "_")
  )


all_mids_long <- all_changes %>%
  pivot_longer(
    cols = c(Pre, Post),
    names_to = "time",
    values_to = "response"
  ) %>%
  mutate(
    time = factor(time, levels = c("Pre", "Post")),
    Condition = factor(Condition),
    item = paste(var, num, sep = "_")
  )


all_inte_long <- all_changes %>%
  pivot_longer(
    cols = c(pre_inte, post_inte),
    names_to = "time",
    values_to = "inte"
  ) %>%
  mutate(
    time = factor(time, levels = c("pre_inte", "post_inte")),
    Condition = factor(Condition),
    item = paste(var, num, sep = "_")
  )


all_inte_rewide <- all_inte_long %>%
  pivot_wider(names_from = time, values_from = inte) %>%
  mutate(moved_to_mid = as.integer(pre_inte == 0 & post_inte == 1))



all_neut_long <- all_changes %>%
  pivot_longer(
    cols = c(pre_neut, post_neut),
    names_to = "time",
    values_to = "neut"
  ) %>%
  mutate(
    time = factor(time, levels = c("pre_neut", "post_neut")),
    Condition = factor(Condition),
    item = paste(var, num, sep = "_")
  )


all_neut_rewide <- all_neut_long %>%
  pivot_wider(names_from = time, values_from = neut) %>%
  mutate(moved_to_mid = as.integer(pre_neut == 0 & post_neut == 1))


all_neut_long <- all_changes %>%
  pivot_longer(
    cols = c(pre_neut, post_neut),
    names_to = "time",
    values_to = "neut"
  ) %>%
  mutate(
    time = factor(time, levels = c("pre_neut", "post_neut")),
    Condition = factor(Condition),
    item = paste(var, num, sep = "_")
  )


all_neut_rewide <- all_neut_long %>%
  pivot_wider(names_from = time, values_from = neut) %>%
  mutate(moved_to_mid = as.integer(pre_neut == 0 & post_neut == 1))

all_amb_long <- all_changes %>%
  pivot_longer(
    cols = c(pre_amb, post_amb),
    names_to = "time",
    values_to = "amb"
  ) %>%
  mutate(
    time = factor(time, levels = c("pre_amb", "post_amb")),
    Condition = factor(Condition),
    item = paste(var, num, sep = "_")
  )


all_amb_rewide <- all_amb_long %>%
  pivot_wider(names_from = time, values_from = amb) %>%
  mutate(moved_to_mid = as.integer(pre_amb == 0 & post_amb == 1))


