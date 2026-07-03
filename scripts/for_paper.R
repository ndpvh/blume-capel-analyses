################################################################################
# DESCRIPTIVES
#
#   Some descriptive statistics of the responses themselves, providing us with 
#   an idea of variation across items, across people, across conditions, and 
#   general tendencies in the responses.


#########################
# DEKKER
DEKKER <- data.table::fread(file.path("results", "descriptives_dekker.csv"))

View(DEKKER)

# Mean responses: Mean, standard deviation, and range across items
mean(DEKKER$mean_response)
sd(DEKKER$mean_response)
range(DEKKER$mean_response)

# SD of the responses: Mean, standard deviation, and range of the individual 
# variation across items
mean(DEKKER$sd_response)
sd(DEKKER$sd_response)
range(DEKKER$sd_response)

# Analysis of the importance question across items and individuals in the same 
# way as for the responses
mean(DEKKER$mean_importance)
sd(DEKKER$mean_importance)
range(DEKKER$mean_importance)

mean(DEKKER$sd_importance)
sd(DEKKER$sd_importance)
range(DEKKER$sd_importance)

plt <- ggplot2::ggplot(
    data = DEKKER, 
    ggplot2::aes(x = s_vlees)
) +
    ggplot2::geom_bar(
        fill = "cornflowerblue"
    ) +
    ggplot2::labs(x = "Item about 'Meat Consumption'") +
    ggplot2::theme_minimal()

ggplot2::ggsave(
    file.path("s_vlees.png"),
    plt, 
    width = 800, 
    height = 900, 
    unit = "px"
)
plt



#########################
# MILLER
MILLER <- data.table::fread(file.path("results", "descriptives_miller.csv"))

View(MILLER)

# Mean responses: Mean, standard deviation, and range across items
mean(MILLER$mean_response)
sd(MILLER$mean_response)
range(MILLER$mean_response)

# SD of the responses: Mean, standard deviation, and range of the individual 
# variation across items
mean(MILLER$sd_response)
sd(MILLER$sd_response)
range(MILLER$sd_response)

# Analysis of the importance question across items and individuals. Note that 
# here, importance was only asked once, allowing us to take a look at the unique
# values in the columns shown here
mean(MILLER$mean_importance)
mean(MILLER$sd_importance)
min(MILLER$min_importance)
max(MILLER$max_importance)

# Analysis of the certainty question across items and individuals. Note that 
# here, importance was only asked once, allowing us to take a look at the unique
# values in the columns shown here
mean(MILLER$mean_certainty)
mean(MILLER$sd_certainty)
min(MILLER$min_certainty)
max(MILLER$max_certainty)



#########################
# PILOT
PILOT <- data.table::fread(file.path("results", "descriptives_pilot_condition.csv"))

View(PILOT)

# Mean responses: Mean, standard deviation, and range across items. Performed 
# per condition and per phase separately
mean(PILOT$mean_response[PILOT$condition == "control" & grepl("_pre", PILOT$item, fixed = TRUE)])
sd(PILOT$mean_response[PILOT$condition == "control" & grepl("_pre", PILOT$item, fixed = TRUE)])
range(PILOT$mean_response[PILOT$condition == "control" & grepl("_pre", PILOT$item, fixed = TRUE)])

mean(PILOT$mean_response[PILOT$condition == "conflict" & grepl("_pre", PILOT$item, fixed = TRUE)])
sd(PILOT$mean_response[PILOT$condition == "conflict" & grepl("_pre", PILOT$item, fixed = TRUE)])
range(PILOT$mean_response[PILOT$condition == "conflict" & grepl("_pre", PILOT$item, fixed = TRUE)])

mean(PILOT$mean_response[PILOT$condition == "integration" & grepl("_pre", PILOT$item, fixed = TRUE)])
sd(PILOT$mean_response[PILOT$condition == "integration" & grepl("_pre", PILOT$item, fixed = TRUE)])
range(PILOT$mean_response[PILOT$condition == "integration" & grepl("_pre", PILOT$item, fixed = TRUE)])

mean(PILOT$mean_response[PILOT$condition == "control" & grepl("_post", PILOT$item, fixed = TRUE)])
sd(PILOT$mean_response[PILOT$condition == "control" & grepl("_post", PILOT$item, fixed = TRUE)])
range(PILOT$mean_response[PILOT$condition == "control" & grepl("_post", PILOT$item, fixed = TRUE)])

mean(PILOT$mean_response[PILOT$condition == "conflict" & grepl("_post", PILOT$item, fixed = TRUE)])
sd(PILOT$mean_response[PILOT$condition == "conflict" & grepl("_post", PILOT$item, fixed = TRUE)])
range(PILOT$mean_response[PILOT$condition == "conflict" & grepl("_post", PILOT$item, fixed = TRUE)])

mean(PILOT$mean_response[PILOT$condition == "integration" & grepl("_post", PILOT$item, fixed = TRUE)])
sd(PILOT$mean_response[PILOT$condition == "integration" & grepl("_post", PILOT$item, fixed = TRUE)])
range(PILOT$mean_response[PILOT$condition == "integration" & grepl("_post", PILOT$item, fixed = TRUE)])

# SD of the responses: Mean, standard deviation, and range of the individual 
# variation across items. Performed per condition and per phase separately
mean(PILOT$sd_response[PILOT$condition == "control" & grepl("_pre", PILOT$item, fixed = TRUE)])
sd(PILOT$sd_response[PILOT$condition == "control" & grepl("_pre", PILOT$item, fixed = TRUE)])
range(PILOT$sd_response[PILOT$condition == "control" & grepl("_pre", PILOT$item, fixed = TRUE)])

mean(PILOT$sd_response[PILOT$condition == "conflict" & grepl("_pre", PILOT$item, fixed = TRUE)])
sd(PILOT$sd_response[PILOT$condition == "conflict" & grepl("_pre", PILOT$item, fixed = TRUE)])
range(PILOT$sd_response[PILOT$condition == "conflict" & grepl("_pre", PILOT$item, fixed = TRUE)])

mean(PILOT$sd_response[PILOT$condition == "integration" & grepl("_pre", PILOT$item, fixed = TRUE)])
sd(PILOT$sd_response[PILOT$condition == "integration" & grepl("_pre", PILOT$item, fixed = TRUE)])
range(PILOT$sd_response[PILOT$condition == "integration" & grepl("_pre", PILOT$item, fixed = TRUE)])

mean(PILOT$sd_response[PILOT$condition == "control" & grepl("_post", PILOT$item, fixed = TRUE)])
sd(PILOT$sd_response[PILOT$condition == "control" & grepl("_post", PILOT$item, fixed = TRUE)])
range(PILOT$sd_response[PILOT$condition == "control" & grepl("_post", PILOT$item, fixed = TRUE)])

mean(PILOT$sd_response[PILOT$condition == "conflict" & grepl("_post", PILOT$item, fixed = TRUE)])
sd(PILOT$sd_response[PILOT$condition == "conflict" & grepl("_post", PILOT$item, fixed = TRUE)])
range(PILOT$sd_response[PILOT$condition == "conflict" & grepl("_post", PILOT$item, fixed = TRUE)])

mean(PILOT$sd_response[PILOT$condition == "integration" & grepl("_post", PILOT$item, fixed = TRUE)])
sd(PILOT$sd_response[PILOT$condition == "integration" & grepl("_post", PILOT$item, fixed = TRUE)])
range(PILOT$sd_response[PILOT$condition == "integration" & grepl("_post", PILOT$item, fixed = TRUE)])

# Analysis of the importance question across items and individuals. This analysis
# is performed per condition and per type of question separately (econ vs meat). 
# Note that phase does not matter here, as the importance questions were asked 
# only once in the study.
#
# Economic questions first
mean(PILOT$mean_importance[PILOT$condition == "control" & grepl("_post", PILOT$item, fixed = TRUE) & !grepl("meat", PILOT$item, fixed = TRUE)])
mean(PILOT$sd_importance[PILOT$condition == "control" & grepl("_post", PILOT$item, fixed = TRUE) & !grepl("meat", PILOT$item, fixed = TRUE)])
min(PILOT$min_importance[PILOT$condition == "control" & grepl("_post", PILOT$item, fixed = TRUE) & !grepl("meat", PILOT$item, fixed = TRUE)])
max(PILOT$max_importance[PILOT$condition == "control" & grepl("_post", PILOT$item, fixed = TRUE) & !grepl("meat", PILOT$item, fixed = TRUE)])

mean(PILOT$mean_importance[PILOT$condition == "conflict" & grepl("_post", PILOT$item, fixed = TRUE) & !grepl("meat", PILOT$item, fixed = TRUE)])
mean(PILOT$sd_importance[PILOT$condition == "conflict" & grepl("_post", PILOT$item, fixed = TRUE) & !grepl("meat", PILOT$item, fixed = TRUE)])
min(PILOT$min_importance[PILOT$condition == "conflict" & grepl("_post", PILOT$item, fixed = TRUE) & !grepl("meat", PILOT$item, fixed = TRUE)])
max(PILOT$max_importance[PILOT$condition == "conflict" & grepl("_post", PILOT$item, fixed = TRUE) & !grepl("meat", PILOT$item, fixed = TRUE)])

mean(PILOT$mean_importance[PILOT$condition == "integration" & grepl("_post", PILOT$item, fixed = TRUE) & !grepl("meat", PILOT$item, fixed = TRUE)])
mean(PILOT$sd_importance[PILOT$condition == "integration" & grepl("_post", PILOT$item, fixed = TRUE) & !grepl("meat", PILOT$item, fixed = TRUE)])
min(PILOT$min_importance[PILOT$condition == "integration" & grepl("_post", PILOT$item, fixed = TRUE) & !grepl("meat", PILOT$item, fixed = TRUE)])
max(PILOT$max_importance[PILOT$condition == "integration" & grepl("_post", PILOT$item, fixed = TRUE) & !grepl("meat", PILOT$item, fixed = TRUE)])

# Meat questions now
mean(PILOT$mean_importance[PILOT$condition == "control" & grepl("_post", PILOT$item, fixed = TRUE) & grepl("meat", PILOT$item, fixed = TRUE)])
mean(PILOT$sd_importance[PILOT$condition == "control" & grepl("_post", PILOT$item, fixed = TRUE) & grepl("meat", PILOT$item, fixed = TRUE)])
min(PILOT$min_importance[PILOT$condition == "control" & grepl("_post", PILOT$item, fixed = TRUE) & grepl("meat", PILOT$item, fixed = TRUE)])
max(PILOT$max_importance[PILOT$condition == "control" & grepl("_post", PILOT$item, fixed = TRUE) & grepl("meat", PILOT$item, fixed = TRUE)])

mean(PILOT$mean_importance[PILOT$condition == "conflict" & grepl("_post", PILOT$item, fixed = TRUE) & grepl("meat", PILOT$item, fixed = TRUE)])
mean(PILOT$sd_importance[PILOT$condition == "conflict" & grepl("_post", PILOT$item, fixed = TRUE) & grepl("meat", PILOT$item, fixed = TRUE)])
min(PILOT$min_importance[PILOT$condition == "conflict" & grepl("_post", PILOT$item, fixed = TRUE) & grepl("meat", PILOT$item, fixed = TRUE)])
max(PILOT$max_importance[PILOT$condition == "conflict" & grepl("_post", PILOT$item, fixed = TRUE) & grepl("meat", PILOT$item, fixed = TRUE)])

mean(PILOT$mean_importance[PILOT$condition == "integration" & grepl("_post", PILOT$item, fixed = TRUE) & grepl("meat", PILOT$item, fixed = TRUE)])
mean(PILOT$sd_importance[PILOT$condition == "integration" & grepl("_post", PILOT$item, fixed = TRUE) & grepl("meat", PILOT$item, fixed = TRUE)])
min(PILOT$min_importance[PILOT$condition == "integration" & grepl("_post", PILOT$item, fixed = TRUE) & grepl("meat", PILOT$item, fixed = TRUE)])
max(PILOT$max_importance[PILOT$condition == "integration" & grepl("_post", PILOT$item, fixed = TRUE) & grepl("meat", PILOT$item, fixed = TRUE)])




################################################################################
# FREQUENCY MIDPOINT
#
#   Analysis of how often midpoint responses occurred across all items, as well 
#   as an analysis of the types of modes that are encountered in the dataset.


#########################
# DEKKER

DEKKER <- readRDS(file.path("results", "modality_dekker.Rds"))

# Descriptives of the frequencies. Happens in the following way:
#   - Extract the dataset containing the frequencies per response
#   - Distinguish midpoint vs non-midpoint responding
#   - Compute the proportions
freq <- DEKKER$frequency |>
    dplyr::mutate(
        agreed = dplyr::case_when(
            response < 4 ~ "disagree",
            response == 4 ~ "midpoint",
            response > 4 ~ "agree"
        )
    ) |>
    dplyr::group_by(variable, agreed) |>
    dplyr::summarize(
        variable = variable[1],
        frequency = sum(frequency)
    ) |>
    dplyr::ungroup()

midpoint <- freq[freq$agreed == "midpoint", ]$frequency
disagree <- freq[freq$agreed == "disagree", ]$frequency
agree <- freq[freq$agreed == "agree", ]$frequency

mean(midpoint / (midpoint + disagree + agree))
sd(midpoint / (midpoint + disagree + agree))
range(midpoint / (midpoint + disagree + agree))

mean(disagree / (midpoint + disagree + agree))
sd(disagree / (midpoint + disagree + agree))
range(disagree / (midpoint + disagree + agree))

mean(agree / (midpoint + disagree + agree))
sd(agree / (midpoint + disagree + agree))
range(agree / (midpoint + disagree + agree))


# Model analysis. Proceeds as follows:
#   - First check the frequencies with which each mode was observed
#   - Then check how often the neutral mode was one of them
modal <- DEKKER$summary

table(modal$significant_modes)
View(modal)



#########################
# MILLER

MILLER <- readRDS(file.path("results", "modality_miller.Rds"))

# Descriptives of the frequencies. Happens in the following way:
#   - Extract the dataset containing the frequencies per response
#   - Distinguish midpoint vs non-midpoint responding
#   - Compute the proportions
freq <- MILLER$frequency |>
    dplyr::mutate(
        agreed = dplyr::case_when(
            response < 3 ~ "disagree",
            response == 3 ~ "midpoint",
            response > 3 ~ "agree"
        )
    ) |>
    dplyr::group_by(variable, agreed) |>
    dplyr::summarize(
        variable = variable[1],
        frequency = sum(frequency)
    ) |>
    dplyr::ungroup()

midpoint <- freq[freq$agreed == "midpoint", ]$frequency
disagree <- freq[freq$agreed == "disagree", ]$frequency
agree <- freq[freq$agreed == "agree", ]$frequency

mean(midpoint / (midpoint + disagree + agree))
sd(midpoint / (midpoint + disagree + agree))
range(midpoint / (midpoint + disagree + agree))

mean(disagree / (midpoint + disagree + agree))
sd(disagree / (midpoint + disagree + agree))
range(disagree / (midpoint + disagree + agree))

mean(agree / (midpoint + disagree + agree))
sd(agree / (midpoint + disagree + agree))
range(agree / (midpoint + disagree + agree))

# Model analysis. Proceeds as follows:
#   - First check the frequencies with which each mode was observed
#   - Then check how often the neutral mode was one of them
modal <- MILLER$summary

table(modal$significant_modes)
View(modal)




#########################
# PILOT

PILOT <- readRDS(file.path("results", "modality_pilot.Rds"))

# Descriptives of the frequencies. Happens in the following way:
#   - Extract the dataset containing the frequencies per response
#   - Distinguish midpoint vs non-midpoint responding
#   - Compute the proportions
freq <- PILOT$frequency |>
    dplyr::mutate(
        agreed = dplyr::case_when(
            response < 3 ~ "disagree",
            response == 3 ~ "midpoint",
            response > 3 ~ "agree"
        )
    ) |>
    dplyr::group_by(variable, agreed) |>
    dplyr::summarize(
        variable = variable[1],
        frequency = sum(frequency)
    ) |>
    dplyr::ungroup()

midpoint <- freq[freq$agreed == "midpoint", ]$frequency
disagree <- freq[freq$agreed == "disagree", ]$frequency
agree <- freq[freq$agreed == "agree", ]$frequency

mean(midpoint / (midpoint + disagree + agree))
sd(midpoint / (midpoint + disagree + agree))
range(midpoint / (midpoint + disagree + agree))

mean(disagree / (midpoint + disagree + agree))
sd(disagree / (midpoint + disagree + agree))
range(disagree / (midpoint + disagree + agree))

mean(agree / (midpoint + disagree + agree))
sd(agree / (midpoint + disagree + agree))
range(agree / (midpoint + disagree + agree))


# Model analysis. Proceeds as follows:
#   - First check the frequencies with which each mode was observed
#   - Then check how often the neutral mode was one of them
modal <- PILOT$summary

table(modal$significant_modes)
View(modal)



################################################################################
# REASONS
#
#   Analysis of the reasons participants gave for responding neutrally.


#########################
# DEKKER

# Descriptives of the different types of neutral responses
DEKKER <- data.table::fread(file.path("results", "descriptives_dekker.csv"))

DEKKER |>
    dplyr::summarize(
        mean_indifferent = mean(indifferent),
        sd_indifferent = sd(indifferent), 
        min_indifferent = min(indifferent), 
        max_indifferent = max(indifferent), 

        mean_neutral = mean(neutral),
        sd_neutral = sd(neutral), 
        min_neutral = min(neutral), 
        max_neutral = max(neutral),

        mean_understanding = mean(no_understanding),
        sd_understanding = sd(no_understanding), 
        min_understanding = min(no_understanding), 
        max_understanding = max(no_understanding)
    ) |>
    View()

# Correlations between the importance of a topic and the choice for indifference
DEKKER <- data.table::fread(file.path("results", "correlations_dekker.csv"))

DEKKER |>
    dplyr::summarize(
        mean = mean(estimate),
        sd = sd(estimate),
        min = min(estimate), 
        max = max(estimate),
        p_value = sum(p_value <= 0.05),
        sample = mean(sample_size, na.rm = TRUE),
        sd_sample = sd(sample_size, na.rm = TRUE), 
        min_sample = min(sample_size, na.rm = TRUE), 
        max_sample = max(sample_size, na.rm = TRUE)
    ) |>
    View()


#########################
# MILLER

# Descriptives of the different types of neutral responses
MILLER <- data.table::fread(file.path("results", "descriptives_miller.csv"))

MILLER |>
    dplyr::summarize(
        mean_indifferent = mean(indifferent),
        sd_indifferent = sd(indifferent), 
        min_indifferent = min(indifferent), 
        max_indifferent = max(indifferent), 

        mean_reluctant = mean(reluctant),
        sd_reluctant = sd(reluctant), 
        min_reluctant = min(reluctant), 
        max_reluctant = max(reluctant),

        mean_cautious = mean(cautious),
        sd_cautious = sd(cautious), 
        min_cautious = min(cautious), 
        max_cautious = max(cautious)
    ) |>
    View()

# Correlations between the importance of a topic and the choice for indifference
MILLER <- data.table::fread(file.path("results", "correlations_miller.csv"))

MILLER |>
    dplyr::summarize(
        mean = mean(estimate, na.rm = TRUE),
        sd = sd(estimate, na.rm = TRUE),
        min = min(estimate, na.rm = TRUE), 
        max = max(estimate, na.rm = TRUE),
        p_value = sum(p_value <= 0.05, na.rm = TRUE),
        missing = sum(is.na(estimate)),
        sample = mean(sample_size, na.rm = TRUE),
        sd_sample = sd(sample_size, na.rm = TRUE), 
        min_sample = min(sample_size, na.rm = TRUE), 
        max_sample = max(sample_size, na.rm = TRUE)
    ) |>
    View()



#########################
# PILOT

# Descriptives of the different types of neutral responses
PILOT <- data.table::fread(file.path("results", "descriptives_pilot.csv"))

PILOT |>
    dplyr::summarize(
        mean_indifferent = mean(indifferent),
        sd_indifferent = sd(indifferent), 
        min_indifferent = min(indifferent), 
        max_indifferent = max(indifferent), 

        mean_reluctant = mean(reluctant),
        sd_reluctant = sd(reluctant), 
        min_reluctant = min(reluctant), 
        max_reluctant = max(reluctant),

        mean_cautious = mean(cautious),
        sd_cautious = sd(cautious), 
        min_cautious = min(cautious), 
        max_cautious = max(cautious),

        mean_neutral = mean(neutral),
        sd_neutral = sd(neutral), 
        min_neutral = min(neutral), 
        max_neutral = max(neutral),

        mean_ambivalent = mean(ambivalent),
        sd_ambivalent = sd(ambivalent), 
        min_ambivalent = min(ambivalent), 
        max_ambivalent = max(ambivalent)
    ) |>
    View()

# Correlations between the importance of a topic and the choice for indifference
PILOT <- data.table::fread(file.path("results", "correlations_pilot.csv"))

PILOT |>
    dplyr::filter(importance == "importance") |>
    dplyr::summarize(
        mean = mean(estimate, na.rm = TRUE),
        sd = sd(estimate, na.rm = TRUE),
        min = min(estimate, na.rm = TRUE), 
        max = max(estimate, na.rm = TRUE),
        p_value = sum(p_value <= 0.05, na.rm = TRUE),
        missing = sum(is.na(estimate)),
        sample = mean(sample_size, na.rm = TRUE), 
        sd_sample = sd(sample_size, na.rm = TRUE), 
        min_sample = min(sample_size, na.rm = TRUE), 
        max_sample = max(sample_size, na.rm = TRUE)
    ) |>
    View()



################################################################################
# SWITCHES
#
#   Analysis of the results for the Markov chain of the PILOT study.

# Midpoint vs Valenced
result <- readRDS(file.path("results", "midpoint-condition.Rds"))

result$summary |>
    dplyr::filter(condition == "control - integration - conflict") |>
    View()

result$comparison |>
    dplyr::filter(!includes_zero) |>
    View()

# Neutrality options vs Valenced
result <- readRDS(file.path("results", "midpoint-differentiated-condition.Rds"))

result$summary |>
    dplyr::filter(condition == "control - integration - conflict") |>
    View()

result$comparison |>
    dplyr::filter(!includes_zero) |>
    View()
