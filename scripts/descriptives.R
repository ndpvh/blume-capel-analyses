################################################################################
# PURPOSE: Compute the descriptive statistics of the data and create some 
# figures that help us visualize the data.
#
# Note that some descriptives are contained in the 
# neutrality_responses.R script, specifically those indicating 
# the number of neutral responses across items across people.
# Here, we include the same results plus some additional ones that
# reflect the design of the study.
################################################################################

################################################################################
# PRELIMINARY STUFF

# Loading in the datasets
filenames <- c("DEKKER", "MILLER", "PILOT")
data <- lapply(
    filenames,
    function(x) data.table::fread(
        file.path("data", paste0(x, ".csv")),
        data.table = FALSE
    )
) |>
    `names<-` (filenames)



################################################################################
# DEKKER

# Per item descriptives
cols <- data[["DEKKER"]] |>
    dplyr::select(tidyselect::contains("s_")) |>
    colnames() |>
    substring(3)

result <- lapply(
    cols,
    function(x) {
        # Create a data.frame with enough spaces for all of the 
        # results
        result <- data.frame(item = x)

        # Select only the relevant data
        dekker <- data[["DEKKER"]][, paste0(c("s_", "n_", "m_"), x)] |>
            `colnames<-` (c("response", "neutral", "importance"))

        # Descriptives for responses
        result$mean_response <- mean(dekker$response, na.rm = TRUE)
        result$sd_response <- sd(dekker$response, na.rm = TRUE)
        result$min_response <- min(dekker$response, na.rm = TRUE)
        result$q25_response <- quantile(dekker$response, prob = 0.25, na.rm = TRUE)
        result$q50_response <- quantile(dekker$response, prob = 0.50, na.rm = TRUE)
        result$q75_response <- quantile(dekker$response, prob = 0.75, na.rm = TRUE)
        result$max_response <- max(dekker$response, na.rm = TRUE)

        # Distributions of responses
        N <- nrow(dekker)
        result$response_1 <- sum(dekker$response == 1, na.rm = TRUE) / N
        result$response_2 <- sum(dekker$response == 2, na.rm = TRUE) / N
        result$response_3 <- sum(dekker$response == 3, na.rm = TRUE) / N
        result$response_4 <- sum(dekker$response == 4, na.rm = TRUE) / N
        result$response_5 <- sum(dekker$response == 5, na.rm = TRUE) / N
        result$response_6 <- sum(dekker$response == 6, na.rm = TRUE) / N
        result$response_7 <- sum(dekker$response == 7, na.rm = TRUE) / N

        # Reason for responding neutrally
        N <- sum(!is.na(dekker$neutral))
        result$indifferent <- sum(dekker$neutral == 1, na.rm = TRUE) / N
        result$neutral <- sum(dekker$neutral == 2, na.rm = TRUE) / N
        result$no_understanding <- sum(dekker$neutral == 3, na.rm = TRUE) / N

        # Descriptives of importance
        result$mean_importance <- mean(dekker$importance, na.rm = TRUE)
        result$sd_importance <- sd(dekker$importance, na.rm = TRUE)
        result$min_importance <- min(dekker$importance, na.rm = TRUE)
        result$q25_importance <- quantile(dekker$importance, prob = 0.25, na.rm = TRUE)
        result$q50_importance <- quantile(dekker$importance, prob = 0.50, na.rm = TRUE)
        result$q75_importance <- quantile(dekker$importance, prob = 0.75, na.rm = TRUE)
        result$max_importance <- max(dekker$importance, na.rm = TRUE)

        # Number of missings in responses and importance
        result$missing_response <- sum(is.na(dekker$response)) / nrow(dekker)
        result$missing_importance <- sum(is.na(dekker$importance)) / nrow(dekker)

        return(result)
    }
)
result <- do.call("rbind", result)

write.table(
    result, 
    file.path("results", "descriptives_dekker.csv")
)

View(result)



################################################################################
# MILLER

# Per item descriptives
data[["MILLER"]] <- data[["MILLER"]] |>
    dplyr::mutate(
        item = paste(part, question, sep = "_")
    )
rows <- unique(data[["MILLER"]]$item)

result <- lapply(
    rows,
    function(x) {
        # Create a data.frame with enough spaces for all of the 
        # results
        result <- data.frame(item = x)

        # Select only the relevant data
        miller <- data[["MILLER"]] |>
            dplyr::filter(item == x) |>
            dplyr::select(
                attitude, 
                certainty, 
                neutrality_reason, 
                uncertainty_reason,
                topic_importance
            ) |>
            dplyr::rename(
                response = attitude,
                neutral = neutrality_reason, 
                importance = topic_importance
            )

        # Descriptives for responses
        result$mean_response <- mean(miller$response, na.rm = TRUE)
        result$sd_response <- sd(miller$response, na.rm = TRUE)
        result$min_response <- min(miller$response, na.rm = TRUE)
        result$q25_response <- quantile(miller$response, prob = 0.25, na.rm = TRUE)
        result$q50_response <- quantile(miller$response, prob = 0.50, na.rm = TRUE)
        result$q75_response <- quantile(miller$response, prob = 0.75, na.rm = TRUE)
        result$max_response <- max(miller$response, na.rm = TRUE)

        # Distributions of responses
        N <- nrow(miller)
        result$response_1 <- sum(miller$response == 1, na.rm = TRUE) / N
        result$response_2 <- sum(miller$response == 2, na.rm = TRUE) / N
        result$response_3 <- sum(miller$response == 3, na.rm = TRUE) / N
        result$response_4 <- sum(miller$response == 4, na.rm = TRUE) / N
        result$response_5 <- sum(miller$response == 5, na.rm = TRUE) / N

        # Descriptives for certainty
        result$mean_certainty <- mean(miller$certainty, na.rm = TRUE)
        result$sd_certainty <- sd(miller$certainty, na.rm = TRUE)
        result$min_certainty <- min(miller$certainty, na.rm = TRUE)
        result$q25_certainty <- quantile(miller$certainty, prob = 0.25, na.rm = TRUE)
        result$q50_certainty <- quantile(miller$certainty, prob = 0.50, na.rm = TRUE)
        result$q75_certainty <- quantile(miller$certainty, prob = 0.75, na.rm = TRUE)
        result$max_certainty <- max(miller$certainty, na.rm = TRUE)

        # Reason for responding neutrally
        N <- sum(miller$neutral != "")
        result$reluctant <- sum(miller$neutral == "reluctance", na.rm = TRUE) / N
        result$cautious <- sum(miller$neutral == "caution", na.rm = TRUE) / N
        result$indifferent <- sum(miller$neutral == "indifference", na.rm = TRUE) / N

        # Reason for responding neutrally when uncertain
        N <- sum(miller$uncertainty_reason != "")
        result$reluctant_certainty <- sum(miller$uncertainty_reason == "reluctance", na.rm = TRUE) / N
        result$cautious_certainty <- sum(miller$uncertainty_reason == "caution", na.rm = TRUE) / N
        result$indifferent_certainty <- sum(miller$uncertainty_reason == "indifference", na.rm = TRUE) / N

        # Descriptives of importance
        result$mean_importance <- mean(miller$importance, na.rm = TRUE)
        result$sd_importance <- sd(miller$importance, na.rm = TRUE)
        result$min_importance <- min(miller$importance, na.rm = TRUE)
        result$q25_importance <- quantile(miller$importance, prob = 0.25, na.rm = TRUE)
        result$q50_importance <- quantile(miller$importance, prob = 0.50, na.rm = TRUE)
        result$q75_importance <- quantile(miller$importance, prob = 0.75, na.rm = TRUE)
        result$max_importance <- max(miller$importance, na.rm = TRUE)

        # Number of missings in responses and importance
        result$missing_response <- sum(is.na(miller$response)) / nrow(miller)
        result$missing_importance <- sum(is.na(miller$importance)) / nrow(miller)

        return(result)
    }
)
result <- do.call("rbind", result)

write.table(
    result, 
    file.path("results", "descriptives_miller.csv")
)

View(result)


# Descriptives per condition
conditions <- unique(data[["MILLER"]]$condition)
result <- lapply(
    rows,
    function(x) {
        # Loop over the unique conditions
        result <- lapply(
            conditions,
            function(y) {
                # Create a data.frame with enough spaces for all of the 
                # results
                result <- data.frame(
                    item = x,
                    condition = y
                )

                # Select only the relevant data
                miller <- data[["MILLER"]] |>
                    dplyr::filter(
                        item == x,
                        condition == y
                    ) |>
                    dplyr::select(
                        attitude, 
                        certainty, 
                        neutrality_reason, 
                        uncertainty_reason,
                        topic_importance
                    ) |>
                    dplyr::rename(
                        response = attitude,
                        neutral = neutrality_reason, 
                        importance = topic_importance
                    )

                # Descriptives for responses
                result$mean_response <- mean(miller$response, na.rm = TRUE)
                result$sd_response <- sd(miller$response, na.rm = TRUE)
                result$min_response <- min(miller$response, na.rm = TRUE)
                result$q25_response <- quantile(miller$response, prob = 0.25, na.rm = TRUE)
                result$q50_response <- quantile(miller$response, prob = 0.50, na.rm = TRUE)
                result$q75_response <- quantile(miller$response, prob = 0.75, na.rm = TRUE)
                result$max_response <- max(miller$response, na.rm = TRUE)

                # Distributions of responses
                N <- nrow(miller)
                result$response_1 <- sum(miller$response == 1, na.rm = TRUE) / N
                result$response_2 <- sum(miller$response == 2, na.rm = TRUE) / N
                result$response_3 <- sum(miller$response == 3, na.rm = TRUE) / N
                result$response_4 <- sum(miller$response == 4, na.rm = TRUE) / N
                result$response_5 <- sum(miller$response == 5, na.rm = TRUE) / N

                # Descriptives for certainty
                result$mean_certainty <- mean(miller$certainty, na.rm = TRUE)
                result$sd_certainty <- sd(miller$certainty, na.rm = TRUE)
                result$min_certainty <- min(miller$certainty, na.rm = TRUE)
                result$q25_certainty <- quantile(miller$certainty, prob = 0.25, na.rm = TRUE)
                result$q50_certainty <- quantile(miller$certainty, prob = 0.50, na.rm = TRUE)
                result$q75_certainty <- quantile(miller$certainty, prob = 0.75, na.rm = TRUE)
                result$max_certainty <- max(miller$certainty, na.rm = TRUE)

                # Reason for responding neutrally
                N <- sum(miller$neutral != "")
                result$reluctant <- sum(miller$neutral == "reluctance", na.rm = TRUE) / N
                result$cautious <- sum(miller$neutral == "caution", na.rm = TRUE) / N
                result$indifferent <- sum(miller$neutral == "indifference", na.rm = TRUE) / N

                # Reason for responding neutrally when uncertain
                N <- sum(miller$uncertainty_reason != "")
                result$reluctant_certainty <- sum(miller$uncertainty_reason == "reluctance", na.rm = TRUE) / N
                result$cautious_certainty <- sum(miller$uncertainty_reason == "caution", na.rm = TRUE) / N
                result$indifferent_certainty <- sum(miller$uncertainty_reason == "indifference", na.rm = TRUE) / N

                # Descriptives of importance
                result$mean_importance <- mean(miller$importance, na.rm = TRUE)
                result$sd_importance <- sd(miller$importance, na.rm = TRUE)
                result$min_importance <- min(miller$importance, na.rm = TRUE)
                result$q25_importance <- quantile(miller$importance, prob = 0.25, na.rm = TRUE)
                result$q50_importance <- quantile(miller$importance, prob = 0.50, na.rm = TRUE)
                result$q75_importance <- quantile(miller$importance, prob = 0.75, na.rm = TRUE)
                result$max_importance <- max(miller$importance, na.rm = TRUE)

                # Number of missings in responses and importance
                result$missing_response <- sum(is.na(miller$response)) / nrow(miller)
                result$missing_importance <- sum(is.na(miller$importance)) / nrow(miller)

                return(result)
            }
        )

        return(do.call("rbind", result))
    }
)
result <- do.call("rbind", result)

write.table(
    result, 
    file.path("results", "descriptives_miller_condition.csv")
)

View(result)



################################################################################
# PILOT

# Per item descriptives
cols <- expand.grid(
    c(
        paste0("hc", 1:3),
        paste0("tariff", 1:3),
        paste0("costs", 1:3),
        paste0("meat", 1:5)
    ),
    c("_pre", "_post")
)
cols <- sapply(
    seq_len(nrow(cols)), 
    function(i) cols[i, ] |>
        unlist() |>
        as.character() |>
        paste(collapse = "")
)

result <- lapply(
    cols,
    function(x) {
        # Create a data.frame with enough spaces for all of the 
        # results
        result <- data.frame(item = x)

        # Select only the relevant data
        importance <- ifelse(
            grepl("meat", x, fixed = TRUE),
            "meat_importance",
            "econ_importance"
        )
        pilot <- data[["PILOT"]][, c(x, importance)] |>
            `colnames<-` (c("neutral", "importance")) |>
            dplyr::mutate(
                response = dplyr::case_match(
                    neutral,
                    "strongly_disagree" ~ 1,
                    "disagree" ~ 2,
                    "agree" ~ 4,
                    "strongly_agree" ~ 5,
                    .default = 3
                ),
                neutral = ifelse(
                    response == 3,
                    neutral,
                    NA
                )
            )

        # Descriptives for responses
        result$mean_response <- mean(pilot$response, na.rm = TRUE)
        result$sd_response <- sd(pilot$response, na.rm = TRUE)
        result$min_response <- min(pilot$response, na.rm = TRUE)
        result$q25_response <- quantile(pilot$response, prob = 0.25, na.rm = TRUE)
        result$q50_response <- quantile(pilot$response, prob = 0.50, na.rm = TRUE)
        result$q75_response <- quantile(pilot$response, prob = 0.75, na.rm = TRUE)
        result$max_response <- max(pilot$response, na.rm = TRUE)

        # Distributions of responses
        N <- nrow(pilot)
        result$response_1 <- sum(pilot$response == 1, na.rm = TRUE) / N
        result$response_2 <- sum(pilot$response == 2, na.rm = TRUE) / N
        result$response_3 <- sum(pilot$response == 3, na.rm = TRUE) / N
        result$response_4 <- sum(pilot$response == 4, na.rm = TRUE) / N
        result$response_5 <- sum(pilot$response == 5, na.rm = TRUE) / N

        # Reason for responding neutrally
        N <- sum(!is.na(pilot$neutral))
        result$reluctant <- sum(pilot$neutral == "reluctant", na.rm = TRUE) / N
        result$neutral <- sum(pilot$neutral == "neutral", na.rm = TRUE) / N
        result$cautious <- sum(pilot$neutral == "cautious", na.rm = TRUE) / N
        result$indifferent <- sum(pilot$neutral == "indifferent", na.rm = TRUE) / N
        result$ambivalent <- sum(pilot$neutral == "ambivalent", na.rm = TRUE) / N

        # Descriptives of importance
        result$mean_importance <- mean(pilot$importance, na.rm = TRUE)
        result$sd_importance <- sd(pilot$importance, na.rm = TRUE)
        result$min_importance <- min(pilot$importance, na.rm = TRUE)
        result$q25_importance <- quantile(pilot$importance, prob = 0.25, na.rm = TRUE)
        result$q50_importance <- quantile(pilot$importance, prob = 0.50, na.rm = TRUE)
        result$q75_importance <- quantile(pilot$importance, prob = 0.75, na.rm = TRUE)
        result$max_importance <- max(pilot$importance, na.rm = TRUE)

        # Number of missings in responses and importance
        result$missing_response <- sum(is.na(pilot$response)) / nrow(pilot)
        result$missing_importance <- sum(is.na(pilot$importance)) / nrow(pilot)

        return(result)
    }
)
result <- do.call("rbind", result)

write.table(
    result, 
    file.path("results", "descriptives_pilot.csv")
)

View(result)


# Per item per condition descriptives
conditions <- unique(data[["PILOT"]]$condition)
result <- lapply(
    cols,
    function(x) {
        result <- lapply(
            conditions, 
            function(y) {
                # Create a data.frame with enough spaces for all of the 
                # results
                result <- data.frame(
                    item = x,
                    condition = y
                )

                # Select only the relevant data
                importance <- ifelse(
                    grepl("meat", x, fixed = TRUE),
                    "meat_importance",
                    "econ_importance"
                )
                pilot <- data[["PILOT"]][, c("condition", x, importance)] |>
                    dplyr::filter(condition == y) |>
                    dplyr::select(-condition) |>
                    `colnames<-` (c("neutral", "importance")) |>
                    dplyr::mutate(
                        response = dplyr::case_match(
                            neutral,
                            "strongly_disagree" ~ 1,
                            "disagree" ~ 2,
                            "agree" ~ 4,
                            "strongly_agree" ~ 5,
                            .default = 3
                        ),
                        neutral = ifelse(
                            response == 3,
                            neutral,
                            NA
                        )
                    )

                # Descriptives for responses
                result$mean_response <- mean(pilot$response, na.rm = TRUE)
                result$sd_response <- sd(pilot$response, na.rm = TRUE)
                result$min_response <- min(pilot$response, na.rm = TRUE)
                result$q25_response <- quantile(pilot$response, prob = 0.25, na.rm = TRUE)
                result$q50_response <- quantile(pilot$response, prob = 0.50, na.rm = TRUE)
                result$q75_response <- quantile(pilot$response, prob = 0.75, na.rm = TRUE)
                result$max_response <- max(pilot$response, na.rm = TRUE)

                # Distributions of responses
                N <- nrow(pilot)
                result$response_1 <- sum(pilot$response == 1, na.rm = TRUE) / N
                result$response_2 <- sum(pilot$response == 2, na.rm = TRUE) / N
                result$response_3 <- sum(pilot$response == 3, na.rm = TRUE) / N
                result$response_4 <- sum(pilot$response == 4, na.rm = TRUE) / N
                result$response_5 <- sum(pilot$response == 5, na.rm = TRUE) / N

                # Reason for responding neutrally
                N <- sum(!is.na(pilot$neutral))
                result$reluctant <- sum(pilot$neutral == "reluctant", na.rm = TRUE) / N
                result$neutral <- sum(pilot$neutral == "neutral", na.rm = TRUE) / N
                result$cautious <- sum(pilot$neutral == "cautious", na.rm = TRUE) / N
                result$indifferent <- sum(pilot$neutral == "indifferent", na.rm = TRUE) / N
                result$ambivalent <- sum(pilot$neutral == "ambivalent", na.rm = TRUE) / N

                # Descriptives of importance
                result$mean_importance <- mean(pilot$importance, na.rm = TRUE)
                result$sd_importance <- sd(pilot$importance, na.rm = TRUE)
                result$min_importance <- min(pilot$importance, na.rm = TRUE)
                result$q25_importance <- quantile(pilot$importance, prob = 0.25, na.rm = TRUE)
                result$q50_importance <- quantile(pilot$importance, prob = 0.50, na.rm = TRUE)
                result$q75_importance <- quantile(pilot$importance, prob = 0.75, na.rm = TRUE)
                result$max_importance <- max(pilot$importance, na.rm = TRUE)

                # Number of missings in responses and importance
                result$missing_response <- sum(is.na(pilot$response)) / nrow(pilot)
                result$missing_importance <- sum(is.na(pilot$importance)) / nrow(pilot)

                return(result)
            }
        )

        return(do.call("rbind", result))
    }
)
result <- do.call("rbind", result)

write.table(
    result, 
    file.path("results", "descriptives_pilot_condition.csv")
)

View(result)