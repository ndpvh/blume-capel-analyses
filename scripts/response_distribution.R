################################################################################
# PURPOSE: Check the response distributions and whether they are uni-, bi-,
# or trimodal, and what their distribution looks like.
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

# Create a function that will look for modes in the data based on frequency. 
# It will furthermore include a significance test in the form of a nonparametric
# bootstrap procedure, allowing us to check the significance of the difference
# between the mode and its immediate neighbours.
#
# @param data Dataset within which to look at the modes
# @param columns Character vector denoting the columns to look at
# @param unique_values Numeric vector containing the unique values of the data
# @param N Number of samples to draw for the parametric bootstrap
#
# @return Data.frame containing the frequencies per response option and the 
# significance of their difference compared to their neighbours
modality <- function(data, 
                     columns, 
                     unique_values = unique(data[, columns]),
                     N = 1000) {

    # Sort the unique values
    unique_values <- sort(unique_values)

    # Descriptives of the modality
    result <- lapply(
        columns, 
        function(x) {
            # Remove NAs from the data
            values <- data[, x] |>
                na.omit() |>
                as.numeric()

            # Create a data.frame with sufficient space for the results
            result <- data.frame(
                variable = rep(x, length(unique_values)),
                response = sort(unique_values)
            )

            # Get the frequencies for the variables
            frequencies <- numeric(length(unique_values)) |>
                `names<-` (unique_values)
            tbl <- table(values)
            frequencies[names(tbl)] <- tbl

            result$frequency <- frequencies

            # Perform the bootstrap to get the confidence intervals around this
            # estimate
            frequencies <- matrix(0, nrow = N, ncol = length(unique_values)) |>
                `colnames<-` (unique_values)
            for(i in 1:N) {
                # Resample
                y <- sample(
                    values,
                    length(values),
                    replace = TRUE
                )

                # Save
                tbl <- table(y)
                frequencies[i, names(tbl)] <- tbl
            }

            result[, c("q025", "q50", "q975")] <- sapply(
                as.character(unique_values),
                function(i) quantile(
                    frequencies[, i], 
                    prob = c(0.025, 0.5, 0.975)
                )
            ) |>
                t()

            # Find out whether the value is a local mode. Works in the following
            # way:
            #   - Check whether the first and last values are larger than their
            #     neigbor. If so, they are marked as a local mode
            #   - Find out when there is an increase or decrease in frequencies
            #   - Mark times in which one moves from an increase to a decrease
            #     as another local mode
            N <- nrow(result)
            result$mode <- logical(N)
            
            result$mode[1] <- result$frequency[1] > result$frequency[2]
            result$mode[N] <- result$frequency[N] > result$frequency[N - 1]

            changes <- result$frequency |>
                diff() |>
                sign() |>
                diff()
            idx <- which(changes < 0)
            result$mode[idx + 1] <- TRUE

            # Return
            return(result)
        }
    )
    frequency <- do.call("rbind", result)

    # Significance of the differences between the frequencies of the responses
    combinations <- cbind(
        unique_values[2:length(unique_values) - 1],
        unique_values[2:length(unique_values)]
    )

    result <- lapply(
        columns,
        function(x) {
            result <- lapply(
                seq_len(nrow(combinations)),
                function(i) {
                    # Remove NAs from the data
                    values <- data[, x] |>
                        na.omit() |>
                        as.numeric()

                    # Create a data.frame with sufficient space for the results
                    result <- data.frame(
                        variable = x,
                        response_1 = combinations[i, 1],
                        response_2 = combinations[i, 2]
                    )

                    # Get the frequencies for the two responses
                    frequencies <- numeric(length(unique_values)) |>
                        `names<-` (unique_values)
                    tbl <- table(values)
                    frequencies[names(tbl)] <- tbl

                    freq <- frequencies[as.character(combinations[i, ])]
                    result$frequency_1 <- freq[1]
                    result$frequency_2 <- freq[2]
                    result$difference_12 <- freq[1] - freq[2]

                    # Perform the bootstrap to get the confidence intervals around this
                    # estimate
                    differences <- numeric(N)
                    for(j in 1:N) {
                        # Resample
                        y <- sample(
                            values,
                            length(values),
                            replace = TRUE
                        )

                        # Save
                        tbl <- table(y)
                        frequencies[names(tbl)] <- tbl
                        
                        freq <- frequencies[as.character(combinations[i, ])]
                        differences[j] <- freq[1] - freq[2]
                    }

                    # Some summary statistics for the differences
                    result$sd <- sd(differences)
                    result[, c("q025", "q50", "q975")] <- quantile(
                        differences, 
                        prob = c(0.025, 0.5, 0.975)
                    )

                    # Check whether 0 is part of the confidence interval
                    result$significant <- result$q025 >= 0 | result$q975 <= 0

                    # Return
                    return(result)
                }
            )

            return(do.call("rbind", result))
        }
    )
    differences <- do.call("rbind", result)

    # Add information to the frequencies about whether they are significantly 
    # different from their immediate neighbors. 
    frequency$significant <- sapply(
        seq_len(nrow(frequency)), 
        function(i) {
            # Select those difference responses that are for the same variable
            # and contains the response in either part of the difference 
            # question
            idx_1 <- differences$response_1 == frequency$response[i]
            idx_2 <- differences$response_2 == frequency$response[i]
            idx_3 <- differences$variable == frequency$variable[i]

            idx <- (idx_1 | idx_2) & idx_3
            
            return(any(differences$significant[idx]))
        }
    )

    # Summarize all information in a little data.frame
    summarized <- lapply(
        columns,
        function(x) {
            # Select relevant data
            freq <- frequency[frequency$variable == x, ]
            diffs <- differences[differences$variable == x, ]

            # Summarize some information here
            return(
                data.frame(
                    variable = x, 
                    number_modes = sum(freq$mode),
                    significant_modes = sum(freq$significant & freq$mode),
                    neutral_mode = median(unique_values) %in% unique_values[freq$mode]
                )
            )
        }
    )
    summarized <- do.call("rbind", summarized)

    return(
        list(
            "frequency" = frequency, 
            "differences" = differences,
            "summary" = summarized
        )
    )
} 



################################################################################
# DEKKER

idx <- grepl("s_", colnames(data[["DEKKER"]]), fixed = TRUE)
result <- modality(
    data[["DEKKER"]],
    columns = colnames(data[["DEKKER"]])[idx],
    unique_values = 1:7,
    N = 10000
)

saveRDS(
    result, 
    file.path("results", "modality_dekker.Rds")
)

View(result$summary)




################################################################################
# MILLER

# Here, we first remove the "don't know" response (response 6)
result <- data[["MILLER"]] |>
    dplyr::select(id, item, attitude) |>
    dplyr::mutate(
        attitude = ifelse(
            attitude == 6, 
            NA, 
            attitude
        )
    ) |>
    tidyr::pivot_wider(
        id_cols = id,
        values_from = attitude, 
        names_from = item
    ) |> 
    as.data.frame() |>
    modality(
        columns = paste(
            rep(1:2, each = 4),
            rep(1:4, times = 2),
            sep = "_"
        ),
        unique_values = 1:5,
        N = 10000
    )

saveRDS(
    result, 
    file.path("results", "modality_miller.Rds")
)

View(result$summary)

# Here, we first recode the "don't know" response to a neutral response
# reflecting caution (what the investigators where interested in)
result <- data[["MILLER"]] |>
    dplyr::select(id, item, attitude) |>
    dplyr::mutate(
        attitude = ifelse(
            attitude == 6, 
            3, 
            attitude
        )
    ) |>
    tidyr::pivot_wider(
        id_cols = id,
        values_from = attitude, 
        names_from = item
    ) |> 
    as.data.frame() |>
    modality(
        columns = paste(
            rep(1:2, each = 4),
            rep(1:4, times = 2),
            sep = "_"
        ),
        unique_values = 1:5,
        N = 10000
    )

saveRDS(
    result, 
    file.path("results", "modality_miller_caution.Rds")
)

View(result$summary)



################################################################################
# PILOT

# Define the columns we are interested in
idx <- grepl(
    "_pre",
    colnames(data[["PILOT"]]),
    fixed = TRUE
)
idy <- grepl(
    "_post",
    colnames(data[["PILOT"]]),
    fixed = TRUE
)
cols <- colnames(data[["PILOT"]])[idx | idy]

# We first have to recode the the variables, but afterward we can do the mode 
# count. In this recoding, we assign a value of 3 to all neutral options of the 
# cross-Likert scale
result <- data[["PILOT"]] |>
    dplyr::mutate(
        dplyr::across(
            hc1_pre:meat5_post,
            function(x) x |>
                dplyr::case_match(
                    "strongly_disagree" ~ 1, 
                    "disagree" ~ 2, 
                    "agree" ~ 4, 
                    "strongly_agree" ~ 5, 
                    .default = 3
                )
        )
    ) |>
    modality(
        columns = cols,
        unique_values = 1:5,
        N = 10000
    )

saveRDS(
    result, 
    file.path("results", "modality_pilot.Rds")
)

View(result$summary)
