################################################################################
# PURPOSE: Estimate the conditional probability matrix defining switches 
#          between one or the other responses after the experimental task.
################################################################################

################################################################################
# PRELIMINARY STUFF

# Load some utility functions
source(file.path("scripts", "utils.R"))

# Load the data + A correction in a typo in the column names
data <- data.table::fread(
    file.path("data", "pilot.csv"),
    data.table = FALSE
)[-c(1:2), ] 

colnames(data) <- gsub("Tarriff", "Tariff", colnames(data))
colnames(data) <- gsub("Tariffs", "Tariff", colnames(data))

# Define the different columns of interest: We focus only on the cross-Likert 
# responses
items <- c(
    vector_paste(
        c("HC", "Tariff", "Costs"),
        c("Pre", "Post"),
        as.character(1:3),
        collapse = " "
    ),
    vector_paste(
        "Meat",
        c("Pre", "Post"),
        as.character(1:5),
        collapse = " "
    )
)

# Loop over each of these columns and merge them into one
for(i in seq_along(items)) {
    # Recode the values contained in the different response-columns related to 
    # the item -- assuming mutual exclusivity -- and save in a new column 
    # denoting the response to this item from every participant
    data[, items[i]] <- ifelse(
        data[, paste(items[i], "1", sep = "_")] == "",
        ifelse(
            data[, paste(items[i], "2", sep = "_")] == "",
            ifelse(
                data[, paste(items[i], "3", sep = "_")] == "",
                ifelse(
                    data[, paste(items[i], "4", sep = "_")] == "",
                    "8",
                    "9"
                ),
                data[, paste(items[i], "3", sep = "_")]
            ),
            "7"
        ),
        "6"
    ) |>
        dplyr::recode(
            "1" = "strongly_disagree",
            "2" = "disagree",
            "3" = "indifferent",
            "4" = "agree",
            "5" = "strongly_agree",
            "6" = "reluctant",
            "7" = "ambivalent",
            "8" = "neutral",
            "9" = "cautious"
        )
}

# Delete people who have no values for "condition" and retain only columns of 
# interest
data <- data |>
    # Select only the relevant data
    dplyr::select(
        PID,
        Pol_1,
        `Know Econ_1`:`Future Meat_1`,
        Condition,
        items[1]:items[length(items)]
    ) |> 
    dplyr::filter(Condition != "") |>

    # Rename some of the columns through transformation. Makes everything just 
    # that little bit easier later on
    dplyr::rename_with(tolower) |>
    dplyr::rename_with(function(x) gsub(" ", "_", x)) |>
    dplyr::rename(
        polarization = pol_1, 
        econ_knowledge = know_econ_1,
        meat_knowledge = know_meat_1,
        econ_importance = imp_econ_1,
        meat_importance = imp_meat_1,
        econ_personal_relevance = direct_econ_1,
        meat_personal_relevance = direct_meat_1,
        econ_future_impact = future_econ_1,
        meat_future_impact = future_meat_1
    ) |>
    dplyr::rename_with(
        function(x) {
            parts <- strsplit(x, "_")
            return(
                sapply(
                    seq_along(parts), 
                    function(i) paste(
                        paste0(
                            parts[[i]][1], 
                            parts[[i]][3]
                        ), 
                        parts[[i]][2], 
                        sep = "_"
                    )
                )
            )
        },
        .cols = hc_pre_1:meat_post_5
    ) |>

    # Recode the conditions so we know what's going on
    dplyr::mutate(
        condition = dplyr::case_match(
            condition,
            "1" ~ "control",
            "2" ~ "conflict",
            "3" ~ "integration"
        )
    ) |>

    # Move the columns around
    dplyr::relocate(
        pid,
        condition:meat5_post,
        polarization:meat_future_impact
    )

# Save these data
data.table::fwrite(
    data, 
    file.path("data", "data_cprob.csv")
)



################################################################################
# ESTIMATION CONDITIONAL PROBABILITIES

# Create a function that allows for different types of estimation given a 
# particular mapping between responses and what we are interested in.
#
# @param data The data to use for the estimation. Defaults to the data 
# preprocessed in the preliminary steps within this code.
# @param x Named list mapping codes (the names of the slots) with its content 
# being a character vector. Default to NULL, using the coding as is.
# @param .cols Character vector denoting the variables to be used for the 
# estimation. Defaults to NULL, using all columns in the dataset.
# @param filename Character denoting the filepath and filename where the 
# results should be saved. Defaults to NULL, indicating the results need not be 
# saved. Needs to include one of the R object extensions.
# @param priors Named list containing the priors to be used within the estimation
# procedure
# @param conditions Character vector containing the names of the conditions to 
# consider for the estimation procedure. Each character should contain the 
# names of the conditions that you would like to merge. For example, the default
# uses all data for the estimation procedure 
# (`"control - integration - conflict"`). Note that the differences of the 
# probabilities are made for all unique instances in this character vector.
# @param ... Arguments passed on to the stan function from rstan, defining the 
# sampling procedure
estimate <- function(data, 
                     x = NULL, 
                     .cols = NULL,
                     model = file.path("models", "conditional_probabilities.stan"),
                     filename = NULL,
                     priors = list("mu" = 0, "sigma" = 1),
                     conditions = "control - integration - conflict",
                     ...) {

    # Select those columns of the data that are relevant for the estimation. 
    # If NULL, then we use all columns in the data
    if(!is.null(.cols)) {
        data <- data[, paste0(rep(.cols, each = 2), c("_pre", "_post"))]
    } else {
        .cols <- c(
            paste0("hc", 1:3),
            paste0("tariff", 1:3),
            paste0("costs", 1:3),
            paste0("meat", 1:5)
        )
    }

    # Ensure the mapping is made correctly, and otherwise go to the defaults
    if(is.null(x)) {
        x <- data[, paste0(rep(.cols, each = 2), c("_pre", "_post"))] |>
            unlist() |>
            unique()
        
        x <- lapply(seq_along(x), function(i) x[i]) |>
            `names<-` (x)

    } else {
        expected <- c(
            "strongly_disagree", "disagree", "indifferent", "agree", "strongly_agree",
            "cautious", "reluctant", "ambivalent", "neutral"
        )

        if(!all(unlist(x) %in% expected)) {
            stop("Not all possible responses are included in the dataset.")
        }
    }
    
    # Recode the variables of interest according to the scheme
    data_matrix <- as.matrix(data)
    for(i in seq_along(x)) {
        data_matrix[data_matrix %in% x[[i]]] <- names(x)[i]
    }
    data <- data_matrix |>
        as.data.frame() |>
        `colnames<-` (colnames(data))

    # Create a matrix of all possible transitions, including remaining within a 
    # particular response
    combos <- expand.grid(names(x), names(x), stringsAsFactors = FALSE) |>
        `colnames<-` (c("s_t", "s_0")) |>
        dplyr::relocate(s_0, s_t)

    # Add rownames to this matrix to make the connection to the parameters. 
    # A partial match is enough, which is why I only provide one here
    indices <- expand.grid(seq_along(x), seq_along(x))
    nm <- sapply(
        seq_len(nrow(indices)), 
        function(x) paste0(
            "theta[",
            indices[x, 1], 
            ",", 
            indices[x, 2]
        )
    )
    combos <- combos |>
        `rownames<-` (nm)

    # Create the variables needed to estimate the conditional probability model.
    # This includes:
    #   - counts: An array containing the number of people who started out in 
    #             an initial state (columns) and moved to a particular state 
    #             after the task inbetween (rows) per condition (3rd dimension)
    #   - totals: A numerical vector containing the number of people who started
    #             out in a particular state, amounting to the sum of the columns
    d <- length(x)
    n <- length(conditions)
    f <- 1

    counts <- array(0, dim = c(d, d, n))
    totals <- matrix(0, nrow = d, ncol = n)
    for(i in seq_along(conditions)) {
        idx <- sapply(
            data$condition,
            function(x) grepl(x, conditions[i], fixed = FALSE)
        )

        for(j in seq_len(nrow(combos))) {
            # Extract the pre and post and make sure they're just vectors
            pre <- data[idx, paste0(.cols, "_pre")] |>
                unlist() |>
                as.character()
            post <- data[idx, paste0(.cols, "_post")] |>
                unlist() |>
                as.character()

            # Look at how many people start in a particular state and, of these, how 
            # many then moved to the target state
            counts[f] <- sum(pre == combos$s_0[j] & post == combos$s_t[j])
            f <- f + 1
        }
        totals[, i] <- colSums(counts[, , i])
    }

    # We also need some additional information for the comparisions between 
    # conditions. Namely indicating how many of these differences should be 
    # created and which differences should be taken
    #
    # If only one condition is provided, leave it at this. If multiple 
    # conditions are provided, then remove all conditions that are the same
    comparison <- expand.grid(
        seq_along(conditions), 
        seq_along(conditions)
    ) |>
        `colnames<-` (NULL) |>
        `rownames<-` (NULL) |>
        unlist() |>
        as.integer() |>
        matrix(ncol = 2)

    if(length(conditions) > 1) {
        # Remove conditions that are the same
        comparison <- comparison[comparison[, 1] != comparison[, 2], ]

        # Remove doubles through creating a unique code
        code <- sapply(
            seq_len(nrow(comparison)),
            function(x) paste(sort(comparison[x, ]), collapse = "")
        )
        comparison <- comparison[!duplicated(code), ]
    }

    # Compute a conditional probability matrix, representing the data that we
    # are working with here
    prob <- counts
    for(i in seq_len(dim(prob)[3])) {
        prob[, , i] <- counts[, , i] / rep(totals[, i], each = d)
    }
    
    # Define the data to be used by the model. First put them in a list and then
    # append the priors provided by the user
    mode(counts) <- "integer"
    mode(totals) <- "integer"
    mode(comparison) <- "integer"

    stan_data <- list(
        "d" = as.integer(d),
        "n" = as.integer(dim(counts)[3]),
        "n_switches" = counts, 
        "col_totals" = totals,
        "m" = nrow(comparison),
        "diff_indices" = comparison
    )
    stan_data <- append(stan_data, priors)

    # Sample from the posterior using Stan and the arguments provided by the 
    # user
    fit <- rstan::stan(
        file = model, 
        data = stan_data, 
        ...
    )

    # Get some diagnostics
    diagnostics <- do.call("rbind", rstan::get_sampler_params(fit)) |>
        rstan::summary()

    # Create some summaries across all chains.
    parameters <- names(fit@sim$samples[[1]])
    parameters <- parameters[!(parameters %in% c("idx_1", "idx_2"))]
    results <- lapply(
        seq_along(parameters),
        function(i) {
            # Add all samples for a particular parameter in a matrix
            results <- sapply(
                seq_along(fit@sim$samples),
                function(x) fit@sim$samples[[x]][[parameters[i]]]
            )

            # Add some information about the conditions. Different for the case
            # in which you're looking at difference parameters and non-difference
            # parameters
            if(parameters[i] != "lp__") {
                idx <- substring(
                    parameters[i], 
                    nchar(parameters[i]) - 1, 
                    nchar(parameters[i]) - 1
                ) |>
                    as.integer()
            }

            if(grepl("_diff", parameters[i], fixed = TRUE)) {
                idx <- comparison[idx, ]
                condition_1 <- conditions[idx[1]]
                condition_2 <- conditions[idx[2]]

            } else if(parameters[i] == "lp__") {
                condition_1 <- condition_2 <- NA 

            } else {
                condition_1 <- conditions[idx]
                condition_2 <- NA
            }

            # Create the summary statistics
            return(
                data.frame(
                    condition_1 = condition_1,
                    condition_2 = condition_2,
                    parameter = parameters[i], 
                    from = NA,
                    to = NA,
                    mean = mean(results),
                    sd = sd(results),
                    q025 = quantile(results, prob = 0.025),
                    q25 = quantile(results, prob = 0.25),
                    q50 = quantile(results, prob = 0.50),
                    q75 = quantile(results, prob = 0.75),
                    q975 = quantile(results, prob = 0.975),
                    rhat = rstan::Rhat(results),
                    ess_bulk = rstan::ess_bulk(results),
                    ess_tail = rstan::ess_tail(results)
                )
            )
        }
    )
    results <- do.call("rbind", results) |>
        `rownames<-` (NULL) 

    # Split the data up in the part about raw estimations and the part about the 
    # comparison between conditions.
    idx <- grepl("_diff", results$parameter, fixed = TRUE)

    results_fit <- results[!idx, ]
    results_comp <- results[idx, ]

    # For the estimation results, add the observed counts and probabilities and 
    # remove the condition_2 column
    results_fit <- results_fit |>
        dplyr::mutate(
            observed_count = c(as.integer(counts), NA),
            observed_probability = c(as.numeric(prob), NA)
        ) |>
        dplyr::rename(condition = condition_1) |>
        dplyr::select(-condition_2) |>
        dplyr::relocate(
            condition,
            parameter, 
            observed_count, 
            observed_probability
        )
        
    # For the comparison results, change the parameter names to conform to those
    # in the fitting data.frame and add whether 0 is part of the 95%CrI. 
    # Furthermore add the `from` and `to`s from the other data.frame, leveraging 
    # on the same ordering across both
    results_comp <- results_comp |>
        dplyr::mutate(
            parameter = sub("theta_diff", "theta", parameter),
            includes_zero = 0 >= q025 & 0 <= q975
        )

    # Retrieve the index to make the connection to the correct transition
    # (from and to) and add this information to both data.frames separately
    for(i in seq_len(nrow(combos))) {
        # For the fit results
        idt <- grepl(
            rownames(combos)[i],
            results_fit$parameter,
            fixed = TRUE
        )
        results_fit$from[idt] <- combos$s_0[i]
        results_fit$to[idt] <- combos$s_t[i]

        # For the comparison results
        idt <- grepl(
            rownames(combos)[i],
            results_comp$parameter,
            fixed = TRUE
        )
        results_comp$from[idt] <- combos$s_0[i]
        results_comp$to[idt] <- combos$s_t[i]
    }

    # Define the output, save, and return
    output <- list(
        "counts" = counts |>
            `rownames<-` (names(x)) |>
            `colnames<-` (names(x)),
        "probability" = prob |>
            `rownames<-` (names(x)) |>
            `colnames<-` (names(x)),
        "fit" = fit,
        "diagnostics" = diagnostics,
        "summary" = results_fit,
        "summary_per_chain" = rstan::summary(fit),
        "comparison" = results_comp
    )

    if(!is.null(filename)) {
        saveRDS(output, filename)
    }

    return(output)
}

# ANALYSIS 1: 
#   - Across all items
#   - Across all conditions
#   - For each possibility separately
results <- estimate(
    data, 
    model_name = "baseline",
    filename = file.path("results", "baseline.Rds"),
    verbose = TRUE, 
    cores = 4, 
    iter = 50000, 
    warmup = 25000
)

# ANALYSIS 2: 
#   - Across all items
#   - Across all conditions
#   - Midpoint - Valenced
results <- estimate(
    data,
    x = list(
        "midpoint" = c("indifferent", "reluctant", "cautious", "neutral", "ambivalent"),
        "valenced" = c("strongly_disagree", "disagree", "agree", "strongly_agree")
    ),
    model_name = "midpoint",
    filename = file.path("results", "midpoint.Rds"),
    verbose = TRUE, 
    cores = 4, 
    iter = 50000, 
    warmup = 25000
)

# ANALYSIS 3: 
#   - Across all items 
#   - Within condition 
#   - Midpoint - Valenced
results <- estimate(
    data,
    x = list(
        "midpoint" = c("indifferent", "reluctant", "cautious", "neutral", "ambivalent"),
        "valenced" = c("strongly_disagree", "disagree", "agree", "strongly_agree")
    ),
    model_name = "midpoint-condition",
    filename = file.path("results", "midpoint-condition.Rds"),
    conditions = c(
        "control - integration - conflict",
        "control",
        "integration",
        "conflict"
    ),
    verbose = TRUE, 
    cores = 4, 
    iter = 50000, 
    warmup = 25000
)

# ANALYSIS 4: 
#   - Across all items 
#   - Within condition 
#   - Midpoint - Valenced
results <- estimate(
    data,
    x = list(
        "indifferent" = "indifferent", 
        "reluctant" = "reluctant", 
        "cautious" = "cautious", 
        "neutral" = "neutral", 
        "ambivalent" = "ambivalent",
        "valenced" = c("strongly_disagree", "disagree", "agree", "strongly_agree")
    ),
    model_name = "midpoint-differentiated-condition",
    filename = file.path("results", "midpoint-differentiated-condition.Rds"),
    conditions = c(
        "control - integration - conflict",
        "control",
        "integration",
        "conflict"
    ),
    verbose = TRUE, 
    cores = 4, 
    iter = 50000, 
    warmup = 25000
)

# For the final analysis, let's look at the differences between conditions
# that are greater/lower than 0
results$comparison |>
    dplyr::filter(!includes_zero) |>
    View()
