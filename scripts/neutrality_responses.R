################################################################################
# PURPOSE: Perform analyses targeted at examining the neutral responses 
#          themselves, checking which reasons participants gave for midpoint 
#          responding.
#
#          Performed on the MILLER, DEKKER, and PILOT datasets. Additionally, 
#          for the DEKKER and PILOT datasets, we also look at the relationship
#          between importance and midpoint responding.
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

# Get the number of neutral responses per item
data[["DEKKER"]] |>
    dplyr::summarize(
        dplyr::across(
            tidyselect::contains("n_"),
            function(x) sum(!is.na(x)) / length(x)
        )
    )

    ## Interpretation: Around 10-30% of the responses were neutral across all 
    ## items


# What reasons did people give for their neutral responses?
neutral <- data[["DEKKER"]] |>
    dplyr::select(tidyselect::contains("n_")) |>
    dplyr::mutate(
        dplyr::across(
            tidyselect::everything(),
            function(x) x |>
                dplyr::case_match(
                    1 ~ "no opinion",
                    2 ~ "neutral or ambivalent",
                    3 ~ "don't understand"
                )
        )
    )
neutral <- lapply(
    seq_len(ncol(neutral)),
    function(i) data.frame(
        col = colnames(neutral)[i],
        no_opinion = sum(neutral[, i] == "no opinion", na.rm = TRUE),
        neutral = sum(neutral[, i] == "neutral or ambivalent", na.rm = TRUE),
        understand = sum(neutral[, i] == "don't understand", na.rm = TRUE),
        not_available = sum(is.na(neutral[, i]))
    )
)
neutral <- do.call("rbind", neutral)

View(neutral)

    ## Interpretation: Accounting for a number of people who don't understand 
    ## the question, we find that about 50/50 of the reasons people selected the
    ## neutral response was because they were indifferent or because they had a 
    ## neutral or ambivalent opinion. The two are thus distinct.


# What is the relationship between indifference/neutral responding and 
# importance?
importance <- data[["DEKKER"]] |>
    dplyr::select(
        tidyselect::contains("n_"),
        tidyselect::contains("m_")
    ) |>
    dplyr::mutate(
        dplyr::across(
            tidyselect::contains("n_"),
            function(x) x |>
                dplyr::case_match(
                    1 ~ "no opinion",
                    2 ~ "neutral or ambivalent",
                    3 ~ "don't understand"
                )
        )
    )

items <- c(
    "klimaat", "vlees", "immigratie", "vluchtelingen", "belasting", 
    "israel", "gender", "leider", "kansen", "mobiel", "paarsevrijdag", 
    "voorlichting", "toiletten", "roddel", "stemmen"
)
result <- lapply(
    items, 
    function(x) {
        result <- importance[, paste0(c("n_", "m_"), x)] |>
            `colnames<-` (c("response", "importance")) |>
            dplyr::filter(!(is.na(response) | response == "don't understand")) |>
            dplyr::mutate(
                response = as.numeric(response == "neutral or ambivalent")
            ) 

        corr <- cor.test(
            result$response, 
            result$importance
        )

        return(
            data.frame(
                issue = x, 
                estimate = corr$estimate, 
                q025 = corr$conf.int[1],
                q975 = corr$conf.int[2],
                p_value = corr$p.value,
                sample_size = nrow(result),
                mean_importance = mean(result$importance),
                sd_importance = sd(result$importance)
            )
        )
    }
)
result <- do.call("rbind", result)

View(result)

    ## Interpretation: All correlations are positive and most of them are 
    ## statistically significant (with the exception of "immigratie" and 
    ## "voorlichting"), indicating that there is a relationship between the 
    ## importance of a topic and the reason for choosing the midpoint response. 
    ## Specifically, if importance is high, then people are more likely to 
    ## select the midpoint response when they are neutral/ambivalent about a 
    ## topic.

    

################################################################################
# MILLER

# Get the number of neutral responses when people answered neutrally
sum(data[["MILLER"]]$neutrality_reason != "") / nrow(data[["MILLER"]]) 

    ## Interpretation: About 6-7% of the items were responded to neutrally across
    ## people


# What reasons did people give for responding neutrally
frequencies <- data[["MILLER"]] |>
    dplyr::filter(neutrality_reason != "") |>
    dplyr::select(neutrality_reason) |>
    unlist() |>
    as.character() |>
    table()
frequencies / sum(frequencies)

    ## Interpretation: By far, most people indicated that they responded neutrally
    ## out of caution (about 90%), then reluctance (about 8%), and finally 
    ## indifference (about 3%).


# Get the number of uncertainty responses
sum(data[["MILLER"]]$uncertainty_reason != "") / nrow(data[["MILLER"]]) 

    ## Interpretation: About 4-5% of the items were responded to uncertainly
    ## across people


# What reasons did people give for responding uncertainly, yet valenced
frequencies <- data[["MILLER"]] |>
    dplyr::filter(uncertainty_reason != "") |>
    dplyr::select(uncertainty_reason) |>
    unlist() |>
    as.character() |>
    table()
frequencies / sum(frequencies)

    ## Interpretation: Results are similar to the results for midpoint 
    ## responding. Interestingly, however, indifference disappeared.


# What is the relationship between topic importance and the indifference or 
# neutrality of the midpoint response?
importance <- data[["MILLER"]] |>
    dplyr::filter(neutrality_reason != "") |>
    dplyr::select(topic_importance, neutrality_reason) |>
    dplyr::mutate(
        neutrality_reason = as.numeric(
            neutrality_reason %in% c("caution", "reluctance")
        )
    )
cor.test(importance$topic_importance, importance$neutrality_reason)

    ## Interpretation: There is a significant correlation between topic 
    ## importance and choosing caution/reluctance as reasons for midpoint 
    ## responding. Suggests, like in the DEKKER data, that involved neutrality
    ## exists.
    ## 
    ## However, take this with a grain of salt: There is a big imbalance in the 
    ## number of indifference and caution/reluctance answers. Additionally, I 
    ## did not account for repeated measures, as some people may have indicated 
    ## a neutral response multiple times throughout the experiment.



################################################################################
# PILOT

# Get the number of neutral responses for each of the items (pre/post separate)
data[["PILOT"]] |>
    dplyr::summarize(
        dplyr::across(
            hc1_pre:meat5_post,
            function(x) sum(!(x %in% c("strongly_disagree", "disagree", "agree", "strongly_agree"))) / length(x)
        )
    )

    ## Interpretation: Around 5-28% of the items were responded to neutrally 
    ## across people.


# What reasons did people give for responding neutrally
items <- expand.grid(
    c(
        paste0("hc", 1:3),
        paste0("tariff", 1:3),
        paste0("costs", 1:3),
        paste0("meat", 1:5)
    ),
    c("_pre", "_post")
)
items <- sapply(
    seq_len(nrow(items)), 
    function(i) items[i, ] |>
        unlist() |>
        as.character() |>
        paste(collapse = "")
)

result <- lapply(
    items, 
    function(x) {
        N <- sum(data[["PILOT"]][, x] %in% c("neutral", "reluctant", "cautious", "ambivalent", "indifferent"))
        
        return(
            data.frame(
                col = x,
                neutral = sum(data[["PILOT"]][, x] == "neutral", na.rm = TRUE) / N,
                reluctant = sum(data[["PILOT"]][, x] == "reluctant", na.rm = TRUE) / N,
                cautious = sum(data[["PILOT"]][, x] == "cautious", na.rm = TRUE) / N,
                ambivalent = sum(data[["PILOT"]][, x] == "ambivalent", na.rm = TRUE) / N,
                indifferent = sum(data[["PILOT"]][, x] == "indifferent", na.rm = TRUE) / N
            )
        )
    }
)
result <- do.call("rbind", result)

View(result)

    ## Interpretation: Several different reasons were given for the neutral 
    ## response, with caution, ambivalence, and indifference taking the cake.
    ## Interestingly, "true neutrality" was not indicated that often.


# What is the relationship between topic importance and the indifference or 
# neutrality of the midpoint response? Perform this separately for the pre and 
# post questions, but across the different conditions
cols <- expand.grid(
    c(rep("econ", 3), "meat"), 
    c("importance", "personal_relevance")
)
cols <- sapply(
    seq_len(nrow(cols)), 
    function(i) cols[i, ] |>
        unlist() |>
        as.character() |>
        paste(collapse = "_")
)
cols <- data.frame(
    item = rep(c("hc", "tariff", "costs", "meat"), times = 2),
    importance = cols
)

result <- lapply(
    seq_len(nrow(cols)), 
    function(i) {
        idx <- which(
            grepl(
                cols[i, 1],
                colnames(data[["PILOT"]]),
                fixed = TRUE
            )
        )
        items <- colnames(data[["PILOT"]])[idx]

        result <- lapply(
            items, 
            function(x) {
                result <- data[["PILOT"]][, c(x, cols[i, 2])] |>
                    `colnames<-` (c("response", "importance")) |>
                    dplyr::filter(!grepl("agree", response, fixed = TRUE)) |>
                    dplyr::mutate(
                        response = as.numeric(response != "indifferent")
                    )
                
                corr <- cor.test(
                    result$response, 
                    result$importance
                )

                return(
                    data.frame(
                        item = x, 
                        importance = strsplit(cols[i, 2], "_", fixed = TRUE)[[1]][2],
                        estimate = corr$estimate, 
                        q025 = corr$conf.int[1],
                        q975 = corr$conf.int[2],
                        p_value = corr$p.value,
                        sample_size = nrow(result),
                        mean_importance = mean(result$importance),
                        sd_importance = sd(result$importance)
                    )
                )
            }
        )

        return(do.call("rbind", result))        
    }
)
result <- do.call("rbind", result) |>
    dplyr::filter(!is.na(estimate))

View(result)

    ## Interpretation: Most correlations are not statistically significant, 
    ## but those that are are positive, conforming to expectations. Most of these
    ## belong to the "importance" question rather than the question on 
    ## personal relevance. Interestingly, some correlations tend to the opposite 
    ## side of the expectation, so that they are negative rather than positive. 
    ## 
    ## Take this analysis with a grain of salt. Sample sizes on which the 
    ## correlations are based are generally quite low, which is reflected in the 
    ## very wide confidence intervals. Additionally, some cases contained 
    ## standard deviations of 0, making it impossible to calculate the 
    ## correlation coefficient.