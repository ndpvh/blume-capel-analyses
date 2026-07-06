################################################################################
# PURPOSE: Check the response distributions and whether they are uni-, bi-,
# or trimodal, and what their distribution looks like.
################################################################################

require(remode)
require(dplyr)
library(ggplot2)

################################################################################
# HELPER: cache results to disk
#
# If path already exists, .rds file loaded. Otherwise the model runs and then
# .rds is saved to path.
#
# @param path File path of the .rds cache
# @param expr Expression producing the result (only runs if no .rds found)
#
# @return The cached or freshly computed result
load_or_run <- function(path, expr) {
  if (file.exists(path)) {
    message("Loading cached result from ", path)
    return(readRDS(path))
  }
  
  result <- expr
  dir.create(dirname(path),
             showWarnings = FALSE,
             recursive = TRUE)
  saveRDS(result, path)
  return(result)
}

################################################################################
# PRELIMINARY STUFF

# Loading in the datasets
filenames <- c("DEKKER", "MILLER", "PILOT")
data <- lapply(filenames, function(x)
  data.table::fread(file.path("data", paste0(x, ".csv")), data.table = FALSE)) |>
  `names<-` (filenames)

### NOTE: need to add the "item" column to the MILLER dataset ###

data[["MILLER"]]$item <- ordered(paste(data[["MILLER"]]$part, data[["MILLER"]]$question, sep = "."))


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
  result <- lapply(columns, function(x) {
    # Remove NAs from the data
    values <- data[, x] |>
      na.omit() |>
      as.numeric()
    
    # Create a data.frame with sufficient space for the results
    result <- data.frame(variable = rep(x, length(unique_values)),
                         response = sort(unique_values))
    
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
    for (i in 1:N) {
      # Resample
      y <- sample(values, length(values), replace = TRUE)
      
      # Save
      tbl <- table(y)
      frequencies[i, names(tbl)] <- tbl
    }
    
    result[, c("q025", "q50", "q975")] <- sapply(as.character(unique_values), function(i)
      quantile(frequencies[, i], prob = c(0.025, 0.5, 0.975))) |>
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
  })
  frequency <- do.call("rbind", result)
  
  # Significance of the differences between the frequencies of the responses
  combinations <- cbind(unique_values[2:length(unique_values) - 1], unique_values[2:length(unique_values)])
  
  result <- lapply(columns, function(x) {
    result <- lapply(seq_len(nrow(combinations)), function(i) {
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
      for (j in 1:N) {
        # Resample
        y <- sample(values, length(values), replace = TRUE)
        
        # Save
        tbl <- table(y)
        frequencies[names(tbl)] <- tbl
        
        freq <- frequencies[as.character(combinations[i, ])]
        differences[j] <- freq[1] - freq[2]
      }
      
      # Some summary statistics for the differences
      result$sd <- sd(differences)
      result[, c("q025", "q50", "q975")] <- quantile(differences, prob = c(0.025, 0.5, 0.975))
      
      # Check whether 0 is part of the confidence interval
      result$significant <- result$q025 >= 0 |
        result$q975 <= 0
      
      # Return
      return(result)
    })
    
    return(do.call("rbind", result))
  })
  differences <- do.call("rbind", result)
  
  # Add information to the frequencies about whether they are significantly
  # different from their immediate neighbors.
  frequency$significant <- sapply(seq_len(nrow(frequency)), function(i) {
    # Select those difference responses that are for the same variable
    # and contains the response in either part of the difference
    # question
    idx_1 <- differences$response_1 == frequency$response[i]
    idx_2 <- differences$response_2 == frequency$response[i]
    idx_3 <- differences$variable == frequency$variable[i]
    
    idx <- (idx_1 | idx_2) & idx_3
    
    return(any(differences$significant[idx]))
  })
  
  # Summarize all information in a little data.frame
  summarized <- lapply(columns, function(x) {
    # Select relevant data
    freq <- frequency[frequency$variable == x, ]
    diffs <- differences[differences$variable == x, ]
    
    # Summarize some information here
    return(
      data.frame(
        variable = x,
        number_modes = sum(freq$mode),
        significant_modes = sum(freq$significant &
                                  freq$mode),
        neutral_mode = median(unique_values) %in% unique_values[freq$mode]
      )
    )
  })
  summarized <- do.call("rbind", summarized)
  
  return(list(
    "frequency" = frequency,
    "differences" = differences,
    "summary" = summarized
  ))
}



################################################################################
# DEKKER

idx <- grepl("s_", colnames(data[["DEKKER"]]), fixed = TRUE)
Dekker_result <- load_or_run(
  file.path("results", "modality_dekker.Rds"),
  modality(
    data[["DEKKER"]],
    columns = colnames(data[["DEKKER"]])[idx],
    unique_values = 1:7,
    N = 10000
  )
)

View(Dekker_result$summary)

###### ReMoDe analysis ######

# First gathering up the relevant columns

Dekker_cols <- data[["DEKKER"]] %>% select(starts_with("s_"))

# Looping over each relevant column, removing NAs

Dekker_remode_results <- load_or_run(file.path("results", "remode_dekker.Rds"), {
  results <- lapply(names(Dekker_cols), function(col) {
    x <- na.omit(Dekker_cols[[col]])
    remode(x, format_raw = TRUE)
  })
  names(results) <- names(Dekker_cols)
  results
})

# Quick table format to get a quick overview

Dekker_remode_table <- do.call(rbind, lapply(names(Dekker_remode_results), function(col) {
  r <- Dekker_remode_results[[col]]
  data.frame(
    item = col,
    nr_modes = r$nr_of_modes,
    mode_locations = paste(r$mode_indeces, collapse = ", "),
    p_values = paste(round(r$p_values, 4), collapse = ", "),
    BF = paste(r$approx_bayes_factors, collapse = ", ")
  )
}))

View(Dekker_remode_table)

Dekker_modes_comparison <- data.frame(
  item = Dekker_remode_table$item,
  remode_modes = Dekker_remode_table$nr_modes,
  manual_modes = Dekker_result$summary$number_modes,
  manual_modes_sig = Dekker_result$summary$significant_modes
)

View(Dekker_modes_comparison)

# Visualizing example remode result


likert_labels <- c(
  "1" = "Strongly\nDisagree",
  "2" = "Disagree",
  "3" = "Somewhat\nDisagree",
  "4" = "Neither\nAgree or\nDisagree",
  "5" = "Somewhat\nAgree",
  "6" = "Agree",
  "7" = "Strongly\nAgree"
)

stars <- rep("",
             length(Dekker_remode_results$s_israel$frequency_input_data))
stars[Dekker_remode_results$s_israel$mode_indeces] <- sapply(Dekker_remode_results$s_israel$p_values, function(p) {
  if (p < 0.001)
    "***"
  else if (p < 0.01)
    "**"
  else if (p < 0.05)
    "*"
  else
    ""
})

remode_plot_df <- data.frame(
  bin = names(Dekker_remode_results$s_israel$frequency_input_data),
  freq = as.numeric(Dekker_remode_results$s_israel$frequency_input_data),
  is_mode = seq_along(Dekker_remode_results$s_israel$frequency_input_data) %in% Dekker_remode_results$s_israel$mode_indeces,
  star = stars
)

fig <- ggplot(remode_plot_df, aes(x = bin, y = freq, fill = is_mode)) +
  geom_col() +
  geom_text(aes(label = star), vjust = -0.3, size = 8) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  scale_x_discrete(labels = likert_labels) +
  scale_fill_manual(values = c("grey70", "steelblue"), guide = "none") +
  labs(x = "Response", y = "Frequency") +
  theme_classic(base_size = 24) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(margin = margin(b = 20)),
    axis.title.y = element_text(margin = margin(r = 20))
  )

ggsave(
  "figures/Dekker-remode-israel.png",
  fig,
  width = 16,
  height = 9,
  dpi = 300
)



################################################################################
# MILLER

# Here, we first remove the "don't know" response (response 6)

### HEADS UP: I changed the separator arg to "." rather than the "_" previously
### used, since the "item" column needed to be created previously.

Miller_result_no_idk <- load_or_run(
  file.path("results", "modality_miller.Rds"),
  data[["MILLER"]] |>
    dplyr::select(id, item, attitude) |>
    dplyr::mutate(attitude = ifelse(attitude == 6, NA, attitude)) |>
    tidyr::pivot_wider(
      id_cols = id,
      values_from = attitude,
      names_from = item
    ) |>
    as.data.frame() |>
    modality(
      columns = paste(rep(1:2, each = 4), rep(1:4, times = 2), sep = "."),
      unique_values = 1:5,
      N = 10000
    )
)

View(Miller_result_no_idk$summary)

# Here, we first recode the "don't know" response to a neutral response
# reflecting caution (what the investigators where interested in)

Miller_result_w_idk <- load_or_run(
  file.path("results", "modality_miller_caution.Rds"),
  data[["MILLER"]] |>
    dplyr::select(id, item, attitude) |>
    dplyr::mutate(attitude = ifelse(attitude == 6, 3, attitude)) |>
    tidyr::pivot_wider(
      id_cols = id,
      values_from = attitude,
      names_from = item
    ) |>
    as.data.frame() |>
    modality(
      columns = paste(rep(1:2, each = 4), rep(1:4, times = 2), sep = "."),
      unique_values = 1:5,
      N = 10000
    )
)

View(Miller_result_w_idk$summary)

###### ReMoDe analysis ######

# Getting the columns we are interested; retaining condition for later analysis
# Also recoding 6 to 3, as "I don't know" falls under our definition of "Caution",
# argued as a possible motivator for neutrality

Miller_cols <- data[["MILLER"]] %>%
  dplyr::select(id, part, question, condition, attitude) %>%
  dplyr::mutate(attitude = ifelse(attitude == 6, 3, attitude)) %>%
  tidyr::pivot_wider(
    id_cols = c(id, condition),
    values_from = attitude,
    names_from = c(part, question)
  )

# Removing the id and condition columns
Miller_remode_cols <- Miller_cols[, -c(1, 2)]

# Looping over each relevant column
Miller_remode_results <- load_or_run(file.path("results", "remode_miller.Rds"), {
  results <- lapply(names(Miller_remode_cols), function(col) {
    remode(Miller_remode_cols[[col]], format_raw = TRUE)
  })
  names(results) <- names(Miller_remode_cols)
  results
})

# Table format for quick look-over
Miller_remode_table <- do.call(rbind, lapply(names(Miller_remode_results), function(col) {
  r <- Miller_remode_results[[col]]
  data.frame(
    item = col,
    nr_modes = r$nr_of_modes,
    mode_locations = paste(r$mode_indeces, collapse = ", "),
    p_values = paste(round(as.numeric(r$p_values), 4), collapse = ", "),
    BF = paste(r$approx_bayes_factors, collapse = ", ")
  )
}))

View(Miller_remode_table)

###### Compare manual modality analysis to remode ######

Miller_modes_comparison <- data.frame(
  item = Miller_remode_table$item,
  remode_modes = Miller_remode_table$nr_modes,
  manual_modes = Miller_result_w_idk$summary$number_modes,
  manual_modes_sig = Miller_result_w_idk$summary$significant_modes
)

View(Miller_modes_comparison)

################################################################################
# PILOT

# Define the columns we are interested in
idx <- grepl("_pre", colnames(data[["PILOT"]]), fixed = TRUE)
idy <- grepl("_post", colnames(data[["PILOT"]]), fixed = TRUE)
cols <- colnames(data[["PILOT"]])[idx | idy]

# We first have to recode the the variables, but afterward we can do the mode
# count. In this recoding, we assign a value of 3 to all neutral options of the
# cross-Likert scale
pilot_result <- load_or_run(
  file.path("results", "modality_pilot.Rds"),
  data[["PILOT"]] |>
    dplyr::mutate(
      dplyr::across(hc1_pre:meat5_post, function(x)
        x |>
          dplyr::case_match(
            "strongly_disagree" ~ 1,
            "disagree" ~ 2,
            "agree" ~ 4,
            "strongly_agree" ~ 5,
            .default = 3
          ))
    ) |>
    modality(
      columns = cols,
      unique_values = 1:5,
      N = 10000
    )
)

View(pilot_result$summary)


###### ReMoDe analysis ######


# Data

pilot_cols <- data[["PILOT"]] |>
  dplyr::mutate(dplyr::across(hc1_pre:meat5_post, function(x)
    x |>
      dplyr::case_match(
        "strongly_disagree" ~ 1,
        "disagree" ~ 2,
        "agree" ~ 4,
        "strongly_agree" ~ 5,
        .default = 3
      )))

target_cols <- names(pilot_cols %>% dplyr::select(hc1_pre:meat5_post))

pilot_remode_results <- load_or_run(file.path("results", "remode_pilot.Rds"), {
  results <- lapply(target_cols, function(col) {
    x <- na.omit(pilot_cols[[col]])
    remode(x, format_raw = TRUE)
  })
  names(results) <- target_cols
  results
})

pilot_remode_table <- do.call(rbind, lapply(names(pilot_remode_results), function(col) {
  r <- pilot_remode_results[[col]]
  data.frame(
    item = col,
    nr_modes = r$nr_of_modes,
    mode_locations = paste(r$mode_indeces, collapse = ", "),
    p_values = paste(round(as.numeric(r$p_values), 4), collapse = ", "),
    BF = paste(r$approx_bayes_factors, collapse = ", ")
  )
}))

View(pilot_remode_table)

pilot_modes_comparison <- data.frame(
  item = pilot_remode_table$item,
  remode_modes = pilot_remode_table$nr_modes,
  manual_modes = pilot_result$summary$number_modes,
  manual_modes_sig = pilot_result$summary$significant_modes
)

View(pilot_modes_comparison)