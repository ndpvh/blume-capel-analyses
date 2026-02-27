local({
  pkgs <- c("stringr", "rlang", "dplyr", "tidyr", "ggplot2")
  invisible(lapply(pkgs, function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
    library(pkg, character.only = TRUE)
  }))
  if (!requireNamespace("ggalluvial", quietly = TRUE)) {
    install.packages("ggalluvial", repos = "https://cloud.r-project.org")
  }
  library(ggalluvial)
})


recode_items <- function(dat) {
  library(stringr)
  
  map <- c(`1` = 6, `2` = 7, `4` = 8, `5` = 9)
  
  col_3 <- grep("_3$", names(dat), value = TRUE)
  
  for (c3 in col_3) {
    
    empty_rows <- which(is.na(dat[[c3]]) | dat[[c3]] == "")
    if (length(empty_rows) == 0) next
    
    base <- str_replace(c3, "_3$", "_")
    
    for (n2 in names(map)) {
      c_other <- paste0(base, n2)
      if (!c_other %in% names(dat)) next
      
      hit <- empty_rows[
        !is.na(dat[[c_other]][empty_rows]) &
          dat[[c_other]][empty_rows] != ""
      ]
      
      dat[[c3]][hit] <- map[n2]
      empty_rows <- setdiff(empty_rows, hit)
      if (length(empty_rows) == 0) break
    }
  }
  
  dat
}


trim_items <- function(dat) {
  
  is_item <- grepl("_[1-5]$", names(dat))
  is_3    <- grepl("_3$", names(dat))
  
  dat_out <- dat[, !is_item | is_3, drop = FALSE]
  
  # recompute on the reduced data
  is_3_out <- grepl("_3$", names(dat_out))
  names(dat_out)[is_3_out] <- sub("_3$", "", names(dat_out)[is_3_out])
  
  dat_out
}


barplot_maker <- function(df,
                          filter1 = NULL,
                          filter2 = NULL,
                          var1 = NULL,
                          var2 = NULL,
                          time_compare = FALSE,
                          time_var1 = NULL,
                          time_var2 = NULL,
                          use_prop = FALSE,
                          pre_summarized = FALSE,
                          ymax = NULL,
                          ylab = NULL,
                          legend_labels = NULL,
                          margins = c(5, 4, 2, 7)) {
  require(rlang)
  require(dplyr)
  
  filter1 <- enquo(filter1)
  filter2 <- enquo(filter2)
  filter1_label <- if (quo_is_null(filter1)) NULL else deparse(quo_get_expr(filter1))
  filter2_label <- if (quo_is_null(filter2)) NULL else deparse(quo_get_expr(filter2))
  
  plot_barplot <- function(plot_table, xlab, main) {
    auto_ylab <- ylab %||% if (use_prop) "Proportion" else "Count"
    auto_ymax <- if (is.null(ymax)) max(plot_table) * 1.25 else ymax
    par(mar = margins)
    on.exit(par(mar = c(5, 4, 4, 2)))
    n_bars <- nrow(plot_table) * ncol(plot_table) + ncol(plot_table) - 1
    bp_width <- n_bars * 1
    bp <- barplot(
      plot_table,
      beside = TRUE,
      col = rep(palette.colors(nrow(plot_table), palette = "Set1"), ncol(plot_table)),
      xlab = xlab,
      ylab = auto_ylab,
      main = main,
      xlim = c(0, n_bars * 1),
      ylim = c(0, auto_ymax)
    )
    par(xpd = TRUE)
    legend(
      x = max(bp) + 1,
      y = auto_ymax,
      legend = legend_labels %||% rownames(plot_table),
      fill = palette.colors(nrow(plot_table), palette = "Set1")
    )
    par(xpd = FALSE)
  }
  
  if (pre_summarized) {
    mat <- t(as.matrix(df[, var2]))
    colnames(mat) <- df[[var1]]
    plot_barplot(mat, xlab = var1, main = NULL)
    return(invisible(mat))
  }
  
  prepare_table <- function(plot_table) {
    if (use_prop) round(prop.table(plot_table, margin = 1), 2) else plot_table
  }
  
  if (time_compare) {
    out <- df %>%
      { if (!quo_is_null(filter1)) filter(., !!filter1) else . } %>%
      select(all_of(c(time_var1, time_var2))) %>%
      pivot_longer(everything(), names_to = "time", values_to = "value") %>%
      mutate(time = factor(time, levels = c(time_var1, time_var2)))
    
    if (!quo_is_null(filter2)) out <- out %>% filter(!!filter2)
    
    plot_table <- prepare_table(table(out$time, out$value))
    subtitle <- if (!is.null(filter2_label))
      paste0(filter1_label, "\nvalue filter: ", filter2_label)
    else
      filter1_label
    
    plot_barplot(plot_table, xlab = "Value", main = subtitle)
    
  } else {
    plot_table <- df %>%
      { if (!quo_is_null(filter1)) filter(., !!filter1) else . } %>%
      { table(.[[var1]], .[[var2]]) } %>%
      prepare_table()
    
    plot_barplot(plot_table, xlab = var2, main = filter1_label)
  }
}


table_maker <- function(df,
                        filter1 = NULL,
                        filter2 = NULL,
                        var1 = NULL,
                        var2 = NULL,
                        stats = "counts") {
  filter1 <- enquo(filter1)
  filter2 <- enquo(filter2)
  filter1_label <- if (quo_is_null(filter1)) NULL else deparse(quo_get_expr(filter1))
  filter2_label <- if (quo_is_null(filter2)) NULL else deparse(quo_get_expr(filter2))
  
  filtered <- df %>%
    { if (!quo_is_null(filter1)) filter(., !!filter1) else . } %>%
    { if (!quo_is_null(filter2)) filter(., !!filter2) else . }
  
  cat("Filter 1:", filter1_label, "\n")
  cat("Filter 2:", filter2_label, "\n\n")
  cat("Rows:", var1, "| Columns:", var2, "\n")
  
  if (stats == "counts") {
    out <- filtered %>%
      group_by(.data[[var1]], .data[[var2]]) %>%
      summarise(n = n(), .groups = "drop") %>%
      pivot_wider(names_from = all_of(var2), values_from = n, values_fill = 0) %>%
      mutate(Sum = rowSums(across(where(is.numeric)))) %>%
      bind_rows(summarise(., across(where(is.numeric), sum),
                          "{var1}" := "Sum"))
    print(out)
    invisible(out)
    
  } else if (stats == "props") {
    out <- filtered %>%
      group_by(.data[[var1]], .data[[var2]]) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(.data[[var1]]) %>%
      mutate(prop = round(n / sum(n), 2)) %>%
      select(-n) %>%
      pivot_wider(names_from = all_of(var2), values_from = prop, values_fill = 0)
    print(out)
    invisible(out)
    
  } else if (stats == "means") {
    out <- filtered %>%
      bind_rows(mutate(., "{var1}" := "Overall")) %>%
      group_by(.data[[var1]]) %>%
      summarise(across(all_of(var2), ~ round(mean(.x, na.rm = TRUE), 2)))
    print(out)
    invisible(out)
    
  } else {
    stop("stats must be 'counts', 'props', or 'means'")
  }
}


alluvial_maker <- function(df,
                           filter1 = NULL,
                           filter2 = NULL,
                           time_var1,
                           time_var2,
                           text_size = NULL) {
  filter1 <- enquo(filter1)
  filter2 <- enquo(filter2)
  
  filtered_df <- df %>%
    { if (!quo_is_null(filter1)) filter(., !!filter1) else . } %>%
    { if (!quo_is_null(filter2)) filter(., !!filter2) else . }
  
  n_strata <- max(
    length(unique(filtered_df[[time_var1]])),
    length(unique(filtered_df[[time_var2]]))
  )
  
  if (is.null(text_size)) text_size <- 20 / n_strata
  
  filtered_df %>%
    count(.data[[time_var1]], .data[[time_var2]]) %>%
    ggplot(aes(axis1 = .data[[time_var1]], axis2 = .data[[time_var2]], y = n)) +
    ggalluvial::geom_alluvium(aes(fill = factor(.data[[time_var1]]))) +
    ggalluvial::geom_stratum() +
    geom_text(stat = ggalluvial::StatStratum, aes(label = paste0(
      after_stat(stratum), "\n(n=", after_stat(count), ")"
    )), size = text_size) +
    scale_x_discrete(limits = c(time_var1, time_var2)) +
    theme_void() + theme(plot.margin = margin(1, 1, 1, 1))
}

extract_coef_table <- function(x) {
  if (inherits(x, "anova")) {
    as.data.frame(x)
  } else {
    as.data.frame(coef(x))
  }
}

sig_coefs <- function(..., p = 0.05) {
  models <- list(...)
  call_args <- as.list(match.call())[-1]
  model_names <- sapply(call_args[names(call_args) == ""], deparse)
  
  results <- vector("list", length(models))
  for (i in seq_along(models)) {
    ct <- extract_coef_table(models[[i]])
    
    # normalize column names
    colnames(ct) <- colnames(ct) %>%
      gsub("LR Chisq", "Chisq", .) %>%
      gsub("Std. Error", "SE", .)
    
    p_col <- grep("Pr\\(>", colnames(ct), value = TRUE)
    sig <- ct[ct[, p_col] <= p & rownames(ct) != "(Intercept)", , drop = FALSE]
    sig <- sig[, !colnames(sig) %in% c("Df", "df"), drop = FALSE]
    sig[, p_col] <- round(sig[, p_col], 3)
    other_cols <- setdiff(colnames(sig), p_col)
    sig[, other_cols] <- round(sig[, other_cols], 2)
    
    # clean term names
    clean_terms <- rownames(sig) %>%
      gsub("scale\\(([^)]+)\\)", "\\1", .)
    
    results[[i]] <- tibble(model = model_names[i], term = clean_terms, as_tibble(sig))
  }
  
  do.call(rbind, results)
}
