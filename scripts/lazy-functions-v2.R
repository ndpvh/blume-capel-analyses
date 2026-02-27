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
                          var1 = NULL,
                          var2 = NULL,
                          filter2 = NULL,
                          time_compare = FALSE,
                          time_var1 = NULL,
                          time_var2 = NULL) {
  require(rlang)
  require(dplyr)
  
  filter1 <- enquo(filter1)
  filter2 <- enquo(filter2)
  
  if (time_compare) {
    out <- df %>%
      filter(!!filter1) %>%
      select(all_of(c(time_var1, time_var2))) %>%
      pivot_longer(everything(), names_to = "time", values_to = "value") %>%
      mutate(time = factor(time, levels = c(time_var1, time_var2)))
    
    if (!quo_is_null(filter2)) out <- out %>% filter(!!filter2)
    
    out %>%
      { table(.$time, .$value) } -> plot_table
    barplot(plot_table,
            beside = TRUE,
            col = rep(palette.colors(nrow(plot_table), palette = "Set1"), ncol(plot_table)))
  } else {
    df %>%
      filter(!!filter1) %>%
      { table(.[[var1]], .[[var2]]) } -> plot_table
    barplot(plot_table, beside = TRUE, col = rep(palette.colors(nrow(plot_table), palette = "Set1"), ncol(plot_table)))
  }
}




barplot_maker <- function(df,
                          filter1 = NULL,
                          var1 = NULL,
                          var2 = NULL,
                          filter2 = NULL,
                          time_compare = FALSE,
                          time_var1 = NULL,
                          time_var2 = NULL) {
  require(rlang)
  require(dplyr)
  
  filter1 <- enquo(filter1)
  filter2 <- enquo(filter2)
  filter1_label <- if (quo_is_null(filter1)) NULL else deparse(quo_get_expr(filter1))
  filter2_label <- if (quo_is_null(filter2)) NULL else deparse(quo_get_expr(filter2))
  
  plot_barplot <- function(plot_table, xlab, main) {
    par(mar = c(5, 4, 4, 7))
    on.exit(par(mar = c(5, 4, 4, 2)))
    bp <- barplot(
      plot_table,
      beside = TRUE,
      col = rep(palette.colors(
        nrow(plot_table), palette = "Set1"), 
        ncol(plot_table)),
      xlab = xlab,
      ylab = "Count",
      main = main
    )
    par(xpd = TRUE)
    legend(
      x = max(bp) + 1,
      y = max(plot_table),
      legend = rownames(plot_table),
      fill = palette.colors(nrow(plot_table), palette = "Set1")
    )
    par(xpd = FALSE)
  }
  
  if (time_compare) {
    out <- df %>%
      { if (!quo_is_null(filter1)) filter(., !!filter1) else . } %>%
      select(all_of(c(time_var1, time_var2))) %>%
      pivot_longer(everything(), names_to = "time", values_to = "value") %>%
      mutate(time = factor(time, levels = c(time_var1, time_var2)))
    
    if (!quo_is_null(filter2)) out <- out %>% filter(!!filter2)
    
    plot_table <- table(out$time, out$value)
    
    subtitle <- if (!is.null(filter2_label))
      paste0(filter1_label, " \nvalue filter: ", filter2_label)
    else
      filter1_label
    
    plot_barplot(plot_table, xlab = "Value", main = subtitle)
    
  } else {
    plot_table <- df %>%
      { if (!quo_is_null(filter1)) filter(., !!filter1) else . } %>%
      { table(.[[var1]], .[[var2]]) }
    
    plot_barplot(plot_table, xlab = var2, main = filter1_label)
  }
}

table_maker <- function(df,
                        filter1 = NULL,
                        filter2 = NULL,
                        var1 = NULL,
                        var2 = NULL,
                        show_prop = TRUE) {
  filter1 <- enquo(filter1)
  filter2 <- enquo(filter2)
  filter1_label <- if (quo_is_null(filter1)) NULL else deparse(quo_get_expr(filter1))
  filter2_label <- if (quo_is_null(filter2)) NULL else deparse(quo_get_expr(filter2))
  
  out <- df %>%
    { if (!quo_is_null(filter1)) filter(., !!filter1) else . } %>%
    { if (!quo_is_null(filter2)) filter(., !!filter2) else . } %>%
    { table(.[[var1]], .[[var2]]) }
  
  cat("Filter 1:", filter1_label, "\n")
  cat("Filter 2:", filter2_label, "\n\n")
  cat("Rows:", var1, "| Columns:", var2, "\n")
  print(addmargins(out))
  
  if (show_prop) {
    cat("\nRow proportions:\n")
    print(round(prop.table(out, margin = 1), 2))
  }
  
  invisible(addmargins(out))
}

alluvial_maker <- function(df,
                           filter1 = NULL,
                           filter2 = NULL,
                           time_var1,
                           time_var2) {
  filter1 <- enquo(filter1)
  filter2 <- enquo(filter2)
  
  df %>%
    {
      if (!quo_is_null(filter1))
        filter(., !!filter1)
      else
        .
    } %>%
    {
      if (!quo_is_null(filter2))
        filter(., !!filter2)
      else
        .
    } %>%
    count(.data[[time_var1]], .data[[time_var2]]) %>%
    ggplot(aes(
      axis1 = .data[[time_var1]],
      axis2 = .data[[time_var2]],
      y = n
    )) +
    ggalluvial::geom_alluvium(aes(fill = factor(.data[[time_var1]]))) +
    ggalluvial::geom_stratum() +
    geom_text(stat = ggalluvial::StatStratum, aes(label = paste0(
      after_stat(stratum), "\n(n=", after_stat(count), ")"
    ))) +
    scale_x_discrete(limits = c(time_var1, time_var2)) +
    theme_void()
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
  
  imap_dfr(models, ~ {
    ct <- extract_coef_table(.x)
    
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
    
    tibble(model = model_names[.y], term = clean_terms, as_tibble(sig))
  })
}
