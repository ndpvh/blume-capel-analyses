################################################################################
# Combined figure: response frequency distribution (Panel A) +
# reasons for neutral response (Panel B), DEKKER data only.
################################################################################

library(ggplot2)
library(patchwork)
library(data.table)

dat <- fread(
    file.path("data", "DEKKER.csv"),
    data.table = FALSE
)

s_cols <- grep("^s_", names(dat), value = TRUE)
n_cols <- grep("^n_", names(dat), value = TRUE)

# Pool all s_ values (Panel A)
s_pooled <- unlist(dat[, s_cols], use.names = FALSE)
s_pooled <- s_pooled[!is.na(s_pooled)]

# Pool all n_ values (Panel B)
n_pooled <- unlist(dat[, n_cols], use.names = FALSE)
n_pooled <- n_pooled[!is.na(n_pooled)]

likert_labels <- c("1" = "Strongly\nDisagree", "2" = "Disagree", "3" = "Somewhat\nDisagree", "4" = "Neither\nAgree or\nDisagree", "5" = "Somewhat\nAgree", "6" = "Agree", "7" = "Strongly\nAgree")
reason_labels <- c("1" = "Both", "2" = "Indifferent", "3" = "Don't\nunderstand")

fill_colors <- c(
  "1" = "#d32f2f", "2" = "#e57373", "3" = "#ef9a9a",
  "4" = "#9E9E9E",
  "5" = "#90CAF9", "6" = "#42A5F5", "7" = "#1565C0"
)



################################################################################
# Panel A: Response frequency distribution

df_s <- data.frame(
    response = factor(s_pooled, levels = 1:7)
)

p_a <- ggplot(df_s, aes(x = response, fill = response)) +
  geom_bar(color = "white", width = 0.7) +
  scale_fill_manual(values = fill_colors, guide = "none") +
  scale_x_discrete(labels = likert_labels) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Response", y = "Count") +
  theme_classic(base_size = 24) +
  theme(
    plot.tag = element_text(face = "bold"),
      axis.title.y = element_text(margin = margin(r = 20)),
      axis.title.x = element_text(margin = margin(t = 20))
  )

################################################################################
# Panel B: Reasons for neutral response

df_n <- data.frame(
    reason = factor(
        reason_labels[as.character(n_pooled)],
        levels = reason_labels
    )
)

p_b <- ggplot(df_n, aes(x = reason)) +
    geom_bar(
        color = "white",
        width = 0.6
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(
        x = "Reason for neutral response",
        y = ""
    ) +
    theme_classic(base_size = 24) +
    theme(
        plot.tag = element_text(face = "bold"),
        axis.line.x = element_line(),
        axis.line.y = element_line(),
        axis.title.x = element_text(margin = margin(t = 10))
    )

################################################################################
# Combine and save
fig <- p_a + p_b +
  plot_layout(widths = c(2, 1))

ggsave("figures/Dekker-pooled.png", fig, width = 20, height = 10, dpi = 300)
