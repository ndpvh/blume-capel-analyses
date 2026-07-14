################################################################################
# Combined figure (single panel): response frequency distribution where the
# neutral bar (4) is subdivided by reason using ggpattern.
################################################################################

library(ggplot2)
library(ggpattern)
library(data.table)

dat <- fread(
    file.path("data", "DEKKER.csv"),
    data.table = FALSE
)

s_cols <- grep("^s_", names(dat), value = TRUE)
n_cols <- grep("^n_", names(dat), value = TRUE)

likert_labels <- c(
    "1" = "Strongly\nDisagree", "2" = "Disagree", "3" = "Somewhat\nDisagree",
    "4" = "Neither\nAgree/Disagree",
    "5" = "Somewhat\nAgree", "6" = "Agree", "7" = "Strongly\nAgree"
)

fill_colors <- c(
    "1" = "#d32f2f", "2" = "#e57373", "3" = "#ef9a9a",
    "4" = "#9E9E9E",
    "5" = "#90CAF9", "6" = "#42A5F5", "7" = "#1565C0"
)

reason_labels <- c("1" = "Both", "2" = "Indifferent", "3" = "Don't understand")

# Add reason colors
reason_colors <- c(
  "none"             = NA,  # will be overridden by response fill
  "Both"             = "#b0b0b0",
  "Indifferent"      = "#6d6d6d",
  "Don't understand" = "#2d2d2d"
)
################################################################################
# Build paired data: match each s_ value to its n_ reason

df_paired <- do.call(rbind, lapply(seq_along(s_cols), function(i) {
    data.frame(s = dat[[s_cols[i]]], n = dat[[n_cols[i]]])
}))
df_paired <- df_paired[!is.na(df_paired$s), ]

# Group: non-neutral rows keep response number as string, neutral get reason label
df_paired$group <- ifelse(
  df_paired$s == 4,
  reason_labels[as.character(df_paired$n)],
  as.character(df_paired$s)
)

df_counts <- aggregate(
  list(count = rep(1, nrow(df_paired))),
  by = list(response = factor(df_paired$s, levels = 1:7), group = df_paired$group),
  FUN = length
)

all_colors <- c(
  fill_colors,
  "Both"             = "#b0b0b0",
  "Indifferent"      = "#6d6d6d",
  "Don't understand" = "#2d2d2d"
)

df_counts$group <- factor(df_counts$group, levels = names(all_colors))

p <- ggplot(df_counts, aes(x = response, y = count, fill = group)) +
  geom_col(color = NA, width = 0.7) +
  scale_fill_manual(
    values = all_colors,
    breaks = c("Both", "Indifferent", "Don't understand"),
    name = "Midpoint Type"
  ) +
  scale_x_discrete(labels = likert_labels) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Response", y = "Count") +
  theme_classic(base_size = 24) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 35, hjust = 1),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.x = element_text(margin = margin(t = 10))
  )
################################################################################
# Save


ggsave(
  "figures/dekker-distributions/Dekker-pooled-stack.png",
  p,
  width = 20,
  height = 10,
  dpi = 300
)


