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
################################################################################
# Build paired data: match each s_ value to its n_ reason

df_paired <- do.call(rbind, lapply(seq_along(s_cols), function(i) {
    data.frame(s = dat[[s_cols[i]]], n = dat[[n_cols[i]]])
}))
df_paired <- df_paired[!is.na(df_paired$s), ]

# Assign reason group: for neutral responses use reason label, else "none"
df_paired$group <- ifelse(
    df_paired$s == 4,
    reason_labels[as.character(df_paired$n)],
    "none"
)

# Counts per (response, group)
df_counts <- aggregate(
    list(count = rep(1, nrow(df_paired))),
    by = list(response = factor(df_paired$s, levels = 1:7), group = df_paired$group),
    FUN = length
)

# Factor levels: "none" first so it sits at the bottom of the neutral stack
group_levels <- c("none", reason_labels)
df_counts$group <- factor(df_counts$group, levels = group_levels)

################################################################################
# Plot

p <- ggplot(df_counts, aes(x = response, y = count, fill = response, pattern = group)) +
    geom_col_pattern(
        color = "white",
        linewidth = 0.1,
        width = 0.7,
        pattern_fill = "gray",
        pattern_color = "NA",
        pattern_density = 0.3,
        pattern_spacing = 0.03,
        pattern_angle = 45
    ) +
    scale_fill_manual(values = fill_colors, guide = "none") +
    scale_pattern_manual(
        values = c(
            "none"              = "none",
            "Both"              = "stripe",
            "Indifferent"       = "crosshatch",
            "Don't understand" = "circle"
        ),
        name = "Reason for\nneutral response",
        breaks = c("Both", "Indifferent", "Don't understand"),
        labels = c("Both", "Indifferent", "Don't understand")
    ) +
    scale_x_discrete(labels = likert_labels) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(x = "Response", y = "Count") +
    theme_classic(base_size = 12) +
    theme(
        legend.position = "right",
        legend.key.size = unit(0.4, "cm"),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9),
        axis.text.x = element_text(angle = 35, hjust = 1),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10))
    )

################################################################################
# Save


ggsave(
    file.path("figures", "Dekker-pooled-pattern.png"),
    p, width = 7, height = 4, dpi = 300
)


