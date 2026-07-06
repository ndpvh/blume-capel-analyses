################################################################################
# Combined figure (single panel, dodged): response frequency distribution
# where the midpoint bar (3/6/7/8/9) is subdivided by subtype using ggpattern,
# bars clustered by experimental condition.
################################################################################

library(ggplot2)
library(ggpattern)
source("scripts/preprocessing.R")

condition_labels <- c("1" = "Control", "2" = "Ambivalence", "3" = "Integration")

subtype_labels <- c(
  "3" = "Indifference",
  "6" = "Caution",
  "7" = "Integration",
  "8" = "Ambivalence",
  "9" = "Reluctance"
)

response_labels <- c(
  "1" = "Strongly\nDisagree",
  "2" = "Disagree",
  "3" = "Midpoint",
  "4" = "Agree",
  "5" = "Strongly\nAgree"
)

condition_colors <- c(
  "Control" = "#4CAF50",
  "Ambivalence" = "#FF9800",
  "Integration" = "#2196F3"
)

################################################################################
# Build data: collapse midpoints, assign subtype group

df_post <- all_changes[!is.na(all_changes$Post), ]
df_post$response_simple <- ifelse(df_post$Post %in% c(3, 6, 7, 8, 9), 3L, as.integer(df_post$Post))
df_post$Condition_label <- factor(condition_labels[as.character(df_post$Condition)], levels = condition_labels)
df_post$group <- ifelse(df_post$Post %in% c(3, 6, 7, 8, 9), subtype_labels[as.character(df_post$Post)], "none")

df_counts <- aggregate(
  list(count = rep(1, nrow(df_post))),
  by = list(
    response        = factor(df_post$response_simple, levels = 1:5),
    group           = df_post$group,
    Condition_label = df_post$Condition_label
  ),
  FUN = length
)

group_levels <- c("none", subtype_labels)
df_counts$group <- factor(df_counts$group, levels = group_levels)

################################################################################
# Pre-compute stacked y-positions within each (response, Condition)

df_counts <- df_counts[order(df_counts$response,
                             df_counts$Condition_label,
                             df_counts$group), ]
df_counts <- do.call(rbind, lapply(split(
  df_counts,
  list(df_counts$response, df_counts$Condition_label),
  drop = TRUE
), function(d) {
  d$ymax <- cumsum(d$count)
  d$ymin <- d$ymax - d$count
  d
}))
rownames(df_counts) <- NULL

# Manually compute dodged x-positions
n_cond <- 3
dodge_width <- 0.7
bar_half <- dodge_width / n_cond / 2 * 0.9
df_counts$x_num <- as.numeric(df_counts$response)
df_counts$x_dodge <- df_counts$x_num +
  (as.numeric(df_counts$Condition_label) - 2) * dodge_width / n_cond

################################################################################
# Plot

p <- ggplot(
  df_counts,
  aes(
    xmin = x_dodge - bar_half,
    xmax = x_dodge + bar_half,
    ymin = ymin,
    ymax = ymax,
    fill = Condition_label,
    pattern = group,
    pattern_angle = group
  )
) +
  geom_rect_pattern(
    color = "white",
    linewidth = 0.1,
    pattern_fill = "gray",
    pattern_color = NA,
    pattern_density = 0.3,
    pattern_spacing = 0.03
  ) +
  scale_fill_manual(
    values = condition_colors,
    name   = "Condition",
    guide  = guide_legend(override.aes = list(pattern = "none"))
  ) +  scale_pattern_manual(
    values = c(
      "none"                  = "none",
      "Indifference"          = "stripe",
      "Caution"               = "crosshatch",
      "Integration"           = "stripe",
      "Ambivalence"           = "circle",
      "Reluctance"            = "none"
    ),
    name   = "Midpoint\nsubtype",
    breaks = subtype_labels,
    labels = subtype_labels,
    guide  = guide_legend(override.aes = list(
      pattern_spacing = 0.01,
      pattern_angle   = c(45, 45, -45, 0, 0)
    ))
  ) +
  scale_pattern_angle_manual(
    values = c(
      "none"                  = 0,
      "Indifference"          = 45,
      "Caution"               = 45,
      "Integration"           = -45,
      "Ambivalence"           = 0,
      "Reluctance"            = 0
    ),
    guide = "none"
  ) +
  scale_x_continuous(breaks = 1:5, labels = response_labels) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Response", y = "Count") +
  theme_classic(base_size = 12) +
  theme(
    plot.tag = element_text(face = "bold"),
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
  file.path("figures", "Post-pooled-pattern-dodged.png"),
  p,
  width = 7,
  height = 4,
  dpi = 300
)
