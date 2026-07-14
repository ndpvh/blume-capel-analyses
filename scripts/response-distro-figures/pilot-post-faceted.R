################################################################################
# Combined figure: response frequency distribution (Panel A) +
# midpoint subtype distribution (Panel B), post-intervention data,
# clustered by experimental condition.
################################################################################

library(ggplot2)
library(patchwork)
library(ggnewscale)

source("scripts/preprocessing.R")

condition_labels <- c("1" = "Control", "2" = "Conflict", "3" = "Integration")
condition_colors <- c(
  "Control" = "#4CAF50",
  "Conflict" = "#FF9800",
  "Integration" = "#2196F3"
)

response_labels <- c(
  "1" = "Strongly\nDisagree",
  "2" = "Disagree",
  "3" = "Midpoint",
  "4" = "Agree",
  "5" = "Strongly\nAgree"
)

subtype_labels <- c(
  "3" = "Indifference",
  "6" = "Caution",
  "7" = "Integration",
  "8" = "Ambivalence",
  "9" = "Reluctance"
)

reason_colors <- c(
  "none"         = NA,
  "Indifference" = "#ffffff",  # 255
  "Caution"      = "#cccccc",  # 204
  "Integration"  = "#999999",  # 153
  "Ambivalence"  = "#666666",  # 102
  "Reluctance"   = "#333333" # 51
)

df_post_econ <- econ_changes[!is.na(econ_changes$Post), ]
df_post_econ$response_simple <- ifelse(df_post_econ$Post %in% c(3, 6, 7, 8, 9), 3L, as.integer(df_post_econ$Post))
df_post_econ$Condition_label <- factor(condition_labels[as.character(df_post_econ$Condition)], levels = condition_labels)
df_post_econ$group <- ifelse(df_post_econ$Post %in% c(3, 6, 7, 8, 9), subtype_labels[as.character(df_post_econ$Post)], "none")
df_post_econ$group <- factor(df_post_econ$group, levels = c("none", subtype_labels))

df_econ_counts <- aggregate(
  list(count = rep(1, nrow(df_post_econ))),
  by = list(
    response        = factor(df_post_econ$response_simple, levels = 1:5),
    group           = df_post_econ$group,
    Condition_label = df_post_econ$Condition_label
  ),
  FUN = length
)

group_levels <- c("none", subtype_labels)
df_econ_counts <- df_econ_counts[order(df_econ_counts$response,
                             df_econ_counts$Condition_label,
                             df_econ_counts$group), ]
df_econ_counts <- do.call(rbind, lapply(split(
  df_econ_counts,
  list(df_econ_counts$response, df_econ_counts$Condition_label),
  drop = TRUE
), function(d) {
  d$ymax <- cumsum(d$count)
  d$ymin <- d$ymax - d$count
  d
}))
rownames(df_econ_counts) <- NULL

n_cond <- 3
dodge_width <- 0.7
bar_half <- dodge_width / n_cond / 2 * 0.9
df_econ_counts$x_num <- as.numeric(df_econ_counts$response)
df_econ_counts$x_dodge <- df_econ_counts$x_num +
  (as.numeric(df_econ_counts$Condition_label) - 2) * dodge_width / n_cond

df_econ_counts$bar_fill <- ifelse(
  df_econ_counts$group == "none",
  condition_colors[as.character(df_econ_counts$Condition_label)],
  reason_colors[as.character(df_econ_counts$group)]
)

################################################################################
# Panel A: Response frequency distribution

gray_bars <- df_econ_counts[df_econ_counts$group != "none", ]
df_midpoint_outline <- do.call(rbind, lapply(
  split(gray_bars, list(gray_bars$x_dodge, gray_bars$Condition_label), drop = TRUE),
  function(d) data.frame(
    x_dodge         = d$x_dodge[1],
    Condition_label = d$Condition_label[1],
    ymin            = min(d$ymin),
    ymax            = max(d$ymax)
  )
))

df_econ_counts <- df_econ_counts[order(df_econ_counts$response,
                                       df_econ_counts$Condition_label,
                                       df_econ_counts$group), ]
df_econ_counts <- do.call(rbind, lapply(split(
  df_econ_counts,
  list(df_econ_counts$response, df_econ_counts$Condition_label),
  drop = TRUE
), function(d) {
  d$ymax <- cumsum(d$count)
  d$ymin <- d$ymax - d$count
  d
}))
rownames(df_econ_counts) <- NULL

econ_plot <- ggplot(
  df_econ_counts,
  aes(
    xmin = x_dodge - bar_half,
    xmax = x_dodge + bar_half,
    ymin = ymin,
    ymax = ymax,
    fill = Condition_label
  )
) +
  geom_hline(yintercept = seq(0, 250, by = 25), color = "grey85", linewidth = 0.3)+
  geom_rect(color = "white", linewidth = 0.1) +
  scale_fill_manual(values = condition_colors, name = "Condition") +
  new_scale_fill() +
  geom_rect(
    data = function(d) d[d$group != "none", ],
    aes(fill = group),
    color = "white",
    linewidth = 0.1
  ) +
  scale_fill_manual(
    values = reason_colors,
    name   = "Midpoint Type",
    breaks = subtype_labels,
    labels = subtype_labels,
    guide = guide_legend(
      reverse = TRUE,
      override.aes = list(color = c(NA, NA, NA, NA, "black"))
    )
  ) +
  geom_rect(
    data = df_midpoint_outline,
    aes(
      xmin  = x_dodge - bar_half,
      xmax  = x_dodge + bar_half,
      ymin  = ymin,
      ymax  = ymax,
      color = Condition_label
    ),
    fill         = NA,
    linewidth    = 1,
    inherit.aes  = FALSE
  ) +
  scale_color_manual(values = condition_colors, guide = "none") +
  scale_x_continuous(breaks = 1:5, labels = response_labels) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Response", y = "Count") +
  theme_classic(base_size = 24) +
  theme(
    plot.tag = element_text(face = "bold"),
    legend.position = "right",

    axis.text.x = element_text(angle = 35, hjust = 1),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.x = element_text(margin = margin(t = 10))
  )
################################################################################
# Panel B: Midpoint subtype distribution

df_post_meat <- meat_changes[!is.na(meat_changes$Post), ]
df_post_meat$response_simple <- ifelse(df_post_meat$Post %in% c(3, 6, 7, 8, 9), 3L, as.integer(df_post_meat$Post))
df_post_meat$Condition_label <- factor(condition_labels[as.character(df_post_meat$Condition)], levels = condition_labels)
df_post_meat$group <- ifelse(df_post_meat$Post %in% c(3, 6, 7, 8, 9), subtype_labels[as.character(df_post_meat$Post)], "none")
df_post_meat$group <- factor(df_post_meat$group, levels = c("none", subtype_labels))


df_meat_counts <- aggregate(
  list(count = rep(1, nrow(df_post_meat))),
  by = list(
    response        = factor(df_post_meat$response_simple, levels = 1:5),
    group           = df_post_meat$group,
    Condition_label = df_post_meat$Condition_label
  ),
  FUN = length
)

df_meat_counts$x_num <- as.numeric(df_meat_counts$response)
df_meat_counts$x_dodge <- df_meat_counts$x_num +
  (as.numeric(df_meat_counts$Condition_label) - 2) * dodge_width / n_cond

df_meat_counts$bar_fill <- ifelse(
  df_meat_counts$group == "none",
  condition_colors[as.character(df_meat_counts$Condition_label)],
  reason_colors[as.character(df_meat_counts$group)]
)

df_meat_counts <- df_meat_counts[order(df_meat_counts$response,
                                       df_meat_counts$Condition_label,
                                       df_meat_counts$group), ]
df_meat_counts <- do.call(rbind, lapply(split(
  df_meat_counts,
  list(df_meat_counts$response, df_meat_counts$Condition_label),
  drop = TRUE
), function(d) {
  d$ymax <- cumsum(d$count)
  d$ymin <- d$ymax - d$count
  d
}))
rownames(df_meat_counts) <- NULL

gray_bars <- df_meat_counts[df_meat_counts$group != "none", ]
df_midpoint_outline <- do.call(rbind, lapply(
  split(gray_bars, list(gray_bars$x_dodge, gray_bars$Condition_label), drop = TRUE),
  function(d) data.frame(
    x_dodge         = d$x_dodge[1],
    Condition_label = d$Condition_label[1],
    ymin            = min(d$ymin),
    ymax            = max(d$ymax)
  )
))

df_meat_counts <- df_meat_counts[order(df_meat_counts$response,
                                       df_meat_counts$Condition_label,
                                       df_meat_counts$group), ]
df_meat_counts <- do.call(rbind, lapply(split(
  df_meat_counts,
  list(df_meat_counts$response, df_meat_counts$Condition_label),
  drop = TRUE
), function(d) {
  d$ymax <- cumsum(d$count)
  d$ymin <- d$ymax - d$count
  d
}))
rownames(df_meat_counts) <- NULL

meat_plot <- ggplot(
  df_meat_counts,
  aes(
    xmin = x_dodge - bar_half,
    xmax = x_dodge + bar_half,
    ymin = ymin,
    ymax = ymax,
    fill = Condition_label
  )
) +
  geom_hline(yintercept = seq(0, 125, by = 12.5), color = "grey85", linewidth = 0.3) +
  geom_rect(color = "white", linewidth = 0.1) +
  scale_fill_manual(values = condition_colors, name = "Condition") +
  new_scale_fill() +
  geom_rect(
    data = function(d) d[d$group != "none", ],
    aes(fill = group),
    color = "white",
    linewidth = 0.1
  ) +
  scale_fill_manual(
    values = reason_colors,
    name   = "Midpoint Type",
    breaks = subtype_labels,
    labels = subtype_labels,
    guide  = "none"
  ) +
  geom_rect(
    data = df_midpoint_outline,
    aes(
      xmin  = x_dodge - bar_half,
      xmax  = x_dodge + bar_half,
      ymin  = ymin,
      ymax  = ymax,
      color = Condition_label
    ),
    fill         = NA,
    linewidth    = 1,
    inherit.aes  = FALSE
  ) +
  scale_color_manual(values = condition_colors, guide = "none") +
  scale_x_continuous(breaks = 1:5, labels = response_labels) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Response") +
  theme_classic(base_size = 24) +
  theme(
    plot.tag = element_text(face = "bold"),
    legend.position = "right",
    axis.text.x = element_text(angle = 35, hjust = 1),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.x = element_text(margin = margin(t = 10))
  ) 

################################################################################
# Combine and save

post_plots <- econ_plot + meat_plot +
  plot_layout(guides = "collect")

ggsave(
  "figures/pilot-distributions/Post-pooled.png",
  post_plots,
  width = 20,
  height = 10,
  dpi = 300
)
