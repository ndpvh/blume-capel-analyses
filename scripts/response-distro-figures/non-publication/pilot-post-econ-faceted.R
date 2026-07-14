################################################################################
# Combined figure: response frequency distribution (Panel A) +
# midpoint subtype distribution (Panel B), post-intervention data,
# clustered by experimental condition.
################################################################################

library(ggplot2)
library(patchwork)
source("scripts/preprocessing.R")

condition_labels <- c("1" = "Control", "2" = "Ambivalence", "3" = "Integration")
condition_colors <- c(
  "Control" = "#4CAF50",
  "Ambivalence" = "#FF9800",
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

df_post <- econ_changes[!is.na(econ_changes$Post), ]
df_post$response_simple <- ifelse(df_post$Post %in% c(3, 6, 7, 8, 9), 3L, as.integer(df_post$Post))
df_post$Condition_label <- factor(condition_labels[as.character(df_post$Condition)], levels = condition_labels)

################################################################################
# Panel A: Response frequency distribution

p_a <- ggplot(df_post, aes(x = factor(response_simple, levels = 1:5), fill = Condition_label)) +
  geom_bar(
    color = "white",
    width = 0.7,
    position = position_dodge(width = 0.72)
  ) +
  scale_fill_manual(values = condition_colors, name = "Condition") +
  scale_x_discrete(labels = response_labels) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Response", y = "Count") +
  theme_classic(base_size = 24) +
  theme(
    plot.tag = element_text(face = "bold"),
    axis.title.y = element_text(margin = margin(r = 20)),
    axis.title.x = element_text(margin = margin(t = 20)),
    legend.key.size = unit(0.8, "cm"),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14)
  )

################################################################################
# Panel B: Midpoint subtype distribution

df_b <- df_post[df_post$Post %in% c(3, 6, 7, 8, 9), ]
df_b$subtype <- factor(subtype_labels[as.character(df_b$Post)], levels = subtype_labels)

p_b <- ggplot(df_b, aes(x = subtype, fill = Condition_label)) +
  geom_bar(
    color = "white",
    width = 0.7,
    position = position_dodge(width = 0.72)
  ) +
  scale_fill_manual(values = condition_colors, name = "Condition") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Midpoint subtype", y = "") +
  theme_classic(base_size = 24) +
  theme(
    plot.tag = element_text(face = "bold"),
    axis.line.x = element_line(),
    axis.line.y = element_line(),
    axis.title.x = element_text(margin = margin(t = 10)),
    legend.key.size = unit(0.8, "cm"),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14)
  )

################################################################################
# Combine and save

fig <- p_a + p_b +
  plot_layout(widths = c(9, 10), guides = "collect")

ggsave(
  "figures/Post-econ.png",
  fig,
  width = 20,
  height = 10,
  dpi = 300
)
