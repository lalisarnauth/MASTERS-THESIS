traits <- read.csv("01 Datasets/01_raw_data/functional_traits_all.csv",
                   row.names = 1,
                   header = TRUE,
                   sep = ";")

traits$species <- rownames(traits)

traits <- traits %>%
  left_join(
    example %>% select(species, family),
    by = "species"
  )

# group

traits <- traits %>%
  mutate(
    fabaceae = ifelse(family == "Fabaceae", "Fabaceae", "Other families")
  )
traits$fabaceae <- factor(traits$fabaceae,
                          levels = c("Other families", "Fabaceae"))

# ---- Wood Density ----

wilcox.test(wd ~ fabaceae, data = traits) # p-value = 0.01

# Plot

library(ggplot2)
library(ggpubr)

y_max <- max(traits$wd, na.rm = TRUE)

wd_fabaceae <- ggplot(traits, aes(x = fabaceae, y = wd, fill = fabaceae)) +
  geom_boxplot(
    width = 0.6,
    alpha = 0.85,
    outlier.shape = NA,
    color = "black"
  ) +
  scale_fill_manual(
    values = c(
      "Other families" = "grey70",
      "Fabaceae" = "#1b9e77"
    )
  ) +
  stat_compare_means(
    method = "wilcox.test",
    comparisons = list(c("Other families", "Fabaceae")),
    label = "p.signif",
    label.y = y_max * 1.10,
    tip.length = 0.02
  ) +
  coord_cartesian(ylim = c(NA, y_max * 1.20)) +
  labs(
    x = "",
    y = "Wood density (g cm⁻³)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )
wd_fabaceae

ggsave(
  filename = "wd_fabaceae_boxplot.png",
  plot = wd_fabaceae,
  path = "~/01 Masters_LA/06 Figures/02 plots",
  width = 7,
  height = 5,
  dpi = 300
)

# ---- LDMC ----

wilcox.test(ldmc ~ fabaceae, data = traits) # p-value = 0.01

# Plot

y_max_ldmc <- max(traits$ldmc, na.rm = TRUE)

ldmc_fabaceae <- ggplot(traits, aes(x = fabaceae, y = ldmc, fill = fabaceae)) +
  geom_boxplot(
    width = 0.6,
    alpha = 0.85,
    outlier.shape = NA,
    color = "black"
  ) +
  scale_fill_manual(
    values = c(
      "Other families" = "grey70",
      "Fabaceae" = "#1b9e77"
    )
  ) +
  stat_compare_means(
    method = "wilcox.test",
    comparisons = list(c("Other families", "Fabaceae")),
    label = "p.signif",
    label.y = y_max_ldmc * 1.10,
    tip.length = 0.02
  ) +
  coord_cartesian(ylim = c(NA, y_max_ldmc * 1.20)) +
  labs(
    x = "",
    y = "Leaf dry matter content (LDMC)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )
ldmc_fabaceae

ggsave(
  filename = "ldmc_fabaceae_boxplot.png",
  plot = ldmc_fabaceae,
  path = "~/01 Masters_LA/06 Figures/02 plots",
  width = 7,
  height = 5,
  dpi = 300
)

# ---- SLA ----

wilcox.test(sla ~ fabaceae, data = traits) # p-value = 0.01

# Plot

ggplot(traits, aes(x = fabaceae, y = sla, fill = fabaceae)) +
  geom_boxplot(
    width = 0.6,
    alpha = 0.85,
    outlier.shape = NA,
    color = "black"
  ) +
  scale_fill_manual(
    values = c(
      "Other families" = "grey70",
      "Fabaceae" = "#1b9e77"
    )
  ) +
  stat_compare_means(
    method = "wilcox.test",
    comparisons = list(c("Other families", "Fabaceae")),
    label = "p.signif",
    tip.length = 0.02
  ) +
  stat_compare_means(
    method = "wilcox.test",
    comparisons = list(c("Other families", "Fabaceae")),
    label = "p.format",
    vjust = 1.5
  ) +
  labs(
    x = "",
    y = "Specific Leaf Area (SLA)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )
