
# ---- Load packages ----

library(dplyr)
library(ggplot2)
library(ggpubr)


traits <- read.csv("01 Datasets/01_raw_data/functional_traits_all.csv",
                   row.names = 1,
                   header = TRUE,
                   sep = ";")

traits$species <- rownames(traits)

traits <- traits %>%
  dplyr::left_join(
    dplyr::select(example, species, family),
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

######## PCPS1 negative side ######## 

familias_neg <- c("Fabaceae",
                  "Lauraceae",
                  "Euphorbiaceae",
                  "Annonaceae")

traits <- traits %>%
  mutate(
    pcps_group = ifelse(family %in% familias_neg,
                        "PCPS1 negative clade",
                        "Other families")
  )

wilcox.test(wd ~ pcps_group, data = traits)

wilcox.test(ldmc ~ pcps_group, data = traits)

aggregate(wd ~ pcps_group, data = traits, median)
aggregate(ldmc ~ pcps_group, data = traits, median)

###### ---- Plot ----

# Test
wilcox.test(ldmc ~ pcps_group, data = traits)  # p ~ 0.041 (no seu output)

# Plot

ldmc_pcps <- ggplot(traits, aes(x = pcps_group, y = ldmc, fill = pcps_group)) +
  geom_boxplot(width = 0.6, alpha = 0.85, outlier.shape = NA, color = "black") +
  scale_fill_manual(values = c(
    "Other families" = "#D9D9D9",
    "PCPS1 negative clade" = "#2E7D32"
  )) +
  stat_compare_means(
    method = "wilcox.test",
    comparisons = list(c("Other families", "PCPS1 negative clade")),
    label = "p.signif",
    label.y = 0.53,
    tip.length = 0.02
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0.2, 0.65)) +
  labs(x = "", y = "Leaf dry matter content (LDMC)") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

ldmc_pcps

ggsave(
  filename = "ldmc_PCPS1_negative_clade_boxplot.jpeg",
  plot = ldmc_pcps,
  path = "~/01 Masters_LA/06 Figures/02 plots",
  width = 7,
  height = 5,
  dpi = 300
)
