library(dplyr)
library(tidyr)
library(ggplot2)

# Selecionar variáveis do modelo
vars_m17 <- c("sr", "pcps1", "season_ppt")

# Transformar para formato longo
dados_long_m17 <- dadosmisto1 %>%
  select(site, all_of(vars_m17)) %>%
  pivot_longer(
    cols = -site,
    names_to = "variable",
    values_to = "value"
  )

# Boxplots
g_box_m17 <- ggplot(dados_long_m17, aes(x = site, y = value)) +
  geom_boxplot(fill = "lightgray") +
  facet_wrap(~ variable, scales = "free_y") +
  theme_bw(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  ) +
  labs(
    title = "Boxplots of predictor variables used in model m17",
    x = "Site",
    y = "Value"
  )

# Print
print(g_box_m17)

# Save
ggsave(
  filename = "~/01 Masters_LA/06 Figures/02 plots/boxplots_m17_predictors.jpeg",
  plot = g_box_m17,
  width = 10, height = 6, dpi = 300
)
