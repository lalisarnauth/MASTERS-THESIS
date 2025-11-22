
# ------------------------------------------
# PCA - Granulometry
# Laíla Iglesias
# ------------------------------------------

library(readxl)
library(dplyr)
library(ggplot2)

# 1) Read data ------------------------------------------------------------

granulometria <- read_excel("~/01 Masters_LA/00 MASTERS-DATA/01 Datasets/01_raw_data/granulometria.xlsx")

# Remove 'site' column for PCA (only numeric variables)
dados_granulometria <- granulometria[, -1]

# 2) PCA ------------------------------------------------------------------

pca_resultado <- prcomp(dados_granulometria, scale. = TRUE)

# % of variance explained (for axis labels)
var_exp <- summary(pca_resultado)$importance["Proportion of Variance", 1:2] * 100


# 3) Scores (sites) ------------------------------------------------------

scores_sites <- granulometria %>%
  select(site) %>%
  bind_cols(
    as.data.frame(pca_resultado$x)[, c("PC1", "PC2")]
  )


# 4) Loadings (vectors of variables) --------------------------------------

loadings <- as.data.frame(pca_resultado$rotation[, c("PC1", "PC2")])

variaveis_granulometria <- loadings %>%
  mutate(
    var   = rownames(loadings),
    # rename to English here
    label = dplyr::recode(
      var,
      "areiaf_g.kg" = "Fine sand",
      "areiag_g.kg" = "Coarse sand",
      "silte_g.kg"  = "Silt",
      "argila_g.kg" = "Clay",
      .default      = var   # se sobrar algo, mantém o nome original
    ),
    x = PC1,
    y = PC2
  ) %>%
  select(var, label, x, y)

# Optional: Scale vectors to fit the size of the points
mult <- min(
  (max(scores_sites$PC1) - min(scores_sites$PC1)) /
    (max(variaveis_granulometria$x) - min(variaveis_granulometria$x)),
  (max(scores_sites$PC2) - min(scores_sites$PC2)) /
    (max(variaveis_granulometria$y) - min(variaveis_granulometria$y))
) * 0.7

variaveis_granulometria <- variaveis_granulometria %>%
  mutate(
    x = x * mult,
    y = y * mult
  )


# 5) Pretty biplot --------------------------------------------------------

ggplot() +
  # pontos (sites)
  geom_point(data = scores_sites,
             aes(x = PC1, y = PC2),
             size = 2.8, color = "black") +
  
  # labels dos sites (sem repel para ficar mais “clean”, mas dá pra trocar)
  ggrepel::geom_text_repel(
    data = scores_sites,
    aes(x = PC1, y = PC2, label = site),
    size = 3, max.overlaps = Inf
  ) +
  
  # vetores das variáveis
  geom_segment(data = variaveis_granulometria,
               aes(x = 0, y = 0, xend = x, yend = y),
               linewidth = 0.6, color = "black") +
  
  # labels das variáveis em caixas brancas
  geom_label(
    data = variaveis_granulometria,
    aes(x = x, y = y, label = label),
    size = 3.5,
    label.size = 0.2,
    fill = "white",
    color = "black"
  ) +
  
  # eixos centrais
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_vline(xintercept = 0, linewidth = 0.4) +
  
  # títulos dos eixos com % de variância
  labs(
    title = "Soil texture PCA",
    x = paste0("Axis 1 (", round(var_exp[1], 1), " %)"),
    y = paste0("Axis 2 (", round(var_exp[2], 1), " %)")
  ) +
  
  coord_equal() +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6)
  )

ggsave(filename = "~/01 Masters_LA/06 Figures/02 plots/PCA_granulometria.jpeg", width = 8, height = 6, dpi = 300)


