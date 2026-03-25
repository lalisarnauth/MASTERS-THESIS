
# PCA - Nutrients #

# Load packages

library(tidyverse)
library(factoextra)
library(readr)

# Read data
dados <- read.csv("C:/Users/Laíla Arnauth/OneDrive/Documentos/01 Masters_LA/00 MASTERS-DATA/01 Datasets/01_raw_data/pca_nutrientes.csv",
                  , header = T)

# Only numeric
sites <- dados$site
dados_num <- dados %>% select(where(is.numeric))

# Run PCA
pca <- prcomp(dados_num, scale. = TRUE)

# Explained variance
var_exp <- (pca$sdev^2) / sum(pca$sdev^2)
var_exp_percent <- round(var_exp * 100, 1)

var_exp_percent

# Scores (sites)
scores <- as.data.frame(pca$x)
scores$site <- sites

# Loadings
loadings <- as.data.frame(pca$rotation)
loadings$var <- rownames(loadings)

# Mult for arrows
mult <- 3

# ---- Plot ----
ggplot() +
  # dots
  geom_point(data = scores,
             aes(x = PC1, y = PC2),
             size = 3) +
  
  geom_text(data = scores,
            aes(x = PC1, y = PC2, label = site),
            vjust = -1, size = 4) +
  
  # arrows
  geom_segment(data = loadings,
               aes(x = 0, y = 0,
                   xend = PC1 * mult,
                   yend = PC2 * mult),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "red") +
  
  geom_text(data = loadings,
            aes(x = PC1 * mult,
                y = PC2 * mult,
                label = var),
            color = "red",
            vjust = -0.5) +
  
  labs(
    title = "PCA: Soil Nutrients",
    x = paste0("PC1 (", var_exp_percent[1], "%)"),
    y = paste0("PC2 (", var_exp_percent[2], "%)")
  ) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  
  theme_minimal()

ggsave(filename = "~/01 Masters_LA/06 Figures/02 plots/PCA_nutrients.jpeg", width = 8, height = 6, dpi = 300)


# --- Loadings ----

loadings %>%
  select(var, PC1, PC2) %>%
  arrange(desc(abs(PC1)))

loadings %>%
  select(var, PC1, PC2) %>%
  arrange(desc(abs(PC2)))


# Contribution
contrib <- loadings[,1:2]^2

# Normalize
contrib$PC1 <- contrib$PC1 / sum(contrib$PC1) * 100
contrib$PC2 <- contrib$PC2 / sum(contrib$PC2) * 100

contrib$var <- rownames(loadings)

# 
contrib %>% arrange(desc(PC1))
contrib %>% arrange(desc(PC2))


# Plot

fviz_contrib(pca, choice = "var", axes = 1, top = 10)
fviz_contrib(pca, choice = "var", axes = 2, top = 10)














