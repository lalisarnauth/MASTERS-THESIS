
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
dados_num <- dados %>% dplyr::select(where(is.numeric))

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
  dplyr::select(var, PC1, PC2) %>%
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


### ---- new ----

# PCA - Soil Nutrients

# 1. Load packages
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(readr)

# 2. Load data
dados <- read.csv(
  "C:/Users/Laíla Arnauth/OneDrive/Documentos/01 Masters_LA/00 MASTERS-DATA/01 Datasets/01_raw_data/pca_nutrientes.csv",
  header = TRUE
)

# 3. Keep site names
sites <- dados$site

# 4. Select and scale numeric variables
dados_padronizados <- dados %>%
  select(where(is.numeric)) %>%
  scale() %>%
  as.data.frame()

rownames(dados_padronizados) <- sites

# 5. Run PCA
pca_nutrients <- PCA(dados_padronizados, graph = FALSE)

# 6. Check explained variance
summary(pca_nutrients)
fviz_eig(pca_nutrients)

# # Flip PC1
pca_nutrients$ind$coord[,1] <- -pca_nutrients$ind$coord[,1]
pca_nutrients$var$coord[,1] <- -pca_nutrients$var$coord[,1]
pca_nutrients$var$contrib[,1] <- pca_nutrients$var$contrib[,1]

# 7. Plot with the same style as climatic PCA
jpeg("~/01 Masters_LA/06 Figures/02 plots/pca_nutrients.jpeg",
     width = 2000, height = 1600, res = 300)

print(
  fviz_pca_var(
    pca_nutrients,
    col.var = "contrib",
    gradient.cols = c("blue", "yellow", "red"),
    repel = TRUE,
    title = "Soil Ions Gradient"
  )
)

dev.off()

# 8. Extract site scores
scores_nutrients <- as.data.frame(pca_nutrients$ind$coord)
scores_nutrients$site <- rownames(scores_nutrients)

# 9. Extract variable loadings
loadings_nutrients <- as.data.frame(pca_nutrients$var$coord)
loadings_nutrients$var <- rownames(loadings_nutrients)

# 10. Extract variable contributions
contrib_nutrients <- as.data.frame(pca_nutrients$var$contrib)
contrib_nutrients$var <- rownames(contrib_nutrients)

# 11. View loadings
loadings_nutrients %>%
  select(var, Dim.1, Dim.2) %>%
  arrange(desc(abs(Dim.1)))

loadings_nutrients %>%
  select(var, Dim.1, Dim.2) %>%
  arrange(desc(abs(Dim.2)))

# 12. View contributions
contrib_nutrients %>%
  select(var, Dim.1, Dim.2) %>%
  arrange(desc(Dim.1))

contrib_nutrients %>%
  select(var, Dim.1, Dim.2) %>%
  arrange(desc(Dim.2))

# 13. Plot contributions
fviz_contrib(pca_nutrients, choice = "var", axes = 1, top = 10)
fviz_contrib(pca_nutrients, choice = "var", axes = 2, top = 10)















