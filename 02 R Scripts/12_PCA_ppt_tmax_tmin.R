
# PCA - Climatic Variables (ppt, tmax, tmin)
# Script ton  SEM - Laíla Iglesias


# ---- 1st version ----

# 1. Load packages
library(tidyverse)  
library(FactoMineR)
library(factoextra)
library(readr)

# 2. Load data 

dados <- read.csv("01 Datasets/01_raw_data/dadosmisto.csv",header = TRUE,row.names = 1)

# 3. Scale variables
dados_padronizados <- dados %>% 
  select(ppt, tmax, tmin) %>% 
  scale() %>% 
  as.data.frame()

# 4. Run PCA
pca_resultado <- PCA(dados_padronizados, graph = FALSE)

# 5. View results
summary(pca_resultado)  # Proportion of explained variance
fviz_eig(pca_resultado) # Scree plot

# 6. Extract variables loadings

jpeg("~/01 Masters_LA/06 Figures/02 plots/pca_aridez.jpeg", width = 2000, height = 1600, res = 300)
print(fviz_pca_var(pca_resultado, 
                   col.var = "contrib",
                   gradient.cols = c("blue", "yellow", "red"),
                   repel = TRUE,
                   title = "Dryness Gradient"))
dev.off()

# 7. PC1 to use in SEM
dados$PC1_aridez <- pca_resultado$ind$coord[, 1]

# 8. Check the direction of PC1 (important for interpretation)
cor(dados_padronizados, dados$PC1_aridez)

# 9. Save PC1 data

write.csv(dados, "~/01 Masters_LA/00 MASTERS-DATA/01 Datasets/02_processed_data/dados_pc1aridez.csv", row.names = FALSE)

# --- 2nd version: Season ppt and altitude ----

library(FactoMineR)
library(factoextra)
library(dplyr)

# 1. Load and prepare PCA data

dados <- read.csv("01 Datasets/01_raw_data/dadosmisto.csv",header = TRUE,row.names = 1)

dados_stnd <- dados %>% 
  select(ppt, tmax, tmin, season_ppt, altitude) %>% 
  scale() %>% 
  as.data.frame()

# 2. Add plot names as rownames
rownames(dados_stnd) <- dados$parcela

# 3. Run PCA
pca_resultado <- PCA(dados_stnd, graph = FALSE)

# 4. View results
summary(pca_resultado)
fviz_eig(pca_resultado)

# 5. Plot variable contributions
jpeg("~/01 Masters_LA/06 Figures/02 plots/pca_aridez1.jpeg", 
     width = 2000, height = 1600, res = 300)

print(
  fviz_pca_var(
    pca_resultado, 
    col.var = "contrib",
    gradient.cols = c("blue", "yellow", "red"),
    repel = TRUE,
    title = "Dryness + Seasonality Gradient"
  )
)

dev.off()

# 6. Extract site scores
scores_sites <- as.data.frame(pca_resultado$ind$coord[, 1:2])
scores_sites$parcela <- rownames(scores_sites)

# 7. Rename axes
scores_sites <- scores_sites %>%
  rename(
    PC1_ClimaA = Dim.1,
    PC2_ClimaB = Dim.2
  )

# 8. Join with dadosmisto
dadosmisto <- dadosmisto %>%
  left_join(scores_sites, by = "parcela")

# 9. Save
write.csv(
  dadosmisto,
  "01 Datasets/01_raw_data/dadosmisto.csv",
  row.names = TRUE
)
