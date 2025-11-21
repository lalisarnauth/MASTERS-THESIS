# ------------------------------------------
# PCA - Climatic Variables (ppt, tmax, tmin)
# Script ton  SEM - Laíla Iglesias
# ------------------------------------------

# 1. Load packages
library(tidyverse)  
library(FactoMineR)
library(factoextra)
library(readr)

# 2. Load data 

dados <- read.csv("01 Datasets/01_raw_data/dadosmisto.csv",
                       header = TRUE,
                       sep = ";")

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

