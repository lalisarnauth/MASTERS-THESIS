################################
### PCA - FUNCTIONAL METRICS ###
################################

library(dplyr)
library(ggplot2)
library(ggrepel)

# Read data

functional_data <- read.csv("~/01 Masters_LA/00 MASTERS-DATA/01 Datasets/01_raw_data/reg_funcional.csv", header = TRUE)

# Seleção das variáveis
vars_pca <- functional_data %>% select(
  log_produt,
  cwm_sla, cwm_ldmc, cwm_wd,
  fdis_sla, fdis_ldmc, fdis_wd,PSE, SR
)

# PCA
pca_res <- prcomp(vars_pca, scale = TRUE)

# Tabela de loadings para plot
loadings <- data.frame(pca_res$rotation,
                       Variable = rownames(pca_res$rotation))

# Renomear variáveis para nomes bonitos
loadings$Variable <- dplyr::recode(loadings$Variable,
                            "log_produt" = "Productivity",
                            "cwm_sla" = "CWM SLA",
                            "cwm_ldmc" = "CWM LDMC",
                            "cwm_wd" = "CWM WD",
                            "fdis_sla" = "FDis SLA",
                            "fdis_ldmc" = "FDis LDMC",
                            "fdis_wd" = "FDis WD",
                            "PSE" = "PSE",
                            "SR" = "Species Richness"
)

# Plot estilo Garnier 2004
p_pca <- ggplot() +
  geom_segment(data = loadings,
               aes(x = 0, y = 0,
                   xend = PC1 * 3, yend = PC2 * 3),
               arrow = arrow(length = unit(0.3, "cm")),
               color = "gray30") +
  geom_text_repel(data = loadings,
                  aes(x = PC1 * 3, y = PC2 * 3, label = Variable),
                  size = 4) +
  coord_equal() +
  theme_minimal(base_size = 14) +
  labs(
    x = paste0("PC1 (", round(summary(pca_res)$importance[2,1]*100), "%)"),
    y = paste0("PC2 (", round(summary(pca_res)$importance[2,2]*100), "%)")
  )

p_pca

ggsave(
  filename = "pca_functional_metrics.jpeg",
  plot = p_pca,
  path = "~/01 Masters_LA/06 Figures/04 Plots_Functional_Diversity",
  width = 9,      # ajuste se quiser maior ou menor
  height = 7,
  dpi = 300
)
