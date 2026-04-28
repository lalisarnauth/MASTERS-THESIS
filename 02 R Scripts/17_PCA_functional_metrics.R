

### PCA - FUNCTIONAL METRICS ###

library(dplyr)
library(ggplot2)
library(ggrepel)
library(writexl)
library(grid)

# --- CWM PCA ----

# Read data
functional_data <- read.csv(
  "~/01 Masters_LA/00 MASTERS-DATA/01 Datasets/02_processed_data/dadosreg.csv",
  header = TRUE
)

# Select variables
vars_pca <- functional_data %>%
  select(cwm_sla, cwm_ldmc, cwm_leafcn, cwm_leafp, cwm_wd)

# PCA
pca_res <- prcomp(vars_pca, scale. = TRUE)

# Explained variance
var_exp <- summary(pca_res)$importance[2, 1:2] * 100

# Loadings table
loadings <- data.frame(
  pca_res$rotation[, 1:2],
  Variable = rownames(pca_res$rotation)
)

# Rename variables (cleaner labels)
loadings$Variable <- dplyr::recode(
  loadings$Variable,
  "cwm_sla"   = "SLA",
  "cwm_ldmc"  = "LDMC",
  "cwm_wd"    = "WD",
  "cwm_leafcn" = "Leaf C:N",
  "cwm_leafp" = "Leaf P"
)

# Arrow scale factor
arrow_scale <- 2.3

# Plot
p_pca <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60", linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60", linewidth = 0.5) +
  geom_segment(
    data = loadings,
    aes(
      x = 0, y = 0,
      xend = PC1 * arrow_scale,
      yend = PC2 * arrow_scale
    ),
    arrow = arrow(length = unit(0.25, "cm")),
    color = "grey30",
    linewidth = 0.8
  ) +
  geom_text_repel(
    data = loadings,
    aes(
      x = PC1 * arrow_scale,
      y = PC2 * arrow_scale,
      label = Variable
    ),
    size = 5,
    color = "black",
    box.padding = 0.35,
    point.padding = 0.2,
    segment.color = NA
  ) +
  theme_classic(base_size = 15) +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 13, color = "black"),
    panel.border = element_blank()
  ) +
  labs(
    x = paste0("PC1 (", round(var_exp[1], 0), "%)"),
    y = paste0("PC2 (", round(var_exp[2], 0), "%)")
  )

p_pca

# Save figure
ggsave(
  filename = "pca_functional_metrics_LeafCN.jpeg",
  plot = p_pca,
  path = "~/01 Masters_LA/06 Figures/04 Plots_Functional_Diversity",
  width = 9,
  height = 6,
  dpi = 300
)

# --- Save PCA scores ----

# scores dos sites (PC1 e PC2)
pc_scores <- as.data.frame(pca_res$x[, 1:2])

# adicionar nome dos sites
pc_scores$site <- rownames(pc_scores)

# reorganizar coluna
pc_scores <- pc_scores[, c("site", "PC1", "PC2")]

# export
write_xlsx(
  pc_scores,
  path = "01 Datasets/03_functional_processed_data/pc_scores_leafcns.xlsx"
)

# ---- old ----


# Read data

functional_data <- read.csv("~/01 Masters_LA/00 MASTERS-DATA/01 Datasets/01_raw_data/reg_funcional.csv", header = TRUE)

# Select variables
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

# --- CWM PCA 2 ----

# Read data
functional_data <- read.csv(
  "~/01 Masters_LA/00 MASTERS-DATA/01 Datasets/02_processed_data/dadosreg.csv",
  header = TRUE
)

# Select variables
vars_pca <- functional_data %>%
  select(cwm_sla, cwm_ldmc, cwm_leafcn,cwm_wd,cwm_leafn)

# PCA
pca_res <- prcomp(vars_pca, scale. = TRUE)

# Explained variance
var_exp <- summary(pca_res)$importance[2, 1:2] * 100

# Loadings table
loadings <- data.frame(
  pca_res$rotation[, 1:2],
  Variable = rownames(pca_res$rotation)
)

# Rename variables (cleaner labels)
loadings$Variable <- dplyr::recode(
  loadings$Variable,
  "cwm_sla"   = "SLA",
  "cwm_ldmc"  = "LDMC",
  "cwm_wd"    = "WD",
  "cwm_leafcn" = "Leaf C:N",
  "cwm_leafn" = "Leaf N")

# Arrow scale factor
arrow_scale <- 2.3

# Plot
p_pca <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60", linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60", linewidth = 0.5) +
  geom_segment(
    data = loadings,
    aes(
      x = 0, y = 0,
      xend = PC1 * arrow_scale,
      yend = PC2 * arrow_scale
    ),
    arrow = arrow(length = unit(0.25, "cm")),
    color = "grey30",
    linewidth = 0.8
  ) +
  geom_text_repel(
    data = loadings,
    aes(
      x = PC1 * arrow_scale,
      y = PC2 * arrow_scale,
      label = Variable
    ),
    size = 5,
    color = "black",
    box.padding = 0.35,
    point.padding = 0.2,
    segment.color = NA
  ) +
  theme_classic(base_size = 15) +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 13, color = "black"),
    panel.border = element_blank()
  ) +
  labs(
    x = paste0("PC1 (", round(var_exp[1], 0), "%)"),
    y = paste0("PC2 (", round(var_exp[2], 0), "%)")
  )

p_pca

# Save figure
ggsave(
  filename = "pca_functional_metrics.jpeg",
  plot = p_pca,
  path = "~/01 Masters_LA/06 Figures/04 Plots_Functional_Diversity",
  width = 9,
  height = 6,
  dpi = 300
)