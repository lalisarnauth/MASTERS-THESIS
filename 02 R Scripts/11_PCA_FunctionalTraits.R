
############################################################
### Functional trait PCA (REGUA + Mico-Leão)
### - Load and combine trait data
### - Run PCA on functional traits
### - Plot PCA biplot (species + trait loadings)
### - Compute trait contributions to each axis
### Author: Laíla Arnauth
############################################################

library(dplyr)
library(ggplot2)
library(tidyr)

# Load datasets

funcional_regua <- read.csv("01 Datasets/01_raw_data/funcional_regua.csv",row.names = 1, sep = ";")

funcional_ml <- read.csv("01 Datasets/01_raw_data/funcional_ml.csv",row.names = 1, sep = ";")

### Incomplete data ### while I still don't have foliar C, N, and P for all

funcional_regua <- funcional_regua[, -((ncol(funcional_regua)-2):ncol(funcional_regua))]

funcional_ml <- funcional_ml[, -((ncol(funcional_ml)-3):ncol(funcional_ml))]


funcional_regua$specie <- rownames(funcional_regua)
funcional_ml$specie    <- rownames(funcional_ml)

funcional_regua$Site <- "REGUA"
funcional_ml$Site    <- "Mico_Leao"

## Bind the dfs

funcional_total <- rbind(funcional_regua, funcional_ml)

# 1) Select only numeric traits
traits_num <- funcional_total[, sapply(funcional_total, is.numeric)]

# 2) Run PCA on scaled traits
pca <- prcomp(traits_num, center = TRUE, scale. = TRUE)

# 3) Species scores (PC1, PC2)
scores <- as.data.frame(pca$x[, 1:2])
colnames(scores) <- c("PC1", "PC2")

# keep species names in the same order as funcional_total
scores$specie <- funcional_total$specie

# 4) Trait loadings
loadings <- as.data.frame(pca$rotation[, 1:2])
colnames(loadings) <- c("PC1", "PC2")
loadings$Trait <- rownames(loadings)

# 5) Axis labels with explained variance
var_expl <- (pca$sdev^2) / sum(pca$sdev^2) * 100
lab_x <- paste0("PC1 (", round(var_expl[1], 1), "%)")
lab_y <- paste0("PC2 (", round(var_expl[2], 1), "%)")

# Plot

g_pca_simple <- ggplot(scores, aes(x = PC1, y = PC2)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey60") +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey60") +
  geom_point(size = 2.8, alpha = 0.9, colour = "black") +
  geom_segment(data = loadings,
               aes(x = 0, y = 0, xend = PC1*3, yend = PC2*3),
               arrow = arrow(length = unit(0.25, "cm")),
               colour = "black", linewidth = 1.1) +
  geom_text(data = loadings,
            aes(x = ifelse(Trait == "LDMC", PC1*3.4, PC1*3.2),
                y = ifelse(Trait == "LDMC", PC2*3.4, PC2*3.2),
                label = Trait),
            colour = "black", size = 4, fontface = "bold", hjust = 0) +
  coord_equal(clip = "off") +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.18))) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    plot.margin = margin(10, 35, 10, 10)
  ) +
  labs(
    title = "PCA of functional traits",
    subtitle = "Points = species; Arrows = trait loadings",
    x = lab_x,
    y = lab_y
  )

g_pca_simple

ggsave("~/01 Masters_LA/06 Figures/04 Plots_Functional_Diversity/PCA_functional_traits.jpeg", g_pca_simple,
       width = 16, height = 12, units = "cm", dpi = 600)

# ============================
# Trait contributions to each PCA axis
# (percentage contribution per trait and axis)
# ============================

# Use full rotation matrix (all PCs)
load_full <- as.data.frame(pca$rotation)
load_full$Trait <- rownames(load_full)

# Square of loadings
L2 <- load_full[, colnames(pca$rotation)]^2

# Contribution (%) for each trait in each axis
contrib <- sweep(L2, 2, colSums(L2), FUN = "/") * 100
contrib_df <- as.data.frame(contrib)
contrib_df$Trait <- rownames(contrib_df)

# Inspect contributions for PC1 and PC2
contrib_PC1_PC2 <- contrib_df %>%
  dplyr::select(Trait, PC1, PC2) %>%
  arrange(desc(PC1))  # change to desc(PC2) to sort by PC2

print(contrib_PC1_PC2)

# PC1: LDMC
# PC2: WD


# ============================
# barplot of trait contributions to PC1
# ============================
g_contrib_PC1 <- contrib_long %>%
  filter(Axis == "PC1") %>%
  arrange(desc(Contribution)) %>%
  ggplot(aes(x = reorder(Trait, Contribution), y = Contribution)) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Trait",
    y = "Contribution to PC1 (%)",
    title = "Trait contributions to PC1"
  ) +
  theme_minimal(base_size = 12)

print(g_contrib_PC1)

ggsave("~/01 Masters_LA/06 Figures/04 Plots_Functional_Diversity/functional_PCA_contrib_PC1.jpeg", g_contrib_PC1,width = 6, height = 5, dpi = 300)

# ============================
# Barplot of trait contributions to PC2
# ============================

g_contrib_PC2 <- contrib_long %>%
  filter(Axis == "PC2") %>%
  arrange(desc(Contribution)) %>%
  ggplot(aes(x = reorder(Trait, Contribution), y = Contribution)) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Trait",
    y = "Contribution to PC2 (%)",
    title = "Trait contributions to PC2"
  ) +
  theme_minimal(base_size = 12)

print(g_contrib_PC2)

ggsave(
  "~/01 Masters_LA/06 Figures/04 Plots_Functional_Diversity/functional_PCA_contrib_PC2.jpeg",
  g_contrib_PC2,
  width = 6,
  height = 5,
  dpi = 300
)

