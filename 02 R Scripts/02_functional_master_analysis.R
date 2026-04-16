
### SCRIPT FOR FUNCTIONAL ANALYSIS ###
###         2nd chapter           ####


# last update: 2026/03/25 (YYYY/MM/DD)
# Authors: Laíla Arnauth & André T. C. Dias
# Post Graduate Program in Ecology - UFRJ - Brazil
# Collaboration with University of Regina - Canada # Forest Dynamics Lab


# Load all packages
library(FD)
library(tidyverse)
library(corrplot)
library(MuMIn) #AICc
library(car)
library(readxl)
library(openxlsx)
library(writexl)
library(ggplot2)
library(MuMIn)
library(patchwork)   # to combine plots side by side
library(ggeffects)
library(webshot2)
library(purrr)
library(stringr)
library(dplyr)
library(plotly) # Masks dplyr filter()
library(broom)


#### MICO-LEÃO ####

# Read data
comunidade_ml <- read.csv("01 Datasets/01_raw_data/comunidade_ml.csv", row.names = 1, header = TRUE, sep = ";", check.names = FALSE)

funcional_ml <- read.csv("01 Datasets/01_raw_data/funcional_ml.csv", row.names = 1)

# Select traits
atributos <- c("WD", "SLA", "LDMC","leafC","leafN","leafP")

# === Combined dbFD ===
resultado_fd_ml <- dbFD(
  x = funcional_ml[, atributos],
  a = comunidade_ml,
  calc.FRic = TRUE,
  calc.FDiv = TRUE,
  calc.CWM  = TRUE,
  m = "max",
  stand.x = TRUE,
  corr = "cailliez"
)

# ---- Create Excel with tabs ----
wb <- createWorkbook()

# Add each metric
addWorksheet(wb, "FDis_Combinado")
writeData(wb, "FDis_Combinado", resultado_fd_ml$FDis)

addWorksheet(wb, "FRic_Combinado")
writeData(wb, "FRic_Combinado", resultado_fd_ml$FRic)

addWorksheet(wb, "FDiv_Combinado")
writeData(wb, "FDiv_Combinado", resultado_fd_ml$FDiv)

addWorksheet(wb, "CWM_por_atributo")
writeData(wb, "CWM_por_atributo", resultado_fd_ml$CWM)

# ---- FD for each trait (no CWM) ----
for (attr in atributos) {
  cat("Calculando para:", attr, "\n")
  dados_attr <- funcional_ml[, attr, drop = FALSE]
  
  res <- dbFD(x = dados_attr,
              a = comunidade_ml,
              calc.FRic = TRUE,
              calc.FDiv = TRUE,
              calc.CWM = FALSE,
              m = "max",
              stand.x = TRUE,
              corr = "cailliez")
  
  # Adicionar aba com resultados
  addWorksheet(wb, paste0("FDis_", attr))
  writeData(wb, paste0("FDis_", attr), res$FDis)
  
  addWorksheet(wb, paste0("FRic_", attr))
  writeData(wb, paste0("FRic_", attr), res$FRic)
  
  addWorksheet(wb, paste0("FDiv_", attr))
  writeData(wb, paste0("FDiv_", attr), res$FDiv)
}

# === Save archive ===
saveWorkbook(wb, file = "01 Datasets/03_functional_processed_data/Resultados_Funcional_ML_Foliar.xlsx", overwrite = TRUE)

# CWM Leaf C:N

comunidade_ml <- read.csv("01 Datasets/01_raw_data/comunidade_ml.csv", row.names = 1, header = TRUE, sep = ";", check.names = FALSE)

funcional_ml <- read.csv("01 Datasets/01_raw_data/funcional_ml.csv", row.names = 1)

atributos <- c("leafCN")

CWM_LeafCN <- dbFD(
  x = funcional_ml[, atributos, drop = FALSE],
  a = comunidade_ml,
  calc.FRic = FALSE,
  calc.FDiv = FALSE,
  calc.CWM  = TRUE,
  stand.x = TRUE,
  corr = "cailliez"
)

CWM_LeafCN <- CWM_LeafCN$CWM

write.xlsx(CWM_LeafCN, file = "01 Datasets/03_functional_processed_data/CWM_LeafCN.xlsx")

#### REGUA ####

# Read data
comunidade_regua <- read.csv("01 Datasets/01_raw_data/comunidade_regua.csv", row.names = 1, sep = ";", check.names = FALSE)

funcional_regua <- read.csv("01 Datasets/01_raw_data/funcional_regua.csv", 
                            row.names = 1)

# === Select traits ===
atributos <- c("WD", "SLA", "LDMC","leafC","leafN","leafP")

# === Run combined dbFD ===
resultado_fd_combinado <- dbFD(
  x = funcional_regua[, atributos],
  a = comunidade_regua,
  calc.FRic = TRUE,
  calc.FDiv = TRUE,
  calc.CWM  = TRUE,
  m = "max",
  stand.x = TRUE,
  corr = "cailliez"
)

# === Create excel with tabs ===
wb <- createWorkbook()

# Add metrics
addWorksheet(wb, "FDis_Combinado")
writeData(wb, "FDis_Combinado", resultado_fd_combinado$FDis)

addWorksheet(wb, "FRic_Combinado")
writeData(wb, "FRic_Combinado", resultado_fd_combinado$FRic)

addWorksheet(wb, "FDiv_Combinado")
writeData(wb, "FDiv_Combinado", resultado_fd_combinado$FDiv)

addWorksheet(wb, "CWM_por_atributo")
writeData(wb, "CWM_por_atributo", resultado_fd_combinado$CWM)

# === FD for each trait (no CWM) ===
for (attr in atributos) {
  cat("Calculando para:", attr, "\n")
  dados_attr <- funcional_regua[, attr, drop = FALSE]
  
  res <- dbFD(x = dados_attr,
              a = comunidade_regua,
              calc.FRic = TRUE,
              calc.FDiv = TRUE,
              calc.CWM = FALSE,
              m = "max",
              stand.x = TRUE,
              corr = "cailliez")
  
  # Adicionar cada resultado como nova aba
  addWorksheet(wb, paste0("FDis_", attr))
  writeData(wb, paste0("FDis_", attr), res$FDis)
  
  addWorksheet(wb, paste0("FRic_", attr))
  writeData(wb, paste0("FRic_", attr), res$FRic)
  
  addWorksheet(wb, paste0("FDiv_", attr))
  writeData(wb, paste0("FDiv_", attr), res$FDiv)
}

# === Save archive ===
saveWorkbook(wb, file = "01 Datasets/03_functional_processed_data/Resultados_Funcional_REGUA_Foliar.xlsx", overwrite = TRUE)

# CWM Leaf C:N

# Read data
comunidade_regua <- read.csv("01 Datasets/01_raw_data/comunidade_regua.csv", row.names = 1, sep = ";", check.names = FALSE)

funcional_regua <- read.csv("01 Datasets/01_raw_data/funcional_regua.csv", 
                            row.names = 1)

atributos <- c("leafCN")

CWM_LeafCN <- dbFD(
  x = funcional_regua[, atributos, drop = FALSE],
  a = comunidade_regua,
  calc.FRic = FALSE,
  calc.FDiv = FALSE,
  calc.CWM  = TRUE,
  stand.x = TRUE,
  corr = "cailliez"
)

CWM_LeafCN <- CWM_LeafCN$CWM

write.xlsx(CWM_LeafCN, file = "01 Datasets/03_functional_processed_data/CWM_LeafCN_REGUA.xlsx")

# ---- CORRELATION MATRIX: FUNCTIONAL #####
##### INDEPENDENT VARIABLES FROM FILE ####

# Read Excel file
functional_data <- read.csv("~/01 Masters_LA/00 MASTERS-DATA/01 Datasets/02_processed_data/dadosreg.csv", header = TRUE,row.names = 1)

# Inspect columns
colnames(functional_data)

# Define independent variables')

variables_func <- c("log_biomass","fdis2", "fric2", "fdiv2","raoq",
                    "cwm_wd", "cwm_sla", "cwm_ldmc","cwm_leafc",                          "cwm_leafn","cwm_leafp","cwm_leafcn","PC1","PC2",
                    "fdis_wd", "fric_wd", "fdis_sla", "fric_sla",
                    "fdis_ldmc", "fric_ldmc","fdis_leafc","fric_leafc"                    , "fdis_leafn","fric_leafn","fdis_leafp","Rao_WD","Rao_SLA",                  "Rao_LDMC","Rao_leafC","Rao_leafN","Rao_leafP"                           ,"fric_leafp","SR", "SESPD","SESMNTD", "PSC",                               "PSEab","Rao_phylo","Simpson")

# Create a dataframe with only these variables
data_func <- functional_data %>%
  dplyr::select(all_of(variables_func))

# Optional: Define pretty labels for plot
labels_pretty_func <- c(
  log_biomass = "Log Biomass",
  fdis2       = "Functional Dispersion",
  fric2       = "Functional Richness",
  fdiv2       = "Functional Divergence",
  raoq        = "RaoQ",
  
  cwm_wd      = "CWM Wood Density",
  cwm_sla     = "CWM SLA",
  cwm_ldmc    = "CWM LDMC",
  cwm_leafc   = "CWM Leaf C",
  cwm_leafn   = "CWM Leaf N",
  cwm_leafp   = "CWM Leaf P",
  cwm_leafcn  = "CWM Leaf C:N",
  PC1         = "PC1 CWM",
  PC2         = "PC2 CWM",
  
  fdis_wd     = "FDis Wood Density",
  fric_wd     = "FRic Wood Density",
  fdis_sla    = "FDis SLA",
  fric_sla    = "FRic SLA",
  fdis_ldmc   = "FDis LDMC",
  fric_ldmc   = "FRic LDMC",
  fdis_leafc  = "FDis Leaf C",
  fric_leafc  = "FRic Leaf C",
  fdis_leafn  = "FDis Leaf N",
  fric_leafn  = "FRic Leaf N",
  fdis_leafp  = "FDis Leaf P",
  fric_leafp  = "FRic Leaf P",
  Rao_WD      = "Rao WD",
  Rao_LDMC    =  "Rao LDMC",
  Rao_SLA     = "Rao SLA",
  Rao_leafC   = "Rao Leaf C",
  Rao_leafN   = "Rao Leaf N",
  Rao_leafP   = "Rao Leaf P",
  
  
  SR          = "Species Richness",
  Simpson     = " Simpson index",
  
  SESPD       = "sesPD",
  SESMNTD     = "sesMNTD",
  PSC         = "PSC",
  PSEab       = "PSE",
  Rao_phylo   = "Rao phylogenetic"
)

# Compute correlation matrix
cor_matrix_func <- cor(data_func, method = "spearman", use = "pairwise.complete.obs")

# Function for p-values
cor.mtest <- function(mat, method = "spearman") {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      test <- suppressWarnings(
        cor.test(mat[, i], mat[, j], method = method, exact = FALSE)
      )
      p.mat[i, j] <- p.mat[j, i] <- test$p.value
    }
  }
  
  colnames(p.mat) <- colnames(mat)
  rownames(p.mat) <- colnames(mat)
  return(p.mat)
}

# p matrix
p_matrix_func <- cor.mtest(data_func, method = "spearman")

# Pretty names
pretty_names <- labels_pretty_func[variables_func]

# Rename BOTH matrices
rownames(cor_matrix_func) <- pretty_names
colnames(cor_matrix_func) <- pretty_names

rownames(p_matrix_func) <- pretty_names
colnames(p_matrix_func) <- pretty_names

# Plot
png("~/01 Masters_LA/06 Figures/04 Plots_Functional_Diversity/correlation_matrix_functional2.png",
    width = 1400, height = 1000, res = 150)

corrplot(cor_matrix_func,
         method = "circle",
         type = "upper",
         tl.col = "black",
         tl.cex = 0.7,
         p.mat = p_matrix_func,
         sig.level = c(0.001, 0.01, 0.05),
         insig = "label_sig",
         pch.cex = 1.2,
         pch.col = "black")

dev.off()


# ---- GLMS ----

dadosreg <- read.csv("~/01 Masters_LA/00 MASTERS-DATA/01 Datasets/02_processed_data/dadosreg.csv", header = TRUE)

#### Rao Melodic ####

# All traits

mod_func <- lm(log_biomass ~ raoq, data = dadosreg)
mod_phylo <- lm(log_biomass ~ Rao_phylo, data = dadosreg)
mod_tax <- lm(log_biomass ~ Simpson, data = dadosreg)

summary(mod_func)
summary(mod_phylo)
summary(mod_tax)

mod_full <- lm(
  log_biomass ~ raoq + Rao_phylo + Simpson,
  data = dadosreg
)

summary(mod_full)
car::vif(mod_full)


# Individual Traits

mod_SLA <- lm(log_biomass ~ Rao_SLA, data = dadosreg)
mod_LDMC <- lm(log_biomass ~ Rao_LDMC, data = dadosreg)
mod_WD <- lm(log_biomass ~ Rao_WD, data = dadosreg)
mod_leafc <- lm(log_biomass ~ Rao_leafC, data = dadosreg)
mod_leafn <- lm(log_biomass ~ Rao_leafN, data = dadosreg)
mod_leafp <- lm(log_biomass ~ Rao_leafP, data = dadosreg)

summary(mod_SLA) # 0.417    
summary(mod_LDMC) # 0.0283 * 
summary(mod_WD) # 0.521
summary(mod_leafc) # 0.924
summary(mod_leafn) # 0.752 
summary(mod_leafp) # 0.985


mod_LDMC_WD <- lm(log_biomass ~ Rao_LDMC + Rao_WD, data = dadosreg)
summary(mod_LDMC_WD)
# Rao_LDMC      4.5276     1.1447   3.955 0.000781 ***
# Rao_WD       -3.4349     1.1598  -2.962 0.007712 ** 

# ---- Dredge ----

library(MuMIn)

# H1: Diversity

options(na.action = "na.fail")

global_H1 <- lm(
  log_biomass ~ 
    fdis2 + fric2 + fdiv2 +
    raoq + Rao_phylo + Rao_WD + Rao_SLA
    + Rao_LDMC + Rao_leafC + Rao_leafN
    + Rao_leafP + 
    fdis_wd + fdis_sla + fric_sla +
    fdis_ldmc + fric_ldmc +
    fdis_leafc + fric_leafc +
    fdis_leafn + fric_leafn +
    fdis_leafp + fric_leafp +
    Simpson,
  data = dadosreg,
  na.action = na.fail
)

dd_H1  <- dredge(
  global_H1,
  subset =
    # global FD metrics
    !(fdis2 && fric2) &
    !(fdis2 && fdiv2) &
    !(fric2 && fdiv2) &
    
    # RaoQ vs. global FD metrics
    !(raoq && fdis2) &
    !(raoq && fric2) &
    !(raoq && fdiv2) &
    
    # global metrics vs. metrics per trait of the same family
    !(fdis2 && fdis_wd) &
    !(fdis2 && fdis_sla) &
    !(fdis2 && fdis_ldmc) &
    !(fdis2 && fdis_leafc) &
    !(fdis2 && fdis_leafn) &
    !(fdis2 && fdis_leafp) &
    
    !(fric2 && fric_sla) &
    !(fric2 && fric_ldmc) &
    !(fric2 && fric_leafc) &
    !(fric2 && fric_leafn) &
    !(fric2 && fric_leafp) &
    
    # strong redundancies between traits
    !(fdis_leafc && fdis_leafn) &
    !(fric_leafc && fric_leafn) &
    !(fdis_ldmc && fdis_sla) &
    !(fric_ldmc && fric_sla)
)

# H2 - Identity

options(na.action = "na.fail")

global_H2 <- lm(
  log_biomass ~ 
    cwm_sla + cwm_ldmc + cwm_wd + cwm_leafc +
    cwm_leafn + cwm_leafp + cwm_leafcn +
    PC1 + PC2 +
    SESPD + SESMNTD + PSC + PSEab,
  data = dadosreg,
  na.action = na.fail
)

dd_H2 <- dredge(
  global_H2,
  subset =
    # CWM foliar strongly correlated
    !(cwm_leafc && cwm_leafn) &
    !(cwm_leafc && cwm_leafp) &
    !(cwm_leafc && cwm_leafcn) &
    !(cwm_leafn && cwm_leafcn) &
    !(cwm_leafp && cwm_leafcn) &
    
    # PCs do not include the original variables that compose them.
    !(PC1 && cwm_sla) &
    !(PC1 && cwm_ldmc) &
    !(PC1 && cwm_wd) &
    !(PC1 && cwm_leafc) &
    !(PC1 && cwm_leafn) &
    !(PC1 && cwm_leafp) &
    !(PC1 && cwm_leafcn) &
    
    !(PC2 && cwm_sla) &
    !(PC2 && cwm_ldmc) &
    !(PC2 && cwm_wd) &
    !(PC2 && cwm_leafc) &
    !(PC2 && cwm_leafn) &
    !(PC2 && cwm_leafp) &
    !(PC2 && cwm_leafcn) &
    
    # correlated phylogenetic metrics
    !(SESPD && SESMNTD) &
    !(SESPD && PSC) &
    !(SESPD && PSEab) &
    !(SESMNTD && PSC) &
    !(SESMNTD && PSEab) &
    !(PSC && PSEab)
)

# Model average

model.avg(dd_H1, subset = delta < 2,rank = "AICc")
sw(dd_H1)

model.avg(dd_H2, subset = delta < 2,rank = "AICc")
sw(dd_H2)
head(dd_H2, 1)

# H3 - Diversity and Identity

global_H3 <- lm(
  log_biomass ~ 
    # Identity (dominant)
    PC1 + PC2 +
    
    # Functional Diversity - LDMC
    Rao_LDMC +
    
    # Functional Diversity all
    raoq +
    
    # Phylogeny
    Rao_phylo +
    
    # taxonomic
    Simpson,
  data = dadosreg,
  na.action = na.fail
)

dd_H3 <- dredge(
  global_H3)

model.avg(dd_H3, subset = delta < 2,rank = "AICc")
sw(dd_H3)
head(dd_H3, 1)

# Best H3 model:

mh3 <- lm(log_biomass~ Rao_LDMC + PC1 + PC2, data = dadosreg)
summary(MH3) # p= 0.0176
vif(MH3)

# ---- Diversity or identity? ----

dadosreg <- read.csv("~/01 Masters_LA/00 MASTERS-DATA/01 Datasets/02_processed_data/dadosreg.csv", header = TRUE)

# Diversity

mdiv1 <- lm(log_biomass~ scale(Rao_LDMC) + scale(Rao_WD), data = dadosreg)
summary(mdiv1) # R2 = 0.39
AICc(mdiv1) # 37.27

mdiv2 <- lm(log_biomass~ raoq, data = dadosreg)
summary(mdiv2) # R2 = -0.03

mdiv3 <- lm(log_biomass ~ Rao_phylo, data = dadosreg)
summary(mdiv3) # R2 = 0.05

mdiv4 <- lm(log_biomass~Simpson, data = dadosreg)
summary(mdiv4) # R2 = -0.04

mdiv5 <- lm(log_biomass~raoq + Rao_phylo, data = dadosreg)
summary(mdiv5) # R2 = 0.05

mdiv6 <- lm(log_biomass~Rao_phylo + Simpson, data = dadosreg)
summary(mdiv6) # R2 = 0.27


# Identity

mid1 <- lm(log_biomass ~ cwm_leafn, data = dadosreg)
summary(mid1) # R2 = 0.50
AICc(mid1) # 30.95

mid2 <- lm(log_biomass ~ PC1, data = dadosreg)
summary(mid2) # R2 =  0.19

mid3 <- lm(log_biomass ~ PC2, data = dadosreg)
summary(mid3) # R2 =  0.19

mid4 <- lm(log_biomass ~ PC1 + PC2, data = dadosreg)
summary(mid4) # R2 =  0.40

mid5 <- lm(log_biomass ~ cwm_leafn + cwm_leafc, data = dadosreg)
summary(mid5) # R2 =  0.40

# Diversity and identity?

mdiv.id <- lm(log_biomass~ Rao_LDMC + cwm_leafn, data = dadosreg)
summary(mdiv.id) # R2 = 0.59
vif(mdiv.id)
AICc(mdiv.id) # 28.4

mdiv.id2 <- lm(log_biomass~ Rao_LDMC + PC1 + PC2, data = dadosreg)
summary(mdiv.id2) # R2 = 0.46

mdiv.id3 <- lm(log_biomass~ Simpson + cwm_leafn, data = dadosreg)
summary(mdiv.id3) # R2 = 0.57
AICc(mdiv.id3) # 29.03

mdiv.id4 <- lm(log_biomass~ Rao_phylo + cwm_leafn, data = dadosreg)
summary(mdiv.id4) # R2 = 0.51
AICc(mdiv.id4) # 32.33

mdiv.id5 <- lm(log_biomass~ Rao_LDMC + Rao_WD + cwm_leafn, data = dadosreg)
summary(mdiv.id5) # R2 = 0.51
vif(mdiv.id5)
AICc(mdiv.id5) # 27.34613

mdiv.id6 <- lm(log_biomass~ PC1 + PC2, data = dadosreg)
summary(mdiv.id6) # R2 = 0.46

# ---- BIC Table ----

# Create named list of models
models <- list(
  mdiv1 = mdiv1,
  mdiv2 = mdiv2,
  mdiv3 = mdiv3,
  mdiv4 = mdiv4,
  mdiv5 = mdiv5,
  mdiv6 = mdiv6,
  
  mid1 = mid1,
  mid2 = mid2,
  mid3 = mid3,
  mid4 = mid4,
  mid5 = mid5,
  
  mdiv.id  = mdiv.id,
  mdiv.id2 = mdiv.id2,
  mdiv.id3 = mdiv.id3,
  mdiv.id4 = mdiv.id4,
  mdiv.id5 = mdiv.id5
)

# Extract BIC, AICc and R2
model_comp <- data.frame(
  Model = names(models),
  BIC   = sapply(models, BIC),
  AIC   = sapply(models, AIC),
  R2    = sapply(models, function(x) summary(x)$r.squared),
  npar  = sapply(models, function(x) length(coef(x)))
)

# Delta BIC 
model_comp <- model_comp %>%
  arrange(BIC) %>%
  mutate(delta_BIC = BIC - min(BIC))

# ---- View BIC table ----
model_comp

# ---- Is PD a good proxy for Functional composition? Could PD be used to assess ecosystem function? 

# Read Excel file
functional_data <- read.csv("~/01 Masters_LA/00 MASTERS-DATA/01 Datasets/02_processed_data/dadosreg.csv", header = TRUE,row.names = 1)

# Inspect columns
colnames(functional_data)

# Define independent variables')

variables_func <- c("fdis2","fdiv2","raoq","cwm_leafn","Rao_LDMC","SESPD","SESMNTD", "PSC","PSEab","Rao_phylo")

# Create a dataframe with only these variables
data_func <- functional_data %>%
  dplyr::select(all_of(variables_func))

# Optional: Define pretty labels for plot
labels_pretty_func <- c(
  fdis2       = "Functional Dispersion",
  fdiv2       = "Functional Divergence",
  raoq        = "RaoQ",
  
  cwm_leafn   = "CWM Leaf N",
  
  Rao_LDMC    =  "Rao LDMC",
  
  
  Rao_phylo   = "Rao phylogenetic",
  SESPD       = "sesPD",
  SESMNTD     = "sesMNTD",
  PSC         = "PSC",
  PSEab       = "PSE"
)

# Compute correlation matrix
cor_matrix_func <- cor(data_func, method = "spearman", use = "pairwise.complete.obs")

# Function for p-values
cor.mtest <- function(mat, method = "spearman") {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      test <- suppressWarnings(
        cor.test(mat[, i], mat[, j], method = method, exact = FALSE)
      )
      p.mat[i, j] <- p.mat[j, i] <- test$p.value
    }
  }
  
  colnames(p.mat) <- colnames(mat)
  rownames(p.mat) <- colnames(mat)
  return(p.mat)
}

# p matrix
p_matrix_func <- cor.mtest(data_func, method = "spearman")

# Pretty names
pretty_names <- labels_pretty_func[variables_func]

# Rename BOTH matrices
rownames(cor_matrix_func) <- pretty_names
colnames(cor_matrix_func) <- pretty_names

rownames(p_matrix_func) <- pretty_names
colnames(p_matrix_func) <- pretty_names

# Plot
png("~/01 Masters_LA/06 Figures/04 Plots_Functional_Diversity/correlation_matrix_pd.png",
    width = 1400, height = 1000, res = 150)

corrplot(cor_matrix_func,
         method = "circle",
         type = "upper",
         tl.col = "black",
         tl.cex = 0.7,
         p.mat = p_matrix_func,
         sig.level = c(0.001, 0.01, 0.05),
         insig = "label_sig",
         pch.cex = 1.2,
         pch.col = "black")

dev.off()

# ---- Table ----

# Load packages

library(dplyr)
library(MuMIn)
library(flextable)
library(officer) # save word

# Fit models

# Diversity
mdiv1 <- lm(log_biomass ~ Rao_LDMC + Rao_WD, data = dadosreg)
mdiv5 <- lm(log_biomass~raoq + Rao_phylo, data = dadosreg)
mdiv6 <- lm(log_biomass~Rao_phylo + Simpson, data = dadosreg)

# Identity
mid1 <- lm(log_biomass ~ cwm_leafn, data = dadosreg)
mid4 <- lm(log_biomass ~ PC1 + PC2, data = dadosreg)

# Diversity + Identity
mdiv.id  <- lm(log_biomass ~ Rao_LDMC + cwm_leafn, data = dadosreg)
mdiv.id2 <- lm(log_biomass~ Rao_LDMC + PC1, data = dadosreg)
mdiv.id3 <- lm(log_biomass~ Simpson + cwm_leafn, data = dadosreg)
mdiv.id4 <- lm(log_biomass~ Rao_phylo + cwm_leafn, data = dadosreg)
mdiv.id5  <- lm(log_biomass ~ Rao_LDMC + Rao_WD + cwm_leafn, data = dadosreg)

# Organize models

models <- list(
  "Rao_LDMC + Rao_WD"      = mdiv1,
  "RaoQ + Rao phylo"       = mdiv5,
  "Rao phylo + Simpson"    = mdiv6,
  "CWM Leaf N"             = mid1,
  "PC1 + PC2"              = mid4,
  "Rao_LDMC + CWM Leaf N"  = mdiv.id,
  "Rao_LDMC + PC1"         = mdiv.id2,
  "Simpson + CWM Leaf N"   = mdiv.id3,
  "Rao phylo + CWM Leaf N" = mdiv.id4,
  "Rao_LDMC + Rao_WD + CWM Leaf N" = mdiv.id5
)

model_type <- c(
  "Rao_LDMC + Rao_WD"      = "Diversity",
  "RaoQ + Rao phylo"       = "Diversity",
  "Rao phylo + Simpson"    = "Diversity",
  "CWM Leaf N"             = "Identity",
  "PC1 + PC2"              = "Identity",
  "Rao_LDMC + CWM Leaf N"  = "Diversity + Identity",
  "Rao_LDMC + PC1"         = "Diversity + Identity",
  "Simpson + CWM Leaf N"   = "Diversity + Identity",
  "Rao phylo + CWM Leaf N" = "Diversity + Identity",
  "Rao_LDMC + Rao_WD + CWM Leaf N" = "Diversity + Identity"
)


# Pretty predictor names

pretty_terms <- c(
  "Rao_LDMC"  = "Rao LDMC",
  "Rao_WD"    = "Rao WD",
  "raoq"      = "RaoQ",
  "Rao_phylo" = "Rao phylogenetic",
  "Simpson"   = "Simpson",
  "cwm_leafn" = "CWM Leaf N",
  "PC1"       = "PC1 CWM",
  "PC2"       = "PC2 CWM"
)

# Helper functions

# model p-value
get_model_p <- function(model) {
  f <- summary(model)$fstatistic
  pf(f[1], f[2], f[3], lower.tail = FALSE)
}

# sample size
get_n <- function(model) {
  length(model$fitted.values)
}

# format p-values
format_p <- function(p) {
  ifelse(p < 0.0001, "< 0.0001", sprintf("%.4f", p))
}

# extract predictor-level info
get_coef_table <- function(model) {
  coef_tab <- summary(model)$coefficients
  
  coef_df <- data.frame(
    term = rownames(coef_tab),
    estimate = coef_tab[, 1],
    p = coef_tab[, 4],
    stringsAsFactors = FALSE
  ) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      `Predictor variables` = dplyr::recode(term, !!!pretty_terms),
      Slope = ifelse(estimate > 0, "+", "−"),
      P = format_p(p)
    ) %>%
    select(`Predictor variables`, Slope, P)
  
  return(coef_df)
}

# Model level metrics

model_metrics <- data.frame(
  Model_name = names(models),
  AICc = sapply(models, AICc),
  R2 = sapply(models, function(x) summary(x)$r.squared),
  N = sapply(models, get_n),
  Model_P = sapply(models, get_model_p),
  stringsAsFactors = FALSE
) %>%
  mutate(
    deltaAICc = AICc - min(AICc)
  )

# Build final table

tab_list <- lapply(names(models), function(mod_name) {
  
  model <- models[[mod_name]]
  coefs <- get_coef_table(model)
  
  metrics <- model_metrics %>% filter(Model_name == mod_name)
  

  model_row <- data.frame(
    `Model type` = model_type[mod_name],
    `Predictor variables` = "Model",
    Slope = "",
    P = format_p(metrics$Model_P),
    N = as.character(metrics$N),
    R2 = sprintf("%.2f", metrics$R2),
    AICc = sprintf("%.2f", metrics$AICc),
    deltaAICc = sprintf("%.2f", metrics$deltaAICc),
    stringsAsFactors = FALSE
  )
  
  predictor_rows <- data.frame(
    `Model type` = "",
    `Predictor variables` = coefs$`Predictor variables`,
    Slope = coefs$Slope,
    P = coefs$P,
    N = "",
    R2 = "",
    AICc = "",
    deltaAICc = "",
    stringsAsFactors = FALSE
  )
  

  bind_rows(model_row, predictor_rows)
})

#
results_table <- bind_rows(tab_list)

# Order by AICc

model_order <- model_metrics %>%
  arrange(AICc) %>%
  pull(Model_name)

results_table$Model_name_aux <- NA_character_

model_rows <- which(results_table$`Predictor variables` == "Model")
results_table$Model_name_aux[model_rows] <- model_order

# propagate model names downward
current_name <- NA
for(i in seq_len(nrow(results_table))) {
  if(!is.na(results_table$Model_name_aux[i])) current_name <- results_table$Model_name_aux[i]
  results_table$Model_name_aux[i] <- current_name
}

results_table <- results_table %>%
  mutate(Model_name_aux = factor(Model_name_aux, levels = model_order)) %>%
  arrange(Model_name_aux) %>%
  select(-Model_name_aux)

# View table

results_table

# Flextable

ft <- flextable(results_table)
ft <- theme_booktabs(ft)
ft <- autofit(ft)

ft <- align(ft, align = "left",
            j = c("Model.type", "Model.form", "Predictor.variables"),
            part = "all")

ft <- align(ft, align = "center",
            j = c("Slope", "P", "N", "R2", "AICc", "deltaAICc"),
            part = "all")

ft <- valign(ft, valign = "top", part = "all")
ft <- bold(ft, i = ~ Predictor.variables == "Model", bold = TRUE, part = "body")

ft

doc <- read_docx()
doc <- body_add_flextable(doc, ft)

print(doc, target = "~/01 Masters_LA/00 MASTERS-DATA/01 Datasets/02_processed_data/model_comparison_table.docx")


# ---- Variance partition ----

mdiv.id <- lm(log_biomass~ Rao_LDMC +Rao_WD + cwm_leafn, data = dadosreg)
summary(mdiv.id) # R2 = 0.59

library(vegan)

# Response
Y <- data.frame(log_biomass = dadosreg$log_biomass)

# Functional diversity (FD)
FD <- data.frame(
  Rao_LDMC = dadosreg$Rao_LDMC,
  Rao_WD   = dadosreg$Rao_WD
)

# Identity (ID)
ID <- data.frame(
  cwm_leafn = dadosreg$cwm_leafn
)

# Variance partitioning
vp <- varpart(Y, FD, ID)

# Results
vp
summary(vp)

# Plot

jpeg("~/01 Masters_LA/06 Figures/04 Plots_Functional_Diversity/varpart_plot.jpeg", width = 1900, height = 1550, res = 300)

plot(vp,
     bg = c("lightblue", "mistyrose"),
     Xnames = c("FD", "Identity"),
     id.size = 1.2,
     cutoff = 0,
     cex = 1.2)
dev.off()

# ---- Proportional Variance partition ----

mdiv.id <- lm(log_biomass ~ Rao_LDMC + Rao_WD + cwm_leafn, data = dadosreg)
summary(mdiv.id)

library(vegan)
library(eulerr)

# Response
Y <- data.frame(log_biomass = dadosreg$log_biomass)

# Functional diversity (FD)
FD <- data.frame(
  Rao_LDMC = dadosreg$Rao_LDMC,
  Rao_WD   = dadosreg$Rao_WD
)

# Identity (ID)
ID <- data.frame(
  cwm_leafn = dadosreg$cwm_leafn
)

# Variance partitioning
vp <- varpart(Y, FD, ID)

# See fractions
vp
vp$part$indfract

# Extract adjusted fractions
fr <- as.data.frame(vp$part$indfract)
fr

a <- as.numeric(fr["[a] = X1|X2", "Adj.R.squared"])
b <- as.numeric(fr["[b] = X2|X1", "Adj.R.squared"])
c <- as.numeric(fr["[c]",         "Adj.R.squared"])

a; b; c
is.numeric(a); is.numeric(b); is.numeric(c)

# Proportional diagram

fit <- euler(c(
  "FD" = a,
  "Identity" = b,
  "FD&Identity" = c
))

# Plot

jpeg("~/01 Masters_LA/06 Figures/04 Plots_Functional_Diversity/varpart_plot_proportional.jpeg",
     width = 1900, height = 1550, res = 300)

plot(
  fit,
  fills = list(fill = c("lightblue", "mistyrose"), alpha = 0.6),
  edges = list(col = "grey30"),
  labels = list(font = 2, cex = 1.4),
  quantities = list(
    labels = c(
      paste0(round(a * 100, 1), "%"),
      paste0(round(b * 100, 1), "%"),
      paste0(round(c * 100, 1), "%")
    ),
    cex = 1.2
  )
)

dev.off()

# ---- Species selection - Laughlin ----

library(Select)

# Trait matrix
traits <- read.csv("~/01 Masters_LA/00 MASTERS-DATA/01 Datasets/01_raw_data/funcional_ml_regua_2.csv",row.names = 1)

# trait to constrain: high leaf N
t2c <- as.matrix(traits[, "leafN", drop = FALSE])

# trait to diversify: LDMC
t2d <- as.matrix(traits[, "LDMC", drop = FALSE])

# constraint value: high CWM leaf N
con <- c(leafN = unname(quantile(traits$leafN, 0.75)))

# run optimization
res <- selectSpecies(
  t2c = t2c,
  con = con,
  t2d = t2d,
  obj = "QH"
)

str(res)
names(res)

# See spp and probabilities 
out <- data.frame(
  species = rownames(traits),
  prob = res$prob,
  leafN = traits$leafN,
  LDMC = traits$LDMC
)

# sort from most important to least important
out <- out[order(out$prob, decreasing = TRUE), ]

out

library(ggplot2)

ggplot(out, aes(x = leafN, y = LDMC, size = prob)) +
  geom_point(alpha = 0.8) +
  geom_text(aes(label = species), hjust = 0, nudge_x = 0.03, size = 3) +
  scale_size(range = c(2, 10)) +
  labs(
    x = "Leaf N",
    y = "LDMC",
    size = "Selection probability"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank())

# What species should I plant and in what proportions?

out <- data.frame(
  species = rownames(traits),
  prob = as.numeric(res$prob),
  leafN = traits$leafN,
  LDMC = traits$LDMC
)

out <- out[order(out$prob, decreasing = TRUE), ]
out$cumprob <- cumsum(out$prob)

out

# Select species that 80%

selected_80 <- subset(out, cumprob <= 0.80)


if(max(selected_80$cumprob) < 0.80){
  selected_80 <- out[1:(nrow(selected_80) + 1), ]
}

selected_80

#

selected_90 <- subset(out, cumprob <= 0.90)

if(max(selected_90$cumprob) < 0.90){
  selected_90 <- out[1:(nrow(selected_90) + 1), ]
}

selected_90

# Planting proportions

selected_80$planting_prop <- selected_80$prob / sum(selected_80$prob)
selected_80

# other plot option

out <- out[order(out$prob, decreasing = TRUE), ]

top10 <- out[1:10, ]
out$group <- ifelse(out$species %in% top10$species, "Key species", "Other species")

# Plot

library(ggrepel)

p <- ggplot(out, aes(x = leafN, y = LDMC)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.20, ymax = 0.30,
           fill = "#f0f0f0", alpha = 0.6) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.30, ymax = 0.40,
           fill = "#d9d9d9", alpha = 0.6) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.40, ymax = 0.55,
           fill = "#f0f0f0", alpha = 0.6) +
  geom_vline(xintercept = unname(target), linetype = "dashed", linewidth = 0.8) +
  geom_point(aes(size = prob), color = "grey75", alpha = 0.7) +
  geom_point(data = top10, aes(size = prob), color = "#2E7D32", alpha = 0.95) +
  geom_text_repel(
    data = top10,
    aes(label = species),
    size = 4,
    max.overlaps = Inf,
    box.padding = 0.5,
    point.padding = 0.5,
    min.segment.length = 0,
    nudge_x = 0.03
  ) +
  scale_size(range = c(2, 10)) +
  labs(
    x = "Leaf nitrogen (CWM driver)",
    y = "LDMC (functional diversity axis)",
    size = "Selection probability"
  ) +
  coord_cartesian(
    xlim = c(min(out$leafN) - 0.05, max(out$leafN) + 0.15),
    ylim = c(0.20, 0.55),
    clip = "on"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  )

print(p)

ggsave(
  filename = "~/01 Masters_LA/06 Figures/04 Plots_Functional_Diversity/species_selection_plot.jpeg",
  plot = p,
  width = 9,
  height = 5,
  dpi = 300,
  units = "in",
  bg = "white"
)


# ---- Individual Plots ----

# Fit the model
fit <- lm(log_biomass ~ fric_ldmc, data = dadosreg)
summary(fit)

# Extract R² e p-value
r2 <- summary(fit)$r.squared
p  <- coef(summary(fit))["fric_ldmc", "Pr(>|t|)"]

# Text
subtxt <- sprintf("R² = %.2f\np = %s",
                  r2,
                  ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))

# plot
g <- dadosreg %>%
  ggplot(aes(fric_ldmc, log_biomass)) +
  geom_point(size = 3, alpha = 0.5) + 
  geom_smooth(method = "lm", se = TRUE, colour = "yellow") +
  labs(
    x = "FRic - Leaf Dry Matter Content (LDMC)",
    y = expression("log Net Biomass (g m"^-2*" year"^-1*")"),
    title = "The Relationship between Functional Richness and Biomass"
  ) +
  annotate("text", 
           x = max(dadosreg$fric_ldmc, na.rm = TRUE) * 0.45, 
           y = max(dadosreg$log_produt, na.rm = TRUE) * 1.002, 
           label = subtxt, 
           hjust = 1, vjust = 1,
           size = 3.5) +
  theme_minimal(base_size = 11)

g

# Save
ggsave("~/01 Masters_LA/06 Figures/04 Plots_Functional_Diversity/FRic_LDMC.jpeg", g, width = 15, height = 10, units = "cm", dpi = 600)


### FDIS ###

fit <- lm(log_biomass ~ fdis_ldmc, data = dadosreg)
summary(fit)

# Extraindo R² e p-valor
r2 <- summary(fit)$r.squared
p  <- coef(summary(fit))["fdis_ldmc", "Pr(>|t|)"]

# Texto formatado
subtxt <- sprintf("R² = %.2f\np = %s",
                  r2,
                  ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))

# Gráfico
g <- dadosreg %>%
  ggplot(aes(fdis_ldmc, log_biomass)) +
  geom_point(size = 3, alpha = 0.5) + 
  geom_smooth(method = "lm", se = TRUE, colour = "brown") +
  labs(
    x = "FDis - Leaf Dry Matter Content (LDMC)",
    y = expression("log Biomass (kg)"),
    title = "The Relationship between Functional Richness and Biomass"
  ) +
  annotate("text", 
           x = max(dadosreg$fric_ldmc, na.rm = TRUE) * 0.1, 
           y = max(dadosreg$log_produt, na.rm = TRUE) * 1.002, 
           label = subtxt, 
           hjust = 1, vjust = 1,
           size = 3.5) +
  theme_minimal(base_size = 11)

g

# Salvar
ggsave("~/01 Masters_LA/06 Figures/04 Plots_Functional_Diversity/FDis_LDMC.jpeg", g, width = 15, height = 10, units = "cm", dpi = 600)

## SR ##

# Fit the model
fit <- lm(log_biomass ~ SR, data = dadosreg)
summary(fit)

# Extract R² e p-value
r2 <- summary(fit)$r.squared
p  <- coef(summary(fit))["SR", "Pr(>|t|)"]

# Text
subtxt <- sprintf("R² = %.2f\np = %s",
                  r2,
                  ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))

# plot
g <- dadosreg %>%
  ggplot(aes(SR, log_biomass)) +
  geom_point(size = 3, alpha = 0.5) + 
  geom_smooth(method = "lm", se = TRUE, colour = "orange") +
  labs(
    x = "Species Richness",
    y = expression("log Biomass (kg)")) +
  annotate("text", 
           x = max(dadosreg$SR, na.rm = TRUE) * 0.28, 
           y = max(dadosreg$log_produt, na.rm = TRUE) * 1.002, 
           label = subtxt, 
           hjust = 1, vjust = 1,
           size = 3.5) +
  theme_minimal(base_size = 11)

g

# Save
ggsave("~/01 Masters_LA/06 Figures/04 Plots_Functional_Diversity/SR.jpeg", g, width = 15, height = 10, units = "cm", dpi = 600)

## CWM LDMC ##

# 1) Fit the model
fit <- lm(log_biomass ~ cwm_ldmc, data = dadosreg)
sm  <- summary(fit)

# 2) Extract stats safely
r2    <- sm$r.squared
adjr2 <- sm$adj.r.squared
p     <- sm$coefficients["cwm_ldmc", "Pr(>|t|)"]

subtxt <- sprintf("R² = %.2f | adj. R² = %.2f\np = %s",
                  r2, adjr2,
                  ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))

# 3) Nice positions for the annotation inside the panel
x_annot <- min(dadosreg$cwm_ldmc,  na.rm = TRUE)
y_annot <- max(dadosreg$log_biomass, na.rm = TRUE)

# 4) Plot
g <- dadosreg %>%
  ggplot(aes(x = cwm_ldmc, y = log_biomass)) +
  geom_point(size = 3, alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.9, colour = "goldenrod") +
  labs(
    x = "CWM — Leaf Dry Matter Content (LDMC)",
    y = expression("log Biomass (kg)"),
    title = "Relationship between CWM-LDMC and Productivity"
  ) +
  annotate("label",
           x = x_annot, y = y_annot,
           hjust = 0, vjust = 1,
           label = subtxt, size = 3.5) +
  theme_minimal(base_size = 12)

g

## CWM Leaf C ##

# Fit the model
fit <- lm(log_biomass ~ cwm_leafc, data = dadosreg)
summary(fit)

# Extract R² e p-value
r2 <- summary(fit)$r.squared
p  <- coef(summary(fit))["cwm_leafc", "Pr(>|t|)"]

# Text
subtxt <- sprintf("R² = %.2f\np = %s",
                  r2,
                  ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))

# plot
g <- dadosreg %>%
  ggplot(aes(cwm_leafc, log_biomass)) +
  geom_point(size = 3, alpha = 0.5) + 
  geom_smooth(method = "lm", se = TRUE, colour = "green") +
  labs(
    x = "CWM Leaf C content (%)",
    y = expression("log Biomass (g m"^-2*" year"^-1*")")) +
  annotate("text", 
           x = max(dadosreg$cwm_leafc, na.rm = TRUE), 
           y = max(dadosreg$log_biomass, na.rm = TRUE), 
           label = subtxt, 
           hjust = 1, vjust = 1,
           size = 3.5) +
  theme_minimal(base_size = 11)

g

# Save
ggsave("~/01 Masters_LA/06 Figures/04 Plots_Functional_Diversity/CWM_LeafC.jpeg", g, width = 15, height = 10, units = "cm", dpi = 600)

## CWM Leaf N ##

# Fit the model
fit <- lm(log_biomass ~ cwm_leafn, data = dadosreg)
summary(fit)

# Extract R² e p-value
r2 <- summary(fit)$r.squared
p  <- coef(summary(fit))["cwm_leafn", "Pr(>|t|)"]

# Text
subtxt <- sprintf("R² = %.2f\np = %s",
                  r2,
                  ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))

# plot
g <- dadosreg %>%
  ggplot(aes(cwm_leafn, log_biomass)) +
  geom_point(size = 3, alpha = 0.5) + 
  geom_smooth(method = "lm", se = TRUE, colour = "red") +
  labs(
    x = "CWM Leaf N content (%)",
    y = expression("log Biomass")) +
  annotate("text", 
           x = max(dadosreg$cwm_leafn, na.rm = TRUE), 
           y = max(dadosreg$log_biomass, na.rm = TRUE), 
           label = subtxt, 
           hjust = 1, vjust = 1,
           size = 3.5) +
  theme_minimal(base_size = 11)

g

# Save
ggsave("~/01 Masters_LA/06 Figures/04 Plots_Functional_Diversity/CWM_LeafN.jpeg", g, width = 15, height = 10, units = "cm", dpi = 600)


## Rao LDMC ##

# Fit the model
fit <- lm(log_biomass ~ Rao_LDMC, data = dadosreg)
summary(fit)

# Extract R² e p-value
r2 <- summary(fit)$r.squared
p  <- coef(summary(fit))["Rao_LDMC", "Pr(>|t|)"]

# Text
subtxt <- sprintf("R² = %.2f\np = %s",
                  r2,
                  ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))

# plot
g <- dadosreg %>%
  ggplot(aes(Rao_LDMC, log_biomass)) +
  geom_point(size = 3, alpha = 0.5) + 
  geom_smooth(method = "lm", se = TRUE, colour = "blue") +
  labs(
    x = "RaoQ LDMC",
    y = expression("log Biomass (g)")) +
  annotate("text", 
           x = max(dadosreg$Rao_LDMC*0.5, na.rm = TRUE), 
           y = max(dadosreg$log_biomass, na.rm = TRUE), 
           label = subtxt, 
           hjust = 1, vjust = 1,
           size = 3.5) +
  theme_minimal(base_size = 11)

g

# Save
ggsave("~/01 Masters_LA/06 Figures/04 Plots_Functional_Diversity/Rao_LDMC.jpeg", g, width = 15, height = 10, units = "cm", dpi = 600)




## sesMNTD

# Fit the model
fit <- lm(log_biomass ~ SESMNTD, data = dadosreg)
summary(fit)

# Extract R² e p-value
r2 <- summary(fit)$r.squared
p  <- coef(summary(fit))["SESMNTD", "Pr(>|t|)"]

# Text
subtxt <- sprintf("R² = %.2f\np = %s",
                  r2,
                  ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))

# plot
g <- dadosreg %>%
  ggplot(aes(SESMNTD, log_biomass)) +
  geom_point(size = 3, alpha = 0.5) + 
  geom_smooth(method = "lm", se = TRUE, colour = "yellow") +
  labs(
    x = "sesMNTD",
    y = expression("log Biomass (kg)")) +
  annotate("text", 
           x = max(dadosreg$fric_ldmc, na.rm = TRUE) * 0.45, 
           y = max(dadosreg$log_biomass, na.rm = TRUE) * 1.002, 
           label = subtxt, 
           hjust = 1, vjust = 1,
           size = 3.5) +
  theme_minimal(base_size = 11)

g

# Save
ggsave("~/01 Masters_LA/06 Figures/04 Plots_Functional_Diversity/sesMNTD.jpeg", g, width = 15, height = 10, units = "cm", dpi = 600)



## Isolating sites ##

ggplot(dadosreg, aes(x = cwm_ldmc, y = log_produt, color = sitemis)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_classic() +
  labs(x = "CWM LDMC", y = "Log(Biomass/age)", color = "Site")

ggplot(dadosreg, aes(x = fdis_ldmc, y = log_produt, color = sitemis)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_classic() +
  labs(x = "FDis LDMC", y = "Log(Biomass/age)", color = "Site")

ggplot(dadosreg, aes(x = RaoQ_LDMC, y = log_produt, color = sitemis)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_classic() +
  labs(x = "FDis LDMC", y = "Log(Biomass/age)", color = "Site")

####### Partial Effects Div + Id model #######


library(ggeffects)
library(ggplot2)
library(patchwork)
library(tidyverse)

# Scale

dadosreg2 <- dadosreg %>%
  mutate(
    z_biomass   = as.numeric(scale(log_biomass)),
    z_leafn     = as.numeric(scale(cwm_leafn)),
    z_Rao_LDMC  = as.numeric(scale(Rao_LDMC))
  )

mdiv.id.z <- lm(z_biomass ~ z_Rao_LDMC + z_leafn, data = dadosreg2)
summary(mdiv.id.z)

pred_leafn <- ggpredict(mdiv.id.z, terms = "z_leafn")
pred_ldmc  <- ggpredict(mdiv.id.z, terms = "z_Rao_LDMC")

p1 <- ggplot() +
  geom_point(
    data = dadosreg2,
    aes(x = z_leafn, y = z_biomass),
    size = 2.5,
    alpha = 0.8
  ) +
  geom_line(
    data = pred_leafn,
    aes(x = x, y = predicted),
    linewidth = 1.2
  ) +
  geom_ribbon(
    data = pred_leafn,
    aes(x = x, ymin = conf.low, ymax = conf.high),
    alpha = 0.2
  ) +
  labs(
    x = "CWM Leaf N (scaled)",
    y = "Log biomass (scaled)",
    tag = "(a)"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    plot.tag = element_text(face = "bold")
  )

p2 <- ggplot() +
  geom_point(
    data = dadosreg2,
    aes(x = z_Rao_LDMC, y = z_biomass),
    size = 2.5,
    alpha = 0.8
  ) +
  geom_line(
    data = pred_ldmc,
    aes(x = x, y = predicted),
    linewidth = 1.2
  ) +
  geom_ribbon(
    data = pred_ldmc,
    aes(x = x, ymin = conf.low, ymax = conf.high),
    alpha = 0.2
  ) +
  labs(
    x = "Rao LDMC (scaled)",
    y = NULL,
    tag = "(b)"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    plot.tag = element_text(face = "bold")
  )

final_plot <- p1 + p2
print(final_plot)

# Coefficient plot (Univariate) ----

library(dplyr)
library(broom)
library(ggplot2)

# Fit standardized univariate models
models <- list(
  "RaoQ"             = lm(log_biomass ~ scale(raoq), data = dadosreg),
  "Rao phylogenetic" = lm(log_biomass ~ scale(Rao_phylo), data = dadosreg),
  "Rao LDMC"         = lm(log_biomass ~ scale(Rao_LDMC), data = dadosreg),
  "Rao WD"           = lm(log_biomass ~ scale(Rao_WD), data = dadosreg),
  "Simpson"          = lm(log_biomass ~ scale(Simpson), data = dadosreg),
  "CWM Leaf N"       = lm(log_biomass ~ scale(cwm_leafn), data = dadosreg),
  "PC1"              = lm(log_biomass ~ scale(PC1), data = dadosreg),
  "PC2"              = lm(log_biomass ~ scale(PC2), data = dadosreg)
)

# Build dataframe for plotting
coef_plot <- bind_rows(
  lapply(names(models), function(x) {
    tidy(models[[x]], conf.int = TRUE) %>%
      filter(term != "(Intercept)") %>%
      mutate(predictor = x)
  })
) %>%
  mutate(
    group = ifelse(
      predictor %in% c("RaoQ", "Rao phylogenetic", "Rao LDMC", "Rao WD", "Simpson"),
      "Diversity", "Identity"
    ),
    significance = ifelse(
      conf.low <= 0 & conf.high >= 0,
      "Not significant", "Significant"
    ),
    predictor = factor(
      predictor,
      levels = rev(c(
        "RaoQ",
        "Rao phylogenetic",
        "Rao LDMC",
        "Rao WD",
        "Simpson",
        "CWM Leaf N",
        "PC1",
        "PC2"
      ))
    )
  )

# Plot
p_coef <- ggplot(coef_plot, aes(x = estimate, y = predictor, color = group)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.7) +
  geom_hline(yintercept = 3.5, linetype = "dotted", linewidth = 0.5, color = "grey40") +
  geom_errorbar(
    aes(xmin = conf.low, xmax = conf.high),
    width = 0.15,
    orientation = "y",
    linewidth = 0.7
  ) +
  geom_point(
    aes(fill = ifelse(significance == "Significant", group, "Not significant"), color = group),
    shape = 21,
    size = 3.2,
    stroke = 1
  ) +
  scale_color_manual(
    values = c(
      "Diversity" = "#8B2E2E",
      "Identity" = "#0B5D69"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Diversity" = "#8B2E2E",
      "Identity" = "#0B5D69",
      "Not significant" = "white"
    )
  ) +
  labs(
    x = "Standardized coefficient",
    y = NULL,
    fill = NULL
  ) +
  guides(fill = "none") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    legend.position = "right"
  )

p_coef

ggsave(
  filename = "~/01 Masters_LA/06 Figures/04 Plots_Functional_Diversity/univariate_coefplot.jpeg",
  plot = p_coef,
  width = 7,
  height = 5.5,
  units = "in",
  dpi = 300,
  bg = "white"
)


# ---- 3D PLOT ----


# install.packages("plotly")

# 1) Fit
t <- lm(log_produt ~ cwm_ldmc + cwm_wd, data = dadosreg)

# prediction grid
xseq <- seq(min(dadosreg$cwm_ldmc, na.rm=TRUE),
            max(dadosreg$cwm_ldmc, na.rm=TRUE), length.out=40)
yseq <- seq(min(dadosreg$cwm_wd,   na.rm=TRUE),
            max(dadosreg$cwm_wd,   na.rm=TRUE), length.out=40)

grid  <- expand.grid(cwm_ldmc = xseq, cwm_wd = yseq)
zpred <- matrix(predict(t, newdata = grid),
                nrow = length(xseq), ncol = length(yseq))

# plot
p <- plot_ly() |>
  add_markers(data = dadosreg,
              x = ~cwm_ldmc, y = ~cwm_wd, z = ~log_produt,
              color = ~sitemis, marker = list(size = 4, opacity = 0.9),
              name = "Observed") |>
  add_trace(x = xseq, y = yseq, z = zpred,
            type = "surface", opacity = 0.6, showscale = FALSE,
            name = "LM surface") |>
  plotly::layout(scene = list(
    xaxis = list(title = "CWM LDMC"),
    yaxis = list(title = "CWM WD"),
    zaxis = list(title = "log(Biomass/age)")
  ))

p

# install.packages("webshot2")

# Save

htmlwidgets::saveWidget(p, "~/01 Masters_LA/06 Figures/04 Plots_Functional_Diversity/3D_model.html")   # salva o gráfico interativo
webshot("~/01 Masters_LA/06 Figures/04 Plots_Functional_Diversity/3D_model.html", "3D_model.jpeg", vwidth = 1600, vheight = 1200)


### FACET WRAP ###

# SR
dadosreg %>%
  ggplot(aes(SR,log_produt))+
  geom_point(size=3,alpha=0.5)+ #alpha é transparência
  geom_smooth(method = lm,se=F)+ #linear model, se = F não tem intervalo de confi
  facet_wrap(~sitemis)+ # divide em dois gráficos diferentes
  labs(x="SR",
       y="Productivity",
       title = "Species Richness and Productivity in ecological restoration plantings")+
  theme_bw()

ggsave("~/01 Masters_LA/06 Figures/04 Plots_Functional_Diversity/SR_prod_facetwrap.jpg", width = 15, height = 10, units = "cm", dpi = 300)

# FDIS_SLA
dadosreg %>%
  ggplot(aes(fric_wd,log_produt))+
  geom_point(size=3,alpha=0.5)+ #alpha é transparência
  geom_smooth(method = lm,se=F)+ #linear model, se = F não tem intervalo de confi
  facet_wrap(~sitemis)+ # divide em dois gráficos diferentes
  labs(x="SR",
       y="Productivity",
       title = "Functional Richness - SLA")+
  theme_bw()

ggsave("~/01 Masters_LA/06 Figures/04 Plots_Functional_Diversity/FDis_SLA_prod_facetwrap.jpg", width = 15, height = 10, units = "cm", dpi = 300)


# Divide by site
modelo_ml <- lm(log_produt ~ fric_wd, data = dadosreg %>% filter(sitemis == "ML"))
modelo_regua <- lm(log_produt ~ fric_wd, data = dadosreg %>% filter(sitemis == "REGUA"))

summary(modelo_ml) # not significant
summary(modelo_regua) # not significant

# FRic_LDMC
dadosreg %>%
  ggplot(aes(fric_ldmc,log_produt))+
  geom_point(size=3,alpha=0.5)+ #alpha é transparência
  geom_smooth(method = lm,se=F)+ #linear model, se = F não tem intervalo de confi
  facet_wrap(~sitemis)+ # divide em dois gráficos diferentes
  labs(x="Fric",
       y="Productivity",
       title = "Functional Richness and Productivity in ecological restoration plantings")+
  theme_bw()

# ggsave("FRIC_LDMC_facetwrap.jpg", width = 15, height = 10, units = "cm", dpi = 300)

# Divide by site
modelo_ml <- lm(log_produt ~ fric_ldmc, data = dadosreg %>% filter(sitemis == "ML"))
modelo_regua <- lm(log_produt ~ fric_ldmc, data = dadosreg %>% filter(sitemis == "REGUA"))

summary(modelo_ml) # not significant
summary(modelo_regua) # not significant
 

# ---- PHYLO SIGNAL FOR TRAITS ----

## ASSEMBLE THE TREE ONLY WITH THE ML AND REGUA SPECIES

# load the packages

library("V.PhyloMaker2")
library(writexl)
library(ape)
library(picante)

# input the sample species list
example <- read.csv("01 Datasets/01_raw_data/filogenia_ml_regua.csv")

### generate a phylogeny for the sample species list
tree <- phylo.maker(example, tree = GBOTB.extended.TPL,output.sp.list = TRUE,nodes = nodes.info.1.TPL, scenarios="S3")
tree_ok <- tree$scenario.3
summary(tree_ok)

dados <- read.csv("01 Datasets/01_raw_data/funcional_ml_regua_2.csv", row.names = 1,header = T)

rownames(dados) <- gsub(" ", "_", rownames(dados))

intersect(rownames(dados), tree_teste$tip.label)
length(intersect(rownames(dados), tree_teste$tip.label))

tree_teste <- multi2di(tree_ok)

resultado <- multiPhylosignal(dados[, c("WD", "SLA", "LDMC","leafC","leafN", "leafP")], tree_teste)
# LDMC - Strong signal
# LeafC - moderate phylogenetic signal
# LeafN marginal

# Fabaceae has more leafN than others?

phylo <- read.csv("01 Datasets/01_raw_data/filogenia_ml_regua.csv")

# padronizar nomes no phylo
phylo2 <- phylo %>%
  mutate(species = gsub(" ", "_", species))

dados2 <- dados2 %>%
  left_join(phylo2[, c("species", "family")], by = "species")

# criar variável Fabaceae vs Other
dados2 <- dados2 %>%
  mutate(
    grupo = ifelse(family == "Fabaceae", "Fabaceae", "Other families")
  )

ggplot(dados2, aes(x = grupo, y = leafN, fill = grupo)) +
  geom_boxplot(alpha = 0.6, width = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.6) +
  scale_fill_manual(values = c("#2c7fb8", "#f03b20")) +
  labs(
    x = "",
    y = "Leaf N",
    title = "Leaf nitrogen in Fabaceae vs other families"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    axis.line = element_line(color = "black")
  )

wilcox.test(leafN ~ grupo, data = dados2) # 0.2

# Select species based on trait optimization ----

library(Select)
library(dplyr)
library(ggplot2)
library(ggrepel)

# 1. Load trait data
traits <- read.csv(
  "~/01 Masters_LA/00 MASTERS-DATA/01 Datasets/01_raw_data/funcional_ml_regua_2.csv",
  row.names = 1
)

# 2. Define traits
# Trait to constrain: high leaf N
t2c <- as.matrix(traits[, "leafN", drop = FALSE])

# Trait to diversify: LDMC
t2d <- as.matrix(traits[, "LDMC", drop = FALSE])

# Target value for leaf N
target <- as.numeric(quantile(traits$leafN, 0.75))

# Named constraint
con <- c(leafN = target)

# 3. Run optimization
res <- selectSpecies(
  t2c = t2c,
  con = con,
  t2d = t2d,
  obj = "QH"
)

# 4. Organize output
out <- data.frame(
  species = rownames(traits),
  prob = as.numeric(res$prob),
  leafN = traits$leafN,
  LDMC = traits$LDMC
)

out <- out[order(out$prob, decreasing = TRUE), ]
top10 <- out[1:10, ]

# 5. Plot
p_select <- ggplot(out, aes(x = leafN, y = LDMC)) +
  
  # LDMC bands in the background
  annotate(
    "rect",
    xmin = -Inf, xmax = Inf,
    ymin = 0.20, ymax = 0.30,
    fill = "grey95", alpha = 0.8
  ) +
  annotate(
    "rect",
    xmin = -Inf, xmax = Inf,
    ymin = 0.30, ymax = 0.40,
    fill = "grey50", alpha = 0.35
  ) +
  annotate(
    "rect",
    xmin = -Inf, xmax = Inf,
    ymin = 0.40, ymax = 0.55,
    fill = "grey85", alpha = 0.8
  ) +
  
  # All species
  geom_point(
    aes(size = prob),
    color = "grey60",
    alpha = 0.7
  ) +
  
  # Top 10 species
  geom_point(
    data = top10,
    aes(size = prob),
    color = "#1b7837",
    alpha = 0.95
  ) +
  
  # Leaf N target
  geom_vline(
    xintercept = target,
    linetype = "dashed",
    linewidth = 0.8
  ) +
  
  # Labels for top 10
  geom_text_repel(
    data = top10,
    aes(label = species),
    size = 4,
    max.overlaps = Inf,
    box.padding = 0.5,
    point.padding = 0.5,
    min.segment.length = 0,
    seed = 123
  ) +
  
  scale_size(range = c(2, 10)) +
  
  labs(
    x = "Leaf nitrogen",
    y = "LDMC",
    size = "Selection probability"
  ) +
  
  coord_cartesian(
    xlim = c(min(out$leafN) - 0.05, max(out$leafN) + 0.15),
    ylim = c(0.20, 0.55),
    clip = "on"
  ) +
  
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.position = "right"
  )

p_select

ggsave(
  filename = "~/01 Masters_LA/06 Figures/04 Plots_Functional_Diversity/select_species_leafN_LDMC.jpeg",
  plot = p_select,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300,
  bg = "white"
)

# ---- DECOUPLED EFFECTS ----


# Load required packages
library(devtools)
devtools::install_github("carmonalab/decouple")

library(decouple)
library(ape)        # For phylogenetic tree manipulation
library(vegan)      # For dissimilarity calculations
library(picante)    # For MPD calculation
library(lme4)       # For mixed models
library(MuMIn)      # For AICc and model R²

# Prepare input data

# Extract only LDMC trait from the trait matrix
traits_ldmc <- as.matrix(traits_df["LDMC"])  # species x 1 trait

# Ensure community matrix is species x plots
abund_matrix <- comunidade_ldmc

# Ensure species names match across tree, traits, and community matrix
all(rownames(traits_ldmc) %in% tree_teste$tip.label)  # Should return TRUE
all(rownames(abund_matrix) %in% tree_teste$tip.label) # Should also return TRUE

# Run the decoupling function
res <- decouple(
  traits = traits_ldmc,
  phylo = tree_teste,
  abund = abund_matrix,
  dist_method = "euclidean",  # for single continuous trait
  stand.x = TRUE,             # standardize trait
  stand.P = TRUE              # standardize phylogenetic distances
)

# Extract decoupled dissimilarity matrices
dcFdist <- res$dcFdist  # Functional dissimilarity independent of phylogeny
dcPdist <- res$dcPdist  # Phylogenetic dissimilarity independent of traits

# Calculate mean pairwise dissimilarity (MPD) per plot
# transpose community matrix to plots x species as required by picante
mpd_dcF <- mpd(samp = t(abund_matrix), dis = dcFdist)
mpd_dcP <- mpd(samp = t(abund_matrix), dis = dcPdist)

# Add results to your data frame for modeling
dadosreg$mpd_dcF <- mpd_dcF[rownames(dadosreg)]
dadosreg$mpd_dcP <- mpd_dcP[rownames(dadosreg)]

# Fit mixed models using decoupled metrics
modelo_dcF <- lmer(log_produt ~ mpd_dcF + (1 | sitemis), data = dadosreg, REML = FALSE)
modelo_dcP <- lmer(log_produt ~ mpd_dcP + (1 | sitemis), data = dadosreg, REML = FALSE)

# Summarize results
summary(modelo_dcF)
summary(modelo_dcP)

# Compare models
r.squaredGLMM(modelo_dcF)
r.squaredGLMM(modelo_dcP)
AICc(modelo_dcF)
AICc(modelo_dcP)

# ---- Melodic function ----

melodic <- function(samp, dis, type = "both") {
  if (!is.matrix(samp)) samp <- as.matrix(samp)
  if (!is.matrix(dis))  dis  <- as.matrix(dis)
  
  if (is.null(colnames(samp)) || is.null(colnames(dis))) {
    stop("Both samp and dis must have column names.")
  }
  
  N <- nrow(samp)
  out <- list()
  
  if (type %in% c("both", "abundance")) {
    out$abundance <- list(
      mpd = numeric(N),
      rao = numeric(N),
      simpson = numeric(N)
    )
  }
  
  if (type %in% c("both", "presence")) {
    out$presence <- list(
      mpd = numeric(N),
      rao = numeric(N),
      simpson = numeric(N)
    )
  }
  
  out$richness <- numeric(N)
  
  for (i in 1:N) {
    spp_in_sample <- names(samp[i, samp[i, ] > 0])
    out$richness[i] <- sum(samp[i, ] > 0)
    
    if (length(spp_in_sample) > 1) {
      sample_dis <- dis[spp_in_sample, spp_in_sample, drop = FALSE]
      
      if (type %in% c("both", "abundance")) {
        abund_w <- samp[i, spp_in_sample] / sum(samp[i, spp_in_sample])
        sample_weights <- outer(abund_w, abund_w)
        
        out$abundance$mpd[i] <- weighted.mean(
          sample_dis[lower.tri(sample_dis)],
          sample_weights[lower.tri(sample_weights)]
        )
        out$abundance$rao[i] <- sum(sample_weights * sample_dis)
        out$abundance$simpson[i] <- sum(2 * sample_weights[lower.tri(sample_weights)])
      }
      
    } else {
      if (type %in% c("both", "abundance")) {
        out$abundance$mpd[i] <- NA
        out$abundance$rao[i] <- 0
        out$abundance$simpson[i] <- 0
      }
    }
  }
  
  return(out)
}


# ---- Helper: calculate Rao for one trait ----

calculate_rao_single_trait <- function(comm, traits, trait_name) {
  trait_df <- traits[, trait_name, drop = FALSE]
  
  dist_mat <- as.matrix(dist(trait_df, method = "euclidean"))
  dist_mat <- dist_mat / max(dist_mat)
  
  res <- melodic(
    samp = comm,
    dis = dist_mat,
    type = "abundance"
  )
  
  res$abundance$rao
}


# ---- Helper: calculate all metrics for one site ----

calculate_functional_metrics <- function(comm_file, trait_file, site_name) {
  comm <- read.csv(
    comm_file,
    row.names = 1,
    header = TRUE,
    sep = ";",
    check.names = FALSE
  )
  
  traits <- read.csv(
    trait_file,
    row.names = 1,
    header = TRUE
  )
  
  # Clean species names
  rownames(traits) <- gsub(" ", "_", trimws(rownames(traits)))
  colnames(comm)   <- gsub(" ", "_", trimws(colnames(comm)))
  
  # Keep only shared species
  shared_species <- intersect(colnames(comm), rownames(traits))
  comm   <- comm[, shared_species, drop = FALSE]
  traits <- traits[shared_species, , drop = FALSE]
  
  # Overall multivariate Rao
  traits_scaled <- scale(traits)
  dist_func <- as.matrix(dist(traits_scaled, method = "euclidean"))
  dist_func <- dist_func / max(dist_func)
  
  res <- melodic(
    samp = comm,
    dis = dist_func,
    type = "abundance"
  )
  
  # Rao for each trait
  rao_wd    <- calculate_rao_single_trait(comm, traits, "WD")
  rao_sla   <- calculate_rao_single_trait(comm, traits, "SLA")
  rao_ldmc  <- calculate_rao_single_trait(comm, traits, "LDMC")
  rao_leafc <- calculate_rao_single_trait(comm, traits, "leafC")
  rao_leafn <- calculate_rao_single_trait(comm, traits, "leafN")
  rao_leafp <- calculate_rao_single_trait(comm, traits, "leafP")
  
  data.frame(
    plot = rownames(comm),
    site = site_name,
    richness = res$richness,
    MPD_func = res$abundance$mpd,
    raoq = res$abundance$rao,   # overall Rao replacing old raoq
    Simpson = res$abundance$simpson,
    Rao_WD = rao_wd,
    Rao_SLA = rao_sla,
    Rao_LDMC = rao_ldmc,
    Rao_leafC = rao_leafc,
    Rao_leafN = rao_leafn,
    Rao_leafP = rao_leafp,
    row.names = NULL
  )
}


# ---- Run for both sites ----

metrics_ml <- calculate_functional_metrics(
  comm_file  = "01 Datasets/01_raw_data/comunidade_ml.csv",
  trait_file = "01 Datasets/01_raw_data/funcional_ml.csv",
  site_name  = "ML"
)

metrics_regua <- calculate_functional_metrics(
  comm_file  = "01 Datasets/01_raw_data/comunidade_regua.csv",
  trait_file = "01 Datasets/01_raw_data/funcional_regua.csv",
  site_name  = "REGUA"
)

new_metrics <- rbind(metrics_ml, metrics_regua)


# ---- Join with dadosreg ----

dadosreg <- merge(
  dadosreg,
  new_metrics,
  by = c("plot", "site"),
  all.x = TRUE
)

write.csv(
  dadosreg,
  "~/01 Masters_LA/00 MASTERS-DATA/01 Datasets/02_processed_data/dadosreg.csv",
  row.names = FALSE
)


# ---- Rao Phylogenetic ----

library("V.PhyloMaker2")

# input the sample species list
example <- read_csv("~/01 Masters_LA/00 MASTERS-DATA/01 Datasets/01_raw_data/filogenia_total.csv")

### generate a phylogeny for the sample species list
tree <- phylo.maker(example, tree = GBOTB.extended.TPL,output.sp.list = TRUE,nodes = nodes.info.1.TPL, scenarios="S3")
tree_ok <- tree$scenario.3

# Prune tree

colnames(comunidade_ml) <- gsub(" ", "_", trimws(colnames(comunidade_ml)))
colnames(comunidade_regua) <- gsub(" ", "_", trimws(colnames(comunidade_regua)))

spp_ml    <- colnames(comunidade_ml)
spp_regua <- colnames(comunidade_regua)

spp_total <- union(spp_ml, spp_regua)

library(ape)

tree_subset <- drop.tip(tree_ok, setdiff(tree_ok$tip.label, spp_total))

# Distance matrix
phylo_dissim <- cophenetic(tree_subset)

# 0 and 1
phylo_dissim_std <- (phylo_dissim - min(phylo_dissim)) / 
  (max(phylo_dissim) - min(phylo_dissim))

# Melodic Rao phy calc - ML

res_phylo_ml <- melodic(
  samp = comunidade_ml,
  dis = phylo_dissim_std,
  type = "abundance"
)

rao_phylo_ml <- res_phylo_ml$abundance$rao # extract

# REGUA

res_phylo_regua <- melodic(
  samp = comunidade_regua,
  dis = phylo_dissim_std,
  type = "abundance"
)

rao_phylo_regua <- res_phylo_regua$abundance$rao

# BIND

melodic_ML$Rao_phylo <- rao_phylo_ml
melodic_REGUA$Rao_phylo <- rao_phylo_regua


library(dplyr)

melodic_all <- bind_rows(melodic_ML, melodic_REGUA)

write.csv(
  melodic_all,
  "01 Datasets/02_processed_data/melodic_all_sites.csv",
  row.names = FALSE
)

dadosreg <- dadosreg %>%
  rename(plot = parcela)

dados_final <- left_join(dadosreg, melodic_all, by = "plot")


# Save

rownames(dados_final) <- dados_final$plot
dados_final$plot <- NULL

write.csv(
  dados_final,
  "~/01 Masters_LA/00 MASTERS-DATA/01 Datasets/02_processed_data/dadosreg.csv",
  row.names = TRUE
)

dadosreg <- read.csv(
  "~/01 Masters_LA/00 MASTERS-DATA/01 Datasets/02_processed_data/dadosreg.csv",
  header = TRUE,
  row.names = 1
)

head(dadosreg)

all(dadosreg$site.x == dadosreg$site.y)

dadosreg$site <- dadosreg$site.x
dadosreg$site.x <- NULL
dadosreg$site.y <- NULL


dadosreg <- dadosreg[, c("site", setdiff(names(dadosreg), "site"))]

write.csv(
  dadosreg,
  "~/01 Masters_LA/00 MASTERS-DATA/01 Datasets/02_processed_data/dadosreg.csv",
  row.names = F
)

# ---- PCPS ----


# input the sample species list without CSA
example <- read.csv("01 Datasets/01_raw_data/filogenia_ml_regua.csv")

### generate a phylogeny for the sample species list
tree <- phylo.maker(example, tree = GBOTB.extended.TPL,output.sp.list = TRUE,nodes = nodes.info.1.TPL, scenarios="S3")
tree_ok <- tree$scenario.3

phylo.dissim = cophenetic(tree_ok)
std.pdis = (phylo.dissim-min(phylo.dissim))/(max(phylo.dissim)-min(phylo.dissim))
pdis.ord = std.pdis[order(row.names(std.pdis)), order(row.names(std.pdis))]

# install.packages("PCPS")
library(PCPS)

### Importing the family groups
# Define Phylogenetic groups
grupos <- example$family

# Importing the community matrix
com_total

setdiff(colnames(com_total), colnames(pdis.ord))
setdiff(colnames(pdis.ord), colnames(com_total))
colnames(com_total) <- gsub(" ", "_", colnames(com_total))

# Running PCPS
res <- pcps(com_total, pdis.ord)
summary(res)

# Calculating explained variance
eig <- res$values$Eigenvalue
var_exp <- eig / sum(eig)
pcps1_var <- round(var_exp[1] * 100, 2)
pcps2_var <- round(var_exp[2] * 100, 2)

# Scores for sites and species
scores_pcps <- scores.pcps(res, choice = c(1,2))
scores_sites <- scores_pcps$scores.sites
scores_species <- scores_pcps$scores.species

# Group arrows (families)
grupos <- example$family
apg_arrows <- apply(scores_species, 2, function(x) tapply(x, list(grupos), mean)) %>%
  as.data.frame()
apg_arrows$angle <- atan2(apg_arrows$pcps.1, apg_arrows$pcps.2) * (180/pi)
apg_arrows$labels <- c("Anac","Aster", "Canna", "Caric", "Faba", "Lami", "Malva", "Melast", "Petive","Primula","Verbe")

# Merging the data into a single data.frame
df_plot <- cbind(as.data.frame(scores_sites),
                 Biomass = dadosreg$log_biomass,
                 raoq = dadosreg$raoq,
                 Rao_phylo = dadosreg$Rao_phylo,
                 cwm_leafN = dadosreg$cwm_leafn)

### only biomass

ggplot(data = df_plot, aes(x = pcps.1, y = pcps.2)) +
  
  geom_point(color = "black", size = 4.5, alpha = 0.6) +
  
  geom_point(aes(color = Biomass), size = 4) +
  
  scale_colour_gradientn(
    colors = c("black", "lightyellow", "red"),
    name = "Aboveground \nBiomass (kg)"
  ) +
  
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(
    x = paste0("PCPS1 (", pcps1_var, "%)"),
    y = paste0("PCPS2 (", pcps2_var, "%)")
  ) +
  
  geom_label(
    data = apg_arrows,
    aes(x = pcps.1, y = pcps.2, label = labels),
    fontface = "bold", fill = "grey", alpha = 0.5,
    color = "black", size = 5
  ) +
  
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.8),
    axis.text = element_text(size = 13),
    axis.title = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 15)
  )

ggsave("~/01 Masters_LA/06 Figures/02 plots/pcps_oficial.jpeg", width = 15, height = 10, dpi = 300, units = "in")

### cwm leaf n

ggplot(data = df_plot, aes(x = pcps.1, y = pcps.2)) +
  
  geom_point(color = "black", size = 4.5, alpha = 0.6) +
  
  geom_point(aes(color = cwm_leafN), size = 4) +
  
  scale_colour_gradientn(
    colors = c("black", "lightyellow", "red"),
    name = "CWM Leaf N"
  ) +
  
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(
    x = paste0("PCPS1 (", pcps1_var, "%)"),
    y = paste0("PCPS2 (", pcps2_var, "%)")
  ) +
  
  geom_label(
    data = apg_arrows,
    aes(x = pcps.1, y = pcps.2, label = labels),
    fontface = "bold", fill = "grey", alpha = 0.5,
    color = "black", size = 5
  ) +
  
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.8),
    axis.text = element_text(size = 13),
    axis.title = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 15)
  )

ggsave("~/01 Masters_LA/06 Figures/04 Plots_Functional_Diversity/pcps_cwmleafn.jpeg", width = 15, height = 10, dpi = 300, units = "in")

# Biomass and CWM Leaf N

ggplot(data = df_plot, aes(x = pcps.1, y = pcps.2)) +
  
  geom_point(aes(size = Biomass),
             color = "black", alpha = 0.6) +
  
  geom_point(aes(color = cwm_leafN, size = Biomass)) +
  
  scale_colour_gradientn(
    colors = c("black", "lightyellow", "red"),
    name = "CWM Leaf N"
  ) +
  
  scale_size_continuous(
    name = "Log Biomass",
    range = c(3, 8)
  ) +
  
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(
    x = paste0("PCPS1 (", pcps1_var, "%)"),
    y = paste0("PCPS2 (", pcps2_var, "%)")
  ) +
  
  geom_label(
    data = apg_arrows,
    aes(x = pcps.1, y = pcps.2, label = labels),
    fontface = "bold", fill = "grey", alpha = 0.5,
    color = "black", size = 5
  ) +
  
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.8),
    axis.text = element_text(size = 13),
    axis.title = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 15)
  )

ggsave("~/01 Masters_LA/06 Figures/04 Plots_Functional_Diversity/pcps_Biomass_leafn.jpeg", width = 15, height = 10, dpi = 300, units = "in")


# ---- OLD GLMS ----

# Functional diversity

fdis <- lm(log_biomass~fdis,data = dadosreg)
summary(fdis) # p-value: 0.232     
residuos_fdis <- residuals(produt.fdis)
shapiro.test(residuos_fdis) #NORMAL

fric <- lm(log_biomass~fric,data = dadosreg)
summary(fric) # p-value: 0.944  
residuos_fric <- residuals(produt.fric)
shapiro.test(residuos_fric) #NORMAL

fdiv <- lm(log_biomass~fdiv,data = dadosreg)
summary(fdiv) # p-value: 0.459
residuos_fdiv <- residuals(produt.fdiv)
shapiro.test(residuos_fdiv) #NORMAL

# With foliage results for C and N

fdis <- lm(log_biomass~fdis2,data = dadosreg)
summary(fdis) # p-value: 0.806         

fric <- lm(log_biomass~fric2,data = dadosreg)
summary(fric) # p-value: 0.0535 .  

fdiv <- lm(log_biomass~fdiv2,data = dadosreg)
summary(fdiv) # p-value: 0.722

#

cwm_wd <- lm(log_biomass~cwm_wd,data = dadosreg)
summary(cwm_wd) # p-value: 0.234 
residuos_cwm_wd <- residuals(produt.cwm_wd)
shapiro.test(residuos_cwm_wd) #NORMAL

cwm_sla <- lm(log_biomass~cwm_sla,data = dadosreg)
summary(cwm_sla) # p-value: 0.537 
residuos_cwm_sla <- residuals(produt.cwm_sla)
shapiro.test(residuos_cwm_sla) #NORMAL

cwm_ldmc <- lm(log_biomass~cwm_ldmc,data = dadosreg)
summary(cwm_ldmc) # p-value: 0.0595 . 
residuos_cwm_ldmc <- residuals(produt.cwm_ldmc)
shapiro.test(residuos_cwm_ldmc) #NORMAL

cwm_leafc <- lm(log_biomass~cwm_leafc,data = dadosreg)
summary(cwm_leafc) # p-value: 0.0238 *
residuos_cwm_leafc <- residuals(cwm_leafc)
shapiro.test(residuos_cwm_leafc) #NORMAL

cwm_leafn <- lm(log_biomass~cwm_leafn,data = dadosreg)
summary(cwm_leafn) # p-value: 9.251e-05
residuos_cwm_leafn <- residuals(cwm_leafn)
shapiro.test(residuos_cwm_leafn) #NORMAL

cwm_leafcn <- lm(log_biomass~cwm_leafcn,data = dadosreg)
summary(cwm_leafcn) # p-value: 0.01504 *
residuos_cwm_leafcn <- residuals(cwm_leafcn)
shapiro.test(residuos_cwm_leafcn) #NORMAL

cwm_leafp <- lm(log_biomass~cwm_leafp,data = dadosreg)
summary(cwm_leafp) # p-value: 0.0564 .
residuos_cwm_leafp <- residuals(cwm_leafp)
shapiro.test(residuos_cwm_leafp) #NORMAL

fdis_wd <- lm(log_biomass~fdis_wd,data = dadosreg)
summary(fdis_wd) # p-value: 0.8 
residuos_fdis_wd <- residuals(produt.fdis_wd)
shapiro.test(residuos_fdis_wd) #NORMAL

fric_wd <- lm(log_biomass~fric_wd,data = dadosreg)
summary(produt.fric_wd) # p-value: 0.892
residuos_fric_wd <- residuals(produt.fric_wd)
shapiro.test(residuos_fric_wd) #NORMAL

fdis_sla <- lm(log_biomass~fdis_sla,data = dadosreg)
summary(fdis_sla) # p-value: 0.124
residuos_fdis_sla <- residuals(produt.fdis_sla)
shapiro.test(residuos_fdis_sla) #NORMAL

fric_sla <- lm(log_biomass~fric_sla,data = dadosreg)
summary(fric_sla) # p-value: 0.267 
residuos_fric_sla <- residuals(produt.fric_sla)
shapiro.test(residuos_fric_sla) #NORMAL

fdis_ldmc <- lm(log_biomass~fdis_ldmc,data = dadosreg)
summary(fdis_ldmc) # p-value: 0.0469 *
residuos_fdis_ldmc <- residuals(produt.fdis_ldmc)
shapiro.test(residuos_fdis_ldmc) #NORMAL

fric_ldmc <- lm(log_biomass~fric_ldmc,data = dadosreg)
summary(fric_ldmc) # p-value:  0.0399 * 
residuos_fric_ldmc <- residuals(produt.fric_ldmc)
shapiro.test(residuos_fric_ldmc) #NORMAL

#

fdis_leafc <- lm(log_biomass~fdis_leafc,data = dadosreg)
summary(fdis_leafc) # p-value: 0.138
residuos_fdis_leafc <- residuals(fdis_leafc)
shapiro.test(residuos_fdis_leafc) #NORMAL

fric_leafc <- lm(log_biomass~fric_leafc,data = dadosreg)
summary(fric_leafc) # p-value:  0.149
residuos_fric_leafc <- residuals(fric_leafc)
shapiro.test(residuos_fric_leafc) #NORMAL

fdis_leafn <- lm(log_biomass~fdis_leafn,data = dadosreg)
summary(fdis_leafn) # p-value: 0.206
residuos_fdis_leafc <- residuals(fdis_leafn)
shapiro.test(residuos_fdis_leafn) #NORMAL

fric_leafn <- lm(log_biomass~fric_leafn,data = dadosreg)
summary(fric_leafn) # p-value:  0.852
residuos_fric_leafn <- residuals(fric_leafn)
shapiro.test(residuos_fric_leafn) #NORMAL

fdis_leafp <- lm(log_biomass~fdis_leafp,data = dadosreg)
summary(fdis_leafp) # p-value: 0.77
residuos_fdis_leafp <- residuals(fdis_leafp)
shapiro.test(residuos_fdis_leafp) #NORMAL

fric_leafp <- lm(log_biomass~fric_leafp,data = dadosreg)
summary(fric_leafp) # p-value:  0.6703
residuos_fric_leafp <- residuals(fric_leafp)
shapiro.test(residuos_fric_leafp) #NORMAL

#

fdis_cwm_wd <- lm(fdis_wd~cwm_wd,data = dadosreg)
summary(fdis_cwm_wd) # p-value: 0.05865 .

# Taxonomic diversity
SR <- lm(log_biomass~SR,data = dadosreg)
summary(SR) # p-value: 0.393
residuos_sr <- residuals(SR)
shapiro.test(residuos_sr) #NORMAL

# Phylogenetic diversity

SESPD <- lm(log_biomass~SESPD,data = dadosreg)
summary(produt.SESPD) # p-value: 0.0497 * 
residuos_SESPD <- residuals(produt.SESPD)
shapiro.test(residuos_SESPD) #NORMAL

SESMPD <- lm(log_biomass~SESMPD,data = dadosreg)
summary(produt.SESMPD) # p-value: 0.187
residuos_SESMPD <- residuals(produt.SESMPD)
shapiro.test(residuos_SESMPD) #NORMAL

SESMNTD <- lm(log_biomass~SESMNTD,data = dadosreg)
summary(produt.SESMNTD) # p-value: 0.0331 *
residuos_SESMNTD <- residuals(produt.SESMNTD)
shapiro.test(residuos_SESMNTD) #NORMAL

PSV <- lm(log_biomass~PSV,data = dadosreg)
summary(produt.PSV) # p-value: 0.211
residuos_PSV <- residuals(produt.PSV)
shapiro.test(residuos_PSV) #NORMAL

PSC <- lm(log_biomass~PSC,data = dadosreg)
summary(produt.PSC) # p-value: 0.0456 * 

PSEab <- lm(log_biomass~PSEab,data = dadosreg)
summary(PSEab) # p-value: 0.105

# Multiple variables

t <- lm(log_biomass~cwm_ldmc+cwm_wd,data = dadosreg)
summary(t) # p-value: 0.02186 *
AICc(t)

t2 <- lm(log_biomass ~ cwm_ldmc + cwm_wd + fdis_ldmc + fric_ldmc, data = dadosreg)
summary(t2) # p-value = 0.07547

# FDis only
m_fdis <- lm(log_biomass ~ fdis_ldmc + fdis_wd, data = dadosreg)
summary(m_fdis) # p-value = 0.0176 *

m_fdis <- lm(log_biomass ~ fdis_sla + fdis_ldmc + fdis_wd, data = dadosreg)
summary(m_fdis) #p=0.04034 *

# FRic only
m_fric <- lm(log_biomass ~ cwm_ldmc + cwm_wd + fric_ldmc, data = dadosreg)
summary(m_fric)

# 2. Nonlinear (quadratic) test for FDis
m_fdis_quad <- lm(log_biomass ~ cwm_ldmc + cwm_wd +
                    fdis_ldmc + I(fdis_ldmc^2), data = dadosreg)
summary(m_fdis_quad) # p-value: 0.08198

### Decoupled ###

dcF <- lm(log_biomass~dcF,data = dadosreg)
summary(dcF) # p-value: 0.0379 *
residuos_dcF <- residuals(produt.dcF)
shapiro.test(residuos_dcF) #NORMAL

dcP <- lm(log_biomass~dcP,data = dadosreg)
summary(produt.dcP) # p-value: 0.432
residuos_dcP <- residuals(produt.dcP)
shapiro.test(residuos_dcP) #NORMAL

s <- lm(log_biomass~cwm_ldmc+cwm_wd+dcF,data = dadosreg)
summary(s)

ss <- lm(log_biomass~cwm_ldmc+cwm_wd+dcP,data = dadosreg)
summary(ss)

# test

ntrees <- lm(log_biomass~n_trees, data = dadosreg)
summary(ntrees) # p= 0.933

ntrees <- lm(cwm_ldmc~n_trees, data = dadosreg)
summary(ntrees) # p= 0.933


# PCA Scores
m_pca <- lm(log_biomass~ PC1 + PC2 + fric2, data = dadosreg)
summary(m_pca) # p= 0.02186
car::vif(m_pca)


m1 <- lm(log_biomass~cwm_leafn + fric2, data = dadosreg)
summary(m1) # p= 0.0176


## Candidate models

# Functional (dispersion)
m1 <- lm(log_biomass~fdis_ldmc + fdis_wd, data = dadosreg)
summary(m1) # p= 0.0176
# ---- old Selected model ----

# CWM (identity)
m2 <- lm(log_biomass~cwm_ldmc + cwm_wd, data = dadosreg)
summary(m2) # p= 0.02186

# Phylogenetic

m3 <- lm(log_biomass~SESMNTD, data = dadosreg)
summary(m3) # p= 0.0331

m4 <- lm(log_biomass~PSC, data = dadosreg)
summary(m4) # p= 0.04558

model_selection <- model.sel(
  m1 = m1,
  m2 = m2,
  m3 = m3,
  m4 = m4,
  rank = "AICc"
)

model_table <- as.data.frame(model_selection) %>%
  mutate(Model = rownames(.)) %>%
  select(Model, df, logLik, AICc, delta, weight)

model_table

model.avg(model_selection)
sw(model_selection) # fdis_ldmc and fdis_wd have the largest Σwi (0.37 each)


m <- lm(log_biomass~fdis_ldmc + fdis_wd + cwm_ldmc + cwm_wd, data = dadosreg)
summary(m)

vif(m)

cor(dadosreg |> select(fdis_ldmc,fdis_wd,cwm_ldmc,cwm_wd))