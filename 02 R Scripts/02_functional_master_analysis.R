
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
# ---- Selected model ----

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
    raoq + Rao_phylo +
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
    !(cwm_leafn && cwm_leafp) &
    !(cwm_leafn && cwm_leafcn) &
    !(cwm_leafp && cwm_leafcn) &
    
    # foliar economic spectrum
    !(cwm_sla && cwm_ldmc) &
    
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

# Diversity

mdiv1 <- lm(log_biomass~ Rao_LDMC+Rao_WD, data = dadosreg)
summary(mdiv1) # R2 = 0.39

mdiv2 <- lm(log_biomass~ raoq, data = dadosreg)
summary(mdiv2) # R2 = -0.02712

mdiv3 <- lm(log_biomass~ Rao_phylo, data = dadosreg)
summary(mdiv3) # R2 = 0.05

# Identity

mid1 <- lm(log_biomass ~ cwm_leafn, data = dadosreg)
summary(mid1) # R2 = 0.50

mid2 <- lm(log_biomass ~ PC1, data = dadosreg)
summary(mid2) # R2 =  0.19

# Diversity and identity?

mdiv.id <- lm(log_biomass~ Rao_LDMC + cwm_leafn, data = dadosreg)
summary(mdiv.id) # R2 = 0.59
vif(mdiv.id)

# ---- Model selection table ----

library(MuMIn)
library(dplyr)
library(purrr)
library(tibble)
library(flextable)
library(officer)

# Extract predictors from model
get_predictors_pretty <- function(model) {
  terms_vec <- attr(terms(model), "term.labels")
  
  pretty_names <- c(
    cwm_leafn = "CWM leaf N",
    cwm_wd = "CWM wood density",
    cwm_ldmc = "CWM LDMC",
    cwm_sla = "CWM SLA",
    cwm_leafc = "CWM leaf C",
    cwm_leafp = "CWM leaf P",
    cwm_leafcn = "CWM leaf C:N",
    raoq = "RaoQ",
    Rao_phylo = "Phylogenetic Rao",
    Simpson = "Simpson",
    fdis_ldmc = "FDis LDMC",
    fdis_wd = "FDis wood density",
    fdis_sla = "FDis SLA",
    fdis_leafc = "FDis leaf C",
    fdis_leafn = "FDis leaf N",
    fdis_leafp = "FDis leaf P",
    fric2 = "FRic",
    fdis2 = "FDis",
    fdiv2 = "FDiv",
    fric_leafp = "FRic leaf P",
    fric_leafn = "FRic leaf N",
    fric_leafc = "FRic leaf C",
    fric_ldmc = "FRic LDMC",
    fric_sla = "FRic SLA",
    SESPD = "sesPD",
    SESMNTD = "sesMNTD",
    PSC = "PSC",
    PSEab = "PSE",
    PC1 = "PC1 CWM",
    PC2 = "PC2 CWM"
  )
  
  terms_vec <- ifelse(terms_vec %in% names(pretty_names),
                      pretty_names[terms_vec],
                      terms_vec)
  
  paste(terms_vec, collapse = " + ")
}

# Function to create hypothesis table

make_top_models_table <- function(dd, hypothesis_name, n_top = 2) {
  
  # pega os melhores modelos
  mods <- get.models(dd, 1:n_top)
  
  # extrai tabela do dredge
  dd_top <- as.data.frame(dd) %>%
    slice(1:n_top)
  
  # monta tabela final
  out <- tibble(
    Hypothesis = hypothesis_name,
    Predictors = map_chr(mods, get_predictors_pretty),
    K = dd_top$df,
    logLik = round(dd_top$logLik, 3),
    AICc = round(dd_top$AICc, 3),
    `ΔAICc` = round(dd_top$delta, 3),
    Weight = round(dd_top$weight, 3)
  )
  
  return(out)
}


# Create a table for each hypothesis

tab_H1 <- make_top_models_table(dd_H1, "Diversity hypothesis", n_top = 2)
tab_H2 <- make_top_models_table(dd_H2, "Identity hypothesis", n_top = 2)
tab_H3 <- make_top_models_table(dd_H3, "Diversity + identity hypothesis", n_top = 2)

# Bind

tab_final_models <- bind_rows(tab_H1, tab_H2, tab_H3)

tab_final_models

ft_models <- flextable(tab_final_models) %>%
  autofit()

ft_models

# Export to word

doc <- read_docx() %>%
  body_add_par("Table X. Top-ranked models for each hypothesis.", style = "heading 1") %>%
  body_add_flextable(ft_models)

print(doc, target = "C:/Users/Laíla Arnauth/OneDrive/Documentos/01 Masters_LA/00 MASTERS-DATA/01 Datasets/02_processed_data/top_models_hypotheses.docx")

# ---- Best model Table ----

best_H3 <- get.models(dd_H3, 1)[[1]]
summary(best_H3)

library(broom)
library(dplyr)
library(flextable)

tab_H3 <- tidy(best_H3) %>%
  mutate(
    term = dplyr::recode(term,
                         "(Intercept)" = "Intercept",
                         "cwm_leafn"   = "CWM leaf N",
                         "raoq"        = "RaoQ",
                         "Simpson"     = "Simpson"
    ),
    across(where(is.numeric), ~ round(.x, 3))
  ) %>%
  select(
    Predictor = term,
    Estimate = estimate,
    `SE` = std.error,
    `t` = statistic,
    `p` = p.value
  )

ft_H3 <- flextable(tab_H3)
ft_H3

library(officer)

doc <- read_docx() %>%
  body_add_par("Table X. Final model for H3.", style = "heading 1") %>%
  body_add_flextable(ft_H3)

print(doc, target = "C:/Users/Laíla Arnauth/OneDrive/Documentos/01 Masters_LA/00 MASTERS-DATA/01 Datasets/02_processed_data/H3_table.docx")


# ---- INDIVIDUAL PLOTS ----

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

# gerar predições
pred_ldmc  <- ggpredict(mdiv.id, terms = "Rao_LDMC")
pred_leafn <- ggpredict(mdiv.id, terms = "cwm_leafn")

plot_partial <- function(pred, data, var, xlab) {
  
  ggplot() +
    # intervalo de confiança
    geom_ribbon(data = pred,
                aes(x = x, ymin = conf.low, ymax = conf.high),
                alpha = 0.2) +
    
    # linha do modelo
    geom_line(data = pred,
              aes(x = x, y = predicted),
              linewidth = 1) +
    
    # pontos observados
    geom_point(data = data,
               aes_string(x = var, y = "log_biomass"),
               alpha = 0.6) +
    
    labs(
      x = xlab,
      y = "Log biomass"
    ) +
    
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black"),
      axis.text = element_text(size = 11),
      axis.title = element_text(size = 12)
    )
}

p1 <- plot_partial(pred_ldmc, dadosreg, "Rao_LDMC", "Rao (LDMC)")
p2 <- plot_partial(pred_leafn, dadosreg, "cwm_leafn", "CWM leaf N")

fig <- p1 + p2 + plot_layout(ncol = 3)

fig


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
 
# ---- COEFPLOT ----

#  keep same rows across all models (recommended) 
dados_uni <- dadosreg %>%
  dplyr::select(log_biomass, fdis, fdis_ldmc, fdis_wd, fdis_sla,
                cwm_ldmc, cwm_wd, SESPD, SESMNTD, SR) %>%
  tidyr::drop_na()

# Predictors in the EXACT order (top -> bottom in the plot)
preds <- c("fdis",
           "fdis_ldmc", "fdis_wd", "fdis_sla",
           "cwm_ldmc", "cwm_wd",
           "SESPD", "SESMNTD",
           "SR")

labels <- c(
  fdis      = "FDis (overall)",
  fdis_ldmc = "FDis LDMC",
  fdis_wd   = "FDis Wood Density",
  fdis_sla  = "FDis SLA",
  cwm_ldmc  = "CWM LDMC",
  cwm_wd    = "CWM Wood Density",
  SESPD     = "sesPD",
  SESMNTD   = "sesMNTD",
  SR        = "Species Richness"
)

# One univariate standardized model per predictor
mods <- map(preds, \(p){
  form <- reformulate(termlabels = sprintf("scale(%s)", p),
                      response   = "scale(log_biomass)")
  lm(form, data = dados_uni)
})
names(mods) <- preds

coef_all_uni <- imap_dfr(mods, \(mod, pred){
  tidy(mod, conf.int = TRUE) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      predictor = pred,
      term = labels[[pred]],
      sig = p.value < 0.05
    )
})

# Factor levels to control y-order (ggplot shows first level at bottom, so we reverse)
desired_order <- labels[preds]  # top->bottom target order
coef_all_uni <- coef_all_uni %>%
  mutate(term = factor(term, levels = rev(desired_order)))

# Plot
coef_plot_uni <- ggplot(coef_all_uni, aes(x = estimate, y = term)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbar(
    aes(xmin = conf.low, xmax = conf.high),
    height = 0.2,
    orientation = "y"
  ) +
  geom_point(aes(shape = sig), size = 3) +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1)) +
  labs(x = "Standardized effect size (β)", y = NULL) +
  theme_classic()

coef_plot_uni

ggsave(
  filename = "~/01 Masters_LA/06 Figures/04 Plots_Functional_Diversity/Coefplot_Univar_Funct.jpeg",
  plot = coef_plot_uni,
  width = 6,
  height = 5,
  units = "in",
  dpi = 300
)

# ---- COEFPLOT - Partial ----

# Models scaled
m1z <- lm(scale(log_biomass) ~ scale(fdis_ldmc) + scale(fdis_wd), data = dadosreg)
m2z <- lm(scale(log_biomass) ~ scale(cwm_ldmc) + scale(cwm_wd), data = dadosreg)
m3z <- lm(scale(log_biomass) ~ scale(SESMNTD), data = dadosreg)
m4z <- lm(scale(log_biomass) ~ scale(SR), data = dadosreg)

# Function to extract coefficients
get_coef <- function(mod, model_name){
  tidy(mod, conf.int = TRUE) %>%
    filter(term != "(Intercept)") %>%
    mutate(model = model_name,
           sig = p.value < 0.05)
}

coef_all <- bind_rows(
  get_coef(m1z, "Functional (FDis)"),
  get_coef(m2z, "Identity (CWM)"),
  get_coef(m3z, "Phylogenetic"),
  get_coef(m4z, "Richness")
) %>%
  mutate(term = gsub("scale\\(|\\)", "", term)) %>%
  mutate(term = dplyr::recode(term,
                              "fdis_ldmc" = "FDis LDMC",
                              "fdis_wd"   = "FDis Wood Density",
                              "cwm_ldmc"  = "CWM LDMC",
                              "cwm_wd"    = "CWM Wood Density",
                              "SESMNTD"   = "sesMNTD",
                              "SR"        = "Species Richness"
  )) %>%
  mutate(term = factor(term, levels = rev(unique(term))))

# Plot
coef_plot <- ggplot(coef_all, aes(x = estimate, y = term)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_point(aes(shape = sig), size = 3) +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1)) +
  labs(
    x = "Standardized effect size (β)",
    y = NULL
  ) +
  theme_classic()

ggsave(
  filename = "~/01 Masters_LA/06 Figures/04 Plots_Functional_Diversity/Coefplot_Functional_Phylo_SR.jpeg",
  plot = coef_plot,
  width = 6,
  height = 5,
  units = "in",
  dpi = 300
)


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
