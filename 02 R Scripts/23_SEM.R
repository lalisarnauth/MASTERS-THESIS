
# ---- SEM ----

library(lavaan)

# Load the data

dadosmisto <- read.csv("01 Datasets/01_raw_data/dadosmisto.csv",
                       header = TRUE)

dadosmisto1 <- dadosmisto[-c(1:7), ]

# Scale

dados_scaled <- as.data.frame(scale(dadosmisto1[, sapply(dadosmisto1, is.numeric)]))

# ---- Model A ----

model_A <- '
  ############################
  # Climate → Soil
  ############################
  c.n_soloid ~ season_ppt

  ############################
  # Phylogenetic/Functional structure
  ############################
  sr    ~ faba
  pcps1 ~ pse + faba

  ############################
  # Soil + phylo → Biomass
  ############################
  log_produt ~ sr + pcps1 + c.n_soloid + season_ppt

  ############################
  # Correlated exogenous
  ############################
  faba ~~ pse
  season_ppt ~~ faba + pse
'


fit_finalA <- sem(
  model = model_A,
  data  = dados_scaled,
  estimator = "ML"
)

summary(fit_finalA, standardized = TRUE, fit.measures = TRUE)

# ---- Model B ----

model_B <- '
  ############################
  # Climate → Soil
  ############################
  c.n_soloid ~ season_ppt

  ############################
  # Soil influences structure
  ############################
  sr    ~ faba + c.n_soloid
  pcps1 ~ pse + faba + c.n_soloid

  ############################
  # Biomass drivers
  ############################
  log_produt ~ sr + pcps1 + c.n_soloid + season_ppt

  ############################
  # Correlated exogenous
  ############################
  faba ~~ pse
  season_ppt ~~ faba + pse
'

fit_finalB <- sem(
  model = model_B,
  data  = dados_scaled,
  estimator = "ML"
)

summary(fit_finalB, standardized = TRUE, fit.measures = TRUE)

# ---- Model C ----

model_C <- '
  ############################
  # Climate → Soil
  ############################
  c.n_soloid ~ season_ppt

  ############################
  # Soil influences structure
  ############################
  sr    ~ c.n_soloid + season_ppt
  pcps1 ~ pse + faba

  ############################
  # Biomass drivers
  ############################
  log_produt ~ sr + pcps1 + c.n_soloid + season_ppt + faba

  ############################
  # Correlated exogenous
  ############################
  faba ~~ pse
  season_ppt ~~ faba + pse
'

fit_finalC <- sem(
  model = model_C,
  data  = dados_scaled,
  estimator = "ML"
)

summary(fit_finalC, standardized = TRUE, fit.measures = TRUE)

# ---- Model D ----

model_D <- '
  ############################
  # Climate → Soil
  ############################
  c.n_soloid ~ season_ppt

  ############################
  # Soil influences structure
  ############################
  sr    ~ faba + c.n_soloid
  pcps1 ~ season_ppt

  ############################
  # Biomass drivers
  ############################
  log_produt ~ sr + pcps1 + c.n_soloid + season_ppt

  ############################
  # Correlated exogenous
  ############################
  faba ~~ pse
  season_ppt ~~ faba + pse
'

fit_finalD <- sem(
  model = model_D,
  data  = dados_scaled,
  estimator = "ML"
)

summary(fit_finalD, standardized = TRUE, fit.measures = TRUE)

# ---- Model D - N of trees ----

model_D <- '
  # Structure and diversity
  sr ~ n_trees

  # Phylogenetic composition
  pcps1 ~ pse + faba

  # Biomass production
  log_produt ~ n_trees + pcps1 + sr

  # Correlated exogenous
  faba ~~ pse
'

fit_finalD <- sem(
  model = model_D,
  data  = dados_scaled,
  estimator = "ML"
)

summary(fit_finalD, standardized = TRUE, fit.measures = TRUE)

# ---- Model E - CWM-LDMC ----

model_D_ldmc1 <- '
  # Structure and diversity
  sr ~ n_trees

  # Phylogenetic composition
  pcps1 ~ pse + faba

  # Biomass production
  log_produt ~ n_trees + pcps1 + sr + ldmc_CWM

  # Correlated exogenous
  faba ~~ pse
'
fit_D_ldmc1 <- sem(model_D_ldmc1, data = dados_scaled, estimator = "ML")
summary(fit_D_ldmc1, standardized = TRUE, fit.measures = TRUE)

# ---- Model E2 ----

model_D_ldmc2 <- '
  # Structure and diversity
  sr ~ n_trees

  # Phylogenetic composition
  pcps1 ~ pse + faba

  # Trait mechanism
  ldmc_CWM ~ pcps1

  # Biomass production
  log_produt ~ n_trees + sr + pcps1 + ldmc_CWM

  # Correlated exogenous
  faba ~~ pse
'
fit_D_ldmc2 <- sem(model_D_ldmc2, data = dados_scaled, estimator = "ML")
summary(fit_D_ldmc2, standardized = TRUE, fit.measures = TRUE)

# ---- Model F - WD as direct predictor ----

model_F_wd1 <- '
  # Structure and diversity
  sr ~ n_trees

  # Phylogenetic composition
  pcps1 ~ pse + faba

  # Biomass production
  log_produt ~ n_trees + pcps1 + sr + wd_CWM

  # Correlated exogenous
  faba ~~ pse
'
fit_F_wd1 <- sem(model_F_wd1, data = dados_scaled, estimator = "ML")
summary(fit_F_wd1, standardized = TRUE, fit.measures = TRUE)

# ---- Model F2 ----

model_F_wd2 <- '
  # Structure and diversity
  sr ~ n_trees

  # Phylogenetic composition
  pcps1 ~ pse + faba

  # Trait mechanism
  wd_CWM ~ pcps1

  # Biomass production
  log_produt ~ n_trees + sr + pcps1 + wd_CWM

  # Correlated exogenous
  faba ~~ pse
'
fit_F_wd2 <- sem(model_F_wd2, data = dados_scaled, estimator = "ML")
summary(fit_F_wd2, standardized = TRUE, fit.measures = TRUE)

# ---- Model F3 ----

model_F_wd3 <- '
  sr ~ n_trees
  pcps1 ~ pse + faba

  ldmc_CWM ~ pcps1
  wd_CWM   ~ pcps1

  log_produt ~ n_trees + sr + pcps1 + ldmc_CWM + wd_CWM

  faba ~~ pse
'
fit_F_wd3 <- sem(model_F_wd3, data = dados_scaled, estimator = "ML")
summary(fit_F_wd3, standardized = TRUE, fit.measures = TRUE)

# ---- Model C2 ----

model_C2 <- '
  ############################
  # Climate → Soil
  ############################
  c.n_soloid ~ season_ppt

  ############################
  # Realized stand structure
  ############################
  n_trees ~ c.n_soloid + season_ppt

  ############################
  # Structure & diversity
  ############################
  sr ~ n_trees + c.n_soloid + season_ppt

  ############################
  # Phylogenetic composition
  ############################
  pcps1 ~ pse + faba

  ############################
  # Biomass drivers
  ############################
  log_produt ~ n_trees + sr + pcps1 + c.n_soloid + season_ppt

  ############################
  # Correlated exogenous
  ############################
  faba ~~ pse
  season_ppt ~~ faba + pse
'
fit_finalC2 <- sem(
  model = model_C2,
  data  = dados_scaled,
  estimator = "ML"
)
summary(fit_finalC2, standardized = TRUE, fit.measures = TRUE)

# ---- Models G ----

G0 <- '
  c.n_soloid ~ season_ppt
  sr ~ n_trees + c.n_soloid + season_ppt
  pcps1 ~ pse + faba
  log_produt ~ n_trees + sr + pcps1 + c.n_soloid + season_ppt

  pse ~~ faba
  season_ppt ~~ pse
'
fit_G0 <- lavaan::sem(G0, data = dados_scaled, estimator="ML")

# G1

G1 <- '
  c.n_soloid ~ season_ppt
  sr ~ n_trees + c.n_soloid + season_ppt
  pcps1 ~ pse + faba

  log_produt ~ n_trees + sr + pcps1 + c.n_soloid + season_ppt + ldmc_CWM + wd_CWM

  ldmc_CWM ~~ wd_CWM
  pse ~~ faba
  season_ppt ~~ pse
'
fit_G1 <- lavaan::sem(G1, data=dados_scaled, estimator="ML")

# G2 - SELECTED

G2 <- '
  c.n_soloid ~ season_ppt
  ldmc_CWM ~ n_trees + c.n_soloid + season_ppt
  
  pcps1 ~ pse + faba

  log_produt ~ n_trees + pcps1 + c.n_soloid + season_ppt + ldmc_CWM

  pse ~~ faba
  season_ppt ~~ pse
'
fit_G2 <- lavaan::sem(G2, data=dados_scaled, estimator="ML")

summary(fit_G2, standardized = TRUE, fit.measures = TRUE)

# G3

G3 <- '
  c.n_soloid ~ season_ppt

  ldmc_CWM ~ n_trees + c.n_soloid + season_ppt
  wd_CWM   ~ n_trees + c.n_soloid + season_ppt

  # your hypothesis: CWM -> PSE
  pse ~ ldmc_CWM + wd_CWM + season_ppt

  pcps1 ~ pse + faba

  log_produt ~ n_trees + pcps1 + c.n_soloid + season_ppt + ldmc_CWM + wd_CWM

  ldmc_CWM ~~ wd_CWM
  pse ~~ faba
'
fit_G3 <- lavaan::sem(G3, data=dados_scaled, estimator="ML")

# G4

G4 <- '
  c.n_soloid ~ season_ppt

  # phylo -> traits (often easier to justify)
  ldmc_CWM ~ pse + season_ppt + c.n_soloid + n_trees
  wd_CWM   ~ pse + season_ppt + c.n_soloid + n_trees

  pcps1 ~ pse + faba

  log_produt ~ n_trees + pcps1 + c.n_soloid + season_ppt + ldmc_CWM + wd_CWM

  ldmc_CWM ~~ wd_CWM
  pse ~~ faba
  season_ppt ~~ pse
'
fit_G4 <- lavaan::sem(G4, data=dados_scaled, estimator="ML")

# Compare

fits <- list(G0=fit_G0, G1=fit_G1, G2=fit_G2, G3=fit_G3, G4=fit_G4)

sapply(fits, lavaan::fitMeasures, c("cfi","tli","rmsea","srmr","aic","bic"))

# G4

G4 <- '
  c.n_soloid ~ season_ppt
  wd_CWM ~ n_trees + c.n_soloid + season_ppt
  
  pcps1 ~ pse + faba

  log_produt ~ n_trees + pcps1 + c.n_soloid + season_ppt + wd_CWM

  pse ~~ faba
  season_ppt ~~ pse
'
fit_G4 <- lavaan::sem(G4, data=dados_scaled, estimator="ML")

summary(fit_G4, standardized = TRUE, fit.measures = TRUE)

# ---- H Models - Functional Diversity ----

H0 <- '
  c.n_soloid ~ season_ppt

  pcps1 ~ pse + faba

  log_produt ~ n_trees + pcps1 + c.n_soloid + season_ppt

  pse ~~ faba
  season_ppt ~~ pse
'
fit_H0 <- lavaan::sem(H0, data=dados_scaled, estimator="ML")

# H1

H1 <- '
  c.n_soloid ~ season_ppt

  pcps1 ~ pse + faba

  log_produt ~ n_trees + pcps1 + c.n_soloid + season_ppt +
               ldmc_FDis + wd_FDis

  ldmc_FDis ~~ wd_FDis
  pse ~~ faba
  season_ppt ~~ pse
'
fit_H1 <- lavaan::sem(H1, data=dados_scaled, estimator="ML")

# H2

H2 <- '
  c.n_soloid ~ season_ppt

  ldmc_FDis ~ season_ppt + c.n_soloid + n_trees
  wd_FDis   ~ season_ppt + c.n_soloid + n_trees

  pcps1 ~ pse + faba

  log_produt ~ n_trees + pcps1 + c.n_soloid + season_ppt

  ldmc_FDis ~~ wd_FDis
  pse ~~ faba
  season_ppt ~~ pse
'
fit_H2 <- lavaan::sem(H2, data=dados_scaled, estimator="ML")

summary(fit_H2, standardized = TRUE, fit.measures = TRUE)


# H3

H3 <- '
  c.n_soloid ~ season_ppt

  ldmc_FDis ~ season_ppt + c.n_soloid + n_trees
  wd_FDis   ~ season_ppt + c.n_soloid + n_trees

  pcps1 ~ pse + faba

  log_produt ~ n_trees + pcps1 + c.n_soloid + season_ppt +
               ldmc_FDis + wd_FDis

  ldmc_FDis ~~ wd_FDis
  pse ~~ faba
  season_ppt ~~ pse
'
fit_H3 <- lavaan::sem(H3, data=dados_scaled, estimator="ML")

# H4

H4 <- '
  c.n_soloid ~ season_ppt

  ldmc_FDis ~ season_ppt + c.n_soloid
  wd_FDis   ~ season_ppt + c.n_soloid

  pcps1 ~ pse + faba + ldmc_FDis + wd_FDis

  log_produt ~ n_trees + pcps1 + c.n_soloid + season_ppt

  ldmc_FDis ~~ wd_FDis
  pse ~~ faba
  season_ppt ~~ pse
'
fit_H4 <- lavaan::sem(H4, data=dados_scaled, estimator="ML")

# Compare

fits_H <- list(H0=fit_H0, H1=fit_H1, H2=fit_H2, H3=fit_H3)

sapply(fits_H, lavaan::fitMeasures,
       c("cfi","tli","rmsea","srmr","aic","bic"))





# H2

H2 <- '
  c.n_soloid ~ season_ppt

  wd_FDis   ~ season_ppt + c.n_soloid + n_trees

  pcps1 ~ pse + faba

  log_produt ~ n_trees + pcps1 + c.n_soloid + season_ppt

  pse ~~ faba
  season_ppt ~~ pse
'
fit_H2 <- lavaan::sem(H2, data=dados_scaled, estimator="ML")

summary(fit_H2, standardized = TRUE, fit.measures = TRUE)


# ---- Model J ----

J <- '
  c.n_soloid ~ season_ppt

  ldmc_FDis ~ season_ppt + c.n_soloid + n_trees

  wd_FDis   ~ season_ppt + c.n_soloid + n_trees

  pcps1 ~ pse + faba

  log_produt ~ n_trees + pcps1 + c.n_soloid + season_ppt + ldmc_FDis

  pse ~~ faba
  season_ppt ~~ pse
'
fit_J <- lavaan::sem(J, data=dados_scaled, estimator="ML")
summary(fit_J, standardized=TRUE, fit.measures=TRUE)


# ---- Model K ---- Not good, don't use it

dados_scaled$biomassa_z_kg <- scale(dadosmisto1$biomassa_z_kg)

k <- '
  # Climate effects
  c.n_soloid ~ season_ppt
  ldmc_FDis  ~ season_ppt + c.n_soloid + n_trees

  # Phylogenetic structure
  pcps1 ~ pse + faba

  # Biomass production (aligned with msel)
  biomassa_z_kg ~ n_trees + pcps1 + c.n_soloid

  # Covariances
  pse ~~ faba
  season_ppt ~~ pse
'

fit_k <- lavaan::sem(
  k,
  data = dados_scaled,
  estimator = "ML"
)

summary(fit_k, standardized = TRUE, fit.measures = TRUE)


# ---- Model K2 - BEST MODEL ---- 

K2 <- '
  # Climate → soil
  c.n_soloid ~ season_ppt

  # Functional diversity (wood density)
  wd_FDis ~ season_ppt + c.n_soloid + n_trees

  # Phylogenetic structure
  pcps1 ~ pse + faba

  # Biomass production (aligned with LMM)
  biomassa_z_kg ~ n_trees + pcps1 + c.n_soloid

  # Covariances
  pse ~~ faba
  season_ppt ~~ pse
'

fit_K2 <- lavaan::sem(
  K2,
  data = dados_scaled,
  estimator = "ML"
)

summary(fit_K2, standardized = TRUE, fit.measures = TRUE)

# ---- Model L ----

L <- '
  # Climate → soil
  c.n_soloid ~ season_ppt

  # Dominant trait (wood density CWM)
  wd_CWM ~ season_ppt + c.n_soloid + n_trees

  # Phylogenetic structure
  pcps1 ~ pse + faba

  # Biomass production
  biomassa_z_kg ~ n_trees + pcps1 + c.n_soloid + wd_CWM

  # Covariances
  pse ~~ faba
  season_ppt ~~ pse
'

fit_L <- lavaan::sem(
  L,
  data = dados_scaled,
  estimator = "ML"
)

summary(fit_L, standardized = TRUE, fit.measures = TRUE)

# ---- Model K3 ---- 

K3 <- '
  # Climate → soil
  c.n_soloid ~ season_ppt

  # Functional diversity (wood density)
  wd_FDis ~ season_ppt + c.n_soloid + n_trees

  # Phylogenetic structure
  pcps1 ~ pse + cwm_nfix

  # Biomass production (aligned with LMM)
  biomassa_z_kg ~ n_trees + pcps1 + c.n_soloid

  # Covariances
  pse ~~ cwm_nfix
  season_ppt ~~ pse
'

fit_K3 <- lavaan::sem(
  K3,
  data = dados_scaled,
  estimator = "ML"
)

summary(fit_K3, standardized = TRUE, fit.measures = TRUE)
