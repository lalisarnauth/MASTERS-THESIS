
# ---- SEM ----

library(lavaan)

# Load the data

dadosmisto <- read.csv("01 Datasets/01_raw_data/dadosmisto.csv",
                       header = TRUE)

# Scale

dados_scaled <- as.data.frame(scale(dadosmisto[, sapply(dadosmisto, is.numeric)]))

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

# ---- Model L ---- SR and N Trees

L <- '
  # Climate → Soil
  c.n_soloid ~ season_ppt

  # Climate → Richness
  sr ~ season_temp + pet

  # Richness → Structure
  n_trees ~ sr

  # Phylogenetic identity
  pcps1 ~ pse + faba

  # Biomass accumulation (final response)
  log_produt ~ season_ppt + c.n_soloid + n_trees + pcps1

  # (optional) exogenous covariances if you want to allow correlation among drivers:
  season_ppt ~~ season_temp + pet + pse + faba
  season_temp ~~ pet
  pse ~~ faba
'

fit_L <- lavaan::sem(L, data = dados_scaled, estimator = "ML")

summary(fit_L, standardized = TRUE, fit.measures = TRUE)

# ---- Model L - SR > N trees > Biomass ----

L <- '

  # Solo
  c.n_solo ~ season_ppt

  # Estrutura
  n_trees ~ sr

  # Identidade
  pcps1 ~ pse + faba

  # Biomassa
  log_biomass ~ c.n_soloid + n_trees + pcps1

  # Covariâncias
  pse ~~ faba
  season_ppt ~~ pse
'

fit_L <- lavaan::sem(
  L,
  data = dados_scaled,
  estimator = "ML"
)

summary(fit_L, standardized = TRUE, fit.measures = TRUE)
# CFI = 0.621

# ---- Model L2 - N trees > SR > Biomass ----

L2 <- '

  # Solo
  c.n_solo ~ season_ppt

  # Estrutura influencia riqueza
  sr ~ n_trees

  # Identidade
  pcps1 ~ pse + faba

  # Biomassa
  log_biomass ~ c.n_soloid + sr + pcps1

  # Covariâncias exógenas
  pse ~~ faba
  season_ppt ~~ pse

'
fit_L2 <- lavaan::sem(
  L2,
  data = dados_scaled,
  estimator = "ML"
)

summary(fit_L2, standardized = TRUE, fit.measures = TRUE)
# CFI = 0.542

# ---- L3 ----

L3 <- '

  # Solo
  c.n_solo ~ season_ppt

  # Estrutura
  n_trees ~ PC1nutri + gini + wd_FDis

  # Identidade
  pcps1 ~ pse + faba

  # Biomassa
  log_biomass ~ c.n_solo + n_trees + pcps1

  # Covariâncias
  pse ~~ faba
  season_ppt ~~ pse

'
fit_L3 <- lavaan::sem(
  L3,
  data = dados_scaled,
  estimator = "ML"
)

summary(fit_L3, standardized = TRUE, fit.measures = TRUE)
# CFI = 0.697

cor(dados_scaled[, c("PC1nutri","wd_FDis","gini","season_ppt","pcps1")])

# ---- L SR + PC1 ----
L_SR_PC1 <- '

  # Soil
  c.n_solo ~ a1*season_ppt

  # Stand structure
  n_trees ~ b1*sr + b2*PC1nutri

  # Phylogenetic identity
  pcps1 ~ c1*pse + c2*faba

  # Biomass
  log_biomass ~ d1*c.n_solo + d2*n_trees + d3*pcps1 + d4*sr

  # Covariances (keep your pattern)
  pse ~~ faba
  season_ppt ~~ pse

  # Indirect effects (THIS answers your question)
  ind_sr := b1*d2
  tot_sr := d4 + (b1*d2)
'
fit_SR_PC1 <- lavaan::sem(L_SR_PC1, data=dados_scaled, estimator="ML")
summary(fit_SR_PC1, standardized=TRUE, fit.measures=TRUE)
# CFI = 0.768

fit_SR_PC1_b <- lavaan::sem(L_SR_PC1, data=dados_scaled, se="bootstrap", bootstrap=2000)
parameterEstimates(fit_SR_PC1_b, standardized=TRUE) |> subset(op == ":=")

# ---- L SR + WD ----

L_SR_WD <- '

  c.n_solo ~ a1*season_ppt

  n_trees ~ b1*sr + b2*wd_FDis

  pcps1 ~ c1*pse + c2*faba

  log_biomass ~ d1*c.n_solo + d2*n_trees + d3*pcps1 + d4*sr

  pse ~~ faba
  season_ppt ~~ pse

  ind_sr := b1*d2
  tot_sr := d4 + (b1*d2)
'
fit_SR_WD <- lavaan::sem(L_SR_WD, data=dados_scaled, estimator="ML")
summary(fit_SR_WD, standardized=TRUE, fit.measures=TRUE)
# CFI = 0.917

##

lavaan::fitMeasures(fit_Lf, c("cfi","tli","rmsea","srmr","aic","bic"))
lavaan::fitMeasures(fit_SR_PC1, c("cfi","tli","rmsea","srmr","aic","bic"))
lavaan::fitMeasures(fit_SR_WD, c("cfi","tli","rmsea","srmr","aic","bic"))

# ---- L5 - PC1nutri > N trees

L_PC1_only <- '

  # Soil
  c.n_solo ~ a1*season_ppt

  # Stand density
  n_trees ~ b1*PC1nutri

  # Identity
  pcps1 ~ c1*pse + c2*faba

  # Biomass
  log_biomass ~ d1*c.n_solo + d2*n_trees + d3*pcps1

  # Covariances
  pse ~~ faba
  season_ppt ~~ pse
'
fit_L_PC1_only <- lavaan::sem(L_PC1_only, data=dados_scaled, estimator="ML")
summary(fit_L_PC1_only, standardized=TRUE, fit.measures=TRUE)

# ---- L6 - PC1nutri > SR > N trees

L_PC1_SR <- '

  c.n_solo ~ a1*season_ppt

  sr ~ e1*PC1nutri

  n_trees ~ b1*sr

  pcps1 ~ c1*pse + c2*faba

  log_biomass ~ d1*c.n_solo + d2*n_trees + d3*pcps1

  pse ~~ faba
  season_ppt ~~ pse

  ind_PC1 := e1*b1*d2
'
fit_L_PC1_SR <- lavaan::sem(L_PC1_SR, data=dados_scaled, estimator="ML")
summary(fit_L_PC1_SR, standardized=TRUE, fit.measures=TRUE)

# ---- WD only ----

L_WD_only <- '

  # Soil
  c.n_solo ~ a1*season_ppt

  # Stand density
  n_trees ~ b1*wd_FDis

  # Identity
  pcps1 ~ c1*pse + c2*faba

  # Biomass
  log_biomass ~ d1*c.n_solo + d2*n_trees + d3*pcps1

  # Covariances
  pse ~~ faba
  season_ppt ~~ pse
'
fit_L_WD_only <- lavaan::sem(L_WD_only, data=dados_scaled, estimator="ML")
summary(fit_L_WD_only, standardized=TRUE, fit.measures=TRUE)

# ---- inv_WD ----

L_inv_WD <- '

  # Soil
  c.n_solo ~ a1*season_ppt

  # Functional diversity structures richness
  sr ~ e1*wd_FDis

  # Stand density
  n_trees ~ b1*sr

  # Phylogenetic identity
  pcps1 ~ c1*pse + c2*faba

  # Biomass
  biomassa_z_kg ~ d1*c.n_solo + d2*n_trees + d3*pcps1

  # Covariances
  pse ~~ faba
  season_ppt ~~ pse

  # Indirect effects
  ind_wd := e1*b1*d2
'
fit_inv_WD <- lavaan::sem(L_inv_WD, data=dados_scaled, estimator="ML")
summary(fit_inv_WD, standardized=TRUE, fit.measures=TRUE)
# CFI = 0.917

# ---- L final ----
##### ---- inv_WD_pptDirect ----

L_inv_WD_pptDirect <- '

  # Soil
  c.n_solo ~ a1*season_ppt

  # Functional diversity structures richness
  sr ~ e1*wd_FDis

  # Stand density
  n_trees ~ b1*sr

  # Phylogenetic identity
  pcps1 ~ c1*pse + c2*faba

  # Biomass (add direct effect of season_ppt)
  log_biomass ~ d0*season_ppt + d1*c.n_solo + d2*n_trees + d3*pcps1

  # Covariances
  pse ~~ faba
  season_ppt ~~ pse

  # Indirect effect of WD on biomass via SR and n_trees (through d2)
  ind_wd := e1*b1*d2

  # Optional: total effect of season_ppt on biomass (direct + via soil)
  tot_ppt := d0 + (a1*d1)
'
fit_inv_WD_pptDirect <- lavaan::sem(L_inv_WD_pptDirect, data=dados_scaled, estimator="ML")
summary(fit_inv_WD_pptDirect, standardized=TRUE, fit.measures=TRUE)
