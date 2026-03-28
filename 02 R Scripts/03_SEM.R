
# ---- SEM ----

library(lavaan)

# Load the data

dadosmisto <- read.csv("01 Datasets/01_raw_data/dadosmisto.csv",header = TRUE,row.names = 1)

# Scale

dados_scaled <- as.data.frame(scale(dadosmisto[, sapply(dadosmisto, is.numeric)]))

# ---- H1: Environment only indirect and Diversity direct ---- 

H1 <- '
  n_trees ~~ sr
  pcps1 ~~ pse
  
  n_trees ~ PC1_clima
  pcps1 ~ PC1_clima + c.n_solo
  log_biomass ~ n_trees + sr + pcps1 + pse
'

fit_H1 <- lavaan::sem(
  H1,
  data = dados_scaled,
  estimator = "ML"
)

summary(fit_H1,
        standardized = TRUE,
        fit.measures = TRUE,
        rsquare = TRUE)


# CFI: 0.725

# ---- H2: Environment is the driver ---- 

H2 <- '
  n_trees ~~ sr
  pcps1 ~~ pse
  
  n_trees ~ PC1_clima
  pcps1 ~ PC1_clima + c.n_solo
  log_biomass ~ n_trees + PC1_clima + c.n_solo
'

fit_H2 <- lavaan::sem(
  H2,
  data = dados_scaled,
  estimator = "ML"
)

summary(fit_H2,
        standardized = TRUE,
        fit.measures = TRUE,
        rsquare = TRUE)

# CFI: 0.825

# ---- H3: Environment & composition affect N. trees ---- 

H3 <- '
  pcps1 ~~ pse
  
  pcps1 ~ PC1_clima + c.n_solo
  n_trees ~ PC1_clima + sr + pcps1 + c.n_solo
    log_biomass ~ n_trees
'

fit_H3 <- lavaan::sem(
  H3,
  data = dados_scaled,
  estimator = "ML"
)

summary(fit_H3,
        standardized = TRUE,
        fit.measures = TRUE,
        rsquare = TRUE)

# CFI: 0.465


# ---- H4: Environment, composition, structure ---- 

H4 <- '
  pcps1 ~~ pse
  n_trees ~~ sr
  
  n_trees ~ PC1nutri
  pcps1 ~ PC1_clima
  
  log_biomass ~ n_trees + pcps1 + sr + pse + c.n_solo
'

fit_H4 <- lavaan::sem(
  H4,
  data = dados_scaled,
  estimator = "ML"
)

summary(fit_H4,
        standardized = TRUE,
        fit.measures = TRUE,
        rsquare = TRUE)

# CFI: 0.893

# sespd was 0.952

# ---- piecewise ----

# Pacotes
library(piecewiseSEM)
library(lme4)

# 1. Submodels

mod_n_trees <- m4 <- glm(n_trees ~ PC1nutri,
                         data = dadosmisto,
                         family = poisson)

mod_pcps1 <- lmer(pcps1 ~ PC1_clima + (1 | site),
                  data = dadosmisto,
                  REML = FALSE)

mod_biomass <- lmer(log(biomassa_z_kg) ~ pcps1 + n_trees + c.n_solo + (1 | site),
                    data = dadosmisto,
                    REML = FALSE)

# 2. Piecewise SEM

sem_H4_pw <- psem(
  mod_n_trees,
  mod_pcps1,
  mod_biomass
)


# 3. Resumo geral

summary(sem_H4_pw)

# 4. Coeficientes padronizados

coefs(sem_H4_pw, standardize = "scale")

# 5. R² dos submodelos

rsquared(sem_H4_pw)

# 6. Testes de independência

dSep(sem_H4_pw)

# 7. Ajuste global

fisherC(sem_H4_pw)

# 8. AIC do modelo

AIC(sem_H4_pw)

# ---- Testing ----

# Packages
library(piecewiseSEM)
library(lme4)

# ---- HA ----

# 1. Submodels

mod_n_trees <- glm(n_trees ~ PC1nutri,
                   data = dadosmisto,
                   family = poisson)

mod_pcps1 <- lmer(pcps1 ~ PC1_clima + (1 | site),
                  data = dadosmisto,
                  REML = FALSE)

mod_biomass <- lmer(log(biomassa_z_kg) ~ pcps1 + n_trees + c.n_solo + (1 | site),
                    data = dadosmisto,
                    REML = FALSE)

# 2. Piecewise SEM

sem_HA <- psem(
  mod_n_trees,
  mod_pcps1,
  mod_biomass
)

# 3. Summary

summary(sem_HA, conserve = TRUE)

# 4. Standardized coefficients

coefs(sem_HA, standardize = "scale")

# 5. R-squared

rsquared(sem_HA)

# 6. Tests of independence

dSep(sem_HA, conserve = TRUE)

# 7. Global fit

fisherC(sem_HA, conserve = TRUE)

# 8. AIC

AIC(sem_HA)


# ---- HB ----

# 1. Submodels

mod_n_trees <- glm(n_trees ~ PC1nutri,
                   data = dadosmisto,
                   family = poisson)

mod_biomass <- lmer(log(biomassa_z_kg) ~ PC1nutri + PC1_clima + n_trees + c.n_solo + (1 | site),
                    data = dadosmisto,
                    REML = FALSE)

# 2. Piecewise SEM

sem_HB <- psem(
  mod_n_trees,
  mod_biomass
)

# 3. Summary

summary(sem_HB, conserve = TRUE)

# 4. Standardized coefficients

coefs(sem_HB, standardize = "scale")

# 5. R-squared

rsquared(sem_HB)

# 6. Tests of independence

dSep(sem_HB, conserve = TRUE)

# 7. Global fit

fisherC(sem_HB, conserve = TRUE)

# 8. AIC

AIC(sem_HB)


# ---- HC ----

# 1. Submodels

mod_n_trees <- glm(n_trees ~ PC1nutri,
                   data = dadosmisto,
                   family = poisson)

mod_pse <- lm(pse ~ season_ppt,
              data = dadosmisto)

mod_biomass <- lmer(log(biomassa_z_kg) ~ pse + n_trees + c.n_solo + (1 | site),
                    data = dadosmisto,
                    REML = FALSE)

# 2. Piecewise SEM

sem_HC <- psem(
  mod_n_trees,
  mod_pse,
  mod_biomass
)

# 3. Summary

summary(sem_HC, conserve = TRUE)

# 4. Standardized coefficients

coefs(sem_HC, standardize = "scale")

# 5. R-squared

rsquared(sem_HC)

# 6. Tests of independence

dSep(sem_HC, conserve = TRUE)

# 7. Global fit

fisherC(sem_HC, conserve = TRUE)

# 8. AIC

AIC(sem_HC)


# ---- HD ----

# 1. Submodels

mod_n_trees <- glm(n_trees ~ PC1nutri,
                   data = dadosmisto,
                   family = poisson)

mod_sr <- glm(sr ~ altitude,
                   data = dadosmisto,
                   family = poisson)

mod_biomass <- lmer(log(biomassa_z_kg) ~ sr + n_trees + altitude + (1 | site),
                    data = dadosmisto,
                    REML = FALSE)

# 2. Piecewise SEM

sem_HD <- psem(
  mod_n_trees,
  mod_sr,
  mod_biomass,
  sr %~~% n_trees
)

# 3. Summary

summary(sem_HD, conserve = TRUE)

# 4. Standardized coefficients

coefs(sem_HD, standardize = "scale")

# 5. R-squared

rsquared(sem_HD)

# 6. Tests of independence

dSep(sem_HD, conserve = TRUE)

# 7. Global fit

fisherC(sem_HD, conserve = TRUE)

# 8. AIC

AIC(sem_HD)

