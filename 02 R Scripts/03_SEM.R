
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






