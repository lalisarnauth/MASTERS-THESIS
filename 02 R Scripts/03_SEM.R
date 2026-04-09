
# ---- SEM ----

# ---- Piecewise ----

# Packages
library(piecewiseSEM)
library(lme4)
library(MASS)

# ---- HA ----

# Climate and soil indirect trough diversity

# 1. Submodels

mod_n_trees_nb <- glm.nb(
  n_trees ~ PC1nutri+ c.n_solo,
  data = dadosmisto
)

mod_pcps1 <- lmer(
  pcps1 ~ PC1_ClimaA + PC2_ClimaB + (1 | site),
  data = dadosmisto
)

mod_biomass <- lmer(
  log_biomass ~ pcps1 + n_trees + sr + pse + (1 | site),
  data = dadosmisto
)


# 2. Piecewise SEM

sem_HA <- psem(
  mod_n_trees,
  mod_pcps1,
  mod_biomass,
  data = dadosmisto
)

# Covariance

sem_HA <- update(
  sem_HA,
  n_trees %~~% sr,
  pcps1 %~~% pse
)

# 3. Summary

summary(sem_HA)

# ---- HA2 ----

mod_n_trees_nb <- glm.nb(
  n_trees ~ PC1nutri+ c.n_solo,
  data = dadosmisto
)

mod_pcps1 <- lm(
  pcps1 ~ PC1_ClimaA + PC2_ClimaB,
  data = dadosmisto
)

mod_biomass <- lmer(
  log_biomass ~ pcps1 + n_trees + sr + pse + c.n_solo + PC2_ClimaB + (1 | site),
  data = dadosmisto
)

# Piecewise SEM

sem_HA2 <- psem(
  mod_n_trees_nb,
  mod_pcps1,
  mod_biomass,
  data = dadosmisto
)

# Covariance

sem_HA2 <- update(
  sem_HA2,
  c.n_solo %~~% PC1nutri,
  n_trees %~~% sr,
  pcps1 %~~% pse,
  sr %~~% pcps1
)

# Summmary

summary(sem_HA2)


# ---- HB ----

mod_n_trees_nb <- glm.nb(
  n_trees ~ PC1nutri+ c.n_solo,
  data = dadosmisto
)

mod_pcps1 <- lm(
  pcps1 ~ PC1_ClimaA + PC2_ClimaB,
  data = dadosmisto
)

mod_biomass <- lmer(
  log_biomass ~ pcps1 + n_trees + c.n_solo + sr + pse + (1 | site),
  data = dadosmisto
)

# Piecewise SEM

sem_HB <- psem(
  mod_n_trees_nb,
  mod_pcps1,
  mod_biomass,
  data = dadosmisto
)

# Covariance

sem_HB <- update(
  sem_HB,
  c.n_solo %~~% PC1nutri,
  n_trees %~~% sr,
  pcps1 %~~% pse,
  pcps1 %~~% sr
)

# Summmary

summary(sem_HB)


# ---- HC ----

mod_n_trees_nb <- glm.nb(
  n_trees ~ PC1nutri+ c.n_solo,
  data = dadosmisto
)

mod_pcps1 <- lm(
  pcps1 ~ PC1_ClimaA + PC2_ClimaB,
  data = dadosmisto
)

mod_biomass <- lmer(
  log_biomass ~ pcps1 + n_trees + PC1_ClimaA + PC2_ClimaB + sr + pse + (1 | site),
  data = dadosmisto
)

# Piecewise SEM

sem_HC <- psem(
  mod_n_trees_nb,
  mod_pcps1,
  mod_biomass,
  data = dadosmisto
)

# Covariance

sem_HC <- update(
  sem_HC,
  c.n_solo %~~% PC1nutri,
  n_trees %~~% sr,
  pcps1 %~~% pse,
  sr %~~% pcps1
)

# Summmary

summary(sem_HC)


# ---- HD ----

mod_n_trees_nb <- glm.nb(
  n_trees ~ PC1nutri+ c.n_solo,
  data = dadosmisto
)

mod_pcps1 <- lm(
  pcps1 ~ PC1_ClimaA + PC2_ClimaB,
  data = dadosmisto
)

mod_biomass <- lmer(
  log_biomass ~ PC1_ClimaA + PC2_ClimaB + PC1nutri + c.n_solo + (1 | site),
  data = dadosmisto
)

# Piecewise SEM

sem_HD <- psem(
  mod_n_trees_nb,
  mod_pcps1,
  mod_biomass,
  data = dadosmisto
)

# Covariance

sem_HD <- update(
  sem_HD,
  c.n_solo %~~% PC1nutri,
  n_trees %~~% sr,
  pcps1 %~~% pse,
  sr %~~% pcps1
)

# Summmary

summary(sem_HD)


# ---- HE ----

mod_n_trees_nb <- glm.nb(
  n_trees ~ PC1nutri + sr + pcps1 + c.n_solo,
  data = dadosmisto
)

mod_pcps1 <- lm(
  pcps1 ~ PC1_ClimaA + PC2_ClimaB,
  data = dadosmisto
)

mod_biomass <- lmer(
  log_biomass ~ n_trees + (1 | site),
  data = dadosmisto
)

# Piecewise SEM

sem_HE <- psem(
  mod_n_trees_nb,
  mod_pcps1,
  mod_biomass,
  data = dadosmisto
)

# Covariance

sem_HE <- update(
  sem_HE,
  c.n_solo %~~% PC1nutri,
  pcps1%~~%pse
)

# Summmary

summary(sem_HE)


# ---- HE2 ----

mod_n_trees_nb <- glm.nb(
  n_trees ~ PC1nutri + sr,
  data = dadosmisto
)

mod_pcps1 <- lm(
  pcps1 ~ PC1_ClimaA + PC2_ClimaB,
  data = dadosmisto
)

mod_biomass <- lmer(
  log_biomass ~ n_trees + c.n_solo + pcps1 + (1 | site),
  data = dadosmisto
)

# Piecewise SEM

sem_HE2 <- psem(
  mod_n_trees_nb,
  mod_pcps1,
  mod_biomass,
  data = dadosmisto
)

# Covariance

sem_HE2 <- update(
  sem_HE2,
  c.n_solo %~~% PC1nutri,
  pse %~~% pcps1,
  pcps1 %~~% sr
)

# Summmary

summary(sem_HE2)


# ---- HF ----

mod_pcps1 <- lm(
  pcps1 ~ PC1_ClimaA + PC2_ClimaB,
  data = dadosmisto
)

mod_biomass <- lmer(
  log_biomass ~ n_trees + sr + pcps1 + pse + (1 | site),
  data = dadosmisto
)

mod_soil_cn <- lm(
  c.n_solo ~ log_biomass,
  data = dadosmisto
)

mod_soil_ions <- lm(
  PC1nutri ~ log_biomass,
  data = dadosmisto
)

# Piecewise SEM

sem_HF <- psem(
  mod_pcps1,
  mod_biomass,
  mod_soil_cn,
  mod_soil_ions
)

# Covariance

sem_HF <- update(
  sem_HF,
  pcps1 %~~% pse,
  sr %~~% n_trees,
  c.n_solo %~~% PC1nutri
)

# Summmary

summary(sem_HF)


# ---- Clean version ----

# ---- H ----

mod_n_trees_nb <- glm.nb(
  n_trees ~ PC1nutri + sr,
  data = dadosmisto
)

mod_pcps1 <- lm(
  pcps1 ~ PC1_ClimaA,
  data = dadosmisto
)

mod_biomass <- lmer(
  log_biomass ~ n_trees + c.n_solo + pcps1 + (1 | site),
  data = dadosmisto
)

# Piecewise SEM

sem_H <- psem(
  mod_n_trees_nb,
  mod_pcps1,
  mod_biomass,
  data = dadosmisto
)

# Covariance

sem_H <- update(
  sem_H,
  c.n_solo %~~% PC1nutri,
  sr %~~% pcps1
)

# Summmary

summary(sem_H)

