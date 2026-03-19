
# ---- SEM ----

library(lavaan)

# Load the data

dadosmisto <- read.csv("01 Datasets/01_raw_data/dadosmisto.csv",header = TRUE,row.names = 1)

# Scale

dados_scaled <- as.data.frame(scale(dadosmisto[, sapply(dadosmisto, is.numeric)]))

# ---- H1: Climate and Diversity ---- 






################################ OLD

# ---- H1: SR ---- 
# PPT SEASON

A1 <- '
  c.n_solo ~~ season_ppt
  sr ~ season_ppt + c.n_solo
  log_biomass ~ sr + c.n_solo + n_trees
'

fit_A1 <- lavaan::sem(
  A1,
  data = dados_scaled,
  estimator = "ML"
)

summary(fit_A1,
        standardized = TRUE,
        fit.measures = TRUE,
        rsquare = TRUE)

# Climate PC1

A2 <- '
  c.n_solo ~~ PC1_clima
  sr ~ PC1_clima + c.n_solo
  log_biomass ~ sr + c.n_solo + n_trees
'

fit_A2 <- lavaan::sem(
  A2,
  data = dados_scaled,
  estimator = "ML"
)

summary(fit_A2,
        standardized = TRUE,
        fit.measures = TRUE,
        rsquare = TRUE)

# ---- H2: PCPS1 ----


B1 <- '
  c.n_solo ~~ season_ppt
  pcps1 ~ season_ppt + c.n_solo
  log_biomass ~ pcps1 + c.n_solo + n_trees
'

fit_B1 <- lavaan::sem(
  B1,
  data = dados_scaled,
  estimator = "ML"
)

summary(fit_B1,
        standardized = TRUE,
        fit.measures = TRUE,
        rsquare = TRUE)

# Climate PC1

B2 <- '
  c.n_solo ~~ PC1_clima
  pcps1 ~ PC1_clima + c.n_solo
  log_biomass ~ pcps1 + c.n_solo + PC1_clima + n_trees
'
fit_B2 <- lavaan::sem(
  B2,
  data = dados_scaled,
  estimator = "ML"
)

summary(fit_B2,
        standardized = TRUE,
        fit.measures = TRUE,
        rsquare = TRUE)

# ---- H3: Nfix ----

C1 <- '
  c.n_solo ~~ season_ppt
  cwm_nfix ~ season_ppt + c.n_solo
  log_biomass ~ cwm_nfix + c.n_solo + n_trees
'

fit_C1 <- lavaan::sem(
  C1,
  data = dados_scaled,
  estimator = "ML"
)

summary(fit_C1,
        standardized = TRUE,
        fit.measures = TRUE,
        rsquare = TRUE)

# cLIMATE pc1

C2 <- '
  c.n_solo ~~ PC1_clima
  cwm_nfix ~ PC1_clima + c.n_solo
  log_biomass ~ cwm_nfix + c.n_solo + n_trees
'

fit_C2 <- lavaan::sem(
  C2,
  data = dados_scaled,
  estimator = "ML"
)

summary(fit_C2,
        standardized = TRUE,
        fit.measures = TRUE,
        rsquare = TRUE)


# ---- H4: sesPD ----

# sEASON PPT

D1 <- '
  c.n_solo ~~ season_ppt
  sespd ~ season_ppt + c.n_solo
  log_biomass ~ sespd + c.n_solo + season_ppt + n_trees
'
fit_D1 <- lavaan::sem(
  D1,
  data = dados_scaled,
  estimator = "ML"
)

summary(fit_D1,
        standardized = TRUE,
        fit.measures = TRUE,
        rsquare = TRUE)

# Climate PC1

D2 <- '
  c.n_solo ~~ PC1_clima
  sespd ~ PC1_clima + c.n_solo
  log_biomass ~ sespd + c.n_solo + n_trees
'
fit_D2 <- lavaan::sem(
  D2,
  data = dados_scaled,
  estimator = "ML"
)

summary(fit_D2,
        standardized = TRUE,
        fit.measures = TRUE,
        rsquare = TRUE)

# ---- H5: id or div? ----

E <- '
  c.n_solo ~~ season_ppt

  pcps1 ~ season_ppt + c.n_solo
  sespd ~ season_ppt + c.n_solo
  sr ~ season_ppt + c.n_solo

  log_biomass ~ pcps1 + sespd + c.n_solo + sr + n_trees'
  
  
fit_E <- lavaan::sem(
  E,
  data = dados_scaled,
  estimator = "ML"
)

summary(fit_E,
        standardized = TRUE,
        fit.measures = TRUE,
        rsquare = TRUE)
  
# PC1 climate

DB2 <- '
  c.n_solo ~~ PC1_clima

  pcps1 ~ PC1_clima + c.n_solo
  sespd ~ PC1_clima + c.n_solo
  sr ~ season_ppt + c.n_solo

  log_biomass ~ pcps1 + sespd + c.n_solo + sr + n_trees
'

fit_DB2 <- lavaan::sem(
  DB2,
  data = dados_scaled,
  estimator = "ML"
)

summary(fit_DB2,
        standardized = TRUE,
        fit.measures = TRUE,
        rsquare = TRUE)

# ---- Model K2 - OLD BEST MODEL ---- 

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

install.packages("usethis")
install.packages("gitcreds")
library(usethis) 
library(gitcreds)

use_git_config(user.name = "lalisarnauth",user.email="laila.iglesasias1@gmail.com")
                
                
                
                
                
                
                