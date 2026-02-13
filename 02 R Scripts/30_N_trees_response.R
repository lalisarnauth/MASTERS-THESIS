m1 <- glm(scale(n_trees)~ wd_CWM), data = dadosmisto, family = "poisson")
summary(m1) 


# ---- N Trees ----

## FDis WD

ggplot(dadosmisto, aes(wd_FDis, n_trees)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)

m_pois_wd <- glm(n_trees ~ wd_FDis,
                 data = dadosmisto,
                 family = poisson)

summary(m_pois_wd)

dispersion <- sum(residuals(m_pois_wd, type = "pearson")^2) /
  m_pois_wd$df.residual
dispersion # 1.50 > not so high

library(MASS)

m_nb_wd <- glm.nb(n_trees ~ wd_FDis, data = dadosmisto)

summary(m_nb_wd)
AIC(m_pois_wd, m_nb_wd) # NegBin better  (deltaAIC = 1.8)

## ph - Not

ggplot(dadosmisto, aes(ph, n_trees)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)

m_pois_ph <- glm(n_trees ~ ph,
                 data = dadosmisto,
                 family = poisson)

summary(m_pois_ph) # p=0.356

dispersion <- sum(residuals(m_pois_ph, type = "pearson")^2) /
  m_pois_ph$df.residual
dispersion # 1.49 > not so high

## Valor_s - Not

ggplot(dadosmisto, aes(valor_s, n_trees)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)

m_pois_valor_s <- glm(n_trees ~ valor_s,
                 data = dadosmisto,
                 family = poisson)

summary(m_pois_valor_s) # p= 0.1

dispersion <- sum(residuals(m_pois_valor_s, type = "pearson")^2) /
  m_pois_valor_s$df.residual
dispersion # 1.43 > not so high


## PC1nutri

ggplot(dados_scaled, aes(PC1nutri, n_trees)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)

m_pois_PC1nutri <- glm(n_trees ~ PC1nutri,
                      data = dadosmisto,
                      family = poisson)

summary(m_pois_PC1nutri) # p= 0.0619 . 

dispersion <- sum(residuals(m_pois_PC1nutri, type = "pearson")^2) /
  m_pois_PC1nutri$df.residual
dispersion # 1.42 > not so high

## gini

ggplot(dados_scaled, aes(gini, n_trees)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)

m_pois_gini <- glm(n_trees ~ gini,
                       data = dadosmisto,
                       family = poisson)

summary(m_pois_gini) # p= 0.0716 .

dispersion <- sum(residuals(m_pois_gini, type = "pearson")^2) /
  m_pois_gini$df.residual
dispersion # 1.42 > not so high

# ---- Models ----

cor(dadosmisto[, c("PC1nutri","gini","wd_FDis")])

m1 <- glm(n_trees ~ PC1nutri + gini,
          data = dadosmisto,
          family = poisson)

summary(m1)

m2 <- glm(n_trees ~ PC1nutri + wd_FDis,
          data = dadosmisto,
          family = poisson)

summary(m2)

m3 <- glm(n_trees ~ PC1nutri + gini + wd_FDis,
          data = dadosmisto,
          family = poisson)

AIC(m_pois_PC1nutri,
    m_pois_gini,
    m1, m2, m3)
summary(m3)

hist(dadosmisto$biomassa_z_kg)
hist(dadosmisto$log_biomass)

