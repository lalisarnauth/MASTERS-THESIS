
m1 <- glm(scale(n_trees)~ wd_CWM, data = dadosmisto, family = "poisson")

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

m1 <- glm(n_trees ~ PC1nutri,
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

m4 <- glm(n_trees ~ PC1nutri + gini,
          data = dadosmisto,
          family = poisson)

summary(m4)

AIC(m_pois_PC1nutri,
    m_pois_gini,
    m1, m2, m3)


m5 <- glm(gini ~ sr,
          data = dadosmisto)
summary(m5)

hist(dadosmisto$biomassa_z_kg)
hist(dadosmisto$log_biomass)

####


dadosmisto$log_biomass_per_tree <- 
  log(dadosmisto$biomassa_z_kg / dadosmisto$n_trees)

t <- lm(log_biomass_per_tree ~ pcps1 + sr + c.n_solo,
   data = dadosmisto)
summary(t)

g <- lm(log_biomass_per_tree ~ pcps1 + c.n_solo + season_ppt,
        data = dadosmisto)
summary(g)

# ---- PLOT M4 ----

library(ggeffects)
library(ggplot2)

# Predições parciais
eff_nutri <- ggpredict(m4, terms = "PC1nutri")
eff_gini  <- ggpredict(m4, terms = "gini")

# Plot PC1nutri
p1 <- ggplot() +
  geom_point(data = dadosmisto,
             aes(x = PC1nutri, y = n_trees),
             alpha = 0.5) +
  geom_line(data = eff_nutri,
            aes(x = x, y = predicted),
            linewidth = 1) +
  geom_ribbon(data = eff_nutri,
              aes(x = x, ymin = conf.low, ymax = conf.high),
              alpha = 0.2) +
  labs(x = "Soil nutrients (PC1)",
       y = "Predicted number of trees") +
  theme_classic()

# Plot Gini
p2 <- ggplot() +
  geom_point(data = dadosmisto,
             aes(x = gini, y = n_trees),
             alpha = 0.5) +
  geom_line(data = eff_gini,
            aes(x = x, y = predicted),
            linewidth = 1) +
  geom_ribbon(data = eff_gini,
              aes(x = x, ymin = conf.low, ymax = conf.high),
              alpha = 0.2) +
  labs(x = "DBH inequality (Gini)",
       y = "Predicted number of trees") +
  theme_classic()

library(patchwork)

final_plot <- p1 + p2

ggsave(
  filename = "Fig_ap1_density_drivers.jpeg",
  plot = final_plot,
  path = "C:/Users/laila/OneDrive/Documentos/01 Masters_LA/06 Figures/02 plots/appendix",
  width = 10,
  height = 5,
  dpi = 300
)

# ---- Plot biomass per tree ----

library(ggeffects)
library(ggplot2)

# Efeito parcial de PCPS1
eff_pcps <- ggpredict(g, terms = "pcps1")

plot_g <- ggplot() +
  geom_point(data = dadosmisto,
             aes(x = pcps1, y = log_biomass_per_tree),
             alpha = 0.5) +
  geom_line(data = eff_pcps,
            aes(x = x, y = predicted),
            linewidth = 1) +
  geom_ribbon(data = eff_pcps,
              aes(x = x, ymin = conf.low, ymax = conf.high),
              alpha = 0.2) +
  labs(x = "Phylogenetic composition (PCPS1)",
       y = "Log biomass per tree") +
  theme_classic()

ggsave(
  filename = "Fig_ap2_density_drivers.jpeg",
  plot = plot_g,
  path = "C:/Users/laila/OneDrive/Documentos/01 Masters_LA/06 Figures/02 plots/appendix",
  width = 10,
  height = 5,
  dpi = 300
)
