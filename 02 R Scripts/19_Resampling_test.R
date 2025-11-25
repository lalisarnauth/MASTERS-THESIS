########################################
#### RESAMPLING TESTS FOR MODEL m17 ####
#### Bootstrap + Leave-one-site-out ####
########################################

library(lme4)
library(MuMIn)
library(dplyr)
library(purrr)
library(ggplot2)

# Model
m17 <- lmer(log_produt ~ sr + pcps1 + season_ppt + (1 | site),
            data = dadosmisto1,
            REML = FALSE)

summary(m17)
r.squaredGLMM(m17)
AICc(m17)

##############################################
#### 1) BOOTSTRAP DAS PARCELAS            ####
##############################################

set.seed(123)   
n_iter <- 1000  

# Para guardar resultados
boot_results <- tibble(
  iter         = integer(n_iter),
  beta_sr      = NA_real_,
  beta_pcps1   = NA_real_,
  beta_season  = NA_real_,
  r2_marginal  = NA_real_,
  r2_conditional = NA_real_,
  AICc         = NA_real_,
  converged    = NA
)

for(i in seq_len(n_iter)) {
  # Line resampling with replacement
  dados_boot <- dadosmisto1[sample(nrow(dadosmisto1),
                                   size = nrow(dadosmisto1),
                                   replace = TRUE), ]
  
  # Adjusting the tryCatch model to prevent the loop from breaking.
  mod_boot <- tryCatch(
    lmer(log_produt ~ sr + pcps1 + season_ppt + (1 | site),
         data = dados_boot,
         REML = FALSE),
    error = function(e) NULL,
    warning = function(w) {
      # still trying to revert the model if possible
      invokeRestart("muffleWarning")
    }
  )
  
  if (is.null(mod_boot)) {
    boot_results$iter[i]      <- i
    boot_results$converged[i] <- FALSE
    next
  }
  
  # Fixed coefficients
  fe <- fixef(mod_boot)
  
  # R² m and c
  r2_vals <- r.squaredGLMM(mod_boot)
  
  boot_results$iter[i]          <- i
  boot_results$beta_sr[i]       <- fe["sr"]
  boot_results$beta_pcps1[i]    <- fe["pcps1"]
  boot_results$beta_season[i]   <- fe["season_ppt"]
  boot_results$r2_marginal[i]   <- r2_vals[1, "R2m"]
  boot_results$r2_conditional[i]<- r2_vals[1, "R2c"]
  boot_results$AICc[i]          <- AICc(mod_boot)
  boot_results$converged[i]     <- TRUE
}

# Filter only models that have converged
boot_ok <- boot_results %>% filter(converged)

# Summary of distributions
summary(boot_ok[, c("beta_sr", "beta_pcps1", "beta_season",
                    "r2_marginal","r2_conditional","AICc")])

# Histogram - SR
g_boot_sr <- ggplot(boot_ok, aes(x = beta_sr)) +
  geom_histogram(bins = 30) +
  geom_vline(xintercept = fixef(m17)["sr"],
             linetype = "dashed") +
  theme_bw(base_size = 13) +
  labs(
    title = "Bootstrap distribution of SR effect (m17)",
    x = "Slope for SR",
    y = "Frequency"
  )

print(g_boot_sr)

ggsave("~/01 Masters_LA/06 Figures/02 plots/bootstrap_m17_sr.jpeg",
       g_boot_sr, width = 7, height = 5, dpi = 300)

# Histogram - PCPS1
g_boot_pcps1 <- ggplot(boot_ok, aes(x = beta_pcps1)) +
  geom_histogram(bins = 30) +
  geom_vline(xintercept = fixef(m17)["pcps1"],
             linetype = "dashed") +
  theme_bw(base_size = 13) +
  labs(
    title = "Bootstrap distribution of PCPS1 effect (m17)",
    x = "Slope for PCPS1",
    y = "Frequency"
  )

print(g_boot_pcps1)

ggsave("~/01 Masters_LA/06 Figures/02 plots/bootstrap_m17_pcps1.jpeg",
       g_boot_pcps1, width = 7, height = 5, dpi = 300)

# Histogram - season_ppt
g_boot_ppt_season <- ggplot(boot_ok, aes(x = beta_season)) +
  geom_histogram(bins = 30) +
  geom_vline(xintercept = fixef(m17)["season_ppt"],
             linetype = "dashed") +
  theme_bw(base_size = 13) +
  labs(
    title = "Bootstrap distribution of PPT seasonality effect (m17)",
    x = "Slope for PPT season",
    y = "Frequency"
  )

print(g_boot_ppt_season)

ggsave("~/01 Masters_LA/06 Figures/02 plots/bootstrap_m17_ppt_season.jpeg", g_boot_ppt_season, width = 7, height = 5, dpi = 300)

######################################
#### 2) LEAVE-ONE-SITE-OUT (LOSO) ####
######################################

sites <- unique(dadosmisto1$site)

loso_results <- map_dfr(sites, function(s) {
  # Remove one site at a time.
  dados_loso <- dadosmisto1 %>% filter(site != s)
  
  mod_loso <- tryCatch(
    lmer(log_produt ~ sr + pcps1 + season_ppt + (1 | site),
         data = dados_loso,
         REML = FALSE),
    error = function(e) NULL
  )
  
  if (is.null(mod_loso)) {
    return(tibble(
      removed_site     = s,
      beta_sr          = NA_real_,
      beta_pcps1       = NA_real_,
      beta_season      = NA_real_,
      r2_marginal      = NA_real_,
      r2_conditional   = NA_real_,
      AICc             = NA_real_,
      converged        = FALSE
    ))
  }
  
  fe <- fixef(mod_loso)
  r2_vals <- r.squaredGLMM(mod_loso)
  
  tibble(
    removed_site     = s,
    beta_sr          = fe["sr"],
    beta_pcps1       = fe["pcps1"],
    beta_season      = fe["season_ppt"],
    r2_marginal      = r2_vals[1, "R2m"],
    r2_conditional   = r2_vals[1, "R2c"],
    AICc             = AICc(mod_loso),
    converged        = TRUE
  )
})

loso_results

# Comparar com o modelo completo
fixef(m17)
r.squaredGLMM(m17)
AICc(m17)

# We assessed the robustness of all fixed effects in the best-supported model (m17) using bootstrap resampling (1000 iterations). All predictors showed highly consistent effects. PCPS1 remained negative in 100% of bootstrap replicates (median ≈ −1.0), SR remained positive in 100% of replicates (median ≈ 0.06), and precipitation seasonality remained negative in 100% of replicates (median ≈ −0.055). These results indicate that the biodiversity–productivity relationship and the climatic constraint driven by seasonal water deficit are both stable and not dependent on specific parcels or sites

# Leave-one-site-out analysis (LOSO) showed that no individual site substantially influenced the results. The effect of species richness remained positive in all models (0.050–0.065), the effect of PCPS1 remained consistently negative (−0.80 to −1.29), and precipitation seasonality remained negative across all runs (−0.039 to −0.066). Marginal R² varied between 0.23 and 0.44, and conditional R² between 0.52 and 0.62, confirming that the structure and magnitude of fixed and random effects were stable. These results demonstrate that model m17 is robust to site-level influence and that the identified biodiversity and climatic effects are general patterns across the restoration areas