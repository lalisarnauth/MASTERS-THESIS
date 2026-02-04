########################################
#### RESAMPLING TESTS FOR MODEL m17 ####
#### Bootstrap + Leave-one-site-out ####
########################################

library(lme4)
library(MuMIn)
library(dplyr)
library(purrr)
library(ggplot2)


#### 0) Fit centered model (as you used)     ####

dadosmisto$c_n_trees <- as.numeric(scale(dadosmisto$n_trees, center = TRUE, scale = FALSE))
dadosmisto$c_cnsolo  <- as.numeric(scale(dadosmisto$c.n_solo, center = TRUE, scale = FALSE))

msel_centered <- lmer(
  log(biomassa_z_kg) ~ pcps1 + c_n_trees + c_cnsolo + (1 | site),
  data = dadosmisto,
  REML = FALSE
)

summary(msel_centered)
r.squaredGLMM(msel_centered)
AICc(msel_centered)


#### 1) BOOTSTRAP (row resampling)        ####

set.seed(123)
n_iter <- 1000

boot_results <- tibble(
  iter            = integer(n_iter),
  beta_pcps1      = rep(NA_real_, n_iter),
  beta_c_n_trees  = rep(NA_real_, n_iter),
  beta_c_cnsolo   = rep(NA_real_, n_iter),
  r2_marginal     = rep(NA_real_, n_iter),
  r2_conditional  = rep(NA_real_, n_iter),
  AICc            = rep(NA_real_, n_iter),
  converged       = rep(FALSE, n_iter),
  singular        = rep(NA, n_iter)
)

for (i in seq_len(n_iter)) {
  
  dados_boot <- dadosmisto[sample.int(nrow(dadosmisto), nrow(dadosmisto), replace = TRUE), ]
  
  mod_boot <- tryCatch(
    suppressWarnings(
      lmer(log(biomassa_z_kg) ~ pcps1 + c_n_trees + c_cnsolo + (1 | site),
           data = dados_boot, REML = FALSE)
    ),
    error = function(e) NULL
  )
  
  boot_results$iter[i] <- i
  if (is.null(mod_boot)) next
  
  boot_results$converged[i] <- is.null(mod_boot@optinfo$conv$lme4$messages)
  boot_results$singular[i]  <- isSingular(mod_boot, tol = 1e-4)
  
  fe <- fixef(mod_boot)
  r2_vals   <- tryCatch(r.squaredGLMM(mod_boot), error = function(e) NULL)
  boot_results$AICc[i] <- tryCatch(AICc(mod_boot), error = function(e) NA_real_)
  
  boot_results$beta_pcps1[i]     <- unname(fe["pcps1"])
  boot_results$beta_c_n_trees[i] <- unname(fe["c_n_trees"])
  boot_results$beta_c_cnsolo[i]  <- unname(fe["c_cnsolo"])
  
  if (!is.null(r2_vals)) {
    boot_results$r2_marginal[i]    <- r2_vals[1, "R2m"]
    boot_results$r2_conditional[i] <- r2_vals[1, "R2c"]
  }
}

boot_conv <- boot_results %>% filter(converged)
boot_ok   <- boot_results %>% filter(converged, singular == FALSE)

mean(boot_conv$singular, na.rm = TRUE)

# Summary
summary(boot_ok[, c("beta_pcps1", "beta_c_n_trees", "beta_c_cnsolo",
                    "r2_marginal", "r2_conditional", "AICc")])

#### PLOTS (histograms + reference line)####

# helper function
plot_boot <- function(df, col, ref, title, xlab) {
  ggplot(df, aes(x = .data[[col]])) +
    geom_histogram(bins = 30) +
    geom_vline(xintercept = ref, linetype = "dashed") +
    theme_bw(base_size = 13) +
    labs(title = title, x = xlab, y = "Frequency")
}

ref_pcps1     <- fixef(msel_centered)["pcps1"]
ref_c_n_trees <- fixef(msel_centered)["c_n_trees"]
ref_c_cnsolo  <- fixef(msel_centered)["c_cnsolo"]

g_boot_pcps1 <- plot_boot(
  boot_ok, "beta_pcps1", ref_pcps1,
  "Bootstrap distribution of PCPS1 effect (msel_centered)",
  "Slope for PCPS1"
)

g_boot_ntrees <- plot_boot(
  boot_ok, "beta_c_n_trees", ref_c_n_trees,
  "Bootstrap distribution of tree density effect (msel_centered)",
  "Slope for centered # of trees"
)

g_boot_cnsolo <- plot_boot(
  boot_ok, "beta_c_cnsolo", ref_c_cnsolo,
  "Bootstrap distribution of Soil C:N effect (msel_centered)",
  "Slope for centered Soil C:N"
)

print(g_boot_pcps1)
print(g_boot_ntrees)
print(g_boot_cnsolo)

# Save (adjust path!)
ggsave("~/01 Masters_LA/06 Figures/02 plots/bootstrap_msel_centered_pcps1.jpeg", g_boot_pcps1, width = 7, height = 5, dpi = 300)
ggsave("~/01 Masters_LA/06 Figures/02 plots/bootstrap_msel_centered_n_trees.jpeg", g_boot_ntrees, width = 7, height = 5, dpi = 300)
ggsave("~/01 Masters_LA/06 Figures/02 plots/bootstrap_msel_centered_cnsolo.jpeg", g_boot_cnsolo, width = 7, height = 5, dpi = 300)


####  2) LEAVE-ONE-SITE-OUT (LOSO) ####

sites <- unique(dadosmisto$site)

loso_results <- map_dfr(sites, function(s) {
  
  dados_loso <- dadosmisto %>% filter(site != s)
  
  mod_loso <- tryCatch(
    suppressWarnings(
      lmer(
        log(biomassa_z_kg) ~ pcps1 + c_n_trees + c_cnsolo + (1 | site),
        data = dados_loso,
        REML = FALSE
      )
    ),
    error = function(e) NULL
  )
  
  # se falhar
  if (is.null(mod_loso)) {
    return(tibble(
      removed_site   = s,
      beta_pcps1     = NA_real_,
      beta_c_n_trees = NA_real_,
      beta_c_cnsolo  = NA_real_,
      r2_marginal    = NA_real_,
      r2_conditional = NA_real_,
      AICc           = NA_real_,
      converged      = FALSE,
      singular       = NA
    ))
  }
  
  conv_ok <- is.null(mod_loso@optinfo$conv$lme4$messages)
  sing_ok <- isSingular(mod_loso, tol = 1e-4)
  
  fe <- fixef(mod_loso)
  
  r2_vals <- tryCatch(r.squaredGLMM(mod_loso), error = function(e) NULL)
  aicc_val <- tryCatch(AICc(mod_loso), error = function(e) NA_real_)
  
  tibble(
    removed_site   = s,
    beta_pcps1     = unname(fe["pcps1"]),
    beta_c_n_trees = unname(fe["c_n_trees"]),
    beta_c_cnsolo  = unname(fe["c_cnsolo"]),
    r2_marginal    = if (!is.null(r2_vals)) r2_vals[1, "R2m"] else NA_real_,
    r2_conditional = if (!is.null(r2_vals)) r2_vals[1, "R2c"] else NA_real_,
    AICc           = aicc_val,
    converged      = conv_ok,
    singular       = sing_ok
  )
})

loso_results


#### Compare with full model       ####


fixef(msel_centered)
r.squaredGLMM(msel_centered)
AICc(msel_centered)


# We assessed the robustness of all fixed effects in the best-supported model (m17) using bootstrap resampling (1000 iterations). All predictors showed highly consistent effects. PCPS1 remained negative in 100% of bootstrap replicates (median ≈ −1.0), SR remained positive in 100% of replicates (median ≈ 0.06), and precipitation seasonality remained negative in 100% of replicates (median ≈ −0.055). These results indicate that the biodiversity–productivity relationship and the climatic constraint driven by seasonal water deficit are both stable and not dependent on specific parcels or sites

# Leave-one-site-out analysis (LOSO) showed that no individual site substantially influenced the results. The effect of species richness remained positive in all models (0.050–0.065), the effect of PCPS1 remained consistently negative (−0.80 to −1.29), and precipitation seasonality remained negative across all runs (−0.039 to −0.066). Marginal R² varied between 0.23 and 0.44, and conditional R² between 0.52 and 0.62, confirming that the structure and magnitude of fixed and random effects were stable. These results demonstrate that model m17 is robust to site-level influence and that the identified biodiversity and climatic effects are general patterns across the restoration areas