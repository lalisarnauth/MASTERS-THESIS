# ============================================================
# Model comparison table (AICc, R2) -> export to Word
# All models: log(biomassa_z_kg) | data = dadosmisto
# ============================================================

# Packages
library(lme4)
library(MuMIn)        # AICc()
library(performance)  # r2_nakagawa()
library(dplyr)
library(officer)
library(flextable)
library(purrr)

# ---- 1) Fit models ----

m1  <- lmer(log(biomassa_z_kg) ~ sr + n_trees + (1 | site),
            data = dadosmisto, REML = FALSE)

m3  <- lmer(log(biomassa_z_kg) ~ pcps1 + n_trees + (1 | site),
            data = dadosmisto, REML = FALSE)

m4  <- lmer(log(biomassa_z_kg) ~ sespd + n_trees + (1 | site),
            data = dadosmisto, REML = FALSE)

m8  <- lmer(log(biomassa_z_kg) ~ c.n_solo + n_trees + (1 | site),
            data = dadosmisto, REML = FALSE)

m10 <- lmer(log(biomassa_z_kg) ~ season_ppt + n_trees + (1 | site),
            data = dadosmisto, REML = FALSE)

m17 <- lmer(log(biomassa_z_kg) ~ wd_FDis + n_trees + (1 | site),
            data = dadosmisto, REML = FALSE)

m18 <- lmer(log(biomassa_z_kg) ~ ldmc_FDis + n_trees + (1 | site),
            data = dadosmisto, REML = FALSE)

m19 <- lmer(log(biomassa_z_kg) ~ pcps1 + n_trees + (1 | site),
            data = dadosmisto, REML = FALSE)

# Combined models
m12 <- lmer(log(biomassa_z_kg) ~ pcps1 + season_ppt + n_trees + (1 | site),
            data = dadosmisto, REML = FALSE)

m20 <- lmer(log(biomassa_z_kg) ~ pcps1 + season_ppt + n_trees + (1 | site),
            data = dadosmisto, REML = FALSE)

# Selected / best model
msel <- lmer(log(biomassa_z_kg) ~ pcps1 + c.n_solo + n_trees + (1 | site),
             data = dadosmisto, REML = FALSE)

# N-fix models
n2 <- lmer(log(biomassa_z_kg) ~ fdis_nfix + (1 | site),
           data = dadosmisto, REML = FALSE)

n3 <- lmer(log(biomassa_z_kg) ~ cwm_nfix + (1 | site),
           data = dadosmisto, REML = FALSE)

n4_cwm <- lmer(log(biomassa_z_kg) ~ cwm_nfix + n_trees + c.n_solo + (1 | site),
               data = dadosmisto, REML = FALSE)

n4_fdis <- lmer(log(biomassa_z_kg) ~ fdis_nfix + n_trees + c.n_solo + (1 | site),
                data = dadosmisto, REML = FALSE)

# ---- 2) Store models in a list ----

mods <- list(
  m1_SR               = m1,
  m3_PCPS1            = m3,
  m4_sesPD            = m4,
  m8_Soil_CN          = m8,
  m10_PPT_seasonality = m10,
  m17_WD_FDis         = m17,
  m18_LDMC_FDis       = m18,
  m19_PCPS1_only      = m19,
  m12_PCPS1_PPT       = m12,
  m20_PCPS1_PPT_alt   = m20,
  msel_PCPS1_SoilCN   = msel,
  n2_FDis_nfix        = n2,
  n3_CWM_nfix         = n3,
  n4_CWMnfix_SoilCN   = n4_cwm,
  n4_FDisnfix_SoilCN  = n4_fdis
)


# ---- 3) Build comparison table ----

tab <- imap_dfr(mods, function(mod, nm) {
  
  r2 <- r2_nakagawa(mod)
  
  tibble(
    Model = nm,
    Fixed_effects = paste(attr(terms(mod), "term.labels"), collapse = " + "),
    n = nobs(mod),
    AIC = AIC(mod),
    AICc = AICc(mod),
    R2_marginal = r2$R2_marginal,
    R2_conditional = r2$R2_conditional
  )
})

# Delta AICc and weights
tab <- tab %>%
  mutate(
    Delta_AICc = AICc - min(AICc),
    Weight_AICc = exp(-0.5 * Delta_AICc) / sum(exp(-0.5 * Delta_AICc))
  ) %>%
  arrange(Delta_AICc)

# Round values
tab_print <- tab %>%
  mutate(
    across(c(AIC, AICc, Delta_AICc), ~ round(.x, 2)),
    across(c(R2_marginal, R2_conditional, Weight_AICc), ~ round(.x, 3))
  )


# ---- 4) Create Word table ----
ft <- flextable(tab_print) %>%
  set_header_labels(
    Model = "Model",
    Fixed_effects = "Fixed effects",
    n = "n",
    AIC = "AIC",
    AICc = "AICc",
    Delta_AICc = "ΔAICc",
    Weight_AICc = "AICc weight",
    R2_marginal = "R²m",
    R2_conditional = "R²c"
  ) %>%
  autofit() %>%
  theme_booktabs() %>%
  fontsize(size = 10, part = "all") %>%
  bold(part = "header") %>%
  flextable::align(
    j = c("Model", "Fixed_effects"),
    align = "left",
    part = "all"
  ) %>%
  flextable::align(
    j = c("n", "AIC", "AICc", "Delta_AICc",
          "Weight_AICc", "R2_marginal", "R2_conditional"),
    align = "center",
    part = "all"
  )

ft <- flextable::valign(ft, valign = "top", part = "all")
ft <- flextable::set_table_properties(ft, layout = "autofit")

# ---- 5) Export to Word ----

output_path <- "~/01 Masters_LA/00 MASTERS-DATA/01 Datasets/02_processed_data/Table_model_comparison_AGB.docx"

doc <- read_docx() %>%
  body_add_par(
    "Table X. Comparison of linear mixed-effects models explaining aboveground biomass.",
    style = "heading 2"
  ) %>%
  body_add_par(
    "Models were fitted with a random intercept for site. AICc, ΔAICc, and Akaike weights are reported together with marginal (R²m) and conditional (R²c) coefficients of determination.",
    style = "Normal"
  ) %>%
  body_add_flextable(ft)

print(doc, target = output_path)



