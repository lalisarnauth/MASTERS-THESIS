

library(rtry)
library(dplyr)
library(stringr)

try_data <- rtry_import("~/01 Masters_LA/00 MASTERS-DATA/01 Datasets/01_raw_data/Try_all_spp/45563.txt")

head(try_data)
str(try_data)

unique(try_data$TraitID) 
unique(try_data$TraitName)

traits_keep <- c(3116, 47, 4)  # SLA, LDMC, WD

traits_species <- try_data %>%
  filter(TraitID %in% traits_keep, !is.na(StdValue)) %>%
  mutate(
    Species_use = if_else(!is.na(AccSpeciesName) & AccSpeciesName != "",
                          AccSpeciesName, SpeciesName),
    Species_use = str_squish(Species_use),
    n_words = str_count(Species_use, "\\S+")
  ) %>%
  filter(n_words == 2) %>%  # mantém só "Genus species"
  mutate(
    trait = case_when(
      TraitID == 3116 ~ "SLA",
      TraitID == 47   ~ "LDMC",
      TraitID == 4    ~ "WD"
    )
  ) %>%
  group_by(Species_use, trait) %>%
  summarise(
    mean = mean(StdValue, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = Species_use,
    names_from = trait,
    values_from = c(mean, n_obs),
    names_glue = "{trait}_{.value}"
  )

traits_species


## Checking coverage of the data

traits <- traits_species %>%
  select(Species_use, SLA_mean, LDMC_mean, WD_mean)

calc_coverage <- function(trait_col, comm, traits){
  spp_ok <- traits %>%
    filter(!is.na(.data[[trait_col]])) %>%
    pull(Species_use)
  
  # garante que só usa spp que existem na comunidade
  spp_ok <- intersect(spp_ok, colnames(comm))
  
  cov <- rowSums(comm[, spp_ok, drop = FALSE]) / rowSums(comm)
  
  tibble(plot = rownames(comm), trait = trait_col, coverage = cov)
}

coverage_all <- bind_rows(
  calc_coverage("SLA_mean",  comunidade, traits),
  calc_coverage("LDMC_mean", comunidade, traits),
  calc_coverage("WD_mean",   comunidade, traits)
)

coverage_all %>%
  group_by(trait) %>%
  summarise(
    min = min(coverage),
    mean = mean(coverage),
    prop_ge_80 = mean(coverage >= 0.8)
  )

# CWM of WD is ok; not recommended to use LDMC and SLA data

# Check for FD

traits_fd <- traits_species %>%
  drop_na(SLA_mean, LDMC_mean, WD_mean)

nrow(traits_fd)

spp_fd <- intersect(traits_fd$Species_use, colnames(comunidade))
cov_fd <- rowSums(comunidade[, spp_fd, drop = FALSE]) / rowSums(comunidade)

summary(cov_fd)
mean(cov_fd >= 0.8)
