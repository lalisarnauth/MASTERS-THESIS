
## FUNCTIONAL STRUCTURE ##


#### Load packages ####

library(dplyr)
library(FD)

#### Load data ####

com <- read.csv("01 Datasets/01_raw_data/comunidade_abund.csv",
                       row.names = 1,
                       header = TRUE,
                       sep = ";")

traits <- read.csv("01 Datasets/01_raw_data/functional_traits_all.csv",
                   row.names = 1,
                   header = TRUE,
                   sep = ";")

# Standardize species names in community (underscores -> spaces)
colnames(com) <- gsub("_", " ", colnames(com))

traits <- as.data.frame(traits)

#### create function ####

run_dbfd_univariate <- function(trait_col, com, traits){
  
  if (!trait_col %in% colnames(traits)) {
    stop(paste0("Trait column not found in traits: ", trait_col,
                "\nAvailable columns are: ", paste(colnames(traits), collapse = ", ")))
  }
  
  # species with non-NA trait value
  spp_ok <- rownames(traits)[!is.na(traits[[trait_col]])]
  
  # shared species between community and traits
  spp_shared <- intersect(colnames(com), spp_ok)
  
  # subset
  com_sub <- com[, spp_shared, drop = FALSE]
  trait_mat <- traits[spp_shared, trait_col, drop = FALSE]  # rows match com_sub columns
  
  # dbFD requires matching labels and order between 'a' and 'x' :contentReference[oaicite:2]{index=2}
  out <- FD::dbFD(
    x = trait_mat,
    a = com_sub,
    stand.x = TRUE,
    calc.CWM = TRUE,
    messages = FALSE
  )
  
  data.frame(
    plot = rownames(com_sub),
    trait = trait_col,
    CWM = as.numeric(out$CWM[, 1]),
    FDis = as.numeric(out$FDis),
    stringsAsFactors = FALSE
  )
}

####  Run for WD and LDMC ####

res_wd   <- run_dbfd_univariate("wd",   com, traits)
res_ldmc <- run_dbfd_univariate("ldmc", com, traits)

results <- rbind(res_wd, res_ldmc)
results

results_wide <- results |>
  tidyr::pivot_wider(
    id_cols = plot,
    names_from = trait,
    values_from = c(CWM, FDis),
    names_glue = "{trait}_{.value}"
  )

library(writexl)

write_xlsx(results_wide,path = "~/01 Masters_LA/00 MASTERS-DATA/01 Datasets/01_raw_data/results_functional.xlsx")

results_wide <- results_wide %>%
  rename(parcela = site)


dadosmisto <- left_join(dadosmisto,results_wide,by="parcela")

write.csv(dadosmisto,file = "~/01 Masters_LA/00 MASTERS-DATA/01 Datasets/01_raw_data/dadosmisto.csv")
