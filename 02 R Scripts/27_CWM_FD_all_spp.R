
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

# ---- Nfix ----

traits_nfix <- traits[, 4, drop = FALSE]
traits_nfix$nfix <- as.factor(traits_nfix$nfix)

common_species <- intersect(colnames(com), rownames(traits_nfix))
comm_nfix <- com[, common_species]
traits_nfix <- traits_nfix[common_species, , drop = FALSE]

# Calculate

fd_nfix <- dbFD(
  x = traits_nfix,
  a = comm_nfix,
  stand.x = FALSE,
  calc.FRic = FALSE,
  calc.CWM = TRUE,
  messages = FALSE
)

# RaoQ N fix
raoQ_nfix <- fd_nfix$RaoQ

raoQ_nfix_noCSA <- raoQ_nfix[!grepl("^CSA", names(raoQ_nfix))]
dadosmisto1$raoQ_nfix <- raoQ_nfix_noCSA[as.character(dadosmisto1$parcela)]

summary(dadosmisto1$raoQ_nfix)
sum(is.na(dadosmisto1$raoQ_nfix))

# FDis

fdis_nfix <- fd_nfix$FDis
fdis_nfix_noCSA <- fdis_nfix[!grepl("^CSA", names(fdis_nfix))]
dadosmisto1$fdis_nfix <- fdis_nfix_noCSA[as.character(dadosmisto1$parcela)]

# ---- CWM ----

calc_cwm_nfix <- function(comm_nfix, traits_nfix) {
  comm_mat <- as.matrix(comm_nfix)
  storage.mode(comm_mat) <- "numeric"
  
  nfix_vec <- as.numeric(as.character(traits_nfix$nfix))  # 0/1
  # opcional (mas bom): nomear para manter alinhado
  names(nfix_vec) <- rownames(traits_nfix)
  
  apply(comm_mat, 1, function(x) {
    sum(x * nfix_vec, na.rm = TRUE) / sum(x, na.rm = TRUE)
  })
}

cwm_nfix <- calc_cwm_nfix(comm_nfix, traits_nfix)
summary(cwm_nfix)
head(cwm_nfix)


cwm_nfix_noCSA <- cwm_nfix[!grepl("^CSA", names(cwm_nfix))]
dadosmisto1$cwm_nfix <- cwm_nfix_noCSA[
  match(as.character(dadosmisto1$parcela), names(cwm_nfix_noCSA))
]

write.csv(dadosmisto1,file = "~/01 Masters_LA/00 MASTERS-DATA/01 Datasets/01_raw_data/dadosmisto.csv",)

