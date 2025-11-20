### DADOS AMBIENTAIS ###

# BAIXAR NO TERRA CLIMATE

#-/-

# Raster
# obs: ppt = precipitation (mm); pet = potential_evaporation (mm)
## tmax (média mensal) = temperatura máxima (ºC); tmin (média mensal) = temperatura mínima (ºC)
### vpd (média mensal) = Vapor Pressure Deficit (kpd)

install.packages("ncdf4")
library(ncdf4)
library(raster)
library(readxl)
library(writexl)

##Carrega o raster como stack
# ppt
terraclimate_rst <- stack(list.files("~/01 Masters_LA/07_environ_data_TerraClimate", pattern = "TerraClimate_ppt", full.names = T))
#tmax
terraclimate_rst <- stack(list.files("~/01 Masters_LA/07_environ_data_TerraClimate", pattern = "TerraClimate_tmax", full.names = T))
#tmin
terraclimate_rst <- stack(list.files("~/01 Masters_LA/07_environ_data_TerraClimate", pattern = "TerraClimate_tmin", full.names = T))
# pet
terraclimate_rst <- stack(list.files("~/01 Masters_LA/07_environ_data_TerraClimate", pattern = "TerraClimate_pet", full.names = T))
# vpd
terraclimate_rst <- stack(list.files("~/01 Masters_LA/07_environ_data_TerraClimate", pattern = "TerraClimate_vpd", full.names = T))
warnings()

##renomear os layers do stack
names(terraclimate_rst) <- apply(expand.grid(1:12, 2018:2022),1, FUN = paste, collapse = "_")

##data.frame com as coordenadas latlong

moderadores <- read_excel("~/01 Masters_LA/04 Maps/moderadores.xlsx")
View(moderadores)

##Cortar o raster em um extent para reduzir o uso da memória
e1 <- extent(-44.608007, -40.974161, -22.859079, -21.272083)
terraclimate_rst_crop <- crop(terraclimate_rst, e1)

##extrair os dados climáticos
env_raw <- extract(terraclimate_rst_crop, moderadores[, c("Longitude", "Latitude")])

env_raw_df <- as.data.frame(env_raw)

# 6) Calcular média temporal por site (média das 60 camadas)
env_mean_by_site <- env_raw_df %>%
  mutate(mean_vpd = rowMeans(., na.rm = TRUE))

env_final <- bind_cols(moderadores, env_mean_by_site["mean_vpd"])

## Salvar em planilha do excel

env_data_df <- as.data.frame(env_data)
write_xlsx(env_data_df, "~/01 Masters_LA/00 MASTERS-DATA/01 Datasets/01_raw_data/amb_vpd.xlsx")













##########################################################################

###############################################
### ENVIRONMENTAL DATA EXTRACTION - TerraClimate
### Averaged for each site (2018–2022)
###############################################

library(ncdf4)
library(raster)
library(readxl)
library(writexl)
library(dplyr)
library(purrr)
library(tibble)  # for tibble()

# Base folder containing TerraClimate rasters
tc_path <- "~/01 Masters_LA/07_environ_data_TerraClimate"

# Load site coordinates
sites <- read_excel("~/01 Masters_LA/04 Maps/moderadores.xlsx")

# Spatial extent to crop rasters (reduces memory usage)
roi_extent <- extent(-44.608007, -40.974161, -22.859079, -21.272083)

# Function:
# - loads raster stack
# - renames layers (month_year)
# - crops to extent
# - extracts raster values at site coordinates
# - calculates temporal mean (2018–2022)
extract_tc_mean <- function(variable, output_col) {
  
  # 1) Load TerraClimate rasters
  rst <- stack(list.files(
    tc_path,
    pattern = paste0("TerraClimate_", variable),
    full.names = TRUE
  ))
  
  # 2) Rename layers (1_2018, 2_2018, ... 12_2022)
  names(rst) <- apply(
    expand.grid(1:12, 2018:2022),
    1,
    FUN = paste,
    collapse = "_"
  )
  
  # 3) Crop raster
  rst_crop <- crop(rst, roi_extent)
  
  # 4) Extract climate values at each site
  raw_values <- extract(rst_crop, sites[, c("Longitude", "Latitude")])
  raw_df <- as.data.frame(raw_values)
  
  # 5) Temporal mean for each site (base R + tibble)
  mean_values <- rowMeans(raw_df, na.rm = TRUE)
  mean_df <- tibble(!!output_col := mean_values)
  
  return(mean_df)
}

# TerraClimate variables and output column names
tc_vars   <- c("ppt", "tmax", "tmin", "pet", "vpd")
tc_output <- c("mean_ppt", "mean_tmax", "mean_tmin", "mean_pet", "mean_vpd")

# Apply extraction function to each variable (bind columns)
tc_means <- purrr::map2_dfc(tc_vars, tc_output, extract_tc_mean)

# Final dataset: site info + climate means
env_data_final <- dplyr::bind_cols(sites, tc_means)

# Export final dataset
writexl::write_xlsx(
  env_data_final,
  "01 Datasets/02_processed_data/amb_TerraClimate_means_2018_2022.xlsx"
)


