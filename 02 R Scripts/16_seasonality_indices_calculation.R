
###############################################
### SEASONALITY INDICES CALCULATION (BIO4 & BIO15)
### Based on TerraClimate data (2018–2022)
###############################################

library(ncdf4)
library(raster)
library(readxl)
library(writexl)
library(dplyr)
library(purrr)
library(tibble)

# Base folder containing TerraClimate rasters
tc_path <- "~/01 Masters_LA/07_environ_data_TerraClimate"

# Load site coordinates
sites <- read_excel("~/01 Masters_LA/04 Maps/moderadores.xlsx")

# Spatial extent to crop rasters
roi_extent <- extent(-44.608007, -40.974161, -22.859079, -21.272083)

# Helper function to load and extract monthly values for a variable
extract_monthly_values <- function(variable) {
  rst <- stack(list.files(tc_path, pattern = paste0("TerraClimate_", variable), full.names = TRUE))
  names(rst) <- apply(expand.grid(1:12, 2018:2022), 1, paste, collapse = "_")
  rst_crop <- crop(rst, roi_extent)
  raw_values <- extract(rst_crop, sites[, c("Longitude", "Latitude")])
  as.data.frame(raw_values)
}

# 1) Extract monthly ppt, tmax, tmin
ppt_df <- extract_monthly_values("ppt")
tmax_df <- extract_monthly_values("tmax")
tmin_df <- extract_monthly_values("tmin")

# 2) Calculate monthly mean temperature for each site
temp_mean_df <- (tmax_df + tmin_df) / 2

# 3) Compute seasonality indices for each site
seasonality_df <- tibble(
  BIO4_temp_seasonality = apply(temp_mean_df, 1, sd, na.rm = TRUE) * 100,
  BIO15_ppt_seasonality = apply(ppt_df, 1, function(x) {
    mean_x <- mean(x, na.rm = TRUE)
    sd_x <- sd(x, na.rm = TRUE)
    (sd_x / mean_x) * 100
  })
)

# 4) Combine with site info
final_df <- bind_cols(sites, seasonality_df)

# 5) Export to Excel
write_xlsx(final_df, "01 Datasets/02_processed_data/amb_TerraClimate_seasonality_2018_2022.xlsx")

###############################################
### OUTPUT:
### Columns: Site info + BIO4_temp_seasonality + BIO15_ppt_seasonality
###############################################

# We quantified climatic seasonality for each site using two bioclimatic indices derived from TerraClimate data (2018–2022): Temperature Seasonality (BIO4) and Precipitation Seasonality (BIO15). Monthly precipitation (ppt) and temperature values were extracted from raster layers using site coordinates. BIO4 was calculated as the standard deviation of monthly mean temperatures (average of tmax and tmin) multiplied by 100, while BIO15 was computed as the coefficient of variation of monthly precipitation, expressed as a percentage. These indices provide a single value per site representing inter-month variability and were generated in R using the raster and dplyr packages for subsequent regression analyses.

## Join with dadosmisto

# Load libraries
library(readr)
library(dplyr)
library(readxl)

# Step 1: Read dadosmisto
dadosmisto <- read.csv("01 Datasets/01_raw_data/dadosmisto.csv",
                       header = TRUE)

dadosmisto <- dadosmisto[, -c(52, 53)]

# Step 2: Read seasonality data
seasonality <- read_excel("~/01 Masters_LA/00 MASTERS-DATA/01 Datasets/02_processed_data/amb_TerraClimate_seasonality_2018_2022.xlsx")

# Step 3: Select relevant columns and rename
seasonality <- seasonality %>%
  dplyr::select(site, BIO4_temp_seasonality, BIO15_ppt_seasonality) %>%
  rename(season_temp = BIO4_temp_seasonality,
         season_ppt = BIO15_ppt_seasonality)

# Step 3.1: Fix site names in seasonality to match dadosmisto

# Remove ML BAIXADA from seasonality
seasonality <- seasonality %>%
  filter(site != "ML BAIXADA")

seasonality$site <- dplyr::recode(seasonality$site,
                           "ML MORRO" = "ML1",
                           "ML BAIXADA" = "ML2",
                           "ML MORRO PEQUENO" = "ML2",
                           "MP_V" = "MP",
                           "REGUA BP" = "REGUA1",
                           "REGUA PV" = "REGUA2",
                           "S_HELENA" = "RESENDE1",
                           "S_JORGE" = "RESENDE3")


# Step 4: Join by 'site' (repeat values for subplots)
dadosmisto <- dadosmisto %>%
  left_join(seasonality, by = "site")

# Step 5: Save updated file
write_csv(dadosmisto, "01 Datasets/01_raw_data/dadosmisto.csv")
