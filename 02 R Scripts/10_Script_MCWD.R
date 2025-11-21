
############################################################
### MCWD extraction per site (2016–2020) + temporal mean
### Author: Laíla Arnauth
### Repository: Master’s Thesis – Functional & Phylogenetic Diversity
############################################################

# ============================
# 1. Load packages
# ============================
library(readxl)  # read Excel tables
library(sf)      # spatial vector data
library(terra)   # raster / SpatRaster
library(writexl) # write Excel files

# ============================
# 2. Define paths (relative to project root)
# ============================
sites_path  <- "01 Datasets/01_raw_data/moderadores.xlsx"
mcwd_dir    <- "C:/Global_MCWD" # (ask me and I can send you the .tif data)
output_path <- "01 Datasets/02_processed_data/mcwd_sites.xlsx"

# Make sure output folder exists
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

# ============================
# 3. Load sites and convert to sf
# ============================
sites <- read_excel(sites_path)

# Expecting columns "Longitude" and "Latitude" in WGS84
sites_sf <- st_as_sf(
  sites,
  coords = c("Longitude", "Latitude"),
  crs    = 4326               # EPSG:4326 (WGS84)
)

# ============================
# 4. Load MCWD rasters (2016–2020)
# ============================
years <- 2016:2020

# Build file paths explicitly to keep the year order correct
raster_files <- file.path(mcwd_dir, paste0("MCWD_", years, ".tif"))

# Optional: check that all files exist
if (!all(file.exists(raster_files))) {
  missing_files <- raster_files[!file.exists(raster_files)]
  stop("These MCWD files are missing:\n", paste(missing_files, collapse = "\n"))
}

# Load as SpatRaster
mcwd_stack <- rast(raster_files)
names(mcwd_stack) <- paste0("MCWD_", years)

# Reproject sites to match raster CRS (if needed)
sites_sf <- st_transform(sites_sf, crs(mcwd_stack))

# ============================
# 5. Extract MCWD values for all sites and years
# ============================
mcwd_values <- extract(mcwd_stack, vect(sites_sf))

# Rename columns: first column is cell ID returned by terra::extract
colnames(mcwd_values) <- c("cell_id", paste0("MCWD_", years))

# ============================
# 6. Compute temporal mean per site (row-wise)
# ============================
# Mean MCWD across 2016–2020 for each site
mcwd_values$MCWD_mean_2016_2020 <- rowMeans(
  mcwd_values[, paste0("MCWD_", years)],
  na.rm = TRUE
)

# ============================
# 7. Combine with original site table
# ============================
# Drop `cell_id` and bind to original sites table
sites_out <- cbind(
  sites,
  mcwd_values[, -1]   # remove cell_id
)

# ============================
# 8. Export to Excel
# ============================
write_xlsx(sites_out, output_path)

# ============================
# (Optional) Quick visual check for a single year
# ============================
 plot(mcwd_stack[[1]], main = "MCWD 2016")
 plot(st_geometry(sites_sf), add = TRUE, col = "red", pch = 20, cex = 1.2)
















