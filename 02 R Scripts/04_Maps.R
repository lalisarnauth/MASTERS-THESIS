

## NEW MAP ##

# ---- Load all packages

library(terra)
library(readxl)
library(ggplot2)
library(tidyterra)
library(ggspatial)
library(geobr)
library(cowplot)

# ---- Paths ----

# shapefile
map_path <- "C:/Users/Laíla Arnauth/OneDrive/Documentos/01 Masters_LA/08 R_map"

# rasters TerraClimate
tc_path <- "~/01 Masters_LA/07_environ_data_TerraClimate"

# sites
sites <- read_excel("~/01 Masters_LA/04 Maps/moderadores.xlsx")

# 

rj <- vect(file.path(map_path, "Estado_RJ_23s.shp"))

# check CRS
crs(rj)

# lat/long
rj <- project(rj, "EPSG:4326")

# ---- PPT - Raster TerraClimate ----

files_ppt <- list.files(
  tc_path,
  pattern = "TerraClimate_ppt",
  full.names = TRUE
)

rst_ppt <- rast(files_ppt)

# temporal mean 2018–2022
ppt_mean <- mean(rst_ppt, na.rm = TRUE)

# ---- Clip RJ format ----

crs(ppt_mean) <- "EPSG:4326"

ppt_rj <- crop(ppt_mean, rj)
ppt_rj <- mask(ppt_rj, rj)

# transform into a spatial object
sites_vect <- vect(
  sites,
  geom = c("Longitude", "Latitude"),
  crs = "EPSG:4326"
)


# ---- Plot with Resampling ----

ppt_fine <- disagg(ppt_rj, fact = 8, method = "bilinear")

sites_df <- as.data.frame(sites_vect, geom = "XY")

p_base <- ggplot() +
  geom_spatraster(data = ppt_fine) +
  scale_fill_viridis_c(
    name = "Mean precipitation (mm)",
    na.value = "white"
  ) +
  geom_spatvector(data = rj, fill = NA, color = "black", linewidth = 0.5) +
  geom_point(
    data = sites_df,
    aes(x = x, y = y),
    shape = 21,
    fill = "red",
    color = "black",
    size = 3,
    stroke = 0.3
  ) +
  coord_sf(expand = FALSE) +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "TerraClimate mean precipitation (2018 - 2022)"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    plot.subtitle = element_text(size = 11, hjust = 0)
  )

p_scale <- p_base +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    pad_x = unit(5.8, "cm")
  ) +
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    style = north_arrow_minimal(),
    height = unit(1, "cm"),
    width = unit(1, "cm")
  )

p_scale

ggsave(
  filename = "C:/Users/Laíla Arnauth/OneDrive/Documentos/01 Masters_LA/08 R_map/mapa_clima_RJ1.jpeg",
  plot = p_scale,
  width = 18,
  height = 12,
  units = "cm",
  dpi = 300
)

# ---- TMAX - Raster TerraClimate ----

files_tmax <- list.files(
  tc_path,
  pattern = "TerraClimate_tmax",
  full.names = TRUE
)

rst_tmax <- rast(files_tmax)

# assign CRS
crs(rst_tmax) <- "EPSG:4326"

# temporal mean 2018–2022
tmax_mean <- mean(rst_tmax, na.rm = TRUE)

# ---- Clip RJ format ----

tmax_rj <- crop(tmax_mean, rj)
tmax_rj <- mask(tmax_rj, rj)

# ---- Sites as spatial object ----

sites_vect <- vect(
  sites,
  geom = c("Longitude", "Latitude"),
  crs = "EPSG:4326"
)

# ---- Plot with resampling ----

tmax_fine <- disagg(tmax_rj, fact = 8, method = "bilinear")

sites_df <- as.data.frame(sites_vect, geom = "XY")

p_base <- ggplot() +
  geom_spatraster(data = tmax_fine) +
  scale_fill_viridis_c(
    name = "Mean maximum\ntemperature (°C)",
    na.value = "white"
  ) +
  geom_spatvector(data = rj, fill = NA, color = "black", linewidth = 0.5) +
  geom_point(
    data = sites_df,
    aes(x = x, y = y),
    shape = 21,
    fill = "red",
    color = "black",
    size = 3,
    stroke = 0.3
  ) +
  coord_sf(expand = FALSE) +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "TerraClimate mean maximum temperature (2018 - 2022)"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    plot.subtitle = element_text(size = 11, hjust = 0)
  )

p_scale <- p_base +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    pad_x = unit(5.8, "cm")
  ) +
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    style = north_arrow_minimal(),
    height = unit(1, "cm"),
    width = unit(1, "cm")
  )

p_scale

ggsave(
  filename = "C:/Users/Laíla Arnauth/OneDrive/Documentos/01 Masters_LA/08 R_map/mapa_clima_RJ_tmax.jpeg",
  plot = p_scale,
  width = 18,
  height = 12,
  units = "cm",
  dpi = 300
)

###### Land Use and Cover - MapBiomas ######

# ---- Load packages

library(terra)
library(readxl)
library(ggplot2)
library(tidyterra)
library(ggspatial)

# ---- Paths ----

map_path <- "C:/Users/Laíla Arnauth/OneDrive/Documentos/01 Masters_LA/08 R_map"

mapbiomas_file <- "C:/Users/Laíla Arnauth/OneDrive/Documentos/01 Masters_LA/08 R_map/mapbiomas_10m_collection2_integration_v1-classification_2023.tif"

sites <- read_excel("~/01 Masters_LA/04 Maps/moderadores.xlsx")

sites_clean <- sites %>%
  filter(!(site %in% c("EEG2", "EEG3", "EEG4", "EEG5")))

# ---- Shapefiles RJ ----

rj <- vect(file.path(map_path, "Estado_RJ_23s.shp"))
rj <- project(rj, "EPSG:4326")

# ---- Raster MapBiomas ----

mapb <- rast(mapbiomas_file)

# ensure CRS compatibility
crs(mapb) <- "EPSG:4326"

# cut out in Rio de Janeiro
mapb_rj <- crop(mapb, rj)
mapb_rj <- mask(mapb_rj, rj)
mapb_rj <- as.factor(mapb_rj)

# ---- Reclassify into broad classes ----
# new codes:
# 1 = Forest
# 2 = Natural non-forest
# 3 = Farming
# 4 = No vegetation
# 5 = Urban area
# 6 = Water
# 7 = No data

# 1 = Forest
# 2 = Natural non-forest
# 3 = Farming
# 4 = No vegetation
# 5 = Urban area
# 6 = Water
# 7 = No data

rcl <- matrix(c(
  0,  7,
  1,  1,
  2,  1,
  3,  1,
  4,  1,
  5,  1,
  6,  1,
  7,  1,
  8,  1,
  9,  3,
  10, 2,
  11, 2,
  12, 2,
  13, 2,
  14, 3,
  15, 3,
  16, 3,
  17, 3,
  18, 3,
  19, 3,
  20, 3,
  21, 3,
  22, 4,
  23, 4,
  24, 5,
  25, 4,
  26, 6,
  27, 7,
  29, 2,
  30, 4,
  31, 6,
  32, 2,
  33, 6,
  36, 3,
  49, 1,
  50, 2
), ncol = 2, byrow = TRUE)

mapb_group <- classify(mapb_rj, rcl = rcl, others = NA)
mapb_group <- as.factor(mapb_group)

levels(mapb_group) <- data.frame(
  value = 1:7,
  cover = c(
    "Forest",
    "Natural non-forest",
    "Farming",
    "No vegetation",
    "Urban area",
    "Water",
    "No data"
  )
)

# sites
sites_vect <- vect(
  sites_clean,
  geom = c("Longitude", "Latitude"),
  crs = "EPSG:4326"
)

sites_df <- as.data.frame(sites_vect, geom = "XY")

# ---- Colors ----
class_colors <- c(
  "Forest" = "#1f8d49",
  "Natural non-forest" = "#7a5900",
  "Farming" = "#ffefc3",
  "No vegetation" = "#ffa07a",
  "Urban area" = "#d4271e",
  "Water" = "#2532e4",
  "No data" = "#ffffff"
)

# ---- Plot ----

p_base <- ggplot() +
  geom_spatraster(data = mapb_group, alpha = 0.85) +
  scale_fill_manual(
    values = class_colors,
    na.value = "white",
    name = "Land cover",
    na.translate = FALSE
  ) +
  geom_point(
    data = sites_df,
    aes(x = x, y = y),
    shape = 21,
    fill = "yellow",
    color = "black",
    size = 3.4,
    stroke = 0.5
  ) +
  coord_sf(expand = FALSE) +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "Land cover (MapBiomas 10 m, 2023)"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = "black", linewidth = 0.2),
    axis.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    plot.subtitle = element_text(size = 11, hjust = 0)
  )

p_final <- p_base +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    pad_x = unit(5.8, "cm")
  ) +
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    style = north_arrow_minimal(),
    height = unit(1, "cm"),
    width = unit(1, "cm")
  )

p_final

ggsave(
  filename = "C:/Users/Laíla Arnauth/OneDrive/Documentos/01 Masters_LA/08 R_map/mapa_landcover.tiff",
  plot = p_final,
  width = 18,
  height = 12,
  units = "cm",
  dpi = 600,
  compression = "lzw"
)


###### Elevation Map ###### 

library(elevatr)
library(terra)
library(sf)

# shapefile RJ
rj <- vect(file.path(map_path, "Estado_RJ_23s.shp"))
rj <- project(rj, "EPSG:4326")
rj_sf <- st_as_sf(rj)

# Sites

sites <- read_excel("~/01 Masters_LA/04 Maps/moderadores.xlsx")

sites_clean <- sites %>%
  filter(!(site %in% c("EEG2", "EEG3", "EEG4", "EEG5")))

sites_vect <- vect(
  sites_clean,
  geom = c("Longitude", "Latitude"),
  crs = "EPSG:4326"
)

sites_df <- as.data.frame(sites_vect, geom = "XY")

# download DEM
elev <- get_elev_raster(
  locations = rj_sf,
  z = 7,
  clip = "locations"
)

elev <- rast(elev)

# cut out RJ
elev_rj <- crop(elev, rj)
elev_rj <- mask(elev_rj, rj)

# ---- Plot ----

p_elev <- ggplot() +
  geom_spatraster(data = elev_rj) +
  scale_fill_viridis_c(
    name = "Elevation (m)",
    na.value = "white"
  ) +
  geom_point(
    data = sites_df,
    aes(x = x, y = y),
    shape = 21,
    fill = "red",
    color = "black",
    size = 3,
    stroke = 0.3
  ) +
  coord_sf(expand = FALSE) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    pad_x = unit(5.8, "cm")
  ) +
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    style = north_arrow_minimal(),
    height = unit(1, "cm"),
    width = unit(1, "cm")
  ) +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "Elevation (SRTM/ASTER-derived DEM)"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    axis.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    plot.subtitle = element_text(size = 11, hjust = 0)
  )

p_elev

ggsave(
  filename = "C:/Users/Laíla Arnauth/OneDrive/Documentos/01 Masters_LA/08 R_map/mapa_elevation_RJ.tiff",
  plot = p_elev,
  width = 18,
  height = 12,
  units = "cm",
  dpi = 600,
  compression = "lzw"
)
