

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

# ---- Raster TerraClimate - PPT ----

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

ggplot() +
  geom_spatraster(data = ppt_fine) +
  scale_fill_viridis_c(name = "Precipitation (mm)") +
  geom_spatvector(data = rj, fill = NA, color = "black", linewidth = 0.8) +
  geom_spatvector(
    data = sites_vect,
    shape = 21,
    fill = "red",
    color = "black",
    size = 2.5
  ) +
  coord_sf() +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "Climatic gradient across restoration sites",
    subtitle = "Mean precipitation (2018-2022)"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(face = "bold", size = 14)
  )

# ---- Oficial version -----

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
    size = 3,
    ) +
  coord_sf(expand = FALSE) +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "Restoration sites across a precipitation gradient",
    subtitle = "TerraClimate mean precipitation (2018–2022)"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11)
  )

p_base

p_scale <- p_base +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    pad_x = unit(5.8, "cm")
  ) +
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    style = north_arrow_minimal()
  )

p_scale

ggsave(
  filename = "C:/Users/Laíla Arnauth/OneDrive/Documentos/01 Masters_LA/08 R_map/mapa_clima_RJ.jpeg",
  plot = p_scale,
  width = 18,
  height = 12,
  units = "cm",
  dpi = 300
)
