library(dplyr)
library(tidyr)

# Environmental variables to summarize
env_vars <- c("ppt", "tmax", "tmin", "pet", "vpd", 
              "declividade", "c_solo", "n_solo",
              "season_temp", "season_ppt","n_trees")

env_summary <- dadosmisto %>%
  select(all_of(env_vars)) %>%
  summarise(across(
    everything(),
    list(
      Min  = ~min(.x, na.rm = TRUE),
      Max  = ~max(.x, na.rm = TRUE),
      Mean = ~mean(.x, na.rm = TRUE),
      SD   = ~sd(.x, na.rm = TRUE)
    ),
    .names = "{.col}__{.fn}"
  )) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Variable", ".value"),
    names_sep = "__"
  ) %>%
  mutate(`Mean ± SD` = paste0(
    round(Mean, 2), " ± ", round(SD, 2)
  )) %>%
  select(Variable, Min, Max, `Mean ± SD`)
