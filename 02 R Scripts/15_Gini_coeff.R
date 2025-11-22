
#############################################
########## STRUCTURAL DIVERSITY #############
###########   Gini (DBH)   ##################
#############################################

# Packages

library(dplyr)
library(readxl)
library(ineq)
library(writexl)

# 1) Load data (must contain at least: site | dap)
data <- read_excel("~/01 Masters_LA/00 MASTERS-DATA/01 Datasets/01_raw_data/dap.xlsx")

# 2) Compute the Gini coefficient per site
gini_results <- data %>%
  group_by(site) %>%
  summarise(
    gini = ineq(dap, type = "Gini")
  )

# 3) Display results
print(gini_results)

# 4) Save results to Excel
write_xlsx(
  gini_results,
  "~/01 Masters_LA/00 MASTERS-DATA/01 Datasets/02_processed_data/gini.xlsx"
)
