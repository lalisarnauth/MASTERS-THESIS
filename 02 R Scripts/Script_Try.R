## EXPORTING DATA FROM TRY ##

install.packages("rtry")
library(rtry)

# Import the TRY file
input <- rtry_import("C:/Users/laila/OneDrive/Documentos/4. Mestrado_02/TRY/33506.txt")

# Create a dataframe with only the columns of interest
data1 <- rtry_select_col(
  input,
  TraitID, SpeciesName, DataID, StdValue,
  OrigObsDataID, UnitName, ObservationID, Reference
)

# Convert to dataframe
dataf <- as.data.frame(data1)

# Load dplyr
library(dplyr)

# Select only rows corresponding to Wood Density (TraitID = 4)
DM <- dplyr::filter(dataf, TraitID %in% c("4"))

# Remove columns that are not needed
DM1 <- rtry_remove_col(DM, OrigObsDataID, ObservationID)

# Export the final file
setwd("C:/Users/laila/OneDrive/Documentos/2. Mestrado/2. Dados")
write.csv(DM1, file = "DM_Try.csv", row.names = FALSE)
