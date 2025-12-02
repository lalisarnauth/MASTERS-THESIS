##################
### BASAL AREA ###
##################

library(readxl)
library(readr)
library(tidyverse)
library(ineq)
library(writexl)

dados <- read_excel("01 Datasets/01_raw_data/dados_principal.xlsx")

dados <- dados %>%
  select(site.parcela,especie,area.transv.cm2)

basal_area <- dados %>%
  group_by(site.parcela) %>%
  summarise(basal_area = sum(area.transv.cm2, na.rm = TRUE))

basal_area <- basal_area%>%
  rename(parcela=site.parcela)

write_xlsx(basal_area, path = "~/01 Masters_LA/00 MASTERS-DATA/01 Datasets/01_raw_data/basal_area.xlsx")

dadosmisto <- read.csv("01 Datasets/01_raw_data/dadosmisto.csv",
                       header = TRUE)
dadosmisto <- left_join(dadosmisto,basal_area,by="parcela")

write.csv(dadosmisto,file="~/01 Masters_LA/00 MASTERS-DATA/01 Datasets/01_raw_data/dadosmisto.csv")
