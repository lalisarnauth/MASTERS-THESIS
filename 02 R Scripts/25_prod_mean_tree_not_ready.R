
library(readxl)
library(readr)
library(tidyverse)
library(ineq)
library(writexl)

dados <- read_excel("01 Datasets/01_raw_data/dados_principal.xlsx")

dados <- dados %>%
  rename(parcela=site.parcela)

count <- dados %>%
  group_by(parcela)%>%
  summarise(tree_count=n())

biomass <- dados%>%
  group_by(parcela) %>%
  summarise(biomass = sum(produt_z_g.ano))

data <- left_join(count,biomass,by = "parcela")

data <- data %>%
  mutate(prod.mean.tree= biomass/tree_count)


dadosmisto <- read.csv("01 Datasets/01_raw_data/dadosmisto.csv",
                       header = TRUE)

dadosmisto <- left_join(dadosmisto,data,by="parcela")

dadosmisto1 <- dadosmisto[-c(1:7), ]



m1 <- lmer(prod.mean.tree ~ scale(basal_area) + (1 | site), data = dadosmisto1, REML = FALSE)
summary(m1)  # p-value =
r.squaredGLMM(m1) 
AICc(m1) 

m1 <- lmer(prod.mean.tree ~ PD_q2 + (1 | site), data = dadosmisto1, REML = FALSE)
summary(m1)  # p-value =
r.squaredGLMM(m1) 
AICc(m1) 

