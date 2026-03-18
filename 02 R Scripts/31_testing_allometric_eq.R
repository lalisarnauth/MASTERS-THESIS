df <- read.csv("C:/Users/laila/OneDrive/Documentos/01 Masters_LA/00 MASTERS-DATA/01 Datasets/01_raw_data/biomass_chaves.txt")

dados_teste <- left_join(dadosmisto,df,by="parcela")

model <- lm(biomassa_z_kg ~ biomassa_chaves, data = dados_teste)

plot(dados_teste$biomassa_chaves, dados_teste$biomassa_z_kg)
abline(model, col = "red")

summary(model)

chaves <- read.csv("C:/Users/laila/OneDrive/Documentos/01 Masters_LA/00 MASTERS-DATA/01 Datasets/01_raw_data/chaves_functional.txt")

dados_teste2 <- left_join(chaves,dadosmisto,by="parcela")

model2 <- lm(biomassa_z_kg ~ biomassa_chave, data = dados_teste2)
summary(model2)

plot(dados_teste2$biomassa_chave, dados_teste2$biomassa_z_kg)
abline(model, col = "red")


