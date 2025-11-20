
################################################
### TESTING SOIL CARBON AS RESPONSE VARIABLE ###
################################################

# Load packages

library(lme4)
library(lmerTest) # LMM
library(MuMIn) #AICc
library(car)
library(dplyr)
# install.packages("goftest")
library(fitdistrplus)
library(goftest)
library(ggplot2)

# Load data

dadosmisto <- read.csv("01 Datasets/01_raw_data/dadosmisto.csv",
                       header = TRUE,
                       sep = ";")

dadosmisto1 <- dadosmisto[-c(1:7), ]



# Fitting a Gamma distribution to the data
fit_gamma <- fitdist(dadosmisto$c_soloid, "gamma")
fit_gamma <- fitdist(dadosmisto$n_sid, "gamma")
fit_gamma <- fitdist(dadosmisto$c.n_soloid, "gamma")

plot(fit_gamma)

# Anderson-Darling's test

ad.test(dadosmisto$c_soloid, "pgamma", shape = fit_gamma$estimate["shape"], rate = fit_gamma$estimate["rate"])
ad.test(dadosmisto$n_sid, "pgamma", shape = fit_gamma$estimate["shape"], rate = fit_gamma$estimate["rate"])
ad.test(dadosmisto$c.n_soloid, "pgamma", shape = fit_gamma$estimate["shape"], rate = fit_gamma$estimate["rate"]) # Fit gamma


ggplot(dadosmisto, aes(x = c.n_soloid)) +
  geom_density(fill = "blue", alpha = 0.5) +
  stat_function(fun = dgamma, args = list(shape = fit_gamma$estimate["shape"], rate = fit_gamma$estimate["rate"]), color = "red", size = 1) +
  labs(title = "Ajuste da distribuição Gama", x = "n_sid", y = "Densidade")


## Trying to find the best fit to c_soloid & n_sid

library(fitdistrplus)
library(ggplot2)
library(MASS)

par(mfrow = c(1,2))
plot(density(dadosmisto$c_soloid), main = "c_soloid")
plot(density(dadosmisto$n_sid), main = "n_sid")


# Soil C

fit_c_gamma     <- fitdist(dadosmisto$c_soloid, "gamma")
fit_c_lognorm   <- fitdist(dadosmisto$c_soloid, "lnorm")
fit_c_weibull   <- fitdist(dadosmisto$c_soloid, "weibull")
fit_c_norm      <- fitdist(dadosmisto$c_soloid, "norm")

# Soil N

fit_n_gamma     <- fitdist(dadosmisto$n_sid, "gamma")
fit_n_lognorm   <- fitdist(dadosmisto$n_sid, "lnorm")
fit_n_weibull   <- fitdist(dadosmisto$n_sid, "weibull")
fit_n_norm      <- fitdist(dadosmisto$n_sid, "norm")

##

goftest_c <- gofstat(list(
  fit_c_gamma, fit_c_lognorm, fit_c_weibull, fit_c_norm
))
goftest_c$aic # Best: Gamma, but the distribution appears multimodal, moderate fit

goftest_n <- gofstat(list(
  fit_n_gamma, fit_n_lognorm, fit_n_weibull, fit_n_norm
))
goftest_n$aic # Best: Lognormal

#####################
### soil C Models ###
#####################

dados_site <- dadosmisto %>%
  group_by(site) %>%
  summarise(
    c_soloid     = mean(c_soloid),
    n_sid        = mean(n_sid),
    
    SR           = mean(sr),
    SESPD        = mean(sespd),
    SESMPD       = mean(sesmpd),
    SESMNTD      = mean(sesmntd),
    PSV          = mean(psv),
    PSC          = mean(psc),
    PSE          = mean(pse),
    pcps1        = mean(pcps1),
    pcps2        = mean(pcps2),
    
    shannon      = mean(shannon),
    unbias_simp  = mean(unbias_simp),
    gini         = mean(gini),
    PC1nutri     = first(PC1nutri),
    faba         = mean(faba),
    silte        = first(silte),
    PC1          = first(PC1),
    PC2          = first(PC2),
    ppt          = first(ppt),
    tmax         = first(tmax),
    tmin         = first(tmin),
    pet          = first(pet),
    vpd          = first(vpd),
    altitude     = first(altitude),
    declividade  = first(declividade),
    mcwd         = first(mcwd),
    
    .groups = "drop"
  )


modelo_c <- glm(c_soloid ~ gini,data   = dados_site,family = Gamma(link = "log"))
summary(modelo_c) 

modelo_c <- glm(c_soloid ~ SR,data   = dados_site,family = Gamma(link = "log"))
summary(modelo_c) 

modelo_c <- glm(c_soloid ~ SESPD,data   = dados_site,family = Gamma(link = "log"))
summary(modelo_c) 

modelo_c <- glm(c_soloid ~ SESMPD,data   = dados_site,family = Gamma(link = "log"))
summary(modelo_c) 

modelo_c <- glm(c_soloid ~ SESMNTD,data   = dados_site,family = Gamma(link = "log"))
summary(modelo_c) 

modelo_c <- glm(c_soloid ~ PSE,data   = dados_site,family = Gamma(link = "log"))
summary(modelo_c)

modelo_c <- glm(c_soloid ~ pcps1,data   = dados_site,family = Gamma(link = "log"))
summary(modelo_c)

modelo_c <- glm(c_soloid ~ pcps2,data   = dados_site,family = Gamma(link = "log"))
summary(modelo_c)

modelo_c <- glm(c_soloid ~ shannon,data   = dados_site,family = Gamma(link = "log"))
summary(modelo_c)

modelo_c <- glm(c_soloid ~ unbias_simp,data   = dados_site,family = Gamma(link = "log"))
summary(modelo_c)

modelo_c <- glm(c_soloid ~ PC1nutri,data   = dados_site,family = Gamma(link = "log"))
summary(modelo_c)

modelo_c <- glm(c_soloid ~ faba,data   = dados_site,family = Gamma(link = "log"))
summary(modelo_c)

modelo_c <- glm(c_soloid ~ silte,data   = dados_site,family = Gamma(link = "log"))
summary(modelo_c)

modelo_c <- glm(c_soloid ~ PC1,data   = dados_site,family = Gamma(link = "log"))
summary(modelo_c) # p-value = 0.00862 ** # BEST MODEL

modelo_c <- glm(c_soloid ~ PC2,data   = dados_site,family = Gamma(link = "log"))
summary(modelo_c)

modelo_c <- glm(c_soloid ~ ppt,data   = dados_site,family = Gamma(link = "log"))
summary(modelo_c)

modelo_c <- glm(c_soloid ~ tmax,data   = dados_site,family = Gamma(link = "log"))
summary(modelo_c)

modelo_c <- glm(c_soloid ~ tmin,data   = dados_site,family = Gamma(link = "log"))
summary(modelo_c)

modelo_c <- glm(c_soloid ~ pet,data   = dados_site,family = Gamma(link = "log"))
summary(modelo_c) # p-value = 0.0332 *

modelo_c <- glm(c_soloid ~ vpd,data   = dados_site,family = Gamma(link = "log"))
summary(modelo_c) # p-value = 0.0576 .

modelo_c <- glm(c_soloid ~ mcwd,data   = dados_site,family = Gamma(link = "log"))
summary(modelo_c) # p-value = 0.0129 * 

modelo_c <- glm(c_soloid ~ altitude,data   = dados_site,family = Gamma(link = "log"))
summary(modelo_c)

modelo_c <- glm(c_soloid ~ declividade,data   = dados_site,family = Gamma(link = "log"))
summary(modelo_c) # p-value = 0.0755 .

# Multiple

m1 <- glm(c_soloid ~ PC1 + declividade, family = Gamma(link="log"), data=dados_site)
summary(m1)

m1 <- glm(c_soloid ~ pet + declividade, family = Gamma(link="log"), data=dados_site)
summary(m1)

m1 <- glm(c_soloid ~ mcwd + declividade, family = Gamma(link="log"), data=dados_site)
summary(m1)

##############
### Soil N ###
##############

dados_site <- dadosmisto |>
  group_by(site) |>
  summarise(
    n_sid = first(n_sid),
    gini = first(gini),
    SR = first(sr),
    SESPD = first(sespd),
    SESMPD = first(sesmpd),
    SESMNTD = first(sesmntd),
    PSV = first(psv),
    PSC = first(psc),
    PSE = first(pse),
    pcps1 = first(pcps1),
    pcps2 = first(pcps2),
    shannon = first(shannon),
    unbias_simp = first(unbias_simp),
    PC1nutri = first(PC1nutri),
    faba = first(faba),
    silte = first(silte),
    PC1 = first(PC1),
    PC2 = first(PC2),
    ppt = first(ppt),
    tmax = first(tmax),
    tmin = first(tmin),
    pet = first(pet),
    vpd = first(vpd),
    altitude = first(altitude),
    declividade = first(declividade),
    mcwd = first(mcwd),
    .groups = "drop"
  )


modelo <- lm(log(n_sid) ~ gini, data = dados_site)
summary(modelo) # p-value = 0.8767

modelo <- lm(log(n_sid) ~ SR, data = dados_site)
summary(modelo) # p-value = 0.3282

modelo <- lm(log(n_sid) ~ SESPD, data = dados_site)
summary(modelo) # p-value = 0.8221

modelo <- lm(log(n_sid) ~ SESMPD, data = dados_site)
summary(modelo) # p-value = 0.9023

modelo <- lm(log(n_sid) ~ SESMNTD, data = dados_site)
summary(modelo) # p-value = 0.8173

modelo <- lm(log(n_sid) ~ PSV, data = dados_site)
summary(modelo) # p-value = 0.959 

modelo <- lm(log(n_sid) ~ PSC, data = dados_site)
summary(modelo) # p-value = 0.644 

modelo <- lm(log(n_sid) ~ PSE, data = dados_site)
summary(modelo) # p-value = 0.5459 

modelo <- lm(log(n_sid) ~ pcps1, data = dados_site)
summary(modelo) # p-value = 0.0176 *

modelo <- lm(log(n_sid) ~ pcps2, data = dados_site)
summary(modelo) # p-value = 0.6573

modelo <- lm(log(n_sid) ~ shannon, data = dados_site)
summary(modelo) # p-value = 0.5194

modelo <- lm(log(n_sid) ~ unbias_simp, data = dados_site)
summary(modelo) # p-value = 0.655

modelo <- lm(log(n_sid) ~ PC1nutri, data = dados_site)
summary(modelo) # p-value = 0.7127

modelo <- lm(log(n_sid) ~ faba, data = dados_site)
summary(modelo) # p-value = 0.0895 .

modelo <- lm(log(n_sid) ~ silte, data = dados_site)
summary(modelo) # p-value = 0.00127 **

modelo <- lm(log(n_sid) ~ PC1, data = dados_site)
summary(modelo) # p-value = 0.00092 ***

modelo <- lm(log(n_sid) ~ ppt, data = dados_site)
summary(modelo) # p-value = 0.0144 * 

modelo <- lm(log(n_sid) ~ tmax, data = dados_site)
summary(modelo) # p-value = 0.0877 .

modelo <- lm(log(n_sid) ~ tmin, data = dados_site)
summary(modelo) # p-value = 0.00881 **

modelo <- lm(log(n_sid) ~ pet, data = dados_site)
summary(modelo) # p-value = 0.00976 **

modelo <- lm(log(n_sid) ~ vpd, data = dados_site)
summary(modelo) # p-value = 0.1678  

modelo <- lm(log(n_sid) ~ mcwd, data = dados_site)
summary(modelo) # p-value = 0.0254 * 

modelo <- lm(log(n_sid) ~ altitude, data = dados_site)
summary(modelo) # p-value = 0.0336 *

modelo <- lm(log(n_sid) ~ declividade, data = dados_site)
summary(modelo) # p-value = 0.1671

# Multiple

m <- lm(log(n_sid) ~ pcps1 + PC1 + altitude, data = dados_site)
summary(m)
vif(m)
AICc(m)

h <- lm(log(n_sid) ~ pcps1 + PC1, data = dados_site) # BEST MODEL
summary(h)
vif(h)
AICc(h)

# C/N no solo ## REFAZER

library(lme4)
modelo_gini <- glmer(c.n_soloid ~ gini + (1 | site), data = dadosmisto, family = Gamma(link = "log"))
summary(modelo_gini) # p-value = 0.18876       

modelo_SR <- glmer(c.n_soloid ~ SR + (1 | site), data = dadosmisto, family = Gamma(link = "log"))
summary(modelo_SR) # p-value = 0.00502 **  AIC 86.6   

modelo_PD <- glmer(c.n_soloid ~ SESPD + (1 | site), data = dadosmisto, family = Gamma(link = "log"))
summary(modelo_PD) # p-value = 0.468200         

modelo_MPD <- glmer(c.n_soloid ~ SESMPD + (1 | site), data = dadosmisto, family = Gamma(link = "log"))
summary(modelo_MPD) # p-value = 0.599257    

modelo_MNTD <- glmer(c.n_soloid ~ SESMNTD + (1 | site), data = dadosmisto, family = Gamma(link = "log"))
summary(modelo_MNTD) # p-value = 0.480426    

modelo_PSV <- glmer(c.n_soloid ~ PSV + (1 | site), data = dadosmisto, family = Gamma(link = "log"))
summary(modelo_PSV) # p-value = 0.3800  

modelo_PSC <- glmer(c.n_soloid ~ PSC + (1 | site), data = dadosmisto, family = Gamma(link = "log"))
summary(modelo_PSC) # p-value = 0.954735     

modelo_PSE <- glmer(c.n_soloid ~ PSE + (1 | site), data = dadosmisto, family = Gamma(link = "log"))
summary(modelo_PSE) # p-value = 0.928718    

modelo_pcps1 <- glmer(c.n_soloid ~ pcps1 + (1 | site), data = dadosmisto, family = Gamma(link = "log"))
summary(modelo_pcps1) # p-value = 0.308419

modelo_pcps2 <- glmer(c.n_soloid ~ pcps2 + (1 | site), data = dadosmisto, family = Gamma(link = "log"))
summary(modelo_pcps2) # p-value = 0.757667 

dadosmisto$shan_scaled <- scale(dadosmisto$shannon)
modelo_shannon <- glmer(c.n_soloid ~ shan_scaled + (1 | site), data = dadosmisto, family = Gamma(link = "log"))
summary(modelo_shannon) # p-value = 0.015125 * AIC 88.4 

modelo_simp <- glmer(c.n_soloid ~ unbias.simp + (1 | site), data = dadosmisto, family = Gamma(link = "log"))
summary(modelo_simp) # p-value = 0.0882 . AIC 91.2    

modelo_PC1nutri <- glmer(c.n_soloid ~ PC1nutri + (1 | site), data = dadosmisto, family = Gamma(link = "log"))
summary(modelo_PC1nutri) # p-value = 0.0857 .  AIC 91.2    

modelo_faba <- glmer(c.n_soloid ~ faba + (1 | site), data = dadosmisto, family = Gamma(link = "log"))
summary(modelo_faba) # p-value = 0.325937     

dadosmisto$silte_scaled <- scale(dadosmisto$silte_g.kg)
modelo_silte <- glmer(c.n_soloid ~ silte_scaled + (1 | site), data = dadosmisto, family = Gamma(link = "log"))
summary(modelo_silte) # p-value = 0.000426 ***

modelo_PC1 <- glmer(c.n_soloid ~ PC1 + (1 | site), data = dadosmisto, family = Gamma(link = "log"))
summary(modelo_PC1) # p-value = 0.636140 

modelo_PC2 <- glmer(c.n_soloid ~ PC2 + (1 | site), data = dadosmisto, family = Gamma(link = "log"))
summary(modelo_PC2) # p-value = 0.50013        

dadosmisto$ppt_scale <- scale(dadosmisto$ppt)
modelo_PPT <- glmer(c.n_soloid ~ ppt_scale + (1 | site), data = dadosmisto, family = Gamma(link = "log"))
summary(modelo_PPT) # p-value = 0.031663 * AIC 86.8     

modelo_tmax <- glmer(c.n_soloid ~ tmax + (1 | site), data = dadosmisto, family = Gamma(link = "log"))
summary(modelo_tmax) # p-value = 0.188

modelo_tmin <- glmer(c.n_soloid ~ tmin + (1 | site), data = dadosmisto, family = Gamma(link = "log"))
summary(modelo_tmin) # p-value = 0.026 * 

dadosmisto$pet_scale <- scale(dadosmisto$pet)
modelo_pet <- glmer(c.n_soloid ~ pet_scale + (1 | site), data = dadosmisto, family = Gamma(link = "log"))
summary(modelo_pet) # p-value =  0.095239 . AIC 90.7

modelo_vpd <- glmer(c.n_soloid ~ vpd + (1 | site), data = dadosmisto, family = Gamma(link = "log"))
summary(modelo_vpd) # p-value = 0.252

dadosmisto$alt_scale <- scale(dadosmisto$altitude)
modelo_alt <- glmer(c.n_soloid ~ alt_scale + (1 | site), data = dadosmisto, family = Gamma(link = "log"))
summary(modelo_alt) # p-value = 0.45148   

modelo_dec <- glmer(c.n_soloid ~ declividade + (1 | site), data = dadosmisto, family = Gamma(link = "log"))
summary(modelo_dec) # p-value = 1.39e-05 *** AIC 92.8

modelo_mcwd <- glmer(c.n_soloid ~ mcwd_scale + (1 | site), data = dadosmisto, family = Gamma(link = "log"))
summary(modelo_mcwd) # p-value = 0.5360  



################
### GRÁFICOS ###
################

library(tidyverse)
library(ggplot2)
setwd("C:/Users/laila/OneDrive/Documentos/2. Mestrado/2. Análise Estatística/graficos")

## valor-t 
dadosmisto %>%
  ggplot(aes(valor_t,c_soloid))+
  geom_point(size=3,alpha=0.5)+ 
  geom_smooth(method = lm,se=T,colour="red")+
  labs(x="t-value",
       y="Soil Carbon (CHN%)",
       title = "Relationship between t-value and Soil Carbon")+
  annotate("text",x=11,y=0.17,label="R²c = 0.76")+
  theme_replace()

ggsave("csoloid_valor.t.jpeg",width = 15,height = 10,units = "cm")

## PC1 
dadosmisto %>%
  ggplot(aes(PC1,c_soloid))+
  geom_point(size=3,alpha=0.5)+ 
  geom_smooth(method = lm,se=T,colour="purple")+
  labs(x="PCA - granulometry - axis 1",
       y="Soil Carbon (CHN%)",
       title = "Relationship between Granulometry and Soil Carbon")+
  annotate("text",x=3,y=0.17,label="R²c = 0.67")+
  theme_replace()

ggsave("csoloid_PC1.jpeg",width = 15,height = 10,units = "cm")

## valor-t 
dadosmisto %>%
  ggplot(aes(PC1nutri,c_soloid))+
  geom_point(size=3,alpha=0.5)+ 
  geom_smooth(method = lm,se=T,colour="yellow")+
  labs(x="PCA soil nutrients - axis 1",
       y="Soil Carbon (CHN%)",
       title = "Relationship between soil nutrients and Soil Carbon")+
  annotate("text",x=1.5,y=0.17,label="R²c = 0.53")+
  theme_replace()

ggsave("csoloid_PC1nutri.jpeg",width = 15,height = 10,units = "cm")

## riqueza
dadosmisto %>%
  ggplot(aes(SR,c_soloid))+
  geom_point(size=3,alpha=0.5)+ 
  geom_smooth(method = lm,se=T,colour="yellow")+
  labs(x="PCA soil nutrients - axis 1",
       y="Soil Carbon (CHN%)",
       title = "Relationship between soil nutrients and Soil Carbon")+
  annotate("text",x=1.5,y=0.17,label="R²c = 0.53")+
  theme_replace()
