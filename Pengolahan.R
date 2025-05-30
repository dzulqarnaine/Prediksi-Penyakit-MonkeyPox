setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
library(pscl)
library(haven)
library(dplyr)
library(lmtest)
library(DescTools)
library(nortest)
library(ResourceSelection)

df = read.csv("sample.csv", row.names = 1)
df
head(df)
str(df)

df = subset(df, select = -Patient_ID)
str(df)
attach(df)

df[] <- lapply(df, as.factor)
str(df)

#MODEL 1
model_awal = glm(MonkeyPox ~ Systemic.Illness+ Rectal.Pain+ Sore.Throat+ Penile.Oedema+ Oral.Lesions+ Solitary.Lesion+ Swollen.Tonsils+ HIV.Infection+ Sexually.Transmitted.Infection, 
           family = binomial(link = 'logit'), data = df)
summary(model_awal)
model_awal

############MODEL AWAL
#MODEL 1
model_awal = glm(MonkeyPox ~ Systemic.Illness+ Rectal.Pain+ Sore.Throat+ Penile.Oedema+ Oral.Lesions+ Solitary.Lesion+ Swollen.Tonsils+ HIV.Infection+ Sexually.Transmitted.Infection, family = binomial(link = 'logit'), data = df)
summary(model_awal)
model_awal

#####Uji Simultan (Likelihood Rasio)
mod_null <- glm(MonkeyPox ~ 1, family = binomial(link = 'logit'), data = df)
mod_alt <- glm(MonkeyPox ~ Systemic.Illness+ Rectal.Pain+ Sore.Throat+ Penile.Oedema+ Oral.Lesions+ Solitary.Lesion+ Swollen.Tonsils+ HIV.Infection+ Sexually.Transmitted.Infection, 
               family = binomial(link = 'logit'), data = df)

logLik_null <- logLik(mod_null)  
logLik_alt <- logLik(mod_alt)   

minus2_logLik_null <- -2 * logLik_null
minus2_logLik_alt <- -2 * logLik_alt

G_statistic <- minus2_logLik_null - minus2_logLik_alt
v <- length(coef(mod_alt)) - length(coef(mod_null))
chi_square_critical <- qchisq(0.95, df = v)

# Menampilkan hasil
cat("Nilai G (Likelihood Ratio):", G_statistic, "\n")
cat("Nilai chi-square:", chi_square_critical, "\n")


#Uji kesesuaian model Awal (Goodnest OF FIT)
hoslem_test = hoslem.test(model_awal$y, fitted(model_awal))
hoslem_test


#########MODEL TERBAIK
#MODEL 2
model_terbaik = glm(MonkeyPox ~ Systemic.Illness+ Sore.Throat+ Oral.Lesions+ Sexually.Transmitted.Infection, family = binomial(link = 'logit'), data = df)
summary(model_terbaik)
model_terbaik

#####Uji Simultan (Likelihood Rasio)
mod_null <- glm(MonkeyPox ~ 1, family = binomial(link = 'logit'), data = df)
mod_alt <- glm(MonkeyPox ~ Systemic.Illness + Sore.Throat + Oral.Lesions + Sexually.Transmitted.Infection, 
               family = binomial(link = 'logit'), data = df)

logLik_null <- logLik(mod_null)  
logLik_alt <- logLik(mod_alt)   

minus2_logLik_null <- -2 * logLik_null
minus2_logLik_alt <- -2 * logLik_alt

G_statistic <- minus2_logLik_null - minus2_logLik_alt
v <- length(coef(mod_alt)) - length(coef(mod_null))
chi_square_critical <- qchisq(0.95, df = v)

# Menampilkan hasil
cat("Nilai G (Likelihood Ratio):", G_statistic, "\n\n")
cat("Nilai chi-square:", chi_square_critical, "\n")

#Uji kesesuaian model Awal (Goodnest OF FIT)
hoslem_test = hoslem.test(model_terbaik$y, fitted(model_terbaik))
hoslem_test

#Nilai Odds rasio
exp(coef(model_terbaik))













