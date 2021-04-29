# Compute IFF Estimates
# Alice Lepissier
# alice.lepissier@gmail.com
# Originally prepared for the United Nations Economic Commission for Africa (UNECA)

## ## ## ## ## ## ## ## ## ## ##
# INDEX                     ####
## ## ## ## ## ## ## ## ## ## ##
# Preamble
# Create Variables
# .. Create dependent variable and predictors
# .. Variable transformation
# Remove Outliers
# .. Truncate panel and remove CIF ratios greater than 100
# .. Estimate regression
# .. Identify and remove outliers
# Mis-Invoicing Estimates
# .. Estimate CIF rates
# .. Compute fitted values when predictors are 0
# .. Compute FOB imports
# .. Estimate fixed effects regression
# .. Harmonization procedure
# .. Compute IFF



## ## ## ## ## ## ## ## ## ## ##
# PREAMBLE                  ####
## ## ## ## ## ## ## ## ## ## ##

setwd("/home/alepissier/IFFe/") # Virtual server
data.disk <- "/scratch/alepissier/IFFe/"
# source("Scripts/Data Preparation.R")
library(car)
library(ggridges)
library(kableExtra)
library(lfe)
library(reshape2)
library(scales)
library(stargazer)
library(tidyverse)
options(scipen = 999)



## ## ## ## ## ## ## ## ## ## ##
# CREATE VARIABLES          ####
## ## ## ## ## ## ## ## ## ## ##

load(paste0(data.disk, "Data/Panel/panel.Rdata"))

length(unique(panel$id)) == nrow(panel)
# TRUE, 23266944 obs

panel %>%
  filter(pExport_value == 0 & is.na(pReExport_value)) %>% nrow
# There is no case where a mirrored Re-Export value is missing while
# the mirrored Export value exists.

panel <- panel %>%
  mutate(NetExport_value = Export_value - ReExport_value,
         pNetExport_value = pExport_value - pReExport_value)

panel <- panel %>%
  select(-ends_with("quantity"), -ends_with("weight"))


# .. Create dependent variables and predictors ####
panel <- panel %>% 
  mutate(ratio_CIF = Import_value / pNetExport_value) %>%
  filter(is.finite(ratio_CIF))
nrow(panel)
# 8981203

summary(panel$ratio_CIF)
hist(panel$ratio_CIF)

panel <- panel %>%
  mutate(ln.ratio_CIF = log(ratio_CIF)) %>%
  filter(is.finite(ln.ratio_CIF))
nrow(panel)
# 8459790

summary(panel$year)
panel <- panel %>% 
  group_by(reporter.ISO, partner.ISO, commodity.code) %>%
  mutate(ln.ratio_CIF_lag = dplyr::lag(ln.ratio_CIF, n = 1)) %>%
  ungroup() %>%
  filter(is.finite(ln.ratio_CIF_lag))
nrow(panel)
# 7633219
summary(panel$year)

panel <- panel %>% 
  group_by(reporter.ISO, partner.ISO, commodity.code) %>%
  mutate(Import_fut = dplyr::lead(Import_value, n = 1)) %>%
  ungroup() %>%
  filter(is.finite(Import_fut))
nrow(panel)
# 6940232
summary(panel$year)

panel <- panel %>%
  mutate(FutImport_misrep = Import_fut / Import_value,
         ReExport_misrep = pReExport_value / Import_value) %>%
  filter(is.finite(FutImport_misrep) & is.finite(ReExport_misrep))
nrow(panel)
# 6940232

panel <- panel %>%
  filter(complete.cases(ratio_CIF, ln.ratio_CIF, ln.ratio_CIF_lag,
                        dist, contig, rLandlocked, pLandlocked,
                        FutImport_misrep, ReExport_misrep, 
                        tariff, rCorruption, pCorruption, rRegulatory.qual, pRegulatory.qual))
nrow(panel)
# 5044407


# .. Variable transformation ####
panel <- panel %>%
  mutate(dist.sq = I(dist^2))

panel <- panel %>%
  mutate(rCorruption = 100 - rCorruption,
         pCorruption = 100 - pCorruption,
         rRegulatory.qual = 100 - rRegulatory.qual,
         pRegulatory.qual = 100 - pRegulatory.qual)

panel <- panel %>%
  rename(rPoorRegulation = rRegulatory.qual,
         pPoorRegulation = pRegulatory.qual)

hist(panel$ln.ratio_CIF)
hist(panel$FutImport_misrep)
hist(log(panel$FutImport_misrep))
hist(panel$ReExport_misrep)
hist(log(panel$ReExport_misrep))
hist(panel$tariff)
hist(log(panel$tariff))
hist(panel$rCorruption)
hist(panel$pCorruption)
hist(panel$rPoorRegulation)
hist(panel$pPoorRegulation)
hist(panel$dist)
hist(panel$dist.sq)

ihs <- function(x){
  x <- log(x + sqrt(x^2 + 1))
  return(x)
}

# Check whether there are zeros in the data
summary(log(panel$FutImport_misrep))
# Fine to log
summary(log(panel$ReExport_misrep))
# Need inverse hyperbolic sine transformation
summary(log(panel$tariff))
# Need inverse hyperbolic sine transformation

panel <- panel %>%
  mutate(ln.FutImport_misrep = log(FutImport_misrep),
         ihs.ReExport_misrep = ihs(ReExport_misrep),
         ihs.tariff = ihs(tariff))
hist(panel$ln.FutImport_misrep)
hist(panel$ihs.ReExport_misrep)
hist(panel$ihs.tariff)

rm(ihs)
nrow(panel)
# 5044407
save(panel, file = paste0(data.disk, "Data/Panel/panel_clean.Rdata"))



## ## ## ## ## ## ## ## ## ## ##
# REMOVE OUTLIERS           ####
## ## ## ## ## ## ## ## ## ## ##

# .. Truncate panel and remove CIF ratios greater than 100 ####
summary(panel$ratio_CIF)

panel %>% filter(ratio_CIF > 10^4) %>% nrow
# 8165
panel %>% filter(ratio_CIF > 10^3) %>% nrow
# 32947

panel <- panel %>% 
  filter(ratio_CIF < 10^2)
nrow(panel)
# 4922786

# .. Estimate regression ####
fit <- lm(ln.ratio_CIF ~ dist + dist.sq +
            contig + 
            rLandlocked +
            pLandlocked +
            ln.FutImport_misrep +
            ihs.ReExport_misrep +
            ln.ratio_CIF_lag +
            tariff + 
            rCorruption + pCorruption +
            rPoorRegulation + pPoorRegulation,
          data = panel)
summary(fit)
mean(exp(fitted(fit)))
# 1.738642
max(panel$ratio_CIF)
# 99.99301
mean(panel$ratio_CIF)
# 3.399862


# .. Identify and remove outliers ####
panel$CD <- cooks.distance(fit)
summary(panel$CD)

while(max(panel$CD) > 2){
  panel <- panel %>%
    filter(CD <= 2)
  fit <- lm(ln.ratio_CIF ~ dist + dist.sq +
              contig + 
              rLandlocked +
              pLandlocked +
              ln.FutImport_misrep +
              ihs.ReExport_misrep +
              ln.ratio_CIF_lag +
              tariff + 
              rCorruption + pCorruption +
              rPoorRegulation + pPoorRegulation,
            data = panel)
  panel$CD <- cooks.distance(fit)
}
nrow(panel)
# 4922786
summary(panel$CD)

Bonferonni.out <- outlierTest(fit, n.max = 10000)
obs <- as.numeric(names(Bonferonni.out[[1]]))
outliers <- panel[c(obs), ]
mean(outliers$ratio_CIF)
# 2.312933
mean(panel$ratio_CIF)
# 3.399862
panel <- panel[-c(obs),]
mean(panel$ratio_CIF)
# 3.400767

fit <- lm(ln.ratio_CIF ~ dist + dist.sq +
            contig + 
            rLandlocked +
            pLandlocked +
            ln.FutImport_misrep +
            ihs.ReExport_misrep +
            ln.ratio_CIF_lag +
            tariff + 
            rCorruption + pCorruption +
            rPoorRegulation + pPoorRegulation,
          data = panel)
Bonferonni.out <- outlierTest(fit, n.max = 10000)
obs <- as.numeric(names(Bonferonni.out[[1]]))
outliers <- panel[c(obs), ]
mean(outliers$ratio_CIF)
# 3.610739
mean(panel$ratio_CIF)
# 3.400767
panel <- panel[-c(obs),]
mean(panel$ratio_CIF)
# 3.400742

fit <- lm(ln.ratio_CIF ~ dist + dist.sq +
            contig + 
            rLandlocked +
            pLandlocked +
            ln.FutImport_misrep +
            ihs.ReExport_misrep +
            ln.ratio_CIF_lag +
            tariff + 
            rCorruption + pCorruption +
            rPoorRegulation + pPoorRegulation,
          data = panel)
Bonferonni.out <- outlierTest(fit, n.max = 10000)
obs <- as.numeric(names(Bonferonni.out[[1]]))
outliers <- panel[c(obs), ]
mean(outliers$ratio_CIF)
# 3.726255
mean(panel$ratio_CIF)
# 3.400742
panel <- panel[-c(obs),]
mean(panel$ratio_CIF)
# 3.400737

rm(Bonferonni.out, outliers, obs)
nrow(panel)
# 4918047
save(panel, file = paste0(data.disk, "Data/Panel/panel_nooutliers.Rdata"))



## ## ## ## ## ## ## ## ## ## ##
# MIS-INVOICING ESTIMATES   ####
## ## ## ## ## ## ## ## ## ## ##

# .. Estimate CIF rates ####
load(paste0(data.disk, "Data/Panel/panel_nooutliers.Rdata"))

nrow(panel)
# 4918047
max(panel$ratio_CIF)
# 99.99301
hist(panel$ratio_CIF)
summary(panel$ratio_CIF)

fit <- lm(ln.ratio_CIF ~ dist + dist.sq +
            contig + 
            rLandlocked +
            pLandlocked +
            ln.FutImport_misrep +
            ihs.ReExport_misrep +
            ln.ratio_CIF_lag +
            tariff + 
            rCorruption + pCorruption +
            rPoorRegulation + pPoorRegulation,
          data = panel)
summary(fit)
mean(exp(fitted(fit)))
# 1.734554

d <- panel %>% distinct(dist) %>%
  mutate(y = coef(fit)["dist"]*dist + coef(fit)["dist.sq"]*dist^2)
ggplot(d, aes(x = dist, y = y)) + 
  geom_line()

kable(vif(fit), digits = 3, format = "rst")

save(fit, file = "Results/fit")

stargazer(fit, type = "html", style = "aer",
          out = "Results/Regression table.html")


# .. Compute fitted values when predictors are 0 ####
IFF.preds <- c("tariff", "rCorruption", "pCorruption",
               "rPoorRegulation", "pPoorRegulation")

coef_illicit <- coef(fit)
for (v in 1:length(coef_illicit)){
  if (!(names(coef_illicit)[v] %in% IFF.preds)){
    coef_illicit[v] <- 0
  }
}
coef_illicit
panel$fitted_IFF <- as.numeric(exp(model.matrix(fit) %*% coef_illicit))
mean(panel$fitted_IFF)
# 0.9412347

coef_licit <- coef(fit)
for (v in 1:length(coef_licit)){
  if ((names(coef_licit)[v] %in% IFF.preds)){
    coef_licit[v] <- 0
  }
}
coef_licit
panel$fitted_nonIFF <- as.numeric(exp(model.matrix(fit) %*% coef_licit))
mean(panel$fitted_nonIFF)
# 1.837073

mean(exp(log(exp(model.matrix(fit) %*% coef_licit)) + 
      log(exp(model.matrix(fit) %*% coef_illicit))))
# 1.734554

round(mean(exp(log(exp(model.matrix(fit) %*% coef_licit)) + 
           log(exp(model.matrix(fit) %*% coef_illicit)))), 10) ==
  round(mean(exp(fitted(fit))), 10)
# TRUE

rm(coef_licit, coef_illicit, d, v, IFF.preds)


# .. Compute FOB imports ####
summary(panel$fitted_IFF)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.09014 0.89222 0.94132 0.94123 0.99291 1.07731 
summary(panel$fitted_nonIFF)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.000    0.828    1.140    1.837    1.597 5533.559 

panel <- panel %>%
  mutate(FOB_Import = Import_value / fitted_nonIFF,
         pFOB_Import = pImport_value / fitted_nonIFF,
         FOB_Import_IFF = Import_value / fitted_IFF)


# .. Estimate fixed effects regression ####
panel <- panel %>%
  mutate(M_dist = abs(log(FOB_Import/pNetExport_value))) %>%
  filter(is.finite(M_dist)) %>%
  mutate(X_dist = abs(log(pFOB_Import/NetExport_value))) %>%
  filter(is.finite(X_dist))
nrow(panel)
# 2887762

panel <- panel %>%
  mutate_at(vars(reporter.ISO, partner.ISO, year),
            funs(as.factor(.)))

FE_M.out <- felm(M_dist ~ 0| reporter.ISO + partner.ISO + year,
                 data = panel)
FE_M <- getfe(FE_M.out, se = T)
save(FE_M, file = "Results/FE_M.Rdata")

FE_X.out <- felm(X_dist ~ 0| reporter.ISO + partner.ISO + year,
                 data = panel)
FE_X <- getfe(FE_X.out, se = T)
save(FE_X, file = "Results/FE_X.Rdata")

load("Results/FE_M.Rdata")
load("Results/FE_X.Rdata")

FE_M <- FE_M %>%
  group_by(fe) %>%
  mutate(min = min(effect)) %>%
  ungroup()

FE_M$sigma <- pi/2*(FE_M$effect - (FE_M$min + 2*FE_M$se))
attr(FE_M$sigma, "extra") <- NULL

FE_X <- FE_X %>%
  group_by(fe) %>%
  mutate(min = min(effect)) %>%
  ungroup()

FE_X$sigma <- pi/2*(FE_X$effect - (FE_X$min + 2*FE_X$se))
attr(FE_X$sigma, "extra") <- NULL

panel <- panel %>%
  mutate_at(vars(reporter.ISO, partner.ISO, year),
            funs(as.character(.)))


# .. Harmonization procedure ####
panel <- left_join(panel, FE_M %>% 
                     filter(fe == "reporter.ISO") %>%
                     select(idx, sigma) %>%
                     mutate(idx = as.character(idx)),
                   by = c("reporter.ISO" = "idx")) %>%
  rename(rSigma_M = sigma)

panel <- left_join(panel, FE_M %>% 
                     filter(fe == "partner.ISO") %>%
                     select(idx, sigma) %>%
                     mutate(idx = as.character(idx)),
                   by = c("partner.ISO" = "idx")) %>%
  rename(pSigma_M = sigma)

panel <- left_join(panel, FE_X %>% 
                     filter(fe == "reporter.ISO") %>%
                     select(idx, sigma) %>%
                     mutate(idx = as.character(idx)),
                   by = c("reporter.ISO" = "idx")) %>%
  rename(rSigma_X = sigma)

panel <- left_join(panel, FE_X %>% 
                     filter(fe == "partner.ISO") %>%
                     select(idx, sigma) %>%
                     mutate(idx = as.character(idx)),
                   by = c("partner.ISO" = "idx")) %>%
  rename(pSigma_X = sigma)

panel <- panel %>%
  mutate(w_r_M = (exp(rSigma_M^2)*(exp(rSigma_M^2) - 1))/
           (exp(rSigma_M^2)*(exp(rSigma_M^2)- 1) + 
              exp(pSigma_M^2)*(exp(pSigma_M^2) - 1)),
         w_p_M = (exp(pSigma_M^2)*(exp(pSigma_M^2) - 1))/
           (exp(rSigma_M^2)*(exp(rSigma_M^2)- 1) + 
              exp(pSigma_M^2)*(exp(pSigma_M^2) - 1)))
summary(panel$w_r_M)
summary(panel$w_p_M)

panel <- panel %>%
  mutate(w_r_X = (exp(rSigma_X^2)*(exp(rSigma_X^2) - 1))/
           (exp(rSigma_X^2)*(exp(rSigma_X^2)- 1) + 
              exp(pSigma_X^2)*(exp(pSigma_X^2) - 1)),
         w_p_X = (exp(pSigma_X^2)*(exp(pSigma_X^2) - 1))/
           (exp(rSigma_X^2)*(exp(rSigma_X^2)- 1) + 
              exp(pSigma_X^2)*(exp(pSigma_X^2) - 1)))
summary(panel$w_r_X)
summary(panel$w_p_X)

ggplot(data = panel %>%
         select(starts_with("w_r") | starts_with("w_p")) %>%
         melt(),
       aes(x = value, y = variable)) +
  geom_density_ridges()

panel %>%
  select(starts_with("w_r") | starts_with("w_p")) %>%
  melt() %>%
  count(variable)

panel <- panel %>%
  mutate(w_M = w_r_M + w_p_M,
         w_X = w_r_X + w_p_X)
summary(panel$w_M)
summary(panel$w_X)

panel <- panel %>%
  mutate(RV_M = w_r_M*FOB_Import + w_p_M*pNetExport_value,
         RV_X = w_p_X*pFOB_Import + w_r_X*NetExport_value)


# .. Compute IFF ####
panel <- panel %>%
  mutate(Imp_IFF = FOB_Import_IFF - RV_M,
         Exp_IFF = RV_X - NetExport_value)
summary(panel$Imp_IFF)
summary(panel$Exp_IFF)

ggplot(data = panel %>%
         select(c(Imp_IFF, Exp_IFF)) %>%
         melt(),
       aes(x = value, y = variable)) +
  geom_density_ridges()

panel %>%
  filter(duplicated(panel$id)) %>% nrow
# 0
rm(fit, FE_M, FE_X, FE_M.out, FE_X.out)

save(panel, file = "Results/panel_results.Rdata")
