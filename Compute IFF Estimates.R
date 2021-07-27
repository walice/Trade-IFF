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
# .. Estimate fixed effects regression (robustness check)
# .. Harmonization procedure
# .. Harmonization procedure (robustness check)
# .. Compute IFF
# .. Tariff Evasion Analysis



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
  mutate(ratio_CIF = Import_value / pNetExport_value,
         ratio_CIF_mirror = pImport_value / NetExport_value) %>%
  filter(is.finite(ratio_CIF)) %>%
  filter(is.finite(ratio_CIF_mirror))
nrow(panel)
# 5762590

summary(panel$ratio_CIF)
hist(panel$ratio_CIF)

summary(panel$ratio_CIF_mirror)
hist(panel$ratio_CIF_mirror)

panel <- panel %>%
  mutate(ln.ratio_CIF = log(ratio_CIF),
         ln.ratio_CIF_mirror = log(ratio_CIF_mirror)) %>%
  filter(is.finite(ln.ratio_CIF)) %>%
  filter(is.finite(ln.ratio_CIF_mirror))
nrow(panel)
# 4821618

summary(panel$year)
panel <- panel %>% 
  group_by(reporter.ISO, partner.ISO, commodity.code) %>%
  mutate(ln.ratio_CIF_lag = dplyr::lag(ln.ratio_CIF, n = 1),
         ln.ratio_CIF_lag_mirror = dplyr::lag(ln.ratio_CIF_mirror, n = 1)) %>%
  ungroup() %>%
  filter(is.finite(ln.ratio_CIF_lag)) %>%
  filter(is.finite(ln.ratio_CIF_lag_mirror))
nrow(panel)
# 4341968
summary(panel$year)

panel <- panel %>% 
  group_by(reporter.ISO, partner.ISO, commodity.code) %>%
  mutate(Import_fut = dplyr::lead(Import_value, n = 1),
         pImport_fut = dplyr::lead(pImport_value, n = 1)) %>%
  ungroup() %>%
  filter(is.finite(Import_fut)) %>%
  filter(is.finite(pImport_fut))
nrow(panel)
# 3944277
summary(panel$year)

panel <- panel %>%
  mutate(FutImport_misrep = Import_fut / Import_value,
         ReExport_misrep = pReExport_value / Import_value,
         FutImport_misrep_mirror = pImport_fut / pImport_value,
         ReExport_misrep_mirror = ReExport_value / pImport_value) %>%
  filter(is.finite(FutImport_misrep) & is.finite(ReExport_misrep)) %>%
  filter(is.finite(FutImport_misrep_mirror) & is.finite(ReExport_misrep_mirror))
nrow(panel)
# 3944277

tariff_mirror <- panel %>%
  select(reporter.ISO, partner.ISO, commodity.code, year, tariff) %>%
  rename(tariff_mirror = tariff)

panel <- left_join(panel, tariff_mirror,
                   by = c("reporter.ISO" = "partner.ISO",
                          "partner.ISO" = "reporter.ISO",
                          "commodity.code" = "commodity.code",
                          "year" = "year"))
rm(tariff_mirror)

panel <- panel %>%
  filter(complete.cases(ratio_CIF, ln.ratio_CIF, ln.ratio_CIF_lag,
                        ratio_CIF_mirror, ln.ratio_CIF_mirror, ln.ratio_CIF_lag_mirror,
                        FutImport_misrep, ReExport_misrep,
                        FutImport_misrep_mirror, ReExport_misrep_mirror,
                        tariff, tariff_mirror,
                        dist, contig, rLandlocked, pLandlocked,
                        rCorruption, pCorruption, rRegulatory.qual, pRegulatory.qual))
nrow(panel)
# 2559456


# .. Variable transformation ####
panel <- panel %>%
  mutate(dist.sq = I(dist^2))

panel <- panel %>%
  mutate(rCorruption = 100 - rCorruption,
         pCorruption = 100 - pCorruption,
         rRegulatory.qual = 100 - rRegulatory.qual,
         pRegulatory.qual = 100 - pRegulatory.qual) %>%
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

summary(log(panel$FutImport_misrep_mirror))
# Fine to log
summary(log(panel$ReExport_misrep_mirror))
# Need inverse hyperbolic sine transformation
summary(log(panel$tariff_mirror))
# Need inverse hyperbolic sine transformation

panel <- panel %>%
  mutate(ln.FutImport_misrep = log(FutImport_misrep),
         ihs.ReExport_misrep = ihs(ReExport_misrep),
         ihs.tariff = ihs(tariff),
         ln.FutImport_misrep_mirror = log(FutImport_misrep_mirror),
         ihs.ReExport_misrep_mirror = ihs(ReExport_misrep_mirror),
         ihs.tariff_mirror = ihs(tariff_mirror))

hist(panel$ln.FutImport_misrep)
hist(panel$ihs.ReExport_misrep)
hist(panel$ihs.tariff)

hist(panel$ln.FutImport_misrep_mirror)
hist(panel$ihs.ReExport_misrep_mirror)
hist(panel$ihs.tariff_mirror)

rm(ihs)
nrow(panel)
# 2559456
save(panel, file = paste0(data.disk, "Data/Panel/panel_clean.Rdata"))



## ## ## ## ## ## ## ## ## ## ##
# REMOVE OUTLIERS           ####
## ## ## ## ## ## ## ## ## ## ##

# .. Truncate panel and remove CIF ratios greater than 100 ####
summary(panel$ratio_CIF)
summary(panel$ratio_CIF_mirror)

panel %>% filter(ratio_CIF > 10^4) %>% nrow
# 3849
panel %>% filter(ratio_CIF > 10^3) %>% nrow
# 15264

panel %>% filter(ratio_CIF_mirror > 10^4) %>% nrow
# 3849
panel %>% filter(ratio_CIF_mirror > 10^3) %>% nrow
# 15264

panel <- panel %>% 
  filter(ratio_CIF < 10^2) %>%
  filter(ratio_CIF_mirror < 10^2)
nrow(panel)
# 2451110


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
            rPoorRegulation + pPoorRegulation +
            factor(year),
          data = panel)
summary(fit)
mean(exp(fitted(fit)))
# 1.719294
max(panel$ratio_CIF)
# 99.99301
mean(panel$ratio_CIF)
# 3.098722


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
              rPoorRegulation + pPoorRegulation +
              factor(year),
            data = panel)
  panel$CD <- cooks.distance(fit)
}
nrow(panel)
# 2451110
summary(panel$CD)

Bonferonni.out <- outlierTest(fit, n.max = 10000)
obs <- as.numeric(names(Bonferonni.out[[1]]))
outliers <- panel[c(obs), ]
mean(outliers$ratio_CIF)
# 3.9108
mean(panel$ratio_CIF)
# 3.098722
panel <- panel[-c(obs),]
mean(panel$ratio_CIF)
# 3.097565

fit <- lm(ln.ratio_CIF ~ dist + dist.sq +
            contig + 
            rLandlocked +
            pLandlocked +
            ln.FutImport_misrep +
            ihs.ReExport_misrep +
            ln.ratio_CIF_lag +
            tariff + 
            rCorruption + pCorruption +
            rPoorRegulation + pPoorRegulation +
            factor(year),
          data = panel)
Bonferonni.out <- outlierTest(fit, n.max = 10000)
obs <- as.numeric(names(Bonferonni.out[[1]]))
outliers <- panel[c(obs), ]
mean(outliers$ratio_CIF)
# 6.117861
mean(panel$ratio_CIF)
# 3.097565
panel <- panel[-c(obs),]
mean(panel$ratio_CIF)
# 3.096567

fit <- lm(ln.ratio_CIF ~ dist + dist.sq +
            contig + 
            rLandlocked +
            pLandlocked +
            ln.FutImport_misrep +
            ihs.ReExport_misrep +
            ln.ratio_CIF_lag +
            tariff + 
            rCorruption + pCorruption +
            rPoorRegulation + pPoorRegulation +
            factor(year),
          data = panel)
Bonferonni.out <- outlierTest(fit, n.max = 10000)
obs <- as.numeric(names(Bonferonni.out[[1]]))
outliers <- panel[c(obs), ]
mean(outliers$ratio_CIF)
# 5.767423
mean(panel$ratio_CIF)
# 3.096567
panel <- panel[-c(obs),]
mean(panel$ratio_CIF)
# 3.09642

mean(panel$ratio_CIF_mirror)
# 3.097388

rm(Bonferonni.out, outliers, obs)
nrow(panel)
# 2446679
save(panel, file = paste0(data.disk, "Data/Panel/panel_nooutliers.Rdata"))



## ## ## ## ## ## ## ## ## ## ##
# MIS-INVOICING ESTIMATES   ####
## ## ## ## ## ## ## ## ## ## ##

# .. Estimate CIF rates ####
load(paste0(data.disk, "Data/Panel/panel_nooutliers.Rdata"))

nrow(panel)
# 2446679
max(panel$ratio_CIF)
# 99.99301
hist(panel$ratio_CIF)
summary(panel$ratio_CIF)
max(panel$ratio_CIF_mirror)
# 99.99301
hist(panel$ratio_CIF_mirror)
summary(panel$ratio_CIF_mirror)

fit <- lm(ln.ratio_CIF ~ dist + dist.sq +
            contig + 
            rLandlocked +
            pLandlocked +
            ln.FutImport_misrep +
            ihs.ReExport_misrep +
            ln.ratio_CIF_lag +
            tariff + 
            rCorruption + pCorruption +
            rPoorRegulation + pPoorRegulation +
            factor(year),
          data = panel)
summary(fit)
mean(exp(fitted(fit)))
# 1.730972

fit_mirror <- lm(ln.ratio_CIF_mirror ~ dist + dist.sq +
            contig + 
            rLandlocked +
            pLandlocked +
            ln.FutImport_misrep_mirror +
            ihs.ReExport_misrep_mirror +
            ln.ratio_CIF_lag_mirror +
            tariff_mirror + 
            rCorruption + pCorruption +
            rPoorRegulation + pPoorRegulation +
            factor(year),
          data = panel)
summary(fit_mirror)
mean(exp(fitted(fit_mirror)))
# 1.719214

d <- panel %>% distinct(dist) %>%
  mutate(y = coef(fit)["dist"]*dist + coef(fit)["dist.sq"]*dist^2)
ggplot(d, aes(x = dist, y = y)) + 
  geom_line()

kable(vif(fit), digits = 3, format = "rst")
kable(vif(fit_mirror), digits = 3, format = "rst")

save(fit, file = "Results/fit")
save(fit_mirror, file = "Results/fit_mirror")

stargazer(fit, type = "html", style = "aer",
          out = "Results/Regression table.html")
stargazer(fit_mirror, type = "html", style = "aer",
          out = "Results/Regression table.html")


# .. Compute fitted values when predictors are 0 ####
IFF.preds <- c("tariff", "tariff_mirror",
               "rCorruption", "pCorruption",
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
# 0.9756726

coef_illicit_mirror <- coef(fit_mirror)
for (v in 1:length(coef_illicit_mirror)){
  if (!(names(coef_illicit_mirror)[v] %in% IFF.preds)){
    coef_illicit_mirror[v] <- 0
  }
}
coef_illicit_mirror
panel$fitted_IFF_mirror <- as.numeric(exp(model.matrix(fit_mirror) %*% coef_illicit_mirror))
mean(panel$fitted_IFF_mirror)
# 0.9685555

coef_licit <- coef(fit)
for (v in 1:length(coef_licit)){
  if ((names(coef_licit)[v] %in% IFF.preds)){
    coef_licit[v] <- 0
  }
}
coef_licit
panel$fitted_nonIFF <- as.numeric(exp(model.matrix(fit) %*% coef_licit))
mean(panel$fitted_nonIFF)
# 1.76583

coef_licit_mirror <- coef(fit_mirror)
for (v in 1:length(coef_licit_mirror)){
  if ((names(coef_licit_mirror)[v] %in% IFF.preds)){
    coef_licit_mirror[v] <- 0
  }
}
coef_licit_mirror
panel$fitted_nonIFF_mirror <- as.numeric(exp(model.matrix(fit_mirror) %*% coef_licit_mirror))
mean(panel$fitted_nonIFF_mirror)
# 1.767022

mean(exp(log(exp(model.matrix(fit) %*% coef_licit)) + 
      log(exp(model.matrix(fit) %*% coef_illicit))))
# 1.730972

panel$fitted <- exp(fitted(fit))
mean(panel$fitted)
# 1.730972

round(mean(exp(log(exp(model.matrix(fit) %*% coef_licit)) + 
           log(exp(model.matrix(fit) %*% coef_illicit)))), 10) ==
  round(mean(exp(fitted(fit))), 10)
# TRUE

panel$resid <- exp(resid(fit))

panel$fitted_mirror <- exp(fitted(fit_mirror))
mean(panel$fitted_mirror)
# 1.719214

panel$resid_mirror <- exp(resid(fit_mirror))

rm(coef_licit, coef_illicit, coef_licit_mirror, coef_illicit_mirror,
   d, v, IFF.preds)


# .. Compute FOB imports ####
summary(panel$fitted)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.000    0.827    1.091    1.731    1.465 3464.136 
summary(panel$fitted_IFF)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.1130  0.9412  0.9824  0.9757  1.0082  1.0730 
summary(panel$fitted_nonIFF)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.000    0.855    1.117    1.766    1.494 3436.165  

noCIF <- c("BRA", "KHM", "CAN", "GIN", "PRY", 
           "ZAF", "UKR", "USA", "MLI", "TJK")

panel <- panel %>%
  mutate(FOB_Import = ifelse(reporter.ISO %in% noCIF,
                             Import_value, Import_value / fitted),
         pFOB_Import = ifelse(reporter.ISO %in% noCIF,
                              pImport_value, pImport_value / fitted_mirror),
         FOB_Import_stripresid = ifelse(reporter.ISO %in% noCIF,
                                        Import_value, Import_value / (fitted * resid)),
         pFOB_Import_stripresid = ifelse(reporter.ISO %in% noCIF,
                                         pImport_value, pImport_value / (fitted_mirror * resid_mirror)),
         FOB_Import_stripnonIFF = ifelse(reporter.ISO %in% noCIF, 
                                         Import_value, Import_value / fitted_nonIFF),
         pFOB_Import_stripnonIFF = ifelse(reporter.ISO %in% noCIF,
                                          pImport_value, pImport_value / fitted_nonIFF_mirror),
         FOB_Import_stripnonIFFresid = ifelse(reporter.ISO %in% noCIF, 
                                              Import_value, Import_value / (fitted_nonIFF * resid)),
         pFOB_Import_stripnonIFFresid = ifelse(reporter.ISO %in% noCIF,
                                               pImport_value, pImport_value / (fitted_nonIFF_mirror * resid_mirror)),
         FOB_Import_stripIFF = ifelse(reporter.ISO %in% noCIF,
                                      Import_value, Import_value / fitted_IFF),
         pFOB_Import_stripIFF = ifelse(reporter.ISO %in% noCIF,
                                       pImport_value, pImport_value / fitted_IFF_mirror))

# Checks
check <- panel %>% filter(!(reporter.ISO %in% noCIF))

# V_{ij}^{M, FOB} = V_{ji}^X * exp(epsilonhat)
sum(round(check$FOB_Import) == 
      round(check$pNetExport_value * check$resid)) == nrow(check)
# TRUE

# V_{ij}^{M, FOB, resid} = V_{ji}^X
sum(round(check$FOB_Import_stripresid) == 
      round(check$pNetExport_value)) == nrow(check)
# TRUE

# V_{ij}^{M, FOB, nonIFF} = V_{ji}^X * exp(lambdahat) * exp(epsilonhat)
sum(round(check$FOB_Import_stripnonIFF, -2) ==
  round(check$pNetExport_value * 
          check$fitted_IFF * check$resid, -2)) == nrow(check)
# TRUE

# V_{ij}^{M, FOB, nonIFFresid} = V_{ji}^X * exp(lambdahat)
sum(round(check$FOB_Import_stripnonIFFresid, -3) ==
      round(check$pNetExport_value * 
              check$fitted_IFF, -3)) == nrow(check)
# TRUE

# V_{ij}^{M, FOB, IFF} = V_{ji}^X * exp(betahat) * exp(epsilonhat)
sum(round(check$FOB_Import_stripIFF, -3) ==
      round(check$pNetExport_value *
              check$fitted_nonIFF * check$resid, -3)) == nrow(check)
# TRUE

# V_{ji}^{M, FOB} = V_{ij}^X * exp(epsilonhat)
sum(round(check$pFOB_Import) == 
      round(check$NetExport_value * check$resid_mirror)) == nrow(check)
# TRUE

# V_{ji}^{M, FOB, resid} = V_{ij}^X
sum(round(check$pFOB_Import_stripresid) == 
      round(check$NetExport_value)) == nrow(check)
# TRUE

# V_{ji}^{M, FOB, nonIFF} = V_{ij}^X * exp(lambdahat) * exp(epsilonhat)
sum(round(check$pFOB_Import_stripnonIFF, -2) ==
      round(check$NetExport_value * 
              check$fitted_IFF_mirror * check$resid_mirror, -2)) == nrow(check)
# TRUE

# V_{ji}^{M, FOB, nonIFFresid} = V_{ij}^X * exp(lambdahat)
sum(round(check$pFOB_Import_stripnonIFFresid, -3) ==
      round(check$NetExport_value * 
              check$fitted_IFF_mirror, -3)) == nrow(check)
# TRUE

# V_{ji}^{M, FOB, IFF} = V_{ij}^X * exp(betahat) * exp(epsilonhat)
sum(round(check$pFOB_Import_stripIFF, -3) ==
      round(check$NetExport_value *
              check$fitted_nonIFF_mirror * check$resid_mirror, -3)) == nrow(check)
# TRUE

rm(check)


# .. Estimate fixed effects regression ####
panel <- panel %>%
  mutate(M_dist = abs(log(FOB_Import/pNetExport_value))) %>%
  filter(is.finite(M_dist)) %>%
  mutate(X_dist = abs(log(pFOB_Import/NetExport_value))) %>%
  filter(is.finite(X_dist)) %>%
  mutate(M_dist_rob = abs(log(FOB_Import_stripresid/pNetExport_value))) %>%
  filter(is.finite(M_dist_rob)) %>%
  mutate(X_dist_rob = abs(log(pFOB_Import_stripresid/NetExport_value))) %>%
  filter(is.finite(X_dist_rob))
nrow(panel)
# 2446679
# Doing the filtering on robust distances in the same chunk
# does not truncate the regular sample

FE_M.out <- felm(M_dist ~ 0| reporter.ISO + partner.ISO + year + commodity.code,
                 data = panel)
FE_M <- getfe(FE_M.out, se = T)
save(FE_M, file = "Results/FE_M.Rdata")

FE_X.out <- felm(X_dist ~ 0| reporter.ISO + partner.ISO + year + commodity.code,
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


# .. Estimate fixed effects regression (robustness check) ####
FE_M_rob.out <- felm(M_dist_rob ~ 0| reporter.ISO + partner.ISO + year + commodity.code,
                     data = panel)
FE_M_rob <- getfe(FE_M_rob.out, se = T)
save(FE_M_rob, file = "Results/FE_M_rob.Rdata")

FE_X_rob.out <- felm(X_dist_rob ~ 0| reporter.ISO + partner.ISO + year + commodity.code,
                 data = panel)
FE_X_rob <- getfe(FE_X_rob.out, se = T)
save(FE_X_rob, file = "Results/FE_X_rob.Rdata")

load("Results/FE_M_rob.Rdata")
load("Results/FE_X_rob.Rdata")

FE_M_rob <- FE_M_rob %>%
  group_by(fe) %>%
  mutate(min = min(effect)) %>%
  ungroup()

FE_M_rob$sigma <- pi/2*(FE_M_rob$effect - (FE_M_rob$min + 2*FE_M_rob$se))
attr(FE_M_rob$sigma, "extra") <- NULL

FE_X_rob <- FE_X_rob %>%
  group_by(fe) %>%
  mutate(min = min(effect)) %>%
  ungroup()

FE_X_rob$sigma <- pi/2*(FE_X_rob$effect - (FE_X_rob$min + 2*FE_X_rob$se))
attr(FE_X_rob$sigma, "extra") <- NULL


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
  mutate(w_r_M = (exp(pSigma_M^2)*(exp(pSigma_M^2) - 1))/
           (exp(rSigma_M^2)*(exp(rSigma_M^2)- 1) + 
              exp(pSigma_M^2)*(exp(pSigma_M^2) - 1)),
         w_p_M = (exp(rSigma_M^2)*(exp(rSigma_M^2) - 1))/
           (exp(rSigma_M^2)*(exp(rSigma_M^2)- 1) + 
              exp(pSigma_M^2)*(exp(pSigma_M^2) - 1)))
summary(panel$w_r_M)
summary(panel$w_p_M)

panel <- panel %>%
  mutate(w_r_X = (exp(pSigma_X^2)*(exp(pSigma_X^2) - 1))/
           (exp(rSigma_X^2)*(exp(rSigma_X^2)- 1) +
              exp(pSigma_X^2)*(exp(pSigma_X^2) - 1)),
         w_p_X = (exp(rSigma_X^2)*(exp(rSigma_X^2) - 1))/
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
         RV_X = w_r_X*NetExport_value + w_p_X*pFOB_Import)


# .. Harmonization procedure (robustness check) ####
panel <- left_join(panel, FE_M_rob %>% 
                     filter(fe == "reporter.ISO") %>%
                     select(idx, sigma) %>%
                     mutate(idx = as.character(idx)),
                   by = c("reporter.ISO" = "idx")) %>%
  rename(rSigma_M_rob = sigma)

panel <- left_join(panel, FE_M_rob %>% 
                     filter(fe == "partner.ISO") %>%
                     select(idx, sigma) %>%
                     mutate(idx = as.character(idx)),
                   by = c("partner.ISO" = "idx")) %>%
  rename(pSigma_M_rob = sigma)

panel <- left_join(panel, FE_X_rob %>% 
                     filter(fe == "reporter.ISO") %>%
                     select(idx, sigma) %>%
                     mutate(idx = as.character(idx)),
                   by = c("reporter.ISO" = "idx")) %>%
  rename(rSigma_X_rob = sigma)

panel <- left_join(panel, FE_X_rob %>% 
                     filter(fe == "partner.ISO") %>%
                     select(idx, sigma) %>%
                     mutate(idx = as.character(idx)),
                   by = c("partner.ISO" = "idx")) %>%
  rename(pSigma_X_rob = sigma)

panel <- panel %>%
  mutate(w_r_M_rob = (exp(pSigma_M_rob^2)*(exp(pSigma_M_rob^2) - 1))/
           (exp(rSigma_M_rob^2)*(exp(rSigma_M_rob^2)- 1) + 
              exp(pSigma_M_rob^2)*(exp(pSigma_M_rob^2) - 1)),
         w_p_M_rob = (exp(rSigma_M_rob^2)*(exp(rSigma_M_rob^2) - 1))/
           (exp(rSigma_M_rob^2)*(exp(rSigma_M_rob^2)- 1) + 
              exp(pSigma_M_rob^2)*(exp(pSigma_M_rob^2) - 1)))
summary(panel$w_r_M_rob)
summary(panel$w_p_M_rob)

panel <- panel %>%
  mutate(w_r_X_rob = (exp(pSigma_X_rob^2)*(exp(pSigma_X_rob^2) - 1))/
           (exp(rSigma_X_rob^2)*(exp(rSigma_X_rob^2)- 1) + 
              exp(pSigma_X_rob^2)*(exp(pSigma_X_rob^2) - 1)),
         w_p_X_rob = (exp(rSigma_X_rob^2)*(exp(rSigma_X_rob^2) - 1))/
           (exp(rSigma_X_rob^2)*(exp(rSigma_X_rob^2)- 1) + 
              exp(pSigma_X_rob^2)*(exp(pSigma_X_rob^2) - 1)))
summary(panel$w_r_X_rob)
summary(panel$w_p_X_rob)

ggplot(data = panel %>%
         select(starts_with("w_r") | starts_with("w_p")) %>%
         select(ends_with("rob")) %>%
         melt(),
       aes(x = value, y = variable)) +
  geom_density_ridges()

panel %>%
  select(starts_with("w_r") | starts_with("w_p")) %>%
  select(ends_with("rob")) %>%
  melt() %>%
  count(variable)

panel <- panel %>%
  mutate(w_M_rob = w_r_M_rob + w_p_M_rob,
         w_X_rob = w_r_X_rob + w_p_X_rob)
summary(panel$w_M_rob)
summary(panel$w_X_rob)

panel <- panel %>%
  mutate(RV_M_rob = w_r_M_rob*FOB_Import_stripresid + w_p_M_rob*pNetExport_value,
         RV_X_rob = w_r_X_rob*NetExport_value + w_p_X_rob*pFOB_Import_stripresid)


# .. Compute IFF ####
panel <- panel %>%
  mutate(Imp_IFF = FOB_Import_stripnonIFF - RV_M,
         Exp_IFF = RV_X - NetExport_value,
         Imp_IFF_rob = FOB_Import_stripnonIFFresid - RV_M_rob,
         Exp_IFF_rob = RV_X_rob - NetExport_value)
summary(panel$Imp_IFF)
summary(panel$Exp_IFF)
summary(panel$Imp_IFF_rob)
summary(panel$Exp_IFF_rob)

ggplot(data = panel %>%
         select(c(Imp_IFF, Exp_IFF)) %>%
         melt(),
       aes(x = value, y = variable)) +
  geom_density_ridges()

panel %>%
  filter(Imp_IFF > 2*10^10) %>%
  distinct(reporter.ISO)
# 1 GBR         
# 2 CHN         
# 3 USA         
# 4 CZE         
# 5 AGO         
# 6 IRL         
# 7 CHE         
# 8 DNK         
# 9 HKG         
# 10 THA  

panel %>%
  filter(Exp_IFF > 2*10^10) %>%
  distinct(reporter.ISO)
# 1 PER         
# 2 TTO         
# 3 YEM         
# 4 ARG         
# 5 BOL         
# 6 CIV         
# 7 TZA         
# 8 COG 

ggplot(data = panel %>%
         select(c(RV_M, RV_X)) %>%
         melt(),
       aes(x = value, y = variable)) +
  geom_density_ridges()

ggplot(data = panel %>%
         select(c(RV_M, FOB_Import_stripnonIFF, Imp_IFF)) %>%
         melt() %>%
         filter(value < 10^6 & value > -10^6),
       aes(x = value, y = variable)) +
  geom_density_ridges()

ggplot(data = panel %>%
         select(c(RV_X, NetExport_value, Exp_IFF)) %>%
         melt() %>%
         filter(value < 10^6 & value > -10^6),
       aes(x = value, y = variable)) +
  geom_density_ridges()

panel %>%
  filter(duplicated(panel$id)) %>% nrow
# 0
rm(fit, fit_mirror, noCIF,
   FE_M, FE_X, FE_M.out, FE_X.out,
   FE_M_rob, FE_X_rob, FE_M_rob.out, FE_X_rob.out)

save(panel, file = "Results/panel_results.Rdata")



## ## ## ## ## ## ## ## ## ## ##
# TARIFF EVASION ANALYSIS   ####
## ## ## ## ## ## ## ## ## ## ##

load("Results/panel_results.Rdata")
load(paste0(data.disk, "Data/WDI/WDI.Rdata"))

# Import under-invoicing
Imp.under <- panel %>%
  select(reporter, reporter.ISO, rIncome, rRegion, rDev, rHDI,
         partner, partner.ISO, pIncome, pRegion, pDev, pHDI,
         tariff, Imp_IFF,
         year, commodity.code,
         Import_value, GDP) %>%
  mutate(Imp_IFF_GDP = Imp_IFF / GDP) %>%
  filter(Imp_IFF < 0) %>%
  filter(tariff < 100) %>%
  left_join(WDI %>% select(ISO3166.3, GNPpc, year),
            by = c("reporter.ISO" = "ISO3166.3",
                   "year" = "year"))

fit1 <- felm(abs(Imp_IFF_GDP) ~ tariff + Import_value + GDP |
               commodity.code + reporter.ISO + partner.ISO,
             data = Imp.under)
summary(fit1)

stargazer(fit1, type = "html", style = "aer",
          out = "Results/Import under-invoicing regression.html")

# Interact development status
# Imp.under <- Imp.under %>%
#   mutate(rIncome = factor(rIncome,
#                           levels = c("LIC", "LMC", "UMC", "HIC")))
Imp.under <- Imp.under %>%
  mutate(rIncome = as.factor(rIncome))
levels(Imp.under$rIncome)
fit2 <- lm(abs(Imp_IFF) ~ tariff*rIncome + Import_value + GDP,
           data = Imp.under)
summary(fit2)

stargazer(fit2, type = "html", style = "aer",
          out = "Results/Tariff evasion regression.html")

fit3 <- lm(abs(Imp_IFF_GDP) ~ tariff*rIncome + Import_value + GDP,
           data = Imp.under)
summary(fit3)

# Import over-invoicing
Imp.over <- panel %>%
  select(reporter, reporter.ISO, rIncome, rRegion, rDev, rHDI,
         partner, partner.ISO, pIncome, pRegion, pDev, pHDI,
         tariff, Imp_IFF,
         year, commodity.code,
         Import_value, GDP) %>%
  mutate(Imp_IFF_GDP = Imp_IFF / GDP) %>%
  filter(Imp_IFF > 0) %>%
  filter(tariff < 100) %>%
  left_join(WDI %>% select(ISO3166.3, GNPpc, year),
            by = c("reporter.ISO" = "ISO3166.3",
                   "year" = "year"))

fit4 <- felm(Imp_IFF_GDP ~ tariff + Import_value + GDP |
               commodity.code + reporter.ISO + partner.ISO,
             data = Imp.over)
summary(fit4)

fit5 <- lm(Imp_IFF_GDP ~ tariff*rIncome + Import_value + GDP,
           data = Imp.over)
summary(fit5)

fit6 <- felm(Imp_IFF ~ tariff + Import_value + GDP |
               commodity.code + reporter.ISO + partner.ISO,
             data = Imp.over)
summary(fit6)

fit7 <- felm(Imp_IFF_GDP ~ tariff*rIncome + Import_value + GDP |
               commodity.code,
             data = Imp.over)
summary(fit7)

fit8 <- lm(Imp_IFF ~ tariff*rIncome + Import_value + GDP,
           data = Imp.over)
summary(fit8)
