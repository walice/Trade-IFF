# Compute IFF Estimates
# Alice Lepissier
# alice.lepissier@gmail.com
# Prepared for UNECA

## ## ## ## ## ## ## ## ## ## ##
# INDEX                     ####
## ## ## ## ## ## ## ## ## ## ##
# Preamble
# Create Variables
# .. Implement weighting by quantities (not used)
# .. Create dependent variable and predictors
# .. Variable transformation
# Remove Outliers
# .. Truncate panel and remove CIF ratios greater than 10,000
# .. Estimate regression
# .. Identify and remove outliers
# Low Estimates
# .. Censor the data-set to get lower-bounds
# .. Estimate CIF rates
# .. Compute fitted values when predictors are 0
# .. Compute FOB imports
# .. Estimate fixed effects regression for import discrepancy
# .. Harmonization procedure for import discrepancy
# .. Estimate fixed effects regression for export discrepancy
# .. Harmonization procedure for export discrepancy
# .. Compute IFF
# High Estimates
# .. Estimate CIF rates
# .. Compute fitted values when predictors are 0
# .. Compute FOB imports
# .. Estimate fixed effects regression for import discrepancy
# .. Harmonization procedure for import discrepancy
# .. Estimate fixed effects regression for export discrepancy
# .. Harmonization procedure for export discrepancy
# .. Compute IFF
# Aggregate by Destination
# .. Aggregate results using Gross Excluding Reversals
# .. Aggregate results using Net Aggregation
# Aggregate by Sector
# .. Aggregate results using Gross Excluding Reversals
# .. Aggregate results using Net Aggregation
# Pilot Country Results



## ## ## ## ## ## ## ## ## ## ##
# PREAMBLE                  ####
## ## ## ## ## ## ## ## ## ## ##

#setwd("C:/cloudstorage/googledrive/Projects/UN Consultancy/Illicit Financial Flows/IFF estimates") # Alice work
#setwd("D:/Google Drive/Projects/UN Consultancy/Illicit Financial Flows/IFF estimates") # Alice laptop
setwd("/home/alice/IFFe/") # Virtual server
library(car)
library(kableExtra)
library(lfe)
library(reshape2)
library(scales)
library(tidyverse)
options(scipen = 999)



## ## ## ## ## ## ## ## ## ## ##
# CREATE VARIABLES          ####
## ## ## ## ## ## ## ## ## ## ##

#source("Scripts/Data Preparation.R")
load("Data/Panel/panel.Rdata")

length(unique(panel$id)) == nrow(panel)
# TRUE, 20864314 obs

panel %>%
  filter(pExport_value == 0 & is.na(pReExport_value)) %>% nrow
# There is no case where a mirrored Re-Export value is missing while
# the mirrored Export value exists.

panel %>%
  filter(pExport_quantity == 0 & is.na(pReExport_quantity)) %>% nrow
# There is no case where a mirrored Re-Export quantity is missing while
# the mirrored Export quantity exists.

panel %>%
  filter(pExport_weight == 0 & is.na(pReExport_weight)) %>% nrow
# There is no case where a mirrored Re-Export weight is missing while
# the mirrored Export weight exists.


# .. Implement weighting by quantities (not used) ####
panel <- panel %>%
  mutate(NetExport_value = Export_value - ReExport_value,
         pNetExport_value = pExport_value - pReExport_value,
         pNetExport_quantity = pExport_quantity - pReExport_quantity,
         pNetExport_weight = pExport_weight - pReExport_weight)

summary(panel$Import_quantity)
summary(panel$pNetExport_quantity)
summary(panel$Import_weight)
summary(panel$pNetExport_weight)

panel <- panel %>%
  mutate(Import_U = Import_value / Import_quantity,
         pNetExport_U = pNetExport_value / pNetExport_quantity)
summary(panel$Import_U)
summary(panel$pNetExport_U)
panel %>% filter(is.finite(Import_U) & is.finite(pNetExport_U)) %>% nrow
# 0

panel <- panel %>%
  mutate(Import_U = Import_value / Import_weight,
         pNetExport_U = pNetExport_value / pNetExport_weight)
summary(panel$Import_U)
summary(panel$pNetExport_U)
panel %>% filter(is.finite(Import_U) & is.finite(pNetExport_U)) %>% nrow
# 0

panel <- panel %>%
  mutate(Import_w = ifelse(pNetExport_quantity < Import_quantity,
                           Import_value - (Import_quantity - pNetExport_quantity) * Import_U,
                           Import_value),
         pNetExport_w = ifelse(pNetExport_quantity > Import_quantity,
                               pNetExport_value - (pNetExport_quantity - Import_quantity) * pNetExport_U,
                               pNetExport_value))

summary(panel$Import_w)
# Inf
summary(panel$pNetExport_w)
# Inf

# It is not possible to implement the weighting by quantities.
# There is simply not enough data.

panel <- panel %>%
  select(-ends_with("quantity"), -ends_with("weight"), -ends_with("_U"), -ends_with("_w"))


# .. Create dependent variables and predictors ####
# panel <- panel %>% 
#   mutate(ratio_CIF_r = Import_value / pNetExport_value,
#          ratio_CIF_p = pImport_value / NetExport_value) %>%
#   filter(is.finite(ratio_CIF_r) & is.finite(ratio_CIF_p))
# nrow(panel)
# # 5153343
panel <- panel %>% 
  mutate(ratio_CIF = Import_value / pNetExport_value) %>%
  filter(is.finite(ratio_CIF))
nrow(panel)
# 8080386

# summary(panel$ratio_CIF_r)
# hist(panel$ratio_CIF_r)
# summary(panel$ratio_CIF_p)
# hist(panel$ratio_CIF_p)
summary(panel$ratio_CIF)
hist(panel$ratio_CIF)

# panel <- panel %>%
#   mutate(ln.ratio_CIF_r = log(ratio_CIF_r),
#          ln.ratio_CIF_p = log(ratio_CIF_p)) %>%
#   filter(is.finite(ln.ratio_CIF_r) & is.finite(ln.ratio_CIF_p))
# nrow(panel)
# # 4298652
panel <- panel %>%
  mutate(ln.ratio_CIF = log(ratio_CIF)) %>%
  filter(is.finite(ln.ratio_CIF))
nrow(panel)
# 7605946

# summary(panel$year)
# panel <- panel %>% 
#   group_by(reporter.ISO, partner.ISO, commodity.code) %>%
#   mutate(ln.ratio_CIF_r_lag = dplyr::lag(ln.ratio_CIF_r, n = 1),
#          ln.ratio_CIF_p_lag = dplyr::lag(ln.ratio_CIF_p, n = 1)) %>%
#   ungroup() %>%
#   filter(is.finite(ln.ratio_CIF_r_lag) & is.finite(ln.ratio_CIF_p_lag))
# nrow(panel)
# # 3841854
# summary(panel$year)
summary(panel$year)
panel <- panel %>% 
  group_by(reporter.ISO, partner.ISO, commodity.code) %>%
  mutate(ln.ratio_CIF_lag = dplyr::lag(ln.ratio_CIF, n = 1)) %>%
  ungroup() %>%
  filter(is.finite(ln.ratio_CIF_lag))
nrow(panel)
# 6812396
summary(panel$year)

# panel <- panel %>% 
#   group_by(reporter.ISO, partner.ISO, commodity.code) %>%
#   mutate(Import_fut = dplyr::lead(Import_value, n = 1),
#          NetExport_fut = dplyr::lead(NetExport_value, n = 1)) %>%
#   ungroup() %>%
#   filter(is.finite(Import_fut) & is.finite(NetExport_fut))
# nrow(panel)
# # 3462595
# summary(panel$year)
panel <- panel %>% 
  group_by(reporter.ISO, partner.ISO, commodity.code) %>%
  mutate(Import_fut = dplyr::lead(Import_value, n = 1)) %>%
  ungroup() %>%
  filter(is.finite(Import_fut))
nrow(panel)
# 6147496
summary(panel$year)

# panel <- panel %>%
#   mutate(FutImport_misrep = Import_fut / Import_value,
#          ReExport_misrep_r = pReExport_value / Import_value,
#          FutExport_misrep = NetExport_fut / NetExport_value,
#          ReExport_misrep_p = ReExport_value / pImport_value) %>%
#   filter(is.finite(FutImport_misrep) & is.finite(ReExport_misrep_r) & is.finite(FutExport_misrep) & is.finite(ReExport_misrep_p))
# nrow(panel)
# # 3462595
panel <- panel %>%
  mutate(FutImport_misrep = Import_fut / Import_value,
         ReExport_misrep = pReExport_value / Import_value) %>%
  filter(is.finite(FutImport_misrep) & is.finite(ReExport_misrep))
nrow(panel)
# 6147496

# panel <- panel %>%
#   filter(complete.cases(ratio_CIF_r, ln.ratio_CIF_r, ln.ratio_CIF_r_lag,
#                         dist, contig, rLandlocked, pLandlocked,
#                         FutImport_misrep, ReExport_misrep_r, 
#                         tariff, rCorruption, pCorruption, rRegulatory.qual, pRegulatory.qual)) %>%
#   filter(complete.cases(ratio_CIF_p, ln.ratio_CIF_p, ln.ratio_CIF_p_lag,
#                         dist, contig, rLandlocked, pLandlocked,
#                         FutExport_misrep, ReExport_misrep_p, 
#                         tariff, rCorruption, pCorruption, rRegulatory.qual, pRegulatory.qual))
# nrow(panel)
# # 1813592
panel <- panel %>%
  filter(complete.cases(ratio_CIF, ln.ratio_CIF, ln.ratio_CIF_lag,
                        dist, contig, rLandlocked, pLandlocked,
                        FutImport_misrep, ReExport_misrep, 
                        tariff, rCorruption, pCorruption, rRegulatory.qual, pRegulatory.qual))
nrow(panel)
# 3662664


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

# hist(panel$ln.ratio_CIF_r)
# hist(panel$ln.ratio_CIF_p)
hist(panel$ln.ratio_CIF)
hist(panel$FutImport_misrep)
hist(log(panel$FutImport_misrep))
# hist(panel$ReExport_misrep_r)
# hist(log(panel$ReExport_misrep_r))
hist(panel$ReExport_misrep)
hist(log(panel$ReExport_misrep))
# hist(panel$FutExport_misrep)
# hist(log(panel$FutExport_misrep))
# hist(panel$ReExport_misrep_p)
# hist(log(panel$ReExport_misrep_p))
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
# summary(log(panel$ReExport_misrep_r))
# # Need inverse hyperbolic sine transformation
summary(log(panel$ReExport_misrep))
# Need inverse hyperbolic sine transformation
# summary(log(panel$FutExport_misrep))
# # Fine to log
# summary(log(panel$ReExport_misrep_p))
# # Need inverse hyperbolic sine transformation
summary(log(panel$tariff))
# Need inverse hyperbolic sine transformation

# panel <- panel %>%
#   mutate(ln.FutImport_misrep = log(FutImport_misrep),
#          ihs.ReExport_misrep_r = ihs(ReExport_misrep_r),
#          ln.FutExport_misrep = log(FutExport_misrep),
#          ihs.ReExport_misrep_p = ihs(ReExport_misrep_p),
#          ihs.tariff = ihs(tariff))
panel <- panel %>%
  mutate(ln.FutImport_misrep = log(FutImport_misrep),
         ihs.ReExport_misrep = ihs(ReExport_misrep),
         ihs.tariff = ihs(tariff))
hist(panel$ln.FutImport_misrep)
# hist(panel$ihs.ReExport_misrep_r)
hist(panel$ihs.ReExport_misrep)
# hist(panel$ln.FutExport_misrep)
# hist(panel$ihs.ReExport_misrep_p)
hist(panel$ihs.tariff)

rm(ihs)
nrow(panel)
# 3662664
save(panel, file = "Data/Panel/panel_clean.Rdata")



## ## ## ## ## ## ## ## ## ## ##
# REMOVE OUTLIERS           ####
## ## ## ## ## ## ## ## ## ## ##

# .. Truncate panel and remove CIF ratios greater than 10,000 (not used) ####
# summary(panel$ratio_CIF_r)
# summary(panel$ratio_CIF_p)
summary(panel$ratio_CIF)

# panel <- panel %>%
#   filter(ratio_CIF_r < 10^4) %>%
#   filter(ratio_CIF_p < 10^4)
# nrow(panel)
# 3323416
# panel <- panel %>%
#   filter(ratio_CIF < 10^4)
# nrow(panel)
# # 3657496
# panel <- panel %>%
#   filter(ratio_CIF < 10^3)
# nrow(panel)
# # 3641071

# panel %>% filter(ratio_CIF_r > 10^3) %>% nrow
# # 9575
# panel %>% filter(ratio_CIF_p > 10^3) %>% nrow
# # 12578

panel %>% filter(ratio_CIF > 10^3) %>% nrow
# 21585


# .. Estimate regression ####
# fit_r <- lm(ln.ratio_CIF_r ~ dist + dist.sq +
#               contig + 
#               rLandlocked +
#               pLandlocked +
#               ln.FutImport_misrep +
#               ihs.ReExport_misrep_r +
#               ln.ratio_CIF_r_lag +
#               tariff + 
#               rCorruption + pCorruption +
#               rPoorRegulation + pPoorRegulation,
#             data = panel)
# summary(fit_r)
# mean(exp(fitted(fit_r)))
# # 3.631353
# max(panel$ratio_CIF_r)
# # 2354224577
# mean(panel$ratio_CIF_r)
# # 4696.118
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
# 3.236832
max(panel$ratio_CIF)
# 2354224577
mean(panel$ratio_CIF)
# 2908.841

# fit_p <- lm(ln.ratio_CIF_p ~ dist + dist.sq +
#               contig + 
#               rLandlocked +
#               pLandlocked +
#               ln.FutExport_misrep +
#               ihs.ReExport_misrep_p +
#               ln.ratio_CIF_p_lag +
#               tariff + 
#               rCorruption + pCorruption +
#               rPoorRegulation + pPoorRegulation,
#             data = panel)
# summary(fit_p)
# mean(exp(fitted(fit_p)))
# # 7.405753
# max(panel$ratio_CIF_p)
# # 1124495105
# mean(panel$ratio_CIF_p)
# # 4065.844


# .. Identify and remove outliers ####
# panel$CD <- cooks.distance(fit_r)
panel$CD <- cooks.distance(fit)
summary(panel$CD)

# while(max(panel$CD) > 2){
#   panel <- panel %>%
#     filter(CD <= 2)
#   fit_r <- lm(ln.ratio_CIF_r ~ dist + dist.sq +
#                 contig + 
#                 rLandlocked +
#                 pLandlocked +
#                 ln.FutImport_misrep +
#                 ihs.ReExport_misrep_r +
#                 ln.ratio_CIF_r_lag +
#                 tariff + 
#                 rCorruption + pCorruption +
#                 rPoorRegulation + pPoorRegulation,
#               data = panel)
#   panel$CD <- cooks.distance(fit_r)
# }
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
# 3662664
summary(panel$CD)

# panel$CD <- cooks.distance(fit_p)
# summary(panel$CD)
# 
# while(max(panel$CD) > 2){
#   panel <- panel %>%
#     filter(CD <= 2)
#   fit_p <- lm(ln.ratio_CIF_p ~ dist + dist.sq +
#                 contig + 
#                 rLandlocked +
#                 pLandlocked +
#                 ln.FutExport_misrep +
#                 ihs.ReExport_misrep_p +
#                 ln.ratio_CIF_p_lag +
#                 tariff + 
#                 rCorruption + pCorruption +
#                 rPoorRegulation + pPoorRegulation,
#               data = panel)
#   panel$CD <- cooks.distance(fit_p)
# }
# nrow(panel)
# # 1813592
# summary(panel$CD)

# Bonferonni.out <- outlierTest(fit_r, n.max = 10000)
Bonferonni.out <- outlierTest(fit, n.max = 10000)
obs <- as.numeric(names(Bonferonni.out[[1]]))
outliers <- panel[c(obs), ]
# mean(outliers$ratio_CIF_r)
mean(outliers$ratio_CIF)
# 2090821
# mean(panel$ratio_CIF_r)
mean(panel$ratio_CIF)
# 2908.841
panel <- panel[-c(obs),]
# mean(panel$ratio_CIF_r)
mean(panel$ratio_CIF)
# 105.5524

# Bonferonni.out <- outlierTest(fit_p, n.max = 10000)
# obs <- as.numeric(names(Bonferonni.out[[1]]))
# outliers <- panel[c(obs), ]
# mean(outliers$ratio_CIF_p, na.rm = T)
# # 99.94448
# mean(panel$ratio_CIF_p)
# # 4071.975
# panel <- panel[-c(obs),]
# mean(panel$ratio_CIF_p)
# # 4079.083

# fit_r <- lm(ln.ratio_CIF_r ~ dist + dist.sq +
#               contig + 
#               rLandlocked +
#               pLandlocked +
#               ln.FutImport_misrep +
#               ihs.ReExport_misrep_r +
#               ln.ratio_CIF_r_lag +
#               tariff + 
#               rCorruption + pCorruption +
#               rPoorRegulation + pPoorRegulation,
#             data = panel)
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
# Bonferonni.out <- outlierTest(fit_r, n.max = 10000)
Bonferonni.out <- outlierTest(fit, n.max = 10000)
obs <- as.numeric(names(Bonferonni.out[[1]]))
outliers <- panel[c(obs), ]
# mean(outliers$ratio_CIF_r)
mean(outliers$ratio_CIF)
# 39245.07
# mean(panel$ratio_CIF_r)
mean(panel$ratio_CIF)
# 105.5524
panel <- panel[-c(obs),]
# mean(panel$ratio_CIF_r)
mean(panel$ratio_CIF)
# 94.42078

# fit_p <- lm(ln.ratio_CIF_p ~ dist + dist.sq +
#               contig + 
#               rLandlocked +
#               pLandlocked +
#               ln.FutExport_misrep +
#               ihs.ReExport_misrep_p +
#               ln.ratio_CIF_p_lag +
#               tariff + 
#               rCorruption + pCorruption +
#               rPoorRegulation + pPoorRegulation,
#             data = panel)
# Bonferonni.out <- outlierTest(fit_p, n.max = 10000)
# obs <- as.numeric(names(Bonferonni.out[[1]]))
# outliers <- panel[c(obs), ]
# mean(outliers$ratio_CIF_p)
# # 1995705
# mean(panel$ratio_CIF_p)
# # 4082.203
# panel <- panel[-c(obs),]
# mean(panel$ratio_CIF_p)
# # 516.3927

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
# 20457.06
mean(panel$ratio_CIF)
# 94.42078
panel <- panel[-c(obs),]
mean(panel$ratio_CIF)
# 93.56875

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
# 65150.41
mean(panel$ratio_CIF)
# 93.56875
panel <- panel[-c(obs),]
mean(panel$ratio_CIF)
# 93.15954

rm(Bonferonni.out, outliers, obs)
nrow(panel)
# 3656537
save(panel, file = "Data/Panel/panel_nooutliers.Rdata")



## ## ## ## ## ## ## ## ## ## ##
# LOW ESTIMATES             ####
## ## ## ## ## ## ## ## ## ## ##

# .. Censor the data-set to get lower-bounds ####
# panel %>% filter(ratio_CIF_r > 2 | ratio_CIF_r < 0.5) %>% nrow
# # 705552
# panel %>% filter(ratio_CIF_p > 2 | ratio_CIF_p < 0.5) %>% nrow
# # 758978
# panel_censor <- panel %>% 
#   filter(ratio_CIF_r <= 2) %>% 
#   filter(ratio_CIF_r >= 0.5) %>%
#   filter(ratio_CIF_p <= 2) %>% 
#   filter(ratio_CIF_p >= 0.5)
# nrow(panel_censor)
# # 668543
# mean(panel_censor$ratio_CIF_r)
# # 1.093699
# mean(panel_censor$ratio_CIF_p)
# # 1.111454
# plot(density(panel_censor$ratio_CIF_r))
# plot(density(panel_censor$ratio_CIF_p))
panel %>% filter(ratio_CIF > 2 | ratio_CIF < 0.5) %>% nrow
# 1658201
panel_censor <- panel %>% 
  filter(ratio_CIF <= 2) %>% 
  filter(ratio_CIF >= 0.5)
nrow(panel_censor)
# 1998336
mean(panel_censor$ratio_CIF)
# 1.101944
plot(density(panel_censor$ratio_CIF))


# .. Estimate CIF rates ####
# fit_censor_r <- lm(ln.ratio_CIF_r ~ dist + dist.sq +
#                      contig + 
#                      rLandlocked +
#                      pLandlocked +
#                      ln.FutImport_misrep +
#                      ihs.ReExport_misrep_r +
#                      ln.ratio_CIF_r_lag +
#                      tariff + 
#                      rCorruption + pCorruption +
#                      rPoorRegulation + pPoorRegulation,
#                    data = panel_censor)
# summary(fit_censor_r)
# mean(exp(fitted(fit_censor_r)))
# # 1.048581

fit_censor <- lm(ln.ratio_CIF ~ dist + dist.sq +
                   contig + 
                   rLandlocked +
                   pLandlocked +
                   ln.FutImport_misrep +
                   ihs.ReExport_misrep +
                   ln.ratio_CIF_lag +
                   tariff + 
                   rCorruption + pCorruption +
                   rPoorRegulation + pPoorRegulation,
                 data = panel_censor)
summary(fit_censor)
mean(exp(fitted(fit_censor)))
# 1.048686

# fit_censor_p <- lm(ln.ratio_CIF_p ~ dist + dist.sq +
#                      contig + 
#                      rLandlocked +
#                      pLandlocked +
#                      ln.FutExport_misrep +
#                      ihs.ReExport_misrep_p +
#                      ln.ratio_CIF_p_lag +
#                      tariff + 
#                      rCorruption + pCorruption +
#                      rPoorRegulation + pPoorRegulation,
#                    data = panel_censor)
# summary(fit_censor_p)
# mean(exp(fitted(fit_censor_p)))
# # 1.064791

panel <- panel_censor
# fit_r <- fit_censor_r
# fit_p <- fit_censor_p
fit <- fit_censor

#rm(fit_censor_r, fit_censor_p, panel_censor)
rm(fit_censor, panel_censor)


# .. Compute fitted values when predictors are 0 ####
IFF.preds <- c("tariff", "rCorruption", "pCorruption",
               "rPoorRegulation", "pPoorRegulation")

# coef <- coef(fit_r)
# for (v in 1:length(coef)){
#   if (!(names(coef)[v] %in% IFF.preds)){
#     coef[v] <- 0
#   }
# }
# coef
# panel$fitted_IFF_r <- as.numeric(exp(model.matrix(fit_r) %*% coef))
coef <- coef(fit)
for (v in 1:length(coef)){
  if (!(names(coef)[v] %in% IFF.preds)){
    coef[v] <- 0
  }
}
coef
panel$fitted_IFF <- as.numeric(exp(model.matrix(fit) %*% coef))

# coef <- coef(fit_p)
# for (v in 1:length(coef)){
#   if (!(names(coef)[v] %in% IFF.preds)){
#     coef[v] <- 0
#   }
# }
# coef
# panel$fitted_IFF_p <- as.numeric(exp(model.matrix(fit_p) %*% coef))

# coef <- coef(fit_r)
# for (v in 1:length(coef)){
#   if ((names(coef)[v] %in% IFF.preds)){
#     coef[v] <- 0
#   }
# }
# coef
# panel$fitted_nonIFF_r <- as.numeric(exp(model.matrix(fit_r) %*% coef))
coef <- coef(fit)
for (v in 1:length(coef)){
  if ((names(coef)[v] %in% IFF.preds)){
    coef[v] <- 0
  }
}
coef
panel$fitted_nonIFF <- as.numeric(exp(model.matrix(fit) %*% coef))

# coef <- coef(fit_p)
# for (v in 1:length(coef)){
#   if ((names(coef)[v] %in% IFF.preds)){
#     coef[v] <- 0
#   }
# }
# coef
# panel$fitted_nonIFF_p <- as.numeric(exp(model.matrix(fit_p) %*% coef))

rm(coef, v, IFF.preds)


# .. Compute FOB imports ####
# panel$fitted_r <- exp(fitted(fit_r))
# panel$fitted_p <- exp(fitted(fit_p))
# panel$resid_r <- exp(resid(fit_r))
# panel$resid_p <- exp(resid(fit_p))
# 
# summary(panel$fitted_r)
# summary(panel$fitted_p)

summary(panel$fitted_IFF)
summary(panel$fitted_nonIFF)

# panel <- panel %>%
#   mutate(fitted_all = exp( log(fitted_IFF) + log(fitted_nonIFF) ))
# sum(round(panel$fitted, 5) == round(panel$fitted_all, 5)) == nrow(panel)
# TRUE

# panel <- panel %>%
#   mutate(fitted_adj = ifelse(fitted < 1, 1, fitted),
#          resid_adj = ratio_CIF - fitted_adj)

# Version 1
panel <- panel %>%
  mutate(FOB_Import = Import_value / fitted_nonIFF,
         FOB_Import_IFF = Import_value / fitted_IFF)

# Version 2
# panel <- panel %>%
#   mutate(FOB_Import = pNetExport_value + (pNetExport_value * fitted_nonIFF),
#          FOB_Import_IFF = pNetExport_value + (pNetExport_value * fitted_IFF))

# Version 3
# panel <- panel %>%
#   mutate(FOB_Import = Import_value / resid_adj,
#          FOB_Import_IFF = Import_value / (resid_adj + fitted_IFF))

# Version 4
# panel <- panel %>%
#   mutate(FOB_Import = pNetExport_value + pNetExport_value * resid_adj,
#          FOB_Import_IFF = pNetExport_value + pNetExport_value * (resid_adj + fitted_IFF))

# Version 5
# panel <- panel %>%
#   mutate(FOB_Import = Import_value / fitted_r,
#          pFOB_Import = pImport_value / fitted_p)

# Version 6
# panel <- panel %>%
#   mutate(FOB_Import = Import_value / fitted_nonIFF_r,
#          pFOB_Import = pImport_value / fitted_nonIFF_p,
#          FOB_Import_IFF = Import_value / fitted_IFF_r,
#          pFOB_Import_IFF = Import_value / fitted_IFF_p)


# .. Estimate fixed effects regression for import discrepancy ####
# panel <- panel %>%
#   mutate(rep_dist_r = abs(log(FOB_Import/pNetExport_value))) %>%
#   filter(is.finite(rep_dist_r))
# nrow(panel)
# # 668543
panel <- panel %>%
  mutate(rep_dist = abs(log(FOB_Import/pNetExport_value))) %>%
  filter(is.finite(rep_dist))
nrow(panel)
# 1998336

panel <- panel %>%
  mutate_at(vars(reporter.ISO, partner.ISO, year),
            funs(as.factor(.)))

# FE.out <- felm(rep_dist_r ~ 0| reporter.ISO + 
#                  partner.ISO + year,
#                data = panel)
FE.out <- felm(rep_dist ~ 0| reporter.ISO + 
                 partner.ISO + year,
               data = panel)
FE <- getfe(FE.out, se = T) 

FE <- FE %>%
  group_by(fe) %>%
  mutate(min = min(effect)) %>%
  ungroup()

FE$sigma <- pi/2*(FE$effect - (FE$min + 2*FE$se))
attr(FE$sigma, "extra") <- NULL

panel <- panel %>%
  mutate_at(vars(reporter.ISO, partner.ISO, year),
            funs(as.character(.)))


# .. Harmonization procedure for import discrepancy ####
panel <- left_join(panel, FE %>% 
                     filter(fe == "reporter.ISO") %>%
                     select(idx, sigma) %>%
                     mutate(idx = as.character(idx)),
                   by = c("reporter.ISO" = "idx")) %>%
  rename(rSigma_r = sigma)

panel <- left_join(panel, FE %>% 
                     filter(fe == "partner.ISO") %>%
                     select(idx, sigma) %>%
                     mutate(idx = as.character(idx)),
                   by = c("partner.ISO" = "idx")) %>%
  rename(pSigma_r = sigma)

panel <- panel %>%
  mutate(w_r = (exp(rSigma_r^2)*(exp(rSigma_r^2) - 1))/(exp(rSigma_r^2)*(exp(rSigma_r^2)- 1) + exp(pSigma_r^2)*(exp(pSigma_r^2) - 1)),
         w_p = (exp(pSigma_r^2)*(exp(pSigma_r^2) - 1))/(exp(rSigma_r^2)*(exp(rSigma_r^2)- 1) + exp(pSigma_r^2)*(exp(pSigma_r^2) - 1)))
summary(panel$w_r)
summary(panel$w_p)

panel <- panel %>%
  mutate(w = w_r + w_p)
summary(panel$w)

# panel <- panel %>%
#   mutate(RV_I = w_r*FOB_Import + w_p*pNetExport_value)
panel <- panel %>%
  mutate(RV = w_r*FOB_Import + w_p*pNetExport_value)


# # .. Estimate fixed effects regression for export discrepancy ####
# panel <- panel %>%
#   mutate(rep_dist_p = abs(log(NetExport_value/pFOB_Import))) %>%
#   filter(is.finite(rep_dist_p))
# nrow(panel)
# # 668543
# 
# panel <- panel %>%
#   mutate_at(vars(reporter.ISO, partner.ISO, year),
#             funs(as.factor(.)))
# 
# FE.out <- felm(rep_dist_p ~ 0| reporter.ISO + 
#                  partner.ISO + year,
#                data = panel)
# FE <- getfe(FE.out, se = T) 
# 
# FE <- FE %>%
#   group_by(fe) %>%
#   mutate(min = min(effect)) %>%
#   ungroup()
# 
# FE$sigma <- pi/2*(FE$effect - (FE$min + 2*FE$se))
# attr(FE$sigma, "extra") <- NULL
# 
# panel <- panel %>%
#   mutate_at(vars(reporter.ISO, partner.ISO, year),
#             funs(as.character(.)))
# 
# 
# # .. Harmonization procedure for export discrepancy ####
# panel <- left_join(panel, FE %>% 
#                      filter(fe == "reporter.ISO") %>%
#                      select(idx, sigma) %>%
#                      mutate(idx = as.character(idx)),
#                    by = c("reporter.ISO" = "idx")) %>%
#   rename(rSigma_p = sigma)
# 
# panel <- left_join(panel, FE %>% 
#                      filter(fe == "partner.ISO") %>%
#                      select(idx, sigma) %>%
#                      mutate(idx = as.character(idx)),
#                    by = c("partner.ISO" = "idx")) %>%
#   rename(pSigma_p = sigma)
# 
# panel <- panel %>%
#   mutate(w_r = (exp(rSigma_p^2)*(exp(rSigma_p^2) - 1))/(exp(rSigma_p^2)*(exp(rSigma_p^2)- 1) + exp(pSigma_p^2)*(exp(pSigma_p^2) - 1)),
#          w_p = (exp(pSigma_p^2)*(exp(pSigma_p^2) - 1))/(exp(rSigma_p^2)*(exp(rSigma_p^2)- 1) + exp(pSigma_p^2)*(exp(pSigma_p^2) - 1)))
# summary(panel$w_r)
# summary(panel$w_p)
# 
# panel <- panel %>%
#   mutate(w = w_r + w_p)
# summary(panel$w)
# 
# panel <- panel %>%
#   mutate(RV_E = w_r*NetExport_value + w_p*pFOB_Import)


# .. Compute IFF ####
panel <- panel %>%
  mutate(Imp_IFF_lo = FOB_Import_IFF - RV,
         Exp_IFF_lo = RV - pNetExport_value)
# panel <- panel %>%
#   mutate(Imp_IFF_lo = FOB_Import_IFF - RV_I,
#          Exp_IFF_lo = RV_E - NetExport_value)
summary(panel$Imp_IFF_lo)
summary(panel$Exp_IFF_lo)


# .. Move export IFF to mirror ####
panel_mirror <- panel %>%
  select(reporter, reporter.ISO, rRegion, rIncome,
         partner, partner.ISO, pRegion, pIncome,
         commodity.code, year,
         section.code, section,
         SITC.code, SITC.section,
         Exp_IFF_lo)

panel_mirror$id <- paste(panel_mirror$partner.ISO,
                         panel_mirror$reporter.ISO,
                         panel_mirror$commodity.code,
                         panel_mirror$year, sep = "_")

panel_mirror <- panel_mirror %>%
  rename(pExp_IFF_lo = Exp_IFF_lo)

panel <- full_join(panel, panel_mirror,
                   by = c("id" = "id",
                          "reporter" = "partner",
                          "reporter.ISO" = "partner.ISO",
                          "rRegion" = "pRegion",
                          "rIncome" = "pIncome",
                          "partner.ISO" = "reporter.ISO",
                          "partner" = "reporter",
                          "pRegion" = "rRegion",
                          "pIncome" = "rIncome",
                          "year" = "year",
                          "commodity.code" = "commodity.code",
                          "section.code" = "section.code",
                          "section" = "section",
                          "SITC.code" = "SITC.code",
                          "SITC.section" = "SITC.section"))

panel %>%
  filter(duplicated(panel$id)) %>% nrow
# 0
rm(panel_mirror)

panel_lo <- panel
save(panel_lo, file = "Results/panel_lo.Rdata")



## ## ## ## ## ## ## ## ## ## ##
# HIGH ESTIMATES            ####
## ## ## ## ## ## ## ## ## ## ##

# .. Estimate CIF rates ####
load("Data/Panel/panel_nooutliers.Rdata")

nrow(panel)
# 3656537
# max(panel$ratio_CIF_r)
# #  3427709
# max(panel$ratio_CIF_p)
# #  281442085
max(panel$ratio_CIF)
#  52568627

# fit_r <- lm(ln.ratio_CIF_r ~ dist + dist.sq +
#               contig + 
#               rLandlocked +
#               pLandlocked +
#               ln.FutImport_misrep +
#               ihs.ReExport_misrep_r +
#               ln.ratio_CIF_r_lag +
#               tariff + 
#               rCorruption + pCorruption +
#               rPoorRegulation + pPoorRegulation,
#             data = panel)
# summary(fit_r)
# mean(exp(fitted(fit_r)))
# # 3.605966
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
# 3.171489

# fit_p <- lm(ln.ratio_CIF_p ~ dist + dist.sq +
#               contig + 
#               rLandlocked +
#               pLandlocked +
#               ln.FutExport_misrep +
#               ihs.ReExport_misrep_p +
#               ln.ratio_CIF_p_lag +
#               tariff + 
#               rCorruption + pCorruption +
#               rPoorRegulation + pPoorRegulation,
#             data = panel)
# summary(fit_p)
# mean(exp(fitted(fit_p)))
# # 7.404523


# .. Compute fitted values when predictors are 0 ####
IFF.preds <- c("tariff", "rCorruption", "pCorruption",
               "rPoorRegulation", "pPoorRegulation")

# coef <- coef(fit_r)
# for (v in 1:length(coef)){
#   if (!(names(coef)[v] %in% IFF.preds)){
#     coef[v] <- 0
#   }
# }
# coef
# panel$fitted_IFF_r <- as.numeric(exp(model.matrix(fit_r) %*% coef))
coef <- coef(fit)
for (v in 1:length(coef)){
  if (!(names(coef)[v] %in% IFF.preds)){
    coef[v] <- 0
  }
}
coef
panel$fitted_IFF <- as.numeric(exp(model.matrix(fit) %*% coef))

# coef <- coef(fit_p)
# for (v in 1:length(coef)){
#   if (!(names(coef)[v] %in% IFF.preds)){
#     coef[v] <- 0
#   }
# }
# coef
# panel$fitted_IFF_p <- as.numeric(exp(model.matrix(fit_p) %*% coef))

# coef <- coef(fit_r)
# for (v in 1:length(coef)){
#   if ((names(coef)[v] %in% IFF.preds)){
#     coef[v] <- 0
#   }
# }
# coef
# panel$fitted_nonIFF_r <- as.numeric(exp(model.matrix(fit_r) %*% coef))
coef <- coef(fit)
for (v in 1:length(coef)){
  if ((names(coef)[v] %in% IFF.preds)){
    coef[v] <- 0
  }
}
coef
panel$fitted_nonIFF <- as.numeric(exp(model.matrix(fit) %*% coef))

# coef <- coef(fit_p)
# for (v in 1:length(coef)){
#   if ((names(coef)[v] %in% IFF.preds)){
#     coef[v] <- 0
#   }
# }
# coef
# panel$fitted_nonIFF_p <- as.numeric(exp(model.matrix(fit_p) %*% coef))

rm(coef, v, IFF.preds)


# .. Compute FOB imports ####
# panel$fitted_r <- exp(fitted(fit_r))
# panel$fitted_p <- exp(fitted(fit_p))
# panel$resid_r <- exp(resid(fit_r))
# panel$resid_p <- exp(resid(fit_p))
# 
# summary(panel$fitted_r)
# summary(panel$fitted_p)

summary(panel$fitted_IFF)
summary(panel$fitted_nonIFF)

# panel <- panel %>%
#   mutate(fitted_all = exp( log(fitted_IFF) + log(fitted_nonIFF) ))
# sum(round(panel$fitted, 5) == round(panel$fitted_all, 5)) == nrow(panel)
# TRUE

# panel <- panel %>%
#   mutate(fitted_adj = ifelse(fitted < 1, 1, fitted),
#          resid_adj = ratio_CIF - fitted_adj)

# Version 1
panel <- panel %>%
  mutate(FOB_Import = Import_value / fitted_nonIFF,
         FOB_Import_IFF = Import_value / fitted_IFF)

# Version 2
# panel <- panel %>%
#   mutate(FOB_Import = pNetExport_value + (pNetExport_value * fitted_nonIFF),
#          FOB_Import_IFF = pNetExport_value + (pNetExport_value * fitted_IFF))

# Version 3
# panel <- panel %>%
#   mutate(FOB_Import = Import_value / resid_adj,
#          FOB_Import_IFF = Import_value / (resid_adj + fitted_IFF))

# Version 4
# panel <- panel %>%
#   mutate(FOB_Import = pNetExport_value + pNetExport_value * resid_adj,
#          FOB_Import_IFF = pNetExport_value + pNetExport_value * (resid_adj + fitted_IFF))

# Version 5
# panel <- panel %>%
#   mutate(FOB_Import = Import_value / fitted_r,
#          pFOB_Import = pImport_value / fitted_p)

# Version 6
# panel <- panel %>%
#   mutate(FOB_Import = Import_value / fitted_nonIFF_r,
#          pFOB_Import = pImport_value / fitted_nonIFF_p,
#          FOB_Import_IFF = Import_value / fitted_IFF_r,
#          pFOB_Import_IFF = Import_value / fitted_IFF_p)


# .. Estimate fixed effects regression for import discrepancy ####
# panel <- panel %>%
#   mutate(rep_dist_r = abs(log(FOB_Import/pNetExport_value))) %>%
#   filter(is.finite(rep_dist_r))
# nrow(panel)
# # 1801269
panel <- panel %>%
  mutate(rep_dist = abs(log(FOB_Import/pNetExport_value))) %>%
  filter(is.finite(rep_dist))
nrow(panel)
# 3656537

panel <- panel %>%
  mutate_at(vars(reporter.ISO, partner.ISO, year),
            funs(as.factor(.)))

# FE.out <- felm(rep_dist_r ~ 0| reporter.ISO + 
#                  partner.ISO + year,
#                data = panel)
FE.out <- felm(rep_dist ~ 0| reporter.ISO + 
                 partner.ISO + year,
               data = panel)
FE <- getfe(FE.out, se = T) 

FE <- FE %>%
  group_by(fe) %>%
  mutate(min = min(effect)) %>%
  ungroup()

FE$sigma <- pi/2*(FE$effect - (FE$min + 2*FE$se))
attr(FE$sigma, "extra") <- NULL

panel <- panel %>%
  mutate_at(vars(reporter.ISO, partner.ISO, year),
            funs(as.character(.)))


# .. Harmonization procedure for import discrepancy ####
panel <- left_join(panel, FE %>% 
                     filter(fe == "reporter.ISO") %>%
                     select(idx, sigma) %>%
                     mutate(idx = as.character(idx)),
                   by = c("reporter.ISO" = "idx")) %>%
  rename(rSigma_r = sigma)

panel <- left_join(panel, FE %>% 
                     filter(fe == "partner.ISO") %>%
                     select(idx, sigma) %>%
                     mutate(idx = as.character(idx)),
                   by = c("partner.ISO" = "idx")) %>%
  rename(pSigma_r = sigma)

panel <- panel %>%
  mutate(w_r = (exp(rSigma_r^2)*(exp(rSigma_r^2) - 1))/(exp(rSigma_r^2)*(exp(rSigma_r^2)- 1) + exp(pSigma_r^2)*(exp(pSigma_r^2) - 1)),
         w_p = (exp(pSigma_r^2)*(exp(pSigma_r^2) - 1))/(exp(rSigma_r^2)*(exp(rSigma_r^2)- 1) + exp(pSigma_r^2)*(exp(pSigma_r^2) - 1)))
summary(panel$w_r)
summary(panel$w_p)

panel <- panel %>%
  mutate(w = w_r + w_p)
summary(panel$w)

# panel <- panel %>%
#   mutate(RV_I = w_r*FOB_Import + w_p*pNetExport_value)
panel <- panel %>%
  mutate(RV = w_r*FOB_Import + w_p*pNetExport_value)


# # .. Estimate fixed effects regression for export discrepancy ####
# panel <- panel %>%
#   mutate(rep_dist_p = abs(log(NetExport_value/pFOB_Import))) %>%
#   filter(is.finite(rep_dist_p))
# nrow(panel)
# # 1801269
# 
# panel <- panel %>%
#   mutate_at(vars(reporter.ISO, partner.ISO, year),
#             funs(as.factor(.)))
# 
# FE.out <- felm(rep_dist_p ~ 0| reporter.ISO + 
#                  partner.ISO + year,
#                data = panel)
# FE <- getfe(FE.out, se = T) 
# 
# FE <- FE %>%
#   group_by(fe) %>%
#   mutate(min = min(effect)) %>%
#   ungroup()
# 
# FE$sigma <- pi/2*(FE$effect - (FE$min + 2*FE$se))
# attr(FE$sigma, "extra") <- NULL
# 
# panel <- panel %>%
#   mutate_at(vars(reporter.ISO, partner.ISO, year),
#             funs(as.character(.)))
# 
# 
# # .. Harmonization procedure for export discrepancy ####
# panel <- left_join(panel, FE %>% 
#                      filter(fe == "reporter.ISO") %>%
#                      select(idx, sigma) %>%
#                      mutate(idx = as.character(idx)),
#                    by = c("reporter.ISO" = "idx")) %>%
#   rename(rSigma_p = sigma)
# 
# panel <- left_join(panel, FE %>% 
#                      filter(fe == "partner.ISO") %>%
#                      select(idx, sigma) %>%
#                      mutate(idx = as.character(idx)),
#                    by = c("partner.ISO" = "idx")) %>%
#   rename(pSigma_p = sigma)
# 
# panel <- panel %>%
#   mutate(w_r = (exp(rSigma_p^2)*(exp(rSigma_p^2) - 1))/(exp(rSigma_p^2)*(exp(rSigma_p^2)- 1) + exp(pSigma_p^2)*(exp(pSigma_p^2) - 1)),
#          w_p = (exp(pSigma_p^2)*(exp(pSigma_p^2) - 1))/(exp(rSigma_p^2)*(exp(rSigma_p^2)- 1) + exp(pSigma_p^2)*(exp(pSigma_p^2) - 1)))
# summary(panel$w_r)
# summary(panel$w_p)
# 
# panel <- panel %>%
#   mutate(w = w_r + w_p)
# summary(panel$w)
# 
# panel <- panel %>%
#   mutate(RV_E = w_r*NetExport_value + w_p*pFOB_Import)


# .. Compute IFF ####
panel <- panel %>%
  mutate(Imp_IFF_hi = FOB_Import_IFF - RV,
         Exp_IFF_hi = RV - pNetExport_value)
# panel <- panel %>%
#   mutate(Imp_IFF_hi = FOB_Import_IFF - RV_I,
#          Exp_IFF_hi = RV_E - NetExport_value)
summary(panel$Imp_IFF_hi)
summary(panel$Exp_IFF_hi)


# .. Move export IFF to mirror ####
panel_mirror <- panel %>%
  select(reporter, reporter.ISO, rRegion, rIncome,
         partner, partner.ISO, pRegion, pIncome,
         commodity.code, year,
         section.code, section,
         SITC.code, SITC.section,
         Exp_IFF_hi)

panel_mirror$id <- paste(panel_mirror$partner.ISO,
                         panel_mirror$reporter.ISO,
                         panel_mirror$commodity.code,
                         panel_mirror$year, sep = "_")

panel_mirror <- panel_mirror %>%
  rename(pExp_IFF_hi = Exp_IFF_hi)

panel <- full_join(panel, panel_mirror,
                   by = c("id" = "id",
                          "reporter" = "partner",
                          "reporter.ISO" = "partner.ISO",
                          "rRegion" = "pRegion",
                          "rIncome" = "pIncome",
                          "partner.ISO" = "reporter.ISO",
                          "partner" = "reporter",
                          "pRegion" = "rRegion",
                          "pIncome" = "rIncome",
                          "year" = "year",
                          "commodity.code" = "commodity.code",
                          "section.code" = "section.code",
                          "section" = "section",
                          "SITC.code" = "SITC.code",
                          "SITC.section" = "SITC.section"))

panel %>%
  filter(duplicated(panel$id)) %>% nrow
# 0
rm(panel_mirror)

panel_hi <- panel
save(panel_hi, file = "Results/panel_hi.Rdata")



## ## ## ## ## ## ## ## ## ## ##
# MERGE RESULTS             ####
## ## ## ## ## ## ## ## ## ## ##

all <- full_join(panel_lo %>%
                   select(id, reporter.ISO, partner.ISO, commodity.code, year,
                          reporter, rRegion, rIncome,
                          partner, pRegion, pIncome,
                          section.code, section,
                          SITC.code, SITC.section,
                          Imp_IFF_lo, pExp_IFF_lo),
                 panel_hi %>%
                   select(id, reporter.ISO, partner.ISO, commodity.code, year,
                          reporter, rRegion, rIncome,
                          partner, pRegion, pIncome,
                          section.code, section,
                          SITC.code, SITC.section,
                          Imp_IFF_hi, pExp_IFF_hi),
                 by = c("id", "reporter.ISO", "partner.ISO", "commodity.code", "year",
                        "reporter", "rRegion", "rIncome",
                        "partner", "pRegion", "pIncome",
                        "section.code", "section",
                        "SITC.code", "SITC.section"))
# all <- full_join(panel_lo %>%
#                    select(id, reporter.ISO, partner.ISO, commodity.code, year,
#                           reporter, rRegion, rIncome,
#                           partner, pRegion, pIncome,
#                           section.code, section,
#                           SITC.code, SITC.section,
#                           Imp_IFF_lo, Exp_IFF_lo),
#                  panel_hi %>%
#                    select(id, reporter.ISO, partner.ISO, commodity.code, year,
#                           reporter, rRegion, rIncome,
#                           partner, pRegion, pIncome,
#                           section.code, section,
#                           SITC.code, SITC.section,
#                           Imp_IFF_hi, Exp_IFF_hi),
#                  by = c("id", "reporter.ISO", "partner.ISO", "commodity.code", "year",
#                         "reporter", "rRegion", "rIncome",
#                         "partner", "pRegion", "pIncome",
#                         "section.code", "section",
#                         "SITC.code", "SITC.section"))
nrow(all)
# 6248254

# all <- all %>%
#   filter(complete.cases(Imp_IFF_lo, pExp_IFF_lo, Imp_IFF_hi, pExp_IFF_hi))
# nrow(all)
panel <- all
# rm(all, FE, FE.out, fit_r, fit_p)
rm(all, FE, FE.out, fit)
rm(panel_hi, panel_lo)

save(panel, file = "Results/panel_results.Rdata")



## ## ## ## ## ## ## ## ## ## ##
# AGGREGATE BY DESTINATION  ####
## ## ## ## ## ## ## ## ## ## ##

# .. Aggregate results using Gross Excluding Reversals ####
GER_Imp_lo_Dest <- panel %>%
  filter(Imp_IFF_lo > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome,
           year,
           partner, partner.ISO, pRegion, pIncome) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T)) %>%
  ungroup()

GER_Imp_hi_Dest <- panel %>%
  filter(Imp_IFF_hi > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome,
           year,
           partner, partner.ISO, pRegion, pIncome) %>%
  summarize(Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T)) %>%
  ungroup()

GER_Imp_Dest <- full_join(GER_Imp_lo_Dest, GER_Imp_hi_Dest,
                          by = c("reporter" = "reporter",
                                 "reporter.ISO" = "reporter.ISO",
                                 "rRegion" = "rRegion",
                                 "rIncome" = "rIncome",
                                 "year" = "year",
                                 "partner" = "partner",
                                 "partner.ISO" = "partner.ISO",
                                 "pRegion" = "pRegion",
                                 "pIncome" = "pIncome"))
rm(GER_Imp_lo_Dest, GER_Imp_hi_Dest)

# GER_Exp_lo_Dest <- panel %>%
#   filter(Exp_IFF_lo > 0) %>%
#   group_by(reporter, reporter.ISO, rRegion, rIncome,
#            year,
#            partner, partner.ISO, pRegion, pIncome) %>%
#   summarize(Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T)) %>%
#   ungroup()
GER_Exp_lo_Dest <- panel %>%
  filter(pExp_IFF_lo > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome,
           year,
           partner, partner.ISO, pRegion, pIncome) %>%
  summarize(Exp_IFF_lo = sum(pExp_IFF_lo, na.rm = T)) %>%
  ungroup()

# GER_Exp_hi_Dest <- panel %>%
#   filter(Exp_IFF_hi > 0) %>%
#   group_by(reporter, reporter.ISO, rRegion, rIncome,
#            year,
#            partner, partner.ISO, pRegion, pIncome) %>%
#   summarize(Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T)) %>%
#   ungroup()
GER_Exp_hi_Dest <- panel %>%
  filter(pExp_IFF_hi > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome,
           year,
           partner, partner.ISO, pRegion, pIncome) %>%
  summarize(Exp_IFF_hi = sum(pExp_IFF_hi, na.rm = T)) %>%
  ungroup()

GER_Exp_Dest <- full_join(GER_Exp_lo_Dest, GER_Exp_hi_Dest,
                          by = c("reporter" = "reporter",
                                 "reporter.ISO" = "reporter.ISO",
                                 "rRegion" = "rRegion",
                                 "rIncome" = "rIncome",
                                 "year" = "year",
                                 "partner" = "partner",
                                 "partner.ISO" = "partner.ISO",
                                 "pRegion" = "pRegion",
                                 "pIncome" = "pIncome"))
rm(GER_Exp_lo_Dest, GER_Exp_hi_Dest)

GER_Orig_Dest_Year <- full_join(GER_Imp_Dest, GER_Exp_Dest,
                                by = c("reporter" = "reporter",
                                       "reporter.ISO" = "reporter.ISO",
                                       "rRegion" = "rRegion",
                                       "rIncome" = "rIncome",
                                       "year" = "year",
                                       "partner" = "partner",
                                       "partner.ISO" = "partner.ISO",
                                       "pRegion" = "pRegion",
                                       "pIncome" = "pIncome"))
rm(GER_Imp_Dest, GER_Exp_Dest)

GER_Orig_Year <- GER_Orig_Dest_Year %>%
  group_by(reporter, reporter.ISO, rRegion, year) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF_lo = Imp_IFF_lo + Exp_IFF_lo,
         Tot_IFF_hi = Imp_IFF_hi + Exp_IFF_hi,
         Tot_IFF_lo_bn = Tot_IFF_lo / 10^9,
         Tot_IFF_hi_bn = Tot_IFF_hi / 10^9)

GER_Orig_Avg <- GER_Orig_Year %>%
  group_by(reporter, reporter.ISO, rRegion) %>%
  summarize(Imp_IFF_lo = mean(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = mean(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = mean(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = mean(Exp_IFF_hi, na.rm = T),
            Tot_IFF_lo = mean(Tot_IFF_lo, na.rm = T),
            Tot_IFF_hi = mean(Tot_IFF_hi, na.rm = T),
            Tot_IFF_lo_bn = mean(Tot_IFF_lo_bn, na.rm = T),
            Tot_IFF_hi_bn = mean(Tot_IFF_hi_bn, na.rm = T)) %>%
  ungroup()

GER_Orig_Sum <- GER_Orig_Year %>%
  group_by(reporter, reporter.ISO, rRegion) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T),
            Tot_IFF_lo = sum(Tot_IFF_lo, na.rm = T),
            Tot_IFF_hi = sum(Tot_IFF_hi, na.rm = T),
            Tot_IFF_lo_bn = sum(Tot_IFF_lo_bn, na.rm = T),
            Tot_IFF_hi_bn = sum(Tot_IFF_hi_bn, na.rm = T)) %>%
  ungroup()

GER_Orig_Dest <- GER_Orig_Dest_Year %>%
  group_by(reporter, reporter.ISO, rRegion, partner, partner.ISO, pRegion) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF_lo = Imp_IFF_lo + Exp_IFF_lo,
         Tot_IFF_hi = Imp_IFF_hi + Exp_IFF_hi,
         Tot_IFF_lo_bn = Tot_IFF_lo / 10^9,
         Tot_IFF_hi_bn = Tot_IFF_hi / 10^9)

GER_Orig_Dest_Year_Africa <- GER_Orig_Dest_Year %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

GER_Orig_Dest_Africa <- GER_Orig_Dest %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

GER_Dest_Africa <- GER_Orig_Dest_Africa %>%
  group_by(partner, partner.ISO, pRegion) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T),
            Tot_IFF_lo = sum(Tot_IFF_lo, na.rm = T),
            Tot_IFF_hi = sum(Tot_IFF_hi, na.rm = T),
            Tot_IFF_lo_bn = sum(Tot_IFF_lo_bn, na.rm = T),
            Tot_IFF_hi_bn = sum(Tot_IFF_hi_bn, na.rm = T)) %>%
  ungroup()

GER_Orig_Year_Africa <- GER_Orig_Year %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

GER_Orig_Avg_Africa <- GER_Orig_Avg %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

GER_Orig_Sum_Africa <- GER_Orig_Sum %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

GER_Year_Africa <- GER_Orig_Year_Africa %>%
  group_by(year) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T),
            Tot_IFF_lo = sum(Tot_IFF_lo, na.rm = T),
            Tot_IFF_hi = sum(Tot_IFF_hi, na.rm = T),
            Tot_IFF_lo_bn = sum(Tot_IFF_lo_bn, na.rm = T),
            Tot_IFF_hi_bn = sum(Tot_IFF_hi_bn, na.rm = T)) %>%
  ungroup() 

GER_Africa <- GER_Year_Africa %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T),
            Tot_IFF_lo = sum(Tot_IFF_lo, na.rm = T),
            Tot_IFF_hi = sum(Tot_IFF_hi, na.rm = T),
            Tot_IFF_lo_bn = sum(Tot_IFF_lo_bn, na.rm = T),
            Tot_IFF_hi_bn = sum(Tot_IFF_hi_bn, na.rm = T))

save(GER_Orig_Dest_Year_Africa, file = "Results/Current Version/GER_Orig_Dest_Year_Africa.Rdata")
write.csv(GER_Orig_Dest_Year_Africa, file = "Results/Current Version/GER_Orig_Dest_Year_Africa.csv",
          row.names = F)
save(GER_Orig_Year_Africa, file = "Results/Current Version/GER_Orig_Year_Africa.Rdata")
write.csv(GER_Orig_Year_Africa, file = "Results/Current Version/GER_Orig_Year_Africa.csv",
          row.names = F)
save(GER_Orig_Avg_Africa, file = "Results/Current Version/GER_Orig_Avg_Africa.Rdata")
write.csv(GER_Orig_Avg_Africa, file = "Results/Current Version/GER_Orig_Avg_Africa.csv",
          row.names = F)
save(GER_Orig_Sum_Africa, file = "Results/Current Version/GER_Orig_Sum_Africa.Rdata")
write.csv(GER_Orig_Sum_Africa, file = "Results/Current Version/GER_Orig_Sum_Africa.csv",
          row.names = F)
save(GER_Orig_Dest_Africa, file = "Results/Current Version/GER_Orig_Dest_Africa.Rdata")
write.csv(GER_Orig_Dest_Africa, file = "Results/Current Version/GER_Orig_Dest_Africa.csv",
          row.names = F)
save(GER_Dest_Africa, file = "Results/Current Version/GER_Dest_Africa.Rdata")
write.csv(GER_Dest_Africa, file = "Results/Current Version/GER_Dest_Africa.csv",
          row.names = F)
save(GER_Year_Africa, file = "Results/Current Version/GER_Year_Africa.Rdata")
write.csv(GER_Year_Africa, file = "Results/Current Version/GER_Year_Africa.csv",
          row.names = F)
save(GER_Africa, file = "Results/Current Version/GER_Africa.Rdata")
write.csv(GER_Africa, file = "Results/Current Version/GER_Africa.csv",
          row.names = F)


# .. Aggregate results using Net Aggregation ####
# Net_Orig_Dest_Year <- panel %>%
#   group_by(reporter, reporter.ISO, rRegion, rIncome,
#            partner, partner.ISO, pRegion, pIncome,
#            year) %>%
#   summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
#             Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
#             Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
#             Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T)) %>%
#   ungroup()
Net_Orig_Dest_Year <- panel %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome,
           partner, partner.ISO, pRegion, pIncome,
           year) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(pExp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(pExp_IFF_hi, na.rm = T)) %>%
  ungroup()

Net_Orig_Year <- Net_Orig_Dest_Year %>%
  group_by(reporter, reporter.ISO, rRegion, year) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF_lo = Imp_IFF_lo + Exp_IFF_lo,
         Tot_IFF_hi = Imp_IFF_hi + Exp_IFF_hi,
         Tot_IFF_lo_bn = Tot_IFF_lo / 10^9,
         Tot_IFF_hi_bn = Tot_IFF_hi / 10^9)

Net_Orig_Avg <- Net_Orig_Year %>%
  group_by(reporter, reporter.ISO, rRegion) %>%
  summarize(Imp_IFF_lo = mean(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = mean(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = mean(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = mean(Exp_IFF_hi, na.rm = T),
            Tot_IFF_lo = mean(Tot_IFF_lo, na.rm = T),
            Tot_IFF_hi = mean(Tot_IFF_hi, na.rm = T),
            Tot_IFF_lo_bn = mean(Tot_IFF_lo_bn, na.rm = T),
            Tot_IFF_hi_bn = mean(Tot_IFF_hi_bn, na.rm = T)) %>%
  ungroup()

Net_Orig_Sum <- Net_Orig_Year %>%
  group_by(reporter, reporter.ISO, rRegion) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T),
            Tot_IFF_lo = sum(Tot_IFF_lo, na.rm = T),
            Tot_IFF_hi = sum(Tot_IFF_hi, na.rm = T),
            Tot_IFF_lo_bn = sum(Tot_IFF_lo_bn, na.rm = T),
            Tot_IFF_hi_bn = sum(Tot_IFF_hi_bn, na.rm = T)) %>%
  ungroup()

Net_Orig_Dest <- Net_Orig_Dest_Year %>%
  group_by(reporter, reporter.ISO, rRegion, partner, partner.ISO, pRegion) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF_lo = Imp_IFF_lo + Exp_IFF_lo,
         Tot_IFF_hi = Imp_IFF_hi + Exp_IFF_hi,
         Tot_IFF_lo_bn = Tot_IFF_lo / 10^9,
         Tot_IFF_hi_bn = Tot_IFF_hi / 10^9)

Net_Orig_Dest_Year_Africa <- Net_Orig_Dest_Year %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

Net_Orig_Dest_Africa <- Net_Orig_Dest %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

Net_Dest_Africa <- Net_Orig_Dest_Africa %>%
  group_by(partner, partner.ISO, pRegion) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T),
            Tot_IFF_lo = sum(Tot_IFF_lo, na.rm = T),
            Tot_IFF_hi = sum(Tot_IFF_hi, na.rm = T),
            Tot_IFF_lo_bn = sum(Tot_IFF_lo_bn, na.rm = T),
            Tot_IFF_hi_bn = sum(Tot_IFF_hi_bn, na.rm = T)) %>%
  ungroup()

Net_Orig_Year_Africa <- Net_Orig_Year %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

Net_Orig_Avg_Africa <- Net_Orig_Avg %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

Net_Orig_Sum_Africa <- Net_Orig_Sum %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

Net_Year_Africa <- Net_Orig_Year_Africa %>%
  group_by(year) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T),
            Tot_IFF_lo = sum(Tot_IFF_lo, na.rm = T),
            Tot_IFF_hi = sum(Tot_IFF_hi, na.rm = T),
            Tot_IFF_lo_bn = sum(Tot_IFF_lo_bn, na.rm = T),
            Tot_IFF_hi_bn = sum(Tot_IFF_hi_bn, na.rm = T)) %>%
  ungroup()

Net_Africa <- Net_Year_Africa %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T),
            Tot_IFF_lo = sum(Tot_IFF_lo, na.rm = T),
            Tot_IFF_hi = sum(Tot_IFF_hi, na.rm = T),
            Tot_IFF_lo_bn = sum(Tot_IFF_lo_bn, na.rm = T),
            Tot_IFF_hi_bn = sum(Tot_IFF_hi_bn, na.rm = T))

save(Net_Orig_Dest_Year_Africa, file = "Results/Current Version/Net_Orig_Dest_Year_Africa.Rdata")
write.csv(Net_Orig_Dest_Year_Africa, file = "Results/Current Version/Net_Orig_Dest_Year_Africa.csv",
          row.names = F)
save(Net_Orig_Year_Africa, file = "Results/Current Version/Net_Orig_Year_Africa.Rdata")
write.csv(Net_Orig_Year_Africa, file = "Results/Current Version/Net_Orig_Year_Africa.csv",
          row.names = F)
save(Net_Orig_Avg_Africa, file = "Results/Current Version/Net_Orig_Avg_Africa.Rdata")
write.csv(Net_Orig_Avg_Africa, file = "Results/Current Version/Net_Orig_Avg_Africa.csv",
          row.names = F)
save(Net_Orig_Sum_Africa, file = "Results/Current Version/Net_Orig_Sum_Africa.Rdata")
write.csv(Net_Orig_Sum_Africa, file = "Results/Current Version/Net_Orig_Sum_Africa.csv",
          row.names = F)
save(Net_Orig_Dest_Africa, file = "Results/Current Version/Net_Orig_Dest_Africa.Rdata")
write.csv(Net_Orig_Dest_Africa, file = "Results/Current Version/Net_Orig_Dest_Africa.csv",
          row.names = F)
save(Net_Dest_Africa, file = "Results/Current Version/Net_Dest_Africa.Rdata")
write.csv(Net_Dest_Africa, file = "Results/Current Version/Net_Dest_Africa.csv",
          row.names = F)
save(Net_Year_Africa, file = "Results/Current Version/Net_Year_Africa.Rdata")
write.csv(Net_Year_Africa, file = "Results/Current Version/Net_Year_Africa.csv",
          row.names = F)
save(Net_Africa, file = "Results/Current Version/Net_Africa.Rdata")
write.csv(Net_Africa, file = "Results/Current Version/Net_Africa.csv",
          row.names = F)



## ## ## ## ## ## ## ## ## ## ##
# AGGREGATE BY SECTOR       ####
## ## ## ## ## ## ## ## ## ## ##

# .. Aggregate results using Gross Excluding Reversals ####
GER_Imp_lo_Sect <- panel %>%
  filter(Imp_IFF_lo > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome,
           year, 
           section.code, section) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T)) %>%
  ungroup()

GER_Imp_hi_Sect <- panel %>%
  filter(Imp_IFF_hi > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome,
           year, 
           section.code, section) %>%
  summarize(Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T)) %>%
  ungroup()

GER_Imp_Sect <- full_join(GER_Imp_lo_Sect, GER_Imp_hi_Sect,
                          by = c("reporter" = "reporter",
                                 "reporter.ISO" = "reporter.ISO",
                                 "rRegion" = "rRegion",
                                 "rIncome" = "rIncome",
                                 "year" = "year",
                                 "section.code" = "section.code",
                                 "section" = "section"))
rm(GER_Imp_lo_Sect, GER_Imp_hi_Sect)

# GER_Exp_lo_Sect <- panel %>%
#   filter(Exp_IFF_lo > 0) %>%
#   group_by(reporter, reporter.ISO, rRegion, rIncome,
#            year, 
#            section.code, section) %>%
#   summarize(Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T)) %>%
#   ungroup()
GER_Exp_lo_Sect <- panel %>%
  filter(pExp_IFF_lo > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome,
           year, 
           section.code, section) %>%
  summarize(Exp_IFF_lo = sum(pExp_IFF_lo, na.rm = T)) %>%
  ungroup()

# GER_Exp_hi_Sect <- panel %>%
#   filter(Exp_IFF_hi > 0) %>%
#   group_by(reporter, reporter.ISO, rRegion, rIncome,
#            year, 
#            section.code, section) %>%
#   summarize(Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T)) %>%
#   ungroup()
GER_Exp_hi_Sect <- panel %>%
  filter(pExp_IFF_hi > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome,
           year, 
           section.code, section) %>%
  summarize(Exp_IFF_hi = sum(pExp_IFF_hi, na.rm = T)) %>%
  ungroup()

GER_Exp_Sect <- full_join(GER_Exp_lo_Sect, GER_Exp_hi_Sect,
                          by = c("reporter" = "reporter",
                                 "reporter.ISO" = "reporter.ISO",
                                 "rRegion" = "rRegion",
                                 "rIncome" = "rIncome",
                                 "year" = "year",
                                 "section.code" = "section.code",
                                 "section" = "section"))
rm(GER_Exp_lo_Sect, GER_Exp_hi_Sect)

GER_Orig_Sect_Year <- full_join(GER_Imp_Sect, GER_Exp_Sect,
                                by = c("reporter" = "reporter",
                                       "reporter.ISO" = "reporter.ISO",
                                       "rRegion" = "rRegion",
                                       "rIncome" = "rIncome",
                                       "year" = "year",
                                       "section.code" = "section.code",
                                       "section" = "section"))
rm(GER_Imp_Sect, GER_Exp_Sect)

GER_Orig_Sect_Avg <- GER_Orig_Sect_Year %>%
  group_by(reporter, reporter.ISO, rRegion, section.code, section) %>%
  summarize(Imp_IFF_lo = mean(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = mean(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = mean(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = mean(Exp_IFF_hi, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF_lo = Imp_IFF_lo + Exp_IFF_lo,
         Tot_IFF_hi = Imp_IFF_hi + Exp_IFF_hi,
         Tot_IFF_lo_bn = Tot_IFF_lo / 10^9,
         Tot_IFF_hi_bn = Tot_IFF_hi / 10^9)

GER_Orig_Sect_Sum <- GER_Orig_Sect_Year %>%
  group_by(reporter, reporter.ISO, rRegion, section.code, section) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF_lo = Imp_IFF_lo + Exp_IFF_lo,
         Tot_IFF_hi = Imp_IFF_hi + Exp_IFF_hi,
         Tot_IFF_lo_bn = Tot_IFF_lo / 10^9,
         Tot_IFF_hi_bn = Tot_IFF_hi / 10^9)

GER_Orig_Sect_Year_Africa <- GER_Orig_Sect_Year %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

GER_Orig_Sect_Avg_Africa <- GER_Orig_Sect_Avg %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

GER_Orig_Sect_Sum_Africa <- GER_Orig_Sect_Sum %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

GER_Sect_Africa <- GER_Orig_Sect_Sum_Africa %>%
  group_by(section.code, section) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T),
            Tot_IFF_lo = sum(Tot_IFF_lo, na.rm = T),
            Tot_IFF_hi = sum(Tot_IFF_hi, na.rm = T),
            Tot_IFF_lo_bn = sum(Tot_IFF_lo_bn, na.rm = T),
            Tot_IFF_hi_bn = sum(Tot_IFF_hi_bn, na.rm = T)) %>%
  ungroup()

save(GER_Orig_Sect_Year_Africa, file = "Results/Current Version/GER_Orig_Sect_Year_Africa.Rdata")
write.csv(GER_Orig_Sect_Year_Africa, file = "Results/Current Version/GER_Orig_Sect_Year_Africa.csv",
          row.names = F)
save(GER_Orig_Sect_Avg_Africa, file = "Results/Current Version/GER_Orig_Sect_Avg_Africa.Rdata")
write.csv(GER_Orig_Sect_Avg_Africa, file = "Results/Current Version/GER_Orig_Sect_Avg_Africa.csv",
          row.names = F)
save(GER_Orig_Sect_Sum_Africa, file = "Results/Current Version/GER_Orig_Sect_Sum_Africa.Rdata")
write.csv(GER_Orig_Sect_Sum_Africa, file = "Results/Current Version/GER_Orig_Sect_Sum_Africa.csv",
          row.names = F)
save(GER_Sect_Africa, file = "Results/Current Version/GER_Sect_Africa.Rdata")
write.csv(GER_Sect_Africa, file = "Results/Current Version/GER_Sect_Africa.csv",
          row.names = F)


# .. Aggregate results using Net Aggregation ####
# Net_Orig_Sect_Year <- panel %>%
#   group_by(reporter, reporter.ISO, rRegion, rIncome,
#            year, section.code, section) %>%
#   summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
#             Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
#             Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
#             Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T)) %>%
#   ungroup()
Net_Orig_Sect_Year <- panel %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome,
           year, section.code, section) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(pExp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(pExp_IFF_hi, na.rm = T)) %>%
  ungroup()

Net_Orig_Sect_Avg <- Net_Orig_Sect_Year %>%
  group_by(reporter, reporter.ISO, rRegion, section.code, section) %>%
  summarize(Imp_IFF_lo = mean(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = mean(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = mean(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = mean(Exp_IFF_hi, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF_lo = Imp_IFF_lo + Exp_IFF_lo,
         Tot_IFF_hi = Imp_IFF_hi + Exp_IFF_hi,
         Tot_IFF_lo_bn = Tot_IFF_lo / 10^9,
         Tot_IFF_hi_bn = Tot_IFF_hi / 10^9)

Net_Orig_Sect_Sum <- Net_Orig_Sect_Year %>%
  group_by(reporter, reporter.ISO, rRegion, section.code, section) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF_lo = Imp_IFF_lo + Exp_IFF_lo,
         Tot_IFF_hi = Imp_IFF_hi + Exp_IFF_hi,
         Tot_IFF_lo_bn = Tot_IFF_lo / 10^9,
         Tot_IFF_hi_bn = Tot_IFF_hi / 10^9)

Net_Orig_Sect_Year_Africa <- Net_Orig_Sect_Year %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

Net_Orig_Sect_Avg_Africa <- Net_Orig_Sect_Avg %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

Net_Orig_Sect_Sum_Africa <- Net_Orig_Sect_Sum %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

Net_Sect_Africa <- Net_Orig_Sect_Sum_Africa %>%
  group_by(section.code, section) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T),
            Tot_IFF_lo = sum(Tot_IFF_lo, na.rm = T),
            Tot_IFF_hi = sum(Tot_IFF_hi, na.rm = T),
            Tot_IFF_lo_bn = sum(Tot_IFF_lo_bn, na.rm = T),
            Tot_IFF_hi_bn = sum(Tot_IFF_hi_bn, na.rm = T)) %>%
  ungroup()

save(Net_Orig_Sect_Year_Africa, file = "Results/Current Version/Net_Orig_Sect_Year_Africa.Rdata")
write.csv(GER_Orig_Sect_Year_Africa, file = "Results/Current Version/Net_Orig_Sect_Year_Africa.csv",
          row.names = F)
save(Net_Orig_Sect_Avg_Africa, file = "Results/Current Version/Net_Orig_Sect_Avg_Africa.Rdata")
write.csv(Net_Orig_Sect_Avg_Africa, file = "Results/Current Version/Net_Orig_Sect_Avg_Africa.csv",
          row.names = F)
save(Net_Orig_Sect_Sum_Africa, file = "Results/Current Version/Net_Orig_Sect_Sum_Africa.Rdata")
write.csv(Net_Orig_Sect_Sum_Africa, file = "Results/Current Version/Net_Orig_Sect_Sum_Africa.csv",
          row.names = F)
save(Net_Sect_Africa, file = "Results/Current Version/Net_Sect_Africa.Rdata")
write.csv(Net_Sect_Africa, file = "Results/Current Version/Net_Sect_Africa.csv",
          row.names = F)



## ## ## ## ## ## ## ## ## ## ##
# HEADLINE FIGURES          ####
## ## ## ## ## ## ## ## ## ## ##

(Gross.IFF.per.year <- sum(GER_Orig_Avg_Africa$Tot_IFF_hi_bn))
# 83.1738

(Net.IFF.per.year <- sum(Net_Orig_Avg_Africa$Tot_IFF_hi_bn))
# 26.42522



## ## ## ## ## ## ## ## ## ## ##
# PILOT COUNTRY RESULTS     ####
## ## ## ## ## ## ## ## ## ## ##

pilots <- c("EGY", "NGA", "SEN", "ZAF", "TZA", "TUN")

ger.avg <- GER_Orig_Avg_Africa %>%
  filter(reporter.ISO %in% pilots) %>%
  select(reporter, Tot_IFF_lo_bn, Tot_IFF_hi_bn)
kable(ger.avg, digits = 2, format = "rst")

net.avg <- Net_Orig_Avg_Africa %>%
  filter(reporter.ISO %in% pilots) %>%
  select(reporter, Tot_IFF_lo_bn, Tot_IFF_hi_bn)
kable(net.avg, digits = 2, format = "rst")

ger.sum <- GER_Orig_Sum_Africa %>%
  filter(reporter.ISO %in% pilots) %>%
  select(reporter, Tot_IFF_lo_bn, Tot_IFF_hi_bn)
kable(ger.sum, digits = 2, format = "rst")

net.sum <- Net_Orig_Sum_Africa %>%
  filter(reporter.ISO %in% pilots) %>%
  select(reporter, Tot_IFF_lo_bn, Tot_IFF_hi_bn)
kable(net.sum, digits = 2, format = "rst")

rm(net.avg, ger.avg, net.sum, ger.sum)