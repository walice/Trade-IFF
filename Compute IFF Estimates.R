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
# First Stage
# .. Estimate regression
# .. Identify and remove outliers
# .. Censor the data-set (not used)
# .. Compute fitted values when predictors are 0
# .. Compute adjusted FOB imports
# Second Stage
# .. Estimate fixed effects regression
# .. Harmonization procedure
# .. Compute IFF
# .. Move export IFF to mirror
# Aggregate by Destination
# .. Aggregate results using Gross Excluding Reversals
# .. Aggregate results using Net Aggregation
# Aggregate by Sector
# .. Aggregate results using Gross Excluding Reversals
# .. Aggregate results using Net Aggregation
# Pilot Country Results
# Figures



## ## ## ## ## ## ## ## ## ## ##
# PREAMBLE                  ####
## ## ## ## ## ## ## ## ## ## ##

#setwd("C:/cloudstorage/googledrive/Projects/UN Consultancy/Illicit Financial Flows/IFF estimates") # Alice work
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


# .. Create dependent variable and predictors ####
panel <- panel %>% 
  mutate(ratio_CIF = Import_value / pNetExport_value) %>%
  filter(is.finite(ratio_CIF))
nrow(panel)
# 8080386

summary(panel$ratio_CIF)
hist(panel$ratio_CIF)

panel <- panel %>%
  mutate(ln.ratio_CIF = log(ratio_CIF)) %>%
  filter(is.finite(ln.ratio_CIF))
nrow(panel)
# 7605946

summary(panel$year)
panel <- panel %>% 
  group_by(reporter.ISO, partner.ISO, commodity.code) %>%
  mutate(ln.ratio_CIF_lag = dplyr::lag(ln.ratio_CIF, n = 1)) %>%
  ungroup() %>%
  filter(is.finite(ln.ratio_CIF_lag))
nrow(panel)
# 6812396
summary(panel$year)

panel <- panel %>% 
  group_by(reporter.ISO, partner.ISO, commodity.code) %>%
  mutate(Import_fut = dplyr::lead(Import_value, n = 1)) %>%
  ungroup() %>%
  filter(is.finite(Import_fut))
nrow(panel)
# 6147496
summary(panel$year)

panel <- panel %>%
  mutate(FutImport_misrep = Import_fut / Import_value,
         ReExport_misrep = pReExport_value / Import_value) %>%
  filter(is.finite(FutImport_misrep) & is.finite(ReExport_misrep))
nrow(panel)
# 6147496

panel <- panel %>%
  filter(complete.cases(ratio_CIF, ln.ratio_CIF, ln.ratio_CIF_lag,
                        dist, contig, rLandlocked, pLandlocked, tariff,
                        FutImport_misrep, ReExport_misrep))
nrow(panel)
# 3673469

panel <- panel %>%
  mutate(dist.sq = I(dist^2))


# .. Variable transformation ####
summary(panel$ratio_CIF)
hist(panel$ln.ratio_CIF)
hist(panel$FutImport_misrep)
hist(log(panel$FutImport_misrep))
hist(panel$ReExport_misrep)
hist(log(panel$ReExport_misrep))
hist(panel$tariff)
hist(log(panel$tariff))
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
save(panel, file = "Data/Panel/panel_clean.Rdata")



## ## ## ## ## ## ## ## ## ## ##
# REMOVE OUTLIERS           ####
## ## ## ## ## ## ## ## ## ## ##

# .. Estimate regression ####
fit <- lm(ln.ratio_CIF ~ dist + dist.sq +
            contig + 
            rLandlocked +
            pLandlocked +
            ln.FutImport_misrep +
            ihs.ReExport_misrep +
            ln.ratio_CIF_lag +
            tariff,
          data = panel)
summary(fit)
# tariff -0.00109230905948
max(panel$ratio_CIF)
# 2354224577
mean(panel$ratio_CIF)
# 2900.504


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
              tariff,
            data = panel)
  panel$CD <- cooks.distance(fit)
}
nrow(panel)
# 3673469
summary(panel$CD)

Bonferonni.out <- outlierTest(fit, n.max = 10000)
obs <- as.numeric(names(Bonferonni.out[[1]]))
outliers <- panel[c(obs), ]
mean(outliers$ratio_CIF)
# 2091726
mean(panel$ratio_CIF)
# 2900.504
panel <- panel[-c(obs),]
mean(panel$ratio_CIF)
# 105.3894

fit <- lm(ln.ratio_CIF ~ dist + dist.sq +
            contig + 
            rLandlocked +
            pLandlocked +
            ln.FutImport_misrep +
            ihs.ReExport_misrep +
            ln.ratio_CIF_lag +
            tariff,
          data = panel)
Bonferonni.out <- outlierTest(fit, n.max = 10000)
obs <- as.numeric(names(Bonferonni.out[[1]]))
outliers <- panel[c(obs), ]
mean(outliers$ratio_CIF)
# 25048.84
mean(panel$ratio_CIF)
# 105.3894
panel <- panel[-c(obs),]
mean(panel$ratio_CIF)
# 98.45228

fit <- lm(ln.ratio_CIF ~ dist + dist.sq +
            contig + 
            rLandlocked +
            pLandlocked +
            ln.FutImport_misrep +
            ihs.ReExport_misrep +
            ln.ratio_CIF_lag +
            tariff,
          data = panel)
Bonferonni.out <- outlierTest(fit, n.max = 10000)
obs <- as.numeric(names(Bonferonni.out[[1]]))
outliers <- panel[c(obs), ]
mean(outliers$ratio_CIF)
# 132929.9
mean(panel$ratio_CIF)
# 98.45228
panel <- panel[-c(obs),]
mean(panel$ratio_CIF)
# 92.83823

fit <- lm(ln.ratio_CIF ~ dist + dist.sq +
            contig + 
            rLandlocked +
            pLandlocked +
            ln.FutImport_misrep +
            ihs.ReExport_misrep +
            ln.ratio_CIF_lag +
            tariff,
          data = panel)
Bonferonni.out <- outlierTest(fit, n.max = 10000)
obs <- as.numeric(names(Bonferonni.out[[1]]))
outliers <- panel[c(obs), ]
mean(outliers$ratio_CIF)
# 8293.587
mean(panel$ratio_CIF)
# 92.83823
panel <- panel[-c(obs),]
mean(panel$ratio_CIF)
# 92.78233

rm(Bonferonni.out, outliers, obs)
save(panel, file = "Data/Panel/panel_nooutliers.Rdata")



## ## ## ## ## ## ## ## ## ## ##
# LOW ESTIMATES             ####
## ## ## ## ## ## ## ## ## ## ##

# .. Censor the data-set to get lower-bounds ####
panel %>% filter(ratio_CIF > 2) %>% nrow
# 938562
panel %>% filter(ratio_CIF < 0.5) %>% nrow
# 725720
panel %>% filter(ratio_CIF > 2 | ratio_CIF < 0.5) %>% nrow
# 1664282
panel_censor <- panel %>% 
  filter(ratio_CIF <= 2) %>% filter(ratio_CIF >= 0.5)
nrow(panel_censor)
# 2003078
mean(panel_censor$ratio_CIF)
# 1.102118
summary(panel_censor$ratio_CIF)
plot(density(panel_censor$ratio_CIF))

# fit_censor <- lm(ln.ratio_CIF ~ dist + dist.sq +
#                    contig + 
#                    rLandlocked +
#                    pLandlocked +
#                    ln.FutImport_misrep +
#                    ihs.ReExport_misrep +
#                    ln.ratio_CIF_lag +
#                    tariff,
#                  data = panel_censor)
fit_censor <- lm(ln.ratio_CIF ~ dist + dist.sq +
                   contig + 
                   rLandlocked +
                   pLandlocked +
                   ln.FutImport_misrep +
                   ihs.ReExport_misrep +
                   ln.ratio_CIF_lag,
                 data = panel_censor)
summary(fit_censor)
# tariff -0.000243995980299

panel <- panel_censor
fit <- fit_censor
rm(fit_censor, panel_censor)

# fit <- lm(ln.ratio_CIF ~ dist + dist.sq +
#             contig + 
#             rLandlocked +
#             pLandlocked +
#             ln.FutImport_misrep +
#             ihs.ReExport_misrep +
#             ln.ratio_CIF_lag +
#             tariff,
#           data = panel)
fit <- lm(ln.ratio_CIF ~ dist + dist.sq +
            contig + 
            rLandlocked +
            pLandlocked +
            ln.FutImport_misrep +
            ihs.ReExport_misrep +
            ln.ratio_CIF_lag,
          data = panel)
max(panel$ratio_CIF)
# 2
nrow(panel)
# 2003078
summary(fit)
# tariff -0.000243995980299


# # .. Compute fitted values when predictors are 0 ####
# fit_IFF <- lm(ln.ratio_CIF ~ - 1 + tariff,
#               data = panel)
# summary(fit_IFF)
# # tariff 0.00068400
# panel$fitted_IFF_lm <- exp(predict(fit_IFF))
# rm(fit_IFF)
# # This does not give the marginal effect of IFF predictors given legitimate predictors of IFF.
# # Rather, this gives the unconditional effect of tariffs on the discrepancy.
# 
# coef <- coef(fit)
# for (v in 1:length(coef)){
# if (names(coef[v]) == "tariff"){
#     coef[v] <- coef(fit)["tariff"]
#   } else {
#     coef[v] <- 0
#   }
# }
# coef
# 
# panel$fitted_IFF_man <- as.numeric(exp(model.matrix(fit) %*% coef))
# 
# panel$fitted_IFF_pred <- exp(predict(fit,
#                                      newdata = data.frame(dist = 0,
#                                                           dist.sq = 0,
#                                                           contig = "0",
#                                                           rLandlocked = "0",
#                                                           pLandlocked = "0",
#                                                           ln.FutImport_misrep = 0,
#                                                           ihs.ReExport_misrep = 0,
#                                                           ln.ratio_CIF_lag = 0,
#                                                           tariff = panel$tariff)))
# 
# summary(panel$fitted_IFF_lm)
# summary(panel$fitted_IFF_man)
# summary(panel$fitted_IFF_pred)
# # fitted_IFF_pred is the same as fitted_IFF_man if the constant were not set to 0.
# 
# panel$fitted_IFF <- panel$fitted_IFF_man
# 
# coef <- coef(fit)
# for (v in 1:length(coef)){
#   if (names(coef[v]) == "tariff"){
#     coef[v] <- 0 
#   }
#   
# }
# coef
# 
# panel$fitted_nonIFF <- as.numeric(exp(model.matrix(fit) %*% coef))
# rm(coef, v)


# .. Compute adjusted FOB imports ####
panel$fitted <- exp(fitted(fit))
panel$resid <- exp(resid(fit))

summary(panel$fitted)
summary(panel$fitted_IFF)
summary(panel$fitted_nonIFF)

panel <- panel %>%
  mutate(fitted_all = exp( log(fitted_IFF) + log(fitted_nonIFF) ))
sum(round(panel$fitted, 5) == round(panel$fitted_all, 5)) == nrow(panel)
# TRUE

panel <- panel %>%
  mutate(fitted_adj = ifelse(fitted < 1, 1, fitted),
         resid_adj = ratio_CIF - fitted_adj)

# Version 1
# panel <- panel %>%
#   mutate(FOB_Import = Import_value / fitted_nonIFF,
#          FOB_Import_IFF = Import_value / fitted_IFF)

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
panel <- panel %>%
  mutate(FOB_Import = Import_value / fitted)


# .. Estimate fixed effects regression ####
panel <- panel %>%
  mutate(rep_dist = abs(log(pNetExport_value/FOB_Import))) %>%
  filter(is.finite(rep_dist))
nrow(panel)
# 2003078

panel <- panel %>%
  mutate_at(vars(reporter.ISO, partner.ISO, year),
            funs(as.factor(.)))

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


# .. Harmonization procedure ####
panel <- left_join(panel, FE %>% 
                     filter(fe == "reporter.ISO") %>%
                     select(idx, sigma) %>%
                     mutate(idx = as.character(idx)),
                   by = c("reporter.ISO" = "idx")) %>%
  rename(rSigma = sigma)

panel <- left_join(panel, FE %>% 
                     filter(fe == "partner.ISO") %>%
                     select(idx, sigma) %>%
                     mutate(idx = as.character(idx)),
                   by = c("partner.ISO" = "idx")) %>%
  rename(pSigma = sigma)

panel <- panel %>%
  mutate(w_r = (exp(rSigma^2)*(exp(rSigma^2) - 1))/(exp(rSigma^2)*(exp(rSigma^2)- 1) + exp(pSigma^2)*(exp(pSigma^2) - 1)),
         w_p = (exp(pSigma^2)*(exp(pSigma^2) - 1))/(exp(rSigma^2)*(exp(rSigma^2)- 1) + exp(pSigma^2)*(exp(pSigma^2) - 1)))
summary(panel$w_r)
summary(panel$w_p)

panel <- panel %>%
  mutate(w = w_r + w_p)
summary(panel$w)

panel <- panel %>%
  mutate(RV = w_r*FOB_Import + w_p*pNetExport_value)


# .. Compute IFF ####
# panel <- panel %>%
#   mutate(Imp_IFF_lo = FOB_Import_IFF - RV,
#          Exp_IFF_lo = RV - pNetExport_value)
panel <- panel %>%
  mutate(Imp_IFF_lo = FOB_Import - RV,
         Exp_IFF_lo = RV - pNetExport_value)
summary(panel$Imp_IFF_lo)
summary(panel$Exp_IFF_lo)


# .. Move export IFF to mirror ####
panel_mirror <- panel %>%
  select(reporter, reporter.ISO, rRegion, rIncome,
         partner, partner.ISO, pRegion, pIncome,
         commodity.code, year,
         section.code, section,
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
                          "section" = "section"))

panel %>%
  filter(duplicated(panel$id)) %>% nrow
# 0
rm(panel_mirror)

panel_lo <- panel
save(panel_lo, file = "Results/panel_lo.Rdata")



## ## ## ## ## ## ## ## ## ## ##
# HIGH ESTIMATES            ####
## ## ## ## ## ## ## ## ## ## ##

load("Data/Panel/panel_nooutliers.Rdata")

# fit <- lm(ln.ratio_CIF ~ dist + dist.sq +
#             contig + 
#             rLandlocked +
#             pLandlocked +
#             ln.FutImport_misrep +
#             ihs.ReExport_misrep +
#             ln.ratio_CIF_lag +
#             tariff,
#           data = panel)
fit <- lm(ln.ratio_CIF ~ dist + dist.sq +
            contig + 
            rLandlocked +
            pLandlocked +
            ln.FutImport_misrep +
            ihs.ReExport_misrep +
            ln.ratio_CIF_lag,
          data = panel)
max(panel$ratio_CIF)
# 52568627
nrow(panel)
# 3667360
summary(fit)
# tariff -0.00110051892134


# # .. Compute fitted values when predictors are 0 ####
# fit_IFF <- lm(ln.ratio_CIF ~ - 1 + tariff,
#               data = panel)
# summary(fit_IFF)
# # tariff 0.00095775
# panel$fitted_IFF_lm <- exp(predict(fit_IFF))
# rm(fit_IFF)
# # This does not give the marginal effect of IFF predictors given legitimate predictors of IFF.
# # Rather, this gives the unconditional effect of tariffs on the discrepancy.
# 
# coef <- coef(fit)
# for (v in 1:length(coef)){
#   if (names(coef[v]) == "tariff"){
#     coef[v] <- coef(fit)["tariff"]
#   } else {
#     coef[v] <- 0
#   }
# }
# coef
# 
# panel$fitted_IFF_man <- as.numeric(exp(model.matrix(fit) %*% coef))
# 
# panel$fitted_IFF_pred <- exp(predict(fit,
#                                      newdata = data.frame(dist = 0,
#                                                           dist.sq = 0,
#                                                           contig = "0",
#                                                           rLandlocked = "0",
#                                                           pLandlocked = "0",
#                                                           ln.FutImport_misrep = 0,
#                                                           ihs.ReExport_misrep = 0,
#                                                           ln.ratio_CIF_lag = 0,
#                                                           tariff = panel$tariff)))
# 
# summary(panel$fitted_IFF_lm)
# summary(panel$fitted_IFF_man)
# summary(panel$fitted_IFF_pred)
# # fitted_IFF_pred is the same as fitted_IFF_man if the constant were not set to 0.
# 
# panel$fitted_IFF <- panel$fitted_IFF_man
# 
# coef <- coef(fit)
# for (v in 1:length(coef)){
#   if (names(coef[v]) == "tariff"){
#     coef[v] <- 0 
#   }
#   
# }
# coef
# 
# panel$fitted_nonIFF <- as.numeric(exp(model.matrix(fit) %*% coef))
# rm(coef, v)


# .. Compute adjusted FOB imports ####
panel$fitted <- exp(fitted(fit))
panel$resid <- exp(resid(fit))

summary(panel$fitted)
summary(panel$fitted_IFF)
summary(panel$fitted_nonIFF)

panel <- panel %>%
  mutate(fitted_all = exp( log(fitted_IFF) + log(fitted_nonIFF) ))
sum(round(panel$fitted, 5) == round(panel$fitted_all, 5)) == nrow(panel)
# TRUE

panel <- panel %>%
  mutate(fitted_adj = ifelse(fitted < 1, 1, fitted),
         resid_adj = ratio_CIF - fitted_adj)

# Version 1
# panel <- panel %>%
#   mutate(FOB_Import = Import_value / fitted_nonIFF,
#          FOB_Import_IFF = Import_value / fitted_IFF)

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
panel <- panel %>%
  mutate(FOB_Import = Import_value / fitted)


# .. Estimate fixed effects regression ####
panel <- panel %>%
  mutate(rep_dist = abs(log(pNetExport_value/FOB_Import))) %>%
  filter(is.finite(rep_dist))
nrow(panel)
# 3667360

panel <- panel %>%
  mutate_at(vars(reporter.ISO, partner.ISO, year),
            funs(as.factor(.)))

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


# .. Harmonization procedure ####
panel <- left_join(panel, FE %>% 
                     filter(fe == "reporter.ISO") %>%
                     select(idx, sigma) %>%
                     mutate(idx = as.character(idx)),
                   by = c("reporter.ISO" = "idx")) %>%
  rename(rSigma = sigma)

panel <- left_join(panel, FE %>% 
                     filter(fe == "partner.ISO") %>%
                     select(idx, sigma) %>%
                     mutate(idx = as.character(idx)),
                   by = c("partner.ISO" = "idx")) %>%
  rename(pSigma = sigma)

panel <- panel %>%
  mutate(w_r = (exp(rSigma^2)*(exp(rSigma^2) - 1))/(exp(rSigma^2)*(exp(rSigma^2)- 1) + exp(pSigma^2)*(exp(pSigma^2) - 1)),
         w_p = (exp(pSigma^2)*(exp(pSigma^2) - 1))/(exp(rSigma^2)*(exp(rSigma^2)- 1) + exp(pSigma^2)*(exp(pSigma^2) - 1)))
summary(panel$w_r)
summary(panel$w_p)

panel <- panel %>%
  mutate(w = w_r + w_p)
summary(panel$w)

panel <- panel %>%
  mutate(RV = w_r*FOB_Import + w_p*pNetExport_value)


# .. Compute IFF ####
# panel <- panel %>%
#   mutate(Imp_IFF_hi = FOB_Import_IFF - RV,
#          Exp_IFF_hi = RV - pNetExport_value)
panel <- panel %>%
  mutate(Imp_IFF_hi = FOB_Import - RV,
         Exp_IFF_hi = RV - pNetExport_value)
summary(panel$Imp_IFF_hi)
summary(panel$Exp_IFF_hi)


# .. Move export IFF to mirror ####
panel_mirror <- panel %>%
  select(reporter, reporter.ISO, rRegion, rIncome,
         partner, partner.ISO, pRegion, pIncome,
         commodity.code, year,
         section.code, section,
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
                          "section" = "section"))

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
                          Imp_IFF_lo, pExp_IFF_lo),
                 panel_hi %>%
                   select(id, reporter.ISO, partner.ISO, commodity.code, year,
                          reporter, rRegion, rIncome,
                          partner, pRegion, pIncome,
                          section.code, section,
                          Imp_IFF_hi, pExp_IFF_hi),
                 by = c("id", "reporter.ISO", "partner.ISO", "commodity.code", "year",
                        "reporter", "rRegion", "rIncome",
                        "partner", "pRegion", "pIncome",
                        "section.code", "section"))
nrow(all)
# 6269074

# all <- all %>%
#   filter(complete.cases(Imp_IFF_lo, pExp_IFF_lo, Imp_IFF_hi, pExp_IFF_hi))
# nrow(all)
panel <- all
rm(all, FE, FE.out, fit)
rm(panel_hi, panel_lo)



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

GER_Exp_lo_Dest <- panel %>%
  filter(pExp_IFF_lo > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome,
           year,
           partner, partner.ISO, pRegion, pIncome) %>%
  summarize(Exp_IFF_lo = sum(pExp_IFF_lo, na.rm = T)) %>%
  ungroup()

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

GER_Exp_lo_Sect <- panel %>%
  filter(pExp_IFF_lo > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome,
           year, 
           section.code, section) %>%
  summarize(Exp_IFF_lo = sum(pExp_IFF_lo, na.rm = T)) %>%
  ungroup()

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

GER_Orig_Sect <- GER_Orig_Sect_Year %>%
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

GER_Orig_Sect_Africa <- GER_Orig_Sect %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

GER_Sect_Africa <- GER_Orig_Sect_Africa %>%
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
save(GER_Orig_Sect_Africa, file = "Results/Current Version/GER_Orig_Sect_Africa.Rdata")
write.csv(GER_Orig_Sect_Africa, file = "Results/Current Version/GER_Orig_Sect_Africa.csv",
          row.names = F)
save(GER_Sect_Africa, file = "Results/Current Version/GER_Sect_Africa.Rdata")
write.csv(GER_Sect_Africa, file = "Results/Current Version/GER_Sect_Africa.csv",
          row.names = F)


# .. Aggregate results using Net Aggregation ####
Net_Orig_Sect_Year <- panel %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome,
           year, section.code, section) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(pExp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(pExp_IFF_hi, na.rm = T)) %>%
  ungroup()

Net_Orig_Sect <- Net_Orig_Sect_Year %>%
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

Net_Orig_Sect_Africa <- Net_Orig_Sect %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

Net_Sect_Africa <- Net_Orig_Sect_Africa %>%
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
save(Net_Orig_Sect_Africa, file = "Results/Current Version/Net_Orig_Sect_Africa.Rdata")
write.csv(Net_Orig_Sect_Africa, file = "Results/Current Version/Net_Orig_Sect_Africa.csv",
          row.names = F)
save(Net_Sect_Africa, file = "Results/Current Version/Net_Sect_Africa.Rdata")
write.csv(Net_Sect_Africa, file = "Results/Current Version/Net_Sect_Africa.csv",
          row.names = F)



## ## ## ## ## ## ## ## ## ## ##
# PILOT COUNTRY RESULTS     ####
## ## ## ## ## ## ## ## ## ## ##

pilots <- c("EGY", "NGA", "SEN", "ZAF", "TUN", "TZA")

net <- Net_Orig_Avg_Africa %>%
  filter(reporter.ISO %in% pilots) %>%
  select(-c(reporter, Tot_IFF_lo_bn, Tot_IFF_hi_bn))
kable(net, format = "rst")

ger <- GER_Orig_Avg_Africa %>%
  filter(reporter.ISO %in% pilots) %>%
  select(-c(reporter, Tot_IFF_lo_bn, Tot_IFF_hi_bn))
kable(ger, format = "rst")

rm(net, ger)

GER_Orig_Sect_Africa %>%
  filter(reporter.ISO %in% pilots)







## ## ## ## ## ## ## ## ## ## ##
# FIGURES                   ####
## ## ## ## ## ## ## ## ## ## ##

g <- ggplot(GER_Year_Africa %>% 
         melt(id.vars = "year") %>%
         filter(str_detect(variable, "Imp")), 
       aes(x = year, y = value, fill = variable)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = dollar_format(scale = 1/10^9, accuracy = 1)) +
  scale_fill_discrete(name = "Import IFF", labels = c("Low", "High")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Illicit Financial Flows in Africa",
       subtitle = "Gross Excluding Reversals",
       x = "Year", y = "Illicit flow in billion USD")
ggsave(g,
       file = "Figures/Current Version/GER_Africa_Import.png",
       width = 6, height = 5, units = "in")

g <- ggplot(GER_Year_Africa %>% 
              melt(id.vars = "year") %>%
              filter(str_detect(variable, "Exp")), 
            aes(x = year, y = value, fill = variable)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = dollar_format(scale = 1/10^9, accuracy = 1)) +
  scale_fill_discrete(name = "Export IFF", labels = c("Low", "High")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Illicit Financial Flows in Africa",
       subtitle = "Gross Excluding Reversals",
       x = "Year", y = "Illicit flow in billion USD")
ggsave(g,
       file = "Figures/Current Version/GER_Africa_Export.png",
       width = 6, height = 5, units = "in")

g <- ggplot(Net_Year_Africa %>% 
         melt(id.vars = "year") %>%
         filter(str_detect(variable, "Imp")), 
       aes(x = year, y = value, fill = variable)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = dollar_format(scale = 1/10^9, accuracy = 1))+
  scale_fill_discrete(name = "Import IFF", labels = c("Low", "High")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Illicit Financial Flows in Africa",
       subtitle = "Net",
       x = "Year", y = "Illicit flow in billion USD")
ggsave(g,
       file = "Figures/Current Version/Net_Africa_Import.png",
       width = 6, height = 5, units = "in")

g <- ggplot(Net_Year_Africa %>% 
              melt(id.vars = "year") %>%
              filter(str_detect(variable, "Exp")), 
            aes(x = year, y = value, fill = variable)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = dollar_format(scale = 1/10^9, accuracy = 1)) +
  scale_fill_discrete(name = "Export IFF", labels = c("Low", "High")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Illicit Financial Flows in Africa",
       subtitle = "Net",
       x = "Year", y = "Illicit flow in billion USD")
ggsave(g,
       file = "Figures/Current Version/Net_Africa_Export.png",
       width = 6, height = 5, units = "in")