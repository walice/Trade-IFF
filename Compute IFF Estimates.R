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
# .. Truncate panel and remove CIF ratios greater than 10,000 (not used)
# .. Estimate regression
# .. Identify and remove outliers
# Mis-Invoicing Estimates
# .. Estimate CIF rates
# .. Compute fitted values when predictors are 0
# .. Compute FOB imports
# .. Estimate fixed effects regression
# .. Harmonization procedure
# .. Compute IFF
# .. Move export IFF to mirror



## ## ## ## ## ## ## ## ## ## ##
# PREAMBLE                  ####
## ## ## ## ## ## ## ## ## ## ##

setwd("/home/alepissier/IFFe/") # Virtual server
data.disk <- "/scratch/alepissier/IFFe/"
# source("Scripts/Data Preparation.R")
library(car)
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

# .. Truncate panel and remove CIF ratios greater than 10,000 (not used) ####
summary(panel$ratio_CIF)

panel %>% filter(ratio_CIF > 10^4) %>% nrow
# 8165
panel %>% filter(ratio_CIF > 10^3) %>% nrow
# 32947


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
# 3.60618
max(panel$ratio_CIF)
# 2354224577
mean(panel$ratio_CIF)
# 2874.508


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
# 5044407
summary(panel$CD)

Bonferonni.out <- outlierTest(fit, n.max = 10000)
obs <- as.numeric(names(Bonferonni.out[[1]]))
outliers <- panel[c(obs), ]
mean(outliers$ratio_CIF)
# 2082723
mean(panel$ratio_CIF)
# 2874.508
panel <- panel[-c(obs),]
mean(panel$ratio_CIF)
# 154.6712

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
# 39536.62
mean(panel$ratio_CIF)
# 154.6712
panel <- panel[-c(obs),]
mean(panel$ratio_CIF)
# 144.3341

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
# 27220.22
mean(panel$ratio_CIF)
# 144.3341
panel <- panel[-c(obs),]
mean(panel$ratio_CIF)
# 143.2105

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
# 332405.6
mean(panel$ratio_CIF)
# 143.2105
panel <- panel[-c(obs),]
mean(panel$ratio_CIF)
# 141.0333

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
# 13789.47
mean(panel$ratio_CIF)
# 141.0333
panel <- panel[-c(obs),]
mean(panel$ratio_CIF)
# 141.0144

rm(Bonferonni.out, outliers, obs)
nrow(panel)
# 5036248
save(panel, file = paste0(data.disk, "Data/Panel/panel_nooutliers.Rdata"))



## ## ## ## ## ## ## ## ## ## ##
# MIS-INVOICING ESTIMATES   ####
## ## ## ## ## ## ## ## ## ## ##

# .. Estimate CIF rates ####
load(paste0(data.disk, "Data/Panel/panel_nooutliers.Rdata"))

nrow(panel)
# 5036248
max(panel$ratio_CIF)
# 52568627

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
# 3.522114

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

coef <- coef(fit)
for (v in 1:length(coef)){
  if (!(names(coef)[v] %in% IFF.preds)){
    coef[v] <- 0
  }
}
coef
panel$fitted_IFF <- as.numeric(exp(model.matrix(fit) %*% coef))
mean(panel$fitted_IFF)
# 0.964646

coef <- coef(fit)
for (v in 1:length(coef)){
  if ((names(coef)[v] %in% IFF.preds)){
    coef[v] <- 0
  }
}
coef
panel$fitted_nonIFF <- as.numeric(exp(model.matrix(fit) %*% coef))
mean(panel$fitted_nonIFF)
# 3.619048

rm(coef, d, v, IFF.preds)


# .. Compute FOB imports ####
summary(panel$fitted_IFF)
summary(panel$fitted_nonIFF)

panel <- panel %>%
  mutate(FOB_Import = Import_value / fitted_nonIFF,
         FOB_Import_IFF = Import_value / fitted_IFF)


# .. Estimate fixed effects regression ####
panel <- panel %>%
  mutate(rep_dist = abs(log(FOB_Import/pNetExport_value))) %>%
  filter(is.finite(rep_dist))
nrow(panel)
# 5036248

panel <- panel %>%
  mutate_at(vars(reporter.ISO, partner.ISO, year),
            funs(as.factor(.)))

FE.out <- felm(rep_dist ~ 0| reporter.ISO + 
                 partner.ISO + year,
               data = panel)
FE <- getfe(FE.out, se = T)
save(FE, file = "Results/FE.Rdata")
load("Results/FE.Rdata")

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
panel <- panel %>%
  mutate(Imp_IFF = FOB_Import_IFF - RV,
         Exp_IFF = RV - pNetExport_value)
summary(panel$Imp_IFF)
summary(panel$Exp_IFF)


# .. Move export IFF to mirror ####
panel_mirror <- panel %>%
  select(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI,
         partner, partner.ISO, pRegion, pIncome, pDev, pHDI,
         commodity.code, year,
         section.code, section,
         SITC.code, SITC.section,
         Exp_IFF)

panel_mirror$id <- paste(panel_mirror$partner.ISO,
                         panel_mirror$reporter.ISO,
                         panel_mirror$commodity.code,
                         panel_mirror$year, sep = "_")

panel_mirror <- panel_mirror %>%
  rename(pExp_IFF = Exp_IFF)

panel <- full_join(panel, panel_mirror,
                   by = c("id" = "id",
                          "reporter" = "partner",
                          "reporter.ISO" = "partner.ISO",
                          "rRegion" = "pRegion",
                          "rIncome" = "pIncome",
                          "rDev" = "pDev",
                          "rHDI" = "pHDI",
                          "partner.ISO" = "reporter.ISO",
                          "partner" = "reporter",
                          "pRegion" = "rRegion",
                          "pIncome" = "rIncome",
                          "pDev" = "rDev",
                          "pHDI" = "rHDI",
                          "year" = "year",
                          "commodity.code" = "commodity.code",
                          "section.code" = "section.code",
                          "section" = "section",
                          "SITC.code" = "SITC.code",
                          "SITC.section" = "SITC.section"))

panel %>%
  filter(duplicated(panel$id)) %>% nrow
# 0
rm(panel_mirror, FE, FE.out, fit)

save(panel, file = "Results/panel_results.Rdata")
