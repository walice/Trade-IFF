# Compute IFF Estimates
# Alice Lepissier
# alice.lepissier@gmail.com
# Prepared for UNECA

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
# Aggregate by Destination
# .. Aggregate results using Gross Excluding Reversals
# .. Aggregate results using Net Aggregation
# Aggregate by Sector
# .. Aggregate results using Gross Excluding Reversals
# .. Aggregate results using Net Aggregation



## ## ## ## ## ## ## ## ## ## ##
# PREAMBLE                  ####
## ## ## ## ## ## ## ## ## ## ##

setwd("/home/alepissier/IFFe/") # Virtual server
data.disk <- "/scratch/alepissier/IFFe/"
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

#source("Scripts/Data Preparation.R")
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



## ## ## ## ## ## ## ## ## ## ##
# AGGREGATE BY DESTINATION  ####
## ## ## ## ## ## ## ## ## ## ##

load("Results/panel_results.Rdata")


# .. Aggregate results using Gross Excluding Reversals ####
load(paste0(data.disk, "Data/WDI/WDI.Rdata"))
load(paste0(data.disk, "Data/Comtrade/comtrade_total_clean.Rdata"))

GER_Imp_Dest <- panel %>%
  filter(Imp_IFF > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI,
           year,
           partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T)) %>%
  ungroup()

GER_Exp_Dest <- panel %>%
  filter(pExp_IFF > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI,
           year,
           partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Exp_IFF = sum(pExp_IFF, na.rm = T)) %>%
  ungroup()

GER_Orig_Dest_Year <- full_join(GER_Imp_Dest, GER_Exp_Dest,
                                by = c("reporter" = "reporter",
                                       "reporter.ISO" = "reporter.ISO",
                                       "rRegion" = "rRegion",
                                       "rIncome" = "rIncome",
                                       "rDev" = "rDev",
                                       "rHDI" = "rHDI",
                                       "year" = "year",
                                       "partner" = "partner",
                                       "partner.ISO" = "partner.ISO",
                                       "pRegion" = "pRegion",
                                       "pIncome" = "pIncome",
                                       "pDev" = "pDev",
                                       "pHDI" = "pHDI"))
rm(GER_Imp_Dest, GER_Exp_Dest)

GER_Orig_Dest_Year_std <- left_join(GER_Orig_Dest_Year %>% mutate(year = as.integer(year)),
                                    WDI,
                                    by = c("reporter.ISO" = "ISO3166.3", 
                                           "year")) %>%
  rename(rGDP = GDP,
         rGNPpc = GNPpc)
GER_Orig_Dest_Year_std <- left_join(GER_Orig_Dest_Year_std %>% mutate(year = as.integer(year)),
                                    WDI,
                                    by = c("partner.ISO" = "ISO3166.3", 
                                           "year")) %>%
  rename(pGDP = GDP,
         pGNPpc = GNPpc)
GER_Orig_Dest_Year_std <- GER_Orig_Dest_Year_std %>%
  mutate(rImp_IFF_GDP = Imp_IFF / rGDP,
         pImp_IFF_GDP = Imp_IFF / pGDP)

GER_Orig_Year <- GER_Orig_Dest_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI, year) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF = Imp_IFF + Exp_IFF,
         Tot_IFF_bn = Tot_IFF / 10^9)
GER_Orig_Year <- left_join(GER_Orig_Year %>% mutate(year = as.integer(year)),
                           WDI,
                           by = c("reporter.ISO" = "ISO3166.3", 
                                  "year")) %>%
  mutate(Tot_IFF_GDP = Tot_IFF / GDP)
GER_Orig_Year <- left_join(GER_Orig_Year,
                           comtrade_total,
                           by = c("reporter.ISO", "year")) %>%
  mutate(Tot_IFF_trade = Tot_IFF / Total_value)

GER_Orig_Avg <- GER_Orig_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI) %>%
  summarize(Imp_IFF = mean(Imp_IFF, na.rm = T),
            Exp_IFF = mean(Exp_IFF, na.rm = T),
            Tot_IFF = mean(Tot_IFF, na.rm = T),
            Tot_IFF_bn = mean(Tot_IFF_bn, na.rm = T),
            Tot_IFF_GDP = mean(Tot_IFF_GDP, na.rm = T),
            Tot_IFF_trade = mean(Tot_IFF_trade, na.rm = T)) %>%
  ungroup()

GER_Orig_Sum <- GER_Orig_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

GER_Orig_Dest_Avg <- GER_Orig_Dest_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI, 
           partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Imp_IFF = mean(Imp_IFF, na.rm = T),
            Exp_IFF = mean(Exp_IFF, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF = Imp_IFF + Exp_IFF,
         Tot_IFF_bn = Tot_IFF / 10^9)
weights <- GER_Orig_Dest_Year %>%
  distinct(reporter.ISO, year, .keep_all = T) %>%
  group_by(reporter.ISO) %>%
  mutate(weight = 1/n()) %>%
  ungroup()
WDI <- left_join(WDI, weights %>% 
                   mutate(year = as.integer(year)) %>%
                   select(reporter.ISO, year, weight),
                 by = c("ISO3166.3" = "reporter.ISO",
                        "year")) %>%
  mutate(weight = ifelse(is.na(weight), 0, weight)) %>%
  group_by(ISO3166.3) %>%
  summarize(GDP = weighted.mean(GDP, weight),
            GNPpc = weighted.mean(GNPpc, weight))
GER_Orig_Dest_Avg <- left_join(GER_Orig_Dest_Avg,
                               WDI,
                               by = c("reporter.ISO" = "ISO3166.3")) %>%
  rename(rGDP = GDP,
         rGNPpc = GNPpc)
GER_Orig_Dest_Avg <- left_join(GER_Orig_Dest_Avg,
                               WDI,
                               by = c("partner.ISO" = "ISO3166.3")) %>%
  rename(pGDP = GDP,
         pGNPpc = GNPpc)

GER_Orig_Dest_Sum <- GER_Orig_Dest_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI,
           partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF = Imp_IFF + Exp_IFF,
         Tot_IFF_bn = Tot_IFF / 10^9)

GER_Orig_Dest_Year_Africa <- GER_Orig_Dest_Year %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

GER_Orig_Dest_Avg_Africa <- GER_Orig_Dest_Avg %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

GER_Orig_Dest_Avg_LMIC <- GER_Orig_Dest_Avg %>%
  filter(rIncome == "LIC" | rIncome == "LMC") %>%
  select(-rIncome)

GER_Orig_Dest_Avg_Developing <- GER_Orig_Dest_Avg %>%
  filter(rDev == "Developing") %>%
  select(-rDev)

GER_Orig_Dest_Avg_LowHDI <- GER_Orig_Dest_Avg %>%
  filter(rHDI == "Lo HDI" | rHDI == "Med HDI") %>%
  select(-rHDI)

GER_Orig_Dest_Sum_Africa <- GER_Orig_Dest_Sum %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

GER_Dest <- GER_Orig_Dest_Sum %>%
  group_by(partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

GER_Dest_Sum_Africa <- GER_Orig_Dest_Sum_Africa %>%
  group_by(partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

GER_Dest_Avg_Africa <- GER_Orig_Dest_Avg_Africa %>%
  group_by(partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

GER_Dest_Avg_LMIC <- GER_Orig_Dest_Avg_LMIC %>%
  group_by(partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

GER_Dest_Avg_Developing <- GER_Orig_Dest_Avg_Developing %>%
  group_by(partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

GER_Dest_Avg_LowHDI <- GER_Orig_Dest_Avg_LowHDI %>%
  group_by(partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

GER_Orig_Year_Africa <- GER_Orig_Year %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

GER_Orig_Year_LMIC <- GER_Orig_Year %>%
  filter(rIncome == "LIC" | rIncome == "LMC") %>%
  select(-rIncome)

GER_Orig_Year_Developing <- GER_Orig_Year %>%
  filter(rDev == "Developing") %>%
  select(-rDev)

GER_Orig_Year_LowHDI <- GER_Orig_Year %>%
  filter(rHDI == "Lo HDI" | rHDI == "Med HDI") %>%
  select(-rHDI)

GER_Orig_Avg_Africa <- GER_Orig_Avg %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

GER_Orig_Avg_LMIC <- GER_Orig_Avg %>%
  filter(rIncome == "LIC" | rIncome == "LMC") %>%
  select(-rIncome)

GER_Orig_Avg_Developing <- GER_Orig_Avg %>%
  filter(rDev == "Developing") %>%
  select(-rDev)

GER_Orig_Avg_LowHDI <- GER_Orig_Avg %>%
  filter(rHDI == "Lo HDI" | rHDI == "Med HDI") %>%
  select(-rHDI)

GER_Orig_Sum_Africa <- GER_Orig_Sum %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

GER_Year_Africa <- GER_Orig_Year_Africa %>%
  group_by(year) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T),
            GDP = sum(GDP, na.rm = T),
            Total_value = sum(Total_value, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF_GDP = Tot_IFF / GDP,
         Tot_IFF_trade = Tot_IFF / Total_value)

GER_Year_LMIC <- GER_Orig_Year_LMIC %>%
  group_by(year) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T),
            GDP = sum(GDP, na.rm = T),
            Total_value = sum(Total_value, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF_GDP = Tot_IFF / GDP,
         Tot_IFF_trade = Tot_IFF / Total_value)

GER_Year_Developing <- GER_Orig_Year_Developing %>%
  group_by(year) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T),
            GDP = sum(GDP, na.rm = T),
            Total_value = sum(Total_value, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF_GDP = Tot_IFF / GDP,
         Tot_IFF_trade = Tot_IFF / Total_value)

GER_Year_LowHDI <- GER_Orig_Year_LowHDI %>%
  group_by(year) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T),
            GDP = sum(GDP, na.rm = T),
            Total_value = sum(Total_value, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF_GDP = Tot_IFF / GDP,
         Tot_IFF_trade = Tot_IFF / Total_value)

GER_Africa <- GER_Year_Africa %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T))

GER_LMIC <- GER_Year_LMIC %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T))

GER_Developing <- GER_Year_Developing %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T))

GER_LowHDI <- GER_Year_LowHDI %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T))

save(GER_Orig_Dest_Year, file = "Results/Summary data-sets/GER_Orig_Dest_Year.Rdata")
write.csv(GER_Orig_Dest_Year, file = "Results/Summary data-sets/GER_Orig_Dest_Year.csv",
          row.names = F)
save(GER_Orig_Dest_Year_std, file = "Results/Summary data-sets/GER_Orig_Dest_Year_std.Rdata")
write.csv(GER_Orig_Dest_Year_std, file = "Results/Summary data-sets/GER_Orig_Dest_Year_std.csv",
          row.names = F)

save(GER_Orig_Year, file = "Results/Summary data-sets/GER_Orig_Year.Rdata")
write.csv(GER_Orig_Year, file = "Results/Summary data-sets/GER_Orig_Year.csv",
          row.names = F)
save(GER_Orig_Avg, file = "Results/Summary data-sets/GER_Orig_Avg.Rdata")
write.csv(GER_Orig_Avg, file = "Results/Summary data-sets/GER_Orig_Avg.csv",
          row.names = F)
save(GER_Orig_Sum, file = "Results/Summary data-sets/GER_Orig_Sum.Rdata")
write.csv(GER_Orig_Sum, file = "Results/Summary data-sets/GER_Orig_Sum.csv",
          row.names = F)

save(GER_Orig_Dest_Year_Africa, file = "Results/Summary data-sets/GER_Orig_Dest_Year_Africa.Rdata")
write.csv(GER_Orig_Dest_Year_Africa, file = "Results/Summary data-sets/GER_Orig_Dest_Year_Africa.csv",
          row.names = F)

save(GER_Orig_Year_Africa, file = "Results/Summary data-sets/GER_Orig_Year_Africa.Rdata")
write.csv(GER_Orig_Year_Africa, file = "Results/Summary data-sets/GER_Orig_Year_Africa.csv",
          row.names = F)
save(GER_Orig_Year_LMIC, file = "Results/Summary data-sets/GER_Orig_Year_LMIC.Rdata")
write.csv(GER_Orig_Year_LMIC, file = "Results/Summary data-sets/GER_Orig_Year_LMIC.csv",
          row.names = F)
save(GER_Orig_Year_Developing, file = "Results/Summary data-sets/GER_Orig_Year_Developing.Rdata")
write.csv(GER_Orig_Year_Developing, file = "Results/Summary data-sets/GER_Orig_Year_Developing.csv",
          row.names = F)
save(GER_Orig_Year_LowHDI, file = "Results/Summary data-sets/GER_Orig_Year_LowHDI.Rdata")
write.csv(GER_Orig_Year_LowHDI, file = "Results/Summary data-sets/GER_Orig_Year_LowHDI.csv",
          row.names = F)

save(GER_Orig_Avg_Africa, file = "Results/Summary data-sets/GER_Orig_Avg_Africa.Rdata")
write.csv(GER_Orig_Avg_Africa, file = "Results/Summary data-sets/GER_Orig_Avg_Africa.csv",
          row.names = F)
save(GER_Orig_Avg_LMIC, file = "Results/Summary data-sets/GER_Orig_Avg_LMIC.Rdata")
write.csv(GER_Orig_Avg_LMIC, file = "Results/Summary data-sets/GER_Orig_Avg_LMIC.csv",
          row.names = F)
save(GER_Orig_Avg_Developing, file = "Results/Summary data-sets/GER_Orig_Avg_Developing.Rdata")
write.csv(GER_Orig_Avg_Developing, file = "Results/Summary data-sets/GER_Orig_Avg_Developing.csv",
          row.names = F)
save(GER_Orig_Avg_LowHDI, file = "Results/Summary data-sets/GER_Orig_Avg_LowHDI.Rdata")
write.csv(GER_Orig_Avg_LowHDI, file = "Results/Summary data-sets/GER_Orig_Avg_LowHDI.csv",
          row.names = F)

save(GER_Orig_Sum_Africa, file = "Results/Summary data-sets/GER_Orig_Sum_Africa.Rdata")
write.csv(GER_Orig_Sum_Africa, file = "Results/Summary data-sets/GER_Orig_Sum_Africa.csv",
          row.names = F)

save(GER_Orig_Dest_Avg, file = "Results/Summary data-sets/GER_Orig_Dest_Avg.Rdata")
write.csv(GER_Orig_Dest_Avg, file = "Results/Summary data-sets/GER_Orig_Dest_Avg.csv",
          row.names = F)
save(GER_Orig_Dest_Avg_Africa, file = "Results/Summary data-sets/GER_Orig_Dest_Avg_Africa.Rdata")
write.csv(GER_Orig_Dest_Avg_Africa, file = "Results/Summary data-sets/GER_Orig_Dest_Avg_Africa.csv",
          row.names = F)
save(GER_Orig_Dest_Avg_LMIC, file = "Results/Summary data-sets/GER_Orig_Dest_Avg_LMIC.Rdata")
write.csv(GER_Orig_Dest_Avg_LMIC, file = "Results/Summary data-sets/GER_Orig_Dest_Avg_LMIC.csv",
          row.names = F)
save(GER_Orig_Dest_Avg_Developing, file = "Results/Summary data-sets/GER_Orig_Dest_Avg_Developing.Rdata")
write.csv(GER_Orig_Dest_Avg_Developing, file = "Results/Summary data-sets/GER_Orig_Dest_Avg_Developing.csv",
          row.names = F)
save(GER_Orig_Dest_Avg_LowHDI, file = "Results/Summary data-sets/GER_Orig_Dest_Avg_LowHDI.Rdata")
write.csv(GER_Orig_Dest_Avg_LowHDI, file = "Results/Summary data-sets/GER_Orig_Dest_Avg_LowHDI.csv",
          row.names = F)

save(GER_Orig_Dest_Sum_Africa, file = "Results/Summary data-sets/GER_Orig_Dest_Sum_Africa.Rdata")
write.csv(GER_Orig_Dest_Sum_Africa, file = "Results/Summary data-sets/GER_Orig_Dest_Sum_Africa.csv",
          row.names = F)

save(GER_Dest, file = "Results/Summary data-sets/GER_Dest.Rdata")
write.csv(GER_Dest, file = "Results/Summary data-sets/GER_Dest.csv",
          row.names = F)
save(GER_Dest_Sum_Africa, file = "Results/Summary data-sets/GER_Dest_Sum_Africa.Rdata")
write.csv(GER_Dest_Sum_Africa, file = "Results/Summary data-sets/GER_Dest_Sum_Africa.csv",
          row.names = F)

save(GER_Dest_Avg_Africa, file = "Results/Summary data-sets/GER_Dest_Avg_Africa.Rdata")
write.csv(GER_Dest_Avg_Africa, file = "Results/Summary data-sets/GER_Dest_Avg_Africa.csv",
          row.names = F)
save(GER_Dest_Avg_LMIC, file = "Results/Summary data-sets/GER_Dest_Avg_LMIC.Rdata")
write.csv(GER_Dest_Avg_LMIC, file = "Results/Summary data-sets/GER_Dest_Avg_LMIC.csv",
          row.names = F)
save(GER_Dest_Avg_Developing, file = "Results/Summary data-sets/GER_Dest_Avg_Developing.Rdata")
write.csv(GER_Dest_Avg_Developing, file = "Results/Summary data-sets/GER_Dest_Avg_Developing.csv",
          row.names = F)
save(GER_Dest_Avg_LowHDI, file = "Results/Summary data-sets/GER_Dest_Avg_LowHDI.Rdata")
write.csv(GER_Dest_Avg_LowHDI, file = "Results/Summary data-sets/GER_Dest_Avg_LowHDI.csv",
          row.names = F)

save(GER_Year_Africa, file = "Results/Summary data-sets/GER_Year_Africa.Rdata")
write.csv(GER_Year_Africa, file = "Results/Summary data-sets/GER_Year_Africa.csv",
          row.names = F)
save(GER_Year_LMIC, file = "Results/Summary data-sets/GER_Year_LMIC.Rdata")
write.csv(GER_Year_LMIC, file = "Results/Summary data-sets/GER_Year_LMIC.csv",
          row.names = F)
save(GER_Year_Developing, file = "Results/Summary data-sets/GER_Year_Developing.Rdata")
write.csv(GER_Year_Developing, file = "Results/Summary data-sets/GER_Year_Developing.csv",
          row.names = F)
save(GER_Year_LowHDI, file = "Results/Summary data-sets/GER_Year_LowHDI.Rdata")
write.csv(GER_Year_LowHDI, file = "Results/Summary data-sets/GER_Year_LowHDI.csv",
          row.names = F)

save(GER_Africa, file = "Results/Summary data-sets/GER_Africa.Rdata")
write.csv(GER_Africa, file = "Results/Summary data-sets/GER_Africa.csv",
          row.names = F)
save(GER_LMIC, file = "Results/Summary data-sets/GER_LMIC.Rdata")
write.csv(GER_LMIC, file = "Results/Summary data-sets/GER_LMIC.csv",
          row.names = F)
save(GER_Developing, file = "Results/Summary data-sets/GER_Developing.Rdata")
write.csv(GER_Developing, file = "Results/Summary data-sets/GER_Developing.csv",
          row.names = F)
save(GER_LowHDI, file = "Results/Summary data-sets/GER_LowHDI.Rdata")
write.csv(GER_LowHDI, file = "Results/Summary data-sets/GER_LowHDI.csv",
          row.names = F)


# .. Aggregate results using Net Aggregation ####
load(paste0(data.disk, "Data/WDI/WDI.Rdata"))
load(paste0(data.disk, "Data/Comtrade/comtrade_total_clean.Rdata"))

Net_Orig_Dest_Year <- panel %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI,
           partner, partner.ISO, pRegion, pIncome, pDev, pHDI,
           year) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(pExp_IFF, na.rm = T)) %>%
  ungroup()

Net_Orig_Year <- Net_Orig_Dest_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI, year) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF = Imp_IFF + Exp_IFF,
         Tot_IFF_bn = Tot_IFF / 10^9)
Net_Orig_Year <- left_join(Net_Orig_Year %>% mutate(year = as.integer(year)),
                           WDI,
                           by = c("reporter.ISO" = "ISO3166.3", 
                                  "year")) %>%
  mutate(Tot_IFF_GDP = Tot_IFF / GDP)
Net_Orig_Year <- left_join(Net_Orig_Year,
                           comtrade_total,
                           by = c("reporter.ISO", "year")) %>%
  mutate(Tot_IFF_trade = Tot_IFF / Total_value)

Net_Orig_Avg <- Net_Orig_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI) %>%
  summarize(Imp_IFF = mean(Imp_IFF, na.rm = T),
            Exp_IFF = mean(Exp_IFF, na.rm = T),
            Tot_IFF = mean(Tot_IFF, na.rm = T),
            Tot_IFF_bn = mean(Tot_IFF_bn, na.rm = T),
            Tot_IFF_GDP = mean(Tot_IFF_GDP, na.rm = T),
            Tot_IFF_trade = mean(Tot_IFF_trade, na.rm = T)) %>%
  ungroup()

Net_Orig_Sum <- Net_Orig_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

Net_Orig_Dest_Sum <- Net_Orig_Dest_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI,
           partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF = Imp_IFF + Exp_IFF,
         Tot_IFF_bn = Tot_IFF / 10^9)

Net_Orig_Dest_Avg <- Net_Orig_Dest_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI,
           partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Imp_IFF = mean(Imp_IFF, na.rm = T),
            Exp_IFF = mean(Exp_IFF, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF = Imp_IFF + Exp_IFF,
         Tot_IFF_bn = Tot_IFF / 10^9)
weights <- Net_Orig_Dest_Year %>%
  distinct(reporter.ISO, year, .keep_all = T) %>%
  group_by(reporter.ISO) %>%
  mutate(weight = 1/n()) %>%
  ungroup()
WDI <- left_join(WDI, weights %>% 
                   mutate(year = as.integer(year)) %>%
                   select(reporter.ISO, year, weight),
                 by = c("ISO3166.3" = "reporter.ISO",
                        "year")) %>%
  mutate(weight = ifelse(is.na(weight), 0, weight)) %>%
  group_by(ISO3166.3) %>%
  summarize(GDP = weighted.mean(GDP, weight),
            GNPpc = weighted.mean(GNPpc, weight))
Net_Orig_Dest_Avg <- left_join(Net_Orig_Dest_Avg,
                               WDI,
                               by = c("reporter.ISO" = "ISO3166.3")) %>%
  rename(rGDP = GDP,
         rGNPpc = GNPpc)
Net_Orig_Dest_Avg <- left_join(Net_Orig_Dest_Avg,
                               WDI,
                               by = c("partner.ISO" = "ISO3166.3")) %>%
  rename(pGDP = GDP,
         pGNPpc = GNPpc)

Net_Orig_Dest_Year_Africa <- Net_Orig_Dest_Year %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

Net_Orig_Dest_Africa <- Net_Orig_Dest_Sum %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

Net_Dest <- Net_Orig_Dest_Sum %>%
  group_by(partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

Net_Dest_Africa <- Net_Orig_Dest_Africa %>%
  group_by(partner, partner.ISO, pRegion, pIncome, pDev, pHDI) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

Net_Orig_Year_Africa <- Net_Orig_Year %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

Net_Orig_Year_LMIC <- Net_Orig_Year %>%
  filter(rIncome == "LIC" | rIncome == "LMC") %>%
  select(-rIncome)

Net_Orig_Year_Developing <- Net_Orig_Year %>%
  filter(rDev == "Developing") %>%
  select(-rDev)

Net_Orig_Year_LowHDI <- Net_Orig_Year %>%
  filter(rHDI == "Lo HDI" | rHDI == "Med HDI") %>%
  select(-rHDI)

Net_Orig_Avg_Africa <- Net_Orig_Avg %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

Net_Orig_Avg_LMIC <- Net_Orig_Avg %>%
  filter(rIncome == "LIC" | rIncome == "LMC") %>%
  select(-rIncome)

Net_Orig_Avg_Developing <- Net_Orig_Avg %>%
  filter(rDev == "Developing") %>%
  select(-rDev)

Net_Orig_Avg_LowHDI <- Net_Orig_Avg %>%
  filter(rHDI == "Lo HDI" | rHDI == "Med HDI") %>%
  select(-rHDI)

Net_Orig_Sum_Africa <- Net_Orig_Sum %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

Net_Year_Africa <- Net_Orig_Year_Africa %>%
  group_by(year) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T),
            GDP = sum(GDP, na.rm = T),
            Total_value = sum(Total_value, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF_GDP = Tot_IFF / GDP,
         Tot_IFF_trade = Tot_IFF / Total_value)

Net_Year_LMIC <- Net_Orig_Year_LMIC %>%
  group_by(year) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T),
            GDP = sum(GDP, na.rm = T),
            Total_value = sum(Total_value, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF_GDP = Tot_IFF / GDP,
         Tot_IFF_trade = Tot_IFF / Total_value)

Net_Year_Developing <- Net_Orig_Year_Developing %>%
  group_by(year) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T),
            GDP = sum(GDP, na.rm = T),
            Total_value = sum(Total_value, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF_GDP = Tot_IFF / GDP,
         Tot_IFF_trade = Tot_IFF / Total_value)

Net_Year_LowHDI <- Net_Orig_Year_LowHDI %>%
  group_by(year) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T),
            GDP = sum(GDP, na.rm = T),
            Total_value = sum(Total_value, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF_GDP = Tot_IFF / GDP,
         Tot_IFF_trade = Tot_IFF / Total_value)

Net_Africa <- Net_Year_Africa %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T))

Net_LMIC <- Net_Year_LMIC %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T))

Net_Developing <- Net_Year_Developing %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T))

Net_LowHDI <- Net_Year_LowHDI %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T))

save(Net_Orig_Dest_Year, file = "Results/Summary data-sets/Net_Orig_Dest_Year.Rdata")
write.csv(Net_Orig_Dest_Year, file = "Results/Summary data-sets/Net_Orig_Dest_Year.csv",
          row.names = F)

save(Net_Orig_Year, file = "Results/Summary data-sets/Net_Orig_Year.Rdata")
write.csv(Net_Orig_Year, file = "Results/Summary data-sets/Net_Orig_Year.csv",
          row.names = F)

save(Net_Orig_Avg, file = "Results/Summary data-sets/Net_Orig_Avg.Rdata")
write.csv(Net_Orig_Avg, file = "Results/Summary data-sets/Net_Orig_Avg.csv",
          row.names = F)
save(Net_Orig_Sum, file = "Results/Summary data-sets/Net_Orig_Sum.Rdata")
write.csv(Net_Orig_Sum, file = "Results/Summary data-sets/Net_Orig_Sum.csv",
          row.names = F)

save(Net_Orig_Dest_Avg, file = "Results/Summary data-sets/Net_Orig_Dest_Avg.Rdata")
write.csv(Net_Orig_Dest_Avg, file = "Results/Summary data-sets/Net_Orig_Dest_Avg.csv",
          row.names = F)
save(Net_Orig_Dest_Year_Africa, file = "Results/Summary data-sets/Net_Orig_Dest_Year_Africa.Rdata")
write.csv(Net_Orig_Dest_Year_Africa, file = "Results/Summary data-sets/Net_Orig_Dest_Year_Africa.csv",
          row.names = F)

save(Net_Orig_Year_Africa, file = "Results/Summary data-sets/Net_Orig_Year_Africa.Rdata")
write.csv(Net_Orig_Year_Africa, file = "Results/Summary data-sets/Net_Orig_Year_Africa.csv",
          row.names = F)
save(Net_Orig_Year_LMIC, file = "Results/Summary data-sets/Net_Orig_Year_LMIC.Rdata")
write.csv(Net_Orig_Year_LMIC, file = "Results/Summary data-sets/Net_Orig_Year_LMIC.csv",
          row.names = F)
save(Net_Orig_Year_Developing, file = "Results/Summary data-sets/Net_Orig_Year_Developing.Rdata")
write.csv(Net_Orig_Year_Developing, file = "Results/Summary data-sets/Net_Orig_Year_Developing.csv",
          row.names = F)
save(Net_Orig_Year_LowHDI, file = "Results/Summary data-sets/Net_Orig_Year_LowHDI.Rdata")
write.csv(Net_Orig_Year_LowHDI, file = "Results/Summary data-sets/Net_Orig_Year_LowHDI.csv",
          row.names = F)

save(Net_Orig_Avg_Africa, file = "Results/Summary data-sets/Net_Orig_Avg_Africa.Rdata")
write.csv(Net_Orig_Avg_Africa, file = "Results/Summary data-sets/Net_Orig_Avg_Africa.csv",
          row.names = F)
save(Net_Orig_Avg_LMIC, file = "Results/Summary data-sets/Net_Orig_Avg_LMIC.Rdata")
write.csv(Net_Orig_Avg_LMIC, file = "Results/Summary data-sets/Net_Orig_Avg_LMIC.csv",
          row.names = F)
save(Net_Orig_Avg_Developing, file = "Results/Summary data-sets/Net_Orig_Avg_Developing.Rdata")
write.csv(Net_Orig_Avg_Developing, file = "Results/Summary data-sets/Net_Orig_Avg_Developing.csv",
          row.names = F)
save(Net_Orig_Avg_LowHDI, file = "Results/Summary data-sets/Net_Orig_Avg_LowHDI.Rdata")
write.csv(Net_Orig_Avg_LowHDI, file = "Results/Summary data-sets/Net_Orig_Avg_LowHDI.csv",
          row.names = F)

save(Net_Orig_Sum_Africa, file = "Results/Summary data-sets/Net_Orig_Sum_Africa.Rdata")
write.csv(Net_Orig_Sum_Africa, file = "Results/Summary data-sets/Net_Orig_Sum_Africa.csv",
          row.names = F)
save(Net_Orig_Dest_Africa, file = "Results/Summary data-sets/Net_Orig_Dest_Africa.Rdata")
write.csv(Net_Orig_Dest_Africa, file = "Results/Summary data-sets/Net_Orig_Dest_Africa.csv",
          row.names = F)

save(Net_Dest, file = "Results/Summary data-sets/Net_Dest.Rdata")
write.csv(Net_Dest, file = "Results/Summary data-sets/Net_Dest.csv",
          row.names = F)
save(Net_Dest_Africa, file = "Results/Summary data-sets/Net_Dest_Africa.Rdata")
write.csv(Net_Dest_Africa, file = "Results/Summary data-sets/Net_Dest_Africa.csv",
          row.names = F)

save(Net_Year_Africa, file = "Results/Summary data-sets/Net_Year_Africa.Rdata")
write.csv(Net_Year_Africa, file = "Results/Summary data-sets/Net_Year_Africa.csv",
          row.names = F)
save(Net_Year_LMIC, file = "Results/Summary data-sets/Net_Year_LMIC.Rdata")
write.csv(Net_Year_LMIC, file = "Results/Summary data-sets/Net_Year_LMIC.csv",
          row.names = F)
save(Net_Year_Developing, file = "Results/Summary data-sets/Net_Year_Developing.Rdata")
write.csv(Net_Year_Developing, file = "Results/Summary data-sets/Net_Year_Developing.csv",
          row.names = F)
save(Net_Year_LowHDI, file = "Results/Summary data-sets/Net_Year_LowHDI.Rdata")
write.csv(Net_Year_LowHDI, file = "Results/Summary data-sets/Net_Year_LowHDI.csv",
          row.names = F)

save(Net_Africa, file = "Results/Summary data-sets/Net_Africa.Rdata")
write.csv(Net_Africa, file = "Results/Summary data-sets/Net_Africa.csv",
          row.names = F)
save(Net_LMIC, file = "Results/Summary data-sets/Net_LMIC.Rdata")
write.csv(Net_LMIC, file = "Results/Summary data-sets/Net_LMIC.csv",
          row.names = F)
save(Net_Developing, file = "Results/Summary data-sets/Net_Developing.Rdata")
write.csv(Net_Developing, file = "Results/Summary data-sets/Net_Developing.csv",
          row.names = F)
save(Net_LowHDI, file = "Results/Summary data-sets/Net_LowHDI.Rdata")
write.csv(Net_LowHDI, file = "Results/Summary data-sets/Net_LowHDI.csv",
          row.names = F)



## ## ## ## ## ## ## ## ## ## ##
# AGGREGATE BY SECTOR       ####
## ## ## ## ## ## ## ## ## ## ##

load(paste0(data.disk, "Data/WDI/WDI.Rdata"))
load(paste0(data.disk, "Data/UN Stats/HS.Rdata"))
panel <- left_join(panel, HS %>% select(chapter, chapter.description),
                   by = c("commodity.code" = "chapter")) %>%
  rename(commodity = chapter.description)


 # .. Aggregate results using Gross Excluding Reversals ####
GER_Imp_Sect <- panel %>%
  filter(Imp_IFF > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI,
           year, 
           section.code, section) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T)) %>%
  ungroup()

GER_Imp_Sect_disag <- panel %>%
  filter(Imp_IFF > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI,
           year, 
           commodity.code, commodity) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T)) %>%
  ungroup()

GER_Exp_Sect <- panel %>%
  filter(pExp_IFF > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI,
           year, 
           section.code, section) %>%
  summarize(Exp_IFF = sum(pExp_IFF, na.rm = T)) %>%
  ungroup()

GER_Exp_Sect_disag <- panel %>%
  filter(pExp_IFF > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI,
           year, 
           commodity.code, commodity) %>%
  summarize(Exp_IFF = sum(pExp_IFF, na.rm = T)) %>%
  ungroup()

GER_Orig_Sect_Year <- full_join(GER_Imp_Sect, GER_Exp_Sect,
                                by = c("reporter" = "reporter",
                                       "reporter.ISO" = "reporter.ISO",
                                       "rRegion" = "rRegion",
                                       "rIncome" = "rIncome",
                                       "rDev" = "rDev",
                                       "rHDI" = "rHDI",
                                       "year" = "year",
                                       "section.code" = "section.code",
                                       "section" = "section"))
rm(GER_Imp_Sect, GER_Exp_Sect)

GER_Orig_Sect_Year_disag <- full_join(GER_Imp_Sect_disag, GER_Exp_Sect_disag,
                                      by = c("reporter" = "reporter",
                                             "reporter.ISO" = "reporter.ISO",
                                             "rRegion" = "rRegion",
                                             "rIncome" = "rIncome",
                                             "rDev" = "rDev",
                                             "rHDI" = "rHDI",
                                             "year" = "year",
                                             "commodity.code" = "commodity.code",
                                             "commodity" = "commodity"))
rm(GER_Imp_Sect_disag, GER_Exp_Sect_disag)

GER_Orig_Sect_Avg <- GER_Orig_Sect_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI, section.code, section) %>%
  summarize(Imp_IFF = mean(Imp_IFF, na.rm = T),
            Exp_IFF = mean(Exp_IFF, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF = Imp_IFF + Exp_IFF,
         Tot_IFF_bn = Tot_IFF / 10^9)
weights <- GER_Orig_Sect_Year %>%
  distinct(reporter.ISO, year, .keep_all = T) %>%
  group_by(reporter.ISO) %>%
  mutate(weight = 1/n()) %>%
  ungroup()
WDI <- left_join(WDI, weights %>% 
                   mutate(year = as.integer(year)) %>%
                   select(reporter.ISO, year, weight),
                 by = c("ISO3166.3" = "reporter.ISO",
                        "year")) %>%
  mutate(weight = ifelse(is.na(weight), 0, weight)) %>%
  group_by(ISO3166.3) %>%
  summarize(GDP = weighted.mean(GDP, weight),
            GNPpc = weighted.mean(GNPpc, weight))
GER_Orig_Sect_Avg <- left_join(GER_Orig_Sect_Avg,
                               WDI,
                               by = c("reporter.ISO" = "ISO3166.3"))

GER_Orig_Sect_Avg_disag <- GER_Orig_Sect_Year_disag %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI, commodity.code, commodity) %>%
  summarize(Imp_IFF = mean(Imp_IFF, na.rm = T),
            Exp_IFF = mean(Exp_IFF, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF = Imp_IFF + Exp_IFF,
         Tot_IFF_bn = Tot_IFF / 10^9)

GER_Orig_Sect_Sum <- GER_Orig_Sect_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, rHDI, section.code, section) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF = Imp_IFF + Exp_IFF,
         Tot_IFF_bn = Tot_IFF / 10^9)

GER_Sect_Year <- GER_Orig_Sect_Year %>%
  group_by(section.code, section, year) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF = Imp_IFF + Exp_IFF,
         Tot_IFF_bn = Tot_IFF / 10^9)

GER_Orig_Sect_Year_Africa <- GER_Orig_Sect_Year %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

GER_Orig_Sect_Avg_Africa <- GER_Orig_Sect_Avg %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

GER_Orig_Sect_Avg_Africa_disag <- GER_Orig_Sect_Avg_disag %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

GER_Orig_Sect_Avg_LMIC <- GER_Orig_Sect_Avg %>%
  filter(rIncome == "LIC" | rIncome == "LMC") %>%
  select(-rIncome)

GER_Orig_Sect_Avg_LMIC_disag <- GER_Orig_Sect_Avg_disag %>%
  filter(rIncome == "LIC" | rIncome == "LMC") %>%
  select(-rIncome)

GER_Orig_Sect_Avg_Developing <- GER_Orig_Sect_Avg %>%
  filter(rDev == "Developing") %>%
  select(-rDev)

GER_Orig_Sect_Avg_Developing_disag <- GER_Orig_Sect_Avg_disag %>%
  filter(rDev == "Developing") %>%
  select(-rDev)

GER_Orig_Sect_Avg_LowHDI <- GER_Orig_Sect_Avg %>%
  filter(rHDI == "Lo HDI" | rHDI == "Med HDI") %>%
  select(-rHDI)

GER_Orig_Sect_Avg_LowHDI_disag <- GER_Orig_Sect_Avg_disag %>%
  filter(rHDI == "Lo HDI" | rHDI == "Med HDI") %>%
  select(-rHDI)

GER_Orig_Sect_Sum_Africa <- GER_Orig_Sect_Sum %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

GER_Orig_Sect_Sum_LMIC <- GER_Orig_Sect_Sum %>%
  filter(rIncome == "LIC" | rIncome == "LMC") %>%
  select(-rIncome)

GER_Orig_Sect_Sum_Developing <- GER_Orig_Sect_Sum %>%
  filter(rDev == "Developing") %>%
  select(-rDev)

GER_Orig_Sect_Sum_LowHDI <- GER_Orig_Sect_Sum %>%
  filter(rHDI == "Lo HDI" | rHDI == "Med HDI") %>%
  select(-rHDI)

GER_Sect_Africa <- GER_Orig_Sect_Sum_Africa %>%
  group_by(section.code, section) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

GER_Sect_LMIC <- GER_Orig_Sect_Sum_LMIC %>%
  group_by(section.code, section) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

GER_Sect_Developing <- GER_Orig_Sect_Sum_Developing %>%
  group_by(section.code, section) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

GER_Sect_LowHDI <- GER_Orig_Sect_Sum_LowHDI %>%
  group_by(section.code, section) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

GER_Sect_Avg_Africa <- GER_Orig_Sect_Avg_Africa %>%
  group_by(section.code, section) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

GER_Sect_Avg_Africa_disag <- GER_Orig_Sect_Avg_Africa_disag %>%
  group_by(commodity.code, commodity) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

GER_Sect_Avg_LMIC <- GER_Orig_Sect_Avg_LMIC %>%
  group_by(section.code, section) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

GER_Sect_Avg_LMIC_disag <- GER_Orig_Sect_Avg_LMIC_disag %>%
  group_by(commodity.code, commodity) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

GER_Sect_Avg_Developing <- GER_Orig_Sect_Avg_Developing %>%
  group_by(section.code, section) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

GER_Sect_Avg_Developing_disag <- GER_Orig_Sect_Avg_Developing_disag %>%
  group_by(commodity.code, commodity) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

GER_Sect_Avg_LowHDI <- GER_Orig_Sect_Avg_LowHDI %>%
  group_by(section.code, section) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

GER_Sect_Avg_LowHDI_disag <- GER_Orig_Sect_Avg_LowHDI_disag %>%
  group_by(commodity.code, commodity) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

save(GER_Orig_Sect_Year_Africa, file = "Results/Summary data-sets/GER_Orig_Sect_Year_Africa.Rdata")
write.csv(GER_Orig_Sect_Year_Africa, file = "Results/Summary data-sets/GER_Orig_Sect_Year_Africa.csv",
          row.names = F)
save(GER_Orig_Sect_Avg, file = "Results/Summary data-sets/GER_Orig_Sect_Avg.Rdata")

write.csv(GER_Orig_Sect_Avg, file = "Results/Summary data-sets/GER_Orig_Sect_Avg.csv",
          row.names = F)
save(GER_Orig_Sect_Avg_Africa, file = "Results/Summary data-sets/GER_Orig_Sect_Avg_Africa.Rdata")
write.csv(GER_Orig_Sect_Avg_Africa, file = "Results/Summary data-sets/GER_Orig_Sect_Avg_Africa.csv",
          row.names = F)
save(GER_Orig_Sect_Sum_Africa, file = "Results/Summary data-sets/GER_Orig_Sect_Sum_Africa.Rdata")
write.csv(GER_Orig_Sect_Sum_Africa, file = "Results/Summary data-sets/GER_Orig_Sect_Sum_Africa.csv",
          row.names = F)

save(GER_Sect_Africa, file = "Results/Summary data-sets/GER_Sect_Africa.Rdata")
write.csv(GER_Sect_Africa, file = "Results/Summary data-sets/GER_Sect_Africa.csv",
          row.names = F)
save(GER_Sect_LMIC, file = "Results/Summary data-sets/GER_Sect_LMIC.Rdata")
write.csv(GER_Sect_LMIC, file = "Results/Summary data-sets/GER_Sect_LMIC.csv",
          row.names = F)
save(GER_Sect_Developing, file = "Results/Summary data-sets/GER_Sect_Developing.Rdata")
write.csv(GER_Sect_Developing, file = "Results/Summary data-sets/GER_Sect_Developing.csv",
          row.names = F)
save(GER_Sect_LowHDI, file = "Results/Summary data-sets/GER_Sect_LowHDI.Rdata")
write.csv(GER_Sect_LowHDI, file = "Results/Summary data-sets/GER_Sect_LowHDI.csv",
          row.names = F)

save(GER_Sect_Avg_Africa, file = "Results/Summary data-sets/GER_Sect_Avg_Africa.Rdata")
write.csv(GER_Sect_Avg_Africa, file = "Results/Summary data-sets/GER_Sect_Avg_Africa.csv",
          row.names = F)
save(GER_Sect_Avg_Africa_disag, file = "Results/Summary data-sets/GER_Sect_Avg_Africa_disag.Rdata")
write.csv(GER_Sect_Avg_Africa_disag, file = "Results/Summary data-sets/GER_Sect_Avg_Africa_disag.csv",
          row.names = F)
save(GER_Sect_Avg_LMIC, file = "Results/Summary data-sets/GER_Sect_Avg_LMIC.Rdata")
write.csv(GER_Sect_Avg_LMIC, file = "Results/Summary data-sets/GER_Sect_Avg_LMIC.csv",
          row.names = F)
save(GER_Sect_Avg_LMIC_disag, file = "Results/Summary data-sets/GER_Sect_Avg_LMIC_disag.Rdata")
write.csv(GER_Sect_Avg_LMIC_disag, file = "Results/Summary data-sets/GER_Sect_Avg_LMIC_disag.csv",
          row.names = F)
save(GER_Sect_Avg_Developing, file = "Results/Summary data-sets/GER_Sect_Avg_Developing.Rdata")
write.csv(GER_Sect_Avg_Developing, file = "Results/Summary data-sets/GER_Sect_Avg_Developing.csv",
          row.names = F)
save(GER_Sect_Avg_Developing_disag, file = "Results/Summary data-sets/GER_Sect_Avg_Developing_disag.Rdata")
write.csv(GER_Sect_Avg_Developing_disag, file = "Results/Summary data-sets/GER_Sect_Avg_Developing_disag.csv",
          row.names = F)
save(GER_Sect_Avg_LowHDI, file = "Results/Summary data-sets/GER_Sect_Avg_LowHDI.Rdata")
write.csv(GER_Sect_Avg_LowHDI, file = "Results/Summary data-sets/GER_Sect_Avg_LowHDI.csv",
          row.names = F)
save(GER_Sect_Avg_LowHDI_disag, file = "Results/Summary data-sets/GER_Sect_Avg_LowHDI_disag.Rdata")
write.csv(GER_Sect_Avg_LowHDI_disag, file = "Results/Summary data-sets/GER_Sect_Avg_LowHDI_disag.csv",
          row.names = F)


# .. Aggregate results using Net Aggregation ####
Net_Orig_Sect_Year <- panel %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rHDI,
           year, section.code, section) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(pExp_IFF, na.rm = T)) %>%
  ungroup()

Net_Orig_Sect_Avg <- Net_Orig_Sect_Year %>%
  group_by(reporter, reporter.ISO, rRegion, section.code, section) %>%
  summarize(Imp_IFF = mean(Imp_IFF, na.rm = T),
            Exp_IFF = mean(Exp_IFF, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF = Imp_IFF + Exp_IFF,
         Tot_IFF_bn = Tot_IFF / 10^9)

Net_Orig_Sect_Sum <- Net_Orig_Sect_Year %>%
  group_by(reporter, reporter.ISO, rRegion, section.code, section) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF = Imp_IFF + Exp_IFF,
         Tot_IFF_bn = Tot_IFF / 10^9)

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
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T),
            Tot_IFF = sum(Tot_IFF, na.rm = T),
            Tot_IFF_bn = sum(Tot_IFF_bn, na.rm = T)) %>%
  ungroup()

save(Net_Orig_Sect_Year_Africa, file = "Results/Summary data-sets/Net_Orig_Sect_Year_Africa.Rdata")
write.csv(GER_Orig_Sect_Year_Africa, file = "Results/Summary data-sets/Net_Orig_Sect_Year_Africa.csv",
          row.names = F)
save(Net_Orig_Sect_Avg_Africa, file = "Results/Summary data-sets/Net_Orig_Sect_Avg_Africa.Rdata")
write.csv(Net_Orig_Sect_Avg_Africa, file = "Results/Summary data-sets/Net_Orig_Sect_Avg_Africa.csv",
          row.names = F)
save(Net_Orig_Sect_Sum_Africa, file = "Results/Summary data-sets/Net_Orig_Sect_Sum_Africa.Rdata")
write.csv(Net_Orig_Sect_Sum_Africa, file = "Results/Summary data-sets/Net_Orig_Sect_Sum_Africa.csv",
          row.names = F)

save(Net_Sect_Africa, file = "Results/Summary data-sets/Net_Sect_Africa.Rdata")
write.csv(Net_Sect_Africa, file = "Results/Summary data-sets/Net_Sect_Africa.csv",
          row.names = F)
