# Compute IFF Estimates
# Alice Lepissier
# alice.lepissier@gmail.com
# Prepared for UNECA

## ## ## ## ## ## ## ## ## ## ##
# INDEX                     ####
## ## ## ## ## ## ## ## ## ## ##
# Preamble
# Create Predictors
# Current Approach
# .. Estimate first stage
# .. Identify and remove outliers
# .. Compute adjusted FOB imports
# .. Estimate second stage
# .. Harmonization procedure
# .. Compute IFF
# .. Move export IFF to mirror
# .. Aggregate results using Gross Excluding Reversals
# Alternative Approach



## ## ## ## ## ## ## ## ## ## ##
# PREAMBLE                  ####
## ## ## ## ## ## ## ## ## ## ##

#setwd("C:/cloudstorage/googledrive/Projects/UN Consultancy/Illicit Financial Flows/IFF estimates") # Alice work
setwd("/home/alice/IFFe/") # Virtual server
library(car)
library(lfe)
library(tidyverse)



## ## ## ## ## ## ## ## ## ## ##
# CREATE PREDICTORS         ####
## ## ## ## ## ## ## ## ## ## ##

#source("Scripts/Data Preparation.R")
load("Data/panel.Rdata")

length(unique(panel$id)) == nrow(panel)
# TRUE, 20864314 obs

panel %>%
  filter(is.finite(pExport) & is.na(pReExport)) %>% nrow
# There is no case where a mirrored Re-Export is missing while
# the mirrored Export exists.

panel <- panel %>%
  mutate(NetExport = Export - ReExport,
         pNetExport = pExport - pReExport,
         ratio_CIF = Import/pNetExport,
         ratio_FOB = NetExport/pImport,
         ln.ratio_CIF = log(ratio_CIF),
         ln.ratio_FOB = log(ratio_FOB)) %>%
  filter(is.finite(ln.ratio_CIF) & is.finite(ln.ratio_FOB))
nrow(panel)
# 4298652

panel <- panel %>% 
  group_by(reporter.ISO, partner.ISO, commodity.code) %>%
  mutate(ln.ratio_CIF_lag = dplyr::lag(ln.ratio_CIF, n = 1),
         ln.ratio_FOB_lag = dplyr::lag(ln.ratio_FOB, n = 1)) %>%
  ungroup() %>%
  filter(is.finite(ln.ratio_CIF_lag) & is.finite(ln.ratio_FOB_lag))
nrow(panel)
# 3841854

panel <- panel %>% 
  group_by(reporter.ISO, partner.ISO, commodity.code) %>%
  mutate(Import_fut = dplyr::lead(Import, n = 1),
         NetExport_fut = dplyr::lead(NetExport, n = 1)) %>%
  ungroup()

panel <- panel %>%
  mutate(FutImport_misrep = Import_fut/Import,
         FutExport_misrep = NetExport_fut/NetExport,
         ReExport_misrep = pReExport/Import) %>%
  filter(is.finite(FutImport_misrep) &
           is.finite(FutExport_misrep) &
           is.finite(ReExport_misrep))
nrow(panel)
# 3462595

panel <- panel %>%
  filter(complete.cases(ratio_CIF, ratio_FOB, 
                        dist, contig, rLandlocked, pLandlocked,
                        FutImport_misrep, FutExport_misrep, ReExport_misrep,
                        ln.ratio_CIF_lag, ln.ratio_FOB_lag))
nrow(panel)
# 3333021

save(panel, file = "Data/panel_clean.Rdata")



## ## ## ## ## ## ## ## ## ## ##
# CURRENT APPROACH          ####
## ## ## ## ## ## ## ## ## ## ##

# .. Estimate first stage ####
fit <- lm(ln.ratio_CIF ~ dist + I(dist^2) +
            ln.ratio_CIF_lag + 
            contig + 
            rLandlocked +
            pLandlocked +
            FutImport_misrep +
            ReExport_misrep +
            factor(year),
          data = panel)
summary(fit)
max(panel$ratio_CIF)
# 2354224577


# .. Identify and remove outliers ####
panel$CD <- cooks.distance(fit)

while(max(panel$CD) > 2){
  panel <- panel %>%
    filter(CD <= 2)
  fit <- lm(ln.ratio_CIF ~ dist + I(dist^2) +
              ln.ratio_CIF_lag + 
              contig + 
              rLandlocked +
              pLandlocked +
              FutImport_misrep +
              ReExport_misrep +
              factor(year),
            data = panel)
  panel$CD <- cooks.distance(fit)
}
nrow(panel)
# 3333016
summary(panel$CD)
max(panel$ratio_CIF)
# 2354224577

Bonferonni.out <- outlierTest(fit, n.max = 10000)

while(length(Bonferonni.out$rstudent != 0)){
  obs <- as.numeric(names(Bonferonni.out[[1]]))
  panel <- panel[-c(obs),]
  fit <- lm(ln.ratio_CIF ~ dist + I(dist^2) +
              ln.ratio_CIF_lag + 
              contig + 
              rLandlocked +
              pLandlocked +
              FutImport_misrep +
              ReExport_misrep +
              factor(year),
            data = panel)
  Bonferonni.out <- outlierTest(fit, n.max = 10)
}

obs <- as.numeric(names(Bonferonni.out[[1]]))
outliers <- panel[c(obs), ]
mean(outliers$ratio_CIF)
# 187749367
mean(panel$ratio_CIF)
# 3315.328
panel <- panel[-c(obs),]
rm(Bonferonni.out, outliers, obs)
mean(panel$ratio_CIF)
# 327.4577

panel %>% filter(ratio_CIF > 10^4) %>% nrow
# 5902
panel <- panel %>% 
  filter(ratio_CIF < 10^4)
nrow(panel)
# 3764261
mean(panel$ratio_CIF)
# 21.923

fit <- lm(ln.ratio_CIF ~ dist + I(dist^2) + 
            contig + 
            rLandlocked +
            pLandlocked +
            FutImport_misrep +
            ReExport_misrep,
          data = panel)
summary(fit)


# .. Compute adjusted FOB imports ####
panel$fitted <- exp(fitted(fit))

panel <- panel %>%
  mutate(fitted_adj = ifelse(fitted < 1, 1, fitted),
         resid_adj = ratio_CIF - fitted_adj)

panel <- panel %>%
  mutate(FOB_Import = pNetExport + (resid_adj * pNetExport))

save(panel, file = "Results/Intermediate/panel_1st_stage_current.Rdata")


# .. Estimate second stage ####
panel <- panel %>%
  mutate(rep_dist = abs(log(pNetExport/FOB_Import))) %>%
  filter(is.finite(rep_dist))
nrow(panel)
# 179524

panel <- panel %>%
  filter(year > 1999)
nrow(panel)
# 174617

FE.out <- felm(rep_dist ~ 0| factor(reporter.ISO) + 
                 factor(partner.ISO) + factor(year),
               data = panel)
FE <- getfe(FE.out, se = T) 

FE <- FE %>%
  group_by(fe) %>%
  mutate(min = min(effect)) %>%
  ungroup()

FE$sigma <- pi/2*(FE$effect - (FE$min + 2*FE$se))


# .. Harmonization procedure ####
panel <- left_join(panel, FE %>% 
                     filter(fe == "factor(reporter.ISO)") %>%
                     select(idx, sigma) %>%
                     mutate(idx = as.character(idx)),
                   by = c("reporter.ISO" = "idx")) %>%
  rename(rSigma = sigma)

panel <- left_join(panel, FE %>% 
                     filter(fe == "factor(partner.ISO)") %>%
                     select(idx, sigma) %>%
                     mutate(idx = as.character(idx)),
                   by = c("partner.ISO" = "idx")) %>%
  rename(pSigma = sigma)

panel <- panel %>%
  mutate(w_M = (exp(rSigma^2)*(exp(rSigma^2) - 1))/(exp(rSigma^2)*(exp(rSigma^2)- 1) + exp(pSigma^2)*(exp(pSigma^2) - 1)),
         w_X = (exp(pSigma^2)*(exp(pSigma^2) - 1))/(exp(rSigma^2)*(exp(rSigma^2)- 1) + exp(pSigma^2)*(exp(pSigma^2) - 1)))
summary(panel$w_M)
summary(panel$w_X)

panel <- panel %>%
  mutate(w = w_M + w_X)
summary(panel$w)

panel <- panel %>%
  mutate(RV = w_M*FOB_Import + w_X*pNetExport)


# .. Compute IFF ####
panel <- panel %>%
  mutate(Imp_IFF = FOB_Import - RV,
         Exp_IFF = RV - pNetExport)
summary(panel$Imp_IFF)
summary(panel$Exp_IFF)


# .. Move export IFF to mirror ####
panel_mirror <- panel %>%
  select(reporter, reporter.ISO, rRegion, rIncome,
         partner, partner.ISO, pRegion, pIncome,
         commodity.code, year,
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
                          "partner.ISO" = "reporter.ISO",
                          "partner" = "reporter",
                          "pRegion" = "rRegion",
                          "pIncome" = "rIncome",
                          "year" = "year",
                          "commodity.code" = "commodity.code"))

rm(panel_mirror)
test_current <- panel %>% filter(id == "CAN_HKG_94_2012")


# .. Aggregate results using Gross Excluding Reversals ####
GER_Imp_current <- panel %>%
  filter(Imp_IFF > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome,
           partner, partner.ISO, pRegion, pIncome,
           year) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T)) %>%
  ungroup()

GER_Exp_current <- panel %>%
  filter(pExp_IFF > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome,
           partner, partner.ISO, pRegion, pIncome,
           year) %>%
  summarize(Exp_IFF = sum(pExp_IFF, na.rm = T)) %>%
  ungroup()

Orig_Dest_Year_current <- full_join(GER_Imp_current, GER_Exp_current,
                            by = c("reporter" = "reporter",
                                   "reporter.ISO" = "reporter.ISO",
                                   "rRegion" = "rRegion",
                                   "rIncome" = "rIncome",
                                   "partner" = "partner",
                                   "partner.ISO" = "partner.ISO",
                                   "pRegion" = "pRegion",
                                   "pIncome" = "pIncome",
                                   "year" = "year"))

Orig_Year_current <- Orig_Dest_Year_current %>%
  group_by(reporter, reporter.ISO, rRegion, year) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T)) %>%
  ungroup()

Africa_current <- Orig_Year_current %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

Africa_agg_current <- Africa_current %>%
  group_by(year) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T)) %>%
  ungroup()

save(Africa_current, file = "Results/Africa_current.Rdata")
write.csv(Africa_current, file = "Results/Africa_current approach.csv",
          row.names = F)
write.csv(Africa_agg_current, file = "Results/Africa_agg_current approach.csv",
          row.names = F)

panel_current <- panel



## ## ## ## ## ## ## ## ## ## ##
# ALTERNATIVE APPROACH      ####
## ## ## ## ## ## ## ## ## ## ##

rm(panel)
load("Data/panel_clean.Rdata")


# .. For mis-invoiced imports ####

# .... Estimate first stage ####
fit.CIF <- lm(ln.ratio_CIF ~ dist + I(dist^2) + 
                contig + 
                rLandlocked +
                pLandlocked +
                FutImport_misrep +
                ReExport_misrep,
              data = panel)
summary(fit.CIF)


# .... Identify and remove outliers ####
Bonferonni.out <- outlierTest(fit.CIF, n.max = 100)
obs <- as.numeric(names(Bonferonni.out[[1]]))
outliers <- panel[c(obs), ]
mean(outliers$ratio_CIF)
# 187749367
mean(panel$ratio_CIF)
# 3315.328
panel <- panel[-c(obs),]
rm(Bonferonni.out, outliers, obs)
mean(panel$ratio_CIF)
# 327.4577

panel %>% filter(ratio_CIF > 10^4) %>% nrow
# 5902
panel <- panel %>% 
  filter(ratio_CIF < 10^4)
nrow(panel)
# 3764261
mean(panel$ratio_CIF)
# 21.923

fit.CIF <- lm(ln.ratio_CIF ~ dist + I(dist^2) + 
                contig + 
                rLandlocked +
                pLandlocked +
                FutImport_misrep +
                ReExport_misrep,
              data = panel)
summary(fit.CIF)


# .... Compute FOB imports ####
panel$fitted_CIF <- exp(fitted(fit.CIF))

panel <- panel %>%
  mutate(FOB_Import = fitted_CIF * pNetExport)

save(panel, file = "Results/Intermediate/panel_1st_stage_Imp.Rdata")


# .... Estimate second stage ####
panel <- panel %>%
  mutate(rep_dist = abs(log(pNetExport/FOB_Import))) %>%
  filter(is.finite(rep_dist))
nrow(panel)
# 3764248

panel <- panel %>%
  filter(year > 1999)
nrow(panel)
# 3612787

panel <- panel %>%
  mutate_at(vars(reporter.ISO, partner.ISO, year),
            funs(as.factor(.)))

FE.out <- felm(rep_dist ~ 0| reporter.ISO +
                 partner.ISO + year,
               data = panel)
FE_Imp <- getfe(FE.out, se = T)

# options(contrasts = rep ("contr.sum", 2))
# X <- model.matrix(~ -1 + panel$reporter.ISO + 
#                     panel$partner.ISO + 
#                     panel$year)
# attr(X,"contrasts")
# FE.out <- solve(t(X) %*% X) %*% t(X) %*% panel$rep_dist

FE_Imp <- FE_Imp %>%
  group_by(fe) %>%
  mutate(min = min(effect)) %>%
  ungroup()

FE_Imp$sigma <- pi/2*(FE_Imp$effect - (FE_Imp$min + 2*FE_Imp$se))

panel <- panel %>%
  mutate_at(vars(reporter.ISO, partner.ISO, year),
            funs(as.character(.)))


# .... Harmonization procedure ####
panel <- left_join(panel, FE_Imp %>% 
                     filter(fe == "reporter.ISO") %>%
                     select(idx, sigma) %>%
                     mutate(idx = as.character(idx)),
                   by = c("reporter.ISO" = "idx")) %>%
  rename(rSigma = sigma)

panel <- left_join(panel, FE_Imp %>% 
                     filter(fe == "partner.ISO") %>%
                     select(idx, sigma) %>%
                     mutate(idx = as.character(idx)),
                   by = c("partner.ISO" = "idx")) %>%
  rename(pSigma = sigma)

panel <- panel %>%
  mutate(w_M = (exp(rSigma^2)*(exp(rSigma^2) - 1))/(exp(rSigma^2)*(exp(rSigma^2)- 1) + exp(pSigma^2)*(exp(pSigma^2) - 1)),
         w_X = (exp(pSigma^2)*(exp(pSigma^2) - 1))/(exp(rSigma^2)*(exp(rSigma^2)- 1) + exp(pSigma^2)*(exp(pSigma^2) - 1)))
summary(panel$w_M)
summary(panel$w_X)

panel <- panel %>%
  mutate(w = w_M + w_X)
summary(panel$w)

panel <- panel %>%
  mutate(RV = w_M*FOB_Import + w_X*pNetExport)


# .... Compute mis-invoiced import ####
panel <- panel %>%
  mutate(Imp_IFF = Import - RV)
summary(panel$Imp_IFF)

panel_Imp <- panel
test_alternate_Imp <- panel %>% filter(id == "CAN_HKG_94_2012")


# .. For mis-invoiced exports ####
rm(panel)
load("Data/panel_clean.Rdata")


# .... Estimate first stage ####
fit.FOB <- lm(ln.ratio_FOB ~ dist + I(dist^2) + 
                contig + 
                rLandlocked +
                pLandlocked +
                FutExport_misrep,
              data = panel)
summary(fit.FOB)


# .... Identify and remove outliers ####
Bonferonni.out <- outlierTest(fit.FOB, n.max = 100)
obs <- as.numeric(names(Bonferonni.out[[1]]))
outliers <- panel[c(obs), ]
mean(outliers$ratio_FOB)
# 7586215
mean(panel$ratio_FOB)
# 265.6377 
panel <- panel[-c(obs),]
rm(Bonferonni.out, outliers, obs)
mean(panel$ratio_FOB)
# 64.4255

panel %>% filter(ratio_FOB > 10^4) %>% nrow
# 2756
panel <- panel %>% 
  filter(ratio_FOB < 10^4)
nrow(panel)
# 3767368
mean(panel$ratio_FOB)
# 13.39388

fit.FOB <- lm(ln.ratio_FOB ~ dist + I(dist^2) + 
                contig + 
                rLandlocked +
                pLandlocked +
                FutExport_misrep,
              data = panel)
summary(fit.FOB)


# .... Compute CIF exports ####
panel$fitted_FOB <- exp(fitted(fit.FOB))

panel <- panel %>%
  mutate(CIF_Export = fitted_FOB * pImport)

save(panel, file = "Results/Intermediate/panel_1st_stage_Exp.Rdata")


# .... Estimate second stage ####
panel <- panel %>%
  mutate(rep_dist = abs(log(pImport/CIF_Export))) %>%
  filter(is.finite(rep_dist))
nrow(panel)
# 3767365

panel <- panel %>%
  filter(year > 1999)
nrow(panel)
# 3615900

panel <- panel %>%
  mutate_at(vars(reporter.ISO, partner.ISO, year),
            funs(as.factor(.)))

FE.out <- felm(rep_dist ~ 0| reporter.ISO +
                 partner.ISO + year,
               data = panel)
FE_Exp <- getfe(FE.out, se = T)

# options(contrasts = rep ("contr.sum", 2))
# X <- model.matrix(~ -1 + panel$reporter.ISO + 
#                     panel$partner.ISO + 
#                     panel$year)
# attr(X,"contrasts")
# FE.out <- solve(t(X) %*% X) %*% t(X) %*% panel$rep_dist

FE_Exp <- FE_Exp %>%
  group_by(fe) %>%
  mutate(min = min(effect)) %>%
  ungroup()

FE_Exp$sigma <- pi/2*(FE_Exp$effect - (FE_Exp$min + 2*FE_Exp$se))

panel <- panel %>%
  mutate_at(vars(reporter.ISO, partner.ISO, year),
            funs(as.character(.)))


# .... Harmonization procedure ####
panel <- left_join(panel, FE_Exp %>% 
                     filter(fe == "reporter.ISO") %>%
                     select(idx, sigma) %>%
                     mutate(idx = as.character(idx)),
                   by = c("reporter.ISO" = "idx")) %>%
  rename(rSigma = sigma)

panel <- left_join(panel, FE_Exp %>% 
                     filter(fe == "partner.ISO") %>%
                     select(idx, sigma) %>%
                     mutate(idx = as.character(idx)),
                   by = c("partner.ISO" = "idx")) %>%
  rename(pSigma = sigma)

panel <- panel %>%
  mutate(w_X = (exp(rSigma^2)*(exp(rSigma^2) - 1))/(exp(rSigma^2)*(exp(rSigma^2)- 1) + exp(pSigma^2)*(exp(pSigma^2) - 1)),
         w_M = (exp(pSigma^2)*(exp(pSigma^2) - 1))/(exp(rSigma^2)*(exp(rSigma^2)- 1) + exp(pSigma^2)*(exp(pSigma^2) - 1)))
summary(panel$w_M)
summary(panel$w_X)

panel <- panel %>%
  mutate(w = w_M + w_X)
summary(panel$w)

panel <- panel %>%
  mutate(RV = w_M*pImport + w_X*CIF_Export)


# .... Compute mis-invoiced exports ####
panel <- panel %>%
  mutate(Exp_IFF = RV - NetExport)
summary(panel$Exp_IFF)

panel_Exp <- panel
test_alternate_Exp <- panel %>% filter(id == "CAN_HKG_94_2012")


# .. Merge IFF for imports and exports ####
panel <- full_join(panel_Imp %>%
                     select(reporter, reporter.ISO, rRegion, rIncome,
                            partner, partner.ISO, pRegion, pIncome,
                            year, commodity.code, Imp_IFF),
                   panel_Exp %>%
                     select(reporter, reporter.ISO, rRegion, rIncome,
                            partner, partner.ISO, pRegion, pIncome,
                            year, commodity.code, Exp_IFF),
                   by = c("reporter" = "reporter",
                          "reporter.ISO" = "reporter.ISO",
                          "rRegion" = "rRegion",
                          "rIncome" = "rIncome",
                          "partner" = "partner",
                          "partner.ISO" = "partner.ISO",
                          "pRegion" = "pRegion",
                          "pIncome" = "pIncome",
                          "year" = "year",
                          "commodity.code" = "commodity.code"))

panel$id <- paste(panel$reporter.ISO, 
                  panel$partner.ISO, 
                  panel$commodity.code, 
                  panel$year, sep = "_")

panel %>%
  filter(duplicated(panel$id)) %>% nrow
# 0

test_alternate <- left_join(test_alternate_Imp, test_alternate_Exp %>%
                              select(id, Exp_IFF),
                            by = c("id" = "id"))


# .. Aggregate results using Gross Excluding Reversals ####

GER_Imp_alt <- panel %>%
  filter(Imp_IFF > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome,
           partner, partner.ISO, pRegion, pIncome,
           year) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T)) %>%
  ungroup()

GER_Exp_alt <- panel %>%
  filter(Exp_IFF > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome,
           partner, partner.ISO, pRegion, pIncome,
           year) %>%
  summarize(Exp_IFF = sum(Exp_IFF, na.rm = T)) %>%
  ungroup()

Orig_Dest_Year_alt <- full_join(GER_Imp_alt, GER_Exp_alt,
                            by = c("reporter" = "reporter",
                                   "reporter.ISO" = "reporter.ISO",
                                   "rRegion" = "rRegion",
                                   "rIncome" = "rIncome",
                                   "partner" = "partner",
                                   "partner.ISO" = "partner.ISO",
                                   "pRegion" = "pRegion",
                                   "pIncome" = "pIncome",
                                   "year" = "year"))

Orig_Year_alt <- Orig_Dest_Year_alt %>%
  group_by(reporter, reporter.ISO, rRegion, year) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T)) %>%
  ungroup()

Africa_alternate <- Orig_Year_alt %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

Africa_agg_alt <- Africa_alternate %>%
  group_by(year) %>%
  summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
            Exp_IFF = sum(Exp_IFF, na.rm = T)) %>%
  ungroup()

save(Africa_alternate, file = "Results/Africa_alternate.Rdata")
write.csv(Africa_alternate, file = "Results/Africa_alternate approach.csv",
          row.names = F)
write.csv(Africa_agg_alt, file = "Results/Africa_agg_alternate approach.csv",
          row.names = F)