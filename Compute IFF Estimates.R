# Compute IFF Estimates
# Alice Lepissier
# alice.lepissier@gmail.com
# Prepared for UNECA

## ## ## ## ## ## ## ## ## ## ##
# INDEX                     ####
## ## ## ## ## ## ## ## ## ## ##
# Preamble
# Create Predictors
# Approach 1
# .. Estimate first stage
# .. Identify and remove outliers
# .. Compute fitted values when non-IFF predictors are 0
# .. Compute adjusted FOB imports
# .. Estimate second stage
# .. Harmonization procedure
# .. Compute IFF
# .. Move export IFF to mirror
# .. Aggregate results using Gross Excluding Reversals
# .. Aggregate results using Net Aggregation
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
                        dist, contig, rLandlocked, pLandlocked, tariff,
                        FutImport_misrep, FutExport_misrep, ReExport_misrep,
                        ln.ratio_CIF_lag, ln.ratio_FOB_lag))
nrow(panel)
# 1295364

panel <- panel %>%
  mutate(dist.sq = I(dist^2))

save(panel, file = "Data/panel_clean.Rdata")



## ## ## ## ## ## ## ## ## ## ##
# APPROACH 1                ####
## ## ## ## ## ## ## ## ## ## ##

# .. Estimate first stage ####
options(contrasts = rep ("contr.treatment", 2))
fit <- lm(ln.ratio_CIF ~ dist + dist.sq +
            contig + 
            rLandlocked +
            pLandlocked +
            FutImport_misrep +
            ReExport_misrep +
            ln.ratio_CIF_lag +
            tariff,
          data = panel)
summary(fit)
max(panel$ratio_CIF)
# 1124495105
mean(panel$ratio_CIF)
# 2197.805


# .. Identify and remove outliers ####
panel$CD <- cooks.distance(fit)

while(max(panel$CD) > 2){
  panel <- panel %>%
    filter(CD <= 2)
  fit <- lm(ln.ratio_CIF ~ dist + dist.sq +
              contig + 
              rLandlocked +
              pLandlocked +
              FutImport_misrep +
              ReExport_misrep +
              ln.ratio_CIF_lag +
              tariff,
            data = panel)
  panel$CD <- cooks.distance(fit)
}
nrow(panel)
# 1295353
summary(panel$CD)

Bonferonni.out <- outlierTest(fit, n.max = 10000)
obs <- as.numeric(names(Bonferonni.out[[1]]))
outliers <- panel[c(obs), ]
mean(outliers$ratio_CIF)
# 753961.1
mean(panel$ratio_CIF)
# 2197.824
panel <- panel[-c(obs),]
mean(panel$ratio_CIF)
# 40.27702

fit <- lm(ln.ratio_CIF ~ dist + dist.sq +
            contig + 
            rLandlocked +
            pLandlocked +
            FutImport_misrep +
            ReExport_misrep +
            ln.ratio_CIF_lag +
            tariff,
          data = panel)
Bonferonni.out <- outlierTest(fit, n.max = 10000)
obs <- as.numeric(names(Bonferonni.out[[1]]))
outliers <- panel[c(obs), ]
mean(outliers$ratio_CIF)
# 3578.59
mean(panel$ratio_CIF)
# 40.27702
panel <- panel[-c(obs),]
mean(panel$ratio_CIF)
# 36.83849

fit <- lm(ln.ratio_CIF ~ dist + dist.sq +
            contig + 
            rLandlocked +
            pLandlocked +
            FutImport_misrep +
            ReExport_misrep +
            ln.ratio_CIF_lag +
            tariff,
          data = panel)
Bonferonni.out <- outlierTest(fit, n.max = 10000)
obs <- as.numeric(names(Bonferonni.out[[1]]))
outliers <- panel[c(obs), ]
mean(outliers$ratio_CIF)
# 2625.254
mean(panel$ratio_CIF)
# 36.83849
panel <- panel[-c(obs),]
mean(panel$ratio_CIF)
# 36.10613

rm(Bonferonni.out, outliers, obs)

summary(panel$ratio_CIF)
panel %>% filter(ratio_CIF > 10^4) %>% nrow
# 439
panel <- panel %>% 
  filter(ratio_CIF < 10^4)
nrow(panel)
# 1289588
mean(panel$ratio_CIF)
# 13.13475
summary(panel$ratio_CIF)

fit <- lm(ln.ratio_CIF ~ dist + dist.sq +
            contig + 
            rLandlocked +
            pLandlocked +
            FutImport_misrep +
            ReExport_misrep +
            ln.ratio_CIF_lag +
            tariff,
          data = panel)
summary(fit)


# .. Compute fitted values when non-IFF predictors are 0 ####
fit_IFF <- lm(ratio_CIF ~ - 1 + tariff,
              data = panel)
panel$fitted_IFF_lm <- predict(fit_IFF)

coef <- coef(fit)
for (v in 1:length(coef)){
  if (names(coef[v]) == "tariff"){
    coef[v] <- coef(fit)["tariff"]
  } else {
    coef[v] <- 0
  }
}
coef
panel$fitted_IFF_man <- exp(model.matrix(fit) %*% coef)
attr(panel$fitted_IFF_man, "dimnames") <- NULL
rm(coef, v)

panel$fitted_IFF_pred <- exp(predict(update(fit, . ~ . - 1),
                                     newdata = data.frame(dist = 0,
                                                          dist.sq = 0,
                                                          contig = "0",
                                                          rLandlocked = "0",
                                                          pLandlocked = "0",
                                                          FutImport_misrep = 0,
                                                          ReExport_misrep = 0,
                                                          ln.ratio_CIF_lag = 0,
                                                          tariff = panel$tariff)))

summary(panel$fitted_IFF_lm)
summary(panel$fitted_IFF_man)
summary(panel$fitted_IFF_pred)

panel$fitted_IFF <- panel$fitted_IFF_man


# .. Compute adjusted FOB imports ####
panel$fitted <- exp(fitted(fit))
panel$resid <- exp(resid(fit))

panel <- panel %>%
  mutate(fitted_adj = ifelse(fitted < 1, 1, fitted),
         resid_adj = ratio_CIF - fitted_adj)

panel <- panel %>%
  mutate(FOB_Import = pNetExport + pNetExport * (resid_adj),
         FOB_Import_IFF_hi = pNetExport + pNetExport * (resid_adj + fitted_IFF),
         FOB_Import_IFF_lo = pNetExport + pNetExport * (fitted_IFF))

panel <- panel %>%
  mutate(FOB_Import_AL = Import/fitted,
         FOB_Import_WD = pNetExport + (pNetExport * resid / fitted))
panel$FOB_Import_AL == panel$FOB_Import_WD

save(panel, file = "Results/Intermediate/panel_1st_stage_A1.Rdata")


# .. Estimate second stage ####
panel <- panel %>%
  mutate(rep_dist = abs(log(pNetExport/FOB_Import))) %>%
  filter(is.finite(rep_dist))
nrow(panel)
# 1200218

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
  mutate(w_M = (exp(rSigma^2)*(exp(rSigma^2) - 1))/(exp(rSigma^2)*(exp(rSigma^2)- 1) + exp(pSigma^2)*(exp(pSigma^2) - 1)),
         w_X = (exp(pSigma^2)*(exp(pSigma^2) - 1))/(exp(rSigma^2)*(exp(rSigma^2)- 1) + exp(pSigma^2)*(exp(pSigma^2) - 1)))
summary(panel$w_M)
summary(panel$w_X)

panel <- panel %>%
  mutate(w = w_M + w_X)
summary(panel$w)

panel <- panel %>%
  mutate(RV = w_X*pNetExport + w_M*FOB_Import)


# .. Compute IFF ####
panel <- panel %>%
  mutate(Imp_IFF_hi = FOB_Import_IFF_hi - RV,
         Exp_IFF_hi = RV - pNetExport,
         Imp_IFF_lo = FOB_Import_IFF_lo - RV,
         Exp_IFF_lo = RV - pNetExport)
summary(panel$Imp_IFF_hi)
summary(panel$Exp_IFF_hi)
summary(panel$Imp_IFF_lo)
summary(panel$Exp_IFF_lo)


# .. Move export IFF to mirror ####
panel_mirror <- panel %>%
  select(reporter, reporter.ISO, rRegion, rIncome,
         partner, partner.ISO, pRegion, pIncome,
         commodity.code, year,
         Exp_IFF_hi, Exp_IFF_lo)

panel_mirror$id <- paste(panel_mirror$partner.ISO, 
                         panel_mirror$reporter.ISO, 
                         panel_mirror$commodity.code,
                         panel_mirror$year, sep = "_")

panel_mirror <- panel_mirror %>% 
  rename(pExp_IFF_hi = Exp_IFF_hi,
         pExp_IFF_lo = Exp_IFF_lo)

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

panel %>%
  filter(duplicated(panel$id)) %>% nrow
# 0
rm(panel_mirror)


# .. Aggregate results using Gross Excluding Reversals ####
GER_Imp_A1_hi <- panel %>%
  filter(Imp_IFF_hi > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome,
           partner, partner.ISO, pRegion, pIncome,
           year) %>%
  summarize(Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T)) %>%
  ungroup()

GER_Imp_A1_lo <- panel %>%
  filter(Imp_IFF_lo > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome,
           partner, partner.ISO, pRegion, pIncome,
           year) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T)) %>%
  ungroup()

GER_Imp_A1 <- full_join(GER_Imp_A1_lo, GER_Imp_A1_hi,
                        by = c("reporter" = "reporter",
                               "reporter.ISO" = "reporter.ISO",
                               "rRegion" = "rRegion",
                               "rIncome" = "rIncome",
                               "partner" = "partner",
                               "partner.ISO" = "partner.ISO",
                               "pRegion" = "pRegion",
                               "pIncome" = "pIncome",
                               "year" = "year"))

GER_Exp_A1_hi <- panel %>%
  filter(pExp_IFF_hi > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome,
           partner, partner.ISO, pRegion, pIncome,
           year) %>%
  summarize(Exp_IFF_hi = sum(pExp_IFF_hi, na.rm = T)) %>%
  ungroup()

GER_Exp_A1_lo <- panel %>%
  filter(pExp_IFF_lo > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome,
           partner, partner.ISO, pRegion, pIncome,
           year) %>%
  summarize(Exp_IFF_lo = sum(pExp_IFF_lo, na.rm = T)) %>%
  ungroup()

GER_Exp_A1 <- full_join(GER_Exp_A1_lo, GER_Exp_A1_hi,
                        by = c("reporter" = "reporter",
                               "reporter.ISO" = "reporter.ISO",
                               "rRegion" = "rRegion",
                               "rIncome" = "rIncome",
                               "partner" = "partner",
                               "partner.ISO" = "partner.ISO",
                               "pRegion" = "pRegion",
                               "pIncome" = "pIncome",
                               "year" = "year"))

GER_Orig_Dest_Year_A1 <- full_join(GER_Imp_A1, GER_Exp_A1,
                                  by = c("reporter" = "reporter",
                                         "reporter.ISO" = "reporter.ISO",
                                         "rRegion" = "rRegion",
                                         "rIncome" = "rIncome",
                                         "partner" = "partner",
                                         "partner.ISO" = "partner.ISO",
                                         "pRegion" = "pRegion",
                                         "pIncome" = "pIncome",
                                         "year" = "year"))

GER_Orig_Year_A1 <- GER_Orig_Dest_Year_A1 %>%
  group_by(reporter, reporter.ISO, rRegion, year) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T)) %>%
  ungroup()

GER_Orig_Year_Africa_A1 <- GER_Orig_Year_A1 %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

GER_Year_Africa_A1 <- GER_Orig_Year_Africa_A1 %>%
  group_by(year) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T)) %>%
  ungroup()

write.csv(GER_Orig_Year_Africa_A1, file = "Results/Approach 1/GER_Orig_Year_Africa_A1.csv",
          row.names = F)
write.csv(GER_Year_Africa_A1, file = "Results/Approach 1/GER_Year_Africa_A1.csv",
          row.names = F)


# .. Aggregate results using Net Aggregation ####
Net_Orig_Dest_Year_A1 <- panel %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome,
           partner, partner.ISO, pRegion, pIncome,
           year) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T)) %>%
  ungroup()

Net_Orig_Year_A1 <- Net_Orig_Dest_Year_A1 %>%
  group_by(reporter, reporter.ISO, rRegion, year) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T)) %>%
  ungroup()

Net_Orig_Year_Africa_A1 <- Net_Orig_Year_A1 %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

Net_Year_Africa_A1 <- Net_Orig_Year_Africa_A1 %>%
  group_by(year) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T)) %>%
  ungroup()

write.csv(Net_Orig_Year_Africa_A1, file = "Results/Approach 1/Net_Orig_Year_Africa_A1.csv",
          row.names = F)
write.csv(Net_Year_Africa_A1, file = "Results/Approach 1/Net_Year_Africa_A1.csv",
          row.names = F)

save(panel, file = "Results/Intermediate/panel_2nd_stage_A1.Rdata")



# ## ## ## ## ## ## ## ## ## ## ##
# # ALTERNATIVE APPROACH      ####
# ## ## ## ## ## ## ## ## ## ## ##
# 
# rm(panel)
# load("Data/panel_clean.Rdata")
# 
# 
# # .. For mis-invoiced imports ####
# 
# # .... Estimate first stage ####
# fit.CIF <- lm(ln.ratio_CIF ~ dist + I(dist^2) + 
#                 contig + 
#                 rLandlocked +
#                 pLandlocked +
#                 FutImport_misrep +
#                 ReExport_misrep,
#               data = panel)
# summary(fit.CIF)
# 
# 
# # .... Identify and remove outliers ####
# Bonferonni.out <- outlierTest(fit.CIF, n.max = 100)
# obs <- as.numeric(names(Bonferonni.out[[1]]))
# outliers <- panel[c(obs), ]
# mean(outliers$ratio_CIF)
# # 187749367
# mean(panel$ratio_CIF)
# # 3315.328
# panel <- panel[-c(obs),]
# rm(Bonferonni.out, outliers, obs)
# mean(panel$ratio_CIF)
# # 327.4577
# 
# panel %>% filter(ratio_CIF > 10^4) %>% nrow
# # 5902
# panel <- panel %>% 
#   filter(ratio_CIF < 10^4)
# nrow(panel)
# # 3764261
# mean(panel$ratio_CIF)
# # 21.923
# 
# fit.CIF <- lm(ln.ratio_CIF ~ dist + I(dist^2) + 
#                 contig + 
#                 rLandlocked +
#                 pLandlocked +
#                 FutImport_misrep +
#                 ReExport_misrep,
#               data = panel)
# summary(fit.CIF)
# 
# 
# # .... Compute FOB imports ####
# panel$fitted_CIF <- exp(fitted(fit.CIF))
# 
# panel <- panel %>%
#   mutate(FOB_Import = fitted_CIF * pNetExport)
# 
# save(panel, file = "Results/Intermediate/panel_1st_stage_Imp.Rdata")
# 
# 
# # .... Estimate second stage ####
# panel <- panel %>%
#   mutate(rep_dist = abs(log(pNetExport/FOB_Import))) %>%
#   filter(is.finite(rep_dist))
# nrow(panel)
# # 3764248
# 
# panel <- panel %>%
#   filter(year > 1999)
# nrow(panel)
# # 3612787
# 
# panel <- panel %>%
#   mutate_at(vars(reporter.ISO, partner.ISO, year),
#             funs(as.factor(.)))
# 
# FE.out <- felm(rep_dist ~ 0| reporter.ISO +
#                  partner.ISO + year,
#                data = panel)
# FE_Imp <- getfe(FE.out, se = T)
# 
# options(contrasts = rep ("contr.sum", 2))
# X <- model.matrix(~ -1 + panel$reporter.ISO +
#                     panel$partner.ISO +
#                     panel$year)
# attr(X,"contrasts")
# FE.out2 <- solve(t(X) %*% X) %*% t(X) %*% panel$rep_dist
# 
# FE_Imp <- FE_Imp %>%
#   group_by(fe) %>%
#   mutate(min = min(effect)) %>%
#   ungroup()
# 
# FE_Imp$sigma <- pi/2*(FE_Imp$effect - (FE_Imp$min + 2*FE_Imp$se))
# 
# panel <- panel %>%
#   mutate_at(vars(reporter.ISO, partner.ISO, year),
#             funs(as.character(.)))
# 
# 
# # .... Harmonization procedure ####
# panel <- left_join(panel, FE_Imp %>% 
#                      filter(fe == "reporter.ISO") %>%
#                      select(idx, sigma) %>%
#                      mutate(idx = as.character(idx)),
#                    by = c("reporter.ISO" = "idx")) %>%
#   rename(rSigma = sigma)
# 
# panel <- left_join(panel, FE_Imp %>% 
#                      filter(fe == "partner.ISO") %>%
#                      select(idx, sigma) %>%
#                      mutate(idx = as.character(idx)),
#                    by = c("partner.ISO" = "idx")) %>%
#   rename(pSigma = sigma)
# 
# panel <- panel %>%
#   mutate(w_M = (exp(rSigma^2)*(exp(rSigma^2) - 1))/(exp(rSigma^2)*(exp(rSigma^2)- 1) + exp(pSigma^2)*(exp(pSigma^2) - 1)),
#          w_X = (exp(pSigma^2)*(exp(pSigma^2) - 1))/(exp(rSigma^2)*(exp(rSigma^2)- 1) + exp(pSigma^2)*(exp(pSigma^2) - 1)))
# summary(panel$w_M)
# summary(panel$w_X)
# 
# panel <- panel %>%
#   mutate(w = w_M + w_X)
# summary(panel$w)
# 
# panel <- panel %>%
#   mutate(RV = w_M*FOB_Import + w_X*pNetExport)
# 
# 
# # .... Compute mis-invoiced import ####
# panel <- panel %>%
#   mutate(Imp_IFF = Import - RV)
# summary(panel$Imp_IFF)
# 
# panel_Imp <- panel
# test_alternate_Imp <- panel %>% filter(id == "CAN_HKG_94_2012")
# 
# 
# # .. For mis-invoiced exports ####
# rm(panel)
# load("Data/panel_clean.Rdata")
# 
# 
# # .... Estimate first stage ####
# fit.FOB <- lm(ln.ratio_FOB ~ dist + I(dist^2) + 
#                 contig + 
#                 rLandlocked +
#                 pLandlocked +
#                 FutExport_misrep,
#               data = panel)
# summary(fit.FOB)
# 
# 
# # .... Identify and remove outliers ####
# Bonferonni.out <- outlierTest(fit.FOB, n.max = 100)
# obs <- as.numeric(names(Bonferonni.out[[1]]))
# outliers <- panel[c(obs), ]
# mean(outliers$ratio_FOB)
# # 7586215
# mean(panel$ratio_FOB)
# # 265.6377 
# panel <- panel[-c(obs),]
# rm(Bonferonni.out, outliers, obs)
# mean(panel$ratio_FOB)
# # 64.4255
# 
# panel %>% filter(ratio_FOB > 10^4) %>% nrow
# # 2756
# panel <- panel %>% 
#   filter(ratio_FOB < 10^4)
# nrow(panel)
# # 3767368
# mean(panel$ratio_FOB)
# # 13.39388
# 
# fit.FOB <- lm(ln.ratio_FOB ~ dist + I(dist^2) + 
#                 contig + 
#                 rLandlocked +
#                 pLandlocked +
#                 FutExport_misrep,
#               data = panel)
# summary(fit.FOB)
# 
# 
# # .... Compute CIF exports ####
# panel$fitted_FOB <- exp(fitted(fit.FOB))
# 
# panel <- panel %>%
#   mutate(CIF_Export = fitted_FOB * pImport)
# 
# save(panel, file = "Results/Intermediate/panel_1st_stage_Exp.Rdata")
# 
# 
# # .... Estimate second stage ####
# panel <- panel %>%
#   mutate(rep_dist = abs(log(pImport/CIF_Export))) %>%
#   filter(is.finite(rep_dist))
# nrow(panel)
# # 3767365
# 
# panel <- panel %>%
#   filter(year > 1999)
# nrow(panel)
# # 3615900
# 
# panel <- panel %>%
#   mutate_at(vars(reporter.ISO, partner.ISO, year),
#             funs(as.factor(.)))
# 
# FE.out <- felm(rep_dist ~ 0| reporter.ISO +
#                  partner.ISO + year,
#                data = panel)
# FE_Exp <- getfe(FE.out, se = T)
# 
# # options(contrasts = rep ("contr.sum", 2))
# # X <- model.matrix(~ -1 + panel$reporter.ISO + 
# #                     panel$partner.ISO + 
# #                     panel$year)
# # attr(X,"contrasts")
# # FE.out <- solve(t(X) %*% X) %*% t(X) %*% panel$rep_dist
# 
# FE_Exp <- FE_Exp %>%
#   group_by(fe) %>%
#   mutate(min = min(effect)) %>%
#   ungroup()
# 
# FE_Exp$sigma <- pi/2*(FE_Exp$effect - (FE_Exp$min + 2*FE_Exp$se))
# 
# panel <- panel %>%
#   mutate_at(vars(reporter.ISO, partner.ISO, year),
#             funs(as.character(.)))
# 
# 
# # .... Harmonization procedure ####
# panel <- left_join(panel, FE_Exp %>% 
#                      filter(fe == "reporter.ISO") %>%
#                      select(idx, sigma) %>%
#                      mutate(idx = as.character(idx)),
#                    by = c("reporter.ISO" = "idx")) %>%
#   rename(rSigma = sigma)
# 
# panel <- left_join(panel, FE_Exp %>% 
#                      filter(fe == "partner.ISO") %>%
#                      select(idx, sigma) %>%
#                      mutate(idx = as.character(idx)),
#                    by = c("partner.ISO" = "idx")) %>%
#   rename(pSigma = sigma)
# 
# panel <- panel %>%
#   mutate(w_X = (exp(rSigma^2)*(exp(rSigma^2) - 1))/(exp(rSigma^2)*(exp(rSigma^2)- 1) + exp(pSigma^2)*(exp(pSigma^2) - 1)),
#          w_M = (exp(pSigma^2)*(exp(pSigma^2) - 1))/(exp(rSigma^2)*(exp(rSigma^2)- 1) + exp(pSigma^2)*(exp(pSigma^2) - 1)))
# summary(panel$w_M)
# summary(panel$w_X)
# 
# panel <- panel %>%
#   mutate(w = w_M + w_X)
# summary(panel$w)
# 
# panel <- panel %>%
#   mutate(RV = w_M*pImport + w_X*CIF_Export)
# 
# 
# # .... Compute mis-invoiced exports ####
# panel <- panel %>%
#   mutate(Exp_IFF = RV - NetExport)
# summary(panel$Exp_IFF)
# 
# panel_Exp <- panel
# test_alternate_Exp <- panel %>% filter(id == "CAN_HKG_94_2012")
# 
# 
# # .. Merge IFF for imports and exports ####
# panel <- full_join(panel_Imp %>%
#                      select(reporter, reporter.ISO, rRegion, rIncome,
#                             partner, partner.ISO, pRegion, pIncome,
#                             year, commodity.code, Imp_IFF),
#                    panel_Exp %>%
#                      select(reporter, reporter.ISO, rRegion, rIncome,
#                             partner, partner.ISO, pRegion, pIncome,
#                             year, commodity.code, Exp_IFF),
#                    by = c("reporter" = "reporter",
#                           "reporter.ISO" = "reporter.ISO",
#                           "rRegion" = "rRegion",
#                           "rIncome" = "rIncome",
#                           "partner" = "partner",
#                           "partner.ISO" = "partner.ISO",
#                           "pRegion" = "pRegion",
#                           "pIncome" = "pIncome",
#                           "year" = "year",
#                           "commodity.code" = "commodity.code"))
# 
# panel$id <- paste(panel$reporter.ISO, 
#                   panel$partner.ISO, 
#                   panel$commodity.code, 
#                   panel$year, sep = "_")
# 
# panel %>%
#   filter(duplicated(panel$id)) %>% nrow
# # 0
# 
# test_alternate <- left_join(test_alternate_Imp, test_alternate_Exp %>%
#                               select(id, Exp_IFF),
#                             by = c("id" = "id"))
# 
# 
# # .. Aggregate results using Gross Excluding Reversals ####
# 
# GER_Imp_alt <- panel %>%
#   filter(Imp_IFF > 0) %>%
#   group_by(reporter, reporter.ISO, rRegion, rIncome,
#            partner, partner.ISO, pRegion, pIncome,
#            year) %>%
#   summarize(Imp_IFF = sum(Imp_IFF, na.rm = T)) %>%
#   ungroup()
# 
# GER_Exp_alt <- panel %>%
#   filter(Exp_IFF > 0) %>%
#   group_by(reporter, reporter.ISO, rRegion, rIncome,
#            partner, partner.ISO, pRegion, pIncome,
#            year) %>%
#   summarize(Exp_IFF = sum(Exp_IFF, na.rm = T)) %>%
#   ungroup()
# 
# Orig_Dest_Year_alt <- full_join(GER_Imp_alt, GER_Exp_alt,
#                             by = c("reporter" = "reporter",
#                                    "reporter.ISO" = "reporter.ISO",
#                                    "rRegion" = "rRegion",
#                                    "rIncome" = "rIncome",
#                                    "partner" = "partner",
#                                    "partner.ISO" = "partner.ISO",
#                                    "pRegion" = "pRegion",
#                                    "pIncome" = "pIncome",
#                                    "year" = "year"))
# 
# Orig_Year_alt <- Orig_Dest_Year_alt %>%
#   group_by(reporter, reporter.ISO, rRegion, year) %>%
#   summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
#             Exp_IFF = sum(Exp_IFF, na.rm = T)) %>%
#   ungroup()
# 
# Africa_alternate <- Orig_Year_alt %>%
#   filter(rRegion == "Africa") %>%
#   select(-rRegion)
# 
# Africa_agg_alt <- Africa_alternate %>%
#   group_by(year) %>%
#   summarize(Imp_IFF = sum(Imp_IFF, na.rm = T),
#             Exp_IFF = sum(Exp_IFF, na.rm = T)) %>%
#   ungroup()
# 
# save(Africa_alternate, file = "Results/Africa_alternate.Rdata")
# write.csv(Africa_alternate, file = "Results/Africa_alternate approach.csv",
#           row.names = F)
# write.csv(Africa_agg_alt, file = "Results/Africa_agg_alternate approach.csv",
#           row.names = F)