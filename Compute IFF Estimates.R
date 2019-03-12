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
# Aggregate Results
# .. Aggregate results using Gross Excluding Reversals
# .. Aggregate results using Net Aggregation



## ## ## ## ## ## ## ## ## ## ##
# PREAMBLE                  ####
## ## ## ## ## ## ## ## ## ## ##

#setwd("C:/cloudstorage/googledrive/Projects/UN Consultancy/Illicit Financial Flows/IFF estimates") # Alice work
setwd("/home/alice/IFFe/") # Virtual server
library(car)
library(lfe)
library(reshape2)
library(scales)
library(tidyverse)



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
  mutate(pNetExport_value = pExport_value - pReExport_value,
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
# 2641583

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
# FIRST STAGE               ####
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
# tariff -0.00137197537000
max(panel$ratio_CIF)
# 1124495105
mean(panel$ratio_CIF)
# 1504.118


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
# 2641583
summary(panel$CD)

Bonferonni.out <- outlierTest(fit, n.max = 10000)
obs <- as.numeric(names(Bonferonni.out[[1]]))
outliers <- panel[c(obs), ]
mean(outliers$ratio_CIF)
# 958317.8
mean(panel$ratio_CIF)
# 1504.118
panel <- panel[-c(obs),]
mean(panel$ratio_CIF)
# 84.67892

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
# 75641.59
mean(panel$ratio_CIF)
# 84.67892
panel <- panel[-c(obs),]
mean(panel$ratio_CIF)
# 60.32255

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
# 8641.641
mean(panel$ratio_CIF)
# 60.32255
panel <- panel[-c(obs),]
mean(panel$ratio_CIF)
# 59.77251

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
# 6003.939
mean(panel$ratio_CIF)
# 59.77251
panel <- panel[-c(obs),]
mean(panel$ratio_CIF)
# 59.70939

fit <- lm(ln.ratio_CIF ~ dist + dist.sq +
            contig + 
            rLandlocked +
            pLandlocked +
            ln.FutImport_misrep +
            ihs.ReExport_misrep +
            ln.ratio_CIF_lag +
            tariff,
          data = panel)
rm(Bonferonni.out, outliers, obs)

max(panel$ratio_CIF)
# 6786522
nrow(panel)
# 2636623
summary(fit)
# tariff -0.00138910768362

save(panel, file = "Data/Panel/panel_nooutliers.Rdata")


# .. Censor the data-set (not used) ####
panel %>% filter(ratio_CIF > 2) %>% nrow
# 661691
panel %>% filter(ratio_CIF < 0.5) %>% nrow
# 516181
panel %>% filter(ratio_CIF > 2 | ratio_CIF < 0.5) %>% nrow
# 1177872
panel_censor <- panel %>% 
  filter(ratio_CIF <= 2) %>% filter(ratio_CIF >= 0.5)
nrow(panel_censor)
# 1458751
mean(panel_censor$ratio_CIF)
# 1.103012
summary(panel_censor$ratio_CIF)
plot(density(panel_censor$ratio_CIF))

fit_censor <- lm(ln.ratio_CIF ~ dist + dist.sq +
                   contig + 
                   rLandlocked +
                   pLandlocked +
                   ln.FutImport_misrep +
                   ihs.ReExport_misrep +
                   ln.ratio_CIF_lag +
                   tariff,
                 data = panel_censor)
summary(fit_censor)
# tariff -0.000346419516116

rm(fit_censor, panel_censor)


# .. Compute fitted values when predictors are 0 ####
fit_IFF <- lm(ln.ratio_CIF ~ - 1 + tariff,
              data = panel)
summary(fit_IFF)
# tariff 0.0016312
panel$fitted_IFF_lm <- exp(predict(fit_IFF))
rm(fit_IFF)
# This does not give the marginal effect of IFF predictors given legitimate predictors of IFF.
# Rather, this gives the unconditional effect of tariffs on the discrepancy.

coef <- coef(fit)
for (v in 1:length(coef)){
if (names(coef[v]) == "tariff"){
    coef[v] <- coef(fit)["tariff"]
  } else {
    coef[v] <- 0
  }
}
coef
# coef["(Intercept)"] <- coef(fit)["(Intercept)"]
# coef

panel$fitted_IFF_man <- as.numeric(exp(model.matrix(fit) %*% coef))

panel$fitted_IFF_pred <- exp(predict(fit,
                                     newdata = data.frame(dist = 0,
                                                          dist.sq = 0,
                                                          contig = "0",
                                                          rLandlocked = "0",
                                                          pLandlocked = "0",
                                                          ln.FutImport_misrep = 0,
                                                          ihs.ReExport_misrep = 0,
                                                          ln.ratio_CIF_lag = 0,
                                                          tariff = panel$tariff)))

summary(panel$fitted_IFF_lm)
summary(panel$fitted_IFF_man)
summary(panel$fitted_IFF_pred)
# fitted_IFF_pred is the same as fitted_IFF_man if the constant were not set to 0.

panel$fitted_IFF <- panel$fitted_IFF_man

coef <- coef(fit)
for (v in 1:length(coef)){
  if (names(coef[v]) == "tariff"){
    coef[v] <- 0 
  }
  
}
coef

panel$fitted_nonIFF <- as.numeric(exp(model.matrix(fit) %*% coef))
rm(coef, v)


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

panel <- panel %>%
  mutate(FOB_Import = pNetExport_value + (pNetExport_value * resid_adj),
         FOB_Import_IFF_hi = pNetExport_value + (pNetExport_value * (resid_adj + fitted_IFF)),
         FOB_Import_IFF_lo = pNetExport_value + (pNetExport_value * fitted_IFF))

panel <- panel %>%
  mutate(FOB_Import_AL = Import_value/fitted,
         FOB_Import_WD = (pNetExport_value + (pNetExport_value * resid ))/ fitted)
sum(panel$FOB_Import_AL == panel$FOB_Import_WD)
panel$FOB_Import_AL <- NULL
panel$FOB_Import_WD <- NULL



## ## ## ## ## ## ## ## ## ## ##
# SECOND STAGE              ####
## ## ## ## ## ## ## ## ## ## ##

# .. Estimate fixed effects regression ####
panel <- panel %>%
  mutate(rep_dist = abs(log(pNetExport_value/FOB_Import))) %>%
  filter(is.finite(rep_dist))
nrow(panel)
# 2636623

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
  mutate(RV = w_X*pNetExport_value + w_M*FOB_Import)


# .. Compute IFF ####
panel <- panel %>%
  mutate(Imp_IFF_hi = FOB_Import_IFF_hi - RV,
         Exp_IFF_hi = RV - pNetExport_value,
         Imp_IFF_lo = FOB_Import_IFF_lo - RV,
         Exp_IFF_lo = RV - pNetExport_value)
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



## ## ## ## ## ## ## ## ## ## ##
# AGGREGATE RESULTS         ####
## ## ## ## ## ## ## ## ## ## ##

# .. Aggregate results using Gross Excluding Reversals ####
GER_Imp_hi <- panel %>%
  filter(Imp_IFF_hi > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome,
           partner, partner.ISO, pRegion, pIncome,
           year) %>%
  summarize(Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T)) %>%
  ungroup()

GER_Imp_lo <- panel %>%
  filter(Imp_IFF_lo > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome,
           partner, partner.ISO, pRegion, pIncome,
           year) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T)) %>%
  ungroup()

GER_Imp <- full_join(GER_Imp_lo, GER_Imp_hi,
                     by = c("reporter" = "reporter",
                            "reporter.ISO" = "reporter.ISO",
                            "rRegion" = "rRegion",
                            "rIncome" = "rIncome",
                            "partner" = "partner",
                            "partner.ISO" = "partner.ISO",
                            "pRegion" = "pRegion",
                            "pIncome" = "pIncome",
                            "year" = "year"))

GER_Exp_hi <- panel %>%
  filter(pExp_IFF_hi > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome,
           partner, partner.ISO, pRegion, pIncome,
           year) %>%
  summarize(Exp_IFF_hi = sum(pExp_IFF_hi, na.rm = T)) %>%
  ungroup()

GER_Exp_lo <- panel %>%
  filter(pExp_IFF_lo > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome,
           partner, partner.ISO, pRegion, pIncome,
           year) %>%
  summarize(Exp_IFF_lo = sum(pExp_IFF_lo, na.rm = T)) %>%
  ungroup()

GER_Exp <- full_join(GER_Exp_lo, GER_Exp_hi,
                     by = c("reporter" = "reporter",
                            "reporter.ISO" = "reporter.ISO",
                            "rRegion" = "rRegion",
                            "rIncome" = "rIncome",
                            "partner" = "partner",
                            "partner.ISO" = "partner.ISO",
                            "pRegion" = "pRegion",
                            "pIncome" = "pIncome",
                            "year" = "year"))

GER_Orig_Dest_Year <- full_join(GER_Imp, GER_Exp,
                                by = c("reporter" = "reporter",
                                       "reporter.ISO" = "reporter.ISO",
                                       "rRegion" = "rRegion",
                                       "rIncome" = "rIncome",
                                       "partner" = "partner",
                                       "partner.ISO" = "partner.ISO",
                                       "pRegion" = "pRegion",
                                       "pIncome" = "pIncome",
                                       "year" = "year"))

GER_Orig_Year <- GER_Orig_Dest_Year %>%
  group_by(reporter, reporter.ISO, rRegion, year) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T)) %>%
  ungroup()

GER_Orig_Year_Africa <- GER_Orig_Year %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

GER_Year_Africa <- GER_Orig_Year_Africa %>%
  group_by(year) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T)) %>%
  ungroup()

write.csv(GER_Orig_Year_Africa, file = "Results/Approach 2/GER_Orig_Year_Africa_pExp-mul-resid-fixexp.csv",
          row.names = F)
write.csv(GER_Year_Africa, file = "Results/Approach 2/GER_Year_Africa_pExp-mul-resid-fixexp.csv",
          row.names = F)


# .. Aggregate results using Net Aggregation ####
Net_Orig_Dest_Year <- panel %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome,
           partner, partner.ISO, pRegion, pIncome,
           year) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T)) %>%
  ungroup()

Net_Orig_Year <- Net_Orig_Dest_Year %>%
  group_by(reporter, reporter.ISO, rRegion, year) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T)) %>%
  ungroup()

Net_Orig_Year_Africa <- Net_Orig_Year %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

Net_Year_Africa <- Net_Orig_Year_Africa %>%
  group_by(year) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T)) %>%
  ungroup()

write.csv(Net_Orig_Year_Africa, file = "Results/Approach 2/Net_Orig_Year_Africa_pExp-mul-resid-fixexp.csv",
          row.names = F)
write.csv(Net_Year_Africa, file = "Results/Approach 2/Net_Year_Africa_pExp-mul-resid-fixexp.csv",
          row.names = F)



## ## ## ## ## ## ## ## ## ## ##
# FIGURES                   ####
## ## ## ## ## ## ## ## ## ## ##

g <- ggplot(GER_Year_Africa %>% 
         melt(id.vars = "year") %>%
         filter(str_detect(variable, "Imp")), 
       aes(x = year, y = value, fill = variable)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = dollar_format(scale = 1/10^9, accuracy = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Illicit Financial Flows in Africa",
       subtitle = "Gross Excluding Reversals",
       x = "Year", y = "Illicit flow in billion USD")
ggsave(g,
       file = "Figures/GER_Africa_Import_pExp-mul-resid-fixexp.png",
       width = 6, height = 5, units = "in")

g <- ggplot(Net_Year_Africa %>% 
         melt(id.vars = "year") %>%
         filter(str_detect(variable, "Imp")), 
       aes(x = year, y = value, fill = variable)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = dollar_format(scale = 1/10^9, accuracy = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Illicit Financial Flows in Africa",
       subtitle = "Net",
       x = "Year", y = "Illicit flow in billion USD")
ggsave(g,
       file = "Figures/Net_Africa_Import_pExp-mul-resid-fixexp.png",
       width = 6, height = 5, units = "in")
