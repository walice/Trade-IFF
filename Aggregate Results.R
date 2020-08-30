# Aggregate Results
# Alice Lepissier
# alice.lepissier@gmail.com
# Prepared for UNECA

## ## ## ## ## ## ## ## ## ## ##
# INDEX                     ####
## ## ## ## ## ## ## ## ## ## ##
# Preamble
# Aggregate by Destination
# .. Aggregate results using Gross Excluding Reversals
# .. Aggregate results using Net Aggregation
# Aggregate by Sector
# .. Aggregate results using Gross Excluding Reversals
# .. Aggregate results using Net Aggregation
# Headline Figures
# .. For Africa
# .. For developing countries
# Pilot Country Results
# LMIC Results



## ## ## ## ## ## ## ## ## ## ##
# PREAMBLE                  ####
## ## ## ## ## ## ## ## ## ## ##

#setwd("C:/cloudstorage/googledrive/Projects/UN Consultancy/Illicit Financial Flows/IFF estimates") # Alice work
#setwd("D:/Google Drive/Projects/UN Consultancy/Illicit Financial Flows/IFF estimates") # Alice laptop
setwd("/home/alice/IFFe/") # Virtual server
library(car)
library(kableExtra)
library(reshape2)
library(scales)
library(stargazer)
library(tidyverse)
options(scipen = 999)



## ## ## ## ## ## ## ## ## ## ##
# AGGREGATE BY DESTINATION  ####
## ## ## ## ## ## ## ## ## ## ##

#source("Scripts/Compute IFF Estimates.R")
load("Results/panel_results.Rdata")


# .. Aggregate results using Gross Excluding Reversals ####
load("Data/WDI/WDI.Rdata")
load("Data/Comtrade/comtrade_total_clean.Rdata")

GER_Imp_lo_Dest <- panel %>%
  filter(Imp_IFF_lo > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev,
           year,
           partner, partner.ISO, pRegion, pIncome, pDev) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T)) %>%
  ungroup()

GER_Imp_hi_Dest <- panel %>%
  filter(Imp_IFF_hi > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev,
           year,
           partner, partner.ISO, pRegion, pIncome, pDev) %>%
  summarize(Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T)) %>%
  ungroup()

GER_Imp_Dest <- full_join(GER_Imp_lo_Dest, GER_Imp_hi_Dest,
                          by = c("reporter" = "reporter",
                                 "reporter.ISO" = "reporter.ISO",
                                 "rRegion" = "rRegion",
                                 "rIncome" = "rIncome",
                                 "rDev" = "rDev",
                                 "year" = "year",
                                 "partner" = "partner",
                                 "partner.ISO" = "partner.ISO",
                                 "pRegion" = "pRegion",
                                 "pIncome" = "pIncome",
                                 "pDev" = "pDev"))
rm(GER_Imp_lo_Dest, GER_Imp_hi_Dest)

GER_Exp_lo_Dest <- panel %>%
  filter(pExp_IFF_lo > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev,
           year,
           partner, partner.ISO, pRegion, pIncome, pDev) %>%
  summarize(Exp_IFF_lo = sum(pExp_IFF_lo, na.rm = T)) %>%
  ungroup()

GER_Exp_hi_Dest <- panel %>%
  filter(pExp_IFF_hi > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev,
           year,
           partner, partner.ISO, pRegion, pIncome, pDev) %>%
  summarize(Exp_IFF_hi = sum(pExp_IFF_hi, na.rm = T)) %>%
  ungroup()

GER_Exp_Dest <- full_join(GER_Exp_lo_Dest, GER_Exp_hi_Dest,
                          by = c("reporter" = "reporter",
                                 "reporter.ISO" = "reporter.ISO",
                                 "rRegion" = "rRegion",
                                 "rIncome" = "rIncome",
                                 "rDev" = "rDev",
                                 "year" = "year",
                                 "partner" = "partner",
                                 "partner.ISO" = "partner.ISO",
                                 "pRegion" = "pRegion",
                                 "pIncome" = "pIncome",
                                 "pDev" = "pDev"))
rm(GER_Exp_lo_Dest, GER_Exp_hi_Dest)

GER_Orig_Dest_Year <- full_join(GER_Imp_Dest, GER_Exp_Dest,
                                by = c("reporter" = "reporter",
                                       "reporter.ISO" = "reporter.ISO",
                                       "rRegion" = "rRegion",
                                       "rIncome" = "rIncome",
                                       "rDev" = "rDev",
                                       "year" = "year",
                                       "partner" = "partner",
                                       "partner.ISO" = "partner.ISO",
                                       "pRegion" = "pRegion",
                                       "pIncome" = "pIncome",
                                       "pDev" = "pDev"))
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
  mutate(rImp_IFF_hi_GDP = Imp_IFF_hi / rGDP,
         pImp_IFF_hi_GDP = Imp_IFF_hi / pGDP)

GER_Orig_Year <- GER_Orig_Dest_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, year) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF_lo = Imp_IFF_lo + Exp_IFF_lo,
         Tot_IFF_hi = Imp_IFF_hi + Exp_IFF_hi,
         Tot_IFF_lo_bn = Tot_IFF_lo / 10^9,
         Tot_IFF_hi_bn = Tot_IFF_hi / 10^9)
GER_Orig_Year <- left_join(GER_Orig_Year %>% mutate(year = as.integer(year)),
                           WDI,
                           by = c("reporter.ISO" = "ISO3166.3", 
                                  "year")) %>%
  mutate(Tot_IFF_lo_GDP = Tot_IFF_lo / GDP,
         Tot_IFF_hi_GDP = Tot_IFF_hi / GDP)
GER_Orig_Year <- left_join(GER_Orig_Year,
                           comtrade_total,
                           by = c("reporter.ISO", "year")) %>%
  mutate(Tot_IFF_lo_trade = Tot_IFF_lo / Total_value,
         Tot_IFF_hi_trade = Tot_IFF_hi / Total_value)

GER_Orig_Avg <- GER_Orig_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev) %>%
  summarize(Imp_IFF_lo = mean(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = mean(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = mean(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = mean(Exp_IFF_hi, na.rm = T),
            Tot_IFF_lo = mean(Tot_IFF_lo, na.rm = T),
            Tot_IFF_hi = mean(Tot_IFF_hi, na.rm = T),
            Tot_IFF_lo_bn = mean(Tot_IFF_lo_bn, na.rm = T),
            Tot_IFF_hi_bn = mean(Tot_IFF_hi_bn, na.rm = T),
            Tot_IFF_lo_GDP = mean(Tot_IFF_lo_GDP, na.rm = T),
            Tot_IFF_hi_GDP = mean(Tot_IFF_hi_GDP, na.rm = T),
            Tot_IFF_lo_trade = mean(Tot_IFF_lo_trade, na.rm = T),
            Tot_IFF_hi_trade = mean(Tot_IFF_hi_trade, na.rm = T)) %>%
  ungroup()

GER_Orig_Sum <- GER_Orig_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T),
            Tot_IFF_lo = sum(Tot_IFF_lo, na.rm = T),
            Tot_IFF_hi = sum(Tot_IFF_hi, na.rm = T),
            Tot_IFF_lo_bn = sum(Tot_IFF_lo_bn, na.rm = T),
            Tot_IFF_hi_bn = sum(Tot_IFF_hi_bn, na.rm = T)) %>%
  ungroup()

GER_Orig_Dest_Avg <- GER_Orig_Dest_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, partner, partner.ISO, pRegion, pIncome, pDev) %>%
  summarize(Imp_IFF_lo = mean(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = mean(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = mean(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = mean(Exp_IFF_hi, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF_lo = Imp_IFF_lo + Exp_IFF_lo,
         Tot_IFF_hi = Imp_IFF_hi + Exp_IFF_hi,
         Tot_IFF_lo_bn = Tot_IFF_lo / 10^9,
         Tot_IFF_hi_bn = Tot_IFF_hi / 10^9)
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
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, partner, partner.ISO, pRegion, pIncome, pDev) %>%
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

GER_Orig_Dest_Year_LMIC <- GER_Orig_Dest_Year %>%
  filter(rIncome == "LIC" | rIncome == "LMC") %>%
  select(-rIncome)

GER_Orig_Dest_Avg_Africa <- GER_Orig_Dest_Avg %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

GER_Orig_Dest_Avg_LMIC <- GER_Orig_Dest_Avg %>%
  filter(rIncome == "LIC" | rIncome == "LMC") %>%
  select(-rIncome)

GER_Orig_Dest_Avg_Developing <- GER_Orig_Dest_Avg %>%
  filter(rDev == "Developing") %>%
  select(-rDev)

GER_Orig_Dest_Sum_Africa <- GER_Orig_Dest_Sum %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

GER_Dest <- GER_Orig_Dest_Sum %>%
  group_by(partner, partner.ISO, pRegion, pIncome, pDev) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T),
            Tot_IFF_lo = sum(Tot_IFF_lo, na.rm = T),
            Tot_IFF_hi = sum(Tot_IFF_hi, na.rm = T),
            Tot_IFF_lo_bn = sum(Tot_IFF_lo_bn, na.rm = T),
            Tot_IFF_hi_bn = sum(Tot_IFF_hi_bn, na.rm = T)) %>%
  ungroup()

GER_Dest_Africa <- GER_Orig_Dest_Sum_Africa %>%
  group_by(partner, partner.ISO, pRegion, pIncome, pDev) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T),
            Tot_IFF_lo = sum(Tot_IFF_lo, na.rm = T),
            Tot_IFF_hi = sum(Tot_IFF_hi, na.rm = T),
            Tot_IFF_lo_bn = sum(Tot_IFF_lo_bn, na.rm = T),
            Tot_IFF_hi_bn = sum(Tot_IFF_hi_bn, na.rm = T)) %>%
  ungroup()

GER_Dest_Avg_Africa <- GER_Orig_Dest_Avg_Africa %>%
  group_by(partner, partner.ISO, pRegion, pIncome, pDev) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T),
            Tot_IFF_lo = sum(Tot_IFF_lo, na.rm = T),
            Tot_IFF_hi = sum(Tot_IFF_hi, na.rm = T),
            Tot_IFF_lo_bn = sum(Tot_IFF_lo_bn, na.rm = T),
            Tot_IFF_hi_bn = sum(Tot_IFF_hi_bn, na.rm = T)) %>%
  ungroup()

GER_Dest_Avg_LMIC <- GER_Orig_Dest_Avg_LMIC %>%
  group_by(partner, partner.ISO, pRegion, pIncome, pDev) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T),
            Tot_IFF_lo = sum(Tot_IFF_lo, na.rm = T),
            Tot_IFF_hi = sum(Tot_IFF_hi, na.rm = T),
            Tot_IFF_lo_bn = sum(Tot_IFF_lo_bn, na.rm = T),
            Tot_IFF_hi_bn = sum(Tot_IFF_hi_bn, na.rm = T)) %>%
  ungroup()

GER_Dest_Avg_Developing <- GER_Orig_Dest_Avg_Developing %>%
  group_by(partner, partner.ISO, pRegion, pIncome, pDev) %>%
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

GER_Orig_Year_LMIC <- GER_Orig_Year %>%
  filter(rIncome == "LIC" | rIncome == "LMC") %>%
  select(-rIncome)

GER_Orig_Year_Developing <- GER_Orig_Year %>%
  filter(rDev == "Developing") %>%
  select(-rDev)

GER_Orig_Avg_Africa <- GER_Orig_Avg %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

GER_Orig_Avg_LMIC <- GER_Orig_Avg %>%
  filter(rIncome == "LIC" | rIncome == "LMC") %>%
  select(-rIncome)

GER_Orig_Avg_Developing <- GER_Orig_Avg %>%
  filter(rDev == "Developing") %>%
  select(-rDev)

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
            Tot_IFF_hi_bn = sum(Tot_IFF_hi_bn, na.rm = T),
            GDP = sum(GDP, na.rm = T),
            Total_value = sum(Total_value, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF_lo_GDP = Tot_IFF_lo / GDP,
         Tot_IFF_hi_GDP = Tot_IFF_hi / GDP,
         Tot_IFF_lo_trade = Tot_IFF_lo / Total_value,
         Tot_IFF_hi_trade = Tot_IFF_hi / Total_value)

GER_Year_LMIC <- GER_Orig_Year_LMIC %>%
  group_by(year) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T),
            Tot_IFF_lo = sum(Tot_IFF_lo, na.rm = T),
            Tot_IFF_hi = sum(Tot_IFF_hi, na.rm = T),
            Tot_IFF_lo_bn = sum(Tot_IFF_lo_bn, na.rm = T),
            Tot_IFF_hi_bn = sum(Tot_IFF_hi_bn, na.rm = T),
            GDP = sum(GDP, na.rm = T),
            Total_value = sum(Total_value, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF_lo_GDP = Tot_IFF_lo / GDP,
         Tot_IFF_hi_GDP = Tot_IFF_hi / GDP,
         Tot_IFF_lo_trade = Tot_IFF_lo / Total_value,
         Tot_IFF_hi_trade = Tot_IFF_hi / Total_value)

GER_Year_Developing <- GER_Orig_Year_Developing %>%
  group_by(year) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T),
            Tot_IFF_lo = sum(Tot_IFF_lo, na.rm = T),
            Tot_IFF_hi = sum(Tot_IFF_hi, na.rm = T),
            Tot_IFF_lo_bn = sum(Tot_IFF_lo_bn, na.rm = T),
            Tot_IFF_hi_bn = sum(Tot_IFF_hi_bn, na.rm = T),
            GDP = sum(GDP, na.rm = T),
            Total_value = sum(Total_value, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF_lo_GDP = Tot_IFF_lo / GDP,
         Tot_IFF_hi_GDP = Tot_IFF_hi / GDP,
         Tot_IFF_lo_trade = Tot_IFF_lo / Total_value,
         Tot_IFF_hi_trade = Tot_IFF_hi / Total_value)

GER_Africa <- GER_Year_Africa %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T),
            Tot_IFF_lo = sum(Tot_IFF_lo, na.rm = T),
            Tot_IFF_hi = sum(Tot_IFF_hi, na.rm = T),
            Tot_IFF_lo_bn = sum(Tot_IFF_lo_bn, na.rm = T),
            Tot_IFF_hi_bn = sum(Tot_IFF_hi_bn, na.rm = T))

GER_LMIC <- GER_Year_LMIC %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T),
            Tot_IFF_lo = sum(Tot_IFF_lo, na.rm = T),
            Tot_IFF_hi = sum(Tot_IFF_hi, na.rm = T),
            Tot_IFF_lo_bn = sum(Tot_IFF_lo_bn, na.rm = T),
            Tot_IFF_hi_bn = sum(Tot_IFF_hi_bn, na.rm = T))

GER_Developing <- GER_Year_Developing %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T),
            Tot_IFF_lo = sum(Tot_IFF_lo, na.rm = T),
            Tot_IFF_hi = sum(Tot_IFF_hi, na.rm = T),
            Tot_IFF_lo_bn = sum(Tot_IFF_lo_bn, na.rm = T),
            Tot_IFF_hi_bn = sum(Tot_IFF_hi_bn, na.rm = T))

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
save(GER_Orig_Dest_Year_LMIC, file = "Results/Summary data-sets/GER_Orig_Dest_Year_LMIC.Rdata")
write.csv(GER_Orig_Dest_Year_LMIC, file = "Results/Summary data-sets/GER_Orig_Dest_Year_LMIC.csv",
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

save(GER_Orig_Avg_Africa, file = "Results/Summary data-sets/GER_Orig_Avg_Africa.Rdata")
write.csv(GER_Orig_Avg_Africa, file = "Results/Summary data-sets/GER_Orig_Avg_Africa.csv",
          row.names = F)
save(GER_Orig_Avg_LMIC, file = "Results/Summary data-sets/GER_Orig_Avg_LMIC.Rdata")
write.csv(GER_Orig_Avg_LMIC, file = "Results/Summary data-sets/GER_Orig_Avg_LMIC.csv",
          row.names = F)
save(GER_Orig_Avg_Developing, file = "Results/Summary data-sets/GER_Orig_Avg_Developing.Rdata")
write.csv(GER_Orig_Avg_Developing, file = "Results/Summary data-sets/GER_Orig_Avg_Developing.csv",
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
save(GER_Orig_Dest_Sum_Africa, file = "Results/Summary data-sets/GER_Orig_Dest_Sum_Africa.Rdata")
write.csv(GER_Orig_Dest_Sum_Africa, file = "Results/Summary data-sets/GER_Orig_Dest_Sum_Africa.csv",
          row.names = F)
save(GER_Dest, file = "Results/Summary data-sets/GER_Dest.Rdata")
write.csv(GER_Dest, file = "Results/Summary data-sets/GER_Dest.csv",
          row.names = F)
save(GER_Dest_Africa, file = "Results/Summary data-sets/GER_Dest_Africa.Rdata")
write.csv(GER_Dest_Africa, file = "Results/Summary data-sets/GER_Dest_Africa.csv",
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

save(GER_Year_Africa, file = "Results/Summary data-sets/GER_Year_Africa.Rdata")
write.csv(GER_Year_Africa, file = "Results/Summary data-sets/GER_Year_Africa.csv",
          row.names = F)
save(GER_Year_LMIC, file = "Results/Summary data-sets/GER_Year_LMIC.Rdata")
write.csv(GER_Year_LMIC, file = "Results/Summary data-sets/GER_Year_LMIC.csv",
          row.names = F)
save(GER_Year_Developing, file = "Results/Summary data-sets/GER_Year_Developing.Rdata")
write.csv(GER_Year_Developing, file = "Results/Summary data-sets/GER_Year_Developing.csv",
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


# .. Aggregate results using Net Aggregation ####
load("Data/WDI/WDI.Rdata")
load("Data/Comtrade/comtrade_total_clean.Rdata")

Net_Orig_Dest_Year <- panel %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev,
           partner, partner.ISO, pRegion, pIncome, pDev,
           year) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(pExp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(pExp_IFF_hi, na.rm = T)) %>%
  ungroup()

Net_Orig_Year <- Net_Orig_Dest_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, year) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF_lo = Imp_IFF_lo + Exp_IFF_lo,
         Tot_IFF_hi = Imp_IFF_hi + Exp_IFF_hi,
         Tot_IFF_lo_bn = Tot_IFF_lo / 10^9,
         Tot_IFF_hi_bn = Tot_IFF_hi / 10^9)
Net_Orig_Year <- left_join(Net_Orig_Year %>% mutate(year = as.integer(year)),
                           WDI,
                           by = c("reporter.ISO" = "ISO3166.3", 
                                  "year")) %>%
  mutate(Tot_IFF_lo_GDP = Tot_IFF_lo / GDP,
         Tot_IFF_hi_GDP = Tot_IFF_hi / GDP)
Net_Orig_Year <- left_join(Net_Orig_Year,
                           comtrade_total,
                           by = c("reporter.ISO", "year")) %>%
  mutate(Tot_IFF_lo_trade = Tot_IFF_lo / Total_value,
         Tot_IFF_hi_trade = Tot_IFF_hi / Total_value)

Net_Orig_Avg <- Net_Orig_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev) %>%
  summarize(Imp_IFF_lo = mean(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = mean(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = mean(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = mean(Exp_IFF_hi, na.rm = T),
            Tot_IFF_lo = mean(Tot_IFF_lo, na.rm = T),
            Tot_IFF_hi = mean(Tot_IFF_hi, na.rm = T),
            Tot_IFF_lo_bn = mean(Tot_IFF_lo_bn, na.rm = T),
            Tot_IFF_hi_bn = mean(Tot_IFF_hi_bn, na.rm = T),
            Tot_IFF_lo_GDP = mean(Tot_IFF_lo_GDP, na.rm = T),
            Tot_IFF_hi_GDP = mean(Tot_IFF_hi_GDP, na.rm = T),
            Tot_IFF_lo_trade = mean(Tot_IFF_lo_trade, na.rm = T),
            Tot_IFF_hi_trade = mean(Tot_IFF_hi_trade, na.rm = T)) %>%
  ungroup()

Net_Orig_Sum <- Net_Orig_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev) %>%
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
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, partner, partner.ISO, pRegion, pIncome, pDev) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF_lo = Imp_IFF_lo + Exp_IFF_lo,
         Tot_IFF_hi = Imp_IFF_hi + Exp_IFF_hi,
         Tot_IFF_lo_bn = Tot_IFF_lo / 10^9,
         Tot_IFF_hi_bn = Tot_IFF_hi / 10^9)

Net_Orig_Dest_Avg <- Net_Orig_Dest_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, partner, partner.ISO, pRegion, pIncome, pDev) %>%
  summarize(Imp_IFF_lo = mean(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = mean(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = mean(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = mean(Exp_IFF_hi, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF_lo = Imp_IFF_lo + Exp_IFF_lo,
         Tot_IFF_hi = Imp_IFF_hi + Exp_IFF_hi,
         Tot_IFF_lo_bn = Tot_IFF_lo / 10^9,
         Tot_IFF_hi_bn = Tot_IFF_hi / 10^9)
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

Net_Orig_Dest_Africa <- Net_Orig_Dest %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

Net_Dest <- Net_Orig_Dest %>%
  group_by(partner, partner.ISO, pRegion, pIncome, pDev) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T),
            Tot_IFF_lo = sum(Tot_IFF_lo, na.rm = T),
            Tot_IFF_hi = sum(Tot_IFF_hi, na.rm = T),
            Tot_IFF_lo_bn = sum(Tot_IFF_lo_bn, na.rm = T),
            Tot_IFF_hi_bn = sum(Tot_IFF_hi_bn, na.rm = T)) %>%
  ungroup()

Net_Dest_Africa <- Net_Orig_Dest_Africa %>%
  group_by(partner, partner.ISO, pRegion, pIncome, pDev) %>%
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

Net_Orig_Year_LMIC <- Net_Orig_Year %>%
  filter(rIncome == "LIC" | rIncome == "LMC") %>%
  select(-rIncome)

Net_Orig_Year_Developing <- Net_Orig_Year %>%
  filter(rDev == "Developing") %>%
  select(-rDev)

Net_Orig_Avg_Africa <- Net_Orig_Avg %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

Net_Orig_Avg_LMIC <- Net_Orig_Avg %>%
  filter(rIncome == "LIC" | rIncome == "LMC") %>%
  select(-rIncome)

Net_Orig_Avg_Developing <- Net_Orig_Avg %>%
  filter(rDev == "Developing") %>%
  select(-rDev)

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
            Tot_IFF_hi_bn = sum(Tot_IFF_hi_bn, na.rm = T),
            GDP = sum(GDP, na.rm = T),
            Total_value = sum(Total_value, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF_lo_GDP = Tot_IFF_lo / GDP,
         Tot_IFF_hi_GDP = Tot_IFF_hi / GDP,
         Tot_IFF_lo_trade = Tot_IFF_lo / Total_value,
         Tot_IFF_hi_trade = Tot_IFF_hi / Total_value)

Net_Year_LMIC <- Net_Orig_Year_LMIC %>%
  group_by(year) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T),
            Tot_IFF_lo = sum(Tot_IFF_lo, na.rm = T),
            Tot_IFF_hi = sum(Tot_IFF_hi, na.rm = T),
            Tot_IFF_lo_bn = sum(Tot_IFF_lo_bn, na.rm = T),
            Tot_IFF_hi_bn = sum(Tot_IFF_hi_bn, na.rm = T),
            GDP = sum(GDP, na.rm = T),
            Total_value = sum(Total_value, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF_lo_GDP = Tot_IFF_lo / GDP,
         Tot_IFF_hi_GDP = Tot_IFF_hi / GDP,
         Tot_IFF_lo_trade = Tot_IFF_lo / Total_value,
         Tot_IFF_hi_trade = Tot_IFF_hi / Total_value)

Net_Year_Developing <- Net_Orig_Year_Developing %>%
  group_by(year) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T),
            Tot_IFF_lo = sum(Tot_IFF_lo, na.rm = T),
            Tot_IFF_hi = sum(Tot_IFF_hi, na.rm = T),
            Tot_IFF_lo_bn = sum(Tot_IFF_lo_bn, na.rm = T),
            Tot_IFF_hi_bn = sum(Tot_IFF_hi_bn, na.rm = T),
            GDP = sum(GDP, na.rm = T),
            Total_value = sum(Total_value, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF_lo_GDP = Tot_IFF_lo / GDP,
         Tot_IFF_hi_GDP = Tot_IFF_hi / GDP,
         Tot_IFF_lo_trade = Tot_IFF_lo / Total_value,
         Tot_IFF_hi_trade = Tot_IFF_hi / Total_value)

Net_Africa <- Net_Year_Africa %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T),
            Tot_IFF_lo = sum(Tot_IFF_lo, na.rm = T),
            Tot_IFF_hi = sum(Tot_IFF_hi, na.rm = T),
            Tot_IFF_lo_bn = sum(Tot_IFF_lo_bn, na.rm = T),
            Tot_IFF_hi_bn = sum(Tot_IFF_hi_bn, na.rm = T))

Net_LMIC <- Net_Year_LMIC %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T),
            Tot_IFF_lo = sum(Tot_IFF_lo, na.rm = T),
            Tot_IFF_hi = sum(Tot_IFF_hi, na.rm = T),
            Tot_IFF_lo_bn = sum(Tot_IFF_lo_bn, na.rm = T),
            Tot_IFF_hi_bn = sum(Tot_IFF_hi_bn, na.rm = T))

Net_Developing <- Net_Year_Developing %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T),
            Tot_IFF_lo = sum(Tot_IFF_lo, na.rm = T),
            Tot_IFF_hi = sum(Tot_IFF_hi, na.rm = T),
            Tot_IFF_lo_bn = sum(Tot_IFF_lo_bn, na.rm = T),
            Tot_IFF_hi_bn = sum(Tot_IFF_hi_bn, na.rm = T))

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

save(Net_Orig_Avg_Africa, file = "Results/Summary data-sets/Net_Orig_Avg_Africa.Rdata")
write.csv(Net_Orig_Avg_Africa, file = "Results/Summary data-sets/Net_Orig_Avg_Africa.csv",
          row.names = F)
save(Net_Orig_Avg_LMIC, file = "Results/Summary data-sets/Net_Orig_Avg_LMIC.Rdata")
write.csv(Net_Orig_Avg_LMIC, file = "Results/Summary data-sets/Net_Orig_Avg_LMIC.csv",
          row.names = F)
save(Net_Orig_Avg_Developing, file = "Results/Summary data-sets/Net_Orig_Avg_Developing.Rdata")
write.csv(Net_Orig_Avg_Developing, file = "Results/Summary data-sets/Net_Orig_Avg_Developing.csv",
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
save(Net_Africa, file = "Results/Summary data-sets/Net_Africa.Rdata")
write.csv(Net_Africa, file = "Results/Summary data-sets/Net_Africa.csv",
          row.names = F)
save(Net_LMIC, file = "Results/Summary data-sets/Net_LMIC.Rdata")
write.csv(Net_LMIC, file = "Results/Summary data-sets/Net_LMIC.csv",
          row.names = F)
save(Net_Developing, file = "Results/Summary data-sets/Net_Developing.Rdata")
write.csv(Net_Developing, file = "Results/Summary data-sets/Net_Developing.csv",
          row.names = F)



## ## ## ## ## ## ## ## ## ## ##
# AGGREGATE BY SECTOR       ####
## ## ## ## ## ## ## ## ## ## ##

load("Data/WDI/WDI.Rdata")
load("Data/UN Stats/HS.Rdata")
panel <- left_join(panel, HS %>% select(chapter, chapter.description),
                   by = c("commodity.code" = "chapter")) %>%
  rename(commodity = chapter.description)


# .. Aggregate results using Gross Excluding Reversals ####
GER_Imp_lo_Sect <- panel %>%
  filter(Imp_IFF_lo > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev,
           year, 
           section.code, section) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T)) %>%
  ungroup()

GER_Imp_lo_Sect_disag <- panel %>%
  filter(Imp_IFF_lo > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev,
           year, 
           commodity.code, commodity) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T)) %>%
  ungroup()

GER_Imp_hi_Sect <- panel %>%
  filter(Imp_IFF_hi > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev,
           year, 
           section.code, section) %>%
  summarize(Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T)) %>%
  ungroup()

GER_Imp_hi_Sect_disag <- panel %>%
  filter(Imp_IFF_hi > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev,
           year, 
           commodity.code, commodity) %>%
  summarize(Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T)) %>%
  ungroup()

GER_Imp_Sect <- full_join(GER_Imp_lo_Sect, GER_Imp_hi_Sect,
                          by = c("reporter" = "reporter",
                                 "reporter.ISO" = "reporter.ISO",
                                 "rRegion" = "rRegion",
                                 "rIncome" = "rIncome",
                                 "rDev" = "rDev",
                                 "year" = "year",
                                 "section.code" = "section.code",
                                 "section" = "section"))
rm(GER_Imp_lo_Sect, GER_Imp_hi_Sect)

GER_Imp_Sect_disag <- full_join(GER_Imp_lo_Sect_disag, GER_Imp_hi_Sect_disag,
                                by = c("reporter" = "reporter",
                                       "reporter.ISO" = "reporter.ISO",
                                       "rRegion" = "rRegion",
                                       "rIncome" = "rIncome",
                                       "rDev" = "rDev",
                                       "year" = "year",
                                       "commodity.code" = "commodity.code",
                                       "commodity" = "commodity"))
rm(GER_Imp_lo_Sect_disag, GER_Imp_hi_Sect_disag)

GER_Exp_lo_Sect <- panel %>%
  filter(pExp_IFF_lo > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev,
           year, 
           section.code, section) %>%
  summarize(Exp_IFF_lo = sum(pExp_IFF_lo, na.rm = T)) %>%
  ungroup()

GER_Exp_lo_Sect_disag <- panel %>%
  filter(pExp_IFF_lo > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev,
           year, 
           commodity.code, commodity) %>%
  summarize(Exp_IFF_lo = sum(pExp_IFF_lo, na.rm = T)) %>%
  ungroup()

GER_Exp_hi_Sect <- panel %>%
  filter(pExp_IFF_hi > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev,
           year, 
           section.code, section) %>%
  summarize(Exp_IFF_hi = sum(pExp_IFF_hi, na.rm = T)) %>%
  ungroup()

GER_Exp_hi_Sect_disag <- panel %>%
  filter(pExp_IFF_hi > 0) %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev,
           year, 
           commodity.code, commodity) %>%
  summarize(Exp_IFF_hi = sum(pExp_IFF_hi, na.rm = T)) %>%
  ungroup()

GER_Exp_Sect <- full_join(GER_Exp_lo_Sect, GER_Exp_hi_Sect,
                          by = c("reporter" = "reporter",
                                 "reporter.ISO" = "reporter.ISO",
                                 "rRegion" = "rRegion",
                                 "rIncome" = "rIncome",
                                 "rDev" = "rDev",
                                 "year" = "year",
                                 "section.code" = "section.code",
                                 "section" = "section"))
rm(GER_Exp_lo_Sect, GER_Exp_hi_Sect)

GER_Exp_Sect_disag <- full_join(GER_Exp_lo_Sect_disag, GER_Exp_hi_Sect_disag,
                                by = c("reporter" = "reporter",
                                       "reporter.ISO" = "reporter.ISO",
                                       "rRegion" = "rRegion",
                                       "rIncome" = "rIncome",
                                       "rDev" = "rDev",
                                       "year" = "year",
                                       "commodity.code" = "commodity.code",
                                       "commodity" = "commodity"))
rm(GER_Exp_lo_Sect_disag, GER_Exp_hi_Sect_disag)

GER_Orig_Sect_Year <- full_join(GER_Imp_Sect, GER_Exp_Sect,
                                by = c("reporter" = "reporter",
                                       "reporter.ISO" = "reporter.ISO",
                                       "rRegion" = "rRegion",
                                       "rIncome" = "rIncome",
                                       "rDev" = "rDev",
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
                                             "year" = "year",
                                             "commodity.code" = "commodity.code",
                                             "commodity" = "commodity"))
rm(GER_Imp_Sect_disag, GER_Exp_Sect_disag)

GER_Orig_Sect_Avg <- GER_Orig_Sect_Year %>%
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, section.code, section) %>%
  summarize(Imp_IFF_lo = mean(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = mean(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = mean(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = mean(Exp_IFF_hi, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF_lo = Imp_IFF_lo + Exp_IFF_lo,
         Tot_IFF_hi = Imp_IFF_hi + Exp_IFF_hi,
         Tot_IFF_lo_bn = Tot_IFF_lo / 10^9,
         Tot_IFF_hi_bn = Tot_IFF_hi / 10^9)
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
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, commodity.code, commodity) %>%
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
  group_by(reporter, reporter.ISO, rRegion, rIncome, rDev, section.code, section) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T)) %>%
  ungroup() %>%
  mutate(Tot_IFF_lo = Imp_IFF_lo + Exp_IFF_lo,
         Tot_IFF_hi = Imp_IFF_hi + Exp_IFF_hi,
         Tot_IFF_lo_bn = Tot_IFF_lo / 10^9,
         Tot_IFF_hi_bn = Tot_IFF_hi / 10^9)

GER_Sect_Year <- GER_Orig_Sect_Year %>%
  group_by(section.code, section, year) %>%
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

GER_Orig_Sect_Sum_Africa <- GER_Orig_Sect_Sum %>%
  filter(rRegion == "Africa") %>%
  select(-rRegion)

GER_Orig_Sect_Sum_LMIC <- GER_Orig_Sect_Sum %>%
  filter(rIncome == "LIC" | rIncome == "LMC") %>%
  select(-rIncome)

GER_Orig_Sect_Sum_Developing <- GER_Orig_Sect_Sum %>%
  filter(rDev == "Developing") %>%
  select(-rDev)

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

GER_Sect_LMIC <- GER_Orig_Sect_Sum_LMIC %>%
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

GER_Sect_Developing <- GER_Orig_Sect_Sum_Developing %>%
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

GER_Sect_Avg_Africa <- GER_Orig_Sect_Avg_Africa %>%
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

GER_Sect_Avg_Africa_disag <- GER_Orig_Sect_Avg_Africa_disag %>%
  group_by(commodity.code, commodity) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T),
            Tot_IFF_lo = sum(Tot_IFF_lo, na.rm = T),
            Tot_IFF_hi = sum(Tot_IFF_hi, na.rm = T),
            Tot_IFF_lo_bn = sum(Tot_IFF_lo_bn, na.rm = T),
            Tot_IFF_hi_bn = sum(Tot_IFF_hi_bn, na.rm = T)) %>%
  ungroup()

GER_Sect_Avg_LMIC <- GER_Orig_Sect_Avg_LMIC %>%
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

GER_Sect_Avg_LMIC_disag <- GER_Orig_Sect_Avg_LMIC_disag %>%
  group_by(commodity.code, commodity) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T),
            Tot_IFF_lo = sum(Tot_IFF_lo, na.rm = T),
            Tot_IFF_hi = sum(Tot_IFF_hi, na.rm = T),
            Tot_IFF_lo_bn = sum(Tot_IFF_lo_bn, na.rm = T),
            Tot_IFF_hi_bn = sum(Tot_IFF_hi_bn, na.rm = T)) %>%
  ungroup()

GER_Sect_Avg_Developing <- GER_Orig_Sect_Avg_Developing %>%
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

GER_Sect_Avg_Developing_disag <- GER_Orig_Sect_Avg_Developing_disag %>%
  group_by(commodity.code, commodity) %>%
  summarize(Imp_IFF_lo = sum(Imp_IFF_lo, na.rm = T),
            Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_lo = sum(Exp_IFF_lo, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T),
            Tot_IFF_lo = sum(Tot_IFF_lo, na.rm = T),
            Tot_IFF_hi = sum(Tot_IFF_hi, na.rm = T),
            Tot_IFF_lo_bn = sum(Tot_IFF_lo_bn, na.rm = T),
            Tot_IFF_hi_bn = sum(Tot_IFF_hi_bn, na.rm = T)) %>%
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


# .. Aggregate results using Net Aggregation ####
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



## ## ## ## ## ## ## ## ## ## ##
# HEADLINE FIGURES          ####
## ## ## ## ## ## ## ## ## ## ##


# .. For Africa ####
(Cumulative.gross.hi <- sum(GER_Year_Africa$Tot_IFF_hi_bn))
# 1204.693

(Cumulative.gross.hi.GDP <- Cumulative.gross.hi / (sum(GER_Year_Africa$GDP) / 10^9)) * 100
# 5.343715

(Cumulative.gross.hi.trade <- Cumulative.gross.hi / (sum(GER_Year_Africa$Total_value) / 10^9)) * 100
# 11.43427

(Cumulative.gross.lo <- sum(GER_Year_Africa$Tot_IFF_lo_bn))
# 337.3599

(Cumulative.gross.lo.GDP <- Cumulative.gross.lo / (sum(GER_Year_Africa$GDP) / 10^9)) * 100
# 1.496444

(Cumulative.gross.lo.trade <- Cumulative.gross.lo / (sum(GER_Year_Africa$Total_value) / 10^9)) * 100
# 3.20203

(Cumulative.net.hi <- sum(Net_Year_Africa$Tot_IFF_hi_bn))
# 362.135

(Cumulative.net.hi.GDP <- Cumulative.net.hi / (sum(Net_Year_Africa$GDP) / 10^9)) * 100
# 1.606249

(Cumulative.net.lo <- sum(Net_Year_Africa$Tot_IFF_lo_bn))
# 139.1025

(Cumulative.net.lo.GDP <- Cumulative.net.lo / (sum(Net_Year_Africa$GDP) / 10^9)) * 100
# 0.616989

(Gross.IFF.per.year.hi <- sum(GER_Orig_Avg_Africa$Tot_IFF_hi_bn))
# 83.31073

(Gross.IFF.per.year.lo <- sum(GER_Orig_Avg_Africa$Tot_IFF_lo_bn))
# 22.525

(Net.IFF.per.year.hi <- sum(Net_Orig_Avg_Africa$Tot_IFF_hi_bn))
# 26.4324

(Net.IFF.per.year.lo <- sum(Net_Orig_Avg_Africa$Tot_IFF_lo_bn))
# 9.264024


# .. For developing countries ####
(Cumulative.gross.hi <- sum(GER_Year_LMIC$Tot_IFF_hi_bn))
# 3421.718

(Cumulative.gross.hi.GDP <- Cumulative.gross.hi / (sum(GER_Year_LMIC$GDP) / 10^9)) * 100
# 5.748657

(Cumulative.gross.hi.trade <- Cumulative.gross.hi / (sum(GER_Year_LMIC$Total_value) / 10^9)) * 100
# 12.95844

(Cumulative.net.hi <- sum(Net_Year_LMIC$Tot_IFF_hi_bn))
# 1472.748

(Cumulative.net.hi.GDP <- Cumulative.net.hi / (sum(Net_Year_LMIC$GDP) / 10^9)) * 100
# 2.474211

(Gross.IFF.per.year.hi <- sum(GER_Orig_Avg_LMIC$Tot_IFF_hi_bn))
# 224.5314

(Net.IFF.per.year.hi <- sum(Net_Orig_Avg_LMIC$Tot_IFF_hi_bn))
# 96.31417



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



## ## ## ## ## ## ## ## ## ## ##
# LMIC RESULTS              ####
## ## ## ## ## ## ## ## ## ## ##

# .. Proportion of LMIC outflows to LMICs ####
load("Results/Summary data-sets/GER_Orig_Avg_LMIC.Rdata")
load("Results/Summary data-sets/GER_Orig_Dest_Avg_LMIC.Rdata")

load("Results/Summary data-sets/GER_Orig_Dest_Year_LMIC.Rdata")

# GER_Orig_Dest_Year_LMIC_toLMICs <- GER_Orig_Dest_Year_LMIC %>%
#   filter(pIncome == "LIC" | pIncome == "LMC") %>%
#   group_by(reporter, reporter.ISO, rRegion, rDev) %>%
#     summarize(Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
#               Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T),
#               Tot_IFF_hi = sum(Tot_IFF_hi, na.rm = T),
#               Tot_IFF_hi_bn = sum(Tot_IFF_hi_bn, na.rm = T)) %>%
#     ungroup()

GER_Orig_Dest_Avg_LMIC_toLMICs <- GER_Orig_Dest_Avg_LMIC %>%
  filter(pIncome == "LIC" | pIncome == "LMC")

GER_Orig_Avg_LMIC_toLMICs <- GER_Orig_Dest_Avg_LMIC_toLMICs %>%
  group_by(reporter, reporter.ISO, rRegion, rDev) %>%
  summarize(Imp_IFF_hi = sum(Imp_IFF_hi, na.rm = T),
            Exp_IFF_hi = sum(Exp_IFF_hi, na.rm = T),
            Tot_IFF_hi = sum(Tot_IFF_hi, na.rm = T),
            Tot_IFF_hi_bn = sum(Tot_IFF_hi_bn, na.rm = T)) %>%
  ungroup()

sum(GER_Orig_Avg_LMIC_toLMICs$Tot_IFF_hi_bn) / sum(GER_Orig_Avg_LMIC$Tot_IFF_hi_bn)
# 0.1454214


# .. Compare net and gross ####
load("Results/Summary data-sets/GER_Orig_Avg_LMIC.Rdata")
load("Results/Summary data-sets/Net_Orig_Avg_LMIC.Rdata")

viz <- full_join(GER_Orig_Avg_LMIC %>% 
                   select(reporter,
                          GER_Tot_IFF_hi = Tot_IFF_hi),
                 Net_Orig_Avg_LMIC %>%
                   select(reporter,
                          Net_Tot_IFF_hi = Tot_IFF_hi),
                 by = c("reporter"))

ggplot(viz %>%  
         melt(id.vars = "reporter"),
       aes(x = reporter, y = value, fill = fct_rev(variable))) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_flip() + 
  scale_y_continuous(labels = dollar_format(scale = 1/10^9, accuracy = 1)) +
  scale_fill_discrete(name = "Estimate", labels = c("Net", "Gross"))
